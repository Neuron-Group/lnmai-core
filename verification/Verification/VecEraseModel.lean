import Verification.TouchHoldRebuildReverse
import Init.Data.List.Nat.Erase
import Aeneas.Std.Vec

/-!
Value model for ordered indexed erase: assert index < length, move the selected
value to the last slot by adjacent swaps, then resize to length - 1.

The point invariant preserves the prefix, records the left-shifted interval,
tracks the removed value at the cursor, and preserves the unvisited suffix.
It permits arbitrary repeated values and requires no equality or Clone law.
The only mutation premise is one SwapPoints fact for one adjacent swap.

The final helper unfolds the existing Aeneas Vec.resize shrink branch, without
assuming that cloning the filler succeeds. No new Generated names are guessed.
Rust kernel/facade connection awaits its actual extraction. Native standard
library refinement, allocation, and observable generic Drop behavior are not
proved here. A discarded Natural::default filler is absent from the value
result; this does not assert anything about its destructor.

Source draft for the mainline's sole compiler. Not compiled by this author.
-/

namespace Verification.VecEraseModel

open TouchHoldRebuildReverseProof

universe u
variable {α : Type u}

/-- The selected value travels right; all other values keep their order. -/
structure Invariant (original : List α) (erased cursor : Nat)
    (current : List α) : Prop where
  length_eq : current.length = original.length
  erased_le : erased ≤ cursor
  cursor_lt : cursor < original.length
  prefix_eq : ∀ pos, pos < erased → current[pos]? = original[pos]?
  shifted_eq : ∀ pos, erased ≤ pos → pos < cursor →
    current[pos]? = original[pos + 1]?
  carried_eq : current[cursor]? = original[erased]?
  suffix_eq : ∀ pos, cursor < pos → pos < original.length →
    current[pos]? = original[pos]?

theorem invariant_init (original : List α) (index : Nat)
    (hvalid : index < original.length) :
    Invariant original index index original := by
  refine ⟨rfl, Nat.le_refl _, hvalid, ?_, ?_, rfl, ?_⟩
  · intro pos _
    rfl
  · intro pos hlo hhi
    omega
  · intro pos _ _
    rfl

/-- Obligations for last := len - 1 after the checked entry condition. -/
theorem entry_scalar_bounds (len index maxValue : Nat)
    (hvalid : index < len) (hlen : len ≤ maxValue) :
    0 < len ∧ index ≤ len - 1 ∧ len - 1 < len ∧ len - 1 ≤ maxValue := by
  omega

/-- The loop guard justifies both swap positions and the checked increment. -/
theorem body_scalar_bounds (len cursor maxValue : Nat)
    (hmore : cursor < len - 1) (hlen : len ≤ maxValue) :
    cursor < len ∧ cursor + 1 < len ∧ cursor + 1 ≤ maxValue := by
  omega

theorem loop_measure_decreases (last cursor : Nat) (hmore : cursor < last) :
    last - (cursor + 1) < last - cursor := by
  omega

/-- One adjacent swap, using only its two-slot effect. -/
theorem invariant_step_of_points {original before after : List α}
    {erased cursor : Nat} (hinv : Invariant original erased cursor before)
    (hmore : cursor < original.length - 1)
    (hswap : SwapPoints before after cursor (cursor + 1)) :
    Invariant original erased (cursor + 1) after := by
  have hec := hinv.erased_le
  have hcn := hinv.cursor_lt
  refine ⟨hswap.length_eq.trans hinv.length_eq, by omega, by omega, ?_, ?_, ?_, ?_⟩
  · intro pos hpos
    rw [hswap.other_eq pos (by omega) (by omega)]
    exact hinv.prefix_eq pos hpos
  · intro pos hlo hhi
    by_cases heq : pos = cursor
    · subst pos
      exact hswap.left_eq.trans (hinv.suffix_eq (cursor + 1) (by omega) (by omega))
    · rw [hswap.other_eq pos heq (by omega)]
      exact hinv.shifted_eq pos hlo (by omega)
  · exact hswap.right_eq.trans hinv.carried_eq
  · intro pos hlo hhi
    rw [hswap.other_eq pos (by omega) (by omega)]
    exact hinv.suffix_eq pos (by omega) hhi

theorem invariant_step {original current : List α} {erased cursor : Nat}
    (hinv : Invariant original erased cursor current)
    (hmore : cursor < original.length - 1) :
    Invariant original erased (cursor + 1) (swapAt current cursor (cursor + 1)) := by
  apply invariant_step_of_points hinv hmore
  apply swapAt_points
  · rw [hinv.length_eq]
    omega
  · rw [hinv.length_eq]
    omega

/-- A false guard, together with the invariant, puts the carried value last. -/
theorem invariant_terminal_cursor {original current : List α} {erased cursor : Nat}
    (hinv : Invariant original erased cursor current)
    (hstop : original.length - 1 ≤ cursor) : cursor = original.length - 1 := by
  have hcn := hinv.cursor_lt
  omega

/-- Removing the last slot now removes exactly the original selected index. -/
theorem invariant_done_take {original current : List α} {erased cursor : Nat}
    (hinv : Invariant original erased cursor current)
    (hstop : original.length - 1 ≤ cursor) :
    current.take (original.length - 1) = original.eraseIdx erased := by
  have hcursor := invariant_terminal_cursor hinv hstop
  have hec := hinv.erased_le
  apply List.ext_getElem?
  intro pos
  by_cases hpos : pos < original.length - 1
  · rw [List.getElem?_take, if_pos hpos]
    by_cases hpre : pos < erased
    · rw [List.getElem?_eraseIdx_of_lt hpre]
      exact hinv.prefix_eq pos hpre
    · have hge : erased ≤ pos := by omega
      rw [List.getElem?_eraseIdx_of_ge hge]
      exact hinv.shifted_eq pos hge (by omega)
  · have hge : erased ≤ pos := by omega
    have hout : original.length ≤ pos + 1 := by omega
    rw [List.getElem?_take, if_neg hpos, List.getElem?_eraseIdx_of_ge hge,
      List.getElem?_eq_none hout]

/-- This is the actual List.resize used by Aeneas, specialized to non-growth. -/
theorem resize_shrink_eq_take (items : List α) (newLen : Nat) (filler : α)
    (hlen : newLen ≤ items.length) :
    items.resize newLen filler = items.take newLen := by
  simp [List.resize, Nat.sub_eq_zero_of_le hlen]

theorem resize_shrink_filler_independent (items : List α) (newLen : Nat)
    (left right : α) (hlen : newLen ≤ items.length) :
    items.resize newLen left = items.resize newLen right := by
  rw [resize_shrink_eq_take items newLen left hlen,
    resize_shrink_eq_take items newLen right hlen]

theorem invariant_done_resize {original current : List α} {erased cursor : Nat}
    (hinv : Invariant original erased cursor current)
    (hstop : original.length - 1 ≤ cursor) (filler : α) :
    current.resize (original.length - 1) filler = original.eraseIdx erased := by
  have hresize : original.length - 1 ≤ current.length := by
    rw [hinv.length_eq]
    omega
  rw [resize_shrink_eq_take current (original.length - 1) filler hresize]
  exact invariant_done_take hinv hstop

/-- Same cursor and snapshotted last index as the installed Rust source. -/
def moveLoop (last cursor : Nat) (current : List α) : List α :=
  if hmore : cursor < last then
    moveLoop last (cursor + 1) (swapAt current cursor (cursor + 1))
  else current
termination_by last - cursor
decreasing_by omega

theorem moveLoop_stop {last cursor : Nat} {current : List α}
    (hstop : last ≤ cursor) : moveLoop last cursor current = current := by
  have hnot : ¬cursor < last := by omega
  rw [moveLoop]
  simp only [hnot, ↓reduceIte, ↓reduceDIte]

theorem moveLoop_step {last cursor : Nat} {current : List α}
    (hmore : cursor < last) :
    moveLoop last cursor current =
      moveLoop last (cursor + 1) (swapAt current cursor (cursor + 1)) := by
  rw [moveLoop]
  simp only [hmore, ↓reduceIte, ↓reduceDIte]

/-- Induct on the number of swaps still required; no whole-loop premise. -/
theorem moveLoop_finished (original : List α) (erased remaining : Nat) :
    ∀ (cursor : Nat) (current : List α), original.length - 1 - cursor = remaining →
      Invariant original erased cursor current →
      Invariant original erased (original.length - 1)
        (moveLoop (original.length - 1) cursor current) := by
  induction remaining with
  | zero =>
      intro cursor current hremaining hinv
      have hstop : original.length - 1 ≤ cursor := by omega
      have hcursor := invariant_terminal_cursor hinv hstop
      rw [moveLoop_stop hstop]
      simpa only [hcursor] using hinv
  | succ remaining ih =>
      intro cursor current hremaining hinv
      have hmore : cursor < original.length - 1 := by omega
      rw [moveLoop_step hmore]
      apply ih (cursor + 1) (swapAt current cursor (cursor + 1))
      · omega
      · exact invariant_step hinv hmore

theorem moveLoop_complete {original current : List α} {erased cursor : Nat}
    (hinv : Invariant original erased cursor current) :
    Invariant original erased (original.length - 1)
      (moveLoop (original.length - 1) cursor current) :=
  moveLoop_finished original erased (original.length - 1 - cursor)
    cursor current rfl hinv

theorem moveLoop_resize_eq_eraseIdx {original current : List α} {erased cursor : Nat}
    (hinv : Invariant original erased cursor current) (filler : α) :
    (moveLoop (original.length - 1) cursor current).resize
        (original.length - 1) filler = original.eraseIdx erased :=
  invariant_done_resize (moveLoop_complete hinv) (Nat.le_refl _) filler

/-- Successful-entry interpretation; its correctness requires index < length. -/
def eraseAt (original : List α) (index : Nat) (filler : α) : List α :=
  (moveLoop (original.length - 1) index original).resize (original.length - 1) filler

theorem eraseAt_eq_eraseIdx (original : List α) (index : Nat) (filler : α)
    (hvalid : index < original.length) :
    eraseAt original index filler = original.eraseIdx index := by
  unfold eraseAt
  exact moveLoop_resize_eq_eraseIdx (invariant_init original index hvalid) filler

theorem eraseAt_eq_take_drop (original : List α) (index : Nat) (filler : α)
    (hvalid : index < original.length) :
    eraseAt original index filler = original.take index ++ original.drop (index + 1) := by
  rw [eraseAt_eq_eraseIdx original index filler hvalid, List.eraseIdx_eq_take_drop_succ]

theorem eraseAt_length (original : List α) (index : Nat) (filler : α)
    (hvalid : index < original.length) :
    (eraseAt original index filler).length = original.length - 1 := by
  rw [eraseAt_eq_eraseIdx original index filler hvalid, List.length_eraseIdx_of_lt hvalid]

theorem eraseAt_length_drop (original : List α) (index : Nat) (filler : α)
    (hvalid : index < original.length) :
    (eraseAt original index filler).length < original.length ∧
      (eraseAt original index filler).length + 1 = original.length := by
  have hlen := eraseAt_length original index filler hvalid
  constructor <;> omega

theorem eraseAt_reads_before (original : List α) (index pos : Nat) (filler : α)
    (hvalid : index < original.length) (hpos : pos < index) :
    (eraseAt original index filler)[pos]? = original[pos]? := by
  rw [eraseAt_eq_eraseIdx original index filler hvalid, List.getElem?_eraseIdx_of_lt hpos]

theorem eraseAt_reads_after (original : List α) (index pos : Nat) (filler : α)
    (hvalid : index < original.length) (hpos : index ≤ pos) :
    (eraseAt original index filler)[pos]? = original[pos + 1]? := by
  rw [eraseAt_eq_eraseIdx original index filler hvalid, List.getElem?_eraseIdx_of_ge hpos]

theorem eraseAt_filler_independent (original : List α) (index : Nat) (left right : α)
    (hvalid : index < original.length) :
    eraseAt original index left = eraseAt original index right := by
  rw [eraseAt_eq_eraseIdx original index left hvalid,
    eraseAt_eq_eraseIdx original index right hvalid]

/-- Value-only checked entry. None abstracts rejection, not a Rust panic/Drop trace. -/
def eraseChecked (original : List α) (index : Nat) (filler : α) : Option (List α) :=
  if index < original.length then some (eraseAt original index filler) else none

theorem eraseChecked_valid (original : List α) (index : Nat) (filler : α)
    (hvalid : index < original.length) :
    eraseChecked original index filler = some (original.eraseIdx index) := by
  simp only [eraseChecked, hvalid, ↓reduceIte,
    eraseAt_eq_eraseIdx original index filler hvalid]

theorem eraseChecked_invalid (original : List α) (index : Nat) (filler : α)
    (hbad : original.length ≤ index) : eraseChecked original index filler = none := by
  simp only [eraseChecked, Nat.not_lt.mpr hbad, ↓reduceIte]

open Aeneas Aeneas.Std

/-- Actual existing Std shrink branch: no hypothesis about Clone, and no call
of cloneInst.clone occurs on this path. This is not a native backend proof. -/
theorem vec_resize_shrink {T : Type} (cloneInst : core.clone.Clone T)
    (values : alloc.vec.Vec T) (newLen : Usize) (filler : T)
    (hshrink : newLen.val < values.val.length) :
    ∃ out : alloc.vec.Vec T,
      alloc.vec.Vec.resize cloneInst values newLen filler = Result.ok out ∧
      out.val = values.val.take newLen.val := by
  simp only [alloc.vec.Vec.resize, alloc.vec.Vec.length, hshrink, ↓reduceIte]
  refine ⟨_, rfl, ?_⟩
  exact resize_shrink_eq_take values.val newLen.val filler (Nat.le_of_lt hshrink)

end Verification.VecEraseModel
