import Verification.Generated
import Verification.TouchHoldRebuildReverse

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldRebuildReverseExtractedProof

open TouchHoldRebuildReverseProof

/-!
Connection to the actual extraction in
/tmp/lnmai-parser-rebuild-batch-extraction-3/LnmaiCoreVerify.lean, SHA256
d037f357eb395803e657d1ca4b445cc73908e6910c59bb5c37af0de3493ab1bf.
The imported Verification.Generated must contain those declarations.

This proof calls reverse_groups_loop.body, reverse_groups_loop and reverse_groups
directly. It does not replace their execution with a list loop or assume a whole
reverse/rebuild equation. The only adapter obligations used are a successful
length read and an in-bounds two-slot swap with the existing SwapPoints laws.

Capacity bounds the represented OUTER list only. Swap preserves its length, so
no spare slot, len + 1, or bound on group/member/trigger identifiers is needed.
Successful point operations remain explicit assumptions: this does not certify
the concrete Rust Vec facade or allocator. The other rebuild scans, upsert,
source traversal, and the Scheduler/ChartLoader adapters are outside this file.
This source has only been statically reviewed; mainline owns Lean validation.
-/

variable {S B Id Ops α : Type}

/-- Only two actual trait fields; the swap law concerns exactly two list slots.
    In particular it does not assert anything about an entire reverse pass. -/
structure ReverseAtoms
    (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id) (ops : Ops)
    (view : B → List α) (capacity : Nat) : Prop where
  group_length : ∀ groups, (view groups).length ≤ capacity →
    ∃ len, call.group_length ops groups = ok len ∧ len.val = (view groups).length
  swap_groups : ∀ groups (left right : Usize), (view groups).length ≤ capacity →
    left.val < (view groups).length → right.val < (view groups).length →
    ∃ next, call.swap_groups ops groups left right = ok next ∧
      SwapPoints (view groups) (view next) left.val right.val

/-- The actual standard-library slice swap supplies the two-slot law. This
uses its two reads and two writes, with no element clone or default premise.
Identifying the runtime Vec facade with this slice operation remains separate. -/
theorem slice_swap_points (groups : Slice α) (left right : Usize)
    (hleft : left.val < groups.val.length) (hright : right.val < groups.val.length) :
    core.slice.Slice.swap groups left right ⦃ out =>
      SwapPoints groups.val out.val left.val right.val ⦄ := by
  obtain ⟨leftValue, hreadLeft, hleftValue⟩ := spec_imp_exists
    (Slice.index_usize_spec groups left hleft)
  obtain ⟨rightValue, hreadRight, hrightValue⟩ := spec_imp_exists
    (Slice.index_usize_spec groups right hright)
  obtain ⟨first, hwriteLeft, hfirst⟩ := spec_imp_exists
    (Slice.update_spec groups left rightValue hleft)
  have hrightFirst : right.val < first.length := by
    rw [hfirst, Slice.set_length]
    exact hright
  obtain ⟨out, hwriteRight, hout⟩ := spec_imp_exists
    (Slice.update_spec first right leftValue hrightFirst)
  have hview : out.val = swapAt groups.val left.val right.val := by
    rw [hout, hfirst]
    simpa only [Slice.set_val_eq, hleftValue, hrightValue] using
      (swapAt_eq_set groups.val left.val right.val hleft hright).symm
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simp only [core.slice.Slice.swap, hreadLeft, hreadRight, hwriteLeft,
      hwriteRight, bind_tc_ok]
  · rw [hview]
    exact swapAt_points groups.val left.val right.val hleft hright

/-- The real division succeeds also for empty and singleton lists. -/
private theorem half_length (len : Usize) :
    ∃ half, len / 2#usize = ok half ∧ half.val = len.val / 2 := by
  obtain ⟨half, hdiv, hvalue⟩ :=
    UScalar.div_spec len (y := 2#usize) (by simp)
  exact ⟨half, hdiv, by simpa using hvalue⟩

/-- Discharge all checked arithmetic from the guard and the original length.
    Both subtractions are kept separate, exactly as in Generated. -/
private theorem checked_indices (len half left : Usize)
    (hhalf : half.val = len.val / 2) (hmore : left.val < half.val) :
    ∃ last right nextLeft,
      len - 1#usize = ok last ∧ last - left = ok right ∧
      left + 1#usize = ok nextLeft ∧
      right.val = len.val - 1 - left.val ∧ nextLeft.val = left.val + 1 := by
  have hguard : left.val < len.val / 2 := by simpa only [hhalf] using hmore
  obtain ⟨hpositive, hleft, _, _, hnextBound⟩ :=
    body_scalar_bounds len.val left.val (UScalar.max .Usize) hguard (by scalar_tac)
  obtain ⟨last, hlast, hlastValue, _⟩ := spec_imp_exists
    (UScalar.sub_spec (x := len) (y := 1#usize) (by simpa using hpositive))
  have hlastNat : last.val = len.val - 1 := by simpa using hlastValue
  have hleftBound : left.val ≤ last.val := by
    rw [hlastNat]
    exact hleft
  obtain ⟨right, hright, hrightValue, _⟩ := spec_imp_exists
    (UScalar.sub_spec (x := last) (y := left) hleftBound)
  obtain ⟨nextLeft, hnext, hnextValue⟩ := spec_imp_exists
    (UScalar.add_spec (x := left) (y := 1#usize) (by simpa using hnextBound))
  refine ⟨last, right, nextLeft, hlast, hright, hnext, ?_, ?_⟩
  · simpa only [hlastNat] using hrightValue
  · simpa using hnextValue

variable (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id) (ops : Ops)
  (view : B → List α) (capacity : Nat)

/-- A false guard returns without invoking any adapter or doing arithmetic. -/
theorem reverse_body_done (groups : B) (len half left : Usize)
    (hstop : ¬left < half) :
    touch_hold_rebuild.reverse_groups_loop.body call ops len half groups left =
      ok (.done groups) := by
  simp only [touch_hold_rebuild.reverse_groups_loop.body, hstop, ↓reduceIte]

/-- One actual extracted body preserves both completed ends and the unchanged
    middle. Repeated IDs and repeated complete group records are unrestricted. -/
theorem reverse_body_step (atoms : ReverseAtoms call ops view capacity)
    (original : List α) (groups : B) (len half left : Usize)
    (hlen : len.val = original.length) (hhalf : half.val = len.val / 2)
    (hfit : original.length ≤ capacity)
    (hinv : Invariant original left.val (view groups)) (hmore : left < half) :
    ∃ next nextLeft,
      touch_hold_rebuild.reverse_groups_loop.body call ops len half groups left =
        ok (.cont (next, nextLeft)) ∧
      Invariant original nextLeft.val (view next) ∧ nextLeft.val = left.val + 1 := by
  have hmoreNat : left.val < half.val := by simpa using hmore
  have hguard : left.val < original.length / 2 := by
    simpa only [hhalf, hlen] using hmoreNat
  obtain ⟨last, right, nextLeft, hlast, hright, hnext, hrightValue, hnextValue⟩ :=
    checked_indices len half left hhalf hmoreNat
  obtain ⟨_, hleftBound, hrightBound, _, _⟩ :=
    body_index_bounds original.length left.val hguard
  have hrightOriginal : right.val = original.length - 1 - left.val := by
    simpa only [hlen] using hrightValue
  have hcurrentFit : (view groups).length ≤ capacity := by
    simpa only [hinv.length_eq] using hfit
  have hleftSlot : left.val < (view groups).length := by
    simpa only [hinv.length_eq] using hleftBound
  have hrightSlot : right.val < (view groups).length := by
    simpa only [hinv.length_eq, hrightOriginal] using hrightBound
  obtain ⟨next, hswap, hpoints⟩ :=
    atoms.swap_groups groups left right hcurrentFit hleftSlot hrightSlot
  have hmirrorPoints :
      SwapPoints (view groups) (view next) left.val (original.length - 1 - left.val) := by
    simpa only [hrightOriginal] using hpoints
  refine ⟨next, nextLeft, ?_, ?_, hnextValue⟩
  · simp only [touch_hold_rebuild.reverse_groups_loop.body, hmore, ↓reduceIte,
      hlast, hright, hswap, hnext, bind_tc_ok]
  · rw [hnextValue]
    exact invariant_step_of_points hinv hguard hmirrorPoints

/-- Total finite execution from any intermediate state satisfying the existing
    list invariant. The decreasing measure is the actual half-left Usize values. -/
theorem reverse_groups_loop_spec (atoms : ReverseAtoms call ops view capacity)
    (original : List α) (initial : B) (len half left : Usize)
    (hlen : len.val = original.length) (hhalf : half.val = len.val / 2)
    (hfit : original.length ≤ capacity)
    (hinv : Invariant original left.val (view initial)) :
    touch_hold_rebuild.reverse_groups_loop call initial ops len half left ⦃ out =>
      view out = original.reverse ⦄ := by
  unfold touch_hold_rebuild.reverse_groups_loop
  apply loop.spec_decr_nat
    (fun state : B × Usize => half.val - state.2.val)
    (fun state => Invariant original state.2.val (view state.1))
  · rintro ⟨groups, index⟩ hinvNow
    dsimp only at hinvNow ⊢
    try simp only [uncurry_apply_pair]
    by_cases hguard : index < half
    · obtain ⟨next, nextIndex, hbody, hinvNext, hindex⟩ :=
        reverse_body_step call ops view capacity atoms original groups len half index
          hlen hhalf hfit hinvNow hguard
      apply exists_imp_spec
      refine ⟨.cont (next, nextIndex), hbody, ?_⟩
      dsimp only
      refine ⟨hinvNext, ?_⟩
      rw [hindex]
      exact loop_measure_decreases half.val index.val (by simpa using hguard)
    · have hstop : half.val ≤ index.val := by simpa using hguard
      have hdone : original.length / 2 ≤ index.val := by
        simpa only [hhalf, hlen] using hstop
      apply exists_imp_spec
      exact ⟨.done groups, reverse_body_done call ops groups len half index hguard,
        invariant_done hinvNow hdone⟩
  · exact hinv

/-- Real wrapper: group_length, checked division by two, then the real loop at
    zero. No initialization or complete-reverse correctness is assumed. -/
theorem reverse_groups_spec (atoms : ReverseAtoms call ops view capacity)
    (groups : B) (hfit : (view groups).length ≤ capacity) :
    touch_hold_rebuild.reverse_groups call groups ops ⦃ out =>
      view out = (view groups).reverse ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.group_length groups hfit
  obtain ⟨half, hdiv, hhalf⟩ := half_length len
  have hinit : Invariant (view groups) (0#usize).val (view groups) := by
    simpa using invariant_init (view groups)
  obtain ⟨out, hloop, hout⟩ := spec_imp_exists
    (reverse_groups_loop_spec call ops view capacity atoms (view groups) groups
      len half 0#usize hlen hhalf hfit hinit)
  apply exists_imp_spec
  refine ⟨out, ?_, hout⟩
  simp only [touch_hold_rebuild.reverse_groups, hlength, hdiv, hloop, bind_tc_ok]

/-- The actual extraction returns ok; both group count and the logical capacity
    condition are preserved. This also rules out failure and divergence. -/
theorem reverse_groups_equiv (atoms : ReverseAtoms call ops view capacity)
    (groups : B) (hfit : (view groups).length ≤ capacity) :
    ∃ out, touch_hold_rebuild.reverse_groups call groups ops = ok out ∧
      view out = (view groups).reverse ∧
      (view out).length = (view groups).length ∧ (view out).length ≤ capacity := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (reverse_groups_spec call ops view capacity atoms groups hfit)
  refine ⟨out, hrun, hout, ?_, ?_⟩
  · simp only [hout, List.length_reverse]
  · simpa only [hout, List.length_reverse] using hfit

end Verification.TouchHoldRebuildReverseExtractedProof
