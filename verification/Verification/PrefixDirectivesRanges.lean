import Verification.PrefixDirectivesModel
import Init.Data.List.Nat.TakeDrop
import Lean.Elab.Tactic.Omega

/-! Concrete range and loop-state lemmas for the pending prefix extraction.
No Generated declarations are imported or guessed. All scripts are uncompiled.
The predicates below are explicit list/index invariants, not facade agreements.
They do not assert that any Rust operation preserves them. -/

namespace Verification.PrefixDirectivesRanges

open Verification.PrefixDirectivesModel

/-- Half-open scalar range. Natural subtraction is intentional; callers of the
Rust range helpers must additionally establish ordered, bounded endpoints. -/
def rangeChars (chars : List Char) (begin finish : Nat) : List Char :=
  (chars.drop begin).take (finish - begin)

@[simp] theorem rangeChars_self (chars : List Char) (cursor : Nat) :
    rangeChars chars cursor cursor = [] := by
  simp [rangeChars]

theorem rangeChars_of_finish_le (chars : List Char) {begin finish : Nat}
    (h : finish ≤ begin) : rangeChars chars begin finish = [] := by
  simp [rangeChars, Nat.sub_eq_zero_of_le h]

theorem rangeChars_of_length_le (chars : List Char) {begin finish : Nat}
    (h : chars.length ≤ begin) : rangeChars chars begin finish = [] := by
  simp [rangeChars, List.drop_eq_nil_of_le h]

@[simp] theorem rangeChars_full (chars : List Char) :
    rangeChars chars 0 chars.length = chars := by
  simp [rangeChars]

theorem rangeChars_length (chars : List Char) {begin finish : Nat}
    (hbegin : begin ≤ finish) (hfinish : finish ≤ chars.length) :
    (rangeChars chars begin finish).length = finish - begin := by
  have h : finish - begin ≤ chars.length - begin := by omega
  simp only [rangeChars, List.length_take, List.length_drop, Nat.min_eq_left h]

theorem rangeChars_sublist (chars : List Char) (begin finish : Nat) :
    (rangeChars chars begin finish).Sublist chars :=
  (List.take_sublist (finish - begin) (chars.drop begin)).trans
    (List.drop_sublist begin chars)

theorem rangeChars_eq_nil_iff (chars : List Char) {begin finish : Nat}
    (hbegin : begin ≤ finish) (hfinish : finish ≤ chars.length) :
    rangeChars chars begin finish = [] ↔ begin = finish := by
  constructor
  · intro hempty
    have hlen := rangeChars_length chars hbegin hfinish
    rw [hempty] at hlen
    simp only [List.length_nil] at hlen
    omega
  · intro heq
    subst finish
    exact rangeChars_self chars begin

/-- Exposes exactly the next indexed character, under the actual forward guard. -/
theorem rangeChars_cons (chars : List Char) {cursor finish : Nat}
    (hmore : cursor < finish) (hfinish : finish ≤ chars.length) :
    rangeChars chars cursor finish =
      chars[cursor]'(by omega) :: rangeChars chars (cursor + 1) finish := by
  have hindex : cursor < chars.length := by omega
  have hsub : finish - cursor = (finish - (cursor + 1)) + 1 := by omega
  unfold rangeChars
  rw [List.drop_eq_getElem_cons hindex, hsub, List.take_succ_cons]

/-- One pushed character extends the copied prefix by exactly one scalar. -/
theorem rangeChars_snoc (chars : List Char) {begin cursor : Nat}
    (hbegin : begin ≤ cursor) (hindex : cursor < chars.length) :
    rangeChars chars begin (cursor + 1) =
      rangeChars chars begin cursor ++ [chars[cursor]] := by
  have hlocal : cursor - begin < (chars.drop begin).length := by
    simp only [List.length_drop]
    omega
  have hsub : cursor + 1 - begin = (cursor - begin) + 1 := by omega
  have hadd : begin + (cursor - begin) = cursor := by omega
  simp only [rangeChars, hsub, List.take_succ_eq_append_getElem hlocal,
    List.getElem_drop, hadd]

/-- Exposes exactly the final indexed character, after checking begin < finish. -/
theorem rangeChars_unsnoc (chars : List Char) {begin finish : Nat}
    (hmore : begin < finish) (hfinish : finish ≤ chars.length) :
    rangeChars chars begin finish =
      rangeChars chars begin (finish - 1) ++ [chars[finish - 1]'(by omega)] := by
  have hbegin : begin ≤ finish - 1 := by omega
  have hindex : finish - 1 < chars.length := by omega
  have hsucc : finish - 1 + 1 = finish := by omega
  simpa only [hsucc] using rangeChars_snoc chars hbegin hindex

/-- Splitting a range neither inserts nor loses separator/Unicode characters. -/
theorem rangeChars_append (chars : List Char) {begin middle finish : Nat}
    (hbegin : begin ≤ middle) (hmiddle : middle ≤ finish) :
    rangeChars chars begin finish =
      rangeChars chars begin middle ++ rangeChars chars middle finish := by
  have hsub : finish - begin = (middle - begin) + (finish - middle) := by omega
  have hadd : begin + (middle - begin) = middle := by omega
  unfold rangeChars
  rw [hsub, List.take_add, List.drop_drop, hadd]

/-- The second trim phase alone; the source model is this applied after the
forward dropWhile, in precisely that order. -/
def trimEndChars (chars : List Char) : List Char :=
  (chars.reverse.dropWhile isPrefixWhitespace).reverse

@[simp] theorem trimEndChars_nil : trimEndChars [] = [] := rfl

theorem trimChars_eq_trimEndChars (chars : List Char) :
    trimChars chars = trimEndChars (chars.dropWhile isPrefixWhitespace) := rfl

theorem front_whitespace_step (chars : List Char) {cursor finish : Nat}
    (hmore : cursor < finish) (hfinish : finish ≤ chars.length)
    (hwhite : isPrefixWhitespace (chars[cursor]'(by omega)) = true) :
    (rangeChars chars cursor finish).dropWhile isPrefixWhitespace =
      (rangeChars chars (cursor + 1) finish).dropWhile isPrefixWhitespace := by
  rw [rangeChars_cons chars hmore hfinish, List.dropWhile_cons]
  simp only [hwhite, ↓reduceIte]

theorem front_nonwhitespace_stop (chars : List Char) {cursor finish : Nat}
    (hmore : cursor < finish) (hfinish : finish ≤ chars.length)
    (hwhite : isPrefixWhitespace (chars[cursor]'(by omega)) = false) :
    (rangeChars chars cursor finish).dropWhile isPrefixWhitespace =
      rangeChars chars cursor finish := by
  simp only [rangeChars_cons chars hmore hfinish, List.dropWhile_cons, hwhite,
    Bool.false_eq_true, ↓reduceIte]

theorem back_whitespace_step (chars : List Char) {begin cursor : Nat}
    (hmore : begin < cursor) (hcursor : cursor ≤ chars.length)
    (hwhite : isPrefixWhitespace (chars[cursor - 1]'(by omega)) = true) :
    trimEndChars (rangeChars chars begin cursor) =
      trimEndChars (rangeChars chars begin (cursor - 1)) := by
  unfold trimEndChars
  rw [rangeChars_unsnoc chars hmore hcursor]
  simp only [List.reverse_append, List.reverse_singleton, List.singleton_append,
    List.dropWhile_cons, hwhite, ↓reduceIte]

theorem back_nonwhitespace_stop (chars : List Char) {begin cursor : Nat}
    (hmore : begin < cursor) (hcursor : cursor ≤ chars.length)
    (hwhite : isPrefixWhitespace (chars[cursor - 1]'(by omega)) = false) :
    trimEndChars (rangeChars chars begin cursor) = rangeChars chars begin cursor := by
  simp only [rangeChars_unsnoc chars hmore hcursor, trimEndChars,
    List.reverse_append, List.reverse_singleton, List.singleton_append,
    List.dropWhile_cons, hwhite, Bool.false_eq_true, ↓reduceIte,
    List.reverse_cons, List.reverse_reverse]

/-- Forward-trim loop invariant, with original endpoints kept explicit. -/
structure FrontInv (chars : List Char) (begin finish cursor : Nat) : Prop where
  lower : begin ≤ cursor
  upper : cursor ≤ finish
  finish_le : finish ≤ chars.length
  preserved : (rangeChars chars cursor finish).dropWhile isPrefixWhitespace =
    (rangeChars chars begin finish).dropWhile isPrefixWhitespace

theorem front_init (chars : List Char) {begin finish : Nat}
    (hbegin : begin ≤ finish) (hfinish : finish ≤ chars.length) :
    FrontInv chars begin finish begin :=
  ⟨Nat.le_refl _, hbegin, hfinish, rfl⟩

theorem front_step {chars : List Char} {begin finish cursor : Nat}
    (inv : FrontInv chars begin finish cursor) (hmore : cursor < finish)
    (hwhite : isPrefixWhitespace
      (chars[cursor]'(Nat.lt_of_lt_of_le hmore inv.finish_le)) = true) :
    FrontInv chars begin finish (cursor + 1) := by
  refine ⟨Nat.le_trans inv.lower (by omega), by omega, inv.finish_le, ?_⟩
  exact (front_whitespace_step chars hmore inv.finish_le hwhite).symm.trans inv.preserved

theorem front_done_empty {chars : List Char} {begin finish cursor : Nat}
    (inv : FrontInv chars begin finish cursor) (hdone : ¬ cursor < finish) :
    rangeChars chars cursor finish =
      (rangeChars chars begin finish).dropWhile isPrefixWhitespace := by
  have hupper := inv.upper
  have heq : cursor = finish := by omega
  subst cursor
  simpa only [rangeChars_self, List.dropWhile_nil] using inv.preserved

theorem front_done_nonwhitespace {chars : List Char} {begin finish cursor : Nat}
    (inv : FrontInv chars begin finish cursor) (hmore : cursor < finish)
    (hwhite : isPrefixWhitespace
      (chars[cursor]'(Nat.lt_of_lt_of_le hmore inv.finish_le)) = false) :
    rangeChars chars cursor finish =
      (rangeChars chars begin finish).dropWhile isPrefixWhitespace := by
  exact (front_nonwhitespace_stop chars hmore inv.finish_le hwhite).symm.trans inv.preserved

/-- Backward-trim loop invariant. The left endpoint is the fixed result of the
forward phase; the right cursor only decreases. -/
structure BackInv (chars : List Char) (begin finish cursor : Nat) : Prop where
  lower : begin ≤ cursor
  upper : cursor ≤ finish
  finish_le : finish ≤ chars.length
  preserved : trimEndChars (rangeChars chars begin cursor) =
    trimEndChars (rangeChars chars begin finish)

theorem back_init (chars : List Char) {begin finish : Nat}
    (hbegin : begin ≤ finish) (hfinish : finish ≤ chars.length) :
    BackInv chars begin finish finish :=
  ⟨hbegin, Nat.le_refl _, hfinish, rfl⟩

theorem back_step {chars : List Char} {begin finish cursor : Nat}
    (inv : BackInv chars begin finish cursor) (hmore : begin < cursor)
    (hwhite : isPrefixWhitespace
      (chars[cursor - 1]'(Nat.lt_of_lt_of_le (by omega)
        (Nat.le_trans inv.upper inv.finish_le))) = true) :
    BackInv chars begin finish (cursor - 1) := by
  refine ⟨by omega, Nat.le_trans (Nat.sub_le _ _) inv.upper, inv.finish_le, ?_⟩
  exact (back_whitespace_step chars hmore (Nat.le_trans inv.upper inv.finish_le) hwhite).symm.trans
    inv.preserved

theorem back_done_empty {chars : List Char} {begin finish cursor : Nat}
    (inv : BackInv chars begin finish cursor) (hdone : ¬ begin < cursor) :
    rangeChars chars begin cursor = trimEndChars (rangeChars chars begin finish) := by
  have hlower := inv.lower
  have heq : cursor = begin := by omega
  subst cursor
  simpa only [rangeChars_self, trimEndChars_nil] using inv.preserved

theorem back_done_nonwhitespace {chars : List Char} {begin finish cursor : Nat}
    (inv : BackInv chars begin finish cursor) (hmore : begin < cursor)
    (hwhite : isPrefixWhitespace
      (chars[cursor - 1]'(Nat.lt_of_lt_of_le (by omega)
        (Nat.le_trans inv.upper inv.finish_le))) = false) :
    rangeChars chars begin cursor = trimEndChars (rangeChars chars begin finish) := by
  exact (back_nonwhitespace_stop chars hmore (Nat.le_trans inv.upper inv.finish_le) hwhite).symm.trans
    inv.preserved

theorem closer_miss_step (chars : List Char) (closer : Char) {cursor finish : Nat}
    (hmore : cursor < finish) (hfinish : finish ≤ chars.length)
    (hmiss : chars[cursor]'(by omega) ≠ closer) :
    afterFirst closer (rangeChars chars cursor finish) =
      afterFirst closer (rangeChars chars (cursor + 1) finish) := by
  rw [rangeChars_cons chars hmore hfinish]
  simp [afterFirst, hmiss]

/-- A hit returns AFTER incrementing, so the closer itself is not copied. -/
theorem closer_hit_step (chars : List Char) (closer : Char) {cursor finish : Nat}
    (hmore : cursor < finish) (hfinish : finish ≤ chars.length)
    (hhit : chars[cursor]'(by omega) = closer) :
    afterFirst closer (rangeChars chars cursor finish) =
      rangeChars chars (cursor + 1) finish := by
  rw [rangeChars_cons chars hmore hfinish]
  simp [afterFirst, hhit]

/-- The consumed range contains no closer, making the FIRST hit explicit. -/
structure CloserInv (chars : List Char) (closer : Char)
    (begin finish cursor : Nat) : Prop where
  lower : begin ≤ cursor
  upper : cursor ≤ finish
  finish_le : finish ≤ chars.length
  nohit : closer ∉ rangeChars chars begin cursor
  preserved : afterFirst closer (rangeChars chars cursor finish) =
    afterFirst closer (rangeChars chars begin finish)

theorem closer_init (chars : List Char) (closer : Char) {begin finish : Nat}
    (hbegin : begin ≤ finish) (hfinish : finish ≤ chars.length) :
    CloserInv chars closer begin finish begin := by
  refine ⟨Nat.le_refl _, hbegin, hfinish, ?_, rfl⟩
  rw [rangeChars_self]
  exact List.not_mem_nil

theorem closer_step {chars : List Char} {closer : Char} {begin finish cursor : Nat}
    (inv : CloserInv chars closer begin finish cursor) (hmore : cursor < finish)
    (hmiss : chars[cursor]'(Nat.lt_of_lt_of_le hmore inv.finish_le) ≠ closer) :
    CloserInv chars closer begin finish (cursor + 1) := by
  refine ⟨Nat.le_trans inv.lower (by omega), by omega, inv.finish_le, ?_, ?_⟩
  · rw [rangeChars_snoc chars inv.lower (Nat.lt_of_lt_of_le hmore inv.finish_le)]
    intro hmem
    rcases List.mem_append.mp hmem with hleft | hright
    · exact inv.nohit hleft
    · exact hmiss (List.mem_singleton.mp hright).symm
  · exact (closer_miss_step chars closer hmore inv.finish_le hmiss).symm.trans inv.preserved

theorem closer_done_hit {chars : List Char} {closer : Char} {begin finish cursor : Nat}
    (inv : CloserInv chars closer begin finish cursor) (hmore : cursor < finish)
    (hhit : chars[cursor]'(Nat.lt_of_lt_of_le hmore inv.finish_le) = closer) :
    rangeChars chars (cursor + 1) finish =
      afterFirst closer (rangeChars chars begin finish) := by
  exact (closer_hit_step chars closer hmore inv.finish_le hhit).symm.trans inv.preserved

theorem closer_done_absent {chars : List Char} {closer : Char} {begin finish cursor : Nat}
    (inv : CloserInv chars closer begin finish cursor) (hdone : ¬ cursor < finish) :
    closer ∉ rangeChars chars begin finish ∧
      rangeChars chars finish finish = afterFirst closer (rangeChars chars begin finish) := by
  have hupper := inv.upper
  have heq : cursor = finish := by omega
  subst cursor
  refine ⟨inv.nohit, ?_⟩
  simpa only [rangeChars_self, afterFirst_nil] using inv.preserved

/-- The output is exactly the already visited prefix of the selected range. -/
structure CopyInv (chars : List Char) (begin finish cursor : Nat) (out : List Char) : Prop where
  lower : begin ≤ cursor
  upper : cursor ≤ finish
  finish_le : finish ≤ chars.length
  copied : out = rangeChars chars begin cursor

theorem copy_init (chars : List Char) {begin finish : Nat}
    (hbegin : begin ≤ finish) (hfinish : finish ≤ chars.length) :
    CopyInv chars begin finish begin [] := by
  exact ⟨Nat.le_refl _, hbegin, hfinish, (rangeChars_self chars begin).symm⟩

theorem copy_step {chars : List Char} {begin finish cursor : Nat} {out : List Char}
    (inv : CopyInv chars begin finish cursor out) (hmore : cursor < finish) :
    CopyInv chars begin finish (cursor + 1)
      (out ++ [chars[cursor]'(Nat.lt_of_lt_of_le hmore inv.finish_le)]) := by
  refine ⟨Nat.le_trans inv.lower (by omega), by omega, inv.finish_le, ?_⟩
  have hindex : cursor < chars.length := Nat.lt_of_lt_of_le hmore inv.finish_le
  exact (congrArg (fun xs : List Char => xs ++ [chars[cursor]'hindex]) inv.copied).trans
    (rangeChars_snoc chars inv.lower hindex).symm

theorem copy_done {chars : List Char} {begin finish cursor : Nat} {out : List Char}
    (inv : CopyInv chars begin finish cursor out) (hdone : ¬ cursor < finish) :
    out = rangeChars chars begin finish := by
  have hupper := inv.upper
  have heq : cursor = finish := by omega
  simpa only [heq] using inv.copied

theorem copy_length {chars : List Char} {begin finish cursor : Nat} {out : List Char}
    (inv : CopyInv chars begin finish cursor out) :
    out.length = cursor - begin := by
  rw [inv.copied]
  exact rangeChars_length chars inv.lower (Nat.le_trans inv.upper inv.finish_le)

theorem copy_order {chars : List Char} {begin finish cursor : Nat} {out : List Char}
    (inv : CopyInv chars begin finish cursor out) : out.Sublist chars := by
  rw [inv.copied]
  exact rangeChars_sublist chars begin cursor

/-- Normal-branch allocation laws still have to justify the real Vec push;
this lemma supplies only its mathematical output-size bound. -/
theorem copy_step_length_bound {chars : List Char} {begin finish cursor : Nat} {out : List Char}
    (inv : CopyInv chars begin finish cursor out) (hmore : cursor < finish) :
    out.length + 1 ≤ chars.length := by
  have hlen := copy_length inv
  have hend := inv.finish_le
  omega

/-- These guards justify arithmetic/read bounds only, not allocator success. -/
theorem forward_point_bounds {cursor finish limit : Nat}
    (hmore : cursor < finish) (hlimit : finish ≤ limit) :
    cursor < limit ∧ cursor + 1 ≤ finish ∧ cursor + 1 ≤ limit ∧
      finish - (cursor + 1) < finish - cursor := by
  omega

theorem backward_point_bounds {begin cursor limit : Nat}
    (hmore : begin < cursor) (hlimit : cursor ≤ limit) :
    0 < cursor ∧ begin ≤ cursor - 1 ∧ cursor - 1 < limit ∧
      (cursor - 1) - begin < cursor - begin := by
  omega

/-- The prefix dispatch branch is a single point observation after trimming. -/
theorem stripTrimmed_range_none (chars : List Char) {begin finish : Nat}
    (hmore : begin < finish) (hfinish : finish ≤ chars.length)
    (hnone : directiveCloser (chars[begin]'(by omega)) = none) :
    stripTrimmed (rangeChars chars begin finish) = rangeChars chars begin finish := by
  simp only [rangeChars_cons chars hmore hfinish, stripTrimmed, hnone]

theorem stripTrimmed_range_some (chars : List Char) {begin finish : Nat} (closer : Char)
    (hmore : begin < finish) (hfinish : finish ≤ chars.length)
    (hsome : directiveCloser (chars[begin]'(by omega)) = some closer) :
    stripTrimmed (rangeChars chars begin finish) =
      trimChars (afterFirst closer (rangeChars chars begin finish)) := by
  simp only [rangeChars_cons chars hmore hfinish, stripTrimmed, hsome]

end Verification.PrefixDirectivesRanges
