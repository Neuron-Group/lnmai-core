import Verification.PrefixDirectivesModel
import LnmaiCore.Simai.Timing
import Init.Data.String.Lemmas

/-! Direct modern String.Slice trimAscii → character-list correspondence.
This deliberately does not use the Batteries Legacy.dropWhile theorem.
All hypotheses below concern a position or one character, never a whole trim.
Source proof draft, not yet elaborated by the sole main worker. -/

namespace Verification.PrefixAsciiTrim

open String
open Verification.PrefixDirectivesModel

private theorem skipPrefix_bool (s : Slice) (p : Char → Bool) :
    s.skipPrefix? p =
      if h : s.startPos ≠ s.endPos then
        if p (s.startPos.get h) then some (s.startPos.next h) else none
      else none := by
  by_cases h : s.startPos ≠ s.endPos
  · rw [dif_pos h]
    by_cases hp : p (s.startPos.get h) = true
    · rw [if_pos hp]
      exact Slice.skipPrefix?_bool_eq_some_iff.mpr ⟨h, rfl, hp⟩
    · rw [if_neg hp]
      cases hm : s.skipPrefix? p with
      | none => rfl
      | some q =>
          obtain ⟨hq, _, hchar⟩ := Slice.skipPrefix?_bool_eq_some_iff.mp hm
          exact False.elim (hp hchar)
  · rw [dif_neg h]
    cases hm : s.skipPrefix? p with
    | none => rfl
    | some q =>
        obtain ⟨hq, _, _⟩ := Slice.skipPrefix?_bool_eq_some_iff.mp hm
        exact False.elim (h hq)

private theorem skipSuffix_bool (s : Slice) (p : Char → Bool) :
    s.skipSuffix? p =
      if h : s.endPos ≠ s.startPos then
        if p ((s.endPos.prev h).get (by simp)) then
          some (s.endPos.prev h) else none
      else none := by
  by_cases h : s.endPos ≠ s.startPos
  · rw [dif_pos h]
    by_cases hp : p ((s.endPos.prev h).get (by simp)) = true
    · rw [if_pos hp]
      exact Slice.skipSuffix?_bool_eq_some_iff.mpr ⟨h, rfl, hp⟩
    · rw [if_neg hp]
      cases hm : s.skipSuffix? p with
      | none => rfl
      | some q =>
          obtain ⟨hq, _, hchar⟩ := Slice.skipSuffix?_bool_eq_some_iff.mp hm
          exact False.elim (hp hchar)
  · rw [dif_neg h]
    cases hm : s.skipSuffix? p with
    | none => rfl
    | some q =>
        obtain ⟨hq, _, _⟩ := Slice.skipSuffix?_bool_eq_some_iff.mp hm
        exact False.elim (h hq)

private theorem skipWhile_step {s : Slice} (pos : s.Pos) (p : Char → Bool) :
    pos.skipWhile p =
      if h : pos ≠ s.endPos then
        if p (pos.get h) then (pos.next h).skipWhile p else pos
      else pos := by
  by_cases h : pos ≠ s.endPos
  · rw [dif_pos h]
    have hn : (s.sliceFrom pos).startPos ≠ (s.sliceFrom pos).endPos := by
      intro he
      apply h
      simpa only [Slice.Pos.ofSliceFrom_startPos, Slice.Pos.ofSliceFrom_endPos]
        using congrArg Slice.Pos.ofSliceFrom he
    have hg : (s.sliceFrom pos).startPos.get hn = pos.get h := by
      simp only [Slice.Pos.get_eq_get_ofSliceFrom, Slice.Pos.ofSliceFrom_startPos]
    have hm : Slice.Pattern.ForwardPattern.skipPrefix? p (s.sliceFrom pos) =
        if p (pos.get h) then some ((s.sliceFrom pos).startPos.next hn) else none := by
      change (s.sliceFrom pos).skipPrefix? p = _
      rw [skipPrefix_bool, dif_pos hn, hg]
    rw [Slice.Pos.skipWhile, hm]
    by_cases hp : p (pos.get h) = true
    · simp only [if_pos hp, Slice.Pos.ofSliceFrom_next,
        Slice.Pos.ofSliceFrom_startPos, Slice.Pos.lt_next, ↓reduceIte]
    · simp only [if_neg hp]
  · rw [dif_neg h]
    have he : pos = s.endPos := by simpa using h
    have hn : (s.sliceFrom pos).startPos = (s.sliceFrom pos).endPos := by
      apply Slice.Pos.ofSliceFrom_inj.mp
      simpa only [Slice.Pos.ofSliceFrom_startPos, Slice.Pos.ofSliceFrom_endPos] using he
    have hm : Slice.Pattern.ForwardPattern.skipPrefix? p (s.sliceFrom pos) = none := by
      change (s.sliceFrom pos).skipPrefix? p = none
      rw [skipPrefix_bool, dif_neg (fun hne => hne hn)]
    rw [Slice.Pos.skipWhile, hm]

private theorem revSkipWhile_step {s : Slice} (pos : s.Pos) (p : Char → Bool) :
    pos.revSkipWhile p =
      if h : pos ≠ s.startPos then
        if p ((pos.prev h).get (by simp)) then
          (pos.prev h).revSkipWhile p else pos
      else pos := by
  by_cases h : pos ≠ s.startPos
  · rw [dif_pos h]
    have hn : (s.sliceTo pos).endPos ≠ (s.sliceTo pos).startPos := by
      intro he
      apply h
      simpa only [Slice.Pos.ofSliceTo_endPos, Slice.Pos.ofSliceTo_startPos]
        using congrArg Slice.Pos.ofSliceTo he
    have hg : ((s.sliceTo pos).endPos.prev hn).get (by simp) =
        (pos.prev h).get (by simp) := by
      simp only [Slice.Pos.get_eq_get_ofSliceTo, Slice.Pos.ofSliceTo_prev,
        Slice.Pos.ofSliceTo_endPos]
    have hm : Slice.Pattern.BackwardPattern.skipSuffix? p (s.sliceTo pos) =
        if p ((pos.prev h).get (by simp)) then
          some ((s.sliceTo pos).endPos.prev hn) else none := by
      change (s.sliceTo pos).skipSuffix? p = _
      rw [skipSuffix_bool, dif_pos hn, hg]
    rw [Slice.Pos.revSkipWhile, hm]
    by_cases hp : p ((pos.prev h).get (by simp)) = true
    · simp only [if_pos hp, Slice.Pos.ofSliceTo_prev,
        Slice.Pos.ofSliceTo_endPos, Slice.Pos.prev_lt, ↓reduceIte]
    · simp only [if_neg hp]
  · rw [dif_neg h]
    have he : pos = s.startPos := by simpa using h
    have hn : (s.sliceTo pos).endPos = (s.sliceTo pos).startPos := by
      apply Slice.Pos.ofSliceTo_inj.mp
      simpa only [Slice.Pos.ofSliceTo_endPos, Slice.Pos.ofSliceTo_startPos] using he
    have hm : Slice.Pattern.BackwardPattern.skipSuffix? p (s.sliceTo pos) = none := by
      change (s.sliceTo pos).skipSuffix? p = none
      rw [skipSuffix_bool, dif_neg (fun hne => hne hn)]
    rw [Slice.Pos.revSkipWhile, hm]

private theorem suffix_cons {s : Slice} (pos : s.Pos) (h : pos ≠ s.endPos) :
    (s.sliceFrom pos).copy.toList =
      pos.get h :: (s.sliceFrom (pos.next h)).copy.toList := by
  simpa only [String.toList_append, String.toList_singleton, List.singleton_append]
    using congrArg String.toList (pos.splits_next_right h).copy_sliceFrom_eq

private theorem prefix_snoc {s : Slice} (pos : s.Pos) (h : pos ≠ s.startPos) :
    (s.sliceTo pos).copy.toList =
      (s.sliceTo (pos.prev h)).copy.toList ++ [(pos.prev h).get (by simp)] := by
  simpa only [String.toList_append, String.toList_singleton]
    using congrArg String.toList (pos.splits_prev_right h).copy_sliceTo_eq

theorem copy_skipWhile {s : Slice} (pos : s.Pos) (p : Char → Bool) :
    (s.sliceFrom (pos.skipWhile p)).copy.toList =
      (s.sliceFrom pos).copy.toList.dropWhile p := by
  rw [skipWhile_step]
  by_cases h : pos ≠ s.endPos
  · rw [dif_pos h]
    rw [suffix_cons pos h, List.dropWhile_cons]
    by_cases hp : p (pos.get h) = true
    · rw [if_pos hp]
      exact copy_skipWhile (pos.next h) p
    · rw [if_neg hp]
      exact suffix_cons pos h
  · rw [dif_neg h]
    have he : pos = s.endPos := by simpa using h
    simp [he]
termination_by (s.sliceFrom pos).copy.toList.length
decreasing_by
  rw [suffix_cons pos h]
  simp

theorem copy_revSkipWhile {s : Slice} (pos : s.Pos) (p : Char → Bool) :
    (s.sliceTo (pos.revSkipWhile p)).copy.toList =
      ((s.sliceTo pos).copy.toList.reverse.dropWhile p).reverse := by
  rw [revSkipWhile_step]
  by_cases h : pos ≠ s.startPos
  · rw [dif_pos h]
    rw [prefix_snoc pos h]
    simp only [List.reverse_append, List.reverse_singleton, List.singleton_append,
      List.dropWhile_cons]
    by_cases hp : p ((pos.prev h).get (by simp)) = true
    · rw [if_pos hp]
      exact copy_revSkipWhile (pos.prev h) p
    · simp only [if_neg hp, List.reverse_cons, List.reverse_reverse]
      exact prefix_snoc pos h
  · rw [dif_neg h]
    have he : pos = s.startPos := by simpa using h
    simp [he]
termination_by (s.sliceTo pos).copy.toList.length
decreasing_by
  rw [prefix_snoc pos h]
  simp

theorem copy_dropWhile (s : Slice) (p : Char → Bool) :
    (s.dropWhile p).copy.toList = s.copy.toList.dropWhile p := by
  simpa only [Slice.dropWhile, Slice.skipPrefixWhile, Slice.sliceFrom_startPos]
    using copy_skipWhile s.startPos p

theorem copy_dropEndWhile (s : Slice) (p : Char → Bool) :
    (s.dropEndWhile p).copy.toList = (s.copy.toList.reverse.dropWhile p).reverse := by
  simpa only [Slice.dropEndWhile, Slice.skipSuffixWhile, Slice.sliceTo_endPos]
    using copy_revSkipWhile s.endPos p

/-- Actual LnmaiCore trim, via modern UTF-8 positions, with no trim equality premise. -/
theorem trim_toList (s : String) :
    (LnmaiCore.Simai.trim s).toList = trimChars s.toList := by
  change ((s.toSlice.dropWhile Char.isWhitespace).dropEndWhile
    Char.isWhitespace).copy.toList = trimChars s.toList
  rw [copy_dropEndWhile, copy_dropWhile, String.copy_toSlice]
  rfl

end Verification.PrefixAsciiTrim
