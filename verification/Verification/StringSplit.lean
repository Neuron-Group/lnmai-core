import Batteries.Data.String.Lemmas
import Lean.Elab.Tactic.Omega

/-! Bridge from the original parser's legacy string splitter to character lists.
The proof follows the installed UTF-8 implementation, including empty fields. -/

namespace Verification.StringSplit
open String

private theorem singleton_get (c : Char) :
    (0 : Pos.Raw).get (String.ofList [c]) = c := by
  simpa using String.get_of_valid [] [c]

private theorem singleton_next_end (c : Char) :
    ((0 : Pos.Raw).next (String.ofList [c])).atEnd (String.ofList [c]) = true := by
  rw [Pos.Raw.next, singleton_get]
  simp [String.atEnd_iff, String.rawEndPos_ofList,
    Pos.Raw.le_iff, Pos.Raw.byteIdx_add_char]

theorem splitOnAux_singleton (s : String) (c : Char) (b i : Pos.Raw)
    (acc : List String) :
    String.splitOnAux s (String.ofList [c]) b i 0 acc =
      String.splitAux s (fun x => x == c) b i acc := by
  rw [String.splitOnAux, String.splitAux]
  by_cases hend : i.atEnd s = true
  · simp only [hend, ↓reduceIte, ↓reduceDIte]
  · simp only [hend, ↓reduceIte, ↓reduceDIte, singleton_get]
    by_cases hmatch : (i.get s == c) = true
    · have hback : (i.next s).unoffsetBy ((0 : Pos.Raw).next (String.ofList [c])) = i := by
        apply Pos.Raw.ext
        simp only [Pos.Raw.byteIdx_unoffsetBy, Pos.Raw.next, singleton_get,
          Pos.Raw.byteIdx_add_char, Pos.Raw.byteIdx_zero, Nat.zero_add]
        rw [(beq_iff_eq ..).1 hmatch, Nat.add_sub_cancel]
      simp only [hmatch, ↓reduceIte, singleton_next_end, hback]
      exact splitOnAux_singleton s c (i.next s) (i.next s) (b.extract s i :: acc)
    · simp only [hmatch, ↓reduceIte, Pos.Raw.unoffsetBy_zero]
      exact splitOnAux_singleton s c b (i.next s) acc
termination_by s.rawEndPos.byteIdx - i.byteIdx
decreasing_by
  all_goals
    exact Nat.sub_lt_sub_left (Nat.gt_of_not_le (mt decide_eq_true hend))
      (Pos.Raw.lt_next s i)

theorem splitOn_singleton (s : String) (c : Char) :
    s.splitOn (String.ofList [c]) =
      (List.splitOnP (fun x => x == c) s.toList).map String.ofList := by
  have hnonempty : String.ofList [c] ≠ "" := by
    intro h
    have := congrArg String.toList h
    simp at this
  simp only [String.splitOn, beq_iff_eq, hnonempty, ↓reduceIte, splitOnAux_singleton]
  exact String.splitToList_of_valid s (fun x => x == c)

end Verification.StringSplit
