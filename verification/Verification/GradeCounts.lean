import Verification.Bridge

namespace Verification.GradeCountsProof
open Aeneas Aeneas.Std Result lnmai_core_verify Bridge
open private LnmaiCore.judgeCountsGrades from LnmaiCore.Types

/- Unlike the arithmetic model, these read/write theorems refer to the actual
   extracted functions used by runtime_score.rs. Fixed array length discharges
   every indexing bound. The element type is opaque and needs no Clone law. -/

def gradeIndex : LnmaiCore.JudgeGrade → Usize
  | .Miss => 0#usize
  | .LateGood => 1#usize
  | .LateGreat3rd => 2#usize
  | .LateGreat2nd => 3#usize
  | .LateGreat => 4#usize
  | .LatePerfect3rd => 5#usize
  | .LatePerfect2nd => 6#usize
  | .Perfect => 7#usize
  | .FastPerfect2nd => 8#usize
  | .FastPerfect3rd => 9#usize
  | .FastGreat => 10#usize
  | .FastGreat2nd => 11#usize
  | .FastGreat3rd => 12#usize
  | .FastGood => 13#usize
  | .TooFast => 14#usize

theorem grade_index_equiv (grade : types.JudgeGrade) :
    grade_counts.grade_index grade = ok (gradeIndex (toLnmJudgeGrade grade)) := by
  cases grade <;> rfl

theorem grades_equiv :
    grade_counts.JUDGE_GRADES.val.map toLnmJudgeGrade = LnmaiCore.judgeCountsGrades := by
  unfold grade_counts.JUDGE_GRADES
  rfl

theorem grade_index_bound (grade : LnmaiCore.JudgeGrade) :
    (gradeIndex grade).val < 15 := by cases grade <;> decide

theorem grade_index_eq_iff (a b : LnmaiCore.JudgeGrade) :
    (gradeIndex a).val = (gradeIndex b).val ↔ a = b := by
  cases a <;> cases b <;> decide

theorem grade_beq_iff (a b : LnmaiCore.JudgeGrade) :
    (a == b) = true ↔ a = b := by
  cases a <;> cases b <;> decide

def read {N : Type} (counts : Std.Array N 15#usize) (grade : LnmaiCore.JudgeGrade) : N :=
  counts.val[(gradeIndex grade).val]'(by
    rw [counts.property]; exact grade_index_bound grade)

theorem index_read {N : Type} (counts : Std.Array N 15#usize)
    (grade : LnmaiCore.JudgeGrade) :
    Std.Array.index_usize counts (gradeIndex grade) = ok (read counts grade) := by
  have hb : (gradeIndex grade).val < counts.val.length := by
    rw [counts.property]; exact grade_index_bound grade
  simp only [Std.Array.index_usize, Std.Array.getElem?_Usize_eq,
    List.getElem?_eq_getElem hb, read]

theorem get_count_equiv {N : Type} (counts : Std.Array N 15#usize)
    (grade : types.JudgeGrade) :
    grade_counts.get_count counts grade = ok (read counts (toLnmJudgeGrade grade)) := by
  simp only [grade_counts.get_count, grade_index_equiv, bind_tc_ok, index_read]

theorem set_count_equiv {N : Type} (counts : Std.Array N 15#usize)
    (grade : types.JudgeGrade) (value : N) :
    grade_counts.set_count counts grade value =
      ok (counts.set (gradeIndex (toLnmJudgeGrade grade)) value) := by
  have hb : (gradeIndex (toLnmJudgeGrade grade)).val < counts.val.length := by
    rw [counts.property]; exact grade_index_bound (toLnmJudgeGrade grade)
  have hassert : massert (gradeIndex (toLnmJudgeGrade grade) < 15#usize) = ok () := by
    simp [massert, UScalar.lt_equiv, grade_index_bound]
  have hrestore : counts.set (gradeIndex (toLnmJudgeGrade grade))
      (read counts (toLnmJudgeGrade grade)) = counts := by
    apply Subtype.ext
    exact List.set_getElem_self hb
  simp only [grade_counts.set_count, grade_index_equiv, hassert, bind_tc_ok,
    Std.Array.index_mut_usize, index_read, hrestore]
  change Std.Array.update
    (counts.set (gradeIndex (toLnmJudgeGrade grade)) (read counts (toLnmJudgeGrade grade)))
    (gradeIndex (toLnmJudgeGrade grade)) value = _
  rw [hrestore]
  simp only [Std.Array.update, Std.Array.getElem?_Usize_eq, List.getElem?_eq_getElem hb,
    Std.Array.set]

def decode {N : Type} (natural : N → Nat) (counts : Std.Array N 15#usize) :
    LnmaiCore.JudgeCounts := fun grade => natural (read counts grade)

theorem decode_set {N : Type} (natural : N → Nat)
    (counts : Std.Array N 15#usize) (grade : LnmaiCore.JudgeGrade) (value : N) :
    decode natural (counts.set (gradeIndex grade) value) =
      (fun g => if g == grade then natural value else decode natural counts g) := by
  funext g
  simp only [decode, read, Std.Array.set_val_eq, List.getElem_set]
  by_cases h : g = grade
  · subst g
    simp [grade_beq_iff]
  · have hi : (gradeIndex grade).val ≠ (gradeIndex g).val :=
      fun hi => h ((grade_index_eq_iff grade g).mp hi).symm
    simp [hi, h, grade_beq_iff]

def bump {N : Type} (add : N → N → N) (counts : Std.Array N 15#usize)
    (grade : types.JudgeGrade) (multiple : N) : Result (Std.Array N 15#usize) := do
  let old ← grade_counts.get_count counts grade
  grade_counts.set_count counts grade (add old multiple)

theorem bump_equiv {N : Type} (natural : N → Nat) (add : N → N → N)
    (hadd : ∀ a b, natural (add a b) = natural a + natural b)
    (counts : Std.Array N 15#usize) (grade : types.JudgeGrade) (multiple : N) :
    (do let next ← bump add counts grade multiple; ok (decode natural next)) =
      ok (fun g => if g == toLnmJudgeGrade grade then
        decode natural counts g + natural multiple else decode natural counts g) := by
  simp only [bump, get_count_equiv, bind_tc_ok, set_count_equiv, decode_set, hadd]
  congr 1
  funext g
  by_cases h : g = toLnmJudgeGrade grade
  · subst g; rfl
  · simp [h, grade_beq_iff]

end Verification.GradeCountsProof
