-- M1d: Convert equivalence.
import Verification.Bridge

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace ConvertProof

open Bridge

theorem convert_maji_equiv (g : types.JudgeGrade) :
    convert.convert_maji g =
      ok (ofLnmJudgeGrade (LnmaiCore.Convert.convertMaji (toLnmJudgeGrade g))) := by
  cases g <;> rfl

theorem convert_gachi_equiv (g : types.JudgeGrade) :
    convert.convert_gachi g =
      ok (ofLnmJudgeGrade (LnmaiCore.Convert.convertGachi (toLnmJudgeGrade g))) := by
  cases g <;> rfl

theorem convert_gori_equiv (g : types.JudgeGrade) :
    convert.convert_gori g =
      ok (ofLnmJudgeGrade (LnmaiCore.Convert.convertGori (toLnmJudgeGrade g))) := by
  cases g <;> rfl

theorem convert_grade_equiv (s : types.JudgeStyle) (g : types.JudgeGrade) :
    convert.convert_grade s g =
      ok (ofLnmJudgeGrade
        (LnmaiCore.Convert.convertGrade (toLnmJudgeStyle s) (toLnmJudgeGrade g))) := by
  cases s <;> cases g <;> rfl

end ConvertProof

end Verification
