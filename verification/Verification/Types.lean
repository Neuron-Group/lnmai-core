-- M1c: core type method equivalence.
import Verification.Bridge

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace TypesProof

open Bridge

theorem judgeGrade_is_miss_or_too_fast_equiv (g : types.JudgeGrade) :
    types.JudgeGrade.is_miss_or_too_fast g =
      ok (LnmaiCore.JudgeGrade.isMissOrTooFast (toLnmJudgeGrade g)) := by
  cases g <;> rfl

theorem judgeGrade_is_fast_equiv (g : types.JudgeGrade) :
    types.JudgeGrade.is_fast g =
      ok (LnmaiCore.JudgeGrade.isFast (toLnmJudgeGrade g)) := by
  cases g <;> rfl

theorem judgeGrade_is_late_equiv (g : types.JudgeGrade) :
    types.JudgeGrade.is_late g =
      ok (LnmaiCore.JudgeGrade.isLate (toLnmJudgeGrade g)) := by
  cases g <;> rfl

theorem judgeGrade_is_perfect_grade_equiv (g : types.JudgeGrade) :
    types.JudgeGrade.is_perfect_grade g =
      ok (LnmaiCore.JudgeGrade.isPerfectGrade (toLnmJudgeGrade g)) := by
  cases g <;> rfl

theorem judgeGrade_is_great_grade_equiv (g : types.JudgeGrade) :
    types.JudgeGrade.is_great_grade g =
      ok (LnmaiCore.JudgeGrade.isGreatGrade (toLnmJudgeGrade g)) := by
  cases g <;> rfl

theorem judgeGrade_is_good_grade_equiv (g : types.JudgeGrade) :
    types.JudgeGrade.is_good_grade g =
      ok (LnmaiCore.JudgeGrade.isGoodGrade (toLnmJudgeGrade g)) := by
  cases g <;> rfl

theorem judgeGrade_dist_from_perfect_equiv (g : types.JudgeGrade) :
    ∃ v : Std.U32, types.JudgeGrade.dist_from_perfect g = ok v ∧
      v.val = LnmaiCore.JudgeGrade.distFromPerfect (toLnmJudgeGrade g) := by
  cases g <;> exact ⟨_, rfl, rfl⟩

theorem noteType_base_score_equiv (n : types.NoteType) :
    ∃ v : Std.U32, types.NoteType.base_score n = ok v ∧
      v.val = LnmaiCore.NoteType.baseScore (toLnmNoteType n) := by
  cases n <;> exact ⟨_, rfl, rfl⟩

theorem noteType_extra_score_equiv (n : types.NoteType) :
    ∃ v : Std.U32, types.NoteType.extra_score n = ok v ∧
      v.val = LnmaiCore.NoteType.extraScore (toLnmNoteType n) := by
  cases n <;> exact ⟨_, rfl, rfl⟩

theorem runtimePos_button_zone_equiv (p : types.RuntimePos) :
    types.RuntimePos.button_zone p =
      ok ((LnmaiCore.RuntimePos.buttonZone? (toLnmRuntimePos p)).map ofLnmButtonZone) := by
  cases p <;> simp [types.RuntimePos.button_zone, LnmaiCore.RuntimePos.buttonZone?,
                    toLnmRuntimePos]

theorem runtimePos_sensor_area_equiv (p : types.RuntimePos) :
    types.RuntimePos.sensor_area p =
      ok ((LnmaiCore.RuntimePos.sensorArea? (toLnmRuntimePos p)).map ofLnmSensorArea) := by
  cases p <;> simp [types.RuntimePos.sensor_area, LnmaiCore.RuntimePos.sensorArea?,
                    toLnmRuntimePos]

end TypesProof

end Verification
