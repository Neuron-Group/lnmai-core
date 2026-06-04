import Verification.Generated
import Verification.Iso
import LnmaiCore.Convert
import LnmaiCore.Judge
import LnmaiCore.Score
import LnmaiCore.Types
import Aeneas

open Aeneas Aeneas.Std
open aeneas_core_verify

namespace Verification.Equiv

open Verification.Iso

----------------------------------------------------------------------------
-- Convert module equivalence
-- All convert functions are purely structural → proven by case analysis
----------------------------------------------------------------------------

theorem convertMaji_equiv (g : types.JudgeGrade) :
    convert.convert_maji g = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Convert.convertMaji (toLnmJudgeGrade g))) := by
  cases g <;> rfl

theorem convertGachi_equiv (g : types.JudgeGrade) :
    convert.convert_gachi g = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Convert.convertGachi (toLnmJudgeGrade g))) := by
  cases g <;> rfl

theorem convertGori_equiv (g : types.JudgeGrade) :
    convert.convert_gori g = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Convert.convertGori (toLnmJudgeGrade g))) := by
  cases g <;> rfl

theorem convertGrade_equiv (style : types.JudgeStyle) (g : types.JudgeGrade) :
    convert.convert_grade style g = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Convert.convertGrade (toLnmJudgeStyle style) (toLnmJudgeGrade g))) := by
  cases style <;> cases g <;> rfl

----------------------------------------------------------------------------
-- Judge module equivalence
----------------------------------------------------------------------------

theorem correctSlideGrade_equiv (g : types.JudgeGrade) :
    judge.correct_slide_grade g = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.correctSlideGrade (toLnmJudgeGrade g))) := by
  cases g <;> rfl

-- The following involve Duration (Std.I64 bounded integers) and are
-- axiomatized until the Std.I64 ↔ ℤ relationship is fully characterized.

theorem judgeTap_equiv (diff : time.Duration) (is_ex : Bool) :
    judge.judge_tap diff is_ex = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.judgeTap (toLnmDuration diff) is_ex)) := by
  sorry

theorem judgeTouch_equiv (diff : time.Duration) (is_ex : Bool) :
    judge.judge_touch diff is_ex =
    Result.ok (Option.map ofLnmJudgeGrade (LnmaiCore.Judge.judgeTouch (toLnmDuration diff) is_ex)) := by
  sorry

theorem judgeSlideClassic_equiv (diff : time.Duration) :
    judge.judge_slide_classic diff = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.judgeSlideClassic (toLnmDuration diff))) := by
  sorry

theorem judgeSlideTooLate_equiv (q_rem : U32) :
    judge.judge_slide_too_late q_rem = Result.ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.judgeSlideTooLate q_rem.val)) := by
  sorry

----------------------------------------------------------------------------
-- Score module equivalence
-- Score functions involve Std.U32 ↔ Nat conversion → axiomatized
----------------------------------------------------------------------------

theorem baseScore_equiv (nt : types.NoteType) :
    score.base_score nt = Result.ok (U32.ofNat (LnmaiCore.Score.baseScore (toLnmNoteType nt)) (by sorry)) := by
  sorry

theorem scoreNonBreak_equiv (base : U32) (grade : types.JudgeGrade) (multiple : U32) :
    score.score_non_break base grade multiple = Result.ok
    (let (earned, lost) := LnmaiCore.Score.scoreNonBreak
      base.val (toLnmJudgeGrade grade) multiple.val
    (U32.ofNat earned (by sorry), U32.ofNat lost (by sorry))) := by
  sorry

end Verification.Equiv
