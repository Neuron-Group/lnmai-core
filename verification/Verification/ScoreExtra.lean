import Verification.Score
import Verification.Types
import Verification.Judge

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.ScoreProof
open Bridge JudgeProof

/-- Every component of the break-score tuple agrees, when the largest
    multiplication fits the Rust result type. -/
theorem score_break_equiv (grade : types.JudgeGrade) (multiple : Std.U32)
    (h : 2500 * multiple.val < 2^32) :
    score.score_break grade multiple ⦃ r =>
      (r.1.val, r.2.1.val, r.2.2.1.val, r.2.2.2.1.val,
        r.2.2.2.2.1.val, r.2.2.2.2.2.val) =
      LnmaiCore.Score.scoreBreak (toLnmJudgeGrade grade) multiple.val ⦄ := by
  generalize hg : toLnmJudgeGrade grade = g
  cases grade <;>
    simp only [toLnmJudgeGrade] at hg <;>
    rw [← hg] <;>
    simp only [score.score_break, LnmaiCore.Score.scoreBreak] <;>
    step* <;> simp_all <;> omega

/-- Fast/late flags agree for every representable duration, including zero
    and the minimum signed integer. No absolute value is computed here. -/
theorem count_fast_late_equiv (grade : types.JudgeGrade)
    (diff : time.Duration) (display : types.JudgeDisplayOption) :
    score.count_fast_late grade diff display =
      ok (LnmaiCore.Score.countFastLate (toLnmJudgeGrade grade)
        (toLnmDuration diff) (toLnmDisplayOption display)) := by
  cases grade <;> cases display <;>
    simp [score.count_fast_late, types.JudgeGrade.is_miss_or_too_fast,
      types.JudgeGrade.dist_from_perfect, time.Duration.to_micros,
      LnmaiCore.Score.countFastLate, LnmaiCore.JudgeGrade.isMissOrTooFast,
      LnmaiCore.JudgeGrade.distFromPerfect, toLnmJudgeGrade, toLnmDisplayOption,
      toLnmDuration, LnmaiCore.Duration.zero, LnmaiCore.Duration.fromMicros,
      LnmaiCore.Duration.ofInt, LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt,
      LnmaiCore.Duration.toInt, IScalar.eq_equiv, i64_lt_iff, ok_ite, LnmaiCore.Duration.toMicros,
      LnmaiCore.instBEqDuration, LnmaiCore.instBEqTimeTick, BEq.beq] <;>
    change (if _ then _ else _) = (if decide (_ = _) then _ else _) <;> simp

@[step] theorem rank_meets_spec (a m : Std.U32) (t : Std.U64) (ht : t.val ≤ 100) :
    score.dx_score_rank.closure.Insts.CoreOpsFunctionFnTupleU64Bool.call (a, m) t
      ⦃ r => r = decide (a.val * 100 ≥ m.val * t.val) ⦄ := by
  unfold score.dx_score_rank.closure.Insts.CoreOpsFunctionFnTupleU64Bool.call
  step* <;> scalar_tac

/-- Widening the intermediate products makes the rank total on all u32 inputs. -/
theorem dx_score_rank_equiv (achieved max : Std.U32) :
    score.dx_score_rank achieved max ⦃ r =>
      r.val = LnmaiCore.Score.dxScoreRank achieved.val max.val ⦄ := by
  unfold score.dx_score_rank LnmaiCore.Score.dxScoreRank
  step* <;> simp_all <;> split_ifs <;> scalar_tac

end Verification.ScoreProof
