import Verification.Judge
import Verification.Types

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 1000000
set_option maxErrors 5

namespace Verification.JudgeProof
open Bridge

/-- The percentage table uses exact integer comparisons throughout its
    representable range. This bound also covers the runtime representatives. -/
@[step] theorem press_band_spec (held reality : Std.I64)
    (hh : -2^40 ≤ held.val ∧ held.val ≤ 2^40)
    (hr : -2^40 ≤ reality.val ∧ reality.val ≤ 2^40) :
    judge.press_band_micros held reality ⦃ r => r.val =
      (if held.val ≥ reality.val then 0
       else if held.val * 100 ≥ reality.val * 67 then 1
       else if held.val * 100 ≥ reality.val * 33 then 2
       else if held.val * 100 ≥ reality.val * 5 then 3 else 4) ⦄ := by
  -- Establish the four checked products once, before splitting the bands.
  -- Symbolic execution after every split repeats the same overflow proofs.
  obtain ⟨held100, hheld100, vheld100⟩ := WP.spec_imp_exists
    (IScalar.mul_spec (by scalar_tac) (by scalar_tac) :
      held * (100#i64 : Std.I64) ⦃ r => r.val = held.val * 100 ⦄)
  obtain ⟨reality67, hreality67, vreality67⟩ := WP.spec_imp_exists
    (IScalar.mul_spec (by scalar_tac) (by scalar_tac) :
      reality * (67#i64 : Std.I64) ⦃ r => r.val = reality.val * 67 ⦄)
  obtain ⟨reality33, hreality33, vreality33⟩ := WP.spec_imp_exists
    (IScalar.mul_spec (by scalar_tac) (by scalar_tac) :
      reality * (33#i64 : Std.I64) ⦃ r => r.val = reality.val * 33 ⦄)
  obtain ⟨reality5, hreality5, vreality5⟩ := WP.spec_imp_exists
    (IScalar.mul_spec (by scalar_tac) (by scalar_tac) :
      reality * (5#i64 : Std.I64) ⦃ r => r.val = reality.val * 5 ⦄)
  simp only [judge.press_band_micros, hheld100, hreality67, hreality33, hreality5,
    bind_tc_ok, i64_le_iff, vheld100, vreality67, vreality33, vreality5,
    IScalar.ofInt_val_eq]
  split_ifs <;> simp

def classicHoldCore (head : LnmaiCore.JudgeGrade) (diff : ℤ) : LnmaiCore.JudgeGrade :=
  if head.isMissOrTooFast then head else
  let absolute := if diff < 0 then -diff else diff
  let tail := if diff > 0 then
      (if absolute < 150003 then .Perfect else .FastGood)
    else (if absolute < 200004 then .Perfect else .LateGood)
  if tail.distFromPerfect > head.distFromPerfect then tail else head

theorem classic_hold_expand (head : LnmaiCore.JudgeGrade) (timing length release : ℤ) :
    LnmaiCore.Judge.judgeHoldClassicEnd head (LnmaiCore.TimePoint.fromMicros timing)
      (LnmaiCore.Duration.fromMicros length) (LnmaiCore.TimePoint.fromMicros release) =
    classicHoldCore head (timing + length - release) := by
  unfold LnmaiCore.Judge.judgeHoldClassicEnd classicHoldCore
  change (if head.isMissOrTooFast then head else
    let diff := timing + length - release
    let absolute := (LnmaiCore.Duration.abs (LnmaiCore.Duration.fromMicros diff)).toMicros
    let tail := if diff > 0 then
      (if absolute < 150003 then .Perfect else .FastGood)
      else (if absolute < 200004 then .Perfect else .LateGood)
    if tail.distFromPerfect > head.distFromPerfect then tail else head) = _
  dsimp only
  simp only [duration_abs_fromMicros]
  rfl

@[step] theorem grade_distance_spec (g : types.JudgeGrade) :
    types.JudgeGrade.dist_from_perfect g ⦃ r =>
      r.val = (toLnmJudgeGrade g).distFromPerfect ⦄ := by
  exact WP.exists_imp_spec (TypesProof.judgeGrade_dist_from_perfect_equiv g)

/-- Classic tail timing, including the choice of the worse head/tail grade.
    The bounds exclude machine overflow in the extracted kernel. -/
theorem judge_hold_classic_end_equiv (head : types.JudgeGrade)
    (timing release : Std.I64) (length : time.Duration)
    (ht : -2^40 ≤ timing.val ∧ timing.val ≤ 2^40)
    (hr : -2^40 ≤ release.val ∧ release.val ≤ 2^40)
    (hl : -2^40 ≤ length.micros.val ∧ length.micros.val ≤ 2^40) :
    judge.judge_hold_classic_end head timing length release =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeHoldClassicEnd (toLnmJudgeGrade head)
        (LnmaiCore.TimePoint.fromMicros timing.val) (toLnmDuration length)
        (LnmaiCore.TimePoint.fromMicros release.val))) := by
  obtain ⟨sum, hsum, hsumval⟩ := i64_add_eq timing length.micros
    (by scalar_tac) (by scalar_tac)
  have hsub : sum - release ⦃ r => r.val = sum.val - release.val ⦄ :=
    IScalar.sub_spec (by scalar_tac) (by scalar_tac)
  obtain ⟨diff, hdiff, hdiffval⟩ := WP.spec_imp_exists hsub
  obtain ⟨absolute, habs, habsval⟩ := abs_eq ⟨diff⟩ (by scalar_tac)
  simp only [toLnmDuration, classic_hold_expand]
  cases head <;>
    simp only [judge.judge_hold_classic_end, types.JudgeGrade.is_miss_or_too_fast,
      bind_tc_ok, Bool.true_eq, Bool.false_eq_true, if_true, if_false,
      hsum, hdiff, habs]
  all_goals
    simp only [classicHoldCore, toLnmJudgeGrade, LnmaiCore.JudgeGrade.isMissOrTooFast,
      Bool.true_eq, Bool.false_eq_true, if_true, if_false,
      constants.HOLD_CLASSIC_END_JUDGE_PERFECT_FAST,
      constants.HOLD_CLASSIC_END_JUDGE_PERFECT_LATE,
      i64_lt_iff, IScalar.ofInt_val_eq, habsval, hdiffval, hsumval]
    (try split_ifs) <;> simp_all [bind_tc_ok, types.JudgeGrade.dist_from_perfect,
      LnmaiCore.JudgeGrade.distFromPerfect, ofLnmJudgeGrade]

end Verification.JudgeProof
