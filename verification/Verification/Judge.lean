-- M1d: Judge equivalence.
--
-- NOTE: proof development is intentionally conservative here. Every module in
-- this package imports `LnmaiCore.*`, which pulls in all of Mathlib, so each
-- recompile is expensive. The arithmetic-window functions are added in small,
-- independently checked steps rather than one large `step`-heavy file.
import Verification.Bridge

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace JudgeProof

open Bridge

theorem correct_slide_grade_equiv (g : types.JudgeGrade) :
    judge.correct_slide_grade g =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.correctSlideGrade (toLnmJudgeGrade g))) := by
  cases g <;> rfl

/-- Value spec for `time.Duration.abs`, avoiding the `i64::MIN` overflow point.
The result's microsecond value is the mathematical absolute value. -/
theorem abs_spec (d : time.Duration) (h : d.micros ≠ IScalar.min .I64) :
    time.Duration.abs d ⦃ r => r.micros.val =
      (if d.micros.val < 0 then -d.micros.val else d.micros.val) ⦄ := by
  unfold time.Duration.abs
  by_cases hlt : d.micros < (0#i64 : Std.I64)
  · rw [if_pos hlt]
    step <;> simp_all
  · rw [if_neg hlt]
    rw [Aeneas.Std.WP.spec_ok]
    simp_all

/-- Register the abs value spec with the Aeneas `step` tactic so it can
symbolically execute `time.Duration.abs` inside other functions. -/
@[step]
theorem time_duration_abs_step (d : time.Duration) (h : d.micros ≠ IScalar.min .I64) :
    time.Duration.abs d ⦃ r => r.micros.val =
      (if d.micros.val < 0 then -d.micros.val else d.micros.val) ⦄ :=
  abs_spec d h

/-- Equality form of the abs spec: the result is `ok` (never `fail`/`div`) and
its microsecond value is the mathematical absolute value. -/
theorem abs_eq (d : time.Duration) (h : d.micros ≠ IScalar.min .I64) :
    ∃ v : Std.I64, time.Duration.abs d = ok ⟨v⟩ ∧
      v.val = (if d.micros.val < 0 then -d.micros.val else d.micros.val) := by
  have hs : (time.Duration.abs d) ⦃ r => r.micros.val =
      (if d.micros.val < 0 then -d.micros.val else d.micros.val) ⦄ := abs_spec d h
  cases hf : time.Duration.abs d with
  | ok x =>
      refine ⟨x.micros, ?_, ?_⟩
      · rfl
      · have := hs
        rw [hf, Aeneas.Std.WP.spec_ok] at this
        exact this
  | fail e => simp [hf, Aeneas.Std.WP.spec_fail] at hs
  | div => simp [hf, Aeneas.Std.WP.spec_div] at hs

/-- Shared ℤ-level tap judgment: the common branch structure of the Rust model
and the Lean spec. -/
def judgeTapCore (dm : ℤ) (is_ex : Bool) : LnmaiCore.JudgeGrade :=
  if is_ex then .Perfect
  else
    let isFast := dm < 0
    let a := if dm < 0 then -dm else dm
    if a ≤ 16667 then .Perfect
    else if a ≤ 33334 then (if isFast then .FastPerfect2nd else .LatePerfect2nd)
    else if a ≤ 50001 then (if isFast then .FastPerfect3rd else .LatePerfect3rd)
    else if a ≤ 66668 then (if isFast then .FastGreat else .LateGreat)
    else if a ≤ 83335 then (if isFast then .FastGreat2nd else .LateGreat2nd)
    else if a ≤ 100002 then (if isFast then .FastGreat3rd else .LateGreat3rd)
    else (if isFast then .FastGood else .LateGood)

@[simp] theorem tapPerfect1Ms_val : LnmaiCore.Constants.tapPerfect1Ms.toMicros = 16667 := by decide
@[simp] theorem tapPerfect2Ms_val : LnmaiCore.Constants.tapPerfect2Ms.toMicros = 33334 := by decide
@[simp] theorem tapPerfect3Ms_val : LnmaiCore.Constants.tapPerfect3Ms.toMicros = 50001 := by decide
@[simp] theorem tapGreat1Ms_val : LnmaiCore.Constants.tapGreat1Ms.toMicros = 66668 := by decide
@[simp] theorem tapGreat2Ms_val : LnmaiCore.Constants.tapGreat2Ms.toMicros = 83335 := by decide
@[simp] theorem tapGreat3Ms_val : LnmaiCore.Constants.tapGreat3Ms.toMicros = 100002 := by decide

/-- `Duration.abs` of a microsecond count, as a microsecond count. -/
theorem duration_abs_fromMicros (m : ℤ) :
    LnmaiCore.Duration.abs (LnmaiCore.Duration.fromMicros m) =
      LnmaiCore.Duration.fromMicros (if m < 0 then -m else m) := by
  simp [LnmaiCore.Duration.abs, LnmaiCore.Duration.fromMicros, LnmaiCore.Duration.zero,
    LnmaiCore.Duration.ofInt, LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt,
    LnmaiCore.Duration.toInt, LnmaiCore.TimeTick.toInt, LnmaiCore.Duration.toTick]
  split <;> rfl

end JudgeProof

end Verification
