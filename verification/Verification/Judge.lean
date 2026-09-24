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

/-! ## Normalization lemmas for the windowed judge proofs

These turn model `I64` comparisons and spec `Duration` comparisons into plain
`ℤ` comparisons on µs values, and expose the model constants numerically. -/

@[simp] theorem i64_le_iff (a b : Std.I64) : (a ≤ b) ↔ a.val ≤ b.val := Iff.rfl
@[simp] theorem i64_lt_iff (a b : Std.I64) : (a < b) ↔ a.val < b.val := Iff.rfl
@[simp] theorem i64_zero_val : (0#i64 : Std.I64).val = (0 : ℤ) := by simp

@[simp] theorem tapP1 : constants.TAP_PERFECT_1ST.micros.val = (16667 : ℤ) := by
  simp [constants.TAP_PERFECT_1ST]
@[simp] theorem tapP2 : constants.TAP_PERFECT_2ND.micros.val = (33334 : ℤ) := by
  simp [constants.TAP_PERFECT_2ND]
@[simp] theorem tapP3 : constants.TAP_PERFECT_3RD.micros.val = (50001 : ℤ) := by
  simp [constants.TAP_PERFECT_3RD]
@[simp] theorem tapG1 : constants.TAP_GREAT_1ST.micros.val = (66668 : ℤ) := by
  simp [constants.TAP_GREAT_1ST]
@[simp] theorem tapG2 : constants.TAP_GREAT_2ND.micros.val = (83335 : ℤ) := by
  simp [constants.TAP_GREAT_2ND]
@[simp] theorem tapG3 : constants.TAP_GREAT_3RD.micros.val = (100002 : ℤ) := by
  simp [constants.TAP_GREAT_3RD]

@[simp] theorem lnmDuration_le_iff_toMicros (a b : LnmaiCore.Duration) :
    (a ≤ b) ↔ a.toMicros ≤ b.toMicros :=
  (LnmaiCore.Duration.toMicros_le_toMicros a b).symm

@[simp] theorem lnmDuration_lt_iff_toMicros (a b : LnmaiCore.Duration) :
    (a < b) ↔ a.toMicros < b.toMicros :=
  (LnmaiCore.Duration.toMicros_lt_toMicros a b).symm

/-- `Duration.abs` of the bridge duration, as a µs value. -/
theorem duration_abs_toMicros (d : time.Duration) :
    (LnmaiCore.Duration.abs (toLnmDuration d)).toMicros =
      (if d.micros.val < 0 then -d.micros.val else d.micros.val) := by
  by_cases h : d.micros.val < 0 <;>
    simp [LnmaiCore.Duration.abs, toLnmDuration, LnmaiCore.Duration.fromMicros,
      LnmaiCore.Duration.toMicros, LnmaiCore.Duration.toInt, LnmaiCore.Duration.ofInt,
      LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt, h]

/-- The bridge duration is negative exactly when the model µs value is. -/
theorem toLnmDuration_lt_zero (d : time.Duration) :
    (toLnmDuration d < LnmaiCore.Duration.zero) ↔ d.micros.val < 0 := by
  rw [← LnmaiCore.Duration.toMicros_lt_toMicros]
  simp [toLnmDuration, LnmaiCore.Duration.zero, LnmaiCore.Duration.fromMicros,
    LnmaiCore.Duration.toMicros, LnmaiCore.Duration.toInt, LnmaiCore.Duration.ofInt,
    LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt]

/-- `Result.ok` distributes over `if`. -/
theorem ok_ite {α : Type} (c : Prop) [Decidable c] (a b : α) :
    ok (if c then a else b) = (if c then ok a else ok b) := by
  split <;> rfl

/-- Explicit expansion of the spec `judgeTap`: it is definitionally equal to
the spec body with the private `absDiff` helper unfolded. Written out so proofs
can refer to `LnmaiCore.Duration.abs` directly. -/
theorem judgeTap_expand (d : LnmaiCore.Duration) (is_ex : Bool) :
    LnmaiCore.Judge.judgeTap d is_ex =
      (if is_ex then LnmaiCore.JudgeGrade.Perfect
       else
        let isFast := d < LnmaiCore.Duration.zero
        let diffMSec := LnmaiCore.Duration.abs d
        if diffMSec ≤ LnmaiCore.Constants.tapPerfect1Ms then LnmaiCore.JudgeGrade.Perfect
        else if diffMSec ≤ LnmaiCore.Constants.tapPerfect2Ms then
          (if isFast then LnmaiCore.JudgeGrade.FastPerfect2nd else LnmaiCore.JudgeGrade.LatePerfect2nd)
        else if diffMSec ≤ LnmaiCore.Constants.tapPerfect3Ms then
          (if isFast then LnmaiCore.JudgeGrade.FastPerfect3rd else LnmaiCore.JudgeGrade.LatePerfect3rd)
        else if diffMSec ≤ LnmaiCore.Constants.tapGreat1Ms then
          (if isFast then LnmaiCore.JudgeGrade.FastGreat else LnmaiCore.JudgeGrade.LateGreat)
        else if diffMSec ≤ LnmaiCore.Constants.tapGreat2Ms then
          (if isFast then LnmaiCore.JudgeGrade.FastGreat2nd else LnmaiCore.JudgeGrade.LateGreat2nd)
        else if diffMSec ≤ LnmaiCore.Constants.tapGreat3Ms then
          (if isFast then LnmaiCore.JudgeGrade.FastGreat3rd else LnmaiCore.JudgeGrade.LateGreat3rd)
        else (if isFast then LnmaiCore.JudgeGrade.FastGood else LnmaiCore.JudgeGrade.LateGood)) := by
  simp only [LnmaiCore.Judge.judgeTap]
  rfl

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

/-- M1d: `judge_tap` equivalence. The windowed tap judgment matches the spec,
assuming the diff is not `i64::MIN` (the only point where `Duration.abs`
overflows). -/
theorem judge_tap_equiv (diff : time.Duration) (is_ex : Bool)
    (hmin : diff.micros ≠ IScalar.min .I64) :
    judge.judge_tap diff is_ex =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeTap (toLnmDuration diff) is_ex)) := by
  obtain ⟨v, hv, hvv⟩ := abs_eq diff hmin
  rw [judgeTap_expand]
  cases is_ex
  · simp only [judge.judge_tap]
    rw [hv]
    simp only [bind_tc_ok, Bool.false_eq_true, if_false]
    simp only [duration_abs_toMicros, tapPerfect1Ms_val, tapPerfect2Ms_val,
      tapPerfect3Ms_val, tapGreat1Ms_val, tapGreat2Ms_val, tapGreat3Ms_val,
      lnmDuration_le_iff_toMicros, i64_le_iff, i64_lt_iff, i64_zero_val,
      toLnmDuration_lt_zero, tapP1, tapP2, tapP3, tapG1, tapG2, tapG3]
    rw [← hvv]
    simp only [ofLnmJudgeGrade_ite]
    simp only [ok_ite]
    repeat (first | rfl | split)
  · simp [judge.judge_tap, ofLnmJudgeGrade]

/-! ## Classic slide -/

@[simp] theorem classicF1 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC.toMicros = 66668 := by decide
@[simp] theorem classicF2 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC.toMicros = 133336 := by decide
@[simp] theorem classicF3 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC.toMicros = 200004 := by decide
@[simp] theorem classicFg1 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC.toMicros = 266672 := by decide
@[simp] theorem classicFg2 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC.toMicros = 333340 := by decide
@[simp] theorem classicFg3 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC.toMicros = 400008 := by decide
@[simp] theorem classicL1 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC.toMicros = 66668 := by decide
@[simp] theorem classicL2 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC.toMicros = 133336 := by decide
@[simp] theorem classicL3 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC.toMicros = 200004 := by decide
@[simp] theorem classicLg1 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC.toMicros = 266672 := by decide
@[simp] theorem classicLg2 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC.toMicros = 333340 := by decide
@[simp] theorem classicLg3 : LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC.toMicros = 400008 := by decide

/-- Explicit expansion of the spec `judgeSlideClassic` (unrolls the private
`pickGrade`/threshold-table helpers into nested `if`s). -/
theorem judgeSlideClassic_expand (d : LnmaiCore.Duration) :
    LnmaiCore.Judge.judgeSlideClassic d =
      (if d < LnmaiCore.Duration.zero then
        (if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC then LnmaiCore.JudgeGrade.Perfect
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC then LnmaiCore.JudgeGrade.FastPerfect2nd
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC then LnmaiCore.JudgeGrade.FastPerfect3rd
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC then LnmaiCore.JudgeGrade.FastGreat
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC then LnmaiCore.JudgeGrade.FastGreat2nd
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC then LnmaiCore.JudgeGrade.FastGreat3rd
         else LnmaiCore.JudgeGrade.FastGood)
       else
        (if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC then LnmaiCore.JudgeGrade.Perfect
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC then LnmaiCore.JudgeGrade.LatePerfect2nd
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC then LnmaiCore.JudgeGrade.LatePerfect3rd
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC then LnmaiCore.JudgeGrade.LateGreat
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC then LnmaiCore.JudgeGrade.LateGreat2nd
         else if LnmaiCore.Duration.abs d ≤ LnmaiCore.Constants.SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC then LnmaiCore.JudgeGrade.LateGreat3rd
         else LnmaiCore.JudgeGrade.LateGood)) := by
  rfl

/-- M1d: `judge_slide_classic` equivalence. -/
theorem judge_slide_classic_equiv (diff : time.Duration)
    (hmin : diff.micros ≠ IScalar.min .I64) :
    judge.judge_slide_classic diff =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeSlideClassic (toLnmDuration diff))) := by
  obtain ⟨v, hv, hvv⟩ := abs_eq diff hmin
  rw [judgeSlideClassic_expand]
  simp only [judge.judge_slide_classic]
  rw [hv]
  simp only [bind_tc_ok]
  simp only [duration_abs_toMicros,
    classicF1, classicF2, classicF3, classicFg1, classicFg2, classicFg3,
    classicL1, classicL2, classicL3, classicLg1, classicLg2, classicLg3,
    lnmDuration_le_iff_toMicros, i64_le_iff, i64_lt_iff, i64_zero_val,
    toLnmDuration_lt_zero, IScalar.ofInt_val_eq]
  rw [← hvv]
  simp only [ofLnmJudgeGrade_ite]
  simp only [ok_ite]
  repeat (first | rfl | split)

/-! ## Touch -/

/-- `Option.map` distributes over `if`. -/
theorem option_map_ite {α β : Type} (f : α → β) (c : Prop) [Decidable c]
    (a b : Option α) :
    Option.map f (if c then a else b) = (if c then Option.map f a else Option.map f b) := by
  split <;> rfl

theorem zero_toMicros : LnmaiCore.Duration.zero.toMicros = 0 := rfl

@[simp] theorem touchP1 : LnmaiCore.Constants.touchPerfect1Ms.toMicros = 150003 := by decide
@[simp] theorem touchP2 : LnmaiCore.Constants.touchPerfect2Ms.toMicros = 175004 := by decide
@[simp] theorem touchP3 : LnmaiCore.Constants.touchPerfect3Ms.toMicros = 200004 := by decide
@[simp] theorem touchG1 : LnmaiCore.Constants.touchGreat1Ms.toMicros = 216671 := by decide
@[simp] theorem touchG2 : LnmaiCore.Constants.touchGreat2Ms.toMicros = 233338 := by decide
@[simp] theorem touchG3 : LnmaiCore.Constants.touchGreat3Ms.toMicros = 250005 := by decide

@[simp] theorem touchM1 : constants.TOUCH_PERFECT_1ST.micros.val = (150003 : ℤ) := by
  simp [constants.TOUCH_PERFECT_1ST]
@[simp] theorem touchM2 : constants.TOUCH_PERFECT_2ND.micros.val = (175004 : ℤ) := by
  simp [constants.TOUCH_PERFECT_2ND]
@[simp] theorem touchM3 : constants.TOUCH_PERFECT_3RD.micros.val = (200004 : ℤ) := by
  simp [constants.TOUCH_PERFECT_3RD]
@[simp] theorem touchMg1 : constants.TOUCH_GREAT_1ST.micros.val = (216671 : ℤ) := by
  simp [constants.TOUCH_GREAT_1ST]
@[simp] theorem touchMg2 : constants.TOUCH_GREAT_2ND.micros.val = (233338 : ℤ) := by
  simp [constants.TOUCH_GREAT_2ND]
@[simp] theorem touchMg3 : constants.TOUCH_GREAT_3RD.micros.val = (250005 : ℤ) := by
  simp [constants.TOUCH_GREAT_3RD]

/-- Explicit expansion of the spec `judgeTouch`, exposing `Duration.abs`. -/
theorem judgeTouch_expand (d : LnmaiCore.Duration) (is_ex : Bool) :
    LnmaiCore.Judge.judgeTouch d is_ex =
      (let isFast := d < LnmaiCore.Duration.zero
       let diffMSec := LnmaiCore.Duration.abs d
       if isFast && diffMSec > LnmaiCore.Constants.touchPerfect1Ms then none
       else
         some
          (if diffMSec ≤ LnmaiCore.Constants.touchPerfect1Ms then LnmaiCore.JudgeGrade.Perfect
           else if diffMSec ≤ LnmaiCore.Constants.touchPerfect2Ms then LnmaiCore.JudgeGrade.LatePerfect2nd
           else if diffMSec ≤ LnmaiCore.Constants.touchPerfect3Ms then LnmaiCore.JudgeGrade.LatePerfect3rd
           else if diffMSec ≤ LnmaiCore.Constants.touchGreat1Ms then LnmaiCore.JudgeGrade.LateGreat
           else if diffMSec ≤ LnmaiCore.Constants.touchGreat2Ms then LnmaiCore.JudgeGrade.LateGreat2nd
           else if diffMSec ≤ LnmaiCore.Constants.touchGreat3Ms then LnmaiCore.JudgeGrade.LateGreat3rd
           else LnmaiCore.JudgeGrade.LateGood)) := by
  simp only [LnmaiCore.Judge.judgeTouch]
  rfl

/-- M1d: `judge_touch` equivalence. `none` is returned exactly for too-early
fast taps, matching the spec's `Option`. -/
theorem judge_touch_equiv (diff : time.Duration) (is_ex : Bool)
    (hmin : diff.micros ≠ IScalar.min .I64) :
    judge.judge_touch diff is_ex =
      ok (Option.map ofLnmJudgeGrade (LnmaiCore.Judge.judgeTouch (toLnmDuration diff) is_ex)) := by
  obtain ⟨v, hv, hvv⟩ := abs_eq diff hmin
  rw [judgeTouch_expand]
  simp only [judge.judge_touch]
  rw [hv]
  simp only [bind_tc_ok]
  by_cases hfast : diff.micros.val < 0
  · simp only [hfast, toLnmDuration_toMicros, zero_toMicros, lnmDuration_lt_iff_toMicros,
      lnmDuration_le_iff_toMicros, duration_abs_toMicros,
      touchP1, touchP2, touchP3, touchG1, touchG2, touchG3,
      touchM1, touchM2, touchM3, touchMg1, touchMg2, touchMg3,
      i64_le_iff, i64_lt_iff, i64_zero_val, hvv, Bool.and_eq_true, decide_eq_true_eq,
      if_true, true_and, Option.map_none, Option.map_some, option_map_ite,
      ofLnmJudgeGrade_ite, ok_ite]
    repeat (first | rfl | split)
  · have hf : (diff.micros.val < 0) = False := by simp [hfast]
    simp only [hf, toLnmDuration_toMicros, zero_toMicros, lnmDuration_lt_iff_toMicros,
      lnmDuration_le_iff_toMicros, duration_abs_toMicros,
      touchP1, touchP2, touchP3, touchG1, touchG2, touchG3,
      touchM1, touchM2, touchM3, touchMg1, touchMg2, touchMg3,
      i64_le_iff, i64_lt_iff, i64_zero_val, hvv, Bool.and_eq_true, decide_eq_true_eq,
      if_false, false_and, Option.map_some, ofLnmJudgeGrade_ite]
    repeat (first | rfl | split)

/-! ## Slide too-late -/

/-- M1d: `judge_slide_too_late` equivalence. -/
theorem judge_slide_too_late_equiv (queue_remaining : Std.U32) :
    judge.judge_slide_too_late queue_remaining =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeSlideTooLate queue_remaining.val)) := by
  unfold judge.judge_slide_too_late LnmaiCore.Judge.judgeSlideTooLate
  by_cases hq : queue_remaining.val = 1
  · have h1 : queue_remaining = 1#u32 := UScalar.eq_of_val_eq (by simpa using hq)
    simp [h1, hq, ofLnmJudgeGrade]
  · have h1 : ¬ (queue_remaining = 1#u32) := by
      intro h; exact hq (by rw [h]; rfl)
    simp [h1, hq, ofLnmJudgeGrade]

end JudgeProof

end Verification
