import Verification.Adapters
import Verification.ScoreExtra

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 1000000
set_option maxErrors 5

namespace Verification.Adapters
open Bridge JudgeProof

def slideWindow (stay : ℤ) : ℤ := 233338 + min (stay / 4) 366674

theorem magnitude_nonnegative (value : ℤ) : 0 ≤ magnitude value := by
  unfold magnitude
  split_ifs <;> omega

theorem slide_window_upper (stay : ℤ) : slideWindow stay ≤ 600012 := by
  unfold slideWindow
  have := min_le_right (stay / 4) (366674 : ℤ)
  omega

theorem slide_window_bounded (stay : ℤ) (h : -4000000 ≤ stay) :
    slideWindow (bounded stay 4000000) = slideWindow stay := by
  unfold bounded
  rw [if_neg (by omega)]
  split_ifs with hh
  · have hs : (366674 : ℤ) ≤ stay / 4 := by omega
    simp only [slideWindow, min_eq_right hs]
    decide
  · rfl

/-- Arbitrarily negative stay times give three negative perfect windows, so
    all perfect-window comparisons are false before and after clamping. -/
theorem slide_perfect_comparisons (value stay : ℤ) :
    (magnitude (bounded value 1000000) ≤ slideWindow (bounded stay 4000000) / 3 ↔
      magnitude value ≤ slideWindow stay / 3) ∧
    (magnitude (bounded value 1000000) ≤ slideWindow (bounded stay 4000000) * 2 / 3 ↔
      magnitude value ≤ slideWindow stay * 2 / 3) ∧
    (magnitude (bounded value 1000000) ≤ slideWindow (bounded stay 4000000) ↔
      magnitude value ≤ slideWindow stay) := by
  by_cases h : stay < -4000000
  · have hb : bounded stay 4000000 = -4000000 := by
      simp only [bounded, if_pos h]
    have hnegative : slideWindow stay < 0 := by
      unfold slideWindow
      have := min_le_left (stay / 4) (366674 : ℤ)
      omega
    have hrep : slideWindow (-4000000) = -766662 := by decide
    have := magnitude_nonnegative value
    have := magnitude_nonnegative (bounded value 1000000)
    rw [hb, hrep]
    omega
  · rw [slide_window_bounded stay (by omega)]
    have hupper := slide_window_upper stay
    exact ⟨bounded_magnitude_le value 1000000 _ (by decide) (by omega),
      bounded_magnitude_le value 1000000 _ (by decide) (by omega),
      bounded_magnitude_le value 1000000 _ (by decide) (by omega)⟩

theorem slide_modern_bounded (value stay : ℤ) (isEX : Bool) :
    LnmaiCore.Judge.judgeSlideModern
      (LnmaiCore.Duration.fromMicros (bounded value 1000000))
      (LnmaiCore.Duration.fromMicros (bounded stay 4000000)) isEX =
    LnmaiCore.Judge.judgeSlideModern (LnmaiCore.Duration.fromMicros value)
      (LnmaiCore.Duration.fromMicros stay) isEX := by
  obtain ⟨h1, h2, h3⟩ := slide_perfect_comparisons value stay
  simp only [slideWindow] at h1 h2 h3
  simp only [judgeSlideModern_expand, lnmDuration_le_iff_toMicros,
    lnmDuration_lt_iff_toMicros, magnitude_value, from_micros_value, zero_toMicros,
    Duration.toMicros_add, Duration.toMicros_min, Duration.toMicros_divNat,
    Duration.toMicros_scaleNat, slideB3_spec, slideMaxExt_spec,
    slideG1_spec, slideG2_spec, slideG3_spec, Nat.cast_ofNat,
    OfNat.ofNat_ne_zero, ite_false, h1, h2, h3,
    bounded_sign value 1000000 (by decide),
    bounded_magnitude_le value 1000000 350007 (by decide) (by decide),
    bounded_magnitude_le value 1000000 416675 (by decide) (by decide),
    bounded_magnitude_le value 1000000 483343 (by decide) (by decide)]

/-- A represented clamped difference can never be the overflowing abs input. -/
theorem representative_not_min (value : ℤ) (diff : time.Duration)
    (h : diff.micros.val = bounded value 1000000) :
    diff.micros ≠ IScalar.min .I64 := by
  have hr := bounded_range value 1000000 (by decide)
  intro he
  simp only [IScalar.min_IScalarTy_I64_eq, I64.min_eq] at he
  omega

/-- These composition theorems connect arbitrary mathematical inputs to the
    actual extracted kernel, assuming the runtime representative is correct. -/
theorem tap_adapter_equiv (value : ℤ) (diff : time.Duration) (isEX : Bool)
    (h : diff.micros.val = bounded value 1000000) :
    judge.judge_tap diff isEX = ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.judgeTap (LnmaiCore.Duration.fromMicros value) isEX)) := by
  rw [judge_tap_equiv diff isEX (representative_not_min value diff h)]
  simp only [toLnmDuration, h, tap_bounded]

theorem touch_adapter_equiv (value : ℤ) (diff : time.Duration) (isEX : Bool)
    (h : diff.micros.val = bounded value 1000000) :
    judge.judge_touch diff isEX = ok ((LnmaiCore.Judge.judgeTouch
      (LnmaiCore.Duration.fromMicros value) isEX).map ofLnmJudgeGrade) := by
  rw [judge_touch_equiv diff isEX (representative_not_min value diff h)]
  simp only [toLnmDuration, h, touch_bounded]

theorem slide_classic_adapter_equiv (value : ℤ) (diff : time.Duration)
    (h : diff.micros.val = bounded value 1000000) :
    judge.judge_slide_classic diff = ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.judgeSlideClassic (LnmaiCore.Duration.fromMicros value))) := by
  rw [judge_slide_classic_equiv diff (representative_not_min value diff h)]
  simp only [toLnmDuration, h, slide_classic_bounded]

theorem slide_modern_adapter_equiv (value stayValue : ℤ)
    (diff stay : time.Duration) (isEX : Bool)
    (hd : diff.micros.val = bounded value 1000000)
    (hs : stay.micros.val = bounded stayValue 4000000) :
    judge.judge_slide_modern diff stay isEX = ok (ofLnmJudgeGrade
      (LnmaiCore.Judge.judgeSlideModern (LnmaiCore.Duration.fromMicros value)
        (LnmaiCore.Duration.fromMicros stayValue) isEX)) := by
  have hr := bounded_range stayValue 4000000 (by decide)
  rw [judge_slide_modern_equiv diff stay isEX (representative_not_min value diff hd)
    (by omega) (by omega)]
  simp only [toLnmDuration, hd, hs, slide_modern_bounded]

theorem classic_hold_adapter_equiv (head : types.JudgeGrade)
    (timing length release : ℤ) (representative : Std.I64)
    (h : representative.val = bounded (timing + length - release) 1000000) :
    judge.judge_hold_classic_end head representative ⟨0#i64⟩ 0#i64 =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeHoldClassicEnd (toLnmJudgeGrade head)
        (LnmaiCore.TimePoint.fromMicros timing) (LnmaiCore.Duration.fromMicros length)
        (LnmaiCore.TimePoint.fromMicros release))) := by
  have hr := bounded_range (timing + length - release) 1000000 (by decide)
  rw [judge_hold_classic_end_equiv head representative 0#i64 ⟨0#i64⟩
    (by omega) (by decide) (by decide)]
  simp only [toLnmDuration, IScalar.ofInt_val_eq, classic_hold_expand,
    add_zero, sub_zero, h, classic_hold_bounded]

def pressBandRepresentative (held reality : ℤ) : Std.U32 :=
  if held ≥ reality then 0#u32
  else if held * 100 ≥ reality * 67 then 1#u32
  else if held * 100 ≥ reality * 33 then 2#u32
  else if held * 100 ≥ reality * 5 then 3#u32 else 4#u32

theorem press_band_representative_value (held reality : ℤ) :
    (pressBandRepresentative held reality).val = pressBandCore held reality := by
  unfold pressBandRepresentative pressBandCore
  split_ifs <;> rfl

def modernHoldAdapter (head : types.JudgeGrade) (diff length ignore release : ℤ) :
    Result types.JudgeGrade :=
  let maximum := max (length - 300000) 0
  let reality := min (max (length - ignore) 0) maximum
  if reality ≤ 0 then ok head else
    judge.hold_band_grade head (decide (diff ≥ 0))
      (pressBandRepresentative (max (reality - release) 0) reality)

/-- The unbounded hold arithmetic calls the extracted grade table only after
    all exact percentage comparisons. No machine-range hypothesis is used. -/
theorem modern_hold_adapter_equiv (head : types.JudgeGrade)
    (diff length ignore release : ℤ) :
    modernHoldAdapter head diff length ignore release =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeHoldEnd (toLnmJudgeGrade head)
        (LnmaiCore.Duration.fromMicros diff) (LnmaiCore.Duration.fromMicros length)
        (LnmaiCore.Duration.fromMicros ignore) (LnmaiCore.Duration.fromMicros release))) := by
  have hreality : min (max (length - ignore) 0) (max (length - 300000) 0) =
      max 0 (min (length - ignore) (length - 300000)) := by
    simp only [max_def, min_def]
    split_ifs <;> omega
  simp only [modernHoldAdapter, hreality, modern_hold_expand, modernHoldCore]
  split_ifs with h
  · simp only [ofLnmJudgeGrade_toLnmJudgeGrade]
  · rw [hold_band_grade_equiv]
    simp only [press_band_representative_value, decide_eq_true_eq, max_comm]

theorem bounded_zero (value : ℤ) : bounded value 1000000 = 0 ↔ value = 0 := by
  unfold bounded
  split_ifs <;> norm_num <;> omega

theorem count_fast_late_bounded (grade : LnmaiCore.JudgeGrade)
    (value : ℤ) (display : LnmaiCore.JudgeDisplayOption) :
    LnmaiCore.Score.countFastLate grade
      (LnmaiCore.Duration.fromMicros (bounded value 1000000)) display =
    LnmaiCore.Score.countFastLate grade (LnmaiCore.Duration.fromMicros value) display := by
  have hz : (LnmaiCore.Duration.fromMicros (bounded value 1000000) == LnmaiCore.Duration.zero) =
      (LnmaiCore.Duration.fromMicros value == LnmaiCore.Duration.zero) := by
    change decide (bounded value 1000000 = 0) = decide (value = 0)
    simp only [bounded_zero]
  cases display <;>
    simp only [LnmaiCore.Score.countFastLate, lnmDuration_lt_iff_toMicros,
      from_micros_value, zero_toMicros, bounded_sign value 1000000 (by decide), hz]

theorem count_fast_late_adapter_equiv (grade : types.JudgeGrade)
    (value : ℤ) (diff : time.Duration) (display : types.JudgeDisplayOption)
    (h : diff.micros.val = bounded value 1000000) :
    score.count_fast_late grade diff display =
      ok (LnmaiCore.Score.countFastLate (toLnmJudgeGrade grade)
        (LnmaiCore.Duration.fromMicros value) (toLnmDisplayOption display)) := by
  rw [ScoreProof.count_fast_late_equiv]
  simp only [toLnmDuration, h, count_fast_late_bounded]

end Verification.Adapters
