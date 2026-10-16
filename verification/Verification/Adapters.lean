import Verification.HoldModern

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 1000000
set_option maxErrors 5

namespace Verification.Adapters
open Bridge JudgeProof

/- These lemmas discharge the mathematical obligations of the runtime's
   representative-value adapters for arbitrary integers. Connecting the
   actual Integer representation and its Rust operations remains separate. -/

def bounded (value limit : ℤ) : ℤ :=
  if value < -limit then -limit else if value > limit then limit else value

def magnitude (value : ℤ) : ℤ := if value < 0 then -value else value

theorem bounded_range (value limit : ℤ) (h : 0 ≤ limit) :
    -limit ≤ bounded value limit ∧ bounded value limit ≤ limit := by
  unfold bounded
  split_ifs <;> omega

theorem bounded_sign (value limit : ℤ) (h : 0 < limit) :
    bounded value limit < 0 ↔ value < 0 := by
  unfold bounded
  split_ifs <;> omega

theorem bounded_magnitude_le (value limit threshold : ℤ) (hlimit : 0 < limit)
    (hthreshold : threshold < limit) :
    magnitude (bounded value limit) ≤ threshold ↔ magnitude value ≤ threshold := by
  unfold bounded magnitude
  split_ifs <;> omega

theorem bounded_magnitude_lt (value limit threshold : ℤ) (hlimit : 0 < limit)
    (hthreshold : threshold ≤ limit) :
    magnitude (bounded value limit) < threshold ↔ magnitude value < threshold := by
  unfold bounded magnitude
  split_ifs <;> omega

@[simp] theorem from_micros_value (value : ℤ) :
    (LnmaiCore.Duration.fromMicros value).toMicros = value := rfl

theorem magnitude_value (value : ℤ) :
    (LnmaiCore.Duration.abs (LnmaiCore.Duration.fromMicros value)).toMicros =
      magnitude value := by
  rw [duration_abs_fromMicros]
  rfl

theorem tap_bounded (value : ℤ) (isEX : Bool) :
    LnmaiCore.Judge.judgeTap (LnmaiCore.Duration.fromMicros (bounded value 1000000)) isEX =
      LnmaiCore.Judge.judgeTap (LnmaiCore.Duration.fromMicros value) isEX := by
  simp only [judgeTap_expand, lnmDuration_le_iff_toMicros, lnmDuration_lt_iff_toMicros,
    magnitude_value, from_micros_value, zero_toMicros, tapPerfect1Ms_val,
    tapPerfect2Ms_val, tapPerfect3Ms_val, tapGreat1Ms_val, tapGreat2Ms_val, tapGreat3Ms_val,
    bounded_sign value 1000000 (by decide),
    bounded_magnitude_le value 1000000 16667 (by decide) (by decide),
    bounded_magnitude_le value 1000000 33334 (by decide) (by decide),
    bounded_magnitude_le value 1000000 50001 (by decide) (by decide),
    bounded_magnitude_le value 1000000 66668 (by decide) (by decide),
    bounded_magnitude_le value 1000000 83335 (by decide) (by decide),
    bounded_magnitude_le value 1000000 100002 (by decide) (by decide)]

theorem touch_bounded (value : ℤ) (isEX : Bool) :
    LnmaiCore.Judge.judgeTouch (LnmaiCore.Duration.fromMicros (bounded value 1000000)) isEX =
      LnmaiCore.Judge.judgeTouch (LnmaiCore.Duration.fromMicros value) isEX := by
  have htooearly : 150003 < magnitude (bounded value 1000000) ↔
      150003 < magnitude value := by
    simpa only [not_le] using not_congr
      (bounded_magnitude_le value 1000000 150003 (by decide) (by decide))
  simp only [judgeTouch_expand, lnmDuration_le_iff_toMicros, lnmDuration_lt_iff_toMicros,
    magnitude_value, from_micros_value, zero_toMicros, touchP1, touchP2, touchP3,
    touchG1, touchG2, touchG3, htooearly,
    bounded_sign value 1000000 (by decide),
    bounded_magnitude_le value 1000000 150003 (by decide) (by decide),
    bounded_magnitude_le value 1000000 175004 (by decide) (by decide),
    bounded_magnitude_le value 1000000 200004 (by decide) (by decide),
    bounded_magnitude_le value 1000000 216671 (by decide) (by decide),
    bounded_magnitude_le value 1000000 233338 (by decide) (by decide),
    bounded_magnitude_le value 1000000 250005 (by decide) (by decide)]

theorem slide_classic_bounded (value : ℤ) :
    LnmaiCore.Judge.judgeSlideClassic (LnmaiCore.Duration.fromMicros (bounded value 1000000)) =
      LnmaiCore.Judge.judgeSlideClassic (LnmaiCore.Duration.fromMicros value) := by
  simp only [judgeSlideClassic_expand, lnmDuration_le_iff_toMicros,
    lnmDuration_lt_iff_toMicros, magnitude_value, from_micros_value, zero_toMicros,
    classicF1, classicF2, classicF3, classicFg1, classicFg2, classicFg3,
    classicL1, classicL2, classicL3, classicLg1, classicLg2, classicLg3,
    bounded_sign value 1000000 (by decide),
    bounded_magnitude_le value 1000000 66668 (by decide) (by decide),
    bounded_magnitude_le value 1000000 133336 (by decide) (by decide),
    bounded_magnitude_le value 1000000 200004 (by decide) (by decide),
    bounded_magnitude_le value 1000000 266672 (by decide) (by decide),
    bounded_magnitude_le value 1000000 333340 (by decide) (by decide),
    bounded_magnitude_le value 1000000 400008 (by decide) (by decide)]

theorem classic_hold_bounded (head : LnmaiCore.JudgeGrade) (value : ℤ) :
    classicHoldCore head (bounded value 1000000) = classicHoldCore head value := by
  have hpositive : 0 < bounded value 1000000 ↔ 0 < value := by
    unfold bounded
    split_ifs <;> omega
  change (if head.isMissOrTooFast then head else
    let tail := if bounded value 1000000 > 0 then
      (if magnitude (bounded value 1000000) < 150003 then .Perfect else .FastGood)
      else (if magnitude (bounded value 1000000) < 200004 then .Perfect else .LateGood)
    if tail.distFromPerfect > head.distFromPerfect then tail else head) = _
  simp only [hpositive,
    bounded_magnitude_lt value 1000000 150003 (by decide) (by decide),
    bounded_magnitude_lt value 1000000 200004 (by decide) (by decide)]
  rfl

end Verification.Adapters
