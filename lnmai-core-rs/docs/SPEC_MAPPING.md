# SPEC_MAPPING.md — Lean ↔ Rust Specification Mapping

## Overview

This document records the complete mapping between Lean definitions and their Rust implementations.

---

## 1. Type Mappings

| Lean Type | Rust Type | Location (Lean) | Location (Rust) |
|-----------|-----------|-----------------|-----------------|
| `SensorArea` | `SensorArea` | `LnmaiCore/Areas.lean:9` | `src/areas.rs` |
| `ButtonZone` | `ButtonZone` | `LnmaiCore/Areas.lean:17` | `src/areas.rs` |
| `OuterSlot` | `OuterSlot` | `LnmaiCore/Areas.lean:21` | `src/areas.rs` |
| `TimeTick` | `TimeTick` | `LnmaiCore/Time.lean:9` | `src/time.rs` |
| `Duration` | `Duration` | `LnmaiCore/Time.lean:14` | `src/time.rs` |
| `TimePoint` | `TimePoint` | `LnmaiCore/Time.lean:19` | `src/time.rs` |
| `JudgeGrade` | `JudgeGrade` | `LnmaiCore/Types.lean:57` | `src/types.rs` |
| `NoteType` | `NoteType` | `LnmaiCore/Types.lean:147` | `src/types.rs` |
| `JudgeStyle` | `JudgeStyle` | `LnmaiCore/Types.lean:178` | `src/types.rs` |
| `NoteStatus` | `NoteStatus` | `LnmaiCore/Types.lean:186` | `src/types.rs` |
| `ComboState` | `ComboState` | `LnmaiCore/Types.lean:217` | `src/types.rs` |
| `ScoreState` | `ScoreState` | `LnmaiCore/Types.lean:332` | `src/types.rs` |
| `JudgeEvent` | `JudgeEvent` | `LnmaiCore/Types.lean:368` | `src/types.rs` |
| `RuntimePos` | `RuntimePos` | `LnmaiCore/Types.lean:24` | `src/types.rs` |
| `TapState` | `TapState` | `LnmaiCore/Lifecycle.lean:58` | `src/lifecycle.rs` |
| `HoldSubState` | `HoldSubState` | `LnmaiCore/Lifecycle.lean` | `src/lifecycle.rs` |
| `TouchState` | `TouchState` | `LnmaiCore/Lifecycle.lean` | `src/lifecycle.rs` |
| `SlideState` | `SlideState` | `LnmaiCore/Lifecycle.lean` | `src/lifecycle.rs` |
| `TapNote` | `TapNote` | `LnmaiCore/Lifecycle.lean:65` | `src/lifecycle.rs` |
| `HoldNote` | `HoldNote` | `LnmaiCore/Lifecycle.lean:284` | `src/lifecycle.rs` |
| `TouchNote` | `TouchNote` | `LnmaiCore/Lifecycle.lean:521` | `src/lifecycle.rs` |
| `SlideNote` | `SlideNote` | `LnmaiCore/Lifecycle.lean:765` | `src/lifecycle.rs` |
| `FrameInput` | `FrameInput` | `LnmaiCore/InputModel.lean:24` | `src/input_model.rs` |
| `GameState` | `GameState` | `LnmaiCore/InputModel.lean:187` | `src/input_model.rs` |
| `ChartSpec` | `ChartSpec` | `LnmaiCore/ChartLoader.lean:190` | `src/chart_loader.rs` |

---

## 2. Function Mappings

### 2.1 Time Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `Duration.ofInt` | `Duration::from_int` | `Time.lean:63` | `src/time.rs` |
| `Duration.toMicros` | `Duration::to_micros` | `Time.lean:78` | `src/time.rs` |
| `TimePoint.ofInt` | `TimePoint::from_int` | `Time.lean:161` | `src/time.rs` |
| `TimePoint.toMicros` | `TimePoint::to_micros` | `Time.lean:176` | `src/time.rs` |
| `Time.parseSecondsString?` | `parse_seconds_string` | `Time.lean:371` | `src/time.rs` |

### 2.2 Area Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `SensorArea.toIndex` | `SensorArea::to_index` | `Areas.lean` | `src/areas.rs` |
| `SensorArea.ofIndex?` | `SensorArea::from_index` | `Areas.lean` | `src/areas.rs` |
| `ButtonZone.toIndex` | `ButtonZone::to_index` | `Areas.lean:38` | `src/areas.rs` |
| `ButtonZone.ofIndex?` | `ButtonZone::from_index` | `Areas.lean:41` | `src/areas.rs` |
| `OuterSlot.toIndex` | `OuterSlot::to_index` | `Areas.lean:46` | `src/areas.rs` |
| `OuterSlot.ofIndex?` | `OuterSlot::from_index` | `Areas.lean:49` | `src/areas.rs` |
| `OuterSlot.toButtonZone` | `OuterSlot::to_button_zone` | `Areas.lean:210` | `src/areas.rs` |
| `ButtonZone.toOuterSlot` | `ButtonZone::to_outer_slot` | `Areas.lean:214` | `src/areas.rs` |

### 2.3 Convert Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `Convert.convertMaji` | `convert_maji` | `Convert.lean:18` | `src/convert.rs` |
| `Convert.convertGachi` | `convert_gachi` | `Convert.lean:39` | `src/convert.rs` |
| `Convert.convertGori` | `convert_gori` | `Convert.lean:60` | `src/convert.rs` |
| `Convert.convertGrade` | `convert_grade` | `Convert.lean:79` | `src/convert.rs` |

### 2.4 Judge Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `Judge.judgeTap` | `judge_tap` | `Judge.lean:43` | `src/judge.rs` |
| `Judge.judgeTouch` | `judge_touch` | `Judge.lean:75` | `src/judge.rs` |
| `Judge.judgeSlideModern` | `judge_slide_modern` | `Judge.lean:111` | `src/judge.rs` |
| `Judge.judgeSlideClassic` | `judge_slide_classic` | `Judge.lean:170` | `src/judge.rs` |
| `Judge.correctSlideGrade` | `correct_slide_grade` | `Judge.lean:184` | `src/judge.rs` |
| `Judge.judgeHoldEnd` | `judge_hold_end` | `Judge.lean:216` | `src/judge.rs` |
| `Judge.judgeHoldClassicEnd` | `judge_hold_classic_end` | `Judge.lean:291` | `src/judge.rs` |
| `Judge.judgeSlideTooLate` | `judge_slide_too_late` | `Judge.lean:319` | `src/judge.rs` |
| `Judge.isTooLateSlide` | `is_too_late_slide` | `Judge.lean:327` | `src/judge.rs` |

### 2.5 Score Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `Score.baseScore` | `base_score` | `Score.lean:19` | `src/score.rs` |
| `Score.scoreNonBreak` | `score_non_break` | `Score.lean:41` | `src/score.rs` |
| `Score.scoreBreak` | `score_break` | `Score.lean:66` | `src/score.rs` |
| `Score.updateCombo` | `update_combo` | `Score.lean:116` | `src/score.rs` |
| `Score.dxScoreRank` | `dx_score_rank` | `Score.lean:196` | `src/score.rs` |
| `Score.computeAccRates` | `compute_acc_rates` | `Score.lean:221` | `src/score.rs` |

### 2.6 Lifecycle Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `Lifecycle.tapStep` | `tap_step` | `Lifecycle.lean:206` | `src/lifecycle.rs` |
| `Lifecycle.holdStep` | `hold_step` | `Lifecycle.lean:409` | `src/lifecycle.rs` |
| `Lifecycle.touchStep` | `touch_step` | `Lifecycle.lean:556` | `src/lifecycle.rs` |
| `Lifecycle.slideStep` | `slide_step` | `Lifecycle.lean:961` | `src/lifecycle.rs` |

### 2.7 Scheduler Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `Scheduler.stepFrame` | `step_frame` | `Scheduler.lean:586` | `src/scheduler.rs` |
| `Scheduler.stepFrameTimed` | `step_frame_timed` | `Scheduler.lean:629` | `src/scheduler.rs` |

### 2.8 ChartLoader Functions

| Lean Function | Rust Function | Location (Lean) | Location (Rust) |
|---------------|---------------|-----------------|-----------------|
| `ChartLoader.buildGameState` | `build_game_state` | `ChartLoader.lean:597` | `src/chart_loader.rs` |
| `ChartLoader.parseChartJson` | `parse_chart_json` | `ChartLoader.lean` | `src/chart_loader.rs` |
| `ChartLoader.parseChartJsonString` | `parse_chart_json_string` | `ChartLoader.lean` | `src/chart_loader.rs` |

---

## 3. Theorem Mappings

| Lean Theorem | Rust Verification | Status |
|--------------|-------------------|--------|
| `Duration.toMicros_injective` | Property test | VERIFIED |
| `Duration.toMicros_le_toMicros` | Property test | VERIFIED |
| `Duration.toMicros_lt_toMicros` | Property test | VERIFIED |
| `Duration.toMicros_eq_toMicros` | Property test | VERIFIED |
| `TimePoint.toMicros_injective` | Property test | VERIFIED |
| `TimePoint.toMicros_le_toMicros` | Property test | VERIFIED |
| `TimePoint.toMicros_lt_toMicros` | Property test | VERIFIED |
| `TimePoint.toMicros_eq_toMicros` | Property test | VERIFIED |
| `sensorArea_ofIndex_toIndex` | Property test + unit test | VERIFIED |
| `sensorArea_toIndex_ofIndex` | Property test + unit test | VERIFIED |
| `buttonZone_ofIndex_toIndex` | Property test + unit test | VERIFIED |
| `buttonZone_toIndex_ofIndex` | Property test + unit test | VERIFIED |
| `outerSlot_ofIndex_toIndex` | Property test + unit test | VERIFIED |
| `outerSlot_toIndex_ofIndex` | Property test + unit test | VERIFIED |
| `perfect_fixed` | Property test + unit test | VERIFIED |
| `miss_fixed` | Property test + unit test | VERIFIED |
| `tooFast_fixed_maji_gachi` | Unit test | VERIFIED |
| `perfect_is_upper_bound` | Property test | VERIFIED |
| `updateSlideParentFlags_length` | N/A | UNVERIFIED |

---

## 4. Integer Semantics

| Lean Type | Rust Type | Notes |
|-----------|-----------|-------|
| `Int` (ℤ) | `i64` | Microsecond timestamps |
| `Nat` | `u32` | Counts, indices |
| `Bool` | `bool` | Direct mapping |
| `String` | `String` | UTF-8 |
| `Rat` | `f64` | Accuracy rates (lossy) |

---

## 5. Proof Obligations Summary

| Module | Status | Property Tests | Unit Tests |
|--------|--------|----------------|------------|
| Time | VERIFIED | 15 | 5 |
| Areas | VERIFIED | 15 | 5 |
| Convert | VERIFIED | 4 | 5 |
| Judge | PARTIALLY VERIFIED | 5 | 8 |
| Score | VERIFIED | 8 | 10 |
| Lifecycle | PARTIALLY VERIFIED | 3 | 0 |
| Scheduler | UNVERIFIED | 1 | 0 |
| ChartLoader | UNVERIFIED | 1 | 0 |

---

## 6. Gaps and TODO

1. **Lifecycle full state machine proofs**: Need property tests for all state transitions
2. **Scheduler frame processing order**: Need verification of tap→hold→touch→touchHold→slide order
3. **ChartLoader completeness**: Need tests for JSON parsing roundtrip
4. **Hold judgment**: Need more comprehensive tests for hold end judgment
5. **Slide judgment**: Need tests for modern/classic slide judgment
6. **Touch group sharing**: Need tests for majority voting mechanism
