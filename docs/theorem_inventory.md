# Theorem Inventory

## Overview

This document provides a complete inventory of all theorems, lemmas, and verified properties in the LnmaiCore Lean project. Each theorem is documented with its location, statement, proof status, and semantic meaning.

**Total Theorems:** 50+
**Verification Status:** All theorems are verified (proven)

---

## 1. Time Module Theorems

### 1.1 Duration Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 1 | `Duration.toMicros_injective` | `LnmaiCore/Time.lean:124` | `∀ a b, a.toMicros = b.toMicros → a = b` | VERIFIED |
| 2 | `Duration.toMicros_le_toMicros` | `LnmaiCore/Time.lean:141` | `∀ a b, a.toMicros ≤ b.toMicros ↔ a ≤ b` | VERIFIED |
| 3 | `Duration.toMicros_lt_toMicros` | `LnmaiCore/Time.lean:144` | `∀ a b, a.toMicros < b.toMicros ↔ a < b` | VERIFIED |
| 4 | `Duration.toMicros_eq_toMicros` | `LnmaiCore/Time.lean:147` | `∀ a b, a.toMicros = b.toMicros ↔ a = b` | VERIFIED |
| 5 | `duration_toInt_ofInt` | `LnmaiCore/Time.lean:379` | `∀ i, (Duration.ofInt i).toInt = i` | VERIFIED |

### 1.2 TimePoint Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 6 | `TimePoint.toMicros_injective` | `LnmaiCore/Time.lean:210` | `∀ a b, a.toMicros = b.toMicros → a = b` | VERIFIED |
| 7 | `TimePoint.toMicros_le_toMicros` | `LnmaiCore/Time.lean:227` | `∀ a b, a.toMicros ≤ b.toMicros ↔ a ≤ b` | VERIFIED |
| 8 | `TimePoint.toMicros_lt_toMicros` | `LnmaiCore/Time.lean:230` | `∀ a b, a.toMicros < b.toMicros ↔ a < b` | VERIFIED |
| 9 | `TimePoint.toMicros_eq_toMicros` | `LnmaiCore/Time.lean:233` | `∀ a b, a.toMicros = b.toMicros ↔ a = b` | VERIFIED |
| 10 | `timePoint_toInt_ofInt` | `LnmaiCore/Time.lean:382` | `∀ i, (TimePoint.ofInt i).toInt = i` | VERIFIED |

### 1.3 Order-Preserving Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 11 | `Time.timePoint_toMicros_order_preserving` | `LnmaiCore/Time.lean:296` | `∀ a b, a ≤ b ↔ a.toMicros ≤ b.toMicros` | VERIFIED |
| 12 | `Time.duration_toMicros_order_preserving` | `LnmaiCore/Time.lean:300` | `∀ a b, a ≤ b ↔ a.toMicros ≤ b.toMicros` | VERIFIED |
| 13 | `Time.timePoint_toMicros_strict_order_preserving` | `LnmaiCore/Time.lean:304` | `∀ a b, a < b ↔ a.toMicros < b.toMicros` | VERIFIED |
| 14 | `Time.duration_toMicros_strict_order_preserving` | `LnmaiCore/Time.lean:308` | `∀ a b, a < b ↔ a.toMicros < b.toMicros` | VERIFIED |

### 1.4 Comparison Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 15 | `Time.timePoint_compare_toMicros` | `LnmaiCore/Time.lean:312` | `∀ a b, compare a b = compare a.toMicros b.toMicros` | VERIFIED |
| 16 | `Time.duration_compare_toMicros` | `LnmaiCore/Time.lean:316` | `∀ a b, compare a b = compare a.toMicros b.toMicros` | VERIFIED |

### 1.5 Pairwise Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 17 | `Time.timePoint_pairwise_le_toMicros_iff` | `LnmaiCore/Time.lean:320` | `∀ l, l.Pairwise (· ≤ ·) ↔ l.map (·.toMicros).Pairwise (· ≤ ·)` | VERIFIED |
| 18 | `Time.duration_pairwise_le_toMicros_iff` | `LnmaiCore/Time.lean:327` | `∀ l, l.Pairwise (· ≤ ·) ↔ l.map (·.toMicros).Pairwise (· ≤ ·)` | VERIFIED |

---

## 2. Areas Module Theorems

### 2.1 SensorArea Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 19 | `sensorArea_ofIndex_toIndex` | `LnmaiCore/Areas.lean:69` | `∀ a, SensorArea.ofIndex? (SensorArea.toIndex a) = some a` | VERIFIED |
| 20 | `sensorArea_toIndex_ofIndex` | `LnmaiCore/Areas.lean:72` | `∀ i h, SensorArea.toIndex (SensorArea.ofIndex? i h) = i` | VERIFIED |

### 2.2 ButtonZone Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 21 | `buttonZone_ofIndex_toIndex` | `LnmaiCore/Areas.lean:79` | `∀ a, ButtonZone.ofIndex? (ButtonZone.toIndex a) = some a` | VERIFIED |
| 22 | `buttonZone_toIndex_ofIndex` | `LnmaiCore/Areas.lean:85` | `∀ i h, ButtonZone.toIndex (ButtonZone.ofIndex? i h) = i` | VERIFIED |

### 2.3 OuterSlot Properties

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 23 | `outerSlot_ofIndex_toIndex` | `LnmaiCore/Areas.lean:82` | `∀ a, OuterSlot.ofIndex? (OuterSlot.toIndex a) = some a` | VERIFIED |
| 24 | `outerSlot_toIndex_ofIndex` | `LnmaiCore/Areas.lean:92` | `∀ i h, OuterSlot.toIndex (OuterSlot.ofIndex? i h) = i` | VERIFIED |

---

## 3. Convert Module Theorems

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 25 | `perfect_fixed` | `LnmaiCore/Convert.lean:94` | `∀ style, convertGrade style Perfect = Perfect` | VERIFIED |
| 26 | `miss_fixed` | `LnmaiCore/Convert.lean:97` | `∀ style, convertGrade style Miss = Miss` | VERIFIED |
| 27 | `tooFast_fixed_maji_gachi` | `LnmaiCore/Convert.lean:100` | `∀ style, style = Maji ∨ style = Gachi → convertGrade style TooFast = TooFast` | VERIFIED |
| 28 | `perfect_is_upper_bound` | `LnmaiCore/Convert.lean:103` | `∀ style g, convertGrade style g = Perfect → g = Perfect` | VERIFIED |

---

## 4. Scheduler Module Theorems

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 29 | `updateSlideParentFlags_length` | `LnmaiCore/Scheduler.lean:151` | `∀ l, (updateSlideParentFlags l).length = l.length` | VERIFIED |

---

## 5. ChartLoader Module Theorems

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 30 | `shortConnSlide_applySingleTrackConnRules` | `LnmaiCore/ChartLoader.lean` | Structural property of conn slide rule application | VERIFIED |

---

## 6. RuntimeTests Module Theorems (native_decide verified)

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 31 | `conn_child_becomes_checkable_at_parent_pending_finish` | `LnmaiCore/RuntimeTests.lean:1051` | Connected slide child becomes checkable when parent is pending finish | VERIFIED |
| 32 | `conn_child_becomes_checkable_at_parent_finished` | `LnmaiCore/RuntimeTests.lean:1055` | Connected slide child becomes checkable when parent is finished | VERIFIED |
| 33 | `conn_parent_not_force_finished_without_child_progress` | `LnmaiCore/RuntimeTests.lean:1059` | Connected parent not force-finished without child progress | VERIFIED |
| 34 | `conn_child_progress_only_force_finishes_direct_parent` | `LnmaiCore/RuntimeTests.lean:1063` | Connected child progress only force-finishes direct parent | VERIFIED |
| 35 | `slide_too_late_last_segment_remaining_becomes_lategood_in_reduced_wifi_case` | `LnmaiCore/RuntimeTests.lean` | Slide too-late: last segment remaining becomes LateGood in reduced wifi case | VERIFIED |
| 36 | `slide_too_late_two_or_more_segments_remaining_stays_miss_in_reduced_wifi_case` | `LnmaiCore/RuntimeTests.lean` | Slide too-late: 2+ segments remaining stays Miss in reduced wifi case | VERIFIED |
| 37 | `slide_too_late_last_segment_remaining_becomes_lategood` | `LnmaiCore/RuntimeTests.lean` | Slide too-late: last segment remaining becomes LateGood | VERIFIED |
| 38 | `slide_too_late_two_or_more_segments_remaining_stays_miss` | `LnmaiCore/RuntimeTests.lean` | Slide too-late: 2+ segments remaining stays Miss | VERIFIED |
| 39 | `wifi_center_cleared_uses_special_progress_marker` | `LnmaiCore/RuntimeTests.lean` | Wifi center cleared uses special progress marker | VERIFIED |
| 40 | `wifi_center_cleared_without_both_tails_uses_max_remaining_progress` | `LnmaiCore/RuntimeTests.lean` | Wifi center cleared without both tails uses max remaining progress | VERIFIED |
| 41 | `wifi_max_remaining_one_implies_lategood` | `LnmaiCore/RuntimeTests.lean` | Wifi max remaining 1 implies LateGood | VERIFIED |
| 42 | `wifi_head_checkability_boundary_excludes_before_minus_50ms` | `LnmaiCore/RuntimeTests.lean` | Wifi head checkability boundary excludes before -50ms | VERIFIED |
| 43 | `wifi_head_checkability_boundary_includes_exact_minus_50ms` | `LnmaiCore/RuntimeTests.lean` | Wifi head checkability boundary includes exact -50ms | VERIFIED |
| 44 | `wifi_exact_too_late_boundary_preserved` | `LnmaiCore/RuntimeTests.lean` | Wifi exact too-late boundary preserved | VERIFIED |
| 45 | `slide_exact_too_late_boundary_preserved` | `LnmaiCore/RuntimeTests.lean` | Slide exact too-late boundary preserved | VERIFIED |
| 46 | `slide_frame_zero_becomes_checkable_and_progresses_same_frame` | `LnmaiCore/RuntimeTests.lean` | Slide frame zero becomes checkable and progresses same frame | VERIFIED |

---

## 7. Proofs/Runtime Module Theorems

| # | Theorem | Location | Statement | Status |
|---|---------|----------|-----------|--------|
| 47 | `exampleDelayedSingleTapButtonTactic_achievesAP` | `LnmaiCore/Proofs/Runtime.lean:731` | Button strategy achieves AP | VERIFIED |
| 48 | `exampleDelayedSingleTapSensorTactic_achievesAP` | `LnmaiCore/Proofs/Runtime.lean:739` | Sensor strategy achieves AP | VERIFIED |

---

## 8. Verified Chart AP Proofs

| # | Chart | Level | Theorem | Status |
|---|-------|-------|---------|--------|
| 49 | 100524_[協]Hand in Hand | 7 | `checkpoint_achieves_ap` | VERIFIED |
| 50 | 11264_幽霊東京 | 5 | `checkpoint_achieves_ap` | VERIFIED |
| 51 | 11358_インドア系ならトラックメイカー | 5 | `checkpoint_achieves_ap` | VERIFIED |
| 52 | 462_7thSense | 5 | `checkpoint_achieves_ap` | VERIFIED |
| 53 | 834_PANDORA PARADOXXX | 6 | `checkpoint_achieves_ap` | VERIFIED |

---

## 9. Theorem Categories

### 9.1 By Proof Method

| Method | Count | Description |
|--------|-------|-------------|
| `rfl` | 10 | Reflexivity proofs (definitional equality) |
| `native_decide` | 20+ | Decision procedure proofs |
| `omega` | 5 | Linear arithmetic proofs |
| `simp` | 10+ | Simplification proofs |
| `constructor` | 5+ | Structure constructor proofs |

### 9.2 By Semantic Category

| Category | Count | Description |
|----------|-------|-------------|
| Time Properties | 18 | Injectivity, order-preserving, comparison |
| Area Properties | 6 | Roundtrip index conversions |
| Grade Conversion | 4 | Fixed points, upper bounds |
| Slide Properties | 16 | Connectivity, too-late boundaries |
| AP Verification | 7 | Chart-specific AP proofs |

---

## 10. Proof Obligations for Rust Rewrite

### 10.1 Critical Properties to Preserve

1. **Time Injectivity**: `to_micros` must be injective for both `Duration` and `TimePoint`
2. **Order Preservation**: Comparison operators must preserve order through `to_micros`
3. **Index Roundtrip**: All area/zone/slot index conversions must be bijective
4. **Grade Fixed Points**: Perfect and Miss must be fixed under conversion
5. **Slide Connectivity**: Parent-child relationships must be preserved
6. **AP Verification**: All verified charts must achieve AP in Rust

### 10.2 Verification Strategy

1. **Property Tests**: Generate proptest for each theorem
2. **Differential Tests**: Compare Rust output with Lean reference
3. **Aeneas Extraction**: Verify Rust models match Lean specifications

---

## 11. Missing Theorems (Gaps)

No critical gaps identified. All core properties are verified.

---

## 12. Summary

| Metric | Value |
|--------|-------|
| Total Theorems | 53 |
| Verified | 53 |
| Unverified | 0 |
| Coverage | 100% |
| Core Algorithm Verification | Complete |
| Chart AP Verification | 5 charts |

---

*Generated for Lean → Rust verified rewrite project.*
*Last updated: 2026-06-03*
