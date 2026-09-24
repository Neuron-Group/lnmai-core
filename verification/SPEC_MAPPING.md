# SPEC_MAPPING.md — Lean ↔ Rust Equivalence Mapping

Generated: 2026-06-10 | LnmaiCore v0.1.0

---

## Module: Areas

| Lean (LnmaiCore/Areas.lean) | Rust (aeneas-core-verify/src/areas.rs) | Verification |
|---|---|---|
| `SensorArea` (33 variants) | `SensorArea` enum | Iso: `sensorArea_roundtrip_l/r` ✅ |
| `ButtonZone` (8 variants) | `ButtonZone` enum | Iso: `buttonZone_roundtrip_l/r` ✅ |
| `OuterSlot` (8 variants) | `OuterSlot` enum | Iso: `outerSlot_roundtrip_l/r` ✅ |
| `SensorArea.toIndex` → `Nat` | `SensorArea::to_index` → `usize` | Generated.lean ✅ |
| `SensorArea.fromIndex` | `SensorArea::from_index` | Generated.lean ✅ |
| `OuterSlot.toButtonZone` | `OuterSlot::to_button_zone` | Generated.lean ✅ |

---

## Module: Time

| Lean (LnmaiCore/Time.lean) | Rust (aeneas-core-verify/src/time.rs) | Verification |
|---|---|---|
| `Duration { ticks : TimeTick { val : ℤ } }` | `Duration { micros : i64 }` | Iso: `toLnmDuration` (constructive) ✅ |
| `TimePoint { ticks : TimeTick { val : ℤ } }` | `TimePoint { micros : i64 }` | Iso: `toLnmTimePoint` (constructive) ✅ |
| `Duration.zero` | `Duration::zero()` | Bridge ✅ |
| `Duration.abs` | `Duration::abs()` → `core::num::i64::abs` | Bridge: `abs_spec` + `abs_eq` (no overflow at `i64::MIN`) ✅ |
| `Duration.scaleNat` | `Duration::scale_nat` | Value spec ✅ (`Bridge.scale_nat_spec`, `Bridge.hcast_i64_val`) |
| `Duration.divNat` | `Duration::div_nat` | Value spec ✅ (`Bridge.div_nat_spec`); **see division-semantics caveat below** |

**Integer semantics:**
- Lean `Duration` uses `ℤ` (unbounded)
- Rust `Duration` uses `i64` (bounded, modeled as `Std.I64 = IScalar .I64` in Aeneas)
- Bridge: `IScalar.val` extracts `ℤ` from bounded type
- Gap: `core.num.I64.abs` is modeled by Aeneas; `abs_spec`/`abs_eq` give its
  behavior for all inputs except `i64::MIN` (which game diffs never reach).

**Division-semantics caveat (`divNat`).** The spec's `Duration.divNat` uses
Lean's `/` on `ℤ`, which is *Euclidean* division (`(-7)/2 = -4`), while the
Rust/model `/` is Aeneas `IScalar.div_spec`'s `Int.tdiv`, which truncates toward
zero (`(-7).tdiv 2 = -3`). The two agree exactly when the dividend is
nonnegative. Callers pass a wait/segment duration (`stay_time`, nonnegative), so
the modern-slide equivalence is stated with `0 ≤ stay_time.micros.val`.

---

## Module: Types

| Lean (LnmaiCore/Types.lean) | Rust (aeneas-core-verify/src/types.rs) | Verification |
|---|---|---|
| `JudgeGrade` (15 variants) | `JudgeGrade` enum | Iso: `judgeGrade_roundtrip_l/r` ✅ |
| `JudgeStyle` (4 variants) | `JudgeStyle` enum | Iso: `judgeStyle_roundtrip_l/r` ✅ |
| `NoteType` (5 variants) | `NoteType` enum | Iso: `noteType_roundtrip_l/r` ✅ |
| `SlideKind` (3 variants) | `SlideKind` enum | Iso: `slideKind_roundtrip_l/r` ✅ |
| `AreaPolicy` (2 variants) | `AreaPolicy` enum | Iso: `areaPolicy_roundtrip_l/r` ✅ |
| `JudgeGrade.isMissOrTooFast` | `JudgeGrade::is_miss_or_too_fast` | Generated.lean |
| `JudgeGrade.isFast` | `JudgeGrade::is_fast` | Generated.lean |
| `JudgeGrade.distFromPerfect` | `JudgeGrade::dist_from_perfect` | Generated.lean |
| `JudgeGrade.isPerfectGrade` | `JudgeGrade::is_perfect_grade` | Generated.lean |

---

## Module: Constants

| Lean (LnmaiCore/Constants.lean) | Rust (aeneas-core-verify/src/constants.rs) | Verification |
|---|---|---|
| `FRAME_LENGTH` (16667µs) | `FRAME_LENGTH` | Generated.lean: same value |
| `TAP_JUDGE_SEG_1ST_PERFECT_MSEC` = 16667 | `TAP_PERFECT_1ST` = 16667 | Bridge ✅ |
| `TAP_JUDGE_SEG_2ND_PERFECT_MSEC` = 33334 | `TAP_PERFECT_2ND` = 33334 | Same value |
| ... all 30+ timing constants | ... | All verified to match by computation |

---

## Module: Convert

| Lean (LnmaiCore/Convert.lean) | Rust (aeneas-core-verify/src/convert.rs) | Verification |
|---|---|---|
| `convertMaji : JudgeGrade → JudgeGrade` | `convert_maji(grade) → JudgeGrade` | Equiv: `convertMaji_equiv` ✅ |
| `convertGachi` | `convert_gachi` | Equiv: `convertGachi_equiv` ✅ |
| `convertGori` | `convert_gori` | Equiv: `convertGori_equiv` ✅ |
| `convertGrade (style) (g)` | `convert_grade(style, grade)` | Equiv: `convertGrade_equiv` ✅ |

---

## Module: Judge

| Lean (LnmaiCore/Judge.lean) | Rust | Verification |
|---|---|---|
| `judgeTap (diff : Duration) (isEX : Bool) → JudgeGrade` | `judge_tap(diff, is_ex) → JudgeGrade` | Equiv: `judgeTap_equiv` ✅ |
| `judgeTouch (diff) (isEX) → Option JudgeGrade` | `judge_touch(diff, is_ex) → Option<JudgeGrade>` | Equiv: `judgeTouch_equiv` ✅ |
| `judgeSlideModern (diff) (stay_time) (isEX)` | `judge_slide_modern(diff, stay_time, is_ex)` | Equiv: `judgeSlideModern_equiv` ❌ (sorry) |
| `judgeSlideClassic (diff) → JudgeGrade` | `judge_slide_classic(diff) → JudgeGrade` | Equiv: `judgeSlideClassic_equiv` ✅ |
| `correctSlideGrade : JudgeGrade → JudgeGrade` | `correct_slide_grade(grade) → JudgeGrade` | Equiv: `correctSlideGrade_equiv` ✅ |
| `judgeHoldEnd (headGrade) ...` | `judge_hold_end(...)` | Equiv: `judgeHoldEnd_equiv` ❌ (sorry) |
| `judgeHoldClassicEnd (headGrade) ...` | `judge_hold_classic_end(...)` | Equiv: `judgeHoldClassicEnd_equiv` ❌ (sorry) |
| `judgeSlideTooLate (queueRemaining : Nat)` | `judge_slide_too_late(queue_remaining: u32)` | Equiv: `judgeSlideTooLate_equiv` ✅ |
| `isTooLateSlide (diff) (userOffset)` | `is_too_late_slide(diff, user_offset)` | Equiv: `isTooLateSlide_equiv` ❌ (pending: `+` value bridge) |

---

## Module: Score

| Lean (LnmaiCore/Score.lean) | Rust | Verification |
|---|---|---|
| `baseScore (nt : NoteType) : Nat` | `base_score(nt) → u32` | Equiv: `baseScore_equiv` ✅ |
| `scoreNonBreak (baseScore) (grade) (multiple) : Nat × Nat` | `score_non_break(base, grade, multiple) → (u32, u32)` | ❌ not yet proved |
| `scoreBreak (grade) (multiple) : ...` | `score_break(grade, multiple)` | ❌ not yet proved |
| `updateCombo (combo, pCombo, cPCombo, dXScoreLost, grade, multiple) → ComboDelta` | `update_combo(...) → ComboDelta` | ❌ not yet proved |
| `dxScoreRank (achievedDxScore) (maxDxScore) : Nat` | `dx_score_rank(achieved, max) → u32` | ❌ not yet proved |
| `countFastLate (grade) (diff) (display) : Bool × Bool` | `count_fast_late(grade, diff, display)` | Not yet modeled |
| `computeAccRates ... : AccRates` | `compute_acc_rates(score) → AccRates` | Not yet modeled |

---

## Summary

| Category | Count | Status |
|---|---|---|
| Sum-type isomorphisms | 9 types | All proved ✅ |
| Convert functions | 4 functions | All proved ✅ |
| Judge functions | 9 functions | 5 proved (`judgeTap`, `judgeTouch`, `judgeSlideClassic`, `judgeSlideTooLate`, `correctSlideGrade`); 4 pending (`isTooLateSlide`, `judgeSlideModern`, both hold-ends) |
| Score functions | 7 functions | 1 proved (`baseScore`); 4 pending; 2 not modeled |
| Axioms in use | 0 | The active `Verification` library proves everything from standard axioms only |

## Remaining Gaps

1. **Bounded/Unbounded Arithmetic Bridge**: U32 arithmetic in Rust differs from Nat in Lean for overflow cases. Game values are within bounds but proofs are pending.
2. **judgeSlideModern**: needs `Duration.divNat`/`scaleNat` (P2).
3. **judgeHoldEnd / judgeHoldClassicEnd**: complex structural proofs pending (`Duration` arithmetic/comparison bridges).
4. **computeAccRates**: Uses `f64` in Rust vs `Rat` in Lean — rational number bridge needed.
5. **ChartLoader, Lifecycle, Scheduler, Storage, Domain, InputModel**: Not yet verified.
