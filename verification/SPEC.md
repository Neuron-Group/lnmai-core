# SPEC.md — Extracted Theorems & Proof Obligations

Generated: 2026-06-10 | From LnmaiCore Lean Specification + Rust Rewrite

---

## Theorems from LnmaiCore/Convert.lean

### perfect_fixed
```
Precondition: ∀ style : JudgeStyle
Postcondition: convertGrade style JudgeGrade.Perfect = JudgeGrade.Perfect
Proved in Rust: convert_grade(style, Perfect) == Perfect
Status: ✅ Equiv.lean (convertGrade_equiv, case analysis)
```

### miss_fixed
```
Precondition: ∀ style : JudgeStyle
Postcondition: convertGrade style Miss = Miss
Proved in Rust: Yes
Status: ✅
```

### tooFast_fixed_maji_gachi
```
Precondition: style ∈ {Maji, Gachi}
Postcondition: convertMaji TooFast = TooFast ∧ convertGachi TooFast = TooFast
Status: ✅
```

### perfect_is_upper_bound
```
Precondition: ∀ style : JudgeStyle, g : JudgeGrade
Postcondition: convertGrade style g = Perfect → g = Perfect
Status: ✅ (case analysis)
```

---

## Theorems from LnmaiCore/Time.lean

### toMicros_injective (Duration)
```
Precondition: ∀ a b : Duration
Postcondition: a.toMicros = b.toMicros ↔ a = b
Status: ✅ (Lean spec internal theorem)
Bridge implication: IScalar.val equality implies I64 equality for game values
```

### duration_toMicros_order_preserving
```
Precondition: ∀ a b : Duration
Postcondition: a ≤ b ↔ a.toMicros ≤ b.toMicros
Status: ✅ (implied by duration construction)
Bridge: i64_le_ok lemma in Bridge.lean
```

---

## Theorems from LnmaiCore/Judge.lean

### judgeTap boundaries
```
Precondition: diff : Duration, isEX : Bool, diff.micros ≠ i64::MIN
Postcondition: returns one of 7 grades (Perfect through Good, on fast or late side)
Status: ✅ Verification/Judge.lean (judge_tap_equiv); standard axioms only
  `Duration.abs` handled via abs_eq (no duration_abs_exists axiom needed)
```

### judgeTouch boundaries
```
Precondition: diff : Duration, isEX : Bool, diff.micros ≠ i64::MIN
Postcondition: returns Option JudgeGrade (none if too-early fast)
Status: ✅ Verification/Judge.lean (judge_touch_equiv); standard axioms only
```

### judgeSlideClassic
```
Precondition: diff : Duration, diff.micros ≠ i64::MIN
Postcondition: uses fixed fast/late threshold tables
Status: ✅ Verification/Judge.lean (judge_slide_classic_equiv); standard axioms only
```

### judgeSlideModern
```
Precondition: diff stay_time : Duration, isEX : Bool, stay_time.micros ≥ 0
Postcondition: dynamic extension based on stay_time
Status: ❌ not yet proved (all arithmetic infrastructure is ready)
Proof obligation: model extraction + threshold alignment (see TODO P0/P2)
```

### judgeHoldEnd
```
Precondition: headGrade : JudgeGrade, judgeDiff length ignoreTime playerReleaseTime : Duration
Postcondition: press-band lookup (0-4) determines final grade
Status: ❌ not yet proved
Proof obligation: press_band arithmetic (multiplication comparisons)
```

### judgeHoldClassicEnd
```
Precondition: headGrade : JudgeGrade, timing : TimePoint, length : Duration, releaseTiming : TimePoint
Postcondition: worse of head vs end grade by distFromPerfect
Status: ❌ not yet proved
Proof obligation: TimePoint ↔ I64 conversion
```

---

## Theorems from LnmaiCore/Score.lean

### baseScore
```
Precondition: nt : NoteType
Postcondition: baseScore ∈ {500, 1000, 1500, 2500}
Status: ✅ Verification/Score.lean (base_score_equiv)
```

### scoreNonBreak proportions
```
Precondition: baseScore : Nat, grade : JudgeGrade, multiple : Nat
Postcondition: 
  - Miss/TooFast → (0, b)
  - Good → (b/2, b - b/2)  (50%)
  - Great → (b*4/5, b - b*4/5)  (80%)
  - Perfect → (b, 0)  (100%)
Status: ❌ not yet proved
Proof obligation: Bounded U32 arithmetic matches Nat for game values
```

### updateCombo invariants
```
Precondition: combo : Nat, grade : JudgeGrade, multiple : Nat
Postcondition:
  - Perfect (CP) → all three combos increment
  - Perfect2nd/3rd → combo + pCombo increment, cPCombo resets
  - Great → combo increments, pCombo/cPCombo reset
  - Good → combo increments, pCombo/cPCombo reset
  - Miss/TooFast → combo resets to 0
Status: ❌ not yet proved
```

### dxScoreRank
```
Precondition: achievedDxScore maxDxScore : Nat
Postcondition: result ∈ {0,1,2,3,4,5} by percentage thresholds (97/95/93/90/85)
Status: ❌ not yet proved (U32/Nat discrepancy)
```

---

## Proof Obligations Summary

| Obligation | Status | Blocking |
|---|---|---|
| `duration_abs_exists` axiom | Obsolete | replaced by `abs_spec`/`abs_eq`; judgeTap & judgeSlideClassic proved without it |
| `ofLnmDuration` constructive definition | Axiom | Duration roundtrip |
| Bounded U32 ↔ unbounded Nat bridge | Deferred | scoreNonBreak, dxScoreRank |
| Duration arithmetic bridge (+/*/div) | Deferred | judgeSlideModern |
| press_band arithmetic | Deferred | judgeHoldEnd |
| TimePoint ↔ I64 conversion | Deferred | judgeHoldClassicEnd |
| Rational ↔ f64 bridge | Not started | computeAccRates |
| ChartLoader verification | Not started | — |
| Lifecycle/Scheduler/Storage | Not started | — |

---

## Verification Architecture

```
Lean Spec (LnmaiCore/           Aeneas Model (Generated.lean
  Areas.lean, Time.lean,          + GeneratedExt.lean)
  Types.lean, Convert.lean,
  Judge.lean, Score.lean)               │
         │                                │
         └───────┬────────────────────────┘
                 │
          verification/
          ├── Bridge.lean    ← integer semantics bridge
          ├── Iso.lean       ← type isomorphisms
          ├── Equiv.lean     ← function equivalence proofs
          └── SPEC_MAPPING.md, SPEC.md
```
