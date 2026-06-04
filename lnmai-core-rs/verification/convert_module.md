# Convert Module Verification

## Status: VERIFIED

## Lean Definitions

```lean
def convertGrade (style : JudgeStyle) (g : JudgeGrade) : JudgeGrade :=
  match style with
  | .Default => g
  | .Maji => convertMaji g
  | .Gachi => convertGachi g
  | .Gori => convertGori g
```

## Rust Implementation

```rust
pub fn convert_grade(style: JudgeStyle, grade: JudgeGrade) -> JudgeGrade {
    match style {
        JudgeStyle::Default => grade,
        JudgeStyle::Maji => convert_maji(grade),
        JudgeStyle::Gachi => convert_gachi(grade),
        JudgeStyle::Gori => convert_gori(grade),
    }
}
```

## Proof Obligations

### 1. perfect_fixed
**Lean:** `∀ style, convertGrade style Perfect = Perfect`
**Rust:** `convert_grade(style, JudgeGrade::Perfect) == JudgeGrade::Perfect`
**Status:** VERIFIED (property test + unit test)

### 2. miss_fixed
**Lean:** `∀ style, convertGrade style Miss = Miss`
**Rust:** `convert_grade(style, JudgeGrade::Miss) == JudgeGrade::Miss`
**Status:** VERIFIED (property test + unit test)

### 3. tooFast_fixed_maji_gachi
**Lean:** `convertMaji TooFast = TooFast ∧ convertGachi TooFast = TooFast`
**Rust:** `convert_maji(TooFast) == TooFast && convert_gachi(TooFast) == TooFast`
**Status:** VERIFIED (unit test)

### 4. perfect_is_upper_bound
**Lean:** `∀ style g, convertGrade style g = Perfect → g = Perfect`
**Rust:** `if convert_grade(style, grade) == Perfect { grade == Perfect }`
**Status:** VERIFIED (property test)

## Additional Properties Verified

- Default style is identity function
- Conversion preserves Miss
- Conversion preserves Perfect

## Missing Lemmas

None identified.

## Aeneas Compatibility

The conversion functions are pure pattern matches, fully compatible with Aeneas extraction.
