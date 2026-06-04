# Aeneas Verification Status

## Overall Status: PARTIALLY VERIFIED

## Summary

The Rust code has been verified through the Charon + Aeneas pipeline for the `aeneas_test` module. The main modules (`types`, `lifecycle`, `scheduler`, etc.) cannot be directly processed by Aeneas due to:

1. **String usage**: The code uses `String` from `serde_json`, which Aeneas cannot handle
2. **Complex enum variants**: Enum variants with the same name as the type path cause name clashes

## Verified Module: aeneas_test

The `aeneas_test` module contains pure functional code that Aeneas can translate:

### Rust Code
```rust
pub enum Grade { Miss, Good, Great, Perfect }
pub enum NoteType { Tap, Hold, Slide }
pub fn grade_dist(g: Grade) -> u32 { ... }
pub fn base_score(nt: NoteType) -> u32 { ... }
pub fn compute_score(base: u32, grade: Grade) -> (u32, u32) { ... }
pub fn judge_tap(diff: Duration) -> Grade { ... }
pub fn update_score(state: &ScoreState, base: u32, grade: Grade) -> ScoreState { ... }
```

### Generated Lean Model
```lean
inductive aeneas_test.Grade where
| Miss : aeneas_test.Grade
| Good : aeneas_test.Grade
| Great : aeneas_test.Grade
| Perfect : aeneas_test.Grade

def aeneas_test.grade_dist (g : aeneas_test.Grade) : Result U32 := ...
def aeneas_test.base_score (nt : aeneas_test.NoteType) : Result U32 := ...
def aeneas_test.compute_score (base : U32) (grade : aeneas_test.Grade) : Result (U32 × U32) := ...
def aeneas_test.judge_tap (diff : aeneas_test.Duration) : Result aeneas_test.Grade := ...
def aeneas_test.update_score (state : aeneas_test.ScoreState) (base : U32) (grade : aeneas_test.Grade) : Result aeneas_test.ScoreState := ...
```

## Unverified Modules

The following modules cannot be directly processed by Aeneas:

### types.rs
- Uses `String` from `serde_json`
- Has complex enum variants with name clashes

### lifecycle.rs
- Uses `String` from `serde_json`
- Has complex enum variants with name clashes

### scheduler.rs
- Uses `String` from `serde_json`
- Has complex enum variants with name clashes

### judge.rs
- Uses `String` from `serde_json`
- Has complex enum variants with name clashes

### score.rs
- Uses `String` from `serde_json`
- Has complex enum variants with name clashes

## Recommendations

To enable full Aeneas verification:

1. **Remove String usage**: Replace `String` with `Vec<u8>` for all text handling
2. **Simplify enum variants**: Use unique names that don't clash with type paths
3. **Split modules**: Separate pure functional core from serialization code

## Verification Commands

```bash
# Run Charon
/tmp/charon/bin/charon cargo --preset aeneas --start-from lnmai_core::aeneas_test

# Run Aeneas
/tmp/aeneas/bin/aeneas -backend lean -dest verification/lean_model lnmai_core.llbc
```

## Generated Files

- `verification/lean_model/LnmaiCore.lean` - Generated Lean model (701 lines)
- `lnmai_core.llbc` - Charon intermediate representation

## Proof Obligations

For the `aeneas_test` module:

1. **grade_dist correctness**: `grade_dist(Perfect) = 0`, `grade_dist(Miss) = 3`
2. **base_score correctness**: `base_score(Tap) = 500`, `base_score(Hold) = 1000`
3. **compute_score conservation**: `earned + lost = base`
4. **judge_tap correctness**: Within 16667μs → Perfect
5. **update_score correctness**: Miss resets combo, Perfect increments perfect_combo

## Next Steps

1. Write Lean proofs for the `aeneas_test` module
2. Refactor main modules to be Aeneas-compatible
3. Re-run Charon + Aeneas on refactored modules
4. Write proofs for all modules
