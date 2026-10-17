# Rust–Lean equivalence proofs

This package relates the Aeneas model of
`../lnmai-core-rs/crates/lnmai-core-verify` to `../LnmaiCore`. It does not extract
the entire runtime crate. The exact formal boundary and outstanding
obligations are in `SPEC_MAPPING.md` and `SPEC.md`.

## Reproduce

From the parent repository:

```bash
export AENEAS_HOME=/path/to/aeneas
export CHARON_HOME=/path/to/charon
bash verification/tools/wire_aeneas.sh
bash lnmai-core-rs/tools/regen_aeneas.sh --check
(cd verification && lake build)
```

Both Lean packages pin stable Lean 4.30.0. The local backend symlink is ignored;
no machine-specific backend path is committed. The extraction tooling was
checked with Aeneas `ac74e1b` and Charon `0.1.210` (pin
`a535e914f74db4fd9e6be7048f4233270d8945c0`). The LLBC is target-sensitive.

`Verification/Generated.lean` and
`Verification/GeneratedTouchHoldBodyAdapter.lean` are generated from Rust;
never edit either manually. The adapter module imports the core module.
`tools/partition_generated.py` preserves unchanged core declaration bytes,
and `extraction-manifest.json` records how to reconstruct the complete
normalized Aeneas output byte for byte in its original order. This preserves
literal characters as well as declarations; partitioning is not proof acceptance.
After a kernel change, regenerate and rebuild the affected proofs and their
dependents. `--check` rejects stale Lean, partition metadata and LLBC.
Complete the source-equivalence pass and axiom audit before running the
deferred differential and native validation. The user authorized one earlier
checkpoint: after the current proof batch and axiom audit pass, compare the
three requested songs (all ten registered difficulties):

```bash
bash lnmai-core-rs/tools/run_differential.sh --release requested_corpus_ --test-threads=1
```

This runs frontend comparisons and complete classic/modern playthroughs with
autoplay and without input. Every frame output, periodic complete states and
the final state are compared. Missing assets, changed difficulty lists or a
missing Lean oracle fail the check. The full 48-difficulty corpus remains
available through the original test entries.

## Proof modules

- `Areas`, `Types`, `Convert`: constructor bridges, index conversions, enum
  predicates and grade conversion.
- `Bridge`: integer representation and arithmetic lemmas, including negative
  Euclidean division. `toLnmDuration` is an embedding of machine durations,
  not an isomorphism with all Lean integers.
- `Judge`: tap, touch, classic/modern slides and timeout behavior.
- `Hold`, `HoldModern`: classic tail timing, modern press bands and grade table.
- `Rotations`, `Time`, `Constants`: modular rotations, time arithmetic and
  every exported kernel constant.
- `Score`, `ScoreExtra`: base/nonbreak/break scores, combo, fast/late display
  and DX rank; bounded arithmetic assumptions are stated in each theorem.
- `Adapters`, `AdapterJudgments`, `AdapterScore`: arbitrary-integer clamp
  invariance, composition with the extracted judgment tables, and unbounded
  score/combo coefficient formulas. These mathematical adapter proofs do not
  establish the correctness of the Rust bigint representation or operations.
- `Audit`: checks the transitive axioms of all imported `Verification`
  theorems. Only `propext`, `Classical.choice` and `Quot.sound` are permitted.

An upstream Aeneas import contains `sorry` declarations and generated debug
formatters can be opaque. Importing these does not by itself make a theorem
unsound: the audit rejects any claimed theorem that transitively depends on
them, including `sorryAx` or `native_decide` axioms. Lean runtime regression
tests in the parent package use `native_decide`; those are executable tests,
not Rust equivalence proofs.

`Audit.lean` at the package root forwards to `Verification.Audit`. Other old
root-level proof experiments are outside the default `Verification` library
and are not evidence of current equivalence.

For the complete executable validation suite, run
`bash lnmai-core-rs/tools/validate_all.sh` from the parent checkout. Differential
test success and kernel proof success must be reported separately from
whole-program equivalence.
