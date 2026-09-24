# verification

Lean-side equivalence proofs between the Rust rewrite (`../../lnmai-core-rs`)
and the authoritative Lean specification (`../LnmaiCore`).

## Pipeline

The Rust verification-facing crate is `../../lnmai-core-rs/crates/lnmai-core-verify`
(crate name `lnmai_core_verify`). It is written in the Aeneas-compatible subset
(no `String`, no collections, no concurrency).

```
lnmai-core-verify (Rust)
  └─ charon cargo --preset=aeneas        → lnmai_core_verify.llbc
       └─ aeneas -backend lean           → Verification/Generated.lean
            └─ Bridge / Equiv proofs     → lemmas about LnmaiCore
```

Regenerate `Verification/Generated.lean` with:

```bash
../../lnmai-core-rs/tools/regen_aeneas.sh
```

Toolchain (already installed on this machine):

- Aeneas `ac74e1b` at `/Users/pingfanh/bin/aeneas` (Lean backend `v4.30.0-rc2`)
- Charon `0.1.210` at `/Users/pingfanh/bin/charon` (pinned by `charon-pin`)

## Layout

- `Verification/Generated.lean` — Aeneas model of `lnmai_core_verify`. Generated; do not edit.
- `Verification/Basic.lean` — package root marker.
- `Verification/Areas.lean` — M1a: `Areas` isomorphism and function equivalences.

`Bridge.lean` / `GeneratedExt.lean` at the package root are from an earlier
attempt against a pre-`lnmai-core-rs` model. They are not part of the
`Verification` library and will be replaced while porting M1.

## Status

| Module | Isomorphism | Functions | Notes |
|---|---|---|---|
| Areas | ✅ | `to_index`/`of_index`/conversions ✅, `rotate` pending | M1a |
| Types | ✅ | all predicates + `base_score`/`extra_score` ✅ | M1c |
| Convert | ✅ | `convert_maji/gachi/gori/grade` ✅ | M1d |
| Constants | — | values transcribed | M1c |
| Time | — | `Duration.abs` value + equality specs ✅ (`abs_spec`, `abs_eq`); `duration_abs_fromMicros` ✅ | M1b |
| Judge | — | `correct_slide_grade` ✅, windowed functions pending | M1d |
| Score | — | `base_score` ✅, arithmetic pending | M1d |

### Known blocker for the windowed judge/score proofs

`judge.judge_tap` (and its siblings) bind the result of the generated
`time.Duration.abs`, which returns `Result`. Aeneas `step` executes the function
but stops at that bind, leaving a `spec_general`-style goal
(`r_post : r = expected`); the proof cannot be closed from that hypothesis
without being circular. Two sound ways forward:

1. prove an **equality** lemma for `Duration.abs` (via `IScalar.tryMk_eq` under
   the no-overflow bound) and `rw` it before `step`; or
2. make the Rust `Duration.abs` return its result in a form `step` can consume
   (e.g. avoid the `Result` bind), then regenerate the model.

Build: `lake build` (requires the `aeneas` and `lnmai-core` packages wired in
`lakefile.toml`), or `tools/build.sh` in the background.

## Environment constraint (important)

Every proof module imports `LnmaiCore.*`, which imports all of Mathlib, and
generated code imports Aeneas. Loaded oleans are ~5.5 GB. On the current 16 GB
machine (with swap already heavily used) a single Lean check spends most of its
time paging and takes **~6 minutes**; Lake's C-generation facet can push that
past 14 minutes per module. Consequences:

- Do not put `lake build` in the inner development loop.
- Batch proof changes and run one background `tools/build.sh` per batch.
- For regular (fast) equivalence checking, prefer the Rust port's tests and the
  differential harness rather than Lean proofs.

If faster formal iteration is needed, run the verification package on a machine
with >= 32 GB RAM (or trim the Mathlib dependency of the Aeneas backend).

