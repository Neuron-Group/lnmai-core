# lnmai-core-rs

Rust rewrite of the Lean `LnmaiCore` gameplay core, plus an Aeneas/Lean
verification package and a differential test harness that checks the Rust
output against the authoritative Lean implementation.

This directory is self-contained: it contains the Rust workspace, the Lean
verification package, helper tooling, and this documentation.

```
lnmai-core-rs/
  Cargo.toml                      # workspace
  crates/
    lnmai-core-verify/            # Aeneas-compatible pure core (single source of truth)
    lnmai-core/                   # full port + FFI (JSON API + C ABI)
      src/{areas,time,types,constants,convert,judge,score}.rs   (re-exported)
      src/{storage,symmetry,rat,time,input_model,lifecycle,
           chart_loader,runtime_score,scheduler,events,default_tactic,ffi,cabi}.rs
      src/simai/{syntax,timing,shape,slide_tables,slide_parser,
                 tokenize,typecheck,ir,normalize,maidata,frontend}.rs
      tests/differential.rs       # Rust vs Lean
  include/lnmai_ffi.h             # C ABI header
  tools/regen_aeneas.sh           # charon + aeneas -> Verification/Generated.lean
  tools/run_differential.sh       # run the differential suite
```

The Lean specification (`LnmaiCore/`) lives in the parent `lnmai-core`
checkout; `../verification/` holds the Aeneas-generated model and the
equivalence proofs.

---

## 1. Architecture

- **`lnmai-core-verify`** is written in the Aeneas-compatible subset (no
  `String`, no collections, no concurrency) and is the *single source of
  truth* for the pure semantics: Areas, Time, Types, Constants, Convert,
  Judge, Score. The main crate re-exports these, so the code that is proved is
  the code that ships.
- **`lnmai-core`** ports the rest: `Storage`, `Symmetry`, exact `Rat`,
  the full Simai pipeline, the runtime state machines
  (`InputModel`/`Lifecycle`/`ChartLoader`/`Scheduler`), and the FFI surface.
- **Verification** runs `charon` on `lnmai-core-verify` to produce `.llbc`,
  then `aeneas -backend lean` to produce `Verification/Generated.lean`, and the
  hand-written Lean files relate that model to `LnmaiCore`.
- **Differential testing** drives the Lean CLIs (`simai-parser-cli`,
  `runtime-step-cli`) and compares JSON payloads field-by-field.

## 2. Prerequisites

- **Rust**: a recent toolchain (`cargo`). Developed with
  `rustc 1.98.0-nightly`.
- **Lean 4** with `elan`, toolchain `leanprover/lean4:v4.30.0-rc2`
  (see `../lean-toolchain`). `lake` must be on `PATH`.
- **Aeneas + Charon**:
  - Aeneas commit used here: `ac74e1b` (its `backends/lean` pins
    `v4.30.0-rc2`, matching this repo).
  - Charon pinned by Aeneas to
    `a535e914f74db4fd9e6be7048f4233270d8945c0` (version `0.1.210`).
  - Install either with Nix
    (`nix run github:aeneasverif/aeneas#charon`,
     `nix run github:aeneasverif/aeneas -- -backend lean …`) or build from
     source: Aeneas needs OCaml/opam (`make setup-charon && make`), Charon
     needs `rustup`.

## 3. Fresh-machine setup

```bash
# Rust build + tests (no Lean needed)
cd lnmai-core-rs
cargo test

# Point the tooling at your Aeneas/Charon builds
export AENEAS_HOME=/path/to/aeneas
export CHARON_HOME=/path/to/charon      # directory containing `charon` + `charon-driver`
```

The verification package references the Aeneas Lean backend by absolute path
in `../verification/lakefile.toml` (`[[require]] name = "aeneas" path = ...`).
On a new machine, edit that path to your `AENEAS_HOME/backends/lean` (or create
it as a symlink). Lake's TOML cannot read environment variables, so this is a
one-time manual edit. Aeneas's own `.lake/packages` may be empty; if Lake tries
to fetch dependencies, symlink the same-revision packages from a built package
(e.g. the verification `.lake/packages`) into `AENEAS_HOME/backends/lean/.lake/packages`.

## 4. Build & run

```bash
# Regenerate the Aeneas Lean model from the Rust verify crate
lnmai-core-rs/tools/regen_aeneas.sh

# Build the Lean verification package (loads Mathlib; slow — see §7)
cd ../verification && lake build          # or: tools/build.sh

# Differential tests (requires the Lean CLIs below)
cd lnmai-core && lake build simai-parser-cli runtime-step-cli
lnmai-core-rs/tools/run_differential.sh
```

The differential harness reads `LNMAI_LEAN_PARSER_CLI` and
`LNMAI_LEAN_RUNTIME_CLI` (both auto-located by `run_differential.sh`). Without
them the tests skip and pass.

### FFI

- `ffi.rs` — JSON API + process-local session registry
  (`create_empty_session_handle`, `load_chart_into_session_from_text/json`,
  `unload_chart_from_session`, `get_lowered_chart_json_by_handle`,
  `free_game_state_handle`, `step_game_state_handle[_light]`,
  `parse_lowered/normalized/frontend_semantic_chart_json`,
  `default_tactic_from_chart_json`), returning
  `{"ok":true,"result":…}` / `{"ok":false,"error":{…}}`.
- `cabi.rs` + `include/lnmai_ffi.h` — the same surface as a C-string ABI
  (symbols mirror `LnmaiCore/FFI.lean`; free results with `lnmai_string_free`).

## 5. Status

**Rewrite: complete.** The Rust port runs the whole pipeline end-to-end
(`maidata.txt → … → ChartSpec → GameState → stepFrame`), with 68 tests.

**Equivalence, differential: strong.** `tools/run_differential.sh` verifies
- 18 charts (parse/lower),
- 6 runtime scenarios,
- 3 seeds × 80 frames of random input
against Lean, field-by-field.

**Equivalence, formal: partial.** Proven under `../verification`:
Areas isomorphisms/index/conversions; Types predicates; Convert functions;
`Duration.abs` value (`abs_spec`) and equality (`abs_eq`) specs;
`duration_abs_fromMicros`; six `decide`-checked constant lemmas;
`ofLnmJudgeGrade_ite`; and the windowed judge equivalences
`judge_tap_equiv`, `judge_touch_equiv` and `judge_slide_classic_equiv` (all free
of custom axioms). See `../verification/README.md` for the module table and the
remaining judge functions.

**Remaining work → `TODO.md`.**

## 6. Regenerating after changing `lnmai-core-verify`

1. `cargo test` (fast) — iterate here.
2. `tools/regen_aeneas.sh` — updates `../verification/Verification/Generated.lean`.
3. `cd ../verification && lake build` — re-check the proofs (slow).

If the generated signatures change, the `admit`-free proofs under
`verification/Verification/*.lean` may need updates; do not commit `sorry`.

## 7. Environment caveat

The verification package imports `LnmaiCore.*`, i.e. all of Mathlib, and the
generated code imports Aeneas. Loaded oleans are ~5.5 GB. On a 16 GB machine
with heavy swap, a single `lake build`/`lean` check costs several minutes.
Do **not** put `lake build` in the inner loop; batch proof changes and run one
background build per batch. Prefer a machine with ≥ 32 GB for proof work.
