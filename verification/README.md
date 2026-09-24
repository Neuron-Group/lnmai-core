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

Toolchain:

- Aeneas `ac74e1b` (Lean backend `v4.30.0-rc2`).
- Charon `0.1.210` (pinned by `charon-pin`).

The Aeneas Lean backend is referenced by absolute path in
`verification/lakefile.toml`. Point it at your checkout with:

```bash
AENEAS_HOME=/path/to/aeneas verification/tools/wire_aeneas.sh
```

On the current Linux machine it is wired to
`/home/pingfanh/aeneas/backends/lean`.

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
| Time | — | `Duration.abs` ✅ (`abs_spec`, `abs_eq`); `duration_abs_fromMicros` ✅; `scale_nat`/`div_nat` value specs ✅ (with u32→i64 `hcast` bridge); `toMicros_add/min/scaleNat/divNat` ✅ | M1b/P2 |
| Judge | — | `correct_slide_grade` ✅; `judge_tap` ✅, `judge_touch` ✅, `judge_slide_classic` ✅, `judge_slide_modern` ✅, `judge_slide_too_late` ✅, `is_too_late_slide` ✅; hold-ends pending | M1d |
| Score | — | `base_score` ✅, `score_non_break` ✅, `update_combo` ✅; `score_break`/`dx_score_rank`/`count_fast_late` pending | M1d |

### Handling the `Duration.abs` bind (previously a blocker)

`judge.judge_tap` (and its siblings) bind the result of the generated
`time.Duration.abs`, which returns `Result`. Aeneas `step` cannot execute that
bind without knowing the sign, so it used to stop and leave a
`spec_general`-style goal.

The working approach (now used for `judge_tap_equiv` and
`judge_slide_classic_equiv`, both `sorry`-free):

1. Extract the model result and its value:
   `obtain ⟨v, hv, hvv⟩ := abs_eq diff hmin`, where
   `hv : diff.abs = ok ⟨v⟩` and `hvv : v.val = if … then -… else …`.
2. `rw [judge.judge_tap]`, `rw [hv]`, then `simp only [bind_tc_ok]` reduces the
   bind to `let abs_diff := ⟨v⟩`.
3. Push the spec's private `absDiff` out of the way by proving an `…_expand`
   lemma whose RHS mentions `LnmaiCore.Duration.abs` directly (proved by
   `rfl`, since `absDiff` is definitionally `Duration.abs`).
4. Normalize both sides to plain `ℤ` comparisons on µs values with the helper
   lemmas `duration_abs_toMicros`, `lnmDuration_*_iff_toMicros`,
   `i64_le_iff`/`i64_lt_iff` and the model-constant value lemmas; align the
   `abs` value with `hvv`; then distribute `ofLnmJudgeGrade`/`ok` over the
   nested `if`s (`ofLnmJudgeGrade_ite`, `ok_ite`) and close every leaf with
   `repeat (first | rfl | split)`.

Remaining windowed functions need the same pattern plus extra bridges:
`judge_slide_modern` (`Duration.divNat`/`scaleNat`), and the hold releases
(time comparison / addition). `judge_touch` is also done: it additionally
normalizes the spec's `Bool` `&&` guard (`Bool.and_eq_true` +
`decide_eq_true_eq`) and pushes `Option.map` through `if`
(`option_map_ite`).

**`divNat` division-semantics caveat.** The spec's `Duration.divNat` uses Lean's
Euclidean `/` on `ℤ` (`(-7)/2 = -4`); the Rust/model op uses Aeneas's
`Int.tdiv` (truncate toward zero, `(-7).tdiv 2 = -3`). They agree only for
nonnegative dividends. Modern-slide callers pass a nonnegative wait time, so
`judge_slide_modern_equiv` should carry `0 ≤ stay_time.micros.val`.

Build: `lake build` (requires the `aeneas` and `lnmai-core` packages wired in
`lakefile.toml`), or `tools/build.sh` in the background.

## Environment notes (important)

Every proof module imports `LnmaiCore.*`, which imports all of Mathlib, and
generated code imports Aeneas. Loaded oleans are ~5.5 GB. On a 16 GB machine
(with swap already used) a single Lean check can take several minutes; on a
32 GB+ machine (the current Linux box) a full `lake build` of the package takes
roughly a minute once Mathlib is cached. Consequences:

- Do not put `lake build` in the inner development loop on a small machine.
- Batch proof changes and run one background build per batch.
- For regular (fast) equivalence checking, prefer the Rust port's tests and the
  differential harness rather than Lean proofs.

On NixOS the `elan`-installed `lake`/`lean` binaries do not run directly
(dynamic-linker mismatch). Run them through an FHS wrapper, e.g.
`nix shell --impure nixpkgs#steam-run -c steam-run lake build` (with
`~/.elan/bin` on `PATH`, `NIXPKGS_ALLOW_UNFREE=1`).

