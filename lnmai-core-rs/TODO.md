# TODO — completing full Rust ↔ Lean formal equivalence

Context: the Rust rewrite is complete and the differential harness already
checks large parts of behaviour against Lean. This file lists the remaining
*formal* (Aeneas + Lean) obligations, in priority order, with concrete starting
points so another agent can finish them.

## Conventions

- The verifiable Rust crate is `crates/lnmai-core-verify`. Changes there are
  regenerated into `../verification/Verification/Generated.lean` by
  `tools/regen_aeneas.sh`.
- Lean proofs live in `../verification/Verification/`. Never leave `sorry` in a
  claimed-complete module.
- `../verification/README.md` holds the per-module status table and the
  environment caveat (Mathlib load ≈ minutes/check).
- Use `#print axioms <thm>` to confirm a proof does not depend on `sorryAx`.

---

## P0 — Judge window functions (`LnmaiCore/Judge.lean`)

Target lemmas (in `../verification/Verification/Judge.lean`):

```lean
judge_tap_equiv, judge_touch_equiv, judge_slide_classic_equiv,
judge_slide_modern_equiv, judge_hold_end_equiv, judge_hold_classic_end_equiv,
is_too_late_slide_equiv, judge_slide_too_late_equiv
```

Already available (proven):
- `abs_spec` / `abs_eq` for `time.Duration.abs`
  (precondition `d.micros ≠ IScalar.min .I64`).
- `judgeTapCore` — the shared ℤ-level branch structure.
- `ofLnmJudgeGrade_ite`, `toLnmDuration_toMicros`, `duration_abs_fromMicros`,
  and six `decide`-checked constant lemmas
  (`tapPerfect1Ms_val` … `tapGreat3Ms_val`).

**Known blocker.** `judge.judge_tap` binds `time.Duration.abs` (a `Result`).
Aeneas `step` stops at that bind and leaves a `spec_general`-style goal
(`r_post : r = expected`); closing it from that hypothesis is circular.

**Recommended approach (proven viable, partially done).**
1. Extract the abs value: `obtain ⟨v, hv, hvv⟩ := abs_eq diff hmin`.
2. `rw [judge.judge_tap, hv]`, then reduce the bind. NOTE: Aeneas's
   `bind_ok` is `@[simp]`; the `do`-block only reduces if the rewrite actually
   fired — verify `hv` matched (it must appear as `time.Duration.abs diff`).
3. Split on `is_ex` (`cases hb : is_ex` — Bool yields `false` first!), then
   `by_cases hm : diff.micros.val < 0`, then split the remaining threshold
   ifs. Normalize `v.val` with `hvv` and the model constants
   (`constants.TAP_PERFECT_1ST`, …), and distribute
   `ofLnmJudgeGrade` with `ofLnmJudgeGrade_ite`.
4. Close each branch with `rfl`.

If the tactic-of-blind-simp approach keeps failing, do it **exhaustively and
manually**: a 6-deep `by_cases` on the thresholds with a `rfl` per leaf. It is
verbose but does not depend on `simp` heuristics. Each build is minutes, so
prepare the whole proof before building.

Alternative (may be simpler): make the Rust `Duration.abs` avoid the `Result`
bind (return the struct directly) in `crates/lnmai-core-verify/src/time.rs`,
regenerate, and re-prove `abs_spec`. This trades proof pain for a model change;
check that `step` can then execute `judge_tap` outright.

`judge_slide_classic` uses the same shape with different constants
(`SLIDE_JUDGE_CLASSIC_*`), and `judge_slide_modern` adds
`Duration.divNat`/`scaleNat` — prove those `Time` ops first (P2).

## P1 — Score functions (`LnmaiCore/Score.lean`)

Target: `scoreNonBreak_equiv`, `scoreBreak_equiv`, `updateCombo_equiv`,
`countFastLate_equiv`, `dxScoreRank_equiv`.

`base_score_equiv` is already proven. The others combine `u32` arithmetic with
enum dispatch. Strategy: state each with explicit no-overflow preconditions
(e.g. `base.val * multiple.val < 2^32`, `b.val * 4 < 2^32`) and use the Aeneas
`U32.*_spec` lemmas; the differential tests already fix the expected values.

`countFastLate` needs `Duration` comparisons (`== 0`, `< 0`) — reuse the
`I64` comparison simp lemmas.

## P2 — Time arithmetic (`LnmaiCore/Time.lean`)

Targets: `scale_nat`, `div_nat`, the `Add`/`Sub` impls, and `from_millis`.
Prove value specs mirroring `abs_spec`, i.e.

```lean
time.Duration.scale_nat d factor ⦃ r => r.micros.val = d.micros.val * factor.val ⦄
```

with overflow preconditions or an `I64`-range argument. `div_nat` mirrors
`Int.ediv` (truncation toward zero) and returns `0` for divisor `0`.

## P3 — Areas.rotate

`SensorArea.rotate` / `ButtonZone.rotate` / `OuterSlot.rotate` equivalence.
Lean uses `((n-1)+steps) % 8` in `Nat`; the Rust model uses `usize` arithmetic.
State equivalence modulo `steps % 8`, and reuse the `ring_*` tables. Also prove
`SensorArea.rotate`'s group-preservation (A/B/D/E rings independent, `C` fixed).

## P4 — Constants batch

Prove (by `decide`/`norm_num`) that every model constant equals its Lean
counterpart's `.toMicros`, for all of `constants.rs` (only six tap constants
are done). Add them as `@[simp]` lemmas so later proofs normalize constants.

## P5 — Broaden differential coverage (fast, always worth it)

- Add charts for: classic holds, `#`-timed segments, `$`/`!`/`?` flags,
  same-head `*` groups under chords, large slide chains.
- Longer random sessions (hundreds of frames) and multiple charts per run.
- Assert full-state invariants that both sides can express identically
  (score, per-grade counts, `currentTime`, queue frontiers). The *whole*
  `GameState` JSON currently uses this port's own schema; comparing it to Lean
  requires mirroring Lean's derived encoding for every note type.

## P6 — Runtime formal verification (large; scope deliberately)

`Storage`, `Lifecycle`, `Scheduler`, `InputModel` are not in the verify crate
because they use `Vec`/`Option`/generics. Options:
- extract a `no_std`-style, `Vec`-based subset to `lnmai-core-verify` and prove
  the queue/state transitions; or
- treat differential testing as the primary evidence for the runtime and keep
  the formal layer on the pure core.
Decide and document the boundary explicitly.

## P7 — CI / reproducibility

- Add a CI job that runs `cargo test` and `tools/run_differential.sh` (building
  the Lean CLIs).
- For proof CI, use a ≥ 32 GB runner; add `verification/tools/build.sh` to a
  workflow.
- Keep `verification/lakefile.toml`'s Aeneas path documented (see README §3);
  consider a setup script that rewrites it from `AENEAS_HOME`.

---

## Handy commands

```bash
# fast inner loop
cd lnmai-core-rs && cargo test

# regenerate Aeneas model
lnmai-core-rs/tools/regen_aeneas.sh

# check proofs (slow)
cd ../verification && lake build

# differential (needs Lean CLIs)
cd .. && lake build simai-parser-cli runtime-step-cli
lnmai-core-rs/tools/run_differential.sh

# no-axiom check for a theorem
#   #print axioms Verification.JudgeProof.abs_eq
```
