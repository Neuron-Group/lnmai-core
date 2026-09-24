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

**Done (`sorry`-free, standard axioms only):**
- `judge_tap_equiv` ✅
- `judge_touch_equiv` ✅ (with `judgeTouch_expand`, `option_map_ite`)
- `judge_slide_classic_equiv` ✅ (with `judgeSlideClassic_expand`)

Already available (proven):
- `abs_spec` / `abs_eq` for `time.Duration.abs`
  (precondition `d.micros ≠ IScalar.min .I64`).
- `judgeTapCore` — the shared ℤ-level branch structure.
- `ofLnmJudgeGrade_ite`, `toLnmDuration_toMicros`, `duration_abs_fromMicros`,
  and `decide`-checked constant lemmas for all tap/touch/slide-classic windows.

**Resolved blocker (was: the `Duration.abs` `Result` bind).** The proof pattern
that works for `judge_tap`/`judge_touch`/`judge_slide_classic`:

1. `obtain ⟨v, hv, hvv⟩ := abs_eq diff hmin`
   (`hv : diff.abs = ok ⟨v⟩`, `hvv : v.val = if … then -… else …`).
2. `rw [judge.judge_tap]`, `rw [hv]`, `simp only [bind_tc_ok]`.
3. Rewrite the spec with an `…_expand` lemma proved by `rfl` that exposes
   `LnmaiCore.Duration.abs` (the private `absDiff` is definitionally equal).
4. Normalize to µs-level `ℤ` comparisons using `duration_abs_toMicros`,
   `lnmDuration_le/lt_iff_toMicros`, `i64_le/lt_iff`, the model-constant value
   lemmas, and `← hvv`; then `ofLnmJudgeGrade_ite`, `ok_ite`, and
   `repeat (first | rfl | split)`.
5. For `judge_touch`, additionally eliminate the Bool `&&` guard via
   `Bool.and_eq_true` + `decide_eq_true_eq`, and push `Option.map` through `if`
   (`option_map_ite`); split the sign with `by_cases` and use
   `hfast`/`(diff.micros.val < 0) = False` to reduce the model branch.

**Remaining.** `judge_slide_modern` needs `Duration.divNat`/`scaleNat` (P2); the
two hold-end functions need duration comparisons/additions
(`TimePoint ↔ I64`).

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

with overflow preconditions or an `I64`-range argument.

Notes / progress:
- Bridge lemma `(UScalar.hcast IScalarTy.I64 x).val = x.val` (u32→i64) is
  proved (`Bridge.hcast_i64_val`).
- `scale_nat` and `div_nat` value specs are proved (`Bridge.scale_nat_spec`,
  `Bridge.div_nat_spec`), together with `Duration.toMicros_{add,min,scaleNat,
  divNat}` and `u32_eq_zero_iff`.
- **`div_nat` does NOT mirror `Int.ediv`.** Aeneas's division spec
  (`IScalar.div_spec`) is `Int.tdiv` (truncation toward zero), while the spec's
  `Duration.divNat` uses Lean `Int./`, i.e. Euclidean division. They coincide
  exactly when the dividend is `≥ 0`. State the equivalence with
  `0 ≤ dividend.micros.val` (modern slides pass a nonnegative `stay_time`).
  The model returns `0` for divisor `0`; the spec does too.
- `Add`/`Sub`/`from_millis` are still open.

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

- The harness only feeds **single-difficulty synthetic** charts (all
  `&inote_1=`) that are mostly one measure. FFI testing against a real
  multi-difficulty, multi-measure chart
  (`~/.maichart/324_Jack-the-Ripper◆`) exposed — and then fixed — two bugs:
  1. the Rust port had no `default_tactic_from_chart` (implemented in
     `src/default_tactic.rs` + FFI/C ABI);
  2. `collect_chart_body` reversed the chart body lines, so measures were
     lowered in reverse order (fixed; regression test
     `multi_measure_body_keeps_source_order`).
  With both fixed the Rust parse now matches Lean for **every** level of that
  chart, and the default tactic is byte-identical. Add a differential case that
  parses a captured real `&inote_N` block so this stays covered.
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
