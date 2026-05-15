# Refactor Conductor

This document records how to conduct semantic-parity refactors in `lnmai-core`, and now also serves as the running incident log for the MajdataPlay parity investigation.

## Goal

The conductor rule is:

- reproduce concretely
- reduce the case
- compare against reference behavior
- add a Lean regression
- patch the root cause
- only then generalize

## Standard loop

For runtime-semantic refactors, use this order:

1. observe a concrete failure on a real chart or focused replay
2. localize the failing note family or interaction
3. inspect `../reference/MajdataPlay` for the relevant runtime rule
4. extract a reduced fragment into `tools/majdata-harness`
5. run the harness under `nix-shell`
6. encode a minimal Lean regression in `LnmaiCore/RuntimeTests.lean`
7. patch the runtime or proof-wrapper root cause
8. rebuild and recheck the real chart

## Real-chart incident

### Assets

- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt`
- `tools/assets/834_PANDORA PARADOXXX/maidata.txt`

### Initial symptom

On level 2 of the real-chart checkpoints, the chart-derived default tactic originally failed with missing judged notes and later with non-perfect hold/slide mismatches.

The first visible failing clusters were:

- regular hold notes `57, 58, 94, 97, 101, 104`
- a remaining slide mismatch on `11358` note `61`
- one remaining slide mismatch on `834`

The first visible pattern in chart text was:

- `1h[2:1]/8h[2:1]`

followed by a short slide run.

## Detailed reproduce path

This is the reproduction ladder that was used to isolate the gap.

### Rung 1 — Real chart fails

Use the real chart with:

- `Simai.compileLowered content 2`
- `defaultTacticFromChart`
- `simulateChartSpecWithTactic`

Observed originally:

- missing judged notes on several short regular holds

### Rung 2 — Reduced reference harness

Inspect `MajdataPlay` hold behavior in:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs`

Extract reduced scenarios into:

- `tools/majdata-harness/src/ReferenceLikeLogic.cs`
- `tools/majdata-harness/src/ScenarioLibrary.cs`

Reference-shaped hold findings:

- hold head requires a click edge
- body sustain uses held state
- short release is ignored briefly
- final hold result is decided at hold end

### Rung 3 — Minimal manual Lean instance

Add a direct manual-batch Lean regression for two simultaneous short regular holds.

Current status:

- this minimized manual replay passes

Relevant test:

- `LnmaiCore/RuntimeTests.lean` — `test_simultaneous_short_regular_holds_can_both_finish`

Interpretation:

- the raw hold lifecycle can succeed in isolation

### Rung 4 — Chart-wrapper instance

Use the same short hold pair, but construct it as a chart and simulate it through:

- `defaultTacticFromChart`
- `simulateChartSpecWithTactic`

Current status before the activation fix:

- this was the first reduced rung that failed

Interpretation:

- the gap was not the raw hold lifecycle alone
- the gap involved the chart/proof-wrapper path

### Rung 5 — Root cause found

Inspection of chart-built state showed:

- `holdQueues` were populated
- `activeHolds` was empty
- `touchHoldQueues` were populated
- `activeTouchHolds` was empty

Since the scheduler processes holds from active lists, chart-built holds were never actually stepped.

That was the first concrete runtime/proof-wrapper root cause.

## Fixed gaps

### Fixed gap 1 — Chart-built hold activation

Problem:

- chart-built regular holds and touch-holds were placed into queues
- but were not seeded into `activeHolds` / `activeTouchHolds`

Fix:

- seed active hold lists from chart-built hold queues in `ChartLoader.buildGameState`

Result:

- the earlier real-chart missing judged notes disappeared
- the reduced chart-wrapper hold instance now succeeds

### Fixed gap 2 — Wrapper settle sequencing

Problem:

- `simulateChartSpecWithTactic` previously built settle batches from the wrong timeline position

Fix:

- generate settle batches from the tactic-result time/state instead of the initial state

Result:

- wrapper replay order is now semantically correct

### Fixed gap 3 — Click-consumption gating for tap/touch vs hold heads

Problem:

- hold heads were still being semantically missed in cases where MajdataPlay click-consumption order should let the correct note family consume the edge first

Reference inspection:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TapDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`

Fix:

- add explicit click-eligibility gating in `LnmaiCore/Scheduler.lean`

Result:

- the short-hold head cluster no longer blocks AP

### Fixed gap 4 — Modern hold end for short real-chart holds

Problem:

- some short modern holds still ended one frame too late and degraded final results

Reference inspection:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/NoteLongDrop.cs`

Fix:

- in `LnmaiCore/Lifecycle.lean`, modern holds now end immediately once the body check window is past or the note length is exhausted

Result:

- `11358` short holds `57, 58, 94, 97, 101, 104` now judge `Perfect`

### Fixed gap 5 — Final slide-step zero-length generator bug

Problem:

- the generated default slide tactic produced a same-timestamp down/up pair for the final slide step
- in full-chart overlap, that collapsed into a false early slide finish and produced a remaining `FastGreat2nd` / `FastGreat`

Reference-first reduction result:

- local queue traversal already matched MajdataPlay
- the real mismatch was in generated input timing, not queue semantics

Fix:

- in `LnmaiCore/RuntimeProofAPI.lean`, keep the last generated slide step held through the judge frame and release one frame later

Result:

- both real-chart checkpoints now AP cleanly

### Fixed gap 6 — Modern hold missed-head fallback release semantics

Problem:

- after a missed/too-fast modern hold head, MajdataPlay skips the short release-ignore grace and starts released-body accounting immediately
- our runtime still gave that grace window

Reference inspection:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`

Fix:

- in `LnmaiCore/Lifecycle.lean`, missed-head modern holds now enter released-body accounting immediately on the first unpressed body frame

### Fixed gap 7 — Touch-hold released-body recovery

Problem:

- once a touch-hold entered `BodyReleased`, our runtime did not let it recover back to held if sensor input or body-group majority returned
- MajdataPlay does

Reference inspection:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`

Fix:

- in `LnmaiCore/Lifecycle.lean`, `BodyReleased` now re-enters `BodyHeld` when body input becomes pressed again before force-end

## Verified semantics added during the conductor loop

The following items are now explicitly reference-verified and covered by reduced tests or harness scenarios:

- slide area-skip law for skippable chains
- tap/touch click-consumption ordering vs hold heads
- modern hold missed-head fallback release semantics
- short modern hold finalization timing
- final slide-step generator timing
- touch-hold released-body recovery
- classic hold strict fast/late perfect boundaries
- touch-hold shared-head strict majority rule
- wifi / conn-slide `QueueRemaining` and `ParentPendingFinish` using max remaining track length
- overlapping slides sharing one sensor hold legitimately

## Important non-bugs

These behaviors were explicitly rechecked against `../reference/MajdataPlay` and should not be “fixed” away:

- overlapping slides can both progress from one held sensor because slide progress reads sensor status, not a consumable click event
- wifi too-late can still be `LateGood` when two tracks each have one tail left, because the reference uses max remaining track length

## Current real-chart status

For the current real-chart checkpoints:

- both charts are full-note judged
- both charts achieve AP under the current default generated tactic

This means the incident has moved from “missing-note / semantic breakage” into targeted parity hardening and regression preservation.

## What is already good and should not be treated as open work

The following items are already in good shape for this incident and should not be re-opened casually:

- reduced Majdata hold-body harness scenarios
- manual simultaneous-short-hold Lean regression
- chart-built hold activation
- wrapper settle sequencing
- missing-note debug helpers in `RuntimeProofAPI`
- reduced slide-skip harness scenarios
- real-chart verification executable
- overlap-sharing slide regressions

## Current verification order

When continuing the parity effort, use this order:

1. inspect the exact MajdataPlay rule first
2. reduce a real or synthetic case into `tools/majdata-harness`
3. mirror it in `LnmaiCore/RuntimeTests.lean`
4. patch only a proven mismatch
5. re-run real-chart verification

Current likely next frontier:

- dense real-chart overlap reductions for wifi / conn-slide / shared-sensor slide clusters

## Commands

- Lean build: `lake build LnmaiCore`
- Full build: `lake build`
- Harness run: `cd tools/majdata-harness && nix-shell --run 'dotnet run --project MajdataHarness.csproj'`
- Real-chart verification: `lake exe real-chart-verification`

## Final rule

When in doubt:

- reduce first
- prove second
- patch third
- generalize last
