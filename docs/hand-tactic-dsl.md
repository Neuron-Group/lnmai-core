# Hand-Tactic DSL Proposal

This document replaces the old refactor placeholder with a concrete proposal for a proof-facing hand-tactic DSL.

## Goal

Users should be able to describe a manual play tactic as a timeline of physical actions, then feed that script into the Lean runtime proof wrapper and prove that a chart section can be cleared with `ALL PERFECT` or `ALL PERFECT+`.

The DSL should model what the player actually does:

- click a button
- click a sensor area
- press or release a button
- press or release a sensor
- hold inputs across time spans

The DSL is proof-facing first, but should stay close to runtime semantics so it remains suitable for executable simulation.

## Semantic model

A hand tactic is a sequence of timestamped trigger actions.

Each action happens at a concrete microsecond timestamp and is interpreted as a runtime input event. The current runtime proof API already supports these atomic events:

- `buttonClick`
- `sensorClick`
- `buttonHold ... true|false`
- `sensorHold ... true|false`

So the DSL should elaborate into `ManualTacticSequence`, which is just a sorted list of `TimedInputEvent` values.

## Current concrete syntax

The current parser already supports one action per line:

```text
500000 tap K1
516000 touch A1
750000 button K1 down
900000 button K1 up
1000000 sensor A1 down
1100000 sensor A1 up
```

Meaning:

- `tap K1` → one button click event
- `touch A1` → one sensor click event
- `button K1 down` → start holding button `K1`
- `button K1 up` → stop holding button `K1`
- `sensor A1 down` → start holding sensor `A1`
- `sensor A1 up` → stop holding sensor `A1`

Comments and blank lines are ignored.

## Proposed ergonomic extensions

The current syntax is correct but too low-level for longer proofs. The next DSL layer should add interval-oriented sugar while preserving the same elaboration target.

### 1. Hold intervals

```text
hold button K1 from 750000 to 900000
hold sensor A1 from 1000000 to 1100000
```

Elaboration:

- `hold button K1 from a to b`
  - `a button K1 down`
  - `b button K1 up`
- `hold sensor A1 from a to b`
  - `a sensor A1 down`
  - `b sensor A1 up`

### 2. Chords

```text
500000 tap K1 K2 K3
516000 touch A1 A2
```

Elaboration:

- one event per listed zone at the same timestamp

### 3. Named macros

```text
let left_wall = touch A1 A8
500000 use left_wall
```

This is optional, but useful once proofs become larger than a few lines.

### 4. Section grouping

```text
section intro {
  500000 tap K1
  516000 touch A1
}
```

This is documentation-oriented sugar and does not need semantic weight initially.

## Deliberate non-goals

The DSL should not initially attempt to encode:

- hand geometry
- left/right hand ownership
- finger count constraints
- motion continuity constraints
- automatic derivation of tactics from note patterns

Those belong to a higher-level tactic language built on top of this event DSL.

## Proof integration

The intended proof flow is:

1. build or quote a `ChartSpec`
2. write a hand tactic with `manual_tactic!`
3. simulate with `simulateChartSpecWithTactic`
4. prove `achievesAP` or `achievesAPPlus`

Example:

```lean
def exampleDelayedSingleTapSection : ChartLoader.ChartSpec :=
  { taps :=
      [ { timing := TimePoint.fromMicros 500000
        , slot := OuterSlot.S1
        , isBreak := false
        , isEX := false
        , noteIndex := 1 } ]
  , holds := []
  , touches := []
  , touchHolds := []
  , slides := []
  , slideSkipping := true }

def exampleDelayedSingleTapButtonTactic : ManualTacticSequence :=
  manual_tactic! "500000 tap K1"

theorem exampleDelayedSingleTapButtonTactic_achievesAP :
    achievesAP (
      simulateChartSpecWithTactic
        exampleDelayedSingleTapSection
        exampleDelayedSingleTapButtonTactic
    ) = true := by
  native_decide
```

## Runtime alignment requirements

For this DSL to stay trustworthy, the proof wrapper must preserve runtime logic flow.

That means:

- intermediate frames between tactic timestamps must be simulated
- click events must be consumed on the frame where they occur
- tap semantics must distinguish button click from sensor click exactly as runtime does
- sensor timing must respect touch panel offset where the runtime does

This has already shaped the current wrapper design and should remain a hard requirement for future DSL expansion.

## Recommended next implementation steps

- extend `manual_tactic!` parser with interval sugar
- add chord syntax for same-timestamp multi-input actions
- add pretty-printer support for `ManualTacticSequence`
- add proof examples for tap, hold, touch, and simple slide sections
- later, consider a higher-level tactic layer for hand assignment and motion constraints

## Chart-derived timing skeletons

The proof API now also supports a chart-derived timing skeleton layer for reducing timestamp boilerplate in proofs.

Useful functions in `LnmaiCore.Proofs.Runtime`:

- `chartTimingSkeleton : ChartSpec → List NoteTimingSkeleton`
- `resolveDefaultTimingSkeleton : NoteTimingSkeleton → ManualTacticSequence`
- `resolveDefaultTimingSkeletonList : List NoteTimingSkeleton → ManualTacticSequence`
- `defaultTacticFromChart : ChartSpec → ManualTacticSequence`
- `resolveTimingSkeletonWithOverrides : List TimingSkeletonOverride → NoteTimingSkeleton → ManualTacticSequence`
- `resolveTimingSkeletonListWithOverrides : List TimingSkeletonOverride → List NoteTimingSkeleton → ManualTacticSequence`
- `tacticFromChartWithOverrides : ChartSpec → List TimingSkeletonOverride → ManualTacticSequence`

Intended workflow:

1. lower a real chart section into `ChartSpec`
2. derive a timing skeleton from the chart
3. use default resolvers for easy note families
4. override only the suspicious or hand-sensitive notes when needed

The current default resolvers intentionally stay simple:

- tap → click at note timing
- hold → press at head timing, release at tail timing
- touch → sensor click at offset-adjusted timing
- touch-hold → sensor press at head timing, release at tail timing
- slide with head → trigger the head, wait for `startTiming`, then walk a representative single-track path with evenly spaced sensor holds until the slide end

So this layer already covers all major runtime note families, not only slides. Slides just need the richest default resolver shape.

This is meant as proof scaffolding, not a full auto-player. The value is that proofs over real charts can start from chart-derived timing structure instead of hand-writing every microsecond.

Example shape:

```lean
def chart : ChartLoader.ChartSpec :=
  simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1,\n"

def tactic : ManualTacticSequence :=
  defaultTacticFromChart chart

#eval chartTimingSkeleton chart
#eval tactic
```

For harder charts, a useful pattern is:

- derive `chartTimingSkeleton chart`
- map over the skeleton
- keep the default resolver for most notes
- replace only selected slide or multi-note sections with custom tactics

The current override surface is `noteIndex`-based, so any note family can be replaced selectively while all other notes still use chart-derived defaults.

Example shape:

```lean
def overrides : List TimingSkeletonOverride :=
  [ fixedTimingSkeletonOverride 42 (manual_tactic! "1500000 tap K3") ]

def tactic : ManualTacticSequence :=
  tacticFromChartWithOverrides chart overrides
```

## Current status on the real chart asset

The repository now wires the chart-derived tactic helpers to the real chart at:

- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt`

Important caveat:

- the current fully automatic default tactic generator does **not** yet prove a full clear on that chart as-is
- this means a theorem claiming that `defaultTacticFromChartSection` solves the full real chart would currently be false

Current remaining gap split after recent runtime fixes:

- chart-generated short regular holds are no longer missing entirely, but some still end as miss-style `LateGood`, which points to hold-head judgment/eligibility timing rather than hold-body sustain
- several following slides are still judged too fast, which points to default slide resolver pacing being early on those shapes

The detailed reproduction ladder and incident history now live in `doc/refactor-conductor.md`.

To support the next tightening step, the proof API now exposes simulation-debug helpers:

- `chartNoteIndices : ChartSpec → List Nat`
- `judgedNoteIndices : RuntimeSimulationResult → List Nat`
- `missingJudgedNoteIndices : RuntimeSimulationResult → List Nat`
- `missingJudgedNoteCount : RuntimeSimulationResult → Nat`

These are intended to help identify the exact remaining `noteIndex` gaps on real charts, so the tactic resolver or note-family defaults can be improved with concrete evidence.
