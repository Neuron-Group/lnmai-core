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
