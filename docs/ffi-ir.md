# FFI IR Reference

This document describes the JSON-visible data structures used across the `lnmai-core`
FFI transformation pipeline.

It complements `docs/ffi-api.md`, which focuses on exported functions and runtime
workflow.

## Pipeline Overview

The chart pipeline exposed through FFI is:

```text
maidata / Simai text
  -> `FrontendChartInspection`
     - maidata metadata
     - selected chart block
     - source events and source spans
     - raw note tokens
     - semantic slide-note interpretation
  -> `NormalizedChart`
     - semantically normalized notes with unified timing/flag fields
  -> `ChartSpec`
     - runtime-oriented declarative note layout
  -> `GameState`
     - loaded mutable runtime state
  -> `RuntimeStepResult` / `RuntimeStepLightResult`
     - per-frame judgments, commands, score, and optional full state
```

## Encoding Conventions

### Time

- `TimePoint`: signed integer microseconds
- `Duration`: signed integer microseconds

### Rational values

`Rat` is encoded as:

```json
{
  "num": 1,
  "den": 4,
  "decimal": "0.25"
}
```

### Position enums

- `SensorArea`: `"A1"` .. `"A8"`, `"B1"` .. `"B8"`, `"C"`, `"D1"` .. `"D8"`, `"E1"` .. `"E8"`
- `ButtonZone`: `"K1"` .. `"K8"`
- `OuterSlot`: `"S1"` .. `"S8"`

### Optional values

Lean `Option α` becomes either the encoded value or `null`.

## Parser Inspection Layer

This layer is best for editor tooling, parser debugging, and source mapping.

### `ParseError`

Defined in `LnmaiCore/Simai/Syntax.lean`.

Fields:

- `kind`: parse error kind enum
- `rawText`: original offending text fragment
- `message`: human-readable error message
- `span`: optional source span

### `SourcePos`

Fields:

- `line`: 1-based or parser-local line index
- `column`: 1-based or parser-local column index

### `SourceSpan`

Fields:

- `start`: `SourcePos`
- `stop`: `SourcePos`

### `MaidataMetadata`

Fields:

- `fields`: list of key/value metadata pairs from the maidata file

### `MaidataChartBlock`

Fields:

- `levelIndex`: selected chart difficulty index
- `rawBody`: raw chart body text

### `ParsedSlideBody`

Fields:

- `rawText`: raw slide fragment text
- `startLane`: `OuterSlot`
- `kind`: parsed body shape kind
- `endArea`: optional `SensorArea`
- `turnArea`: optional `SensorArea`

### `SlideNoteSemantics`

Fields:

- `rawText`: raw slide note text
- `startSlot`: `OuterSlot`
- `endArea`: `SensorArea`
- `shape`: `SlideShape`
- `isJustRight`: boolean semantic modifier

### `RawNoteToken`

Defined in `LnmaiCore/Simai/Syntax.lean`.

Fields:

- `rawText`: original token text
- `kind`: raw token kind (`tap`, `hold`, `slide`, `touch`, `touchHold`, `rest`, `unknown`)
- `timing`: `TimePoint`
- `bpm`: `Rat`
- `hSpeed`: `Rat`
- `divisor`: note subdivision divisor at that point
- `slot`: optional `OuterSlot`
- `sensorPos`: optional `SensorArea`
- `slideBody`: optional `ParsedSlideBody`
- `length`: optional `Duration`
- `starWait`: optional `Duration`
- `isBreak`: break-note flag
- `isEX`: EX-note flag
- `isHanabi`: hanabi flag
- `isSlideNoHead`: no-head slide flag
- `isForceStar`: force-star flag
- `isFakeRotate`: fake-rotate flag
- `isSlideBreak`: slide-break flag
- `sourceGroupId`: optional slide/touch grouping id
- `sourceGroupIndex`: optional position inside source group
- `sourceGroupSize`: optional source group size
- `sourcePos`: optional `SourceSpan`

### `SourceNote`

Fields:

- `token`: `RawNoteToken`
- `sourcePos`: optional `SourceSpan`

### `SourceEvent`

Fields:

- `timing`: `TimePoint`
- `bpm`: `Rat`
- `hSpeed`: `Rat`
- `divisor`: note divisor
- `notes`: list of `SourceNote`
- `sourcePos`: optional `SourceSpan`

### `SourceChart`

Fields:

- `events`: list of `SourceEvent`

### `FrontendChartInspection`

Fields:

- `metadata`: `MaidataMetadata`
- `chart`: `MaidataChartBlock`
- `source`: `SourceChart`
- `tokens`: list of `RawNoteToken`
- `slideNotes`: list of `SlideNoteSemantics`

## Semantic / Normalized Layer

This layer is the semantically normalized chart IR before runtime lowering.

### `SlideShape`

Fields:

- `canonical`: canonical shape enum/value
- `symmetry`: slide symmetry mode

### `NormalizedSlideDebug`

Fields:

- `noteIndex`: note id shared with normalized/lowered/runtime layers
- `rawText`: original slide text used for debugging

### `NormalizedTap`

Fields:

- `timing`: `TimePoint`
- `slot`: `OuterSlot`
- `isBreak`: boolean
- `isEX`: boolean
- `isHanabi`: boolean
- `isForceStar`: boolean
- `noteIndex`: note id

### `NormalizedHold`

Fields:

- `timing`: `TimePoint`
- `slot`: `OuterSlot`
- `length`: `Duration`
- `isBreak`: boolean
- `isEX`: boolean
- `isHanabi`: boolean
- `noteIndex`: note id

### `NormalizedTouch`

Fields:

- `timing`: `TimePoint`
- `sensorPos`: `SensorArea`
- `isBreak`: boolean
- `isHanabi`: boolean
- `noteIndex`: note id

### `NormalizedTouchHold`

Fields:

- `timing`: `TimePoint`
- `sensorPos`: `SensorArea`
- `length`: `Duration`
- `isBreak`: boolean
- `isEX`: boolean
- `isHanabi`: boolean
- `noteIndex`: note id

### `NormalizedSlide`

Fields:

- `timing`: `TimePoint`
- `slot`: `OuterSlot`
- `length`: `Duration`
- `startTiming`: `TimePoint`
- `hSpeed`: `Rat`
- `slideKind`: runtime slide kind
- `isClassic`: boolean
- `trackCount`: number of logical tracks
- `judgeAt`: optional explicit judge time
- `isBreak`: boolean
- `isEX`: boolean
- `isHanabi`: boolean
- `isSlideNoHead`: boolean
- `isForceStar`: boolean
- `isFakeRotate`: boolean
- `isSlideBreak`: boolean
- `isConnSlide`: boolean
- `parentNoteIndex`: optional parent note id
- `isGroupHead`: boolean
- `isGroupEnd`: boolean
- `totalJudgeQueueLen`: total judge segments
- `judgeQueues`: list of judge queues, each queue a list of `SlideAreaSpec`
- `sourceGroupId`: optional original group id
- `sourceGroupIndex`: optional original group position
- `sourceGroupSize`: optional original group size
- `noteIndex`: note id
- `simaiShape`: `SlideShape`

### `NormalizedChart`

Fields:

- `taps`: list of `NormalizedTap`
- `holds`: list of `NormalizedHold`
- `touches`: list of `NormalizedTouch`
- `touchHolds`: list of `NormalizedTouchHold`
- `slides`: list of `NormalizedSlide`
- `slideDebug`: list of `NormalizedSlideDebug`
- `slideSkipping`: global slide-skipping behavior flag

### `FrontendSemanticChart`

Fields:

- `normalized`: `NormalizedChart`
- `lowered`: `ChartSpec`

### `FrontendChartResult`

Fields:

- `semantic`: `FrontendSemanticChart`
- `inspection`: `FrontendChartInspection`

## Lowered Runtime Chart Layer

This layer is the runtime-ready declarative chart form used to build `GameState`.

### `TapChartNote`

Fields:

- `timing`: `TimePoint`
- `slot`: `OuterSlot`
- `isBreak`: boolean
- `isEX`: boolean
- `buttonQueueIndex`: per-lane queue slot chosen by lowering
- `noteIndex`: note id

### `HoldChartNote`

Fields:

- `timing`: `TimePoint`
- `slot`: `OuterSlot`
- `length`: `Duration`
- `isBreak`: boolean
- `isEX`: boolean
- `isTouch`: whether the lowered hold behaves as touch-hold-like logic
- `isClassic`: optional classic-mode hint
- `buttonQueueIndex`: per-lane queue slot
- `touchHoldGroupId`: optional touch-hold share group
- `touchHoldGroupSize`: optional touch-hold share group size
- `noteIndex`: note id

### `TouchChartNote`

Fields:

- `timing`: `TimePoint`
- `sensorPos`: `SensorArea`
- `isBreak`: boolean
- `touchQueueIndex`: per-area queue slot
- `touchGroupId`: optional simultaneous touch group id
- `touchGroupSize`: optional simultaneous touch group size
- `noteIndex`: note id

### `TouchHoldChartNote`

Fields:

- `timing`: `TimePoint`
- `sensorPos`: `SensorArea`
- `length`: `Duration`
- `isBreak`: boolean
- `isEX`: boolean
- `touchQueueIndex`: per-area queue slot
- `touchGroupId`: optional touch group id
- `touchGroupSize`: optional touch group size
- `touchHoldGroupId`: optional touch-hold group id
- `touchHoldGroupSize`: optional touch-hold group size
- `noteIndex`: note id

### `SlideChartNote`

Fields:

- `timing`: `TimePoint`
- `slot`: `OuterSlot`
- `length`: `Duration`
- `startTiming`: `TimePoint`
- `slideKind`: runtime slide kind
- `isClassic`: boolean
- `isConnSlide`: boolean
- `parentNoteIndex`: optional parent note id
- `isGroupHead`: boolean
- `isGroupEnd`: boolean
- `parentFinished`: lowering/runtime bookkeeping flag
- `parentPendingFinish`: lowering/runtime bookkeeping flag
- `totalJudgeQueueLen`: total judge segments across tracks
- `trackCount`: number of tracks
- `judgeAt`: optional explicit judge time
- `isBreak`: boolean
- `isEX`: boolean
- `noteIndex`: note id
- `judgeQueues`: list of judge queues, each queue a list of `SlideAreaSpec`
- `debugSimai`: optional slide debug tuple

### `ChartSpec`

Fields:

- `taps`: list of `TapChartNote`
- `holds`: list of `HoldChartNote`
- `touches`: list of `TouchChartNote`
- `touchHolds`: list of `TouchHoldChartNote`
- `slides`: list of `SlideChartNote`
- `slideSkipping`: optional global slide-skipping override

## Runtime Input Layer

### `TimedInputEvent`

Variants:

- `buttonClick(tp, zone)`
- `buttonHold(tp, zone, isDown)`
- `sensorClick(tp, area)`
- `sensorHold(tp, area, isDown)`

Meaning:

- click events represent trigger-like press actions
- hold events represent persistent pressed/released state updates

### `TimedInputBatch`

Fields:

- `currentTime`: frame end time as `TimePoint`
- `events`: list of `TimedInputEvent`

Frame inclusion policy used by runtime stepping:

- zero-duration frame includes exactly `currentTime`
- positive-duration frame includes `(prevTime, currentTime]`

## Runtime State Layer

`GameState` is the fully loaded mutable state stored inside Lean handles.

It is useful for debugging, replay tooling, and low-level host inspection, but is
more volatile and more implementation-shaped than the parser IR layers.

### `GameState`

Fields:

- `currentTime`: current runtime time
- `prevButton`: previous-frame button-held vector
- `prevSensor`: previous-frame sensor-held vector
- `buttonQueueFrontiers`: current consumption frontier for each button lane
- `touchQueueFrontiers`: current consumption frontier for each touch area
- `tapQueues`: per-button tap lifecycle queues
- `holdQueues`: per-button hold lifecycle queues
- `touchHoldQueues`: per-sensor touch-hold lifecycle queues
- `touchQueues`: per-sensor touch lifecycle queues
- `slides`: list of active/runtime slide notes
- `activeHolds`: currently active button holds
- `activeTouchHolds`: currently active sensor holds
- `touchGroupStates`: touch-group bookkeeping state
- `touchHoldGroupStates`: touch-hold-group bookkeeping state
- `currentBatch`: last processed `TimedInputBatch`
- `score`: `ScoreState`
- `judgeStyle`: current judge style
- `touchPanelOffset`: timing offset for touch panel logic
- `useButtonRingForTouch`: runtime option flag
- `subdivideSlideJudgeGrade`: runtime option flag

Host guidance:

- do not treat `GameState` as the most stable integration boundary
- prefer `ChartSpec` / step results unless full state inspection is required

## Runtime Output Layer

### `JudgeGrade`

The judgment lattice contains 15 grades:

- `Miss`
- `LateGood`
- `LateGreat3rd`
- `LateGreat2nd`
- `LateGreat`
- `LatePerfect3rd`
- `LatePerfect2nd`
- `Perfect`
- `FastPerfect2nd`
- `FastPerfect3rd`
- `FastGreat`
- `FastGreat2nd`
- `FastGreat3rd`
- `FastGood`
- `TooFast`

### `ScoreState`

Fields:

- `combo`
- `pCombo`
- `cPCombo`
- `totalBase`
- `totalExtra`
- `earnedBase`
- `earnedExtra`
- `lostBase`
- `lostExtra`
- `dxScore`
- `maxDxScore`
- `fastCount`
- `lateCount`
- `counts`: `NoteTypeJudgeCounts`

### `NoteTypeJudgeCounts`

Fields:

- `tapCount`
- `holdCount`
- `slideCount`
- `touchCount`
- `breakCount`

Each of those fields is itself a map-like object keyed by `JudgeGrade` string,
for example:

```json
{
  "Miss": 0,
  "LateGood": 0,
  "LateGreat3rd": 0,
  "LateGreat2nd": 0,
  "LateGreat": 0,
  "LatePerfect3rd": 0,
  "LatePerfect2nd": 0,
  "Perfect": 12,
  "FastPerfect2nd": 1,
  "FastPerfect3rd": 0,
  "FastGreat": 0,
  "FastGreat2nd": 0,
  "FastGreat3rd": 0,
  "FastGood": 0,
  "TooFast": 0
}
```

### `JudgeEvent`

Fields:

- `kind`: note/judge event kind
- `grade`: `JudgeGrade`
- `diff`: signed timing delta as `Duration`
- `position`: runtime position descriptor
- `noteIndex`: note id

### `AudioCommand`

Variants:

- `PlayJudgeSfx(kind, grade, atTime, noteIndex)`
- `PlaySlideCue(noteIndex, trackIndex, atTime)`

### `RenderCommand`

Variants:

- `ShowJudgeResult(kind, grade, diff, noteIndex)`
- `UpdateSlideProgress(noteIndex, remaining)`
- `UpdateSlideTrackProgress(noteIndex, trackIndex, remaining)`
- `HideAllSlideBars(noteIndex)`
- `HideSlideBars(noteIndex, endIndex)`
- `HideSlideTrackBars(noteIndex, trackIndex, endIndex)`

### `RuntimeStepResult`

Fields:

- `state`: `GameState`
- `events`: list of `JudgeEvent`
- `audioCommands`: list of `AudioCommand`
- `renderCommands`: list of `RenderCommand`

### `RuntimeStepLightResult`

Fields:

- `events`: list of `JudgeEvent`
- `audioCommands`: list of `AudioCommand`
- `renderCommands`: list of `RenderCommand`
- `score`: `ScoreState`
- `currentTime`: `TimePoint`

## Stability Guidance

From most stable / integration-friendly to most implementation-shaped:

1. `FrontendChartInspection` for parser tooling
2. `NormalizedChart` for semantic chart tooling
3. `ChartSpec` for runtime-ready chart interchange
4. `RuntimeStepLightResult` for gameplay stepping
5. `GameState` for debugging and deep inspection

For gameplay hosts, the recommended path is:

- chart text -> session load
- per frame `TimedInputBatch`
- consume `RuntimeStepLightResult`
