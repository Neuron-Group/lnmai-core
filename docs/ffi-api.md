# FFI API

This document describes the currently implemented Lean-side FFI of `lnmai-core`.

It is an API reference for the exported symbols that exist today, their input and
output contracts, and the intended host-side calling pattern.

## Scope

Implemented in:

- `LnmaiCore/FFI.lean`

Core runtime logic behind the FFI lives in:

- `LnmaiCore/Simai/Frontend.lean`
- `LnmaiCore/ChartLoader.lean`
- `LnmaiCore/Scheduler.lean`

Generated exported C symbols can be inspected in:

- `.lake/build/ir/LnmaiCore/FFI.c`
- `.lake/build/ir/LnmaiCore/FFI.c:470`

Public host-side declarations added in this repo:

- `include/lnmai_ffi.h`
- `bindings/rust/mod.rs`

## Runtime Model

The implemented FFI has two layers:

- parse/lower APIs that operate on chart text and JSON payloads
- runtime APIs that operate either on JSON state snapshots or on opaque `UInt64` handles

The preferred runtime API is the handle-based API.

## Threading

Runtime handle access is serialized inside Lean with `Std.Mutex` in `LnmaiCore/FFI.lean`.

This supports the recommended host workflow:

- collect frame input events on the host
- package them into a `TimedInputBatch` JSON payload
- submit a single runtime-step job to a dedicated Lean worker thread
- perform unrelated host-side work in parallel if needed
- wait for the Lean step result
- consume returned judgment and command outputs
- advance to the next frame

Recommended rule:

- use one dedicated runtime worker thread per process
- do not intentionally issue overlapping step calls for the same handle

## Common Encoding

### Strings

- all exported parse and runtime APIs return a JSON string

### Time values

Defined in:

- `LnmaiCore/Time.lean`
- `LnmaiCore/Time.lean`

Encoding:

- `Duration` is a signed integer microsecond count
- `TimePoint` is a signed integer microsecond count

Host rule:

- treat all FFI time values as `int64` microseconds

### Area and slot enums

Defined in `LnmaiCore/Areas.lean`.

Encoding:

- `SensorArea`: strings like `"A1"`, `"B4"`, `"C"`, `"E8"`
- `ButtonZone`: strings like `"K1"` .. `"K8"`
- `OuterSlot`: strings like `"S1"` .. `"S8"`

## Common Response Envelope

All exported APIs return a JSON object with the following top-level shape.

### Success

```json
{
  "ok": true,
  "result": { "...": "payload" }
}
```

### Error

```json
{
  "ok": false,
  "error": {
    "code": "string",
    "message": "string"
  },
  "details": { "...": "optional structured payload" }
}
```

The `details` field is currently used for structured parse errors.

## Exported Functions

## Parse APIs

### `lnmai_parse_frontend_chart_json`

Symbol:

- `lnmai_parse_frontend_chart_json` in `.lake/build/ir/LnmaiCore/FFI.c`

Lean definition:

- `LnmaiCore/FFI.lean`

Signature:

- input: chart text `String`, level index `UInt32`
- output: JSON envelope string

Success payload:

- `FrontendChartResult`

### `lnmai_parse_frontend_semantic_chart_json`

Lean definition:

- `LnmaiCore/FFI.lean`

Success payload:

- `FrontendSemanticChart`

### `lnmai_parse_frontend_inspection_chart_json`

Lean definition:

- `LnmaiCore/FFI.lean`

Success payload:

- `FrontendChartInspection`

### `lnmai_parse_normalized_chart_json`

Lean definition:

- `LnmaiCore/FFI.lean`

Success payload:

- `NormalizedChart`

### `lnmai_parse_lowered_chart_json`

Lean definition:

- `LnmaiCore/FFI.lean`

Success payload:

- `ChartSpec`

### Parse errors

Defined in `LnmaiCore/Simai/Syntax.lean` and `LnmaiCore/Simai/Syntax.lean`.

Current parse error code:

- `parse_error`

Structured `details` payload:

- `ParseError`

## JSON-State Runtime APIs

These APIs are implemented and valid, but are primarily useful for debugging,
tooling, and integration bring-up. The preferred gameplay API is the handle API.

### `lnmai_build_game_state_json`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `ChartSpec` JSON string

Success payload:

- `GameState`

Error code:

- `invalid_chart_spec_json`

### `lnmai_step_game_state_json`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `GameState` JSON string
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepResult`

Error code:

- `invalid_runtime_json`

## Handle Runtime APIs

These are the preferred gameplay/runtime APIs.

### `lnmai_create_game_state_handle`

Symbol:

- `lnmai_create_game_state_handle` in `.lake/build/ir/LnmaiCore/FFI.c`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `ChartSpec` JSON string

Success payload:

```json
{
  "handle": 1
}
```

Notes:

- handle type is `UInt64`
- handle values are process-local
- handle values are not stable across restarts

Error code:

- `invalid_chart_spec_json`

### `lnmai_free_game_state_handle`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `UInt64` handle

Success payload:

```json
{
  "freed": true
}
```

Error code:

- `invalid_runtime_handle`

### `lnmai_get_game_state_json_by_handle`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `UInt64` handle

Success payload:

- `GameState`

Use:

- runtime inspection
- debugging
- state snapshot export

Error code:

- `invalid_runtime_handle`

### `lnmai_step_game_state_handle`

Symbol:

- `lnmai_step_game_state_handle` in `.lake/build/ir/LnmaiCore/FFI.c`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `UInt64` handle
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepResult`

Error codes:

- `invalid_runtime_json`
- `invalid_runtime_handle`

### `lnmai_step_game_state_handle_light`

Symbol:

- `lnmai_step_game_state_handle_light` in `.lake/build/ir/LnmaiCore/FFI.c`

Lean definition:

- `LnmaiCore/FFI.lean`

Input:

- `UInt64` handle
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepLightResult`

This is the recommended per-frame API for gameplay hosts because it avoids
returning the full serialized `GameState` each frame.

Error codes:

- `invalid_runtime_json`
- `invalid_runtime_handle`

## Payload Types

## `ChartSpec`

Defined in `LnmaiCore/ChartLoader.lean`.

Fields:

- `taps`
- `holds`
- `touches`
- `touchHolds`
- `slides`
- `slideSkipping`

This is the runtime-construction input type.

## `TimedInputBatch`

Defined in `LnmaiCore/InputModel.lean`.

Fields:

- `currentTime : TimePoint`
- `events : List TimedInputEvent`

### `TimedInputEvent`

Defined in `LnmaiCore/InputModel.lean`.

Constructors:

- `buttonClick(tp, zone)`
- `buttonHold(tp, zone, isDown)`
- `sensorClick(tp, area)`
- `sensorHold(tp, area, isDown)`

Frame-window semantics are implemented by `stepFrameTimed` via `TimedInputBatch.toFrameInput`.

Event inclusion policy:

- zero-duration frame includes exactly `currentTime`
- positive-duration frame includes `(prevTime, currentTime]`

See `LnmaiCore/InputModel.lean`.

## `RuntimeStepResult`

Defined in `LnmaiCore/FFI.lean`.

Fields:

- `state : GameState`
- `events : List JudgeEvent`
- `audioCommands : List AudioCommand`
- `renderCommands : List RenderCommand`

## `RuntimeStepLightResult`

Defined in `LnmaiCore/FFI.lean`.

Fields:

- `events : List JudgeEvent`
- `audioCommands : List AudioCommand`
- `renderCommands : List RenderCommand`
- `score : ScoreState`
- `currentTime : TimePoint`

## `JudgeEvent`

Defined in `LnmaiCore/Types.lean`.

Fields:

- `kind : JudgeEventKind`
- `grade : JudgeGrade`
- `diff : Duration`
- `position : RuntimePos`
- `noteIndex : Nat`

## `AudioCommand`

Defined in `LnmaiCore/Types.lean`.

Constructors:

- `PlayJudgeSfx(kind, grade, atTime, noteIndex)`
- `PlaySlideCue(noteIndex, trackIndex, atTime)`

## `RenderCommand`

Defined in `LnmaiCore/Types.lean`.

Constructors:

- `ShowJudgeResult(kind, grade, diff, noteIndex)`
- `UpdateSlideProgress(noteIndex, remaining)`
- `UpdateSlideTrackProgress(noteIndex, trackIndex, remaining)`
- `HideAllSlideBars(noteIndex)`
- `HideSlideBars(noteIndex, endIndex)`
- `HideSlideTrackBars(noteIndex, trackIndex, endIndex)`

## Host Workflow

## Recommended gameplay loop

1. parse chart text with `lnmai_parse_lowered_chart_json`
2. create a runtime handle with `lnmai_create_game_state_handle`
3. for each frame, collect host input events
4. package those events into `TimedInputBatch` JSON
5. send one step request to the dedicated Lean runtime worker thread
6. call `lnmai_step_game_state_handle_light`
7. wait for completion
8. consume `events`, `audioCommands`, `renderCommands`, `score`, and `currentTime`
9. repeat for next frame

## Debug workflow

- use `lnmai_step_game_state_handle` when full state snapshots are needed
- use `lnmai_get_game_state_json_by_handle` to inspect the current runtime state

## Implementation Notes That Matter To Hosts

- the runtime is pure at the gameplay step level, but the exported handle service is stateful
- stateful handle storage is process-local inside Lean
- handles must be freed with `lnmai_free_game_state_handle`
- the repo does not yet ship a C header or Rust wrapper crate

## Summary

The currently implemented FFI supports:

- parsing maidata/Simai chart text
- retrieving normalized or lowered chart data
- building runtime state
- stepping runtime state from timed per-frame input
- receiving judge, audio, and render commands
- operating through a dedicated-thread-friendly handle API

For gameplay hosts, the primary API is:

- `lnmai_create_game_state_handle`
- `lnmai_step_game_state_handle_light`
- `lnmai_free_game_state_handle`

For native integration scaffolding in this repo, see:

- `include/lnmai_ffi.h`
- `bindings/rust/mod.rs`
