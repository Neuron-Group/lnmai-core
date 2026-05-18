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

Public host-side declarations added in this repo:

- `include/lnmai_ffi.h`
- `include/lnmai_session.h`
- `bindings/rust/mod.rs`
- `bindings/rust/session.rs`

## Runtime Model

The implemented FFI has three layers:

- parse/lower APIs that operate on chart text and JSON payloads
- legacy runtime APIs that operate on JSON state snapshots or direct loaded handles
- session APIs that operate on stateful process-local handles with typestate-like transitions

The preferred gameplay API is now the session API.

## Threading

Runtime handle access is serialized inside Lean with `Std.Mutex` in `LnmaiCore/FFI.lean`.

Recommended host workflow:

- collect frame input events on the host
- package them into a `TimedInputBatch` JSON payload
- submit a single runtime-step job to a dedicated Lean worker thread
- wait for the Lean step result asynchronously on the host side
- consume returned judgment and command outputs
- advance to the next frame

Recommended rule:

- use one dedicated runtime worker thread per process
- do not intentionally issue overlapping step calls for the same handle

## Common Encoding

### Strings

- all exported parse and runtime APIs return a JSON string

### Time values

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

## Parse APIs

### `lnmai_parse_frontend_chart_json`
- input: chart text `String`, level index `UInt32`
- success payload: `FrontendChartResult`

### `lnmai_parse_frontend_semantic_chart_json`
- success payload: `FrontendSemanticChart`

### `lnmai_parse_frontend_inspection_chart_json`
- success payload: `FrontendChartInspection`

### `lnmai_parse_normalized_chart_json`
- success payload: `NormalizedChart`

### `lnmai_parse_lowered_chart_json`
- success payload: `ChartSpec`

### Parse errors
- code: `parse_error`
- `details`: structured `ParseError`

## Session Runtime APIs

These are the preferred gameplay/runtime APIs.

### Session states

A session handle is process-local and stored inside Lean. Each handle is in one of
these states:

- `empty`
- `loaded`

Only `load` transitions `empty -> loaded`.

Frame stepping mutates the loaded runtime state in place but does not change the
session kind.

### `lnmai_create_empty_session_handle`

Input:

- none

Success payload:

```json
{
  "handle": 1,
  "state": "empty"
}
```

### `lnmai_load_chart_into_session_from_text`

Input:

- `UInt64` handle
- chart text `String`
- level index `UInt32`

Behavior:

- parses and lowers inside Lean
- builds runtime state internally
- transitions `empty -> loaded`

Success payload:

```json
{
  "handle": 1,
  "state": "loaded",
  "summary": {
    "tapCount": 0,
    "holdCount": 0,
    "touchCount": 0,
    "touchHoldCount": 0,
    "slideCount": 0
  }
}
```

Error codes:

- `parse_error`
- `invalid_session_state`

### `lnmai_load_chart_into_session_from_json`

Input:

- `UInt64` handle
- `ChartSpec` JSON string

Behavior:

- builds runtime state internally
- transitions `empty -> loaded`

Error codes:

- `invalid_chart_spec_json`
- `invalid_session_state`

### `lnmai_unload_chart_from_session`

Input:

- `UInt64` handle

Behavior:

- transitions `loaded -> empty`

Success payload:

```json
{
  "handle": 1,
  "state": "empty"
}
```

Error code:

- `invalid_session_state`

### `lnmai_get_lowered_chart_json_by_handle`

Input:

- `UInt64` handle

Success payload:

- `ChartSpec`

Error code:

- `invalid_session_state`

### `lnmai_step_game_state_handle_light`

Input:

- `UInt64` handle
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepLightResult`

This is the recommended per-frame gameplay API.

Error codes:

- `invalid_runtime_json`
- `invalid_runtime_handle` for unknown handle ids

A loaded-state violation currently also returns a handle-related runtime error from
Lean’s handle stepping path.

### `lnmai_step_game_state_handle`

Input:

- `UInt64` handle
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepResult`

Use this when you need full `GameState` snapshots.

## Legacy Runtime APIs

These remain useful for debugging, tooling, and bring-up.

### `lnmai_build_game_state_json`
- input: `ChartSpec` JSON string
- success payload: `GameState`
- error code: `invalid_chart_spec_json`

### `lnmai_step_game_state_json`
- input: `GameState` JSON string and `TimedInputBatch` JSON string
- success payload: `RuntimeStepResult`
- error code: `invalid_runtime_json`

### `lnmai_create_game_state_handle`
- input: `ChartSpec` JSON string
- success payload: `{ "handle": N }`

This is the older direct-loaded-handle entrypoint. New gameplay integrations should
prefer `lnmai_create_empty_session_handle` plus `lnmai_load_chart_into_session_*`.

### `lnmai_free_game_state_handle`
- input: `UInt64` handle
- success payload: `{ "freed": true }`
- error code: `invalid_runtime_handle`

### `lnmai_get_game_state_json_by_handle`
- input: `UInt64` handle
- success payload: `GameState`
- error code: `invalid_runtime_handle`

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

## `TimedInputBatch`

Defined in `LnmaiCore/InputModel.lean`.

Fields:

- `currentTime : TimePoint`
- `events : List TimedInputEvent`

### `TimedInputEvent`

Constructors:

- `buttonClick(tp, zone)`
- `buttonHold(tp, zone, isDown)`
- `sensorClick(tp, area)`
- `sensorHold(tp, area, isDown)`

Event inclusion policy:

- zero-duration frame includes exactly `currentTime`
- positive-duration frame includes `(prevTime, currentTime]`

## `RuntimeStepResult`

Fields:

- `state : GameState`
- `events : List JudgeEvent`
- `audioCommands : List AudioCommand`
- `renderCommands : List RenderCommand`

## `RuntimeStepLightResult`

Fields:

- `events : List JudgeEvent`
- `audioCommands : List AudioCommand`
- `renderCommands : List RenderCommand`
- `score : ScoreState`
- `currentTime : TimePoint`

## Host Workflow

## Recommended gameplay loop

1. create an empty session with `lnmai_create_empty_session_handle`
2. load chart text with `lnmai_load_chart_into_session_from_text`
3. optionally inspect lowered chart with `lnmai_get_lowered_chart_json_by_handle`
4. for each frame, collect host input events
5. package those events into `TimedInputBatch` JSON
6. send one step request to the dedicated Lean runtime worker thread
7. call `lnmai_step_game_state_handle_light`
8. wait for completion
9. consume `events`, `audioCommands`, `renderCommands`, `score`, and `currentTime`
10. free the session handle with `lnmai_free_game_state_handle`

## Summary

The currently implemented FFI supports:

- parsing maidata/Simai chart text
- loading chart text directly into a stateful session handle
- retrieving lowered chart JSON from a loaded session
- stepping runtime state from timed per-frame input
- receiving judge, audio, and render commands
- operating through a dedicated-thread-friendly handle API

For gameplay hosts, the primary API is:

- `lnmai_create_empty_session_handle`
- `lnmai_load_chart_into_session_from_text`
- `lnmai_step_game_state_handle_light`
- `lnmai_free_game_state_handle`

## Wrapper Layers

### C session wrapper

For C hosts that want API-level state distinction between empty and loaded
handles, use:

- `include/lnmai_session.h`

This header provides:

- `lnmai_empty_handle`
- `lnmai_loaded_handle`
- `lnmai_session_init`
- `lnmai_session_load_chart_from_text`
- `lnmai_session_load_chart_from_json`
- `lnmai_session_advance_frame_light`
- `lnmai_session_get_lowered_chart_json`
- `lnmai_session_unload_chart`

The wrapper is header-only and keeps the typestate split at the C API level,
while still using the underlying `UInt64` Lean handle internally.

### Rust typestate wrapper

For Rust hosts, use:

- `bindings/rust/mod.rs`
- `bindings/rust/session.rs`

The wrapper exposes:

- `Session<Empty>`
- `Session<Loaded>`

with transitions like:

- `Session::<Empty>::create()`
- `empty.load_chart_text(...) -> Session<Loaded>`
- `loaded.advance_frame_light(...)`
- `loaded.unload_chart() -> Session<Empty>`
