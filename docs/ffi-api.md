# FFI API

This document describes the currently implemented Lean-side FFI of `lnmai-core`.

It focuses on exported symbols, host calling patterns, state transitions, and how
those APIs connect to the parser/runtime IR.

For payload schemas and transformation-stage data structures, see:

- `docs/ffi-ir.md`

## Scope

Implemented in:

- `LnmaiCore/FFI.lean`

Core runtime logic behind the FFI lives in:

- `LnmaiCore/Simai/Frontend.lean`
- `LnmaiCore/ChartLoader.lean`
- `LnmaiCore/Scheduler.lean`
- `LnmaiCore/InputModel.lean`

Public host-side declarations added in this repo:

- `include/lnmai_ffi.h`
- `include/lnmai_session.h`
- `bindings/rust/mod.rs`
- `bindings/rust/session.rs`

## Runtime Model

The implemented FFI has three layers:

- parse/lower APIs that operate on chart text and return parser IR as JSON
- legacy runtime APIs that operate on `ChartSpec`, `GameState`, or direct loaded handles
- session APIs that operate on process-local stateful handles with `empty` / `loaded` transitions

The preferred gameplay API is the session API.

## Transformation Pipeline

The exported parser functions expose multiple points in the internal pipeline:

```text
maidata / Simai text
  -> source tokens + source positions
  -> semantic slide interpretation
  -> normalized chart IR
  -> lowered runtime chart IR (`ChartSpec`)
  -> loaded runtime state (`GameState` in Lean handle registry)
  -> stepped frame results (`JudgeEvent`, `AudioCommand`, `RenderCommand`, score)
```

The parser-side APIs are intentionally layered so hosts can stop at whichever IR
boundary they need.

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

Defined in `LnmaiCore/Time.lean`.

Encoding:

- `TimeTick` serializes as a signed JSON integer
- `Duration` serializes as a signed JSON integer in microseconds
- `TimePoint` serializes as a signed JSON integer in microseconds

Host rule:

- treat all FFI time values as `int64` microseconds

### Rational values

Lean `Rat` values do not serialize as plain numbers.

They encode as:

```json
{
  "num": 3,
  "den": 2,
  "decimal": "1.5"
}
```

Host rule:

- use `num` and `den` as the authoritative machine-readable value
- treat `decimal` as a convenience string for debugging/display

### Area and slot enums

Defined in `LnmaiCore/Areas.lean`.

Encoding:

- `SensorArea`: strings like `"A1"`, `"B4"`, `"C"`, `"E8"`
- `ButtonZone`: strings like `"K1"` .. `"K8"`
- `OuterSlot`: strings like `"S1"` .. `"S8"`

### Algebraic data types

Many Lean sum types are derived with `ToJson` / `FromJson`.

Practical host guidance:

- product types (`structure`) appear as JSON objects with named fields
- optional values appear as either the encoded value or `null`
- enum / constructor types appear in Lean-derived tagged JSON form

Because the exact constructor encoding is produced by Lean derivation, hosts should:

- prefer treating parse/runtime outputs as data to deserialize with generated bindings
- avoid hard-coding assumptions about constructor JSON beyond tested payload examples
- use the same library-level schema across the integration if possible

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

These APIs parse chart text and stop at different IR boundaries.

### `lnmai_parse_frontend_chart_json`

Input:

- chart text `String`
- level index `UInt32`

Success payload:

- `FrontendChartResult`

Meaning:

- full parser-side result bundle
- includes both semantic/lowered data and inspection/source-oriented data

### `lnmai_parse_frontend_semantic_chart_json`

Input:

- chart text `String`
- level index `UInt32`

Success payload:

- `FrontendSemanticChart`

Meaning:

- normalized chart IR plus lowered runtime `ChartSpec`

### `lnmai_parse_frontend_inspection_chart_json`

Input:

- chart text `String`
- level index `UInt32`

Success payload:

- `FrontendChartInspection`

Meaning:

- source-oriented parser artifacts for tooling and debugging
- includes raw tokens, source spans, maidata metadata, and interpreted slide notes

### `lnmai_parse_normalized_chart_json`

Input:

- chart text `String`
- level index `UInt32`

Success payload:

- `NormalizedChart`

Meaning:

- gameplay-relevant normalized IR before lowering into runtime queues and note states

### `lnmai_parse_lowered_chart_json`

Input:

- chart text `String`
- level index `UInt32`

Success payload:

- `ChartSpec`

Meaning:

- runtime-ready declarative note specification used to build `GameState`

### Parse errors

Error code:

- `parse_error`

Error details payload:

- `ParseError`

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

Success payload:

- same shape as `lnmai_load_chart_into_session_from_text`

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

Use this for per-frame gameplay integration when the host does not need the full
internal `GameState` snapshot back every frame.

Error codes:

- `invalid_runtime_json`
- `invalid_runtime_handle`

A loaded-state violation currently also returns a handle-related runtime error from
Lean’s handle stepping path.

### `lnmai_step_game_state_handle`

Input:

- `UInt64` handle
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepResult`

Use this when the host needs the complete `GameState` after every step.

### `lnmai_get_game_state_json_by_handle`

Input:

- `UInt64` handle

Success payload:

- `GameState`

Error code:

- `invalid_runtime_handle`

### `lnmai_free_game_state_handle`

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

## Legacy Runtime APIs

These remain useful for debugging, tooling, and bring-up.

### `lnmai_build_game_state_json`

Input:

- `ChartSpec` JSON string

Success payload:

- `GameState`

Error code:

- `invalid_chart_spec_json`

### `lnmai_step_game_state_json`

Input:

- `GameState` JSON string
- `TimedInputBatch` JSON string

Success payload:

- `RuntimeStepResult`

Error code:

- `invalid_runtime_json`

### `lnmai_create_game_state_handle`

Input:

- `ChartSpec` JSON string

Success payload:

```json
{
  "handle": 1
}
```

This is the older direct-loaded-handle entrypoint. New gameplay integrations should
prefer `lnmai_create_empty_session_handle` plus `lnmai_load_chart_into_session_*`.

## Key Payload Families

The full schema reference lives in `docs/ffi-ir.md`.

Most hosts will primarily interact with:

- parser-side IR: `FrontendChartResult`, `FrontendSemanticChart`, `FrontendChartInspection`
- normalized IR: `NormalizedChart`
- lowered runtime IR: `ChartSpec`
- runtime input: `TimedInputBatch`
- runtime output: `RuntimeStepResult`, `RuntimeStepLightResult`
- debugging state: `GameState`

### Boundary comparison

| Boundary | Main type | Best for | Includes | Avoid when |
| --- | --- | --- | --- | --- |
| Inspection | `FrontendChartInspection` | editors, diagnostics, parser tooling | raw tokens, source spans, maidata metadata, interpreted slide semantics | you only need gameplay-ready notes |
| Normalized | `NormalizedChart` | semantic chart tooling, analysis, content transforms | normalized timing, note flags, slide semantics, note indices | you need exact runtime queue layout |
| Lowered | `ChartSpec` | runtime-oriented interchange, deterministic host-side inspection | runtime-ready note lists, queue indices, slide judge queues | you want Lean to own live state transitions |
| Runtime | `RuntimeStepLightResult` | gameplay stepping | judgments, audio/render commands, score, current time | you need deep parser/source provenance |
| Full state | `GameState` | debugging, replay inspection, internal verification | entire loaded mutable runtime state | you want a stable, minimal host contract |

Quick selection guide:

- choose `inspection` to understand how source text was parsed
- choose `normalized` to work with semantically cleaned chart content
- choose `lowered` to inspect the exact runtime-ready chart IR
- choose `RuntimeStepLightResult` for the main gameplay loop
- choose `GameState` only when full internal state visibility is necessary

## Example Payloads

These examples are illustrative and intentionally minimal. Actual parse/runtime
responses often include more fields depending on chart content and current state.

### Example parse request context

Input chart text:

```text
&inote_1=
(120)
1,
```

Level index:

```json
1
```

### Example parse response

Example success response for `lnmai_parse_lowered_chart_json`:

```json
{
  "ok": true,
  "result": {
    "taps": [
      {
        "timing": 0,
        "slot": "S1",
        "isBreak": false,
        "isEX": false,
        "buttonQueueIndex": 0,
        "noteIndex": 0
      }
    ],
    "holds": [],
    "touches": [],
    "touchHolds": [],
    "slides": [],
    "slideSkipping": true
  }
}
```

Example success response for `lnmai_parse_frontend_chart_json`:

```json
{
  "ok": true,
  "result": {
    "semantic": {
      "normalized": {
        "taps": [
          {
            "timing": 0,
            "slot": "S1",
            "isBreak": false,
            "isEX": false,
            "isHanabi": false,
            "isForceStar": false,
            "noteIndex": 0
          }
        ],
        "holds": [],
        "touches": [],
        "touchHolds": [],
        "slides": [],
        "slideDebug": [],
        "slideSkipping": true
      },
      "lowered": {
        "taps": [
          {
            "timing": 0,
            "slot": "S1",
            "isBreak": false,
            "isEX": false,
            "buttonQueueIndex": 0,
            "noteIndex": 0
          }
        ],
        "holds": [],
        "touches": [],
        "touchHolds": [],
        "slides": [],
        "slideSkipping": true
      }
    },
    "inspection": {
      "metadata": {
        "fields": []
      },
      "chart": {
        "levelIndex": 1,
        "rawBody": "(120)\n1,\n"
      },
      "source": {
        "events": [
          {
            "timing": 0,
            "bpm": {
              "num": 120,
              "den": 1,
              "decimal": "120"
            },
            "hSpeed": {
              "num": 1,
              "den": 1,
              "decimal": "1"
            },
            "divisor": 4,
            "notes": [
              {
                "token": {
                  "rawText": "1",
                  "kind": "tap",
                  "timing": 0,
                  "bpm": {
                    "num": 120,
                    "den": 1,
                    "decimal": "120"
                  },
                  "hSpeed": {
                    "num": 1,
                    "den": 1,
                    "decimal": "1"
                  },
                  "divisor": 4,
                  "slot": "S1",
                  "sensorPos": null,
                  "slideBody": null,
                  "length": null,
                  "starWait": null,
                  "isBreak": false,
                  "isEX": false,
                  "isHanabi": false,
                  "isSlideNoHead": false,
                  "isForceStar": false,
                  "isFakeRotate": false,
                  "isSlideBreak": false,
                  "sourceGroupId": null,
                  "sourceGroupIndex": null,
                  "sourceGroupSize": null,
                  "sourcePos": null
                },
                "sourcePos": null
              }
            ],
            "sourcePos": null
          }
        ]
      },
      "tokens": [
        {
          "rawText": "1",
          "kind": "tap",
          "timing": 0,
          "bpm": {
            "num": 120,
            "den": 1,
            "decimal": "120"
          },
          "hSpeed": {
            "num": 1,
            "den": 1,
            "decimal": "1"
          },
          "divisor": 4,
          "slot": "S1",
          "sensorPos": null,
          "slideBody": null,
          "length": null,
          "starWait": null,
          "isBreak": false,
          "isEX": false,
          "isHanabi": false,
          "isSlideNoHead": false,
          "isForceStar": false,
          "isFakeRotate": false,
          "isSlideBreak": false,
          "sourceGroupId": null,
          "sourceGroupIndex": null,
          "sourceGroupSize": null,
          "sourcePos": null
        }
      ],
      "slideNotes": []
    }
  }
}
```

This bundled response is the broadest parser-facing payload:

- `semantic.normalized` gives the semantically normalized note IR
- `semantic.lowered` gives the runtime-ready `ChartSpec`
- `inspection` preserves parser/source-oriented artifacts for tooling and diagnostics

Example error response for any parse API:

```json
{
  "ok": false,
  "error": {
    "code": "parse_error",
    "message": "unexpected token"
  },
  "details": {
    "kind": "invalidSyntax",
    "rawText": "?",
    "message": "unexpected token",
    "span": {
      "start": { "line": 2, "column": 1 },
      "stop": { "line": 2, "column": 2 }
    }
  }
}
```

### Example frame-step request

Example request payload for `lnmai_step_game_state_handle_light`:

```json
{
  "currentTime": 0,
  "events": [
    {
      "tag": "buttonClick",
      "tp": 0,
      "zone": "K1"
    }
  ]
}
```

Practical note:

- constructor-shaped values such as `TimedInputEvent` use Lean-derived tagged JSON
- field naming for constructor payloads should be treated as schema-driven and verified against the binding you use
- the example above shows the intended semantic shape: one `buttonClick` at time `0` on `K1`

### Example frame-step response

Example success response for `lnmai_step_game_state_handle_light`:

```json
{
  "ok": true,
  "result": {
    "events": [
      {
        "kind": "Tap",
        "grade": "Perfect",
        "diff": 0,
        "position": {
          "tag": "button",
          "value": "K1"
        },
        "noteIndex": 0
      }
    ],
    "audioCommands": [
      {
        "tag": "PlayJudgeSfx",
        "kind": "Tap",
        "grade": "Perfect",
        "atTime": 0,
        "noteIndex": 0
      }
    ],
    "renderCommands": [
      {
        "tag": "ShowJudgeResult",
        "kind": "Tap",
        "grade": "Perfect",
        "diff": 0,
        "noteIndex": 0
      }
    ],
    "score": {
      "combo": 1,
      "pCombo": 1,
      "cPCombo": 1,
      "totalBase": 0,
      "totalExtra": 0,
      "earnedBase": 500,
      "earnedExtra": 0,
      "lostBase": 0,
      "lostExtra": 0,
      "dxScore": 3,
      "maxDxScore": 3,
      "fastCount": 0,
      "lateCount": 0,
      "counts": {
        "tapCount": {
          "Miss": 0,
          "LateGood": 0,
          "LateGreat3rd": 0,
          "LateGreat2nd": 0,
          "LateGreat": 0,
          "LatePerfect3rd": 0,
          "LatePerfect2nd": 0,
          "Perfect": 1,
          "FastPerfect2nd": 0,
          "FastPerfect3rd": 0,
          "FastGreat": 0,
          "FastGreat2nd": 0,
          "FastGreat3rd": 0,
          "FastGood": 0,
          "TooFast": 0
        },
        "holdCount": {
          "Miss": 0,
          "LateGood": 0,
          "LateGreat3rd": 0,
          "LateGreat2nd": 0,
          "LateGreat": 0,
          "LatePerfect3rd": 0,
          "LatePerfect2nd": 0,
          "Perfect": 0,
          "FastPerfect2nd": 0,
          "FastPerfect3rd": 0,
          "FastGreat": 0,
          "FastGreat2nd": 0,
          "FastGreat3rd": 0,
          "FastGood": 0,
          "TooFast": 0
        },
        "slideCount": {
          "Miss": 0,
          "LateGood": 0,
          "LateGreat3rd": 0,
          "LateGreat2nd": 0,
          "LateGreat": 0,
          "LatePerfect3rd": 0,
          "LatePerfect2nd": 0,
          "Perfect": 0,
          "FastPerfect2nd": 0,
          "FastPerfect3rd": 0,
          "FastGreat": 0,
          "FastGreat2nd": 0,
          "FastGreat3rd": 0,
          "FastGood": 0,
          "TooFast": 0
        },
        "touchCount": {
          "Miss": 0,
          "LateGood": 0,
          "LateGreat3rd": 0,
          "LateGreat2nd": 0,
          "LateGreat": 0,
          "LatePerfect3rd": 0,
          "LatePerfect2nd": 0,
          "Perfect": 0,
          "FastPerfect2nd": 0,
          "FastPerfect3rd": 0,
          "FastGreat": 0,
          "FastGreat2nd": 0,
          "FastGreat3rd": 0,
          "FastGood": 0,
          "TooFast": 0
        },
        "breakCount": {
          "Miss": 0,
          "LateGood": 0,
          "LateGreat3rd": 0,
          "LateGreat2nd": 0,
          "LateGreat": 0,
          "LatePerfect3rd": 0,
          "LatePerfect2nd": 0,
          "Perfect": 0,
          "FastPerfect2nd": 0,
          "FastPerfect3rd": 0,
          "FastGreat": 0,
          "FastGreat2nd": 0,
          "FastGreat3rd": 0,
          "FastGood": 0,
          "TooFast": 0
        }
      }
    },
    "currentTime": 0
  }
}
```

## Host Workflow

### Recommended gameplay loop

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

### When to use each parser boundary

- use `inspection` when building editors, diagnostics, or parser-debug tools
- use `normalized` when you need semantically-stable chart content before runtime lowering
- use `lowered` when you need the exact runtime-ready note specification
- use the session runtime when you want Lean to own the authoritative live state

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
- `lnmai_session_advance_frame_full`
- `lnmai_session_get_lowered_chart_json`
- `lnmai_session_get_state_json`
- `lnmai_session_unload_chart`
- `lnmai_session_free_empty`
- `lnmai_session_free_loaded`

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

## Summary

The currently implemented FFI supports:

- parsing maidata / Simai chart text at multiple IR boundaries
- exposing source inspection, normalized IR, and lowered runtime IR
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
