# Runtime Event and Command FFI Contract

This document defines the gameplay-facing runtime output of `lnmai-core`.
It is the contract for a frontend that renders notes, plays feedback, and
tracks judgment results while the Lean runtime owns note state and scoring.

The contract is intentionally independent of MajdataPlay's autoplay modes.
The core receives real hand-tactic input (`TimedInputBatch`); autoplay is not a
runtime compatibility target.

**Priority note:** timestamp-model refinement is currently an unimportant
target. The click model remains frame-batch based temporarily; this document
records the current contract rather than promising timestamp-exact click
behavior before the adapter is changed.

## One step, three output streams

Every successful runtime step returns one `RuntimeStepLightResult` (or the same
fields inside `RuntimeStepResult`):

```json
{
  "events": [],
  "audioCommands": [],
  "renderCommands": [],
  "score": {},
  "currentTime": 1234567,
  "terminated": false
}
```

The lists have different ownership and must not be merged by the host:

| Stream | Meaning | Host use |
| --- | --- | --- |
| `events` | semantic gameplay judgments and note lifecycle results | score UI, combo/result state, replay logs, analytics |
| `audioCommands` | presentation requests derived from semantic state | sound engine |
| `renderCommands` | presentation requests for transient result/slide visuals | frontend renderer |

`events` are the authoritative gameplay output. Audio and render commands are
derived presentation output. A command may be absent because of a display/audio
policy even when its semantic event exists. A frontend must never infer score or
judgment from a command list.

The three lists are not a normalized event log. Their lengths do not have to
match: one semantic event can produce zero, one, or several commands, and body
audio/progress commands can be emitted without a new judgment event.

The lists apply only to the step that was just processed. They are not a
backlog and must be consumed before the next step. The order within each list
is deterministic and is part of replay behavior. Hosts should preserve order.

Because Lean uses immutable values and returns a new `GameState` from each
`stepFrameTimed`, a command is a value describing the transition; it is not a
callback and has no hidden mutation. The host applies the returned values after
the step completes.

## Time and FPS invariance

All runtime times and differences are signed integer microseconds:

- `TimePoint` is an absolute chart/session time in microseconds.
- `Duration` is a signed difference in microseconds.
- `TimedInputBatch.currentTime` is the requested step end time.
- `TimedInputEvent` timestamps are absolute session times.

Judgment windows, hold body windows, release time, touch-group timing, and slide
timing are evaluated from these integer values. `FrameInput.delta` is derived as
`batch.currentTime - previousState.currentTime`.

There are two separate invariance guarantees:

1. **Numeric timing:** no runtime timing value is stored as a floating-point
   frame count. The Lean model stores and compares integer microseconds, so
   changing FPS cannot introduce a rounding change in a fixed timestamp.
2. **Input observation:** the current `TimedInputBatch.toFrameInput` adapter
   collapses all in-window click edges per button/sensor into click flags/counts
   and takes the last in-window hold state. Therefore, the host must submit
   batches at a cadence that preserves the intended input edges and hold
   transitions. A single large batch containing multiple alternating hold
   transitions is not equivalent to multiple smaller batches today.

Full timestamp invariance remains a future target, not a current guarantee:
identical timestamped input traces can produce different results when their
edges are grouped into different frame batches. Until the adapter consumes
timestamped edges directly in the scheduler, treat the input batch cadence and
event ordering as part of the runtime contract.

The host may submit 30 FPS, 60 FPS, 144 FPS, or irregular frame intervals when
the batches preserve those input observations. It must not round timestamps to
render-frame numbers or reconstruct input edges from wall-clock frames.

For deterministic replay, record the exact `TimedInputBatch` values and feed
them back in the same order. Do not replay only the returned frame count.

## Semantic event schema

`JudgeEvent` is the semantic output type:

```json
{
  "kind": "Hold",
  "phase": "head",
  "grade": "Perfect",
  "diff": 0,
  "position": { "button": "K1" },
  "noteIndex": 42,
  "isBreak": false,
  "isEX": false,
  "multiple": 1
}
```

Fields:

- `kind`: gameplay family: `Tap`, `Hold`, `Slide`, `Touch`, or `Break`.
- `phase`: `head` or `tail`.
- `grade`: the converted runtime grade.
- `diff`: signed judgment difference in microseconds. Negative is early, zero is
  exact, positive is late. Timeout events use the defined judge-boundary value.
- `position`: button or sensor runtime position.
- `noteIndex`: stable lowered-chart note id.
- `isBreak`, `isEX`: chart flags relevant to presentation/scoring.
- `multiple`: simultaneous folded-note multiplicity; minimum value is `1`.

### Event visibility

The compact event API uses one `JudgeEvent` type for all gameplay families and
all judgment phases. It should expose every semantic judgment edge that the
frontend may need to show or record, including score-neutral hold heads. The
frontend should treat every received event as immutable historical data and key
persistent UI state by `noteIndex` plus `phase`.

The scheduler returns `Hold/head` values in the public `events` list. They are
score-neutral because `foldEventIntoScore` ignores the hold-head phase, while
the `phase` field lets the frontend observe the transition without a second
`HoldHead` event type. This keeps the semantic event API compact and makes the
public event stream independent of audio/render policy.

Some transitions are intentionally silent semantic details and do not require a
new event type:

- touch-hold body press/release/recovery is represented by body commands and
  final `Hold/tail`, not one event per frame;
- touch-hold shared-group resolution does not replay direct-input feedback;
- command absence must never be interpreted as a missed judgment.

### Event families

| Kind/phase | Meaning | Score effect |
| --- | --- | --- |
| `Tap/head` | tap judged by input or timeout | scores immediately |
| `Touch/head` | touch judged by input, touch-group share, or timeout | scores immediately |
| `Slide/head` | slide final judgment (including group-end judgment) | scores immediately |
| `Hold/head` | hold head accepted/missed; score-neutral semantic edge | no score/combo effect |
| `Hold/tail` | regular hold or touch-hold completed | scores the hold result |

Break identity is carried by `isBreak`. `JudgeEventKind.Break` also exists for
the generic event algebra, but normal hold/tap/touch events keep their gameplay
family in `kind` and set `isBreak = true`. Hosts should use `isBreak` for break
scoring/presentation classification.

## Audio command schema

`AudioCommand` is presentation-only:

```json
{ "PlayJudgeSfx": ["Hold", "Perfect", false, 1234567, 42] }
```

Current constructors:

- `PlayJudgeSfx(kind, grade, isBreak, atTime, noteIndex)`
- `PlaySlideCue(noteIndex, trackIndex, isBreak, atTime)`
- `PlayTouchHoldBody(noteIndex, atTime)`

`atTime` is the step's microsecond `currentTime`, not a Unity/render-frame
number. It identifies the runtime step that emitted the request; it is not a
sample-accurate source timestamp for the original input edge. The audio backend
may schedule or play immediately, but it must preserve the command order
returned by the core.

Important policy rules:

- regular hold head success may produce tap-style judgment SFX;
- regular hold head miss/too-fast produces no normal head SFX;
- touch-hold direct head input produces no tap-style head SFX;
- touch-hold body commands represent the riser/body presentation and are not
  score events;
- final hold judgment produces the final judgment SFX command.

The audio layer must not award score, advance queues, or synthesize missing
semantic events.

### Hold and touch-hold examples

The following table describes the intended compact stream. `head` rows are
semantic events; command columns are independent and may be empty by policy.

| Runtime transition | Semantic event | Audio command | Render command |
| --- | --- | --- | --- |
| regular hold head accepted | `Hold/head` at button position | tap-style `PlayJudgeSfx` may be emitted | optional `ShowJudgeResult` by hold-head display policy |
| regular hold head timeout | `Hold/head` with `Miss` | none | optional miss display by hold-head policy |
| regular hold completion | `Hold/tail` | final `PlayJudgeSfx` | `ShowJudgeResult` |
| touch-hold direct head | `Hold/head` at sensor position | no tap-style head SFX | optional hold-head display if enabled |
| touch-hold shared head | semantic head resolution when exposed; no direct-input replay | no head SFX | no required head effect |
| touch-hold body held | no per-frame judgment event | `PlayTouchHoldBody` as needed | body/riser rendering is frontend state |
| touch-hold completion | `Hold/tail` at sensor position | final `PlayJudgeSfx` | `ShowJudgeResult` |

The `kind = Hold` plus `position` is sufficient to distinguish regular and
touch-hold events; a second public event kind would make the API larger without
adding gameplay information.

## Render command schema

`RenderCommand` is presentation-only:

- `ShowJudgeResult(kind, grade, isBreak, diff, noteIndex)`
- `UpdateSlideProgress(noteIndex, remaining)`
- `UpdateSlideTrackProgress(noteIndex, trackIndex, remaining)`
- `HideAllSlideBars(noteIndex)`
- `HideSlideBars(noteIndex, endIndex)`
- `HideSlideTrackBars(noteIndex, trackIndex, endIndex)`

Hold-head result rendering is policy-controlled by
`GameState.displayHoldHeadJudgeResult`. With the default policy, a hold head can
exist semantically without a `ShowJudgeResult` command. The final hold result is
rendered normally.

Slide progress commands are state/render updates, not judgment events. They may
be emitted on frames with no new semantic judgment. `remaining` and track
indices are runtime progress values, not timestamps.

The renderer must treat commands as value instructions for the current step and
must not use them as the source of score/combo truth. Commands are not
guaranteed to be globally idempotent across repeated host delivery; the host
must deliver each successful step result once.

Current Lean limitation: `PlayTouchHoldBody` is emitted whenever the functional
state says the body is active and pressed, so a held touch-hold can produce one
command per step. The command is an "ensure body audio" request; the frontend
may coalesce it into one looping riser and stop that riser when no corresponding
command is received, but it must not turn it into a judgment event.

## Ordering and frame pipeline

The runtime processes one step in this semantic order:

1. tap-family queues, including ordinary slide heads;
2. regular hold heads and active regular hold bodies;
3. slide bodies and slide progress;
4. touch queues and touch-group sharing;
5. touch-hold heads, body polling, and touch-hold body groups;
6. score folding from semantic events;
7. audio and render command derivation.

This ordering affects shared button/sensor click consumption. For example, a tap
can consume a same-frame click before a hold head on the same lane; a touch can
consume a sensor click before a touch-hold head on the same area. The host must
submit all same-step input edges in one `TimedInputBatch` so the core can apply
this ordering.

Within a result, consume `events`, `audioCommands`, and `renderCommands` in the
returned order. Do not sort by note index or by command constructor.

## Input contract for real hand tactics

The host supplies physical intent as timestamped events:

- `buttonClick(time, zone)` and `sensorClick(time, area)` are edge events;
- `buttonHold(time, zone, isDown)` and `sensorHold(time, area, isDown)` change
  held state;
- `currentTime` closes the frame interval.

A held state persists from the previous step until a corresponding `isDown=false`
event or a new state snapshot changes it. A click is consumable only once per
input edge unless the input representation explicitly supplies multiple click
events/counts. The frontend should pass the generated real hand tactic events
without converting them to render-frame booleans.

## Recommended frontend integration

For normal gameplay use `lnmai_step_game_state_handle_light`:

1. collect all real input events whose timestamps belong to the next step;
2. submit one `TimedInputBatch` with an absolute microsecond `currentTime`; keep
   each click edge and each hold transition in its own frame batch. The click
   model is intentionally frame-batch based for now;
3. apply `events` to score/combo/result UI;
4. dispatch `audioCommands` to the audio backend;
5. dispatch `renderCommands` to the renderer;
6. retain `currentTime` and the input held state for the next batch. Do not
   assume that a large batch with multiple hold transitions is equivalent to
   smaller batches under the current adapter.

Use `lnmai_step_game_state_handle` only when debugging/replay inspection needs
the complete mutable `GameState`. The light result is the stable gameplay
contract and avoids coupling the frontend to internal lifecycle fields.

## Compatibility rule

The semantic event stream is the compatibility surface. New presentation
commands may be added without changing event meaning. A change to event kind,
phase, grade, diff, note index, queue consumption, or score timing is a runtime
semantic change and requires an ABI/schema version review and replay fixtures.
