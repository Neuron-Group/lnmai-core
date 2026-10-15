# Hold-Head MajdataPlay Alignment

## Verified Reference Behavior

MajdataPlay treats hold-head feedback and final hold scoring as separate operations.

- Regular `HoldDrop.HeadCheck()` judges a direct button or sensor click, calls `PlaySFX()`, and advances the hold queue. `PlaySFX()` delegates to `PlayJudgeSFX()`, which plays the tap-style judgment sound.
- Regular hold-head display is conditional on `DisplayHoldHeadJudgeResult`. The default setting is false, but successful head audio remains unconditional.
- A regular hold timeout sets a miss and advances the queue without playing the normal head SFX. It may display the head miss when the display setting is enabled.
- The final `HoldDrop.End()` path emits the final hold result and reports the scored hold outcome.
- Direct `TouchHoldDrop.HeadCheck()` judges the sensor input, advances the touch queue, plays the hold effect, and registers the group grade. It does not call the tap-style `PlayJudgeSFX()` path for normal direct user input.
- Touch-hold body processing separately starts or maintains the touch-hold riser through `PlayTouchHoldSound()`.
- Touch-hold autoplay explicitly calls `PlayJudgeSFX()`, so autoplay feedback is a distinct path from normal direct touch input.
- Touch-hold shared-group resolution is silent at the head and does not replay direct-input feedback.

## Current Core Behavior

- Hold heads use `JudgeEventKind.Hold` with `phase = .head`; completed hold results use
  `phase = .tail`.
- Direct regular hold-head judgments emit `{ kind := .Hold, phase := .head, ... }`.
- `.head` events are ignored by score folding, so only the final `.tail` event changes
  score, combo, counters, and fast/late statistics.
- Head rendering is controlled by the explicit `DisplayHoldHeadJudgeResult` policy.
- Timeout and shared touch-group head resolutions remain event-silent.

## Confirmed Gaps

1. Direct touch-hold input is over-emitting tap-style head audio. MajdataPlay's normal `TouchHoldDrop.HeadCheck()` does not call `PlayJudgeSFX()`.
2. Hold-head rendering is unconditionally suppressed. MajdataPlay supports optional head-result display through `DisplayHoldHeadJudgeResult`, so the core needs an explicit display-policy path if that setting is supported.

## Chosen Product-Type Design

Use one `JudgeEvent` product type with an explicit phase field instead of adding a separate `HoldHead` event kind or a separate feedback structure.

```lean
inductive JudgePhase
  | head
  | tail

structure JudgeEvent where
  kind : JudgeEventKind
  phase : JudgePhase
  grade : JudgeGrade
  diff : Duration
  position : RuntimePos
  noteIndex : Nat
  isBreak : Bool := false
  multiple : Nat := 1
```

The semantic contract is:

- Regular hold direct head feedback is `{ kind := .Hold, phase := .head, ... }`.
- The final scored hold result is `{ kind := .Hold, phase := .tail, ... }`.
- `Scheduler` folds only score-bearing phases, currently `.tail` for holds.
- `Scheduler` derives audio/render commands from `kind` plus `phase` and applies the hold-head display policy there.
- `Lifecycle` remains responsible only for state transitions and semantic events; it does not construct presentation commands.
- Touch-hold direct input, shared resolution, timeout, and autoplay behavior must remain explicitly distinguishable through the phase and event-production path rather than by inventing another event kind.

This keeps the public gameplay family stable (`Tap`, `Hold`, `Slide`, `Touch`, `Break`) while making head/tail intent explicit to score, audio, render, serialization, and FFI consumers.
