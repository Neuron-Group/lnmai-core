# Refactor Roadmap for Semantic Parity

## Goal

This document describes a refactor plan for `lnmai-core` under the following rule:

- We do **not** need to reproduce the Unity object model used by `MajdataPlay`.
- We **do** need to reproduce the same **game semantics**.

In this document, **game semantics** means the observable gameplay results produced from the same chart and the same player input timeline, including:

- which note is eligible to judge
- which input is consumed
- the resulting judgment grade
- fast / late polarity and diff
- queue advancement
- hold / touch-hold / touch-group behavior
- slide queue progression and final grade timing
- combo, AP state, DX score loss, and final counters

It does **not** require us to match Unity-specific implementation details such as:

- `MonoBehaviour` update pass layout
- object activation / visibility lifecycles
- renderer ordering implementation details
- internal animation state transitions

The design target is therefore:

- preserve or improve proof-friendliness
- preserve deterministic replay behavior
- converge toward semantic equivalence with the reference implementation
- reduce local duplication and ad hoc timing boundaries


## Status Update

Current implementation progress against this roadmap:

- Phase 1 is complete
- Phase 2 is complete
- Phase 3 is complete for tap / touch / hold-head immediate judgment cleanup
- Phase 4 is complete
- Phase 5 is complete

What has been verified and tightened already:

- frame-window semantics are now explicit in the input model
- boundary inclusion policy is covered by direct runtime tests
- mixed-family same-frame competition tests now lock current scheduler-sensitive behavior
- mixed-chart replay-level golden tests now lock end-to-end combo / AP / DX-score outcomes on compact multi-family sections
- touch-hold head eligibility now follows shared touch-queue progression more closely
- same-frame repeated-click behavior was checked against reduced `MajdataPlay` logic in the local harness
- slide semantic transition has been separated from render/audio derivation
- conn-slide parent/child propagation semantics are covered beyond the earlier narrow force-finish case
- wifi progression markers, judged-wait timing, and too-late grading are now covered by targeted parity tests

Important reference finding from `MajdataPlay`:

- click consumption is platform-sensitive in the reference runtime
- desktop-style paths behave like one-shot per-frame usage
- mobile-style paths consume per-frame click counts and can feed multiple same-frame judgments when counts allow

The Lean runtime now explicitly preserves the repeated-click semantics that matter for deterministic replay and mixed touch / touch-hold head behavior.


## Current State Summary

The current runtime has a solid semantics-oriented architecture:

- `LnmaiCore/InputModel.lean` models per-frame inputs and per-zone note queues
- `LnmaiCore/Lifecycle.lean` models per-note state transitions
- `LnmaiCore/Scheduler.lean` applies one frame step over the whole state
- `LnmaiCore/Proofs/Runtime.lean` provides replay-style simulation over timed inputs
- `LnmaiCore/RuntimeTests.lean` gives focused regression coverage

This architecture is already better than the reference implementation for:

- deterministic replay
- proof use
- pure-state inspection
- targeted semantic testing

However, several parts are still less elegant than they should be:

- duplicated same-frame judgment logic across note families
- split touch / touch-hold head queue modeling that currently relies on synchronized indices
- implicit scheduler ordering policy
- incomplete semantic-equivalence coverage across all note interactions


## Semantic Comparison Baseline

When comparing with `../reference/MajdataPlay`, the right baseline is not architectural similarity.

The right baseline is whether the following observables match:

1. note eligibility timing
2. input consumption timing
3. queue head progression
4. group / shared judgment propagation
5. slide checkability and queue clearing behavior
6. delayed-vs-immediate event emission rules
7. score / combo / AP / fast-late results

If these match, the implementation is semantically acceptable even if the architecture is completely different.


## Major Semantic Areas

### 1. Input Semantics

Current sources:

- `LnmaiCore/InputModel.lean`
- `LnmaiCore/Scheduler.lean`

Reference comparison points:

- per-frame button / sensor status
- click count consumption
- sensor-vs-button fallback behavior
- touch-panel offset handling

What already looks good:

- per-frame click counts exist
- held states exist
- fallback button/sensor routing exists
- timed batches support deterministic replay

Current status:

- frame inclusion semantics are now named explicitly through `FrameWindow`

Remaining concern:

- broader replay-level validation is still useful for mixed result accumulation

Why this matters:

- zero-delta is not only a frame-zero issue
- it is a symptom that frame interval semantics are not modeled explicitly enough


### 2. Queue Eligibility Semantics

Current sources:

- `LnmaiCore/InputModel.lean`
- `LnmaiCore/Scheduler.lean`

Reference comparison points:

- current note index gating in `NoteManager`
- queue advancement only after judgment / completion

What already looks good:

- tap and touch queues are explicit
- hold queues and active notes are both represented
- queue advancement behavior is testable and pure

Current status:

- tap and touch queue progression are explicit and tested
- touch-hold head gating now follows shared touch-queue semantics more closely

Remaining concern:

- slide queue semantics still live in a separate style from tap/touch queues


### 3. Tap / Hold / Touch Head Semantics

Current source:

- `LnmaiCore/Lifecycle.lean`

Reference comparison points:

- same-frame judgment once inside range and queue-eligible
- too-late miss path
- fallback sensor/button paths
- head judgment feeding later hold end evaluation

What already looks good:

- same-frame eligibility bug is fixed
- head timing diff is stored and reused for hold-end logic
- touch/touch-hold head interaction now matches the reference-style shared-click and shared-group behavior more closely

Current status:

- the duplicated immediate-judge pattern has now been factored into small helpers
- `tapStep`, `touchStep`, and hold head branches are materially smaller

Remaining concern:

- the helper layer should stay small and explicit rather than growing into a generic framework


### 4. Touch Group / Touch-Hold Group Semantics

Current source:

- `LnmaiCore/Scheduler.lean`
- `LnmaiCore/Lifecycle.lean`

Reference comparison points:

- group majority-sharing
- same-frame propagation of group results
- ordering against too-late checks

What already looks good:

- explicit group state representation exists
- majority logic exists
- there is already runtime coverage

Remaining concern:

- dense multi-note same-frame interactions still deserve more replay-level coverage outside the currently targeted touch/touch-hold and hold/tap cases


### 5. Slide Semantics

Current source:

- `LnmaiCore/Lifecycle.lean`
- `LnmaiCore/Scheduler.lean`

Reference comparison points:

- checkability threshold near start timing
- queue clearing in the same frame
- skippable / multi-area progression
- parent-child conn slide finish propagation
- internal judged state vs final event emission delay
- special wifi / conn rendering progress semantics

What already looks good:

- immediate same-frame queue consumption after becoming checkable
- delayed event semantics are represented explicitly
- conn slide logic and wifi-specific progress handling already exist

Current weakness:

- slide code mixes semantic queue transition with render/audio command derivation
- this makes the semantic core harder to audit than necessary


### 6. Score and Result Semantics

Current source:

- `LnmaiCore/Score.lean`

Reference comparison points:

- combo and AP progression
- DX score loss rules
- fast / late counting rules
- final accuracy formulas

What already looks good:

- logic is already modeled as pure functions
- formulas are readable and inspectable

Remaining concern:

- these formulas need more end-to-end runtime validation against mixed note sequences


## Refactor Objectives

The following objectives are listed in recommended priority order.


## Objective A: Introduce an Explicit Frame-Interval Abstraction

### Problem

The current code treats input inclusion through this pattern:

- positive `delta`: `(currentTime - delta, currentTime]`
- zero `delta`: `{currentTime}` via special case

This is correct, but not elegant.

### Goal

Make the frame inclusion semantics explicit and reusable.

### Proposed design

Add a small abstraction in `LnmaiCore/InputModel.lean` or a nearby timing module:

- `FrameWindow`
- constructor from `(prevTime, currentTime)`
- method `containsEventTime`

Possible shape:

- point window when `prevTime = currentTime`
- left-open right-closed interval otherwise

### Benefits

- removes ad hoc zero-delta branching from event folding logic
- makes replay semantics clearer
- gives one shared place to document boundary policy

### Success criteria

- zero-delta branch disappears from event folding code
- all existing input timing tests still pass
- frame-zero replay tests remain green


## Objective B: Unify Immediate Judgment Logic for Tap-Like Heads

### Problem

Tap, hold-head, touch-hold-head, and touch all perform variants of:

- too-late check
- in-range check
- same-frame input consume
- judge raw grade
- convert grade
- store diff and/or emit event

This logic is duplicated.

### Goal

Extract shared semantic machinery while keeping note-specific results explicit.

### Proposed design

Introduce a small internal helper layer in `LnmaiCore/Lifecycle.lean` for tap-like head judgment.

Possible helpers:

- `judgeTapLikeNow?`
- `judgeTouchLikeNow?`
- `enterJudgeableOrConsumeNow`

These helpers should not hide note-specific behavior too much. They should only centralize the repeated timing/input pattern.

### Non-goal

Do **not** try to force all note families into one giant generic framework. That would likely make the proof-facing code worse.

### Benefits

- reduces duplication
- makes frame-zero semantics permanent and harder to regress
- clarifies what is shared versus note-specific

### Success criteria

- less repeated range/input/too-late code in `tapStep`, `holdStep`, and `touchStep`
- no behavior changes in current runtime tests


## Objective C: Separate Pure Semantic Transition from Command Derivation

### Problem

In slide logic especially, semantic queue transformation and command generation are intertwined.

### Goal

Keep the semantic transition pure and inspectable, and derive render/audio effects from transition deltas afterward.

### Proposed design

Split slide stepping into two layers:

1. a pure semantic transition returning:
   - new slide note state
   - semantic transition flags
   - optional judged result

2. a side-effect derivation layer returning:
   - `AudioCommand`
   - `RenderCommand`

The same pattern can later be applied to other note families if useful.

### Benefits

- easier semantic review
- easier equivalence testing
- easier proof reuse later

### Success criteria

- `slideStep` becomes shorter and easier to audit
- render/audio tests still pass unchanged


## Objective D: Make Scheduler Ordering an Explicit Policy

### Problem

The scheduler currently applies note families in a fixed order embedded directly in code.

This is acceptable, but it is not clearly documented as semantic policy.

### Goal

State clearly whether the subsystem order is intended semantic behavior or just an implementation detail.

### Proposed design

Document in `LnmaiCore/Scheduler.lean`:

- current processing order
- why this order is chosen
- whether shared input competition relies on that order

Add tests for cases where order matters:

- simultaneous eligible button notes and holds
- touch versus touch-hold competition
- same-frame shared-input cases with slide interaction

### Benefits

- future refactors become safer
- semantic divergence from the reference is easier to detect


## Objective E: Add a Semantic Equivalence Test Matrix

### Problem

Current runtime tests are strong but still selective.

### Goal

Turn semantic parity into a checklist-driven test matrix.

### Categories to cover

#### Tap

- frame-zero hit
- exact early boundary
- exact late boundary
- too-late miss
- queue-blocked same-lane second note
- multi-click same frame

#### Hold

- frame-zero head hit
- head miss with later body behavior
- classic end behavior
- modern end behavior
- sensor fallback behavior

#### Touch

- frame-zero hit
- button-ring fallback
- group majority same frame
- too-late miss

#### Touch Hold

- frame-zero head
- majority-shared head result
- body hold / release edge cases

#### Slide

- frame-zero start consumption
- exact checkable threshold
- too-late finalization
- conn parent-child propagation
- wifi special progression
- delayed judged-to-ended event

#### Mixed charts

- multiple note families in one frame
- shared input competition
- score / combo / AP correctness

### Benefits

- semantic claims become evidence-backed
- refactors become safer


## Objective F: Add Result-Level Golden Tests

### Problem

Single-step tests are necessary but not sufficient.

### Goal

Validate that end-to-end replay yields the right cumulative gameplay results.

### Golden outputs to compare

- event sequence
- event kinds and note indices
- grades
- diffs
- final combo state
- AP / AP+ result
- final score counters

### Suggested method

Use `LnmaiCore.Proofs.Runtime` for chart replay tests and compare against expected event lists and final state summaries.


## Objective G: Clarify Semantic Layers in the Codebase

### Problem

The current modules are already reasonable, but the conceptual layers are not yet fully explicit.

### Goal

Make the codebase easier to understand by documenting and preserving these layers:

1. chart lowering / queue construction
2. timed input framing
3. note-local semantic transition
4. frame scheduler ordering
5. score accumulation
6. replay/proof API

### Suggested action

Add short module-level comments where needed and keep future helpers inside the right layer.


## Suggested Refactor Order

The recommended order is:

### Phase 1: Safety Before Cleanup

Status: complete

1. expand mixed-family semantic tests
2. expand mixed-chart replay-level tests
3. add more slide and scheduler-order edge-case tests

Reason:

- refactors should happen only after we have stronger semantic fences

### Phase 2: Timing Boundary Cleanup

Status: complete

4. introduce explicit frame-window abstraction
5. remove direct inline frame-boundary branching from event logic

Reason:

- this makes the input semantics cleaner everywhere

### Phase 3: Judgment Logic Cleanup

Status: complete for tap / touch / hold-head immediate judgment logic

6. extract shared tap-like immediate judgment helpers
7. simplify `tapStep`, `holdStep`, `touchStep`

Reason:

- this is the highest duplication cluster

### Phase 4: Slide Semantic Cleanup

Status: complete

8. separate slide semantic transition from render/audio derivation
9. add equivalence tests for conn and wifi edge cases

Reason:

- slide logic is the densest semantic code

### Phase 5: Policy Documentation

Status: complete

10. document scheduler order as semantic policy
11. add tests for order-sensitive shared-input cases


## What Not to Do

The refactor should avoid the following traps:

### Do not mirror Unity classes one-to-one

That would reduce clarity without improving semantic parity.

### Do not over-generalize all notes into one giant abstraction

Tap, hold, touch, and slide share concepts, but not enough to justify one monolithic generic engine.

### Do not hide timing boundaries in clever abstractions

The code should stay explicit about interval endpoints and offset application.

### Do not mix semantic cleanup with unrelated feature changes

This roadmap is about semantic clarity and parity, not feature expansion.


## Definition of Done

This refactor direction should be considered successful when the following are true:

1. frame boundary semantics are explicit and no longer ad hoc
2. tap / hold-head / touch immediate-judge logic is centralized enough to avoid repetition
3. slide semantic code is easier to inspect separately from command generation
4. scheduler order is documented and tested where relevant
5. replay-level and result-level semantic tests cover all major note families
6. we can state semantic parity claims with evidence instead of intuition


## Immediate Next Actions

If continuing from the current codebase, the best next concrete actions are:

1. add broader mixed-family same-frame competition tests
2. add a compact mixed-chart result-level golden replay
3. introduce a `FrameWindow` abstraction
4. refactor tap / hold-head / touch same-frame judgment through shared helpers


## Final Position

The current implementation is already a strong semantics-first runtime core.

It is good enough to continue building on, but not yet clean enough to treat as the final semantic model.

The right direction is:

- keep the pure-state architecture
- refactor toward clearer shared semantics
- strengthen semantic equivalence tests
- avoid chasing Unity implementation details unless they affect observable gameplay outcomes
