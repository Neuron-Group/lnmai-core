# Semantic Parity Refactor Task List

This document converts `doc/refactor-roadmap-semantic-parity.md` into an actionable implementation checklist.

The central rule remains:

- match **game semantics** with the reference implementation
- do **not** waste effort matching Unity-specific implementation details


## How To Use This List

Each task should be completed with the following discipline:

1. write or extend tests first where practical
2. make one focused semantic or refactor change
3. rerun narrow validation first
4. rerun broader validation after the task is complete
5. avoid bundling unrelated cleanups into the same patch

Each task includes:

- purpose
- target files
- concrete actions
- acceptance criteria


## Current Status

Completed or materially advanced:

- `Task 1.3` — substantially advanced
- `Task 1.4` — complete
- `Task 2.1` — complete
- `Task 2.2` — complete
- `Task 2.3` — complete
- `Task 3.1` — complete
- `Task 3.2` — complete
- `Task 3.3` — complete
- `Task 3.4` — complete
- `Task 4.1` — complete
- `Task 4.2` — complete
- `Task 4.3` — complete

Verified reference finding:

- `MajdataPlay` input consumption differs between desktop-style one-shot usage and mobile-style click-count usage
- reduced C# harness checks were added under `tools/majdata-harness/` to avoid guessing on mixed same-frame competition behavior
- the harness is also now used proactively for conn-slide and wifi semantic rule-shape checks before tightening Lean-side parity tests

Current next target:

- `Task 5.1 — Document subsystem processing order`


## Phase 1 — Strengthen Semantic Safety Rails

This phase should happen before major refactors.

Status: complete


### Task 1.3 — Add mixed-family same-frame input competition tests

Status: substantially complete

**Purpose**

- make scheduler-order-sensitive semantics visible before cleanup

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- add tests where the same frame contains:
  - tap and hold-head on related inputs
  - touch and touch-hold on overlapping group conditions beyond the current shared-click and shared-group cases
  - slide sensor-hold plus touch input near the same timing
- record what the current scheduler order does
- explicitly assert the expected result

**Acceptance criteria**

- tests describe current semantic policy clearly
- future scheduler refactors will fail loudly if behavior shifts

Implementation note:

- current tests now cover tap/hold-head competition, touch/touch-hold competition, shared queue behavior, repeated same-frame click behavior, and verified touch-hold queue progression


### Task 1.4 — Add mixed-chart result-level golden tests

Status: complete

**Purpose**

- validate final semantic outputs instead of only step-local transitions

**Target files**

- `LnmaiCore/RuntimeTests.lean`
- possibly `LnmaiCore/RuntimeProofAPI.lean`

**Actions**

- create one or two compact mixed charts covering:
  - tap
  - hold
  - touch
  - touch-hold
  - slide
- replay a fixed tactic sequence
- assert final results:
  - emitted event sequence length
  - event grades
  - combo state
  - AP / AP+ outcome when applicable
  - selected score counters

**Acceptance criteria**

- at least one mixed chart replay has end-to-end result assertions

Implementation note:

- compact mixed replay goldens now assert emitted event order, grade sequence, combo state, and DX-score loss
- the proof-facing replay API was refined so explicit initial-state simulations can be reused by future parity tests instead of duplicating local batch-expansion helpers


## Phase 2 — Clean Up Input Time Boundary Semantics

Status: complete


### Task 2.1 — Introduce an explicit frame-window abstraction

Status: complete

**Purpose**

- replace ad hoc time-boundary logic with a named semantic object

**Target files**

- `LnmaiCore/InputModel.lean`
- possibly `LnmaiCore/Time.lean`

**Actions**

- introduce a small abstraction, such as:
  - `FrameWindow`
  - constructor from previous and current time
  - predicate `contains : TimePoint → Bool`
- encode these semantics explicitly:
  - zero-duration frame means exact-point inclusion
  - positive-duration frame means left-open right-closed inclusion

**Acceptance criteria**

- boundary logic is centralized in one place
- no behavior regressions in existing replay tests


### Task 2.2 — Refactor `TimedInputBatch.toFrameInput` to use `FrameWindow`

Status: complete

**Purpose**

- make batch-to-frame conversion easier to reason about

**Target files**

- `LnmaiCore/InputModel.lean`

**Actions**

- remove direct inline boundary branching from `withinFrame`
- use the new window abstraction in button click and sensor click handling

**Acceptance criteria**

- the special-case zero-delta logic disappears from the event-fold body
- code reads as policy rather than patch


### Task 2.3 — Add boundary-focused tests

Status: complete

**Purpose**

- prove the new boundary abstraction preserves semantics

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- add tests for events exactly at:
  - `currentTime`
  - `currentTime - delta`
  - just inside the open boundary
  - just outside the window

**Acceptance criteria**

- boundary inclusion policy is directly tested


## Phase 3 — Unify Tap-Like Immediate Judgment Logic

Status: complete for tap / touch / hold-head immediate judgment logic


### Task 3.1 — Identify shared judgment pattern helpers

Status: complete

**Purpose**

- isolate duplicated semantics before changing code shape

**Target files**

- `LnmaiCore/Lifecycle.lean`

**Actions**

- identify repeated logic across tap / hold-head / touch:
  - too-late check
  - in-range eligibility check
  - input-present decision
  - raw grade calculation
  - converted grade wrapping
- write small internal helper signatures before moving logic

**Acceptance criteria**

- helper boundaries are clear and minimal
- no giant generic abstraction is introduced


### Task 3.2 — Extract tap-like immediate-judge helper(s)

Status: complete

**Purpose**

- reduce duplicated same-frame judgment logic

**Target files**

- `LnmaiCore/Lifecycle.lean`

**Actions**

- extract helper(s) for tap-like immediate judgment
- refactor `tapStep` to use them
- refactor the non-touch-hold head path in `holdStep`

**Acceptance criteria**

- `tapStep` and hold-head logic shrink visibly
- behavior stays unchanged


### Task 3.3 — Extend helper use to touch and touch-hold heads where appropriate

Status: complete

**Purpose**

- unify the repeated pattern without flattening note-specific logic too much

**Target files**

- `LnmaiCore/Lifecycle.lean`

**Actions**

- refactor `touchStep`
- refactor touch-hold head branch in `holdStep`
- preserve group-sharing special cases explicitly

**Acceptance criteria**

- same-frame judge path is conceptually shared across note heads
- group-specific behavior remains readable


### Task 3.4 — Add regression tests for helper refactor

Status: complete

**Purpose**

- ensure the cleanup is semantics-preserving

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- rerun or expand tests covering:
  - frame-zero taps
  - frame-zero hold heads
  - frame-zero touches
  - fallback sensor/button behavior
  - too-late transitions

**Acceptance criteria**

- all relevant preexisting tests still pass unchanged


## Phase 4 — Clean Up Slide Semantic Core

Status: complete


### Task 4.1 — Separate slide semantic transition from side-effect derivation

Status: complete

**Purpose**

- make slide semantics easier to audit and compare

**Target files**

- `LnmaiCore/Lifecycle.lean`

**Actions**

- split the current slide step into:
  - semantic transition logic
  - render/audio derivation logic
- avoid changing behavior while doing this split

**Acceptance criteria**

- semantic state changes can be read without also reading render/audio details


### Task 4.2 — Add conn-slide propagation equivalence tests

Status: complete

**Purpose**

- make parent-finish and child-checkable semantics more robust

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- add cases covering:
  - parent pending finish
  - child becomes checkable
  - force-finish semantics
  - same-frame parent-child interactions where relevant

**Acceptance criteria**

- conn-slide propagation is covered beyond the current narrow tests


### Task 4.3 — Add wifi progression and finalization matrix tests

Status: complete

**Purpose**

- strengthen confidence in the most complex slide family

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- add tests for:
  - center-cleared progress behavior
  - side-tail finish behavior
  - judged-wait delayed final event
  - too-late outcomes

**Acceptance criteria**

- wifi-specific semantics are documented by tests, not only code inspection


## Phase 5 — Clarify Scheduler Semantic Policy


### Task 5.1 — Document subsystem processing order

Status: complete

**Purpose**

- make the scheduler’s order a conscious semantic policy

**Target files**

- `LnmaiCore/Scheduler.lean`

**Actions**

- add a module or function comment describing:
  - subsystem order
  - why it exists
  - whether shared-input competition depends on it

**Acceptance criteria**

- future maintainers can explain why the order is what it is


### Task 5.2 — Add order-sensitive scheduler tests

Status: complete

**Purpose**

- lock in current policy before deeper refactors

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- add tests where changing scheduler order would change observable results

**Acceptance criteria**

- policy is enforced by tests, not just comments


## Phase 6 — Strengthen Score / Result Equivalence Confidence


### Task 6.1 — Add end-to-end combo and AP tests

**Purpose**

- validate cumulative semantic outputs for complete short replays

**Target files**

- `LnmaiCore/RuntimeTests.lean`
- `LnmaiCore/RuntimeProofAPI.lean`

**Actions**

- add scenarios that yield:
  - AP
  - AP+
  - a combo break
  - fast/late counts without combo break

**Acceptance criteria**

- final score state assertions exist for each scenario


### Task 6.2 — Add selected golden score counter tests

**Purpose**

- confirm scoring logic and event accumulation cooperate correctly

**Target files**

- `LnmaiCore/RuntimeTests.lean`

**Actions**

- assert selected final counters for mixed note sequences:
  - tap / hold / touch / slide counts
  - miss count
  - fast / late totals
  - DX score loss deltas where practical

**Acceptance criteria**

- score/result behavior is checked by end-to-end state assertions


## Optional Cleanup Tasks


### Task O.1 — Add a small semantic helper module

**Purpose**

- avoid overcrowding `Lifecycle.lean` and `InputModel.lean`

**Possible target**

- `LnmaiCore/RuntimeSemantics.lean`

**Use cases**

- shared frame-window logic
- shared tap-like judgment helpers
- explicit small semantic helper functions


### Task O.2 — Add a semantic parity checklist section to README

**Purpose**

- communicate project direction clearly

**Target files**

- `README.md`

**Actions**

- add a short section explaining:
  - semantics-first design
  - reference parity goal
  - non-goal of mirroring Unity internals


## Recommended Execution Order

Implement in this order:

1. `Task 1.3`
2. `Task 1.4`
3. `Task 2.1`
4. `Task 2.2`
5. `Task 2.3`
6. `Task 3.1`
7. `Task 3.2`
8. `Task 3.3`
9. `Task 3.4`
10. `Task 4.1`
11. `Task 4.2`
12. `Task 4.3`
13. `Task 5.1`
14. `Task 5.2`
15. `Task 6.1`
16. `Task 6.2`


## Minimal Definition of Progress

The roadmap should be considered meaningfully advanced once the following are done:

- Phase 1 is complete
- Phase 2 is complete
- Phase 3 is complete

At that point the runtime will already be:

- better fenced by tests
- less ad hoc at timing boundaries
- less duplicated in immediate-judgment logic


## Immediate Next Recommended Task

If continuing right now, the best next concrete step is:

- `Task 5.1 — Document subsystem processing order`

Reason:

- the remaining risk has shifted from local slide semantics toward making scheduler policy explicit
- mixed-family and mixed-chart parity fences are now much stronger than before
- documenting processing order is the cleanest way to prevent silent semantic drift in future cleanup work
