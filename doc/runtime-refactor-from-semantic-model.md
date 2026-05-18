# Runtime Refactor Proposal from the Semantic Model

This document proposes a refactor direction for `lnmai-core` based on the
semantic model in `doc/majdataplay-lean-semantic-model.md`.

The goal is not “more abstraction” for its own sake. The goal is to make the
runtime implementation:

- simpler
- more uniform across note species
- easier to check against `../reference/MajdataPlay`
- easier to prove correct in Lean

Important reminder:

- this document is a refactor proposal, not a replacement for reference-based
  parity checking
- if any proposed abstraction conflicts with observed behavior in
  `../reference/MajdataPlay`, the reference behavior wins
- every refactor step should preserve the concrete propositions listed in
  `doc/majdataplay-lean-semantic-model.md`

## 1. Motivation

The current runtime already has a strong semantic shape, but it is distributed
across several note-specific implementations.

Useful current anchors:

- frame scheduler entrypoint: `LnmaiCore/Scheduler.lean:510`
- note lifecycles: `LnmaiCore/Lifecycle.lean:55`
- runtime state and frontiers: `LnmaiCore/InputModel.lean:135`
- chart-loaded queue construction: `LnmaiCore/ChartLoader.lean:302`

The semantic document shows that many note families follow the same abstract
pattern:

1. determine whether the note is unlocked
2. determine whether it may consume a frame resource
3. optionally consume that resource
4. optionally publish a shared result
5. advance a frontier or family-local queue
6. persist into a later state or emit a final event

Today, these steps appear repeatedly in note-specific forms.

## 1.1 Why This Is More Plausible Now

After the latest parity audits, the runtime is in a better position for this
refactor than it was when this proposal was first drafted.

Important audited improvements now already present in the code:

- explicit shared button and touch frontiers in
  `LnmaiCore/InputModel.lean:179` and `LnmaiCore/InputModel.lean:180`
- explicit runtime touch shared indices in `LnmaiCore/Lifecycle.lean:364`
- reference-aligned unlockedness law `index <= currentIndex` reflected in the
  scheduler for both button and touch families in `LnmaiCore/Scheduler.lean:170`
  and `LnmaiCore/Scheduler.lean:283`
- centralized touch-group accumulator behavior in
  `LnmaiCore/Scheduler.lean:269`
- audited short modern hold body-window disabling in
  `LnmaiCore/Lifecycle.lean:270`
- audited touch waiting-state too-late handling under large frame jumps in
  `LnmaiCore/Lifecycle.lean:399`

These changes mean the runtime already has the semantic resource split that a
small kernel would need. So the refactor is now plausible as a controlled
reorganization rather than a speculative redesign.

## 2. Refactor Objective

The objective is to reorganize the runtime around a small semantic kernel.

That kernel should make explicit:

- shared resources
- per-species transition rules
- scheduler composition
- event projection

The kernel should not try to erase all differences between species. It should
factor out the repeated control structure while leaving species-specific timing
and grading laws explicit.

## 3. Non-Goals

This proposal does not aim to:

- mimic Unity object lifecycle details literally
- merge all note species into one giant state machine
- hide important operational differences such as updater order
- refactor slides first
- remove reference-driven regressions in favor of pure abstraction

## 4. Proposed Runtime Layers

The refactor should separate the runtime into four conceptual layers.

### 4.1 Semantic resource layer

This layer holds the frame-global resources consumed or observed by note steps.

Candidate contents:

- current time
- frame delta
- button click budget
- sensor click budget
- held button snapshot
- held sensor snapshot
- shared button frontiers
- shared touch frontiers
- touch-group accumulators
- touch-hold-group accumulators

This is mostly present already in `LnmaiCore/InputModel.lean:173` and threaded
through `LnmaiCore/Scheduler.lean:510`.

Refactor direction:

- define one explicit runtime resource record that is passed through all
  subsystem transitions
- make resource updates the primary scheduler effect

### 4.2 Species transition layer

Each note species should define a transition relation over:

- note-local state
- family-local queue context
- shared resource context

Each species module should provide the same semantic interface:

- unlock predicate
- input-consumption policy
- shared-result import policy
- shared-result export policy
- frontier-advance policy
- persistence policy
- event-emission policy

This interface can be implemented either as a structure of functions or as a
namespace of definitions per species.

### 4.3 Scheduler layer

The scheduler should become a small composition engine.

Instead of encoding most logic directly in note-family-specific loops, it
should primarily do three things:

1. choose subsystem order
2. run a species transition over the current family-local queue or active list
3. thread updated shared resources and emitted events forward

The important semantic order from `MajdataPlay` must stay explicit. See
`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`
and `doc/majdataplay-lean-semantic-model.md`.

### 4.4 Projection layer

Score, audio, and render commands should be treated as projections from already
computed semantic outcomes.

Refactor direction:

- keep judgment computation separate from side-effect formatting
- avoid mixing queue/state mutation with render-command formatting when possible

This is already partly visible in `LnmaiCore/Scheduler.lean:473` and
`LnmaiCore/Scheduler.lean:490`.

## 5. Generic Semantic Skeleton

The semantic model suggests a reusable skeleton for head-like judgments.

For a note `n`, one frame step should conceptually compute:

1. `Unlocked(n, resources, queueCtx)`
2. `Consumable(n, resources)`
3. `SharedIn(n, resources)`
4. `StepLocal(n, directInput?, sharedInput?)`
5. `Advance(localResult, resources, queueCtx)`
6. `SharedOut(localResult, resources)`
7. `Emit(localResult)`

This skeleton is now more realistic because the current runtime already exposes
most of the needed axes separately:

- frontiers as first-class fields
- family-local queues as separate structures
- click budgets as explicit cursor-threaded resources
- group-share behavior as explicit accumulator operations

What is still species-specific and should remain explicit in early refactors:

- timing windows and too-late boundaries
- button versus sensor fallback order
- hold/touch-hold body continuation semantics
- slide queue progression and delayed emission

This skeleton already covers most of:

- tap
- hold head
- touch
- touch-hold head

The body phase for hold and touch-hold extends this skeleton with a persistent
phase transition. Slides extend it with queue traversal instead of direct click
consumption.

## 6. Family Pairs that Want Unification

### 6.1 Tap and hold-head

These share:

- button-lane frontier semantics
- button/sensor fallback input sources
- too-late head unlocking consequences

They differ in:

- what happens after head judgment
- whether final event emission is immediate

Refactor idea:

- one generic button-head transition kernel
- one species-specific continuation for tap versus hold body

### 6.2 Touch and touch-hold-head

These share:

- touch-area frontier semantics
- button-ring/sensor priority rules
- touch-group sharing machinery

They differ in:

- touch-hold persistence into body state
- touch-hold body majority reactivation

Refactor idea:

- one generic touch-head transition kernel
- one species-specific continuation for touch versus touch-hold

Constraint from current audits:

- do not try to unify touch-hold body semantics into that first kernel
- keep touch-group result import/export explicit until the new abstraction has a
  reduced witness for strict-majority preservation and same-frame order

## 7. Slides Should Be Refactored Last

Slides are semantically richer than the other families.

They involve:

- track-local queue traversal
- skippable versus non-skippable areas
- parent-child coupling for connected slides
- delayed emission after internal resolution
- queue-progress render side effects

Reference files that must remain authoritative:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/SlideUpdater.cs`

So the recommended sequence is:

1. unify tap/hold-head semantics
2. unify touch/touch-hold-head semantics
3. extract shared scheduler/resource threading
4. only then revisit slides

## 8. Candidate Lean-Level Abstractions

These are plausible abstraction points for Lean code, not final API claims.

### 8.1 Shared frontier context

A small abstraction for:

- current frontier value
- note shared index
- unlocked predicate
- advance rule

This would reduce repeated unlockedness/advance logic currently spread across
`LnmaiCore/Scheduler.lean:170`, `LnmaiCore/Scheduler.lean:173`,
`LnmaiCore/Scheduler.lean:283`, and `LnmaiCore/Scheduler.lean:286`.

Important design constraint:

- this abstraction must preserve the distinction between
  - shared unlockedness, and
  - family-local queue headness
- it must not collapse those two checks into one predicate

### 8.2 Consumable click resource

A small abstraction for:

- click source type
- availability query
- consume operation
- priority order among multiple sources

This would make button/sensor fallback policy more declarative.

### 8.3 Shared-result accumulator

A small abstraction for:

- group membership
- strict-majority predicate
- publication rule
- lookup rule

This would simplify touch and touch-hold group-share handling.

Important design constraint from the current audited behavior:

- once strict majority is already reached, the stored shared result/diff should
  no longer be overwritten, matching
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/Misc/Notes/Touch/TouchGroup.cs`
- any accumulator abstraction must keep that law explicit

### 8.4 Transition result type

A normalized step result could include:

- updated note
- queue advance request
- shared frontier advance request
- shared publication request
- emitted event
- projection hints

This would make the scheduler more uniform and proof-friendly.

## 9. Expected Benefits

If done carefully, the refactor should bring:

- less duplicated logic across note species
- easier parity review against `MajdataPlay`
- smaller scheduler functions
- clearer invariants around queue/frontier/resource interaction
- more local proofs and better theorem statements
- easier addition of reduced regression witnesses

## 10. Main Risks

### 10.1 Over-abstraction

If we abstract before locking down operational facts, the implementation may
look elegant but drift from `MajdataPlay`.

### 10.2 Hidden order dependence

Some behavior depends on same-frame order. If the abstraction hides that order,
it will become harder to reason about parity.

### 10.3 Slide contamination

Trying to force slides into the same abstraction too early may make the design
worse instead of better.

There is an additional nuance here from the current codebase:

- Lean already uses a recursive semantic update structure for slide
  segmentation/address propagation that is more plausible than a literal Unity
  mutation order in some cases
- this proposal should not treat every slide implementation difference from
  `MajdataPlay` as a bug candidate
- only reduced observational mismatches should justify slide-path changes

### 10.4 Proof-oriented distortion

An implementation can become convenient for proofs but inconvenient for runtime
parity work if it stops reflecting the operational structure that matters.

## 11. Recommended Refactor Sequence

### Phase 1

- extract shared button-head kernel for tap + hold-head
- preserve current tests and regressions
- do not alter slide code

Recommended concrete scope:

- extract only head-like unlock/consume/advance structure
- do not fold hold body semantics into the first kernel
- preserve the existing same-frame scheduler order in visible code

### Phase 2

- extract shared touch-head kernel for touch + touch-hold-head
- centralize touch-group accumulator operations

Recommended concrete scope:

- keep touch-hold body continuation separate
- make strict-majority result preservation and same-frame group-share ordering
  explicit acceptance checks

### Phase 3

- introduce a normalized scheduler resource record
- simplify scheduler threading around uniform transition outputs

Recommended concrete scope:

- start as a façade over existing `GameState`/`FrameInput`-derived fields rather
  than replacing all state threading at once
- do not hide subsystem order inside higher-order combinators that are hard to
  review against `MajdataPlay`

### Phase 4

- revisit chart-loader invariants and connect them to the new kernel interfaces

### Phase 5

- revisit slide refactor only after the previous layers are stable and parity is
  re-checked against `MajdataPlay`

Recommended concrete scope:

- treat slide refactor as optional until a concrete benefit is shown
- preserve the possibility that Lean's recursive segmentation/update structure
  is the better design unless an observational mismatch is demonstrated

## 12. Acceptance Criteria

A refactor in this direction is successful only if all of the following remain
true:

- existing reduced regressions still pass
- no checked proposition in
  `doc/majdataplay-lean-semantic-model.md` becomes false
- scheduler order remains explicit and reviewable
- frontier/resource invariants become easier, not harder, to state
- the code becomes shorter or clearer in the hot paths it touches
- the refactor does not erase the audited distinction between shared-frontier
  unlockedness and family-local queue-head blocking
- the refactor does not weaken the newly checked edge cases around short modern
  holds, large-delta touch misses, or strict-majority result preservation

## 13. Verification Rule

For every proposed abstraction:

1. state the semantic law it is trying to capture
2. identify the current Lean concrete sites it would replace
3. identify the authoritative `MajdataPlay` file(s) for that law
4. add or keep a reduced witness for the law
5. only then perform the refactor

Important reminder:

- when evaluating any abstraction or simplification here, also re-check the
  original logic in `../reference/MajdataPlay`
- this document is a design aid, not a source of truth over the reference repo

## 14. Relationship to the Semantic Model

This document depends on:

- `doc/majdataplay-lean-semantic-model.md`

That document answers:

- what behaviors exist
- which are checked
- which are still open

This document answers:

- how to reorganize the implementation so those behaviors are expressed more
  directly and elegantly
