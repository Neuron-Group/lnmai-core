# MajdataPlay ↔ Lean Runtime Semantic Model

This document defines a concrete and abstract semantic model for comparing
`../reference/MajdataPlay` with `lnmai-core`.

The goal is not to restate source code line by line. The goal is to provide a
precise shared vocabulary so that:

- Unity-style object/update logic can be interpreted in the same language as
  Lean step functions
- parity claims can be stated as model obligations instead of ad hoc intuition
- newly found mismatches can be classified as either
  - implementation bugs,
  - modeling differences, or
  - still-unverified regions

This document avoids guessing. Where behavior is asserted as reference truth,
it is grounded in `../reference/MajdataPlay`.

Important reminder:

- when checking any proposition in this document, also check the original
  runtime logic in `../reference/MajdataPlay`; do not treat this document as a
  substitute for the reference implementation

## 1. Scope

This model covers the runtime judgment layer for:

- tap
- hold head and hold body
- touch
- touch-hold head and touch-hold body
- shared frontier / queue unlocking
- per-frame click consumption
- same-frame subsystem ordering

This document does not attempt to fully model:

- rendering animation internals
- audio playback internals
- full slide semantics beyond scheduler-order interaction vocabulary
- Unity scene lifecycle beyond the gameplay-relevant update order

Those concerns may be observationally relevant, but they are not the primary
 semantic layer discussed here.

## 2. Two Levels of Model

We use two models.

### 2.1 Concrete operational model

This is implementation-facing. It contains explicit runtime state:

- current time
- per-family local note queues
- shared frontier indices
- note-local substates
- frame input click counts and held state
- group-share accumulators
- emitted judgment events

Both `MajdataPlay` and Lean are interpreted into this model.

### 2.2 Abstract semantic model

This is proof-facing. It erases implementation details and keeps only:

- unlockedness of a note
- ability to consume input this frame
- resulting judgment outcome
- frontier advance conditions
- order-sensitive consumption laws

The abstract model is where long-term parity obligations should be stated.

## 3. Time Model

### 3.1 Lean time

Lean represents time using exact microsecond `TimePoint` and `Duration`
values. Core timing constants live in `LnmaiCore/Constants.lean`.

Relevant constants:

- tap good area: `LnmaiCore/Constants.lean:27`
- touch good area: `LnmaiCore/Constants.lean:47`
- hold head ignore length: `LnmaiCore/Constants.lean:61`
- touch-hold head ignore length: `LnmaiCore/Constants.lean:63`
- generic judgeable early window: `LnmaiCore/Constants.lean:114`

### 3.2 MajdataPlay time

`MajdataPlay` uses floating-second timing derived from Unity frame updates.
Judgeable ranges and too-late checks are set per note family:

- tap judgeable range and checks in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TapDrop.cs`
- hold judgeable range and checks in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs:249` and `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs:507`
- touch judgeable range and checks in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs:166` and `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs:288`
- touch-hold judgeable range and checks in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs:258` and `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs:543`

### 3.3 Frame interpretation law

Both runtimes can be interpreted as computing a function

`step : State × FrameInput -> State × Events × SideEffects`

where `FrameInput` includes:

- click events in this frame
- held-button / held-sensor state
- frame delta

Lean makes this explicit in `LnmaiCore/Scheduler.lean:502`.

## 4. Scheduler Order

### 4.1 Lean order

Lean scheduler order is explicit in `LnmaiCore/Scheduler.lean:507`:

1. tap
2. hold
3. touch
4. touch-hold
5. slide

This order is part of semantic behavior, not just implementation structure.

### 4.2 MajdataPlay order

`NoteManager.OnPreUpdate` drives updater order in
`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:124`.
The relevant updater order shown there is:

- tap updater
- hold updater
- slide updater
- touch updater
- touch-hold updater

For note families discussed here, the important reference order is:

- tap before hold
- touch before touch-hold

This matches Lean’s order-sensitive intent for those families, even though
Lean runs slide after touch-hold while `MajdataPlay` runs slide earlier in the
Unity updater list.

This document only treats tap/hold and touch/touch-hold ordering claims where
the source has been checked directly.

## 5. Concrete State Vocabulary

We separate state into three layers.

### 5.1 Note-local state

Examples in Lean:

- tap state: `LnmaiCore/Lifecycle.lean:56`
- hold substate: `LnmaiCore/Lifecycle.lean:130`

Concrete fields relevant for parity:

- note timing
- note kind
- lane / sensor position
- current substate
- queue index within a shared frontier family
- timing diff stored on judgment

### 5.2 Family-local queue state

Family-local queues store concrete note objects in order. Lean uses
`ZoneQueue` in `LnmaiCore/InputModel.lean:135`.

These local queues answer:

- which concrete note object is next for that family on that lane / area?
- should the family-local queue remove or retain the note object after a head
  judgment?

### 5.3 Shared frontier state

Shared frontier state answers a different question:

- which logical position is currently unlocked for judgment in a shared lane or
  area?

In `MajdataPlay`, this appears as:

- button-ring frontier via `NoteManager.IsCurrentNoteJudgeable(in TapQueueInfo)` at
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:381`
- touch frontier via `NoteManager.IsCurrentNoteJudgeable(in TouchQueueInfo)` at
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:395`

In Lean, this is represented by:

- `buttonQueueFrontiers` in `LnmaiCore/InputModel.lean:179`
- touch frontier via `touchQueues.currentIndex` within each sensor area queue

## 6. Concrete Interpretation of MajdataPlay

### 6.1 Button-lane family: tap + hold share one frontier

`MajdataPlay` assigns both tap and hold `TapQueueInfo`:

- tap creation uses `_noteIndex[startPos]++` in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs:526`
- hold creation also uses `_noteIndex[startPos]++` in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs:585`

Therefore tap and hold are not independent queue families for unlocking. They
share one button-lane frontier.

Frontier advancement on hold head happens in two places:

- too-late hold head miss: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs:487`
- successful hold head judgment: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs:539`

### 6.2 Sensor family: touch + touch-hold share one frontier

`MajdataPlay` assigns both touch and touch-hold `TouchQueueInfo`:

- touch creation uses `_touchIndex[sensorPos]++` in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs:694`
- touch-hold creation uses `_touchIndex[sensorPos]++` in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs:764`

Frontier advancement on touch-hold head occurs in:

- too-late head miss: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs:534`
- successful head judgment: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs:575`

### 6.3 Click-consumption model

Reference click consumption is explicit in `NoteManager`:

- button click usage in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:435`
- sensor click usage in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:458`

Thus a click is a frame-local consumable resource. Any parity model must track
consumption order, not just note-local timing windows.

## 7. Concrete Interpretation of Lean

### 7.1 Shared frontiers now represented explicitly

Lean now uses:

- shared button frontiers in `LnmaiCore/InputModel.lean:179`
- shared touch frontier through `touchQueues.currentIndex`

Shared button frontier assignment is constructed in
`LnmaiCore/ChartLoader.lean:325` and applied in
`LnmaiCore/ChartLoader.lean:384` by interleaving taps and holds per lane.

Shared touch frontier assignment is constructed in
`LnmaiCore/ChartLoader.lean:302` and applied in
`LnmaiCore/ChartLoader.lean:396` by interleaving touches and touch-holds per
sensor area.

### 7.2 Family-local queues are still present

This is intentional.

Shared frontier answers whether a note is logically unlocked.
Family-local queue answers which concrete note object for that family is being
stepped and retained.

This is a semantic decomposition, not duplication.

### 7.3 Lean step entrypoint

The concrete Lean runtime step is `LnmaiCore/Scheduler.lean:502`.

Relevant subsystem processors:

- tap: `LnmaiCore/Scheduler.lean:187`
- hold: `LnmaiCore/Scheduler.lean:293`
- touch: `LnmaiCore/Scheduler.lean:382`
- touch-hold: `LnmaiCore/Scheduler.lean:324`

## 8. Abstract Semantic Model

We define an abstract note descriptor:

- family `F ∈ {tap, holdHead, touch, touchHoldHead, ...}`
- lane or area `L`
- scheduled timing `T`
- shared frontier index `Ishared`
- local-family queue position `Ilocal`
- local state `S`

We define a shared frontier map:

- `B : ButtonZone -> Nat`
- `S : SensorArea -> Nat`

We define a frame click budget:

- `Cbutton : ButtonZone -> Nat`
- `Csensor : SensorArea -> Nat`

Then an abstract note is eligible for direct head judgment only if:

1. it is within its family timing gate
2. its shared frontier index is current for its shared family
3. its family-local head object is the one being stepped
4. a click budget remains in the relevant input source

The exact family timing gate differs by note kind.

### 8.1 Species-by-species semantic signature

To cover the remaining runtime without copying Unity realization details, each
note species should be modeled by the same semantic signature:

- `Spawn`: when the note becomes present in runtime state
- `Unlock`: when shared-frontier and local-head conditions allow judgment work
- `Consume`: which frame-local resource it may consume
- `Share`: which same-frame derived facts it may publish to sibling notes
- `Advance`: which frontier or local queue moves after judgment or miss
- `Persist`: which body or post-head state remains into later frames
- `Emit`: which judgment event, if any, becomes externally visible

This signature is general enough to compare Lean and `MajdataPlay` while still
ignoring purely Unity-specific object lifecycle details.

### 8.2 Remaining species model surface

Under that signature, the runtime still needs explicit semantic coverage for:

- `tap`: early-window entry, click-source choice, miss timing, frontier advance
- `hold`: head judgment, head miss, body-held, body-released, force-end,
  classic-vs-deluxe branching
- `touch`: late-only judgment, group-share acceptance, button-ring fallback,
  miss timing
- `touch-hold`: head judgment, shared-group resolution, body majority
  reactivation, force-end, shared touch-frontier advance
- `slide`: activation, queue progression, skip policy, parent/child coupling,
  delayed visible judgment, finish propagation

### 8.3 Auxiliary semantic resources

Besides note-local state, a faithful abstract model must treat the following as
first-class semantic resources:

- frame window inclusion policy
- click budgets per button and sensor
- held-state snapshots for button and sensor channels
- shared frontier maps for button lanes and touch areas
- touch-group and touch-hold-group accumulators
- parent-child relation for connected slides
- delayed side-effect channels for score, audio, and render emission

In Lean, the frame-window and timed-input abstraction already appears in
`LnmaiCore/InputModel.lean:54`, `LnmaiCore/InputModel.lean:63`, and
`LnmaiCore/InputModel.lean:99`.

## 9. Core Laws

### 9.1 Frontier monotonicity

Shared frontiers never decrease.

### 9.2 No click reuse

Once a click is consumed by an earlier subsystem or earlier note in the same
subsystem, it cannot be consumed again in the same frame.

### 9.3 Shared-family blocking

If note `n2` has a strictly later shared frontier index than pending note `n1`
on the same shared family, then `n2` cannot consume input before `n1` causes
the frontier to advance.

This law is the core reason taps cannot bypass earlier same-lane hold heads in
the reference model.

### 9.4 Frontier advancement law

When a head judgment or head miss is specified by the reference to call
`NextNote` or `NextTouch`, the corresponding shared frontier must advance in
Lean as well.

### 9.5 Same-frame order law

If subsystem `A` runs before subsystem `B`, then `A` may consume frame-local
click budget before `B` is stepped.

This is essential for:

- tap before hold
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`)
- touch before touch-hold
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`)

### 9.6 State persistence law

If a note species has a post-head body phase, then head judgment does not by
itself imply event emission or object removal. The semantic model must record
an intermediate persistent state that remains judge-relevant in later frames.

This matters for hold, touch-hold, and slide families.

Reference files to check:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`

### 9.7 Shared-result publication law

If one note publishes a same-frame shared result to a group accumulator, a
later note in scheduler order may resolve from that accumulator without
consuming its own direct click resource, provided the reference family rule
permits such sharing.

This matters for touch and touch-hold groups.

Reference files to check:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`

### 9.8 Frame-window law

Timed inputs are not just sets of events; they are interpreted through a frame
window inclusion rule. Any parity argument about frame-zero behavior or replay
timing must quantify over this window rule explicitly.

In Lean this is the exact-point rule for zero-delta frames and the left-open,
right-closed rule for positive-delta frames in `LnmaiCore/InputModel.lean:63`.

### 9.9 Delayed-emission law

Some note families may become internally resolved before their externally
visible event is emitted or before all render-side consequences settle. A full
abstract model must distinguish internal resolution from externally emitted
judgment.

This especially matters for slides.

Reference files to check:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs`
- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`

## 10. Interpreting the Known Fixed Gaps

### 10.1 Touch-hold frontier advancement

Concrete gap found:

- `MajdataPlay` advances shared touch frontier after touch-hold head judgment
- Lean originally advanced only `touchHoldQueues`

This is now fixed in `LnmaiCore/Scheduler.lean:324`,
`LnmaiCore/Scheduler.lean:370`, and `LnmaiCore/Scheduler.lean:515`.

Concrete regression:

- `LnmaiCore/RuntimeTests.lean:2221`

### 10.2 Tap/hold shared frontier

Concrete gap found:

- `MajdataPlay` assigns tap and hold from one `_noteIndex[startPos]`
- Lean originally split them into independent family frontiers

This is now modeled by shared button indices and `buttonQueueFrontiers`.

Concrete regressions live around:

- `LnmaiCore/RuntimeTests.lean:2070`
- `LnmaiCore/RuntimeTests.lean:2090`

### 10.3 Slide queue and finish propagation

Concrete reference facts:

- slide queue completion is based on the maximum remaining unfinished queue
  length in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs:44` and
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs:60`
- slide becomes checkable when its start timing crosses the `-0.05s` threshold,
  or when a connected child inherits parent finish state, in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:517` and
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:522`
- slide sensor progression checks the first queue item first, conditionally the
  second item, then the first again in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:430`
- parent finish propagation is explicit in
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:581`

Lean currently models the same semantic shape with `SlideNote.queueTracks`,
`slideShouldBeCheckable`, `slideStepSemantic`, `updateSlideParentFlags`, and
`forceFinishParentSlides`.

Concrete regressions and proof-facing witnesses for the slide path live around:

- `LnmaiCore/RuntimeTests.lean:1809`
- `LnmaiCore/RuntimeTests.lean:1862`
- `LnmaiCore/RuntimeTests.lean:2506`

## 11. Evidence-Backed Parity Spots

The following regions have direct reference-grounded evidence plus Lean
regressions or executable checks. This section does not claim total parity for
an entire note family; it only records the specific behaviors that were
checked.

- touch uses late-only grading with early bail outside first-perfect
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`)
- touch-hold head can judge on frame zero when eligible
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`)
- button-ring touch input takes priority before sensor input when enabled
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`)
- touch-group strict majority is `Percent > 0.5`
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`)
- touch-hold head judgments advance the shared touch frontier
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`)
- same-lane earlier hold head blocks later tap through a shared button frontier
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs`)

## 12. Still Unverified or Only Partially Verified

These regions still need more careful parity work:

- complete too-late miss parity for all head families under all scheduler
  orderings
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TapDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`)
- exact parity of group-share propagation under larger mixed touch/touch-hold
  sets
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs`)
- full slide interaction ordering against Unity updater structure
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/SlideUpdater.cs`,
  `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`)
- proof that every abstract frontier law is preserved by every constructor path
  used in chart loading
  (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs`)

## 13. Coverage Matrix

The right question is not “do we have a model?” but “which semantic cells are
covered?” The following matrix records that.

Status vocabulary:

- `Modeled`: an explicit semantic account exists in this document and Lean
- `Checked`: reference-grounded source inspection was done
- `Regressed`: there is a focused executable regression or proof witness
- `Open`: still incomplete, ambiguous, or only partially checked

### 13.1 Tap

- spawn / queue placement: `Modeled`, `Checked`
- judgeable entry and early-window semantics: `Modeled`, `Open`
- button-vs-sensor fallback click choice: `Modeled`, `Open`
- same-lane shared-frontier blocking against hold: `Modeled`, `Checked`, `Regressed`
- too-late miss boundary under all frame timings: `Modeled`, `Open`

### 13.2 Hold

- shared button-frontier unlock: `Modeled`, `Checked`, `Regressed`
- head judgment timing: `Modeled`, `Open`
- head miss and frontier advance: `Modeled`, `Checked`, `Open`
- classic-vs-deluxe body evolution: `Modeled`, `Open`
- release-ignore and re-press behavior: `Modeled`, `Open`
- final event grading after body phase: `Modeled`, `Open`

### 13.3 Touch

- shared touch-frontier unlock: `Modeled`, `Checked`
- late-only judgment region: `Modeled`, `Checked`, `Regressed`
- button-ring priority over sensor path: `Modeled`, `Checked`, `Regressed`
- same-frame group-share publication: `Modeled`, `Checked`, `Regressed`
- too-late miss boundary under replay/frame-window variations: `Modeled`, `Open`

### 13.4 Touch-Hold

- shared touch-frontier unlock: `Modeled`, `Checked`
- head judgment on frame zero: `Modeled`, `Checked`, `Regressed`
- head judgment advancing shared touch frontier: `Modeled`, `Checked`, `Regressed`
- resolution from shared touch-group result: `Modeled`, `Checked`, `Regressed`
- body majority reactivation: `Modeled`, `Regressed`, `Open`
- larger mixed-group propagation and ordering: `Modeled`, `Open`

### 13.5 Slide

- activation and internal progress state: `Modeled`, `Checked`, `Open`
- queue-consumption and area-policy semantics: `Modeled`, `Checked`, `Open`
- skip rules and connected-slide lowering interaction: `Modeled`, `Checked`, `Open`
- parent-child finish propagation: `Modeled`, `Checked`, `Regressed`, `Open`
- delayed final event emission: `Modeled`, `Checked`, `Regressed`, `Open`
- ordering against other families in reference runtime: `Modeled`, `Checked`, `Open`

### 13.6 Cross-cutting

- frame-window semantics for timed replay: `Modeled`, `Regressed`
- click-budget consumption and no-reuse law: `Modeled`, `Checked`
- constructor preservation of queue/frontier invariants: `Modeled`, `Open`
- score/audio/render projection from judgments: `Modeled`, `Open`

## 14. Remaining Model Obligations

To claim broad runtime faithfulness without fighting Unity details, the next
mathematical obligations should be:

1. define one abstract transition relation per note species using the semantic
   signature from Section 8.1
2. define a cross-species scheduler relation that sequences those transitions
   and threads shared resources
3. state preservation lemmas for queue-head, frontier-monotonicity, and
   click-budget monotonicity
4. state observational equivalence lemmas for emitted judgments, not raw object
   fields
5. attach every open cell in the coverage matrix to either
   - a checked reference anchor,
   - a reduced regression witness, or
   - an explicitly postponed assumption

## 15. Slide Witness Checklist

For the slide family, the next reduced witnesses should target:

1. checkability threshold at exactly `-0.05s`
   (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`)
2. queue draining when first/second areas finish in one step
   (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`)
3. `parentPendingFinish` becoming sufficient for a connected child
   (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`)
4. `SetParentFinish`-style propagation from child to parent
   (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`)
5. judge emission only after `IsFinished` is true
   (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`,
   `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs`)
6. delayed end-to-emit separation for already-judged slides
   (`../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs`,
   `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideBase.cs`)

## 16. Recommended Next Usage

When a new suspected mismatch is found, the workflow should be:

1. identify the reference operation in `MajdataPlay`
2. state the suspected law in the abstract model vocabulary
3. find the Lean concrete interpreter site
4. write a reduced regression against that law
5. patch only after the law has a concrete failing witness
6. re-check the original `MajdataPlay` source file for that proposition before
   concluding parity or divergence

This keeps runtime parity work disciplined and avoids guessing from surface
similarity.

## 17. Reference Map

Useful entrypoints:

- Lean scheduler: `LnmaiCore/Scheduler.lean:502`
- Lean runtime states: `LnmaiCore/Lifecycle.lean:62`
- Lean queue state: `LnmaiCore/InputModel.lean:135`
- Lean constants: `LnmaiCore/Constants.lean:14`
- MajdataPlay note manager: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:124`
- MajdataPlay tap behavior: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TapDrop.cs:321`
- MajdataPlay hold behavior: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/HoldDrop.cs:501`
- MajdataPlay touch behavior: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchDrop.cs:282`
- MajdataPlay touch-hold behavior: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/TouchHoldDrop.cs:537`
