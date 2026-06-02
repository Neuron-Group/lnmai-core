# Slide Head / Body Split Refactor Proposal

## Status

Urgent refactor in progress.

Current implementation state:

- design direction is frozen for semantic-parity work
- lowered chart IR now has an explicit `slideHeads` collection alongside slide-body objects
- ordinary slides lower to head + body
- no-head singleton slides and connected child parts lower to body only
- runtime now routes explicit `slideHeads` through tap-family queue loading
- prover timing skeleton / default replay now derives slide-head input from explicit lowered `slideHeads` rather than from slide-body flags such as `isSlideNoHead`
- Rust FFI mirrors the split lowered model via explicit `slideHeads` plus body-side `headTiming`
- slide-body IR now carries an explicit `headTiming` field for the head-trigger timing anchor instead of overloading a generic body `timing` field
- lowered head/body pairs now carry explicit `logicalSlideId`
- lowered slide heads and slide bodies now use distinct `noteIndex` values while sharing `logicalSlideId`
- lowered JSON decoding now requires body `headTiming` explicitly
- proof-facing Lean DSL split views now expose both `logicalNoteIndex` and per-object `runtimeNoteIndex`
- proof-facing Lean DSL aggregate normalized-slide views now expose `headRuntimeNoteIndex`, `bodyRuntimeNoteIndex`, and `primaryRuntimeNoteIndex` via `NormalizedSlideIr.runtimeIds`
- Rust lowered typed mirrors already expose both `logicalSlideId` and per-object `noteIndex`, while aggregate normalized-slide identity is still reconstructed from normalized + lowered data rather than packaged as a dedicated helper

This document proposes the next semantic-parity refactor for slide handling:

- split slide head and slide body into separate lowered objects before runtime
- stop overloading a single slide runtime object with both head-note and path-traversal semantics
- preserve MajdataPlay gameplay semantics while keeping the Lean runtime proof-friendly


## Why This Refactor Is Needed

Current `lnmai-core` slide handling still mixes two distinct concepts:

- slide head semantics
- slide body / path semantics

That becomes awkward in edge cases we now care about:

- singleton slide with no head
- connected child slide with no head
- possible head-only artifact
- possible body-only artifact

The current model can simulate some of these cases, but only by special-casing behavior around a single slide object. That is becoming brittle across:

- parser normalization
- lowered chart IR
- runtime judging
- prover tactic generation
- FFI export

MajdataPlay itself already treats these concerns as partly separate:

- chart-level `note.IsSlideNoHead` controls whether a separate head object is created
- slide-body runtime still judges by path queues
- connected child parts are body-only
- a separate runtime/mod flag controls visual suppression of the slide-head star

So the semantic split is real. Our model should represent it directly instead of encoding it through incidental field combinations.


## Reference Runtime Semantics

This refactor should follow the gameplay semantics visible in `MajdataPlay`.

Observed reference behavior:

- the slide head is created as a separate tap-family star object
- the slide head judges at the slide note timing, not at body `startTiming`
- the slide head participates in the ordinary tap/button queue family
- the slide head consumes the same button/sensor click resources as tap-like notes
- the slide body owns path traversal, connected-slide chaining, queue progression, and final slide judgment
- connected child parts do not create new head notes

Critical semantic distinction:

- chart headlessness and visual head suppression are different concepts in the reference runtime

More concretely:

- chart-level no-head semantics decide whether a separate judged head note exists
- runtime/mod “hide slide head” state only suppresses the visible star behavior on the body-side visual object
- therefore a body-side field named like `IsSlideNoHead` must not be treated as the authoritative statement that no judged head note exists

The Lean refactor should preserve that distinction explicitly instead of overloading one field.


## Target Architecture

Lower every Simai slide into up to two runtime-facing components:

1. `SlideHeadSpec`
2. `SlideBodySpec`

They are linked by a shared logical slide identity, but they are judged independently.

Important boundary:

- a combined head+body slide aggregate may exist before lowering
- after lowering, head and body must be treated as entirely different widgets
- runtime, prover, and FFI must not keep a persistent composite slide widget
- shared logical identity is bookkeeping only, not a runtime ownership container


## Conceptual Model

### Slide head

The head is a note-like object.

In runtime terms, it should be treated as a tap-family note with star-specific presentation rather than as a miniature slide body.

Responsibilities:

- own head timing
- own head lane / zone
- own break / EX head semantics where applicable
- participate in shared tap/button queue ordering
- consume button/sensor click input with tap-style eligibility
- participate in note-count / queue / replay / FFI as an explicit note object

Non-responsibilities:

- slide path traversal
- slide queue progression
- conn parent-child propagation
- slide-body `startTiming`
- slide-body checkability

### Slide body

The body is a path-traversal object.

Responsibilities:

- own start timing, length, judge timing
- own `judgeQueues`
- own connected-slide metadata
- own wifi / multi-track progression semantics
- own final slide judgment semantics
- own parent/child timing propagation for connected bodies

Non-responsibilities:

- synthetic head click generation
- implicit head-note existence
- tap-family queue advancement
- tap-style head judgment


## Supported Cases After Split

This split should make the following combinations first-class rather than ad hoc:

- normal slide: head + body
- no-head singleton slide: body only
- connected child slide: body only
- head-only artifact: head only

The last case is not a current priority feature, but this architecture makes it naturally representable instead of impossible or fake.


## Lowering Boundary Rule

This proposal depends on one strict rule:

- composite slide meaning is allowed in syntax and normalization
- composite slide widgets are not allowed after lowering

In other words:

- parser and normalization may still talk about one logical slide aggregate
- lowering must split that aggregate into independent head/body widgets
- runtime state, prover input generation, and FFI export must consume those split widgets directly

This is not just a presentation choice. It is the semantic cut that prevents the old ambiguity from leaking into runtime again.


## Proposed Data Model

Exact names may change, but the semantic split should look like this.

### Parser / normalized layer

Keep parser-facing compatibility with `isSlideNoHead`, but normalize immediately into:

- `hasHeadNote : Bool`
- `hasBody : Bool`
- `visualHeadSuppressed : Bool` only if a later runtime/render layer genuinely needs that separate concept

For current real slides:

- ordinary slide: `hasHeadNote = true`, `hasBody = true`
- singleton `!/?` slide: `hasHeadNote = false`, `hasBody = true`
- conn child: `hasHeadNote = false`, `hasBody = true`

Important normalization rule:

- parser-originated semantic headlessness must not be conflated with any mod/render flag that hides slide-head visuals

At this layer it is still acceptable to keep a temporary logical aggregate that describes one original slide literal, as long as lowering consumes it and does not pass that aggregate shape onward.

### Lowered chart layer

Introduce explicit lowered structures:

- `SlideHeadChartNote`
- `SlideBodyChartNote`

and store them separately in `ChartSpec`, or in a single sum-type list if that integrates better with existing schedulers.

Do not introduce a lowered `SlideCompositeChartNote` as the primary runtime-facing object. If a temporary aggregate helper is needed during lowering, it should disappear before `ChartSpec` is finalized.

Recommended shape:

- head note keeps its own `noteIndex`
- body note keeps its own `noteIndex`
- both also carry a shared `logicalSlideId`

If separate `noteIndex` values are too invasive for current score/event code, keep one logical slide id and add an optional component id. The main requirement is that runtime and FFI can tell the two objects apart.

Current in-repo transitional state:

- lowered `ChartSpec` already has explicit `slideHeads`
- lowered head/body pairs now carry explicit `logicalSlideId`
- lowered slide heads and slide bodies now use distinct `noteIndex` values while sharing `logicalSlideId`
- runtime now has a dedicated `SlideHeadNote`
- `SlideHeadNote` still lives in the shared tap-family queue via tagged `TapFamilyNote` entries, preserving tap-family competition semantics while exposing runtime object kind explicitly
- slide bodies still use the existing `SlideNote` runtime type; a future dedicated `SlideBodyNote` rename remains optional cleanup, not an urgent semantic blocker

The shared logical id is metadata only. It should not imply that lowered head and lowered body are owned by one runtime wrapper node.

### Runtime layer

Add distinct runtime note types:

- `SlideHeadNote`
- `SlideBodyNote`

Do not keep head semantics hidden inside `Lifecycle.SlideNote`.
Do not replace that hidden coupling with a new runtime composite note type either.

`SlideHeadNote` should reuse tap-family judgment semantics.

This is not just an implementation convenience. It is the reference-aligned semantic target.

`SlideHeadNote` should therefore:

- judge at the slide head timing
- share the tap/button queue frontier rules
- consume button or sensor click input with ordinary tap-style competition policy
- contribute head-note counting and grade reporting independently from the slide body

`SlideBodyNote` should own:

- path queues
- conn parent linkage
- parent finished / pending-finish flags
- wifi / multi-track state
- final slide judgment

`SlideBodyNote` should also own body-side timing semantics such as:

- `startTiming`
- connected-body parent timing propagation
- body checkability progression from the preserved head-timing anchor

Reference inspection in `MajdataPlay` now shows that body checkability is driven by the preserved slide-head timing anchor (`Timing`) together with connected-parent progression for child parts, not by body `startTiming` and not by a separate successful head judgment. The Lean runtime should preserve that structure.

Runtime should therefore operate on:

- head widgets
- body widgets

and not on a single widget that conditionally contains both.

### Prover layer

The timing skeleton and tactic generator should derive input from actual lowered objects:

- emit a head click only when a `SlideHeadNote` exists
- emit path sensor holds only when a `SlideBodyNote` exists

This removes the current need to infer head-click existence from slide-body metadata.

### FFI layer

Export head and body explicitly.

At minimum the exported IR should distinguish:

- object kind: `slide_head` vs `slide_body`
- shared logical slide id
- body conn metadata
- body path queues


## Separation of Syntax No-Head and Visual No-Head

This refactor should also make one semantic split explicit:

- chart semantic no-head
- runtime/mod visual no-head

Chart semantic no-head:

- belongs to parser / normalize / lowering
- decides whether a head note object exists

Visual no-head:

- belongs to playback/runtime configuration
- decides whether star-head visuals are suppressed
- must not decide whether the head note logically exists

These two concerns should not share a single field after normalization.


## Why Lower Before Runtime

Lowering the split before runtime is the key design decision.

That is preferable to keeping a single runtime slide object because:

1. runtime transitions become simpler
2. prover/replay generation becomes structural rather than heuristic
3. FFI becomes honest about what exists
4. edge cases become representable without fake empty queues or magic booleans

It is also preferable to keeping a lowered composite head+body widget, because that would still force runtime and prover code to branch on component presence and would reintroduce the same ambiguity in a different wrapper type.

In particular, avoid representing head-only slides as a slide body with empty `judgeQueues`. If there is no body, there should be no body object.
Likewise, avoid representing body-only slides as a composite widget with `head := none` if runtime can instead hold a plain body widget directly.


## Migration Strategy

Implement in phases to avoid mixing semantic changes with large file churn.

### Phase A: Design-safe type introduction

Goals:

- add new data structures
- preserve old behavior temporarily

Actions:

- add normalized fields for `hasHeadNote` and `hasBody`
- if needed, add a separately named visual suppression field rather than reusing semantic no-head naming
- add lowered `SlideHeadChartNote` and `SlideBodyChartNote`
- keep existing slide lowering path alive in parallel if needed during migration

Acceptance criteria:

- new structures compile
- existing tests still pass
- DSL inspection can print the new split information

### Phase B: Lowering split

Goals:

- create explicit head/body objects from normalized slides

Actions:

- ordinary slide lowers to both head and body
- singleton `!/?` lowers to body only
- conn child lowers to body only
- preserve current conn metadata and path queues on body notes
- ensure any temporary logical aggregate disappears before finalized lowered chart output

Acceptance criteria:

- lowered IR tests show the correct object pairs
- fallback demo chart still shows exactly one head object for `3qq7qq5[...]`
- no finalized lowered IR node is a persistent head+body composite wrapper

### Phase C: Runtime introduction

Goals:

- teach runtime to judge head and body independently

Actions:

- add `SlideHeadNote` to runtime state
- load `SlideHeadNote` into the tap-family scheduler path
- add head-note stepping using tap-family judgment and queue semantics
- narrow `SlideBodyNote` to path semantics only
- remove any remaining body-side runtime meaning of semantic no-head flags except clearly named visual/debug state if still required

Acceptance criteria:

- existing conn-slide body behavior remains unchanged
- singleton no-head slide is body-only with no synthetic head assumption
- ordinary slide head competes with taps and hold heads as a tap-family note

### Phase D: Prover rewrite

Goals:

- remove inferred head-click generation

Actions:

- build proof timing skeleton from actual lowered head/body objects
- headless slides emit no head click because no head object exists
- ordinary slides emit a real head-note action rather than inferring one from body flags

Acceptance criteria:

- generated tactic for connected child still omits bogus child head click
- singleton no-head slide also omits head click

### Phase E: FFI and DSL alignment

Goals:

- expose the split clearly outside Lean

Actions:

- update exported IR schema
- update Rust-side IR and FFI bindings
- update DSL helper printers to show head/body split explicitly

Acceptance criteria:

- FFI consumers can distinguish head from body without reverse-engineering flags
- local DSL `#eval` output stays useful for debugging
- DSL/FFI output does not present a post-lowered composite widget as the primary object model


## Risks

### Risk 1: Note indexing churn

If head and body become separate objects, event indexing and score bookkeeping may drift unless the logical slide identity is introduced carefully.

Mitigation:

- introduce shared logical slide id early
- decide explicitly whether score/event aggregation is by logical slide or by runtime object
- treat the current shared head/body `noteIndex` as transitional if it blocks correct independent runtime reporting

### Risk 2: Overfitting to Unity object layout

This refactor should follow MajdataPlay semantics, not Unity implementation details.

Mitigation:

- keep the split only where it clarifies gameplay semantics
- do not import renderer-lifecycle details into Lean runtime state

### Risk 3: Too-large patch set

This cut touches parser, runtime, prover, and FFI at once.

Mitigation:

- phase the migration
- land inspection/data-model changes before behavioral rewrites
- keep narrow regression tests around each parity finding


## Test Plan

The following tests should be added before or during migration.

### Parser / normalize tests

- singleton no-head slide lowers to body only
- connected child lowers to body only
- ordinary slide lowers to head + body

### Runtime tests

- singleton no-head body judges without any head object
- ordinary slide head and body coexist without double-consuming wrong input
- connected child remains gated by parent body state
- ordinary slide head follows tap-family queue competition against taps and hold heads on the same lane
- body checkability remains driven by explicit head timing / conn-parent progression rather than a synthetic inferred head state

### Prover tests

- ordinary slide tactic contains head click + body path
- singleton no-head tactic contains body path only
- connected child tactic contains body path only

### FFI / DSL tests

- IR output shows split objects and shared logical slide id
- full slide path is still visible on body IR


## Immediate Recommendation

Do this refactor, but do it as the next focused semantic-parity project, not as an opportunistic side edit.

Reference-aligned design stance:

- slide head in runtime is a tap-family judged note with star presentation
- slide body in runtime is a pure path/judgment object
- chart headlessness and visual head suppression must remain separate concepts

The required order is:

1. document the split
2. lock expected behavior with tests
3. add new head/body structures
4. migrate lowering
5. migrate runtime
6. migrate prover
7. migrate FFI and DSL

That sequence keeps the work auditable and avoids burying a semantic redesign inside unrelated parser fixes.

Final design stance:

- before lowering: a logical slide aggregate is acceptable
- after lowering: head and body are entirely different widgets
- shared identity remains only for bookkeeping, reporting, and aggregation
