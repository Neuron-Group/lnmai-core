# Typed Domain Refactor Plan

The project is committed to a structural refactor: sensor areas and button zones
should be represented by typed inductive values throughout the Lean core, rather
than by raw numeric identities.

This is not cosmetic cleanup. It is an architectural correction: gameplay
identity should be expressed by the type system, while numeric indexing should
be treated as a private storage concern.

In practical terms, the plan is to make typed identities the language of the
engine and push numeric encodings to the implementation edge.

## Current Status

The refactor has started and several early stages are already partially or
substantially complete.

- **Stage 1 largely completed**
  - shared typed gameplay domains now live in `LnmaiCore/Areas.lean`
  - `SensorArea` and `ButtonZone` exist with symbolic/index conversions, inverse
    theorems, JSON/string support, and rotation helpers
- **Stage 2 largely completed**
  - Simai tokenization, syntax, and normalized semantic IR now use typed
    `SensorArea` / `ButtonZone` identities instead of raw `Nat`
  - touch-holds now have a dedicated typed semantic form rather than being
    squeezed through button-lane fields
  - slide semantic queue specs now carry typed `SensorArea` targets instead of
    legacy numeric target indices
  - DSL / infoview output for normalized slides now presents symbolic sensor
    targets directly rather than numeric queue coordinates
  - parser-facing `SlideNoteSemantics` now exposes typed start/end identities
    (`ButtonZone` / `SensorArea`) instead of raw numeric positions
- **Stage 4 substantially completed at the API layer**
  - `ChartLoader.ChartSpec` note types now use typed `ButtonZone` and
    `SensorArea`
  - `ChartLoader` touch/touch-hold grouping logic is symbolic
- **Stage 9 started and partially completed**
  - gameplay identity JSON for `SensorArea` / `ButtonZone` now emits symbolic
    names as the canonical form
  - gameplay identity JSON input is now symbolic-only at the boundary; legacy
    numeric forms are no longer accepted
  - runtime `RuntimePos` JSON now uses explicit typed symbolic tagging rather
    than implicit numeric identities
  - runtime `JudgeEvent` serialization now carries typed runtime positions
- **Adjacency correction completed**
  - touch grouping / touch-hold grouping now use a symbolic adjacency table
    aligned with the `MajdataPlay` reference implementation

The remaining major work is concentrated in the runtime engine internals:

- runtime queue/storage wrappers still rely on numeric indexing internally in
  several places
- some scheduler and slide-runtime logic still carries bookkeeping-oriented
  numeric identifiers or local track/storage coordinates internally, even
  though gameplay-facing identity flow is now largely typed
- FFI/serialization/proof-facing migration remains unfinished

That means the semantic-facing layers are already mostly typed, but the runtime
core is still in a partly symbolic, partly indexed transition state.

## Direction

- Slide semantics should use exact canonical areas, not opaque numeric
  encodings.
- Touch sensors should be modeled by a typed `SensorArea` domain.
- Outer buttons should be modeled by a typed `ButtonZone` domain.
- Numeric indices should survive only as hidden storage details or explicit FFI
  encodings.

Additional runtime target:

- `ChartLoader`, lifecycle, scheduler, and input APIs should eventually stop
  exposing numeric positions entirely; if indexing remains at all, it should be
  buried in narrow storage-only helpers.

## Design Principles

- **Typed identity first** — areas and buttons are gameplay entities, not
  indices.
- **Semantic layers stay symbolic** — parser, normalization, chart IR, and
  proof-facing APIs should not rely on raw `Nat` identities.
- **Indexed storage stays private** — arrays, lists, and packed buffers may
  still use numeric indexing internally.
- **Lower late** — convert to indices only at storage or ABI boundaries.
- **Prove the boundary** — every symbolic/indexed conversion should come with
  inverse theorems.
- **One source of truth** — exact slide topology should come from symbolic
  tables, not reconstructed numeric surrogates.

## Domain Modeling Note: Abstract Outer Slot

Outer-lane note identity should ultimately be modeled as an abstract 8-way slot
rather than as a button-specific domain.

Why this is the right semantic model:

- outer-lane gameplay identity is “which ring position”
- runtime behavior aligned with `MajdataPlay` may resolve some lane notes
  through either the physical outer button or the matching outer `A`-ring sensor
- therefore the note identity should not hard-code a single physical input
  surface into the semantic type

Recommended target design:

- introduce a dedicated abstract lane domain, for example:
  - `OuterSlot`
  - `RingSlot`
  - `LaneSlot`
- define it as an explicit 8-element inductive type
- make it the semantic identity used by:
  - taps
  - holds
  - slide heads / slide start lanes
  - parser-facing lane semantics
  - normalized chart IR
  - chart-loading boundary types

Then define explicit projections from the abstract slot to concrete surfaces:

- abstract slot → outer button zone
- abstract slot → matching outer `A`-ring sensor area

That gives the engine a cleaner structure:

- **semantic layer** — note identity is the abstract slot
- **input layer** — button and sensor are concrete realizations of that slot
- **runtime judgment layer** — rules decide which realizations are accepted for
  each note type or mode

Recommended migration strategy:

- treat the current `ButtonZone` usage as a transitional approximation of the
  future abstract slot domain
- avoid documenting lane identity as inherently button-only
- prefer names such as “lane”, “outer slot”, or “ring position” in semantic APIs
  and documentation whenever practical
- once the typed-domain refactor is stable, consider a follow-up migration from
  `ButtonZone` to the abstract slot type with thin conversion helpers retained
  at the input/storage boundary

This is not required to justify the current typed-domain migration, but it is
the more faithful long-term semantic design.

## Motivation

- Earlier Lean slide tables used lossy numeric placeholders that could not
  faithfully represent exact sensor areas.
- The authoritative Majdata slide tables use exact named areas like `A1`, `B8`,
  and `C`.
- Typed domains prevent accidental conflation of semantically distinct areas and
  make parser/runtime/proof code align with external references.
- The runtime, FFI, and proof layers all become easier to audit when identities
  are explicit in the types.

## Scope of the Refactor

This plan applies across the full Lean stack:

- Simai parser and lowering pipeline
- normalized semantic IR
- runtime chart loading
- input state and note lifecycle execution
- proof-facing queue and replay APIs
- serialization and Rust FFI boundaries

The desired end state is that no gameplay-facing API within `lnmai-core` uses
raw `Nat` to mean “which area” or “which button”.

## Non-goals

The refactor does not aim to:

- redesign gameplay rules or judgment windows
- change slide semantics away from existing authoritative references
- force a single giant rewrite that leaves the repo broken for long periods

The goal is to stop using indices as the semantic identity of gameplay objects.

## Refactor Outline

The work falls into five major tracks:

1. **Type foundation**
   - introduce `SensorArea` and `ButtonZone`
   - define symbolic/indexed conversions and prove them correct

2. **Semantic/compiler pipeline**
   - refactor Simai parsing, normalization, and slide topology around typed
     identities

3. **Runtime engine**
   - move chart loading, input state, lifecycle, and judgment APIs onto typed
     identities

4. **Boundary layers**
   - update DSL display, JSON, and Rust FFI to expose typed identities
     explicitly

5. **Proof migration**
   - rewrite proof-facing APIs over typed identities and isolate index-level
     lemmas

## Detailed Roadmap

### Stage 1 — Core typed domains

Primary goal: establish the two finite gameplay identity domains used by the
engine.

- Add a dedicated module for gameplay identities, likely `LnmaiCore/Areas.lean`
  or `LnmaiCore/InputDomain.lean`.
- Define:
  - `SensorArea := A1 .. A8 | B1 .. B8 | C | D1 .. D8 | E1 .. E8`
  - `ButtonZone := K1 .. K8` or equivalent explicit outer-button names
- Provide:
  - `SensorArea.toIndex : SensorArea → Nat`
  - `SensorArea.ofIndex? : Nat → Option SensorArea`
  - `ButtonZone.toIndex : ButtonZone → Nat`
  - `ButtonZone.ofIndex? : Nat → Option ButtonZone`
- Prove:
  - left inverse and right inverse theorems
  - coverage/cardinality lemmas for all 33 sensors and 8 buttons
  - rotation helpers for the 8-fold rings used by slide placement
- Add derived support:
  - `DecidableEq`, `BEq`, `Ord` if needed
  - `ToJson`, `FromJson`, `Repr`, `ToString`

Additional recommendation:

- Introduce small typed helpers early:
  - `SensorArea.rotate : Nat → SensorArea → SensorArea`
  - `ButtonZone.rotate : Nat → ButtonZone → ButtonZone`
  - ring membership and classification helpers if later runtime logic needs them

This stage establishes the canonical vocabulary that every later module depends
on.

Status:

- implemented in large part
- one follow-up validation task remains: confirm the authoritative numeric
  storage ordering behind `SensorArea.toIndex` / `SensorArea.ofIndex?` against
  the reference runtime so the storage boundary itself is known-correct

### Stage 2 — Simai semantic/compiler pipeline

Primary goal: make the whole parser/normalizer pipeline produce typed
identities, not raw `Nat`s.

Files likely involved:

- `LnmaiCore/Simai/Tokenize.lean`
- `LnmaiCore/Simai/Syntax.lean`
- `LnmaiCore/Simai/IR.lean`
- `LnmaiCore/Simai/SlideTables.lean`
- `LnmaiCore/Simai/Normalize.lean`
- `LnmaiCore/Simai/Frontend.lean`

Changes:

- Replace touch parsing helpers with typed results:
  - old: `touchAreaToSensorPos? : String → Option Nat`
  - new: `touchAreaToSensorArea? : String → Option SensorArea`
- Replace lane parsing results with `ButtonZone` where appropriate.
- Refactor `NormalizedTap`, `NormalizedHold`, `NormalizedTouch`,
  `NormalizedSlide` fields:
  - `lane : ButtonZone`
  - `sensorPos : SensorArea`
  - slide queues carry `List SensorArea`, not `List Nat`
- Keep slide rotation and canonical table semantics entirely symbolic.
- Ensure parser-facing semantics match exact external references like Majdata
  slide tables.

Preferred rule:

- the Simai compiler pipeline should not “borrow” runtime storage encodings to
  express semantic meaning

Expected outcome:

- the Simai frontend produces a typed semantic IR
- no semantic slide or touch identity is represented by a raw integer

Status:

- implemented in large part
- typed identities now flow through tokenization, syntax, normalization, and
  semantic IR
- slide-table semantic specs now use typed `SensorArea` target lists as their
  canonical field, and no longer keep a parallel numeric semantic field
- parser-facing slide semantics now use typed `ButtonZone` / `SensorArea`
  identities rather than raw start/end position numbers
- `simai_normalized_slide!` / related DSL views now display symbolic slide
  target areas directly
- the remaining numeric residue in the semantic/compiler path is now narrow and
  intentional: relative slide-shape encoding such as `SlideShape.relEnd` remains
  numeric because it represents relative ring distance rather than a gameplay
  identity domain

### Stage 3 — DSL, infoview, and compiler-facing display

Primary goal: make elaboration-time values readable and typed by construction.

Files likely involved:

- `LnmaiCore/Simai/DSL.lean`
- `LnmaiCore/Simai/Frontend.lean`
- typed-domain modules providing `Repr`/`ToString`

Changes:

- Remove dependence on debug-only numeric area printing for semantic-facing
  output.
- Print exact symbolic values directly in DSL results.
- Preserve optional numeric display only where debugging indexed storage is
  useful.
- Ensure examples like `simai_normalized_slide!` show exact canonical/runtime
  sensor identities clearly.

Polish target:

- a DSL value shown in infoview should be readable as a gameplay object without
  mentally decoding indices

Expected outcome:

- infoview output becomes a trustworthy presentation of the real semantic model

### Stage 4 — ChartLoader boundary

Primary goal: move the normalized-to-runtime lowering boundary onto typed
domains.

Files likely involved:

- `LnmaiCore/ChartLoader.lean`

Changes:

- Refactor chart note structures:
  - `TapChartNote.lane : ButtonZone`
  - `HoldChartNote.lane : ButtonZone`
  - `TouchChartNote.sensorPos : SensorArea`
  - `SlideChartNote.judgeQueues : List (List SlideAreaSpec)` with typed
    `SensorArea`
- Refactor slide/touch construction helpers to consume typed values.
- If runtime structures still require indices internally, perform lowering
  inside small local helpers only.

Important boundary rule:

- `ChartLoader` should be the last major module that knows both the symbolic and
  indexed views at the same time

Expected outcome:

- `ChartLoader` no longer exposes raw gameplay identities as `Nat`

Status:

- mostly implemented for `ChartSpec`, runtime note construction, and grouping
  logic
- `ChartLoader` public/runtime-facing chart note structures now use typed
  `ButtonZone` / `SensorArea`
- runtime note builders now construct typed shared runtime positions rather than
  raw numeric `startPos`
- local lowering to numeric queue/storage indices still exists when building
  queue arrays and active-note state

### Stage 5 — Runtime input/state model

Primary goal: keep runtime APIs typed even if storage stays indexed.

Files likely involved:

- `LnmaiCore/InputModel.lean`
- `LnmaiCore/Constants.lean`

Changes:

- Replace raw-index getters with typed accessors:
  - `getSensorHeld : FrameInput → SensorArea → Bool`
  - `getSensorClicked : FrameInput → SensorArea → Bool`
  - `getButtonHeld : FrameInput → ButtonZone → Bool`
  - `getButtonClicked : FrameInput → ButtonZone → Bool`
- Introduce typed update/set helpers for per-frame state.
- Consider adding typed wrappers around storage collections:
  - `SensorVec α`
  - `ButtonVec α`
- Keep `SENSOR_AREA_COUNT` and `BUTTON_ZONE_COUNT` only for backing storage
  sizing.

Recommended implementation detail:

- use thin wrappers over arrays/lists so runtime code stays efficient while APIs
  become type-safe

Expected outcome:

- runtime code no longer manually indexes gameplay states with arbitrary `Nat`

Status:

- substantially advanced, still not fully complete
- typed `FrameInput` accessors have been introduced and `FrameInput` storage now
  uses dedicated typed wrappers rather than bare per-domain `List`s
- active runtime hold collections now use typed `ButtonZone` / `SensorArea`
  identities instead of raw `Nat` keys
- a dedicated shared storage wrapper layer now exists in
  `LnmaiCore/Storage.lean`, providing thin `ButtonVec` / `SensorVec` wrappers
  for runtime button/sensor state
- persistent runtime state such as `GameState.prevButton` /
  `GameState.prevSensor` and scheduler click-cursor usage now go through typed
  storage wrappers instead of raw lists
- timed input events no longer expose raw numeric button/sensor identities; they
  now carry typed `ButtonZone` / `SensorArea` values and lower to storage only
  inside `TimedInputBatch.toFrameInput`
- storage sizing and queue backing structures still remain index-based
  internally where appropriate

### Stage 6 — Lifecycle and judgment engine

Primary goal: make runtime note execution logic typed as well.

Files likely involved:

- `LnmaiCore/Lifecycle.lean`
- `LnmaiCore/Judge.lean`
- `LnmaiCore/Scheduler.lean`
- `LnmaiCore/Convert.lean`

Changes:

- Replace `SlideArea.targetAreas : List Nat` with `List SensorArea` or a typed
  runtime equivalent.
- Refactor note params so slide/touch/button notes use the correct identity
  type.
- Replace direct list indexing on `sensorHeld[target]?` with typed getters.
- Ensure event emission and judgment records carry typed areas/buttons.

Guiding constraint:

- runtime stepping code should not need to know how a `SensorArea` is physically
  indexed in memory

Expected outcome:

- the actual gameplay engine speaks the same typed identity language as the
  parser

Status:

- substantially advanced
- `Lifecycle.NoteParams.startPos` has been migrated from raw `Nat` to a shared
  typed runtime position sum
- `JudgeEvent` now reports a typed runtime position instead of a numeric
  `sensorPos`
- slide target areas already use typed `SensorArea`
- scheduler/lifecycle logic now lowers to numeric indexing only at local
  queue/input-storage boundaries
- scheduler tap/touch queue traversal now iterates over typed `ButtonZone` /
  `SensorArea` domains rather than treating numeric indices as the semantic
  identity during traversal
- slide-runtime sensor-held plumbing has been migrated from raw `List Bool` to
  typed `SensorVec Bool`, so slide queue progression now reads typed storage at
  the API boundary
- regular button taps and holds have been re-aligned with the `MajdataPlay`
  reference behavior: they may resolve through either the button lane or the
  matching `A`-ring sensor
- slide judgment timing has been re-aligned with the reference so judged slide
  diffs use touch-panel-offset-adjusted time and preserve that diff through the
  delayed end event
- one remaining scheduler-side raw-index helper has been removed, shrinking the
  set of runtime entry points that still accept numeric gameplay identity
- the public runtime/storage surface has been tightened further: index-oriented
  storage accessors are now private to `LnmaiCore.Storage`, and the previously
  exposed `RuntimePos → Nat` storage conversion helper has been removed from the
  shared typed domain API
- scheduler slide parent-link and queue-track plumbing have been tightened so
  local runtime indexing now sits behind small helper functions rather than
  being rebuilt ad hoc inside the main stepping logic
- the remaining work is mostly cleanup and encapsulation:
  - continuing to shrink or eliminate the remaining low-level list/index helper
    internals that still back typed storage wrappers
  - deciding whether to split some shared runtime note structures further
  - continuing parity validation for rare multi-frame conn/wifi edge cases

Recent validation work:

- a focused co-scan against `reference/MajdataPlay` was performed over tap,
  hold, touch, touch-hold, queue gating, and slide runtime timing paths
- a dedicated runtime regression module now covers key subtle cases, including:
  - button tap resolved by matching `A`-sensor fallback
  - classic hold body sustained by matching `A` sensor
  - connected-slide child progress forcing parent queue finish
  - slide judgment storing touch-panel-offset-adjusted timing diff
- the runtime regression harness itself now constructs frame input and previous
  sensor state by iterating typed `ButtonZone.all` / `SensorArea.all` domains
  rather than by rebuilding gameplay identity from numeric storage indices

### Stage 7 — Adjacency, touch grouping, and geometry-derived helpers

Primary goal: eliminate hidden numeric topology from helper logic.

Files likely involved:

- `LnmaiCore/ChartLoader.lean`
- `LnmaiCore/InputModel.lean`
- possibly new helper modules for adjacency

Changes:

- Rewrite neighbor/group logic using typed adjacency:
  - old: `touchHoldNeighbors : Nat → List Nat`
  - new: `touchHoldNeighbors : SensorArea → List SensorArea`
- Use exact symbolic domains in touch-group and touch-hold grouping logic.
- Keep conversion to indices private to storage helpers only.

This stage is especially important because hidden topology tables tend to
preserve accidental numeric assumptions longest.

Expected outcome:

- topological relationships become explicit and reviewable

Status:

- completed
- touch/touch-hold adjacency and grouping have already been rewritten to use
  symbolic `SensorArea`
- the adjacency table now matches the `MajdataPlay` reference `TOUCH_GROUPS`
  table exactly
- the remaining `groupId` / group-size fields used by grouped touch and
  touch-hold runtime logic are intentional bookkeeping keys for connected
  components, not gameplay identity encodings

## New Runtime Completion Target

The original roadmap already aimed to make semantic APIs typed and leave indices
as private storage details. Experience during implementation has clarified that
this should be pushed one step further:

- `ChartLoader` should become entirely symbolic in both API and internal logic
- lifecycle note params should stop using a shared raw numeric `startPos`
- judgment events should stop reporting raw numeric sensor/button identities
- any surviving numeric encoding should be isolated behind very small storage
  wrappers or accessor helpers

In other words, the long-term goal is not merely “typed at the parser layer” or
“typed until ChartLoader”. The long-term goal is:

- typed identities all the way through semantic and gameplay logic
- numeric indices only inside storage plumbing

This stronger target should guide remaining runtime refactor work.

### Stage 8 — FFI and Rust boundary

Primary goal: export stable typed gameplay identities across the host boundary.

Files likely involved:

- Lean-side export modules and any Rust-facing chart serialization layer

Changes:

- Define a stable external representation for `SensorArea` and `ButtonZone`.
- Preferred representations:
  - symbolic names like `"A1"`, `"B7"`, `"C"`
  - or a stable enum layout documented for Rust
- Avoid exposing storage indices as the semantic FFI contract.
- Ensure rendered slides and runtime events use the same typed area schema.

Recommended policy:

- treat the FFI format as a documented public contract, not as an implementation
  leak from Lean internals

Expected outcome:

- Rust consumes a stable typed gameplay model instead of ambiguous integers

Status:

- not yet completed
- Lean-side boundary types are now better prepared for this migration because
  runtime positions and judgment events already have typed symbolic JSON
  encodings
- Rust-facing schema documentation and any direct host-bound export layer still
  remain to be defined explicitly

### Stage 9 — JSON and compatibility layer

Primary goal: preserve practical interoperability while changing the core model.

Changes:

- Update `ToJson` / `FromJson` instances to emit symbolic forms.
- Accept symbolic forms only at the external JSON boundary.
- Remove temporary numeric-compatibility shims once typed codecs are in place.

Compatibility should be one-way where possible:

- require typed/symbolic data on input
- emit typed/symbolic data on output

Expected outcome:

- external data remains usable while the semantic model becomes safer

Status:

- started and partially completed
- `SensorArea` / `ButtonZone` JSON output is now symbolic by default
- `SensorArea` / `ButtonZone` JSON input is now symbolic-only
- runtime `RuntimePos` now serializes as an explicit typed symbolic object
  and parses the same symbolic object form at the boundary
- `JudgeEvent` serialization now flows through typed runtime positions instead
  of unstructured numeric area/button fields
- focused runtime regression coverage now checks that symbolic JSON is accepted
  and legacy numeric forms are rejected at the boundary
- remaining work includes documenting the symbolic host-facing contract for
  external consumers and aligning any out-of-tree host code with it

### Stage 10 — Proof migration

Primary goal: make proofs reason over the same typed identities as the
implementation.

Files likely involved:

- everything under `LnmaiCore/Proofs/`

Changes:

- Rewrite queue replay helpers over `SensorArea` and `ButtonZone`.
- Add foundational lemmas for:
  - typed accessors and updates
  - typed rotation of 8-fold rings
  - lowering from exact symbolic domains to hidden indices
- Reserve numeric proofs for storage correctness only.

Proof policy:

- high-level correctness theorems should talk about typed gameplay identities
- low-level index lemmas should remain isolated and reusable

Expected outcome:

- the proof-facing API reflects the final design, not a transitional encoding

## Runtime Storage Policy

The refactor does not require abandoning compact indexed storage.

- Allowed internally:
  - `Array Bool`, `List Bool`, numeric counters, packed queues
- Not allowed semantically:
  - public/raw `Nat` as the identity of a sensor or button

The intended pattern is:

- semantic APIs use `SensorArea` / `ButtonZone`
- private helpers convert those to numeric indices for storage
- proofs show that the conversions are total and lossless

This allows the core to stay efficient without sacrificing semantic clarity.

## Risks and Constraints

The main risks are architectural churn rather than conceptual uncertainty.

- **Proof churn** — typed-domain migration will ripple through parser-derived
  queue proofs and replay lemmas.
- **Runtime churn** — input-state and lifecycle modules currently assume direct
  list indexing in many places.
- **Boundary churn** — JSON and FFI contracts may require coordinated host-side
  migration.
- **Naming churn** — button identities and `B`-ring sensor identities must
  remain clearly distinguished.

These risks are acceptable, but they reinforce the execution rule that each
stage should land in a buildable state.

## Execution Order

For implementation safety, the preferred order is:

1. typed domains and proofs
2. Simai pipeline and slide tables
3. DSL / frontend display
4. `ChartLoader`
5. runtime input/state wrappers
6. lifecycle/judgment internals
7. adjacency/group helpers
8. FFI / serialization
9. proof migration and cleanup

Execution rule:

- each stage should leave the repo building, even if temporary bridge helpers
  still exist
- any temporary bridge helpers should be introduced centrally and deleted
  aggressively once downstream modules migrate

## Definition of Done

This refactor is complete when all of the following hold:

- no semantic-facing API uses `Nat` for sensor/button identity
- slide tables and normalized/runtime note identities are typed
- runtime APIs consume typed identities even if storage remains indexed
- Rust/FFI sees stable typed sensor/button identities
- proofs reason over typed domains instead of ad hoc numeric encodings

Additional completion criteria:

- no new gameplay module introduces raw sensor/button `Nat` identities
- any remaining index-level code is clearly marked as storage-only
  infrastructure

## Transition State

- `LnmaiCore.Simai.SlideTables` already uses exact symbolic sensor areas for
  canonical slide topology.
- parser-facing slide semantics and normalized slide queue specs now also carry
  typed symbolic identities rather than raw numeric area positions.
- runtime note params, event positions, and slide target areas now all use typed
  symbolic identities; remaining numeric structure is concentrated in
  storage-only wrappers and other narrow internal plumbing.
- within the semantic/compiler path, remaining numeric structure is now mostly
  limited to relative shape encoding rather than raw gameplay identity.
- The long-term goal is to move the rest of the core onto typed domains as well.

The slide-table layer has already validated the direction of the refactor. The
remaining work is to propagate the same typed-domain design through the rest of
the engine.

## Final Principle

The semantic language of the engine should be typed and explicit. Numeric
encodings may remain internally for compact storage or ABI purposes, but they
should no longer define the meaning of gameplay areas.

If an identity appears in parser output, normalized IR, runtime note logic,
proof APIs, or FFI payloads, it should be represented by a typed gameplay domain
rather than by an unstructured integer.
