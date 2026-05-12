# LnMai Core Architecture Proposal

This repository should move toward a stricter separation of concerns:

- Lean is the single gameplay authority.
- Rust is the host shell for file IO, device input, rendering, audio, timing, and FFI.
- Simai parsing and chart-to-judgment-topology resolution should live in Lean, not in Rust.

The purpose of this proposal is to replace the previous handoff note with a concrete implementation direction for the repo.

## Core Position

The current slide parity work shows that judgment correctness depends on chart semantics, not just on frame stepping. In MajdataPlay, slide behavior is not determined merely by a generic `SlideKind`; it is determined by:

- Simai text parsing
- slide-shape classification
- shape-to-judge-table mapping
- connected-slide grouping
- Wifi special-case topology
- per-segment skip flags and area membership

If those semantics live outside Lean, the Lean core can only judge whatever topology the host happens to hand it. That weakens the claim that Lean is the gameplay authority.

Therefore the repository should evolve so that Lean owns:

- parsing-relevant chart semantics
- normalized chart IR construction
- judgment topology construction
- final judgment state transitions

Rust should provide:

- file loading
- input collection
- render/audio execution
- app integration
- FFI marshaling

## Proposed Architecture

Introduce a four-layer Lean pipeline:

1. `Simai` syntax layer
2. `Simai` front-end layer
3. `Chart IR` normalization layer
4. `Runtime note` construction layer

The flow should be:

`Simai text or Lean DSL -> Lean parser/front-end -> normalized chart IR -> runtime notes/queues -> scheduler/judgment`

Rust should call into Lean at the boundary between raw chart text and normalized chart IR, or between raw chart text and fully built `GameState` if the FFI shape remains manageable.

## Proposed Lean Modules

The current repo already has runtime modules such as `Types`, `Lifecycle`, `ChartLoader`, and `Scheduler`. The proposal is to add a parsing and topology stack ahead of the current loader responsibilities.

### 1. `LnmaiCore.Simai.Syntax`

Purpose:

- represent raw parsed Simai notes before semantic lowering
- preserve enough source information for diagnostics

Suggested contents:

- `SimaiChart`
- `SimaiTimingPoint`
- `SimaiNote`
- `SimaiSlideSpec`
- `SimaiSlideToken`
- note modifiers such as break/ex/each/no-head/no-track
- source-location metadata for parse errors

This layer should not know about `SlideAreaSpec` yet.

### 2. `LnmaiCore.Simai.Parser`

Purpose:

- parse Simai text into `LnmaiCore.Simai.Syntax`

Suggested outputs:

- `Except SimaiParseError SimaiChart`

Suggested error design:

- line/column information
- raw note snippet
- structured error kind

This should mirror the useful parts of MajdataPlay parsing behavior, but remain a pure Lean parser.

### 3. `LnmaiCore.Simai.DSL`

Purpose:

- provide a Lean-native authoring surface for Simai examples, proof witnesses, and spec tests
- elaborate Simai literals into the same core syntax/IR used by the normal parser

This module should be treated as a front-end over the parser, not as a separate semantic engine.

Recommended first form:

- string-based compile-time forms such as:
  - `simai_note! "1-3"`
  - `simai_note! "1V35"`
  - `simai_note! "1w5"`
  - `simai_chart! "..."`

Recommended design rules:

- the elaborator must call the same parsing/normalization core used by runtime APIs
- the DSL must not invent alternate semantics
- compile-time convenience should not replace the plain parser API

Why the DSL belongs in the architecture:

- it makes proofs readable
- it avoids hand-written `SlideAreaSpec` witnesses where the real concern is Simai semantics
- it gives the repository a specification language for parity work

Longer-term optional extensions:

- `LnmaiCore.Simai.Quoter`
- non-string notation once the grammar surface is stable

### 4. `LnmaiCore.Simai.SlideShape`

Purpose:

- classify slide text into semantic slide species used by the reference implementation

This is the key missing layer today.

Suggested explicit shape datatype:

- `Line relEnd`
- `Circle relEnd dir`
- `V relEnd`
- `BigV turn relEnd`
- `P relEnd`
- `Q relEnd`
- `PP relEnd`
- `QQ relEnd`
- `S`
- `Z`
- `Wifi`

or, if mirroring MajdataPlay more closely is easier:

- `line3` ... `line7`
- `circle1` ... `circle8`
- `v1`, `v2`, `v3`, `v4`, `v6`, `v7`, `v8`
- `pq1` ... `pq8`
- `ppqq1` ... `ppqq8`
- `L2` ... `L5`
- `s`
- `wifi`

The second form matches MajdataPlay tables more directly and may reduce translation ambiguity.

This module should also own:

- relative-end-position computation
- mirror handling
- rotation-friendly normalization rules
- validation of illegal shape forms

### 5. `LnmaiCore.Simai.SlideTables`

Purpose:

- encode the real judgment topology for slide species

This module should be a Lean version of the mapping currently seen in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/Utils/SlideTables.cs`.

It should define:

- ordinary slide tables
- Wifi tables
- modern/classic differences where needed
- per-segment `targetAreas`
- per-segment `isSkippable`
- per-segment `isLast`
- per-segment hide-bar progress metadata

This is where the current implementation gap is most visible.

The current Lean repo only has generic `judgeQueues` provided from chart JSON. It does not yet contain the authoritative topology constructor.

### 6. `LnmaiCore.Simai.Normalize`

Purpose:

- convert parsed Simai notes into normalized chart IR used by the runtime

Suggested normalized structures:

- `NormalizedChart`
- `NormalizedTap`
- `NormalizedHold`
- `NormalizedTouch`
- `NormalizedSlide`

For slides, normalized information should include:

- slide species / shape key
- start position
- end position
- turn position if applicable
- `SlideKind` (`Single`, `Wifi`, `ConnPart`)
- group-parent linkage for connected slides
- `trackCount`
- `judgeQueues`
- `totalJudgeQueueLen`
- `judgeAtSec`
- `isClassic`
- all derived flags used by runtime judgment

This layer should absorb most of what `ChartLoader` currently expects from external JSON.

### 7. `LnmaiCore.ChartLoader`

After the above work, `ChartLoader` should shrink in responsibility.

It should become a thin adapter from normalized chart IR into runtime `GameState` and note structures.

That means:

- keep construction helpers
- keep sorting and queue placement logic
- stop being the place where slide semantics are invented

## Proposed Rust Boundary

Rust should not construct slide topology by itself.

Preferred FFI contract:

- Rust passes raw chart text or pre-extracted Simai content into Lean
- Lean returns either parse/normalize errors or a normalized chart payload
- Rust then requests initial `GameState` construction from Lean, or passes the normalized chart back into Lean for state construction

Two viable designs:

### Option A: Rust sends chart text, Lean returns `GameState`

Pros:

- strongest authority boundary
- least semantic duplication

Cons:

- larger FFI surface for state serialization

### Option B: Rust sends chart text, Lean returns normalized chart IR, then Lean builds `GameState`

Pros:

- easier debugging and inspection
- IR can be persisted and tested independently

Cons:

- one extra representation to maintain

Preferred recommendation: **Option B**.

It gives a stable semantic checkpoint between parsing and runtime judgment.

The DSL fits into this boundary as a Lean-only authoring front-end. Rust should not depend on the elaborator; Rust should depend on the pure parser/normalizer API.

## Why This Fits the Current Repo

This design matches the repo’s existing direction:

- Lean already owns note stepping and judgment.
- Lean already owns slide queue traversal.
- Lean already carries explicit slide metadata such as `SlideKind`, parent links, `trackCount`, `judgeAtSec`, and queue state.
- The missing authority is the parser/topology constructor layer.

So this proposal extends the current design rather than replacing it.

## Concrete Implementation Plan

### Phase 1: Introduce explicit slide topology modules

Add modules for:

- `LnmaiCore.Simai.Syntax`
- `LnmaiCore.Simai.Parser`
- `LnmaiCore.Simai.DSL`
- `LnmaiCore.Simai.SlideShape`
- `LnmaiCore.Simai.SlideTables`
- `LnmaiCore.Simai.Normalize`

Goal:

- represent and construct reference-faithful slide judge queues inside Lean

Status:

- partially completed
- implemented modules now include:
  - `LnmaiCore.Simai.Syntax`
  - `LnmaiCore.Simai.Timing`
  - `LnmaiCore.Simai.Tokenize`
  - `LnmaiCore.Simai.Shape`
  - `LnmaiCore.Simai.SlideParser`
  - `LnmaiCore.Simai.SlideTables`
  - `LnmaiCore.Simai.IR`
  - `LnmaiCore.Simai.Source.Maidata`
  - `LnmaiCore.Simai.Frontend`
  - `LnmaiCore.Simai.Normalize`
- remaining work in this phase is primarily connected-slide source semantics and final topology-authority cleanup

### Phase 2: Add pure parser entry points

Add:

- `String -> Except Error SimaiChart`
- small parser-focused tests/examples

Goal:

- runtime and proof code share the same parsing core

Status:

- substantially completed for the current maidata/Simai source pipeline
- implemented entrypoints now include:
  - `LnmaiCore.Simai.parseFrontendMaidata`
  - `LnmaiCore.Simai.parseFrontendChartByLevel`
  - `LnmaiCore.Simai.frontendNormalizedChart`
  - `LnmaiCore.Simai.frontendLoweredChart`
- current Lean parity mirrors all 18 Python reference tests from `../reference/PySimaiParser/tests/test_core.py`

### Phase 3: Add Lean DSL elaborator

Add:

- `simai_note! "..."`
- `simai_chart! "..."`

Goal:

- proof/spec authoring becomes ergonomic without splitting semantics from the parser core

Status:

- not started

### Phase 4: Port MajdataPlay shape detection

Mirror the shape classification logic from:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs`

Especially:

- `-`
- `>`
- `<`
- `^`
- `v`
- `p`
- `q`
- `pp`
- `qq`
- `s`
- `z`
- `V`
- `w`

Goal:

- Lean can derive the same shape key that the reference uses

Status:

- substantially completed for ordinary single-slide tokens in the current parser pipeline
- parser support exists for:
  - `-`
  - `>`
  - `<`
  - `^`
  - `v`
  - `p`
  - `q`
  - `pp`
  - `qq`
  - `s`
  - `z`
  - `V`
  - `w`
- remaining work is same-head slide `*` handling and connected-slide/group semantics

### Phase 5: Port MajdataPlay table data

Mirror the actual queue tables from:

- `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/Utils/SlideTables.cs`

Goal:

- ordinary slide tables become authoritative Lean data
- Wifi left/center/right tables become authoritative Lean data

Status:

- partially completed
- `LnmaiCore.Simai.SlideTables` exists and is used as the fallback authoritative source when runtime slide notes do not already carry `judgeQueues`
- remaining work is to make topology construction fully sourced from Simai normalization rather than fallback attachment during runtime slide building

### Phase 6: Connect normalized slides to runtime `SlideChartNote`

Replace externally supplied hand-authored `judgeQueues` as the primary source of truth.

Goal:

- runtime slides are built from parsed Simai semantics inside Lean

Status:

- partially completed
- normalized slide timing, star-wait timing, shape key, and modifier semantics are sourced from Lean parsing/normalization
- remaining work is connected-slide grouping and direct normalized construction of authoritative `judgeQueues`

### Phase 7: Add proof-facing propositions

Once the table data is real, then the proofs become meaningful rather than self-declared.

Examples:

- ordinary slide tables never use `D`/`E`
- Wifi tables may use `D`
- every ordinary slide table is a queue of judgment segments
- each segment uses one or two zones
- short connected-slide no-skip rule is applied at the correct queue positions
- `V` / `L` turning exception is reflected in the affected segment skip flags

Status:

- not started in the new parser/IR architecture

### Phase 8: Add grouped-source and connected-slide lowering

Add modules or layers for:

- same-head slide source groups (`*`)
- connected-slide group/source semantics
- source-position-aware timing events if needed for diagnostics/spec parity

Goal:

- represent grouped source syntax before normalization
- lower grouped slide source into `ConnPart`/parent-linked normalized slides
- remove the remaining semantic gap between current single-slide parity and MajdataPlay connected-slide behavior

## Known Implementation Gaps Today

The current codebase does **not** yet have the following:

### Gap 1: Slide-shape classification exists, but grouped slide source semantics remain incomplete

The repo now has an explicit single-slide shape classifier, but still lacks full source-level handling for:

- same-head slide `*`
- connected-slide grouping
- source-group-to-runtime parent linkage

### Gap 1b: Lean parser front-end exists, but DSL front-end is still missing

The repo now has a pure Lean parser/front-end pipeline for current maidata/Simai source text, but still lacks:

- a Lean DSL elaborator for proof/spec authoring

The parser core now precedes the DSL as intended; the DSL is the remaining piece.

### Gap 2: Slide table library exists, but full topology construction is not yet front-loaded into normalization

The repo no longer depends only on external slide semantics, but normalized slides still do not fully construct authoritative `judgeQueues` before runtime adaptation.

### Gap 3: Wifi parsing exists, but grouped/connected source semantics are still partial

`Wifi` shape and track semantics are represented, but topology construction should move fully into normalization rather than runtime fallback.

### Gap 4: Ordinary-slide D/E exclusion is not yet enforced purely by constructor path

It is only true if the input `judgeQueues` happen to respect it.

### Gap 5: Turning-slide and connected-slide exceptions are not yet fully first-class in normalized constructors

The `V` / big-turn skip exception is not encoded as a Lean constructor rule today.

### Gap 6: Proof layer still trails the final constructor path

Some proofs currently use local witness models because the authoritative mapping module does not exist yet.

That is acceptable temporarily, but the long-term target is to prove facts about the actual constructor path, not a local surrogate.

## Design Rules for Future Work

When extending this repo, prefer the following rules:

- Do not add new gameplay semantics only in Rust.
- Do not treat external JSON `judgeQueues` as the final desired architecture.
- Do not hide slide species behind only `SlideKind`.
- Keep parsing pure and deterministic.
- Keep the elaborator as a front-end over the parser, never as a second semantic implementation.
- Keep runtime judgment independent from raw textual syntax.
- Normalize first, then build runtime notes.
- Write proofs against the real topology constructor whenever possible.

## Short-Term Development Priority

The next major milestone should be:

1. add grouped source syntax for same-head slide `*`
2. add connected-slide grouping and lowering
3. move authoritative slide topology construction into normalization
4. add Lean DSL front-end
5. expose normalized chart / topology IR cleanly to Rust FFI
6. then continue proof-facing propositions over the real constructor path

This order is preferred because slide correctness depends heavily on parser-derived topology.

## Current Implementation Snapshot

The repository currently has:

- a Lean Simai/maidata source parser pipeline
- a Lean front-end module for parser entrypoints
- a true normalized chart IR module
- Lean-side parity wiring for all current Python reference tests in `../reference/PySimaiParser/tests/test_core.py`
- slide timing/modifier semantics for single-slide source tokens

The repository does not yet have:

- a Lean DSL elaborator
- source-level same-head slide `*` grouping
- connected-slide lowering into normalized parent-linked runtime-ready slides
- fully front-loaded authoritative topology construction for every slide inside normalization

## Acceptance Criteria for This Proposal

This proposal is considered implemented once the repo has:

- a Lean Simai slide-shape classifier
- a Lean Simai parser API
- a Lean Simai DSL front-end
- a Lean authoritative slide table module
- a normalized chart IR layer
- runtime slide construction sourced from Lean topology mapping
- FFI that exposes parser/normalizer results to Rust without moving semantics into Rust

## Repository Intent

Treat this repository as:

- a verified gameplay engine in Lean
- with Rust as a host/runtime shell
- and with parsing semantics moved inward toward Lean over time

This should be the default architectural assumption for future work.
