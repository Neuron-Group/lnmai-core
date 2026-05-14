# Exact Time Refactor Plan

This document specifies the exact-time refactor for `lnmai-core`.

The refactor replaces gameplay-facing `Float` timing with exact integer ticks
across the Simai compiler, normalized and lowered chart representations,
runtime state, scheduler logic, judge logic, replay logic, and Rust FFI
boundaries.

## Objective

The codebase will use one exact machine representation of time throughout the
 core.

The refactor will establish:

- exact compiler timing
- exact normalized and lowered chart timing
- exact runtime current-time and note timing
- exact scheduler and judge comparisons
- exact replay timing
- exact Rust FFI time exchange

The end state is that gameplay-facing core timing no longer uses `Float`.

## Core Time Model

The refactor introduces three time-domain types.

### Machine representation

- `structure TimeTick where`
- `  val : Int`

`TimeTick` is the exact machine-facing representation of time.

### Semantic time domains

- `structure Duration where`
- `  ticks : TimeTick`

- `structure TimePoint where`
- `  ticks : TimeTick`

`Duration` and `TimePoint` are distinct semantic domains. Both are backed by
`TimeTick`. The machine representation is shared. The semantic roles are not.

## Time Algebra

The refactor uses an affine time model.

The allowed operations are:

- `TimePoint + Duration -> TimePoint`
- `TimePoint - Duration -> TimePoint`
- `TimePoint - TimePoint -> Duration`
- `Duration + Duration -> Duration`
- `Duration - Duration -> Duration`
- comparison on `TimePoint`
- comparison on `Duration`

The refactor does not define:

- `TimePoint + TimePoint`
- implicit mixing of raw `Int` with time-domain values
- APIs that accept either point-or-duration without making the role explicit

This algebra is part of the target API surface. It is not deferred to a later
phase.

## Local Timeline Semantics

`TimePoint` denotes a point on the local song or simulation timeline.

Examples:

- note timing
- slide start timing
- judge timing
- current runtime time
- input event time relative to play start

`Duration` denotes a span on that same local timeline.

Examples:

- hold duration
- slide wait
- slide travel duration
- frame delta
- judge window width
- offset correction
- difference between actual and expected timing

`Duration` is the result of subtracting one `TimePoint` from another.

## Machine Representation Policy

The codebase will not use:

- `abbrev TimeTick := Int`
- alias-based distinctions between `Duration` and `TimePoint`

The codebase will use:

- a real wrapper for machine ticks
- real wrappers for semantic domains

This keeps the machine representation explicit and the semantic distinction
enforced in the type system.

## Tick Unit

The refactor uses one fixed tick unit across compiler, runtime, and FFI.

The canonical unit is:

- microseconds (`μs`)

The codebase will define this unit in one canonical time module and reference
it through shared helpers. The core semantics will not treat the unit as a
runtime-configurable parameter.

All gameplay-facing timing in the Lean core will therefore be represented as an
exact integer number of microseconds.

## Conversion Policy

All chart timing constructs will convert to `TimeTick` in microseconds through
one centralized set of helpers.

The refactor will not leave timing conversion spread across unrelated modules.

Chart timing constructs include:

- BPM-derived beat durations
- note fractions such as `4:1`
- absolute forms such as `#2.5`
- mixed slide timing forms such as `[0.2##0.75]`

The implementation will define one documented quantization policy for cases
where chart-derived timing does not land on an exact integer number of
microseconds.

The codebase will not use:

- local one-off rounding rules
- hidden truncation inside parser or runtime helpers
- separate compiler and runtime interpretations of the same syntax

## Required API Surface

The time module will provide:

- equality and decidable equality
- ordering
- zero values where appropriate
- conversion between wrappers and `TimeTick`
- conversion between `TimeTick` and `Int`
- conversion between `TimeTick` and microseconds
- FFI conversion helpers
- debug and display helpers
- the affine algebra operations listed above
- core lemmas for arithmetic, ordering, and wrapper conversions

The implementation may provide additional named helpers such as:

- `ofInt`
- `toInt`
- `fromMillis`
- `fromMicros`
- `toMicros`
- `scaleNat`
- `quantize`
- BPM- and beat-derived time constructors

## Refactor Scope

The refactor applies to the full Lean core.

Primary targets include:

- `LnmaiCore/Simai/Timing.lean`
- `LnmaiCore/Simai/Tokenize.lean`
- `LnmaiCore/Simai/Syntax.lean`
- `LnmaiCore/Simai/IR.lean`
- `LnmaiCore/Simai/Frontend.lean`
- `LnmaiCore/Simai/Normalize.lean`
- lowered chart timing structures
- runtime lifecycle state
- scheduler timing progression
- judge window and diff calculations
- runtime and input timing models
- replay and proof-facing APIs

Every gameplay-facing timing field in these layers will migrate away from
`Float`.

## Migration Sequence

The migration proceeds in four phases.

### Phase 1 — Introduce exact time domain types

Add a dedicated time module, likely:

- `LnmaiCore/Time.lean`

Define:

- `TimeTick`
- `Duration`
- `TimePoint`
- the affine time algebra
- wrapper conversion helpers
- ordering and arithmetic lemmas
- centralized quantization and construction helpers

This phase establishes the new time API.

### Phase 2 — Migrate compiler-side timing

Replace `Float` in the Simai compiler pipeline.

Migrate:

- parsed timing fields
- note durations
- slide wait times
- normalized timing fields
- lowered chart timing outputs

This phase establishes exact chart semantics.

### Phase 3 — Migrate runtime timing

Replace runtime-facing `Float` timing fields.

Migrate:

- current runtime time
- note timing and durations
- offsets
- frame deltas
- judge windows
- judge diffs

This phase establishes exact runtime semantics.

### Phase 4 — Migrate tests and proof-facing APIs

Replace `floatEq`-style assertions with exact equality over `Duration` and
`TimePoint`.

Strengthen proof-facing APIs around:

- parsing
- normalization
- lowered chart output
- runtime stepping
- event emission

This phase establishes exact proof-facing behavior.

## Implementation Outcomes

After the refactor:

- timing regressions use exact equality
- scheduler invariants use integer order reasoning
- judge logic uses exact deltas and windows
- replay timing is deterministic in the core model
- Rust FFI carries exact time values
- proof-facing APIs expose exact runtime traces

## Non-goals

The refactor does not define UTC or wall-clock semantics inside the Lean core.

The refactor does not keep gameplay-facing `Float` timing for convenience.

The refactor does not use aliases in place of semantic time structures.

## End State

The final system has:

- exact chart compilation over microsecond ticks
- exact runtime progression over microsecond ticks
- exact judgment and replay semantics over microsecond ticks
- exact proof-facing APIs over the same timing model
- exact Rust FFI time exchange over the same machine representation

The final system uses one exact local game timeline and no gameplay-facing
`Float` timing.
