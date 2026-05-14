# `lnmai-core`

`lnmai-core` is a Lean-first gameplay core for maimai-style chart parsing,
normalization, queue construction, and judgment.

The project direction is:

- Lean is the gameplay authority.
- Rust is the host shell for IO, rendering, audio, timing, input, and FFI.
- Simai parsing and slide-topology construction belong in Lean, not Rust.

## Current Architecture

The repository is organized around a Lean pipeline:

`Simai source -> frontend/parser -> normalized chart IR -> runtime notes -> scheduler/judgment`

Important modules:

- `LnmaiCore.Simai.Syntax` — parse-facing source data and errors
- `LnmaiCore.Simai.Source.Maidata` — maidata/front-end source handling
- `LnmaiCore.Simai.Frontend` — public parser/normalizer entrypoints
- `LnmaiCore.Simai.Shape` / `LnmaiCore.Simai.SlideParser` — slide-shape semantics
- `LnmaiCore.Simai.SlideTables` — authoritative slide topology tables
- `LnmaiCore.Simai.Normalize` — normalized chart IR construction
- `LnmaiCore.ChartLoader` — adapter from normalized IR into runtime note state
- `LnmaiCore.Lifecycle` — runtime note stepping and judgment transitions

## Simai DSL

The repo now includes a Lean DSL layer backed by the same parser/normalizer core used at runtime.

Available forms include:

- `simai_chart! "..."`
- `simai_chart_at! 2 "..."`
- `simai_semantic_chart! "..."`
- `simai_inspection_chart! "..."`
- `simai_normalized_chart! "..."`
- `simai_lowered_chart! "..."`
- `simai_note! "1-3[4:1]"`
- `simai_slide! "1V35"`
- `simai_normalized_slide! "1w5[4:1]"`

These macros validate literals at elaboration time and do not introduce alternate semantics.

## Proof Direction

The proof layer is moving away from hand-written local witnesses toward parser-derived topology.

Recent work includes:

- shared proof-facing queue replay over the real slide queue core
- shared Simai-spec to runtime-queue interop helpers
- Ghost Tokyo proofs rewritten around parser-derived full queues and exact replay
- removal of older reduced Ghost Tokyo proof scaffolding

## Runtime Boundary

The preferred long-term boundary is:

- Rust sends raw chart text to Lean
- Lean parses and normalizes it
- Rust consumes normalized IR or requests Lean-built runtime state

This keeps chart semantics, slide topology, and judgment behavior inside Lean.

## Status Summary

Substantially in place today:

- Lean Simai/maidata source parsing
- Lean slide-shape classification
- Lean authoritative slide-table topology attachment
- normalized chart IR
- parser-backed Lean DSL macros
- runtime slide construction sourced from Lean topology
- proof-facing exact replay over parser-derived queues

Still open or incomplete:

- cleaner FFI exposure of normalized chart/inspection boundaries
- broader proof coverage over the real constructor path
- more cleanup around generic proof APIs and shared conversion surfaces

## Typed Domain Refactor Plan

`lnmai-core` is moving toward a typed gameplay-identity model: sensor areas and outer buttons should be represented by explicit inductive domains throughout the Lean core, while numeric indices are retained only as hidden storage or ABI details.

This change is architectural, not cosmetic. The goal is to ensure that parser IR, normalized IR, runtime note logic, proof APIs, and FFI payloads all speak in terms of gameplay identities rather than unstructured integers.

A detailed design and migration roadmap lives in `docs/typed-domain-refactor.md`.

Summary:

- use typed `SensorArea` and `ButtonZone` domains across semantic and runtime-facing APIs
- keep exact symbolic slide topology as the source of truth
- confine numeric encodings to private storage helpers and explicit boundary layers
- migrate proofs and FFI around the same typed identity model

Current status:

- `LnmaiCore.Simai.SlideTables` already uses exact symbolic sensor areas for canonical slide topology
- runtime-facing numeric area lists still exist as compatibility/storage artifacts
- the remaining refactor is to propagate the same design through the rest of the engine

## Building

This repository uses Lean 4 and Lake.

- Toolchain: `leanprover/lean4:v4.29.0`
- Build library: `lake build LnmaiCore`

Some repo-local executable/demo code may lag behind the library target; the Lean library is the authoritative target for current architecture work.
