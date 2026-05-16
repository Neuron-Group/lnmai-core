<p align="center">
  <img src="icon/icon.svg" alt="Project icon" width="96" height="96" />
</p>

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
- `LnmaiCore.Proofs.Simai` — proof-oriented parser/normalizer helpers
- `LnmaiCore.Proofs.Runtime` — replay-style simulation and manual tactic helpers
- `Proofs.RealChartVerification11358` — theorem-backed proof for the 11358 level-5 chart
- `Proofs.RealChartVerificationPandora` — theorem-backed proof for the Pandora level-6 chart

Repository layout highlights:

- `LnmaiCore/` — library source
- `Proofs/` — proof modules and verification entrypoints
- `Scratch/` — ad hoc investigation scripts and experiments
- `tools/` — helper scripts and external tooling assets

## Simai DSL

The repo now includes a Lean DSL layer backed by the same parser/normalizer core used at runtime.

Available forms include:

- `simai_chart! "..."`
- `simai_chart_at! 2 "..."`
- `simai_semantic_chart! "..."`
- `simai_inspection_chart! "..."`
- `simai_normalized_chart! "..."`
- `simai_lowered_chart! "..."`
- `simai_chart_file! "path/to/maidata.txt"`
- `simai_chart_file_at! 2 "path/to/maidata.txt"`
- `simai_semantic_chart_file! "path/to/maidata.txt"`
- `simai_inspection_chart_file! "path/to/maidata.txt"`
- `simai_normalized_chart_file! "path/to/maidata.txt"`
- `simai_lowered_chart_file_at! 6 "path/to/maidata.txt"`
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
- real-chart proof modules backed by file-loaded Simai literals and `native_decide`

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

## Hand-Tactic DSL Proposal

`lnmai-core` now has a proof-facing runtime wrapper for simulating manual tactics against chart sections. The next design focus is the hand-tactic DSL used to describe those manual actions precisely.

The proposal lives in `docs/hand-tactic-dsl.md`.

Summary:

- represent a hand tactic as timestamped button and sensor trigger actions
- keep the DSL close to runtime semantics so proofs execute the same logic flow
- support direct clicks, press/release events, and future interval sugar for holds
- use the DSL as the proof-facing surface for AP and AP+ theorems

## Building

This repository uses Lean 4 and Lake.

- Toolchain: `leanprover/lean4:v4.29.0`
- Build library and executables: `lake build`
- Build aggregate verification executable: `lake build real-chart-verification`
- Build theorem-backed chart proofs: `lake build Proofs.RealChartVerification11358 Proofs.RealChartVerificationPandora`
