# LNMai / MajDataPlay parser parity review

This review follows [`majdataplay-parser-parity-plan.md`](majdataplay-parser-parity-plan.md). The scope is maidata parsing and parser-owned lowering. Runtime judgment, scheduling, rendering, Rust host code, and FFI contracts were left unchanged.

## Pipeline inventory

LNMai's pipeline is `Source/Maidata.lean` -> `Tokenize.lean` -> `Typecheck.lean` -> `Shape.lean` / `SlideParser.lean` -> `Normalize.lean` -> `IR.lean` / `ChartLoader.ChartSpec`. `parseSourceMaidata` extracts metadata and `&inote_N` blocks; `parseAndLowerSourceMaidata` selects a level, strips comments, tokenizes comma segments, annotates touch `Each` groups, typechecks connected slides, then produces normalized and lowered charts ([`Maidata.lean:147`](../LnmaiCore/Simai/Source/Maidata.lean#L147), [`Maidata.lean:173`](../LnmaiCore/Simai/Source/Maidata.lean#L173)).

The public boundaries are `FrontendChartInspection` (metadata, source block, tokens, slide semantics), `FrontendSemanticChart` (normalized chart plus `ChartSpec`), and `FrontendChartResult` ([`Frontend.lean`](../LnmaiCore/Simai/Frontend.lean), [`IR.lean`](../LnmaiCore/Simai/IR.lean)). `RawNoteToken` owns lexical flags and timing; `TypedSlidePart` / `TypedSlideExpr` own slide grammar and connection validation; `NormalizedSlide` owns connected-group identity, parent links, queue metadata, timing, break flags, and multiplicity.

MajDataPlay's checked-in integration is centered on `Assets/Scripts/Scenes/Game/NoteLoader.cs`. `CreateSlideGroup` splits connected slides, assigns parent/group metadata, distributes slide time, and shares judge-queue totals ([`NoteLoader.cs:965-1229`](/home/neuron/Projects/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs#L965)). `DetectShapeFromText` maps source text to runtime shape keys ([`NoteLoader.cs:1696`](/home/neuron/Projects/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs#L1696)); `NoteFolding` defines multiplicity/folding ([`NoteLoader.cs:2148-2230`](/home/neuron/Projects/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs#L2148)). Queue shape, `IsSkippable`, and `IsLast` are table data in [`SlideTables.cs`](/home/neuron/Projects/MajdataPlay/Assets/Scripts/Scenes/Game/Utils/SlideTables.cs). `ObjectCounter` demonstrates the independent head-break and slide-body-break count semantics ([`ObjectCounter.cs:614-665`](/home/neuron/Projects/MajdataPlay/Assets/Scripts/Scenes/Game/ObjectCounter.cs#L614)). Slide path/geometry ownership lives under `Assets/Scripts/Scenes/Game/Misc/Parsing/Slide/` (`SlidePathConstructor.cs`, `ParametricSlidePath.cs`, and segment types); those rendering/path details are outside the LNMai parser schema. The checked-out `Assets/Plugins/MajSimai` submodule is empty; the differential oracle was built from external MajSimai source at revision `c292734d028c7993688813d82007a93a631fffc1`.

## Ownership and difference matrix

| ID | Area | Reference behavior | LNMai correction | Status |
|---|---|---|---|---|
| CONN-1 | Connected timing | `CreateSlideGroup` weights each part by prefab child/bar count and redistributes the complete duration | Segment weights and cumulative rational boundaries now preserve the exact whole duration | Fixed |
| CONN-2 | Connected queues | Shared group queue total counts shared endpoints once | `sum(lengths) - (parts - 1)` is attached to every group part; short queues use head/end skip rules | Fixed |
| CONN-3 | `*` and chains | First part owns the head; later parts are headless, parented to the immediate predecessor; groups remain distinct | Group IDs, parent links, group head/end flags, and nested groups now match | Fixed |
| BREAK-1 | Break flags | `IsBreak` describes the head; `IsSlideBreak` describes slide body/segment output | Head and segment parsing are independent and preserved through normalization/lowering | Fixed |
| TIME-1 | Absolute timing | MajSimai accumulates event times from measure/BPM changes | LNMai accumulates exact rational time and quantizes only at token output | Fixed; at most 2 microseconds oracle delta |
| TIME-2 | Wait/duration forms | Seconds, ratios, custom BPM/wait forms are accepted | Added exact parsing and all-bracket custom wait/BPM search | Fixed |
| PARSE-1 | Compatibility grammar | Whitespace, metadata leading whitespace, two-digit shorthand, backticks, and directive placement follow MajSimai | Tokenization and directive handling now match accepted behavior, including discarding pending text before a mid-segment directive | Fixed |
| PARSE-2 | Diagnostics | Unsupported actual `K`, `@`, `c`, and `m` modifiers are errors; standalone unknown `c` is ignored | Explicit errors remain for unsupported modifier use; standalone `c` is ignored | Fixed |
| OPT-1 | Hot paths | Parser behavior is linear in long malformed bracket inputs | Cons/reverse accumulation and flattened multiplicity paths remove quadratic list concatenation | Fixed |

Intentional boundaries remain: rendering-only geometry and runtime judge execution are not copied into the Lean IR, and unsupported gameplay modifiers remain explicit parser errors rather than silently changing gameplay.

## Connected-slide truth table

The values below are normalized per-part lengths, followed by the shared connected-group queue total.

| Fixture | Per-part totals | Shared total |
|---|---:|---:|
| `1-3[4:1]-5[4:1]` | 3, 3 | 5 |
| `1-3[4:1]>5[4:1]` | 3, 7 | 9 |
| `3qq7qq5[192#30:109]` | 7, 7 | 13 |

An asterisk is a same-head simultaneous branch delimiter. Each branch is parsed as a separate slide expression; only the first branch owns the head, while later branches are headless and receive distinct group IDs when they themselves contain chains. A continuous `>`/`<`/chain expression is one connection group, so its later parts are parented to the immediately preceding part. Wifi is accepted in ordinary `*` branches but rejected in continuous connection groups, matching the typecheck boundary and MajDataPlay's connection construction.

## Break truth table

| Fixture | head `isBreak` | body/segment `isSlideBreak` |
|---|---:|---:|
| `1-3[4:1]` | false | false |
| `1b-3[4:1]` | true | false |
| `1-3b[4:1]` | false | true |
| `1b-3b[4:1]` | true | true |
| `1b>3[4:1]` | true | false |
| `1>3b[4:1]` | false | true |
| `1b-3[4:1]b` | true | true |

MajDataPlay copies the independent fields while creating sub-slides (`NoteLoader.cs:1106-1113`) and uses `IsSlideBreak` for slide component break state (`NoteLoader.cs:1348`, `1430`, `1509`).

## Validation evidence

- `lake build LnmaiCore.Simai.Tests`: 75/75 tests pass, including the MajDataPlay regression cases.
- `lake build`: completed successfully, 17,002 jobs.
- The parser CLI was smoke-tested in `frontend`, `inspection`, `semantic`, `normalized`, and `lowered` modes.
- Differential corpus: 85 chart levels from LNMai assets and MajDataPlay Original charts; 85/85 accepted by both parsers. No note-count, kind, position, modifier, or star-wait mismatches were found. Canonical timing differences are float-versus-rational rounding only: maximum timing delta 2 microseconds and maximum duration delta 1 microsecond.
- Repeated normalized output is deterministic; SHA-256 `12b272346f4a10347476779b87df2b5a364363260b8b7074a2a756b91834206e`.
- Real-corpus normalized benchmark: median changed from about 2.348 seconds to 2.473 seconds. Long malformed ratio/bracket stress improved substantially; the 16,000-character case changed from 3.619 seconds to 0.199 seconds median.
- Oracle project rebuilt successfully with `dotnet build tools/parser-parity/Oracle.csproj -c Release -p:MajSimaiSource=/tmp/lnmai-parity-reference`.

The comparison harness and oracle sources are in [`tools/parser-parity/`](../tools/parser-parity). The corpus oracle compares accepted/error outcomes and parser-visible note records; it does not claim equivalence for Unity path meshes, rendering, or runtime judgment. Remaining gaps are limited to unavailable vendored MajSimai source, rendering/runtime behavior outside the parser boundary, and the documented float-versus-exact-rational microsecond rounding.

## Changed files

Parser behavior changes are confined to `LnmaiCore/Simai/Normalize.lean`, `SlideTables.lean`, `Source/Maidata.lean`, `Tests.lean`, `Timing.lean`, and `Tokenize.lean`; two runtime fixture strings in `LnmaiCore/RuntimeTests.lean` were updated only to exercise corrected parser semantics. The comparison harness is under `tools/parser-parity/`.
