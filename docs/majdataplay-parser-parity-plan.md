## Scope and repositories

The only review and modification target is parsing and parser-owned lowering:

- LNMai: `/home/neuron/Projects/LambdaDX/lambdaDX/lnmai-core-rs/lnmai-core-ffi/lnmai-core`
- MajDataPlay reference: `/home/neuron/Projects/MajdataPlay`
- Vendored reference, if present: `reference/MajdataPlay`

Do not modify runtime judgment, scheduler, rendering, Rust host code, FFI
bindings, or unrelated proof/application code unless a parser test or build
requires a strictly minimal compatibility change. The parser boundary includes
maidata extraction, tokenization, syntax/type checking, slide-shape parsing,
slide topology/queue construction, timing/metadata lowering, normalized IR, and
the parser's lowered chart representation.

Use the repository `AGENTS.md` instructions. If `.codegraph/` exists, use
CodeGraph before broad text searches; otherwise use `rg` and direct file reads.
Record exact file and line references for every finding. Do not infer MajDataPlay
behavior from names or comments when executable code, tests, or generated data
can establish it.

## Required first pass: document both projects

Before comparing or editing code, create documentation for **both** projects.
For each parser-relevant module, document:

1. every relevant struct/class/record/enum/inductive type, its fields, and its
   role;
2. every relevant public and private function/method, its inputs, outputs, and
   role in the parse/lower pipeline;
3. the data flow from maidata text to parsed notes, slide shapes, timing,
   connected-slide metadata, break flags, judge areas, normalized IR, and
   lowered runtime objects;
4. the ownership of semantics: which stage decides syntax validity, slide
   identity, timing, grouping, multiplicity, and break classification.

Put the documentation in a review document under `docs/` (or update an
existing parser document), with links to source files and line numbers. Clearly
label facts, inferred behavior, and unresolved questions. Include the
MajDataPlay dependency boundary: `Assets/Plugins/MajSimai` is a git submodule
and may be empty, so document the missing source and use the checked-in
MajDataPlay integration as evidence rather than pretending the dependency is
available.

## Required second pass: TODO list after understanding

Only after both project documents are complete, write a prioritized TODO list.
Each item must include:

- a stable ID and severity (`correctness`, `compatibility`, `diagnostics`, or
  `optimization`);
- the LNMai and MajDataPlay source locations;
- the triggering syntax/fixture;
- current LNMai behavior;
- reference behavior;
- the intended change and its risk;
- the test or oracle that will prove it fixed.

Separate confirmed mismatches from behavior that is intentionally different or
not observable at the parser boundary. Do not start implementation while
semantic questions remain undocumented.

## Reference anchors to inspect

### LNMai

Read the complete call chain, not only exported entry points:

- `LnmaiCore/Simai/Source/Maidata.lean`: metadata/chart block extraction,
  comment handling, event grouping, `parseSourceMaidata`,
  `lowerSourceChartBlock`, and `parseAndLowerSourceMaidata`;
- `LnmaiCore/Simai/Frontend.lean`: public parser and inspection/semantic/
  normalized/lowered boundaries;
- `LnmaiCore/Simai/Tokenize.lean`: directives, token kind inference,
  `parseHeadBreak`, `parseSlideSegmentBreak`, `mkRawToken`, continuous-slide
  parsing (`parseContinuousSlideSegments?`, timing-layout classification and
  expansion), same-head `*` expansion, shared flags, and group tagging;
- `LnmaiCore/Simai/Shape.lean`, `SlideParser.lean`: body grammar, shape
  solving, terminal area parsing, wifi/turn/arc/line classification, and
  syntax errors;
- `LnmaiCore/Simai/Typecheck.lean`: `TypedSlidePart`, connection group
  collection/validation, wifi rejection, and malformed-group errors;
- `LnmaiCore/Simai/SlideTables.lean`: authoritative judge-area tables,
  rotations/mirroring, classic wifi variants, skip flags, and progress values;
- `LnmaiCore/Simai/Normalize.lean`: `lowerSlideToken`, judge queue
  attachment, connected-slide timing/parent metadata, short-queue skip rules,
  whole-group multiplicity folding, and `toChartSpec`;
- `LnmaiCore/Simai/IR.lean` and `LnmaiCore/Simai/Tests.lean`: output schema and
  existing parity/proof fixtures.

### MajDataPlay

Inspect the checked-in source and preserve the distinction between parsing,
loading, and runtime behavior:

- `Assets/Scripts/Scenes/Game/Misc/Parsing/SlideCodeParser.cs`: command grammar
  (`A/B/C/P/Q/K`), node/orbit transitions, invalid sequences, and parse markers;
- `SlidePathConstructor.cs`, `SlideGeo.cs`, `SlideDataBuilder.cs`,
  `Parsing/Slide/*`: path construction, geometry, arrow spacing, hit-area
  lookup, and area timing/progress generation;
- `Assets/Scripts/Scenes/Game/Utils/SlideTables.cs`: ordinary and wifi judge
  queues, classic constants, `IsSkippable`, `IsLast`, and progress values;
- `Assets/Scripts/Scenes/Game/NoteLoader.cs`, especially slide group folding,
  sub-slide splitting, timing distribution, no-head handling, slide/wifi
  creation, `DetectShapeFromText`, and `IsSlideBreak` propagation;
- `Assets/Scripts/Misc/Extensions/SimaiProcessExtensions.cs` and
  `ObjectCounter.cs`: note-family and break-count semantics at the parser /
  chart-data boundary;
- `Assets/Plugins/MajSimai` and its git history/submodule metadata. If it is
  unavailable, state exactly which parser behavior cannot be proven from the
  checkout and construct an explicit gap fixture instead of guessing.

## Comparison method

Build a behavior matrix by parser stage. For each syntax family, compare both
acceptance/errors and successful output:

- maidata fields, chart blocks, comments, blank lines, offsets, BPM/divisor and
  h-speed directives;
- tap, hold, touch, touch-hold, simultaneous `/` entries, Each grouping,
  no-head slides, modifiers (`b`, `x`, `f`, `!`, `?`, `$`);
- every slide operator and shape, including mirrored forms, turn/wifi forms,
  terminal areas, absolute/relative timing, malformed inputs, and unsupported
  paths;
- normalized timing, star wait, duration, source positions, group identity,
  queue areas, skip flags, progress markers, and lowered head/body identity.

Use a canonical intermediate comparison record rather than comparing Lean and
C# object layouts directly. Include source token text, note family, timing in
integer microseconds, slot/area codes, shape key/symmetry, all break/no-head/
EX/hanabi/force/fake flags, connection group index/size/parent, multiplicity,
queue area sets and flags, and error category/message. Mark fields that are
rendering-only or outside parser scope.

## Priority investigation: connected slides (conslide)

Treat connected slides as a first-class audit, not a normal slide variant.
Trace both explicit same-head `*` groups and continuous `>`/`<`/chain syntax.
For each one-, two-, and three-part case, verify:

1. exact grammar and delimiter consumption;
2. whether the first part owns the head and subsequent parts are headless;
3. source group ID/index/size and immediate-parent links;
4. inherited start/end timing and proportional whole-chain timing;
5. rejection of wifi in a connection group and malformed/incomplete groups;
6. same-head tap plus connected-slide behavior;
7. identical whole-group folding and `multiple`, including the negative case
   where only a shared prefix matches;
8. judge queue concatenation, short-queue skip policy, group head/end flags,
   and lowered head/body runtime identities.

Use and extend the existing LNMai fixtures around
`test_same_head_slide_group_lowering`, `test_same_head_conn_three_part_parent_chain`,
`test_continuous_conn_qq_chain_matches_majdataplay`,
`test_identical_simultaneous_connected_slides_fold_group_multiplicity`, and
`test_connected_slide_multiplicity_requires_whole_group_match`. Add minimal
counterexamples for every discrepancy found.

## Priority investigation: break slides (breakslide)

Treat head break and slide-body/segment break as independent semantics. Compare
MajDataPlay's `IsBreak` and `IsSlideBreak` through parsing, sub-slide creation,
group folding, lowered IR, and any parser-visible count data. Audit at least:

- `1b-3[4:1]`, `1-3b[4:1]`, and `1b-3b[4:1]`;
- `1b>3[4:1]`, `1>3b[4:1]`, and equivalent wifi forms;
- connected groups where only the head, only a child segment, or multiple
  segments carry `b`;
- no-head (`!`/`?`) and modifiers combined with segment break;
- simultaneous slides with different head/body break flags;
- break propagation through multiplicity folding and split head/body output;
- malformed marker placement and whether it must be rejected or preserved.

Do not collapse `isBreak` and `isSlideBreak` merely because both contain the
word “break”. Establish the reference rule for each output object and preserve
the distinction in `RawNoteToken`, `NormalizedSlide`, slide semantics, and
lowered objects. Extend `test_slide_break_on_segment` and
`test_lowered_slide_break_split_uses_segment_break_for_body` with a complete
truth table and negative/error cases.

## Implementation rules

For each confirmed mismatch:

1. reproduce it with the smallest parser fixture;
2. identify the earliest incorrect stage;
3. fix that stage with total, deterministic Lean code and an explicit
   `ParseError` for invalid syntax;
4. avoid duplicating semantics in later lowering stages;
5. preserve exact integer/rational timing where possible and avoid accidental
   float rounding or quadratic scans in hot parser paths;
6. keep public IR compatibility unless the comparison proves the field is
   wrong, and document any schema change;
7. add a regression test before or with the fix, using `native_decide` proof
   tests where the existing suite supports them;
8. optimize only after parity is established, and show that optimization does
   not change canonical output.

Do not weaken validation, silently accept malformed slides, or copy C# runtime
workarounds into the parser without proving that they are parser semantics.

## Verification gates

Run and record:

- `lake build`;
- the focused Simai test/proof targets and all new parity fixtures;
- parser CLI JSON checks for `frontend`, `inspection`, `semantic`,
  `normalized`, and `lowered` modes;
- a differential corpus containing existing LNMai assets plus representative
  MajDataPlay maidata files, with success/error outcomes and canonical IR diffs;
- deterministic repeated runs and, if an optimization changes complexity, a
  before/after benchmark on the same corpus.

For every remaining mismatch, report whether it is caused by unavailable
MajSimai source, an intentional product difference, an unsupported syntax, or a
confirmed LNMai defect. Never report parity from aggregate counts alone.

## Final deliverables

Return:

1. the two-project parser documentation;
2. the post-understanding TODO list and its completion status;
3. a source-anchored difference matrix;
4. the connected-slide and break-slide truth tables;
5. the LNMai changes, grouped by root cause;
6. tests, corpus results, build output, and benchmark evidence;
7. unresolved gaps, assumptions, and compatibility risks;
8. a concise list of files changed and why.

Keep all edits inside the parser scope defined above and leave the repository in
a buildable state.
