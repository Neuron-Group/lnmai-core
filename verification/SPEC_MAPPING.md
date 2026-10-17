# Lean ↔ Rust verification map

The source of truth is `LnmaiCore/`. There are two distinct Rust layers:
`lnmai-core-verify` is extracted by Aeneas; `lnmai-core` is the arbitrary-precision
runtime. A theorem about the first does not automatically cover the second.
The required validation command is `bash lnmai-core-rs/tools/validate_all.sh`.

## Extracted kernel

The following table names proof modules, not a blanket claim that every
function in the corresponding Rust module is proved. A fresh `lake build` and
`Verification.Audit` must accept the current source and generated artifacts.

| Lean specification | Extracted Rust | Proof module and scope |
|---|---|---|
| `Areas` | `areas.rs` | `Areas`: enum bridges, button/slot indices and conversions. `Rotations`: rotations for every machine-size step count. `AreasPublic`: defaults, arrays, clones, equality/order, storage order and conditional arbitrary-Nat rotation/index composition. |
| `Types` | `types.rs` | `Types`: grade/style/note/status/position bridges, grade predicates, score metadata. `TypesExtra`: status order, judgment-result predicates and initial score counters. `TypesPublic`: defaults, clones, equality and exported order, including event kind/phase and runtime positions. |
| `Constants` | `constants.rs` | `Constants`: every exported kernel constant, checked by Lean reduction. Window constants also occur in judge proofs. |
| `Time` | `time.rs` | `Bridge`, `Time`, `Judge`: construction, projection, zero, addition/subtraction, absolute value, scale and Euclidean division. Overflow preconditions are explicit. |
| `Convert` | `convert.rs` | `Convert`: all four grade conversion functions, all enum values. |
| `Judge` | `judge.rs` | `Judge`: tap, touch, classic/modern slide, grade correction and late timeout. `Hold`: classic tail judgment and press-band arithmetic. `HoldModern`: modern tail calculation and all grade-table branches. |
| Unbounded judgment adapters | `judgment_adapter.rs`, runtime `judge.rs` | `JudgmentAdapter`: fifteen theorems passed combined-source elaboration and axiom checking for the actual extracted clamp, tap/touch/slide wrappers, hold calculations, timeout cancellation and fast/late counting. The bundle uses thirteen verbatim declarations from real extraction, with matching source/artifact hashes. The full model is installed; independent audit is pending. Only atomic integer-operation laws are assumed; actual Integer/BigInt and runtime trait refinement remain open. |
| `Score` | `score.rs` | `Score`: base score, nonbreak score and combo. `ScoreExtra`: break score, fast/late flags and DX rank. |
| `Types.JudgeCounts` | `grade_counts.rs` (also used by runtime scoring) | `GradeCounts`: all 15 indices and grade order, generic fixed-array reads/writes, decoded point updates and increments. Bounds are discharged for every grade. No Clone law is needed; the increment theorem assumes decoded addition. |
| `Storage` | `storage.rs` (also re-exported by the runtime) | `Storage`, `StorageLoops`, `StorageAccum`, `StoragePublic`: indices/order, empty defaults, replication, read/write, list conversion, entries, function construction, state accumulation and cloning. Quantifies arbitrary stored values and representable vector lengths. |
| `Simai.Symmetry` | `symmetry.rs` (also re-exported by the runtime) | `Symmetry`: direct/mirror constructors, cloning, mirrored flag, rotation steps, equality modulo eight and action on every sensor area for every `u32` rotation argument. |
| `Simai.Shape` | `shape.rs` | `Shape`, `ShapeSolver`: default, equality, shape constructors, digits, mirror and relative endpoints, and canonical-shape solving for the extracted argument types. |
| `Simai.Tokenize` | `tokenize.rs` (also used by the runtime parser) | `Tokenize`: both character classifiers, bracket-depth scanning, termination and buffer bounds, and exact top-level split results for arbitrary character sequences with length below `Usize.max`. Includes unmatched brackets and empty pieces. Principal theorems passed combined-source elaboration and axiom checks; UTF-8/string conversion and the remaining parser are outside this proof. |
| `Lifecycle.SlideArea` | `lifecycle.rs` (also used by the runtime) | `SlideHistory`, `SlideAreaLoops`, `SlideArea`, `SlideAreaTrace`: indexed history model, loop bounds/termination, defaults, cloning, on/completion queries, checkpoint updates and arbitrary finite repetition of those updates. Progress is an opaque parameter; cloning must preserve values. Current proof-build status is tracked in `SOURCE_AUDIT.md`. |
| `Lifecycle.slideQueueCoreFuel`, `slideQueueCore`, `replaySlideQueue` | `slide_queue.rs`, runtime `lifecycle.rs` | Runtime delegates traversal to the extracted kernel. `SlideQueueModel` compiled separately. `SlideQueue`, `SlideQueueTraversal`, `SlideQueueTrace` and `SlideQueueReferenceTrace` passed combined-source elaboration: extracted recursion, entry points, every intermediate queue, ordered commands and command-free replay over finite frame traces. Fresh separate-module audit is pending. Clone purity and machine container-length conditions remain explicit. |
| `Lifecycle.slideStepSemantic`, `slideStep` | `slide_lifecycle.rs`, runtime `lifecycle.rs` | `SlideLifecycle` passed combined-source elaboration and thirteen principal axiom checks. Shared generic state/control covers checkability, completion/timeout/sampling priority, delayed final judgments, grade conversion, output flags and finite-frame traces. Time, raw judgments, queue snapshots, events and rendering remain reference-interpreted adapters, not extracted runtime implementations. |
| `Lifecycle.slideQueueRemaining`, `slideQueuesCleared` | `slide_queue_metrics.rs`, runtime `lifecycle.rs` | `SlideQueueMetrics`: extracted maximum-length and all-empty scans, termination, index bounds, and equivalence for arbitrary payloads. Nine principal axiom checks passed; runtime container interpretation remains to be connected. |
| Scheduler parent lookup/update, flag refresh and ancestor completion | `slide_parents.rs`, runtime `scheduler.rs` | `SchedulerParents`: thirteen principal checks passed for extracted scans and bounded ancestor traversal, including cycles and duplicate IDs. Atomic read/write/equality/emit, clone, container and capacity contracts remain explicit. `SchedulerParentFields` adds decoded Slice/Vec read/write/clone dictionaries and initialization composition; concrete Rust field/ID encoding remains open. |
| `Simai.Timing.extractBracketContents`, duration-field dispatch | `timing_parser.rs`, runtime `simai/timing.rs` | `TimingParser`: thirteen principal checks passed for nested-bracket scanning, all field routes, strict empty fields and index safety. Numeric parsing, splitting, Rat operations and aggregation remain outside this module; see `NaturalParser` and `TimingAggregate` below for scanning/aggregation proofs and their remaining atomic contracts. |
| `Simai.SlideParser.parseTimingPoint` | `slide_parser_sequence.rs`, runtime `simai/slide_parser.rs` | `SlideParserSequence`: fifteen principal checks passed for indexed collection, reachable-prefix callback contracts, first-error return, result order and note-parser composition. The concrete runtime closure and representations remain explicit boundaries. |
| Installed Lean `String.toNat?` | `natural_parser.rs`, runtime `natural.rs` | `NaturalParser`: 24 principal theorems passed combined-source elaboration and axiom checks for digit classification, separator syntax, fused validation/decimal accumulation and the actual scanner loop. Concrete UTF-8/character collection and BigUint zero/decimal operations remain explicit contracts. |
| `Simai.Timing.parseRatString?` | `timing_number.rs`, runtime `simai/timing.rs` | `StringSplit`, `TimingNumberModel`, `TimingNumberReference`, `TimingNumber`: independently compiled and transitively audited (12 local modules, 146 theorems) for the original splitter/signed rational connection and actual whole/fraction loops. Only decimal primitive laws and character representation are assumed by the extracted scan proof; concrete BigUint/Rat wiring remains open. |
| `Simai.Tokenize` slide-chain parsing | `slide_chain.rs`, runtime `simai/tokenize.rs` | `SlideChainReference`, `SlideChainModel`, six `SlideChainExtracted*` modules, `SlideChainWrapper` and `SlideChainFrontend`: original source/cursor connection, actual scans/render/encoding and optional-chain/error composition. Runtime character filtering calls the proved sanitizer. Current build/audit results are in `SOURCE_AUDIT.md`; prefix stripping, single-call String encoding, UTF-8 and allocation remain explicit boundaries. |
| `Simai.Maidata` line/body parsing and consecutive grouping | `maidata_scan.rs`, runtime `simai/maidata.rs` | `MaidataScanModel`, `MaidataScanFuel`, `MaidataScan`, `MaidataScanGroups*`: original fuel sufficiency plus actual endpoint, body-append, outer collection, count, emission and annotation loops. Installed proof scripts require current build/audit acceptance; native single-slot reads/writes, string/token encoding, resources and frontend wiring remain open. |
| Scheduler/ChartLoader TouchHold group rebuilding | `touch_hold_rebuild.rs`, `touch_hold_body_adapter.rs`, runtime `scheduler.rs`, `chart_loader.rs` | Both callers use the actual extracted generic body adapter. `TouchHoldBodyReads`, `TouchHoldBodyTraits`, `TouchHoldBodyFields`, `TouchHoldBodyScans`, `TouchHoldBodyViewPoints`, `TouchHoldBodyUpdates`, `TouchHoldBodyErase`, `TouchHoldBodyRebuild` and `TouchHoldRebuildAdapter` connect the concrete record/container operations and trait dictionary to the original rebuilds. These new modules await current-source compilation and axiom-audit acceptance. Four immutable HoldSource reads, identifier representation and allocation remain explicit obligations. |
| TouchHold body membership and strict majority | `touch_hold_body_adapter.rs`, runtime `scheduler.rs` | `TouchHoldBodyMajority`, `TouchHoldBodyAdapter` and `TouchHoldScheduleAdapter` connect the actual dictionary to first-match lookup, membership updates, ordered erase and overflow-free majority arithmetic. Pending current-source compilation and axiom audit; head/lifecycle/queue point laws, identifier representation and local capacity remain open. |
| Scheduler counter guards and queue fuel | runtime `scheduler.rs`, extracted scalar/container primitives | Tap/Touch counter-update contracts apply only where a click is available. `SchedulerCounterLocal` and `SchedulerRemainingFields` add local finite-Vec/scalar witnesses, including the exact Touch-head fuel headroom criterion. Pending compilation/audit. Runtime click counters and cursors use Natural(BigUint); these witnesses do not establish native BigUint conversion, actual queue field wiring or allocation. |
| `Simai.Timing.parseDurationSpec`, `parseStarWaitSpec` | `timing_aggregate.rs`, runtime `simai/timing.rs` | `TimingAggregate`: 15 principal theorems passed combined-source elaboration and axiom checks for ordered optional-value accumulation, one final quantization and first-success search composed with the extracted bracket scanner. Atomic parsing, Rat/addition/quantization, closure/string and default-beat laws remain explicit. |
| `Scheduler.processSlideNotesCore` | `slide_schedule.rs`, runtime `scheduler.rs` | `SlideSchedule`: 18 principal theorems passed combined-source elaboration and axiom checks for the extracted traversal, original fuel recursion, immediate parent effects, buffer order, capacity and a history of visits to pending slots. Read/replace, lifecycle, parent and append adapters remain explicit. |
| Scheduler parent callbacks inside slide traversal | `slide_parents.rs`, `slide_schedule.rs`, runtime `scheduler.rs` | `SlideScheduleParents`: seven composition theorems passed combined-source elaboration and axiom checks, connecting the actual parent kernels to the traversal's parent contracts. Concrete facade wiring, metadata, point operations, cloning, container interpretation and capacity remain explicit. |
| Scheduler feedback generation | `frame_commands.rs`, runtime `scheduler.rs` | `FrameCommandsReference`, `FrameCommandCollection`, `FrameCommands`, `FrameCommandFinalization`, `FrameCommandState`: actual optional audio/render/body selection, ordered collection, source settings/time/score and full output composition. Isolated bundle compilation passed; current separate-module audit is pending. Payload/UTF-8/numeric representation and allocation remain explicit. |
| Scheduler initialization and complete frame composition | `slide_parents.rs`, `touch_hold_rebuild.rs`, `scheduler.rs`, `frame_commands.rs` | `SchedulerFrameInitialization`, `SchedulerParentFields`, `SchedulerFramePipeline`: conditional group rebuild and immutable parent snapshot, five derived phases and final feedback, including finite histories. Installed as pending proof scripts; no complete initialization/phase/finalizer equivalence premise, but point contracts and decoded record/number/capacity boundaries remain. |

The installed Tap/Hold/queue extraction is additionally covered by
TapSchedule, HoldSchedule, QueueMaintenance, StorageRefinement,
TapSchedulePass and their judgment/frame compositions. Their v23 evidence
contains 144 principal reports from 51 current source modules. The theorems
preserve all duplicate queue identifiers, arbitrary Nat cursor values,
per-zone accumulation and ordered frame outputs. They retain clone, field,
numeric/container, exact-encoding and capacity premises. The later
HoldScheduleQueueJudgments removes the composite queue-update premise using
slot and primitive queue laws. It and the complete Audit root passed v24
combined-source elaboration and axiom checking (90 modules, 150 principal
reports, 3146 audited theorems, 202 matching hashes). A separate complete
module build remains required; see SOURCE_AUDIT.md for immutable evidence.

`Defaults` additionally states the six extracted structure-default contracts
for duration, time point, judgment result, group state, judgment event and
combo delta. The event default preserves Lean's derived multiplicity of zero;
event construction and the custom JSON fallback separately use one. See
`SOURCE_AUDIT.md` for their current compilation/audit status.

Representative theorem names are `judge_tap_equiv`, `judge_touch_equiv`,
`judge_slide_classic_equiv`, `judge_slide_modern_equiv`,
`judge_slide_too_late_equiv`, `is_too_late_slide_equiv`,
`judge_hold_classic_end_equiv`, `judge_hold_end_equiv`,
`hold_band_grade_equiv`, `score_non_break_equiv`, `update_combo_equiv`,
`score_break_equiv`, `count_fast_late_equiv` and `dx_score_rank_equiv`.

Machine durations embed into Lean integers through `Bridge.toLnmDuration`;
this is not an isomorphism with all Lean durations. Kernel addition,
subtraction, multiplication, absolute value and scoring require representable
intermediates. Corrected `div_nat` uses Euclidean division even on negative
inputs and returns zero for divisor zero. DX rank widens products to `u64`, so
its theorem covers every `u32` input.

Storage proofs relate the extracted implementation directly to `LnmaiCore.Storage`,
including short/long backing vectors, unchanged out-of-range writes and zip
truncation. The `VecEquality` and `StorageEquality` proofs additionally
cover exact backing-list equality, including unequal vector lengths.
`SlideAreaEquality` applies represented equality to all checkpoint fields.
These three modules passed combined-source elaboration; their fresh
separate-module audit is pending.
Loop proofs include termination and bounds for indexing, increments
and pushes. Generic theorems explicitly require value-preserving `Clone` and
callbacks that return the specified pure result; proving those conditions for
each runtime element/callback remains a separate obligation. Allocation/resource
failures, derived debug formatting and JSON serialization are outside this scope.

`SlideParser` proves the shared command classifiers and endpoint indices, then
composes them with explicitly specified character/error adapters. It also proves
all scalar bar-count and last-wait tables under the string-key classifier and
exact rational-construction laws. `SlideParserComposition` replaces the opaque
solver hypothesis with the already proved extracted shape solver. Judge-queue
payloads, transformations and strings remain open; timing-point iteration is
covered by the additional collection proof described below.
`SlideParserFrontend` proves the extracted character reads and single/double
character scan loops, then composes just-type selection and the extracted
shape solver with the original parser. It preserves full diagnostics and
failure order for malformed input as well as success values. Its installed
frontend no longer assumes whole reader, just-type or solver equivalence.
`SlideParserText` additionally proves all thirteen command observations and
the runtime-used character filter, including termination, index/push bounds
and preservation of order. The composition accounts for filtering creating
adjacent pp/qq pairs and removes whole-sanitizer/command-scan hypotheses.
UTF-8 decoding, String encoding, decimal formatting, allocation and exact
rational construction remain explicit boundaries. `SlideParserSequence` proves
the actual indexed collection loop and composes those note interpretations
with the reference timing-point parser. Its callback contract is required only
after a reachable successful prefix; it does not assume a whole batch result.
The concrete runtime closure still needs to satisfy that per-call contract.

`SchedulerFrame` proves extracted pass sequencing, event/command append order
and a conditional bridge to the original `Scheduler.stepFrame`. Its touch-group
search terminates, cannot overflow its index, and selects the first matching ID;
sharing and registration agree with the reference, preserving later duplicates
and using the majority before the update. Pass, point-access, append, arithmetic
and frame initialization/finalization contracts remain explicit hypotheses.
These modules passed combined-source elaboration and principal axiom checks;
they are not proofs of the unextracted runtime callers.

## Runtime modules

`Adapters` and `AdapterJudgments` prove mathematical clamp invariance for
arbitrary integer judgment inputs and compose it with the extracted kernel
theorems. They also model the exact modern-Hold adapter and cover classic Hold
and fast/late flags. `AdapterScore` proves the finite coefficient decomposition
for arbitrary natural scores and combo counters. These are conditional on
the runtime representations implementing the stated integer/natural operations;
the actual Rust `Integer`, `Natural` and bigint-library connection is still
required.

`IntegerAdapters` proves the runtime's mathematical Euclidean-division
algorithm for positive, negative and zero divisors. `AccuracyAdapters`
composes the five rational formulas under explicit arithmetic/decoding laws.
`RuntimeScore` composes the extracted score coefficients and fast/late flags
with the reference's single-event fold, ordered list fold and intermediate
score trace. Their principal theorems passed combined-source elaboration
and axiom checks; the actual bigint/rational implementations and unextracted
runtime callers remain to be connected to those contracts.
`RuntimeCounts` additionally proves decoded select/replace and five-table
routing/update composition using the extracted generic array accessors. Its
principal theorems passed combined-source elaboration and axiom checks under
explicit arithmetic/decoding laws; this does not discharge the runtime bigint
representation or the unextracted caller.

`ShapeAdapters` composes the extracted shape constructors and endpoint
operations with the runtime branches for natural endpoints beyond `usize`.
Its four main theorems passed combined-source elaboration and their axiom
checks. They assume conversion succeeds exactly for representable values
and preserves the value; the actual `Natural::to_usize` implementation still
needs to be connected to this contract. Large endpoints remain unchanged by
mirroring; checked constructors accept them only for circle, v, pq and ppqq.

| Runtime Rust modules | Lean counterpart / validation | Remaining formal obligation |
|---|---|---|
| `areas`, `convert` | Shared extracted enum/conversion code; `AreasPublic` composes arbitrary-Nat rotations/indices with the kernel. | Prove actual unbounded adapters satisfy the exact residue/representation hypotheses. |
| `integer`, `natural`, `rat`, `time` | Exact arithmetic and Lean JSON/numeric oracles, including values beyond 64/128 bits. | Extract or verify the representation, normalization, library operations and all adapters. |
| `types`, `constants`, `judge` | Shared enums/tables; mathematical adapter proofs and grade/timing differential tests. | Connect the actual Rust representations and operations to the proved adapters. |
| `score`, `accuracy`, `runtime_score` | Coefficient, conditional accuracy and event-fold/trace proofs; actual shared count-array reads/writes. Deferred numeric and restored-state comparisons. | Connect actual unbounded arithmetic and runtime callers to the formulas; complete count/state composition. |
| `simai/*` | All frontend modes, shape tables, valid/invalid syntax, errors and the deferred 48-difficulty corpus. | Parser, normalization and lowering equivalence for all inputs. |
| `storage` | Re-exports the extracted implementation; generic operations proved in `Storage*`. | Discharge generic clone/callback hypotheses for runtime types; serialization and allocation assumptions. |
| `symmetry` | Re-exports the extracted implementation; `Symmetry` relates its operations to Lean's dihedral group. | JSON decoding of unbounded rotation arguments and derived debug formatting remain outside the kernel proof. |
| `chart_loader`, `input_model` | Chart/state decoding, loading, ordering and frame comparisons. | Representation invariants and operations on arrays/maps. |
| `lifecycle`, `scheduler`, `events`, `default_tactic` | Extracted Tap/Hold/queue traversal and judgment/frame compositions passed v23; Hold queue point composition and the former Audit root passed v24. SchedulerFrameAllTraversals derives all five passes and finite traces under primitive contracts; the current independent-module audit accepted it and its imports (112 local modules, 5074 theorems). | concrete numeric/container/lifecycle facades, uniform capacity, frame initialization, rebuild and tactic generation. |
| `runtime_seek` | Forward/backward seek, settings, autoplay, replay and JSON restoration scenarios. | Arbitrary trace equivalence and state restoration invariants. |
| `json_decode`, `lean_json`, `ffi`, `cabi`, `native` | Malformed input, JSON text, native records/session errors and compiled C smoke test. | Serialization and session lifecycle equivalence, memory ownership and ABI correctness. |

Native encoding deliberately converts mathematical integers to their low
64-bit words, as Lean's native interface does. This conversion is separate
from gameplay arithmetic, which remains unbounded. Physical array positions
remain machine-size indices; note IDs and gameplay counts do not.

## What constitutes evidence

1. `regen_aeneas.sh --check` checks that extracted LLBC and generated Lean
   correspond to the current kernel source.
2. `lake build` in `verification` checks the declared theorems and runs the
   transitive axiom audit. Only Lean's standard logical axioms are allowed.
3. Differential tests execute both runtimes. Ordinary Cargo tests explicitly
   ignore this suite; the required script enables it and rejects a missing
   oracle. Full corpus outputs are compared every frame; full states are
   compared periodically and at completion. Synthetic traces compare states
   every frame.
4. Native C tests exercise exported symbols, ABI layout, ownership and errors.

These checks have different scopes. Even when all pass, the unproved runtime
rows above prevent a claim of whole-program formal equivalence.

`SOURCE_AUDIT.md` tracks source coverage and the current work order. The
validation scenarios listed above are coverage descriptions, not a claim
that the current revision has passed them. The user authorized the ten
difficulties of Usagi Flap, サイエンス and 蛙石 after the current proof batch
and axiom audit; the rest of the tests remain deferred until the
implementation-equivalence pass is complete.
