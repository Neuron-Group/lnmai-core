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
| `Score` | `score.rs` | `Score`: base score, nonbreak score and combo. `ScoreExtra`: break score, fast/late flags and DX rank. |
| `Types.JudgeCounts` | `grade_counts.rs` (also used by runtime scoring) | `GradeCounts`: all 15 indices and grade order, generic fixed-array reads/writes, decoded point updates and increments. Bounds are discharged for every grade. No Clone law is needed; the increment theorem assumes decoded addition. |
| `Storage` | `storage.rs` (also re-exported by the runtime) | `Storage`, `StorageLoops`, `StorageAccum`, `StoragePublic`: indices/order, empty defaults, replication, read/write, list conversion, entries, function construction, state accumulation and cloning. Quantifies arbitrary stored values and representable vector lengths. |
| `Simai.Symmetry` | `symmetry.rs` (also re-exported by the runtime) | `Symmetry`: direct/mirror constructors, cloning, mirrored flag, rotation steps, equality modulo eight and action on every sensor area for every `u32` rotation argument. |
| `Simai.Shape` | `shape.rs` | `Shape`, `ShapeSolver`: default, equality, shape constructors, digits, mirror and relative endpoints, and canonical-shape solving for the extracted argument types. |
| `Simai.Tokenize` | `tokenize.rs` (also used by the runtime parser) | `Tokenize`: both character classifiers, bracket-depth scanning, termination and buffer bounds, and exact top-level split results for arbitrary character sequences with length below `Usize.max`. Includes unmatched brackets and empty pieces. Principal theorems passed combined-source elaboration and axiom checks; UTF-8/string conversion and the remaining parser are outside this proof. |
| `Lifecycle.SlideArea` | `lifecycle.rs` (also used by the runtime) | `SlideHistory`, `SlideAreaLoops`, `SlideArea`, `SlideAreaTrace`: indexed history model, loop bounds/termination, defaults, cloning, on/completion queries, checkpoint updates and arbitrary finite repetition of those updates. Progress is an opaque parameter; cloning must preserve values. Current proof-build status is tracked in `SOURCE_AUDIT.md`. |
| `Lifecycle.slideQueueCoreFuel`, `slideQueueCore`, `replaySlideQueue` | `slide_queue.rs`, runtime `lifecycle.rs` | Runtime delegates traversal to the extracted kernel. `SlideQueueModel` compiled separately. `SlideQueue`, `SlideQueueTraversal`, `SlideQueueTrace` and `SlideQueueReferenceTrace` passed combined-source elaboration: extracted recursion, entry points, every intermediate queue, ordered commands and command-free replay over finite frame traces. Fresh separate-module audit is pending. Clone purity and machine container-length conditions remain explicit. |

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
| `lifecycle`, `scheduler`, `events`, `default_tactic` | Synthetic frames, seeded input, full corpus playthroughs in four mode combinations. | Transition relation, event ordering, scoring/audio/render and tactic generation. |
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
that the current revision has passed them. Tests remain deferred until the
implementation-equivalence pass is complete.
