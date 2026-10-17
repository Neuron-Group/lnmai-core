# Source equivalence audit

This is the working record for the Rust rewrite, not a whole-program proof.
The reference is the current `LnmaiCore/` tree on `rust`, including the
`main` merge at `9b6bffe`. The working tree is part of the audit target.

## Work order and evidence

The requested order is implementation/source equivalence first, tests second.
The latest user instruction authorizes testing the three requested songs as
soon as the current proof batch and its axiom audit finish, before continuing
the remaining equivalence obligations. The full suite remains deferred.
No unit, differential, real-chart or ABI tests have been run in the current
equivalence pass. Library-only Rust typechecking, extraction and Lean theorem
compilation are permitted in this pass. The interrupted Koishi diagnostic is
not a passing acceptance run.
The deferred differential corpus now contains 48 difficulties from 14 songs,
including `011996 Usagi Flap` (levels 2–5), `011951 サイエンス` (levels 2–5)
and `蛙石` (levels 5–6). The same registration feeds frontend and complete
playthrough comparisons. All ten also appear in Lean's AP/no-missing-note
acceptance checkpoints. This is source registration only, not a test result.
Dedicated `requested_corpus_*` differential entries enforce the exact ten
difficulties and reuse the full-corpus frontend/playthrough comparisons.
They have not run yet.

The current priority is parser, complete note lifecycles, scheduler and
judgments. Count/combo-display work is deferred behind those paths.

There are three distinct kinds of evidence:

- Source review compares branches, error precedence, defaults and ordering.
  It can identify and repair differences, but is not a universal theorem.
- Adapter theorems prove mathematical formulas assuming the runtime's integer,
  natural, rational and container operations implement those formulas.
- Extracted Rust theorems refer to the Aeneas translation of the actual kernel.
  Their explicit bounds and clone/callback assumptions remain part of the
  contract. They do not prove unextracted callers or resource/ABI behavior.

`SPEC.md` and `SPEC_MAPPING.md` describe theorem scope. `Verification.Audit`
checks transitive axiom dependencies. Neither a source-review entry nor a
passing finite comparison discharges the remaining formal obligations.

## Module coverage and concrete obligations

| Lean source | Rust source | Current evidence and reviewed behavior | Still open |
|---|---|---|---|
| `Areas`, `Types`, `Constants`, `Convert` | kernel modules and runtime adapters | Extracted enum constructors, defaults, equality, order, arrays, indices, conversions and constants. `AreasPublic` composes arbitrary-Nat rotation/index adapters with the kernel under exact residue/representation hypotheses. `TypesPublic` includes runtime positions and event kinds/phases. | Prove actual unbounded adapter operations and serialization. |
| `Time` and Lean integer/natural/rational arithmetic | `integer`, `natural`, `rat`, `time` | Reviewed canonical small/large integer promotion, Euclidean division, division by zero, saturating Nat subtraction, modulo zero, positive normalized rational denominator and truncated decimal output. Kernel arithmetic proofs retain overflow conditions. | Actual bigint representation/library refinement. |
| `Judge`, `Score` | kernel modules, `grade_counts`, `judge`, `score`, `accuracy`, `runtime_score` | Extracted bounded judgment/scoring and array-access theorems; conditional unbounded coefficients, five accuracy formulas and event-fold/trace composition. Runtime now shares all 15 grade indices, grade order and generic count-array reads/writes with the extracted kernel. Source review also covers count-based combo display, Hold-head exclusion, minimum multiplicity, Break routing and display selection. | Actual bigint/rational representations, runtime caller and complete score-state refinement. |
| `Storage`, `Simai.Symmetry` | shared kernel `storage`, `symmetry` | Runtime directly re-exports extracted code. Storage proofs include short/long backing vectors, out-of-range writes, truncation, construction and accumulation. Symmetry uses rotations modulo eight. | Runtime clone/callback hypotheses, allocation and JSON. |
| `Simai.Shape` | kernel `shape`, runtime `simai/shape` | `Shape`/`ShapeSolver` cover defaults, equality, constructors, digits, mirror/relative endpoints and shape solving. `ShapeAdapters` composes the extracted operations with the runtime's large-endpoint branches under an explicit exact-conversion contract. | Prove actual bigint conversion satisfies that contract; parser/table composition. |
| `Simai.Syntax`, `IR`, `Tokenize`, `SlideParser`, `Timing`, `Maidata`, `Typecheck`, `Normalize`, `Frontend`, `DSL`, `SlideTables` | `simai/*` | Source pass corrected error order, public APIs, exact timing and normalization/lowering. Lean numeric underscores now work in natural timing, rational whole parts and Maidata suffixes; public Maidata aliases and typed-slide defaults are aligned. JSON inventory includes Syntax/IR and canonical shapes. | End-to-end arbitrary-input parser, table, normalization and lowering proof. |
| `ChartLoader`, `InputModel` | `chart_loader`, `input_model` | Earlier source pass aligned public loading/input/queue APIs and defaults. `FrameInput` JSON decoding added in this pass; defaults do not make required JSON fields optional. | Container/ordering representation proofs and frame composition. |
| `Lifecycle`, `Scheduler`, `AutoplayPlan` | `lifecycle`, `scheduler`, `events`, `default_tactic` | Completed source pass through lifecycle, scheduler and probes: per-target latching, zip truncation, legacy seeding, queue traversal/fuel, WiFi progress, gating, timeout-before-traversal and command order. Autoplay review covers equal-time ties, stable events, duplicate IDs, connected roots, WiFi deduplication and hold release timing. Runtime shares generic `SlideArea<P>` and queue traversal with the extracted kernel. Checkpoint proofs compiled separately; queue proofs and every-frame reference/replay bridges passed combined-source elaboration. | Fresh separate-module audit; complete transition/event-order proof, including EX, hold heads/tails, parent slides, touch groups and all settings. |
| `RuntimeSeek` | `runtime_seek` | Reviewed duplicate IDs (first root lookup, last map entry), original queue ranks, queue rebuild truncation/default filling, replay boundary `max(current, target - 1)`, target-time input retention, settings/held state, score reset and expired slide restoration. Nat hash uses low 64 bits; render-memory bucket/list order follows Lean HashMap. | Arbitrary-trace seek/restoration and HashMap iteration-order proofs. |
| JSON instances throughout `LnmaiCore` | `json_decode`, `json_encode`, `lean_json`, FFI encoders | Inventory covers 75 derived ToJson, 71 derived FromJson, 26 concrete custom instances and 50 structure field sequences. Reviewed constructor retrieval/error order, missing-as-null, qualified field errors, custom fallback distinctions, nested Lean triples, escaping, rational errors and unbounded rotation reduction. FFI runtime step/light results and chart summaries now use their shared Encode implementations. | JSON parser/formatter and arbitrary-input serialization proof. |
| `FFI` | `ffi`, `cabi`, `native` | Public result/summary/session/registry types; one registry lock; wrap/collision/unload/erase semantics; native output success before state commit; exact missing seek-origin error. Native low-word conversion is separate from unbounded gameplay arithmetic. | Session/ownership/ABI proof and deferred native acceptance. |

## Most recent non-test checks

The current source checkpoint installs the actual extracted Rust TouchHold
body adapter used by Scheduler and ChartLoader, with proofs connecting its
rebuild and scheduling dictionaries to record, first-match scan, update/erase
and strict-majority operations. The new proof modules are pending separate
compilation and the current-extraction axiom audit. Rebuild retains four
immutable source-read laws and primitive identifier laws; scheduling retains
head, lifecycle, queue and local size contracts. These are not whole-runtime
equivalence theorems.

The same batch narrows Tap/Touch counter-update laws to available-click
branches and adds concrete finite-Vec remaining/fuel witnesses. Runtime click
counters are Natural(BigUint), not usize. The actual fixed-width Touch-head
fuel condition is remaining + 1 <= Usize.max at the visited queue; its native
field/conversion wiring remains open. Prefix/Maidata diagnostic repairs and
the two frame-trace tuple-destructuring repairs are included. No tests were
run, and committing these sources is not proof acceptance.

Checkpoint `5753781` preserves the prefix/prepend/erase kernels and literal-safe
extraction before the current batch. TouchHold now shares its actual generic
record and field operations with the extracted kernel: indexed reads, first-match
scans, membership, prepend, ordered erase, append, remove and swap. The public
runtime type is this record at Natural. Duplicate IDs and order are preserved;
the erase shrink branch never clones a retained value. Formatting and the
preceding workspace library-only Cargo check passed. The repeat library check
after the explicit Default/empty correction passed in
`/tmp/lnmai-touch-hold-fields-library-check-5.log`. No execution tests were run.

The installed canonical extraction is
`/tmp/lnmai-touch-hold-adapter-extraction-6/`. All current kernel/runtime source
hashes match its manifests. The normalized full Aeneas SHA256 is
`6256a77c459e9d1b0974f172065a6bb01d9cd0f4276e06777e25f632c707af2c`.
`partition_generated.py` installs its 73 new adapter blocks separately in
`GeneratedTouchHoldBodyAdapter.lean`; the 1229 unchanged core blocks retain
the exact previous `Generated.lean` bytes. `extraction-manifest.json` records
every block once and reconstructs the full extraction byte for byte, including
literal CR characters and original order. This avoids invalidating all core
proof artifacts on an additive extraction. The partition and regeneration
integration were independently reviewed in
`/tmp/lnmai-stage6-partition-static-review-gq9x9xj4/delivery-1/REVIEW.md`;
the review and byte reconstruction are provenance checks, not proof acceptance.

The unchanged core comes from `/tmp/lnmai-touch-hold-fields-extraction-5/`.
Its Generated SHA256 is
`61f040d6f4e25a3d202a3737085eef0fd13533994296e7570d6066ed7188c634`;
the historical stage5 LLBC SHA256 is
`b7828eddad6aba4e2d21042a1a99e504039811cb4e4240b0cff903f8cabae619`.
All 1188 previous declaration bodies are unchanged and 40 TouchHold declaration
blocks were added. The byte-preserving comparison is recorded in its
`declaration-comparison.json`. Immutable raw Aeneas output has SHA256
`c67082347dd7a49777fe4db36e7c733efa8b363c6606a13f401874a5d454c262`.

The stage4 diagnostic extraction exposed two new opaque runtime operations,
Vec.is_empty and Vec.Default. They are not accepted assumptions: Rust now uses
length comparison and explicit Vec::new, and stage5 removes both axioms.
`check_generated_axioms.py` rejects new nonformatter axioms before installation;
the transitive Lean audit must still show that no theorem reaches any of the
six existing formatter axioms. The stage3 literal-CR correction is preserved.
The byte-preserving normalizer rejects changes to literal contents, and no
CR-free theorem is accepted as unrestricted prefix equivalence. Generated is
installed from actual Charon/Aeneas output, never edited by hand. Extraction
and matching hashes alone do not certify the pending proofs.

The preceding separate-module batch finished in
`/tmp/lnmai-frame-feedback-proof-batch-1.log`. It compiled the frame feedback
selectors/collector/finalization, all five scheduling traversals, normalization
frontend, timing-number parser and TouchHold rebuild traversal against the
checkpoint extraction. FrameCommandState failed on record-update syntax; the
source correction passed the following diagnostic batch. That batch was not an independent
axiom-audit acceptance and does not certify the newer runtime wiring.

A separate-module build against stage5 finished in
`/tmp/lnmai-fields-proof-batch-5.log`: 120 compilation attempts, 113 passed and
seven failed modules, with eight additional dependent skips. The failures were
TouchHoldBodyReads, VecPrependModel, SchedulerFramePipeline,
PrefixDirectivesReference, PrefixDirectivesRanges, MaidataScanGroupsPlan and
SlideChainExtractedEncode. All seven now have source repairs; acceptance of
those repairs is pending the consolidated
`/tmp/lnmai-adapter-proof-batch-6.log` build and independent axiom audit.
The actual GeneratedTouchHoldBodyAdapter extension has compiled in batch6.
No test suite has been run. The preceding stage4 diagnostic compiled
Generated, PrefixDirectivesModel and MaidataScan, but found errors in
PrefixAsciiTrim, PrefixDirectivesRanges and MaidataScanGroupsModel. It was
stopped at a module boundary before installing stage5. Those diagnostics and
the older stage2 run are not stage5 acceptance evidence.

New source proofs include unrestricted prefix stripping, composed connected
slide parsing, and Maidata value byte bounds derived from the original parser,
removing the valueBytes assumption. Actual TouchHold record reads, Default and
Clone proofs are being integrated with concrete mutation/scan proofs. The
indexed vector prepend/erase proofs and source-length-derived rebuild capacity
proofs still require the consolidated build and fresh independent axiom audit.
The stage5 build has compiled Generated, VecEraseModel, VecEraseExtracted and
TouchHoldRebuildReverseExtracted. The first actual record-read diagnostics and
the VecPrependModel singleton-index goal have source fixes awaiting the next
batch. No completed current audit or whole-runtime equivalence is claimed.

`SchedulerParentFields` supplies decoded Slice/Vec read/write/clone dictionaries;
`SchedulerFramePipeline` composes initialization, the five derived phases and
extracted feedback. These dictionaries are not extractions of the Rust record
and bigint facades. Their capacity/point assumptions must have concrete witnesses;
a theorem with unrealizable universal finite-capacity premises is not runtime
coverage. Those obligations, UTF-8 conversions, allocation, scalar representations
and remaining seek/JSON/ABI work remain open.

Previous independent audits remain evidence for their recorded snapshots:

- `/tmp/lnmai-priority-independent-audit-3.accepted.json`: 112 recursive local
  modules, 5074 transitive theorems, all 40 previous kernel and 40 runtime
  source hashes checked. Roots cover all five scheduling phases, TouchHold
  queue/judgment composition, extracted reverse/swap and normalization frontend.
- `/tmp/lnmai-timing-number-independent-audit-1.accepted.json`: 12 local
  modules, 146 theorems for original signed timing parsing and the actual
  extracted whole/fraction loops under decimal/character point laws.

Both admit only `propext`, `Classical.choice` and `Quot.sound`, with no
reachable external-formatting axiom. Their imports overlap, so their counts
must not be added as unique coverage. The previous canonical extraction had
40 kernel files, Generated SHA256
`d037f357eb395803e657d1ca4b445cc73908e6910c59bb5c37af0de3493ab1bf`
and LLBC SHA256
`6751f3cdccc3f32e4c87e650f02da956778ee84a3bab69785ed92cc97dd49125`.
Those historical audits do not by themselves accept the new runtime wiring.

Before this batch, batch26 separately compiled `SlideChainModel`,
`SlideChainExtractedReads`, `TouchHoldRebuildUpsert` and
`TouchHoldRebuildTraversal`; batch27 also compiled `SlideChainExtractedTiming`.
The current tree includes fixes for the explicit batch27 diagnostics in the
remaining parser/initializer roots. Passing compilation must still be followed
by an independent axiom audit. Runtime representations, capacity, allocation,
UTF-8 and numeric primitive contracts remain open.

The 2026-10-17 composition batch passed combined-source elaboration with 76
current source modules, 93 principal axiom reports and a transitive audit of
2744 theorems. Only `propext`, `Classical.choice` and `Quot.sound` occur.
Acceptance checked 185 source/artifact hashes against the installed real
judgment extraction. The immutable snapshots are
`/tmp/lnmai-composition-proof-accepted-v6.json`, `.sources.json`,
`.evidence.json` and `.lean`. No tests or completed independent-module audit
are claimed by this result.

The following traversal batch passed combined-source elaboration: 51 current
source modules, 144 principal reports, 2028 transitively audited theorems and
149 matching hashes. Immutable evidence is in
/tmp/lnmai-traversal-proof-accepted-v23.json and its .sources.json,
.evidence.json and .lean snapshots. All reports use only standard logical
axioms. These counts describe evidence, not percentage coverage.

TapSchedule and HoldSchedule now prove the actual extracted traversals against
the original Lean recursions. TapSchedulePass composes the real ButtonVec
map_accum, preserving inter-zone cursor/frontier/reversed-event state.
HoldSchedule records every immutable source slot exactly once in order.
QueueMaintenance proves replacement of ALL matching identifiers and resolved
prefix normalization, including duplicate IDs and arbitrary out-of-range Nat
cursors. QueueMaintenanceComposition derives complete updates from the real
replacement/normalization loops plus slot/facade point contracts.

The accepted v23 SchedulerFrameTraversals composes judgment-aware Tap, Hold
and Slide phases at each CURRENT frame. Only Touch and TouchHold remain
complete-pass premises; its Hold update_queue contract was still explicit.
The subsequent HoldScheduleQueueJudgments patch derives that contract from
queue point laws and connects it to the frame composition. This patch and the
full current Audit root passed the v24 combined-source check: 90 source modules,
150 principal reports, 3146 transitively audited theorems and 202 matching hashes.
Only standard logical axioms occur. Immutable evidence is preserved in
/tmp/lnmai-traversal-proof-accepted-v24.json and its adjacent snapshots.
Concrete numeric/field/container/facade laws, exact encoding, uniform capacity
and initialization remain explicit; no full runtime or independent-module
acceptance is claimed.

The three shared Tap/Hold/queue kernels and their runtime callers are installed
at checkpoint 2a3333c. Formatting, library-only Cargo checking and real
Charon/Aeneas extraction passed in the corresponding
/tmp/lnmai-scheduler-batch-{format,library-check,extraction}-1.log files.
The extraction is /tmp/lnmai-scheduler-batch-extraction/; Generated also
compiled independently. Tests remain deferred.

`NaturalParser`, `TimingAggregate` and `SlideSchedule` passed combined-source
elaboration and all 57 principal axiom checks. Every report uses only standard
logical axioms; diagnostics and matching source hashes are recorded in
`/tmp/lnmai-natural-schedule-proof-accepted-v8.json` and its `.sources.json`.
All three runtime callers use the extracted kernels, with the synchronized
extraction recorded in `/tmp/lnmai-natural-schedule-extraction-4.log`. Earlier
library-only Cargo, formatting and whitespace checks passed. All 57 are
mandatory in `Verification.Audit` and included in the deferred validation
script, which has not been run. Arithmetic, character/string, closure,
lifecycle/parent and container/capacity contracts remain explicit. A fresh
separate-module audit is still required, including previous accepted batches
against the updated Generated module.

`SlideScheduleParents` adds seven composition theorems that fill the three
parent callbacks using the actual extracted sensor, force, ancestor and refresh
kernels. All 64 principal reports (57 + 7) passed combined-source elaboration
and axiom checks; diagnostics and matching hashes are in
`/tmp/lnmai-natural-schedule-parents-proof-accepted-v9.json` and its `.sources.json`.
The independent-module audit in
`/tmp/lnmai-natural-schedule-independent-audit-1.log` completed fifteen modules
without failure before its driver was held at a module boundary for the new
judgment proofs. The superseded driver has been stopped. The independent
audit of the installed full judgment extraction in
`/tmp/lnmai-judgment-independent-audit-1.log` compiled Generated and Areas before
its driver was stopped at the Bridge module boundary to batch further changes.
Completed artifacts are retained; no completed whole audit is claimed. Runtime facade wiring,
metadata, atomic operations, cloning and capacity remain explicit obligations;
constructing a generic trait is not extraction of that facade.

The new `judgment_adapter` kernel is called by the actual runtime judgment API.
Library-only checking passed in `/tmp/lnmai-judgment-adapter-library-check-1.log`,
and real extraction succeeded in `/tmp/lnmai-judgment-adapter-extraction-1.log`.
All fifteen `JudgmentAdapter` theorems passed combined-source elaboration and
axiom checking, with no errors, `sorryAx` or nonstandard axioms. Acceptance is
recorded in `/tmp/lnmai-judgment-proof-accepted-v3.json`, with 65 matching
source/artifact hashes in `.sources.json` and provenance in `.evidence.json`.
The temporary proof bundle copies all thirteen new declarations verbatim from
the full extraction. Every existing
generated declaration block is identical, allowing only declaration order to
change. The bundle records both model hashes, Rust source hashes, proof hashes
and compiled-import hashes; it is not an independent full-module audit. The
full Generated/LLBC artifacts are now installed for the new independent audit.
Atomic integer laws remain explicit, including successful conversion only
after proving representability. No bound is imposed on decoded input integers.
The concrete Integer/BigInt implementation and runtime trait facade remain open.

The default-value pass found a semantic distinction that must be preserved:
Lean's derived `Inhabited.default` initializes every field with its type's
default, whereas a structure literal with omitted fields uses field
initializers. Rust `Default` now follows the former. In particular,
`JudgeEvent.multiple`, hold/touch group sizes, slide track/multiplicity counts,
autoplay version and source/normalized timing fields default to zero;
checkpoint skippability and normalized-slide body/head flags default to false.
`FrameInput`, `GameState`, `ClickCursor` and `RuntimeRegistry` provide explicit
`with_field_defaults()` constructors for the production paths corresponding
to Lean structure literals. Those paths retain populated zone vectors,
configured game settings and the initial registry handle of one. The remaining
default call sites were checked for record updates and panic fallbacks.

The obsolete `lnmai-core-rs/src/simai.rs` was removed. It was outside the Cargo
targets, had no references, and retained a divergent floating-point parser;
the active implementation is `crates/lnmai-core/src/simai/`.

- The six default theorems compiled in
  `/tmp/lnmai-defaults-slide-proof-1.log`. Its later checkpoint failure was
  repaired; `Verification.SlideArea` and `Verification.SlideAreaTrace` then
  compiled successfully in `/tmp/lnmai-defaults-slide-proof-2.log`.
- Ordered queue traversal now lives in the shared generic `slide_queue`
  kernel. Runtime gameplay and replay delegate to it and attach render
  identifiers to progress values in returned order.
- Fresh queue extraction succeeded
  (`/tmp/lnmai-slide-queue-extraction-1.log`) and library-only
  `cargo check --workspace --lib` passed
  (`/tmp/lnmai-slide-queue-library-check-1.log`).
- `SlideQueueModel` compiled in `/tmp/lnmai-slide-queue-model-2.log`, covering
  the original reference bridge, output bounds and suppressed replay commands.
  `SlideQueue`, `SlideQueueTraversal`, `SlideQueueTrace` and
  `SlideQueueReferenceTrace` passed combined-source Lean elaboration. They
  cover slice/append bounds, extracted recursion, public fuel initialization
  and each intermediate queue and ordered command list over finite frame
  traces. The replay bridge proves that each frame emits no commands.
  Clone purity and container bounds remain explicit hypotheses.
- `VecEquality`, `SlideAreaEquality` and `StorageEquality` also passed that
  combined-source elaboration, including unequal backing-vector lengths and
  independently sized checkpoint histories.
- The combined-source audit in `/tmp/lnmai-proof-lsp-diagnostics.json`
  (version 20) elaborated 39 handwritten modules and audited 1449 theorems
  with only `propext`, `Classical.choice` and `Quot.sound`. This is not the
  separate-module build; theorem counts differ and are not coverage metrics.
- `ShapeAdapters` then passed isolated combined-source elaboration in
  the shape proof session (version 1). Its shutdown cleared the diagnostics
  file; the successful axiom reports were observed before shutdown. All four public
  composition theorems use only those same axioms. They cover checked and
  unchecked constructors and relative endpoints beyond `usize`; they do not
  prove the actual bigint conversion. The theorems are required by the audit.
- The latest successful full axiom audit remains 2284 theorems
  (`/tmp/lnmai-public-axiom-audit-1.log`). It predates the defaults, checkpoint
  and queue additions. The separate-module build in
  `/tmp/lnmai-audit-direct-3.log` was stopped at a module boundary after fresh
  extraction made its dependency plan obsolete. A complete fresh audit is
  still required. Older interrupted or failed builds are not acceptance evidence.
- `IntegerAdapters`, `AccuracyAdapters` and `RuntimeScore` passed
  combined-source elaboration. The six principal axiom reports are preserved
  in `/tmp/lnmai-score-proof-accepted-v2.json`, with source hashes in the
  adjacent `.sources.json`. They cover signed/zero-divisor Euclidean
  division, five accuracy formulas, event deltas, single-event folding,
  ordered event-list folding and every intermediate score state. These are
  mathematical/representation-parametric adapters, not extraction of
  `integer.rs`, `accuracy.rs` or `runtime_score.rs`.
- The generic `grade_counts` kernel is now used by runtime scoring. Its
  extraction passed (`/tmp/lnmai-grade-counts-extraction-1.log`) and
  `cargo check --workspace --lib --locked` passed
  (`/tmp/lnmai-grade-counts-library-check-1.log`). Fresh Generated compiled
  separately in `/tmp/lnmai-generated-grade-counts-1.log`. `GradeCounts`
  passed combined-source elaboration against that fresh module; six principal
  axiom reports and source hashes are preserved in
  `/tmp/lnmai-grade-proof-accepted-v4.json` and its `.sources.json`. Read/write
  proofs quantify over arbitrary array contents and require no Clone law.
  The decoded increment theorem assumes only the element addition law.
  `RuntimeCounts` also passed combined-source elaboration and principal axiom
  checks. Its decoded select/replace and five-table routing/update proofs are
  preserved in `/tmp/lnmai-runtime-counts-proof-accepted-v6.json` and the
  adjacent source-hash manifest. Arithmetic/decoding hypotheses remain.
- The priority source review covered Tap/SlideHead, Hold/TouchHold, Touch,
  Slide/WiFi and scheduler frame completion. It includes timeout before shared
  touch judgment before own input; early touch clicks; Hold head transitions;
  Slide completion/timeout before this frame's sensors; delayed judgments;
  parent readiness, direct-parent force finish and cycle-bounded ancestor
  ending; click ownership and hold cursor normalization; touch-group majority
  and body membership; duplicate-ID first matches and accumulator order. No
  new discrepancy was found in that pass. These are source comparisons, not
  complete state-transition theorems.
- Scheduler source order is Tap, Hold, Slide, Touch, TouchHold for processing;
  Tap, Hold, Touch, TouchHold, Slide for reported/scored judgments; slide cues,
  TouchHold body and judgment sounds for audio; slide commands then judgment
  display for rendering. Both implementations preserve these distinct orders.
- The tokenizer's two character classifiers and bracket-aware top-level
  splitter now run through the shared extracted `tokenize` kernel. Extraction
  succeeded in `/tmp/lnmai-tokenize-extraction-2.log` and the fresh generated
  Lean module compiled in `/tmp/lnmai-generated-tokenize-1.log`. The first
  extraction attempt failed on character switches and is not passing evidence;
  explicit character comparisons removed that unsupported translation.
- Library-only `cargo check --workspace --lib --locked -j 1` passed in
  `/tmp/lnmai-tokenize-library-check-1.log`. This also typechecked the JSON
  zero-mantissa exponent fix: zero no longer converts an arbitrarily large
  power to `usize` before returning zero. No huge-exponent reference execution
  or test was performed.
- `Tokenize` passed combined-source Lean elaboration and all seven requested
  axiom checks. Accepted diagnostics and matching source hashes are preserved
  in `/tmp/lnmai-tokenize-proof-accepted-v3.json` and its `.sources.json`. The
  public splitter proof includes termination, safe indexing/pushes and exact
  outputs for all character sequences of length below `Usize.max`; unmatched
  closing brackets saturate depth at zero, and empty pieces are preserved.
  UTF-8/string conversion and full parsing remain separate obligations. The
  required-theorem audit and deferred validation script include this module.
- Tap/SlideHead, Touch and Hold/TouchHold runtime steps now share extracted
  control flow: `tap_transition` with `TapState`, `touch_transition` and
  `finish_touch_judgment` with `TouchState`, and the Hold head/body selectors
  (`hold_head_action`, `hold_after_head_action`, `hold_body_action`,
  `HoldSubState`, `HoldBodyFacts`) whose priority order encodes the regular
  versus touch-hold head rules and the classic versus modern body rules.
  Library-only `cargo check --workspace --lib --locked` passed
  (`/tmp/lnmai-tap-transition-library-check-1.log`,
  `/tmp/lnmai-touch-transition-library-check-1.log`,
  `/tmp/lnmai-hold-lifecycle-library-check-1.log`) and the kernel re-extracted
  cleanly (`/tmp/lnmai-hold-transition-extraction-1.log`).
- `TapLifecycle`, `TouchLifecycle` and `HoldLifecycle` passed combined-source
  elaboration and all twenty requested axiom checks with only `propext`,
  `Classical.choice` and `Quot.sound`. Accepted diagnostics and the matching
  source-hash manifest are preserved in
  `/tmp/lnmai-lifecycle-proof-accepted-v1.json` and its `.sources.json`.
  Tap covers every state branch including restored judged states and
  nonmonotone frame times, the star-head variant, and intermediate states and
  events over arbitrary finite frame lists. Touch covers
  timeout-before-shared-before-local precedence, early input without an event,
  and the extracted grade conversion. Hold covers head priority, the after-head
  body sampling that can replace head feedback with a tail event, classic and
  modern body transitions, release grace accumulation, and every terminal path
  over arbitrary frame lists. The exact-time arithmetic, raw judgment tables
  and event constructors remain adapter contracts; unbounded counter and bigint
  representations stay outside the kernel.
- Scheduler click ownership now runs through the extracted `scheduler` kernel
  (`consume_click`, `tap_queue_has_unused_click`, `touch_input_available`):
  the runtime supplies its `used < available` comparisons and performs the
  counter updates, while the kernel fixes button-before-sensor precedence, the
  suppression of the sensor fallback after a consumed button, and the absence
  of consumption when input is not accepted. `SchedulerClicks` passed isolated
  module compilation (`/tmp/lnmai-scheduler-clicks-build-4.log`); its three
  theorems relate the runtime composition to the reference
  `consumeButtonThenFallbackSensor`, `tapFamilyQueueHasUnusedClick` and
  `hasUnusedTouchInputAt`.
- The separate-module audit in `/tmp/lnmai-audit-direct-4.log` was stopped at
  a module boundary before tokenizer re-extraction. It recorded successful
  completion through `Judge`, not a complete audit. A fresh full audit remains
  required after the generated code and proof imports stabilize.

- Slide/WiFi lifecycle control now runs through the extracted generic
  `slide_lifecycle` state, transition, active-action, grade-conversion and
  output-flag kernels. Runtime durations stay unbounded. `SlideLifecycle`
  passed combined-source elaboration and all thirteen requested axiom checks,
  using only `propext` and `Quot.sound`. Diagnostics and matching source hashes
  are preserved in `/tmp/lnmai-slide-lifecycle-proof-accepted-v2.json` and its
  `.sources.json`. The proof covers restored states, checkability latching,
  completion before timeout before sampling, nonfinal connected parts, delayed
  judgments, audio/render gating, and every intermediate step of finite traces.
  It composes the extracted kernel with the reference exact-time, raw judgment,
  queue-snapshot, event and rendering adapters; those concrete runtime adapters
  still require refinement proofs. It is not a proof of the full runtime.

- The slide command parser, scalar tables, parser/shape-solver composition and
  scheduler frame driver passed combined-source elaboration together with
  SlideLifecycle. All 50 requested axiom reports passed with only standard
  logical axioms; no errors were reported. Accepted diagnostics and matching
  source hashes are in `/tmp/lnmai-parser-scheduler-proof-accepted-v16.json` and
  its adjacent `.sources.json`. The 37 added principal theorems are required by
  the audit and their modules are registered in the deferred validation script.
  The parser covers command precedence, endpoint indices, error order, all
  scalar table keys (including unknown keys) and both timing modes. The solver
  composition calls the extracted shape solver instead of assuming its
  whole-function equivalence. String observations, exact rational construction
  and concrete runtime representations remain explicit boundaries.
- `SchedulerFrame` proves the extracted driver's pass order, event/command batch
  order, and its conditional composition with the original `stepFrame`. Its
  touch-group loop proves first-match selection, safe increment/index bounds,
  append when absent, the old majority flag and preservation of later duplicate
  IDs. Runtime pass bodies, point-read/write/append adapters and initialization
  and finalization remain open; these results do not prove the entire scheduler.

- `SlideParserFrontend` passed combined-source elaboration with the preceding
  parser/lifecycle/scheduler batch. All 74 principal axiom reports passed,
  including its 24 new theorems; three reports have no axiom dependencies and
  the others use only the standard logical axioms. Accepted diagnostics and
  source hashes are in `/tmp/lnmai-frontend-proof-accepted-v14.json` and the
  adjacent `.sources.json`. This covers arbitrary character inputs, digit and
  lane/area reads, index bounds, single/double character scan loops, just-type
  priority, complete diagnostics and fallible-operation order. Composition
  discharges whole-function reader/just-type/shape-solver assumptions. UTF-8
  decoding, decimal formatting, allocation and exact rational construction
  remain explicit obligations. Sanitization/command observations were
  subsequently discharged by `SlideParserText`, described below. The new
  theorems are registered in the audit and deferred validation script; no
  runtime tests were run.

- `SlideParserText` passed combined-source elaboration and all 18 principal
  axiom checks with only standard logical axioms. The accepted snapshot and
  source hashes are `/tmp/lnmai-parser-text-proof-accepted-v1.json` and its
  adjacent `.sources.json`. It proves the actual thirteen command scans and
  character-filter loop, their bounds/termination, and composition with the
  body, terminal, note, shape and just-type parsers. UTF-8 decoding, String
  encoding, diagnostics and numeric representation obligations remain; this
  is not a whole-runtime proof. The public shape parser now delegates to the
  same command/read kernel as the other slide-parser entry points.
- Library-only `cargo check --workspace --lib --locked -j 1` passed in
  `/tmp/lnmai-parser-metrics-library-check-1.log`, including the new timing,
  ordered note-collection and queue-length kernels. The combined extraction
  also succeeded in `/tmp/lnmai-parser-metrics-extraction-1.log`; the generated
  artifacts match the kernel sources, with no additional opaque declarations
  beyond the existing five formatter axioms. Typechecking and extraction alone
  are not equivalence acceptance.
- The Text, parent-chain, timing-route, collection and queue-metric proofs
  subsequently passed combined-source elaboration against this extraction.
  All 68 principal reports use only standard logical axioms; diagnostics and
  source hashes are in `/tmp/lnmai-priority-proof-accepted-v6.json` and its
  adjacent `.sources.json`. The 50 new principal theorems are now required by
  `Verification.Audit` and their modules are in the deferred validation script.
  A fresh separate-module audit of the changed dependency tree remains open.
- `SchedulerParents` proves the actual extracted first-parent lookup, all-ID
  updates, snapshot flag refresh, sensor-head scan, direct-parent completion
  and bounded ancestor traversal. Cycles and duplicate IDs need no uniqueness
  or acyclicity premise; an updated ended parent stops further traversal.
  Point reads/writes, Natural equality, queue/container representation, cloning
  and command allocation remain explicit contracts. Whole-pass scheduling is
  not assumed or proved by these parent-loop theorems.
- `TimingParser` proves nested-bracket scanning against the original reference
  and every duration-field route, including strict empty-field checks and
  safe indices. Natural scanning and aggregation have the later `NaturalParser`
  and `TimingAggregate` proofs; per-piece numeric/Rat parsing, string splitting
  and time quantization laws remain open. `SlideQueueMetrics` proves maximum queue length
  and all-empty scanning for arbitrary payloads, with no Clone hypothesis.
  Runtime container decoding still needs composition with its callers.
- `SlideParserSequence` proves the runtime-used indexed collection loop from
  reachable successful prefixes, preserving result order and returning the
  first error without assumptions on unreachable callbacks. It composes the
  existing note parser with the original timing-point parser. The actual
  closure, String/record/error representation and allocation remain boundaries.
- The subsequently accepted shared kernels are `natural_parser`, `timing_aggregate` and
  `slide_schedule`, now called by the actual runtime. Library-only checks
  passed in `/tmp/lnmai-natural-schedule-library-check-3.log`; unified extraction
  succeeded in `/tmp/lnmai-natural-schedule-extraction-3.log` with only the
  existing five formatter axioms. The natural scanner uses explicit digit
  comparisons because the installed extractor rejects char-valued switches.
  The 57 principal theorems now passed combined-source elaboration and axiom
  checks against the installed original String parser, timing definitions and
  scheduler fuel recursion. The final synchronized extraction is recorded in
  `/tmp/lnmai-natural-schedule-extraction-4.log`, with acceptance diagnostics
  and hashes in `/tmp/lnmai-natural-schedule-proof-accepted-v8.json`. The
  previous accepted snapshots remain historical evidence for their source
  hashes, not acceptance of this new Generated.lean.
  Parent-kernel composition added seven reports for a total of 64, recorded
  above. The next composition connects this traversal to the Slide frame phase;
  other passes, initialization/finalization and actual facade contracts remain.

Apart from the explicitly authorized three-song checkpoint, do not start
deferred acceptance until the implementation-equivalence checklist is complete.
Do not report whole-program formal equivalence while the open
representation, parser, lifecycle, scheduler, seek, JSON or ABI obligations
remain.
