# Source equivalence audit

This is the working record for the Rust rewrite, not a whole-program proof.
The reference is the current `LnmaiCore/` tree on `rust`, including the
`main` merge at `9b6bffe`. The uncommitted rewrite is part of the audit target.

## Work order and evidence

The requested order is implementation/source equivalence first, tests second.
No unit, differential, real-chart or ABI tests have been run in the current
equivalence pass. Library-only Rust typechecking, extraction and Lean theorem
compilation are permitted in this pass. The interrupted Koishi diagnostic is
not a passing acceptance run.
The deferred differential corpus now contains 48 difficulties from 14 songs,
including `011996 Usagi Flap` (levels 2–5), `011951 サイエンス` (levels 2–5)
and `蛙石` (levels 5–6). The same registration feeds frontend and complete
playthrough comparisons. All ten also appear in Lean's AP/no-missing-note
acceptance checkpoints. This is source registration only, not a test result.

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
- Tap and SlideHead runtime steps now share the extracted `tap_transition`
  control flow and `TapState`. Library-only typechecking passed in
  `/tmp/lnmai-tap-transition-library-check-1.log`; extraction passed in
  `/tmp/lnmai-tap-transition-extraction-1.log`. `TapLifecycle` contains a
  pending proof of single-step and finite-frame event/state composition under
  the exact-time/judgment/event adapter interpretation. It is not yet accepted.
- The separate-module audit in `/tmp/lnmai-audit-direct-4.log` was stopped at
  a module boundary before tokenizer re-extraction. It recorded successful
  completion through `Judge`, not a complete audit. A fresh full audit remains
  required after the generated code and proof imports stabilize.

Do not start deferred acceptance until the implementation-equivalence checklist
is complete. Do not report whole-program formal equivalence while the open
representation, parser, lifecycle, scheduler, seek, JSON or ABI obligations
remain.
