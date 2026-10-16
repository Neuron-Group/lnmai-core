# Remaining work toward complete Rust–Lean equivalence

The complete runtime pipeline is implemented, but whole-program equivalence
has not been proved. `../verification/SPEC_MAPPING.md` identifies the current
boundary. Do not mark this task complete solely because a corpus or test
suite passes.

`../verification/SOURCE_AUDIT.md` records module coverage, concrete source
findings, current proof builds and remaining gaps.

## Current work order

Finish the module-by-module implementation alignment and equivalence proofs
before running tests, as requested by the user. Rust extraction and Lean
equivalence-proof compilation belong to this first phase. Unit, differential,
real-chart and ABI acceptance runs are deferred until that phase is complete.
Prioritize parser, complete lifecycles, scheduler and judgments. Defer further
count/combo-display proofs until those priority paths are covered.
The interrupted Koishi diagnostic is not a passing acceptance result.
The deferred corpus includes every difficulty from `011996 Usagi Flap`,
`011951 サイエンス` and `蛙石` (10 additional cases, 48 total). Both frontend
comparison and complete runtime playthroughs use this same corpus. Registration
is complete; these newly added cases have not been executed. The Lean
real-chart acceptance also includes all ten, retaining AP and no-missing-note
requirements.

## Required acceptance for each kernel/runtime change

- Run `tools/validate_all.sh` from the parent checkout. It checks formatting,
  Clippy, debug/release tests, extraction freshness, Lean regressions and real
  charts, equivalence proofs with axiom audit, all differential tests and C ABI.
- Keep LLBC and generated Lean synchronized. Never edit generated Lean or add
  `sorry`/custom axioms to make a claimed equivalence theorem pass.
- Record whether evidence is a universal theorem, a bounded theorem or a
  finite executable comparison. Preserve all theorem preconditions.

## Formal work still required

1. Relate the actual `Integer`/`Natural`/`Rat` runtime representations and
   operations to Lean arithmetic, including promotion/normalization, division
   by zero, Euclidean division and saturating natural subtraction.
2. Prove exact-time adapters, threshold clamping and runtime score/accuracy
   implementations. Shared finite tables alone do not prove the adapters.
3. Extend extraction/verification to Simai parsing, shape tables, timing,
   normalization, lowering, validation and error/JSON representation.
4. Prove chart loading, storage/map order, input state and every note lifecycle
   transition. Cover Hold head/tail events, EX grades, parent/dependent slides,
   touch groups, score/audio/render event order and all settings.
5. Prove scheduler/frame composition, default tactic generation, seek/replay
   and restoration for arbitrary traces.
6. Prove JSON/session/FFI semantics and ownership/ABI obligations. The native
   protocol's modulo-64-bit representation must match Lean separately from
   unbounded gameplay arithmetic.

## Kernel additions in this working revision

`Hold`, `HoldModern`, `Rotations`, `Time`, `Constants` and `ScoreExtra` contain
new obligations for the formerly missing pure functions. Their completion
requires a passing fresh proof build and axiom audit, not just the presence of
source files. Modern-slide arithmetic now uses Euclidean division; negative
wait times are included in the stated bounded range.

Runtime storage and slide symmetry now re-export their extracted implementations.
`Storage*` proofs cover arbitrary vector contents and require value-preserving
cloning and pure callback specifications; `Symmetry` covers every machine
rotation argument, including noncanonical representatives modulo eight.

Runtime slide checkpoints now also use the extracted `lifecycle::SlideArea<P>`
with arbitrary-precision progress values as the opaque parameter.
`SlideHistory`, `SlideAreaLoops`, `SlideArea` and `SlideAreaTrace` compiled
successfully, covering malformed histories, independent one-target legacy
seeding, defaults, cloning, updates, observations and finite input traces.
The runtime now also delegates ordered queue traversal to the extracted
`slide_queue` module. That extraction and library-only typecheck succeeded;
the queue proofs passed combined-source elaboration, while a fresh
separate-module axiom audit is still pending.
This does not cover the scheduler that selects checkpoint updates.

The parser now uses the extracted character classifiers and bracket-aware
top-level splitter. `Tokenize` passed combined-source elaboration and axiom
checks, including loop termination, bounds and complete output equivalence
for character sequences with length below `Usize.max`. UTF-8/string conversion
and the rest of parsing/normalization/lowering remain open.

Tap and SlideHead now use the shared extracted `TapState` and `tap_transition`.
Their fresh extraction and library-only typecheck passed. `TapLifecycle`
single-step and finite-frame composition proofs are pending Lean acceptance;
actual unbounded time/judgment/event adapters remain explicit boundaries.

`SlideQueueModel` has compiled separately. Extracted traversal, finite-frame
composition and `SlideQueueReferenceTrace` now elaborate together, including
every intermediate reference queue, ordered command list and command-free
replay. The new `VecEquality`, `SlideAreaEquality` and `StorageEquality`
proofs passed the same combined-source check. Clone/container hypotheses
remain; this does not replace the fresh separate-module audit.

The default-value source pass corrected the difference between Lean's derived
`Inhabited.default` and structure field initializers. Rust `Default` now
matches the former; `with_field_defaults()` preserves the latter for input,
game state, click cursors and the global registry. The corrected kernel was
re-extracted and the runtime library typechecked; tests remain deferred.
The unused floating-point parser at `src/simai.rs` was removed; all Cargo
targets use the exact parser in `crates/lnmai-core/src/simai/`.

`Shape` and `ShapeSolver` cover the shared canonical-shape solver.
`AreasPublic` and `TypesPublic` add public defaults, arrays, clones, equality
and order. Unbounded area adapter composition remains conditional on the
actual runtime producing the exact natural residue/index.
`ShapeAdapters` also passed combined-source elaboration and individual axiom
checks for four unbounded-endpoint composition theorems. They explicitly
assume the runtime conversion to `usize` preserves representable values and
fails only above its maximum. Actual bigint conversion remains unproved.

`IntegerAdapters`, `AccuracyAdapters` and `RuntimeScore` passed combined-source
elaboration and principal-theorem axiom checks. They establish the decoded
Euclidean-division algorithm, five rational accuracy formulas and ordered
event-fold/trace composition under explicit representation laws. Actual
bigint/rational operations and unextracted runtime callers remain open.

Runtime grade indexing, enumeration and fixed-array reads/writes now delegate
to the extracted `grade_counts` kernel. Fresh extraction, library-only Rust
typechecking and the Generated module compilation passed. `GradeCounts`
passed combined-source elaboration and axiom checks for arbitrary contents;
its read/write theorems need no Clone law, and decoded increments assume
the addition law. `RuntimeCounts` five-table routing/update composition also
passed combined-source elaboration and principal axiom checks, retaining its
explicit arithmetic/decoding laws.
The previous full proof-build driver was stopped at a module boundary after
fresh extraction invalidated its plan; a fresh full audit is still required.

Runtime JSON decoding now includes Syntax/IR, autoplay, `FrameInput`, public
FFI results and event/position types. The session registry and native output
commit order are aligned with Lean in source; serialization and session/ABI
proof obligations remain open.
The source inventory covers 75 derived encoders, 71 derived decoders, 26
concrete custom instances and 50 structure field sequences. Numeric parsing
now accepts Lean-style digit separators, and runtime time constants share
the kernel values. Combo-state classification uses judgment counts, including
the stricter Break requirement for AP+. These remain source/implementation
evidence until their representation and composition proofs are discharged.
