# Equivalence theorem contracts

The authoritative contracts are the theorem statements in `Verification/`.
This document explains their scope; it is not an alternate executable model.

## Representation

Constructor bridges for finite enums are bijections. A machine duration maps
to `Duration.fromMicros d.micros.val`; machine naturals map to `.val`. These
embeddings do not represent every Lean integer/natural. The runtime uses
separate arbitrary-precision types whose full refinement proof remains open.

## Arithmetic

- `abs_eq`/`abs_spec`: exclude `i64::MIN`, whose absolute value is not an i64.
- Addition/subtraction/scaling: mathematical result must lie between i64 min
  and max.
- `div_nat_spec`/`div_nat_eq`: exact Euclidean quotient, including negative
  dividends; divisor zero returns zero. The stated overflow guard is trivial
  for the nonnegative u32 divisor.
- Rotations: all machine-size step counts; reduce steps modulo eight before
  addition. Ring indices lie in 1–8.
  `AreasPublic` additionally composes these theorems with arbitrary Lean
  naturals when the machine argument equals the exact residue modulo eight.
  Index adapters require either exact representation or proof that the
  natural index is at least eight. The bigint-to-residue implementation is
  still a runtime refinement obligation.

## Public finite interfaces and shapes

`AreasPublic` and `TypesPublic` cover extracted defaults, enumeration arrays,
clones, equality and the exported order operations, including `RuntimePos`,
`JudgeEventKind` and `JudgePhase`. They do not cover JSON or debug formatting.

`Shape` and `ShapeSolver` cover canonical-shape constructors, equality, digit
parsing, endpoint/mirror formulas and solving for the extracted argument
types. Runtime unbounded endpoint conversion and parser/table composition
remain separate obligations.

## Slide checkpoints

The runtime aliases the extracted `lifecycle::SlideArea<P>` with its exact
natural-number progress type. `SlideHistory` relates indexed target histories
to the original Lean recursion; `SlideAreaLoops` and `SlideArea` state the loop
and public-checkpoint contracts. Their current build/audit status is recorded
in `SOURCE_AUDIT.md`; the contracts alone do not establish a passing proof.

The statements quantify arbitrary target, held-sensor and history vectors.
Missing held/history entries are false. Completion truncates histories to
their common length, including AND over an empty zip. Legacy on/off flags seed
their respective empty histories independently, and only for a single target.
The `check` aggregate additionally requires nonempty targets for AND completion.

Progress values are opaque to these operations. The default theorem requires
the supplied default to represent zero; cloning/checking require a pure clone
that preserves the value. `SlideAreaEquality` additionally requires progress
comparison to agree with equality of its natural-number representation. It
compares every checkpoint field, including the full, independently sized
history vectors. These conditions do not establish the bigint
representation refinement. `SlideAreaTrace` composes the actual extracted
update over arbitrary finite sensor-input lists and relates both final state
and public observations to the reference fold. This covers repeated calls to
one checkpoint. The runtime now delegates queue traversal to the extracted
`slide_queue` module. `SlideQueueModel` factors render routing out of the Lean
reference, while `SlideQueue` and `SlideQueueTraversal` carry the extracted
recursion obligations. These new queue proofs are still being compiled.
The fuel-bounded statement permits every machine fuel value; the public
length-based wrapper additionally needs queue length strictly below
`Usize.max`. Render routing must retain command order and suppress all
commands when replay disables emission. Parent gating, note transitions and
scheduler composition remain separate obligations.

## Container equality

`VecEquality` relates the extracted Vec comparison to equality of lists of
represented elements. It requires a pure element comparator that returns the
correct represented equality. Unequal vector lengths compare false.
`StorageEquality` applies this result to both storage wrappers, preserving all
backing entries even when their length differs from the physical zone count.
These new proofs are pending compilation; `SOURCE_AUDIT.md` records results.

## Judgments

- Tap, touch and classic slide: all representable diffs except i64 min.
- Modern slide: diff excludes i64 min; stay time lies in `[-2^40, 2^40]`.
- Classic Hold end: timing, release and length lie in `[-2^40, 2^40]`.
- Modern Hold end: length, ignore and release lie in `[-2^38, 2^38]`; the diff
  is unrestricted because only its sign is inspected. The finite grade-table
  theorem covers all grades, both timing signs and every u32 band.
- Slide timeout: the adjusted deadline must fit i64.

The runtime adapters preserve unbounded inputs and use shared finite tables.
The above bounded theorems do not by themselves prove those adapters.

## Score

`score_non_break_equiv` and `update_combo_equiv` state explicit no-overflow
conditions on products, sums and DX loss. `score_break_equiv` requires
`2500 * multiple < 2^32`. `count_fast_late_equiv` covers all grades, display
modes and representable diffs including i64 min. `dx_score_rank_equiv` covers
all u32 achieved/max values by widening intermediate products to u64.

Runtime score and accuracy now use exact naturals/rationals. There is no
remaining float/rational conversion to justify, but their actual Rust
implementations still need a refinement proof.

## Trust and validation

`Verification.Audit` traverses theorem dependencies and rejects every axiom
except `propext`, `Classical.choice` and `Quot.sound`. The extraction pipeline
and the Lean kernel remain part of the verification trust boundary. Imported
upstream `sorry` declarations must not be reachable from our theorems.

Parent-package `native_decide` regressions and Rust/Lean differential tests
are execution-based checks. They do not replace universal theorems for the
parser, runtime, state restoration or FFI. See `SPEC_MAPPING.md` for every
remaining runtime obligation.
