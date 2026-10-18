# Remaining equivalence work

Status of the Rust rewrite versus the Lean reference (`LnmaiCore/`). The
checked items are proven and audited; the open items are the gap that still
prevents a claim of full equivalence.

## Current checkpoint

- 8000 audited theorems (`Verification.Audit`), only `propext`,
  `Classical.choice` and `Quot.sound`. Independent audit accepted over the
  213-module closure with byte-exact source hashes.
- Full differential suite green at the previous checkpoint
  (`22 passed; 0 failed`, 232 playthroughs, 2,010,664 compared frames).
- The extraction covers `lnmai-core-verify`; anything that crate does not call
  is outside the Lean model.

## 1. Numbers

The runtime numbers were `num-bigint`/`num-rational` wrappers, so score, tick
and accuracy arithmetic had no Lean model at all.

- [x] Extracted limb kernels in `lnmai-core-verify`:
      `big_nat` (limb vectors, total subtraction/division, decimal digits),
      `big_int` (sign plus magnitude, Euclidean and truncated division),
      `big_rat` (lowest terms by gcd, cross-product comparison, twelve-digit
      decimal expansion).
- [x] Runtime `Natural`, `Integer` and `Rat` delegate to those kernels; score
      state, grade counts, chart totals, timeline rounding and the FFI
      encoders carry `Natural`/`Integer`.
- [ ] Regenerate the extraction so the kernels appear in the Aeneas output;
      install the new blocks as their own partition module, extend
      `extraction-manifest.json`, and keep the unchanged core blocks
      byte-identical.
- [ ] Lean model and per-operation refinement proofs:
  - [ ] `big_nat`: `normalize`, `compare`, `equal`, `add`, `sub`, `mul`,
        `mul_add_small`, `divmod_small`, `divmod`, `div`, `rem`, `low_u64`,
        `to_usize`, `gcd`, `to_decimal_digits`, `bit_length`, `bit_at`,
        `shl_one`, `succ`, `add_u64`
  - [ ] `big_int`: additive and multiplicative sign laws, `div_trunc`,
        `rem_trunc`, `div_euclid`, `rem_euclid`, `low_u64`, `to_i64`, `gcd`
  - [ ] `big_rat`: `reduce` normalization, `add`, `sub`, `mul`, `div`,
        `compare`, `whole_digits`, `fraction_digits` against Lean `Rat`
- [ ] Prove the runtime `Integer` inline i64 path is a refinement of the
      kernel path: `checked_add`/`checked_sub`/`checked_mul`/`checked_neg`
      agree with the kernel exactly when they return `Some`.
- [ ] Prove the boundary shims preserve value: `From<BigUint> for Natural`,
      `From<BigInt> for Integer`, `to_biguint`/`to_bigint`, so JSON decoding
      and encoding agree with Lean on the same spelling.

## 2. End-to-end parser

- [ ] UTF-8 decoding, allocation and prefix stripping modeled explicitly
      rather than treated as adapters.
- [ ] Single lowering theorem from raw chart text to `ChartSpec`.

## 3. Scheduler and judgment composition

- [ ] Compose the per-phase frame theorems into one arbitrary-trace theorem
      from initial state plus any input frame sequence to the final state.
- [ ] Cover the still-open lifecycle branches with arbitrary, not just finite,
      inputs.

## 4. Boundaries

- [ ] Seek/restore equivalence over arbitrary persisted states.
- [ ] JSON decode/encode (`lean_json`, `json_decode`, `json_encode`) including
      the arbitrary-precision number spellings still handled by num-bigint.
- [ ] FFI/ABI payload equality and the native word truncation rules.

## 5. Verification hygiene

Every step above ends with: full proof build, `Verification.Audit`,
independent audit, unit tests, the three requested charts
(`011996 Usagi Flap`, `011951 サイエンス`, `蛙石`) and then the full
differential suite (about 2.5 hours).
