# Real Chart Verification

This document extracts the current real-chart verification snapshots used during semantic-parity refactors.

## Standard verification path

For each chart snapshot below, use:

- `Simai.compileLowered content <level>`
- `defaultTacticFromChart`
- `simulateChartSpecWithTactic`

The current intent is not to claim full MajdataPlay parity for all charts, but to keep concrete replay checkpoints visible while refactors proceed.

Current executable entrypoint:

- `lake exe real-chart-verification`

## Verification snapshot — `11358_インドア系ならトラックメイカー`

### Asset

- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt`

### Level

- `2`

### Current replay status

- total chart notes: `108`
- judged events: `108`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = true`

### Current grade summary

- `Perfect`: `108`

### Interpretation

- the earlier missing-note failure is gone
- the earlier short-hold and slide-quality gaps are also resolved for this checkpoint
- this chart currently serves as a clean regression checkpoint for hold and slide parity work

## Verification snapshot — `834_PANDORA PARADOXXX`

### Asset

- `tools/assets/834_PANDORA PARADOXXX/maidata.txt`

### Level

- `2`

### Current replay status

- total chart notes: `292`
- judged events: `292`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = true`

### Current grade summary

- `Perfect`: `292`

### Interpretation

- this chart currently replays cleanly with no missing judged notes and no non-perfect grades
- it remains useful as a dense overlap / slide-family checkpoint because of its crowded slide content

## Reference-verified reductions now tracked alongside the real charts

The real-chart checkpoints above are now backed by reduced reference-style checks for:

- slide area-skip law
- modern hold missed-head fallback release semantics
- touch-hold released-body recovery
- classic hold strict boundary behavior
- touch-hold strict-majority shared-head behavior
- wifi / conn-slide max-remaining overlap semantics
- overlapping slides sharing one held sensor

## Commands

- `lake build LnmaiCore`
- `lake exe real-chart-verification`
- use a small `lake env lean --run` script when deeper note-level tracing is needed

## Rule

When adding a new real-chart checkpoint:

- record the exact asset path
- record the exact level
- record note count, judged count, and missing-note status
- summarize current grades
- state plainly whether the checkpoint is still failing or already clean
