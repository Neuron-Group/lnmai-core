# Connected-Slide Gap: 3-Part Chain

This document records a remaining semantic gap between `lnmai-core` and the reference implementation in `../reference/MajdataPlay`.

The gap is **not** about the already-fixed 2-part connected-slide timing bug. That bug is resolved and covered by compile-checked regressions.

The remaining gap appears on a **minimal 3-part connected slide chain**.

## Status

- Verified aligned:
  - 2-part same-head connected-slide pair lowers and replays correctly
  - real chart `11358_インドア系ならトラックメイカー` level 5 reaches AP
  - real chart `834_PANDORA PARADOXXX` level 6 reaches AP
- Still uncovered:
  - minimal 3-part connected-slide chain does not fully resolve in `lnmai-core`

## Minimal reproduction

Use this lowered chart:

```lean
simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1]*<7[4:1],\n"
```

This lowers to three connected slide parts:

- note `1`: group head
- note `2`: middle connected part
- note `3`: group end

Observed lowered timing shape:

- note `1`: `startTiming = 500000`, `judgeAt = 1000000`
- note `2`: `startTiming = 1000000`, `judgeAt = 1500000`
- note `3`: `startTiming = 1500000`, `judgeAt = 2000000`

This timing shape is structurally consistent with the current connected-slide lowering rule in `LnmaiCore/Simai/Normalize.lean`.

## Expected behavior

Based on MajdataPlay’s connected-slide semantics, we expect:

- child parts become checkable when the direct parent is finished or pending finish
- only the group-end connected part is judged
- a valid default replay for this reduced chain should eventually produce one judged slide event for note `3`

The reference behaviors that motivate this expectation are:

- child start timing is derived from parent end timing in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:238`
- only end parts are judgable in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:253`
- parent-finish propagation happens from sensor progress in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:484`
- pre-update and update ordering is `SlideCheck()` before `SensorCheck()` in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:301` and `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteBehaviours/SlideDrop.cs:310`

## Actual behavior in `lnmai-core`

For the reduced 3-part chain under `defaultTacticFromChart`:

- `missingJudgedNoteIndices = [3]`
- `result.events = []`
- `achievesAP = false`

Final runtime state observed during investigation:

- note `1`: queue empty, still `Active`
- note `2`: queue empty, still `Active`
- note `3`: queue not progressed, still `Active`, `checkable = false`

That means the last part never becomes effectively runnable under the current replay/runtime interaction, even though the earlier parts have already progressed.

## Important contrast with the verified 2-part case

This 2-part grouped case is already compile-checked and passes:

```lean
simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1],\n"
```

Observed result there:

- `missingJudgedNoteIndices = []`
- one judged event only
- judged note index is `2`
- grade is `Perfect`
- `achievesAP = true`

So the remaining gap is **not** “connected slides are broken in general”. It is more specifically about **longer connected chains**, at least for the 3-part micro-case above.

## Why this matters

This gap means we should not yet claim full MajdataPlay parity for all connected-slide chains, even though the real-chart targets used so far now pass.

In particular:

- a compile-checked proof for 3-part connected-chain end judgment is not yet justified
- the current real-chart coverage is strong, but not exhaustive for connected-slide topology

## Current likely hypotheses

These are ordered by investigation value, not certainty.

### 1. Parent linkage for deeper chains may still differ from the reference

Current lowering tracks:

- `parentNoteIndex`
- inherited `startTiming`
- shifted `judgeAt`

But for chains longer than 2, we need to verify whether the runtime expects the last child to reference the **immediate previous part** or whether some other relation is accidentally preserved or flattened incorrectly.

In the observed lowered 3-part case, note `3` currently has `parentNoteIndex = some 1` instead of the immediate previous note `2`.

That is suspicious.

If MajdataPlay’s `Parent` is always the immediately previous `SlideDrop`, then our longer-chain parent assignment is likely still wrong.

Reference loader context:

- `parent = result.SlideInstance` is updated on every created sub-slide in `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs:1153`

That suggests the next child should link to the immediately previous part, not necessarily to the original head.

### 2. Parent-finish propagation may be correct for depth 1 but insufficient for depth > 1

Our runtime tests already cover:

- direct parent `pending finish`
- direct parent `finished`
- direct parent force-finish

But they do not yet cover a concrete 3-part chain where:

- middle part progresses
- last part must then become checkable
- last part then judges at group end only

If the chain parent pointer is wrong, this symptom follows naturally.

### 3. Default replay generation may need chain-aware coordination

This is less likely than hypothesis 1 because the 2-part grouped case already succeeds under the default tactic.

Still, if deeper chains need slightly different hold timing overlap than our current “independent per-slide timing skeleton” replay generates, then tactic synthesis could also contribute.

At the moment, the stronger suspicion remains the parent topology for deeper conn groups.

## Evidence pointing at parent-topology mismatch

The strongest concrete signal found during investigation is:

- in the reduced 3-part lowered chart, note `3` points to `parentNoteIndex = some 1`

If the reference runtime builds the chain as:

- note `2` parent = note `1`
- note `3` parent = note `2`

then our current 3-part lowering still disagrees with MajdataPlay.

That would directly explain why note `3` never becomes checkable in the reduced reproduction.

## What is already proved today

These compile-checked results already exist:

- connected child timing inherits parent end in `LnmaiCore/Simai/Tests.lean`
- same-head 2-part connected pair APs in `LnmaiCore/RuntimeTests.lean`
- non-end connected parts do not judge in `LnmaiCore/RuntimeTests.lean`
- parent pending-finish / finished semantics are covered in `LnmaiCore/RuntimeTests.lean`

So the documentation here is about the **remaining uncovered gap**, not about the already-fixed connected-slide timing bug.

## Recommended next steps

1. Inspect MajdataPlay parent topology for 3-part groups directly.
   - Confirm whether the third part’s `Parent` is the second part.
   - Relevant file: `../reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteLoader.cs:1144`

2. Add a compile-checked lowering regression for 3-part parent linkage.
   - Assert exact `parentNoteIndex` chain on the reduced chart.

3. If needed, fix `applyConnectedSlideMetadata` in `LnmaiCore/Simai/Normalize.lean`.
   - The likely required behavior is immediate-parent chaining across the whole group.

4. After that, add a compile-checked runtime regression.
   - Reduced 3-part chain should emit exactly one judged slide event for the final note.

5. Re-run real-chart verification after the chain fix.
   - Even if current real charts already pass, this guards against accidental regression.

## Non-goals of this document

This document does not claim:

- that MajdataPlay and `lnmai-core` are globally equivalent
- that the 3-part chain bug is definitely in lowering rather than tactic generation
- that all remaining gaps are now known

It records one concrete, reproducible, reference-motivated gap so future work can proceed from evidence instead of guesswork.
