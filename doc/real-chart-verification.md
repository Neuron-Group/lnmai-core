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
- focused runners:
- `lake exe real-chart-11358`
- `lake exe real-chart-pandora`

## Verification snapshot — `小石DISCO`

### Asset

- `../assets/小石DISCO/maidata.txt`

### Level

- `5`

### Current replay status

- total chart notes: `1119`
- judged events: `1119`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = false`

### Current grade summary

- `Perfect`: `1110`
- `LatePerfect2nd`: `2`
- `LateGood`: `4`
- `LateGreat2nd`: `3`

### Resolved bug — far-future hold head consumed matching `A`-sensor input too early

This was a real runtime gap against `MajdataPlay`, not chart unfairness.

Confirmed `小石DISCO` probe facts:

- touch note `54` on `A5` had a same-frame `sensorClick A5`
- it still stayed `Judgeable`
- the consumed click was traced to hold note `73` on `K5`
- note `73` was still about `5.64s` early and remained `HeadWaiting`

Reference comparison:

- `MajdataPlay` regular hold heads only try button / fallback-sensor clicks while `JudgableRange.InRange(ThisFrameSec)`
- `MajdataPlay` touch-hold heads do the same with the wider touch-style late range
- our runtime previously gated hold-head input only by queue-head/frontier status, so far-future heads could steal clicks

Fix status:

- regular hold heads now require their reference-style head judgeable range before they can consume button or fallback-sensor clicks
- touch-hold heads now require their reference-style touch-hold judgeable range before they can consume button/sensor clicks
- focused regressions now cover both cases
- the `A5` probe now shows note `54` judging `Perfect` at `14280000`

### Fixed bug — K4 same-time shared-queue order mismatch

This was a confirmed runtime/proof replay gap, not a vague replay-quality issue.

Affected chart tokens:

- `4h[16:1]` at `assets/小石DISCO/maidata.txt:12` → runtime note `43`
- `4h[16:17]` at `assets/小石DISCO/maidata.txt:12` → runtime note `44`

Previous trace facts:

- both notes are K4 holds with semantic/input time `11880000`
- the default tactic emits two K4 clicks at `11880000`
- the same frame therefore carries `buttonClickCount K4 = 2`
- neither hold consumes those clicks
- both remain head-waiting / head-judgeable until they auto-miss later

The concrete queue state for K4 at that point is:

- shared frontier before the frame: `10`
- note `44` shared `buttonQueueIndex = 10`
- note `43` shared `buttonQueueIndex = 11`
- K4 hold queue order: `[43, 44, ...]`

That means:

- queue head is note `43`
- note `43` is still locked because `11 > 10`
- note `44` is unlocked, but it is behind note `43`
- `queueHeadMatches` only allows the queue head to consume input

So the two clicks were present but unusable for both notes.

Confirmed root cause:

- shared button queue indices are assigned by merged time order in `lnmai-core/LnmaiCore/ChartLoader.lean:421`
- hold queues were then rebuilt by per-family timing sort in `lnmai-core/LnmaiCore/ChartLoader.lean:502`
- for equal-time same-lane holds, that later queue order can disagree with the earlier shared `buttonQueueIndex` order
- scheduler gating then deadlocks the head because queue-head order and shared-index order no longer describe the same note

Fix status:

- runtime queue construction now preserves the already-assigned shared queue order instead of rebuilding same-lane equal-time queues by timing only
- a focused regression test now covers equal-time same-lane holds consuming two same-frame clicks in queue order
- the real-chart replay no longer has missing judged notes from this deadlock

This was therefore a real semantic bug in queue construction / ordering, not a hold-tail shaping issue.

### Clarified non-bug — simultaneous same-frame hold heads are allowed in principle

Today’s trace also ruled out a previously suspected explanation.

- The scheduler can consume multiple same-frame clicks on one lane.
- With a correct queue/frontier alignment, the first note can consume click `#1` and the second note can consume click `#2` in the same frame.
- So the failure above is not “same-lane simultaneous hold heads are impossible”.

### Historical probe trail in `小石DISCO`

The rest of this section records an earlier investigation state before the later grouping fix and the hold-head timing-gate fix above.
It is kept as probe history, not as the current remaining-gap list.

The remaining non-perfect set after the queue-order fix and sensor-storage refactor is now:

- `31` → `LatePerfect2nd`
- `44` → `LateGood`
- `54` → `Miss`
- `118` → `Miss`
- `179` → `LateGood`
- `181` → `LateGood`
- `235` → `LateGreat2nd`
- `401` → `Miss`
- `402` → `Miss`
- `403` → `Miss`
- `404` → `Miss`
- `992` → `LatePerfect2nd`
- `993` → `LateGreat2nd`
- `1030` → `LateGreat2nd`
- `1037` → `LateGood`

These split into two materially different buckets.

#### Bucket A — likely default hold-body / release-shaping mismatch

This bucket still looks like replay-shaping rather than missing-note behavior:

- `31`
- `44`
- `179`
- `181`
- `235`
- `992`
- `993`
- `1030`
- `1037`

Observed status:

- an exact-release override experiment only improved note `181`
- the others remain non-perfect even when that narrow release path is forced
- so there is still no single proven hold-tail rule that explains the whole bucket

#### Bucket B — touch / touch-hold grouping semantics mismatch

The strongest remaining structural stem is now:

- `54` → `A5` at `assets/小石DISCO/maidata.txt:14`
- `118` → `A8` at `assets/小石DISCO/maidata.txt:23`
- `401` → `A7h[48:7]` at `assets/小石DISCO/maidata.txt:56`
- `402` → `A2h[48:7]` at `assets/小石DISCO/maidata.txt:57`
- `403` → `A6h[48:7]` at `assets/小石DISCO/maidata.txt:57`
- `404` → `A3h[48:7]` at `assets/小石DISCO/maidata.txt:57`

Why `401..404` is the right next stem:

- four consecutive touch-holds miss together
- they form one same-phrase `each`-style cluster in the chart
- that pattern is much more likely to expose a grouping/modeling mismatch than four unrelated timing errors

### Confirmed loader mismatch against `MajdataPlay`

After checking the reference implementation again, the current Lean loader diverges in one important way.

Reference behavior:

- `NoteLoader` creates fresh `touchGroupMembers` and `touchHoldGroupMembers` per `SimaiTimingPoint`
- only notes added to those timing-local lists are passed to `AllocTouchGroup` / `AllocTouchHoldGroup`
- `CreateTouch` and `CreateTouchHold` add members only when `isEach`
- group connectivity is then computed only inside that timing-local `each` batch

Current Lean behavior:

- `assignTouchGroups` runs over `chart.touches` globally
- `assignTouchHoldGroups` runs over `chart.touchHolds` globally
- both functions group by connected sensor areas without any timing-point or `isEach` boundary

This is not just an internal representation difference.
It changes gameplay semantics because touch shared-result propagation and touch-hold body majority both consult those group ids and group sizes at runtime.

### Why this is a credible explanation for the current misses

This single loader mismatch explains two otherwise awkward observations:

- note `54` was previously observed as `touchGroupId = some 0` even though it is not part of a same-time `each` group in the reference sense
- `401..404` are exactly the kind of same-time touch-hold cluster whose body-sharing semantics depend on a correct timing-local `TouchHoldGroup`

Under the current global assignment, a note can inherit:

- a group id that spans unrelated earlier or later notes
- an inflated `touchGroupSize` / `touchHoldGroupSize`
- stale shared-result visibility from notes that should not be in the same group at all

That is sufficient to distort both:

- touch head auto-sharing in `processTouchNotes`
- touch-hold effective press / majority sharing in `processTouchHoldNotes`

### Concrete redesign proposition

Do not change queue/frontier logic for this step.
The redesign should stay at the chart-loader grouping boundary.

Proposed change:

1. stop assigning touch and touch-hold groups from the whole lowered chart
2. compute groups only inside timing-local `each` batches, matching the reference loader shape
3. leave non-`each` touch and touch-hold notes with `touchGroupId = none` / `touchHoldGroupId = none`
4. preserve existing shared queue indices and runtime queue order exactly as they are

Recommended implementation shape:

- move grouping to the lowering/build path that still has timing-local note batches available
- for each timing point:
  - collect `TouchChartNote` / `TouchHoldChartNote` members that belong to that timing point and are semantically `each`
  - run the connected-component assignment only on that local slice
  - write the resulting ids/sizes back into those notes
- after that, run the existing shared touch-queue index assignment over the full chart

Why this is the safe next cut:

- it matches the reference code path directly
- it changes only group metadata, not queue order, queue indices, or scheduler traversal order
- it targets exactly the runtime fields that the remaining `54` and `401..404` symptoms are already pointing at

### Reproduce path for the next proof step

Use this workflow after the grouping redesign lands.

1. Run `lake exe real-chart-verification`.

   - confirm `小石DISCO` still has `judged: 1119`
   - compare whether `54` and `401..404` improve or collapse together

2. Recheck the chart regions:

   - note `54`: `assets/小石DISCO/maidata.txt:14`
   - notes `401..404`: `assets/小石DISCO/maidata.txt:56-57`

3. Inspect lowered group metadata before runtime stepping:

   - note `54` should no longer carry a synthetic shared touch group unless it is actually in a timing-local `each`
   - notes `401..404` should share one timing-local touch-hold group with size `4`

4. If `401..404` still miss after that loader change:

   - the next search space is narrow and runtime-specific
   - focus only on:
     - touch-hold `groupTriggeredCount`
     - the exact frame when group state becomes visible to sibling holds
     - body-only majority sharing vs head-time click consumption

Current recommendation:

- first fix timing-local touch / touch-hold group assignment in the loader
- then rerun `lake exe real-chart-verification`
- only if `54` and `401..404` still survive should further instrumentation go back into `processTouchNotes` / `processTouchHoldNotes`

## Verification snapshot — `11358_インドア系ならトラックメイカー`

### Asset

- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt`

### Level

- `5`

### Current replay status

- total chart notes: `703`
- judged events: `703`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = true`

### Current grade summary

- `Perfect`: `703`

### Interpretation

- the default replay now APs this level-5 chart
- the root cause was missing Lean slide-table coverage for `pq1` through `pq8`
- this checkpoint now specifically guards parser/runtime parity for `pq` slides against `MajdataPlay`

## Verification snapshot — `834_PANDORA PARADOXXX`

### Asset

- `tools/assets/834_PANDORA PARADOXXX/maidata.txt`

### Level

- `6`

### Current replay status

- total chart notes: `1341`
- judged events: `1341`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = true`

### Current grade summary

- `Perfect`: `1341`

## Verification snapshot — `100524_[協]Hand in Hand`

### Asset

- `tools/assets/100524_[協]Hand in Hand/maidata.txt`

### Level

- `7`

### Current replay status

- total chart notes: `1187`
- judged events: `1187`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = true`

### Current grade summary

- `Perfect`: `1187`

## Verification snapshot — `11264_幽霊東京`

### Asset

- `tools/assets/11264_幽霊東京/maidata.txt`

### Level

- `5`

### Current replay status

- total chart notes: `670`
- judged events: `670`
- missing judged note count: `0`
- missing judged notes: `[]`
- `achievesAP = true`

### Current grade summary

- `Perfect`: `670`

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
- `lake exe real-chart-11358`
- `lake exe real-chart-pandora`
- use a small `lake env lean` script when deeper note-level tracing is needed

### Focused trace helper

- `lake env lean tools/trace_conn_three_part.lean`
- this prints the lowered 3-part connected-slide chain, the default replay input events, and the final judged result
- use it as a reference probe when connected-slide parent linkage or end-part judgment regresses

## Rule

When adding a new real-chart checkpoint:

- record the exact asset path
- record the exact level
- record note count, judged count, and missing-note status
- summarize current grades
- state plainly whether the checkpoint is still failing or already clean
- separate confirmed bugs from suspected bugs; do not merge them into one explanation prematurely
- when a bug is confirmed, record the exact note indices, queue/input facts, and the concrete code path that makes the failure happen
