# MaiMuriDX Proposition Transfer Proof Plan

This document records a proof plan for transferring suitable propositions from
`tools/MaiMuriDX/docs/开发笔记.md` into the current Lean proof and runtime
verification stack.

The plan explicitly excludes propositions that depend on a player-hand model.

Excluded examples include:

- assumptions that the player only has two hands
- hand radius or contact-circle heuristics
- left/right-hand assignment claims
- finger-count constraints
- geometric reachability claims stated in terms of human posture
- detector policy claims whose meaning depends on human play style rather than
  runtime note semantics

The goal here is narrower and more formal:

- transfer propositions that can be stated as parser, queue, scheduler,
  judgment, or replay invariants
- prove them against the current Lean runtime authority
- connect reduced lemmas to real-chart checkpoints where possible

## Real-chart assets now available

The repository now has the following chart assets available for theorem-backed
checkpoints and local-slice witness extraction.

- `tools/assets/11264_幽霊東京/maidata.txt`
- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt`
- `tools/assets/11475_SUPER AMBULANCE/maidata.txt`
- `tools/assets/230_CYCLES/maidata.txt`
- `tools/assets/24_Sun Dance/maidata.txt`
- `tools/assets/462_7thSense/maidata.txt`
- `tools/assets/589_Panopticon/maidata.txt`
- `tools/assets/834_PANDORA PARADOXXX/maidata.txt`
- `tools/assets/100230_[宴]CYCLES/maidata.txt`

These assets should be used in two different ways:

- as full-chart semantic checkpoints when the whole chart provides broad runtime
  coverage
- as local-slice witness sources when only a short fragment expresses the
  proposition cleanly

## Real-chart proposition candidates

This section records which real charts are the best current sources for
non-hand-model propositions.

### Full-chart checkpoint charts

These are the best charts to use as theorem-backed whole-chart checkpoints.

#### `11358_インドア系ならトラックメイカー`

Asset:

- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt`

Best role:

- full-chart checkpoint

Best proposition families:

- parser/runtime parity for parsed slide families
- chart-derived default replay completeness
- stable AP checkpoint for regression guarding

Why it is valuable:

- this chart already serves as a proof-backed clean replay checkpoint
- it is especially useful for guarding parser-derived slide-family coverage

Current anchor:

- `Proofs/RealChartVerification11358.lean:1`

#### `834_PANDORA PARADOXXX`

Asset:

- `tools/assets/834_PANDORA PARADOXXX/maidata.txt`

Best role:

- full-chart checkpoint

Best proposition families:

- dense overlap replay stability
- crowded slide-family interaction stability
- chart-level AP replay guard for overlap-heavy content

Why it is valuable:

- this chart stresses overlapping and dense slide behavior without requiring a
  hand-feasibility argument

Current anchor:

- `Proofs/RealChartVerificationPandora.lean:1`

#### `24_Sun Dance`

Asset:

- `tools/assets/24_Sun Dance/maidata.txt`

Best role:

- optional full-chart checkpoint, but more likely a local-slice source

Best proposition families:

- fast timing boundary behavior
- frame-window sensitivity
- exact interval regression guards around dense timing runs

Why it is valuable:

- the note uses it as a timing-motivation reference, so it is a natural source
  for exact-boundary witness slices

### Local-slice witness charts

These charts are best used by extracting short local fragments into proof
modules rather than insisting on whole-chart claims first.

#### `11264_幽霊東京`

Asset:

- `tools/assets/11264_幽霊東京/maidata.txt`

Best role:

- local-slice witness source

Best proposition families:

- contrasting-tactic witness theorems
- local same-head or hold-through-start replay differences

Why it is valuable:

- the repo already has a strong witness-style proof from this chart

Current anchor:

- `Proofs/RealChartVerification11264.lean:1`

#### `11475_SUPER AMBULANCE`

Asset:

- `tools/assets/11475_SUPER AMBULANCE/maidata.txt`

Best role:

- local-slice witness source

Best proposition families:

- slide-touch interaction semantics
- local replay facts where a touch outcome is determined by slide progression

Why it is valuable:

- the note explicitly points to a slide-touch interaction example from this
  chart, and that proposition can be restated without any hand model

#### `230_CYCLES`

Asset:

- `tools/assets/230_CYCLES/maidata.txt`

Best role:

- local-slice witness source

Best proposition families:

- slide-end timing boundary behavior
- exact local replay theorems around end-area collision timing

Why it is valuable:

- the note gives explicit timing-sensitive examples here that are natural for
  boundary proofs

#### `100230_[宴]CYCLES`

Asset:

- `tools/assets/100230_[宴]CYCLES/maidata.txt`

Best role:

- optional local-slice witness source

Best proposition families:

- variant timing or slide-density witness checks if the standard chart does not
  isolate the desired pattern cleanly

Why it is valuable:

- it may provide an alternate denser source for CYCLES-style local boundaries

#### `462_7thSense`

Asset:

- `tools/assets/462_7thSense/maidata.txt`

Best role:

- local-slice witness source

Best proposition families:

- one-stroke connected-slide local semantics
- skip-sensitive queue behavior
- parser-to-queue preservation on local slide chains

Why it is valuable:

- the note discusses concrete connected-slide sequences from this chart that can
  be reframed as queue and replay propositions

#### `589_Panopticon`

Asset:

- `tools/assets/589_Panopticon/maidata.txt`

Best role:

- local-slice witness source

Best proposition families:

- connected-slide activation timing
- early-start versus gated-checkability semantics

Why it is valuable:

- the note gives a concrete early-start connected-slide narrative here that can
  be restated as a runtime-state theorem

## Real-chart usage policy

When choosing between a whole-chart checkpoint and a local-slice proof, use the
following rule.

Use a whole-chart checkpoint when:

- the chart already replays cleanly and covers a broad semantic family
- the chart is useful as a long-lived regression guard
- the target proposition is diffuse across the chart rather than concentrated in
  one short fragment

Use a local-slice proof when:

- the proposition is expressed by one short pattern in the note
- the claim is timing-sensitive or queue-sensitive at a specific location
- the whole chart would add noise without increasing theorem clarity

For the current asset set, the default recommendation is:

- whole-chart checkpoints: `11358_インドア系ならトラックメイカー`,
  `834_PANDORA PARADOXXX`
- primary local-slice sources: `11264_幽霊東京`, `11475_SUPER AMBULANCE`,
  `230_CYCLES`, `462_7thSense`, `589_Panopticon`
- optional timing-focused source: `24_Sun Dance`

## Transfer rule

A proposition from `开发笔记.md` is a good transfer candidate if it can be
rephrased in one of the following forms:

- a parser or lowering property
- a slide queue construction property
- a queue-update or area-skip property
- a judgment-window boundary property
- a replay equivalence or replay witness property
- a connected-slide or wifi progression property
- a note-interaction property expressible without human-hand assumptions

A proposition is not a current transfer candidate if its statement requires any
of the following:

- choosing how many hands a player has
- choosing a hand radius or touch-contact geometry model
- deciding whether a configuration is “reasonable for humans”
- classifying a pattern as “muri” only because of human technique limits

## Selected propositions

The following families from `tools/MaiMuriDX/docs/开发笔记.md` are the best
targets for the current repo.

### 1. Slide queue theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:468`

Transfer shape:

- slide queue progression is determined by explicit queue state
- queue advancement depends on press, release, and skip conditions
- queue advancement is stable under parser-derived topology
- queue completion implies a precise judgment transition

Why it fits:

- this is discrete operational semantics
- the repo already treats Lean slide topology as authoritative
- reduced queue theorems can be tied directly to parser-derived runtime notes

Current anchors:

- `LnmaiCore/Simai/SlideTables.lean:1`
- `LnmaiCore/Lifecycle.lean:1`
- `LnmaiCore/RuntimeTests.lean:2389`
- `doc/real-chart-verification.md:81`

### 2. Area-skip preservation theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:468`

Transfer shape:

- if skip is permitted by queue law, then entering an allowed later area
  advances exactly the intended queue prefix
- skipping does not clear forbidden areas early
- skip-sensitive end segments preserve their boundary behavior

Why it fits:

- this is already close to existing regression tests
- it avoids any human-motion assumptions
- it is one of the most reusable proof kernels for future slide correctness

Current anchors:

- `LnmaiCore/RuntimeTests.lean:2389`
- `LnmaiCore/RuntimeTests.lean:2392`
- `doc/real-chart-verification.md:81`

### 3. Connected-slide and wifi max-remaining theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:468`
- `tools/MaiMuriDX/docs/开发笔记.md:743`

Transfer shape:

- connected-slide child checkability depends on parent completion semantics
- wifi progression and too-late semantics depend on max remaining track length
- special marker/progress behavior for wifi is stable under queue reduction

Why it fits:

- these are runtime-state theorems, not detector heuristics
- the repo already has reduced runtime tests and real-chart coverage

Current anchors:

- `LnmaiCore/RuntimeTests.lean:356`
- `LnmaiCore/RuntimeTests.lean:388`
- `LnmaiCore/RuntimeTests.lean:1091`
- `LnmaiCore/RuntimeTests.lean:1122`
- `doc/real-chart-verification.md:86`

### 4. Overlap and shared-input theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:468`

Transfer shape:

- overlapping slides may legitimately advance from one shared held sensor if
  runtime sensor-state semantics say so
- shared clicks are consumed according to scheduler order rather than human
  intent

Why it fits:

- this is exactly the kind of reference-style runtime law that Lean can fix in
  place

Current anchors:

- `LnmaiCore/RuntimeTests.lean:434`
- `LnmaiCore/RuntimeTests.lean:2446`
- `LnmaiCore/RuntimeTests.lean:2458`
- `doc/real-chart-verification.md:87`

### 5. Hold and touch-hold judgment theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:690`
- `tools/MaiMuriDX/docs/开发笔记.md:735`

Transfer shape:

- head-miss fallback semantics for modern holds
- released-body recovery behavior for touch-holds
- strict perfect-boundary behavior for classic holds
- shared-head click-consumption behavior between note families

Why it fits:

- these are exact timing and state-transition properties
- they are already partially encoded in regression tests

Current anchors:

- `LnmaiCore/RuntimeTests.lean:138`
- `LnmaiCore/RuntimeTests.lean:161`
- `LnmaiCore/RuntimeTests.lean:279`
- `doc/real-chart-verification.md:82`
- `doc/real-chart-verification.md:83`
- `doc/real-chart-verification.md:84`
- `doc/real-chart-verification.md:85`

### 6. Judgment-window boundary theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:633`
- `tools/MaiMuriDX/docs/开发笔记.md:643`
- `tools/MaiMuriDX/docs/开发笔记.md:743`

Transfer shape:

- exact boundary inclusions and exclusions are preserved
- zero-delta and positive-delta frame windows behave as specified
- strict versus non-strict inequalities are fixed by theorem

Why it fits:

- these are small, mechanical, and highly valuable regression theorems

Current anchors:

- `LnmaiCore/RuntimeTests.lean:2467`
- `LnmaiCore/RuntimeTests.lean:2470`
- `LnmaiCore/RuntimeTests.lean:2473`
- `LnmaiCore/RuntimeTests.lean:2476`

### 7. Local replay witness theorem family

Source area:

- `tools/MaiMuriDX/docs/开发笔记.md:119`
- `tools/MaiMuriDX/docs/开发笔记.md:307`

Transfer shape:

- for a reduced chart slice, one explicit tactic fails in a specific way
- another explicit tactic succeeds with AP or AP+
- the theorem is about runtime replay outcome, not human feasibility rhetoric

Why it fits:

- the repo already has this proof shape working well
- it gives concrete semantic checkpoints without importing hand heuristics

Current anchors:

- `Proofs/RealChartVerification11264.lean:34`
- `Proofs/RealChartVerification11264.lean:94`

## Propositions intentionally excluded

The following narrative families from `开发笔记.md` should not be transferred
into proofs right now.

### Excluded family 1. Multi-hand propositions

Excluded source area:

- `tools/MaiMuriDX/docs/开发笔记.md:57`

Reason:

- statements such as “this needs three hands” are not runtime semantics in the
  current repo
- they require a resource model for hands, ownership, and simultaneity policy

Possible future prerequisite:

- a formal hand-allocation semantics layer above the current runtime

### Excluded family 2. Hand-radius or touch-contact geometry propositions

Excluded source area:

- `tools/MaiMuriDX/docs/开发笔记.md:25`
- `tools/MaiMuriDX/docs/开发笔记.md:119`

Reason:

- these claims depend on a continuous geometric contact model absent from the
  repo’s current authority boundary

Possible future prerequisite:

- a formal geometric sensor-contact model with explicit assumptions

### Excluded family 3. Minimum-cover-circle TouchGroup propositions

Excluded source area:

- `tools/MaiMuriDX/docs/开发笔记.md:57`

Reason:

- the circle-cover threshold is a detector policy choice tied to human reach,
  not a current parser/judgment invariant

### Excluded family 4. Muri labels whose meaning depends on player technique

Excluded source area:

- `tools/MaiMuriDX/docs/开发笔记.md:7`

Reason:

- the current repo proves runtime facts and replay outcomes
- it does not yet formalize the detector taxonomy as a trusted semantic layer

## Complete proof plan

This section gives the recommended implementation order.

The plan is structured so that earlier results become reusable lemmas for later
chart-level proofs.

### Phase 1. Fix the theorem vocabulary

Goal:

- define a stable vocabulary for queue, skip, progression, overlap, and replay
  witness statements

Deliverables:

- a small proof-facing terminology section in code or docs
- theorem naming conventions for queue and replay properties

Proposed naming pattern:

- `slide_queue_*` for queue-state theorems
- `slide_skip_*` for area-skip laws
- `wifi_*` for wifi-specific progression theorems
- `conn_*` for connected-slide gating theorems
- `hold_*` and `touch_hold_*` for hold-family laws
- `replay_*` for local witness theorems

Suggested file targets:

- `LnmaiCore/RuntimeTests.lean` for executable regression statements
- `Proofs/` for higher-level chart-backed theorem modules
- optionally introduce focused theorem files later if `RuntimeTests.lean`
  becomes too crowded

Exit condition:

- every selected proposition family has a target theorem namespace and naming
  style

### Phase 2. Consolidate reduced queue kernels

Goal:

- isolate the smallest reusable reduced examples for slide queue and skip laws

Tasks:

- identify existing reduced tests that already encode skip behavior
- group them under a documented “queue kernel” subsection
- add any missing single-slide reductions needed to cover:
  - press-to-progress
  - release-to-progress
  - allowed skip-to-progress
  - forbidden skip does not progress
  - final-area preservation

Primary source propositions to encode:

- queue progression only changes when queue-state rules fire
- skip never clears protected segments early

Suggested theorem inventory:

- `slide_queue_press_enters_current_segment`
- `slide_queue_release_finishes_current_segment`
- `slide_skip_allowed_advances_exact_prefix`
- `slide_skip_forbidden_preserves_current_segment`
- `slide_queue_last_area_not_cleared_early`

Expected implementation style:

- reduced `GameState`
- one or a few `Scheduler.stepFrame` applications
- theorem by `native_decide`

Exit condition:

- all core queue laws can be pointed to by later proofs without restating the
  entire scenario

### Phase 3. Formalize parser-to-queue preservation checkpoints

Goal:

- connect parser-derived slide topology to runtime queue behavior

Tasks:

- choose a small set of slide families that exercise distinct queue laws
- construct parser-derived charts using `simai_*` DSL forms
- prove that replay results depend on the parser-derived queues, not hand-made
  ad hoc queue stubs

Best candidates:

- a basic one-track slide
- a skip-sensitive slide
- a connected-slide chain
- a wifi shape

Recommended real-chart sources:

- `tools/assets/462_7thSense/maidata.txt` for skip-sensitive and one-stroke
  local slide chains
- `tools/assets/589_Panopticon/maidata.txt` for connected-slide chain timing
- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt` for
  parser-backed chart-level slide-family preservation

Suggested theorem inventory:

- `parsed_slide_queue_replays_like_reference_kernel`
- `parsed_skip_sensitive_slide_preserves_non_skippable_segment`
- `parsed_conn_child_waits_for_parent_finish_rule`
- `parsed_wifi_uses_max_remaining_progress_rule`

Why this phase matters:

- it closes the gap between abstract runtime kernels and real parser authority
- it makes `开发笔记.md`-style queue narratives traceable back to chart text

Exit condition:

- at least one parser-backed theorem exists for each of the queue, connected,
  and wifi families

### Phase 4. Complete wifi and connected-slide law coverage

Goal:

- make wifi and connected-slide semantics a first-class theorem cluster

Tasks:

- consolidate existing max-remaining and parent-pending-finish tests
- add any missing edge cases around:
  - exactly one remaining segment
  - center-cleared progress markers
  - too-late transition at exact boundary
  - child activation versus parent pending-finish

Suggested theorem inventory:

- `wifi_max_remaining_one_implies_lategood`
- `wifi_center_cleared_uses_special_progress_marker`
- `wifi_exact_too_late_boundary_preserved`
- `conn_child_becomes_checkable_at_parent_pending_finish`

Recommended real-chart sources:

- `tools/assets/589_Panopticon/maidata.txt` for connected-slide timing witnesses
- `tools/assets/834_PANDORA PARADOXXX/maidata.txt` for overlap-heavy slide
  replay coverage
- `tools/assets/11358_インドア系ならトラックメイカー/maidata.txt` when a
  parser-backed whole-chart checkpoint is needed in addition to reduced laws

Exit condition:

- wifi and connected-slide semantics are represented by a coherent theorem set
  rather than scattered regressions

### Phase 5. Complete hold-family law coverage

Goal:

- fully encode the non-hand-model hold and touch-hold propositions from the
  note appendix and workflow discussion

Tasks:

- gather existing head-miss, released-body, and boundary tests
- identify any missing exact-boundary equalities
- add explicit same-click consumption theorems for shared note-head scenarios

Suggested theorem inventory:

- `modern_hold_head_miss_allows_late_good_end`
- `modern_hold_release_ignore_grace_is_skipped_after_head_miss`
- `touch_hold_released_body_can_recover`
- `classic_hold_exact_boundary_degrades_from_perfect`
- `shared_click_is_consumed_by_first_matching_note`

Recommended real-chart sources:

- `tools/assets/11264_幽霊東京/maidata.txt` for existing local witness style
- `tools/assets/24_Sun Dance/maidata.txt` for fast timing-boundary local slices

Exit condition:

- all currently referenced hold-family semantics in
  `doc/real-chart-verification.md:81` are backed by named theorem clusters

### Phase 6. Add local witness proofs for note narratives

Goal:

- convert selected development-note narratives into explicit replay witness
  theorems, without importing hand heuristics

Tasks:

- identify reduced chart slices whose semantic point is independent of hand
  geometry
- encode two contrasting tactics where useful:
  - one tactic fails with a precise non-perfect outcome
  - one tactic succeeds with AP

Good witness targets:

- same-head interaction timing
- connected-slide activation timing
- skip-sensitive local slide chains
- overlap/shared-sensor behavior

Recommended chart sources:

- `tools/assets/11264_幽霊東京/maidata.txt`
- `tools/assets/11475_SUPER AMBULANCE/maidata.txt`
- `tools/assets/230_CYCLES/maidata.txt`
- `tools/assets/462_7thSense/maidata.txt`
- `tools/assets/589_Panopticon/maidata.txt`

Proof style:

- parser-derived local chart slice
- `defaultTacticFromChart` or `tacticFromChartWithModules`
- theorem over `missingJudgedNoteIndices`, `achievesAP`, or exact non-perfect
  list

Exit condition:

- at least three local witness modules exist in `Proofs/` following the style of
  `Proofs/RealChartVerification11264.lean:1`

### Phase 7. Lift reduced laws into real-chart checkpoint interpretations

Goal:

- tie abstract theorems back to real charts already tracked in the repo

Tasks:

- update checkpoint docs when a theorem family explains why a chart now passes
- add one new real-chart checkpoint only when it guards a genuinely distinct
  semantic family

Current chart roles:

- `Proofs/RealChartVerification11358.lean:1` guards parser/runtime parity for
  `pq` slide coverage and clean default replay
- `Proofs/RealChartVerificationPandora.lean:1` guards dense overlap and
  slide-family replay behavior
- `Proofs/RealChartVerification11264.lean:1` shows a strong local-witness proof
  pattern that should be repeated
- `tools/assets/11475_SUPER AMBULANCE/maidata.txt` is the leading candidate for
  a future slide-touch local witness module
- `tools/assets/230_CYCLES/maidata.txt` is the leading candidate for future
  slide-end boundary witness modules
- `tools/assets/462_7thSense/maidata.txt` is the leading candidate for future
  skip-sensitive and one-stroke queue witness modules
- `tools/assets/589_Panopticon/maidata.txt` is the leading candidate for future
  connected-slide activation witness modules

Suggested documentation rule:

- every real-chart checkpoint should cite which reduced theorem families it is
  intended to guard

Exit condition:

- the relationship between reduced laws and real-chart checkpoints is explicit
  in docs

## Proposed file-by-file work breakdown

### `LnmaiCore/RuntimeTests.lean`

Use for:

- small-step runtime laws
- exact boundary checks
- queue and skip kernel tests
- overlap/shared-input consumption laws

Add next:

- missing queue-kernel names and groupings
- any missing exact-boundary wifi or slide checks
- parser-backed micro regressions only if they remain compact

### `Proofs/`

Use for:

- local witness theorem modules
- chart-slice proofs with explicit tactic contrasts
- real-chart checkpoint theorems

Add next:

- one module for slide skip witnesses
- one module for wifi/connected-slide witnesses
- one module for shared-click or overlap witness proofs
- one module sourced from `tools/assets/11475_SUPER AMBULANCE/maidata.txt`
- one module sourced from `tools/assets/230_CYCLES/maidata.txt`
- one module sourced from `tools/assets/462_7thSense/maidata.txt`
- one module sourced from `tools/assets/589_Panopticon/maidata.txt`

### `doc/real-chart-verification.md`

Use for:

- recording which theorem families each real chart guards
- keeping the reduced-law to chart-checkpoint mapping visible

Add next:

- a short theorem-family mapping subsection under each chart checkpoint

## Priority order

Recommended order of execution:

1. area-skip preservation theorems
2. slide queue theorem cluster
3. wifi max-remaining and connected-slide activation cluster
4. hold and touch-hold exact-boundary cluster
5. overlap/shared-input cluster
6. local witness proof modules
7. documentation linkage to real-chart checkpoints

This order is recommended because:

- skip and queue laws are the smallest reusable kernels
- wifi and connected-slide laws build directly on queue semantics
- hold-family laws are independent and can proceed in parallel if desired
- local witness proofs become cleaner once the kernel theorems already exist

Recommended chart order alongside the theorem order:

1. `tools/assets/462_7thSense/maidata.txt`
2. `tools/assets/589_Panopticon/maidata.txt`
3. `tools/assets/11475_SUPER AMBULANCE/maidata.txt`
4. `tools/assets/230_CYCLES/maidata.txt`
5. `tools/assets/24_Sun Dance/maidata.txt`

## Immediate next theorem candidates

The next concrete theorems to implement should be:

1. one named area-skip preservation theorem extracted from existing regressions
2. one parser-backed slide queue preservation theorem
3. one named wifi max-remaining theorem with explicit statement wording cleanup
4. one connected-slide child-activation theorem
5. one local witness module showing a skip-sensitive slide slice with two
   contrasting replay tactics

The next concrete real-chart modules to add after that should be:

1. a `7thSense` local-slice witness module for skip-sensitive one-stroke flow
2. a `Panopticon` local-slice witness module for connected-slide activation
3. a `SUPER AMBULANCE` local-slice witness module for slide-touch interaction
4. a `CYCLES` local-slice witness module for end-boundary timing

## Proof start log

This section records concrete proof work that has already started, together
with obstacles that should be treated as suspicious points rather than guessed
through.

### Completed first step

The first completed proof step is the reduced area-skip kernel extraction in
`LnmaiCore/RuntimeTests.lean`.

Added named theorems:

- `slide_skip_forbidden_preserves_current_segment`
- `slide_skip_allowed_advances_exact_prefix`
- `slide_queue_last_area_not_cleared_early`

These theorems are intentionally reduced and proved by `native_decide` directly
over `Lifecycle.replaySlideQueue`, so they can serve as the first stable queue
kernel without importing any hand-model assumptions.

### Completed second step

The next started proof step is a parser-backed local witness module sourced from
`tools/assets/462_7thSense/maidata.txt`.

Added module:

- `Proofs/RealChartVerification7thSense.lean`

Current parser-backed theorems in that module:

- `local_skip_chain_has_expected_parser_slice`
- `local_skip_chain_default_replay_has_no_missing_notes`
- `local_skip_chain_default_replay_achieves_ap`
- `local_skip_chain_default_replay_has_no_non_perfect_notes`
- `parsed_slide_queue_replays_like_reference_kernel`

The current slice is the note-cited sequence
`7p1[4:1], 1p3[4:1], 3p5[4:1], 5>2[4:1], 2<6[4:1]`, extracted by exact lowered
note indices `649` through `653` from the level-5 chart asset.

### Completed third step

The next note-faithful proposition cluster promoted existing reduced runtime
facts for Wifi semantics into proposition-shaped theorem names in
`LnmaiCore/RuntimeTests.lean`.

Added named theorems:

- `wifi_center_cleared_uses_special_progress_marker`
- `wifi_center_cleared_without_both_tails_uses_max_remaining_progress`
- `wifi_max_remaining_one_implies_lategood`
- `wifi_head_checkability_boundary_excludes_before_minus_50ms`
- `wifi_head_checkability_boundary_includes_exact_minus_50ms`
- `wifi_exact_too_late_boundary_preserved`

These are intended to stay faithful to the runtime propositions described in the
note’s Wifi/Slide judgment discussion, without introducing any stronger global
claim than the reduced runtime scenarios already justify.

### Completed fourth step

The next note-faithful proposition cluster promoted existing reduced connected-
slide gating facts into proposition-shaped theorem names in
`LnmaiCore/RuntimeTests.lean`.

Added named theorems:

- `conn_child_becomes_checkable_at_parent_pending_finish`
- `conn_child_becomes_checkable_at_parent_finished`
- `conn_parent_not_force_finished_without_child_progress`
- `conn_child_progress_only_force_finishes_direct_parent`

These stay within the note-compatible runtime semantics boundary: they describe
child activation and direct-parent finish propagation only as reduced scheduler
facts, without importing any hand-model or detector-taxonomy assumptions.

### Completed fifth step

The next note-faithful proposition cluster covered the “留尾判绿” rule in the
reduced Wifi case already present in runtime witnesses.

Added named theorems:

- `slide_too_late_last_segment_remaining_becomes_lategood_in_reduced_wifi_case`
- `slide_too_late_two_or_more_segments_remaining_stays_miss_in_reduced_wifi_case`

These are deliberately scoped to the reduced Wifi witness already available in
`LnmaiCore/RuntimeTests.lean`, because that is the exact fragment currently
proved. This avoids overstating the result as a fully general Slide theorem
before a broader reduced or parser-backed proof is added.

### Completed sixth step

The ordinary single-slide version of the “留尾判绿” proposition is now also
covered by reduced runtime witnesses in `LnmaiCore/RuntimeTests.lean`.

Added named theorems:

- `slide_too_late_last_segment_remaining_becomes_lategood`
- `slide_too_late_two_or_more_segments_remaining_stays_miss`

These theorem names match the note’s ordinary Slide judgment proposition more
directly than the earlier Wifi-scoped witnesses, while still remaining reduced
runtime theorems rather than broader unproved generalizations.

### Completed seventh step

The skip-protection side now has an explicit reduced-kernel plus parser-backed
linkage.

Reduced kernel anchors already present in `LnmaiCore/RuntimeTests.lean`:

- `slide_skip_forbidden_preserves_current_segment`
- `slide_skip_allowed_advances_exact_prefix`
- `slide_queue_last_area_not_cleared_early`

Parser-backed `7thSense` local-slice theorems now added in
`Proofs/RealChartVerification7thSense.lean`:

- `parsed_skip_sensitive_slide_chain_has_no_missing_notes`
- `parsed_skip_sensitive_slide_chain_achieves_ap`
- `parsed_skip_sensitive_slide_chain_preserves_queue_completion_order`

This step stays faithful to the development note by linking parser-derived real-
chart material to the queue/skip proposition family, without claiming a stronger
skip law than the current reduced and parser-backed witnesses justify.

### Completed eighth step

The next refinement step for short-slide protection discovered an important
authority-boundary detail and was adjusted accordingly.

Added named theorems in `LnmaiCore/RuntimeTests.lean`:

- `short_conn_child_becomes_checkable_with_short_queue_rule`
- `short_conn_child_waits_without_progress_but_does_not_force_finish_parent`

These reflect the current runtime-supported short connected-slide protection
rule, which is already encoded by `applySingleTrackConnRules` and the existing
reduced connected-slide witnesses.

### Completed ninth step

The parser/lowering-layer proof for the note’s short `1-3` protection narrative
 is now in place in `LnmaiCore/Simai/Tests.lean`.

Added parser-level theorem:

- `test_normalized_line3_has_protected_middle_segment_proof`

What it proves:

- `1-3[4:1]` normalizes and lowers with total queue length `3`
- the queue shape is exactly `A1`, `A2/B2`, `A3`
- the middle `A2/B2` segment is marked non-skippable in both normalized and
  lowered authority

This is the correct proof layer for the note’s operational short-slide
protection story, and it resolves the earlier proof-layer mismatch without
guessing a runtime-only witness.

### Completed tenth step

An ordinary-slide activation boundary fact is now promoted in
`LnmaiCore/RuntimeTests.lean`.

Added theorem:

- `slide_frame_zero_becomes_checkable_and_progresses_same_frame`

This theorem records the reduced runtime fact that an ordinary single-track
slide at frame zero can become checkable and consume a matching sensor hold in
that same frame. It is a small mechanical runtime-boundary theorem and fits the
note-compatible proof style, while remaining narrower than any broader global
claim about all slide activation boundaries.

### Completed eleventh step

The strict too-late equality boundary now has both Wifi and ordinary single-
slide reduced witnesses in `LnmaiCore/RuntimeTests.lean`.

Added theorem:

- `slide_exact_too_late_boundary_preserved`

This complements the earlier Wifi theorem
`wifi_exact_too_late_boundary_preserved` by showing that an ordinary single-
track slide also remains active at the exact too-late equality boundary, i.e.
too-late uses a strict `>` comparison in the reduced runtime witness.

### Completed twelfth step

The slide head-versus-body timing distinction is now made explicit at the
parser/lowering layer in `LnmaiCore/Simai/Tests.lean`.

Added theorems:

- `slide_body_start_is_later_than_head_for_44pace_reference_case`
- `slide_body_start_supports_explicit_absolute_star_wait`

These formalize the already-existing authoritative fact that a slide has a head
timing and a later body `startTiming`, and that the later body start can be
specified both by the standard `4:1` form and by explicit absolute star-wait
syntax.

### Suspicious points currently recorded

1. There is not yet an obvious compact parser-backed skip-sensitive theorem in
   the current proof tree that mirrors the new reduced skip kernel exactly.
   Existing parser-backed material is stronger for connected-slide and whole-
   chart replay coverage than for a minimal skip-only preservation statement.

2. The current reduced skip kernel is phrased in queue-length terms. That is
   enough for a first theorem start, but it may be too weak if later proofs need
   exact queue-content preservation rather than only exact remaining-prefix
   length.

3. The plan’s next candidate, “one parser-backed slide queue preservation
   theorem”, needs a careful choice of chart slice. `462_7thSense` is the best
   documented candidate in the plan, but the exact local fragment has not yet
   been extracted in code. This should be treated as an extraction task, not
   guessed from prose alone.

4. Existing named runtime regressions already cover connected-slide and wifi
   semantics well, but their theorem names are still test-shaped. Promoting them
   into proposition-shaped theorem names should be done carefully to avoid
   introducing duplicate statements with slightly different wording.

5. The current `7thSense` parser-backed module proves clean replay facts for the
   extracted local slice, but it does not yet prove the stronger contrastive
   narrative from the development note (for example, one tactic failing while
   another succeeds). That stronger statement needs an explicitly constructed
   alternative tactic module rather than assumption from prose.

6. The new Wifi theorem names are proposition-shaped aliases over existing
   reduced regression cases. This is faithful to the note’s semantic claims, but
   if later work needs direct theorem statements over raw runtime fields instead
   of `.passed = true` wrappers, that should be done as a separate refinement
   step rather than folded into the current transfer blindly.

7. The new connected-slide theorem names have the same status: they are
   proposition-shaped aliases over reduced runtime witnesses. This is acceptable
   for the current transfer, but not yet the final form for a more abstract API
   unless later work explicitly refactors the statements themselves.

8. The “留尾判绿” proposition is currently formalized only in the reduced Wifi
   case. The note states it as a Slide-wide judgment rule, so a future widening
   step may be desirable, but it should only be done once a non-Wifi reduced
   witness or parser-backed witness is extracted explicitly.

9. The non-Wifi reduced witness has now been added, which significantly narrows
   that gap. What is still not done is a parser-backed local chart witness for
   the same proposition; that remains optional unless a note-cited real-chart
   slice is needed for regression value.

10. The parser-backed `7thSense` skip slice currently proves clean replay and
    preserved event order for the extracted local chain. It still does not prove
    a stronger counterfactual statement such as “this exact protected segment
    would fail under an alternative skip attempt” unless such an alternative
    tactic is explicitly constructed.

11. A direct reduced witness for the prose-style `1-3` event-queue narrative was
    attempted and rejected. Suspicious point: the note states the protection rule
    at the event-queue / normalized-slide level, while the current runtime proof
    layer exposes `SensorArea` queue witnesses that do not line up naively with a
    hand-written `A2/B2` reduced state. Rather than guess the correspondence, the
    proof step was narrowed to the already authoritative short-connection runtime
    rule.

12. The parser/lowering theorem now covers the authoritative `1-3` queue shape
    itself, so the remaining gap is no longer about visibility of `B` areas. Any
    future runtime-layer theorem should be derived from this authority rather than
    re-invented as an ad hoc queue witness.

13. The new frame-zero ordinary-slide theorem is a reduced runtime boundary
    witness. It is valuable as a mechanical guardrail, but it should not be read
    as a complete replacement for the note’s broader timing-boundary discussion.

14. The strict too-late equality theorem is still a reduced witness theorem. It
    supports the note-compatible exact-boundary story, but does not yet claim a
    single unified theorem simultaneously quantifying over every slide family.

15. The head/body timing proposition is currently represented at the parser /
    normalization layer rather than the runtime scheduler layer. That is the
    correct authority for this fact, because `startTiming` originates in slide
    lowering semantics rather than in frame-by-frame queue update behavior.

## Success criteria

This transfer effort is successful when:

- the transferred propositions are stated without player-hand assumptions
- the theorem set is organized by semantic family rather than by bug history
- reduced laws and real-chart checkpoints reference each other clearly
- future refactors can use the theorem families as stable semantic guardrails

## Non-goal reminder

This plan does not attempt to formalize:

- hand-count feasibility
- hand radius
- touch-contact area geometry
- minimum-cover-circle human reach arguments
- broad “muri” taxonomy as a detector-policy layer

Those may become a future project, but they should not be mixed into the
current runtime-semantic proof transfer effort.
