# Recent Semantic Parity Audit

Date: 2026-06-04

## Summary

This audit checks whether the recent gameplay-facing fixes really tighten
`lnmai-core` toward `reference/MajdataPlay`.

Primary audit targets:

- top-level `573c471` / submodule `588b193` — connected/conjunction slide parser fix
- top-level `f715e03` / submodule `ebeba89` — DSL/proof representation fix
- top-level `4340b6a` / submodule `927d16f` — queue/frontier, touch grouping, and slide head/body split follow-through

Ignored as a semantic target:

- submodule `fd1ea75` — benchmark/runner cleanup only

Overall verdict:

- the audited fixes are semantically aligned with the currently checked
  `MajdataPlay` laws
- no regression was found in parser/lowering tests, runtime tests, real-chart
  replay, or the reduced C# reference harness
- the only caution is that full slide-body behavior is still broader than the
  reduced harness coverage, so slide semantics are confirmed for the currently
  tested laws rather than for every Unity-side implementation detail
- Lean's recursive slide-body progression/update structure is not treated as a
  bug by default; if it produces a small one-frame observational difference
  without replay-visible mismatch, treat that as semantic innovation unless a
  concrete reduced witness shows disagreement against `MajdataPlay`

## Frozen Audit Target

Top-level log used for wrapper/submodule mapping:

- `45b1f24` `Refine the benchmark`
- `4340b6a` `Fix the queue problem`
- `f715e03` `fix the dsl representation`
- `573c471` `fix the conjuction slide parser`

`lnmai-core` log used for actual semantic changes:

- `fd1ea75` `Refine the benchmark`
- `927d16f` `Fix the queue problem`
- `ebeba89` `fix the dsl representation`
- `588b193` `fix the conjuction slide parser`

## Audit Execution

Commands run in this workspace:

1. `git log --oneline --decorate -n 12`
2. `git -C lnmai-core log --oneline --decorate -n 15`
3. `lake env lean LnmaiCore/Simai/Tests.lean`
4. `lake env lean LnmaiCore/RuntimeTests.lean`
5. `lake exe real-chart-verification`
6. `cd tools/majdata-harness && nix-shell --run 'dotnet run --project MajdataHarness.csproj'`

Observed results:

- `LnmaiCore/Simai/Tests.lean`: `54/54` passing
- `LnmaiCore/RuntimeTests.lean`: completed successfully
- `real-chart-verification`: AP on all tracked charts with no missing judged
  notes
- `majdata-harness`: all current reduced reference scenarios passed

Real-chart replay baseline:

- `[協]Hand in Hand`: `1187 / 1187`, AP
- `小石DISCO`: `1119 / 1119`, AP
- `11358_インドア系ならトラックメイカー`: `703 / 703`, AP
- `11264_幽霊東京`: `670 / 670`, AP
- `834_PANDORA PARADOXXX`: `1341 / 1341`, AP

## Verdict Matrix

| Area | Verdict | Lean evidence | Real-chart witness | Reference / harness evidence | Notes |
| --- | --- | --- | --- | --- | --- |
| Connected/conjunction slide parsing from `588b193` | confirmed parity | `test_continuous_conn_qq_chain_matches_majdataplay`, `test_same_head_conn_three_part_parent_chain`, `test_same_head_subsequent_parts_are_headless`, `test_lowered_conn_group_has_one_head_for_first_body` in `LnmaiCore/Simai/Tests.lean` | `PANDORA` and `11358` remain AP after the parser fix | `SimaiProcessExtensions.cs:124-177` shows separate head-vs-body note indexing and headless slide handling during clamp | Continuous `qq` chains now split into connected parts with preserved proportional timing and correct child headlessness. |
| DSL / proof representation fix from `ebeba89` | confirmed parity | `test_chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap` in `LnmaiCore/RuntimeTests.lean` | `小石DISCO` and `PANDORA` remain AP with default replay generation intact | `SimaiProcessExtensions.cs:147-173` shows that headless slide cases should not synthesize an extra judged head | The default replay/tactic layer no longer invents bogus child head taps. |
| Lowered slide head/body split from `927d16f` | confirmed parity | `test_simai_lowered_slide_split_ir_dsl`, `test_lowered_ordinary_slide_splits_head_and_body`, `test_lowered_headless_slide_has_body_only`, `test_lowered_conn_group_has_one_head_for_first_body` in `LnmaiCore/Simai/Tests.lean`; `build_game_state_routes_slide_head_into_tap_queue` and the lowered JSON `headTiming` test in `LnmaiCore/RuntimeTests.lean` | All tracked charts remain AP after the split | `SimaiProcessExtensions.cs:147-173`; `docs/ffi-ir.md:395-432`; `ChartLoader.lean:118-195,598-620` | Ordinary slides lower to explicit `slideHeads + slides`; no-head singleton slides and connected children lower to body-only; `logicalSlideId` is shared while runtime `noteIndex` stays distinct. |
| Queue frontier unlock law | confirmed parity | Runtime tests around the shared frontier notes in `LnmaiCore/RuntimeTests.lean:2636,2770` and clean `RuntimeTests` completion | No chart replay regressions after the queue fix | `NoteManager.cs:389-406`; harness scenarios `Reference-like touch-hold stays judgeable after touch frontier advances past it` and `Reference-like older touch remains judgeable once unlocked frontier moves ahead` in `ScenarioLibrary.cs:108-116,285-315` | Lean now matches the reference unlock-frontier rule `index <= currentIndex` instead of exact-equality gating. |
| Click-consumption law, including mobile-style counts | confirmed parity | RuntimeTests passed with same-frame competition coverage already in-tree | No replay regressions on mixed charts | `NoteManager.cs:433-520`; harness scenarios `Future tap head does not steal click before judgeable`, `Two button clicks let tap then hold both consume on mobile-style counts`, and `Two sensor clicks let touch then touch-hold both consume on mobile-style counts` in `ScenarioLibrary.cs:25-26,45-51` | Desktop one-shot and mobile count-based consumption shapes both match the reduced reference checks. |
| Touch / touch-hold grouping law | confirmed parity | `test_touch_group_majority_shares_result_same_frame` and `test_touch_hold_group_share_requires_strict_majority` in `LnmaiCore/RuntimeTests.lean` | `小石DISCO` now remains AP, which is the canary for this area | `TouchGroup.cs:8-45`; harness scenarios `Touch-hold shared head requires strict majority` and `Touch-hold head can resolve from group share` in `ScenarioLibrary.cs:40-41,80-81` | The June 2 loader/runtime changes are consistent with MajdataPlay-style group-share and strict-majority behavior. |
| Slide head vs slide body runtime semantics | partially confirmed | `build_game_state_routes_slide_head_into_tap_queue`, `test_chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap`, `test_conn_child_progress_force_finishes_parent`, wifi judged-wait tests, exact too-late boundary tests, and related slide tests in `LnmaiCore/RuntimeTests.lean` | `11358`, `PANDORA`, and `[協]Hand in Hand` remain AP | `SimaiProcessExtensions.cs:147-173`; `SlideBase.cs:44-60`; `SlideDrop.cs:430-547`; `doc/slide-head-body-refactor-proposal.md`; expanded harness scenarios for slide checkability, judged-wait, skip-shape, and recursive single-track cascade | The audited laws are confirmed: explicit slide heads compete in the tap-family queue, bodies keep traversal/judgment duties, connected children remain body-only, checkability follows the reference `-50ms`/parent-pending-finish law, and judged-wait delay is preserved. This remains partial because the reduced harness still does not model every Unity slide-body branch. Recursive same-frame slide progression differences are treated as Lean semantic innovation unless a concrete observational mismatch is found. |
| Wifi / connected-slide rule shape | confirmed parity | `test_conn_child_progress_force_finishes_parent`, `test_conn_parent_not_force_finished_without_child_progress`, `test_conn_child_progress_only_force_finishes_direct_parent`, wifi too-late tests around `LnmaiCore/RuntimeTests.lean:1408-1619` | `PANDORA` and `小石DISCO` remain AP | Harness scenarios `Wifi too-late with two single tails is LateGood by max remaining`, `One sensor hold may advance overlapping slides together`, `Conn-slide child checkability and force-finish follow reference rule shape`, and `Wifi progress markers and too-late grading follow reference rule shape` in `ScenarioLibrary.cs:85-100` | Max-remaining wifi grading, overlapping-slide advancement, and conn-slide force-finish behavior all match the current reduced reference model. |

## Evidence Notes

### 1. Parser and lowering

The parser/lowering side is in good shape.

The strongest direct evidence is:

- `LnmaiCore/Simai/Tests.lean` passed `54/54`
- `test_continuous_conn_qq_chain_matches_majdataplay` explicitly checks
  `3qq7qq5[192#30:109]`
- `test_lowered_ordinary_slide_splits_head_and_body` checks ordinary
  head/body split
- `test_lowered_headless_slide_has_body_only` checks singleton no-head lowering
- `test_lowered_conn_group_has_one_head_for_first_body` checks same-head
  connected groups

The reference-side clamp logic in
`reference/MajdataPlay/Assets/Scripts/Misc/Extensions/SimaiProcessExtensions.cs:124-177`
still matches the current Lean interpretation:

- no-head slides occupy one note slot
- headed slides can expose a separate head-note slot
- headless children are not given synthetic extra heads

### 2. Queue frontier and click consumption

The key reference law is explicit in
`reference/MajdataPlay/Assets/Scripts/Scenes/Game/NoteControllers/NoteManager.cs:389-406`:

- tap-family unlock: `index <= currentIndex`
- touch-family unlock: `index <= currentIndex`

The same file at `:433-520` also shows:

- mobile count-based click consumption
- desktop one-shot consumption

The reduced C# harness rechecked the important edge cases directly:

- future non-judgeable heads do not steal a click
- two button clicks can feed tap then hold head on mobile-style counts
- two sensor clicks can feed touch then touch-hold head on mobile-style counts
- older unlocked touch/touch-hold indices stay judgeable after the frontier
  advances

### 3. Touch grouping

`reference/MajdataPlay/Assets/Scripts/Scenes/Game/Misc/Notes/Touch/TouchGroup.cs:8-45`
still reflects the strict-majority style result sharing that the Lean runtime now
tests directly.

The strongest matching checks are:

- `test_touch_group_majority_shares_result_same_frame`
- `test_touch_hold_group_share_requires_strict_majority`
- harness scenario `Touch-hold shared head requires strict majority`

`小石DISCO` remaining AP after the June 2 fix is especially important because it
was the most sensitive chart-level witness for touch/touch-hold grouping.

### 4. Slide head/body split

Current runtime-visible structure matches the intended split:

- `ChartSpec.slideHeads`
- slide-body `headTiming`
- shared `logicalSlideId`
- distinct per-object `noteIndex`

Supporting implementation and schema references:

- `LnmaiCore/ChartLoader.lean:118-195`
- `LnmaiCore/ChartLoader.lean:598-620`
- `docs/ffi-ir.md:395-432`

The audit confirms the intended semantic cut:

- head is a tap-family judged note
- body retains slide traversal and final grading
- connected child parts stay body-only

Expanded slide-body-specific reduced-model coverage now includes:

- head-time checkability boundary at `-50ms`
- connected child body checkability from `parentPendingFinish`
- judged-wait delay before final event emission
- ordinary non-wifi judged-wait delay before final event emission
- single-track skip/cascade queue shape under the reference-style first/second/first check order
- connected multi-part same-frame unlock plus child-body cascade once parent pending-finish state is visible

Important interpretation rule for future audits:

- Lean's recursive slide-body progression and parent-flag propagation are allowed
  to differ slightly from Unity realization order
- do not classify that alone as a parity bug
- only treat it as a bug candidate when a concrete reduced witness or replay-visible
  mismatch shows an observational disagreement against `MajdataPlay`

## SlideDrop Branch Checklist

The following checklist is organized branch-by-branch against the current
`reference/MajdataPlay/.../SlideDrop.cs` implementation so future audits can
see which slide-body cells are already evidenced and which remain open.

### Covered or materially covered

- `InitializeSlideGroup`
  - connected child `StartTiming = parent.StartTiming + parent.Length`
  - child last-area `SetIsLast` vs non-end-part `SetNonLast`
  - end-part or non-conn-only `JudgeTiming` / `LastWaitTimeSec`
  - evidence:
    - `test_same_head_conn_three_part_chain_achieves_ap`
    - `test_conn_child_pending_finish_becomes_checkable`
    - parser/lowering connected-slide tests

- `UpdateJudgeQueue`
  - short conn skip shape for head and end parts
  - long conn all-skippable shape
  - evidence:
    - `test_normalized_short_conn_skip_rule`
    - `test_reference_like_slide_skip_chain_does_not_clear_last_area_early`
    - `test_reference_like_slide_skip_chain_c_off_only_does_not_clear_all`

- `SensorCheck`
  - first/second/first queue-check order
  - same-frame recursive single-track cascade
  - connected child same-frame unlock plus body progress
  - overlapping shared-sensor progress across slides
  - evidence:
    - harness `Reference-like single-track slide body may cascade through multiple areas in one frame`
    - harness `Connected child slide may unlock and cascade in the same frame once parent is pending finish`
    - `test_overlapping_slides_can_both_progress_from_one_sensor_hold`

- `SlideCheck`
  - ordinary and wifi `-50ms` checkability boundary
  - connected child checkability from `parentFinished || parentPendingFinish`
  - strict too-late boundary
  - judged-wait delayed emission
  - evidence:
    - harness `Slide head-time checkability boundary is inclusive at -50ms`
    - harness `Conn child slide body becomes checkable from parent pending finish`
    - `test_wifi_not_checkable_before_minus_50ms`
    - `test_wifi_exact_minus_50ms_becomes_checkable`
    - `test_wifi_exact_too_late_boundary_does_not_judge`
    - `test_single_slide_exact_too_late_boundary_does_not_judge`
    - harness `Slide judged-wait delays final event until wait expires`
    - harness `Ordinary non-wifi slide judged-wait delays final event until wait expires`
    - `test_wifi_judged_wait_emits_delayed_event_then_hides`
    - `test_wifi_judged_wait_before_expiry_emits_nothing`
    - `test_replay_slide_delays_final_event_after_internal_judged`

- `SetParentFinish`
  - first-progress-only parent force-finish trigger
  - no re-force after prior progress
  - direct-parent-only force-finish law
  - evidence:
    - `test_conn_child_progress_force_finishes_parent`
    - `test_conn_parent_not_force_finished_without_child_progress`
    - `test_conn_child_progress_only_force_finishes_direct_parent`
    - `test_conn_already_progressed_child_does_not_re_force_finish_parent`
    - harness `Conn-slide child checkability and force-finish follow reference rule shape`

- `TooLateJudge` and `End`
  - final grade shape by remaining queue length
  - final report only for non-conn or conn end-part
  - evidence:
    - `test_wifi_too_late_ends_immediately`
    - `test_wifi_too_late_one_remaining_becomes_lategood`
    - `test_wifi_too_late_two_single_tails_is_lategood_by_max_remaining`
    - `test_single_slide_too_late_two_segments_remaining_stays_miss`
    - `test_single_slide_too_late_last_segment_remaining_becomes_lategood`
    - `test_conn_non_end_part_does_not_judge_when_finished`
    - `test_conn_non_end_part_does_not_too_late_judge`

### Still open or only partially audited

- `OnUpdate` star-visual state branches
  - `Inited -> Scaling -> Running -> Arrived` transitions for:
    - ordinary slide
    - no-head slide
    - connected child with hidden head star
  - current status:
    - gameplay semantics indirectly covered by replay
    - visual/state-timing parity not reduced-modeled

- `SlideBarFadeIn` timing/cutoff interaction from `OnPreUpdate`
  - fade-in start, cutoff, and completion timing under varied speeds/offsets
  - current status:
    - not covered by reduced harness
    - not a current gameplay-authority blocker, but still unaudited as a branch set

- `SensorCheck` SFX-side gating
  - `canPlaySFX = group head or non-conn`
  - one-shot or repeated SFX policy as queue progresses
  - current status:
    - body progression semantics covered
    - audio-side branch parity still open

- `Autoplay`
  - full `AutoplayModeOption.Enable` body progression and bar hiding thresholds
  - `DJAutoplay_*` branch family and simulated sensor path
  - current status:
    - not covered in reduced harness
    - open unless autoplay semantics become a parity target

- `LoadSlidePath` / `LoadSkin` branch effects that may feed body-side behavior
  - mirrored path rotation inheritance from connected parent
  - `IsJustR` / `SetR` and `SetL` transform tweaks
  - current status:
    - parser/runtime realpaths are covered
    - exact visual transform branch parity remains open

- connected multi-track composed recursion beyond current witnesses
  - current reduced witnesses cover:
    - single-track recursive cascade
    - child unlock plus single child-body cascade
  - still open:
    - same-frame child unlock plus multi-track wifi-style body progression
    - same-frame parent/child overlap where both family gating and per-track max-remaining logic matter together

- exact observational equivalence of Lean recursive realization order
  - current policy:
    - treat one-frame differences as intentional Lean semantic innovation by default
  - still open:
    - produce a reduced witness where Unity and Lean are compared on the same connected recursive frame if a concrete mismatch is ever suspected

## Lean Runtime Smell Audit

This pass separately audits the Lean runtime/support code for stale debug
leftovers, fake/toy-like implementations, and local parity patches that could
accidentally masquerade as first-principles semantics.

### Confirmed stale runtime artifact, now removed

- `LnmaiCore/Scheduler.lean`
  - `processTouchNotes` previously contained hardcoded `dbg_trace` probes for
    `note.params.noteIndex = 54`, including a frame-specific
    `currentTime.toMicros = 14280000` branch.
  - classification:
    - stale debug leftover in the live runtime path
  - semantic status:
    - probably harmless to gameplay semantics
    - removed during this audit pass from the authoritative scheduler path

### Intentional runtime parity shims, not currently classified as bugs

- `LnmaiCore/Lifecycle.lean:460-470`
  - deluxe-hold body release handling explicitly skips the short
    release-ignore grace after a missed/too-fast head.
  - classification:
    - intentional parity shim, reference-backed
  - basis:
    - `reference/MajdataPlay/.../HoldDrop.cs` seeds `_releaseTime = 114514`
      immediately on too-late head miss before body processing
    - `reference/MajdataPlay/.../TouchHoldDrop.cs` does the same for
      touch-holds
    - the Lean code comment ties this to MajdataPlay seeding `_releaseTime` to
      a sentinel after head miss
    - runtime tests explicitly lock this behavior in:
      `test_modern_hold_head_miss_skips_release_ignore_grace`
      and `test_modern_hold_perfect_head_keeps_release_ignore_grace`
  - audit note:
    - this is exactly the kind of "head hurts, patch head" rule worth keeping
      under review
    - however, it is not a fake Lean-only patch; the Unity reference really
      does implement it via sentinel-state forcing
    - keep it only as an explicitly documented reference-compatibility rule,
      not as a silently generalized hold law

- `LnmaiCore/Scheduler.lean:114-121,342-351`
  - button-family notes can consume matching outer-ring sensor input via
    `fallbackSensorAreaForButtonNote` and related helpers.
  - classification:
    - intentional runtime compatibility policy, reference-backed
  - basis:
    - `reference/MajdataPlay/.../TapDrop.cs` checks button click first and then
      matching `SensorPos` click
    - `reference/MajdataPlay/.../HoldDrop.cs` checks button click first and then
      matching `SensorPos` click for the head
    - `reference/MajdataPlay/.../HoldDrop.cs` body logic also treats button or
      matching sensor press as active hold input
    - runtime tests explicitly rely on it, such as
      `test_button_tap_can_use_matching_a_sensor`
      and `test_classic_hold_matching_a_sensor_keeps_body_pressed`
  - audit note:
    - odd-looking, but not stale or fake under the current semantic model
    - this should be treated as a real MajdataPlay law rather than a local Lean
      convenience

### Proof / tooling helpers that are simplified on purpose

- `LnmaiCore/Proofs/Runtime.lean:352-390`
  - `chooseSlideStepAreas` and `slideRepresentativePathSteps` collapse
    missing-target cases to a fallback area and merge wifi tracks into a
    representative path.
  - classification:
    - proof-facing simplification
  - audit note:
    - toy-like in the modeling sense, but not currently used as live runtime
      authority

- `LnmaiCore/Scheduler.lean:634-775`
  - `probeTouchHeadAt` and `probeTapHoldSensorConsumers` are explicit
    diagnostic/probe APIs.
  - classification:
    - tooling/debug APIs
  - audit note:
    - acceptable as long as they remain clearly out of the main stepping path

- `LnmaiCore/Simai/Frontend.lean:62-89`
- `LnmaiCore/Simai/DSL.lean:202-300`
  - literal helpers use `panic!` on invalid embedded chart/note text.
  - classification:
    - elaboration/test helper behavior
  - audit note:
    - sharp-edged by design, but not a gameplay runtime smell

### Compatibility / fallback surfaces worth watching

- `LnmaiCore/ChartLoader.lean:32-35,142-165`
- `LnmaiCore/Lifecycle.lean:91-94,114-130`
  - `getObjValAsD?` swallows decode errors and substitutes defaults for many
    optional or compatibility-shaped fields.
  - classification:
    - permissive compatibility surface
  - semantic status:
    - not all fallback decoding is dangerous
    - the most important recent slide-body schema split is protected by the
      explicit `headTiming` requirement and the runtime test
      `test_lowered_slide_chart_json_requires_head_timing_and_rejects_legacy_timing`
  - audit note:
    - still a real risk for less-tested fields because malformed JSON can be
      silently normalized into defaults

- `LnmaiCore/ChartLoader.lean:139,165`
- `LnmaiCore/Simai/Normalize.lean:41,256`
  - `debugSimai` still exists on lowered slide bodies and is threaded through
    normalization/loading.
  - classification:
    - inspection/debug metadata, not currently a semantic branch
  - audit note:
    - not a bug on its own, but should not become runtime authority

### Metadata and compatibility fields audited as non-authoritative

- `LnmaiCore/RuntimeTests.lean:632-670`
- `LnmaiCore/Proofs/Runtime.lean:396-431,527-548`
  - the replay/tactic layer is explicitly tested not to synthesize slide-head
    input from slide-body metadata alone.
  - evidence:
    - `test_default_tactic_does_not_infer_slide_head_from_body_metadata_alone`
    - `test_default_tactic_uses_explicit_slide_head_even_if_body_compat_flag_is_headless`
    - `test_default_tactic_replays_head_only_lowered_slide_chart`
  - classification:
    - cleared as a current semantic risk
  - audit note:
    - body-side `isSlideNoHead` is not the replay authority
    - explicit `slideHeads` objects remain the authority for head-tap replay
    - `logicalSlideId` is used to correlate head/body objects, not to invent
      missing head judgments on its own

- `LnmaiCore/Simai/Normalize.lean:235-257`
- `LnmaiCore/ChartLoader.lean:139-188`
  - lowered slide bodies still carry `debugSimai` and `isSlideNoHead`, but the
    normalization/build pipeline currently treats them as metadata rather than
    as primary runtime control.
  - evidence:
    - normalization writes `logicalSlideId := note.noteIndex` and threads
      `debugSimai` as inspection data
    - runtime build derives tap-family heads from explicit `slideHeads` in
      `buildGameState`
    - `test_build_game_state_ignores_debug_simai_metadata_for_runtime_shape`
  - classification:
    - obscure-looking but currently non-authoritative metadata
  - audit note:
    - keep watching these fields for future drift, but this pass found no
      hidden branch where they override the explicit head/body split

- `LnmaiCore/Scheduler.lean:142-149`
- `LnmaiCore/ChartLoader.lean:155-156,178-179,589-590`
  - `parentFinished` and `parentPendingFinish` can be loaded from chart JSON,
    which looks suspicious at first glance.
  - evidence:
    - scheduler recomputes parent status each frame via
      `updateSlideParentFlags`
    - `test_scheduler_recomputes_stale_conn_parent_flags_before_child_progress`
  - classification:
    - compatibility-shaped initial state, not current long-term semantic
      authority
  - audit note:
    - still somewhat awkward because the fields live on the serialized chart
    - however, they are overwritten by runtime state propagation rather than
      permanently trusted as authored truth

### Current conclusion of the Lean smell audit

- confirmed stale runtime leftover, now cleaned up:
  - the `probe54` `dbg_trace` block formerly present in `Scheduler.lean`
- intentional but ad hoc-looking parity rules, now source-checked:
  - modern hold missed-head release-ignore bypass
  - outer-ring sensor fallback for button-family notes
- simplified but non-authoritative helpers:
  - proof slide representative-path construction
  - explicit scheduler probe APIs
  - literal/DSL `panic!` helpers
- permissive surfaces to keep watching:
  - JSON fallback decoding via `getObjValAsD?`

No additional confirmed fake runtime semantics were found in this pass beyond
the now-removed scheduler debug probe. The remaining suspicious areas are
either documented compatibility policies or proof/tooling helpers and should be
judged accordingly instead of being mixed into gameplay-authority parity
conclusions.

### Final repo-wide sweep result

A final repo-wide Lean search over suspicious patterns such as:

- `dbg_trace`
- `panic!`
- `fallback`
- `compat`
- `legacy`
- `probe`
- `debugSimai`
- `getObjValAsD?`
- `isSlideNoHead`
- `parentPendingFinish`
- `parentFinished`
- `logicalSlideId`

did not reveal any materially new unclassified runtime-authority smells beyond
the cases already covered above.

The remaining hits were confined to:

- runtime/test evidence for already-documented parity rules
- proof/tooling helpers already classified as non-authoritative
- IR/normalization metadata definitions already classified as data carriers
- the still-open permissive JSON fallback surface already documented above

## Conclusion

The recent fixes centered on `588b193`, `ebeba89`, and `927d16f` should be
treated as semantically validated against the currently checked
`MajdataPlay` laws.

Recommended status:

- parser/lowering parity for the recent connected-slide and head/body changes:
  accepted
- runtime queue/frontier and click-consumption parity for the recent fixes:
  accepted
- touch/touch-hold grouping parity for the recent fixes: accepted
- slide-body semantics beyond the currently tested law set: keep under normal
  regression watch, but no active mismatch is currently indicated
- recursive one-frame slide-body differences without observational mismatch:
  treat as intentional Lean semantic innovation, not as a bug by default
