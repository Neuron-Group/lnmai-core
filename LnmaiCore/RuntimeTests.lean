import LnmaiCore.Basic
import LnmaiCore.Storage
import LnmaiCore.RuntimeProofAPI
import Lean.Data.Json

namespace LnmaiCore.RuntimeTests

open Lean

structure RuntimeCase where
  name : String
  passed : Bool
  note : String := ""
deriving Repr

private def passCase (name : String) (passed : Bool) (note : String := "") : RuntimeCase :=
  { name := name, passed := passed, note := note }

private def tp (secondsMicros : Int) : TimePoint :=
  TimePoint.fromMicros secondsMicros

private def dur (micros : Int) : Duration :=
  Duration.fromMicros micros

private def secs (whole : Int) : TimePoint :=
  tp (whole * 1000000)

private def runtimePosJsonEq (lhs rhs : RuntimePos) : Bool :=
  toJson lhs == toJson rhs

private def exceptIsError : Except ε α → Bool
  | .error _ => true
  | .ok _ => false

private def eventNoteIndices (events : List JudgeEvent) : List Nat :=
  events.map (fun evt => evt.noteIndex)

private def eventGrades (events : List JudgeEvent) : List JudgeGrade :=
  events.map (fun evt => evt.grade)

private def eventKinds (events : List JudgeEvent) : List JudgeEventKind :=
  events.map (fun evt => evt.kind)

private def hasTrackProgress (cmds : List RenderCommand) (noteIndex trackIndex remaining : Nat) : Bool :=
  cmds.any (fun cmd =>
    match cmd with
    | .UpdateSlideTrackProgress noteIndex' trackIndex' remaining' =>
        noteIndex' == noteIndex && trackIndex' == trackIndex && remaining' == remaining
    | _ => false)

private def hasHideAllSlideBars (cmds : List RenderCommand) (noteIndex : Nat) : Bool :=
  cmds.any (fun cmd =>
    match cmd with
    | .HideAllSlideBars noteIndex' => noteIndex' == noteIndex
    | _ => false)

private def sensorHeldVec (held : List SensorArea) : SensorVec Bool :=
  SensorVec.ofFn (fun area => held.any (fun item => item == area))

private def buttonFlagVec (pressed : List ButtonZone) : ButtonVec Bool :=
  ButtonVec.ofFn (fun zone => pressed.any (fun item => item == zone))

private def sensorFlagVec (pressed : List SensorArea) : SensorVec Bool :=
  SensorVec.ofFn (fun area => pressed.any (fun item => item == area))

private def buttonCountVec (clicks : List ButtonZone) : ButtonVec Nat :=
  ButtonVec.ofFn (fun zone => (clicks.filter (fun item => item == zone)).length)

private def sensorCountVec (clicks : List SensorArea) : SensorVec Nat :=
  SensorVec.ofFn (fun area => (clicks.filter (fun item => item == area)).length)

private def mkButtonFrameInput
    (buttonClicks : List ButtonZone := [])
    (buttonHeld : List ButtonZone := [])
    (sensorClicks : List SensorArea := [])
    (sensorHeld : List SensorArea := [])
    (delta : Duration := dur 16000) : InputModel.FrameInput :=
  { buttonClicked := buttonFlagVec buttonClicks
  , buttonHeld := buttonFlagVec buttonHeld
  , sensorClicked := sensorFlagVec sensorClicks
  , sensorHeld := sensorFlagVec sensorHeld
  , buttonClickCount := buttonCountVec buttonClicks
  , sensorClickCount := sensorCountVec sensorClicks
  , delta := delta }

private def activeSingleTapState : InputModel.GameState :=
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 1 }
    , lane := .S1
    , state := .Judgeable }
  { currentTime := tp 984000
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

def test_button_tap_can_use_matching_a_sensor : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (_, events, _, _) := Scheduler.stepFrame activeSingleTapState input
  match events with
  | [evt] =>
      passCase "button_tap_can_use_matching_a_sensor"
        (evt.kind = .Tap && evt.position = .button .K1)
        "regular tap resolves from matching A sensor fallback"
  | _ => passCase "button_tap_can_use_matching_a_sensor" false "expected one tap event"

private def activeClassicHoldState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 2 }
    , start := .button .K1
    , state := .BodyHeld
    , length := dur 200000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isClassic := true }
  { currentTime := tp 1050000
  , activeHolds := [(.K1, hold)]
  , prevSensor := sensorHeldVec [.A1] }

def test_classic_hold_matching_a_sensor_keeps_body_pressed : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame activeClassicHoldState input
  let stillActive := nextState.activeHolds.length = 1
  passCase "classic_hold_matching_a_sensor_keeps_body_pressed"
    (events.isEmpty && stillActive)
    "classic hold body remains active while matching A sensor stays held"

private def activeModernHoldHeadMissState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 3 }
    , start := .button .K1
    , state := .HeadJudged .Miss
    , length := dur 800000
    , headDiff := dur 150000
    , headGrade := .Miss
    , playerReleaseTime := Duration.zero
    , isClassic := false }
  { currentTime := tp 1700000
  , activeHolds := [(.K1, hold)] }

def test_modern_hold_head_miss_can_end_as_late_good : RuntimeCase :=
  let input := mkButtonFrameInput [] [.K1] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame activeModernHoldHeadMissState input
  match nextState.activeHolds, events with
  | [], [evt] =>
      passCase "modern_hold_head_miss_can_end_as_late_good"
        (evt.kind = .Hold && evt.grade = .LateGood)
        "modern hold can recover a missed head into LateGood if the body is sufficiently held"
  | _, _ => passCase "modern_hold_head_miss_can_end_as_late_good" false "expected one final hold event and no remaining active hold"

private def modernHoldHeadMissNoPressState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 31 }
    , start := .button .K1
    , state := .HeadJudged .Miss
    , length := dur 800000
    , headDiff := dur 150000
    , headGrade := .Miss
    , playerReleaseTime := Duration.zero
    , isClassic := false }
  { currentTime := tp 1160000
  , activeHolds := [(.K1, hold)] }

def test_modern_hold_head_miss_skips_release_ignore_grace : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame modernHoldHeadMissNoPressState input
  match nextState.activeHolds, events with
  | [(_, holdAfter)], [] =>
      let enteredReleased := match holdAfter.state with | .BodyReleased => true | _ => false
      passCase "modern_hold_head_miss_skips_release_ignore_grace"
        (enteredReleased && holdAfter.playerReleaseTime = dur 16000)
        "MajdataPlay seeds release-ignore away after a missed head, so the next unpressed frame should enter released state immediately"
  | _, _ =>
      passCase "modern_hold_head_miss_skips_release_ignore_grace" false "expected active hold to enter BodyReleased without judging yet"

private def modernHoldPerfectHeadNoPressState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 32 }
    , start := .button .K1
    , state := .HeadJudged .Perfect
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := Duration.zero
    , isClassic := false }
  { currentTime := tp 1160000
  , activeHolds := [(.K1, hold)] }

def test_modern_hold_perfect_head_keeps_release_ignore_grace : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame modernHoldPerfectHeadNoPressState input
  match nextState.activeHolds, events with
  | [(_, holdAfter)], [] =>
      let stillHeadJudged := match holdAfter.state with | .HeadJudged .Perfect => true | _ => false
      passCase "modern_hold_perfect_head_keeps_release_ignore_grace"
        (stillHeadJudged && holdAfter.playerReleaseTime = dur 16000)
        "a normal judged head should still spend the short release-ignore grace before entering released state"
  | _, _ =>
      passCase "modern_hold_perfect_head_keeps_release_ignore_grace" false "expected active hold to remain in head-judged grace state"

private def touchHoldReleasedWithBodyMajorityState : InputModel.GameState :=
  let holdA1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 33 }
    , start := .sensor .A1
    , state := .BodyReleased
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := dur 32000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 44
    , touchHoldGroupSize := 3 }
  let holdA2 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 34 }
    , start := .sensor .A2
    , state := .BodyHeld
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 44
    , touchHoldGroupSize := 3
    , touchHoldGroupTriggered := true }
  let holdA3 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 35 }
    , start := .sensor .A3
    , state := .BodyHeld
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 44
    , touchHoldGroupSize := 3
    , touchHoldGroupTriggered := true }
  { currentTime := tp 1300000
  , activeTouchHolds := [(.A1, holdA1), (.A2, holdA2), (.A3, holdA3)] }

def test_touch_hold_body_majority_reactivates_released_note : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldReleasedWithBodyMajorityState input
  let recovered :=
    nextState.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .BodyHeld => true | _ => false)
  passCase "touch_hold_body_majority_reactivates_released_note"
    (events.isEmpty && recovered)
    "MajdataPlay body-group majority should turn a released touch-hold back into held before force-end"

private def touchHoldReleasedWithLocalPressState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 36 }
    , start := .sensor .A1
    , state := .BodyReleased
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := dur 32000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  { currentTime := tp 1300000
  , activeTouchHolds := [(.A1, hold)] }

def test_touch_hold_local_press_reactivates_released_note : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldReleasedWithLocalPressState input
  let recovered :=
    nextState.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .BodyHeld => true | _ => false)
  passCase "touch_hold_local_press_reactivates_released_note"
    (events.isEmpty && recovered)
    "a released touch-hold should also recover when its own sensor is pressed again"

def test_classic_hold_fast_boundary_is_strict : RuntimeCase :=
  let timing := secs 1
  let length := dur 500000
  let releaseTiming := timing + length - Constants.HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC
  let grade := Judge.judgeHoldClassicEnd .Perfect timing length releaseTiming
  passCase "classic_hold_fast_boundary_is_strict"
    (grade = .FastGood)
    "MajdataPlay uses a strict `<` fast perfect boundary for classic hold end; equality should degrade to FastGood"

def test_classic_hold_late_boundary_is_strict : RuntimeCase :=
  let timing := secs 1
  let length := dur 500000
  let releaseTiming := timing + length + Constants.HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC
  let grade := Judge.judgeHoldClassicEnd .Perfect timing length releaseTiming
  passCase "classic_hold_late_boundary_is_strict"
    (grade = .LateGood)
    "MajdataPlay uses a strict `<` late perfect boundary for classic hold end; equality should degrade to LateGood"

private def touchHoldGroupHalfShareState : InputModel.GameState :=
  let holdA1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 37 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 55
    , touchGroupSize := 4
    , touchHoldGroupId := some 55
    , touchHoldGroupSize := 4 }
  { currentTime := tp 984000
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [holdA1] } else { notes := [] })
  , activeTouchHolds := [(.A1, holdA1)]
  , touchGroupStates := [{ groupId := 55, count := 2, size := 4, grade := .Perfect, diff := Duration.zero }] }

def test_touch_hold_group_share_requires_strict_majority : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldGroupHalfShareState input
  let unresolved :=
    nextState.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .HeadJudgeable => true | _ => false)
  passCase "touch_hold_group_share_requires_strict_majority"
    (events.isEmpty && unresolved)
    "MajdataPlay requires `Percent > 0.5`, so an exact half share must not silently judge the touch-hold head"

private def wifiParentPendingFinishState : InputModel.GameState :=
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 40 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 500000
    , startTiming := tp 900000
    , slideKind := .Wifi
    , isConnSlide := true
    , isGroupPartHead := true
    , initialQueueRemaining := 3
    , totalJudgeQueueLen := 3
    , trackCount := 3
    , judgeQueues := [[{ targetAreas := [.A1], policy := .Or, isLast := true, isSkippable := true, arrowProgressWhenOn := 1, arrowProgressWhenFinished := 2 }], [], []] }
  let childArea : Lifecycle.SlideArea :=
    { targetAreas := [.A2]
    , policy := .Or
    , isLast := true
    , isSkippable := true
    , arrowProgressWhenOn := 1
    , arrowProgressWhenFinished := 2 }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 41 }
    , lane := .S2
    , state := .Active Duration.zero
    , length := dur 500000
    , startTiming := tp 900000
    , slideKind := .Single
    , isConnSlide := true
    , parentNoteIndex := some 40
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , trackCount := 1
    , judgeQueues := [[childArea]] }
  { currentTime := tp 900000
  , slides := [parent, child] }

def test_conn_child_wifi_parent_pending_finish_becomes_checkable : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame wifiParentPendingFinishState input
  match nextState.slides with
  | _parent :: child :: _ =>
      passCase "conn_child_wifi_parent_pending_finish_becomes_checkable"
        child.isCheckable
        "connected child should become checkable when a wifi parent is pending-finish by max remaining track length = 1"
  | _ =>
      passCase "conn_child_wifi_parent_pending_finish_becomes_checkable" false "expected parent and child slides"

private def wifiTooLateTwoSingleTailsState : InputModel.GameState :=
  let lastLeft : Lifecycle.SlideArea :=
    { targetAreas := [.A1], policy := .Or, isLast := true, isSkippable := true, arrowProgressWhenOn := 8, arrowProgressWhenFinished := 8 }
  let lastRight : Lifecycle.SlideArea :=
    { targetAreas := [.A3], policy := .Or, isLast := true, isSkippable := true, arrowProgressWhenOn := 8, arrowProgressWhenFinished := 8 }
  let wifi : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 42 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 200000
    , startTiming := tp 800000
    , slideKind := .Wifi
    , isClassic := true
    , initialQueueRemaining := 3
    , totalJudgeQueueLen := 3
    , trackCount := 3
    , judgeQueues := [[lastLeft], [], [lastRight]] }
  { currentTime := tp 1601000
  , slides := [wifi] }

def test_wifi_too_late_two_single_tails_is_lategood_by_max_remaining : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame wifiTooLateTwoSingleTailsState input
  match nextState.slides, events with
  | [slide], [evt] =>
      passCase "wifi_too_late_two_single_tails_is_lategood_by_max_remaining"
        (match slide.state with | .Ended => evt.grade = .LateGood | _ => false)
        "wifi too-late should be LateGood when max remaining track length is exactly 1, even if two tracks each still have one tail"
  | _, _ =>
      passCase "wifi_too_late_two_single_tails_is_lategood_by_max_remaining" false "expected one ended wifi slide and one event"

private def overlappingSlideSharedSensorState : InputModel.GameState :=
  let sharedArea : Lifecycle.SlideArea :=
    { targetAreas := [.A1]
    , policy := .Or
    , isLast := true
    , isSkippable := true
    , arrowProgressWhenOn := 1
    , arrowProgressWhenFinished := 2 }
  let slide1 : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 43 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 300000
    , startTiming := tp 900000
    , slideKind := .Single
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , trackCount := 1
    , judgeQueues := [[sharedArea]] }
  let slide2 : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 44 }
    , lane := .S8
    , state := .Active Duration.zero
    , length := dur 300000
    , startTiming := tp 900000
    , slideKind := .Single
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , trackCount := 1
    , judgeQueues := [[sharedArea]] }
  { currentTime := tp 950000
  , slides := [slide1, slide2] }

def test_overlapping_slides_can_both_progress_from_one_sensor_hold : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, _, _, renderCmds) := Scheduler.stepFrame overlappingSlideSharedSensorState input
  let clearedBoth := nextState.slides.all (fun slide => slide.judgeQueues.all List.isEmpty)
  let renderedBoth := hasHideAllSlideBars renderCmds 43 && hasHideAllSlideBars renderCmds 44
  passCase "overlapping_slides_can_both_progress_from_one_sensor_hold"
    (clearedBoth && renderedBoth)
    "MajdataPlay slide progress reads shared sensor status, so one held sensor may legitimately advance overlapping slides at once"

private def simultaneousShortRegularHoldsState : InputModel.GameState :=
  let holdK1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 200 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 937500 }
  let holdK8 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 201 }
    , start := .button .K8
    , state := .HeadWaiting
    , length := dur 937500 }
  { currentTime := tp 984000
  , holdQueues := ButtonVec.ofFn (fun zone =>
      if zone == .K1 then { notes := [holdK1] }
      else if zone == .K8 then { notes := [holdK8] }
      else { notes := [] })
  , activeHolds := [(.K1, holdK1), (.K8, holdK8)] }

private def simulateSimultaneousShortHoldSequence : InputModel.GameState × List JudgeEvent :=
  let batches : List InputModel.TimedInputBatch :=
    [ { currentTime := secs 1
      , events := [ InputModel.TimedInputEvent.buttonClick (secs 1) .K1
                  , InputModel.TimedInputEvent.buttonHold (secs 1) .K1 true
                  , InputModel.TimedInputEvent.buttonClick (secs 1) .K8
                  , InputModel.TimedInputEvent.buttonHold (secs 1) .K8 true ] }
    , { currentTime := tp 1953500, events := [] }
    , { currentTime := tp 1969500
      , events := [ InputModel.TimedInputEvent.buttonHold (tp 1969500) .K1 false
                  , InputModel.TimedInputEvent.buttonHold (tp 1969500) .K8 false ] }
    , { currentTime := tp 1985500, events := [] }
    ]
  let replay := replayBatchesFromState simultaneousShortRegularHoldsState batches
  (replay.finalState, replay.events)

def test_simultaneous_short_regular_holds_can_both_finish : RuntimeCase :=
  let (finalState, events) := simulateSimultaneousShortHoldSequence
  let holdEvents := events.filter (fun evt => evt.kind = .Hold)
  passCase "simultaneous_short_regular_holds_can_both_finish"
    (eventNoteIndices holdEvents = [200, 201]
      && eventGrades holdEvents = [.Perfect, .Perfect]
      && finalState.activeHolds.isEmpty)
    "two simultaneous short regular holds should both reach final hold judgment when both heads are clicked and both bodies stay held"

private def chartBuiltSimultaneousShortRegularHolds : ChartLoader.ChartSpec :=
  { holds :=
      [ { timing := secs 1, slot := .S1, length := dur 937500, noteIndex := 210 }
      , { timing := secs 1, slot := .S8, length := dur 937500, noteIndex := 211 } ] }

def test_chart_wrapper_short_regular_hold_pair_can_finish : RuntimeCase :=
  let tactic := defaultTacticFromChart chartBuiltSimultaneousShortRegularHolds
  let result := simulateChartSpecWithTactic chartBuiltSimultaneousShortRegularHolds tactic
  let holdEvents := result.events.filter (fun evt => evt.kind = .Hold)
  passCase "chart_wrapper_short_regular_hold_pair_can_finish"
    (missingJudgedNoteIndices result = []
      && eventNoteIndices holdEvents = [210, 211]
      && eventGrades holdEvents = [.Perfect, .Perfect]
      && endsWithNoActiveRuntimeNotes result)
    "chart-wrapper replay should also finish the same short simultaneous regular hold pair"

private def chartBuiltHoldPairWithPrecedingTaps : ChartLoader.ChartSpec :=
  { taps :=
      [ { timing := tp 72187500, slot := .S6, noteIndex := 220 }
      , { timing := tp 73125000, slot := .S7, noteIndex := 221 } ]
  , holds :=
      [ { timing := tp 74062500, slot := .S1, length := dur 937500, noteIndex := 222 }
      , { timing := tp 74062500, slot := .S8, length := dur 937500, noteIndex := 223 } ] }

def test_chart_wrapper_hold_pair_with_preceding_taps_can_finish : RuntimeCase :=
  let tactic := defaultTacticFromChart chartBuiltHoldPairWithPrecedingTaps
  let result := simulateChartSpecWithTactic chartBuiltHoldPairWithPrecedingTaps tactic
  let holdEvents := result.events.filter (fun evt => evt.kind = .Hold)
  passCase "chart_wrapper_hold_pair_with_preceding_taps_can_finish"
    (missingJudgedNoteIndices result = []
      && eventNoteIndices holdEvents = [222, 223]
      && eventGrades holdEvents = [.Perfect, .Perfect]
      && endsWithNoActiveRuntimeNotes result)
    "adding the immediate preceding taps should still allow the short simultaneous hold pair to finish"

private def chartBuiltShortHoldPairAfterUnrelatedTaps : ChartLoader.ChartSpec :=
  { taps :=
      [ { timing := tp 71250000, slot := .S6, noteIndex := 230 }
      , { timing := tp 72187500, slot := .S6, noteIndex := 231 }
      , { timing := tp 73125000, slot := .S7, noteIndex := 232 } ]
  , holds :=
      [ { timing := tp 74062500, slot := .S1, length := dur 937500, noteIndex := 233 }
      , { timing := tp 74062500, slot := .S8, length := dur 937500, noteIndex := 234 } ] }

def test_chart_wrapper_short_hold_pair_after_unrelated_taps_can_finish : RuntimeCase :=
  let tactic := defaultTacticFromChart chartBuiltShortHoldPairAfterUnrelatedTaps
  let result := simulateChartSpecWithTactic chartBuiltShortHoldPairAfterUnrelatedTaps tactic
  let holdEvents := result.events.filter (fun evt => evt.kind = .Hold)
  passCase "chart_wrapper_short_hold_pair_after_unrelated_taps_can_finish"
    (missingJudgedNoteIndices result = []
      && eventNoteIndices holdEvents = [233, 234]
      && holdEvents.all (fun evt => !evt.grade.isMissOrTooFast)
      && endsWithNoActiveRuntimeNotes result)
    "future same-lane taps must not pre-consume clicks needed by short hold heads"

private def activeConnSlidesState : InputModel.GameState :=
  let parentArea : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let childArea : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true }
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 10 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[parentArea]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 11 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 10
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[childArea]] }
  { currentTime := secs 1
  , slides := [parent, child] }

def test_conn_child_progress_force_finishes_parent : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A2] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame activeConnSlidesState input
  match nextState.slides with
  | parent :: child :: _ =>
      passCase "conn_child_progress_force_finishes_parent"
        (parent.judgeQueues.all List.isEmpty && child.isCheckable)
        "child progress forces parent queues empty once child starts consuming"
  | _ => passCase "conn_child_progress_force_finishes_parent" false "expected parent and child slides"

private def activeFinishedSlideState : InputModel.GameState :=
  let finishedArea : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, wasOn := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 20 }
    , lane := .S1
    , state := .Active (dur 50000)
    , length := dur 200000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[finishedArea]] }
  { currentTime := tp 1184000
  , slides := [slide]
  , touchPanelOffset := dur 16000 }

def test_slide_judge_uses_touch_panel_offset : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame activeFinishedSlideState input
  match nextState.slides with
  | slide :: _ =>
      match slide.state with
      | .Judged _ _ judgeDiff =>
          passCase "slide_judge_uses_touch_panel_offset"
            (judgeDiff = Time.fromMillis 184)
            "finished slide stores offset-adjusted judge diff"
      | _ => passCase "slide_judge_uses_touch_panel_offset" false "expected slide to enter judged wait state"
  | _ => passCase "slide_judge_uses_touch_panel_offset" false "expected one slide"

private def sharedTouchGroupState : InputModel.GameState :=
  let noteA1 : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 30 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 7
    , touchGroupSize := 3 }
  let noteA2 : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 31 }
    , state := .Judgeable
    , sensorPos := .A2
    , touchGroupId := some 7
    , touchGroupSize := 3 }
  let noteA3 : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 32 }
    , state := .Judgeable
    , sensorPos := .A3
    , touchGroupId := some 7
    , touchGroupSize := 3 }
  { currentTime := tp 984000
  , touchQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [noteA1] }
      else if area == .A2 then { notes := [noteA2] }
      else if area == .A3 then { notes := [noteA3] }
      else { notes := [] }) }

def test_touch_group_majority_shares_result_same_frame : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1, .A2] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame sharedTouchGroupState input
  let groupStored :=
    match nextState.touchGroupStates with
    | [group] => group.groupId == 7 && group.count == 3 && group.size == 3
    | _ => false
  match events with
  | [evt1, evt2, evt3] =>
      passCase "touch_group_majority_shares_result_same_frame"
        (groupStored
          && evt1.kind = .Touch
          && evt2.kind = .Touch
          && evt3.kind = .Touch
          && evt1.position = .sensor .A1
          && evt2.position = .sensor .A2
          && evt3.position = .sensor .A3
          && evt1.grade = evt2.grade
          && evt2.grade = evt3.grade
          && evt1.diff = evt2.diff
          && evt2.diff = evt3.diff)
        "grouped touch majority shares the judged result to the later sibling in the same frame"
  | _ => passCase "touch_group_majority_shares_result_same_frame" false "expected three touch events in one frame"

private def pendingConnChildState : InputModel.GameState :=
  let parentArea : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let childArea : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true }
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 40 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[parentArea]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 41 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 40
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentPendingFinish := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[childArea]] }
  { currentTime := tp 984000
  , slides := [parent, child] }

def test_conn_child_pending_finish_becomes_checkable : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame pendingConnChildState input
  match nextState.slides with
  | _parent :: child :: _ =>
      passCase "conn_child_pending_finish_becomes_checkable"
        child.isCheckable
        "connected child becomes checkable when parent pending-finish is already set"
  | _ => passCase "conn_child_pending_finish_becomes_checkable" false "expected parent and child slides"

private def finishedConnChildState : InputModel.GameState :=
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 42 }
    , lane := .S1
    , state := .Ended
    , length := dur 400000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , parentFinished := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  let childArea : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 43 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 42
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentFinished := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[childArea]] }
  { currentTime := tp 984000
  , slides := [parent, child] }

def test_conn_child_finished_parent_becomes_checkable : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame finishedConnChildState input
  match nextState.slides with
  | _parent :: child :: _ =>
      passCase "conn_child_finished_parent_becomes_checkable"
        child.isCheckable
        "connected child becomes checkable when parent is already finished"
  | _ => passCase "conn_child_finished_parent_becomes_checkable" false "expected parent and child slides"

private def noProgressConnSlidesState : InputModel.GameState :=
  let parentArea : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let childArea : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true }
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 44 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[parentArea]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 45 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 44
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentPendingFinish := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[childArea]] }
  { currentTime := tp 984000
  , slides := [parent, child] }

def test_conn_parent_not_force_finished_without_child_progress : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame noProgressConnSlidesState input
  match nextState.slides with
  | parent :: child :: _ =>
      passCase "conn_parent_not_force_finished_without_child_progress"
        (!parent.judgeQueues.all List.isEmpty && child.isCheckable)
        "parent stays unfinished when child merely becomes checkable without consuming"
  | _ => passCase "conn_parent_not_force_finished_without_child_progress" false "expected parent and child slides"

private def activeWifiClassicTailState : InputModel.GameState :=
  let mkLast (progress : Nat) : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, arrowProgressWhenFinished := progress }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 50 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 500000
    , startTiming := tp 500000
    , slideKind := .Wifi
    , isClassic := true
    , trackCount := 3
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , isCheckable := true
    , judgeQueues := [[mkLast 1], [mkLast 2], [mkLast 3]] }
  { currentTime := tp 984000
  , slides := [slide] }

def test_wifi_classic_tail_progress_uses_special_marker : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [.A1] (dur 16000)
  let (_, _, _, renderCmds) := Scheduler.stepFrame activeWifiClassicTailState input
  let hasExpected :=
    hasTrackProgress renderCmds 50 0 8
      && hasTrackProgress renderCmds 50 1 8
      && hasTrackProgress renderCmds 50 2 8
  passCase "wifi_classic_tail_progress_uses_special_marker"
    hasExpected
    "classic wifi uses progress marker 8 when all three tracks are down to their last segment"

private def activeWifiCenterClearedState : InputModel.GameState :=
  let sideTail : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 4, arrowProgressWhenFinished := 4 }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 51 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 500000
    , startTiming := tp 500000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , isCheckable := true
    , judgeQueues := [[sideTail], [], [sideTail]] }
  { currentTime := tp 984000
  , slides := [slide] }

def test_wifi_center_cleared_progress_uses_special_marker : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [.A1] (dur 16000)
  let (_, _, _, renderCmds) := Scheduler.stepFrame activeWifiCenterClearedState input
  let hasExpected :=
    hasTrackProgress renderCmds 51 0 9
      && hasTrackProgress renderCmds 51 1 9
      && hasTrackProgress renderCmds 51 2 9
  passCase "wifi_center_cleared_progress_uses_special_marker"
    hasExpected
    "modern wifi uses progress marker 9 when the center track is empty and both side tracks are at their last segment"

private def activeWifiCenterClearedNonTailState : InputModel.GameState :=
  let finishedLeftHead : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := false, arrowProgressWhenFinished := 6, wasOn := true, wasOff := true }
  let leftMid : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := false, arrowProgressWhenFinished := 7 }
  let leftTail : Lifecycle.SlideArea :=
    { targetAreas := [.A3], isLast := true, arrowProgressWhenFinished := 8 }
  let rightTail : Lifecycle.SlideArea :=
    { targetAreas := [.A4], isLast := true, arrowProgressWhenFinished := 9 }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 54 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 500000
    , startTiming := tp 500000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 3
    , totalJudgeQueueLen := 3
    , isCheckable := true
    , judgeQueues := [[finishedLeftHead, leftMid, leftTail], [], [rightTail]] }
  { currentTime := tp 984000
  , slides := [slide] }

def test_wifi_center_cleared_without_both_tails_uses_max_queue_marker : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (_, _, _, renderCmds) := Scheduler.stepFrame activeWifiCenterClearedNonTailState input
  let hasExpected :=
    hasTrackProgress renderCmds 54 0 7
      && hasTrackProgress renderCmds 54 1 7
      && hasTrackProgress renderCmds 54 2 7
  let rejectsSpecial :=
    !(hasTrackProgress renderCmds 54 0 9
      || hasTrackProgress renderCmds 54 1 9
      || hasTrackProgress renderCmds 54 2 9)
  passCase "wifi_center_cleared_without_both_tails_uses_max_queue_marker"
    (hasExpected && rejectsSpecial)
    "modern wifi falls back to the max-queue head marker when the center is empty but a side still has more than one segment"

private def activeWifiJudgedWaitState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 52 }
    , lane := .S1
    , state := .Judged .Perfect (dur 10000) (dur 123000)
    , length := dur 500000
    , startTiming := tp 500000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 0
    , totalJudgeQueueLen := 0
    , isCheckable := true
    , judgeQueues := [[], [], []] }
  { currentTime := secs 1
  , slides := [slide] }

def test_wifi_judged_wait_emits_delayed_event_then_hides : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (_, events, _, renderCmds) := Scheduler.stepFrame activeWifiJudgedWaitState input
  match events with
  | [evt] =>
      passCase "wifi_judged_wait_emits_delayed_event_then_hides"
        (evt.kind = .Slide
          && evt.diff = Time.fromMillis 123
          && hasHideAllSlideBars renderCmds 52)
        "wifi judged-wait preserves stored diff and emits the final slide event on expiry"
  | _ => passCase "wifi_judged_wait_emits_delayed_event_then_hides" false "expected one final slide event"

private def activeWifiJudgedWaitNotExpiredState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 55 }
    , lane := .S1
    , state := .Judged .Perfect (dur 30000) (dur 123000)
    , length := dur 500000
    , startTiming := tp 500000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 0
    , totalJudgeQueueLen := 0
    , isCheckable := true
    , judgeQueues := [[], [], []] }
  { currentTime := secs 1
  , slides := [slide] }

def test_wifi_judged_wait_before_expiry_emits_nothing : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, renderCmds) := Scheduler.stepFrame activeWifiJudgedWaitNotExpiredState input
  match nextState.slides with
  | [slide] =>
      let stillWaiting :=
        match slide.state with
        | .Judged .Perfect remaining judgeDiff => remaining = dur 14000 && judgeDiff = dur 123000
        | _ => false
      passCase "wifi_judged_wait_before_expiry_emits_nothing"
        (events.isEmpty && renderCmds.isEmpty && stillWaiting)
        "wifi judged-wait does not emit or hide before the stored wait expires"
  | _ => passCase "wifi_judged_wait_before_expiry_emits_nothing" false "expected one wifi slide to remain in judged wait"

private def activeWifiTooLateState : InputModel.GameState :=
  let unfinishedHead : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := false }
  let unfinishedTail : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 53 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , startTiming := tp 800000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , isCheckable := true
    , judgeQueues := [[unfinishedHead, unfinishedTail], [], []] }
  { currentTime := tp 1600000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_wifi_too_late_ends_immediately : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame activeWifiTooLateState input
  match nextState.slides, events with
  | [slide], [evt] =>
      let ended :=
        match slide.state with
        | .Ended => true
        | _ => false
      passCase "wifi_too_late_ends_immediately"
        (ended && evt.kind = .Slide && evt.grade = .Miss)
        "wifi too-late path emits Miss and ends immediately when more than one queue segment remains"
  | _, _ => passCase "wifi_too_late_ends_immediately" false "expected one ended wifi slide and one event"

private def activeWifiTooLateOneRemainingState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 56 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , startTiming := tp 800000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[unfinished], [], []] }
  { currentTime := tp 1600000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_wifi_too_late_one_remaining_becomes_lategood : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (_, events, _, _) := Scheduler.stepFrame activeWifiTooLateOneRemainingState input
  match events with
  | [evt] =>
      passCase "wifi_too_late_one_remaining_becomes_lategood"
        (evt.kind = .Slide && evt.grade = .LateGood)
        "wifi too-late grade is LateGood when exactly one queue segment remains"
  | _ => passCase "wifi_too_late_one_remaining_becomes_lategood" false "expected one wifi event"

private def frameZeroTapState : InputModel.GameState :=
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 60 }
    , lane := .S1
    , state := .Waiting }
  { currentTime := TimePoint.zero
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

def test_frame_zero_tap_judges_same_frame : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed frameZeroTapState batch
  match nextState.tapQueues.getD .K1 { notes := [] }, events with
  | queue, [evt] =>
      passCase "frame_zero_tap_judges_same_frame"
        (queue.currentIndex = 1 && evt.kind = .Tap && evt.noteIndex = 60)
        "tap becomes judgeable and resolves on frame zero"
  | _, _ => passCase "frame_zero_tap_judges_same_frame" false "expected one frame-zero tap event"

private def frameZeroHoldState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 61 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000 }
  { currentTime := TimePoint.zero
  , activeHolds := [(.K1, hold)]
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [hold] } else { notes := [] }) }

def test_frame_zero_hold_head_judges_same_frame : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true] }
  let (nextState, _, _, _) := Scheduler.stepFrameTimed frameZeroHoldState batch
  match nextState.activeHolds with
  | [(_, hold)] =>
      let judged :=
        match hold.state with
        | .HeadJudged _ => true
        | .BodyHeld => true
        | _ => false
      passCase "frame_zero_hold_head_judges_same_frame"
        (judged && hold.headDiff = Duration.zero && hold.params.noteIndex = 61)
        "hold head resolves from waiting on frame zero"
  | _ => passCase "frame_zero_hold_head_judges_same_frame" false "expected one active hold after frame-zero head judgment"

private def frameZeroTouchState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 62 }
    , state := .Waiting
    , sensorPos := .A1 }
  { currentTime := TimePoint.zero
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] }) }

def test_frame_zero_touch_judges_same_frame : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed frameZeroTouchState batch
  match nextState.touchQueues.getD .A1 { notes := [] }, events with
  | queue, [evt] =>
      passCase "frame_zero_touch_judges_same_frame"
        (queue.currentIndex = 1 && evt.kind = .Touch && evt.noteIndex = 62)
        "touch becomes judgeable and resolves on frame zero"
  | _, _ => passCase "frame_zero_touch_judges_same_frame" false "expected one frame-zero touch event"

def test_replay_frame_zero_tap_judges_same_frame : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := [ { timing := TimePoint.zero, slot := .S1, isBreak := false, isEX := false, noteIndex := 70 } ]
    , holds := []
    , touches := []
    , touchHolds := []
    , slides := []
    , slideSkipping := true }
  let seq : ManualTacticSequence :=
    { events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1] }
  let result := simulateChartSpecWithTactic chart seq
  match result.events with
  | [evt] =>
      let firstBatchAtZero :=
        match result.batches with
        | batch :: _ => batch.currentTime = TimePoint.zero
        | [] => false
      passCase "replay_frame_zero_tap_judges_same_frame"
        (firstBatchAtZero && evt.kind = .Tap && evt.noteIndex = 70 && evt.grade = .Perfect)
        "replay path preserves frame-zero tap judgment"
  | _ => passCase "replay_frame_zero_tap_judges_same_frame" false "expected one replay tap event"

def test_replay_frame_zero_touch_judges_same_frame : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := []
    , holds := []
    , touches := [ { timing := TimePoint.zero, sensorPos := .A1, isBreak := false, touchGroupId := none, touchGroupSize := none, noteIndex := 71 } ]
    , touchHolds := []
    , slides := []
    , slideSkipping := true }
  let seq : ManualTacticSequence :=
    { events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1] }
  let result := simulateChartSpecWithTactic chart seq
  match result.events with
  | [evt] =>
      passCase "replay_frame_zero_touch_judges_same_frame"
        (evt.kind = .Touch && evt.noteIndex = 71 && evt.grade = .Perfect)
        "replay path preserves frame-zero touch judgment"
  | _ => passCase "replay_frame_zero_touch_judges_same_frame" false "expected one replay touch event"

def test_frame_zero_slide_can_start_progress_same_frame : RuntimeCase :=
  let area : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 72 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 200000
    , startTiming := TimePoint.zero
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[area]] }
  let state : InputModel.GameState :=
    { currentTime := TimePoint.zero
    , slides := [slide] }
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let (nextState, events, audioCmds, renderCmds) := Scheduler.stepFrameTimed state batch
  match nextState.slides with
  | [nextSlide] =>
      let cleared := nextSlide.judgeQueues.all List.isEmpty
      let checkable := nextSlide.isCheckable
      let judged :=
        match nextSlide.state with
        | .Judged .Perfect waitTime judgeDiff => waitTime = Duration.zero && judgeDiff = Duration.zero
        | .Ended => true
        | _ => false
      let hasProgress :=
        renderCmds.any (fun cmd =>
          match cmd with
          | .UpdateSlideProgress noteIndex remaining => noteIndex = 72 && remaining = 0
          | _ => false)
      let hidesTrack :=
        renderCmds.any (fun cmd =>
          match cmd with
          | .HideSlideBars noteIndex trackIndex => noteIndex = 72 && trackIndex = 0
          | _ => false)
      passCase "frame_zero_slide_can_start_progress_same_frame"
        (checkable && cleared && judged && events.isEmpty && hasProgress && hidesTrack && audioCmds.isEmpty)
        "slide becomes checkable and consumes frame-zero sensor hold immediately"
  | _ => passCase "frame_zero_slide_can_start_progress_same_frame" false "expected one slide after frame-zero step"

def test_replay_slide_delays_final_event_after_internal_judged : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := []
    , holds := []
    , touches := []
    , touchHolds := []
    , slides :=
        [ { timing := TimePoint.zero
          , slot := .S1
          , length := dur 200000
          , startTiming := TimePoint.zero
          , slideKind := .Single
          , isClassic := false
          , isConnSlide := false
          , parentNoteIndex := none
          , isGroupHead := false
          , isGroupEnd := false
          , parentFinished := false
          , parentPendingFinish := false
          , totalJudgeQueueLen := 1
          , trackCount := 1
          , judgeAt := some TimePoint.zero
          , isBreak := false
          , isEX := false
          , noteIndex := 73
          , judgeQueues := [[{ targetAreas := [.A1], policy := .Or, isLast := true, isSkippable := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] } ]
    , slideSkipping := true }
  let initialState := ChartLoader.buildGameState chart
  let firstBatch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed initialState firstBatch
  let secondBatch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero + Constants.FRAME_LENGTH
    , events := [] }
  let (stateAfterSecond, secondEvents, _, _) := Scheduler.stepFrameTimed stateAfterFirst secondBatch
  let settleFrameCount := 12
  let rec advanceEmptyFrames (fuel : Nat) (state : InputModel.GameState) : InputModel.GameState × List JudgeEvent :=
    match fuel with
    | 0 => (state, [])
    | fuel + 1 =>
        let batch : InputModel.TimedInputBatch :=
          { currentTime := state.currentTime + Constants.FRAME_LENGTH
          , events := [] }
        let (nextState, events, _, _) := Scheduler.stepFrameTimed state batch
        let (finalState, restEvents) := advanceEmptyFrames fuel nextState
        (finalState, events ++ restEvents)
  let (settledState, settleEvents) := advanceEmptyFrames settleFrameCount stateAfterFirst
  let replayResult :=
    simulateChartSpecWithTactic chart { events := [InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  match stateAfterFirst.slides, stateAfterSecond.slides, settledState.slides, settleEvents, replayResult.events with
  | [firstSlide], [secondSlide], [settledSlide], [delayedEvt], [replayEvt] =>
      let preSettledJudged :=
        match firstSlide.state with
        | .Judged .Perfect waitTime judgeDiff => waitTime > Duration.zero && judgeDiff = Duration.zero
        | _ => false
      let stillJudgedNextFrame :=
        match secondSlide.state with
        | .Judged .Perfect waitTime judgeDiff => waitTime > Duration.zero && judgeDiff = Duration.zero
        | _ => false
      let settledEnded :=
        match settledSlide.state with
        | .Ended => true
        | _ => false
      passCase "replay_slide_delays_final_event_after_internal_judged"
        (firstEvents.isEmpty
          && preSettledJudged
          && secondEvents.isEmpty
          && stillJudgedNextFrame
          && settledEnded
          && delayedEvt.kind = .Slide
          && delayedEvt.noteIndex = 73
          && delayedEvt.grade = .Perfect
          && delayedEvt.diff = Duration.zero
          && replayEvt.kind = .Slide
          && replayEvt.noteIndex = 73
          && replayEvt.grade = .Perfect
          && replayEvt.diff = Duration.zero)
        "slide replay preserves internal judged state before delayed final event emission"
  | _, _, _, _, _ =>
      passCase "replay_slide_delays_final_event_after_internal_judged" false "expected pre-settle judged slide and one delayed final slide event"

private def sameLaneTapQueueState : InputModel.GameState :=
  let tap1 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 80 }
    , lane := .S1
    , state := .Waiting }
  let tap2 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 81 }
    , lane := .S1
    , state := .Waiting }
  { currentTime := TimePoint.zero
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap1, tap2] } else { notes := [] }) }

def test_same_lane_tap_queue_blocks_second_note_until_first_advances : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameLaneTapQueueState batch1
  let batch2 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero + Constants.FRAME_LENGTH
    , events := [InputModel.TimedInputEvent.buttonClick (TimePoint.zero + Constants.FRAME_LENGTH) .K1] }
  let (stateAfterSecond, secondEvents, _, _) := Scheduler.stepFrameTimed stateAfterFirst batch2
  match stateAfterFirst.tapQueues.getD .K1 { notes := [] }, firstEvents, secondEvents, stateAfterSecond.tapQueues.getD .K1 { notes := [] } with
  | queueAfterFirst, [evt1], [evt2], queueAfterSecond =>
      passCase "same_lane_tap_queue_blocks_second_note_until_first_advances"
        (evt1.noteIndex = 80
          && evt2.noteIndex = 81
          && queueAfterFirst.currentIndex = 1
          && queueAfterSecond.currentIndex = 2)
        "same-lane tap queue only unlocks the second note after the first advances"
  | _, _, _, _ =>
      passCase "same_lane_tap_queue_blocks_second_note_until_first_advances" false "expected two ordered tap events across two frames"

private def sameAreaTouchQueueState : InputModel.GameState :=
  let touch1 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 82 }
    , state := .Waiting
    , sensorPos := .A1 }
  let touch2 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 83 }
    , state := .Waiting
    , sensorPos := .A1 }
  { currentTime := TimePoint.zero
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch1, touch2] } else { notes := [] }) }

def test_same_area_touch_queue_blocks_second_note_until_first_advances : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameAreaTouchQueueState batch1
  let batch2 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero + Constants.FRAME_LENGTH
    , events := [InputModel.TimedInputEvent.sensorClick (TimePoint.zero + Constants.FRAME_LENGTH) .A1] }
  let (stateAfterSecond, secondEvents, _, _) := Scheduler.stepFrameTimed stateAfterFirst batch2
  match stateAfterFirst.touchQueues.getD .A1 { notes := [] }, firstEvents, secondEvents, stateAfterSecond.touchQueues.getD .A1 { notes := [] } with
  | queueAfterFirst, [evt1], [evt2], queueAfterSecond =>
      passCase "same_area_touch_queue_blocks_second_note_until_first_advances"
        (evt1.noteIndex = 82
          && evt2.noteIndex = 83
          && queueAfterFirst.currentIndex = 1
          && queueAfterSecond.currentIndex = 2)
        "same-area touch queue only unlocks the second note after the first advances"
  | _, _, _, _ =>
      passCase "same_area_touch_queue_blocks_second_note_until_first_advances" false "expected two ordered touch events across two frames"

private def sameLaneHoldThenTapState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 84 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000 }
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 85 }
    , lane := .S1
    , state := .Waiting }
  { currentTime := TimePoint.zero
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [hold] } else { notes := [] })
  , activeHolds := [(.K1, hold)]
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

private def sameLaneHoldWithFutureTapState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 86 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000 }
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 87 }
    , lane := .S1
    , state := .Waiting }
  { currentTime := TimePoint.zero
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [hold] } else { notes := [] })
  , activeHolds := [(.K1, hold)]
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

def test_future_same_lane_tap_head_does_not_steal_hold_click : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameLaneHoldWithFutureTapState batch1
  match firstEvents, stateAfterFirst.holdQueues.getD .K1 { notes := [] }, stateAfterFirst.tapQueues.getD .K1 { notes := [] }, stateAfterFirst.activeHolds with
  | [], holdQueueAfterFirst, tapQueueAfterFirst, [(_, holdAfterFirst)] =>
      let holdHeadJudged := match holdAfterFirst.state with | .HeadJudged .Perfect => true | _ => false
      let tapStillWaiting :=
        match tapQueueAfterFirst.peek with
        | some tapAfterFirst => match tapAfterFirst.state with | .Waiting => true | _ => false
        | none => false
      passCase "future_same_lane_tap_head_does_not_steal_hold_click"
        (holdQueueAfterFirst.currentIndex = 1
          && tapQueueAfterFirst.currentIndex = 0
          && holdHeadJudged
          && tapStillWaiting)
        "reference-style gating: a future same-lane tap head must not consume the click before it is judgeable"
  | _, _, _, _ =>
      passCase "future_same_lane_tap_head_does_not_steal_hold_click" false "expected head judgment state advance for the current hold while the future tap stayed queued"

def test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameLaneHoldThenTapState batch1
  match firstEvents, stateAfterFirst.holdQueues.getD .K1 { notes := [] }, stateAfterFirst.tapQueues.getD .K1 { notes := [] }, stateAfterFirst.activeHolds with
  | [evt1], holdQueueAfterFirst, tapQueueAfterFirst, [(_, holdAfterFirst)] =>
      let holdHeadJudgeable := match holdAfterFirst.state with | .HeadJudgeable => true | _ => false
      passCase "same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click"
        (evt1.kind = .Tap
          && evt1.noteIndex = 85
          && holdQueueAfterFirst.currentIndex = 0
          && tapQueueAfterFirst.currentIndex = 1
          && holdHeadJudgeable)
        "same-frame tap consumes the shared click; the hold head stays queued but becomes judgeable"
  | _, _, _, _ =>
      passCase "same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click" false "expected one tap event and a non-advanced hold head"

def test_reference_style_hold_head_does_not_advance_without_own_click : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameLaneHoldThenTapState batch1
  match firstEvents, stateAfterFirst.holdQueues.getD .K1 { notes := [] }, stateAfterFirst.activeHolds with
  | [evt1], holdQueueAfterFirst, [(_, holdAfterFirst)] =>
      let holdHeadJudgeable := match holdAfterFirst.state with | .HeadJudgeable => true | _ => false
      passCase "reference_style_hold_head_does_not_advance_without_own_click"
        (evt1.kind = .Tap
          && evt1.noteIndex = 85
          && holdQueueAfterFirst.currentIndex = 0
          && holdHeadJudgeable)
        "reference-style expectation: tap consumes the click and the hold head remains queued until it gets its own click"
  | _, _, _ =>
      passCase "reference_style_hold_head_does_not_advance_without_own_click" false "expected one tap event and a non-advanced hold head"

def test_same_lane_extra_click_allows_hold_head_after_tap : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [ InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true ] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameLaneHoldThenTapState batch1
  match firstEvents, stateAfterFirst.holdQueues.getD .K1 { notes := [] }, stateAfterFirst.tapQueues.getD .K1 { notes := [] }, stateAfterFirst.activeHolds with
  | [evt1], holdQueueAfterFirst, tapQueueAfterFirst, [(_, holdAfterFirst)] =>
      let holdHeadJudged := match holdAfterFirst.state with | .HeadJudged .Perfect => true | _ => false
      passCase "same_lane_extra_click_allows_hold_head_after_tap"
        (evt1.kind = .Tap
          && evt1.noteIndex = 85
          && tapQueueAfterFirst.currentIndex = 1
          && holdQueueAfterFirst.currentIndex = 1
          && holdHeadJudged)
        "with two same-frame clicks, tap consumes the first click and the hold head consumes the second"
  | _, _, _, _ =>
      passCase "same_lane_extra_click_allows_hold_head_after_tap" false "expected tap event plus a judged hold head"

private def sameAreaTouchThenTouchHoldState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 86 }
    , state := .Waiting
    , sensorPos := .A1
    , touchGroupId := some 11
    , touchGroupSize := 2 }
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 87 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1
    , touchGroupId := some 11
    , touchGroupSize := 2 }
  { currentTime := TimePoint.zero
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, touchHold)] }

def test_same_area_touch_consumes_shared_click_before_touch_hold_head : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameAreaTouchThenTouchHoldState batch1
  match firstEvents, stateAfterFirst.touchQueues.getD .A1 { notes := [] }, stateAfterFirst.touchHoldQueues.getD .A1 { notes := [] }, stateAfterFirst.activeTouchHolds with
  | [evt1], touchQueueAfterFirst, touchHoldQueueAfterFirst, [(_, holdAfterFirst)] =>
      let holdHeadJudgeable := match holdAfterFirst.state with | .HeadJudgeable => true | _ => false
      passCase "same_area_touch_consumes_shared_click_before_touch_hold_head"
        (evt1.kind = .Touch
          && evt1.noteIndex = 86
          && touchQueueAfterFirst.currentIndex = 1
          && touchHoldQueueAfterFirst.currentIndex = 0
          && holdHeadJudgeable)
        "reference-style shared touch queue: touch consumes the click and touch-hold head becomes judgeable but stays queued"
  | _, _, _, _ =>
      passCase "same_area_touch_consumes_shared_click_before_touch_hold_head" false "expected one touch event and a blocked touch-hold head"

def test_same_area_extra_click_allows_touch_hold_head_after_touch : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [ InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
                , InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1 ] }
  let (stateAfterFirst, firstEvents, _, _) := Scheduler.stepFrameTimed sameAreaTouchThenTouchHoldState batch1
  match firstEvents, stateAfterFirst.touchQueues.getD .A1 { notes := [] }, stateAfterFirst.touchHoldQueues.getD .A1 { notes := [] }, stateAfterFirst.activeTouchHolds with
  | [evt1], touchQueueAfterFirst, touchHoldQueueAfterFirst, [(_, holdAfterFirst)] =>
      let holdHeadJudged := match holdAfterFirst.state with | .HeadJudged .Perfect => true | _ => false
      passCase "same_area_extra_click_allows_touch_hold_head_after_touch"
        (evt1.kind = .Touch
          && evt1.noteIndex = 86
          && touchQueueAfterFirst.currentIndex = 1
          && touchHoldQueueAfterFirst.currentIndex = 1
          && holdHeadJudged)
        "with two same-frame touch clicks, touch consumes the first click and touch-hold head consumes the second"
  | _, _, _, _ =>
      passCase "same_area_extra_click_allows_touch_hold_head_after_touch" false "expected touch event plus a judged touch-hold head"

private def touchHoldGroupShareState : InputModel.GameState :=
  let holdA1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 88 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 12
    , touchGroupSize := 3
    , touchHoldGroupId := some 12
    , touchHoldGroupSize := 3
    , touchHoldGroupTriggered := true }
  let holdA2 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 89 }
    , start := .sensor .A2
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 12
    , touchGroupSize := 3
    , touchHoldGroupId := some 12
    , touchHoldGroupSize := 3
    , touchHoldGroupTriggered := true }
  let holdA3 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 90 }
    , start := .sensor .A3
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 12
    , touchGroupSize := 3
    , touchHoldGroupId := some 12
    , touchHoldGroupSize := 3 }
  { currentTime := tp 984000
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [holdA1] }
      else if area == .A2 then { notes := [holdA2] }
      else if area == .A3 then { notes := [holdA3] }
      else { notes := [] })
  , activeTouchHolds := [(.A1, holdA1), (.A2, holdA2), (.A3, holdA3)]
  , touchGroupStates := [{ groupId := 12, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_hold_head_can_resolve_from_shared_touch_group : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldGroupShareState input
  match events, nextState.touchHoldQueues.getD .A3 { notes := [] }, nextState.activeTouchHolds with
  | [], queueAfter, holdsAfter =>
      let resolved := holdsAfter.any (fun entry => entry.1 == .A3 && match entry.2.state with | .HeadJudged .Perfect => true | _ => false)
      passCase "touch_hold_head_can_resolve_from_shared_touch_group"
        (queueAfter.currentIndex = 1 && resolved)
        "reference-style touch-hold head can resolve from the shared touch group majority without its own click"
  | _, _, _ =>
      passCase "touch_hold_head_can_resolve_from_shared_touch_group" false "expected silent head resolution with queue advance"

private def touchThenTouchHoldGroupShareSameFrameState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 91 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 13
    , touchGroupSize := 3 }
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 92 }
    , start := .sensor .A3
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 13
    , touchGroupSize := 3
    , touchHoldGroupId := some 13
    , touchHoldGroupSize := 3 }
  { currentTime := tp 984000
  , touchQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [touch] }
      else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A3 then { notes := [touchHold] }
      else { notes := [] })
  , activeTouchHolds := [(.A3, touchHold)]
  , touchGroupStates := [{ groupId := 13, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_scheduler_policy_touch_runs_before_touch_hold_group_share : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchThenTouchHoldGroupShareSameFrameState input
  match events, nextState.touchQueues.getD .A1 { notes := [] }, nextState.touchHoldQueues.getD .A3 { notes := [] }, nextState.activeTouchHolds with
  | [evt], touchQueueAfter, touchHoldQueueAfter, holdsAfter =>
      let holdResolved := holdsAfter.any (fun entry => entry.1 == .A3 && match entry.2.state with | .HeadJudged .Perfect => true | _ => false)
      passCase "scheduler_policy_touch_runs_before_touch_hold_group_share"
        (evt.kind = .Touch
          && evt.noteIndex = 91
          && touchQueueAfter.currentIndex = 1
          && touchHoldQueueAfter.currentIndex = 1
          && holdResolved)
        "same-frame touch updates shared group state before touch-hold heads consume it"
  | _, _, _, _ =>
      passCase "scheduler_policy_touch_runs_before_touch_hold_group_share" false "expected one touch event and a silently resolved touch-hold head"

private def mixedGoldenSlideArea1 : Lifecycle.SlideArea :=
  { targetAreas := [.A5]
  , policy := .Or
  , isLast := false
  , isSkippable := true
  , arrowProgressWhenOn := 1
  , arrowProgressWhenFinished := 2 }

private def mixedGoldenSlideArea2 : Lifecycle.SlideArea :=
  { targetAreas := [.A6]
  , policy := .Or
  , isLast := true
  , isSkippable := true
  , arrowProgressWhenOn := 3
  , arrowProgressWhenFinished := 4 }

private def reducedSlide61Area1 : Lifecycle.SlideArea :=
  { targetAreas := [.C]
  , policy := .Or
  , isLast := false
  , isSkippable := true
  , arrowProgressWhenOn := 10
  , arrowProgressWhenFinished := 12 }

private def reducedSlide61Area2 : Lifecycle.SlideArea :=
  { targetAreas := [.B4]
  , policy := .Or
  , isLast := false
  , isSkippable := true
  , arrowProgressWhenOn := 13
  , arrowProgressWhenFinished := 16 }

private def reducedSlide61Area3 : Lifecycle.SlideArea :=
  { targetAreas := [.A4]
  , policy := .Or
  , isLast := true
  , isSkippable := true
  , arrowProgressWhenOn := 17
  , arrowProgressWhenFinished := 19 }

def test_reference_like_slide_skip_chain_does_not_clear_last_area_early : RuntimeCase :=
  let queue0 := [reducedSlide61Area1, reducedSlide61Area2, reducedSlide61Area3]
  let heldC := SensorVec.ofFn (fun area => area == .C)
  let heldNone := SensorVec.ofFn (fun _ => false)
  let heldB4 := SensorVec.ofFn (fun area => area == .B4)
  let queue1 := Lifecycle.replaySlideQueue queue0 heldC
  let queue2 := Lifecycle.replaySlideQueue queue1 heldNone
  let queue3 := Lifecycle.replaySlideQueue queue2 heldB4
  passCase "reference_like_slide_skip_chain_does_not_clear_last_area_early"
    (queue1.length = 3
      && queue2.length = 2
      && queue3.length = 2)
    "reference-like skippable slide chain should still leave the last area pending when only the middle area turns on"

def test_reference_like_slide_skip_chain_c_off_only_does_not_clear_all : RuntimeCase :=
  let queue0 := [reducedSlide61Area1, reducedSlide61Area2, reducedSlide61Area3]
  let heldC := SensorVec.ofFn (fun area => area == .C)
  let heldNone := SensorVec.ofFn (fun _ => false)
  let queue1 := Lifecycle.replaySlideQueue queue0 heldC
  let queue2 := Lifecycle.replaySlideQueue queue1 heldNone
  passCase "reference_like_slide_skip_chain_c_off_only_does_not_clear_all"
    (queue2.length = 2)
    "after C turns on and then off, the reference-like skip chain should leave B4 and the last A4 area pending"

private def mixedGoldenInitialState : InputModel.GameState :=
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := tp 500000, judgeOffset := Duration.zero, noteIndex := 1 }
    , lane := .S1
    , state := .Waiting }
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 2 }
    , start := .button .K2
    , state := .HeadWaiting
    , length := dur 200000 }
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := tp 1500000, judgeOffset := Duration.zero, noteIndex := 3 }
    , state := .Waiting
    , sensorPos := .A3 }
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := tp 2000000, judgeOffset := Duration.zero, noteIndex := 4 }
    , start := .sensor .A4
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 2500000, judgeOffset := Duration.zero, noteIndex := 5 }
    , lane := .S5
    , state := .Active Duration.zero
    , length := dur 200000
    , startTiming := tp 2300000
    , slideKind := .Single
    , trackCount := 1
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , judgeQueues := [[mixedGoldenSlideArea1, mixedGoldenSlideArea2]] }
  { currentTime := TimePoint.zero
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] })
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K2 then { notes := [hold] } else { notes := [] })
  , touchQueues := SensorVec.ofFn (fun area => if area == .A3 then { notes := [touch] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A4 then { notes := [touchHold] } else { notes := [] })
  , activeHolds := [(.K2, hold)]
  , activeTouchHolds := [(.A4, touchHold)]
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

private def simulateMixedGoldenSequence (seq : ManualTacticSequence) : InputModel.GameState × List JudgeEvent :=
  let replay := LnmaiCore.simulateStateWithTacticUntil mixedGoldenInitialState seq (tp 3500000)
  (replay.finalState, replay.events)

private structure MixedGoldenExpectation where
  grades : List JudgeGrade
  combo : Nat
  pCombo : Nat
  cPCombo : Nat
  dxScore : Int
  comboState : ComboState

private def mixedGoldenEventsMatchBaseShape (events : List JudgeEvent) : Bool :=
  eventKinds events = [.Tap, .Hold, .Touch, .Hold, .Slide]
    && eventNoteIndices events = [1, 2, 3, 4, 5]

private def mixedGoldenMatches
    (finalState : InputModel.GameState)
    (events : List JudgeEvent)
    (expected : MixedGoldenExpectation) : Bool :=
  mixedGoldenEventsMatchBaseShape events
    && eventGrades events = expected.grades
    && finalState.score.combo = expected.combo
    && finalState.score.pCombo = expected.pCombo
    && finalState.score.cPCombo = expected.cPCombo
    && finalState.score.dxScore = expected.dxScore
    && LnmaiCore.comboState finalState.score = expected.comboState

private def mixedGoldenAPTactic : ManualTacticSequence :=
  manual_tactic! "500000 tap K1\n1000000 tap K2\n1000000 button K2 down\n1220000 button K2 up\n1500000 touch A3\n2000000 touch A4\n2000000 sensor A4 down\n2220000 sensor A4 up\n2320000 sensor A5 down\n2400000 sensor A5 up\n2420000 sensor A6 down"

private def mixedGoldenAPTacticLateTouch : ManualTacticSequence :=
  manual_tactic! "500000 tap K1\n1000000 tap K2\n1000000 button K2 down\n1220000 button K2 up\n1660000 touch A3\n2000000 touch A4\n2000000 sensor A4 down\n2220000 sensor A4 up\n2320000 sensor A5 down\n2400000 sensor A5 up\n2420000 sensor A6 down"

def test_mixed_chart_golden_ap_plus : RuntimeCase :=
  let (finalState, events) := simulateMixedGoldenSequence mixedGoldenAPTactic
  let expected : MixedGoldenExpectation :=
    { grades := [.Perfect, .Perfect, .Perfect, .Perfect, .Perfect]
    , combo := 5
    , pCombo := 5
    , cPCombo := 5
    , dxScore := 0
    , comboState := .APPlus }
  passCase "mixed_chart_golden_ap_plus"
    (mixedGoldenMatches finalState events expected)
    "mixed replay golden keeps AP+ with perfect tap, hold, touch, touch-hold, and slide results"

def test_mixed_chart_golden_ap_with_late_touch : RuntimeCase :=
  let (finalState, events) := simulateMixedGoldenSequence mixedGoldenAPTacticLateTouch
  let expected : MixedGoldenExpectation :=
    { grades := [.Perfect, .Perfect, .LatePerfect2nd, .Perfect, .Perfect]
    , combo := 5
    , pCombo := 5
    , cPCombo := 2
    , dxScore := -1
    , comboState := .AP }
  passCase "mixed_chart_golden_ap_with_late_touch"
    (mixedGoldenMatches finalState events expected)
    "mixed replay golden drops from AP+ to AP when the touch becomes LatePerfect2nd while the rest stay perfect"

def test_frame_window_zero_delta_includes_exact_point : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := secs 1
    , events := [InputModel.TimedInputEvent.buttonClick (secs 1) .K1] }
  let input := batch.toFrameInput Duration.zero
  passCase "frame_window_zero_delta_includes_exact_point"
    (input.getButtonClickCount .K1 = 1)
    "zero-duration frames include only events exactly at currentTime"

def test_frame_window_positive_delta_excludes_left_boundary : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := secs 1
    , events := [InputModel.TimedInputEvent.buttonClick (secs 1 - dur 16000) .K1] }
  let input := batch.toFrameInput (dur 16000)
  passCase "frame_window_positive_delta_excludes_left_boundary"
    (input.getButtonClickCount .K1 = 0)
    "positive-duration frames exclude the left boundary of the window"

def test_frame_window_positive_delta_includes_inside_and_right_boundary : RuntimeCase :=
  let insideTime := secs 1 - dur 1
  let batch : InputModel.TimedInputBatch :=
    { currentTime := secs 1
    , events := [ InputModel.TimedInputEvent.buttonClick insideTime .K1
                , InputModel.TimedInputEvent.sensorClick (secs 1) .A1 ] }
  let input := batch.toFrameInput (dur 16000)
  passCase "frame_window_positive_delta_includes_inside_and_right_boundary"
    (input.getButtonClickCount .K1 = 1 && input.getSensorClickCount .A1 = 1)
    "positive-duration frames include events just inside the window and exactly at currentTime"

def test_frame_window_positive_delta_excludes_outside_window : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := secs 1
    , events := [InputModel.TimedInputEvent.sensorClick (secs 1 - dur 16001) .A1] }
  let input := batch.toFrameInput (dur 16000)
  passCase "frame_window_positive_delta_excludes_outside_window"
    (input.getSensorClickCount .A1 = 0)
    "positive-duration frames exclude events outside the left-open interval"

def test_typed_json_boundary_symbolic_only : RuntimeCase :=
  let sensorFromSymbolic : Except String SensorArea := fromJson? (Json.str "B1")
  let buttonFromSymbolic : Except String ButtonZone := fromJson? (Json.str "K1")
  let rejectedLegacySensor : Except String SensorArea := fromJson? (Json.num 25)
  let rejectedLegacyButton : Except String ButtonZone := fromJson? (Json.num 0)
  let runtimePos := RuntimePos.sensor .B1
  let runtimePosFromSymbolic : Except String RuntimePos :=
    fromJson? (Json.mkObj [("sensor", Json.str "B1")])
  let rejectedLegacyRuntimePos : Except String RuntimePos :=
    fromJson? (Json.arr #[Json.str "sensor", Json.num 25])
  passCase "typed_json_boundary_symbolic_only"
    (sensorFromSymbolic == .ok .B1
      && buttonFromSymbolic == .ok .K1
      && exceptIsError rejectedLegacySensor
      && exceptIsError rejectedLegacyButton
      && runtimePosJsonEq runtimePos (match runtimePosFromSymbolic with | .ok pos => pos | _ => .button .K1)
      && exceptIsError rejectedLegacyRuntimePos
      && toJson runtimePos == Json.mkObj [("sensor", Json.str "B1")])
    "typed JSON input and output are symbolic only"

def all : List RuntimeCase :=
  [ test_button_tap_can_use_matching_a_sensor
  , test_classic_hold_matching_a_sensor_keeps_body_pressed
  , test_modern_hold_head_miss_can_end_as_late_good
  , test_modern_hold_head_miss_skips_release_ignore_grace
  , test_modern_hold_perfect_head_keeps_release_ignore_grace
  , test_touch_hold_body_majority_reactivates_released_note
  , test_touch_hold_local_press_reactivates_released_note
  , test_classic_hold_fast_boundary_is_strict
  , test_classic_hold_late_boundary_is_strict
  , test_touch_hold_group_share_requires_strict_majority
  , test_conn_child_wifi_parent_pending_finish_becomes_checkable
  , test_wifi_too_late_two_single_tails_is_lategood_by_max_remaining
  , test_overlapping_slides_can_both_progress_from_one_sensor_hold
  , test_simultaneous_short_regular_holds_can_both_finish
  , test_chart_wrapper_short_hold_pair_after_unrelated_taps_can_finish
  , test_conn_child_progress_force_finishes_parent
  , test_slide_judge_uses_touch_panel_offset
  , test_touch_group_majority_shares_result_same_frame
  , test_conn_child_pending_finish_becomes_checkable
  , test_conn_child_finished_parent_becomes_checkable
  , test_conn_parent_not_force_finished_without_child_progress
  , test_wifi_classic_tail_progress_uses_special_marker
  , test_wifi_center_cleared_progress_uses_special_marker
  , test_wifi_center_cleared_without_both_tails_uses_max_queue_marker
  , test_wifi_judged_wait_emits_delayed_event_then_hides
  , test_wifi_judged_wait_before_expiry_emits_nothing
  , test_wifi_too_late_ends_immediately
  , test_wifi_too_late_one_remaining_becomes_lategood
  , test_frame_zero_tap_judges_same_frame
  , test_frame_zero_hold_head_judges_same_frame
  , test_frame_zero_touch_judges_same_frame
  , test_replay_frame_zero_tap_judges_same_frame
  , test_replay_frame_zero_touch_judges_same_frame
  , test_frame_zero_slide_can_start_progress_same_frame
  , test_replay_slide_delays_final_event_after_internal_judged
  , test_same_lane_tap_queue_blocks_second_note_until_first_advances
  , test_same_area_touch_queue_blocks_second_note_until_first_advances
  , test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click
  , test_future_same_lane_tap_head_does_not_steal_hold_click
  , test_reference_style_hold_head_does_not_advance_without_own_click
  , test_same_lane_extra_click_allows_hold_head_after_tap
  , test_same_area_touch_consumes_shared_click_before_touch_hold_head
  , test_same_area_extra_click_allows_touch_hold_head_after_touch
  , test_touch_hold_head_can_resolve_from_shared_touch_group
  , test_scheduler_policy_touch_runs_before_touch_hold_group_share
  , test_reference_like_slide_skip_chain_does_not_clear_last_area_early
  , test_reference_like_slide_skip_chain_c_off_only_does_not_clear_all
  , test_mixed_chart_golden_ap_plus
  , test_mixed_chart_golden_ap_with_late_touch
  , test_frame_window_zero_delta_includes_exact_point
  , test_frame_window_positive_delta_excludes_left_boundary
  , test_frame_window_positive_delta_includes_inside_and_right_boundary
  , test_frame_window_positive_delta_excludes_outside_window
  , test_typed_json_boundary_symbolic_only
  ]

def passedCount : Nat :=
  (all.filter (·.passed)).length

-- #eval all
-- #eval (passedCount, all.length)

theorem test_button_tap_can_use_matching_a_sensor_proof :
    test_button_tap_can_use_matching_a_sensor.passed = true := by native_decide

theorem test_classic_hold_matching_a_sensor_keeps_body_pressed_proof :
    test_classic_hold_matching_a_sensor_keeps_body_pressed.passed = true := by native_decide

theorem test_modern_hold_head_miss_can_end_as_late_good_proof :
    test_modern_hold_head_miss_can_end_as_late_good.passed = true := by native_decide

theorem test_modern_hold_head_miss_skips_release_ignore_grace_proof :
    test_modern_hold_head_miss_skips_release_ignore_grace.passed = true := by native_decide

theorem test_modern_hold_perfect_head_keeps_release_ignore_grace_proof :
    test_modern_hold_perfect_head_keeps_release_ignore_grace.passed = true := by native_decide

theorem test_touch_hold_body_majority_reactivates_released_note_proof :
    test_touch_hold_body_majority_reactivates_released_note.passed = true := by native_decide

theorem test_touch_hold_local_press_reactivates_released_note_proof :
    test_touch_hold_local_press_reactivates_released_note.passed = true := by native_decide

theorem test_classic_hold_fast_boundary_is_strict_proof :
    test_classic_hold_fast_boundary_is_strict.passed = true := by native_decide

theorem test_classic_hold_late_boundary_is_strict_proof :
    test_classic_hold_late_boundary_is_strict.passed = true := by native_decide

theorem test_touch_hold_group_share_requires_strict_majority_proof :
    test_touch_hold_group_share_requires_strict_majority.passed = true := by native_decide

theorem test_conn_child_wifi_parent_pending_finish_becomes_checkable_proof :
    test_conn_child_wifi_parent_pending_finish_becomes_checkable.passed = true := by native_decide

theorem test_wifi_too_late_two_single_tails_is_lategood_by_max_remaining_proof :
    test_wifi_too_late_two_single_tails_is_lategood_by_max_remaining.passed = true := by native_decide

theorem test_overlapping_slides_can_both_progress_from_one_sensor_hold_proof :
    test_overlapping_slides_can_both_progress_from_one_sensor_hold.passed = true := by native_decide

theorem test_simultaneous_short_regular_holds_can_both_finish_proof :
    test_simultaneous_short_regular_holds_can_both_finish.passed = true := by native_decide

theorem test_chart_wrapper_short_hold_pair_after_unrelated_taps_can_finish_proof :
    test_chart_wrapper_short_hold_pair_after_unrelated_taps_can_finish.passed = true := by native_decide

theorem test_conn_child_progress_force_finishes_parent_proof :
    test_conn_child_progress_force_finishes_parent.passed = true := by native_decide

theorem test_slide_judge_uses_touch_panel_offset_proof :
    test_slide_judge_uses_touch_panel_offset.passed = true := by native_decide

theorem test_touch_group_majority_shares_result_same_frame_proof :
    test_touch_group_majority_shares_result_same_frame.passed = true := by native_decide

theorem test_conn_child_pending_finish_becomes_checkable_proof :
    test_conn_child_pending_finish_becomes_checkable.passed = true := by native_decide

theorem test_conn_child_finished_parent_becomes_checkable_proof :
    test_conn_child_finished_parent_becomes_checkable.passed = true := by native_decide

theorem test_conn_parent_not_force_finished_without_child_progress_proof :
    test_conn_parent_not_force_finished_without_child_progress.passed = true := by native_decide

theorem test_wifi_classic_tail_progress_uses_special_marker_proof :
    test_wifi_classic_tail_progress_uses_special_marker.passed = true := by native_decide

theorem test_wifi_center_cleared_progress_uses_special_marker_proof :
    test_wifi_center_cleared_progress_uses_special_marker.passed = true := by native_decide

theorem test_wifi_center_cleared_without_both_tails_uses_max_queue_marker_proof :
    test_wifi_center_cleared_without_both_tails_uses_max_queue_marker.passed = true := by native_decide

theorem test_wifi_judged_wait_emits_delayed_event_then_hides_proof :
    test_wifi_judged_wait_emits_delayed_event_then_hides.passed = true := by native_decide

theorem test_wifi_judged_wait_before_expiry_emits_nothing_proof :
    test_wifi_judged_wait_before_expiry_emits_nothing.passed = true := by native_decide

theorem test_wifi_too_late_ends_immediately_proof :
    test_wifi_too_late_ends_immediately.passed = true := by native_decide

theorem test_scheduler_policy_touch_runs_before_touch_hold_group_share_proof :
    test_scheduler_policy_touch_runs_before_touch_hold_group_share.passed = true := by native_decide

theorem test_reference_like_slide_skip_chain_does_not_clear_last_area_early_proof :
    test_reference_like_slide_skip_chain_does_not_clear_last_area_early.passed = true := by native_decide

theorem test_reference_like_slide_skip_chain_c_off_only_does_not_clear_all_proof :
    test_reference_like_slide_skip_chain_c_off_only_does_not_clear_all.passed = true := by native_decide

theorem test_wifi_too_late_one_remaining_becomes_lategood_proof :
    test_wifi_too_late_one_remaining_becomes_lategood.passed = true := by native_decide

theorem test_frame_zero_tap_judges_same_frame_proof :
    test_frame_zero_tap_judges_same_frame.passed = true := by native_decide

theorem test_frame_zero_hold_head_judges_same_frame_proof :
    test_frame_zero_hold_head_judges_same_frame.passed = true := by native_decide

theorem test_frame_zero_touch_judges_same_frame_proof :
    test_frame_zero_touch_judges_same_frame.passed = true := by native_decide

theorem test_replay_frame_zero_tap_judges_same_frame_proof :
    test_replay_frame_zero_tap_judges_same_frame.passed = true := by native_decide

theorem test_replay_frame_zero_touch_judges_same_frame_proof :
    test_replay_frame_zero_touch_judges_same_frame.passed = true := by native_decide

theorem test_frame_zero_slide_can_start_progress_same_frame_proof :
    test_frame_zero_slide_can_start_progress_same_frame.passed = true := by native_decide

theorem test_replay_slide_delays_final_event_after_internal_judged_proof :
    test_replay_slide_delays_final_event_after_internal_judged.passed = true := by native_decide

theorem test_same_lane_tap_queue_blocks_second_note_until_first_advances_proof :
    test_same_lane_tap_queue_blocks_second_note_until_first_advances.passed = true := by native_decide

theorem test_same_area_touch_queue_blocks_second_note_until_first_advances_proof :
    test_same_area_touch_queue_blocks_second_note_until_first_advances.passed = true := by native_decide

theorem test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click_proof :
    test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click.passed = true := by native_decide

theorem test_future_same_lane_tap_head_does_not_steal_hold_click_proof :
    test_future_same_lane_tap_head_does_not_steal_hold_click.passed = true := by native_decide

theorem test_reference_style_hold_head_does_not_advance_without_own_click_proof :
    test_reference_style_hold_head_does_not_advance_without_own_click.passed = true := by native_decide

theorem test_same_lane_extra_click_allows_hold_head_after_tap_proof :
    test_same_lane_extra_click_allows_hold_head_after_tap.passed = true := by native_decide

theorem test_same_area_extra_click_allows_touch_hold_head_after_touch_proof :
    test_same_area_extra_click_allows_touch_hold_head_after_touch.passed = true := by native_decide

theorem test_mixed_chart_golden_ap_plus_proof :
    test_mixed_chart_golden_ap_plus.passed = true := by native_decide

theorem test_mixed_chart_golden_ap_with_late_touch_proof :
    test_mixed_chart_golden_ap_with_late_touch.passed = true := by native_decide

theorem test_frame_window_zero_delta_includes_exact_point_proof :
    test_frame_window_zero_delta_includes_exact_point.passed = true := by native_decide

theorem test_frame_window_positive_delta_excludes_left_boundary_proof :
    test_frame_window_positive_delta_excludes_left_boundary.passed = true := by native_decide

theorem test_frame_window_positive_delta_includes_inside_and_right_boundary_proof :
    test_frame_window_positive_delta_includes_inside_and_right_boundary.passed = true := by native_decide

theorem test_frame_window_positive_delta_excludes_outside_window_proof :
    test_frame_window_positive_delta_excludes_outside_window.passed = true := by native_decide

theorem test_typed_json_boundary_symbolic_only_proof :
    test_typed_json_boundary_symbolic_only.passed = true := by native_decide

end LnmaiCore.RuntimeTests
