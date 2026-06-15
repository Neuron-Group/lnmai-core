import LnmaiCore.Basic
import LnmaiCore.Storage
import LnmaiCore.Proofs.Runtime
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

private def tp (secondsMicros : ℤ) : TimePoint :=
  TimePoint.fromMicros secondsMicros

private def dur (micros : ℤ) : Duration :=
  Duration.fromMicros micros

private def secs (whole : ℤ) : TimePoint :=
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

private def eventBreakFlags (events : List JudgeEvent) : List Bool :=
  events.map (fun evt => evt.isBreak)

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

private def queueAreaGroups (queue : Lifecycle.SlideQueue) : List (List SensorArea) :=
  queue.map (fun area => area.targetAreas)

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

private def classicHoldReleaseBeforeHeadIgnoreState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 2010 }
    , start := .button .K1
    , state := .HeadJudged .Perfect
    , length := dur 400000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isClassic := true }
  { currentTime := tp 1050000
  , activeHolds := [(.K1, hold)] }

def test_classic_hold_release_before_head_ignore_ends : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame classicHoldReleaseBeforeHeadIgnoreState input
  match nextState.activeHolds, events with
  | [], [evt] =>
      passCase "classic_hold_release_before_head_ignore_ends"
        (evt.kind = .Hold && evt.grade = .FastGood && evt.noteIndex = 2010)
        "classic hold body checking starts at the tap-good early edge, not after the modern head-ignore window"
  | _, _ =>
      passCase "classic_hold_release_before_head_ignore_ends" false
        "expected released classic hold to finish before the modern head-ignore window ends"

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
  { currentTime := tp 1800000
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
        (stillHeadJudged && holdAfter.playerReleaseTime = Duration.zero && holdAfter.releaseIgnoreTime = dur 16000)
        "a normal judged head should spend release-ignore grace without adding scored release time"
  | _, _ =>
      passCase "modern_hold_perfect_head_keeps_release_ignore_grace" false "expected active hold to remain in head-judged grace state"

private def shortModernHoldHeadJudgedBeforeEndState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 178 }
    , start := .button .K1
    , state := .HeadJudged .Perfect
    , length := dur 250000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := Duration.zero
    , isClassic := false }
  { currentTime := tp 1120000
  , activeHolds := [(.K1, hold)] }

def test_short_modern_hold_does_not_force_end_before_remaining_time_zero : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame shortModernHoldHeadJudgedBeforeEndState input
  match nextState.activeHolds, events with
  | [(_, holdAfter)], [] =>
      let stillHeadJudged := match holdAfter.state with | .HeadJudged .Perfect => true | _ => false
      passCase "short_modern_hold_does_not_force_end_before_remaining_time_zero"
        stillHeadJudged
        "MajdataPlay disables body-check processing for short modern holds, so they should stay active until remaining time reaches zero"
  | _, _ =>
      passCase "short_modern_hold_does_not_force_end_before_remaining_time_zero" false "expected short modern hold to remain active before end time"

private def modernHoldPastTailIgnoreBeforeEndState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 2020 }
    , start := .button .K1
    , state := .BodyHeld
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := dur 64000
    , isClassic := false }
  { currentTime := tp 1600000
  , activeHolds := [(.K1, hold)] }

def test_modern_hold_past_tail_ignore_waits_until_remaining_time_zero : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame modernHoldPastTailIgnoreBeforeEndState input
  match nextState.activeHolds, events with
  | [(_, holdAfter)], [] =>
      let stillHeld := match holdAfter.state with | .BodyHeld => true | _ => false
      passCase "modern_hold_past_tail_ignore_waits_until_remaining_time_zero"
        (stillHeld && holdAfter.playerReleaseTime = dur 64000)
        "after the body-check window closes, MajdataPlay waits for GetRemainingTime() == 0 without adding release time"
  | _, _ =>
      passCase "modern_hold_past_tail_ignore_waits_until_remaining_time_zero" false
        "expected modern hold to remain active between tail-ignore boundary and true end"

private def modernHoldReleasedAtTrueEndState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 2021 }
    , start := .button .K1
    , state := .BodyReleased
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := dur 164000
    , isClassic := false }
  { currentTime := tp 1790000
  , activeHolds := [(.K1, hold)] }

def test_modern_hold_force_end_does_not_add_final_release_delta : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame modernHoldReleasedAtTrueEndState input
  match nextState.activeHolds, events with
  | [], [evt] =>
      passCase "modern_hold_force_end_does_not_add_final_release_delta"
        (evt.kind = .Hold && evt.grade = .LatePerfect2nd && evt.noteIndex = 2021)
        "ForceEndCheck ends at true remaining-time zero without another body-release accounting frame"
  | _, _ =>
      passCase "modern_hold_force_end_does_not_add_final_release_delta" false
        "expected one final modern hold event using the stored release time"

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

private def touchHoldBodyRawTimingWithOffsetState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := dur 100000, noteIndex := 3600 }
    , start := .sensor .A1
    , state := .HeadJudged .Perfect
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 360
    , touchHoldGroupSize := 1 }
  { currentTime := tp 1240000
  , activeTouchHolds := [(.A1, hold)] }

def test_touch_hold_body_window_uses_raw_timing_despite_judge_offset : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, events, _, _) :=
    Scheduler.stepFrame touchHoldBodyRawTimingWithOffsetState input
  let recovered :=
    nextState.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .BodyHeld => true | _ => false)
  let registeredBodyTrigger :=
    match nextState.touchHoldGroupStates with
    | [group] => group.groupId = 360 && group.triggeredNoteIndices.contains 3600
    | _ => false
  passCase "touch_hold_body_window_uses_raw_timing_despite_judge_offset"
    (events.isEmpty && recovered && registeredBodyTrigger)
    "MajdataPlay touch-hold body polling uses raw Timing, while head judgment still uses JudgeTimingWithOffset"

private def breakTapState : InputModel.GameState :=
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, isBreak := true, noteIndex := 360 }
    , lane := .S1
    , state := .Judgeable }
  { currentTime := tp 984000
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

def test_break_tap_event_preserves_family_and_counts_as_break : RuntimeCase :=
  let input := mkButtonFrameInput [.K1] [] [] [] (dur 16000)
  let (nextState, events, audioCmds, renderCmds) := Scheduler.stepFrame breakTapState input
  match events, audioCmds, renderCmds with
  | [evt], [audio], [render] =>
      let audioOk :=
        match audio with
        | .PlayJudgeSfx kind grade isBreak _ noteIndex =>
            kind = .Tap && grade = .Perfect && isBreak && noteIndex = 360
        | _ => false
      let renderOk :=
        match render with
        | .ShowJudgeResult kind grade isBreak _ noteIndex =>
            kind = .Tap && grade = .Perfect && isBreak && noteIndex = 360
        | _ => false
      passCase "break_tap_event_preserves_family_and_counts_as_break"
        (evt.kind = .Tap
          && evt.isBreak
          && audioOk
          && renderOk
          && nextState.score.counts.breakCount .Perfect = 1
          && nextState.score.counts.tapCount .Perfect = 0
          && nextState.score.earnedClassicExtra = 100
          && nextState.score.lostClassicExtra = 0
          && LnmaiCore.comboState nextState.score = .APPlus)
        "break tap events should keep tap-family identity while folding into break counters"
  | _, _, _ =>
      passCase "break_tap_event_preserves_family_and_counts_as_break" false
        "expected one judged break tap event plus break-aware audio/render commands"

private def breakHoldState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, isBreak := true, noteIndex := 361 }
    , start := .button .K1
    , state := .BodyHeld
    , length := dur 200000
    , headDiff := Duration.zero
    , headGrade := .Perfect }
  { currentTime := tp 1300000
  , activeHolds := [(.K1, hold)] }

def test_break_hold_event_preserves_family_and_counts_as_break : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, audioCmds, renderCmds) := Scheduler.stepFrame breakHoldState input
  match events, audioCmds, renderCmds, nextState.activeHolds with
  | [evt], [audio], [render], [] =>
      let audioOk :=
        match audio with
        | .PlayJudgeSfx kind grade isBreak _ noteIndex =>
            kind = .Hold && grade = .Perfect && isBreak && noteIndex = 361
        | _ => false
      let renderOk :=
        match render with
        | .ShowJudgeResult kind grade isBreak _ noteIndex =>
            kind = .Hold && grade = .Perfect && isBreak && noteIndex = 361
        | _ => false
      passCase "break_hold_event_preserves_family_and_counts_as_break"
        (evt.kind = .Hold
          && evt.isBreak
          && audioOk
          && renderOk
          && nextState.score.counts.breakCount .Perfect = 1
          && nextState.score.counts.holdCount .Perfect = 0)
        "break hold finals should keep hold-family identity while folding into break counters"
  | _, _, _, _ =>
      passCase "break_hold_event_preserves_family_and_counts_as_break" false
        "expected one final break hold event, break-aware audio/render commands, and no remaining active hold"

private def breakTouchState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, isBreak := true, noteIndex := 362 }
    , state := .Judgeable
    , sensorPos := .A1 }
  { currentTime := tp 984000
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] }) }

def test_break_touch_event_preserves_family_and_counts_as_break : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (nextState, events, audioCmds, renderCmds) := Scheduler.stepFrame breakTouchState input
  match events, audioCmds, renderCmds with
  | [evt], [audio], [render] =>
      let audioOk :=
        match audio with
        | .PlayJudgeSfx kind grade isBreak _ noteIndex =>
            kind = .Touch && grade = .Perfect && isBreak && noteIndex = 362
        | _ => false
      let renderOk :=
        match render with
        | .ShowJudgeResult kind grade isBreak _ noteIndex =>
            kind = .Touch && grade = .Perfect && isBreak && noteIndex = 362
        | _ => false
      passCase "break_touch_event_preserves_family_and_counts_as_break"
        (evt.kind = .Touch
          && evt.isBreak
          && audioOk
          && renderOk
          && nextState.score.counts.breakCount .Perfect = 1
          && nextState.score.counts.touchCount .Perfect = 0)
        "break touch events should keep touch-family identity while folding into break counters"
  | _, _, _ =>
      passCase "break_touch_event_preserves_family_and_counts_as_break" false
        "expected one judged break touch event plus break-aware audio/render commands"

private def breakSlideState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, isBreak := true, noteIndex := 363 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[unfinished]] }
  { currentTime := tp 1600000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_break_slide_event_preserves_family_and_counts_as_break : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, audioCmds, renderCmds) := Scheduler.stepFrame breakSlideState input
  match events, audioCmds with
  | [evt], [audio] =>
      let audioOk :=
        match audio with
        | .PlayJudgeSfx kind grade isBreak _ noteIndex =>
            kind = .Slide && grade = .LateGood && isBreak && noteIndex = 363
        | _ => false
      let renderOk :=
        renderCmds.any (fun render =>
          match render with
          | .ShowJudgeResult kind grade isBreak _ noteIndex =>
              kind = .Slide && grade = .LateGood && isBreak && noteIndex = 363
          | _ => false)
      passCase "break_slide_event_preserves_family_and_counts_as_break"
        (evt.kind = .Slide
          && evt.isBreak
          && evt.grade = .LateGood
          && audioOk
          && renderOk
          && nextState.score.counts.breakCount .LateGood = 1
          && nextState.score.counts.slideCount .LateGood = 0)
        "break slide too-late events should keep slide-family identity while folding into break counters"
  | _, _ =>
      passCase "break_slide_event_preserves_family_and_counts_as_break" false
        "expected one judged break slide event plus break-aware audio/render commands"

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

private def touchHoldBodyExitShrinksMajorityState : InputModel.GameState :=
  let holdA1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 380 }
    , start := .sensor .A1
    , state := .BodyReleased
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := dur 32000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 90
    , touchHoldGroupSize := 4 }
  let holdA2 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 381 }
    , start := .sensor .A2
    , state := .BodyHeld
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 90
    , touchHoldGroupSize := 4
    , touchHoldGroupTriggered := true }
  let holdA3 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 382 }
    , start := .sensor .A3
    , state := .BodyHeld
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchHoldGroupId := some 90
    , touchHoldGroupSize := 4
    , touchHoldGroupTriggered := true }
  { currentTime := tp 1300000
  , activeTouchHolds := [(.A1, holdA1), (.A2, holdA2), (.A3, holdA3)]
  , touchHoldGroupStates :=
      [{ groupId := 90, memberNoteIndices := [380, 381, 382], triggeredNoteIndices := [381, 382] }] }

def test_touch_hold_body_group_exit_shrinks_majority_denominator : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldBodyExitShrinksMajorityState input
  let recovered :=
    nextState.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .BodyHeld => true | _ => false)
  passCase "touch_hold_body_group_exit_shrinks_majority_denominator"
    (events.isEmpty && recovered)
    "body majority should use the live body-group member count after exits, not the original chart size"

private def wifiParentPendingFinishState : InputModel.GameState :=
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 40 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 500000
    , headTiming := tp 900000
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
    , headTiming := tp 900000
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
    , headTiming := tp 800000
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
    , headTiming := tp 900000
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
    , headTiming := tp 900000
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
  let hidFinalBars :=
    renderCmds.any (fun cmd =>
      match cmd with
      | .HideSlideBars 43 2 => true
      | _ => false) &&
    renderCmds.any (fun cmd =>
      match cmd with
      | .HideSlideBars 44 2 => true
      | _ => false)
  let renderedBoth :=
    hidFinalBars &&
      renderCmds.any (fun cmd =>
        match cmd with
        | .UpdateSlideProgress 43 0 => true
        | _ => false) &&
      renderCmds.any (fun cmd =>
        match cmd with
        | .UpdateSlideProgress 44 0 => true
        | _ => false)
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

private def chartBuiltSameHeadConnPair : ChartLoader.ChartSpec :=
  simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1],\n"

private def chartBuiltSameHeadConnThreePartChain : ChartLoader.ChartSpec :=
  simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1]*<7[4:1],\n"

private def fallbackDemoChartLevel6 : String :=
  "&title=Fallback Demo Chart\n&artist=\n&first=0\n&wholebpm=180\n&lv_6=?\n&inote_6=(180){16},,,,,,,,,\n{64}3qq7qq5[192#30:109],,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,{1},,,,,\nE"

def test_chart_wrapper_same_head_conn_pair_achieves_ap : RuntimeCase :=
  let tactic := defaultTacticFromChart chartBuiltSameHeadConnPair
  let result := simulateChartSpecWithTactic chartBuiltSameHeadConnPair tactic
  passCase "chart_wrapper_same_head_conn_pair_achieves_ap"
    (missingJudgedNoteIndices result = []
      && eventKinds result.events = [.Tap, .Slide]
      && eventNoteIndices result.events = [3, 2]
      && eventGrades result.events = [.Perfect, .Perfect]
      && achievesAP result)
    "MajdataPlay-style same-head connected slides still judge only the group end on the body side, but default replay now also includes the explicit lowered slide-head tap"

def test_chart_wrapper_same_head_conn_three_part_chain_achieves_ap : RuntimeCase :=
  let tactic := defaultTacticFromChart chartBuiltSameHeadConnThreePartChain
  let result := simulateChartSpecWithTactic chartBuiltSameHeadConnThreePartChain tactic
  passCase "chart_wrapper_same_head_conn_three_part_chain_achieves_ap"
    (missingJudgedNoteIndices result = []
      && eventKinds result.events = [.Tap, .Slide]
      && eventNoteIndices result.events = [4, 3]
      && eventGrades result.events = [.Perfect, .Perfect]
      && achievesAP result)
    "3-part connected-slide chains should propagate parent progress through immediate links, with one explicit lowered slide-head tap plus only the final body judgment"

def test_chart_wrapper_fallback_demo_level6_achieves_ap : RuntimeCase :=
  match defaultTacticFromChartSection fallbackDemoChartLevel6 6 with
  | .ok tactic =>
      match simulateChartSectionWithTactic fallbackDemoChartLevel6 tactic 6 with
      | .ok result =>
          passCase "chart_wrapper_fallback_demo_level6_achieves_ap"
            (missingJudgedNoteIndices result = []
              && eventKinds result.events = [.Tap, .Slide]
              && eventNoteIndices result.events = [3, 2]
              && eventGrades result.events = [.Perfect, .Perfect]
              && achievesAP result)
            "level-6 maidata fallback demo chart should parse as a connected slide chain and AP under default replay, with one explicit lowered slide-head tap plus the final body judgment"
      | .error err =>
          passCase "chart_wrapper_fallback_demo_level6_achieves_ap" false s!"unexpected simulation error: {err.message}"
  | .error err =>
      passCase "chart_wrapper_fallback_demo_level6_achieves_ap" false s!"unexpected tactic build error: {err.message}"

def test_chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap : RuntimeCase :=
  match defaultTacticFromChartSection fallbackDemoChartLevel6 6 with
  | .ok tactic =>
      let headClicks := tactic.events.filterMap (fun evt =>
        match evt with
        | .buttonClick _ zone => some zone
        | _ => none)
      passCase "chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap"
        (headClicks = [.K3])
        "MajdataPlay-style connected slide children remain headless in the default replay tactic and do not emit their own button click"
  | .error err =>
      passCase "chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap" false s!"unexpected tactic build error: {err.message}"

def test_default_tactic_does_not_infer_slide_head_from_body_metadata_alone : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := []
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , isSlideNoHead := false
         , logicalSlideId := 620
         , noteIndex := 620
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let tactic := defaultTacticFromChart chart
  let headClicks := tactic.events.filterMap (fun evt =>
    match evt with
    | .buttonClick _ zone => some zone
    | _ => none)
  passCase "default_tactic_does_not_infer_slide_head_from_body_metadata_alone"
    (headClicks = [])
    "default replay must derive slide-head taps from explicit lowered head objects, not from body-side legacy flags"

def test_default_tactic_uses_explicit_slide_head_even_if_body_compat_flag_is_headless : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, logicalSlideId := 621, noteIndex := 721 }]
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , isSlideNoHead := true
         , logicalSlideId := 621
         , noteIndex := 621
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let tactic := defaultTacticFromChart chart
  let headClicks := tactic.events.filterMap (fun evt =>
    match evt with
    | .buttonClick _ zone => some zone
    | _ => none)
  passCase "default_tactic_uses_explicit_slide_head_even_if_body_compat_flag_is_headless"
    (headClicks = [.K1])
    "explicit lowered slide-head objects remain the replay authority even if a body-side compatibility flag says headless"

def test_default_tactic_replays_head_only_lowered_slide_chart : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, logicalSlideId := 622, noteIndex := 722 }]
    , slides := [] }
  let tactic := defaultTacticFromChart chart
  let headClicks := tactic.events.filterMap (fun evt =>
    match evt with
    | .buttonClick _ zone => some zone
    | _ => none)
  let result := simulateChartSpecWithTactic chart tactic
  passCase "default_tactic_replays_head_only_lowered_slide_chart"
    (headClicks = [.K1]
      && eventKinds result.events = [.Tap]
      && eventNoteIndices result.events = [722]
      && eventGrades result.events = [.Perfect]
      && missingJudgedNoteIndices result = []
      && achievesAP result)
    "head-only lowered slide artifacts still need an explicit replayed tap from the standalone slide-head object"

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
    , headTiming := tp 600000
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
    , headTiming := secs 1
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
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 20 }
    , lane := .S1
    , state := .Active (dur 50000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
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
    | [group] => group.groupId == 7 && group.count == 2 && group.size == 3
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
        "grouped touch majority shares the result without registering the shared sibling again"
  | _ => passCase "touch_group_majority_shares_result_same_frame" false "expected three touch events in one frame"

private def touchGroupSharedConvertedGradeState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 33 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 33
    , touchGroupSize := 3 }
  { currentTime := tp (-16000)
  , judgeStyle := .Maji
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] })
  , touchGroupStates :=
      [{ groupId := 33, count := 2, size := 3, grade := .LateGood, diff := dur 180000 }] }

def test_touch_group_share_reuses_converted_grade_without_second_conversion : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (_, events, _, _) := Scheduler.stepFrame touchGroupSharedConvertedGradeState input
  match events with
  | [evt] =>
      passCase "touch_group_share_reuses_converted_grade_without_second_conversion"
        (evt.kind = .Touch
          && evt.noteIndex = 33
          && evt.grade = .LateGood
          && evt.diff = dur 180000)
        "stored touch-group grades are already converted in MajdataPlay and must not be converted again"
  | _ =>
      passCase "touch_group_share_reuses_converted_grade_without_second_conversion" false
        "expected one shared touch event"

private def touchGroupSharedTooLateState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 34 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 34
    , touchGroupSize := 3 }
  { currentTime := tp 300000
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] })
  , touchGroupStates :=
      [{ groupId := 34, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_group_share_does_not_override_too_late_miss : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 1000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchGroupSharedTooLateState input
  match nextState.touchQueues.getD .A1 { notes := [] }, events with
  | queueAfter, [evt] =>
      passCase "touch_group_share_does_not_override_too_late_miss"
        (queueAfter.currentIndex = 1
          && evt.kind = .Touch
          && evt.noteIndex = 34
          && evt.grade = .Miss
          && evt.diff = dur (-1000))
        "MajdataPlay checks touch too-late before applying shared group results"
  | _, _ =>
      passCase "touch_group_share_does_not_override_too_late_miss" false
        "expected one too-late touch miss"

private def touchGroupShareLeavesClickForTouchHoldState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 35 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 35
    , touchGroupSize := 3
    , touchQueueIndex := 0 }
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 36 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1 }
  { currentTime := tp (-16000)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, touchHold)]
  , touchGroupStates :=
      [{ groupId := 35, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_group_share_does_not_consume_sensor_click : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchGroupShareLeavesClickForTouchHoldState input
  match events, nextState.touchQueues.getD .A1 { notes := [] }, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | [evt], touchQueueAfter, holdQueueAfter, [(_, holdAfter)] =>
      let holdHeadJudged :=
        match holdAfter.state with
        | .HeadJudged .Perfect => true
        | _ => false
      passCase "touch_group_share_does_not_consume_sensor_click"
        (evt.kind = .Touch
          && evt.noteIndex = 35
          && evt.grade = .Perfect
          && touchQueueAfter.currentIndex = 1
          && holdQueueAfter.currentIndex = 1
          && holdHeadJudged)
        "a shared touch result resolves before Check(), leaving the physical click for the next touch-family head"
  | _, _, _, _ =>
      passCase "touch_group_share_does_not_consume_sensor_click" false
        "expected the shared touch event and a judged touch-hold head"

private def touchGroupShareBehindUnresolvedHeadState : InputModel.GameState :=
  let head : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 4300 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchQueueIndex := 0 }
  let shared : Lifecycle.TouchNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 4301 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 430
    , touchGroupSize := 3
    , touchQueueIndex := 1 }
  { currentTime := tp 984000
  , touchQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [head, shared] } else { notes := [] })
  , touchGroupStates :=
      [{ groupId := 430, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_group_share_resolves_non_head_without_skipping_unresolved_head : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) :=
    Scheduler.stepFrame touchGroupShareBehindUnresolvedHeadState input
  let queueAfter := nextState.touchQueues.getD .A1 { notes := [] }
  let headStillCurrent :=
    match queueAfter.peek with
    | some note =>
        note.params.noteIndex = 4300 && match note.state with | .Judgeable => true | _ => false
    | none => false
  let sharedEnded :=
    match queueAfter.notes[1]? with
    | some note =>
        note.params.noteIndex = 4301 && match note.state with | .Ended => true | _ => false
    | none => false
  match events with
  | [evt] =>
      passCase "touch_group_share_resolves_non_head_without_skipping_unresolved_head"
        (evt.kind = .Touch
          && evt.noteIndex = 4301
          && evt.grade = .Perfect
          && queueAfter.currentIndex = 0
          && nextState.touchQueueFrontiers.getD .A1 99 = 1
          && headStillCurrent
          && sharedEnded)
        "automatic touch-group resolution updates the sibling in place and advances only the shared unlock frontier"
  | _ =>
      passCase "touch_group_share_resolves_non_head_without_skipping_unresolved_head" false
        "expected only the non-head shared touch to resolve"

def test_touch_group_share_non_head_is_skipped_after_earlier_head_clears : RuntimeCase :=
  let noInput := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (stateAfterShare, firstEvents, _, _) :=
    Scheduler.stepFrame touchGroupShareBehindUnresolvedHeadState noInput
  let clickInput := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (nextState, secondEvents, _, _) := Scheduler.stepFrame stateAfterShare clickInput
  let queueAfter := nextState.touchQueues.getD .A1 { notes := [] }
  match firstEvents, secondEvents with
  | [sharedEvt], [headEvt] =>
      passCase "touch_group_share_non_head_is_skipped_after_earlier_head_clears"
        (sharedEvt.noteIndex = 4301
          && headEvt.noteIndex = 4300
          && queueAfter.currentIndex = 2
          && nextState.touchQueueFrontiers.getD .A1 99 = 2)
        "once the earlier head clears, the family queue should normalize past the sibling already resolved by shared group state"
  | _, _ =>
      passCase "touch_group_share_non_head_is_skipped_after_earlier_head_clears" false
        "expected shared sibling first, then the earlier head on its own click"

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
    , headTiming := tp 600000
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
    , headTiming := secs 1
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
    , headTiming := tp 600000
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
    , headTiming := secs 1
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
    , headTiming := tp 600000
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
    , headTiming := secs 1
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

private def chainedConnSlidesState : InputModel.GameState :=
  let mkArea (area : SensorArea) : Lifecycle.SlideArea :=
    { targetAreas := [area], isLast := true }
  let grandparent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 60 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := tp 600000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[mkArea .A1]] }
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 61 }
    , lane := .S2
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := secs 1
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 60
    , isGroupPartHead := false
    , isGroupPartEnd := false
    , parentPendingFinish := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[mkArea .A2]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1800000, judgeOffset := Duration.zero, noteIndex := 62 }
    , lane := .S3
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := tp 1400000
    , startTiming := tp 1400000
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 61
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentPendingFinish := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[mkArea .A3]] }
  { currentTime := tp 1384000
  , slides := [grandparent, parent, child] }

def test_conn_child_progress_only_force_finishes_direct_parent : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A3] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame chainedConnSlidesState input
  match nextState.slides with
  | grandparent :: parent :: child :: _ =>
      passCase "conn_child_progress_only_force_finishes_direct_parent"
        (!grandparent.judgeQueues.all List.isEmpty && parent.judgeQueues.all List.isEmpty && child.isCheckable)
        "MajdataPlay force-finishes only the direct parent when a conn child first progresses"
  | _ =>
      passCase "conn_child_progress_only_force_finishes_direct_parent" false
        "expected grandparent, parent, and child slides"

theorem conn_child_becomes_checkable_at_parent_pending_finish :
    test_conn_child_pending_finish_becomes_checkable.passed = true := by
  native_decide

theorem conn_child_becomes_checkable_at_parent_finished :
    test_conn_child_finished_parent_becomes_checkable.passed = true := by
  native_decide

theorem conn_parent_not_force_finished_without_child_progress :
    test_conn_parent_not_force_finished_without_child_progress.passed = true := by
  native_decide

theorem conn_child_progress_only_force_finishes_direct_parent :
    test_conn_child_progress_only_force_finishes_direct_parent.passed = true := by
  native_decide

private def nonEndConnSlideFinishedState : InputModel.GameState :=
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 70 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 400000
    , headTiming := tp 600000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 71 }
    , lane := .S2
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := secs 1
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 70
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentFinished := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[{ targetAreas := [.A2], isLast := true }]] }
  { currentTime := tp 1384000
  , slides := [parent, child]
  , touchPanelOffset := dur 16000 }

def test_conn_non_end_part_does_not_judge_when_finished : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame nonEndConnSlideFinishedState input
  match nextState.slides with
  | parent :: child :: _ =>
      passCase "conn_non_end_part_does_not_judge_when_finished"
        (events.all (fun evt => evt.noteIndex != 70) &&
          match parent.state with
          | .Active _ => true
          | _ => false &&
          child.isCheckable)
        "MajdataPlay only judges conn slides at group end; finished non-end parts stay non-judged"
  | _ =>
      passCase "conn_non_end_part_does_not_judge_when_finished" false
        "expected parent and child slides"

private def tooLateNonEndConnSlideState : InputModel.GameState :=
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 72 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 400000
    , headTiming := tp 600000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[{ targetAreas := [.A1], isLast := true }]] }
  { currentTime := tp 2500000
  , slides := [parent]
  , touchPanelOffset := dur 16000 }

def test_conn_non_end_part_does_not_too_late_judge : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame tooLateNonEndConnSlideState input
  match nextState.slides with
  | parent :: _ =>
      passCase "conn_non_end_part_does_not_too_late_judge"
        (events.all (fun evt => evt.noteIndex != 72) &&
          match parent.state with
          | .Active _ => true
          | _ => false)
        "MajdataPlay skips too-late judging for non-end conn parts because only group-end parts are judgable"
  | _ =>
      passCase "conn_non_end_part_does_not_too_late_judge" false
        "expected parent slide"

private def progressedConnSlidesState : InputModel.GameState :=
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 73 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := tp 600000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 74 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := secs 1
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 73
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentFinished := true
    , trackCount := 1
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , isCheckable := false
    , judgeQueues := [[{ targetAreas := [.A2], isLast := false, wasOn := true, wasOff := true }, { targetAreas := [.A3], isLast := true }]] }
  { currentTime := tp 1384000
  , slides := [parent, child] }

def test_conn_already_progressed_child_does_not_re_force_finish_parent : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, _, _, _) := Scheduler.stepFrame progressedConnSlidesState input
  match nextState.slides with
  | parent :: child :: _ =>
      passCase "conn_already_progressed_child_does_not_re_force_finish_parent"
        (parent.judgeQueues.all List.isEmpty && child.isCheckable)
        "reference force-finish is one-shot after first progress; replaying later frames keeps the parent finished without extra semantic change"
  | _ =>
      passCase "conn_already_progressed_child_does_not_re_force_finish_parent" false
        "expected parent and child slides"


private def activeWifiClassicTailState : InputModel.GameState :=
  let mkLast (progress : Nat) : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, arrowProgressWhenFinished := progress }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 50 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 500000
    , headTiming := tp 500000
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
    , headTiming := tp 500000
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
    , headTiming := tp 500000
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
    , headTiming := tp 500000
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

def test_wifi_judged_wait_crossing_zero_waits_one_more_frame : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, renderCmds) := Scheduler.stepFrame activeWifiJudgedWaitState input
  match nextState.slides with
  | [slide] =>
      let stillWaiting :=
        match slide.state with
        | .Judged .Perfect remaining judgeDiff =>
            remaining = dur (-6000) && judgeDiff = dur 123000
        | _ => false
      passCase "wifi_judged_wait_crossing_zero_waits_one_more_frame"
        (events.isEmpty && renderCmds.isEmpty && stillWaiting)
        "MajdataPlay checks LastWaitTimeSec before subtracting DeltaTime, so crossing zero this frame still waits until the next SlideCheck"
  | _ => passCase "wifi_judged_wait_crossing_zero_waits_one_more_frame" false "expected one wifi slide to remain in judged wait"

def test_wifi_judged_wait_emits_when_nonpositive_at_frame_start : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (stateAfterCrossing, _, _, _) := Scheduler.stepFrame activeWifiJudgedWaitState input
  let (nextState, events, _, renderCmds) := Scheduler.stepFrame stateAfterCrossing input
  match nextState.slides, events with
  | [slide], [evt] =>
      let ended := match slide.state with | .Ended => true | _ => false
      passCase "wifi_judged_wait_emits_when_nonpositive_at_frame_start"
        (ended
          && evt.kind = .Slide
          && evt.diff = Time.fromMillis 123
          && hasHideAllSlideBars renderCmds 52)
        "a judged slide emits only when the stored wait is already non-positive at frame start"
  | _, _ =>
      passCase "wifi_judged_wait_emits_when_nonpositive_at_frame_start" false
        "expected one final slide event after the stored wait was non-positive at frame start"

private def activeWifiJudgedWaitNotExpiredState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 55 }
    , lane := .S1
    , state := .Judged .Perfect (dur 30000) (dur 123000)
    , length := dur 500000
    , headTiming := tp 500000
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
    , headTiming := tp 800000
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
        (ended && evt.kind = .Slide && evt.grade = .Miss && evt.diff = dur (-1000))
        "wifi too-late path emits Miss with MajdataPlay's default -1ms diff and ends immediately when more than one queue segment remains"
  | _, _ => passCase "wifi_too_late_ends_immediately" false "expected one ended wifi slide and one event"

private def activeWifiTooLateOneRemainingState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 56 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
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
        (evt.kind = .Slide && evt.grade = .LateGood && evt.diff = dur (-1000))
        "wifi too-late grade is LateGood with MajdataPlay's default -1ms diff when exactly one queue segment remains"
  | _ => passCase "wifi_too_late_one_remaining_becomes_lategood" false "expected one wifi event"

private def activeSingleSlideTooLateState : InputModel.GameState :=
  let unfinishedHead : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := false }
  let unfinishedTail : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 156 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , isCheckable := true
    , judgeQueues := [[unfinishedHead, unfinishedTail]] }
  { currentTime := tp 1600000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_single_slide_too_late_two_segments_remaining_stays_miss : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame activeSingleSlideTooLateState input
  match nextState.slides, events with
  | [slide], [evt] =>
      let ended :=
        match slide.state with
        | .Ended => true
        | _ => false
      passCase "single_slide_too_late_two_segments_remaining_stays_miss"
        (ended && evt.kind = .Slide && evt.grade = .Miss && evt.diff = dur (-1000))
        "ordinary slide too-late path emits Miss with MajdataPlay's default -1ms diff and ends immediately when at least two queue segments remain"
  | _, _ => passCase "single_slide_too_late_two_segments_remaining_stays_miss" false "expected one ended ordinary slide and one event"

private def activeSingleSlideTooLateOneRemainingState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 157 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[unfinished]] }
  { currentTime := tp 1600000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_single_slide_too_late_last_segment_remaining_becomes_lategood : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (_, events, _, _) := Scheduler.stepFrame activeSingleSlideTooLateOneRemainingState input
  match events with
  | [evt] =>
      passCase "single_slide_too_late_last_segment_remaining_becomes_lategood"
        (evt.kind = .Slide && evt.grade = .LateGood && evt.diff = dur (-1000))
        "ordinary slide too-late grade is LateGood with MajdataPlay's default -1ms diff when exactly the last queue segment remains"
  | _ => passCase "single_slide_too_late_last_segment_remaining_becomes_lategood" false "expected one ordinary slide event"

private def activeSingleSlideTooLateLastSegmentHitState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 158 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[unfinished]] }
  { currentTime := tp 1600000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_single_slide_too_late_uses_pre_sensor_queue_remaining : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame activeSingleSlideTooLateLastSegmentHitState input
  match nextState.slides, events with
  | [slide], [evt] =>
      let ended := match slide.state with | .Ended => true | _ => false
      passCase "single_slide_too_late_uses_pre_sensor_queue_remaining"
        (ended && evt.kind = .Slide && evt.grade = .LateGood && evt.diff = dur (-1000))
        "MajdataPlay runs SlideCheck before SensorCheck, so TooLateJudge sees the old one-segment queue"
  | _, _ =>
      passCase "single_slide_too_late_uses_pre_sensor_queue_remaining" false
        "expected too-late slide to use pre-sensor queue remaining even when the segment is held"

def test_slide_too_late_lategood_counts_as_fast_from_default_diff : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame activeSingleSlideTooLateOneRemainingState input
  match events with
  | [evt] =>
      passCase "slide_too_late_lategood_counts_as_fast_from_default_diff"
        (evt.grade = .LateGood
          && evt.diff = dur (-1000)
          && nextState.score.fastCount = 1
          && nextState.score.lateCount = 0)
        "MajdataPlay reports slide TooLateJudge with default -1ms JudgeDiff, so its LateGood contributes to fast rather than late counters"
  | _ =>
      passCase "slide_too_late_lategood_counts_as_fast_from_default_diff" false
        "expected one ordinary slide too-late LateGood event"

theorem slide_too_late_last_segment_remaining_becomes_lategood_in_reduced_wifi_case :
    test_wifi_too_late_one_remaining_becomes_lategood.passed = true := by
  native_decide

theorem slide_too_late_two_or_more_segments_remaining_stays_miss_in_reduced_wifi_case :
    test_wifi_too_late_ends_immediately.passed = true := by
  native_decide

theorem slide_too_late_last_segment_remaining_becomes_lategood :
    test_single_slide_too_late_last_segment_remaining_becomes_lategood.passed = true := by
  native_decide

theorem slide_too_late_two_or_more_segments_remaining_stays_miss :
    test_single_slide_too_late_two_segments_remaining_stays_miss.passed = true := by
  native_decide

theorem slide_too_late_uses_pre_sensor_queue_remaining :
    test_single_slide_too_late_uses_pre_sensor_queue_remaining.passed = true := by
  native_decide

theorem slide_too_late_lategood_counts_as_fast_from_default_diff :
    test_slide_too_late_lategood_counts_as_fast_from_default_diff.passed = true := by
  native_decide

private def wifiPreCheckableState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 57 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 1200000
    , startTiming := tp 1200000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[{ targetAreas := [.A1], isLast := true }], [], []] }
  { currentTime := tp 1133000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_wifi_not_checkable_before_minus_50ms : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A1] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame wifiPreCheckableState input
  match nextState.slides with
  | [_slide] =>
      passCase "wifi_not_checkable_before_minus_50ms"
        (events = [])
        "MajdataPlay wifi becomes checkable from head timing >= -50ms, even before star movement startTiming"
  | _ =>
      passCase "wifi_not_checkable_before_minus_50ms" false "expected one wifi slide"

private def wifiAtCheckableBoundaryState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 58 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 1200000
    , startTiming := tp 1200000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[{ targetAreas := [.A1], isLast := true }], [], []] }
  { currentTime := tp 1134000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_wifi_exact_minus_50ms_becomes_checkable : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [.A1] (dur 16000)
  let (nextState, _, _, renderCmds) := Scheduler.stepFrame wifiAtCheckableBoundaryState input
  match nextState.slides with
  | [slide] =>
      let progressed :=
        hasTrackProgress renderCmds 58 0 9 && hasTrackProgress renderCmds 58 1 9 && hasTrackProgress renderCmds 58 2 9
      passCase "wifi_exact_minus_50ms_becomes_checkable"
        (slide.isCheckable && progressed)
        "MajdataPlay wifi head-time checkability boundary is inclusive at -50ms"
  | _ =>
      passCase "wifi_exact_minus_50ms_becomes_checkable" false "expected one wifi slide"

private def wifiExactTooLateBoundaryState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 59 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Wifi
    , isClassic := false
    , trackCount := 3
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[unfinished], [], []] }
  { currentTime := tp 1550000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_wifi_exact_too_late_boundary_does_not_judge : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame wifiExactTooLateBoundaryState input
  match nextState.slides with
  | [slide] =>
      passCase "wifi_exact_too_late_boundary_does_not_judge"
        (events = [] &&
          match slide.state with
          | .Active _ => true
          | _ => false)
        "MajdataPlay uses a strict `>` too-late check for wifi slides; equality is not too-late yet"
  | _ =>
      passCase "wifi_exact_too_late_boundary_does_not_judge" false "expected one wifi slide"

private def singleSlideExactTooLateBoundaryState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 159 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 200000
    , headTiming := tp 800000
    , startTiming := tp 800000
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[unfinished]] }
  { currentTime := tp 1550000
  , slides := [slide]
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

def test_single_slide_exact_too_late_boundary_does_not_judge : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame singleSlideExactTooLateBoundaryState input
  match nextState.slides with
  | [slide] =>
      passCase "single_slide_exact_too_late_boundary_does_not_judge"
        (events = [] &&
          match slide.state with
          | .Active _ => true
          | _ => false)
        "ordinary slide too-late uses a strict `>` boundary; equality is not too-late yet"
  | _ =>
      passCase "single_slide_exact_too_late_boundary_does_not_judge" false "expected one ordinary slide"

theorem wifi_center_cleared_uses_special_progress_marker :
    test_wifi_center_cleared_progress_uses_special_marker.passed = true := by
  native_decide

theorem wifi_center_cleared_without_both_tails_uses_max_remaining_progress :
    test_wifi_center_cleared_without_both_tails_uses_max_queue_marker.passed = true := by
  native_decide

theorem wifi_max_remaining_one_implies_lategood :
    test_wifi_too_late_one_remaining_becomes_lategood.passed = true := by
  native_decide

theorem wifi_head_checkability_boundary_excludes_before_minus_50ms :
    test_wifi_not_checkable_before_minus_50ms.passed = true := by
  native_decide

theorem wifi_head_checkability_boundary_includes_exact_minus_50ms :
    test_wifi_exact_minus_50ms_becomes_checkable.passed = true := by
  native_decide

theorem wifi_exact_too_late_boundary_preserved :
    test_wifi_exact_too_late_boundary_does_not_judge.passed = true := by
  native_decide

theorem slide_exact_too_late_boundary_preserved :
    test_single_slide_exact_too_late_boundary_does_not_judge.passed = true := by
  native_decide

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

private def waitingTouchLargeDeltaState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 177 }
    , state := .Waiting
    , sensorPos := .A1 }
  { currentTime := tp (-200000)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] }) }

def test_touch_waiting_large_delta_uses_reference_too_late_boundary : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := tp 301000
    , events := [] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed waitingTouchLargeDeltaState batch
  match nextState.touchQueues.getD .A1 { notes := [] }, events with
  | queue, [evt] =>
      passCase "touch_waiting_large_delta_uses_reference_too_late_boundary"
        (queue.currentIndex = 1
          && evt.kind = .Touch
          && evt.noteIndex = 177
          && evt.grade = .Miss
          && evt.diff = dur (-1000))
        "a touch that stays in Waiting across a large frame jump should miss once time is strictly past the reference good boundary"
  | _, _ => passCase "touch_waiting_large_delta_uses_reference_too_late_boundary" false "expected one touch miss event after large-delta waiting step"

private def frameZeroTouchHoldState : InputModel.GameState :=
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 63 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  { currentTime := TimePoint.zero
  , touchQueueFrontiers := SensorVec.ofFn (fun area => if area == .A1 then 0 else 0)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, touchHold)] }

def test_frame_zero_touch_hold_head_judges_same_frame : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
                , InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let (nextState, _, _, _) := Scheduler.stepFrameTimed frameZeroTouchHoldState batch
  match nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | holdQueue, [(_, hold)] =>
      let judged :=
        match hold.state with
        | .HeadJudged _ => true
        | .BodyHeld => true
        | _ => false
      passCase "frame_zero_touch_hold_head_judges_same_frame"
        (nextState.touchQueueFrontiers.getD .A1 0 = 1 && holdQueue.currentIndex = 1 && judged && hold.headDiff = Duration.zero && hold.params.noteIndex = 63)
        "touch-hold head resolves from waiting on frame zero when its shared touch queue is already current"
  | _, _ => passCase "frame_zero_touch_hold_head_judges_same_frame" false "expected one active touch-hold after frame-zero head judgment"

private def touchIgnoresOuterButtonState : InputModel.GameState :=
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 64 }
    , state := .Judgeable
    , sensorPos := .A1 }
  { currentTime := TimePoint.zero
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] })
  , touchPanelOffset := dur 100000 }

def test_touch_ignores_outer_button_without_sensor_input : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed touchIgnoresOuterButtonState batch
  match nextState.touchQueues.getD .A1 { notes := [] }, events with
  | queue, [] =>
      passCase "touch_ignores_outer_button_without_sensor_input"
        (queue.currentIndex = 0)
        "core touch judgment should require sensor input; desktop button mapping belongs before core input framing"
  | _, _ => passCase "touch_ignores_outer_button_without_sensor_input" false "expected no touch event from an outer button alone"

private def tooLateTapLeavesClickState : InputModel.GameState :=
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 66 }
    , lane := .S1
    , state := .Judgeable }
  { currentTime := tp 151000
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

def test_too_late_tap_does_not_consume_button_click : RuntimeCase :=
  let input := mkButtonFrameInput [.K1] [] [] [] (dur 1000)
  let (nextState, events, _, _) := Scheduler.stepFrame tooLateTapLeavesClickState input
  match events, nextState.tapQueues.getD .K1 { notes := [] } with
  | [evt], queueAfter =>
      let missed := evt.kind = .Tap && evt.noteIndex = 66 && evt.grade = .Miss
      passCase "too_late_tap_does_not_consume_button_click"
        (missed && queueAfter.currentIndex = 1)
        "a too-late tap should miss before click consumption and leave the lane for later notes"
  | _, _ =>
      passCase "too_late_tap_does_not_consume_button_click" false "expected one tap miss without extra consumption"

private def tooLateTouchLeavesClickState : InputModel.GameState :=
  let lateTouch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 68 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchQueueIndex := 0 }
  let clickedTouchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := tp 301000, judgeOffset := Duration.zero, noteIndex := 69 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  { currentTime := tp 300000
  , touchQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [lateTouch] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [clickedTouchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, clickedTouchHold)] }

def test_too_late_touch_does_not_consume_sensor_click : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 1000)
  let (nextState, events, _, _) := Scheduler.stepFrame tooLateTouchLeavesClickState input
  match events, nextState.touchQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | [missEvt], queueAfter, [(_, holdAfter)] =>
      let holdClicked :=
        match holdAfter.state with
        | .HeadJudged .Perfect => holdAfter.headDiff = Duration.zero
        | _ => false
      passCase "too_late_touch_does_not_consume_sensor_click"
        (missEvt.kind = .Touch
          && missEvt.noteIndex = 68
          && missEvt.grade = .Miss
          && holdClicked
          && queueAfter.currentIndex = 1)
        "a too-late touch should miss before Check() and leave the sensor click for a same-frame touch-hold head"
  | _, _, _ =>
      passCase "too_late_touch_does_not_consume_sensor_click" false "expected a touch miss followed by a clicked touch-hold head"

private def holdReleaseGraceState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 67 }
    , start := .button .K1
    , state := .BodyHeld
    , length := dur 800000
    , headDiff := Duration.zero
    , headGrade := .Perfect
    , playerReleaseTime := Duration.zero
    , releaseIgnoreTime := dur 16000
    , isClassic := false }
  { currentTime := tp 1300000
  , activeHolds := [(.K1, hold)] }

def test_modern_hold_release_grace_does_not_count_toward_release_time : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame holdReleaseGraceState input
  match nextState.activeHolds, events with
  | [(_, holdAfter)], [] =>
      let stillGrace := match holdAfter.state with | .BodyHeld => true | _ => false
      passCase "modern_hold_release_grace_does_not_count_toward_release_time"
        (stillGrace && holdAfter.playerReleaseTime = Duration.zero && holdAfter.releaseIgnoreTime = dur 32000)
        "release grace should not add to scored release time on the frame it is still expiring"
  | _, _ =>
      passCase "modern_hold_release_grace_does_not_count_toward_release_time" false "expected a released hold to stay active without ending yet"

private def touchHoldIgnoresOuterButtonState : InputModel.GameState :=
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 65 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  { currentTime := TimePoint.zero
  , touchQueueFrontiers := SensorVec.ofFn (fun area => if area == .A1 then 0 else 0)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, touchHold)]
  , touchPanelOffset := dur 100000 }

def test_touch_hold_head_ignores_outer_button_without_sensor_input : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed touchHoldIgnoresOuterButtonState batch
  match nextState.touchQueues.getD .A1 { notes := [] }, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds, events with
  | touchQueue, holdQueue, [(_, hold)], [] =>
      let stillWaiting := match hold.state with | .HeadJudgeable => true | _ => false
      passCase "touch_hold_head_ignores_outer_button_without_sensor_input"
        (nextState.touchQueueFrontiers.getD .A1 0 = 0 && touchQueue.currentIndex = 0 && holdQueue.currentIndex = 0 && stillWaiting)
        "core touch-hold head judgment should require sensor input; desktop button mapping belongs before core input framing"
  | _, _, _, _ => passCase "touch_hold_head_ignores_outer_button_without_sensor_input" false "expected no touch-hold head event from an outer button alone"

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
    , touches := [ { timing := TimePoint.zero, sensorPos := .A1, isBreak := false, sourceGroupId := none, sourceGroupIndex := none, sourceGroupSize := none, touchGroupId := none, touchGroupSize := none, noteIndex := 71 } ]
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

def test_replay_frame_zero_touch_hold_head_judges_same_frame : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := []
    , holds := []
    , touches := []
    , touchHolds := [ { timing := TimePoint.zero
                      , sensorPos := .A1
                      , length := dur 200000
                      , isBreak := false
                      , isEX := false
                      , sourceGroupId := none
                      , sourceGroupIndex := none
                      , sourceGroupSize := none
                      , touchQueueIndex := 0
                      , touchGroupId := none
                      , touchGroupSize := none
                      , touchHoldGroupId := none
                      , touchHoldGroupSize := none
                      , noteIndex := 72 } ]
    , slides := []
    , slideSkipping := true }
  let seq : ManualTacticSequence :=
    { events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
                , InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let result := simulateChartSpecWithTactic chart seq
  match result.events with
  | [evt] =>
      let firstBatchAtZero :=
        match result.batches with
        | batch :: _ => batch.currentTime = TimePoint.zero
        | [] => false
      passCase "replay_frame_zero_touch_hold_head_judges_same_frame"
        (firstBatchAtZero && evt.kind = .Hold && evt.noteIndex = 72 && evt.grade = .Perfect)
        "replay path preserves frame-zero touch-hold head judgment"
  | _ => passCase "replay_frame_zero_touch_hold_head_judges_same_frame" false "expected one replay touch-hold event"

def test_build_game_state_groups_touch_each_batch_locally : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := []
    , holds := []
    , touches :=
        [ { timing := TimePoint.zero, sensorPos := .A1, isBreak := false, sourceGroupId := some 10, sourceGroupIndex := some 0, sourceGroupSize := some 2, noteIndex := 301 }
        , { timing := TimePoint.zero, sensorPos := .A2, isBreak := false, sourceGroupId := some 10, sourceGroupIndex := some 1, sourceGroupSize := some 2, noteIndex := 302 }
        , { timing := secs 2, sensorPos := .A5, isBreak := false, sourceGroupId := none, sourceGroupIndex := none, sourceGroupSize := none, noteIndex := 303 } ]
    , touchHolds := []
    , slides := []
    , slideSkipping := true }
  let state := ChartLoader.buildGameState chart
  match state.touchQueues.getD .A1 { notes := [] }, state.touchQueues.getD .A2 { notes := [] }, state.touchQueues.getD .A5 { notes := [] } with
  | qA1, qA2, qA5 =>
      match qA1.peek, qA2.peek, qA5.peek with
      | some a1, some a2, some a5 =>
          passCase "build_game_state_groups_touch_each_batch_locally"
            (a1.touchGroupId.isSome
              && a1.touchGroupId = a2.touchGroupId
              && a1.touchGroupSize = 2
              && a5.touchGroupId = none
              && a5.touchGroupSize = 1)
            "only timing-local each touches should receive a shared touch group"
      | _, _, _ =>
          passCase "build_game_state_groups_touch_each_batch_locally" false "expected A1, A2, and A5 touch queue heads"

def test_build_game_state_groups_touch_hold_body_locally : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := []
    , holds := []
    , touches := []
    , touchHolds :=
        [ { timing := TimePoint.zero, sensorPos := .A1, length := dur 200000, isBreak := false, isEX := false, sourceGroupId := some 20, sourceGroupIndex := some 0, sourceGroupSize := some 2, noteIndex := 311 }
        , { timing := TimePoint.zero, sensorPos := .A2, length := dur 200000, isBreak := false, isEX := false, sourceGroupId := some 20, sourceGroupIndex := some 1, sourceGroupSize := some 2, noteIndex := 312 }
        , { timing := secs 2, sensorPos := .A5, length := dur 200000, isBreak := false, isEX := false, sourceGroupId := none, sourceGroupIndex := none, sourceGroupSize := none, noteIndex := 313 } ]
    , slides := []
    , slideSkipping := true }
  let state := ChartLoader.buildGameState chart
  match state.touchHoldQueues.getD .A1 { notes := [] }, state.touchHoldQueues.getD .A2 { notes := [] }, state.touchHoldQueues.getD .A5 { notes := [] } with
  | qA1, qA2, qA5 =>
      match qA1.peek, qA2.peek, qA5.peek with
      | some a1, some a2, some a5 =>
          passCase "build_game_state_groups_touch_hold_body_locally"
            (a1.touchGroupId.isSome
              && a1.touchGroupId = a2.touchGroupId
              && a1.touchGroupSize = 2
              && a1.touchHoldGroupId.isSome
              && a1.touchHoldGroupId = a2.touchHoldGroupId
              && a1.touchHoldGroupSize = 2
              && a5.touchGroupId = none
              && a5.touchHoldGroupId = none)
            "touch-hold each batches should get timing-local touch share and timing-local body-share groups only"
      | _, _, _ =>
          passCase "build_game_state_groups_touch_hold_body_locally" false "expected A1, A2, and A5 touch-hold queue heads"

private def sameSlotBriefGapHoldChainChart : ChartLoader.ChartSpec :=
  { taps := []
  , holds :=
      [ { timing := tp 1000000, slot := .S1, length := dur 400000, noteIndex := 300 }
      , { timing := tp 1401000, slot := .S1, length := dur 400000, noteIndex := 301 }
      , { timing := tp 1802000, slot := .S1, length := dur 400000, noteIndex := 302 } ]
  , touches := []
  , touchHolds := []
  , slides := []
  , slideSkipping := true }

private def sameSlotBriefGapHoldChainButtonHeldSensorClicks : ManualTacticSequence :=
  mkManualTacticSequence
    [ holdButtonAt 1000000 .K1 true
    , touchAt 1000000 .A1
    , touchAt 1401000 .A1
    , touchAt 1802000 .A1
    , holdButtonAt 2202000 .K1 false ]

private def sameSlotBriefGapHoldChainSensorHeldButtonClicks : ManualTacticSequence :=
  mkManualTacticSequence
    [ holdSensorAt 1000000 .A1 true
    , tapAt 1000000 .K1
    , tapAt 1401000 .K1
    , tapAt 1802000 .K1
    , holdSensorAt 2202000 .A1 false ]

def test_same_slot_brief_gap_hold_chain_button_held_sensor_clicks_achieves_ap : RuntimeCase :=
  let result :=
    simulateChartSpecWithTactic
      sameSlotBriefGapHoldChainChart
      sameSlotBriefGapHoldChainButtonHeldSensorClicks
  passCase "same_slot_brief_gap_hold_chain_button_held_sensor_clicks_achieves_ap"
    (missingJudgedNoteIndices result = []
      && eventNoteIndices result.events = [300, 301, 302]
      && eventGrades result.events = [.Perfect, .Perfect, .Perfect]
      && achievesAP result)
    "for a same-slot hold chain with 1ms gaps, holding the outer button continuously and clicking the matching inner sensor at each head should AP"

def test_same_slot_brief_gap_hold_chain_sensor_held_button_clicks_achieves_ap : RuntimeCase :=
  let result :=
    simulateChartSpecWithTactic
      sameSlotBriefGapHoldChainChart
      sameSlotBriefGapHoldChainSensorHeldButtonClicks
  passCase "same_slot_brief_gap_hold_chain_sensor_held_button_clicks_achieves_ap"
    (missingJudgedNoteIndices result = []
      && eventNoteIndices result.events = [300, 301, 302]
      && eventGrades result.events = [.Perfect, .Perfect, .Perfect]
      && achievesAP result)
    "the swapped assignment also APs: hold the inner sensor continuously and click the matching outer button at each hold head"

def test_frame_zero_slide_can_start_progress_same_frame : RuntimeCase :=
  let area : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 72 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 200000
    , headTiming := TimePoint.zero
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
      let stillActive :=
        match nextSlide.state with
        | .Active waitTime => waitTime = Duration.zero
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
        (checkable && cleared && stillActive && events.isEmpty && hasProgress && hidesTrack && audioCmds.isEmpty)
        "slide becomes checkable and consumes frame-zero sensor hold immediately, but MajdataPlay's SlideCheck observes the cleared queue next frame"
  | _ => passCase "frame_zero_slide_can_start_progress_same_frame" false "expected one slide after frame-zero step"

theorem slide_frame_zero_becomes_checkable_and_progresses_same_frame :
    test_frame_zero_slide_can_start_progress_same_frame.passed = true := by
  native_decide

def test_slide_cleared_queue_enters_judged_on_next_frame : RuntimeCase :=
  let area : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 720 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 200000
    , headTiming := TimePoint.zero
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
  let firstBatch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let (stateAfterClear, firstEvents, _, _) := Scheduler.stepFrameTimed state firstBatch
  let secondBatch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero + Constants.FRAME_LENGTH
    , events := [] }
  let (stateAfterJudge, secondEvents, _, _) := Scheduler.stepFrameTimed stateAfterClear secondBatch
  match stateAfterClear.slides, stateAfterJudge.slides with
  | [clearedSlide], [judgedSlide] =>
      let clearedButActive :=
        clearedSlide.judgeQueues.all List.isEmpty &&
          match clearedSlide.state with
          | .Active waitTime => waitTime = Duration.zero
          | _ => false
      let judgedNextFrame :=
        match judgedSlide.state with
        | .Judged .Perfect waitTime judgeDiff =>
            waitTime = Duration.zero && judgeDiff = Constants.FRAME_LENGTH
        | _ => false
      passCase "slide_cleared_queue_enters_judged_on_next_frame"
        (firstEvents.isEmpty && secondEvents.isEmpty && clearedButActive && judgedNextFrame)
        "MajdataPlay's SensorCheck can clear the queue this frame, but SlideCheck judges that cleared queue on the next frame"
  | _, _ =>
      passCase "slide_cleared_queue_enters_judged_on_next_frame" false
        "expected the cleared slide to remain active for one frame and enter judged on the next"

def test_replay_slide_delays_final_event_after_internal_judged : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { taps := []
    , holds := []
    , touches := []
    , touchHolds := []
    , slides :=
        [ { headTiming := TimePoint.zero
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
  let settleFrameCount := 14
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
      let firstClearedButActive :=
        match firstSlide.state with
        | .Active _ => firstSlide.judgeQueues.all List.isEmpty
        | _ => false
      let stillJudgedNextFrame :=
        match secondSlide.state with
        | .Judged .Perfect waitTime judgeDiff =>
            waitTime > Duration.zero && judgeDiff = Constants.FRAME_LENGTH
        | _ => false
      let settledEnded :=
        match settledSlide.state with
        | .Ended => true
        | _ => false
      passCase "replay_slide_delays_final_event_after_internal_judged"
        (firstEvents.isEmpty
          && firstClearedButActive
          && secondEvents.isEmpty
          && stillJudgedNextFrame
          && settledEnded
          && delayedEvt.kind = .Slide
          && delayedEvt.noteIndex = 73
          && delayedEvt.grade = .Perfect
          && delayedEvt.diff = Constants.FRAME_LENGTH
          && replayEvt.kind = .Slide
          && replayEvt.noteIndex = 73
          && replayEvt.grade = .Perfect
          && replayEvt.diff = Constants.FRAME_LENGTH)
        "slide replay preserves internal judged state before delayed final event emission"
  | _, _, _, _, _ =>
      passCase "replay_slide_delays_final_event_after_internal_judged" false "expected pre-settle judged slide and one delayed final slide event"

private def finishedModernLateSlideState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 74 }
    , lane := .S1
    , state := .Active (dur 300000)
    , length := dur 500000
    , headTiming := tp 500000
    , startTiming := tp 500000
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  { currentTime := tp 1584012
  , slides := [slide] }

def test_modern_slide_late_good_clamps_judged_wait_to_50ms : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame finishedModernLateSlideState input
  match nextState.slides with
  | [slide] =>
      let clamped :=
        match slide.state with
        | .Judged .LateGood waitTime judgeDiff =>
            waitTime = Constants.SLIDE_JUDGED_LATE_CLEAR_WAIT && judgeDiff = dur 600012
        | _ => false
      passCase "modern_slide_late_good_clamps_judged_wait_to_50ms"
        (events.isEmpty && clamped)
        "MajdataPlay sets LastWaitTimeSec to 50ms for modern late-good-or-worse slide clears"
  | _ => passCase "modern_slide_late_good_clamps_judged_wait_to_50ms" false "expected one judged slide"

private def finishedModernMajiLateGreatSlideState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 77 }
    , lane := .S1
    , state := .Active Duration.zero
    , length := dur 1000000
    , headTiming := secs 1
    , startTiming := secs 1
    , slideKind := .Single
    , isClassic := false
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  { currentTime := tp 1284000
  , judgeStyle := .Maji
  , slides := [slide] }

def test_modern_slide_maji_reconverts_stored_judge_result_at_end : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (stateAfterJudged, firstEvents, _, _) :=
    Scheduler.stepFrame finishedModernMajiLateGreatSlideState input
  let (stateAfterEnd, secondEvents, _, _) := Scheduler.stepFrame stateAfterJudged input
  match stateAfterJudged.slides, stateAfterEnd.slides, firstEvents, secondEvents with
  | [judgedSlide], [endedSlide], [], [evt] =>
      let storedConverted :=
        match judgedSlide.state with
        | .Judged .LateGood waitTime judgeDiff =>
            waitTime = Duration.zero && judgeDiff = dur 300000
        | _ => false
      let ended :=
        match endedSlide.state with
        | .Ended => true
        | _ => false
      passCase "modern_slide_maji_reconverts_stored_judge_result_at_end"
        (storedConverted
          && ended
          && evt.kind = .Slide
          && evt.noteIndex = 77
          && evt.grade = .Miss
          && evt.diff = dur 300000)
        "modern slide stores the converted JudgeResult, then End applies MajdataPlay conversion again"
  | _, _, _, _ =>
      passCase "modern_slide_maji_reconverts_stored_judge_result_at_end" false
        "expected one internally judged slide followed by one final slide event"

private def finishedClassicLateSlideState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 75 }
    , lane := .S1
    , state := .Active (dur 300000)
    , length := dur 500000
    , headTiming := tp 500000
    , startTiming := tp 500000
    , slideKind := .Single
    , isClassic := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  { currentTime := tp 1584012
  , slides := [slide] }

def test_classic_slide_late_clear_keeps_existing_judged_wait : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame finishedClassicLateSlideState input
  match nextState.slides with
  | [slide] =>
      let kept :=
        match slide.state with
        | .Judged .LateGood waitTime judgeDiff => waitTime = dur 300000 && judgeDiff = dur 600012
        | _ => false
      passCase "classic_slide_late_clear_keeps_existing_judged_wait"
        (events.isEmpty && kept)
        "MajdataPlay's classic slide JudgeClassic only applies the early-start wait adjustment"
  | _ => passCase "classic_slide_late_clear_keeps_existing_judged_wait" false "expected one judged slide"

private def finishedConnEndEarlyAgainstGroupStartState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 850000, judgeOffset := Duration.zero, noteIndex := 76 }
    , lane := .S1
    , state := .Active (dur 400000)
    , length := dur 500000
    , headTiming := TimePoint.zero
    , startTiming := tp 500000
    , groupStartTiming := some (secs 1)
    , slideKind := .ConnPart
    , isClassic := false
    , isConnSlide := true
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentFinished := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , judgeQueues := [[]] }
  { currentTime := tp 884000
  , slides := [slide] }

def test_conn_slide_early_clear_uses_group_start_for_judged_wait : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame finishedConnEndEarlyAgainstGroupStartState input
  match nextState.slides with
  | [slide] =>
      let adjusted :=
        match slide.state with
        | .Judged .Perfect waitTime judgeDiff =>
            waitTime = Constants.SLIDE_JUDGED_LATE_CLEAR_WAIT && judgeDiff = dur 50000
        | _ => false
      passCase "conn_slide_early_clear_uses_group_start_for_judged_wait"
        (events.isEmpty && adjusted)
        "connected slides mirror MajdataPlay's ConnectInfo.StartTiming early-start wait adjustment"
  | _ => passCase "conn_slide_early_clear_uses_group_start_for_judged_wait" false "expected one judged conn slide"

private def sameLaneTapQueueState : InputModel.GameState :=
  let tap1 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 80 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 0 }
  let tap2 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 81 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 1 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K1 then 0 else 0)
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

def test_same_lane_tap_queue_consumes_multiple_same_frame_clicks : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events :=
        [ InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
        , InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1 ] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed sameLaneTapQueueState batch
  let queueAfter := nextState.tapQueues.getD .K1 { notes := [] }
  let judged80 :=
    events.any (fun evt => evt.kind = .Tap && evt.noteIndex = 80 && evt.grade = .Perfect)
  let judged81 :=
    events.any (fun evt => evt.kind = .Tap && evt.noteIndex = 81 && evt.grade = .Perfect)
  passCase "same_lane_tap_queue_consumes_multiple_same_frame_clicks"
    (events.length = 2
      && judged80
      && judged81
      && queueAfter.currentIndex = 2
      && nextState.buttonQueueFrontiers.getD .K1 99 = 2)
    "same-lane tap-family queues can recursively advance through multiple same-frame clicks"

private def threeSameLaneTapQueueState : InputModel.GameState :=
  let tap1 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 180 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 0 }
  let tap2 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 181 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 1 }
  let tap3 : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 182 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 2 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K1 then 0 else 0)
  , tapQueues := ButtonVec.ofFn (fun zone =>
      if zone == .K1 then { notes := [tap1, tap2, tap3] } else { notes := [] }) }

def test_same_lane_tap_recursion_stops_when_clicks_exhausted : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events :=
        [ InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
        , InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1 ] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed threeSameLaneTapQueueState batch
  let queueAfter := nextState.tapQueues.getD .K1 { notes := [] }
  let thirdStillWaiting :=
    match queueAfter.peek with
    | some note =>
        let waiting := match note.state with | .Waiting => true | _ => false
        note.params.noteIndex = 182 && waiting
    | none => false
  passCase "same_lane_tap_recursion_stops_when_clicks_exhausted"
    (events.length = 2
      && queueAfter.currentIndex = 2
      && nextState.buttonQueueFrontiers.getD .K1 99 = 2
      && thirdStillWaiting)
    "tap recursion should not apply a no-input semantic step to the next queued head"

def test_build_game_state_routes_slide_head_into_tap_queue : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, logicalSlideId := 410, noteIndex := 510 }]
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , logicalSlideId := 410
         , noteIndex := 410
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let state := ChartLoader.buildGameState chart
  match state.tapQueues.getD .K1 { notes := [] } with
  | queue =>
      match queue.notes with
      | [.slideHead note] =>
          passCase "build_game_state_routes_slide_head_into_tap_queue"
            (note.params.noteIndex = 510 && note.lane = .S1 && queue.currentIndex = 0)
            "explicit lowered slide heads load through the shared tap-family button queue as dedicated slide-head runtime notes"
      | _ =>
          passCase "build_game_state_routes_slide_head_into_tap_queue" false "expected one tap-family queue entry for the slide head"

def test_game_state_json_preserves_tap_family_kind_for_slide_head : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, logicalSlideId := 410, noteIndex := 510 }]
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , logicalSlideId := 410
         , noteIndex := 410
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let state := ChartLoader.buildGameState chart
  let encoded := toJson state
  let decoded : Except String InputModel.GameState :=
    match fromJson? (α := InputModel.GameState) encoded with
    | .ok value => .ok value
    | .error err => .error err
  passCase "game_state_json_preserves_tap_family_kind_for_slide_head"
    (match decoded with
     | .ok roundtrip =>
         match roundtrip.tapQueues.getD .K1 { notes := [] } with
         | { notes := [.slideHead note], currentIndex := 0 } =>
             note.params.noteIndex = 510 && note.logicalSlideId = 410
         | _ => false
     | .error _ => false)
    "runtime state JSON preserves slide-head queue entries as tagged tap-family notes"

def test_build_game_state_keeps_body_only_slide_out_of_tap_queue : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := []
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , isSlideNoHead := true
         , logicalSlideId := 411
         , noteIndex := 411
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let state := ChartLoader.buildGameState chart
  let tapQueue := state.tapQueues.getD .K1 { notes := [] }
  passCase "build_game_state_keeps_body_only_slide_out_of_tap_queue"
    tapQueue.notes.isEmpty
    "body-only slides do not synthesize a tap-family head from slide-body metadata"

def test_build_game_state_accepts_head_only_lowered_slide_chart : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, logicalSlideId := 413, noteIndex := 513 }]
    , slides := [] }
  let state := ChartLoader.buildGameState chart
  match state.tapQueues.getD .K1 { notes := [] } with
  | { notes := [.slideHead note], currentIndex := 0 } =>
      passCase "build_game_state_accepts_head_only_lowered_slide_chart"
        (note.params.noteIndex = 513 && note.logicalSlideId = 413 && state.slides.isEmpty)
        "lowered charts can represent a head-only slide artifact without fabricating an empty slide body"
  | _ =>
      passCase "build_game_state_accepts_head_only_lowered_slide_chart" false
        "expected one slide-head queue entry and no slide bodies"

def test_build_game_state_scores_slide_head_and_body_break_separately : RuntimeCase :=
  let segmentBreakChart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, isBreak := false, logicalSlideId := 415, noteIndex := 515 }]
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , isBreak := true
         , logicalSlideId := 415
         , noteIndex := 415
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let headBreakChart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, isBreak := true, logicalSlideId := 416, noteIndex := 516 }]
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , isBreak := false
         , logicalSlideId := 416
         , noteIndex := 416
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let segmentScore := (ChartLoader.buildGameState segmentBreakChart).score
  let headScore := (ChartLoader.buildGameState headBreakChart).score
  passCase "build_game_state_scores_slide_head_and_body_break_separately"
    (segmentScore.totalBase = 3000
      && segmentScore.totalExtra = 100
      && segmentScore.maxDxScore = 6
      && headScore.totalBase = 4000
      && headScore.totalExtra = 100
      && headScore.maxDxScore = 6)
    "lowered slide heads and bodies contribute break score totals from their own break flags"

def test_build_game_state_scores_slide_body_multiplicity : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := []
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , multiple := 3
         , logicalSlideId := 417
         , noteIndex := 417
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let state := ChartLoader.buildGameState chart
  match state.slides with
  | [slide] =>
      passCase "build_game_state_scores_slide_body_multiplicity"
        (slide.multiple = 3
          && state.score.totalBase = 4500
          && state.score.totalExtra = 0
          && state.score.maxDxScore = 9)
        "folded identical slide bodies score as their MajdataPlay Multiple count"
  | _ =>
      passCase "build_game_state_scores_slide_body_multiplicity" false
        "expected one folded slide body"

private def judgedMultipleSlideState : InputModel.GameState :=
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 418 }
    , lane := .S1
    , state := .Judged .LateGreat Duration.zero (dur 120000)
    , length := dur 200000
    , headTiming := TimePoint.zero
    , startTiming := TimePoint.zero
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := true
    , multiple := 3
    , judgeQueues := [[]] }
  { currentTime := TimePoint.zero
  , slides := [slide] }

def test_slide_event_multiplicity_accumulates_score : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame judgedMultipleSlideState input
  match events with
  | [evt] =>
      passCase "slide_event_multiplicity_accumulates_score"
        (evt.kind = .Slide
          && evt.noteIndex = 418
          && evt.grade = .LateGreat
          && evt.multiple = 3
          && nextState.score.combo = 3
          && nextState.score.counts.slideCount .LateGreat = 3
          && nextState.score.dxScore = -6)
        "slide result events carry Multiple through combo, counters, and DX score loss"
  | _ =>
      passCase "slide_event_multiplicity_accumulates_score" false
        "expected one multiplied slide event"

def test_build_game_state_ignores_debug_simai_metadata_for_runtime_shape : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := []
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , isSlideNoHead := true
         , logicalSlideId := 414
         , noteIndex := 414
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]]
         , debugSimai := some ("1-5[4:1]", "pretend-shape", true) }] }
  let state := ChartLoader.buildGameState chart
  let tapQueue := state.tapQueues.getD .K1 { notes := [] }
  match state.slides with
  | [slide] =>
      let stateOk :=
        match slide.state with
        | .Active waitTime => waitTime = dur 200000
        | _ => false
      let queueShapeOk :=
        match slide.judgeQueues with
        | [[area]] => area.targetAreas = [.A1] && area.isLast
        | _ => false
      passCase "build_game_state_ignores_debug_simai_metadata_for_runtime_shape"
        (tapQueue.notes.isEmpty
          && slide.params.noteIndex = 414
          && stateOk
          && queueShapeOk)
        "debug/inspection slide metadata must not synthesize extra runtime heads or alter body-side runtime queue shape"
  | _ =>
      passCase "build_game_state_ignores_debug_simai_metadata_for_runtime_shape" false
        "expected exactly one runtime slide body and no synthesized tap-family head"

private def staleConnParentFlagsState : InputModel.GameState :=
  let parentArea1 : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := false, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }
  let parentArea2 : Lifecycle.SlideArea :=
    { targetAreas := [.A2], isLast := true, arrowProgressWhenOn := 1, arrowProgressWhenFinished := 1 }
  let childArea : Lifecycle.SlideArea :=
    { targetAreas := [.A3], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }
  let parent : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 415 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := tp 600000
    , startTiming := tp 600000
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 2
    , totalJudgeQueueLen := 2
    , isCheckable := true
    , judgeQueues := [[parentArea1, parentArea2]] }
  let child : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 1400000, judgeOffset := Duration.zero, noteIndex := 416 }
    , lane := .S1
    , state := .Active (dur 100000)
    , length := dur 400000
    , headTiming := secs 1
    , startTiming := secs 1
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 415
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , parentFinished := true
    , parentPendingFinish := true
    , trackCount := 1
    , initialQueueRemaining := 1
    , totalJudgeQueueLen := 1
    , isCheckable := false
    , judgeQueues := [[childArea]] }
  { currentTime := tp 1200000
  , slides := [parent, child] }

def test_scheduler_recomputes_stale_conn_parent_flags_before_child_progress : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [.A3] Duration.zero
  let (nextState, events, _, _) := Scheduler.stepFrame staleConnParentFlagsState input
  match nextState.slides with
  | [parentAfter, childAfter] =>
      let childQueueStillIntact :=
        match childAfter.judgeQueues with
        | [[area]] => area.targetAreas = [.A3] && area.isLast
        | _ => false
      passCase "scheduler_recomputes_stale_conn_parent_flags_before_child_progress"
        (events.isEmpty
          && parentAfter.judgeQueues.length = 1
          && childAfter.parentFinished = false
          && childAfter.parentPendingFinish = false
          && childAfter.isCheckable = false
          && childQueueStillIntact)
        "stale serialized conn-parent flags must be recomputed from the parent's remaining runtime queue before the child can unlock"
  | _ =>
      passCase "scheduler_recomputes_stale_conn_parent_flags_before_child_progress" false
        "expected parent and child slides to remain present after one frame"

def test_same_lane_equal_time_holds_consume_shared_clicks_in_queue_order : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { holds :=
        [ { timing := TimePoint.zero
          , slot := .S4
          , length := dur 200000
          , noteIndex := 43 }
        , { timing := TimePoint.zero
          , slot := .S4
          , length := dur 200000
          , noteIndex := 44 } ] }
  let state := ChartLoader.buildGameState chart
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [ InputModel.TimedInputEvent.buttonClick TimePoint.zero .K4
                , InputModel.TimedInputEvent.buttonClick TimePoint.zero .K4
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K4 true ] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed state batch
  let holdQueue := nextState.holdQueues.getD .K4 { notes := [] }
  let headsJudged :=
    nextState.activeHolds.all (fun item =>
      match item with
      | (.K4, hold) =>
          match hold.state with
          | .HeadJudged .Perfect => true
          | _ => false
      | _ => false)
  passCase "same_lane_equal_time_holds_consume_shared_clicks_in_queue_order"
    (events.isEmpty
      && nextState.buttonQueueFrontiers.getD .K4 99 = 2
      && holdQueue.currentIndex = 2
      && nextState.activeHolds.length = 2
      && headsJudged)
    "equal-time same-lane hold queues preserve shared button-queue order, so two same-frame clicks can judge both heads"

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

def test_same_area_touch_queue_consumes_multiple_same_frame_clicks : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events :=
        [ InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
        , InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1 ] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed sameAreaTouchQueueState batch
  let queueAfter := nextState.touchQueues.getD .A1 { notes := [] }
  let judged82 :=
    events.any (fun evt => evt.kind = .Touch && evt.noteIndex = 82 && evt.grade = .Perfect)
  let judged83 :=
    events.any (fun evt => evt.kind = .Touch && evt.noteIndex = 83 && evt.grade = .Perfect)
  passCase "same_area_touch_queue_consumes_multiple_same_frame_clicks"
    (events.length = 2
      && judged82
      && judged83
      && queueAfter.currentIndex = 2
      && nextState.touchQueueFrontiers.getD .A1 99 = 2)
    "same-area touch queues can recursively advance through multiple same-frame sensor clicks"

private def threeSameAreaTouchQueueState : InputModel.GameState :=
  let touch1 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 183 }
    , state := .Waiting
    , sensorPos := .A1
    , touchQueueIndex := 0 }
  let touch2 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 184 }
    , state := .Waiting
    , sensorPos := .A1
    , touchQueueIndex := 1 }
  let touch3 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 185 }
    , state := .Waiting
    , sensorPos := .A1
    , touchQueueIndex := 2 }
  { currentTime := TimePoint.zero
  , touchQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [touch1, touch2, touch3] } else { notes := [] }) }

def test_same_area_touch_recursion_stops_when_clicks_exhausted : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events :=
        [ InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
        , InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1 ] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed threeSameAreaTouchQueueState batch
  let queueAfter := nextState.touchQueues.getD .A1 { notes := [] }
  let thirdStillWaiting :=
    match queueAfter.peek with
    | some note =>
        let waiting := match note.state with | .Waiting => true | _ => false
        note.params.noteIndex = 185 && waiting
    | none => false
  passCase "same_area_touch_recursion_stops_when_clicks_exhausted"
    (events.length = 2
      && queueAfter.currentIndex = 2
      && nextState.touchQueueFrontiers.getD .A1 99 = 2
      && thirdStillWaiting)
    "touch recursion should not apply a no-input semantic step to the next queued head"

private def sameLaneHoldThenTapState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 84 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000
    , buttonQueueIndex := 1 }
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 85 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 0 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K1 then 0 else 0)
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [hold] } else { notes := [] })
  , activeHolds := [(.K1, hold)]
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

private def sameLaneHoldWithFutureTapState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 86 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000
    , buttonQueueIndex := 0 }
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 87 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 1 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K1 then 0 else 0)
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [hold] } else { notes := [] })
  , activeHolds := [(.K1, hold)]
  , tapQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [tap] } else { notes := [] }) }

private def sameLaneEarlyHoldLaterTapState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 173 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000
    , buttonQueueIndex := 0 }
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero + dur 100000, judgeOffset := Duration.zero, noteIndex := 174 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 1 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K1 then 0 else 0)
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

private def futureHoldMustNotStealTouchState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero + dur 5640000, judgeOffset := Duration.zero, noteIndex := 73 }
    , start := .button .K5
    , state := .HeadWaiting
    , length := dur 200000
    , buttonQueueIndex := 5 }
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 54 }
    , state := .Judgeable
    , sensorPos := .A5
    , touchQueueIndex := 0 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K5 then 5 else 0)
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K5 then { notes := [hold] } else { notes := [] })
  , activeHolds := [(.K5, hold)]
  , touchQueues := SensorVec.ofFn (fun area => if area == .A5 then { notes := [touch] } else { notes := [] })
  , touchQueueFrontiers := SensorVec.ofFn (fun area => if area == .A5 then 0 else 0) }

def test_future_hold_head_does_not_steal_touch_sensor_click : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A5] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed futureHoldMustNotStealTouchState batch
  match events, nextState.holdQueues.getD .K5 { notes := [] }, nextState.touchQueues.getD .A5 { notes := [] }, nextState.activeHolds with
  | [evt], holdQueueAfter, touchQueueAfter, [(_, holdAfter)] =>
      let holdStillWaiting := match holdAfter.state with | .HeadWaiting => true | _ => false
      passCase "future_hold_head_does_not_steal_touch_sensor_click"
        (evt.kind = .Touch
          && evt.noteIndex = 54
          && holdQueueAfter.currentIndex = 0
          && touchQueueAfter.currentIndex = 1
          && holdStillWaiting)
        "reference-style gating: a far-future hold head must not consume the matching A-sensor click before it reaches its head judgeable range"
  | _, _, _, _ =>
      passCase "future_hold_head_does_not_steal_touch_sensor_click" false "expected the touch to judge while the far-future hold head stayed untouched"

private def futureTouchHoldMustNotStealTouchState : InputModel.GameState :=
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero + dur 5640000, judgeOffset := Duration.zero, noteIndex := 74 }
    , start := .sensor .A5
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 75 }
    , state := .Judgeable
    , sensorPos := .A5
    , touchQueueIndex := 0 }
  { currentTime := TimePoint.zero
  , touchQueueFrontiers := SensorVec.ofFn (fun area => if area == .A5 then 0 else 0)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A5 then { notes := [touch] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A5 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A5, touchHold)] }

def test_future_touch_hold_head_does_not_steal_touch_sensor_click : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A5] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed futureTouchHoldMustNotStealTouchState batch
  match events, nextState.touchQueues.getD .A5 { notes := [] }, nextState.touchHoldQueues.getD .A5 { notes := [] }, nextState.activeTouchHolds with
  | [evt], touchQueueAfter, touchHoldQueueAfter, [(_, touchHoldAfter)] =>
      let holdStillWaiting := match touchHoldAfter.state with | .HeadWaiting => true | _ => false
      passCase "future_touch_hold_head_does_not_steal_touch_sensor_click"
        (evt.kind = .Touch
          && evt.noteIndex = 75
          && touchQueueAfter.currentIndex = 1
          && touchHoldQueueAfter.currentIndex = 0
          && holdStillWaiting)
        "reference-style gating: a far-future touch-hold head must not consume same-area sensor input before its head judgeable range"
  | _, _, _, _ =>
      passCase "future_touch_hold_head_does_not_steal_touch_sensor_click" false "expected the touch to judge while the far-future touch-hold head stayed untouched"

def test_later_same_lane_tap_does_not_bypass_earlier_hold_head : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero + dur 100000
    , events := [InputModel.TimedInputEvent.buttonClick (TimePoint.zero + dur 100000) .K1] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed sameLaneEarlyHoldLaterTapState batch
  match events, nextState.holdQueues.getD .K1 { notes := [] }, nextState.tapQueues.getD .K1 { notes := [] }, nextState.activeHolds with
  | [], holdQueueAfter, tapQueueAfter, [(_, holdAfter)] =>
      let holdHeadJudged := match holdAfter.state with | .HeadJudged _ => true | _ => false
      passCase "later_same_lane_tap_does_not_bypass_earlier_hold_head"
        (nextState.buttonQueueFrontiers.getD .K1 99 = 1
          && holdQueueAfter.currentIndex = 1
          && tapQueueAfter.currentIndex = 0
          && holdHeadJudged)
        "shared button frontier blocks the later tap until the earlier hold head consumes or clears the lane"
  | _, _, _, _ =>
      passCase "later_same_lane_tap_does_not_bypass_earlier_hold_head" false "expected the earlier hold head to consume the click and keep the later tap blocked"

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

def test_same_lane_slide_head_consumes_shared_click_before_hold_head : RuntimeCase :=
  let chart : ChartLoader.ChartSpec :=
    { slideHeads := [{ timing := TimePoint.zero, slot := .S1, logicalSlideId := 412, noteIndex := 512 }]
    , holds := [{ timing := TimePoint.zero, slot := .S1, length := dur 200000, noteIndex := 413 }]
    , slides :=
        [{ headTiming := TimePoint.zero
         , slot := .S1
         , length := dur 200000
         , startTiming := TimePoint.zero
         , logicalSlideId := 412
         , noteIndex := 412
         , judgeQueues := [[{ targetAreas := [.A1], isLast := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]] }] }
  let state := ChartLoader.buildGameState chart
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.buttonClick TimePoint.zero .K1
                , InputModel.TimedInputEvent.buttonHold TimePoint.zero .K1 true] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed state batch
  match events, nextState.tapQueues.getD .K1 { notes := [] }, nextState.holdQueues.getD .K1 { notes := [] }, nextState.activeHolds with
  | [evt], tapQueue, holdQueue, [(_, holdAfter)] =>
      let holdHeadJudgeable := match holdAfter.state with | .HeadJudgeable => true | _ => false
      passCase "same_lane_slide_head_consumes_shared_click_before_hold_head"
        (evt.kind = .Tap
          && evt.noteIndex = 512
          && tapQueue.currentIndex = 1
          && holdQueue.currentIndex = 0
          && holdHeadJudgeable)
        "ordinary slide heads now compete in the tap-family queue and consume the shared click before the hold head"
  | _, _, _, _ =>
      passCase "same_lane_slide_head_consumes_shared_click_before_hold_head" false "expected one slide-head tap event and a non-advanced hold head"

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

private def unlockedButtonFrontierHoldState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 174 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := dur 200000
    , buttonQueueIndex := 1 }
  { currentTime := TimePoint.zero
  , buttonQueueFrontiers := ButtonVec.ofFn (fun zone => if zone == .K1 then 2 else 0)
  , holdQueues := ButtonVec.ofFn (fun zone => if zone == .K1 then { notes := [hold] } else { notes := [] })
  , activeHolds := [(.K1, hold)] }

def test_unlocked_button_frontier_still_allows_older_hold : RuntimeCase :=
  let frontier := unlockedButtonFrontierHoldState.buttonQueueFrontiers.getD .K1 0
  let exactMatch := frontier == 1
  let unlocked := 1 ≤ frontier
  passCase "unlocked_button_frontier_still_allows_older_hold"
    (!exactMatch && unlocked)
    "the shared button frontier has advanced past queue index 1; MajdataPlay still treats that older hold head as unlocked while exact-match gating would reject it"

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
          && stateAfterFirst.touchQueueFrontiers.getD .A1 0 = 1
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
          && stateAfterFirst.touchQueueFrontiers.getD .A1 0 = 2
          && touchHoldQueueAfterFirst.currentIndex = 1
          && holdHeadJudged)
        "with two same-frame touch clicks, touch consumes the first click, touch-hold head consumes the second, and the shared touch frontier advances twice"
  | _, _, _, _ =>
      passCase "same_area_extra_click_allows_touch_hold_head_after_touch" false "expected touch event plus a judged touch-hold head"

private def sameAreaTwoTouchesThenTouchHoldState : InputModel.GameState :=
  let touch1 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 176 }
    , state := .Waiting
    , sensorPos := .A1
    , touchQueueIndex := 0 }
  let touch2 : Lifecycle.TouchNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 177 }
    , state := .Waiting
    , sensorPos := .A1
    , touchQueueIndex := 1 }
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 178 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 2 }
  { currentTime := TimePoint.zero
  , touchQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [touch1, touch2] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, touchHold)] }

def test_same_area_recursive_touches_precede_touch_hold_head : RuntimeCase :=
  let batch : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events :=
        [ InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
        , InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1 ] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed sameAreaTwoTouchesThenTouchHoldState batch
  let touchQueue := nextState.touchQueues.getD .A1 { notes := [] }
  let touchHoldQueue := nextState.touchHoldQueues.getD .A1 { notes := [] }
  let judged176 := events.any (fun evt => evt.kind = .Touch && evt.noteIndex = 176)
  let judged177 := events.any (fun evt => evt.kind = .Touch && evt.noteIndex = 177)
  match nextState.activeTouchHolds with
  | [(_, holdAfter)] =>
      let holdJudgeable := match holdAfter.state with | .HeadJudgeable => true | _ => false
      passCase "same_area_recursive_touches_precede_touch_hold_head"
        (events.length = 2
          && judged176
          && judged177
          && touchQueue.currentIndex = 2
          && nextState.touchQueueFrontiers.getD .A1 99 = 2
          && touchHoldQueue.currentIndex = 0
          && holdJudgeable)
        "recursive same-area touches consume available clicks before the later touch-hold head"
  | _ =>
      passCase "same_area_recursive_touches_precede_touch_hold_head" false
        "expected both touches to judge and the touch-hold head to stay queued"

private def sameAreaConsecutiveTouchHoldsState : InputModel.GameState :=
  let hold1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 171 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  let hold2 : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 172 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1 }
  { currentTime := TimePoint.zero
  , touchQueueFrontiers := SensorVec.ofFn (fun area => if area == .A1 then 0 else 0)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [hold1, hold2] } else { notes := [] })
  , activeTouchHolds := [(.A1, hold1), (.A1, hold2)] }

def test_same_area_consecutive_touch_holds_advance_shared_frontier : RuntimeCase :=
  let batch1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
                , InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let (stateAfterFirst, _, _, _) := Scheduler.stepFrameTimed sameAreaConsecutiveTouchHoldsState batch1
  let batch2 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero + Constants.FRAME_LENGTH
    , events := [InputModel.TimedInputEvent.sensorClick (TimePoint.zero + Constants.FRAME_LENGTH) .A1
                , InputModel.TimedInputEvent.sensorHold (TimePoint.zero + Constants.FRAME_LENGTH) .A1 true] }
  let (stateAfterSecond, _, _, _) := Scheduler.stepFrameTimed stateAfterFirst batch2
  match stateAfterSecond.touchHoldQueues.getD .A1 { notes := [] }, stateAfterSecond.activeTouchHolds with
  | touchHoldQueueAfterSecond, [(_, firstAfterSecond), (_, secondAfterSecond)] =>
      let firstJudged :=
        match firstAfterSecond.state with
        | .HeadJudged _ | .BodyHeld | .BodyReleased | .Ended _ => true
        | _ => false
      let secondJudged :=
        match secondAfterSecond.state with
        | .HeadJudged _ | .BodyHeld | .BodyReleased | .Ended _ => true
        | _ => false
      passCase "same_area_consecutive_touch_holds_advance_shared_frontier"
        (stateAfterFirst.touchQueueFrontiers.getD .A1 0 = 1
          && stateAfterSecond.touchQueueFrontiers.getD .A1 0 = 2
          && touchHoldQueueAfterSecond.currentIndex = 2
          && firstJudged
          && secondJudged)
        "consecutive same-area touch-hold heads should advance the shared touch frontier just like MajdataPlay's `NextTouch`"
  | _, _ =>
      passCase "same_area_consecutive_touch_holds_advance_shared_frontier" false "expected both same-area touch-hold heads to judge across two frames and advance the shared frontier twice"

private def unlockedTouchFrontierTouchHoldState : InputModel.GameState :=
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 170 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1 }
  { currentTime := TimePoint.zero
  , touchQueueFrontiers := SensorVec.ofFn (fun area => if area == .A1 then 2 else 0)
  , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [] } else { notes := [] })
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touchHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, touchHold)] }

def test_unlocked_touch_frontier_still_allows_older_touch_hold : RuntimeCase :=
  let frontier := unlockedTouchFrontierTouchHoldState.touchQueueFrontiers.getD .A1 0
  let exactMatch := frontier == 1
  let unlocked := 1 ≤ frontier
  passCase "unlocked_touch_frontier_still_allows_older_touch_hold"
    (!exactMatch && unlocked)
    "the shared touch frontier has advanced past queue index 1; MajdataPlay treats that older index as still unlocked while exact-match gating would reject it"

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
  match events, nextState.touchHoldQueues.getD .A3 { notes := [] },
      nextState.activeTouchHolds, nextState.touchGroupStates with
  | [], queueAfter, holdsAfter, [groupAfter] =>
      let resolved :=
        holdsAfter.any (fun entry =>
          entry.1 == .A3 && match entry.2.state with | .HeadJudged .Perfect => true | _ => false)
      let groupUnchanged := groupAfter.groupId = 12 && groupAfter.count = 2 && groupAfter.size = 3
      passCase "touch_hold_head_can_resolve_from_shared_touch_group"
        (queueAfter.currentIndex = 1 && resolved && groupUnchanged)
        "touch-hold shared head resolution must not register another touch-group result"
  | _, _, _, _ =>
      passCase "touch_hold_head_can_resolve_from_shared_touch_group" false
        "expected silent head resolution with queue advance"

private def touchHoldDirectHeadSingleGroupMemberState : InputModel.GameState :=
  let holdA1 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 3700 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 800000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 370
    , touchGroupSize := 2 }
  let holdA2 : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 3701 }
    , start := .sensor .A2
    , state := .HeadJudgeable
    , length := dur 800000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 370
    , touchGroupSize := 2 }
  { currentTime := tp 984000
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [holdA1] }
      else if area == .A2 then { notes := [holdA2] }
      else { notes := [] })
  , activeTouchHolds := [(.A1, holdA1), (.A2, holdA2)] }

def test_touch_hold_head_registers_touch_group_only_on_direct_judgment_edge : RuntimeCase :=
  let firstInput := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (stateAfterFirst, firstEvents, _, _) :=
    Scheduler.stepFrame touchHoldDirectHeadSingleGroupMemberState firstInput
  let secondInput := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (stateAfterSecond, secondEvents, _, _) := Scheduler.stepFrame stateAfterFirst secondInput
  let groupCountOk :=
    match stateAfterSecond.touchGroupStates with
    | [group] => group.groupId = 370 && group.count = 1 && group.size = 2
    | _ => false
  let firstStillJudged :=
    stateAfterSecond.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .HeadJudged .Perfect => true | _ => false)
  let secondStillUnresolved :=
    stateAfterSecond.activeTouchHolds.any (fun entry =>
      entry.1 = .A2 && match entry.2.state with | .HeadJudgeable => true | _ => false)
  passCase "touch_hold_head_registers_touch_group_only_on_direct_judgment_edge"
    (firstEvents.isEmpty
      && secondEvents.isEmpty
      && groupCountOk
      && firstStillJudged
      && secondStillUnresolved)
    "a judged touch-hold head must not re-register itself on later active frames"

private def touchHoldHeadUsesTouchGroupNotBodyGroupState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 390 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 140
    , touchGroupSize := 3
    , touchHoldGroupId := some 240
    , touchHoldGroupSize := 3 }
  { currentTime := tp 984000
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [hold] } else { notes := [] })
  , activeTouchHolds := [(.A1, hold)]
  , touchGroupStates := [{ groupId := 140, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }]
  , touchHoldGroupStates := [{ groupId := 240, memberNoteIndices := [390, 391, 392], triggeredNoteIndices := [] }] }

def test_touch_hold_head_share_uses_touch_group_not_body_group : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldHeadUsesTouchGroupNotBodyGroupState input
  match events, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | [], queueAfter, holdsAfter =>
      let resolved :=
        holdsAfter.any (fun entry =>
          entry.1 == .A1 && match entry.2.state with | .HeadJudged .Perfect => true | _ => false)
      passCase "touch_hold_head_share_uses_touch_group_not_body_group"
        (queueAfter.currentIndex = 1 && resolved)
        "touch-hold head sharing should read the shared touch group even when the body-group id differs"
  | _, _, _ =>
      passCase "touch_hold_head_share_uses_touch_group_not_body_group" false
        "expected silent head resolution from touch-group state"

private def touchHoldHeadIgnoresBodyGroupShareState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 393 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 141
    , touchGroupSize := 3
    , touchHoldGroupId := some 241
    , touchHoldGroupSize := 3 }
  { currentTime := tp 984000
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [hold] } else { notes := [] })
  , activeTouchHolds := [(.A1, hold)]
  , touchGroupStates := []
  , touchHoldGroupStates :=
      [{ groupId := 241, memberNoteIndices := [393, 394, 395], triggeredNoteIndices := [394, 395] }] }

def test_touch_hold_head_share_does_not_resolve_from_body_group_state : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldHeadIgnoresBodyGroupShareState input
  let unresolved :=
    nextState.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && match entry.2.state with | .HeadJudgeable => true | _ => false)
  let queueAfter := nextState.touchHoldQueues.getD .A1 { notes := [] }
  passCase "touch_hold_head_share_does_not_resolve_from_body_group_state"
    (events.isEmpty && unresolved && queueAfter.currentIndex = 0)
    "body-group majority should not silently judge a touch-hold head without touch-group share"

private def touchHoldHeadShareLeavesClickState : InputModel.GameState :=
  let sharedHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 394 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 142
    , touchGroupSize := 3 }
  let clickedHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 395 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1 }
  { currentTime := tp (-16000)
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [sharedHold, clickedHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, sharedHold), (.A1, clickedHold)]
  , touchGroupStates := [{ groupId := 142, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_hold_head_share_does_not_consume_sensor_click : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 16000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldHeadShareLeavesClickState input
  match events, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | [], queueAfter, [(_, firstAfter), (_, secondAfter)] =>
      let firstShared :=
        match firstAfter.state with
        | .HeadJudged .Perfect => firstAfter.headDiff = Duration.zero
        | _ => false
      let secondClicked :=
        match secondAfter.state with
        | .HeadJudged .Perfect => secondAfter.headDiff = Duration.zero
        | _ => false
      passCase "touch_hold_head_share_does_not_consume_sensor_click"
        (queueAfter.currentIndex = 2 && firstShared && secondClicked)
        "touch-hold shared head resolution happens before Check(), so it must not consume the physical sensor click"
  | _, _, _ =>
      passCase "touch_hold_head_share_does_not_consume_sensor_click" false
        "expected one shared touch-hold head and one clicked touch-hold head"

private def touchHoldGroupShareBehindUnresolvedHeadState : InputModel.GameState :=
  let head : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 4400 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  let shared : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 4401 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1
    , touchGroupId := some 440
    , touchGroupSize := 3 }
  { currentTime := tp (-16000)
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [head, shared] } else { notes := [] })
  , activeTouchHolds := [(.A1, head), (.A1, shared)]
  , touchGroupStates :=
      [{ groupId := 440, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_hold_group_share_non_head_normalizes_after_earlier_head_clears :
    RuntimeCase :=
  let noInput := mkButtonFrameInput [] [] [] [] (dur 16000)
  let (stateAfterShare, firstEvents, _, _) :=
    Scheduler.stepFrame touchHoldGroupShareBehindUnresolvedHeadState noInput
  let firstQueue := stateAfterShare.touchHoldQueues.getD .A1 { notes := [] }
  let firstHeadStillCurrent :=
    match firstQueue.peek with
    | some note =>
        note.params.noteIndex = 4400 && match note.state with | .HeadJudgeable => true | _ => false
    | none => false
  let sharedHeadResolved :=
    stateAfterShare.activeTouchHolds.any (fun entry =>
      entry.1 = .A1 && entry.2.params.noteIndex = 4401 &&
        match entry.2.state with | .HeadJudged .Perfect => true | _ => false)
  let clickInput := mkButtonFrameInput [] [] [.A1] [.A1] (dur 16000)
  let (nextState, secondEvents, _, _) := Scheduler.stepFrame stateAfterShare clickInput
  let queueAfter := nextState.touchHoldQueues.getD .A1 { notes := [] }
  let bothResolved :=
    nextState.activeTouchHolds.all (fun entry =>
      match entry with
      | (.A1, hold) => match hold.state with | .HeadJudged .Perfect => true | _ => false
      | _ => false)
  passCase "touch_hold_group_share_non_head_normalizes_after_earlier_head_clears"
    (firstEvents.isEmpty
      && secondEvents.isEmpty
      && firstQueue.currentIndex = 0
      && stateAfterShare.touchQueueFrontiers.getD .A1 99 = 1
      && firstHeadStillCurrent
      && sharedHeadResolved
      && queueAfter.currentIndex = 2
      && nextState.touchQueueFrontiers.getD .A1 99 = 2
      && bothResolved)
    "a touch-hold shared head may resolve behind an earlier head, then the family queue skips it after the earlier head clears"

private def touchHoldHeadShareTooLateState : InputModel.GameState :=
  let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 396 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 142
    , touchGroupSize := 3
    , touchHoldGroupId := some 242
    , touchHoldGroupSize := 3 }
  { currentTime := tp 300000
  , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [hold] } else { notes := [] })
  , activeTouchHolds := [(.A1, hold)]
  , touchGroupStates := [{ groupId := 142, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }

def test_touch_hold_head_share_does_not_override_too_late_miss : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [] [] (dur 1000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldHeadShareTooLateState input
  match events, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | [], queueAfter, [(_, holdAfter)] =>
      let missedHead :=
        match holdAfter.state with
        | .HeadJudged .Miss => true
        | _ => false
      passCase "touch_hold_head_share_does_not_override_too_late_miss"
        (queueAfter.currentIndex = 1 && missedHead)
        "MajdataPlay touch-hold heads check too-late before consuming shared touch-group results"
  | _, _, _ =>
      passCase "touch_hold_head_share_does_not_override_too_late_miss" false
        "expected a silent too-late head miss and queue advance"

private def touchHoldHeadTooLateLeavesClickState : InputModel.GameState :=
  let lateHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 397 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0 }
  let clickedHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := tp 301000, judgeOffset := Duration.zero, noteIndex := 398 }
    , start := .sensor .A1
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1 }
  { currentTime := tp 300000
  , touchHoldQueues := SensorVec.ofFn (fun area =>
      if area == .A1 then { notes := [lateHold, clickedHold] } else { notes := [] })
  , activeTouchHolds := [(.A1, lateHold), (.A1, clickedHold)] }

def test_touch_hold_too_late_head_does_not_consume_sensor_click : RuntimeCase :=
  let input := mkButtonFrameInput [] [] [.A1] [] (dur 1000)
  let (nextState, events, _, _) := Scheduler.stepFrame touchHoldHeadTooLateLeavesClickState input
  match events, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds with
  | [], queueAfter, [(_, firstAfter), (_, secondAfter)] =>
      let firstMissed :=
        match firstAfter.state with
        | .HeadJudged .Miss => true
        | _ => false
      let secondClicked :=
        match secondAfter.state with
        | .HeadJudged .Perfect => secondAfter.headDiff = Duration.zero
        | _ => false
      passCase "touch_hold_too_late_head_does_not_consume_sensor_click"
        (queueAfter.currentIndex = 2 && firstMissed && secondClicked)
        "a touch-hold head that is already too late should miss before Check() and leave the physical click available"
  | _, _, _ =>
      passCase "touch_hold_too_late_head_does_not_consume_sensor_click" false
        "expected a too-late touch-hold head miss followed by a clicked touch-hold head"

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

theorem slide_skip_forbidden_preserves_current_segment :
    let queue0 := [reducedSlide61Area1, reducedSlide61Area2, reducedSlide61Area3]
    let heldC := SensorVec.ofFn (fun area => area == .C)
    let queue1 := Lifecycle.replaySlideQueue queue0 heldC
    queue1.length = 3 := by
  native_decide

theorem slide_skip_allowed_advances_exact_prefix :
    let queue0 := [reducedSlide61Area1, reducedSlide61Area2, reducedSlide61Area3]
    let heldC := SensorVec.ofFn (fun area => area == .C)
    let heldNone := SensorVec.ofFn (fun _ => false)
    let queue1 := Lifecycle.replaySlideQueue queue0 heldC
    let queue2 := Lifecycle.replaySlideQueue queue1 heldNone
    queue2.length = 2 := by
  native_decide

theorem slide_queue_last_area_not_cleared_early :
    let queue0 := [reducedSlide61Area1, reducedSlide61Area2, reducedSlide61Area3]
    let heldC := SensorVec.ofFn (fun area => area == .C)
    let heldNone := SensorVec.ofFn (fun _ => false)
    let heldB4 := SensorVec.ofFn (fun area => area == .B4)
    let queue1 := Lifecycle.replaySlideQueue queue0 heldC
    let queue2 := Lifecycle.replaySlideQueue queue1 heldNone
    let queue3 := Lifecycle.replaySlideQueue queue2 heldB4
    queue3.length = 2 := by
  native_decide

theorem short_conn_child_becomes_checkable_with_short_queue_rule :
    test_conn_child_pending_finish_becomes_checkable.passed = true := by
  native_decide

theorem short_conn_child_waits_without_progress_but_does_not_force_finish_parent :
    test_conn_parent_not_force_finished_without_child_progress.passed = true := by
  native_decide

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
    , headTiming := tp 2300000
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
  dxScore : ℤ
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

def test_mixed_chart_golden_ap_without_break : RuntimeCase :=
  let (finalState, events) := simulateMixedGoldenSequence mixedGoldenAPTactic
  let expected : MixedGoldenExpectation :=
    { grades := [.Perfect, .Perfect, .Perfect, .Perfect, .Perfect]
    , combo := 5
    , pCombo := 5
    , cPCombo := 5
    , dxScore := 0
    , comboState := .AP }
  passCase "mixed_chart_golden_ap_without_break"
    (mixedGoldenMatches finalState events expected)
    "MajdataPlay's AP+ result requires theoretical Break notes, so a no-Break all-perfect chart is AP"

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
    "no-Break all-perfect replay stays AP when a non-Break touch becomes LatePerfect2nd"

def test_maji_grade_conversion_preserves_perfect2nd : RuntimeCase :=
  passCase "maji_grade_conversion_preserves_perfect2nd"
    (LnmaiCore.Convert.convertMaji .LatePerfect2nd = .LatePerfect2nd
      && LnmaiCore.Convert.convertMaji .FastPerfect2nd = .FastPerfect2nd
      && LnmaiCore.Convert.convertMaji .LatePerfect3rd = .LateGreat
      && LnmaiCore.Convert.convertMaji .FastPerfect3rd = .FastGreat)
    "MAJI conversion should preserve Perfect2nd and demote only Perfect3rd to Great"

def test_combo_state_matches_majdata_break_theoretical_rule : RuntimeCase :=
  let breakCriticalAndTapPerfect2nd : NoteTypeJudgeCounts :=
    { emptyNoteTypeJudgeCounts with
      tapCount := fun grade => if grade == .LatePerfect2nd then 1 else 0
      breakCount := fun grade => if grade == .Perfect then 1 else 0 }
  let breakPerfect2ndAndTapCritical : NoteTypeJudgeCounts :=
    { emptyNoteTypeJudgeCounts with
      tapCount := fun grade => if grade == .Perfect then 1 else 0
      breakCount := fun grade => if grade == .LatePerfect2nd then 1 else 0 }
  let noBreakCritical : NoteTypeJudgeCounts :=
    { emptyNoteTypeJudgeCounts with
      tapCount := fun grade => if grade == .Perfect then 1 else 0 }
  passCase "combo_state_matches_majdata_break_theoretical_rule"
    (LnmaiCore.comboState ({ counts := breakCriticalAndTapPerfect2nd } : ScoreState) = .APPlus
      && LnmaiCore.comboState ({ counts := breakPerfect2ndAndTapCritical } : ScoreState) = .AP
      && LnmaiCore.comboState ({ counts := noBreakCritical } : ScoreState) = .AP)
    "MajdataPlay gates AP+ on theoretical Break notes while allowing non-Break Perfect2nd/3rd"

private def scoreAccumulationChart : ChartLoader.ChartSpec :=
  { taps :=
      [ { timing := TimePoint.zero, slot := .S1, isBreak := false, isEX := false, noteIndex := 501 }
      , { timing := secs 1, slot := .S2, isBreak := true, isEX := false, noteIndex := 502 }
      , { timing := secs 2, slot := .S3, isBreak := false, isEX := false, noteIndex := 503 } ]
  , holds := []
  , touches := []
  , touchHolds := []
  , slideHeads := []
  , slides := []
  , slideSkipping := true }

private def scoreAccumulationTactic : ManualTacticSequence :=
  mkManualTacticSequence
    [ tapAtTime TimePoint.zero .K1
    , tapAtTime (secs 1 + dur 40000) .K2
    , tapAtTime (secs 2 + dur 70000) .K3 ]

def test_runtime_score_accumulates_base_extra_and_fc_plus : RuntimeCase :=
  let result := simulateChartSpecWithTactic scoreAccumulationChart scoreAccumulationTactic
  let score := result.finalState.score
  passCase "runtime_score_accumulates_base_extra_and_fc_plus"
    (eventGrades result.events = [.Perfect, .LatePerfect3rd, .LateGreat2nd]
      && score.totalBase = 3500
      && score.totalExtra = 100
      && score.earnedBase = 3400
      && score.earnedExtra = 50
      && score.earnedClassicExtra = 0
      && score.lostBase = 100
      && score.lostExtra = 50
      && score.lostClassicExtra = 100
      && score.maxDxScore = 9
      && score.dxScore = -3
      && score.fastCount = 0
      && score.lateCount = 1
      && score.counts.tapCount .Perfect = 1
      && score.counts.breakCount .LatePerfect3rd = 1
      && score.counts.tapCount .LateGreat2nd = 1
      && LnmaiCore.comboState score = .FCPlus)
    "runtime score fold should accumulate base/extra score, DX loss, default fast/late counts, and FC+ state"

private def breakPerfect2ndChart : ChartLoader.ChartSpec :=
  { taps :=
      [ { timing := TimePoint.zero
        , slot := .S1
        , isBreak := true
        , isEX := false
        , noteIndex := 511 } ]
  , holds := []
  , touches := []
  , touchHolds := []
  , slideHeads := []
  , slides := []
  , slideSkipping := true }

private def breakPerfect2ndTactic : ManualTacticSequence :=
  mkManualTacticSequence [tapAtTime (tp 30000) .K1]

def test_runtime_break_perfect2nd_accumulates_classic_extra : RuntimeCase :=
  let result := simulateChartSpecWithTactic breakPerfect2ndChart breakPerfect2ndTactic
  let score := result.finalState.score
  passCase "runtime_break_perfect2nd_accumulates_classic_extra"
    (eventGrades result.events = [.LatePerfect2nd]
      && score.earnedBase = 2500
      && score.earnedExtra = 75
      && score.earnedClassicExtra = 50
      && score.lostBase = 0
      && score.lostExtra = 25
      && score.lostClassicExtra = 50
      && LnmaiCore.comboState score = .AP)
    "Break Perfect2nd should carry MajdataPlay's classic extra 50/50 split through runtime state"

def test_fast_late_disable_counter_matches_object_counter : RuntimeCase :=
  passCase "fast_late_disable_counter_matches_object_counter"
    (Score.countFastLate .LateGreat (dur 30000) .Disable = (false, true)
      && Score.countFastLate .LatePerfect3rd (dur 30000) .Disable = (false, false)
      && Score.countFastLate .LateGood (dur 30000) .MissOnly = (false, false))
    "MajdataPlay ObjectCounter treats Disable like BelowP for fast/late stats and MissOnly as no fast/late contribution"

def test_runtime_fast_late_display_options_follow_game_state : RuntimeCase :=
  let initialState :=
    { ChartLoader.buildGameState scoreAccumulationChart with
      noteFastLateDisplay := .MissOnly
      breakFastLateDisplay := .All }
  let result := simulateStateWithTacticAndBatches initialState scoreAccumulationTactic []
  let score := result.finalState.score
  passCase "runtime_fast_late_display_options_follow_game_state"
    (eventGrades result.events = [.Perfect, .LatePerfect3rd, .LateGreat2nd]
      && score.fastCount = 0
      && score.lateCount = 1
      && score.counts.breakCount .LatePerfect3rd = 1
      && score.counts.tapCount .LateGreat2nd = 1)
    "scheduler score folding should use the GameState note/break fast-late display settings"

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

def test_frame_window_filters_timed_hold_events : RuntimeCase :=
  let prevButtonHeld := ButtonVec.ofFn (fun zone => zone == .K1)
  let prevSensorHeld := SensorVec.ofFn (fun area => area == .A1)
  let batch : InputModel.TimedInputBatch :=
    { currentTime := secs 1
    , events :=
        [ InputModel.TimedInputEvent.buttonHold (secs 1 - dur 16000) .K1 false
        , InputModel.TimedInputEvent.sensorHold (secs 1 - dur 16001) .A1 false
        , InputModel.TimedInputEvent.buttonHold (secs 1 - dur 1) .K2 true
        , InputModel.TimedInputEvent.sensorHold (secs 1) .A2 true ] }
  let input := batch.toFrameInput (dur 16000) prevButtonHeld prevSensorHeld
  passCase "frame_window_filters_timed_hold_events"
    (input.getButtonHeld .K1
      && input.getSensorHeld .A1
      && input.getButtonHeld .K2
      && input.getSensorHeld .A2)
    "timed hold down/up events should obey the same frame-window filtering as click events"

def test_manual_tactic_hold_interval_sugar : RuntimeCase :=
  let parsed := parseManualTacticSequence "hold button K2 from 1000000 to 1220000\nhold sensor A4 from 2000000 to 2220000"
  let expected : ManualTacticSequence :=
    mkManualTacticSequence
      [ holdButtonAtTime (TimePoint.fromMicros 1000000) .K2 true
      , holdButtonAtTime (TimePoint.fromMicros 1220000) .K2 false
      , holdSensorAtTime (TimePoint.fromMicros 2000000) .A4 true
      , holdSensorAtTime (TimePoint.fromMicros 2220000) .A4 false ]
  passCase "manual_tactic_hold_interval_sugar"
    (match parsed with
    | .ok seq => reprStr seq.events == reprStr expected.events
    | .error _ => false)
    "manual tactic parser expands hold intervals into down/up events"

def test_manual_tactic_chord_sugar : RuntimeCase :=
  let parsed := parseManualTacticSequence "500000 tap K1 K2 K3\n516000 touch A1 A2"
  let expected : ManualTacticSequence :=
    mkManualTacticSequence
      [ tapAtTime (TimePoint.fromMicros 500000) .K1
      , tapAtTime (TimePoint.fromMicros 500000) .K2
      , tapAtTime (TimePoint.fromMicros 500000) .K3
      , touchAtTime (TimePoint.fromMicros 516000) .A1
      , touchAtTime (TimePoint.fromMicros 516000) .A2 ]
  passCase "manual_tactic_chord_sugar"
    (match parsed with
    | .ok seq => reprStr seq.events == reprStr expected.events
    | .error _ => false)
    "manual tactic parser expands same-timestamp tap and touch chords"

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

def test_lowered_slide_chart_json_requires_head_timing_and_rejects_legacy_timing : RuntimeCase :=
  let expectedQueues : List (List ChartLoader.SlideAreaSpec) :=
    [[{ targetAreas := [.A1], policy := .Or, isLast := true, isSkippable := true, arrowProgressWhenOn := 0, arrowProgressWhenFinished := 0 }]]
  let chartWithHeadTiming :=
    "{\"taps\":[],\"holds\":[],\"touches\":[],\"touchHolds\":[],\"slideHeads\":[],\"slides\":[{\"headTiming\":0,\"slot\":\"S1\",\"length\":1,\"startTiming\":0,\"slideKind\":\"Single\",\"logicalSlideId\":91,\"noteIndex\":91,\"judgeQueues\":[[{\"targetAreas\":[\"A1\"],\"policy\":\"Or\",\"isLast\":true,\"isSkippable\":true,\"arrowProgressWhenOn\":0,\"arrowProgressWhenFinished\":0}]]}],\"slideSkipping\":true}"
  let chartWithLegacyTiming :=
    "{\"taps\":[],\"holds\":[],\"touches\":[],\"touchHolds\":[],\"slideHeads\":[],\"slides\":[{\"timing\":0,\"slot\":\"S1\",\"length\":1,\"startTiming\":0,\"slideKind\":\"Single\",\"noteIndex\":92,\"judgeQueues\":[[{\"targetAreas\":[\"A1\"],\"policy\":\"Or\",\"isLast\":true,\"isSkippable\":true,\"arrowProgressWhenOn\":0,\"arrowProgressWhenFinished\":0}]]}],\"slideSkipping\":true}"
  let parsedHeadTiming := ChartLoader.parseChartJsonString chartWithHeadTiming
  let parsedLegacyTiming := ChartLoader.parseChartJsonString chartWithLegacyTiming
  let headTimingOk :=
    match parsedHeadTiming with
    | .ok chart =>
        match chart.slides with
        | [slide] =>
            slide.headTiming = TimePoint.zero &&
            slide.slot = .S1 &&
            slide.length = dur 1 &&
            slide.startTiming = TimePoint.zero &&
            slide.logicalSlideId = 91 &&
            slide.noteIndex = 91 &&
            reprStr slide.judgeQueues = reprStr expectedQueues
        | _ => false
    | .error _ => false
  let legacyTimingRejected := exceptIsError parsedLegacyTiming
  passCase "lowered_slide_chart_json_requires_head_timing_and_rejects_legacy_timing"
    (headTimingOk && legacyTimingRejected)
    "lowered slide-body JSON now requires `headTiming` and rejects legacy body `timing`"

def test_lowered_slide_chart_json_malformed_logical_slide_id_fails : RuntimeCase :=
  let chartWithMalformedLogicalSlideId :=
    "{\"taps\":[],\"holds\":[],\"touches\":[],\"touchHolds\":[],\"slideHeads\":[],\"slides\":[{\"headTiming\":0,\"slot\":\"S1\",\"length\":1,\"startTiming\":0,\"slideKind\":\"Single\",\"logicalSlideId\":\"oops\",\"noteIndex\":93,\"judgeQueues\":[[{\"targetAreas\":[\"A1\"],\"policy\":\"Or\",\"isLast\":true,\"isSkippable\":true,\"arrowProgressWhenOn\":0,\"arrowProgressWhenFinished\":0}]]}],\"slideSkipping\":true}"
  let parsed := ChartLoader.parseChartJsonString chartWithMalformedLogicalSlideId
  passCase "lowered_slide_chart_json_malformed_logical_slide_id_fails"
    (exceptIsError parsed)
    "malformed gameplay-relevant optional `logicalSlideId` should now fail decode instead of silently falling back to `noteIndex`"

def test_tap_family_json_malformed_button_queue_index_fails : RuntimeCase :=
  let json :=
    Json.mkObj
      [ ("kind", Json.str "slideHead")
      , ("params", toJson ({ judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 94 } : Lifecycle.CommonNoteParams))
      , ("lane", toJson OuterSlot.S1)
      , ("state", toJson Lifecycle.TapState.Waiting)
      , ("logicalSlideId", Json.num 94)
      , ("buttonQueueIndex", Json.str "oops") ]
  let parsed : Except String Lifecycle.TapFamilyNote := fromJson? json
  passCase "tap_family_json_malformed_button_queue_index_fails"
    (exceptIsError parsed)
    "malformed gameplay-relevant optional tap-family `buttonQueueIndex` should now fail decode instead of silently falling back to zero"

def all : List RuntimeCase :=
  [ test_button_tap_can_use_matching_a_sensor
  , test_classic_hold_matching_a_sensor_keeps_body_pressed
  , test_classic_hold_release_before_head_ignore_ends
  , test_modern_hold_head_miss_can_end_as_late_good
  , test_modern_hold_head_miss_skips_release_ignore_grace
  , test_modern_hold_perfect_head_keeps_release_ignore_grace
  , test_short_modern_hold_does_not_force_end_before_remaining_time_zero
  , test_modern_hold_past_tail_ignore_waits_until_remaining_time_zero
  , test_modern_hold_force_end_does_not_add_final_release_delta
  , test_touch_hold_body_majority_reactivates_released_note
  , test_touch_hold_local_press_reactivates_released_note
  , test_break_tap_event_preserves_family_and_counts_as_break
  , test_break_hold_event_preserves_family_and_counts_as_break
  , test_break_touch_event_preserves_family_and_counts_as_break
  , test_break_slide_event_preserves_family_and_counts_as_break
  , test_classic_hold_fast_boundary_is_strict
  , test_classic_hold_late_boundary_is_strict
  , test_touch_hold_group_share_requires_strict_majority
  , test_touch_hold_body_group_exit_shrinks_majority_denominator
  , test_touch_hold_body_window_uses_raw_timing_despite_judge_offset
  , test_conn_child_wifi_parent_pending_finish_becomes_checkable
  , test_wifi_too_late_two_single_tails_is_lategood_by_max_remaining
  , test_overlapping_slides_can_both_progress_from_one_sensor_hold
  , test_simultaneous_short_regular_holds_can_both_finish
  , test_chart_wrapper_short_hold_pair_after_unrelated_taps_can_finish
  , test_chart_wrapper_same_head_conn_pair_achieves_ap
  , test_chart_wrapper_same_head_conn_three_part_chain_achieves_ap
  , test_chart_wrapper_fallback_demo_level6_achieves_ap
  , test_chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap
  , test_conn_child_progress_force_finishes_parent
  , test_slide_judge_uses_touch_panel_offset
  , test_touch_group_majority_shares_result_same_frame
  , test_touch_group_share_reuses_converted_grade_without_second_conversion
  , test_touch_group_share_does_not_override_too_late_miss
  , test_touch_group_share_does_not_consume_sensor_click
  , test_touch_group_share_resolves_non_head_without_skipping_unresolved_head
  , test_touch_group_share_non_head_is_skipped_after_earlier_head_clears
  , test_conn_child_pending_finish_becomes_checkable
  , test_conn_child_finished_parent_becomes_checkable
  , test_conn_parent_not_force_finished_without_child_progress
  , test_conn_child_progress_only_force_finishes_direct_parent
  , test_conn_non_end_part_does_not_judge_when_finished
  , test_conn_non_end_part_does_not_too_late_judge
  , test_conn_already_progressed_child_does_not_re_force_finish_parent
  , test_wifi_classic_tail_progress_uses_special_marker
  , test_wifi_center_cleared_progress_uses_special_marker
  , test_wifi_center_cleared_without_both_tails_uses_max_queue_marker
  , test_wifi_judged_wait_crossing_zero_waits_one_more_frame
  , test_wifi_judged_wait_emits_when_nonpositive_at_frame_start
  , test_wifi_judged_wait_before_expiry_emits_nothing
  , test_wifi_too_late_ends_immediately
  , test_wifi_too_late_one_remaining_becomes_lategood
  , test_single_slide_too_late_uses_pre_sensor_queue_remaining
  , test_slide_too_late_lategood_counts_as_fast_from_default_diff
  , test_wifi_not_checkable_before_minus_50ms
  , test_wifi_exact_minus_50ms_becomes_checkable
  , test_wifi_exact_too_late_boundary_does_not_judge
  , test_frame_zero_tap_judges_same_frame
  , test_frame_zero_hold_head_judges_same_frame
  , test_frame_zero_touch_judges_same_frame
  , test_touch_waiting_large_delta_uses_reference_too_late_boundary
  , test_frame_zero_touch_hold_head_judges_same_frame
  , test_touch_ignores_outer_button_without_sensor_input
  , test_too_late_tap_does_not_consume_button_click
  , test_too_late_touch_does_not_consume_sensor_click
  , test_modern_hold_release_grace_does_not_count_toward_release_time
  , test_touch_hold_head_ignores_outer_button_without_sensor_input
  , test_replay_frame_zero_tap_judges_same_frame
  , test_replay_frame_zero_touch_judges_same_frame
  , test_replay_frame_zero_touch_hold_head_judges_same_frame
  , test_same_slot_brief_gap_hold_chain_button_held_sensor_clicks_achieves_ap
  , test_same_slot_brief_gap_hold_chain_sensor_held_button_clicks_achieves_ap
  , test_frame_zero_slide_can_start_progress_same_frame
  , test_slide_cleared_queue_enters_judged_on_next_frame
  , test_replay_slide_delays_final_event_after_internal_judged
  , test_modern_slide_late_good_clamps_judged_wait_to_50ms
  , test_modern_slide_maji_reconverts_stored_judge_result_at_end
  , test_classic_slide_late_clear_keeps_existing_judged_wait
  , test_conn_slide_early_clear_uses_group_start_for_judged_wait
  , test_same_lane_tap_queue_blocks_second_note_until_first_advances
  , test_same_lane_tap_queue_consumes_multiple_same_frame_clicks
  , test_same_lane_tap_recursion_stops_when_clicks_exhausted
  , test_same_area_touch_queue_blocks_second_note_until_first_advances
  , test_same_area_touch_queue_consumes_multiple_same_frame_clicks
  , test_same_area_touch_recursion_stops_when_clicks_exhausted
  , test_build_game_state_routes_slide_head_into_tap_queue
  , test_game_state_json_preserves_tap_family_kind_for_slide_head
  , test_build_game_state_ignores_debug_simai_metadata_for_runtime_shape
  , test_build_game_state_accepts_head_only_lowered_slide_chart
  , test_build_game_state_scores_slide_head_and_body_break_separately
  , test_build_game_state_scores_slide_body_multiplicity
  , test_slide_event_multiplicity_accumulates_score
  , test_scheduler_recomputes_stale_conn_parent_flags_before_child_progress
  , test_same_lane_equal_time_holds_consume_shared_clicks_in_queue_order
  , test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click
  , test_future_same_lane_tap_head_does_not_steal_hold_click
  , test_future_hold_head_does_not_steal_touch_sensor_click
  , test_future_touch_hold_head_does_not_steal_touch_sensor_click
  , test_later_same_lane_tap_does_not_bypass_earlier_hold_head
  , test_reference_style_hold_head_does_not_advance_without_own_click
  , test_same_lane_extra_click_allows_hold_head_after_tap
  , test_unlocked_button_frontier_still_allows_older_hold
  , test_same_area_touch_consumes_shared_click_before_touch_hold_head
  , test_same_area_extra_click_allows_touch_hold_head_after_touch
  , test_same_area_recursive_touches_precede_touch_hold_head
  , test_same_area_consecutive_touch_holds_advance_shared_frontier
  , test_unlocked_touch_frontier_still_allows_older_touch_hold
  , test_touch_hold_head_can_resolve_from_shared_touch_group
  , test_touch_hold_head_registers_touch_group_only_on_direct_judgment_edge
  , test_touch_hold_head_share_uses_touch_group_not_body_group
  , test_touch_hold_head_share_does_not_resolve_from_body_group_state
  , test_touch_hold_head_share_does_not_consume_sensor_click
  , test_touch_hold_group_share_non_head_normalizes_after_earlier_head_clears
  , test_touch_hold_head_share_does_not_override_too_late_miss
  , test_touch_hold_too_late_head_does_not_consume_sensor_click
  , test_scheduler_policy_touch_runs_before_touch_hold_group_share
  , test_reference_like_slide_skip_chain_does_not_clear_last_area_early
  , test_reference_like_slide_skip_chain_c_off_only_does_not_clear_all
  , test_mixed_chart_golden_ap_without_break
  , test_mixed_chart_golden_ap_with_late_touch
  , test_maji_grade_conversion_preserves_perfect2nd
  , test_combo_state_matches_majdata_break_theoretical_rule
  , test_runtime_score_accumulates_base_extra_and_fc_plus
  , test_runtime_break_perfect2nd_accumulates_classic_extra
  , test_fast_late_disable_counter_matches_object_counter
  , test_runtime_fast_late_display_options_follow_game_state
  , test_frame_window_zero_delta_includes_exact_point
  , test_frame_window_positive_delta_excludes_left_boundary
  , test_frame_window_positive_delta_includes_inside_and_right_boundary
  , test_frame_window_positive_delta_excludes_outside_window
  , test_frame_window_filters_timed_hold_events
  , test_manual_tactic_hold_interval_sugar
  , test_manual_tactic_chord_sugar
  , test_typed_json_boundary_symbolic_only
  , test_lowered_slide_chart_json_requires_head_timing_and_rejects_legacy_timing
  , test_lowered_slide_chart_json_malformed_logical_slide_id_fails
  , test_tap_family_json_malformed_button_queue_index_fails
  ]

def passedCount : Nat :=
  (all.filter (·.passed)).length

-- #eval all
-- #eval (passedCount, all.length)

theorem test_button_tap_can_use_matching_a_sensor_proof :
    test_button_tap_can_use_matching_a_sensor.passed = true := by native_decide

theorem test_classic_hold_matching_a_sensor_keeps_body_pressed_proof :
    test_classic_hold_matching_a_sensor_keeps_body_pressed.passed = true := by native_decide

theorem test_classic_hold_release_before_head_ignore_ends_proof :
    test_classic_hold_release_before_head_ignore_ends.passed = true := by native_decide

theorem test_modern_hold_head_miss_can_end_as_late_good_proof :
    test_modern_hold_head_miss_can_end_as_late_good.passed = true := by native_decide

theorem test_modern_hold_head_miss_skips_release_ignore_grace_proof :
    test_modern_hold_head_miss_skips_release_ignore_grace.passed = true := by native_decide

theorem test_modern_hold_perfect_head_keeps_release_ignore_grace_proof :
    test_modern_hold_perfect_head_keeps_release_ignore_grace.passed = true := by native_decide

theorem test_short_modern_hold_does_not_force_end_before_remaining_time_zero_proof :
    test_short_modern_hold_does_not_force_end_before_remaining_time_zero.passed = true := by native_decide

theorem test_modern_hold_past_tail_ignore_waits_until_remaining_time_zero_proof :
    test_modern_hold_past_tail_ignore_waits_until_remaining_time_zero.passed = true := by
  native_decide

theorem test_modern_hold_force_end_does_not_add_final_release_delta_proof :
    test_modern_hold_force_end_does_not_add_final_release_delta.passed = true := by native_decide

theorem test_touch_hold_body_majority_reactivates_released_note_proof :
    test_touch_hold_body_majority_reactivates_released_note.passed = true := by native_decide

theorem test_touch_hold_local_press_reactivates_released_note_proof :
    test_touch_hold_local_press_reactivates_released_note.passed = true := by native_decide

theorem test_touch_hold_body_window_uses_raw_timing_despite_judge_offset_proof :
    test_touch_hold_body_window_uses_raw_timing_despite_judge_offset.passed = true := by
  native_decide

theorem test_classic_hold_fast_boundary_is_strict_proof :
    test_classic_hold_fast_boundary_is_strict.passed = true := by native_decide

theorem test_classic_hold_late_boundary_is_strict_proof :
    test_classic_hold_late_boundary_is_strict.passed = true := by native_decide

theorem test_touch_hold_group_share_requires_strict_majority_proof :
    test_touch_hold_group_share_requires_strict_majority.passed = true := by native_decide

theorem test_touch_hold_body_group_exit_shrinks_majority_denominator_proof :
    test_touch_hold_body_group_exit_shrinks_majority_denominator.passed = true := by
  native_decide

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

theorem test_chart_wrapper_same_head_conn_pair_achieves_ap_proof :
    test_chart_wrapper_same_head_conn_pair_achieves_ap.passed = true := by native_decide

theorem test_chart_wrapper_same_head_conn_three_part_chain_achieves_ap_proof :
    test_chart_wrapper_same_head_conn_three_part_chain_achieves_ap.passed = true := by native_decide

theorem test_chart_wrapper_fallback_demo_level6_achieves_ap_proof :
    test_chart_wrapper_fallback_demo_level6_achieves_ap.passed = true := by native_decide

theorem test_chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap_proof :
    test_chart_wrapper_fallback_demo_level6_headless_child_emits_no_head_tap.passed = true := by native_decide

theorem test_default_tactic_does_not_infer_slide_head_from_body_metadata_alone_proof :
    test_default_tactic_does_not_infer_slide_head_from_body_metadata_alone.passed = true := by native_decide

theorem test_default_tactic_uses_explicit_slide_head_even_if_body_compat_flag_is_headless_proof :
    test_default_tactic_uses_explicit_slide_head_even_if_body_compat_flag_is_headless.passed = true := by native_decide

theorem test_default_tactic_replays_head_only_lowered_slide_chart_proof :
    test_default_tactic_replays_head_only_lowered_slide_chart.passed = true := by native_decide

theorem test_conn_child_progress_force_finishes_parent_proof :
    test_conn_child_progress_force_finishes_parent.passed = true := by native_decide

theorem test_slide_judge_uses_touch_panel_offset_proof :
    test_slide_judge_uses_touch_panel_offset.passed = true := by native_decide

theorem test_touch_group_majority_shares_result_same_frame_proof :
    test_touch_group_majority_shares_result_same_frame.passed = true := by native_decide

theorem test_touch_group_share_reuses_converted_grade_without_second_conversion_proof :
    test_touch_group_share_reuses_converted_grade_without_second_conversion.passed = true := by native_decide

theorem test_touch_group_share_does_not_override_too_late_miss_proof :
    test_touch_group_share_does_not_override_too_late_miss.passed = true := by native_decide

theorem test_touch_group_share_does_not_consume_sensor_click_proof :
    test_touch_group_share_does_not_consume_sensor_click.passed = true := by native_decide

theorem test_touch_group_share_resolves_non_head_without_skipping_unresolved_head_proof :
    test_touch_group_share_resolves_non_head_without_skipping_unresolved_head.passed = true := by
  native_decide

theorem test_touch_group_share_non_head_is_skipped_after_earlier_head_clears_proof :
    test_touch_group_share_non_head_is_skipped_after_earlier_head_clears.passed = true := by
  native_decide

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

theorem test_wifi_judged_wait_crossing_zero_waits_one_more_frame_proof :
    test_wifi_judged_wait_crossing_zero_waits_one_more_frame.passed = true := by
  native_decide

theorem test_wifi_judged_wait_emits_when_nonpositive_at_frame_start_proof :
    test_wifi_judged_wait_emits_when_nonpositive_at_frame_start.passed = true := by
  native_decide

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

theorem test_single_slide_too_late_uses_pre_sensor_queue_remaining_proof :
    test_single_slide_too_late_uses_pre_sensor_queue_remaining.passed = true := by native_decide

theorem test_slide_too_late_lategood_counts_as_fast_from_default_diff_proof :
    test_slide_too_late_lategood_counts_as_fast_from_default_diff.passed = true := by
  native_decide

theorem test_conn_child_progress_only_force_finishes_direct_parent_proof :
    test_conn_child_progress_only_force_finishes_direct_parent.passed = true := by native_decide

theorem test_conn_non_end_part_does_not_judge_when_finished_proof :
    test_conn_non_end_part_does_not_judge_when_finished.passed = true := by native_decide

theorem test_conn_non_end_part_does_not_too_late_judge_proof :
    test_conn_non_end_part_does_not_too_late_judge.passed = true := by native_decide

theorem test_conn_already_progressed_child_does_not_re_force_finish_parent_proof :
    test_conn_already_progressed_child_does_not_re_force_finish_parent.passed = true := by native_decide

theorem test_wifi_not_checkable_before_minus_50ms_proof :
    test_wifi_not_checkable_before_minus_50ms.passed = true := by native_decide

theorem test_wifi_exact_minus_50ms_becomes_checkable_proof :
    test_wifi_exact_minus_50ms_becomes_checkable.passed = true := by native_decide

theorem test_wifi_exact_too_late_boundary_does_not_judge_proof :
    test_wifi_exact_too_late_boundary_does_not_judge.passed = true := by native_decide

theorem test_frame_zero_tap_judges_same_frame_proof :
    test_frame_zero_tap_judges_same_frame.passed = true := by native_decide

theorem test_frame_zero_hold_head_judges_same_frame_proof :
    test_frame_zero_hold_head_judges_same_frame.passed = true := by native_decide

theorem test_frame_zero_touch_judges_same_frame_proof :
    test_frame_zero_touch_judges_same_frame.passed = true := by native_decide

theorem test_touch_waiting_large_delta_uses_reference_too_late_boundary_proof :
    test_touch_waiting_large_delta_uses_reference_too_late_boundary.passed = true := by native_decide

theorem test_frame_zero_touch_hold_head_judges_same_frame_proof :
    test_frame_zero_touch_hold_head_judges_same_frame.passed = true := by native_decide

theorem test_touch_ignores_outer_button_without_sensor_input_proof :
    test_touch_ignores_outer_button_without_sensor_input.passed = true := by native_decide

theorem test_too_late_tap_does_not_consume_button_click_proof :
    test_too_late_tap_does_not_consume_button_click.passed = true := by native_decide

theorem test_too_late_touch_does_not_consume_sensor_click_proof :
    test_too_late_touch_does_not_consume_sensor_click.passed = true := by native_decide

theorem test_modern_hold_release_grace_does_not_count_toward_release_time_proof :
    test_modern_hold_release_grace_does_not_count_toward_release_time.passed = true := by
  native_decide

theorem test_touch_hold_head_ignores_outer_button_without_sensor_input_proof :
    test_touch_hold_head_ignores_outer_button_without_sensor_input.passed = true := by native_decide

theorem test_replay_frame_zero_tap_judges_same_frame_proof :
    test_replay_frame_zero_tap_judges_same_frame.passed = true := by native_decide

theorem test_replay_frame_zero_touch_judges_same_frame_proof :
    test_replay_frame_zero_touch_judges_same_frame.passed = true := by native_decide

theorem test_replay_frame_zero_touch_hold_head_judges_same_frame_proof :
    test_replay_frame_zero_touch_hold_head_judges_same_frame.passed = true := by native_decide

theorem test_same_slot_brief_gap_hold_chain_button_held_sensor_clicks_achieves_ap_proof :
    test_same_slot_brief_gap_hold_chain_button_held_sensor_clicks_achieves_ap.passed = true := by native_decide

theorem test_same_slot_brief_gap_hold_chain_sensor_held_button_clicks_achieves_ap_proof :
    test_same_slot_brief_gap_hold_chain_sensor_held_button_clicks_achieves_ap.passed = true := by native_decide

theorem test_frame_zero_slide_can_start_progress_same_frame_proof :
    test_frame_zero_slide_can_start_progress_same_frame.passed = true := by native_decide

theorem test_replay_slide_delays_final_event_after_internal_judged_proof :
    test_replay_slide_delays_final_event_after_internal_judged.passed = true := by native_decide

theorem test_slide_cleared_queue_enters_judged_on_next_frame_proof :
    test_slide_cleared_queue_enters_judged_on_next_frame.passed = true := by native_decide

theorem test_modern_slide_late_good_clamps_judged_wait_to_50ms_proof :
    test_modern_slide_late_good_clamps_judged_wait_to_50ms.passed = true := by native_decide

theorem test_modern_slide_maji_reconverts_stored_judge_result_at_end_proof :
    test_modern_slide_maji_reconverts_stored_judge_result_at_end.passed = true := by
  native_decide

theorem test_classic_slide_late_clear_keeps_existing_judged_wait_proof :
    test_classic_slide_late_clear_keeps_existing_judged_wait.passed = true := by native_decide

theorem test_conn_slide_early_clear_uses_group_start_for_judged_wait_proof :
    test_conn_slide_early_clear_uses_group_start_for_judged_wait.passed = true := by native_decide

theorem test_same_lane_tap_queue_blocks_second_note_until_first_advances_proof :
    test_same_lane_tap_queue_blocks_second_note_until_first_advances.passed = true := by native_decide

theorem test_same_lane_tap_queue_consumes_multiple_same_frame_clicks_proof :
    test_same_lane_tap_queue_consumes_multiple_same_frame_clicks.passed = true := by native_decide

theorem test_same_lane_tap_recursion_stops_when_clicks_exhausted_proof :
    test_same_lane_tap_recursion_stops_when_clicks_exhausted.passed = true := by
  native_decide

theorem test_build_game_state_routes_slide_head_into_tap_queue_proof :
    test_build_game_state_routes_slide_head_into_tap_queue.passed = true := by native_decide

theorem test_game_state_json_preserves_tap_family_kind_for_slide_head_proof :
    test_game_state_json_preserves_tap_family_kind_for_slide_head.passed = true := by native_decide

theorem test_build_game_state_keeps_body_only_slide_out_of_tap_queue_proof :
    test_build_game_state_keeps_body_only_slide_out_of_tap_queue.passed = true := by native_decide

theorem test_build_game_state_ignores_debug_simai_metadata_for_runtime_shape_proof :
    test_build_game_state_ignores_debug_simai_metadata_for_runtime_shape.passed = true := by native_decide

theorem test_build_game_state_accepts_head_only_lowered_slide_chart_proof :
    test_build_game_state_accepts_head_only_lowered_slide_chart.passed = true := by native_decide

theorem test_build_game_state_scores_slide_head_and_body_break_separately_proof :
    test_build_game_state_scores_slide_head_and_body_break_separately.passed = true := by native_decide

theorem test_build_game_state_scores_slide_body_multiplicity_proof :
    test_build_game_state_scores_slide_body_multiplicity.passed = true := by native_decide

theorem test_slide_event_multiplicity_accumulates_score_proof :
    test_slide_event_multiplicity_accumulates_score.passed = true := by native_decide

theorem test_scheduler_recomputes_stale_conn_parent_flags_before_child_progress_proof :
    test_scheduler_recomputes_stale_conn_parent_flags_before_child_progress.passed = true := by native_decide

theorem test_same_lane_equal_time_holds_consume_shared_clicks_in_queue_order_proof :
    test_same_lane_equal_time_holds_consume_shared_clicks_in_queue_order.passed = true := by native_decide

theorem test_same_area_touch_queue_blocks_second_note_until_first_advances_proof :
    test_same_area_touch_queue_blocks_second_note_until_first_advances.passed = true := by native_decide

theorem test_same_area_touch_queue_consumes_multiple_same_frame_clicks_proof :
    test_same_area_touch_queue_consumes_multiple_same_frame_clicks.passed = true := by
  native_decide

theorem test_same_area_touch_recursion_stops_when_clicks_exhausted_proof :
    test_same_area_touch_recursion_stops_when_clicks_exhausted.passed = true := by
  native_decide

theorem test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click_proof :
    test_same_lane_hold_head_does_not_advance_when_tap_consumes_shared_click.passed = true := by native_decide

theorem test_same_lane_slide_head_consumes_shared_click_before_hold_head_proof :
    test_same_lane_slide_head_consumes_shared_click_before_hold_head.passed = true := by native_decide

theorem test_future_same_lane_tap_head_does_not_steal_hold_click_proof :
    test_future_same_lane_tap_head_does_not_steal_hold_click.passed = true := by native_decide

theorem test_future_hold_head_does_not_steal_touch_sensor_click_proof :
    test_future_hold_head_does_not_steal_touch_sensor_click.passed = true := by native_decide

theorem test_future_touch_hold_head_does_not_steal_touch_sensor_click_proof :
    test_future_touch_hold_head_does_not_steal_touch_sensor_click.passed = true := by native_decide

theorem test_later_same_lane_tap_does_not_bypass_earlier_hold_head_proof :
    test_later_same_lane_tap_does_not_bypass_earlier_hold_head.passed = true := by native_decide

theorem test_reference_style_hold_head_does_not_advance_without_own_click_proof :
    test_reference_style_hold_head_does_not_advance_without_own_click.passed = true := by native_decide

theorem test_same_lane_extra_click_allows_hold_head_after_tap_proof :
    test_same_lane_extra_click_allows_hold_head_after_tap.passed = true := by native_decide

theorem test_unlocked_button_frontier_still_allows_older_hold_proof :
    test_unlocked_button_frontier_still_allows_older_hold.passed = true := by native_decide

theorem test_same_area_extra_click_allows_touch_hold_head_after_touch_proof :
    test_same_area_extra_click_allows_touch_hold_head_after_touch.passed = true := by native_decide

theorem test_same_area_recursive_touches_precede_touch_hold_head_proof :
    test_same_area_recursive_touches_precede_touch_hold_head.passed = true := by native_decide

theorem test_same_area_consecutive_touch_holds_advance_shared_frontier_proof :
    test_same_area_consecutive_touch_holds_advance_shared_frontier.passed = true := by native_decide

theorem test_unlocked_touch_frontier_still_allows_older_touch_hold_proof :
    test_unlocked_touch_frontier_still_allows_older_touch_hold.passed = true := by native_decide

theorem test_touch_hold_head_registers_touch_group_only_on_direct_judgment_edge_proof :
    test_touch_hold_head_registers_touch_group_only_on_direct_judgment_edge.passed = true := by
  native_decide

theorem test_touch_hold_head_share_does_not_consume_sensor_click_proof :
    test_touch_hold_head_share_does_not_consume_sensor_click.passed = true := by native_decide

theorem test_touch_hold_group_share_non_head_normalizes_after_earlier_head_clears_proof :
    test_touch_hold_group_share_non_head_normalizes_after_earlier_head_clears.passed = true := by
  native_decide

theorem test_touch_hold_head_share_does_not_override_too_late_miss_proof :
    test_touch_hold_head_share_does_not_override_too_late_miss.passed = true := by native_decide

theorem test_touch_hold_too_late_head_does_not_consume_sensor_click_proof :
    test_touch_hold_too_late_head_does_not_consume_sensor_click.passed = true := by native_decide

theorem test_mixed_chart_golden_ap_without_break_proof :
    test_mixed_chart_golden_ap_without_break.passed = true := by native_decide

theorem test_mixed_chart_golden_ap_with_late_touch_proof :
    test_mixed_chart_golden_ap_with_late_touch.passed = true := by native_decide

theorem test_maji_grade_conversion_preserves_perfect2nd_proof :
    test_maji_grade_conversion_preserves_perfect2nd.passed = true := by native_decide

theorem test_combo_state_matches_majdata_break_theoretical_rule_proof :
    test_combo_state_matches_majdata_break_theoretical_rule.passed = true := by native_decide

theorem test_runtime_score_accumulates_base_extra_and_fc_plus_proof :
    test_runtime_score_accumulates_base_extra_and_fc_plus.passed = true := by native_decide

theorem test_runtime_break_perfect2nd_accumulates_classic_extra_proof :
    test_runtime_break_perfect2nd_accumulates_classic_extra.passed = true := by native_decide

theorem test_fast_late_disable_counter_matches_object_counter_proof :
    test_fast_late_disable_counter_matches_object_counter.passed = true := by native_decide

theorem test_runtime_fast_late_display_options_follow_game_state_proof :
    test_runtime_fast_late_display_options_follow_game_state.passed = true := by native_decide

theorem test_frame_window_zero_delta_includes_exact_point_proof :
    test_frame_window_zero_delta_includes_exact_point.passed = true := by native_decide

theorem test_frame_window_positive_delta_excludes_left_boundary_proof :
    test_frame_window_positive_delta_excludes_left_boundary.passed = true := by native_decide

theorem test_frame_window_positive_delta_includes_inside_and_right_boundary_proof :
    test_frame_window_positive_delta_includes_inside_and_right_boundary.passed = true := by native_decide

theorem test_frame_window_positive_delta_excludes_outside_window_proof :
    test_frame_window_positive_delta_excludes_outside_window.passed = true := by native_decide

theorem test_frame_window_filters_timed_hold_events_proof :
    test_frame_window_filters_timed_hold_events.passed = true := by native_decide

theorem test_manual_tactic_hold_interval_sugar_proof :
    test_manual_tactic_hold_interval_sugar.passed = true := by native_decide

theorem test_manual_tactic_chord_sugar_proof :
    test_manual_tactic_chord_sugar.passed = true := by native_decide

theorem test_typed_json_boundary_symbolic_only_proof :
    test_typed_json_boundary_symbolic_only.passed = true := by native_decide

theorem test_lowered_slide_chart_json_requires_head_timing_and_rejects_legacy_timing_proof :
    test_lowered_slide_chart_json_requires_head_timing_and_rejects_legacy_timing.passed = true := by native_decide

end LnmaiCore.RuntimeTests
