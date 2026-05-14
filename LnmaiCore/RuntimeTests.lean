import LnmaiCore.Basic
import LnmaiCore.Storage
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
  , touchPanelOffset := Constants.TOUCH_PANEL_OFFSET }

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

private def activeWifiTooLateState : InputModel.GameState :=
  let unfinished : Lifecycle.SlideArea :=
    { targetAreas := [.A1], isLast := true }
  let slide : Lifecycle.SlideNote :=
    { params := { judgeTiming := secs 1, judgeOffset := Duration.zero, noteIndex := 53 }
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
    , judgeQueues := [[unfinished], [unfinished], [unfinished]] }
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
        (ended && evt.kind = .Slide)
        "wifi too-late path emits the event and ends immediately"
  | _, _ => passCase "wifi_too_late_ends_immediately" false "expected one ended wifi slide and one event"

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
  , test_conn_child_progress_force_finishes_parent
  , test_slide_judge_uses_touch_panel_offset
  , test_touch_group_majority_shares_result_same_frame
  , test_conn_child_pending_finish_becomes_checkable
  , test_wifi_classic_tail_progress_uses_special_marker
  , test_wifi_center_cleared_progress_uses_special_marker
  , test_wifi_judged_wait_emits_delayed_event_then_hides
  , test_wifi_too_late_ends_immediately
  , test_typed_json_boundary_symbolic_only
  ]

def passedCount : Nat :=
  (all.filter (·.passed)).length

-- #eval all
-- #eval (passedCount, all.length)

end LnmaiCore.RuntimeTests
