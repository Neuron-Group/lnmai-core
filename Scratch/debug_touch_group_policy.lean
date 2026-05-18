import LnmaiCore.Basic
import LnmaiCore.Storage
import LnmaiCore.Proofs.Runtime
open LnmaiCore
private def tp (secondsMicros : Int) : TimePoint := TimePoint.fromMicros secondsMicros
private def dur (micros : Int) : Duration := Duration.fromMicros micros
#eval let touch : Lifecycle.TouchNote :=
    { params := { judgeTiming := tp 1000000, judgeOffset := Duration.zero, noteIndex := 91 }
    , state := .Judgeable
    , sensorPos := .A1
    , touchGroupId := some 13
    , touchGroupSize := 3 }
  let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := tp 1000000, judgeOffset := Duration.zero, noteIndex := 92 }
    , start := .sensor .A3
    , state := .HeadJudgeable
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 0
    , touchGroupId := some 13
    , touchGroupSize := 3
    , touchHoldGroupId := some 13
    , touchHoldGroupSize := 3 }
  let st : InputModel.GameState :=
    { currentTime := tp 984000
    , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touch] } else { notes := [] })
    , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A3 then { notes := [touchHold] } else { notes := [] })
    , activeTouchHolds := [(.A3, touchHold)]
    , touchGroupStates := [{ groupId := 13, count := 2, size := 3, grade := .Perfect, diff := Duration.zero }] }
  let input := { buttonClicked := ButtonVec.replicate 8 false,
                 buttonHeld := ButtonVec.replicate 8 false,
                 sensorClicked := SensorVec.ofFn (fun a => a == .A1),
                 sensorHeld := SensorVec.replicate 33 false,
                 buttonClickCount := ButtonVec.replicate 8 0,
                 sensorClickCount := SensorVec.ofFn (fun a => if a == .A1 then 1 else 0),
                 delta := dur 16000 }
  let (nextState, events, _, _) := Scheduler.stepFrame st input
  (events, nextState.touchQueues.getD .A1 {notes:=[]}, nextState.touchHoldQueues.getD .A3 {notes:=[]}, nextState.activeTouchHolds)
