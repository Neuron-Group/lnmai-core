import LnmaiCore.Basic
import LnmaiCore.Storage
import LnmaiCore.Proofs.Runtime

open LnmaiCore

private def dur (micros : Int) : Duration := Duration.fromMicros micros

#eval let touchHold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 170 }
    , start := .sensor .A1
    , state := .HeadWaiting
    , length := dur 200000
    , isTouchHold := true
    , touchQueueIndex := 1 }
  let st : InputModel.GameState :=
    { currentTime := TimePoint.zero
    , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [], currentIndex := 2 } else { notes := [] })
    , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [touchHold] } else { notes := [] })
    , activeTouchHolds := [(.A1, touchHold)] }
  let now := TimePoint.zero + Constants.FRAME_LENGTH
  let batch : InputModel.TimedInputBatch :=
    { currentTime := now
    , events := [InputModel.TimedInputEvent.sensorClick now .A1] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed st batch
  (events, nextState.touchHoldQueues.getD .A1 { notes := [] }, nextState.activeTouchHolds)
