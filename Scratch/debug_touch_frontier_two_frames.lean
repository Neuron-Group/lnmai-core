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
  let t1 := TimePoint.zero + Constants.FRAME_LENGTH
  let batch1 : InputModel.TimedInputBatch := { currentTime := t1, events := [] }
  let (st1, ev1, _, _) := Scheduler.stepFrameTimed st batch1
  let t2 := t1 + Constants.FRAME_LENGTH
  let batch2 : InputModel.TimedInputBatch := { currentTime := t2, events := [InputModel.TimedInputEvent.sensorClick t2 .A1] }
  let (st2, ev2, _, _) := Scheduler.stepFrameTimed st1 batch2
  (ev1, st1.activeTouchHolds, ev2, st2.touchHoldQueues.getD .A1 { notes := [] }, st2.activeTouchHolds)
