import LnmaiCore.Basic
import LnmaiCore.Storage
import LnmaiCore.Proofs.Runtime

open LnmaiCore

private def dur (micros : Int) : Duration := Duration.fromMicros micros

#eval let hold1 : Lifecycle.HoldNote :=
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
  let st : InputModel.GameState :=
    { currentTime := TimePoint.zero
    , touchQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [], currentIndex := 0 } else { notes := [] })
    , touchHoldQueues := SensorVec.ofFn (fun area => if area == .A1 then { notes := [hold1, hold2] } else { notes := [] })
    , activeTouchHolds := [(.A1, hold1), (.A1, hold2)] }
  let b1 : InputModel.TimedInputBatch :=
    { currentTime := TimePoint.zero
    , events := [InputModel.TimedInputEvent.sensorClick TimePoint.zero .A1
                , InputModel.TimedInputEvent.sensorHold TimePoint.zero .A1 true] }
  let (s1,e1,_,_) := Scheduler.stepFrameTimed st b1
  let t2 := TimePoint.zero + Constants.FRAME_LENGTH
  let b2 : InputModel.TimedInputBatch :=
    { currentTime := t2
    , events := [InputModel.TimedInputEvent.sensorClick t2 .A1
                , InputModel.TimedInputEvent.sensorHold t2 .A1 true] }
  let (s2,e2,_,_) := Scheduler.stepFrameTimed s1 b2
  (s1.touchQueues.getD .A1 {notes:=[]}, s1.touchHoldQueues.getD .A1 {notes:=[]}, s1.activeTouchHolds, e1,
   s2.touchQueues.getD .A1 {notes:=[]}, s2.touchHoldQueues.getD .A1 {notes:=[]}, s2.activeTouchHolds, e2)
