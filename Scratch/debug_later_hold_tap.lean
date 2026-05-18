import LnmaiCore.Basic
import LnmaiCore.Storage
import LnmaiCore.Proofs.Runtime
open LnmaiCore
#eval let hold : Lifecycle.HoldNote :=
    { params := { judgeTiming := TimePoint.zero, judgeOffset := Duration.zero, noteIndex := 173 }
    , start := .button .K1
    , state := .HeadWaiting
    , length := Duration.fromMicros 200000
    , buttonQueueIndex := 0 }
  let tap : Lifecycle.TapNote :=
    { params := { judgeTiming := TimePoint.zero + Duration.fromMicros 100000, judgeOffset := Duration.zero, noteIndex := 174 }
    , lane := .S1
    , state := .Waiting
    , buttonQueueIndex := 1 }
  let st : InputModel.GameState :=
    { currentTime := TimePoint.zero
    , buttonQueueFrontiers := ButtonVec.ofFn (fun z => if z == .K1 then 0 else 0)
    , holdQueues := ButtonVec.ofFn (fun z => if z == .K1 then { notes := [hold] } else { notes := [] })
    , activeHolds := [(.K1, hold)]
    , tapQueues := ButtonVec.ofFn (fun z => if z == .K1 then { notes := [tap] } else { notes := [] }) }
  let now := TimePoint.zero + Duration.fromMicros 100000
  let batch : InputModel.TimedInputBatch := { currentTime := now, events := [InputModel.TimedInputEvent.buttonClick now .K1] }
  let (nextState, events, _, _) := Scheduler.stepFrameTimed st batch
  (events, nextState.buttonQueueFrontiers.getD .K1 99, nextState.holdQueues.getD .K1 {notes:=[]}, nextState.tapQueues.getD .K1 {notes:=[]}, nextState.activeHolds)
