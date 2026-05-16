import LnmaiCore.Basic
import LnmaiCore.Scheduler
import LnmaiCore.InputModel
import LnmaiCore.Lifecycle

open LnmaiCore

private def tp (secondsMicros : Int) : TimePoint :=
  TimePoint.fromMicros secondsMicros

private def dur (micros : Int) : Duration :=
  Duration.fromMicros micros

private def sameTimeConnPairState : InputModel.GameState :=
  let mkArea (area : SensorArea) : Lifecycle.SlideArea :=
    { targetAreas := [area], isLast := false }
  let mkLast (area : SensorArea) : Lifecycle.SlideArea :=
    { targetAreas := [area], isLast := true }
  let head : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 117600054, judgeOffset := Duration.zero, noteIndex := 80 }
    , lane := .S1
    , state := .Active (dur 400000)
    , length := dur 400000
    , startTiming := tp 117200054
    , slideKind := .ConnPart
    , isConnSlide := true
    , isGroupPartHead := true
    , isGroupPartEnd := false
    , trackCount := 1
    , initialQueueRemaining := 5
    , totalJudgeQueueLen := 5
    , isCheckable := true
    , judgeQueues := [[mkArea .A1, mkArea .A8, mkArea .A7, mkArea .A6, mkLast .A5]] }
  let tail : Lifecycle.SlideNote :=
    { params := { judgeTiming := tp 117600054, judgeOffset := Duration.zero, noteIndex := 81 }
    , lane := .S1
    , state := .Active (dur 400000)
    , length := dur 400000
    , startTiming := tp 117200054
    , slideKind := .ConnPart
    , isConnSlide := true
    , parentNoteIndex := some 80
    , isGroupPartHead := false
    , isGroupPartEnd := true
    , trackCount := 1
    , initialQueueRemaining := 5
    , totalJudgeQueueLen := 5
    , isCheckable := false
    , judgeQueues := [[mkArea .A1, mkArea .A2, mkArea .A3, mkArea .A4, mkLast .A5]] }
  { currentTime := tp 117183387
  , slides := [head, tail] }

#eval do
  let sensorHeld := SensorVec.ofFn (fun area => area = SensorArea.A1 || area = SensorArea.A8 || area = SensorArea.A2)
  let input : InputModel.FrameInput :=
    { sensorHeld := sensorHeld
    , delta := dur 16667 }
  let (nextState, events, _, _) := Scheduler.stepFrame sameTimeConnPairState input
  IO.println s!"events={repr events}"
  IO.println s!"slides={repr nextState.slides}"
