import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader
open InputModel

private def sensorInputTime' (semanticTime : TimePoint) : TimePoint :=
  semanticTime + Constants.TOUCH_PANEL_OFFSET

private def nthOpt {α : Type} : List α → Nat → Option α
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: xs, n + 1 => nthOpt xs n

private def mergeAreas (left right : List SensorArea) : List SensorArea :=
  (left ++ right).eraseDups

private def chooseAllAreas (fallback : SensorArea) (step : SlideAreaSpec) : List SensorArea :=
  match step.policy, step.targetAreas with
  | .And, targets => if targets.isEmpty then [fallback] else targets
  | .Or, targets => if targets.isEmpty then [fallback] else targets

private def unionSlidePathSteps (note : SlideChartNote) : List (List SensorArea) :=
  let fallback := note.slot.toOuterSensorArea
  let maxLen := note.judgeQueues.foldl (fun acc q => max acc q.length) 0
  (List.range maxLen).map (fun idx =>
    note.judgeQueues.foldl
      (fun acc q =>
        match nthOpt q idx with
        | some step => mergeAreas acc (chooseAllAreas fallback step)
        | none => acc)
      ([] : List SensorArea))

private def representativePathSteps (note : SlideChartNote) : List (List SensorArea) :=
  let fallback := note.slot.toOuterSensorArea
  match note.judgeQueues with
  | queue :: _ =>
      queue.map (fun step =>
        match step.policy, step.targetAreas with
        | .And, targets => if targets.isEmpty then [fallback] else targets
        | .Or, targets => if targets.isEmpty then [fallback] else targets)
  | [] => []

private def evenlySpacedTimesBetween (startTime endTime : TimePoint) (count : Nat) : List TimePoint :=
  match count with
  | 0 => []
  | 1 => [endTime]
  | count + 1 =>
      let span := endTime - startTime
      let stride := Duration.divNat span count
      (List.range (count + 1)).map (fun index => startTime + Duration.scaleNat stride index)

private def slideStepReleaseTimes (startTimes : List TimePoint) (endTime : TimePoint) : List TimePoint :=
  let rec loop : List TimePoint → List TimePoint
    | [] => []
    | [_last] => [endTime + Constants.FRAME_LENGTH]
    | _current :: nextStart :: rest =>
        let releaseTime := nextStart - Duration.fromMicros 1
        releaseTime :: loop (nextStart :: rest)
  loop startTimes

private def flattenEventLists (lists : List (List TimedInputEvent)) : List TimedInputEvent :=
  lists.foldr (· ++ ·) []

private def resolveSlideMixed (note : SlideChartNote) : ManualTacticSequence :=
  let headTap : List ManualTacticAction := [tapAtTime note.timing note.slot.toButtonZone]
  let judgeSemanticTime := note.judgeAt.getD (note.startTiming + note.length)
  let startInputTime := sensorInputTime' note.startTiming
  let endInputTime := sensorInputTime' judgeSemanticTime
  let pathSteps := if note.slideKind = .Wifi then unionSlidePathSteps note else representativePathSteps note
  let startTimes := evenlySpacedTimesBetween startInputTime endInputTime pathSteps.length
  let endTimes := slideStepReleaseTimes startTimes endInputTime
  let headHold :=
    if note.slideKind = .Wifi then
      match pathSteps with
      | firstAreas :: _ =>
          let downs := firstAreas.map (fun area => holdSensorAtTime note.timing area true)
          let ups := firstAreas.map (fun area => holdSensorAtTime startInputTime area false)
          downs ++ ups
      | [] => []
    else
      []
  let pathEvents :=
    flattenEventLists <| (List.zip pathSteps (List.zip startTimes endTimes)).map (fun item =>
      let areas := item.1
      let start := item.2.1
      let stop := item.2.2
      let downs := areas.map (fun area => holdSensorAtTime start area true)
      let ups := areas.map (fun area => holdSensorAtTime stop area false)
      downs ++ ups)
  mkManualTacticSequence (headTap ++ headHold ++ pathEvents)

private def mixedTacticFromChart (chart : ChartSpec) : ManualTacticSequence :=
  let taps := chart.taps.map (fun note => tapAtTime note.timing note.slot.toButtonZone)
  let holds :=
    flattenEventLists <| chart.holds.map (fun note =>
      let releaseTime := note.timing + note.length + Duration.scaleNat Constants.FRAME_LENGTH 4
      [ tapAtTime note.timing note.slot.toButtonZone
      , holdButtonAtTime note.timing note.slot.toButtonZone true
      , holdButtonAtTime releaseTime note.slot.toButtonZone false ])
  let touches := chart.touches.map (fun note => touchAtTime (sensorInputTime' note.timing) note.sensorPos)
  let touchHolds :=
    flattenEventLists <| chart.touchHolds.map (fun note =>
      let inputTime := sensorInputTime' note.timing
      let releaseTime := sensorInputTime' (note.timing + note.length) + Duration.scaleNat Constants.FRAME_LENGTH 4
      [ touchAtTime inputTime note.sensorPos
      , holdSensorAtTime inputTime note.sensorPos true
      , holdSensorAtTime releaseTime note.sensorPos false ])
  let slides := flattenEventLists (chart.slides.map (fun note => (resolveSlideMixed note).events))
  mkManualTacticSequence (taps ++ holds ++ touches ++ touchHolds ++ slides)

private def nonPerfects (result : RuntimeSimulationResult) : List Nat :=
  result.events.filterMap (fun evt => if evt.grade = .Perfect then none else some evt.noteIndex)

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (mixedTacticFromChart chart)
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result} bads={nonPerfects result}"
