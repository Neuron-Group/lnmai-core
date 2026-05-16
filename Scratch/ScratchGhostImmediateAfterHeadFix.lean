import LnmaiCore
open LnmaiCore
open InputModel

private def flattenEventLists (lists : List (List TimedInputEvent)) : List TimedInputEvent :=
  lists.foldr (· ++ ·) []

private def spacedTimes (startTime endTime : TimePoint) (count : Nat) : List TimePoint :=
  match count with
  | 0 => []
  | 1 => [endTime]
  | countPlusOne =>
      let span := endTime - startTime
      let stride := Duration.divNat span countPlusOne
      (List.range (countPlusOne + 1)).map (fun index => startTime + Duration.scaleNat stride index)

private def releaseTimes (startTimes : List TimePoint) (endTime : TimePoint) : List TimePoint :=
  let rec loop : List TimePoint → List TimePoint
    | [] => []
    | [_last] => [endTime + Constants.FRAME_LENGTH]
    | _current :: nextStart :: rest =>
        let releaseTime := nextStart - Duration.fromMicros 1
        releaseTime :: loop (nextStart :: rest)
  loop startTimes

private def tapHeadSensorAndReleaseNextFrame (entry : NoteTimingSkeleton) : ManualTacticSequence :=
  match entry with
  | .slide spec =>
      let headTap : List ManualTacticAction := [tapAtTime spec.headInputTime spec.headZone]
      let startTimes := spacedTimes spec.startInputTime spec.endInputTime spec.pathSteps.length
      let endTimes := releaseTimes startTimes spec.endInputTime
      let headTouch :=
        match spec.pathSteps with
        | firstAreas :: _ =>
            let downs := firstAreas.map (fun area => holdSensorAtTime spec.headInputTime area true)
            let ups := firstAreas.map (fun area => holdSensorAtTime (spec.headInputTime + Constants.FRAME_LENGTH) area false)
            downs ++ ups
        | [] => []
      let pathEvents :=
        flattenEventLists <| (List.zip spec.pathSteps (List.zip startTimes endTimes)).map (fun item =>
          let areas := item.1
          let start := item.2.1
          let stop := item.2.2
          let downs := areas.map (fun area => holdSensorAtTime start area true)
          let ups := areas.map (fun area => holdSensorAtTime stop area false)
          downs ++ ups)
      mkManualTacticSequence (headTap ++ headTouch ++ pathEvents)
  | _ => resolveDefaultTimingSkeleton entry

private def ghostOverrides : List TimingSkeletonOverride :=
  [ { noteIndex := 345, resolve := tapHeadSensorAndReleaseNextFrame }
  , { noteIndex := 458, resolve := tapHeadSensorAndReleaseNextFrame }
  , { noteIndex := 534, resolve := tapHeadSensorAndReleaseNextFrame } ]

private def nonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade × Int) :=
  result.events.filterMap fun evt =>
    if evt.grade = JudgeGrade.Perfect then none else some (evt.noteIndex, evt.grade, evt.diff.toMicros)

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (tacticFromChartWithOverrides chart ghostOverrides)
      IO.println s!"missing={missingJudgedNoteIndices result}"
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result}"
      IO.println s!"nonPerfects={repr (nonPerfects result)}"
