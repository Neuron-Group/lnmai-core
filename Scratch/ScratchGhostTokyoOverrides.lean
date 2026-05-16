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

private def holdHeadThroughSecondStep (entry : NoteTimingSkeleton) : ManualTacticSequence :=
  match entry with
  | .slide spec =>
      let headTap : List ManualTacticAction := [tapAtTime spec.headInputTime spec.headZone]
      let startTimes := spacedTimes spec.startInputTime spec.endInputTime spec.pathSteps.length
      let endTimes := releaseTimes startTimes spec.endInputTime
      let headHold :=
        match spec.pathSteps, startTimes.drop 1 with
        | firstAreas :: _, secondStart :: _ =>
            let releaseTime := secondStart + Constants.FRAME_LENGTH
            let downs := firstAreas.map (fun area => holdSensorAtTime spec.headInputTime area true)
            let ups := firstAreas.map (fun area => holdSensorAtTime releaseTime area false)
            downs ++ ups
        | _, _ => []
      let pathEvents :=
        flattenEventLists <| (List.zip spec.pathSteps (List.zip startTimes endTimes)).map (fun item =>
          let areas := item.1
          let start := item.2.1
          let stop := item.2.2
          let downs := areas.map (fun area => holdSensorAtTime start area true)
          let ups := areas.map (fun area => holdSensorAtTime stop area false)
          downs ++ ups)
      mkManualTacticSequence (headTap ++ headHold ++ pathEvents)
  | _ => resolveDefaultTimingSkeleton entry

private def ghostOverrides : List TimingSkeletonOverride :=
  [ { noteIndex := 345, resolve := holdHeadThroughSecondStep }
  , { noteIndex := 458, resolve := holdHeadThroughSecondStep }
  , { noteIndex := 534, resolve := holdHeadThroughSecondStep } ]

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (tacticFromChartWithOverrides chart ghostOverrides)
      IO.println s!"missing={missingJudgedNoteIndices result}"
      IO.println s!"ap={achievesAP result}"
      for evt in result.events do
        if evt.grade != JudgeGrade.Perfect then
          IO.println s!"bad={repr evt}"
