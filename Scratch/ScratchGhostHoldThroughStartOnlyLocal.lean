import LnmaiCore
open LnmaiCore
open InputModel
open LnmaiCore.ChartLoader

private def holdHeadThroughStart (note : SlideChartNote) : ManualTacticSequence :=
  let fallback := note.slot.toOuterSensorArea
  let firstAreas :=
    match note.judgeQueues with
    | queue :: _ =>
        match queue with
        | first :: _ =>
            match first.policy, first.targetAreas with
            | .And, targets => if targets.isEmpty then [fallback] else targets
            | .Or, targets => if targets.isEmpty then [fallback] else targets
        | [] => []
    | [] => []
  let releaseTime := note.startTiming + Constants.TOUCH_PANEL_OFFSET
  mkManualTacticSequence <|
    [tapAtTime note.timing note.slot.toButtonZone] ++
    (firstAreas.map (fun area => holdSensorAtTime note.timing area true)) ++
    (firstAreas.map (fun area => holdSensorAtTime releaseTime area false))

private def override345 : IO TimingSkeletonOverride := do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => panic! s!"parse error: {repr err}"
  | .ok chart =>
      let some note := chart.slides.find? (fun (n : SlideChartNote) => n.noteIndex = 345)
        | panic! "missing 345"
      pure { noteIndex := 345, resolve := fun _ => holdHeadThroughStart note }

private def nonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt => if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  let ov ← override345
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (tacticFromChartWithOverrides chart [ov])
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result}"
      IO.println s!"nonPerfects={repr (nonPerfects result)}"
