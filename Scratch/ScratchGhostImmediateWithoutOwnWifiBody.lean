import LnmaiCore
open LnmaiCore
open InputModel
open LnmaiCore.ChartLoader

private def keepEventForNonTarget (evt : TimedInputEvent) : Bool :=
  let t := evt.at.toMicros
  !(t = 81774171 || t = 82258042 || t = 82338686 || t = 82338687 || t = 82419331 || t = 82419332 || t = 82499976 || t = 82499977 || t = 82516644)

private def immediateHeadOnly (note : SlideChartNote) : List TimedInputEvent :=
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
  [ tapAtTime note.timing note.slot.toButtonZone ] ++
  (firstAreas.map (fun area => holdSensorAtTime note.timing area true)) ++
  (firstAreas.map (fun area => holdSensorAtTime (note.timing + Constants.FRAME_LENGTH) area false))

private def nonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt => if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let base := defaultTacticFromChart chart
      let preserved := base.events.filter keepEventForNonTarget
      let some target := chart.slides.find? (fun (n : SlideChartNote) => n.noteIndex = 345)
        | IO.println "missing target"; return
      let seq := mkManualTacticSequence (preserved ++ immediateHeadOnly target)
      let result := simulateChartSpecWithTactic chart seq
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result}"
      IO.println s!"nonPerfects={repr (nonPerfects result)}"
