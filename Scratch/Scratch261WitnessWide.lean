import LnmaiCore
open LnmaiCore
open InputModel
open LnmaiCore.ChartLoader

private def keepOnlyWindow (idx : Nat) : Bool :=
  255 <= idx && idx <= 270

private def remove261BodyEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    !(t = 64112899 || t = 64596770 || t = 64677415 || t = 64758059 || t = 64758060 || t = 64838704 || t = 64838705 ||
      t = 64919349 || t = 64919350 || t = 64999994 || t = 64999995 || t = 65080639 || t = 65161284 || t = 65161285))

private def headOnly261 (note : SlideChartNote) : List TimedInputEvent :=
  let fallback := note.slot.toOuterSensorArea
  let firstAreas :=
    match note.judgeQueues with
    | queue :: _ =>
        match queue with
        | first :: _ =>
            match first.policy, first.targetAreas with
            | AreaPolicy.And, targets => if targets.isEmpty then [fallback] else targets
            | AreaPolicy.Or, targets => if targets.isEmpty then [fallback] else targets
        | [] => []
    | [] => []
  [tapAtTime note.timing note.slot.toButtonZone] ++
  (firstAreas.map (fun area => holdSensorAtTime note.timing area true)) ++
  (firstAreas.map (fun area => holdSensorAtTime (note.timing + Constants.FRAME_LENGTH) area false))

private def nonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt => if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let localChart : ChartSpec :=
        { taps := chart.taps.filter (fun n => keepOnlyWindow n.noteIndex)
        , holds := chart.holds.filter (fun n => keepOnlyWindow n.noteIndex)
        , touches := chart.touches.filter (fun n => keepOnlyWindow n.noteIndex)
        , touchHolds := chart.touchHolds.filter (fun n => keepOnlyWindow n.noteIndex)
        , slides := chart.slides.filter (fun n => keepOnlyWindow n.noteIndex)
        , slideSkipping := chart.slideSkipping }
      let base := defaultTacticFromChart localChart
      let preserved := remove261BodyEvents base.events
      let some note261 := localChart.slides.find? (fun (n : SlideChartNote) => n.noteIndex = 261)
        | IO.println "missing 261"; return
      let seq := mkManualTacticSequence (preserved ++ headOnly261 note261)
      let result := simulateChartSpecWithTactic localChart seq
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result}"
      IO.println s!"nonPerfects={repr (nonPerfects result)}"
