import LnmaiCore
open LnmaiCore
open InputModel
open LnmaiCore.ChartLoader

private def remove261BodyEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    !(t = 64112899 || t = 64596770 || t = 64677415 || t = 64758059 || t = 64758060 || t = 64838704 || t = 64838705 ||
      t = 64919349 || t = 64919350 || t = 64999994 || t = 64999995 || t = 65080639 || t = 65080640 || t = 65097308))

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

private def nonPerfects (result : RuntimeSimulationResult) : List Nat :=
  result.events.filterMap (fun evt => if evt.grade = .Perfect then none else some evt.noteIndex)

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let some note261 := chart.slides.find? (fun (n : SlideChartNote) => n.noteIndex = 261)
        | IO.println "missing 261"; return
      for low in [255, 256, 257, 258, 259, 260] do
        for high in [263, 264, 265, 266, 267, 268, 269, 270] do
          let localChart : ChartSpec :=
            { taps := chart.taps.filter (fun n => low <= n.noteIndex && n.noteIndex <= high)
            , holds := chart.holds.filter (fun n => low <= n.noteIndex && n.noteIndex <= high)
            , touches := chart.touches.filter (fun n => low <= n.noteIndex && n.noteIndex <= high)
            , touchHolds := chart.touchHolds.filter (fun n => low <= n.noteIndex && n.noteIndex <= high)
            , slides := chart.slides.filter (fun n => low <= n.noteIndex && n.noteIndex <= high)
            , slideSkipping := chart.slideSkipping }
          let base := defaultTacticFromChart localChart
          let seq := mkManualTacticSequence (remove261BodyEvents base.events ++ headOnly261 note261)
          let result := simulateChartSpecWithTactic localChart seq
          let bads := nonPerfects result
          if bads = [261] then
            IO.println s!"window {low}-{high}"
            return
      IO.println "none"
