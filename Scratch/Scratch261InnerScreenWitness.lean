import LnmaiCore
open LnmaiCore
open InputModel
open LnmaiCore.ChartLoader

private def keepOnlyWindow (idx : Nat) : Bool :=
  255 <= idx && idx <= 270

private def stripCriticalButtonEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    match evt with
    | .buttonClick _ zone =>
        !((t = 64112899 && zone = .K1) || (t = 64596769 && zone = .K5) || (t = 64838704 && zone = .K6) || (t = 65927412 && zone = .K5))
    | _ => true)

private def strip261BodyEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    !(t = 64596770 || t = 64677415 || t = 64758059 || t = 64758060 || t = 64838705 ||
      t = 64919349 || t = 64919350 || t = 64999994 || t = 64999995 || t = 65080639 || t = 65080640 || t = 65097308))

private def innerScreenCriticalActions : List TimedInputEvent :=
  [ touchAt 64112899 SensorArea.A1
  , holdSensorAt 64112899 SensorArea.A1 true
  , holdSensorAt (64112899 + Constants.FRAME_LENGTH.toMicros) SensorArea.A1 false
  , touchAt 64596769 SensorArea.A5
  , touchAt 64838704 SensorArea.A6
  , touchAt 65927412 SensorArea.A5
  ]

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
      let seq := mkManualTacticSequence (stripCriticalButtonEvents (strip261BodyEvents base.events) ++ innerScreenCriticalActions)
      let result := simulateChartSpecWithTactic localChart seq
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result}"
      IO.println s!"nonPerfects={repr (nonPerfects result)}"
