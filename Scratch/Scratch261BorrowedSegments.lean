import LnmaiCore
open LnmaiCore
open InputModel
open LnmaiCore.ChartLoader

private def keepOnlyWindow (idx : Nat) : Bool :=
  255 <= idx && idx <= 270

private def keepBorrowed261Events (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    t = 64112899 || -- 261 head tap
    t = 64112900 || -- 260 A7 on
    t = 64173382 || t = 64173383 ||
    t = 64233865 || t = 64233866 ||
    t = 64294348 || t = 64294349 ||
    t = 64354831 || t = 64354832 ||
    t = 64371502 || -- 260 end
    t = 64596769 || -- 262 K5 tap
    t = 64838704    -- 263 K6 tap
  )

private def addImmediateRelease261 (events : List TimedInputEvent) : List TimedInputEvent :=
  holdSensorAt 64112899 SensorArea.A1 true ::
  holdSensorAt (64112899 + Constants.FRAME_LENGTH.toMicros) SensorArea.A1 false ::
  events

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
      let seq := mkManualTacticSequence (addImmediateRelease261 (keepBorrowed261Events base.events))
      let result := simulateChartSpecWithTactic localChart seq
      IO.println s!"ap={achievesAP result} applus={achievesAPPlus result}"
      IO.println s!"nonPerfects={repr (nonPerfects result)}"
