import LnmaiCore

open LnmaiCore
open InputModel

namespace Proofs.RealChartVerification11264

def checkpointName : String := "11264_幽霊東京"

def checkpointAssetPath : String :=
  "tools/assets/11264_幽霊東京/maidata.txt"

def checkpointLevel : Nat := 5

def checkpointChart : ChartLoader.ChartSpec :=
  simai_lowered_chart_file_at! 5 "tools/assets/11264_幽霊東京/maidata.txt"

def checkpointResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart (defaultTacticFromChart checkpointChart)

theorem checkpoint_has_no_missing_notes :
    missingJudgedNoteIndices checkpointResult = [] := by
  native_decide

theorem checkpoint_achieves_ap :
    achievesAP checkpointResult = true := by
  native_decide

theorem checkpoint_has_no_non_perfect_notes :
    (checkpointResult.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex) = [] := by
  native_decide

private def local261Window (idx : Nat) : Bool :=
  255 <= idx && idx <= 270

private def local261Chart : ChartLoader.ChartSpec :=
  { taps := checkpointChart.taps.filter (fun n => local261Window n.noteIndex)
  , holds := checkpointChart.holds.filter (fun n => local261Window n.noteIndex)
  , touches := checkpointChart.touches.filter (fun n => local261Window n.noteIndex)
  , touchHolds := checkpointChart.touchHolds.filter (fun n => local261Window n.noteIndex)
  , slides := checkpointChart.slides.filter (fun n => local261Window n.noteIndex)
  , slideSkipping := checkpointChart.slideSkipping }

private def stripCriticalButtonEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    match evt with
    | .buttonClick _ zone =>
        !((t = 64112899 && zone = .K1) ||
          (t = 64596769 && zone = .K5) ||
          (t = 64838704 && zone = .K6) ||
          (t = 65927412 && zone = .K5))
    | _ => true)

private def strip261BodyEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.filter (fun evt =>
    let t := evt.at.toMicros
    !(t = 64596770 || t = 64677415 || t = 64758059 || t = 64758060 || t = 64838705 ||
      t = 64919349 || t = 64919350 || t = 64999994 || t = 64999995 || t = 65080639 ||
      t = 65080640 || t = 65097308))

private def local261NonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

private def immediateRelease261Actions : List TimedInputEvent :=
  [ touchAt 64112899 SensorArea.A1
  , holdSensorAt 64112899 SensorArea.A1 true
  , holdSensorAt (64112899 + Constants.FRAME_LENGTH.toMicros) SensorArea.A1 false
  , touchAt 64596769 SensorArea.A5
  , touchAt 64838704 SensorArea.A6
  , touchAt 65927412 SensorArea.A5
  ]

private def holdThroughStart261Actions : List TimedInputEvent :=
  [ touchAt 64112899 SensorArea.A1
  , holdSensorAt 64112899 SensorArea.A1 true
  , holdSensorAt 64354837 SensorArea.A1 false
  , touchAt 64596769 SensorArea.A5
  , touchAt 64838704 SensorArea.A6
  , touchAt 65927412 SensorArea.A5
  ]

private def local261ImmediateReleaseResult : RuntimeSimulationResult :=
  let base := defaultTacticFromChart local261Chart
  let seq := mkManualTacticSequence
    (stripCriticalButtonEvents (strip261BodyEvents base.events) ++ immediateRelease261Actions)
  simulateChartSpecWithTactic local261Chart seq

private def local261HoldThroughStartResult : RuntimeSimulationResult :=
  let base := defaultTacticFromChart local261Chart
  let seq := mkManualTacticSequence
    (stripCriticalButtonEvents base.events ++ holdThroughStart261Actions)
  simulateChartSpecWithTactic local261Chart seq

theorem local_1xs5_immediate_release_fails :
    local261NonPerfects local261ImmediateReleaseResult = [(261, JudgeGrade.Miss)] := by
  native_decide

theorem local_1xs5_hold_through_start_achieves_ap :
    achievesAP local261HoldThroughStartResult = true := by
  native_decide

theorem local_1xs5_hold_through_start_has_no_non_perfect_notes :
    local261NonPerfects local261HoldThroughStartResult = [] := by
  native_decide

end Proofs.RealChartVerification11264
