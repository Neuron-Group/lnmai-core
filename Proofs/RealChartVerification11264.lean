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

private def local261NonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

private def local261TouchReplacementModule
    (noteIndex : Nat) (timeMicros : Int) (area : SensorArea) : TimingSkeletonModule :=
  fixedNoteIndexModule noteIndex
    (mkManualTacticSequence [touchAt timeMicros area])

private def local261ImmediateReleaseModule : TimingSkeletonModule :=
  noteIndexModule 261 (fun _ =>
    mkManualTacticSequence
      [ touchAt 64112899 SensorArea.A1
      , holdSensorAt 64112899 SensorArea.A1 true
      , holdSensorAt (64112899 + Constants.FRAME_LENGTH.toMicros) SensorArea.A1 false ])

private def local261DefaultBodyOnly (entry : NoteTimingSkeleton) : ManualTacticSequence :=
  match entry with
  | .slide spec =>
      mkManualTacticSequence <|
        (resolveSingleTrackSlideWithHeadEvenly spec).events.filter (fun evt =>
          match evt with
          | .buttonClick _ _ => false
          | _ => true)
  | _ => mkManualTacticSequence []

private def local261HoldThroughStartModule : TimingSkeletonModule :=
  noteIndexModule 261 (fun entry =>
    mkManualTacticSequence
      [ touchAt 64112899 SensorArea.A1
      , holdSensorAt 64112899 SensorArea.A1 true
      , holdSensorAt 64354837 SensorArea.A1 false ]
    ++ local261DefaultBodyOnly entry)

private def local261SharedTapReplacementModules : List TimingSkeletonModule :=
  [ local261TouchReplacementModule 262 64596769 SensorArea.A5
  , local261TouchReplacementModule 263 64838704 SensorArea.A6
  , local261TouchReplacementModule 266 65927412 SensorArea.A5 ]

private def local261ImmediateReleaseResult : RuntimeSimulationResult :=
  let seq := tacticFromChartWithModules local261Chart
    (local261ImmediateReleaseModule :: local261SharedTapReplacementModules)
  simulateChartSpecWithTactic local261Chart seq

private def local261HoldThroughStartResult : RuntimeSimulationResult :=
  let seq := tacticFromChartWithModules local261Chart
    (local261HoldThroughStartModule :: local261SharedTapReplacementModules)
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
