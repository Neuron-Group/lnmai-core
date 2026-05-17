import LnmaiCore

open LnmaiCore
open InputModel

namespace Proofs.RealChartVerification7thSense

def checkpointName : String := "462_7thSense"

def checkpointAssetPath : String :=
  "tools/assets/462_7thSense/maidata.txt"

def checkpointLevel : Nat := 5

def checkpointChart : ChartLoader.ChartSpec :=
  simai_lowered_chart_file_at! 5 "tools/assets/462_7thSense/maidata.txt"

private def localSkipWindow (idx : Nat) : Bool :=
  649 <= idx && idx <= 653

private def localSkipChart : ChartLoader.ChartSpec :=
  { taps := checkpointChart.taps.filter (fun n => localSkipWindow n.noteIndex)
  , holds := checkpointChart.holds.filter (fun n => localSkipWindow n.noteIndex)
  , touches := checkpointChart.touches.filter (fun n => localSkipWindow n.noteIndex)
  , touchHolds := checkpointChart.touchHolds.filter (fun n => localSkipWindow n.noteIndex)
  , slides := checkpointChart.slides.filter (fun n => localSkipWindow n.noteIndex)
  , slideSkipping := checkpointChart.slideSkipping }

private def localSkipResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic localSkipChart (defaultTacticFromChart localSkipChart)

private def localSkipNonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

theorem local_skip_chain_has_expected_parser_slice :
    localSkipChart.slides.map (fun n => n.noteIndex) = [649, 650, 651, 652, 653] := by
  native_decide

theorem local_skip_chain_default_replay_has_no_missing_notes :
    missingJudgedNoteIndices localSkipResult = [] := by
  native_decide

theorem local_skip_chain_default_replay_achieves_ap :
    achievesAP localSkipResult = true := by
  native_decide

theorem local_skip_chain_default_replay_has_no_non_perfect_notes :
    localSkipNonPerfects localSkipResult = [] := by
  native_decide

theorem parsed_slide_queue_replays_like_reference_kernel :
    localSkipResult.events.map (fun evt => evt.noteIndex) = [649, 650, 651, 652, 653] := by
  native_decide

theorem parsed_skip_sensitive_slide_chain_has_no_missing_notes :
    missingJudgedNoteIndices localSkipResult = [] := by
  native_decide

theorem parsed_skip_sensitive_slide_chain_achieves_ap :
    achievesAP localSkipResult = true := by
  native_decide

theorem parsed_skip_sensitive_slide_chain_preserves_queue_completion_order :
    localSkipResult.events.map (fun evt => evt.noteIndex) = [649, 650, 651, 652, 653] := by
  native_decide

end Proofs.RealChartVerification7thSense
