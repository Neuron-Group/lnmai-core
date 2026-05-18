import LnmaiCore

open LnmaiCore
open InputModel

namespace Proofs.RealChartVerification100524

def checkpointName : String := "100524_[協]Hand in Hand"

def checkpointAssetPath : String :=
  "tools/assets/100524_[協]Hand in Hand/maidata.txt"

def checkpointLevel : Nat := 7

def checkpointChart : ChartLoader.ChartSpec :=
  simai_lowered_chart_file_at! 7 "tools/assets/100524_[協]Hand in Hand/maidata.txt"

private def sensorTapModule : TimingSkeletonModule :=
  noteKindModule .tap (fun entry =>
    match entry with
    | .tap _ _ inputTime zone =>
        mkManualTacticSequence [touchAtTime inputTime zone.toOuterSensorArea]
    | _ =>
        resolveDefaultTimingSkeleton entry)

def checkpointCustomTactic : ManualTacticSequence :=
  tacticFromChartWithModules checkpointChart [sensorTapModule]

def checkpointCustomResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart checkpointCustomTactic

def checkpointNonPerfects : List (Nat × JudgeGrade) :=
  checkpointCustomResult.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

theorem checkpoint_custom_has_no_missing_notes :
    missingJudgedNoteIndices checkpointCustomResult = [] := by
  native_decide

theorem checkpoint_custom_achieves_ap :
    achievesAP checkpointCustomResult = true := by
  native_decide

theorem checkpoint_custom_achieves_ap_plus :
    achievesAPPlus checkpointCustomResult = true := by
  native_decide

theorem checkpoint_custom_has_no_non_perfect_notes :
    checkpointNonPerfects = [] := by
  native_decide

end Proofs.RealChartVerification100524
