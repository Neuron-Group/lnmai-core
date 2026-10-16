import LnmaiCore.RealChart.Koishi

open LnmaiCore

namespace Proofs.RealChartVerificationKoishiDISCO

def checkpointName : String := "小石DISCO"

def checkpointAssetPath : String :=
  "tools/assets/小石DISCO/maidata.txt"

def checkpointLevel : Nat := 5

def checkpointChart : ChartLoader.ChartSpec :=
  simai_lowered_chart_file_at! 5 "tools/assets/小石DISCO/maidata.txt"

def checkpointResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart (defaultTacticFromChart checkpointChart)

private abbrev holdReleaseModules := RealChart.Koishi.holdReleaseModules
private abbrev overlapSplitModules := RealChart.Koishi.overlapSplitModules

theorem checkpoint_modules_target_holds :
    RealChart.Koishi.modulesTargetHolds checkpointChart holdReleaseModules = true ∧
    RealChart.Koishi.modulesTargetHolds checkpointChart overlapSplitModules = true := by
  native_decide

def checkpointHoldReleaseResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart
    (tacticFromChartWithModules checkpointChart holdReleaseModules)

def checkpointOverlapSplitResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart
    (tacticFromChartWithModules checkpointChart overlapSplitModules)

def checkpointOverlapSplitNonPerfects : List (Nat × JudgeGrade) :=
  checkpointOverlapSplitResult.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

-- Evaluate each simulation once while retaining every original assertion.
private def defaultChecks (result : RuntimeSimulationResult) : Bool :=
  decide (missingJudgedNoteIndices result = [] ∧
    achievesAP result = false ∧
    (result.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex) ≠ [])

private def overlapChecks (result : RuntimeSimulationResult) : Bool :=
  decide (missingJudgedNoteIndices result = [] ∧
    achievesAP result = true ∧
    (result.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some (evt.noteIndex, evt.grade)) = [])

private theorem default_checks_pass : defaultChecks checkpointResult = true := by
  native_decide

private theorem overlap_checks_pass : overlapChecks checkpointOverlapSplitResult = true := by
  native_decide

theorem checkpoint_has_no_missing_notes :
    missingJudgedNoteIndices checkpointResult = [] :=
  (of_decide_eq_true default_checks_pass).1

theorem checkpoint_default_tactic_does_not_achieve_ap :
    achievesAP checkpointResult = false :=
  (of_decide_eq_true default_checks_pass).2.1

theorem checkpoint_has_non_perfect_notes :
    (checkpointResult.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex) ≠ [] :=
  (of_decide_eq_true default_checks_pass).2.2

theorem checkpoint_hold_release_pass_has_no_missing_notes :
    missingJudgedNoteIndices checkpointHoldReleaseResult = [] := by
  native_decide

theorem checkpoint_overlap_split_has_no_missing_notes :
    missingJudgedNoteIndices checkpointOverlapSplitResult = [] :=
  (of_decide_eq_true overlap_checks_pass).1

theorem checkpoint_overlap_split_achieves_ap :
    achievesAP checkpointOverlapSplitResult = true :=
  (of_decide_eq_true overlap_checks_pass).2.1

theorem checkpoint_overlap_split_has_no_non_perfect_notes :
    checkpointOverlapSplitNonPerfects = [] :=
  (of_decide_eq_true overlap_checks_pass).2.2

end Proofs.RealChartVerificationKoishiDISCO
