import LnmaiCore

open LnmaiCore

namespace Proofs.RealChartVerification小石DISCO

def checkpointName : String := "小石DISCO"

def checkpointAssetPath : String :=
  "../assets/小石DISCO/maidata.txt"

def checkpointLevel : Nat := 5

def checkpointChart : ChartLoader.ChartSpec :=
  simai_lowered_chart_file_at! 5 "../assets/小石DISCO/maidata.txt"

def checkpointResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart (defaultTacticFromChart checkpointChart)

private def exactHoldReleaseModule (noteIndex : Nat) : TimingSkeletonModule :=
  noteIndexModule noteIndex (fun entry =>
    match entry with
    | .hold _ _ inputTime releaseInputTime zone =>
        mkManualTacticSequence
          [ tapAtTime inputTime zone
          , holdButtonAtTime inputTime zone true
          , holdButtonAtTime releaseInputTime zone false ]
    | _ => resolveDefaultTimingSkeleton entry)

private def exactSensorHoldModule (noteIndex : Nat) : TimingSkeletonModule :=
  noteIndexModule noteIndex (fun entry =>
    match entry with
    | .hold _ _ inputTime releaseInputTime zone =>
        let area := zone.toOuterSensorArea
        mkManualTacticSequence
          [ touchAtTime inputTime area
          , holdSensorAtTime inputTime area true
          , holdSensorAtTime releaseInputTime area false ]
    | _ => resolveDefaultTimingSkeleton entry)

private def holdReleaseModules : List TimingSkeletonModule :=
  [ exactHoldReleaseModule 31
  , exactHoldReleaseModule 179
  , exactHoldReleaseModule 181
  , exactHoldReleaseModule 235
  , exactHoldReleaseModule 992
  , exactHoldReleaseModule 993
  , exactHoldReleaseModule 1030
  , exactHoldReleaseModule 1037 ]

def checkpointHoldReleaseResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart
    (tacticFromChartWithModules checkpointChart holdReleaseModules)

private def overlapSplitModules : List TimingSkeletonModule :=
  [ exactHoldReleaseModule 31
  , exactSensorHoldModule 33
  , exactSensorHoldModule 43
  , exactSensorHoldModule 179
  , exactHoldReleaseModule 181
  , exactSensorHoldModule 243
  , exactSensorHoldModule 251
  , exactSensorHoldModule 257
  , exactSensorHoldModule 993
  , exactSensorHoldModule 995
  , exactSensorHoldModule 1032
  , exactSensorHoldModule 1037 ]

def checkpointOverlapSplitResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart
    (tacticFromChartWithModules checkpointChart overlapSplitModules)

def checkpointOverlapSplitNonPerfects : List (Nat × JudgeGrade) :=
  checkpointOverlapSplitResult.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

theorem checkpoint_has_no_missing_notes :
    missingJudgedNoteIndices checkpointResult = [] := by
  native_decide

theorem checkpoint_default_tactic_does_not_achieve_ap :
    achievesAP checkpointResult = false := by
  native_decide

theorem checkpoint_has_non_perfect_notes :
    (checkpointResult.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex) ≠ [] := by
  native_decide

theorem checkpoint_hold_release_pass_has_no_missing_notes :
    missingJudgedNoteIndices checkpointHoldReleaseResult = [] := by
  native_decide

theorem checkpoint_overlap_split_has_no_missing_notes :
    missingJudgedNoteIndices checkpointOverlapSplitResult = [] := by
  native_decide

theorem checkpoint_overlap_split_achieves_ap :
    achievesAP checkpointOverlapSplitResult = true := by
  native_decide

theorem checkpoint_overlap_split_has_no_non_perfect_notes :
    checkpointOverlapSplitNonPerfects = [] := by
  native_decide

end Proofs.RealChartVerification小石DISCO
