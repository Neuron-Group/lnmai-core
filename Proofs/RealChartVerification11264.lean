import LnmaiCore

open LnmaiCore

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

theorem checkpoint_default_tactic_misses_documented_gap :
    achievesAP checkpointResult = false := by
  native_decide

theorem checkpoint_documented_gap_note_indices :
    (checkpointResult.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex) = [345, 458, 534] := by
  native_decide

end Proofs.RealChartVerification11264
