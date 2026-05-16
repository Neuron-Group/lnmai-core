import LnmaiCore

open LnmaiCore

namespace Proofs.RealChartVerificationPandora

def checkpointName : String := "834_PANDORA PARADOXXX"

def checkpointAssetPath : String :=
  "tools/assets/834_PANDORA PARADOXXX/maidata.txt"

def checkpointLevel : Nat := 6

def checkpointChart : ChartLoader.ChartSpec :=
  simai_lowered_chart_file_at! checkpointLevel checkpointAssetPath

def checkpointResult : RuntimeSimulationResult :=
  simulateChartSpecWithTactic checkpointChart (defaultTacticFromChart checkpointChart)

theorem checkpoint_has_no_missing_notes :
    missingJudgedNoteIndices checkpointResult = [] := by
  native_decide

theorem checkpoint_achieves_ap :
    achievesAP checkpointResult = true := by
  native_decide

end Proofs.RealChartVerificationPandora
