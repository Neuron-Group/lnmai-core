import LnmaiCore

open LnmaiCore

namespace Proofs.RealChartVerification11358

def checkpointName : String := "11358_インドア系ならトラックメイカー"

def checkpointAssetPath : String :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

def checkpointLevel : Nat := 5

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

end Proofs.RealChartVerification11358
