import Proofs.RealChartVerification100524

open LnmaiCore
open Proofs.RealChartVerification100524

def main : IO Unit := do
  IO.println s!"[{checkpointName}]"
  IO.println s!"  asset: {checkpointAssetPath}"
  IO.println s!"  level: {checkpointLevel}"
  IO.println s!"  notes: {chartNoteIndices checkpointChart |>.length}"
  IO.println s!"  judged: {checkpointCustomResult.events.length}"
  IO.println s!"  missing: {missingJudgedNoteIndices checkpointCustomResult}"
  IO.println s!"  achievesAP: {achievesAP checkpointCustomResult}"
  IO.println s!"  achievesAPPlus: {achievesAPPlus checkpointCustomResult}"
  IO.println s!"  nonPerfects: {checkpointNonPerfects}"
