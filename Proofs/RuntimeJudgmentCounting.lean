import LnmaiCore.Proofs.Runtime

namespace Proofs.RuntimeJudgmentCounting
open LnmaiCore

private def chart : ChartLoader.ChartSpec :=
  match Simai.compileLowered "&first=0\n&inote_1=(120)1h[4:1],\n" 1 with
  | .ok chart => chart
  | .error _ => {}

private def result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)

-- A hold emits a head notification and a tail judgment but scores once.
example : result.events.length = 2 := by native_decide
example : result.finalState.score.combo = 1 := by native_decide
example : allNotesJudged result = true := by native_decide
example : achievesAP result = true := by native_decide

-- Seeing a head notification alone must not count as a completed hold.
private def headOnly : RuntimeSimulationResult :=
  { result with events := result.events.filter (fun event => event.phase == .head) }

example : allNotesJudged headOnly = false := by native_decide
example : missingJudgedNoteIndices headOnly = [1] := by native_decide

end Proofs.RuntimeJudgmentCounting
