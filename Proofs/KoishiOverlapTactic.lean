import LnmaiCore.RealChart.Koishi

namespace Proofs.KoishiOverlapTactic
open LnmaiCore

-- The two simultaneous K4 holds from the real fixture are enough to expose
-- the stale override: releasing the short one must not release the long one.
private def chart : ChartLoader.ChartSpec :=
  { holds :=
    [ { timing := TimePoint.fromMicros 1000000, slot := .S4,
        length := Duration.fromMicros 120000, noteIndex := 45 }
    , { timing := TimePoint.fromMicros 1000000, slot := .S4,
        length := Duration.fromMicros 2040000, noteIndex := 46 } ] }

private def defaultResult :=
  simulateChartSpecWithTactic chart (defaultTacticFromChart chart)

private def splitResult :=
  simulateChartSpecWithTactic chart
    (tacticFromChartWithModules chart RealChart.Koishi.overlapSplitModules)

example : achievesAP defaultResult = false := by native_decide

example : missingJudgedNoteIndices splitResult = [] ∧
    achievesAP splitResult = true ∧
    splitResult.events.all (fun event => event.grade == .Perfect) = true := by
  native_decide

end Proofs.KoishiOverlapTactic
