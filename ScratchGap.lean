import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader

private def secs' (whole : Int) : TimePoint := TimePoint.fromMicros (whole * 1000000)
private def dur' (micros : Int) : Duration := Duration.fromMicros micros

private def wrapperChart : ChartSpec :=
  { holds :=
      [ { timing := secs' 1, slot := .S1, length := dur' 937500, noteIndex := 210 }
      , { timing := secs' 1, slot := .S8, length := dur' 937500, noteIndex := 211 } ] }

#eval do
  let tactic := defaultTacticFromChart wrapperChart
  IO.println s!"events={repr tactic.events}"
  IO.println s!"batches={repr (tacticBatches tactic)}"
  let result := simulateChartSpecWithTactic wrapperChart tactic
  IO.println s!"missing={missingJudgedNoteIndices result}"
  IO.println s!"result={repr result.events}"
