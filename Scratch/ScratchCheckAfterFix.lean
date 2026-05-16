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
  let result := simulateChartSpecWithTactic wrapperChart tactic
  IO.println s!"wrapper missing={missingJudgedNoteIndices result}"
  IO.println s!"wrapper events={repr (result.events.map (fun evt => (evt.noteIndex, evt.grade)))}"
