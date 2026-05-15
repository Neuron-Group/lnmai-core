import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader

private def secs' (whole : Int) : TimePoint :=
  TimePoint.fromMicros (whole * 1000000)

private def miniChart : ChartSpec :=
  { holds :=
      [ { timing := secs' 1, slot := .S1, length := Duration.fromMicros 937500, noteIndex := 1 }
      , { timing := secs' 1, slot := .S8, length := Duration.fromMicros 937500, noteIndex := 2 } ] }

#eval do
  let tactic := defaultTacticFromChart miniChart
  let result := simulateChartSpecWithTactic miniChart tactic
  IO.println s!"missing={missingJudgedNoteIndices result}"
  IO.println s!"events={repr (result.events.map (fun evt => (evt.noteIndex, evt.grade)))}"
