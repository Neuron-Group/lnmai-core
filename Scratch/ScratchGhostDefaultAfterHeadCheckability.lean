import LnmaiCore
open LnmaiCore

private def nonPerfects (result : RuntimeSimulationResult) : List Nat :=
  result.events.filterMap fun evt => if evt.grade = .Perfect then none else some evt.noteIndex

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
      IO.println s!"ap={achievesAP result} bads={nonPerfects result}"
