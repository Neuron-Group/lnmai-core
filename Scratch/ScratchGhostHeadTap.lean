import LnmaiCore
open LnmaiCore

private def bads (result : RuntimeSimulationResult) : List Nat :=
  result.events.filterMap (fun evt => if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex)

private def runCase (label : String) (extras : List ManualTacticAction) : IO Unit := do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let base := defaultTacticFromChart chart
      let seq := mkManualTacticSequence (base.events ++ extras)
      let result := simulateChartSpecWithTactic chart seq
      IO.println s!"{label}: ap={achievesAP result} bads={bads result}"

#eval do
  runCase "head taps" [tapAt 81774171 ButtonZone.K1, tapAt 117580625 ButtonZone.K5, tapAt 132096759 ButtonZone.K1]
  runCase "start taps" [tapAt 82258042 ButtonZone.K1, tapAt 118064496 ButtonZone.K5, tapAt 132580630 ButtonZone.K1]
  runCase "end taps" [tapAt 82499977 ButtonZone.K1, tapAt 118306431 ButtonZone.K5, tapAt 132822565 ButtonZone.K1]
