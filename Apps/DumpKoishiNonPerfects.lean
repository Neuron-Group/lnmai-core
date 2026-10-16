import LnmaiCore.RealChart.Koishi

open LnmaiCore

private def nonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

private def report (label : String) (result : RuntimeSimulationResult) : IO Unit := do
  IO.println s!"{label}={repr <| nonPerfects result}"
  IO.println s!"{label}_ap={achievesAP result}"
  IO.println s!"{label}_missing={repr <| missingJudgedNoteIndices result}"
  IO.println s!"{label}_counts=(expected={chartNoteIndices result.chart |>.length}, raw={result.events.length}, completed={completedJudgmentEvents result |>.length})"
  IO.println s!"{label}_score={repr result.finalState.score}"
  (← IO.getStdout).flush

def main (args : List String) : IO Unit := do
  let content ← IO.FS.readFile "tools/assets/小石DISCO/maidata.txt"
  let chart ←
    match Simai.compileLowered content 5 with
    | .ok chart => pure chart
    | .error err => throw <| IO.userError s!"parse error: {repr err}"
  if args.isEmpty || args.contains "default" then
    report "default" (simulateChartSpecWithTactic chart (defaultTacticFromChart chart))
  if args.isEmpty || args.contains "hold_release" then
    report "hold_release"
      (simulateChartSpecWithTactic chart (tacticFromChartWithModules chart RealChart.Koishi.holdReleaseModules))
  if args.isEmpty || args.contains "overlap_split" then
    report "overlap_split"
      (simulateChartSpecWithTactic chart (tacticFromChartWithModules chart RealChart.Koishi.overlapSplitModules))
