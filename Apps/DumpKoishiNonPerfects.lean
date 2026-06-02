import LnmaiCore

open LnmaiCore

private def nonPerfects (result : RuntimeSimulationResult) : List (Nat × JudgeGrade) :=
  result.events.filterMap (fun evt =>
    if evt.grade = .Perfect then none else some (evt.noteIndex, evt.grade))

private def exactSensorHoldModule (noteIndex : Nat) : TimingSkeletonModule :=
  noteIndexModule noteIndex (fun entry =>
    match entry with
    | .hold _ _ inputTime releaseInputTime zone =>
        let area := zone.toOuterSensorArea
        mkManualTacticSequence
          [ touchAtTime inputTime area
          , holdSensorAtTime inputTime area true
          , holdSensorAtTime releaseInputTime area false ]
    | _ => resolveDefaultTimingSkeleton entry)

def main : IO Unit := do
  let content ← IO.FS.readFile "../assets/小石DISCO/maidata.txt"
  let chart ←
    match Simai.compileLowered content 5 with
    | .ok chart => pure chart
    | .error err => throw <| IO.userError s!"parse error: {repr err}"
  let exactHoldReleaseModule (noteIndex : Nat) : TimingSkeletonModule :=
    noteIndexModule noteIndex (fun entry =>
      match entry with
      | .hold _ _ inputTime releaseInputTime zone =>
          mkManualTacticSequence
            [ tapAtTime inputTime zone
            , holdButtonAtTime inputTime zone true
            , holdButtonAtTime releaseInputTime zone false ]
      | _ => resolveDefaultTimingSkeleton entry)
  let holdReleaseModules : List TimingSkeletonModule :=
    [ exactHoldReleaseModule 31
    , exactHoldReleaseModule 179
    , exactHoldReleaseModule 181
    , exactHoldReleaseModule 235
    , exactHoldReleaseModule 992
    , exactHoldReleaseModule 993
    , exactHoldReleaseModule 1030
    , exactHoldReleaseModule 1037 ]
  let overlapSplitModules : List TimingSkeletonModule :=
    [ exactHoldReleaseModule 31
    , exactSensorHoldModule 33
    , exactSensorHoldModule 43
    , exactSensorHoldModule 179
    , exactHoldReleaseModule 181
    , exactSensorHoldModule 243
    , exactSensorHoldModule 251
    , exactSensorHoldModule 257
    , exactSensorHoldModule 993
    , exactSensorHoldModule 995
    , exactSensorHoldModule 1032
    , exactSensorHoldModule 1037 ]
  let defaultResult := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
  let holdReleaseResult := simulateChartSpecWithTactic chart (tacticFromChartWithModules chart holdReleaseModules)
  let overlapSplitResult := simulateChartSpecWithTactic chart (tacticFromChartWithModules chart overlapSplitModules)
  IO.println s!"default={repr <| nonPerfects defaultResult}"
  IO.println s!"hold_release={repr <| nonPerfects holdReleaseResult}"
  IO.println s!"overlap_split={repr <| nonPerfects overlapSplitResult}"
  IO.println s!"overlap_split_ap={achievesAP overlapSplitResult}"
