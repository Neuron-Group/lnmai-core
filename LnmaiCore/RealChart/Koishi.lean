import LnmaiCore.Proofs.Runtime

namespace LnmaiCore.RealChart.Koishi

def exactHoldReleaseModule (noteIndex : Nat) : TimingSkeletonModule :=
  noteIndexModule noteIndex (fun entry =>
    match entry with
    | .hold _ _ inputTime releaseInputTime zone =>
        mkManualTacticSequence
          [ tapAtTime inputTime zone
          , holdButtonAtTime inputTime zone true
          , holdButtonAtTime releaseInputTime zone false ]
    | _ => resolveDefaultTimingSkeleton entry)

def exactSensorHoldModule (noteIndex : Nat) : TimingSkeletonModule :=
  noteIndexModule noteIndex (fun entry =>
    match entry with
    | .hold _ _ inputTime releaseInputTime zone =>
        let area := zone.toOuterSensorArea
        mkManualTacticSequence
          [ touchAtTime inputTime area
          , holdSensorAtTime inputTime area true
          , holdSensorAtTime releaseInputTime area false ]
    | _ => resolveDefaultTimingSkeleton entry)

-- Indices in the checked-in fixture after connected slides are lowered.
def holdReleaseModules : List TimingSkeletonModule :=
  [ exactHoldReleaseModule 33
  , exactHoldReleaseModule 181
  , exactHoldReleaseModule 183
  , exactHoldReleaseModule 237
  , exactHoldReleaseModule 995
  , exactHoldReleaseModule 996
  , exactHoldReleaseModule 1033
  , exactHoldReleaseModule 1043 ]

def overlapSplitModules : List TimingSkeletonModule :=
  [ exactHoldReleaseModule 33
  , exactSensorHoldModule 35
  , exactSensorHoldModule 45
  , exactSensorHoldModule 181
  , exactHoldReleaseModule 183
  , exactSensorHoldModule 245
  , exactSensorHoldModule 253
  , exactSensorHoldModule 259
  , exactSensorHoldModule 996
  , exactSensorHoldModule 998
  , exactSensorHoldModule 1036
  , exactSensorHoldModule 1043 ]

/-- Reject stale fixture indices instead of silently using the default tactic. -/
def modulesTargetHolds (chart : ChartLoader.ChartSpec) (modules : List TimingSkeletonModule) : Bool :=
  let holds := (chartTimingSkeleton chart).filter (fun entry => entry.kind == .hold)
  modules.all (fun module => holds.any module.applies)

end LnmaiCore.RealChart.Koishi
