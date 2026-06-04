import LnmaiCore.RealChart.Shared

open LnmaiCore
open LnmaiCore.RealChart.Shared

def benchmarkIterations : Nat := 20

def microsPerNoteString (elapsedNanos : Nat) (noteCount : Nat) : String :=
  if noteCount = 0 then
    "n/a"
  else
    let microsTimes100 := elapsedNanos / 10 / noteCount
    let whole := microsTimes100 / 100
    let frac := microsTimes100 % 100
    s!"{whole}.{frac}"

def millisString (elapsedNanos : Nat) : String :=
  let msTimes100 := elapsedNanos / 10000
  let whole := msTimes100 / 100
  let frac := msTimes100 % 100
  s!"{whole}.{frac}"

def repeatSimulationWithChecksum
    (chart : ChartLoader.ChartSpec) (tactic : ManualTacticSequence) : Nat → Nat → RuntimeSimulationResult × Nat
  | 0, checksum =>
      let result := simulateChartSpecWithTactic chart tactic
      let checksum' := checksum + result.events.length + (missingJudgedNoteIndices result).length
      (result, checksum')
  | n + 1, checksum =>
      let result := simulateChartSpecWithTactic chart tactic
      let checksum' := checksum + result.events.length + (missingJudgedNoteIndices result).length
      repeatSimulationWithChecksum chart tactic n checksum'

def benchmarkCheckpoint (checkpoint : Checkpoint) : IO Unit := do
  let content ← IO.FS.readFile checkpoint.assetPath
  match Simai.compileLowered content checkpoint.level with
  | .error err =>
      IO.println s!"[{checkpoint.name}] parse error at level {checkpoint.level}: {repr err}"
  | .ok chart =>
      let tactic := tacticForCheckpoint checkpoint chart
      let startNanos ← IO.monoNanosNow
      let (result, checksum) := repeatSimulationWithChecksum chart tactic benchmarkIterations 0
      let endNanos ← IO.monoNanosNow
      let elapsedNanos := endNanos - startNanos
      let avgNanos := elapsedNanos / benchmarkIterations
      let missing := missingJudgedNoteIndices result
      let noteCount := chartNoteIndices chart |>.length
      let gradeSummary := summarizeGrades result.events
      IO.println s!"[{checkpoint.name}]"
      IO.println s!"  asset: {checkpoint.assetPath}"
      IO.println s!"  level: {checkpoint.level}"
      IO.println s!"  notes: {noteCount}"
      IO.println s!"  judged: {result.events.length}"
      IO.println s!"  missingCount: {missing.length}"
      IO.println s!"  achievesAP: {achievesAP result}"
      IO.println s!"  checksum: {checksum}"
      IO.println s!"  iterations: {benchmarkIterations}"
      IO.println s!"  totalElapsedMs: {millisString elapsedNanos}"
      IO.println s!"  avgElapsedMs: {millisString avgNanos}"
      IO.println s!"  avgMicrosPerNote: {microsPerNoteString avgNanos noteCount}"
      IO.println s!"  grades: {formatGradeSummary gradeSummary}"

partial def benchmarkAll : List Checkpoint → IO Unit
  | [] => pure ()
  | checkpoint :: rest => do
      benchmarkCheckpoint checkpoint
      benchmarkAll rest

def benchmarkMain : IO Unit := do
  benchmarkAll checkpoints

def main : IO Unit := do
  benchmarkMain
