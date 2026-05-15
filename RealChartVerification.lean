import LnmaiCore

open LnmaiCore

structure RealChartCheckpoint where
  name : String
  assetPath : System.FilePath
  level : Nat

def realChartCheckpoints : List RealChartCheckpoint :=
  [ { name := "11358_インドア系ならトラックメイカー"
    , assetPath := "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"
    , level := 2 }
  , { name := "834_PANDORA PARADOXXX"
    , assetPath := "tools/assets/834_PANDORA PARADOXXX/maidata.txt"
    , level := 2 } ]

def summarizeGrades (events : List JudgeEvent) : List (JudgeGrade × Nat) :=
  let grades := events.map (fun evt => evt.grade)
  let uniq := grades.eraseDups
  uniq.map (fun grade => (grade, grades.count grade))

def formatGradeSummary (items : List (JudgeGrade × Nat)) : String :=
  String.intercalate ", " <|
    items.map (fun item => s!"{item.1}: {item.2}")

def verifyCheckpoint (checkpoint : RealChartCheckpoint) : IO Bool := do
  let content ← IO.FS.readFile checkpoint.assetPath
  match Simai.compileLowered content checkpoint.level with
  | .error err =>
      IO.println s!"[{checkpoint.name}] parse error at level {checkpoint.level}: {repr err}"
      pure false
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let result := simulateChartSpecWithTactic chart tactic
      let missing := missingJudgedNoteIndices result
      let gradeSummary := summarizeGrades result.events
      IO.println s!"[{checkpoint.name}]"
      IO.println s!"  asset: {checkpoint.assetPath}"
      IO.println s!"  level: {checkpoint.level}"
      IO.println s!"  notes: {chartNoteIndices chart |>.length}"
      IO.println s!"  judged: {result.events.length}"
      IO.println s!"  missingCount: {missing.length}"
      IO.println s!"  missing: {repr missing}"
      IO.println s!"  achievesAP: {achievesAP result}"
      IO.println s!"  grades: {formatGradeSummary gradeSummary}"
      pure (missing.isEmpty)

partial def verifyAll : List RealChartCheckpoint → IO Bool
  | [] => pure true
  | checkpoint :: rest => do
      let okHere ← verifyCheckpoint checkpoint
      let okRest ← verifyAll rest
      pure (okHere && okRest)

def main : IO Unit := do
  let ok ← verifyAll realChartCheckpoints
  if ok then
    IO.println "real-chart verification completed with no missing judged notes"
  else
    throw <| IO.userError "real-chart verification failed"
