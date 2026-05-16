import LnmaiCore

open LnmaiCore

def checkpointName : String := "11358_インドア系ならトラックメイカー"

def checkpointAssetPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

def checkpointLevel : Nat := 5

private def summarizeGrades (events : List JudgeEvent) : List (JudgeGrade × Nat) :=
  let grades := events.map (fun evt => evt.grade)
  let uniq := grades.eraseDups
  uniq.map (fun grade => (grade, grades.count grade))

private def formatGradeSummary (items : List (JudgeGrade × Nat)) : String :=
  String.intercalate ", " <|
    items.map (fun item => s!"{item.1}: {item.2}")

def main : IO Unit := do
  let content ← IO.FS.readFile checkpointAssetPath
  match Simai.compileLowered content checkpointLevel with
  | .error err =>
      throw <| IO.userError s!"[{checkpointName}] parse error at level {checkpointLevel}: {repr err}"
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let result := simulateChartSpecWithTactic chart tactic
      let missing := missingJudgedNoteIndices result
      let gradeSummary := summarizeGrades result.events
      IO.println s!"[{checkpointName}]"
      IO.println s!"  asset: {checkpointAssetPath}"
      IO.println s!"  level: {checkpointLevel}"
      IO.println s!"  notes: {chartNoteIndices chart |>.length}"
      IO.println s!"  judged: {result.events.length}"
      IO.println s!"  missingCount: {missing.length}"
      IO.println s!"  missing: {repr missing}"
      IO.println s!"  achievesAP: {achievesAP result}"
      IO.println s!"  grades: {formatGradeSummary gradeSummary}"
      if missing.isEmpty && achievesAP result then
        IO.println "real-chart verification completed with AP"
      else
        throw <| IO.userError "real-chart verification failed"
