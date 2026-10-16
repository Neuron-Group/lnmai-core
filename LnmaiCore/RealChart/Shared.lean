import LnmaiCore.RealChart.Koishi

open LnmaiCore

namespace LnmaiCore.RealChart.Shared

structure Checkpoint where
  name : String
  assetPath : System.FilePath
  level : Nat

private def checkpointsAtLevels (name : String) (levels : List Nat) : List Checkpoint :=
  levels.map fun level =>
    { name := name, assetPath := s!"tools/assets/{name}/maidata.txt", level := level }

def checkpoints : List Checkpoint :=
  [ { name := "100524_[協]Hand in Hand"
    , assetPath := "tools/assets/100524_[協]Hand in Hand/maidata.txt"
    , level := 7 }
  , { name := "小石DISCO"
    , assetPath := "tools/assets/小石DISCO/maidata.txt"
    , level := 5 }
  , { name := "11358_インドア系ならトラックメイカー"
    , assetPath := "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"
    , level := 5 }
  , { name := "11264_幽霊東京"
    , assetPath := "tools/assets/11264_幽霊東京/maidata.txt"
    , level := 5 }
  , { name := "834_PANDORA PARADOXXX"
    , assetPath := "tools/assets/834_PANDORA PARADOXXX/maidata.txt"
    , level := 6 } ] ++
  checkpointsAtLevels "011996 Usagi Flap" [2, 3, 4, 5] ++
  checkpointsAtLevels "011951 サイエンス" [2, 3, 4, 5] ++
  checkpointsAtLevels "蛙石" [5, 6]

def summarizeGrades (events : List JudgeEvent) : List (JudgeGrade × Nat) :=
  let grades := events.map (fun evt => evt.grade)
  let uniq := grades.eraseDups
  uniq.map (fun grade => (grade, grades.count grade))

def formatGradeSummary (items : List (JudgeGrade × Nat)) : String :=
  String.intercalate ", " <|
    items.map (fun item => s!"{item.1}: {item.2}")

def tacticForCheckpoint
    (checkpoint : Checkpoint) (chart : ChartLoader.ChartSpec) : ManualTacticSequence :=
  if checkpoint.name = "小石DISCO" then
    tacticFromChartWithModules chart Koishi.overlapSplitModules
  else
    defaultTacticFromChart chart

def verifyCheckpoint (checkpoint : Checkpoint) : IO Bool := do
  let content ← IO.FS.readFile checkpoint.assetPath
  match Simai.compileLowered content checkpoint.level with
  | .error err =>
      IO.println s!"[{checkpoint.name}] parse error at level {checkpoint.level}: {repr err}"
      pure false
  | .ok chart =>
      let tactic := tacticForCheckpoint checkpoint chart
      let result := simulateChartSpecWithTactic chart tactic
      let missing := missingJudgedNoteIndices result
      let completed := completedJudgmentEvents result
      let gradeSummary := summarizeGrades completed
      IO.println s!"[{checkpoint.name}]"
      IO.println s!"  asset: {checkpoint.assetPath}"
      IO.println s!"  level: {checkpoint.level}"
      IO.println s!"  notes: {chartNoteIndices chart |>.length}"
      IO.println s!"  judged: {completed.length}"
      IO.println s!"  missingCount: {missing.length}"
      IO.println s!"  missing: {repr missing}"
      IO.println s!"  achievesAP: {achievesAP result}"
      IO.println s!"  grades: {formatGradeSummary gradeSummary}"
      pure (missing.isEmpty && achievesAP result)

def verifyAll : List Checkpoint → IO Bool
  | [] => pure true
  | checkpoint :: rest => do
      let okHere ← verifyCheckpoint checkpoint
      let okRest ← verifyAll rest
      pure (okHere && okRest)

def verifyMain : IO Unit := do
  let ok ← verifyAll checkpoints
  if ok then
    IO.println "real-chart verification completed: every checkpoint achieves AP with no missing notes"
  else
    throw <| IO.userError "real-chart verification failed"

end LnmaiCore.RealChart.Shared
