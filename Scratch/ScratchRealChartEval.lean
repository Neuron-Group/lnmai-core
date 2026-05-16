import LnmaiCore
open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
      IO.println s!"achievesAP={achievesAP result}"
      IO.println s!"missingCount={missingJudgedNoteCount result}"
      IO.println s!"missing={missingJudgedNoteIndices result}"
