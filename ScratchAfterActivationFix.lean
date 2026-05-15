import LnmaiCore
open LnmaiCore


def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err => IO.println s!"parse error {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
      IO.println s!"AP={achievesAP result} APPlus={achievesAPPlus result} missing={missingJudgedNoteIndices result}"
      IO.println s!"nonPerfect={repr (result.events.filter (fun evt => evt.grade != .Perfect) |>.map (fun evt => (evt.noteIndex, evt.grade)))}"
