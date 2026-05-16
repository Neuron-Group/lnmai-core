import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/834_PANDORA PARADOXXX/maidata.txt"

def targets : List Nat := [984, 985]

private def isTarget (n : Nat) : Bool := targets.contains n

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 6 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      let skeleton := chartTimingSkeleton chart
      for entry in skeleton do
        if isTarget entry.noteIndex then
          IO.println s!"skeleton {entry.noteIndex}: {repr entry}"
          let seq := resolveDefaultTimingSkeleton entry
          IO.println s!"events {entry.noteIndex}: {repr seq.events}"

