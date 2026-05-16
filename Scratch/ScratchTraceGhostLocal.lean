import LnmaiCore
open LnmaiCore

private def dumpEntry (entry : NoteTimingSkeleton) : IO Unit := do
  IO.println s!"entry={repr entry}"
  IO.println s!"events={repr (resolveDefaultTimingSkeleton entry).events}"

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let skeleton := chartTimingSkeleton chart
      for target in ([343, 344, 345, 346, 457, 458, 459, 532, 533, 534, 535] : List Nat) do
        IO.println s!"== {target} =="
        match skeleton.find? (fun (entry : NoteTimingSkeleton) => entry.noteIndex = target) with
        | some entry => dumpEntry entry
        | none => IO.println "missing"
