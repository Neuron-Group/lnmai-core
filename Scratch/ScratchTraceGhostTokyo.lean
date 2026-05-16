import LnmaiCore
open LnmaiCore

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err =>
      IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let skeleton := chartTimingSkeleton chart
      let targets : List Nat := [345, 458, 534]
      for target in targets do
        IO.println s!"== target {target} =="
        for entry in skeleton do
          let i := entry.noteIndex
          if i + 4 >= target && i <= target + 4 then
            IO.println s!"{i}: {repr entry}"
