import LnmaiCore
open LnmaiCore


def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err => IO.println s!"parse error {repr err}"
  | .ok chart =>
      for h in chart.holds do
        IO.println s!"hold#{h.noteIndex} time={h.timing.toMicros} zone={repr h.slot.toButtonZone} len={h.length.toMicros}"
