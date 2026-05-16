import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader


def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

def targetNotes : List Nat := [57,58,59,60,61,66,67,68,69,94,97,101,104]

def target (idx : Nat) : Bool := targetNotes.any (fun x => x = idx)

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err => IO.println s!"parse error {repr err}"
  | .ok chart =>
      for h in chart.holds do
        if target h.noteIndex then
          IO.println s!"hold#{h.noteIndex} timing={h.timing.toMicros} len={h.length.toMicros} end={h.timing.toMicros + h.length.toMicros}"
      for s in chart.slides do
        if target s.noteIndex then
          IO.println s!"slide#{s.noteIndex} timing={s.timing.toMicros} start={s.startTiming.toMicros} len={s.length.toMicros} judgeAt={repr s.judgeAt}"
