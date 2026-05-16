import LnmaiCore

open LnmaiCore

def countFile (path : System.FilePath) (level : Nat) : IO Unit := do
  let content ← IO.FS.readFile path
  match Simai.compileLowered content level with
  | .error err => IO.println s!"parse error {repr err}"
  | .ok chart =>
      let connNonEnd := chart.slides.filter (fun note : ChartLoader.SlideChartNote => note.isConnSlide && !note.isGroupEnd)
      IO.println s!"file={path} level={level} totalSlides={chart.slides.length} connNonEnd={connNonEnd.length}"
      IO.println s!"sample={repr (connNonEnd.take 10 |>.map (fun n => (n.noteIndex, n.isGroupHead, n.isGroupEnd, n.parentNoteIndex, n.debugSimai)))}"

#eval countFile "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt" 5
#eval countFile "tools/assets/834_PANDORA PARADOXXX/maidata.txt" 6

