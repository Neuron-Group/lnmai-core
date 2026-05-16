import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let some note := chart.slides.find? (fun (n : SlideChartNote) => n.noteIndex = 345)
        | IO.println "missing 345"; return
      IO.println s!"index={note.noteIndex} slot={repr note.slot} kind={repr note.slideKind} timing={note.timing.toMicros} start={note.startTiming.toMicros} judgeAt={repr (note.judgeAt.map TimePoint.toMicros)}"
      IO.println s!"debug={repr note.debugSimai}"
      IO.println s!"queues={repr note.judgeQueues}"
