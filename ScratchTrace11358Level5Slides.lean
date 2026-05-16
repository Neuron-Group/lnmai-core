import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

def targetNotes : List Nat := [240, 248, 382, 390, 399, 421, 429, 437]

private def isTarget (noteIndex : Nat) : Bool :=
  targetNotes.contains noteIndex

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 5 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      for slide in chart.slides do
        if isTarget slide.noteIndex then
          IO.println s!"note={slide.noteIndex}"
          IO.println s!"  timing={slide.timing.toMicros} start={slide.startTiming.toMicros} len={slide.length.toMicros} judgeAt={repr (slide.judgeAt.map TimePoint.toMicros)}"
          IO.println s!"  kind={repr slide.slideKind} trackCount={slide.trackCount} totalJudgeQueueLen={slide.totalJudgeQueueLen} isClassic={slide.isClassic} isConn={slide.isConnSlide}"
          IO.println s!"  debugSimai={repr slide.debugSimai}"
          IO.println s!"  queues={repr slide.judgeQueues}"
