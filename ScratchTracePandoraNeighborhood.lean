import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/834_PANDORA PARADOXXX/maidata.txt"

def lo : Nat := 980
def hi : Nat := 990

private def inWindow (noteIndex : Nat) : Bool :=
  lo ≤ noteIndex && noteIndex ≤ hi

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 6 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      for slide in chart.slides do
        if inWindow slide.noteIndex then
          IO.println s!"slide {slide.noteIndex}: conn={slide.isConnSlide} head={slide.isGroupHead} end={slide.isGroupEnd} parent={repr slide.parentNoteIndex} timing={slide.timing.toMicros} start={slide.startTiming.toMicros} judgeAt={repr (slide.judgeAt.map TimePoint.toMicros)} queues={slide.totalJudgeQueueLen} debug={repr slide.debugSimai}"

