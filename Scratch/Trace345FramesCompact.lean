import LnmaiCore
open LnmaiCore
open InputModel

private def summarizeSlide (slide : Lifecycle.SlideNote) : String :=
  s!"time={slide.timing.toMicros} start={slide.startTiming.toMicros} checkable={slide.isCheckable} rem={Lifecycle.slideQueueRemaining slide.judgeQueues} queues={repr slide.judgeQueues}"

private def keepNote (idx : Nat) : Bool := idx = 345
private def inWindow (t : Int) : Bool := 81700000 <= t && t <= 82600000

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let batches := (tacticBatches tactic).filter (fun (b : TimedInputBatch) => inWindow b.currentTime.toMicros)
      let init := ChartLoader.buildGameState { chart with taps := [], holds := [], touches := [], touchHolds := [], slides := chart.slides.filter (fun n => keepNote n.noteIndex) }
      let mut st := init
      IO.println s!"init {summarizeSlide st.slides.head!}"
      for batch in batches do
        let before := st.slides.head!
        let (next, evts, _, _) := Scheduler.stepFrameTimed st batch
        let after := next.slides.head!
        IO.println s!"frame={batch.currentTime.toMicros} input={repr batch.events} judged={repr (evts.map (fun (e : JudgeEvent) => (e.noteIndex, e.grade)))}"
        IO.println s!"  before {summarizeSlide before}"
        IO.println s!"  after  {summarizeSlide after}"
        st := next
