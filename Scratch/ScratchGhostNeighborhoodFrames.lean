import LnmaiCore
open LnmaiCore
open InputModel

private def summarizeSlide (slide : Lifecycle.SlideNote) : String :=
  s!"note={slide.params.noteIndex} timing={slide.timing.toMicros} start={slide.startTiming.toMicros} checkable={slide.isCheckable} rem={Lifecycle.slideQueueRemaining slide.judgeQueues} queues={repr slide.judgeQueues}"

private def inWindow (t : Int) : Bool :=
  81200000 <= t && t <= 82600000

private def keepNote (idx : Nat) : Bool :=
  idx = 343 || idx = 344 || idx = 345 || idx = 346

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let batches := (tacticBatches tactic).filter (fun (b : TimedInputBatch) => inWindow b.currentTime.toMicros)
      let init := ChartLoader.buildGameState { chart with
        taps := chart.taps.filter (fun n => keepNote n.noteIndex)
        , holds := []
        , touches := chart.touches.filter (fun n => keepNote n.noteIndex)
        , touchHolds := []
        , slides := chart.slides.filter (fun n => keepNote n.noteIndex) }
      let mut st := init
      IO.println s!"init time={st.currentTime.toMicros}"
      for slide in st.slides do
        IO.println (summarizeSlide slide)
      for batch in batches do
        let (next, evts, _, _) := Scheduler.stepFrameTimed st batch
        IO.println s!"== frame {batch.currentTime.toMicros} events={repr batch.events} judged={repr (evts.map (fun (e : JudgeEvent) => (e.noteIndex, e.grade)))} =="
        for slide in next.slides do
          IO.println (summarizeSlide slide)
        st := next
