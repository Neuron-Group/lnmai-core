import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/834_PANDORA PARADOXXX/maidata.txt"

private def findSlide? (slides : List Lifecycle.SlideNote) (noteIndex : Nat) : Option Lifecycle.SlideNote :=
  slides.find? (fun slide => slide.params.noteIndex = noteIndex)

private def showSlideState (label : String) (slide : Lifecycle.SlideNote) : IO Unit := do
  IO.println s!"{label}: state={repr slide.state} checkable={slide.isCheckable} parentFinished={slide.parentFinished} parentPending={slide.parentPendingFinish} remaining={Lifecycle.slideQueueRemaining slide.judgeQueues}"

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 6 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let initialState := ChartLoader.buildGameState chart
      let batches := expandReplayBatchesFrom initialState.currentTime (tacticBatches tactic)
      let interesting := batches.filter (fun (batch : InputModel.TimedInputBatch) =>
        117150000 ≤ batch.currentTime.toMicros && batch.currentTime.toMicros ≤ 117650000)
      let rec loop (state : InputModel.GameState) : List InputModel.TimedInputBatch → IO Unit
        | [] => pure ()
        | batch :: rest => do
            let before984 := findSlide? state.slides 984
            let before985 := findSlide? state.slides 985
            IO.println s!"frame t={batch.currentTime.toMicros} events={repr batch.events}"
            match before984 with
            | some slide => showSlideState "  before 984" slide
            | none => pure ()
            match before985 with
            | some slide => showSlideState "  before 985" slide
            | none => pure ()
            let (nextState, events, _, _) := Scheduler.stepFrameTimed state batch
            IO.println s!"  judged={repr (events.map (fun (evt : JudgeEvent) => (evt.noteIndex, evt.grade, evt.diff.toMicros)))}"
            match findSlide? nextState.slides 984 with
            | some slide => showSlideState "  after  984" slide
            | none => pure ()
            match findSlide? nextState.slides 985 with
            | some slide => showSlideState "  after  985" slide
            | none => pure ()
            loop nextState rest
      loop initialState interesting
