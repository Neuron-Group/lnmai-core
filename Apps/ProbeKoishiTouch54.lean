import LnmaiCore

open LnmaiCore

private def summarizeEvents (events : List JudgeEvent) : String :=
  reprStr <| events.map (fun evt => (evt.noteIndex, evt.kind, evt.grade, evt.diff.toMicros))

private def touchAreasA : List SensorArea :=
  [.A1, .A2, .A3, .A4, .A5, .A6, .A7, .A8]

private def summarizeTapHead (currentTime : TimePoint) (q : InputModel.ZoneQueue Lifecycle.TapFamilyNote) : String :=
  match q.peek with
  | some note =>
      let params := Lifecycle.TapFamilyNote.params note
      reprStr
        ( params.noteIndex
        , Lifecycle.TapFamilyNote.state note
        , params.effectiveTiming.toMicros
        , (currentTime - params.effectiveTiming).toMicros
        , Lifecycle.TapFamilyNote.buttonQueueIndex note
        , Lifecycle.TapFamilyNote.position note )
  | none => "none"

private def summarizeHoldHead (currentTime : TimePoint) (q : InputModel.ZoneQueue Lifecycle.HoldNote) : String :=
  match q.peek with
  | some note =>
      let params := note.params
      reprStr
        ( params.noteIndex
        , note.state
        , params.effectiveTiming.toMicros
        , (currentTime - params.effectiveTiming).toMicros
        , note.buttonQueueIndex
        , note.start )
  | none => "none"

private def summarizeTouchHead (currentTime : TimePoint) (q : InputModel.ZoneQueue Lifecycle.TouchNote) : String :=
  match q.peek with
  | some note =>
      let params := note.params
      reprStr
        ( params.noteIndex
        , note.state
        , note.sensorPos
        , params.effectiveTiming.toMicros
        , (currentTime - params.effectiveTiming).toMicros
        , note.touchQueueIndex
        , note.touchGroupId
        , Judge.judgeTouch (currentTime - params.effectiveTiming) params.isEX )
  | none => "none"

private def summarizeTouchHoldHead (q : InputModel.ZoneQueue Lifecycle.HoldNote) : String :=
  match q.peek with
  | some note =>
      reprStr
        ( note.params.noteIndex
        , note.state
        , note.touchQueueIndex
        , note.touchHoldGroupId )
  | none => "none"

def main : IO Unit := do
  let content ← IO.FS.readFile "../assets/小石DISCO/maidata.txt"
  let chart ←
    match Simai.compileLowered content 5 with
    | .ok chart => pure chart
    | .error err => throw <| IO.userError s!"parse error: {repr err}"
  let tactic := defaultTacticFromChart chart
  let batches := tacticBatches tactic
  let rec loop (state : InputModel.GameState) : List InputModel.TimedInputBatch → IO Unit
    | [] => pure ()
    | batch :: rest => do
        let (nextState, events, _, _) := Scheduler.stepFrameTimed state batch
        let t := batch.currentTime.toMicros
        if t >= 14160000 && t <= 14400000 then
          let frameInput := batch.toFrameInput (batch.currentTime - state.currentTime) state.prevButton state.prevSensor
          let touchProbe := Scheduler.probeTouchHeadAt state frameInput .A5
          let sensorConsumers := (Scheduler.probeTapHoldSensorConsumers state frameInput).filter (fun probe => probe.sensorArea == .A5)
          let tapK1Before := state.tapQueues.getD .K1 { notes := [] }
          let tapK5Before := state.tapQueues.getD .K5 { notes := [] }
          let holdK5Before := state.holdQueues.getD .K5 { notes := [] }
          let touchA5Before := state.touchQueues.getD .A5 { notes := [] }
          let touchHoldA5Before := state.touchHoldQueues.getD .A5 { notes := [] }
          IO.println s!"frame={t}"
          IO.println s!"  batch={repr batch.events}"
          IO.println s!"  events={summarizeEvents events}"
          IO.println s!"  sensorClickCountA={reprStr <| touchAreasA.map (fun area => (area, frameInput.getSensorClickCount area))}"
          IO.println s!"  touchProbeA5={reprStr touchProbe}"
          IO.println s!"  sensorConsumersA5={reprStr sensorConsumers}"
          IO.println s!"  before.tapK1={summarizeTapHead batch.currentTime tapK1Before}"
          IO.println s!"  before.tapK5={summarizeTapHead batch.currentTime tapK5Before}"
          IO.println s!"  before.holdK5={summarizeHoldHead batch.currentTime holdK5Before}"
          IO.println s!"  before.touchA5={summarizeTouchHead batch.currentTime touchA5Before}"
          IO.println s!"  before.touchHoldA5={summarizeTouchHoldHead touchHoldA5Before}"
          let tapQ := nextState.tapQueues.getD .K1 { notes := [] }
          let tapK5Q := nextState.tapQueues.getD .K5 { notes := [] }
          let holdQ := nextState.holdQueues.getD .K5 { notes := [] }
          let touchQ := nextState.touchQueues.getD .A5 { notes := [] }
          let touchHoldQ := nextState.touchHoldQueues.getD .A5 { notes := [] }
          IO.println s!"  tapK1.currentIndex={tapQ.currentIndex}"
          IO.println s!"  tapK5.currentIndex={tapK5Q.currentIndex}"
          IO.println s!"  holdK5.currentIndex={holdQ.currentIndex}"
          IO.println s!"  touchA5.currentIndex={touchQ.currentIndex}"
          IO.println s!"  touchHoldA5.currentIndex={touchHoldQ.currentIndex}"
          IO.println s!"  after.tapK1={summarizeTapHead batch.currentTime tapQ}"
          IO.println s!"  after.tapK5={summarizeTapHead batch.currentTime tapK5Q}"
          IO.println s!"  after.holdK5={summarizeHoldHead batch.currentTime holdQ}"
          IO.println s!"  after.touchA5={summarizeTouchHead batch.currentTime touchQ}"
          IO.println s!"  after.touchHoldA5={summarizeTouchHoldHead touchHoldQ}"
          let activeHoldsK5 := nextState.activeHolds.filter (fun entry => entry.1 == .K5)
          let activeTouchHoldsA5 := nextState.activeTouchHolds.filter (fun entry => entry.1 == .A5)
          IO.println s!"  activeHoldsK5={reprStr (activeHoldsK5.map (fun entry => (entry.2.params.noteIndex, entry.2.state, entry.2.params.effectiveTiming.toMicros, (batch.currentTime - entry.2.params.effectiveTiming).toMicros)))}"
          IO.println s!"  activeTouchHoldsA5={reprStr (activeTouchHoldsA5.map (fun entry => (entry.2.params.noteIndex, entry.2.state)))}"
          IO.println s!"  touchQueueHeadsA={reprStr <| touchAreasA.map (fun area => (area, (nextState.touchQueues.getD area { notes := [] }).peek.map (fun note => (note.params.noteIndex, note.state, note.touchQueueIndex, (batch.currentTime - note.params.effectiveTiming).toMicros))))}"
        if t >= 14400000 then
          pure ()
        else
          loop nextState rest
  loop (ChartLoader.buildGameState chart) batches
