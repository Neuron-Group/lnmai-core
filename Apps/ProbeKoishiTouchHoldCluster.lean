import LnmaiCore

open LnmaiCore

private def targetAreas : List SensorArea := [.A7, .A2, .A6, .A3]

private def summarizeEvents (events : List JudgeEvent) : String :=
  reprStr <| events.map (fun evt => (evt.noteIndex, evt.kind, evt.grade, evt.diff.toMicros))

private def activeForArea (state : InputModel.GameState) (area : SensorArea) :
    List (Nat × Lifecycle.HoldSubState × Nat × Option Nat × Nat × Bool × Int) :=
  (state.activeTouchHolds.filter (fun entry => entry.1 == area)).map (fun entry =>
    ( entry.2.params.noteIndex
    , entry.2.state
    , entry.2.touchQueueIndex
    , entry.2.touchHoldGroupId
    , entry.2.touchHoldGroupSize
    , entry.2.touchHoldGroupTriggered
    , (state.currentTime - entry.2.params.effectiveTiming).toMicros))

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
        if t >= 104400000 && t <= 106800000 then
          IO.println s!"frame={t}"
          IO.println s!"  batch={repr batch.events}"
          IO.println s!"  events={summarizeEvents events}"
          IO.println s!"  touchGroupStates={reprStr nextState.touchGroupStates}"
          IO.println s!"  touchHoldGroupStates={reprStr nextState.touchHoldGroupStates}"
          for area in targetAreas do
            let touchQ := nextState.touchQueues.getD area { notes := [] }
            let holdQ := nextState.touchHoldQueues.getD area { notes := [] }
            IO.println s!"  area={repr area} frontier={nextState.touchQueueFrontiers.getD area 0}"
            IO.println s!"    touch.currentIndex={touchQ.currentIndex}"
            IO.println s!"    hold.currentIndex={holdQ.currentIndex}"
            IO.println s!"    touch.peek={reprStr <| touchQ.peek.map (fun note => (note.params.noteIndex, note.state, note.touchQueueIndex, note.touchGroupId, note.touchGroupSize, (batch.currentTime - note.params.effectiveTiming).toMicros))}"
            IO.println s!"    hold.peek={reprStr <| holdQ.peek.map (fun note => (note.params.noteIndex, note.state, note.touchQueueIndex, note.touchGroupId, note.touchGroupSize, note.touchHoldGroupId, note.touchHoldGroupSize, note.touchHoldGroupTriggered, (batch.currentTime - note.params.effectiveTiming).toMicros))}"
            IO.println s!"    active={reprStr (activeForArea nextState area)}"
        if t >= 106800000 then
          pure ()
        else
          loop nextState rest
  loop (ChartLoader.buildGameState chart) batches
