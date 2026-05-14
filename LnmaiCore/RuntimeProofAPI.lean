import LnmaiCore.Simai.ProofAPI
import LnmaiCore.Simai.DSL
import LnmaiCore.ChartLoader
import LnmaiCore.Scheduler
import Lean

namespace LnmaiCore

open InputModel
open Lean Elab Term

abbrev ManualTacticAction := TimedInputEvent

structure ManualTacticSequence where
  events : List ManualTacticAction := []
deriving Inhabited, Repr

structure RuntimeSimulationResult where
  chart : ChartLoader.ChartSpec
  initialState : InputModel.GameState
  finalState : InputModel.GameState
  batches : List TimedInputBatch
  events : List JudgeEvent
deriving Repr

def compileRuntimeChartSection (content : String) (levelIndex : Nat := 1) : Except Simai.ParseError ChartLoader.ChartSpec :=
  Simai.compileLowered content levelIndex

private def eventLe (lhs rhs : TimedInputEvent) : Bool :=
  lhs.at ≤ rhs.at

private def insertEvent (evt : TimedInputEvent) : List TimedInputEvent → List TimedInputEvent
  | [] => [evt]
  | head :: rest =>
    if eventLe evt head then evt :: head :: rest else head :: insertEvent evt rest

def sortTacticEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.foldl (fun acc evt => insertEvent evt acc) []

private def pushEventIntoBatches (evt : TimedInputEvent) : List TimedInputBatch → List TimedInputBatch
  | [] => [{ currentTime := evt.at, events := [evt] }]
  | batch :: rest =>
    if batch.currentTime = evt.at then
      { batch with events := batch.events ++ [evt] } :: rest
    else
      batch :: pushEventIntoBatches evt rest

private def groupSortedEvents (events : List TimedInputEvent) : List TimedInputBatch :=
  events.foldl (fun acc evt => pushEventIntoBatches evt acc) []

def tacticBatches (seq : ManualTacticSequence) : List TimedInputBatch :=
  groupSortedEvents (sortTacticEvents seq.events)

private def foldSimulation
    (state : InputModel.GameState × List JudgeEvent)
    (batch : TimedInputBatch) : InputModel.GameState × List JudgeEvent :=
  let (st, acc) := state
  let (nextState, events, _, _) := Scheduler.stepFrameTimed st batch
  (nextState, acc ++ events)

private def frameBatchesBetween (startTime endTime : TimePoint) : List TimedInputBatch :=
  let frame := Constants.FRAME_LENGTH
  let rec loop (current : TimePoint) (fuel : Nat) : List TimedInputBatch :=
    if fuel = 0 then
      []
    else
      let next := current + frame
      if next < endTime then
        { currentTime := next, events := [] } :: loop next (fuel - 1)
      else
        []
  loop startTime 4096

private def expandTimedBatchesFrom (startTime : TimePoint) (batches : List TimedInputBatch) : List TimedInputBatch :=
  let rec loop (current : TimePoint) : List TimedInputBatch → List TimedInputBatch
    | [] => []
    | batch :: rest =>
      let fillers := frameBatchesBetween current batch.currentTime
      fillers ++ (batch :: loop batch.currentTime rest)
  loop startTime batches

private def noteCount (chart : ChartLoader.ChartSpec) : Nat :=
  chart.taps.length + chart.holds.length + chart.touches.length + chart.touchHolds.length + chart.slides.length

private def chartEndTime (chart : ChartLoader.ChartSpec) : TimePoint :=
  let tapEnd := chart.taps.foldl (fun acc note => max acc note.timing) TimePoint.zero
  let holdEnd := chart.holds.foldl (fun acc note => max acc (note.timing + note.length)) TimePoint.zero
  let touchEnd := chart.touches.foldl (fun acc note => max acc note.timing) TimePoint.zero
  let touchHoldEnd := chart.touchHolds.foldl (fun acc note => max acc (note.timing + note.length)) TimePoint.zero
  let slideEnd := chart.slides.foldl (fun acc note => max acc (note.startTiming + note.length)) TimePoint.zero
  max tapEnd (max holdEnd (max touchEnd (max touchHoldEnd slideEnd)))

private def settleBatchesFrom (startTime : TimePoint) (chart : ChartLoader.ChartSpec) : List TimedInputBatch :=
  let targetTime := chartEndTime chart + Duration.fromMicros 1000000
  let frame := Constants.FRAME_LENGTH
  let rec loop (current : TimePoint) (fuel : Nat) : List TimedInputBatch :=
    if fuel = 0 then
      []
    else if current ≥ targetTime then
      []
    else
      let next := current + frame
      { currentTime := next, events := [] } :: loop next (fuel - 1)
  loop startTime 512

def simulateChartSpecWithTactic (chart : ChartLoader.ChartSpec) (seq : ManualTacticSequence) : RuntimeSimulationResult :=
  let initialState := ChartLoader.buildGameState chart
  let batches := expandTimedBatchesFrom initialState.currentTime (tacticBatches seq)
  let (stateAfterTactic, tacticEvents) := batches.foldl foldSimulation (initialState, [])
  let settleBatches := settleBatchesFrom stateAfterTactic.currentTime chart
  let (finalState, allEvents) := settleBatches.foldl foldSimulation (stateAfterTactic, tacticEvents)
  { chart := chart
  , initialState := initialState
  , finalState := finalState
  , batches := batches ++ settleBatches
  , events := allEvents }

def simulateChartSectionWithTactic
    (content : String) (seq : ManualTacticSequence) (levelIndex : Nat := 1) :
    Except Simai.ParseError RuntimeSimulationResult := do
  let chart ← compileRuntimeChartSection content levelIndex
  return simulateChartSpecWithTactic chart seq

def allEventsArePerfect (result : RuntimeSimulationResult) : Bool :=
  result.events.all (fun evt => evt.grade.isPerfectGrade)

def allNotesJudged (result : RuntimeSimulationResult) : Bool :=
  result.events.length = noteCount result.chart

def endsWithNoActiveRuntimeNotes (result : RuntimeSimulationResult) : Bool :=
  result.finalState.activeHolds.isEmpty &&
  result.finalState.activeTouchHolds.isEmpty &&
  result.finalState.slides.all (fun slide =>
    match slide.state with
    | Lifecycle.SlideState.Ended => true
    | _ => false)

def achievesAP (result : RuntimeSimulationResult) : Bool :=
  allNotesJudged result &&
  allEventsArePerfect result &&
  (comboState result.finalState.score = ComboState.AP
    || comboState result.finalState.score = ComboState.APPlus)

def achievesAPPlus (result : RuntimeSimulationResult) : Bool :=
  allNotesJudged result &&
  result.events.all (fun evt => evt.grade = JudgeGrade.Perfect) &&
  comboState result.finalState.score = ComboState.APPlus

def chartSectionAchievesAP
    (content : String) (seq : ManualTacticSequence) (levelIndex : Nat := 1) : Bool :=
  match simulateChartSectionWithTactic content seq levelIndex with
  | .ok result => achievesAP result
  | .error _ => false

def chartSectionAchievesAPPlus
    (content : String) (seq : ManualTacticSequence) (levelIndex : Nat := 1) : Bool :=
  match simulateChartSectionWithTactic content seq levelIndex with
  | .ok result => achievesAPPlus result
  | .error _ => false

def tapAt (micros : Int) (zone : ButtonZone) : ManualTacticAction :=
  .buttonClick (TimePoint.fromMicros micros) zone

def holdButtonAt (micros : Int) (zone : ButtonZone) (isDown : Bool := true) : ManualTacticAction :=
  .buttonHold (TimePoint.fromMicros micros) zone isDown

def touchAt (micros : Int) (area : SensorArea) : ManualTacticAction :=
  .sensorClick (TimePoint.fromMicros micros) area

def holdSensorAt (micros : Int) (area : SensorArea) (isDown : Bool := true) : ManualTacticAction :=
  .sensorHold (TimePoint.fromMicros micros) area isDown

theorem compileRuntimeChartSection_eq_compileLowered
    (content : String) (levelIndex : Nat := 1) :
    compileRuntimeChartSection content levelIndex = Simai.compileLowered content levelIndex := rfl

private def parseIntToken? (text : String) : Option Int :=
  text.toInt?

private def parseBoolToken? (text : String) : Option Bool :=
  match text.trimAscii.toString with
  | "down" | "true" => some true
  | "up" | "false" => some false
  | _ => none

private def parseButtonZoneToken? (text : String) : Option ButtonZone :=
  match (fromJson? (Lean.Json.str text.trimAscii.toString) : Except String ButtonZone) with
  | .ok zone => some zone
  | .error _ => none

private def parseSensorAreaToken? (text : String) : Option SensorArea :=
  match (fromJson? (Lean.Json.str text.trimAscii.toString) : Except String SensorArea) with
  | .ok area => some area
  | .error _ => none

private def parseManualTacticLine? (line : String) : Except String (Option ManualTacticAction) :=
  let trimmed := line.trimAscii.toString
  if trimmed.isEmpty || trimmed.startsWith "--" || trimmed.startsWith "#" then
    return none
  else
    let parts := trimmed.splitOn " " |>.filter (fun item => !item.trimAscii.isEmpty)
    match parts with
    | [timeText, kindText, targetText] =>
        match parseIntToken? timeText with
        | none => .error s!"invalid time token {repr timeText}"
        | some micros =>
        match kindText.trimAscii.toString with
        | "tap" =>
            match parseButtonZoneToken? targetText with
            | some zone => return some (.buttonClick (TimePoint.fromMicros micros) zone)
            | none => .error s!"invalid button zone {repr targetText}"
        | "touch" =>
            match parseSensorAreaToken? targetText with
            | some area => return some (.sensorClick (TimePoint.fromMicros micros) area)
            | none => .error s!"invalid sensor area {repr targetText}"
        | _ => .error s!"unknown action kind {repr kindText}"
    | [timeText, kindText, targetText, stateText] =>
        match parseIntToken? timeText, parseBoolToken? stateText with
        | none, _ => .error s!"invalid time token {repr timeText}"
        | _, none => .error s!"invalid hold state {repr stateText}; expected down/up"
        | some micros, some isDown =>
          match kindText.trimAscii.toString with
          | "button" | "holdButton" =>
              match parseButtonZoneToken? targetText with
              | some zone => return some (.buttonHold (TimePoint.fromMicros micros) zone isDown)
              | none => .error s!"invalid button zone {repr targetText}"
          | "sensor" | "holdSensor" =>
              match parseSensorAreaToken? targetText with
              | some area => return some (.sensorHold (TimePoint.fromMicros micros) area isDown)
              | none => .error s!"invalid sensor area {repr targetText}"
          | _ => .error s!"unknown hold action kind {repr kindText}"
    | _ => .error s!"invalid tactic line {repr trimmed}"

def parseManualTacticSequence (content : String) : Except String ManualTacticSequence := do
  let lines := content.splitOn "\n"
  let mut events : List ManualTacticAction := []
  for line in lines do
    match (← parseManualTacticLine? line) with
    | some evt => events := events ++ [evt]
    | none => pure ()
  return { events := events }

def manualTacticLiteral (content : String) : ManualTacticSequence :=
  match parseManualTacticSequence content with
  | .ok seq => seq
  | .error err => panic! s!"manual_tactic! parse failed: {err}"

def exampleSingleTapSection : ChartLoader.ChartSpec :=
  simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1,\n"

def exampleDelayedSingleTapSection : ChartLoader.ChartSpec :=
  { taps :=
      [ { timing := TimePoint.fromMicros 500000
        , slot := OuterSlot.S1
        , isBreak := false
        , isEX := false
        , noteIndex := 1 } ]
  , holds := []
  , touches := []
  , touchHolds := []
  , slides := []
  , slideSkipping := true }

#eval simai_normalized_chart! "&first=0\n&inote_1=\n(120)\n1,\n"
syntax "manual_tactic!" str : term

private def getStringLiteral? (stx : Syntax) : Option String :=
  stx.isStrLit?

private def validateManualTacticLiteral (content : String) : TermElabM Unit := do
  match parseManualTacticSequence content with
  | .ok _ => pure ()
  | .error err => throwError m!"manual_tactic! parse failed for {repr content}: {err}"

elab_rules : term
  | `(manual_tactic! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateManualTacticLiteral content
      let stx ← `((manualTacticLiteral $s : ManualTacticSequence))
      elabTerm stx none

def exampleDelayedSingleTapButtonTactic : ManualTacticSequence :=
  manual_tactic! "500000 tap K1"

#eval manual_tactic! "500000 tap K1"

def exampleDelayedSingleTapSensorTactic : ManualTacticSequence :=
  manual_tactic! "516000 touch A1"

#eval manual_tactic! "516000 touch A1"

#print exampleDelayedSingleTapSection

theorem exampleDelayedSingleTapButtonTactic_achievesAP :
    achievesAP (
      simulateChartSpecWithTactic
        exampleDelayedSingleTapSection
        exampleDelayedSingleTapButtonTactic
    ) = true := by
  native_decide

theorem exampleDelayedSingleTapSensorTactic_achievesAP :
    achievesAP (
      simulateChartSpecWithTactic
        exampleDelayedSingleTapSection
        exampleDelayedSingleTapSensorTactic
    ) = true := by
  native_decide

end LnmaiCore
