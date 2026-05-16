import LnmaiCore.Proofs.Simai
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

inductive NoteTimingSkeletonKind where
  | tap
  | hold
  | touch
  | touchHold
  | slide
deriving Inhabited, Repr, DecidableEq

structure SlideTimingSkeleton where
  noteIndex : Nat
  headSemanticTime : TimePoint
  headInputTime : TimePoint
  startSemanticTime : TimePoint
  startInputTime : TimePoint
  endSemanticTime : TimePoint
  endInputTime : TimePoint
  headZone : ButtonZone
  pathSteps : List (List SensorArea) := []
deriving Inhabited, Repr

inductive NoteTimingSkeleton where
  | tap (noteIndex : Nat) (semanticTime inputTime : TimePoint) (zone : ButtonZone)
  | hold (noteIndex : Nat) (semanticTime inputTime releaseInputTime : TimePoint) (zone : ButtonZone)
  | touch (noteIndex : Nat) (semanticTime inputTime : TimePoint) (area : SensorArea)
  | touchHold (noteIndex : Nat) (semanticTime inputTime releaseInputTime : TimePoint) (area : SensorArea)
  | slide (spec : SlideTimingSkeleton)
deriving Inhabited, Repr

abbrev TimingSkeletonResolver := NoteTimingSkeleton → ManualTacticSequence

structure TimingSkeletonOverride where
  noteIndex : Nat
  resolve : TimingSkeletonResolver

structure RuntimeSimulationResult where
  chart : ChartLoader.ChartSpec
  initialState : InputModel.GameState
  finalState : InputModel.GameState
  batches : List TimedInputBatch
  events : List JudgeEvent
deriving Repr

structure RuntimeReplayResult where
  initialState : InputModel.GameState
  finalState : InputModel.GameState
  batches : List TimedInputBatch
  events : List JudgeEvent
deriving Repr

def compileRuntimeChartSection (content : String) (levelIndex : Nat := 1) : Except Simai.ParseError ChartLoader.ChartSpec :=
  Simai.compileLowered content levelIndex

private def eventLe (lhs rhs : TimedInputEvent) : Bool :=
  lhs.at ≤ rhs.at

def sortTacticEvents (events : List TimedInputEvent) : List TimedInputEvent :=
  events.mergeSort eventLe

def mkManualTacticSequence (events : List TimedInputEvent) : ManualTacticSequence :=
  { events := sortTacticEvents events }

def ManualTacticSequence.append (lhs rhs : ManualTacticSequence) : ManualTacticSequence :=
  mkManualTacticSequence (lhs.events ++ rhs.events)

instance : Append ManualTacticSequence where
  append := ManualTacticSequence.append

private def groupSortedEvents (events : List TimedInputEvent) : List TimedInputBatch :=
  let rec loop (currentTime? : Option TimePoint) (currentEventsRev : List TimedInputEvent)
      (accRev : List TimedInputBatch) (remaining : List TimedInputEvent) : List TimedInputBatch :=
    match remaining with
    | [] =>
        match currentTime? with
        | none => accRev.reverse
        | some currentTime =>
            ({ currentTime := currentTime, events := currentEventsRev.reverse } :: accRev).reverse
    | evt :: rest =>
        match currentTime? with
        | none =>
            loop (some evt.at) [evt] accRev rest
        | some currentTime =>
            if evt.at = currentTime then
              loop currentTime? (evt :: currentEventsRev) accRev rest
            else
              let batch := { currentTime := currentTime, events := currentEventsRev.reverse }
              loop (some evt.at) [evt] (batch :: accRev) rest
  loop none [] [] events

def tacticBatches (seq : ManualTacticSequence) : List TimedInputBatch :=
  groupSortedEvents (sortTacticEvents seq.events)

private def foldSimulation
    (state : InputModel.GameState × List JudgeEvent)
    (batch : TimedInputBatch) : InputModel.GameState × List JudgeEvent :=
  let (st, accRev) := state
  let (nextState, events, _, _) := Scheduler.stepFrameTimed st batch
  (nextState, events.reverse ++ accRev)

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

def expandReplayBatchesFrom (startTime : TimePoint) (batches : List TimedInputBatch) : List TimedInputBatch :=
  expandTimedBatchesFrom startTime batches

private def noteCount (chart : ChartLoader.ChartSpec) : Nat :=
  chart.taps.length +
  chart.holds.length +
  chart.touches.length +
  chart.touchHolds.length +
  (chart.slides.filter (fun note => !note.isConnSlide || note.isGroupEnd)).length

private def judgedSlideNoteIndices (slides : List ChartLoader.SlideChartNote) : List Nat :=
  slides.filterMap (fun note =>
    if note.isConnSlide && !note.isGroupEnd then none else some note.noteIndex)

def chartNoteIndices (chart : ChartLoader.ChartSpec) : List Nat :=
  (chart.taps.map (fun note => note.noteIndex)) ++
  (chart.holds.map (fun note => note.noteIndex)) ++
  (chart.touches.map (fun note => note.noteIndex)) ++
  (chart.touchHolds.map (fun note => note.noteIndex)) ++
  judgedSlideNoteIndices chart.slides

def judgedNoteIndices (result : RuntimeSimulationResult) : List Nat :=
  result.events.map (fun evt => evt.noteIndex)

def missingJudgedNoteIndices (result : RuntimeSimulationResult) : List Nat :=
  let judged := judgedNoteIndices result
  (chartNoteIndices result.chart).filter (fun idx => !(judged.any (fun judgedIdx => judgedIdx = idx)))

def missingJudgedNoteCount (result : RuntimeSimulationResult) : Nat :=
  (missingJudgedNoteIndices result).length

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

def settleReplayBatchesFromChart (startTime : TimePoint) (chart : ChartLoader.ChartSpec) : List TimedInputBatch :=
  settleBatchesFrom startTime chart

def settleReplayBatchesUntil (startTime targetTime : TimePoint) : List TimedInputBatch :=
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

def replayBatchesFromState (initialState : InputModel.GameState) (batches : List TimedInputBatch) : RuntimeReplayResult :=
  let (finalState, eventsRev) := batches.foldl foldSimulation (initialState, [])
  { initialState := initialState
  , finalState := finalState
  , batches := batches
  , events := eventsRev.reverse }

def simulateStateWithTacticAndBatches
    (initialState : InputModel.GameState)
    (seq : ManualTacticSequence)
    (settleBatches : List TimedInputBatch) : RuntimeReplayResult :=
  let tactic := expandReplayBatchesFrom initialState.currentTime (tacticBatches seq)
  replayBatchesFromState initialState (tactic ++ settleBatches)

def simulateStateWithTacticUntil
    (initialState : InputModel.GameState)
    (seq : ManualTacticSequence)
    (targetTime : TimePoint) : RuntimeReplayResult :=
  let tactic := expandReplayBatchesFrom initialState.currentTime (tacticBatches seq)
  let tacticResult := replayBatchesFromState initialState tactic
  let settle := settleReplayBatchesUntil tacticResult.finalState.currentTime targetTime
  let finalResult := replayBatchesFromState tacticResult.finalState settle
  { initialState := initialState
  , finalState := finalResult.finalState
  , batches := tactic ++ settle
  , events := tacticResult.events ++ finalResult.events }

def simulateChartSpecWithTactic (chart : ChartLoader.ChartSpec) (seq : ManualTacticSequence) : RuntimeSimulationResult :=
  let initialState := ChartLoader.buildGameState chart
  let tactic := expandReplayBatchesFrom initialState.currentTime (tacticBatches seq)
  let tacticResult := replayBatchesFromState initialState tactic
  let settle := settleReplayBatchesFromChart tacticResult.finalState.currentTime chart
  let finalResult := replayBatchesFromState tacticResult.finalState settle
  { chart := chart
  , initialState := initialState
  , finalState := finalResult.finalState
  , batches := tactic ++ settle
  , events := tacticResult.events ++ finalResult.events }

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

def tapAtTime (time : TimePoint) (zone : ButtonZone) : ManualTacticAction :=
  .buttonClick time zone

def touchAtTime (time : TimePoint) (area : SensorArea) : ManualTacticAction :=
  .sensorClick time area

def holdButtonAtTime (time : TimePoint) (zone : ButtonZone) (isDown : Bool := true) : ManualTacticAction :=
  .buttonHold time zone isDown

def holdSensorAtTime (time : TimePoint) (area : SensorArea) (isDown : Bool := true) : ManualTacticAction :=
  .sensorHold time area isDown

def NoteTimingSkeleton.kind : NoteTimingSkeleton → NoteTimingSkeletonKind
  | .tap .. => .tap
  | .hold .. => .hold
  | .touch .. => .touch
  | .touchHold .. => .touchHold
  | .slide _ => .slide

def NoteTimingSkeleton.noteIndex : NoteTimingSkeleton → Nat
  | .tap noteIndex _ _ _ => noteIndex
  | .hold noteIndex _ _ _ _ => noteIndex
  | .touch noteIndex _ _ _ => noteIndex
  | .touchHold noteIndex _ _ _ _ => noteIndex
  | .slide spec => spec.noteIndex

def NoteTimingSkeleton.semanticTime : NoteTimingSkeleton → TimePoint
  | .tap _ semanticTime _ _ => semanticTime
  | .hold _ semanticTime _ _ _ => semanticTime
  | .touch _ semanticTime _ _ => semanticTime
  | .touchHold _ semanticTime _ _ _ => semanticTime
  | .slide spec => spec.headSemanticTime

private def skeletonLe (lhs rhs : NoteTimingSkeleton) : Bool :=
  lhs.semanticTime ≤ rhs.semanticTime

private def insertSkeleton (entry : NoteTimingSkeleton) : List NoteTimingSkeleton → List NoteTimingSkeleton
  | [] => [entry]
  | head :: rest =>
    if skeletonLe entry head then entry :: head :: rest else head :: insertSkeleton entry rest

def sortTimingSkeleton (entries : List NoteTimingSkeleton) : List NoteTimingSkeleton :=
  entries.foldl (fun acc entry => insertSkeleton entry acc) []

private def sensorInputTime (semanticTime : TimePoint) : TimePoint :=
  semanticTime + Constants.TOUCH_PANEL_OFFSET

private def chooseSlideStepAreas (fallback : SensorArea) (step : ChartLoader.SlideAreaSpec) : List SensorArea :=
  match step.policy, step.targetAreas with
  | .And, targets => if targets.isEmpty then [fallback] else targets
  | .Or, targets => if targets.isEmpty then [fallback] else targets

private def mergeSensorAreas (lhs rhs : List SensorArea) : List SensorArea :=
  (lhs ++ rhs).eraseDups

private def nthSlideAreaSpec?
    (steps : List ChartLoader.SlideAreaSpec) (index : Nat) : Option ChartLoader.SlideAreaSpec :=
  match steps, index with
  | [], _ => none
  | step :: _, 0 => some step
  | _ :: rest, index + 1 => nthSlideAreaSpec? rest index

private def slideRepresentativePathSteps (note : ChartLoader.SlideChartNote) : List (List SensorArea) :=
  let fallback := note.slot.toOuterSensorArea
  match note.slideKind with
  | SlideKind.Wifi =>
      let maxLen := note.judgeQueues.foldl (fun acc queue => max acc queue.length) 0
      (List.range maxLen).map (fun index =>
        note.judgeQueues.foldl
          (fun acc queue =>
            match nthSlideAreaSpec? queue index with
            | some step => mergeSensorAreas acc (chooseSlideStepAreas fallback step)
            | none => acc)
          ([] : List SensorArea))
  | _ =>
      match note.judgeQueues with
      | queue :: _ => queue.map (chooseSlideStepAreas fallback)
      | [] => []

def chartTimingSkeleton (chart : ChartLoader.ChartSpec) : List NoteTimingSkeleton :=
  let taps := chart.taps.map (fun note =>
    NoteTimingSkeleton.tap note.noteIndex note.timing note.timing note.slot.toButtonZone)
  let holds := chart.holds.map (fun note =>
    let releaseTime := note.timing + note.length
    NoteTimingSkeleton.hold note.noteIndex note.timing note.timing releaseTime note.slot.toButtonZone)
  let touches := chart.touches.map (fun note =>
    NoteTimingSkeleton.touch note.noteIndex note.timing (sensorInputTime note.timing) note.sensorPos)
  let touchHolds := chart.touchHolds.map (fun note =>
    let releaseTime := sensorInputTime (note.timing + note.length)
    NoteTimingSkeleton.touchHold note.noteIndex note.timing (sensorInputTime note.timing) releaseTime note.sensorPos)
  let slides := chart.slides.map (fun note =>
    let judgeSemanticTime := note.judgeAt.getD (note.startTiming + note.length)
    NoteTimingSkeleton.slide
      { noteIndex := note.noteIndex
      , headSemanticTime := note.timing
      , headInputTime := note.timing
      , startSemanticTime := note.startTiming
      , startInputTime := sensorInputTime note.startTiming
      , endSemanticTime := judgeSemanticTime
      , endInputTime := sensorInputTime judgeSemanticTime
      , headZone := note.slot.toButtonZone
      , pathSteps := slideRepresentativePathSteps note })
  sortTimingSkeleton (taps ++ holds ++ touches ++ touchHolds ++ slides)

private def evenlySpacedTimesBetween (startTime endTime : TimePoint) (count : Nat) : List TimePoint :=
  match count with
  | 0 => []
  | 1 => [endTime]
  | count + 1 =>
      let span := endTime - startTime
      let stride := Duration.divNat span count
      (List.range (count + 1)).map (fun index => startTime + Duration.scaleNat stride index)

private def slideStepReleaseTimes (startTimes : List TimePoint) (endTime : TimePoint) : List TimePoint :=
  let rec loop : List TimePoint → List TimePoint
    | [] => []
    | [_last] => [endTime + Constants.FRAME_LENGTH]
    | _current :: nextStart :: rest =>
        let releaseTime := nextStart - Duration.fromMicros 1
        releaseTime :: loop (nextStart :: rest)
  loop startTimes

private def resolveSlidePathStep (areas : List SensorArea) (startTime endTime : TimePoint) : List TimedInputEvent :=
  let downs := areas.map (fun area => holdSensorAtTime startTime area true)
  let ups := areas.map (fun area => holdSensorAtTime endTime area false)
  downs ++ ups

private def flattenEventLists (lists : List (List TimedInputEvent)) : List TimedInputEvent :=
  lists.foldr (· ++ ·) []

def resolveSingleTrackSlideWithHeadEvenly (spec : SlideTimingSkeleton) : ManualTacticSequence :=
  let head := [tapAtTime spec.headInputTime spec.headZone]
  let startTimes := evenlySpacedTimesBetween spec.startInputTime spec.endInputTime spec.pathSteps.length
  let endTimes := slideStepReleaseTimes startTimes spec.endInputTime
  let pathEvents :=
    flattenEventLists <| (List.zip spec.pathSteps (List.zip startTimes endTimes)).map (fun item =>
      let (areas, times) := item
      resolveSlidePathStep areas times.1 times.2)
  mkManualTacticSequence (head ++ pathEvents)

def resolveDefaultTimingSkeleton : NoteTimingSkeleton → ManualTacticSequence
  | .tap _ _ inputTime zone =>
      mkManualTacticSequence [tapAtTime inputTime zone]
  | .hold _ _ inputTime releaseInputTime zone =>
      let releaseTime := releaseInputTime + Duration.scaleNat Constants.FRAME_LENGTH 4
      mkManualTacticSequence [tapAtTime inputTime zone, holdButtonAtTime inputTime zone true, holdButtonAtTime releaseTime zone false]
  | .touch _ _ inputTime area =>
      mkManualTacticSequence [touchAtTime inputTime area]
  | .touchHold _ _ inputTime releaseInputTime area =>
      let releaseTime := releaseInputTime + Duration.scaleNat Constants.FRAME_LENGTH 4
      mkManualTacticSequence [touchAtTime inputTime area, holdSensorAtTime inputTime area true, holdSensorAtTime releaseTime area false]
  | .slide spec =>
      resolveSingleTrackSlideWithHeadEvenly spec

def resolveDefaultTimingSkeletonList (entries : List NoteTimingSkeleton) : ManualTacticSequence :=
  mkManualTacticSequence (flattenEventLists <| entries.map (fun entry => (resolveDefaultTimingSkeleton entry).events))

private def findTimingSkeletonOverride
    (overrides : List TimingSkeletonOverride) (noteIndex : Nat) : Option TimingSkeletonOverride :=
  match overrides with
  | [] => none
  | head :: rest =>
      if head.noteIndex = noteIndex then some head else findTimingSkeletonOverride rest noteIndex

def fixedTimingSkeletonOverride (noteIndex : Nat) (seq : ManualTacticSequence) : TimingSkeletonOverride :=
  { noteIndex := noteIndex, resolve := fun _ => seq }

def resolveTimingSkeletonWithOverrides
    (overrides : List TimingSkeletonOverride) (entry : NoteTimingSkeleton) : ManualTacticSequence :=
  match findTimingSkeletonOverride overrides entry.noteIndex with
  | some override => override.resolve entry
  | none => resolveDefaultTimingSkeleton entry

def resolveTimingSkeletonListWithOverrides
    (overrides : List TimingSkeletonOverride) (entries : List NoteTimingSkeleton) : ManualTacticSequence :=
  mkManualTacticSequence <|
    flattenEventLists <| entries.map (fun entry => (resolveTimingSkeletonWithOverrides overrides entry).events)

def defaultTacticFromChart (chart : ChartLoader.ChartSpec) : ManualTacticSequence :=
  resolveDefaultTimingSkeletonList (chartTimingSkeleton chart)

def tacticFromChartWithOverrides
    (chart : ChartLoader.ChartSpec) (overrides : List TimingSkeletonOverride) : ManualTacticSequence :=
  resolveTimingSkeletonListWithOverrides overrides (chartTimingSkeleton chart)

def timingSkeletonFromChartSection
    (content : String) (levelIndex : Nat := 1) :
    Except Simai.ParseError (List NoteTimingSkeleton) := do
  let chart ← compileRuntimeChartSection content levelIndex
  return chartTimingSkeleton chart

def defaultTacticFromChartSection
    (content : String) (levelIndex : Nat := 1) :
    Except Simai.ParseError ManualTacticSequence := do
  let chart ← compileRuntimeChartSection content levelIndex
  return defaultTacticFromChart chart

def tacticFromChartSectionWithOverrides
    (content : String) (overrides : List TimingSkeletonOverride) (levelIndex : Nat := 1) :
    Except Simai.ParseError ManualTacticSequence := do
  let chart ← compileRuntimeChartSection content levelIndex
  return tacticFromChartWithOverrides chart overrides

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

def exampleDelayedSingleTapSensorTactic : ManualTacticSequence :=
  manual_tactic! "516000 touch A1"

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
