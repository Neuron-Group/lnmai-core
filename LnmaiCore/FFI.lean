
import Lean.Data.Json
import LnmaiCore.Simai.Frontend
import LnmaiCore.ChartLoader
import LnmaiCore.Proofs.Runtime
import LnmaiCore.Scheduler
import LnmaiCore.InputModel
import LnmaiCore.AutoplayPlan
import Std.Data.HashMap
import Std.Sync.Mutex

set_option compiler.ignoreBorrowAnnotation true

open Lean

namespace LnmaiCore.FFI

open Std

def ffiAbiVersion : Nat := 2

private def jsonString (json : Json) : String :=
  Json.compress json

private def okJson (payload : Json) : Json :=
  Json.mkObj
    [ ("ok", Json.bool true)
    , ("result", payload) ]

private def errorJson (code message : String) (details : Option Json := none) : Json :=
  let base :=
    [ ("ok", Json.bool false)
    , ("error", Json.mkObj [ ("code", Json.str code), ("message", Json.str message) ]) ]
  match details with
  | some extra => Json.mkObj (base ++ [("details", extra)])
  | none => Json.mkObj base

private def parseResultJson {α : Type} [ToJson α] (result : Except Simai.ParseError α) : String :=
  match result with
  | .ok value => jsonString <| okJson (toJson value)
  | .error err =>
      jsonString <| errorJson "parse_error" err.message (some (toJson err))

private def decodeJsonString (content : String) : Except String Json :=
  Json.parse content

private def decodeValueFromString {α : Type} [FromJson α] (content : String) : Except String α := do
  let json ← decodeJsonString content
  fromJson? json

private def stringResultJson {α : Type} [ToJson α] (errorCode : String) (result : Except String α) : String :=
  match result with
  | .ok value => jsonString <| okJson (toJson value)
  | .error err => jsonString <| errorJson errorCode err

structure RuntimeStepResult where
  state : InputModel.GameState
  events : List JudgeEvent
  audioCommands : List AudioCommand
  renderCommands : List RenderCommand
  terminated : Bool
deriving Inhabited, Repr, ToJson, FromJson

structure RuntimeStepLightResult where
  events : List JudgeEvent
  audioCommands : List AudioCommand
  renderCommands : List RenderCommand
  score : ScoreState
  currentTime : TimePoint
  terminated : Bool
deriving Inhabited, Repr, ToJson, FromJson

structure LoadedChartSummary where
  tapCount : Nat
  holdCount : Nat
  touchCount : Nat
  touchHoldCount : Nat
  slideHeadCount : Nat
  slideCount : Nat
deriving Inhabited, Repr, ToJson, FromJson

inductive RuntimeSession where
  | empty
  | loaded (chartSpec : ChartLoader.ChartSpec) (state : InputModel.GameState)
deriving Inhabited

structure RuntimeRegistry where
  nextHandle : UInt64 := 1
  sessions : HashMap UInt64 RuntimeSession := {}
deriving Inhabited

initialize runtimeRegistryMutex : Std.Mutex RuntimeRegistry ← Std.Mutex.new {}

private def makeHandleJson (handle : UInt64) : Json :=
  Json.mkObj [("handle", toJson handle)]

private def makeVersionJson : Json :=
  Json.mkObj
    [ ("abiVersion", toJson ffiAbiVersion)
    , ("schema", Json.str "lnmai-core-ffi-json") ]

private def makeLoadedChartSummary (chartSpec : ChartLoader.ChartSpec) : LoadedChartSummary :=
  { tapCount := chartSpec.taps.length
  , holdCount := chartSpec.holds.length
  , touchCount := chartSpec.touches.length
  , touchHoldCount := chartSpec.touchHolds.length
  , slideHeadCount := chartSpec.slideHeads.length
  , slideCount := chartSpec.slides.length }

private def makeLoadedChartJson (handle : UInt64) (chartSpec : ChartLoader.ChartSpec) : Json :=
  Json.mkObj
    [ ("handle", toJson handle)
    , ("summary", toJson (makeLoadedChartSummary chartSpec)) ]

private def makeSessionStateJson (state : String) : Json :=
  Json.mkObj [("state", Json.str state)]

private def makeLoadedSessionStateJson (handle : UInt64) (chartSpec : ChartLoader.ChartSpec) : Json :=
  Json.mkObj
    [ ("handle", toJson handle)
    , ("state", Json.str "loaded")
    , ("summary", toJson (makeLoadedChartSummary chartSpec)) ]

private def makeUnloadedSessionStateJson (handle : UInt64) : Json :=
  Json.mkObj
    [ ("handle", toJson handle)
    , ("state", Json.str "empty") ]

private def getHandleState (handle : UInt64) : IO (Except String InputModel.GameState) := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    pure <| match registry.sessions.get? handle with
      | some (.loaded _ state) => .ok state
      | some .empty => .error s!"runtime handle {handle} has no loaded chart"
      | none => .error s!"unknown runtime handle: {handle}"

private def getHandleChartSpec (handle : UInt64) : IO (Except String ChartLoader.ChartSpec) := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    pure <| match registry.sessions.get? handle with
      | some (.loaded chartSpec _) => .ok chartSpec
      | some .empty => .error s!"runtime handle {handle} has no loaded chart"
      | none => .error s!"unknown runtime handle: {handle}"

private def createEmptyHandle : IO UInt64 := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    let handle := registry.nextHandle
    let nextHandle := handle + 1
    let sessions := registry.sessions.insert handle .empty
    let nextRegistry : RuntimeRegistry := { nextHandle := nextHandle, sessions := sessions }
    set nextRegistry
    pure handle

private def insertHandleState (state : InputModel.GameState) : IO UInt64 := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    let handle := registry.nextHandle
    let nextHandle := handle + 1
    let sessions := registry.sessions.insert handle (.loaded { } state)
    let nextRegistry : RuntimeRegistry := { nextHandle := nextHandle, sessions := sessions }
    set nextRegistry
    pure handle

private def insertLoadedHandle (chartSpec : ChartLoader.ChartSpec) : IO UInt64 := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    let handle := registry.nextHandle
    let nextHandle := handle + 1
    let state := ChartLoader.buildGameState chartSpec
    let sessions := registry.sessions.insert handle (.loaded chartSpec state)
    let nextRegistry : RuntimeRegistry := { nextHandle := nextHandle, sessions := sessions }
    set nextRegistry
    pure handle

private def updateHandleState (handle : UInt64) (state : InputModel.GameState) : IO (Except String PUnit) := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    match registry.sessions.get? handle with
    | none =>
        pure <| .error s!"unknown runtime handle: {handle}"
    | some .empty =>
        pure <| .error s!"runtime handle {handle} has no loaded chart"
    | some (.loaded chartSpec _) =>
        let nextRegistry : RuntimeRegistry := { registry with sessions := registry.sessions.insert handle (.loaded chartSpec state) }
        set nextRegistry
        pure <| .ok PUnit.unit

private def loadHandleChart (handle : UInt64) (chartSpec : ChartLoader.ChartSpec) : IO (Except String PUnit) := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    match registry.sessions.get? handle with
    | none =>
        pure <| .error s!"unknown runtime handle: {handle}"
    | some (.loaded _ _) =>
        pure <| .error s!"runtime handle {handle} already has a loaded chart"
    | some .empty =>
        let state := ChartLoader.buildGameState chartSpec
        let nextRegistry : RuntimeRegistry :=
          { registry with sessions := registry.sessions.insert handle (.loaded chartSpec state) }
        set nextRegistry
        pure <| .ok PUnit.unit

private def unloadHandleChart (handle : UInt64) : IO (Except String PUnit) := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    match registry.sessions.get? handle with
    | none =>
        pure <| .error s!"unknown runtime handle: {handle}"
    | some .empty =>
        pure <| .error s!"runtime handle {handle} already has no loaded chart"
    | some (.loaded _ _) =>
        let nextRegistry : RuntimeRegistry :=
          { registry with sessions := registry.sessions.insert handle .empty }
        set nextRegistry
        pure <| .ok PUnit.unit

private def stepHandleState
    (handle : UInt64)
    (batch : InputModel.TimedInputBatch)
    : IO (Except String (InputModel.GameState × List JudgeEvent × List AudioCommand × List RenderCommand)) := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    match registry.sessions.get? handle with
    | none =>
        pure <| .error s!"unknown runtime handle: {handle}"
    | some .empty =>
        pure <| .error s!"runtime handle {handle} has no loaded chart"
    | some (.loaded chartSpec state) =>
        let (nextState, events, audioCommands, renderCommands) := Scheduler.stepFrameTimed state batch
        let sessions := registry.sessions.insert handle (.loaded chartSpec nextState)
        let nextRegistry : RuntimeRegistry := { registry with sessions := sessions }
        set nextRegistry
        pure <| .ok (nextState, events, audioCommands, renderCommands)

private def eraseHandleState (handle : UInt64) : IO Bool := do
  runtimeRegistryMutex.atomically do
    let registry ← get
    let present := registry.sessions.contains handle
    if present then
      let nextRegistry : RuntimeRegistry := { registry with sessions := registry.sessions.erase handle }
      set nextRegistry
    pure present

@[export lnmai_ffi_version_json]
def ffiVersionJson : IO String :=
  pure <| jsonString <| okJson makeVersionJson

@[export lnmai_parse_frontend_chart_json]
def parseFrontendChartJson (content : @& String) (levelIndex : UInt32) : String :=
  parseResultJson <| Simai.parseFrontendChartResult content levelIndex.toNat

@[export lnmai_parse_frontend_chart_mode_json]
def parseFrontendChartModeJson (content : @& String) (levelIndex : UInt32)
    (isClassic : Bool) : String :=
  parseResultJson <| Simai.parseFrontendChartResultWithMode content levelIndex.toNat isClassic

@[export lnmai_parse_frontend_semantic_chart_json]
def parseFrontendSemanticChartJson (content : @& String) (levelIndex : UInt32) : String :=
  parseResultJson <| Simai.parseFrontendSemanticChart content levelIndex.toNat

@[export lnmai_parse_frontend_inspection_chart_json]
def parseFrontendInspectionChartJson (content : @& String) (levelIndex : UInt32) : String :=
  parseResultJson <| Simai.parseFrontendInspectionChart content levelIndex.toNat

@[export lnmai_parse_normalized_chart_json]
def parseNormalizedChartJson (content : @& String) (levelIndex : UInt32) : String :=
  parseResultJson <| Simai.frontendNormalizedChart content levelIndex.toNat

@[export lnmai_parse_lowered_chart_json]
def parseLoweredChartJson (content : @& String) (levelIndex : UInt32) : String :=
  parseResultJson <| Simai.frontendLoweredChart content levelIndex.toNat

@[export lnmai_parse_lowered_chart_mode_json]
def parseLoweredChartModeJson (content : @& String) (levelIndex : UInt32) (isClassic : Bool) : String :=
  parseResultJson <| Simai.frontendLoweredChartWithMode content levelIndex.toNat isClassic

@[export lnmai_build_game_state_json]
def buildGameStateJson (chartSpecJson : @& String) : String :=
  stringResultJson "invalid_chart_spec_json" <| do
    let chartSpec : ChartLoader.ChartSpec ← decodeValueFromString chartSpecJson
    pure <| ChartLoader.buildGameState chartSpec

@[export lnmai_default_tactic_from_chart_json]
def defaultTacticFromChartJson (chartSpecJson : @& String) : String :=
  stringResultJson "invalid_chart_spec_json" <| do
    let chartSpec : ChartLoader.ChartSpec ← decodeValueFromString chartSpecJson
    pure <| defaultTacticFromChart chartSpec

@[export lnmai_step_game_state_json]
def stepGameStateJson (stateJson : @& String) (batchJson : @& String) : String :=
  stringResultJson "invalid_runtime_json" <| do
    let state : InputModel.GameState ← decodeValueFromString stateJson
    let batch : InputModel.TimedInputBatch ← decodeValueFromString batchJson
    let (nextState, events, audioCommands, renderCommands) := Scheduler.stepFrameTimed state batch
    let result : RuntimeStepResult :=
      { state := nextState
      , events := events
      , audioCommands := audioCommands
      , renderCommands := renderCommands
      , terminated := Scheduler.isTerminated nextState }
    pure result

@[export lnmai_create_game_state_handle]
def createGameStateHandle (chartSpecJson : @& String) : IO String := do
  match decodeValueFromString (α := ChartLoader.ChartSpec) chartSpecJson with
  | .error err =>
      pure <| jsonString <| errorJson "invalid_chart_spec_json" err
  | .ok chartSpec =>
      let handle ← insertLoadedHandle chartSpec
      pure <| jsonString <| okJson (makeHandleJson handle)

@[export lnmai_create_empty_session_handle]
def createEmptySessionHandle : IO String := do
  let handle ← createEmptyHandle
  pure <| jsonString <| okJson (Json.mkObj [("handle", toJson handle), ("state", Json.str "empty")])

@[export lnmai_load_chart_into_session_from_text]
def loadChartIntoSessionFromText (handle : UInt64) (content : @& String) (levelIndex : UInt32) : IO String := do
  match Simai.frontendLoweredChart content levelIndex.toNat with
  | .error err =>
      pure <| jsonString <| errorJson "parse_error" err.message (some (toJson err))
  | .ok chartSpec =>
      let loaded ← loadHandleChart handle chartSpec
      match loaded with
      | .error err =>
          pure <| jsonString <| errorJson "invalid_session_state" err
      | .ok _ =>
          pure <| jsonString <| okJson (makeLoadedSessionStateJson handle chartSpec)

@[export lnmai_load_chart_into_session_from_json]
def loadChartIntoSessionFromJson (handle : UInt64) (chartSpecJson : @& String) : IO String := do
  match decodeValueFromString (α := ChartLoader.ChartSpec) chartSpecJson with
  | .error err =>
      pure <| jsonString <| errorJson "invalid_chart_spec_json" err
  | .ok chartSpec =>
      let loaded ← loadHandleChart handle chartSpec
      match loaded with
      | .error err =>
          pure <| jsonString <| errorJson "invalid_session_state" err
      | .ok _ =>
          pure <| jsonString <| okJson (makeLoadedSessionStateJson handle chartSpec)

@[export lnmai_unload_chart_from_session]
def unloadChartFromSession (handle : UInt64) : IO String := do
  let unloaded ← unloadHandleChart handle
  match unloaded with
  | .error err =>
      pure <| jsonString <| errorJson "invalid_session_state" err
  | .ok _ =>
      pure <| jsonString <| okJson (makeUnloadedSessionStateJson handle)

@[export lnmai_get_lowered_chart_json_by_handle]
def getLoweredChartJsonByHandle (handle : UInt64) : IO String := do
  let result ← getHandleChartSpec handle
  match result with
  | .error err =>
      pure <| jsonString <| errorJson "invalid_session_state" err
  | .ok chartSpec =>
      pure <| jsonString <| okJson (toJson chartSpec)

@[export lnmai_free_game_state_handle]
def freeGameStateHandle (handle : UInt64) : IO String := do
  let existed ← eraseHandleState handle
  if existed then
    pure <| jsonString <| okJson (Json.mkObj [("freed", Json.bool true)])
  else
    pure <| jsonString <| errorJson "invalid_runtime_handle" s!"unknown runtime handle: {handle}"

@[export lnmai_get_game_state_json_by_handle]
def getGameStateJsonByHandle (handle : UInt64) : IO String := do
  let result ← getHandleState handle
  pure <| stringResultJson "invalid_runtime_handle" result

@[export lnmai_step_game_state_handle]
def stepGameStateHandle (handle : UInt64) (batchJson : @& String) : IO String := do
  match decodeValueFromString (α := InputModel.TimedInputBatch) batchJson with
  | .error err =>
      pure <| jsonString <| errorJson "invalid_runtime_json" err
  | .ok batch =>
      let stepped ← stepHandleState handle batch
      match stepped with
      | .error err =>
          pure <| jsonString <| errorJson "invalid_runtime_handle" err
      | .ok (nextState, events, audioCommands, renderCommands) =>
          let result : RuntimeStepResult :=
            { state := nextState
            , events := events
            , audioCommands := audioCommands
            , renderCommands := renderCommands
            , terminated := Scheduler.isTerminated nextState }
          pure <| jsonString <| okJson (toJson result)

@[export lnmai_step_game_state_handle_light]
def stepGameStateHandleLight (handle : UInt64) (batchJson : @& String) : IO String := do
  match decodeValueFromString (α := InputModel.TimedInputBatch) batchJson with
  | .error err =>
      pure <| jsonString <| errorJson "invalid_runtime_json" err
  | .ok batch =>
      let stepped ← stepHandleState handle batch
      match stepped with
      | .error err =>
          pure <| jsonString <| errorJson "invalid_runtime_handle" err
      | .ok (nextState, events, audioCommands, renderCommands) =>
          let result : RuntimeStepLightResult :=
            { events := events
            , audioCommands := audioCommands
            , renderCommands := renderCommands
            , score := nextState.score
            , currentTime := nextState.currentTime
            , terminated := Scheduler.isTerminated nextState }
          pure <| jsonString <| okJson (toJson result)

/- Native ABI 3. Only C bridge code sees pointers; Lean sees opaque addresses. -/
@[extern "lnmai_native_request_word"]
opaque nativeRequestWord (request field : UInt64) : UInt64
@[extern "lnmai_native_event_word"]
opaque nativeEventWord (request index field : UInt64) : UInt64
@[extern "lnmai_native_request_text"]
opaque nativeRequestText (request : UInt64) : String
@[extern "lnmai_native_push"]
opaque nativePush (out tag a b c d e f g h : UInt64) : IO Unit
@[extern "lnmai_native_output_ok"]
opaque nativeOutputOk (out : UInt64) : IO Bool
@[extern "lnmai_native_set_error"]
opaque nativeSetError (out : UInt64) (message : String) : IO Unit

private def emit (out : UInt64) (tag : UInt64) (values : List Int) : IO Unit :=
  nativePush out tag (UInt64.ofInt (values[0]?.getD 0)) (UInt64.ofInt (values[1]?.getD 0))
    (UInt64.ofInt (values[2]?.getD 0)) (UInt64.ofInt (values[3]?.getD 0))
    (UInt64.ofInt (values[4]?.getD 0)) (UInt64.ofInt (values[5]?.getD 0))
    (UInt64.ofInt (values[6]?.getD 0)) (UInt64.ofInt (values[7]?.getD 0))

private def signedWord (value : UInt64) : Int :=
  if value.toNat >= 2^63 then (value.toNat : Int) - 2^64 else value.toNat
private def areaIndex (area : SensorArea) : Nat := SensorArea.all.idxOf area
private def nativeGrades : List JudgeGrade :=
  [.Miss, .LateGood, .LateGreat3rd, .LateGreat2nd, .LateGreat,
   .LatePerfect3rd, .LatePerfect2nd, .Perfect, .FastPerfect2nd,
   .FastPerfect3rd, .FastGreat, .FastGreat2nd, .FastGreat3rd, .FastGood, .TooFast]
private def gradeIndex (grade : JudgeGrade) : Nat := nativeGrades.idxOf grade
private def kindIndex : JudgeEventKind → Nat
  | .Tap => 0 | .Hold => 1 | .Slide => 2 | .Touch => 3 | .Break => 4
private def optionalNat (value : Option Nat) : Int := value.map Int.ofNat |>.getD (-1)
private def optionalTime (value : Option TimePoint) : Int := value.map TimePoint.toMicros |>.getD (-9223372036854775808)
private def flags (a b : Bool) : Int := (if a then 1 else 0) + (if b then 2 else 0)

private def emitScore (out : UInt64) (state : InputModel.GameState) : IO Unit := do
  let s := state.score
  emit out 1 [state.currentTime.toMicros, if Scheduler.isTerminated state then 1 else 0]
  emit out 2 [s.combo, s.pCombo, s.cPCombo, s.totalBase, s.totalExtra, s.earnedBase, s.earnedExtra, s.earnedClassicExtra]
  emit out 3 [s.lostBase, s.lostExtra, s.lostClassicExtra, s.dxScore, s.maxDxScore, s.fastCount, s.lateCount]
  for (counts, kind) in [(s.counts.tapCount, 0), (s.counts.holdCount, 1), (s.counts.slideCount, 2),
      (s.counts.touchCount, 3), (s.counts.breakCount, 4)] do
    for grade in nativeGrades do
      emit out 4 [kind, gradeIndex grade, counts grade]

private def emitStep (out : UInt64) (state : InputModel.GameState)
    (events : List JudgeEvent) (audio : List AudioCommand) (render : List RenderCommand) : IO Unit := do
  emitScore out state
  for event in events do
    let pos := (event.position.buttonZone?.map (fun zone => zone.toIndex + 1) |>.getD 0) +
      256 * (event.position.sensorArea?.map (fun area => areaIndex area + 1) |>.getD 0)
    emit out 10 [kindIndex event.kind, if event.phase == .head then 0 else 1, gradeIndex event.grade,
      event.diff.toMicros, event.noteIndex, pos, flags event.isBreak event.isEX, event.multiple]
  for command in audio do
    match command with
    | .PlayJudgeSfx kind grade isBreak atTime noteIndex =>
      emit out 20 [0, kindIndex kind, gradeIndex grade, if isBreak then 1 else 0, atTime.toMicros, noteIndex]
    | .PlaySlideCue noteIndex trackIndex isBreak atTime =>
      emit out 20 [1, noteIndex, trackIndex, if isBreak then 1 else 0, atTime.toMicros]
    | .PlayTouchHoldBody noteIndex atTime => emit out 20 [2, noteIndex, atTime.toMicros]
  for command in render do
    match command with
    | .ShowJudgeResult kind grade isBreak diff noteIndex =>
      emit out 30 [0, kindIndex kind, gradeIndex grade, if isBreak then 1 else 0, diff.toMicros, noteIndex]
    | .UpdateSlideProgress noteIndex remaining => emit out 30 [1, noteIndex, remaining]
    | .UpdateSlideTrackProgress noteIndex trackIndex remaining => emit out 30 [2, noteIndex, trackIndex, remaining]
    | .HideAllSlideBars noteIndex => emit out 30 [3, noteIndex]
    | .HideSlideBars noteIndex endIndex => emit out 30 [4, noteIndex, endIndex]
    | .HideSlideTrackBars noteIndex trackIndex endIndex => emit out 30 [5, noteIndex, trackIndex, endIndex]

private def readNativeBatch (request : UInt64) : InputModel.TimedInputBatch := Id.run do
  let mut events := []
  for index in List.range (nativeRequestWord request 2).toNat do
    let word := nativeEventWord request index.toUInt64
    let time := TimePoint.fromMicros (signedWord (word 0))
    let pos := (word 2).toNat
    let down := word 3 != 0
    let event := match (word 1).toNat with
      | 0 => InputModel.TimedInputEvent.buttonClick time (ButtonZone.ofIndex? pos |>.getD .K1)
      | 1 => .buttonHold time (ButtonZone.ofIndex? pos |>.getD .K1) down
      | 2 => .sensorClick time (SensorArea.all[pos]?.getD .A1)
      | _ => .sensorHold time (SensorArea.all[pos]?.getD .A1) down
    events := event :: events
  return { currentTime := TimePoint.fromMicros (signedWord (nativeRequestWord request 0)), events := events.reverse }

private def emitTacticEvent (out : UInt64) (event : InputModel.TimedInputEvent) : IO Unit :=
  match event with
  | .buttonClick time zone => emit out 52 [0, time.toMicros, zone.toIndex]
  | .buttonHold time zone down => emit out 52 [1, time.toMicros, zone.toIndex, if down then 1 else 0]
  | .sensorClick time area => emit out 52 [2, time.toMicros, areaIndex area]
  | .sensorHold time area down => emit out 52 [3, time.toMicros, areaIndex area, if down then 1 else 0]

private def emitPlan (out : UInt64) (chart : ChartLoader.ChartSpec) : IO Unit := do
  for group in (autoplayPlanFromChart chart).groups do
    emit out 50 [group.id, group.kind]
    for part in group.parts do
      emit out 51 [part.runtimeNoteIndex, part.role, optionalNat part.trackIndex]
      for event in part.events do emitTacticEvent out event

private def emitChart (out : UInt64) (chart : ChartLoader.ChartSpec) : IO Unit := do
  emit out 100 [optionalNat (chart.slideSkipping.map (fun b => if b then 1 else 0))]
  for note in chart.taps do
    emit out 101 [note.noteIndex, note.timing.toMicros, note.slot.toIndex, flags note.isBreak note.isEX, note.buttonQueueIndex]
  for note in chart.holds do
    emit out 102 [note.noteIndex, note.timing.toMicros, note.slot.toIndex, note.length.toMicros,
      flags note.isBreak note.isEX, if note.isTouch then 1 else 0, optionalNat (note.isClassic.map (fun b => if b then 1 else 0)), note.buttonQueueIndex]
    emit out 112 [optionalNat note.touchHoldGroupId, optionalNat note.touchHoldGroupSize]
  for note in chart.touches do
    emit out 103 [note.noteIndex, note.timing.toMicros, areaIndex note.sensorPos, if note.isBreak then 1 else 0,
      optionalNat note.sourceGroupId, optionalNat note.sourceGroupIndex, optionalNat note.sourceGroupSize, note.touchQueueIndex]
    emit out 113 [optionalNat note.touchGroupId, optionalNat note.touchGroupSize]
  for note in chart.touchHolds do
    emit out 104 [note.noteIndex, note.timing.toMicros, areaIndex note.sensorPos, note.length.toMicros,
      flags note.isBreak note.isEX, optionalNat note.sourceGroupId, optionalNat note.sourceGroupIndex, optionalNat note.sourceGroupSize]
    emit out 114 [note.touchQueueIndex, optionalNat note.touchGroupId, optionalNat note.touchGroupSize,
      optionalNat note.touchHoldGroupId, optionalNat note.touchHoldGroupSize]
  for note in chart.slideHeads do
    emit out 105 [note.noteIndex, note.logicalSlideId, note.timing.toMicros, note.slot.toIndex, flags note.isBreak note.isEX]
  for note in chart.slides do
    let kind := match note.slideKind with | .Single => 0 | .Wifi => 1 | .ConnPart => 2
    emit out 106 [note.noteIndex, note.logicalSlideId, note.headTiming.toMicros, note.startTiming.toMicros,
      note.length.toMicros, note.slot.toIndex, kind, flags note.isBreak note.isEX]
    emit out 107 [optionalNat note.parentNoteIndex, note.totalJudgeQueueLen, note.trackCount,
      optionalTime note.judgeAt, note.multiple, flags note.isClassic note.isSlideNoHead,
      flags note.isConnSlide note.isGroupHead, flags note.isGroupEnd note.parentFinished]
    emit out 117 [if note.parentPendingFinish then 1 else 0]
    for (queue, track) in note.judgeQueues.zipIdx do
      emit out 108 [track]
      for area in queue do
        let mask := area.targetAreas.foldl (fun mask sensor => mask ||| (1 <<< areaIndex sensor)) (0 : Nat)
        emit out 109 [mask, if area.policy == .Or then 0 else 1, flags area.isLast area.isSkippable,
          area.arrowProgressWhenOn, area.arrowProgressWhenFinished]

@[export lnmai_native_dispatch]
def nativeDispatch (handle : UInt64) (operation : UInt32) (request out : UInt64) : IO UInt32 := do
  runtimeRegistryMutex.atomically do
    let registry : RuntimeRegistry ← get
    let fail (message : String) : IO UInt32 := do
      nativeSetError out message
      pure 1
    if operation == 0 then
      let id := registry.nextHandle
      emit out 0 [id.toNat]
      if !(← nativeOutputOk out) then return 2
      set { registry with nextHandle := id + 1, sessions := registry.sessions.insert id .empty }
      return 0
    if operation == 7 then
      if !registry.sessions.contains handle then return ← fail s!"unknown runtime handle: {handle}"
      set { registry with sessions := registry.sessions.erase handle }
      return 0
    match registry.sessions.get? handle with
    | none => return ← fail s!"unknown runtime handle: {handle}"
    | some .empty =>
      if operation != 1 then return ← fail "session has no loaded chart"
      match Simai.frontendLoweredChart (nativeRequestText request) (nativeRequestWord request 1).toNat with
      | .error error => return ← fail error.message
      | .ok chart =>
        let state := ChartLoader.buildGameState chart
        emitScore out state
        if !(← nativeOutputOk out) then return 2
        set { registry with sessions := registry.sessions.insert handle (.loaded chart state) }
        return 0
    | some (.loaded chart state) =>
      if operation == 2 then
        let batch := readNativeBatch request
        if batch.currentTime < state.currentTime then return ← fail "clock moved backwards: use seek"
        let (next, events, audio, render) := Scheduler.stepFrameTimed state batch
        emitStep out next events audio render
        if !(← nativeOutputOk out) then return 2
        set { registry with sessions := registry.sessions.insert handle (.loaded chart next) }
        return 0
      if operation == 3 then
        let target := TimePoint.fromMicros (signedWord (nativeRequestWord request 0))
        let next := RuntimeSeek.rebuild chart state target
        let retained := RuntimeSeek.noteIndices (RuntimeSeek.chartFrom chart target)
        emitScore out next
        for id in RuntimeSeek.noteIndices chart do emit out (if retained.contains id then 40 else 41) [id]
        if !(← nativeOutputOk out) then return 2
        set { registry with sessions := registry.sessions.insert handle (.loaded chart next) }
        return 0
      if operation == 4 then
        emitPlan out chart
        return if (← nativeOutputOk out) then 0 else 2
      if operation == 5 then
        emitChart out chart
        return if (← nativeOutputOk out) then 0 else 2
      if operation == 6 then
        for slide in state.slides do
          emit out 110 [slide.params.noteIndex, slide.headTiming.toMicros, slide.totalJudgeQueueLen,
            slide.trackCount, slide.initialQueueRemaining]
        return if (← nativeOutputOk out) then 0 else 2
      return ← fail "unsupported native operation"

end LnmaiCore.FFI
