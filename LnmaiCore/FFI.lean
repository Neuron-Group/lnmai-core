
import Lean.Data.Json
import LnmaiCore.Simai.Frontend
import LnmaiCore.ChartLoader
import LnmaiCore.Scheduler
import LnmaiCore.InputModel
import Std.Data.HashMap
import Std.Sync.Mutex

set_option compiler.ignoreBorrowAnnotation true

open Lean

namespace LnmaiCore.FFI

open Std

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
deriving Inhabited, Repr, ToJson, FromJson

structure RuntimeStepLightResult where
  events : List JudgeEvent
  audioCommands : List AudioCommand
  renderCommands : List RenderCommand
  score : ScoreState
  currentTime : TimePoint
deriving Inhabited, Repr, ToJson, FromJson

structure LoadedChartSummary where
  tapCount : Nat
  holdCount : Nat
  touchCount : Nat
  touchHoldCount : Nat
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

private def makeLoadedChartSummary (chartSpec : ChartLoader.ChartSpec) : LoadedChartSummary :=
  { tapCount := chartSpec.taps.length
  , holdCount := chartSpec.holds.length
  , touchCount := chartSpec.touches.length
  , touchHoldCount := chartSpec.touchHolds.length
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

@[export lnmai_parse_frontend_chart_json]
def parseFrontendChartJson (content : @& String) (levelIndex : UInt32) : String :=
  parseResultJson <| Simai.parseFrontendChartResult content levelIndex.toNat

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

@[export lnmai_build_game_state_json]
def buildGameStateJson (chartSpecJson : @& String) : String :=
  stringResultJson "invalid_chart_spec_json" <| do
    let chartSpec : ChartLoader.ChartSpec ← decodeValueFromString chartSpecJson
    pure <| ChartLoader.buildGameState chartSpec

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
      , renderCommands := renderCommands }
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
            , renderCommands := renderCommands }
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
            , currentTime := nextState.currentTime }
          pure <| jsonString <| okJson (toJson result)

end LnmaiCore.FFI
