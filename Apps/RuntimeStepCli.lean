/-
  Stateful runtime companion to `simai-parser-cli`.

  Reads newline-delimited JSON requests on stdin and writes one JSON response
  line per request. It owns a single process-local session handle.

  Requests:
    {"op":"load","content":"<maidata>","levelIndex":1}
    {"op":"step","batch":{...}}          -- TimedInputBatch JSON
    {"op":"state"}                       -- current GameState JSON
    {"op":"lowered"}                     -- loaded ChartSpec JSON
    {"op":"free"}
-/
import LnmaiCore.FFI
import LnmaiCore.RuntimeSeek
import Lean.Data.Json

open Lean

@[extern "lnmai_test_native_call"]
opaque testNativeCall (handle : UInt64) (operation : UInt32) (time : UInt64)
    (level : UInt32) (events : @& Array UInt64) (content : @& String) :
    IO (UInt32 × Array (Array UInt64) × String)

private def ok (value : Json) : String :=
  (Json.mkObj [("ok", toJson true), ("result", value)]).compress

private def jsonRequest (json : Json) : IO String := do
  let action := (json.getObjValAs? String "action").toOption.getD ""
  let arg := (json.getObjValAs? String "arg").toOption.getD "null"
  let batch := (json.getObjValAs? String "batchText").toOption.getD "null"
  let handle := ((json.getObjValAs? Nat "handle").toOption.getD 0).toUInt64
  let level := ((json.getObjValAs? Nat "levelIndex").toOption.getD 1).toUInt32
  match action with
  | "version" => LnmaiCore.FFI.ffiVersionJson
  | "build" => pure (LnmaiCore.FFI.buildGameStateJson arg)
  | "tactic" => pure (LnmaiCore.FFI.defaultTacticFromChartJson arg)
  | "step" => pure (LnmaiCore.FFI.stepGameStateJson arg batch)
  | "create" => LnmaiCore.FFI.createGameStateHandle arg
  | "empty" => LnmaiCore.FFI.createEmptySessionHandle
  | "loadText" => LnmaiCore.FFI.loadChartIntoSessionFromText handle arg level
  | "loadJson" => LnmaiCore.FFI.loadChartIntoSessionFromJson handle arg
  | "unload" => LnmaiCore.FFI.unloadChartFromSession handle
  | "chart" => LnmaiCore.FFI.getLoweredChartJsonByHandle handle
  | "state" => LnmaiCore.FFI.getGameStateJsonByHandle handle
  | "stepHandle" => LnmaiCore.FFI.stepGameStateHandle handle batch
  | "stepLight" => LnmaiCore.FFI.stepGameStateHandleLight handle batch
  | "free" => LnmaiCore.FFI.freeGameStateHandle handle
  | _ => pure "null"

private def nativeRequest (json : Json) : IO String := do
  let handle := (json.getObjValAs? Nat "handle").toOption.getD 0
  let operation := (json.getObjValAs? Nat "operation").toOption.getD 0
  let time := (json.getObjValAs? Int "time").toOption.getD 0
  let level := (json.getObjValAs? Nat "levelIndex").toOption.getD 1
  let content := (json.getObjValAs? String "content").toOption.getD ""
  let events := (json.getObjValAs? (List LnmaiCore.InputModel.TimedInputEvent) "events").toOption.getD []
  let words := events.flatMap fun event =>
    let time := UInt64.ofInt event.at.toMicros
    match event with
    | .buttonClick _ zone => [time, 0, zone.toIndex.toUInt64, 0]
    | .buttonHold _ zone down => [time, 1, zone.toIndex.toUInt64, if down then 1 else 0]
    | .sensorClick _ area => [time, 2, (LnmaiCore.SensorArea.all.idxOf area).toUInt64, 0]
    | .sensorHold _ area down => [time, 3, (LnmaiCore.SensorArea.all.idxOf area).toUInt64, if down then 1 else 0]
  let (status, records, error) ← testNativeCall handle.toUInt64 operation.toUInt32
    (UInt64.ofInt time) level.toUInt32 words.toArray content
  pure <| (Json.mkObj [("status", toJson status.toNat), ("records", toJson records), ("error", toJson error)]).compress

private def numericCase (json : Json) : Json := Id.run do
  let op := (json.getObjValAs? String "kind").toOption.getD ""
  let int (key : String) := (json.getObjValAs? Int key).toOption.getD 0
  let nat (key : String) := (json.getObjValAs? Nat key).toOption.getD 0
  let duration (key : String) := LnmaiCore.Duration.fromMicros (int key)
  let grade := (json.getObjValAs? LnmaiCore.JudgeGrade "grade").toOption.getD .Perfect
  let ex := (json.getObjValAs? Bool "ex").toOption.getD false
  match op with
  | "integer" =>
    let a := int "a"
    let b := int "b"
    return Json.arr #[toJson (a + b), toJson (a - b), toJson (a * b),
      toJson (a / b), toJson (-a), toJson a.natAbs,
      toJson (a == b), toJson (decide (a < b)), toJson (decide (a ≤ b)),
      toJson (UInt64.ofInt a).toNat]
  | "natural" =>
    let a := nat "a"
    let b := nat "b"
    return Json.arr #[toJson (a + b), toJson (a - b), toJson (a * b),
      toJson (a / b), toJson (a % b),
      toJson (a == b), toJson (decide (a < b)), toJson (decide (a ≤ b)),
      toJson a.toUInt64.toNat]
  | "rational" =>
    let a : Rat := (int "a" : Rat) / (int "b" : Rat)
    let b : Rat := (int "c" : Rat) / (int "d" : Rat)
    let pair (q : Rat) := Json.arr #[toJson q.num, toJson q.den]
    return Json.arr #[pair a, pair b, pair (a + b), pair (a - b), pair (a * b),
      pair (a / b), toJson (a == b), toJson (decide (a < b)),
      toJson (decide (a ≤ b)), toJson (LnmaiCore.Simai.ratToDecimalString a)]
  | "div" => return toJson ((duration "value").divNat (nat "divisor"))
  | "quantize" => return toJson (LnmaiCore.Time.quantizeRatMicros ((int "num" : Rat) / (int "den" : Rat)))
  | "seconds" => return toJson (LnmaiCore.Time.quantizeSecondsString ((json.getObjValAs? String "text").toOption.getD ""))
  | "tap" => return toJson (LnmaiCore.Judge.judgeTap (duration "diff") ex)
  | "touch" => return toJson (LnmaiCore.Judge.judgeTouch (duration "diff") ex)
  | "slideModern" => return toJson (LnmaiCore.Judge.judgeSlideModern (duration "diff") (duration "stay") ex)
  | "slideClassic" => return toJson (LnmaiCore.Judge.judgeSlideClassic (duration "diff"))
  | "hold" => return toJson (LnmaiCore.Judge.judgeHoldEnd grade (duration "diff") (duration "length") (duration "ignore") (duration "release"))
  | "holdClassic" => return toJson (LnmaiCore.Judge.judgeHoldClassicEnd grade
      (LnmaiCore.TimePoint.fromMicros (int "timing")) (duration "length")
      (LnmaiCore.TimePoint.fromMicros (int "release")))
  | "tooLate" => return toJson (LnmaiCore.Judge.isTooLateSlide (duration "diff") (duration "offset"))
  | "fastLate" => return toJson (LnmaiCore.Score.countFastLate grade (duration "diff")
      ((json.getObjValAs? LnmaiCore.JudgeDisplayOption "display").toOption.getD .All))
  | "break" => return toJson (LnmaiCore.Score.scoreBreak grade (nat "multiple"))
  | "nonbreak" => return toJson (LnmaiCore.Score.scoreNonBreak (nat "base") grade (nat "multiple"))
  | "combo" =>
    let r := LnmaiCore.Score.updateCombo (nat "combo") (nat "pCombo") (nat "cPCombo")
      (int "dxScoreLost") grade (nat "multiple")
    return Json.arr #[toJson r.combo, toJson r.pCombo, toJson r.cPCombo, toJson r.dXScoreLost]
  | "rank" => return toJson (LnmaiCore.Score.dxScoreRank (nat "achieved") (nat "max"))
  | "acc" =>
    let r := LnmaiCore.Score.computeAccRates (nat "totalBase") (nat "currentBase") (nat "lostBase")
      (nat "totalExtra") (nat "currentExtra") (nat "lostExtra") (nat "classicExtra")
    return toJson ([r.classicAccPlus, r.classicAccMinus, r.dxAccMinus101, r.dxAccMinus100, r.dxAccPlus].map
      (fun q => Json.arr #[toJson q.num, toJson q.den]))
  | _ => return Json.null

private def settings (state : LnmaiCore.InputModel.GameState) (json : Json) : LnmaiCore.InputModel.GameState :=
  { state with
    judgeStyle := (json.getObjValAs? LnmaiCore.JudgeStyle "judgeStyle").toOption.getD state.judgeStyle
    touchPanelOffset := (json.getObjValAs? LnmaiCore.Duration "touchPanelOffset").toOption.getD state.touchPanelOffset
    subdivideSlideJudgeGrade := (json.getObjValAs? Bool "subdivideSlideJudgeGrade").toOption.getD state.subdivideSlideJudgeGrade
    noteFastLateDisplay := (json.getObjValAs? LnmaiCore.JudgeDisplayOption "noteFastLateDisplay").toOption.getD state.noteFastLateDisplay
    breakFastLateDisplay := (json.getObjValAs? LnmaiCore.JudgeDisplayOption "breakFastLateDisplay").toOption.getD state.breakFastLateDisplay
    displayHoldHeadJudgeResult := (json.getObjValAs? Bool "displayHoldHeadJudgeResult").toOption.getD state.displayHoldHeadJudgeResult
    buttonRingForTouch := (json.getObjValAs? Bool "buttonRingForTouch").toOption.getD state.buttonRingForTouch }

private def query (handle : UInt64) (op : String) (json : Json) : IO String := do
  LnmaiCore.FFI.runtimeRegistryMutex.atomically do
    let registry ← get
    let some (.loaded chart state) := registry.sessions.get? handle
      | return (Json.mkObj [("ok", toJson false)]).compress
    if op == "autoplay" then return ok (toJson (LnmaiCore.autoplayPlanFromChart chart))
    if op == "tactic" then return ok (toJson (LnmaiCore.defaultTacticFromChart chart))
    if op == "settings" then
      let next := settings state json
      set { registry with sessions := registry.sessions.insert handle (.loaded chart next) }
      return ok (toJson next)
    let some (initial, tactic) := registry.seekOrigins.get? handle
      | return (Json.mkObj [("ok", toJson false)]).compress
    let target := (json.getObjValAs? LnmaiCore.TimePoint "target").toOption.getD LnmaiCore.TimePoint.zero
    let events := (json.getObjValAs? (List LnmaiCore.InputModel.TimedInputEvent) "events").toOption.getD tactic
    let keepHeld := (json.getObjValAs? Bool "keepHeld").toOption.getD false
    let (next, render) := LnmaiCore.RuntimeSeek.seek chart initial state target events keepHeld
    set { registry with sessions := registry.sessions.insert handle (.loaded chart next) }
    return ok (Json.mkObj [("state", toJson next), ("renderCommands", toJson render)])

structure Request where
  op : String
  content : String := ""
  levelIndex : UInt32 := 1
  batch : String := "{}"

private def parseRequest (json : Json) : Except String Request := do
  let op ← json.getObjValAs? String "op"
  let content := (json.getObjValAs? String "content").toOption.getD ""
  let levelNat := (json.getObjValAs? Nat "levelIndex").toOption.getD 1
  let batch :=
    match json.getObjVal? "batch" with
    | .ok b => b.compress
    | .error _ => "{}"
  pure { op := op, content := content, levelIndex := levelNat.toUInt32, batch := batch }

private def errJson (code message : String) : String :=
  "{\"ok\":false,\"error\":{\"code\":" ++ (Json.str code).compress ++ ",\"message\":" ++ (Json.str message).compress ++ "}}"

private def handleOf (created : String) : UInt64 :=
  match Json.parse created with
  | .ok j =>
      match j.getObjVal? "result" with
      | .ok r =>
          let n :=
            match r.getObjValAs? Nat "handle" with
            | .ok n => n
            | .error _ =>
                match r.getObjValAs? String "handle" with
                | .ok s => s.toNat?.getD 0
                | .error _ => 0
          n.toUInt64
      | .error _ => 0
  | .error _ => 0

private def dispatch (hRef : IO.Ref (Option UInt64)) (line : String) : IO String := do
  match Json.parse line with
  | .error err => pure (errJson "invalid_request" err)
  | .ok json =>
    match parseRequest json with
    | .error err => pure (errJson "invalid_request" err)
    | .ok req =>
      match req.op with
      | "json" => jsonRequest json
      | "numeric" => pure <| ok (Json.arr (((json.getObjVal? "cases").toOption.getD (Json.arr #[])).getArr?.toOption.getD #[] |>.map numericCase))
      | "native" => nativeRequest json
      | "parseNat" =>
          let level := (json.getObjValAs? Nat "levelIndex").toOption.getD 1
          pure <| match LnmaiCore.Simai.frontendLoweredChart req.content level with
          | .ok chart => ok (toJson chart)
          | .error error => errJson "parse_error" error.message
      | "parse" =>
          let mode := (json.getObjValAs? String "mode").toOption.getD "lowered"
          let classic := (json.getObjValAs? Bool "isClassic").toOption.getD false
          pure <| match mode with
          | "frontend" => LnmaiCore.FFI.parseFrontendChartModeJson req.content req.levelIndex classic
          | "inspection" => LnmaiCore.FFI.parseFrontendInspectionChartJson req.content req.levelIndex
          | "semantic" => LnmaiCore.FFI.parseFrontendSemanticChartJson req.content req.levelIndex
          | "normalized" => LnmaiCore.FFI.parseNormalizedChartJson req.content req.levelIndex
          | _ => LnmaiCore.FFI.parseLoweredChartModeJson req.content req.levelIndex classic
      | "load" | "loadChart" => do
          match (← hRef.get) with
          | some old => discard <| LnmaiCore.FFI.freeGameStateHandle old
          | none => pure ()
          let created ← LnmaiCore.FFI.createEmptySessionHandle
          let handle := handleOf created
          hRef.set (some handle)
          if req.op == "loadChart" then
            LnmaiCore.FFI.loadChartIntoSessionFromJson handle
              ((json.getObjVal? "chart").toOption.getD Json.null).compress
          else if (json.getObjValAs? Bool "isClassic").toOption.getD false then
            match LnmaiCore.Simai.frontendLoweredChartWithMode req.content req.levelIndex.toNat true with
            | .ok chart => LnmaiCore.FFI.loadChartIntoSessionFromJson handle (toJson chart).compress
            | .error err => pure (errJson "parse_error" err.message)
          else LnmaiCore.FFI.loadChartIntoSessionFromText handle req.content req.levelIndex
      | "autoplay" | "tactic" | "seek" | "settings" => do
          match (← hRef.get) with
          | some handle => query handle req.op json
          | none => pure (errJson "invalid_session_state" "no session")
      | "step" => do
          match (← hRef.get) with
          | some handle => LnmaiCore.FFI.stepGameStateHandleLight handle req.batch
          | none => pure (errJson "invalid_session_state" "no session")
      | "lowered" => do
          match (← hRef.get) with
          | some handle => LnmaiCore.FFI.getLoweredChartJsonByHandle handle
          | none => pure (errJson "invalid_session_state" "no session")
      | "state" => do
          match (← hRef.get) with
          | some handle => LnmaiCore.FFI.getGameStateJsonByHandle handle
          | none => pure (errJson "invalid_session_state" "no session")
      | "free" => do
          match (← hRef.get) with
          | some handle => do
              hRef.set none
              LnmaiCore.FFI.freeGameStateHandle handle
          | none => pure (errJson "invalid_runtime_handle" "no session")
      | other => pure (errJson "unknown_op" other)

partial def loop (hRef : IO.Ref (Option UInt64)) : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let line ← stdin.getLine
  if line.isEmpty then
    pure ()
  else do
    let trimmed := line.trimAscii.toString
    if trimmed.isEmpty then
      loop hRef
    else do
      let resp ← dispatch hRef trimmed
      stdout.putStrLn resp
      stdout.flush
      loop hRef

def main : IO Unit := do
  let hRef ← IO.mkRef (none : Option UInt64)
  loop hRef
