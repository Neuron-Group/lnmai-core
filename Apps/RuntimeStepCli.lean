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
import Lean.Data.Json

open Lean

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
      | "load" => do
          match (← hRef.get) with
          | some old => discard <| LnmaiCore.FFI.freeGameStateHandle old
          | none => pure ()
          let created ← LnmaiCore.FFI.createEmptySessionHandle
          let handle := handleOf created
          hRef.set (some handle)
          LnmaiCore.FFI.loadChartIntoSessionFromText handle req.content req.levelIndex
      | "step" => do
          match (← hRef.get) with
          | some handle => LnmaiCore.FFI.stepGameStateHandleLight handle req.batch
          | none => pure (errJson "invalid_session_state" "no session")
      | "lowered" => do
          match (← hRef.get) with
          | some handle => LnmaiCore.FFI.getLoweredChartJsonByHandle handle
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
