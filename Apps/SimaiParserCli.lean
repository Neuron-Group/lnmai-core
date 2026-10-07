import Lean.Data.Json
import LnmaiCore.FFI

open Lean

namespace LnmaiCore.ParserCli

inductive ParseMode where
  | frontend
  | semantic
  | inspection
  | normalized
  | lowered
  deriving Inhabited, Repr

instance : FromJson ParseMode where
  fromJson?
    | Json.str "frontend" => .ok .frontend
    | Json.str "semantic" => .ok .semantic
    | Json.str "inspection" => .ok .inspection
    | Json.str "normalized" => .ok .normalized
    | Json.str "lowered" => .ok .lowered
    | json => .error s!"invalid parse mode: expected string, got {json.compress}"

structure ParseRequest where
  mode : ParseMode := .lowered
  content : String
  levelIndex : UInt32 := 1
  isClassic : Bool := false
  deriving Inhabited, Repr

instance : FromJson ParseRequest where
  fromJson?
    | json@(Json.obj _) => do
        let content ← json.getObjValAs? String "content"
        let mode := (json.getObjValAs? ParseMode "mode").toOption.getD .lowered
        let levelNat := (json.getObjValAs? Nat "levelIndex").toOption.getD 1
        let isClassic ← match json.getObjVal? "isClassic" with
          | .ok value => fromJson? value
          | .error _ => pure false
        pure { mode := mode
             , content := content
             , levelIndex := levelNat.toUInt32
             , isClassic := isClassic }
    | json => .error s!"expected request object, got {json.compress}"

private def errorJson (code message : String) (details : Option Json := none) : String :=
  let fields :=
    [ ("ok", Json.bool false)
    , ("error", Json.mkObj [("code", Json.str code), ("message", Json.str message)]) ]
  (Json.mkObj (fields ++ details.toList.map (fun value => ("details", value)))).compress

private def handleRequest (request : ParseRequest) : String :=
  match Simai.parseFrontendChartResultWithMode
      request.content request.levelIndex.toNat request.isClassic with
  | .error err => errorJson "parse_error" err.message (some (toJson err))
  | .ok result =>
      let payload := match request.mode with
        | .frontend => toJson result
        | .semantic => toJson result.semantic
        | .inspection => toJson result.inspection
        | .normalized => toJson result.semantic.normalized
        | .lowered => toJson result.semantic.lowered
      (Json.mkObj [("ok", Json.bool true), ("result", payload)]).compress

private def handleLine (line : String) : String :=
  match Json.parse line with
  | .error err => errorJson "invalid_request_json" err
  | .ok json =>
      match (fromJson? json : Except String ParseRequest) with
      | .error err => errorJson "invalid_request_json" err
      | .ok request => handleRequest request

private partial def loop : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let line ← stdin.getLine
  if line.isEmpty then
    pure ()
  else
    let trimmed := line.trimAscii.toString
    if !trimmed.isEmpty then
      stdout.putStrLn (handleLine trimmed)
      stdout.flush
    loop

def run : IO Unit := do
  loop

end LnmaiCore.ParserCli

def main : IO Unit :=
  LnmaiCore.ParserCli.run
