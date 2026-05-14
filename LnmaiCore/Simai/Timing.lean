import LnmaiCore.Simai.Syntax
import LnmaiCore.Time

namespace LnmaiCore.Simai

def trim (s : String) : String := s.trimAscii.toString

def parseNatString? (s : String) : Option Nat :=
  let t := trim s
  if t = "" then none else t.toNat?

def parseRatString? (s : String) : Option Rat :=
  let t := trim s
  if t = "" then
    none
  else
    let negative := t.startsWith "-"
    let unsigned := if negative then (t.drop 1).toString else t
    match unsigned.splitOn "." with
    | [whole] =>
        match whole.toNat? with
        | some n =>
            let value : Rat := Int.ofNat n
            some <| if negative then -value else value
        | none => none
    | [whole, frac] =>
        match whole.toNat? with
        | none => none
        | some wholeNat =>
            if frac.toList.all Char.isDigit then
              let fracDigits := frac.length
              let fracNat := frac.toNat?.getD 0
              let denom : Rat := Int.ofNat ((10 : Nat) ^ fracDigits)
              let value : Rat := Int.ofNat wholeNat + Int.ofNat fracNat / denom
              some <| if negative then -value else value
            else
              none
    | _ => none

def parseRatDef (s : String) (fallback : Rat) : Rat :=
  match parseRatString? s with
  | some value => value
  | none => fallback

def parseNatDef (s : String) (fallback : Nat) : Nat :=
  match parseNatString? s with
  | some value => value
  | none => fallback

def parseDurationString? (s : String) : Option Duration :=
  Time.parseSecondsString? s

def parseSecondsRatString? (s : String) : Option Rat :=
  parseRatString? s

def measureDurSec (bpm : Rat) : Duration :=
  Time.durationFromRatMicros (Time.bpmMeasureMicrosRat bpm)

def beatSec (bpm : Rat) : Duration :=
  Time.durationFromRatMicros (Time.bpmBeatMicrosRat bpm)

def extractBracketContents (token : String) : List String :=
  let rec loop (chars : List Char) (inside : Bool) (current : List Char) (acc : List String) : List String :=
    match chars with
    | [] => acc.reverse
    | '[' :: rest =>
        if inside then
          loop rest inside ('[' :: current) acc
        else
          loop rest true [] acc
    | ']' :: rest =>
        if inside then
          loop rest false [] (String.ofList current :: acc)
        else
          loop rest inside current acc
    | c :: rest =>
        if inside then loop rest inside (current.concat c) acc else loop rest inside current acc
  loop token.toList false [] []

def splitHash2 (s : String) : Option (String × String × String) :=
  match s.splitOn "#" with
  | [a, b, c] => some (a, b, c)
  | _ => none

def splitHash1 (s : String) : Option (String × String) :=
  match s.splitOn "#" with
  | [a, b] => some (a, b)
  | _ => none

def parseNdDuration (bpm : Rat) (timing : String) : Option Duration :=
  match timing.splitOn ":" with
  | [numStr, denStr] =>
      match parseNatString? numStr, parseNatString? denStr with
      | some beatDivision, some numBeats =>
          if beatDivision = 0 then none
          else
            let noteMicros := Time.bpmMeasureMicrosRat bpm * Int.ofNat numBeats / Int.ofNat beatDivision
            some <| Time.durationFromRatMicros noteMicros
      | _, _ => none
  | _ => none

def parseNdDurationExact (bpm : Rat) (timing : String) : Option Duration :=
  parseNdDuration bpm timing

def parseDurationInner (currentBpm : Rat) (inner : String) : Option Duration :=
  if inner.startsWith "#" && inner.count '#' = 1 && !inner.contains ':' then
    parseDurationString? ((inner.drop 1).toString)
  else if inner.count '#' = 2 then
    match splitHash2 inner with
    | some (_, _, durationPart) => parseDurationString? durationPart
    | none => none
  else if inner.count '#' = 1 then
    match splitHash1 inner with
    | some (customBpmStr, timingStr) =>
        let segBpm :=
          match parseRatString? customBpmStr with
          | some v => if v > 0 then v else currentBpm
          | none => currentBpm
        match parseNdDurationExact segBpm timingStr with
        | some duration => some duration
        | none => parseDurationString? timingStr
    | none => none
  else
    match parseNdDurationExact currentBpm inner with
    | some duration => some duration
    | none =>
        if !inner.startsWith "#" then parseDurationString? inner else none

def parseDurationSpec (bpm : Rat) (token : String) : Option Duration :=
  let contents := extractBracketContents token
  contents.foldl
    (fun acc inner =>
      match acc, parseDurationInner bpm inner with
      | some sum, some duration => some (sum + duration)
      | some sum, none => some sum
      | none, some duration => some duration
      | none, none => none)
    none

def parseStarWaitSpec (bpm : Rat) (token : String) : Option Duration :=
  match extractBracketContents token |>.head? with
  | none => some (beatSec bpm)
  | some inner =>
      if inner.count '#' = 2 then
        match splitHash2 inner with
        | some (waitPart, _, _) =>
            if waitPart = "" then some (beatSec bpm)
            else
              match parseSecondsRatString? waitPart with
              | some waitSeconds => some (Time.durationFromSecondsRat waitSeconds)
              | none => parseDurationString? waitPart
        | none => some (beatSec bpm)
      else if inner.count '#' = 1 then
        match splitHash1 inner with
        | some (waitBpmStr, _) =>
            match parseRatString? waitBpmStr with
            | some waitBpm => if waitBpm > 0 then some (beatSec waitBpm) else some (beatSec bpm)
            | none => some (beatSec bpm)
        | none => some (beatSec bpm)
      else
        some (beatSec bpm)

def noteTimingIncrement (bpm : Rat) (divisor : Nat) : Duration :=
  if bpm > 0 && divisor > 0 then
    Time.durationFromRatMicros (Time.bpmMeasureMicrosRat bpm / Int.ofNat divisor)
  else
    Duration.zero

def pseudoIncrement (bpm : Rat) : Duration :=
  if bpm > 0 then
    Time.durationFromRatMicros (Time.bpmBeatMicrosRat bpm / 32)
  else
    Duration.fromMicros 1000

end LnmaiCore.Simai
