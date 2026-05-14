import LnmaiCore.Simai.Syntax
import LnmaiCore.Time

namespace LnmaiCore.Simai

def trim (s : String) : String := s.trimAscii.toString

def parseNatString? (s : String) : Option Nat :=
  let t := trim s
  if t = "" then none else t.toNat?

def parseFloatString? (s : String) : Option Float :=
  let t := trim s
  if t = "" then
    none
  else
    match t.splitOn "." with
    | [whole] =>
        match whole.toInt? with
        | some n => some <| if n < 0 then - Float.ofNat (Int.natAbs n) else Float.ofNat (Int.natAbs n)
        | none => none
    | [whole, frac] =>
        if frac = "" then
          match whole.toInt? with
          | some n => some <| if n < 0 then - Float.ofNat (Int.natAbs n) else Float.ofNat (Int.natAbs n)
          | none => none
        else
          match whole.toInt?, frac.toNat? with
          | some wn, some fn =>
              let sign : Float := if wn < 0 then -1.0 else 1.0
              let absWhole : Float := Int.natAbs wn |>.toFloat
              let denom := (10 : Nat) ^ frac.length
              some <| sign * (absWhole + (Float.ofNat fn / Float.ofNat denom))
          | _, _ => none
    | _ => none

def parseFloatDef (s : String) (fallback : Float) : Float :=
  match parseFloatString? s with
  | some value => value
  | none => fallback

def parseNatDef (s : String) (fallback : Nat) : Nat :=
  match parseNatString? s with
  | some value => value
  | none => fallback

def parseDurationString? (s : String) : Option Duration :=
  Time.parseSecondsString? s

def measureDurSec (bpm : Float) : Float :=
  if bpm > 0.0 then (60.0 / bpm) * 4.0 else 0.0

def beatSec (bpm : Float) : Float :=
  if bpm > 0.0 then 60.0 / bpm else 0.001

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

def parseNdDuration (bpm : Float) (timing : String) : Option Float :=
  match timing.splitOn ":" with
  | [numStr, denStr] =>
      match parseNatString? numStr, parseNatString? denStr with
      | some beatDivision, some numBeats =>
          if beatDivision = 0 then none
          else some <| beatSec bpm * (4.0 / Float.ofNat beatDivision) * Float.ofNat numBeats
      | _, _ => none
  | _ => none

def parseNdDurationExact (bpm : Float) (timing : String) : Option Duration :=
  match parseNdDuration bpm timing with
  | some seconds => Time.parseSecondsString? s!"{seconds}"
  | none => none

def parseDurationInner (currentBpm : Float) (inner : String) : Option Duration :=
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
          match parseFloatString? customBpmStr with
          | some v => if v > 0.0 then v else currentBpm
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

def parseDurationSpec (bpm : Float) (token : String) : Option Duration :=
  let contents := extractBracketContents token
  contents.foldl
    (fun acc inner =>
      match acc, parseDurationInner bpm inner with
      | some sum, some duration => some (sum + duration)
      | some sum, none => some sum
      | none, some duration => some duration
      | none, none => none)
    none

def parseStarWaitSpec (bpm : Float) (token : String) : Option Duration :=
  match extractBracketContents token |>.head? with
  | none => some (Time.beatDurationMicros bpm)
  | some inner =>
      if inner.count '#' = 2 then
        match splitHash2 inner with
        | some (waitPart, _, _) =>
            if waitPart = "" then some (Time.beatDurationMicros bpm) else parseDurationString? waitPart
        | none => some (Time.beatDurationMicros bpm)
      else if inner.count '#' = 1 then
        match splitHash1 inner with
        | some (waitBpmStr, _) =>
            match parseFloatString? waitBpmStr with
            | some waitBpm => if waitBpm > 0.0 then some (Time.beatDurationMicros waitBpm) else some (Time.beatDurationMicros bpm)
            | none => some (Time.beatDurationMicros bpm)
        | none => some (Time.beatDurationMicros bpm)
      else
        some (Time.beatDurationMicros bpm)

def noteTimingIncrement (bpm : Float) (divisor : Nat) : Duration :=
  if bpm > 0.0 && divisor > 0 then
    Duration.ofInt ((Time.measureDurationMicros bpm).toInt / Int.ofNat divisor)
  else
    Duration.zero

def pseudoIncrement (bpm : Float) : Duration :=
  if bpm > 0.0 then
    Duration.ofInt ((Time.beatDurationMicros bpm).toInt / 32)
  else
    Duration.fromMicros 1000

end LnmaiCore.Simai
