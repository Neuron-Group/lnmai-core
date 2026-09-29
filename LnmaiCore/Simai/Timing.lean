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
          -- Preserve the existing nested-bracket behavior on malformed input.
          loop rest inside (current.concat '[') acc
        else
          loop rest true [] acc
    | ']' :: rest =>
        if inside then
          loop rest false [] (String.ofList current.reverse :: acc)
        else
          loop rest inside current acc
    | c :: rest =>
        if inside then loop rest inside (c :: current) acc else loop rest inside current acc
  loop token.toList false [] []

def splitHash2 (s : String) : Option (String × String × String) :=
  match s.splitOn "#" with
  | [a, b, c] => some (a, b, c)
  | _ => none

def splitHash1 (s : String) : Option (String × String) :=
  match s.splitOn "#" with
  | [a, b] => some (a, b)
  | _ => none

private def parseNdMicros (bpm : Rat) (timing : String) : Option Rat :=
  match timing.splitOn ":" with
  | [numStr, denStr] =>
      match parseNatString? numStr, parseNatString? denStr with
      | some beatDivision, some numBeats =>
          if beatDivision = 0 then none
          else some (Time.bpmMeasureMicrosRat bpm * Int.ofNat numBeats / Int.ofNat beatDivision)
      | _, _ => none
  | _ => none

def parseNdDuration (bpm : Rat) (timing : String) : Option Duration :=
  (parseNdMicros bpm timing).map Time.durationFromRatMicros

def parseNdDurationExact (bpm : Rat) (timing : String) : Option Duration :=
  parseNdDuration bpm timing

private def parseSecondsMicros (text : String) : Option Rat :=
  (parseSecondsRatString? text).map (· * Time.microsPerSecond)

private def parseRatioOrSecondsMicros (bpm : Rat) (text : String) : Option Rat :=
  (parseNdMicros bpm text).orElse (fun _ => parseSecondsMicros text)

private def durationBpm (currentBpm : Rat) (text : String) : Rat :=
  match parseRatString? text with
  | some value => if value > 0 then value else currentBpm
  | none => currentBpm

private def parseDurationInnerMicros (currentBpm : Rat) (inner : String) : Option Rat :=
  match inner.splitOn "#" with
  | [timing] => parseRatioOrSecondsMicros currentBpm timing
  | ["", seconds] => parseSecondsMicros seconds
  | [customBpm, timing] =>
      parseRatioOrSecondsMicros (durationBpm currentBpm customBpm) timing
  | [_, _, timing] => parseRatioOrSecondsMicros currentBpm timing
  | [_, _, customBpm, timing] => parseNdMicros (durationBpm currentBpm customBpm) timing
  | _ => none

def parseDurationInner (currentBpm : Rat) (inner : String) : Option Duration :=
  (parseDurationInnerMicros currentBpm inner).map Time.durationFromRatMicros

def parseDurationSpec (bpm : Rat) (token : String) : Option Duration :=
  -- Sum exact durations before rounding, including continuous chains.
  let total := (extractBracketContents token).foldl
    (fun acc inner =>
      match acc, parseDurationInnerMicros bpm inner with
      | some sum, some duration => some (sum + duration)
      | some sum, none => some sum
      | none, some duration => some duration
      | none, none => none)
    none
  total.map Time.durationFromRatMicros

def parseStarWaitSpec (bpm : Rat) (token : String) : Option Duration :=
  -- MajSimai uses the first custom BPM or wait across all brackets.
  let custom := (extractBracketContents token).findSome? fun inner =>
    match inner.splitOn "#" with
    | [customBpm, _] =>
        (parseRatString? customBpm).map fun value =>
          if value > 0 then beatSec value else beatSec bpm
    | [wait, _, _] | [wait, _, _, _] =>
        (parseSecondsRatString? wait).map Time.durationFromSecondsRat
    | _ => none
  some (custom.getD (beatSec bpm))

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
