import LnmaiCore.ChartLoader
import LnmaiCore.Simai.Parser

open Lean

namespace LnmaiCore.Simai

inductive RawNoteKind where
  | tap
  | hold
  | slide
  | touch
  | touchHold
  | rest
  | unknown
deriving DecidableEq, Repr, Inhabited, BEq

structure MaidataMetadata where
  fields : List (String × String) := []
deriving Inhabited, Repr

structure MaidataChartBlock where
  levelIndex : Nat
  rawBody : String
deriving Inhabited, Repr

structure MaidataFile where
  metadata : MaidataMetadata := {}
  charts : List MaidataChartBlock := []
deriving Inhabited, Repr

structure RawNoteToken where
  rawText : String
  kind : RawNoteKind
  timingSec : Float
  bpm : Float
  divisor : Nat
  lane : Option Nat := none
  sensorPos : Option Nat := none
  lengthSec : Option Float := none
  isBreak : Bool := false
  isEX : Bool := false
deriving Inhabited, Repr

structure ParsedMaidataChart where
  metadata : MaidataMetadata
  chart : MaidataChartBlock
  tokens : List RawNoteToken
  slideNotes : List SlideNoteSemantics
  lowered : ChartLoader.ChartSpec
deriving Inhabited, Repr

private def trim (s : String) : String := s.trimAscii.toString

private def startsWithAmp (s : String) : Bool :=
  match s.toList with
  | '&' :: _ => true
  | _ => false

private def parseKeyValueLine (line : String) : Option (String × String) :=
  match line.splitOn "=" with
  | key :: rest => some (trim key, trim (String.intercalate "=" rest))
  | _ => none

private partial def collectChartBody (lines : List String) (acc : List String) : List String × List String :=
  match lines with
  | [] => (acc.reverse, [])
  | line :: rest =>
      if startsWithAmp line then
        (acc.reverse, line :: rest)
      else
        collectChartBody rest (line :: acc)

private partial def parseMaidataLines (lines : List String) (fields : List (String × String)) (charts : List MaidataChartBlock) : MaidataFile :=
  match lines with
  | [] => { metadata := { fields := fields.reverse }, charts := charts.reverse }
  | line :: rest =>
      if trim line = "" then
        parseMaidataLines rest fields charts
      else if startsWithAmp line then
        match parseKeyValueLine line with
        | some (key, value) =>
            if key.startsWith "&inote_" then
              let levelIndex := ((key.drop 7).toString.toNat?).getD 0
              let (bodyLines, remaining) := collectChartBody rest [value]
              let body := String.intercalate "\n" bodyLines
              parseMaidataLines remaining fields ({ levelIndex := levelIndex, rawBody := body } :: charts)
            else
              parseMaidataLines rest ((key, value) :: fields) charts
        | none => parseMaidataLines rest fields charts
      else
        parseMaidataLines rest fields charts

private def metadataField (md : MaidataMetadata) (key : String) : Option String :=
  match md.fields.find? (fun pair => pair.1 = key) with
  | some (_, value) => some value
  | none => none

private def parseFloatDef (s : String) (fallback : Float) : Float :=
  let t := trim s
  if t = "128" then 128.0
  else if t = "120" then 120.0
  else if t = "180" then 180.0
  else fallback

private def parseNatDef (s : String) (fallback : Nat) : Nat :=
  match s.toNat? with
  | some v => v
  | none => fallback

private def measureDurSec (bpm : Float) : Float :=
  (60.0 / bpm) * 4.0

private def parseDurationSpec (bpm : Float) (token : String) : Option Float :=
  match token.splitOn "[" with
  | _ :: rest =>
      match rest with
      | inside :: _ =>
          let inner := match inside.splitOn "]" with | x :: _ => x | [] => inside
          match inner.splitOn ":" with
          | a :: b :: _ =>
              let denom := parseFloatDef a 1.0
              let numer := parseFloatDef b 1.0
              some <| measureDurSec bpm * (numer / denom)
          | _ => none
      | [] => none
  | _ => none

private partial def removeLeadingDirectives (bpm : Float) (divisor : Nat) (text : String) : Float × Nat × String :=
  let t := trim text
  if t.startsWith "(" then
    let after := (t.drop 1).toString
    let parts := after.splitOn ")"
    match parts with
    | inside :: rest =>
        let bpm' := parseFloatDef inside bpm
        let remain := String.intercalate ")" rest
        removeLeadingDirectives bpm' divisor remain
    | _ => (bpm, divisor, t)
  else if t.startsWith "{" then
    let after := (t.drop 1).toString
    let parts := after.splitOn "}"
    match parts with
    | inside :: rest =>
        let divisor' := parseNatDef inside divisor
        let remain := String.intercalate "}" rest
        removeLeadingDirectives bpm divisor' remain
    | _ => (bpm, divisor, t)
  else
    (bpm, divisor, t)

private def firstDigit? (s : String) : Option Nat :=
  s.toList.findSome? digitToNat?

private def leadingDigit? (s : String) : Option Nat :=
  match s.toList with
  | c :: _ => digitToNat? c
  | _ => none

private def touchAreaToSensorPos? (s : String) : Option Nat :=
  let cs := s.toList
  match cs with
  | area :: rest =>
      match area, rest with
      | 'C', _ => some 16
      | 'A', digit :: _ => digitToNat? digit |>.map (fun n => n - 1)
      | 'D', digit :: _ => digitToNat? digit |>.map (fun n => 7 + n)
      | 'E', digit :: _ => digitToNat? digit |>.map (fun n => 16 + n)
      | 'B', digit :: _ => digitToNat? digit |>.map (fun n => 24 + n)
      | _ , _ => none
  | _ => none

private def stripPrefixes (token : String) : String :=
  let t := trim token
  if t = "" then t
  else if t.startsWith "{" then
    match t.splitOn "}" with
    | _ :: rest => trim (String.intercalate "}" rest)
    | _ => t
  else
    t

private def inferKind (token : String) : RawNoteKind :=
  let t := stripPrefixes token
  if t = "" then .rest
  else if leadingDigit? t |>.isSome then
    if t.contains '-' || t.contains '>' || t.contains '<' || t.contains '^' || t.contains 'v' || t.contains 'V' || t.contains 'p' || t.contains 'q' || t.contains 's' || t.contains 'z' || t.contains 'w' then .slide
    else if t.contains 'h' then .hold
    else .tap
  else
  match t.toList with
    | area :: _ =>
        if area = 'A' || area = 'B' || area = 'C' || area = 'D' || area = 'E' then
          if t.contains 'h' then .touchHold else .touch
        else .unknown
    | _ => .unknown

private def splitEntryTokens (entry : String) : List String :=
  (entry.splitOn "/").map stripPrefixes |>.filter (fun t => t ≠ "")

private def mkRawToken (timingSec bpm : Float) (divisor : Nat) (token : String) : RawNoteToken :=
  let t := stripPrefixes token
  let kind := inferKind t
  let lane := leadingDigit? t |>.map (fun n => n - 1)
  let sensorPos := touchAreaToSensorPos? t
  let lengthSec := parseDurationSpec bpm t
  let isBreak := t.contains 'b'
  let isEX := t.contains 'x'
  { rawText := t
  , kind := kind
  , timingSec := timingSec
  , bpm := bpm
  , divisor := divisor
  , lane := lane
  , sensorPos := sensorPos
  , lengthSec := lengthSec
  , isBreak := isBreak
  , isEX := isEX }

private def advanceTime (currentSec bpm : Float) (divisor : Nat) : Float :=
  currentSec + measureDurSec bpm / Float.ofNat (max divisor 1)

private partial def parseEntries (entries : List String) (currentSec bpm : Float) (divisor : Nat) (acc : List RawNoteToken) : List RawNoteToken :=
  match entries with
  | [] => acc.reverse
  | entry :: rest =>
      let (bpm', divisor', body) := removeLeadingDirectives bpm divisor entry
      let tokens := splitEntryTokens body
      let newTokens := tokens.map (mkRawToken currentSec bpm' divisor')
      let nextSec := advanceTime currentSec bpm' divisor'
      parseEntries rest nextSec bpm' divisor' (newTokens.reverse ++ acc)

private def lowerSlideToken (noteIndex : Nat) (token : RawNoteToken) : Option (ChartLoader.SlideChartNote × SlideNoteSemantics) :=
  match token.lane with
  | some lane =>
      let endPos := firstDigit? ((token.rawText.drop 1).toString) |>.getD (lane + 1)
      match parseSlideNote token.rawText (lane + 1) endPos with
      | .ok parsed =>
          let shapeKey := shapeKey parsed.shape
          let isWifi := parsed.shape.kind = SlideKind.wifi
          let lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1))
          let slide : ChartLoader.SlideChartNote :=
            { timingSec := token.timingSec
            , lane := lane
            , lengthSec := lengthSec
            , startTimingSec := token.timingSec
            , slideKind := if isWifi then LnmaiCore.SlideKind.Wifi else LnmaiCore.SlideKind.Single
            , isClassic := false
            , trackCount := if isWifi then 3 else 1
            , judgeAtSec := some (token.timingSec + lengthSec)
            , isBreak := token.isBreak
            , isEX := token.isEX
            , noteIndex := noteIndex
            , judgeQueues := []
            , simaiRawText := some token.rawText
            , simaiShapeKey := some shapeKey
            , simaiIsJustRight := some parsed.isJustRight }
          some (slide, parsed)
      | .error _ => none
  | none => none

private def lowerRawTokens (tokens : List RawNoteToken) : ChartLoader.ChartSpec × List SlideNoteSemantics :=
  let (_, taps, holds, touches, touchHolds, slides, slideSemantics) :=
    tokens.foldl
      (fun (state : Nat × List ChartLoader.TapChartNote × List ChartLoader.HoldChartNote × List ChartLoader.TouchChartNote × List ChartLoader.HoldChartNote × List ChartLoader.SlideChartNote × List SlideNoteSemantics) token =>
        let (noteIndex, taps, holds, touches, touchHolds, slides, slideSemantics) := state
        match token.kind with
        | .tap =>
            match token.lane with
            | some lane =>
                (noteIndex + 1,
                 { timingSec := token.timingSec, lane := lane, isBreak := token.isBreak, isEX := token.isEX, noteIndex := noteIndex } :: taps,
                 holds, touches, touchHolds, slides, slideSemantics)
            | none => state
        | .hold =>
            match token.lane with
            | some lane =>
                (noteIndex + 1,
                 taps,
                 { timingSec := token.timingSec, lane := lane, lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1)), isBreak := token.isBreak, isEX := token.isEX, noteIndex := noteIndex } :: holds,
                 touches, touchHolds, slides, slideSemantics)
            | none => state
        | .touch =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds,
                 { timingSec := token.timingSec, sensorPos := sensorPos, isBreak := token.isBreak, noteIndex := noteIndex } :: touches,
                 touchHolds, slides, slideSemantics)
            | none => state
        | .touchHold =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds, touches,
                 { timingSec := token.timingSec, lane := sensorPos, lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1)), isBreak := token.isBreak, isEX := token.isEX, isTouch := true, noteIndex := noteIndex } :: touchHolds,
                 slides, slideSemantics)
            | none => state
        | .slide =>
            match lowerSlideToken noteIndex token with
            | some (slide, parsed) =>
                (noteIndex + 1, taps, holds, touches, touchHolds, slide :: slides, parsed :: slideSemantics)
            | none => state
        | _ => state)
      (1, [], [], [], [], [], [])
  ({ taps := taps.reverse, holds := holds.reverse, touches := touches.reverse, touchHolds := touchHolds.reverse, slides := slides.reverse, slideSkipping := some true }, slideSemantics.reverse)


def parseMaidata (content : String) : Except ParseError MaidataFile :=
  Except.ok <| parseMaidataLines (content.splitOn "\n") [] []


def lowerChartBlock (file : MaidataFile) (block : MaidataChartBlock) : Except ParseError ParsedMaidataChart := do
  let bpm := parseFloatDef ((metadataField file.metadata "&wholebpm").getD "120") 120.0
  let entries := block.rawBody.splitOn ","
  let tokens := parseEntries entries 0.0 bpm 4 []
  let (lowered, slideNotes) := lowerRawTokens tokens
  pure { metadata := file.metadata, chart := block, tokens := tokens, slideNotes := slideNotes, lowered := lowered }


def lowerChartByLevel (file : MaidataFile) (levelIndex : Nat) : Except ParseError ParsedMaidataChart := do
  match file.charts.find? (fun block => block.levelIndex = levelIndex) with
  | some block => lowerChartBlock file block
  | none => Except.error { kind := .invalidSyntax, rawText := "", message := s!"missing inote block {levelIndex}" }


def parseAndLowerMaidata (content : String) (levelIndex : Nat) : Except ParseError ParsedMaidataChart := do
  let file ← parseMaidata content
  lowerChartByLevel file levelIndex

end LnmaiCore.Simai
