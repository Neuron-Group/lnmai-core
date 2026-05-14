import LnmaiCore.Simai.Timing
import LnmaiCore.Simai.Shape
import LnmaiCore.Areas
import LnmaiCore.Time

namespace LnmaiCore.Simai

def firstDigit? (s : String) : Option Nat :=
  s.toList.findSome? digitToNat?

def leadingDigit? (s : String) : Option Nat :=
  match s.toList with
  | c :: _ => digitToNat? c
  | _ => none

def leadingTouchPos? (s : String) : Option Nat :=
  let cs := s.toList
  match cs with
  | area :: rest =>
      match area, rest with
      | 'C', _ => some 8
      | _, digit :: _ => digitToNat? digit
      | _, _ => none
  | _ => none

def touchAreaToSensorArea? (s : String) : Option SensorArea :=
  let cs := s.toList
  match cs with
  | area :: rest =>
      match area, rest with
      | 'C', _ => some .C
      | 'A', digit :: _ => digitToNat? digit >>= (fun n => SensorArea.ofIndex? (n - 1))
      | 'D', digit :: _ => digitToNat? digit >>= (fun n => SensorArea.ofIndex? (7 + n))
      | 'E', digit :: _ => digitToNat? digit >>= (fun n => SensorArea.ofIndex? (16 + n))
      | 'B', digit :: _ => digitToNat? digit >>= (fun n => SensorArea.ofIndex? (24 + n))
      | _ , _ => none
  | _ => none

def stripComments (s : String) : String :=
  String.intercalate "\n" <| (s.splitOn "\n").map (fun line =>
    match line.splitOn "||" with
    | head :: _ => head
    | [] => line)

def stripPrefixDirectives (token : String) : String :=
  let t := trim token
  if t = "" then
    t
  else if t.startsWith "{" then
    match t.splitOn "}" with
    | _ :: rest => trim (String.intercalate "}" rest)
    | _ => t
  else if t.startsWith "(" then
    match t.splitOn ")" with
    | _ :: rest => trim (String.intercalate ")" rest)
    | _ => t
  else if t.startsWith "<" then
    match t.splitOn ">" with
    | _ :: rest => trim (String.intercalate ">" rest)
    | _ => t
  else
    t

def sanitizeSlideToken (token : String) : String :=
  let t := stripPrefixDirectives token
  let filtered :=
    t.toList.filter (fun c =>
      c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$')
  String.ofList filtered

def isTouchAreaChar (c : Char) : Bool :=
  c = 'A' || c = 'B' || c = 'C' || c = 'D' || c = 'E'

def isSlideMarkChar (c : Char) : Bool :=
  c = '-' || c = '^' || c = 'v' || c = '<' || c = '>' || c = 'V' || c = 'p' || c = 'q' || c = 's' || c = 'z' || c = 'w'

def isSlideText (t : String) : Bool :=
  t.toList.any isSlideMarkChar

def inferKind (token : String) : RawNoteKind :=
  let t := stripPrefixDirectives token
  if t = "" then .rest
  else if leadingDigit? t |>.isSome then
    if isSlideText t then .slide
    else if t.contains 'h' then .hold
    else .tap
  else
    match t.toList with
    | area :: _ =>
        if isTouchAreaChar area then
          if t.contains 'h' then .touchHold else .touch
        else .unknown
    | _ => .unknown

def splitTopLevel (sep : Char) (s : String) : List String :=
  let rec loop (chars : List Char) (depth : Nat) (current : List Char) (acc : List String) : List String :=
    match chars with
    | [] => (String.ofList current :: acc).reverse
    | '[' :: rest => loop rest (depth + 1) (current.concat '[') acc
    | ']' :: rest => loop rest (depth - 1) (current.concat ']') acc
    | c :: rest =>
        if c = sep && depth = 0 then
          loop rest depth [] (String.ofList current :: acc)
        else
          loop rest depth (current.concat c) acc
  loop s.toList 0 [] []

def splitEntryTokens (entry : String) : List String :=
  (splitTopLevel '/' entry).map trim |>.filter (fun t => t ≠ "")

def parseHeadBreak (token : String) : Bool :=
  let t := stripPrefixDirectives token
  if isSlideText t then
    match splitTopLevel '-' t |>.head? with
    | some pre => pre.contains 'b'
    | none => t.contains 'b'
  else
    t.contains 'b'

def parseSlideSegmentBreak (token : String) : Bool :=
  let t := stripPrefixDirectives token
  if !isSlideText t then false
  else
    match t.splitOn "[" with
    | head :: _ => head.endsWith "b"
    | [] => false

def parseHSpeedDirective (text : String) (current : Rat) : Rat :=
  let t := trim text
  if !t.startsWith "<H" then current
  else
    let body :=
      match t.splitOn ">" with
      | head :: _ => (head.drop 2).toString
      | [] => ""
    let valueText :=
      if body.startsWith "S*" then (body.drop 2).toString else body
    parseRatDef valueText current

partial def applyInlineDirective (bpm : Rat) (divisor : Nat) (hSpeed : Rat) (segment : String) : Rat × Nat × Rat × String :=
  let t := trim segment
  if t.startsWith "(" then
    let after := (t.drop 1).toString
    match after.splitOn ")" with
    | inside :: rest =>
        let nextBpm := parseRatDef inside bpm
        applyInlineDirective nextBpm divisor hSpeed (String.intercalate ")" rest)
    | _ => (bpm, divisor, hSpeed, t)
  else if t.startsWith "{" then
    let after := (t.drop 1).toString
    match after.splitOn "}" with
    | inside :: rest =>
        applyInlineDirective bpm (parseNatDef inside divisor) hSpeed (String.intercalate "}" rest)
    | _ => (bpm, divisor, hSpeed, t)
  else if t.startsWith "<H" then
    let after :=
      match t.splitOn ">" with
      | _ :: rest => String.intercalate ">" rest
      | [] => t
    applyInlineDirective bpm divisor (parseHSpeedDirective t hSpeed) after
  else
    (bpm, divisor, hSpeed, t)

def mkRawToken (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (token : String) : RawNoteToken :=
  let t := trim token
  let kind := inferKind t
  let parsedText := if kind = .slide then sanitizeSlideToken t else t
  let slot := leadingDigit? parsedText >>= (fun n => OuterSlot.ofIndex? (n - 1))
  let sensorPos := touchAreaToSensorArea? t
  let length := parseDurationSpec bpm t
  let starWait := if kind = .slide then parseStarWaitSpec bpm t else none
  let isBreak := parseHeadBreak t
  let isEX := t.contains 'x'
  let isHanabi := t.contains 'f'
  let isSlideNoHead := t.contains '!' || t.contains '?'
  let isForceStar := t.contains '$'
  let isFakeRotate := (t.toList.filter (fun c => c = '$')).length >= 2
  let isSlideBreak := parseSlideSegmentBreak t
  { rawText := parsedText
  , kind := kind
  , timing := timing
  , bpm := bpm
  , hSpeed := hSpeed
  , divisor := divisor
  , slot := slot
  , sensorPos := sensorPos
  , length := length
  , starWait := starWait
  , isBreak := isBreak
  , isEX := isEX
  , isHanabi := isHanabi
  , isSlideNoHead := isSlideNoHead
  , isForceStar := isForceStar
  , isFakeRotate := isFakeRotate
  , isSlideBreak := isSlideBreak }

private def sameHeadGroupParts (token : String) : List String :=
  (splitTopLevel '*' token).map trim |>.filter (fun t => t ≠ "")

private def sameHeadHeadPrefix (token : String) : String :=
  let t := trim <| stripPrefixDirectives token
  match t.toList with
  | [] => ""
  | first :: rest =>
      if isTouchAreaChar first then
        match first, rest with
        | 'C', _ => "C"
        | _, digit :: _ =>
            if digit.isDigit then String.singleton first ++ String.singleton digit else String.singleton first
        | _, _ => String.singleton first
      else if first.isDigit then
        String.singleton first
      else
        ""

private def expandSameHeadGroupRest (groupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat)
    (headPrefix : String) (size : Nat) : Nat → List String → List RawNoteToken
  | _, [] => []
  | idx, part :: rest =>
      let rebuilt := if headPrefix = "" then part else headPrefix ++ part
      let tok := mkRawToken timing bpm hSpeed divisor rebuilt
      { tok with
        isSlideNoHead := true,
        sourceGroupId := some groupId,
        sourceGroupIndex := some idx,
        sourceGroupSize := some size } ::
      expandSameHeadGroupRest groupId timing bpm hSpeed divisor headPrefix size (idx + 1) rest

private def expandSameHeadGroup (groupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (token : String) : List RawNoteToken :=
  let parts := sameHeadGroupParts token
  match parts with
  | [] => []
  | first :: rest =>
      let headPrefix := sameHeadHeadPrefix first
      let firstTok := mkRawToken timing bpm hSpeed divisor first
      let firstIsGroupedSlide := firstTok.kind = .slide
      let groupedSlideCount := (if firstIsGroupedSlide then 1 else 0) + rest.length
      let firstTok :=
        if firstIsGroupedSlide then
          { firstTok with sourceGroupId := some groupId, sourceGroupIndex := some 0, sourceGroupSize := some groupedSlideCount }
        else
          firstTok
      let restStartIndex := if firstIsGroupedSlide then 1 else 0
      let restToks := expandSameHeadGroupRest groupId timing bpm hSpeed divisor headPrefix groupedSlideCount restStartIndex rest
      firstTok :: restToks

private def expandTokenList (baseGroupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) : Nat → List String → List RawNoteToken
  | _, [] => []
  | idx, tokText :: rest =>
      let current :=
        if tokText.contains '*' then
          expandSameHeadGroup (baseGroupId + idx) timing bpm hSpeed divisor tokText
        else
          [mkRawToken timing bpm hSpeed divisor tokText]
      current ++ expandTokenList baseGroupId timing bpm hSpeed divisor (idx + 1) rest

def parseSegmentNotes (segment : String) (time : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) : List RawNoteToken :=
  let normalized := trim <| segment.replace "\n" ""
  if normalized = "" then
    []
  else if normalized.contains '`' then
    let parts := normalized.splitOn "`"
    let (_, acc) :=
      parts.foldl
        (fun (state : TimePoint × List RawNoteToken) part =>
          let (currentTime, acc) := state
          let tokens := expandTokenList 0 currentTime bpm hSpeed divisor 0 (splitEntryTokens part)
          (currentTime + pseudoIncrement bpm, acc ++ tokens))
        (time, [])
    acc
  else
    expandTokenList 0 time bpm hSpeed divisor 0 (splitEntryTokens normalized)

partial def parseSegments (segments : List String) (time : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (acc : List RawNoteToken) : List RawNoteToken :=
  match segments with
  | [] => acc.reverse
  | segment :: rest =>
      let clean := trim segment
      let (bpm', divisor', hSpeed', body) := applyInlineDirective bpm divisor hSpeed clean
      let newTokens := parseSegmentNotes body time bpm' hSpeed' divisor'
      let nextTime := time + noteTimingIncrement bpm' divisor'
      parseSegments rest nextTime bpm' hSpeed' divisor' (newTokens.reverse ++ acc)

end LnmaiCore.Simai
