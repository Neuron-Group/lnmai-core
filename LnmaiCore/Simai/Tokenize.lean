import LnmaiCore.Simai.Timing
import LnmaiCore.Simai.Shape
import LnmaiCore.Simai.SlideTables
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
      | 'A', digit :: _ =>
          match digitToNat? digit with
          | some 1 => some .A1 | some 2 => some .A2 | some 3 => some .A3 | some 4 => some .A4
          | some 5 => some .A5 | some 6 => some .A6 | some 7 => some .A7 | some 8 => some .A8
          | _ => none
      | 'D', digit :: _ =>
          match digitToNat? digit with
          | some 1 => some .D1 | some 2 => some .D2 | some 3 => some .D3 | some 4 => some .D4
          | some 5 => some .D5 | some 6 => some .D6 | some 7 => some .D7 | some 8 => some .D8
          | _ => none
      | 'E', digit :: _ =>
          match digitToNat? digit with
          | some 1 => some .E1 | some 2 => some .E2 | some 3 => some .E3 | some 4 => some .E4
          | some 5 => some .E5 | some 6 => some .E6 | some 7 => some .E7 | some 8 => some .E8
          | _ => none
      | 'B', digit :: _ =>
          match digitToNat? digit with
          | some 1 => some .B1 | some 2 => some .B2 | some 3 => some .B3 | some 4 => some .B4
          | some 5 => some .B5 | some 6 => some .B6 | some 7 => some .B7 | some 8 => some .B8
          | _ => none
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

private def takeUntilSlideMark (text : String) : String :=
  let rec loop : List Char → List Char → String
    | [], acc => String.ofList acc.reverse
    | c :: rest, acc =>
        if isSlideMarkChar c then String.ofList acc.reverse
        else loop rest (c :: acc)
  loop text.toList []

def parseHeadBreak (token : String) : Bool :=
  let t := stripPrefixDirectives token
  if isSlideText t then
    (takeUntilSlideMark t).contains 'b'
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
  let slideBody := if kind = .slide then parseSlideBodyFromText parsedText |>.toOption else none
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
  , slideBody := slideBody
  , length := length
  , starWait := starWait
  , isBreak := isBreak
  , isEX := isEX
  , isHanabi := isHanabi
  , isSlideNoHead := isSlideNoHead
  , isForceStar := isForceStar
  , isFakeRotate := isFakeRotate
  , isSlideBreak := isSlideBreak }

private structure ContinuousChainSegment where
  rawText : String
  hasTiming : Bool

private def chainSyntaxError (rawText : String) (message : String) : ParseError :=
  { kind := .invalidSyntax, rawText := rawText, message := message }

private def readDigitChar (rawText : String) : List Char → Except ParseError (Char × List Char)
  | c :: rest =>
      if c.isDigit then pure (c, rest)
      else Except.error <| chainSyntaxError rawText "invalid connected slide syntax"
  | [] => Except.error <| chainSyntaxError rawText "invalid connected slide syntax"

private partial def readBracketSuffix (rawText : String) : List Char → List Char → Except ParseError (String × List Char)
  | [], _ => Except.error <| chainSyntaxError rawText "unterminated slide timing spec"
  | c :: rest, acc =>
      let acc := acc.concat c
      if c = ']' then
        pure (String.ofList acc, rest)
      else
        readBracketSuffix rawText rest acc

private def parseSlideShapeChars (rawText : String) (op : Char) (rest : List Char) : Except ParseError (String × List Char) := do
  if op = 'V' then
    let (middle, rest) ← readDigitChar rawText rest
    let (finish, rest) ← readDigitChar rawText rest
    pure (String.singleton op ++ String.singleton middle ++ String.singleton finish, rest)
  else
    let (shapeText, rest) :=
      if (op = 'p' || op = 'q') then
        match rest with
        | next :: tail =>
            if next = op then
              (String.singleton op ++ String.singleton next, tail)
            else
              (String.singleton op, rest)
        | [] => (String.singleton op, rest)
      else
        (String.singleton op, rest)
    let (finish, rest) ← readDigitChar rawText rest
    pure (shapeText ++ String.singleton finish, rest)

private partial def parseContinuousSlideSegmentsCore
    (rawText : String) (currentStart : Char) : List Char → Except ParseError (List ContinuousChainSegment)
  | [] => pure []
  | c :: rest =>
      if c.isDigit then
        Except.error <| chainSyntaxError rawText "connected slide chain cannot contain a fresh numeric head"
      else if !isSlideMarkChar c then
        Except.error <| chainSyntaxError rawText "invalid connected slide syntax"
      else do
        let (shapeAndEnd, rest) ← parseSlideShapeChars rawText c rest
        let segmentCore := String.singleton currentStart ++ shapeAndEnd
        let (timingSuffix, rest, hasTiming) ←
          match rest with
          | '[' :: tail =>
              let (suffix, rest') ← readBracketSuffix rawText tail ['[']
              pure (suffix, rest', true)
          | _ => pure ("", rest, false)
        let endChar := shapeAndEnd.toList.reverse.head?.getD currentStart
        let tail ← parseContinuousSlideSegmentsCore rawText endChar rest
        pure ({ rawText := segmentCore ++ timingSuffix, hasTiming := hasTiming } :: tail)

private def parseContinuousSlideSegments? (token : String) : Except ParseError (Option (List ContinuousChainSegment)) := do
  let sanitized := sanitizeSlideToken token
  match sanitized.toList with
  | start :: rest =>
      if !start.isDigit then
        pure none
      else do
        let segments ← parseContinuousSlideSegmentsCore sanitized start rest
        if segments.length ≤ 1 then pure none else pure (some segments)
  | [] => pure none

private def segmentBarCount (rawText : String) : Except ParseError Nat := do
  let shape ← detectShapeFromText rawText
  let queues := judgeQueuesForShape shape false |>.getD []
  let count := queues.foldl (fun acc queue => Nat.max acc queue.length) 0
  if count = 0 then
    Except.error <| chainSyntaxError rawText "missing slide table for connected slide segment"
  else
    pure count

private def applySharedSlideFlags (baseTok segmentTok : RawNoteToken) (isHeadless : Bool) : RawNoteToken :=
  { segmentTok with
    isBreak := baseTok.isBreak
    , isEX := baseTok.isEX
    , isHanabi := baseTok.isHanabi
    , isSlideNoHead := isHeadless
    , isForceStar := baseTok.isForceStar
    , isFakeRotate := baseTok.isFakeRotate
    , isSlideBreak := baseTok.isSlideBreak }

private def tagConnectedGroup (groupId : Nat) (size : Nat) (tokens : List RawNoteToken) : List RawNoteToken :=
  let rec loop (index : Nat) : List RawNoteToken → List RawNoteToken
    | [] => []
    | tok :: rest =>
        { tok with
          sourceGroupId := some groupId
          , sourceGroupIndex := some index
          , sourceGroupSize := some size } :: loop (index + 1) rest
  loop 0 tokens

private def buildPerSegmentChainTokens
    (groupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat)
    (baseTok : RawNoteToken) (segments : List ContinuousChainSegment) : Except ParseError (List RawNoteToken) := do
  let rec loop (isFirst : Bool) : List ContinuousChainSegment → List RawNoteToken
    | [] => []
    | segment :: rest =>
        let tok := mkRawToken timing bpm hSpeed divisor segment.rawText
        applySharedSlideFlags baseTok tok (if isFirst then baseTok.isSlideNoHead else true) :: loop false rest
  let tokens := loop true segments
  pure <| tagConnectedGroup groupId tokens.length tokens

private def buildWholeDurationChainTokens
    (groupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat)
    (baseTok : RawNoteToken) (segments : List ContinuousChainSegment) : Except ParseError (List RawNoteToken) := do
  let some totalLength := baseTok.length
    | Except.error <| chainSyntaxError baseTok.rawText "connected slide chain requires an explicit timing spec"
  let barCounts ← segments.mapM (fun segment => segmentBarCount segment.rawText)
  let totalBars := barCounts.foldl (· + ·) 0
  if totalBars = 0 then
    Except.error <| chainSyntaxError baseTok.rawText "connected slide chain has no measurable segments"
  else
    let baseMicros := totalLength.toMicros
    let rec loop (isFirst : Bool) : List (ContinuousChainSegment × Nat) → List RawNoteToken
      | [] => []
      | (segment, bars) :: rest =>
          let segTok := mkRawToken timing bpm hSpeed divisor segment.rawText
          let segLen := Duration.fromMicros (baseMicros * Int.ofNat bars / Int.ofNat totalBars)
          let segWait := if isFirst then baseTok.starWait else none
          applySharedSlideFlags baseTok { segTok with length := some segLen, starWait := segWait } (if isFirst then baseTok.isSlideNoHead else true) :: loop false rest
    let rawTokens := loop true (List.zip segments barCounts)
    pure <| tagConnectedGroup groupId rawTokens.length rawTokens

private inductive ChainTimingLayout where
  | perSegment
  | overallFinal

private def classifyChainTimingLayout (rawText : String) (segments : List ContinuousChainSegment) : Except ParseError ChainTimingLayout :=
  let flags := segments.map ContinuousChainSegment.hasTiming
  if flags.all id then
    pure .perSegment
  else
    match flags.reverse with
    | true :: restRev =>
        if restRev.all (fun flag => !flag) then
          pure .overallFinal
        else
          Except.error <| chainSyntaxError rawText "invalid connected slide timing layout"
    | false :: _ =>
        if flags.all (fun flag => !flag) then
          Except.error <| chainSyntaxError rawText "connected slide chain requires either per-segment timing or a final overall timing spec"
        else
          Except.error <| chainSyntaxError rawText "invalid connected slide timing layout"
    | [] =>
        Except.error <| chainSyntaxError rawText "invalid connected slide timing layout"

private def expandContinuousChainToken
    (groupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (token : String) :
    Except ParseError (List RawNoteToken) := do
  let baseTok := mkRawToken timing bpm hSpeed divisor token
  match baseTok.kind with
  | .slide =>
      match (← parseContinuousSlideSegments? token) with
      | none => pure [baseTok]
      | some segments =>
          match (← classifyChainTimingLayout baseTok.rawText segments) with
          | .perSegment => buildPerSegmentChainTokens groupId timing bpm hSpeed divisor baseTok segments
          | .overallFinal => buildWholeDurationChainTokens groupId timing bpm hSpeed divisor baseTok segments
  | _ => pure [baseTok]

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
    (headPrefix : String) (size : Nat) : Nat → List String → Except ParseError (List RawNoteToken)
  | _, [] => pure []
  | idx, part :: rest => do
      let rebuilt := if headPrefix = "" then part else headPrefix ++ part
      let tok := mkRawToken timing bpm hSpeed divisor rebuilt
      let tail ← expandSameHeadGroupRest groupId timing bpm hSpeed divisor headPrefix size (idx + 1) rest
      pure ({ tok with
        isSlideNoHead := true,
        sourceGroupId := some groupId,
        sourceGroupIndex := some idx,
        sourceGroupSize := some size } :: tail)

private def expandSameHeadGroup (groupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (token : String) : Except ParseError (List RawNoteToken) := do
  let parts := sameHeadGroupParts token
  match parts with
  | [] => pure []
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
      let restToks ← expandSameHeadGroupRest groupId timing bpm hSpeed divisor headPrefix groupedSlideCount restStartIndex rest
      pure (firstTok :: restToks)

private def expandTokenList (baseGroupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) : Nat → List String → Except ParseError (List RawNoteToken)
  | _, [] => pure []
  | idx, tokText :: rest => do
      let current ←
        if tokText.contains '*' then
          expandSameHeadGroup (baseGroupId + idx) timing bpm hSpeed divisor tokText
        else
          expandContinuousChainToken (baseGroupId + idx) timing bpm hSpeed divisor tokText
      let tail ← expandTokenList baseGroupId timing bpm hSpeed divisor (idx + 1) rest
      pure (current ++ tail)

def parseSegmentNotes (segment : String) (time : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) : Except ParseError (List RawNoteToken) := do
  let normalized := trim <| segment.replace "\n" ""
  if normalized = "" then
    pure []
  else if normalized.contains '`' then
    let parts := normalized.splitOn "`"
    let (_, acc) ←
      parts.foldlM
        (fun (state : TimePoint × List RawNoteToken) part => do
          let (currentTime, acc) := state
          let tokens ← expandTokenList 0 currentTime bpm hSpeed divisor 0 (splitEntryTokens part)
          pure (currentTime + pseudoIncrement bpm, acc ++ tokens))
        (time, [])
    pure acc
  else
    expandTokenList 0 time bpm hSpeed divisor 0 (splitEntryTokens normalized)

partial def parseSegments (segments : List String) (time : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (acc : List RawNoteToken) : Except ParseError (List RawNoteToken) :=
  match segments with
  | [] => pure acc.reverse
  | segment :: rest => do
      let clean := trim segment
      let (bpm', divisor', hSpeed', body) := applyInlineDirective bpm divisor hSpeed clean
      let newTokens ← parseSegmentNotes body time bpm' hSpeed' divisor'
      let nextTime := time + noteTimingIncrement bpm' divisor'
      parseSegments rest nextTime bpm' hSpeed' divisor' (newTokens.reverse ++ acc)

end LnmaiCore.Simai
