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
  c = '-' || c = '^' || c = 'v' || c = '<' || c = '>' || c = 'V' || c = 'p' || c = 'q' || c = 's' || c = 'z' || c = 'w' || c = 'K'

def isSlideText (t : String) : Bool :=
  t.toList.any isSlideMarkChar

-- Simai inline FX note head: `<digit>fx` → hold, `<area>fx` → touchHold.
-- Checked before the ordinary heuristics so e.g. `1fx@gate` is not classified
-- as a tap, and `1fx@hpf` / `A3fx@sidechain` are not misread.
def fxHeadKind (t : String) : Option RawNoteKind :=
  match t.toList with
  | c :: rest =>
      if c.isDigit then
        match rest with
        | 'f' :: 'x' :: _ => some .hold
        | _ => none
      else if isTouchAreaChar c then
        let afterArea :=
          match c, rest with
          | 'C', _ => rest
          | _, d :: r => if d.isDigit then r else rest
          | _, _ => rest
        match afterArea with
        | 'f' :: 'x' :: _ => some .touchHold
        | _ => none
      else
        none
  | [] => none

def inferKind (token : String) : RawNoteKind :=
  let t := stripPrefixDirectives token
  match fxHeadKind t with
  | some kind => kind
  | none =>
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

-- Extract the raw `type(params)` after the first `@`, dropping the trailing
-- `[timing]`. Returns `none` when the token carries no FX marker.
def fxEffectOf (token : String) : Option String :=
  let t := stripPrefixDirectives token
  match t.splitOn "@" with
  | _ :: rest =>
      let after := String.intercalate "@" rest
      let body :=
        match after.splitOn "[" with
        | head :: _ => head
        | [] => after
      let b := trim body
      if b = "" then none else some b
  | _ => none

def splitTopLevel (sep : Char) (s : String) : List String :=
  let rec loop (chars : List Char) (depth : Nat) (current : List Char) (acc : List String) : List String :=
    match chars with
    | [] => (String.ofList current.reverse :: acc).reverse
    | '[' :: rest => loop rest (depth + 1) ('[' :: current) acc
    | ']' :: rest => loop rest (depth - 1) (']' :: current) acc
    | '(' :: rest => loop rest (depth + 1) ('(' :: current) acc
    | ')' :: rest => loop rest (depth - 1) (')' :: current) acc
    | c :: rest =>
        if c = sep && depth = 0 then
          loop rest depth [] (String.ofList current.reverse :: acc)
        else
          loop rest depth (c :: current) acc
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
  let rec loop (seenSlide : Bool) : List Char → Bool
    | [] => false
    | c :: rest =>
        if c == 'b' && seenSlide && (rest.isEmpty || rest.head? == some '[') then true
        else loop (seenSlide || isSlideMarkChar c) rest
  loop false t.toList

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

private def applyInlineDirectiveFuel (fuel : Nat) (bpm : Rat) (divisor : Nat) (hSpeed : Rat) (segment : String) : Rat × Nat × Rat × String :=
  let t := trim segment
  match fuel with
  | 0 => (bpm, divisor, hSpeed, t)
  | fuel + 1 =>
      if t.startsWith "(" then
        let after := (t.drop 1).toString
        match after.splitOn ")" with
        | inside :: rest =>
            let nextBpm := parseRatDef inside bpm
            applyInlineDirectiveFuel fuel nextBpm divisor hSpeed (String.intercalate ")" rest)
        | _ => (bpm, divisor, hSpeed, t)
      else if t.startsWith "{" then
        let after := (t.drop 1).toString
        match after.splitOn "}" with
        | inside :: rest =>
            applyInlineDirectiveFuel fuel bpm (parseNatDef inside divisor) hSpeed (String.intercalate "}" rest)
        | _ => (bpm, divisor, hSpeed, t)
      else if t.startsWith "<H" then
        let after :=
          match t.splitOn ">" with
          | _ :: rest => String.intercalate ">" rest
          | [] => t
        applyInlineDirectiveFuel fuel bpm divisor (parseHSpeedDirective t hSpeed) after
      else if t.contains '@' then
        -- Inline FX: the `(...)` after `@` are effect parameters, not a BPM
        -- directive; keep the whole note text intact.
        (bpm, divisor, hSpeed, t)
      else
        -- MajSimai clears the pending note when a BPM/divisor directive is encountered,
        -- even if note text precedes it in the comma segment.
        let nextDirective := t.toList.dropWhile (fun c => c != '(' && c != '{')
        if nextDirective.isEmpty then (bpm, divisor, hSpeed, t)
        else applyInlineDirectiveFuel fuel bpm divisor hSpeed (String.ofList nextDirective)

def applyInlineDirective (bpm : Rat) (divisor : Nat) (hSpeed : Rat) (segment : String) : Rat × Nat × Rat × String :=
  applyInlineDirectiveFuel (segment.length + 1) bpm divisor hSpeed segment

def mkRawToken (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) (token : String) : RawNoteToken :=
  let t := trim token
  let fxEffect := fxEffectOf t
  let isFx := fxEffect.isSome
  let kind := inferKind t
  let parsedText := if kind = .slide then sanitizeSlideToken t else t
  let slot := leadingDigit? parsedText >>= (fun n => OuterSlot.ofIndex? (n - 1))
  let sensorPos := touchAreaToSensorArea? t
  let slideBody := if kind = .slide then parseSlideBodyFromText parsedText |>.toOption else none
  let length := parseDurationSpec bpm t
  let starWait := if kind = .slide then parseStarWaitSpec bpm t else none
  -- FX heuristics must be overridden: `bit_crusher` contains 'b' (break),
  -- `fx` contains 'f' (hanabi) and `x` (EX). FX heads are EX (design §5).
  let isBreak := if isFx then false else parseHeadBreak t
  let isEX := if isFx then true else t.contains 'x'
  let isHanabi := if isFx then false else t.contains 'f'
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
  , isSlideBreak := isSlideBreak
  , fxEffect := fxEffect }

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

private def readBracketSuffixFuel (fuel : Nat) (rawText : String) : List Char → List Char → Except ParseError (String × List Char)
  | [], _ => Except.error <| chainSyntaxError rawText "unterminated slide timing spec"
  | c :: rest, acc =>
      let acc := c :: acc
      if c = ']' then
        pure (String.ofList acc.reverse, rest)
      else
        match fuel with
        | 0 => Except.error <| chainSyntaxError rawText "unterminated slide timing spec"
        | fuel + 1 => readBracketSuffixFuel fuel rawText rest acc

private def readBracketSuffix (rawText : String) (chars acc : List Char) : Except ParseError (String × List Char) :=
  readBracketSuffixFuel (chars.length + 1) rawText chars acc

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

private def parseContinuousSlideSegmentsCoreFuel
    (fuel : Nat) (rawText : String) (currentStart : Char) : List Char → Except ParseError (List ContinuousChainSegment)
  | [] => pure []
  | c :: rest =>
      match fuel with
      | 0 => Except.error <| chainSyntaxError rawText "invalid connected slide syntax"
      | fuel + 1 =>
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
            let tail ← parseContinuousSlideSegmentsCoreFuel fuel rawText endChar rest
            pure ({ rawText := segmentCore ++ timingSuffix, hasTiming := hasTiming } :: tail)

private def parseContinuousSlideSegmentsCore
    (rawText : String) (currentStart : Char) (chars : List Char) : Except ParseError (List ContinuousChainSegment) :=
  parseContinuousSlideSegmentsCoreFuel (chars.length + 1) rawText currentStart chars

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
  if shape.kind == .wifi then
    Except.error <| chainSyntaxError rawText
      "wifi slide cannot be part of a connection slide group"
  else match slideBarCountForShape shape with
  | some count => pure count
  | none =>
    Except.error <| chainSyntaxError rawText "missing slide table for connected slide segment"

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
    let baseMicros : Rat := totalLength.toMicros
    let rec loop (isFirst : Bool) (elapsedBars : Nat) : List (ContinuousChainSegment × Nat) → List RawNoteToken
      | [] => []
      | (segment, bars) :: rest =>
          let segTok := mkRawToken timing bpm hSpeed divisor segment.rawText
          -- Quantize cumulative boundaries so the parts sum to the original duration.
          let startOffset := Time.durationFromRatMicros
            (baseMicros * Int.ofNat elapsedBars / Int.ofNat totalBars)
          let endOffset := Time.durationFromRatMicros
            (baseMicros * Int.ofNat (elapsedBars + bars) / Int.ofNat totalBars)
          let segLen := endOffset - startOffset
          let segWait := if isFirst then baseTok.starWait else none
          applySharedSlideFlags baseTok
            { segTok with length := some segLen, starWait := segWait }
            (if isFirst then baseTok.isSlideNoHead else true) :: loop false (elapsedBars + bars) rest
    let rawTokens := loop true 0 (List.zip segments barCounts)
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
        Except.error <| chainSyntaxError rawText "connected slide chain requires either per-segment timing or a final overall timing spec"
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
          let _ ← classifyChainTimingLayout baseTok.rawText segments
          -- NoteLoader redistributes the summed duration in both accepted timing layouts.
          buildWholeDurationChainTokens groupId timing bpm hSpeed divisor baseTok segments
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

private def expandSameHeadGroupRest
    (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat)
    (headPrefix : String) : Nat → List String → Except ParseError (List RawNoteToken)
  | _, [] => pure []
  | groupId, part :: rest => do
      let rebuilt := headPrefix ++ part
      let tokens ← expandContinuousChainToken groupId timing bpm hSpeed divisor rebuilt
      let tail ← expandSameHeadGroupRest timing bpm hSpeed divisor headPrefix
        (groupId + tokens.length) rest
      pure (tokens.map (fun tok => { tok with isSlideNoHead := true }) ++ tail)

private def expandSameHeadGroup (groupId : Nat) (timing : TimePoint) (bpm : Rat)
    (hSpeed : Rat) (divisor : Nat) (token : String) : Except ParseError (List RawNoteToken) := do
  let parts := sameHeadGroupParts token
  match parts with
  | [] => pure []
  | first :: rest =>
      let headPrefix := sameHeadHeadPrefix first
      let firstTokens ← expandContinuousChainToken groupId timing bpm hSpeed divisor first
      let restTokens ← expandSameHeadGroupRest timing bpm hSpeed divisor headPrefix
        (groupId + firstTokens.length) rest
      pure (firstTokens ++ restTokens)

private def expandTokenList (baseGroupId : Nat) (timing : TimePoint) (bpm : Rat) (hSpeed : Rat) (divisor : Nat) : Nat → List String → Except ParseError (List RawNoteToken)
  | _, [] => pure []
  | idx, tokText :: rest => do
      -- FX notes legitimately contain `@` and effect letters (`c`, `m`, ...);
      -- exempt them from the extended-modifier rejection.
      if fxHeadKind (stripPrefixDirectives tokText) == none &&
          inferKind tokText != .unknown &&
          tokText.toList.any (fun c => c == 'K' || c == '@' || c == 'c' || c == 'm') then
        throw <| chainSyntaxError tokText "unsupported extended slide or note modifier (K, @, c, m)"
      let current ←
        if tokText.contains '*' then
          expandSameHeadGroup (baseGroupId + idx) timing bpm hSpeed divisor tokText
        else
          expandContinuousChainToken (baseGroupId + idx) timing bpm hSpeed divisor tokText
      let tail ← expandTokenList baseGroupId timing bpm hSpeed divisor (idx + current.length) rest
      pure (current ++ tail)

private def expandEntryText (baseGroupId : Nat) (timing : TimePoint) (bpm : Rat)
    (hSpeed : Rat) (divisor : Nat) (entry : String) : Except ParseError (List RawNoteToken) := do
  let text := trim entry
  if text.length == 2 && text.toList.all (fun c => (digitToNat? c).isSome) then
    expandTokenList baseGroupId timing bpm hSpeed divisor 0 (text.toList.map String.singleton)
  else
    expandTokenList baseGroupId timing bpm hSpeed divisor 0 (splitEntryTokens text)

private def parseSegmentNotesExact (segment : String) (time : Rat) (bpm : Rat)
    (hSpeed : Rat) (divisor : Nat) : Except ParseError (List RawNoteToken) := do
  let normalized := String.ofList (segment.toList.filter (fun c => !c.isWhitespace))
  if normalized = "" then
    pure []
  else if normalized.contains '`' then
    let parts := (normalized.splitOn "`").filter (· != "")
    let increment := if bpm > 0 then Time.bpmBeatMicrosRat bpm / 32 else 1000
    let (_, acc) ← parts.foldlM
      (fun (state : Rat × List RawNoteToken) part => do
        let (currentTime, acc) := state
        let tokens ← expandEntryText 0 (Time.pointFromRatMicros currentTime) bpm hSpeed divisor part
        pure (currentTime + increment, tokens.reverse ++ acc))
      (time, [])
    pure acc.reverse
  else
    expandEntryText 0 (Time.pointFromRatMicros time) bpm hSpeed divisor normalized

def parseSegmentNotes (segment : String) (time : TimePoint) (bpm : Rat)
    (hSpeed : Rat) (divisor : Nat) : Except ParseError (List RawNoteToken) :=
  parseSegmentNotesExact segment time.toMicros bpm hSpeed divisor

private def parseSegmentsExact (segments : List String) (time : Rat) (bpm : Rat)
    (hSpeed : Rat) (divisor : Nat) (acc : List RawNoteToken) :
    Except ParseError (List RawNoteToken) :=
  match segments with
  | [] => pure acc.reverse
  | segment :: rest => do
      let clean := trim segment
      let (bpm', divisor', hSpeed', body) := applyInlineDirective bpm divisor hSpeed clean
      let newTokens ← parseSegmentNotesExact body time bpm' hSpeed' divisor'
      let increment :=
        if bpm' > 0 && divisor' > 0 then Time.bpmMeasureMicrosRat bpm' / Int.ofNat divisor'
        else 0
      parseSegmentsExact rest (time + increment) bpm' hSpeed' divisor' (newTokens.reverse ++ acc)

def parseSegments (segments : List String) (time : TimePoint) (bpm : Rat) (hSpeed : Rat)
    (divisor : Nat) (acc : List RawNoteToken) : Except ParseError (List RawNoteToken) :=
  parseSegmentsExact segments time.toMicros bpm hSpeed divisor acc

end LnmaiCore.Simai
