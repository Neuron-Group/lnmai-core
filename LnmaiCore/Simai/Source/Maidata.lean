import LnmaiCore.Simai.Tokenize
import LnmaiCore.Simai.Normalize
import LnmaiCore.Simai.Typecheck
import LnmaiCore.Time

namespace LnmaiCore.Simai

private def sameEventKey (token : RawNoteToken) (timing : TimePoint) (bpm hSpeed : Rat) (divisor : Nat) : Bool :=
  token.timing == timing && token.bpm == bpm && token.hSpeed == hSpeed && token.divisor == divisor

private def isExpandedSlideGroupChild (token : RawNoteToken) : Bool :=
  token.kind == .slide &&
    match token.sourceGroupIndex with
    | some index => index != 0
    | none => false

private def majdataTimingNoteCount (tokens : List RawNoteToken) : Nat :=
  (tokens.filter (fun token => !isExpandedSlideGroupChild token)).length

private def majdataNoHeadSlideCount (tokens : List RawNoteToken) : Nat :=
  (tokens.filter (fun token =>
    !isExpandedSlideGroupChild token && token.kind == .slide && token.isSlideNoHead)).length

-- MajdataPlay derives touch Each from the whole timing point, but suppresses
-- ordinary touch Each when no-head slides are the only companions. Touch-holds
-- keep Each in that case and join both touch-head and touch-hold body groups.
private def tagTouchEachGroup (groupId : Nat) (tokens : List RawNoteToken) : List RawNoteToken :=
  let timingNoteCount := majdataTimingNoteCount tokens
  let nonNoHeadCount := timingNoteCount - majdataNoHeadSlideCount tokens
  let touchIsEach := timingNoteCount > 1 && nonNoHeadCount != 1
  let touchHoldIsEach := timingNoteCount > 1
  let rec loop (index : Nat) : List RawNoteToken → List RawNoteToken
    | [] => []
    | token :: rest =>
        let shouldTag :=
          match token.kind with
          | .touch => touchIsEach
          | .touchHold => touchHoldIsEach
          | _ => false
        if shouldTag then
          { token with
            sourceGroupId := some groupId
            , sourceGroupIndex := some index
            , sourceGroupSize := some timingNoteCount } :: loop (index + 1) rest
        else
          token :: loop index rest
  loop 0 tokens

private partial def takeSameEventRest
    (timing : TimePoint) (bpm hSpeed : Rat) (divisor : Nat) :
    List RawNoteToken → List RawNoteToken × List RawNoteToken
  | [] => ([], [])
  | token :: rest =>
      if sameEventKey token timing bpm hSpeed divisor then
        let (same, remaining) := takeSameEventRest timing bpm hSpeed divisor rest
        (token :: same, remaining)
      else
        ([], token :: rest)

private partial def annotateTouchEachGroupsFrom (groupId : Nat) :
    List RawNoteToken → List RawNoteToken
  | [] => []
  | token :: rest =>
      let (same, remaining) :=
        takeSameEventRest token.timing token.bpm token.hSpeed token.divisor rest
      let eventTokens := token :: same
      tagTouchEachGroup groupId eventTokens ++ annotateTouchEachGroupsFrom (groupId + 1) remaining

private def annotateTouchEachGroups (tokens : List RawNoteToken) : List RawNoteToken :=
  annotateTouchEachGroupsFrom 0 tokens

private def sourceChartFromTokens (tokens : List RawNoteToken) : SourceChart :=
  let rec loop (remaining : List RawNoteToken) (current : Option (TimePoint × Rat × Rat × Nat × List SourceNote)) (acc : List SourceEvent) :=
    match remaining, current with
    | [], none => { events := acc.reverse }
    | [], some (timing, bpm, hSpeed, divisor, notes) =>
        { events := ({ timing := timing, bpm := bpm, hSpeed := hSpeed, divisor := divisor, notes := notes.reverse } :: acc).reverse }
    | token :: rest, none =>
        loop rest (some (token.timing, token.bpm, token.hSpeed, token.divisor, [{ token := token, sourcePos := token.sourcePos }])) acc
    | token :: rest, some (timing, bpm, hSpeed, divisor, notes) =>
        if sameEventKey token timing bpm hSpeed divisor then
          loop rest (some (timing, bpm, hSpeed, divisor, { token := token, sourcePos := token.sourcePos } :: notes)) acc
        else
          let event : SourceEvent := { timing := timing, bpm := bpm, hSpeed := hSpeed, divisor := divisor, notes := notes.reverse }
          loop rest (some (token.timing, token.bpm, token.hSpeed, token.divisor, [{ token := token, sourcePos := token.sourcePos }])) (event :: acc)
  loop tokens none []

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

def parseSourceMaidata (content : String) : Except ParseError MaidataFile :=
  Except.ok <| parseMaidataLines (content.splitOn "\n") [] []

def lowerSourceChartBlock (file : MaidataFile) (block : MaidataChartBlock) : Except ParseError FrontendChartResult := do
  let baseBpm := parseRatDef ((metadataField file.metadata "&wholebpm").getD "120") 120
  let firstOffset :=
    match Time.parseSecondsPointString? ((metadataField file.metadata "&first").getD "0") with
    | some value => value
    | none => TimePoint.zero
  let cleanedBody := stripComments block.rawBody
  let segments := cleanedBody.splitOn ","
  let rawTokens ← parseSegments segments firstOffset baseBpm 1 4 []
  let tokens := annotateTouchEachGroups rawTokens
  let _ ← typecheckSlides tokens
  let source := sourceChartFromTokens tokens
  let (normalized, slideNotes) := lowerRawTokens (fun bpm => Time.durationFromRatMicros (Time.bpmMeasureMicrosRat bpm)) tokens
  let lowered := toChartSpec normalized
  pure
    { semantic := { normalized := normalized, lowered := lowered }
    , inspection := { metadata := file.metadata, chart := block, source := source, tokens := tokens, slideNotes := slideNotes } }

def lowerSourceChartByLevel (file : MaidataFile) (levelIndex : Nat) : Except ParseError FrontendChartResult := do
  match file.charts.find? (fun block => block.levelIndex = levelIndex) with
  | some block => lowerSourceChartBlock file block
  | none => Except.error { kind := .invalidSyntax, rawText := "", message := s!"missing inote block {levelIndex}" }

def parseAndLowerSourceMaidata (content : String) (levelIndex : Nat) : Except ParseError FrontendChartResult := do
  let file ← parseSourceMaidata content
  lowerSourceChartByLevel file levelIndex

end LnmaiCore.Simai
