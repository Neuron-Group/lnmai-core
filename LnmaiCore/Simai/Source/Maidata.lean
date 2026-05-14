import LnmaiCore.Simai.Tokenize
import LnmaiCore.Simai.Normalize
import LnmaiCore.Time

namespace LnmaiCore.Simai

private def sameEventKey (token : RawNoteToken) (timing : TimePoint) (bpm hSpeed : Float) (divisor : Nat) : Bool :=
  token.timing == timing && token.bpm == bpm && token.hSpeed == hSpeed && token.divisor == divisor

private def sourceChartFromTokens (tokens : List RawNoteToken) : SourceChart :=
  let rec loop (remaining : List RawNoteToken) (current : Option (TimePoint × Float × Float × Nat × List SourceNote)) (acc : List SourceEvent) :=
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
  let baseBpm := parseFloatDef ((metadataField file.metadata "&wholebpm").getD "120") 120.0
  let firstOffset :=
    match Time.parseSecondsPointString? ((metadataField file.metadata "&first").getD "0") with
    | some value => value
    | none => TimePoint.zero
  let cleanedBody := stripComments block.rawBody
  let segments := cleanedBody.splitOn ","
  let tokens := parseSegments segments firstOffset baseBpm 1.0 4 []
  let source := sourceChartFromTokens tokens
  let (normalized, slideNotes) := lowerRawTokens Time.measureDurationMicros tokens
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
