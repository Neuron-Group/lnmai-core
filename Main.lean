import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "../assets/11358_インドア系ならトラックメイカー/maidata.txt"

def stripTokenPrefix (token : String) : String :=
  match token.splitOn "}" with
  | [] => token.trimAscii.toString
  | parts =>
      match parts.reverse.head? with
      | some content => content.trimAscii.toString
      | none => token.trimAscii.toString

def isSupportedSimaiToken (token : String) : Bool :=
  token.contains '-' || token.contains '>' || token.contains '<' || token.contains '^' ||
  token.contains 'v' || token.contains 'V' || token.contains 'p' || token.contains 'q' ||
  token.contains 's' || token.contains 'z' || token.contains 'w'

def sampleRealSimaiTokens (content : String) : List String :=
  let afterBlock :=
    match content.splitOn "&inote_4=" with
    | _ :: rest =>
        match rest with
        | head :: _ => head
        | [] => content
    | [] => content
  let tokens := ((afterBlock.splitOn ",").map (fun line => line.splitOn "\n")).foldr List.append []
  tokens.map stripTokenPrefix |>.filter (fun token => token ≠ "" && token ≠ "E" && isSupportedSimaiToken token)

def realSimaiAstProbe : IO (Except Simai.ParseError Simai.TimingPointSemantics) := do
  let content ← IO.FS.readFile realChartPath
  let tokens := sampleRealSimaiTokens content |>.take 8
  pure <| Simai.parseTimingPointSlides 0.0 128.0 1.0 tokens

def realMaidataLoweredProbe : IO (Except Simai.ParseError Simai.ParsedMaidataChart) := do
  let content ← IO.FS.readFile realChartPath
  pure <| Simai.parseAndLowerMaidata content 4

def sampleChartJson : String :=
  "{\n" ++
  "  \"taps\": [\n" ++
  "    { \"timingSec\": 1.0, \"lane\": 0, \"isBreak\": false, \"isEX\": false, \"noteIndex\": 1 }\n" ++
  "  ],\n" ++
  "  \"holds\": [\n" ++
  "    { \"timingSec\": 2.0, \"lane\": 1, \"lengthSec\": 0.6, \"isBreak\": false, \"isEX\": false, \"isTouch\": false, \"noteIndex\": 2 }\n" ++
  "  ],\n" ++
  "  \"touches\": [\n" ++
  "    { \"timingSec\": 3.0, \"sensorPos\": 4, \"isBreak\": false, \"noteIndex\": 3 }\n" ++
  "  ],\n" ++
  "  \"touchHolds\": [],\n" ++
  "  \"slides\": []\n" ++
  "}"

def sampleChartSpec : Except String ChartLoader.ChartSpec :=
  ChartLoader.parseChartJsonString sampleChartJson

#eval sampleChartSpec

def sampleSimaiAst : Except Simai.ParseError Simai.TimingPointSemantics :=
  Simai.parseTimingPointSlides 12.0 180.0 1.0 ["1-3", "1V35", "1w5"]

#eval sampleSimaiAst

#eval! realSimaiAstProbe

#eval! realMaidataLoweredProbe

def sampleSlideAnnotation : Except Simai.ParseError ChartLoader.SimaiSlideAnnotation := do
  let note ← Simai.parseSlideNote "1V35" 1 5
  pure <| ChartLoader.slideAnnotationFromSemantics 42 note

#eval sampleSlideAnnotation

def main : IO Unit := do
  match sampleChartSpec with
  | .error err =>
      IO.println s!"parse error: {err}"
  | .ok chart =>
      let state := ChartLoader.buildGameState chart
      IO.println s!"chart parsed: taps={chart.taps.length}, holds={chart.holds.length}, touches={chart.touches.length}, slides={chart.slides.length}"
      IO.println s!"state: {repr state}"
