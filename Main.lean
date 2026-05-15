import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

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
  pure <| Simai.parseSlideTimingPoint TimePoint.zero 128 1 tokens

def realChartLevel4Probe : IO (Except Simai.ParseError ChartLoader.ChartSpec) := do
  let content ← IO.FS.readFile realChartPath
  pure <| Simai.compileLowered content 4

def realChartLevel4SkeletonProbe : IO (Except Simai.ParseError (List NoteTimingSkeleton)) := do
  let content ← IO.FS.readFile realChartPath
  pure <| timingSkeletonFromChartSection content 4

def realChartLevel4DefaultTacticProbe : IO (Except Simai.ParseError ManualTacticSequence) := do
  let content ← IO.FS.readFile realChartPath
  pure <| defaultTacticFromChartSection content 4

def realChartLevel2DefaultResultProbe : IO Unit := do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err =>
      IO.println s!"real level-2 default tactic parse error: {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
      IO.println s!"real level-2 default tactic AP={achievesAP result}, missing={missingJudgedNoteIndices result}"

def sampleChartJson : String :=
  "{\n" ++
  "  \"taps\": [\n" ++
  "    { \"timing\": 1000000, \"slot\": \"S1\", \"isBreak\": false, \"isEX\": false, \"noteIndex\": 1 }\n" ++
  "  ],\n" ++
  "  \"holds\": [\n" ++
  "    { \"timing\": 2000000, \"slot\": \"S2\", \"length\": 600000, \"isBreak\": false, \"isEX\": false, \"isTouch\": false, \"noteIndex\": 2 }\n" ++
  "  ],\n" ++
  "  \"touches\": [\n" ++
  "    { \"timing\": 3000000, \"sensorPos\": \"A5\", \"isBreak\": false, \"touchQueueIndex\": 0, \"noteIndex\": 3 }\n" ++
  "  ],\n" ++
  "  \"touchHolds\": [],\n" ++
  "  \"slides\": []\n" ++
  "}"

def sampleChartSpec : Except String ChartLoader.ChartSpec :=
  ChartLoader.parseChartJsonString sampleChartJson

#eval sampleChartSpec

def sampleSimaiAst : Except Simai.ParseError Simai.TimingPointSemantics :=
  Simai.parseSlideTimingPoint (LnmaiCore.Time.pointFromSecondsRat 12) 180 1 ["1-3", "1V35", "1w5"]

#eval sampleSimaiAst

#eval realSimaiAstProbe

#eval do
  match (← realChartLevel4Probe) with
  | .error err =>
      IO.println s!"real level-4 chart parse error: {repr err}"
  | .ok chart =>
      IO.println s!"real level-4 chart parsed: taps={chart.taps.length}, holds={chart.holds.length}, touches={chart.touches.length}, touchHolds={chart.touchHolds.length}, slides={chart.slides.length}"

#eval do
  match (← realChartLevel4SkeletonProbe) with
  | .error err =>
      IO.println s!"real level-4 skeleton error: {repr err}"
  | .ok skeleton =>
      IO.println s!"real level-4 timing skeleton entries: {skeleton.length}"
      IO.println s!"first entries: {repr (skeleton.take 5)}"

#eval do
  match (← realChartLevel4DefaultTacticProbe) with
  | .error err =>
      IO.println s!"real level-4 default tactic error: {repr err}"
  | .ok tactic =>
      IO.println s!"real level-4 default tactic event count: {tactic.events.length}"
      IO.println s!"first events: {repr (tactic.events.take 10)}"

#eval realChartLevel2DefaultResultProbe

def main : IO Unit := do
  match sampleChartSpec with
  | .error err =>
      IO.println s!"parse error: {err}"
  | .ok chart =>
      let state := ChartLoader.buildGameState chart
      IO.println s!"chart parsed: taps={chart.taps.length}, holds={chart.holds.length}, touches={chart.touches.length}, slides={chart.slides.length}"
      IO.println s!"state: {repr state}"
