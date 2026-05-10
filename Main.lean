import LnmaiCore

open LnmaiCore

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

def main : IO Unit := do
  match sampleChartSpec with
  | .error err =>
      IO.println s!"parse error: {err}"
  | .ok chart =>
      let state := ChartLoader.buildGameState chart
      IO.println s!"chart parsed: taps={chart.taps.length}, holds={chart.holds.length}, touches={chart.touches.length}, slides={chart.slides.length}"
      IO.println s!"state: {repr state}"
