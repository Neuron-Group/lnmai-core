import LnmaiCore

open LnmaiCore

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

def main : IO Unit := do
  match sampleChartSpec with
  | .error err =>
      IO.println s!"parse error: {err}"
  | .ok chart =>
      let state := ChartLoader.buildGameState chart
      IO.println s!"chart parsed: taps={chart.taps.length}, holds={chart.holds.length}, touches={chart.touches.length}, slides={chart.slides.length}"
      IO.println s!"state initialized: {repr state.currentTime}"
