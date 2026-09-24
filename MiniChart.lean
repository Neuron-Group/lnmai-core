import LnmaiCore
open LnmaiCore

def main : IO Unit := do
  let content ← IO.FS.readFile "/tmp/mini_chart.txt"
  match Simai.compileLowered content 1 with
  | .error err => IO.println s!"Parse error: {err}"
  | .ok chart =>
      IO.println s!"=== Lean Mini Chart ==="
      IO.println s!"taps: {chart.taps.length}"
      IO.println s!"holds: {chart.holds.length}"
      IO.println s!"touches: {chart.touches.length}"
      IO.println s!"touchHolds: {chart.touchHolds.length}"
      IO.println s!"slides: {chart.slides.length}"
      IO.println "--- tap details ---"
      for t in chart.taps do
        IO.println s!"  tap: timing={t.timing} slot={t.slot} isBreak={t.isBreak} isEX={t.isEX}"
      IO.println "--- hold details ---"
      for h in chart.holds do
        IO.println s!"  hold: timing={h.timing} slot={h.slot} length={h.length}"
      IO.println "--- touch details ---"
      for t in chart.touches do
        IO.println s!"  touch: timing={t.timing} sensor={t.sensorPos}"
