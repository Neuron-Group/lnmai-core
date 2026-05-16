import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/834_PANDORA PARADOXXX/maidata.txt"

def targetNote : Nat := 984

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 6 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      let skeleton := chartTimingSkeleton chart
      IO.println s!"skeleton={repr (skeleton.find? (fun entry : NoteTimingSkeleton => entry.noteIndex = targetNote))}"
      match chart.slides.find? (fun note : ChartLoader.SlideChartNote => note.noteIndex = targetNote) with
      | some slide =>
          IO.println s!"slide={repr slide}"
      | none =>
          IO.println "target note is not a slide"
      match chart.holds.find? (fun note : ChartLoader.HoldChartNote => note.noteIndex = targetNote) with
      | some hold => IO.println s!"hold={repr hold}"
      | none => pure ()
      match chart.taps.find? (fun note : ChartLoader.TapChartNote => note.noteIndex = targetNote) with
      | some tap => IO.println s!"tap={repr tap}"
      | none => pure ()
      match chart.touches.find? (fun note : ChartLoader.TouchChartNote => note.noteIndex = targetNote) with
      | some touch => IO.println s!"touch={repr touch}"
      | none => pure ()
      match chart.touchHolds.find? (fun note : ChartLoader.TouchHoldChartNote => note.noteIndex = targetNote) with
      | some touchHold => IO.println s!"touchHold={repr touchHold}"
      | none => pure ()
