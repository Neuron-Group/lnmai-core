import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader
open LnmaiCore.InputModel


def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

def failing : List Nat := [57, 58, 94, 97, 101, 104]

private def isFailing (idx : Nat) : Bool :=
  failing.any (fun x => x = idx)

private def holdInfo (chart : ChartSpec) : List HoldChartNote :=
  chart.holds.filter (fun note => isFailing note.noteIndex)

private def notesNearTime (chart : ChartSpec) (center : TimePoint) : List String :=
  let low := center.toMicros - 2000000
  let high := center.toMicros + 2000000
  let taps := chart.taps.filter (fun note => note.timing.toMicros >= low && note.timing.toMicros <= high)
    |>.map (fun note => s!"tap#{note.noteIndex}@{note.timing.toMicros}:{repr note.slot.toButtonZone}")
  let holds := chart.holds.filter (fun note => note.timing.toMicros >= low && note.timing.toMicros <= high)
    |>.map (fun note => s!"hold#{note.noteIndex}@{note.timing.toMicros}:{repr note.slot.toButtonZone}/len={note.length.toMicros}")
  let touches := chart.touches.filter (fun note => note.timing.toMicros >= low && note.timing.toMicros <= high)
    |>.map (fun note => s!"touch#{note.noteIndex}@{note.timing.toMicros}:{repr note.sensorPos}")
  let touchHolds := chart.touchHolds.filter (fun note => note.timing.toMicros >= low && note.timing.toMicros <= high)
    |>.map (fun note => s!"touchHold#{note.noteIndex}@{note.timing.toMicros}:{repr note.sensorPos}/len={note.length.toMicros}")
  let slides := chart.slides.filter (fun note => note.timing.toMicros >= low && note.timing.toMicros <= high)
    |>.map (fun note => s!"slide#{note.noteIndex}@{note.timing.toMicros}:{repr note.slot.toButtonZone}/start={note.startTiming.toMicros}/len={note.length.toMicros}")
  taps ++ holds ++ touches ++ touchHolds ++ slides

private def eventsNearTime (events : List TimedInputEvent) (center : TimePoint) : List String :=
  let low := center.toMicros - 100000
  let high := center.toMicros + 1200000
  events.filter (fun evt => evt.at.toMicros >= low && evt.at.toMicros <= high)
    |>.map (fun evt =>
      match evt with
      | .buttonClick tp zone => s!"buttonClick@{tp.toMicros}:{repr zone}"
      | .buttonHold tp zone isDown => s!"buttonHold@{tp.toMicros}:{repr zone}:{isDown}"
      | .sensorClick tp area => s!"sensorClick@{tp.toMicros}:{repr area}"
      | .sensorHold tp area isDown => s!"sensorHold@{tp.toMicros}:{repr area}:{isDown}")

private def judgedNearTime (events : List JudgeEvent) (center : TimePoint) : List String :=
  let low := center.toMicros - 100000
  let high := center.toMicros + 2000000
  events.filter (fun evt => evt.noteIndex ∈ failing || (evt.diff.toMicros + center.toMicros >= low && evt.diff.toMicros + center.toMicros <= high))
    |>.map (fun evt => s!"judge#{evt.noteIndex}:{repr evt.kind}:{repr evt.grade}:diff={evt.diff.toMicros}")

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err => IO.println s!"parse error {repr err}"
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let result := simulateChartSpecWithTactic chart tactic
      IO.println s!"missing={missingJudgedNoteIndices result}"
      for hold in holdInfo chart do
        IO.println s!"=== hold #{hold.noteIndex} at {hold.timing.toMicros} zone={repr hold.slot.toButtonZone} len={hold.length.toMicros} ==="
        IO.println s!"near notes={repr (notesNearTime chart hold.timing)}"
        IO.println s!"near tactic events={repr (eventsNearTime tactic.events hold.timing)}"
        IO.println s!"near judged events={repr (judgedNearTime result.events hold.timing)}"
