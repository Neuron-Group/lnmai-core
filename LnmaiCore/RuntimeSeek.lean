import LnmaiCore.ChartLoader
import LnmaiCore.Scheduler

namespace LnmaiCore

namespace RuntimeSeek

open ChartLoader InputModel

def slideRoot (slides : List SlideChartNote) (noteIndex : Nat) : Nat :=
  let rec walk (fuel : Nat) (id : Nat) : Nat :=
    match fuel with
    | 0 => id
    | fuel + 1 =>
      match slides.find? (fun note => note.noteIndex == id) with
      | some note => match note.parentNoteIndex with
        | some parent => walk fuel parent
        | none => id
      | none => id
  walk slides.length noteIndex

def noteIndices (chart : ChartSpec) : List Nat :=
  chart.taps.map (·.noteIndex) ++ chart.holds.map (·.noteIndex) ++
  chart.touches.map (·.noteIndex) ++ chart.touchHolds.map (·.noteIndex) ++
  chart.slideHeads.map (·.noteIndex) ++ chart.slides.map (·.noteIndex)

/-- Keep crossing bodies and complete connected ownership groups. -/
def chartFrom (chart : ChartSpec) (target : TimePoint) : ChartSpec :=
  let activeRoots := (chart.slides.filter (fun note => note.startTiming + note.length >= target)).map
    (fun note => slideRoot chart.slides note.noteIndex)
  let slides := chart.slides.filter (fun note =>
    activeRoots.contains (slideRoot chart.slides note.noteIndex))
  let slideHeads := chart.slideHeads.filter (fun head =>
    head.timing >= target || slides.any (fun note => note.logicalSlideId == head.logicalSlideId))
  { chart with
    taps := chart.taps.filter (fun note => note.timing >= target)
    holds := chart.holds.filter (fun note => note.timing + note.length >= target)
    touches := chart.touches.filter (fun note => note.timing >= target)
    touchHolds := chart.touchHolds.filter (fun note => note.timing + note.length >= target)
    slideHeads := slideHeads
    slides := slides }

/-- Restore immutable runtime queues, then silently position their lifecycles.
    Chart lowering, queue construction and slide geometry are never repeated. -/
def seek (chart : ChartSpec) (initial previous : GameState) (target : TimePoint)
    (events : List TimedInputEvent) (keepHeld : Bool) : GameState × List RenderCommand := Id.run do
  let mut state := { initial with
    judgeStyle := previous.judgeStyle
    touchPanelOffset := previous.touchPanelOffset
    subdivideSlideJudgeGrade := previous.subdivideSlideJudgeGrade
    noteFastLateDisplay := previous.noteFastLateDisplay
    breakFastLateDisplay := previous.breakFastLateDisplay
    displayHoldHeadJudgeResult := previous.displayHoldHeadJudgeResult
    buttonRingForTouch := previous.buttonRingForTouch }
  let mut render : Array RenderCommand := #[]
  for event in events do
    if event.at < target then
      let (next, _, _, commands) := Scheduler.stepFrameTimed state
        { currentTime := max state.currentTime event.at, events := [event] }
      state := next
      render := render ++ commands.toArray
  let (next, _, _, commands) := Scheduler.stepFrameTimed state { currentTime := target }
  -- Expired bodies must not emit delayed judgments after the score reset.
  -- Keep their indices and empty queues so connected children retain ownership.
  let expired := next.slides.filter (fun note => note.startTiming + note.length < target)
  let next : GameState := { next with slides := next.slides.map (fun (note : Lifecycle.SlideNote) =>
    if note.startTiming + note.length < target then
      { note with state := .Ended, judgeQueues := note.judgeQueues.map (fun _ => []) }
    else note) }
  let remaining := chartFrom chart target
  -- Past star heads belong to a retained body group, but are not scored twice.
  let remainingHeads := remaining.slideHeads.filter (fun head => head.timing >= target)
  let remainingSlides := remaining.slides.filter (fun note => note.startTiming + note.length >= target)
  let totals := ChartLoader.chartScoreTotals
    { remaining with
      slideHeads := remainingHeads
      slides := remainingSlides }
  let next := { next with score :=
    { totalBase := totals.totalBase, totalExtra := totals.totalExtra, maxDxScore := totals.noteCount * 3 } }
  let next := if keepHeld then next else { next with
    prevButton := initial.prevButton, prevSensor := initial.prevSensor }
  return (next, render.toList ++ commands ++
    expired.map (fun note => .HideAllSlideBars note.params.noteIndex))

end RuntimeSeek
end LnmaiCore
