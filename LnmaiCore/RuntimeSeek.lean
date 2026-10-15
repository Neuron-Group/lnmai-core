import LnmaiCore.ChartLoader
import LnmaiCore.Scheduler
import Std.Data.HashMap

namespace LnmaiCore

namespace RuntimeSeek

open ChartLoader InputModel Lifecycle Std

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

private def slideRoots (slides : List SlideChartNote) : HashMap Nat Nat := Id.run do
  let parents := slides.foldl (fun map note => map.insert note.noteIndex note.parentNoteIndex)
    ({} : HashMap Nat (Option Nat))
  let rec root (fuel id : Nat) : Nat :=
    match fuel with
    | 0 => id
    | fuel + 1 => match parents.get? id with
      | some (some parent) => root fuel parent
      | _ => id
  return slides.foldl (fun map note => map.insert note.noteIndex (root slides.length note.noteIndex)) {}

/-- Keep crossing bodies and complete connected ownership groups. -/
def chartFrom (chart : ChartSpec) (target : TimePoint) : ChartSpec :=
  let roots := slideRoots chart.slides
  let activeRoots := chart.slides.foldl (fun map note =>
    if note.startTiming + note.length >= target then
      map.insert (roots.getD note.noteIndex note.noteIndex) true
    else map) ({} : HashMap Nat Bool)
  let slides := chart.slides.filter (fun note =>
    activeRoots.contains (roots.getD note.noteIndex note.noteIndex))
  let logicalIds := slides.foldl (fun map note => map.insert note.logicalSlideId true)
    ({} : HashMap Nat Bool)
  let slideHeads := chart.slideHeads.filter (fun head =>
    head.timing >= target || logicalIds.contains head.logicalSlideId)
  { chart with
    taps := chart.taps.filter (fun note => note.timing >= target)
    holds := chart.holds.filter (fun note => note.timing + note.length >= target)
    touches := chart.touches.filter (fun note => note.timing >= target)
    touchHolds := chart.touchHolds.filter (fun note => note.timing + note.length >= target)
    slideHeads := slideHeads
    slides := slides }

/-- A temporary replay view contains only crossing bodies and competing heads
    in their input windows. Dense unrelated history never enters the scheduler. -/
private def crossingView (initial : GameState) (remaining : ChartSpec)
    (target : TimePoint) : GameState := Id.run do
  let crossing (note : HoldNote) :=
    note.params.judgeTiming < target && note.params.judgeTiming + note.length >= target
  let holds := initial.activeHolds.filter (fun entry => crossing entry.2)
  let touchHolds := initial.activeTouchHolds.filter (fun entry => crossing entry.2)
  let timings := holds.map (fun entry => entry.2.params.judgeTiming) ++
    touchHolds.map (fun entry => entry.2.params.judgeTiming)
  let needsHead (timing : TimePoint) := timing < target && timings.any (fun head =>
    Duration.abs (timing - head) <= Constants.touchGoodMs + Constants.JUDGABLE_RANGE_SEC)
  let tapQueues : ButtonQueueVec TapFamilyNote := { data := initial.tapQueues.data.map (fun queue =>
    { notes := queue.notes.filter (fun note => needsHead note.params.judgeTiming) }) }
  let holdQueues : ButtonQueueVec HoldNote := { data := initial.holdQueues.data.map (fun queue =>
    { notes := queue.notes.filter (fun note => crossing note || needsHead note.params.judgeTiming) }) }
  let touchQueues : SensorQueueVec TouchNote := { data := initial.touchQueues.data.map (fun queue =>
    { notes := queue.notes.filter (fun note => needsHead note.params.judgeTiming) }) }
  let touchHoldQueues : SensorQueueVec HoldNote := { data := initial.touchHoldQueues.data.map (fun queue =>
    { notes := queue.notes.filter (fun note => crossing note || needsHead note.params.judgeTiming) }) }
  -- The sparse view has gaps in the original shared indices; rank its heads
  -- locally for arbitration, without changing the session queues or note IDs.
  let tapQueues : ButtonQueueVec TapFamilyNote := ButtonVec.ofFn (fun zone =>
    let taps := buttonQueueAt tapQueues zone
    let hs := buttonQueueAt holdQueues zone
    let indices := taps.notes.map (·.buttonQueueIndex) ++ hs.notes.map (·.buttonQueueIndex)
    { notes := taps.notes.map (fun note =>
      let index := (indices.filter (· < note.buttonQueueIndex)).length
      match note with
      | .tap n => .tap { n with buttonQueueIndex := index }
      | .slideHead n => .slideHead { n with buttonQueueIndex := index }) })
  let holdQueues : ButtonQueueVec HoldNote := ButtonVec.ofFn (fun zone =>
    let hs := buttonQueueAt holdQueues zone
    let originalTaps := buttonQueueAt initial.tapQueues zone
    let taps := originalTaps.notes.filter (fun note => needsHead note.params.judgeTiming)
    let indices := taps.map (·.buttonQueueIndex) ++ hs.notes.map (·.buttonQueueIndex)
    { notes := hs.notes.map (fun note => { note with
      buttonQueueIndex := (indices.filter (· < note.buttonQueueIndex)).length }) })
  let touchQueues : SensorQueueVec TouchNote := SensorVec.ofFn (fun area =>
    let ts := sensorQueueAt touchQueues area
    let hs := sensorQueueAt touchHoldQueues area
    let indices := ts.notes.map (·.touchQueueIndex) ++ hs.notes.map (·.touchQueueIndex)
    { notes := ts.notes.map (fun note => { note with
      touchQueueIndex := (indices.filter (· < note.touchQueueIndex)).length }) })
  let touchHoldQueues : SensorQueueVec HoldNote := SensorVec.ofFn (fun area =>
    let hs := sensorQueueAt touchHoldQueues area
    let originalTouches := sensorQueueAt initial.touchQueues area
    let ts := originalTouches.notes.filter (fun note => needsHead note.params.judgeTiming)
    let indices := ts.map (·.touchQueueIndex) ++ hs.notes.map (·.touchQueueIndex)
    { notes := hs.notes.map (fun note => { note with
      touchQueueIndex := (indices.filter (· < note.touchQueueIndex)).length }) })
  let logicalIds := remaining.slides.foldl (fun map note =>
    if note.headTiming < target then map.insert note.logicalSlideId true else map)
    ({} : HashMap Nat Bool)
  let ids := remaining.slides.foldl (fun map note =>
    if logicalIds.contains note.logicalSlideId then map.insert note.noteIndex true else map)
    ({} : HashMap Nat Bool)
  return { initial with
    tapQueues := tapQueues, holdQueues := holdQueues
    touchQueues := touchQueues, touchHoldQueues := touchHoldQueues
    activeHolds := holdQueues.entries.flatMap (fun entry => entry.2.notes.map (entry.1, ·))
    activeTouchHolds := touchHoldQueues.entries.flatMap (fun entry => entry.2.notes.map (entry.1, ·))
    slides := initial.slides.filter (fun note => ids.contains note.params.noteIndex)
    touchGroupStates := [], touchHoldGroupStates := [] }

private def latch (held : FrameInput) (event : TimedInputEvent) : FrameInput :=
  match event with
  | .buttonHold _ zone down => { held with buttonHeld := held.buttonHeld.set zone down }
  | .sensorHold _ area down => { held with sensorHeld := held.sensorHeld.set area down }
  | _ => held

private def rememberRender (commands : HashMap (Nat × Nat × Nat) RenderCommand)
    (command : RenderCommand) : HashMap (Nat × Nat × Nat) RenderCommand :=
  match command with
  | .ShowJudgeResult .. => commands
  | .UpdateSlideProgress id _ => commands.insert (id, 0, 0) command
  | .UpdateSlideTrackProgress id track _ => commands.insert (id, 1, track) command
  | .HideSlideBars id _ => commands.insert (id, 2, 0) command
  | .HideSlideTrackBars id track _ => commands.insert (id, 3, track) command
  | .HideAllSlideBars id => commands.insert (id, 4, 0) command

/-- Position immutable queues directly; replay only the crossing-body view.
    No chart lowering, geometry construction, or session replacement occurs. -/
def seek (chart : ChartSpec) (initial previous : GameState) (target : TimePoint)
    (events : List TimedInputEvent) (keepHeld : Bool) : GameState × List RenderCommand := Id.run do
  let remaining := chartFrom chart target
  let base : GameState := { initial with
    judgeStyle := previous.judgeStyle
    touchPanelOffset := previous.touchPanelOffset
    subdivideSlideJudgeGrade := previous.subdivideSlideJudgeGrade
    noteFastLateDisplay := previous.noteFastLateDisplay
    breakFastLateDisplay := previous.breakFastLateDisplay
    displayHoldHeadJudgeResult := previous.displayHoldHeadJudgeResult
    buttonRingForTouch := previous.buttonRingForTouch }
  let mut state := crossingView base remaining target
  let starts := state.activeHolds.map (fun entry => entry.2.params.judgeTiming) ++
    state.activeTouchHolds.map (fun entry => entry.2.params.judgeTiming) ++
    state.tapQueues.data.flatMap (fun queue => queue.notes.map (·.params.judgeTiming)) ++
    state.touchQueues.data.flatMap (fun queue => queue.notes.map (·.params.judgeTiming)) ++
    state.slides.map (·.headTiming)
  let start := max TimePoint.zero
    (starts.foldl min target - Constants.JUDGABLE_RANGE_SEC)
  state := { state with currentTime := start }
  let mut held : FrameInput := {}
  let mut render : HashMap (Nat × Nat × Nat) RenderCommand := {}
  for event in events do
    if event.at < target then
      if event.at >= start && !starts.isEmpty then
        let (next, _, _, commands) := Scheduler.stepFrameTimed state
          { currentTime := max state.currentTime event.at, events := [event] }
        state := next
        render := commands.foldl rememberRender render
      else
        let latched := latch { buttonHeld := state.prevButton, sensorHeld := state.prevSensor } event
        state := { state with prevButton := latched.buttonHeld, prevSensor := latched.sensorHeld }
      held := latch held event
  -- Target-time inputs and exact-tail judgments belong to the resumed frame.
  let boundary := max state.currentTime (target - Duration.fromMicros 1)
  let (restored, _, _, commands) := Scheduler.stepFrameTimed state { currentTime := boundary }
  render := commands.foldl rememberRender render
  let restoredHolds := (restored.activeHolds.map Prod.snd ++ restored.activeTouchHolds.map Prod.snd).foldl
    (fun map note => map.insert note.params.noteIndex note) ({} : HashMap Nat HoldNote)
  let restoreHold (note : HoldNote) := match restoredHolds.get? note.params.noteIndex with
    | some restored => { restored with
        buttonQueueIndex := note.buttonQueueIndex, touchQueueIndex := note.touchQueueIndex }
    | none => note
  let restoredSlides := restored.slides.foldl
    (fun map note => map.insert note.params.noteIndex note) ({} : HashMap Nat SlideNote)
  let expired := initial.slides.filter (fun note => note.startTiming + note.length < target)
  let tapQueues : ButtonQueueVec TapFamilyNote := { data := initial.tapQueues.data.map (fun queue =>
    { queue with currentIndex := (queue.notes.takeWhile (fun note => note.params.judgeTiming < target)).length }) }
  let holdQueues : ButtonQueueVec HoldNote := { data := initial.holdQueues.data.map (fun queue =>
    { queue with currentIndex := (queue.notes.takeWhile (fun note => note.params.judgeTiming < target)).length }) }
  let touchQueues : SensorQueueVec TouchNote := { data := initial.touchQueues.data.map (fun queue =>
    { queue with currentIndex := (queue.notes.takeWhile (fun note => note.params.judgeTiming < target)).length }) }
  let touchHoldQueues : SensorQueueVec HoldNote := { data := initial.touchHoldQueues.data.map (fun queue =>
    { queue with currentIndex := (queue.notes.takeWhile (fun note => note.params.judgeTiming < target)).length }) }
  let next : GameState := { base with
    currentTime := target, currentBatch := { currentTime := target }
    prevButton := if keepHeld then held.buttonHeld else initial.prevButton
    prevSensor := if keepHeld then held.sensorHeld else initial.prevSensor
    tapQueues := tapQueues, holdQueues := holdQueues
    touchQueues := touchQueues, touchHoldQueues := touchHoldQueues
    buttonQueueFrontiers := ButtonVec.ofFn (fun zone =>
      (buttonQueueAt tapQueues zone).currentIndex + (buttonQueueAt holdQueues zone).currentIndex)
    touchQueueFrontiers := SensorVec.ofFn (fun area =>
      (sensorQueueAt touchQueues area).currentIndex + (sensorQueueAt touchHoldQueues area).currentIndex)
    activeHolds := (initial.activeHolds.filter (fun entry =>
      entry.2.params.judgeTiming + entry.2.length >= target)).map (fun entry => (entry.1, restoreHold entry.2))
    activeTouchHolds := (initial.activeTouchHolds.filter (fun entry =>
      entry.2.params.judgeTiming + entry.2.length >= target)).map (fun entry => (entry.1, restoreHold entry.2))
    slides := initial.slides.map (fun note =>
      if note.startTiming + note.length < target then
        { note with state := .Ended, judgeQueues := note.judgeQueues.map (fun _ => []) }
      else restoredSlides.getD note.params.noteIndex note)
    touchGroupStates := [], touchHoldGroupStates := [] }
  -- Past star heads belong to a retained body group, but are not scored twice.
  let remainingHeads := remaining.slideHeads.filter (fun head => head.timing >= target)
  let remainingSlides := remaining.slides.filter (fun note => note.startTiming + note.length >= target)
  let totals := ChartLoader.chartScoreTotals
    { remaining with
      slideHeads := remainingHeads
      slides := remainingSlides }
  let next := { next with score :=
    { totalBase := totals.totalBase, totalExtra := totals.totalExtra, maxDxScore := totals.noteCount * 3 } }
  return (next, render.toList.map Prod.snd ++
    expired.map (fun note => .HideAllSlideBars note.params.noteIndex))

end RuntimeSeek
end LnmaiCore
