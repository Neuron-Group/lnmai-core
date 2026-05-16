/-
  Frame Scheduler — step one frame of gameplay.

  The Core receives all active notes and frame input, advances each
  note's lifecycle, and returns updated notes plus a list of JudgeEvents.

  Semantic policy:

  This module intentionally fixes a subsystem processing order inside one
  frame:

  1. tap queues
  2. hold heads / active holds
  3. touch queues
  4. touch-hold heads / active touch-holds
  5. slide progression

  This is not just an implementation accident. It is part of the runtime's
  observable game semantics because multiple subsystems can compete for the
  same same-frame input or depend on same-frame side effects from earlier
  subsystems.

  Important examples:

  - tap before hold means a shared button click can be consumed by the tap,
    leaving the hold head judgeable but still queued unless another click is
    available in the same frame
  - touch before touch-hold means a touch can populate shared touch-group
    state early enough for a later touch-hold head to resolve from that group
    share in the very same frame
  - slides run after tap-like families because slide progression depends on
    held sensor state rather than the per-frame click cursor consumed by those
    earlier families

  The order should therefore only change together with deliberate semantic
  review and order-sensitive regression tests.
-/

import LnmaiCore.Types
import LnmaiCore.Areas
import LnmaiCore.Constants
import LnmaiCore.Judge
import LnmaiCore.Convert
import LnmaiCore.Score
import LnmaiCore.Lifecycle
import LnmaiCore.Storage
import LnmaiCore.InputModel
import LnmaiCore.Time

set_option linter.unusedVariables false

namespace LnmaiCore.Scheduler

open Constants
open InputModel
open Lifecycle
open Score
open LnmaiCore

structure ClickCursor where
  buttonUsed : ButtonVec Nat := ButtonVec.replicate BUTTON_ZONE_COUNT 0
  sensorUsed : SensorVec Nat := SensorVec.replicate SENSOR_AREA_COUNT 0
deriving Inhabited

private def tryUseButtonClickAt (input : FrameInput) (cursor : ClickCursor) (zone : ButtonZone) : Bool × ClickCursor :=
  let used := cursor.buttonUsed.getD zone 0
  let available := input.getButtonClickCount zone
  if used < available then
    (true, { cursor with buttonUsed := cursor.buttonUsed.set zone (used + 1) })
  else
    (false, cursor)

private def tryUseSensorClickAt (input : FrameInput) (cursor : ClickCursor) (area : SensorArea) : Bool × ClickCursor :=
  let used := cursor.sensorUsed.getD area 0
  let available := input.getSensorClickCount area
  if used < available then
    (true, { cursor with sensorUsed := cursor.sensorUsed.set area (used + 1) })
  else
    (false, cursor)

private def fallbackSensorAreaForButtonNote (zone : ButtonZone) : SensorArea :=
  zone.toOuterSensorArea

private def fallbackSensorHeldForButtonNote (input : FrameInput) (zone : ButtonZone) : Bool :=
  input.getSensorHeld (fallbackSensorAreaForButtonNote zone)

private def fallbackPrevSensorHeldForButtonNote (prevSensor : SensorVec Bool) (zone : ButtonZone) : Bool :=
  InputModel.prevSensorHeldAt prevSensor (fallbackSensorAreaForButtonNote zone)

private def listSetAt : List α → Nat → α → List α
  | [], _, _ => []
  | _ :: rest, 0, value => value :: rest
  | head :: rest, index + 1, value => head :: listSetAt rest index value

private def slideRemaining (slide : SlideNote) : Nat :=
  Lifecycle.slideQueueRemaining slide.judgeQueues

private def emptySlideQueues (slide : SlideNote) : SlideNote :=
  { slide with judgeQueues := slide.judgeQueues.map (fun _ => []) }

private def shouldForceFinishParent (parent child : SlideNote) : Bool :=
  parent.isConnSlide && !parent.isGroupPartEnd && !child.parentFinished &&
  slideRemaining child < child.initialQueueRemaining

private def updateSlideParentFlags (slides : List SlideNote) : List SlideNote :=
  let statuses := slides.map (fun slide => (slide.params.noteIndex, slideRemaining slide))
  let findRemaining? (noteIndex : Nat) : Option Nat :=
    statuses.findSome? (fun entry => if entry.1 = noteIndex then some entry.2 else none)
  slides.map (fun slide =>
    match slide.parentNoteIndex with
    | none => { slide with parentFinished := false, parentPendingFinish := false }
    | some parentIndex =>
        match findRemaining? parentIndex with
        | none => { slide with parentFinished := false, parentPendingFinish := false }
        | some remaining =>
            { slide with parentFinished := remaining == 0, parentPendingFinish := remaining == 1 })

private theorem updateSlideParentFlags_length (slides : List SlideNote) :
    (updateSlideParentFlags slides).length = slides.length := by
  simp [updateSlideParentFlags]

private def forceFinishParentSlides (slides : List SlideNote) : List SlideNote :=
  let childRequests := slides.foldl (fun acc child =>
    match child.parentNoteIndex with
    | none => acc
    | some parentIndex =>
        if slideRemaining child < child.initialQueueRemaining && !child.parentFinished then
          parentIndex :: acc
        else
          acc) []
  slides.map (fun slide =>
    if slide.isConnSlide && !slide.isGroupPartEnd && childRequests.contains slide.params.noteIndex then
      emptySlideQueues slide
    else
      slide)

private def hideSlideRenderCmds (slide : SlideNote) : List RenderCommand :=
  match slide.slideKind with
  | SlideKind.Single => [RenderCommand.HideAllSlideBars slide.params.noteIndex]
  | SlideKind.Wifi | SlideKind.ConnPart => [RenderCommand.HideAllSlideBars slide.params.noteIndex]

private def forceFinishRenderCmds (before after : List SlideNote) : List RenderCommand :=
  let rec go (before after : List SlideNote) : List RenderCommand :=
    match before, after with
    | [], _ => []
    | _, [] => []
    | beforeSlide :: beforeRest, afterSlide :: afterRest =>
      let rest := go beforeRest afterRest
      if slideRemaining beforeSlide > 0 && slideRemaining afterSlide == 0 then
        hideSlideRenderCmds afterSlide ++ rest
      else
        rest
  go before after

----------------------------------------------------------------------------
-- Active Notes (all types pooled together for one frame)
----------------------------------------------------------------------------

inductive ActiveNote where
  | tapNote   : TapNote → ActiveNote
  | holdNote  : HoldNote → ActiveNote
  | touchNote : TouchNote → ActiveNote
  | slideNote : SlideNote → ActiveNote
deriving Inhabited

private def tapEligibleForClick (note : TapNote) (currentTime : TimePoint) : Bool :=
  let timing := note.params.effectiveTiming
  currentTime ≥ timing - JUDGABLE_RANGE_SEC

private def touchEligibleForClick (note : TouchNote) (currentTime : TimePoint) : Bool :=
  let timing := note.params.effectiveTiming
  currentTime ≥ timing - JUDGABLE_RANGE_SEC

----------------------------------------------------------------------------
-- Process tap notes
----------------------------------------------------------------------------

private def processTapNotes (queues : ButtonQueueVec TapNote) (input : FrameInput) (currentTime : TimePoint) (touchPanelOffset : Duration) (style : JudgeStyle) (cursor : ClickCursor) : ButtonQueueVec TapNote × List JudgeEvent × ClickCursor :=
  let (nextQueues, (cursor', evsRev)) :=
    queues.mapAccum (cursor, ([] : List JudgeEvent)) (fun zone q state =>
      let (cursor, evsRev) := state
      match q.peek with
      | none => (q, (cursor, evsRev))
      | some note =>
        let timing := note.params.effectiveTiming
        let buttonDiff := currentTime - timing
        let sensorDiff := (currentTime - touchPanelOffset) - timing
        let canConsumeClick := tapEligibleForClick note currentTime
        let (usedButton, cursor1) :=
          if canConsumeClick then tryUseButtonClickAt input cursor zone else (false, cursor)
        let (clicked, diff, cursor2) :=
          if usedButton then (true, buttonDiff, cursor1)
          else
            let (usedSensor, cursorS) :=
              if canConsumeClick then
                tryUseSensorClickAt input cursor1 (fallbackSensorAreaForButtonNote note.lane.toButtonZone)
              else
                (false, cursor1)
            if usedSensor then (true, sensorDiff, cursorS) else (false, buttonDiff, cursorS)
        match tapStep note currentTime diff clicked style with
        | (newNote, some evt) =>
            let nextQueue :=
              match newNote.state with
              | Lifecycle.TapState.Ended => q.advance
              | _ => { q with notes := listSetAt q.notes q.currentIndex newNote }
            (nextQueue, (cursor2, evt :: evsRev))
        | (newNote, none) =>
            let nextQueue := { q with notes := listSetAt q.notes q.currentIndex newNote }
            (nextQueue, (cursor2, evsRev)))
  (nextQueues, evsRev.reverse, cursor')

----------------------------------------------------------------------------
-- Process hold notes
----------------------------------------------------------------------------

private def isHeadJudgedState : HoldSubState → Bool
  | .HeadJudged _ => true
  | _ => false

private def enteredHeadJudged (before after : HoldSubState) : Bool :=
  !isHeadJudgedState before && isHeadJudgedState after

private def keepHoldActive (note : HoldNote) : Bool :=
  match note.state with
  | .HeadWaiting | .HeadJudgeable | .HeadJudged _ | .BodyHeld | .BodyReleased => true
  | .Ended _ => false

private def queueHeadMatches (queue : ZoneQueue HoldNote) (note : HoldNote) : Bool :=
  match queue.peek with
  | some head => head.params.noteIndex == note.params.noteIndex
  | none => false

private def advanceButtonQueueIfHead (queues : ButtonQueueVec HoldNote) (zone : ButtonZone) (note : HoldNote) : ButtonQueueVec HoldNote :=
  let queue := InputModel.buttonQueueAt queues zone
  if queueHeadMatches queue note then
    InputModel.setButtonQueueAt queues zone queue.advance
  else
    queues

private def advanceSensorQueueIfHead (queues : SensorQueueVec HoldNote) (area : SensorArea) (note : HoldNote) : SensorQueueVec HoldNote :=
  let queue := InputModel.sensorQueueAt queues area
  if queueHeadMatches queue note then
    InputModel.setSensorQueueAt queues area queue.advance
  else
    queues

private def touchHoldGroupTriggeredCount (holds : List (SensorArea × HoldNote)) (groupId : Nat) : Nat :=
  (holds.filter (fun note => note.2.touchHoldGroupId == some groupId && note.2.touchHoldGroupTriggered)).length

private def hasStrictMajority (count size : Nat) : Bool :=
  size > 0 && count * 2 > size

private def groupShareResult (groups : List GroupState) (groupId : Nat) : Option (JudgeGrade × Duration) :=
  match groups.find? (fun group => group.groupId == groupId) with
  | some group =>
      if hasStrictMajority group.count group.size then some (group.grade, group.diff) else none
  | none => none

private def updateGroupState (groups : List GroupState) (groupId : Nat) (groupSize : Nat) (grade : JudgeGrade) (diff : Duration) : List GroupState :=
  let rec loop (items : List GroupState) : List GroupState :=
    match items with
    | [] => [{ groupId := groupId, count := 1, size := groupSize, grade := grade, diff := diff }]
    | group :: rest =>
      if group.groupId == groupId then
        { group with count := group.count + 1, size := groupSize, grade := grade, diff := diff } :: rest
      else
        group :: loop rest
  loop groups

private def touchQueueIndexMatches {α : Type} (queue : ZoneQueue α) (index : Nat) : Bool :=
  queue.currentIndex == index

private def advanceSharedTouchQueueIfCurrent {α : Type} (queues : SensorQueueVec α) (area : SensorArea) (index : Nat) : SensorQueueVec α :=
  let queue := InputModel.sensorQueueAt queues area
  if touchQueueIndexMatches queue index then
    InputModel.setSensorQueueAt queues area queue.advance
  else
    queues

private def processHoldNotes (queues : ButtonQueueVec HoldNote) (holds : List (ButtonZone × HoldNote)) (input : FrameInput) (currentTime : TimePoint) (delta : Duration) (style : JudgeStyle) (touchPanelOffset : Duration) (prevSensor : SensorVec Bool) (cursor : ClickCursor) : ButtonQueueVec HoldNote × List (ButtonZone × HoldNote) × List JudgeEvent × ClickCursor :=
  match holds with
  | [] => (queues, [], [], cursor)
  | (zone, note) :: rest =>
    let timing := note.params.effectiveTiming
    let buttonDiff := currentTime - timing
    let sensorDiff := (currentTime - touchPanelOffset) - timing
    let currentButtonPressed := input.getButtonHeld zone
    let currentSensorPressed := fallbackSensorHeldForButtonNote input zone
    let prevSensorPressed := fallbackPrevSensorHeldForButtonNote prevSensor zone
    let allowInput := queueHeadMatches (InputModel.buttonQueueAt queues zone) note
    let (usedButton, cursor1) := if allowInput then tryUseButtonClickAt input cursor zone else (false, cursor)
    let (usedSensor, cursor2) :=
      if allowInput && !usedButton then
        tryUseSensorClickAt input cursor1 (fallbackSensorAreaForButtonNote zone)
      else
        (false, cursor1)
    let clicked := usedButton || usedSensor
    let diff := if usedButton then buttonDiff else sensorDiff
    let (newNote, evt?) :=
      holdStep note currentTime diff HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked (currentButtonPressed || currentSensorPressed) currentButtonPressed prevSensorPressed touchPanelOffset none delta style
    let queues' := if enteredHeadJudged note.state newNote.state then advanceButtonQueueIfHead queues zone newNote else queues
    let (restQueues, restNotes, restEvs, cursor3) := processHoldNotes queues' rest input currentTime delta style touchPanelOffset prevSensor cursor2
    let restNotes' := if keepHoldActive newNote then (zone, newNote) :: restNotes else restNotes
    match evt? with
    | some evt =>
      (restQueues, restNotes', evt :: restEvs, cursor3)
    | none =>
      (restQueues, restNotes', restEvs, cursor3)

private def processTouchHoldNotes (touchQueues : SensorQueueVec TouchNote) (queues : SensorQueueVec HoldNote) (holds : List (SensorArea × HoldNote)) (input : FrameInput) (currentTime : TimePoint) (delta : Duration) (style : JudgeStyle) (touchPanelOffset : Duration) (useButtonRingForTouch : Bool) (cursor : ClickCursor) (groupStates : List GroupState) : SensorQueueVec HoldNote × List (SensorArea × HoldNote) × List JudgeEvent × ClickCursor × List GroupState :=
  match holds with
  | [] => (queues, [], [], cursor, groupStates)
  | (area, note) :: rest =>
    let timing := note.params.effectiveTiming
    let buttonDiff := currentTime - timing
    let sensorDiff := (currentTime - touchPanelOffset) - timing
    let groupTriggeredCount :=
      match note.touchHoldGroupId with
      | some groupId =>
          touchHoldGroupTriggeredCount holds groupId
      | none => 0
    let effectivePressed := input.getSensorHeld area || hasStrictMajority groupTriggeredCount note.touchHoldGroupSize
    let allowInput := touchQueueIndexMatches (InputModel.sensorQueueAt touchQueues area) note.touchQueueIndex
    let (usedButton, cursor1) :=
      if allowInput && useButtonRingForTouch then
        match area.toOuterButtonZone? with
        | some zone => tryUseButtonClickAt input cursor zone
        | none => (false, cursor)
      else
        (false, cursor)
    let (usedSensor, cursor1) := if usedButton then (false, cursor1) else if allowInput then tryUseSensorClickAt input cursor1 area else (false, cursor1)
    let sharedResult :=
      match note.touchHoldGroupId with
      | some groupId =>
        match groupShareResult groupStates groupId with
        | some shared => some shared
        | none => none
      | none => none
    let headDiff := if usedButton then buttonDiff else sensorDiff
    let (newNote, evt?) :=
      holdStep note currentTime headDiff TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC (usedButton || usedSensor) effectivePressed usedButton false touchPanelOffset sharedResult delta style
    let queues' := if enteredHeadJudged note.state newNote.state then advanceSensorQueueIfHead queues area newNote else queues
    let groupStates' :=
      match evt?, note.touchHoldGroupId with
      | some evt, some groupId =>
        if evt.grade.isMissOrTooFast then groupStates else updateGroupState groupStates groupId note.touchHoldGroupSize evt.grade newNote.headDiff
      | _, _ =>
        match newNote.state with
        | HoldSubState.HeadJudged grade =>
          if grade.isMissOrTooFast then groupStates else
            match note.touchHoldGroupId with
            | some groupId => updateGroupState groupStates groupId note.touchHoldGroupSize grade newNote.headDiff
            | none => groupStates
        | _ => groupStates
    let (restQueues, restNotes, restEvs, cursor2, restGroups) := processTouchHoldNotes touchQueues queues' rest input currentTime delta style touchPanelOffset useButtonRingForTouch cursor1 groupStates'
    let restNotes' := if keepHoldActive newNote then (area, newNote) :: restNotes else restNotes
    match evt? with
    | some evt =>
      (restQueues, restNotes', evt :: restEvs, cursor2, restGroups)
    | none =>
      (restQueues, restNotes', restEvs, cursor2, restGroups)

----------------------------------------------------------------------------
-- Process touch notes
----------------------------------------------------------------------------

private def processTouchNotes (queues : SensorQueueVec TouchNote) (input : FrameInput) (currentTime : TimePoint) (style : JudgeStyle) (cursor : ClickCursor) (useButtonRingForTouch : Bool) (touchPanelOffset : Duration) (groupStates : List GroupState) : SensorQueueVec TouchNote × List JudgeEvent × ClickCursor × List GroupState :=
  let (nextQueues, (cursor', groups', evsRev)) :=
    queues.mapAccum (cursor, groupStates, ([] : List JudgeEvent)) (fun area q state =>
      let (cursor, groups, evsRev) := state
      match q.peek with
      | none => (q, (cursor, groups, evsRev))
      | some note =>
        let timing := note.params.effectiveTiming
        let buttonDiff := currentTime - timing
        let sensorDiff := (currentTime - touchPanelOffset) - timing
        let canConsumeClick := touchEligibleForClick note currentTime
        let (usedButton, cursor1) :=
          if canConsumeClick && useButtonRingForTouch then
            match area.toOuterButtonZone? with
            | some zone => tryUseButtonClickAt input cursor zone
            | none => (false, cursor)
          else
            (false, cursor)
        let (usedSensor, cursor2) :=
          if usedButton then (false, cursor1)
          else
            if canConsumeClick then tryUseSensorClickAt input cursor1 note.sensorPos else (false, cursor1)
        let clicked := usedButton || usedSensor
        let diff := if usedButton then buttonDiff else sensorDiff
        let sharedResult :=
          match note.touchGroupId with
          | some groupId => groupShareResult groups groupId
          | none => none
        match touchStep note currentTime diff clicked sharedResult style with
        | (newNote, some evt) =>
          let groups' :=
            if evt.grade.isMissOrTooFast then groups
            else
              match note.touchGroupId with
              | some groupId => updateGroupState groups groupId note.touchGroupSize evt.grade diff
              | none => groups
          let nextQueue := if touchQueueIndexMatches q q.currentIndex then q.advance else q
          (nextQueue, (cursor2, groups', evt :: evsRev))
        | (newNote, none) =>
          let groups' :=
            match note.touchGroupId, newNote.state with
            | some groupId, TouchState.Ended => groups
            | some groupId, TouchState.Judged grade =>
              if grade.isMissOrTooFast then groups else updateGroupState groups groupId note.touchGroupSize grade diff
            | _, _ => groups
          (q, (cursor2, groups', evsRev)))
  (nextQueues, evsRev.reverse, cursor', groups')

----------------------------------------------------------------------------
-- Process slide notes
----------------------------------------------------------------------------

partial def processSlideNotesCore (processedRev pending : List SlideNote)
    (input : FrameInput) (currentTime : TimePoint) (touchPanelOffset : Duration) (delta : Duration)
    (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool)
    (eventsRev : List JudgeEvent) (audioRev : List AudioCommand) (renderRev : List RenderCommand) :
    List SlideNote × List JudgeEvent × List AudioCommand × List RenderCommand :=
  match pending with
  | [] => (processedRev.reverse, eventsRev.reverse, audioRev.reverse, renderRev.reverse)
  | note :: rest =>
      match slideStep note currentTime input.sensorHeld touchPanelOffset delta style subdivideSlideJudgeGrade with
      | (newNote, evt?, audioCmds, renderCmds) =>
          let updatedPending : List SlideNote :=
            match updateSlideParentFlags (newNote :: rest) with
            | [] => []
            | _current :: updatedRest => updatedRest
          let processedRev := newNote :: processedRev
          let eventsRev := match evt? with | some evt => evt :: eventsRev | none => eventsRev
          let audioRev := audioCmds.reverse ++ audioRev
          let renderRev := renderCmds.reverse ++ renderRev
          processSlideNotesCore processedRev updatedPending input currentTime touchPanelOffset delta style subdivideSlideJudgeGrade eventsRev audioRev renderRev
private def processSlideNotes (slides : List SlideNote) (input : FrameInput) (currentTime : TimePoint) (touchPanelOffset : Duration) (delta : Duration) (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool) : List SlideNote × List JudgeEvent × List AudioCommand × List RenderCommand :=
  processSlideNotesCore [] slides input currentTime touchPanelOffset delta style subdivideSlideJudgeGrade [] [] []

----------------------------------------------------------------------------
-- Score Accumulation from Events
----------------------------------------------------------------------------

private def foldEventIntoScore (s : ScoreState) (evt : JudgeEvent) : ScoreState :=
  let multiple : Nat := 1
  let comboDelta := Score.updateCombo s.combo s.pCombo s.cPCombo s.dxScore evt.grade multiple
  let counts := match evt.kind with
    | .Tap   => { s.counts with tapCount   := λ g => if g == evt.grade then s.counts.tapCount g + 1 else s.counts.tapCount g }
    | .Hold  => { s.counts with holdCount  := λ g => if g == evt.grade then s.counts.holdCount g + 1 else s.counts.holdCount g }
    | .Slide => { s.counts with slideCount := λ g => if g == evt.grade then s.counts.slideCount g + 1 else s.counts.slideCount g }
    | .Touch => { s.counts with touchCount := λ g => if g == evt.grade then s.counts.touchCount g + 1 else s.counts.touchCount g }
    | .Break => { s.counts with breakCount := λ g => if g == evt.grade then s.counts.breakCount g + 1 else s.counts.breakCount g }
  { s with
    combo       := comboDelta.combo
    pCombo      := comboDelta.pCombo
    cPCombo     := comboDelta.cPCombo
    dxScore     := comboDelta.dXScoreLost
    counts      := counts
  }

private def foldEventsIntoScore (s : ScoreState) (events : List JudgeEvent) : ScoreState :=
  match events with
  | [] => s
  | evt :: rest => foldEventsIntoScore (foldEventIntoScore s evt) rest

private def eventToAudioCommands (evt : JudgeEvent) (timePoint : TimePoint) : List AudioCommand :=
  [ AudioCommand.PlayJudgeSfx evt.kind evt.grade timePoint evt.noteIndex ]

private def eventToRenderCommands (evt : JudgeEvent) : List RenderCommand :=
  [ RenderCommand.ShowJudgeResult evt.kind evt.grade evt.diff evt.noteIndex ]

private def eventsToAudioCommands (events : List JudgeEvent) (timePoint : TimePoint) : List AudioCommand :=
  match events with
  | [] => []
  | evt :: rest => eventToAudioCommands evt timePoint ++ eventsToAudioCommands rest timePoint

private def eventsToRenderCommands (events : List JudgeEvent) : List RenderCommand :=
  match events with
  | [] => []
  | evt :: rest => eventToRenderCommands evt ++ eventsToRenderCommands rest

----------------------------------------------------------------------------
-- Frame Step: advance all active notes one frame (entry point)
----------------------------------------------------------------------------

def stepFrame (st : GameState) (input : FrameInput) : GameState × List JudgeEvent × List AudioCommand × List RenderCommand :=
  let newTime := st.currentTime + input.delta
  let cursor : ClickCursor := {}
  let resolvedSlides := updateSlideParentFlags st.slides

  -- Semantic order is deliberate; see module comment above.
  let (tapNotes, tapEvents, cursorTap) :=
    processTapNotes st.tapQueues input newTime st.touchPanelOffset st.judgeStyle cursor
  let (holdQueues, holdNotes, holdEvents, cursor1) :=
    processHoldNotes st.holdQueues st.activeHolds input newTime input.delta st.judgeStyle st.touchPanelOffset st.prevSensor cursorTap
  let (touchNotes, touchEvents, cursor2, touchGroupStates) :=
    processTouchNotes st.touchQueues input newTime st.judgeStyle cursor1 st.useButtonRingForTouch st.touchPanelOffset st.touchGroupStates
  let (touchHoldQueues, touchHoldNotes, touchHoldEvents, _cursor3, touchHoldGroupStates) :=
    processTouchHoldNotes touchNotes st.touchHoldQueues st.activeTouchHolds input newTime input.delta st.judgeStyle st.touchPanelOffset st.useButtonRingForTouch cursor2 touchGroupStates
  let (slideNotes, slideEvents, slideAudioCommands, slideRenderCommands) :=
    processSlideNotes resolvedSlides input newTime st.touchPanelOffset input.delta st.judgeStyle st.subdivideSlideJudgeGrade
  let slideNotes := forceFinishParentSlides slideNotes
  let slideNotes := updateSlideParentFlags slideNotes
  let forceFinishCommands := forceFinishRenderCmds resolvedSlides slideNotes

  let allEvents := tapEvents ++ holdEvents ++ touchHoldEvents ++ touchEvents ++ slideEvents
  let newScore := foldEventsIntoScore st.score allEvents
  let audioCommands := slideAudioCommands ++ eventsToAudioCommands allEvents newTime
  let renderCommands := slideRenderCommands ++ forceFinishCommands ++ eventsToRenderCommands allEvents

  ({ st with
      currentTime := newTime
    , prevButton  := input.buttonHeld
    , prevSensor  := input.sensorHeld
    , tapQueues   := tapNotes
    , holdQueues  := holdQueues
    , touchHoldQueues := touchHoldQueues
    , touchQueues := touchNotes
    , score       := newScore
    , slides      := slideNotes
    , activeHolds := holdNotes
    , activeTouchHolds := touchHoldNotes
    , touchGroupStates := touchGroupStates
    , touchHoldGroupStates := touchHoldGroupStates
  }, allEvents, audioCommands, renderCommands)

def stepFrameTimed (st : GameState) (batch : TimedInputBatch) : GameState × List JudgeEvent × List AudioCommand × List RenderCommand :=
  let input := batch.toFrameInput (batch.currentTime - st.currentTime) st.prevButton st.prevSensor
  let (nextState, events, audioCommands, renderCommands) := stepFrame { st with currentBatch := batch } input
  ({ nextState with currentBatch := batch }, events, audioCommands, renderCommands)

end LnmaiCore.Scheduler
