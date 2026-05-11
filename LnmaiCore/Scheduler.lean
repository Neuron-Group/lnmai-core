/-
  Frame Scheduler — step one frame of gameplay.

  The Core receives all active notes and frame input, advances each
  note's lifecycle, and returns updated notes plus a list of JudgeEvents.
-/

import LnmaiCore.Types
import LnmaiCore.Constants
import LnmaiCore.Judge
import LnmaiCore.Convert
import LnmaiCore.Score
import LnmaiCore.Lifecycle
import LnmaiCore.InputModel

set_option linter.unusedVariables false

namespace LnmaiCore.Scheduler

open Constants
open InputModel
open Lifecycle
open Score
open LnmaiCore

structure ClickCursor where
  buttonUsed : List Nat := List.replicate BUTTON_ZONE_COUNT 0
  sensorUsed : List Nat := List.replicate SENSOR_AREA_COUNT 0
deriving Inhabited

private def getUsed (xs : List Nat) (i : Nat) : Nat :=
  (xs[i]?).getD 0

private def setUsed : List Nat → Nat → Nat → List Nat
  | [], _, _ => []
  | _ :: rest, 0, v => v :: rest
  | x :: rest, i+1, v => x :: setUsed rest i v

private def tryUseButtonClick (input : FrameInput) (cursor : ClickCursor) (zone : Nat) : Bool × ClickCursor :=
  let used := getUsed cursor.buttonUsed zone
  let available := input.getButtonClickCount zone
  if used < available then
    (true, { cursor with buttonUsed := setUsed cursor.buttonUsed zone (used + 1) })
  else
    (false, cursor)

private def tryUseSensorClick (input : FrameInput) (cursor : ClickCursor) (area : Nat) : Bool × ClickCursor :=
  let used := getUsed cursor.sensorUsed area
  let available := input.getSensorClickCount area
  if used < available then
    (true, { cursor with sensorUsed := setUsed cursor.sensorUsed area (used + 1) })
  else
    (false, cursor)

private def getSlideByIndex (slides : List SlideNote) (noteIndex : Nat) : Option SlideNote :=
  slides.find? (fun slide => slide.params.noteIndex == noteIndex)

private def slideRemaining (slide : SlideNote) : Nat :=
  Lifecycle.slideQueueRemaining slide.judgeQueues

private def emptySlideQueues (slide : SlideNote) : SlideNote :=
  { slide with judgeQueues := slide.judgeQueues.map (fun _ => []) }

private def resolveSlideLinks (slides : List SlideNote) : List SlideNote :=
  slides.map (fun slide =>
    match slide.parentNoteIndex with
    | none => slide
    | some parentIndex =>
      match getSlideByIndex slides parentIndex with
      | none => slide
      | some parent =>
        let remaining := slideRemaining parent
        { slide with parentFinished := remaining == 0, parentPendingFinish := remaining == 1 })

private def shouldForceFinishParent (parent child : SlideNote) : Bool :=
  parent.isConnSlide && !parent.isGroupPartEnd && !child.parentFinished &&
  slideRemaining child < child.initialQueueRemaining

private def forceFinishParentPass (baseSlides : List SlideNote) (slides : List SlideNote) : List SlideNote :=
  slides.map (fun slide =>
    let hasActiveChild := baseSlides.any (fun child =>
      match child.parentNoteIndex with
      | some parentIndex => parentIndex == slide.params.noteIndex && shouldForceFinishParent slide child
      | none => false)
    if hasActiveChild then emptySlideQueues slide else slide)

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

private def iterateForceFinishParents : Nat → List SlideNote → List SlideNote
  | 0, slides => slides
  | n+1, slides =>
      let updated := forceFinishParentPass slides slides
      iterateForceFinishParents n updated

----------------------------------------------------------------------------
-- Active Notes (all types pooled together for one frame)
----------------------------------------------------------------------------

inductive ActiveNote where
  | tapNote   : TapNote → ActiveNote
  | holdNote  : HoldNote → ActiveNote
  | touchNote : TouchNote → ActiveNote
  | slideNote : SlideNote → ActiveNote
deriving Inhabited

----------------------------------------------------------------------------
-- Process tap notes
----------------------------------------------------------------------------

private def processTapNotes (queues : List (ZoneQueue TapNote)) (input : FrameInput) (currentSec : Float) (touchPanelOffsetSec : Float) (style : JudgeStyle) (cursor : ClickCursor) : List (ZoneQueue TapNote) × List JudgeEvent × ClickCursor :=
  let rec loop (i : Nat) (qs : List (ZoneQueue TapNote)) (evs : List JudgeEvent) (cursor : ClickCursor) :=
    match qs with
    | [] => ([], evs, cursor)
    | q :: rest =>
      match q.peek with
      | none => loop (i+1) rest evs cursor
      | some note =>
        let timing := note.params.effectiveTiming
        let buttonDiffMs := (currentSec - timing) * 1000.0
        let sensorDiffMs := (currentSec - touchPanelOffsetSec - timing) * 1000.0
        let (usedButton, cursor1) := tryUseButtonClick input cursor i
        let (clicked, diffMs, cursor2) :=
          if usedButton then (true, buttonDiffMs, cursor1)
          else
            let (usedSensor, cursorS) := tryUseSensorClick input cursor1 note.params.startPos
            if usedSensor then (true, sensorDiffMs, cursorS) else (false, buttonDiffMs, cursorS)
        match tapStep note currentSec diffMs clicked style with
        | (_, some evt) =>
          let (restQs, restEvs, cursor3) := loop (i+1) rest evs cursor2
          (q.advance :: restQs, evt :: restEvs, cursor3)
        | (_, none) =>
          let (restQs, restEvs, cursor3) := loop (i+1) rest evs cursor2
          (q :: restQs, restEvs, cursor3)
  loop 0 queues [] cursor

----------------------------------------------------------------------------
-- Process hold notes
----------------------------------------------------------------------------

private def zoneQueueAt {α : Type} (queues : List (ZoneQueue α)) (index : Nat) : ZoneQueue α :=
  (queues[index]?).getD { notes := [] }

private def setZoneQueueAt {α : Type} (queues : List (ZoneQueue α)) (index : Nat) (queue : ZoneQueue α) : List (ZoneQueue α) :=
  match queues, index with
  | [], _ => []
  | _ :: rest, 0 => queue :: rest
  | head :: rest, index + 1 => head :: setZoneQueueAt rest index queue

private def isHeadJudgedState : HoldSubState → Bool
  | .HeadJudged _ => true
  | _ => false

private def enteredHeadJudged (before after : HoldSubState) : Bool :=
  !isHeadJudgedState before && isHeadJudgedState after

private def queueHeadMatches (queue : ZoneQueue HoldNote) (note : HoldNote) : Bool :=
  match queue.peek with
  | some head => head.params.noteIndex == note.params.noteIndex
  | none => false

private def advanceQueueIfHead (queues : List (ZoneQueue HoldNote)) (zone : Nat) (note : HoldNote) : List (ZoneQueue HoldNote) :=
  let queue := zoneQueueAt queues zone
  if queueHeadMatches queue note then
    setZoneQueueAt queues zone queue.advance
  else
    queues

private def touchHoldGroupTriggeredCount (holds : List (Nat × HoldNote)) (groupId : Nat) : Nat :=
  (holds.filter (fun note => note.2.touchHoldGroupId == some groupId && note.2.touchHoldGroupTriggered)).length

private def groupShareResult (groups : List GroupState) (groupId : Nat) : Option (JudgeGrade × Float) :=
  match groups.find? (fun group => group.groupId == groupId) with
  | some group =>
      if group.size > 0 && group.count * 2 > group.size then some (group.grade, group.diffMs) else none
  | none => none

private def updateGroupState (groups : List GroupState) (groupId : Nat) (groupSize : Nat) (grade : JudgeGrade) (diffMs : Float) : List GroupState :=
  let rec loop (items : List GroupState) : List GroupState :=
    match items with
    | [] => [{ groupId := groupId, count := 1, size := groupSize, grade := grade, diffMs := diffMs }]
    | group :: rest =>
      if group.groupId == groupId then
        { group with count := group.count + 1, size := groupSize, grade := grade, diffMs := diffMs } :: rest
      else
        group :: loop rest
  loop groups

private def processHoldNotes (queues : List (ZoneQueue HoldNote)) (holds : List (Nat × HoldNote)) (input : FrameInput) (currentSec : Float) (deltaSec : Float) (style : JudgeStyle) (touchPanelOffsetSec : Float) (prevSensor : List Bool) (cursor : ClickCursor) : List (ZoneQueue HoldNote) × List (Nat × HoldNote) × List JudgeEvent × ClickCursor :=
  match holds with
  | [] => (queues, [], [], cursor)
  | (zone, note) :: rest =>
    let timing := note.params.effectiveTiming
    let buttonDiffMs := (currentSec - timing) * 1000.0
    let sensorDiffMs := (currentSec - touchPanelOffsetSec - timing) * 1000.0
    let currentButtonPressed := input.getButtonHeld zone
    let currentSensorPressed := input.getSensorHeld note.params.startPos
    let prevSensorPressed := (prevSensor[note.params.startPos]?).getD false
    let allowInput := queueHeadMatches (zoneQueueAt queues zone) note
    let (usedButton, cursor1) := if allowInput then tryUseButtonClick input cursor zone else (false, cursor)
    let (usedSensor, cursor2) := if allowInput && !usedButton then tryUseSensorClick input cursor1 note.params.startPos else (false, cursor1)
    let clicked := usedButton || usedSensor
    let diffMs := if usedButton then buttonDiffMs else sensorDiffMs
    let (newNote, evt?) :=
      holdStep note currentSec diffMs HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked (currentButtonPressed || currentSensorPressed) currentButtonPressed prevSensorPressed touchPanelOffsetSec none deltaSec style
    let queues' := if enteredHeadJudged note.state newNote.state then advanceQueueIfHead queues zone newNote else queues
    let (restQueues, restNotes, restEvs, cursor3) := processHoldNotes queues' rest input currentSec deltaSec style touchPanelOffsetSec prevSensor cursor2
    match evt? with
    | some evt =>
      (restQueues, restNotes, evt :: restEvs, cursor3)
    | none =>
      (restQueues, (zone, newNote) :: restNotes, restEvs, cursor3)

private def processTouchHoldNotes (queues : List (ZoneQueue HoldNote)) (holds : List (Nat × HoldNote)) (input : FrameInput) (currentSec : Float) (deltaSec : Float) (style : JudgeStyle) (touchPanelOffsetSec : Float) (useButtonRingForTouch : Bool) (cursor : ClickCursor) (groupStates : List GroupState) : List (ZoneQueue HoldNote) × List (Nat × HoldNote) × List JudgeEvent × ClickCursor × List GroupState :=
  match holds with
  | [] => (queues, [], [], cursor, groupStates)
  | (area, note) :: rest =>
    let timing := note.params.effectiveTiming
    let buttonDiffMs := (currentSec - timing) * 1000.0
    let sensorDiffMs := (currentSec - touchPanelOffsetSec - timing) * 1000.0
    let groupPercent :=
      match note.touchHoldGroupId with
      | some groupId =>
          if note.touchHoldGroupSize == 0 then 0.0 else Float.ofNat (touchHoldGroupTriggeredCount holds groupId) / Float.ofNat note.touchHoldGroupSize
      | none => 0.0
    let effectivePressed := input.getSensorHeld area || groupPercent > 0.5
    let allowInput := queueHeadMatches (zoneQueueAt queues area) note
    let buttonAvailable := useButtonRingForTouch && area < BUTTON_ZONE_COUNT
    let (usedButton, cursor1) := if allowInput && buttonAvailable then tryUseButtonClick input cursor area else (false, cursor)
    let (usedSensor, cursor1) := if usedButton then (false, cursor1) else if allowInput then tryUseSensorClick input cursor1 area else (false, cursor1)
    let sharedResult :=
      match note.touchHoldGroupId with
      | some groupId =>
        match groupShareResult groupStates groupId with
        | some shared => some shared
        | none => none
      | none => none
    let headDiffMs := if usedButton then buttonDiffMs else sensorDiffMs
    let (newNote, evt?) :=
      holdStep note currentSec headDiffMs TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC (usedButton || usedSensor) effectivePressed usedButton false touchPanelOffsetSec sharedResult deltaSec style
    let queues' := if enteredHeadJudged note.state newNote.state then advanceQueueIfHead queues area newNote else queues
    let groupStates' :=
      match evt?, note.touchHoldGroupId with
      | some evt, some groupId =>
        if evt.grade.isMissOrTooFast then groupStates else updateGroupState groupStates groupId note.touchHoldGroupSize evt.grade evt.diffMs
      | _, _ =>
        match newNote.state with
        | HoldSubState.HeadJudged grade =>
          if grade.isMissOrTooFast then groupStates else
            match note.touchHoldGroupId with
            | some groupId => updateGroupState groupStates groupId note.touchHoldGroupSize grade newNote.headDiffMs
            | none => groupStates
        | _ => groupStates
    let (restQueues, restNotes, restEvs, cursor2, restGroups) := processTouchHoldNotes queues' rest input currentSec deltaSec style touchPanelOffsetSec useButtonRingForTouch cursor1 groupStates'
    match evt? with
    | some evt =>
      (restQueues, restNotes, evt :: restEvs, cursor2, restGroups)
    | none =>
      (restQueues, (area, newNote) :: restNotes, restEvs, cursor2, restGroups)

----------------------------------------------------------------------------
-- Process touch notes
----------------------------------------------------------------------------

private def processTouchNotes (queues : List (ZoneQueue TouchNote)) (input : FrameInput) (currentSec : Float) (style : JudgeStyle) (cursor : ClickCursor) (useButtonRingForTouch : Bool) (touchPanelOffsetSec : Float) (groupStates : List GroupState) : List (ZoneQueue TouchNote) × List JudgeEvent × ClickCursor × List GroupState :=
  let rec loop (i : Nat) (qs : List (ZoneQueue TouchNote)) (evs : List JudgeEvent) (cursor : ClickCursor) (groups : List GroupState) :=
    match qs with
    | [] => ([], evs, cursor, groups)
    | q :: rest =>
      match q.peek with
      | none => loop (i+1) rest evs cursor groups
      | some note =>
        let timing := note.params.effectiveTiming
        let buttonDiffMs := (currentSec - timing) * 1000.0
        let sensorDiffMs := (currentSec - touchPanelOffsetSec - timing) * 1000.0
        let buttonAvailable := useButtonRingForTouch && i < BUTTON_ZONE_COUNT
        let (usedButton, cursor1) := if buttonAvailable then tryUseButtonClick input cursor i else (false, cursor)
        let (usedSensor, cursor2) := if usedButton then (false, cursor1) else tryUseSensorClick input cursor1 note.params.startPos
        let clicked := usedButton || usedSensor
        let diffMs := if usedButton then buttonDiffMs else sensorDiffMs
        let sharedResult :=
          match note.touchGroupId with
          | some groupId =>
            match groupShareResult groups groupId with
            | some shared => some shared
            | none => none
          | none => none
        match touchStep note currentSec diffMs clicked sharedResult style with
        | (newNote, some evt) =>
          let groups' :=
            if evt.grade.isMissOrTooFast then groups
            else
              match note.touchGroupId with
              | some groupId => updateGroupState groups groupId note.touchGroupSize evt.grade evt.diffMs
              | none => groups
          let (restQs, restEvs, cursor3, restGroups) := loop (i+1) rest evs cursor2 groups'
          (q.advance :: restQs, evt :: restEvs, cursor3, restGroups)
        | (newNote, none) =>
          let groups' :=
            match note.touchGroupId, newNote.state with
            | some groupId, TouchState.Ended => groups
            | some groupId, TouchState.Judged grade =>
              if grade.isMissOrTooFast then groups else updateGroupState groups groupId note.touchGroupSize grade diffMs
            | _, _ => groups
          let (restQs, restEvs, cursor3, restGroups) := loop (i+1) rest evs cursor2 groups'
          (q :: restQs, restEvs, cursor3, restGroups)
  loop 0 queues [] cursor groupStates

----------------------------------------------------------------------------
-- Process slide notes
----------------------------------------------------------------------------

private def processSlideNotes (slides : List SlideNote) (input : FrameInput) (currentSec : Float) (deltaSec : Float) (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool) : List SlideNote × List JudgeEvent × List AudioCommand × List RenderCommand :=
  match slides with
  | [] => ([], [], [], [])
  | note :: rest =>
    match slideStep note currentSec input.sensorHeld deltaSec style subdivideSlideJudgeGrade with
    | (newNote, some evt, audioCmds, renderCmds) =>
      let (restSlides, restEvs, restAudio, restRender) := processSlideNotes rest input currentSec deltaSec style subdivideSlideJudgeGrade
      (newNote :: restSlides, evt :: restEvs, audioCmds ++ restAudio, renderCmds ++ restRender)
    | (newNote, none, audioCmds, renderCmds) =>
      let (restSlides, restEvs, restAudio, restRender) := processSlideNotes rest input currentSec deltaSec style subdivideSlideJudgeGrade
      (newNote :: restSlides, restEvs, audioCmds ++ restAudio, renderCmds ++ restRender)

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

private def eventToAudioCommands (evt : JudgeEvent) (atSec : Float) : List AudioCommand :=
  [ AudioCommand.PlayJudgeSfx evt.kind evt.grade atSec evt.noteIndex ]

private def eventToRenderCommands (evt : JudgeEvent) : List RenderCommand :=
  [ RenderCommand.ShowJudgeResult evt.kind evt.grade evt.diffMs evt.noteIndex ]

private def eventsToAudioCommands (events : List JudgeEvent) (atSec : Float) : List AudioCommand :=
  match events with
  | [] => []
  | evt :: rest => eventToAudioCommands evt atSec ++ eventsToAudioCommands rest atSec

private def eventsToRenderCommands (events : List JudgeEvent) : List RenderCommand :=
  match events with
  | [] => []
  | evt :: rest => eventToRenderCommands evt ++ eventsToRenderCommands rest

----------------------------------------------------------------------------
-- Frame Step: advance all active notes one frame (entry point)
----------------------------------------------------------------------------

def stepFrame (st : GameState) (input : FrameInput) : GameState × List JudgeEvent × List AudioCommand × List RenderCommand :=
  let newTime := st.currentTime + input.deltaSec
  let cursor : ClickCursor := {}
  let resolvedSlides := resolveSlideLinks st.slides

  let (tapNotes, tapEvents) :=
    let (notes, evs, _) := processTapNotes st.tapQueues input newTime st.touchPanelOffsetSec st.judgeStyle cursor
    (notes, evs)
  let (holdQueues, holdNotes, holdEvents, cursor1) :=
    processHoldNotes st.holdQueues st.activeHolds input newTime input.deltaSec st.judgeStyle st.touchPanelOffsetSec st.prevSensor cursor
  let (touchHoldQueues, touchHoldNotes, touchHoldEvents, cursor2, touchHoldGroupStates) :=
    processTouchHoldNotes st.touchHoldQueues st.activeTouchHolds input newTime input.deltaSec st.judgeStyle st.touchPanelOffsetSec st.useButtonRingForTouch cursor1 st.touchHoldGroupStates
  let (touchNotes, touchEvents, _cursor3, touchGroupStates) :=
    processTouchNotes st.touchQueues input newTime st.judgeStyle cursor2 st.useButtonRingForTouch st.touchPanelOffsetSec st.touchGroupStates
  let (slideNotes, slideEvents, slideAudioCommands, slideRenderCommands) :=
    processSlideNotes resolvedSlides input newTime input.deltaSec st.judgeStyle st.subdivideSlideJudgeGrade
  let slideNotes := iterateForceFinishParents slideNotes.length slideNotes
  let slideNotes := resolveSlideLinks slideNotes
  let forceFinishCommands := forceFinishRenderCmds resolvedSlides slideNotes

  let allEvents := tapEvents ++ holdEvents ++ touchHoldEvents ++ touchEvents ++ slideEvents
  let newScore := foldEventsIntoScore st.score allEvents
  let audioCommands := slideAudioCommands ++ eventsToAudioCommands allEvents newTime
  let renderCommands := slideRenderCommands ++ forceFinishCommands ++ eventsToRenderCommands allEvents

  ({ st with
      currentTime := newTime
    , prevButton  := input.buttonHeld
    , prevSensor  := input.sensorHeld
    , holdQueues  := holdQueues
    , touchHoldQueues := touchHoldQueues
    , score       := newScore
    , slides      := slideNotes
    , activeHolds := holdNotes
    , activeTouchHolds := touchHoldNotes
    , touchGroupStates := touchGroupStates
    , touchHoldGroupStates := touchHoldGroupStates
  }, allEvents, audioCommands, renderCommands)

def stepFrameTimed (st : GameState) (batch : TimedInputBatch) : GameState × List JudgeEvent × List AudioCommand × List RenderCommand :=
  let input := batch.toFrameInput (batch.currentSec - st.currentTime) st.prevButton st.prevSensor
  let (nextState, events, audioCommands, renderCommands) := stepFrame { st with currentBatch := batch } input
  ({ nextState with currentBatch := batch }, events, audioCommands, renderCommands)

end LnmaiCore.Scheduler
