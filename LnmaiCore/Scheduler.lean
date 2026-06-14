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

structure TouchHeadProbe where
  area : SensorArea
  noteIndex : Nat
  frontier : Nat
  queueCurrentIndex : Nat
  stateBefore : TouchState
  cursorSensorUsedBefore : Nat
  inputSensorClickCount : Nat
  canConsumeClick : Bool
  usedButton : Bool
  usedSensor : Bool
  clicked : Bool
  sharedResult : Option (JudgeGrade × Duration)
  diff : Duration
  stateAfter : TouchState
  event? : Option JudgeEvent
deriving Inhabited, Repr

inductive SensorClickConsumerKind where
  | tap
  | hold
deriving Inhabited, Repr

structure SensorClickConsumerProbe where
  kind : SensorClickConsumerKind
  noteIndex : Nat
  zone : ButtonZone
  sensorArea : SensorArea
  queueIndex : Nat
  frontier : Nat
  diff : Duration
deriving Inhabited, Repr

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

private def holdBodyPressedFromButtonOrSensor (currentButtonPressed currentSensorPressed : Bool) : Bool :=
  currentButtonPressed || currentSensorPressed

private def consumeButtonThenFallbackSensor
    (input : FrameInput)
    (cursor : ClickCursor)
    (allowInput : Bool)
    (zone : ButtonZone)
    (sensorArea : SensorArea) : Bool × Bool × ClickCursor :=
  let (usedButton, cursor1) :=
    if allowInput then
      tryUseButtonClickAt input cursor zone
    else
      (false, cursor)
  let (usedSensor, cursor2) :=
    if allowInput && !usedButton then
      tryUseSensorClickAt input cursor1 sensorArea
    else
      (false, cursor1)
  (usedButton, usedSensor, cursor2)

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
  | tapNote   : TapFamilyNote → ActiveNote
  | holdNote  : HoldNote → ActiveNote
  | touchNote : TouchNote → ActiveNote
  | slideNote : SlideNote → ActiveNote
deriving Inhabited

private def tapEligibleForClick (note : TapFamilyNote) (currentTime : TimePoint) : Bool :=
  let timing := note.params.effectiveTiming
  currentTime ≥ timing - JUDGABLE_RANGE_SEC

private def buttonQueueIndexUnlocked (frontiers : ButtonVec Nat) (zone : ButtonZone) (index : Nat) : Bool :=
  index ≤ frontiers.getD zone 0

private def advanceSharedButtonQueue (frontiers : ButtonVec Nat) (zone : ButtonZone) : ButtonVec Nat :=
  frontiers.set zone (frontiers.getD zone 0 + 1)

private def touchEligibleForClick (note : TouchNote) (currentTime : TimePoint) : Bool :=
  let timing := note.params.effectiveTiming
  currentTime ≥ timing - JUDGABLE_RANGE_SEC

private def holdHeadEligibleForClick (note : HoldNote) (currentTime : TimePoint) : Bool :=
  let timing := note.params.effectiveTiming
  let lateLimit :=
    if note.isTouchHold then
      timing + JUDGABLE_RANGE_SEC + TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC
    else
      timing + JUDGABLE_RANGE_SEC
  currentTime ≥ timing - JUDGABLE_RANGE_SEC && currentTime ≤ lateLimit

----------------------------------------------------------------------------
-- Process tap notes
----------------------------------------------------------------------------

private def processTapNotes (frontiers : ButtonVec Nat) (queues : ButtonQueueVec TapFamilyNote) (input : FrameInput) (currentTime : TimePoint) (touchPanelOffset : Duration) (style : JudgeStyle) (cursor : ClickCursor) : ButtonVec Nat × ButtonQueueVec TapFamilyNote × List JudgeEvent × ClickCursor :=
  let (nextQueues, (frontiers', cursor', evsRev)) :=
    queues.mapAccum (frontiers, cursor, ([] : List JudgeEvent)) (fun zone q state =>
      let (frontiers, cursor, evsRev) := state
      match q.peek with
      | none => (q, (frontiers, cursor, evsRev))
      | some note =>
        let timing := note.params.effectiveTiming
        let buttonDiff := currentTime - timing
        let sensorDiff := (currentTime - touchPanelOffset) - timing
        let canConsumeClick := tapEligibleForClick note currentTime && buttonQueueIndexUnlocked frontiers zone note.buttonQueueIndex
        let fallbackArea := fallbackSensorAreaForButtonNote note.lane.toButtonZone
        let (usedButton, usedSensor, cursor2) :=
          consumeButtonThenFallbackSensor input cursor canConsumeClick zone fallbackArea
        let (clicked, diff) :=
          if usedButton then (true, buttonDiff)
          else if usedSensor then (true, sensorDiff)
          else (false, buttonDiff)
        match tapFamilyStep note currentTime diff clicked style with
        | (newNote, some evt) =>
            let frontiers' :=
              match newNote.state with
              | Lifecycle.TapState.Ended => advanceSharedButtonQueue frontiers zone
              | _ => frontiers
            let nextQueue :=
              match newNote.state with
              | Lifecycle.TapState.Ended => q.advance
              | _ => { q with notes := listSetAt q.notes q.currentIndex newNote }
            (nextQueue, (frontiers', cursor2, evt :: evsRev))
        | (newNote, none) =>
            let nextQueue := { q with notes := listSetAt q.notes q.currentIndex newNote }
            (nextQueue, (frontiers, cursor2, evsRev)))
  (frontiers', nextQueues, evsRev.reverse, cursor')

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
        let keepStoredResult := hasStrictMajority group.count group.size
        let nextGroup :=
          if keepStoredResult then
            { group with count := group.count + 1, size := groupSize }
          else
            { group with count := group.count + 1, size := groupSize, grade := grade, diff := diff }
        nextGroup :: rest
      else
        group :: loop rest
  loop groups

private def touchHoldBodyGroupStatesFromHolds
    (holds : List (SensorArea × HoldNote)) : List TouchHoldBodyGroupState :=
  let rec loop (remaining : List (SensorArea × HoldNote)) (acc : List TouchHoldBodyGroupState) :
      List TouchHoldBodyGroupState :=
    match remaining with
    | [] => acc.reverse
    | (_, note) :: rest =>
        match note.touchHoldGroupId with
        | none => loop rest acc
        | some groupId =>
            let noteIndex := note.params.noteIndex
            let triggered := note.touchHoldGroupTriggered
            let rec upsert (items : List TouchHoldBodyGroupState) : List TouchHoldBodyGroupState :=
              match items with
              | [] =>
                  let triggeredNoteIndices := if triggered then [noteIndex] else []
                  [{ groupId := groupId, memberNoteIndices := [noteIndex], triggeredNoteIndices := triggeredNoteIndices }]
              | item :: tail =>
                  if item.groupId == groupId then
                    let memberNoteIndices :=
                      if item.memberNoteIndices.contains noteIndex then item.memberNoteIndices
                      else noteIndex :: item.memberNoteIndices
                    let triggeredNoteIndices :=
                      if triggered && !item.triggeredNoteIndices.contains noteIndex then
                        noteIndex :: item.triggeredNoteIndices
                      else if !triggered then
                        item.triggeredNoteIndices.erase noteIndex
                      else
                        item.triggeredNoteIndices
                    { item with
                        memberNoteIndices := memberNoteIndices
                      , triggeredNoteIndices := triggeredNoteIndices } :: tail
                  else
                    item :: upsert tail
            loop rest (upsert acc)
  loop holds []

private def touchHoldBodyGroupTriggeredCount (group : TouchHoldBodyGroupState) : Nat :=
  group.triggeredNoteIndices.length

private def touchHoldBodyGroupMemberCount (group : TouchHoldBodyGroupState) : Nat :=
  group.memberNoteIndices.length

private def touchHoldBodyGroupMajorityPressed
    (groups : List TouchHoldBodyGroupState) (groupId : Nat) : Bool :=
  match groups.find? (fun group => group.groupId == groupId) with
  | some group => hasStrictMajority (touchHoldBodyGroupTriggeredCount group) (touchHoldBodyGroupMemberCount group)
  | none => false

private def registerTouchHoldBodyTrigger
    (groups : List TouchHoldBodyGroupState) (groupId : Nat) (noteIndex : Nat) : List TouchHoldBodyGroupState :=
  let rec loop (items : List TouchHoldBodyGroupState) : List TouchHoldBodyGroupState :=
    match items with
    | [] => [{ groupId := groupId, memberNoteIndices := [noteIndex], triggeredNoteIndices := [noteIndex] }]
    | group :: rest =>
        if group.groupId == groupId then
          let memberNoteIndices :=
            if group.memberNoteIndices.contains noteIndex then group.memberNoteIndices
            else noteIndex :: group.memberNoteIndices
          let triggeredNoteIndices :=
            if group.triggeredNoteIndices.contains noteIndex then group.triggeredNoteIndices
            else noteIndex :: group.triggeredNoteIndices
          { group with
              memberNoteIndices := memberNoteIndices
            , triggeredNoteIndices := triggeredNoteIndices } :: rest
        else
          group :: loop rest
  loop groups

private def unregisterTouchHoldBodyTrigger
    (groups : List TouchHoldBodyGroupState) (groupId : Nat) (noteIndex : Nat) : List TouchHoldBodyGroupState :=
  let rec loop (items : List TouchHoldBodyGroupState) : List TouchHoldBodyGroupState :=
    match items with
    | [] => []
    | group :: rest =>
        if group.groupId == groupId then
          { group with triggeredNoteIndices := group.triggeredNoteIndices.erase noteIndex } :: rest
        else
          group :: loop rest
  loop groups

private def exitTouchHoldBodyGroupMember
    (groups : List TouchHoldBodyGroupState) (groupId : Nat) (noteIndex : Nat) : List TouchHoldBodyGroupState :=
  let rec loop (items : List TouchHoldBodyGroupState) : List TouchHoldBodyGroupState :=
    match items with
    | [] => []
    | group :: rest =>
        if group.groupId == groupId then
          let memberNoteIndices := group.memberNoteIndices.erase noteIndex
          if memberNoteIndices.isEmpty then
            rest
          else
            { group with
                memberNoteIndices := memberNoteIndices
              , triggeredNoteIndices := group.triggeredNoteIndices.erase noteIndex } :: rest
        else
          group :: loop rest
  loop groups

private def touchHoldBodyCheckActive (note : HoldNote) (currentTime : TimePoint) : Bool :=
  let timing := note.params.effectiveTiming
  let bodyCheckStart := timing + TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC
  let bodyCheckEnd := timing + note.length - TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
  let bodyWindowDisabled :=
    !note.isClassic && note.length ≤ TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC + TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
  !bodyWindowDisabled && currentTime ≥ bodyCheckStart && currentTime ≤ bodyCheckEnd

private def touchQueueIndexUnlocked (frontiers : SensorVec Nat) (area : SensorArea) (index : Nat) : Bool :=
  index ≤ frontiers.getD area 0

private def advanceSharedTouchQueue (frontiers : SensorVec Nat) (area : SensorArea) : SensorVec Nat :=
  frontiers.set area (frontiers.getD area 0 + 1)

private def processHoldNotes (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote) (holds : List (ButtonZone × HoldNote)) (input : FrameInput) (currentTime : TimePoint) (delta : Duration) (style : JudgeStyle) (touchPanelOffset : Duration) (prevSensor : SensorVec Bool) (cursor : ClickCursor) : ButtonVec Nat × ButtonQueueVec HoldNote × List (ButtonZone × HoldNote) × List JudgeEvent × ClickCursor :=
  match holds with
  | [] => (frontiers, queues, [], [], cursor)
  | (zone, note) :: rest =>
    let timing := note.params.effectiveTiming
    let buttonDiff := currentTime - timing
    let sensorDiff := (currentTime - touchPanelOffset) - timing
    let currentButtonPressed := input.getButtonHeld zone
    let currentSensorPressed := fallbackSensorHeldForButtonNote input zone
    let prevSensorPressed := fallbackPrevSensorHeldForButtonNote prevSensor zone
    let allowInput :=
      queueHeadMatches (InputModel.buttonQueueAt queues zone) note
        && buttonQueueIndexUnlocked frontiers zone note.buttonQueueIndex
        && holdHeadEligibleForClick note currentTime
    let fallbackArea := fallbackSensorAreaForButtonNote zone
    let (usedButton, usedSensor, cursor2) :=
      consumeButtonThenFallbackSensor input cursor allowInput zone fallbackArea
    let clicked := usedButton || usedSensor
    let diff := if usedButton then buttonDiff else sensorDiff
    let (newNote, evt?) :=
      holdStep note currentTime diff HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked
        (holdBodyPressedFromButtonOrSensor currentButtonPressed currentSensorPressed)
        currentButtonPressed prevSensorPressed touchPanelOffset none delta style
    let frontiers' := if enteredHeadJudged note.state newNote.state then advanceSharedButtonQueue frontiers zone else frontiers
    let queues' := if enteredHeadJudged note.state newNote.state then advanceButtonQueueIfHead queues zone newNote else queues
    let (restFrontiers, restQueues, restNotes, restEvs, cursor3) := processHoldNotes frontiers' queues' rest input currentTime delta style touchPanelOffset prevSensor cursor2
    let restNotes' := if keepHoldActive newNote then (zone, newNote) :: restNotes else restNotes
    match evt? with
    | some evt =>
      (restFrontiers, restQueues, restNotes', evt :: restEvs, cursor3)
    | none =>
      (restFrontiers, restQueues, restNotes', restEvs, cursor3)

private def processTouchHoldNotes
    (touchFrontiers : SensorVec Nat)
    (queues : SensorQueueVec HoldNote)
    (holds : List (SensorArea × HoldNote))
    (input : FrameInput)
    (currentTime : TimePoint)
    (delta : Duration)
    (style : JudgeStyle)
    (touchPanelOffset : Duration)
    (cursor : ClickCursor)
    (touchGroupStates : List GroupState)
    (touchHoldBodyGroups : List TouchHoldBodyGroupState) :
    SensorVec Nat × SensorQueueVec HoldNote × List (SensorArea × HoldNote) × List JudgeEvent ×
      ClickCursor × List GroupState × List TouchHoldBodyGroupState :=
  match holds with
  | [] => (touchFrontiers, queues, [], [], cursor, touchGroupStates, touchHoldBodyGroups)
  | (area, note) :: rest =>
    let timing := note.params.effectiveTiming
    let sensorDiff := (currentTime - touchPanelOffset) - timing
    let localBodyPressed := input.getSensorHeld area
    let touchHoldBodyGroups1 :=
      if touchHoldBodyCheckActive note currentTime then
        match note.touchHoldGroupId with
        | some groupId =>
            if localBodyPressed then
              registerTouchHoldBodyTrigger touchHoldBodyGroups groupId note.params.noteIndex
            else
              unregisterTouchHoldBodyTrigger touchHoldBodyGroups groupId note.params.noteIndex
        | none => touchHoldBodyGroups
      else
        touchHoldBodyGroups
    let effectivePressed :=
      localBodyPressed ||
        match note.touchHoldGroupId with
        | some groupId => touchHoldBodyGroupMajorityPressed touchHoldBodyGroups1 groupId
        | none => false
    let allowInput :=
      queueHeadMatches (InputModel.sensorQueueAt queues area) note
        && touchQueueIndexUnlocked touchFrontiers area note.touchQueueIndex
        && holdHeadEligibleForClick note currentTime
    let (usedSensor, cursor1) :=
      if allowInput then
        tryUseSensorClickAt input cursor area
      else
        (false, cursor)
    let sharedResult :=
      match note.touchGroupId with
      | some groupId => groupShareResult touchGroupStates groupId
      | none => none
    let (newNote, evt?) :=
      holdStep note currentTime sensorDiff TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
        usedSensor effectivePressed false false touchPanelOffset sharedResult delta style
    let touchFrontiers' := if enteredHeadJudged note.state newNote.state then advanceSharedTouchQueue touchFrontiers area else touchFrontiers
    let queues' := if enteredHeadJudged note.state newNote.state then advanceSensorQueueIfHead queues area newNote else queues
    let touchGroupStates' :=
      match evt?, note.touchGroupId with
      | some evt, some groupId =>
          if evt.grade.isMissOrTooFast then
            touchGroupStates
          else
            updateGroupState touchGroupStates groupId note.touchGroupSize evt.grade newNote.headDiff
      | _, _ =>
          match newNote.state with
          | HoldSubState.HeadJudged grade =>
              if grade.isMissOrTooFast then
                touchGroupStates
              else
                match note.touchGroupId with
                | some groupId =>
                    updateGroupState touchGroupStates groupId note.touchGroupSize grade newNote.headDiff
                | none => touchGroupStates
          | _ => touchGroupStates
    let touchHoldBodyGroups2 :=
      match note.touchHoldGroupId with
      | some groupId =>
          if keepHoldActive newNote then
            touchHoldBodyGroups1
          else
            exitTouchHoldBodyGroupMember touchHoldBodyGroups1 groupId note.params.noteIndex
      | none => touchHoldBodyGroups1
    let (restTouchFrontiers, restQueues, restNotes, restEvs, cursor2, restTouchGroups, restBodyGroups) :=
      processTouchHoldNotes touchFrontiers' queues' rest input currentTime delta style touchPanelOffset
        cursor1 touchGroupStates' touchHoldBodyGroups2
    let restNotes' := if keepHoldActive newNote then (area, newNote) :: restNotes else restNotes
    match evt? with
    | some evt =>
        (restTouchFrontiers, restQueues, restNotes', evt :: restEvs, cursor2, restTouchGroups,
          restBodyGroups)
    | none =>
        (restTouchFrontiers, restQueues, restNotes', restEvs, cursor2, restTouchGroups,
          restBodyGroups)

----------------------------------------------------------------------------
-- Process touch notes
----------------------------------------------------------------------------

private def processTouchNotes (frontiers : SensorVec Nat) (queues : SensorQueueVec TouchNote) (input : FrameInput) (currentTime : TimePoint) (style : JudgeStyle) (cursor : ClickCursor) (touchPanelOffset : Duration) (groupStates : List GroupState) : SensorVec Nat × SensorQueueVec TouchNote × List JudgeEvent × ClickCursor × List GroupState :=
  let (nextQueues, (frontiers', cursor', groups', evsRev)) :=
    queues.mapAccum (frontiers, cursor, groupStates, ([] : List JudgeEvent)) (fun area q state =>
      let (frontiers, cursor, groups, evsRev) := state
      match q.peek with
      | none => (q, (frontiers, cursor, groups, evsRev))
      | some note =>
        let timing := note.params.effectiveTiming
        let sensorDiff := (currentTime - touchPanelOffset) - timing
        let canConsumeClick := touchEligibleForClick note currentTime && touchQueueIndexUnlocked frontiers area note.touchQueueIndex
        let (usedSensor, cursor2) :=
          if canConsumeClick then tryUseSensorClickAt input cursor note.sensorPos else (false, cursor)
        let clicked := usedSensor
        let diff := sensorDiff
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
          let nextQueue := q.advance
          let frontiers' := advanceSharedTouchQueue frontiers area
          (nextQueue, (frontiers', cursor2, groups', evt :: evsRev))
        | (newNote, none) =>
          let groups' :=
            match note.touchGroupId, newNote.state with
            | some groupId, TouchState.Ended => groups
            | some groupId, TouchState.Judged grade =>
              if grade.isMissOrTooFast then groups else updateGroupState groups groupId note.touchGroupSize grade diff
            | _, _ => groups
          let frontiers' :=
            match newNote.state with
            | TouchState.Ended => advanceSharedTouchQueue frontiers area
            | _ => frontiers
          let nextQueue :=
            match newNote.state with
            | TouchState.Ended => q.advance
            | _ => { q with notes := listSetAt q.notes q.currentIndex newNote }
          (nextQueue, (frontiers', cursor2, groups', evsRev)))
  (frontiers', nextQueues, evsRev.reverse, cursor', groups')

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

private def eventScoreNoteType (evt : JudgeEvent) : NoteType :=
  if evt.isBreak || evt.kind == .Break then
    .Break
  else
    match evt.kind with
    | .Tap => .Tap
    | .Hold => .Hold
    | .Slide => .Slide
    | .Touch => .Touch
    | .Break => .Break

private def eventScoreDeltas (evt : JudgeEvent) (multiple : Nat) : Nat × Nat × Nat × Nat :=
  match eventScoreNoteType evt with
  | .Break =>
      let (earnedBase, earnedExtra, _earnedClassicExtra, lostBase, lostExtra, _lostClassicExtra) :=
        Score.scoreBreak evt.grade multiple
      (earnedBase, earnedExtra, lostBase, lostExtra)
  | kind =>
      let (earnedBase, lostBase) := Score.scoreNonBreak (NoteType.baseScore kind) evt.grade multiple
      (earnedBase, 0, lostBase, 0)

private def foldEventIntoScore
    (noteDisplay breakDisplay : JudgeDisplayOption) (s : ScoreState) (evt : JudgeEvent) :
    ScoreState :=
  let multiple : Nat := 1
  let comboDelta := Score.updateCombo s.combo s.pCombo s.cPCombo s.dxScore evt.grade multiple
  let (earnedBaseDelta, earnedExtraDelta, lostBaseDelta, lostExtraDelta) :=
    eventScoreDeltas evt multiple
  let display := if evt.isBreak || evt.kind == .Break then breakDisplay else noteDisplay
  let (isFast, isLate) := Score.countFastLate evt.grade evt.diff display
  let counts :=
    if evt.isBreak || evt.kind == .Break then
      { s.counts with breakCount := λ g => if g == evt.grade then s.counts.breakCount g + 1 else s.counts.breakCount g }
    else
      match evt.kind with
      | .Tap   => { s.counts with tapCount   := λ g => if g == evt.grade then s.counts.tapCount g + 1 else s.counts.tapCount g }
      | .Hold  => { s.counts with holdCount  := λ g => if g == evt.grade then s.counts.holdCount g + 1 else s.counts.holdCount g }
      | .Slide => { s.counts with slideCount := λ g => if g == evt.grade then s.counts.slideCount g + 1 else s.counts.slideCount g }
      | .Touch => { s.counts with touchCount := λ g => if g == evt.grade then s.counts.touchCount g + 1 else s.counts.touchCount g }
      | .Break => { s.counts with breakCount := λ g => if g == evt.grade then s.counts.breakCount g + 1 else s.counts.breakCount g }
  { s with
    combo       := comboDelta.combo
    pCombo      := comboDelta.pCombo
    cPCombo     := comboDelta.cPCombo
    earnedBase  := s.earnedBase + earnedBaseDelta
    earnedExtra := s.earnedExtra + earnedExtraDelta
    lostBase    := s.lostBase + lostBaseDelta
    lostExtra   := s.lostExtra + lostExtraDelta
    dxScore     := comboDelta.dXScoreLost
    fastCount   := s.fastCount + if isFast then multiple else 0
    lateCount   := s.lateCount + if isLate then multiple else 0
    counts      := counts
  }

private def foldEventsIntoScore
    (noteDisplay breakDisplay : JudgeDisplayOption) (s : ScoreState) (events : List JudgeEvent) :
    ScoreState :=
  match events with
  | [] => s
  | evt :: rest =>
      foldEventsIntoScore noteDisplay breakDisplay
        (foldEventIntoScore noteDisplay breakDisplay s evt) rest

private def eventToAudioCommands (evt : JudgeEvent) (timePoint : TimePoint) : List AudioCommand :=
  [ AudioCommand.PlayJudgeSfx evt.kind evt.grade evt.isBreak timePoint evt.noteIndex ]

private def eventToRenderCommands (evt : JudgeEvent) : List RenderCommand :=
  [ RenderCommand.ShowJudgeResult evt.kind evt.grade evt.isBreak evt.diff evt.noteIndex ]

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
  let touchHoldBodyGroups :=
    if st.touchHoldGroupStates.isEmpty then
      touchHoldBodyGroupStatesFromHolds st.activeTouchHolds
    else
      st.touchHoldGroupStates

  -- Semantic order is deliberate; see module comment above.
  let (buttonFrontiers1, tapNotes, tapEvents, cursorTap) :=
    processTapNotes st.buttonQueueFrontiers st.tapQueues input newTime st.touchPanelOffset st.judgeStyle cursor
  let (buttonFrontiers2, holdQueues, holdNotes, holdEvents, cursor1) :=
    processHoldNotes buttonFrontiers1 st.holdQueues st.activeHolds input newTime input.delta st.judgeStyle st.touchPanelOffset st.prevSensor cursorTap
  let (touchFrontiers1, touchNotes, touchEvents, cursor2, touchGroupStates) :=
    processTouchNotes st.touchQueueFrontiers st.touchQueues input newTime st.judgeStyle cursor1 st.touchPanelOffset st.touchGroupStates
  let (touchFrontiers2, touchHoldQueues, touchHoldNotes, touchHoldEvents, _cursor3, touchGroupStates', touchHoldGroupStates) :=
    processTouchHoldNotes touchFrontiers1 st.touchHoldQueues st.activeTouchHolds input newTime input.delta st.judgeStyle st.touchPanelOffset cursor2 touchGroupStates touchHoldBodyGroups
  let (slideNotes, slideEvents, slideAudioCommands, slideRenderCommands) :=
    processSlideNotes resolvedSlides input newTime st.touchPanelOffset input.delta st.judgeStyle st.subdivideSlideJudgeGrade
  let slideNotes := forceFinishParentSlides slideNotes
  let slideNotes := updateSlideParentFlags slideNotes
  let forceFinishCommands := forceFinishRenderCmds resolvedSlides slideNotes

  let allEvents := tapEvents ++ holdEvents ++ touchHoldEvents ++ touchEvents ++ slideEvents
  let newScore :=
    foldEventsIntoScore st.noteFastLateDisplay st.breakFastLateDisplay st.score allEvents
  let audioCommands := slideAudioCommands ++ eventsToAudioCommands allEvents newTime
  let renderCommands := slideRenderCommands ++ forceFinishCommands ++ eventsToRenderCommands allEvents

  ({ st with
      currentTime := newTime
    , prevButton  := input.buttonHeld
    , prevSensor  := input.sensorHeld
    , buttonQueueFrontiers := buttonFrontiers2
    , tapQueues   := tapNotes
    , holdQueues  := holdQueues
    , touchQueueFrontiers := touchFrontiers2
    , touchHoldQueues := touchHoldQueues
    , touchQueues := touchNotes
    , score       := newScore
    , slides      := slideNotes
    , activeHolds := holdNotes
    , activeTouchHolds := touchHoldNotes
    , touchGroupStates := touchGroupStates'
    , touchHoldGroupStates := touchHoldGroupStates
  }, allEvents, audioCommands, renderCommands)

def stepFrameTimed (st : GameState) (batch : TimedInputBatch) : GameState × List JudgeEvent × List AudioCommand × List RenderCommand :=
  let input := batch.toFrameInput (batch.currentTime - st.currentTime) st.prevButton st.prevSensor
  let (nextState, events, audioCommands, renderCommands) := stepFrame { st with currentBatch := batch } input
  ({ nextState with currentBatch := batch }, events, audioCommands, renderCommands)

def probeTouchHeadAt (st : GameState) (input : FrameInput) (area : SensorArea) : Option TouchHeadProbe :=
  let newTime := st.currentTime + input.delta
  let cursor : ClickCursor := {}
  let (buttonFrontiers1, _, _, cursorTap) :=
    processTapNotes st.buttonQueueFrontiers st.tapQueues input newTime st.touchPanelOffset st.judgeStyle cursor
  let (_, _, _, _, cursor1) :=
    processHoldNotes buttonFrontiers1 st.holdQueues st.activeHolds input newTime input.delta st.judgeStyle st.touchPanelOffset st.prevSensor cursorTap
  let q := st.touchQueues.getD area { notes := [] }
  match q.peek with
  | none => none
  | some note =>
      let timing := note.params.effectiveTiming
      let sensorDiff := (newTime - st.touchPanelOffset) - timing
      let frontier := st.touchQueueFrontiers.getD area 0
      let canConsumeClick := touchEligibleForClick note newTime && touchQueueIndexUnlocked st.touchQueueFrontiers area note.touchQueueIndex
      let (usedSensor, _cursor3) :=
        if canConsumeClick then tryUseSensorClickAt input cursor1 note.sensorPos else (false, cursor1)
      let clicked := usedSensor
      let diff := sensorDiff
      let sharedResult :=
        match note.touchGroupId with
        | some groupId => groupShareResult st.touchGroupStates groupId
        | none => none
      let (newNote, evt?) := touchStep note newTime diff clicked sharedResult st.judgeStyle
      some
        { area := area
        , noteIndex := note.params.noteIndex
        , frontier := frontier
        , queueCurrentIndex := q.currentIndex
        , stateBefore := note.state
        , cursorSensorUsedBefore := cursor1.sensorUsed.getD note.sensorPos 0
        , inputSensorClickCount := input.getSensorClickCount note.sensorPos
        , canConsumeClick := canConsumeClick
        , usedButton := false
        , usedSensor := usedSensor
        , clicked := clicked
        , sharedResult := sharedResult
        , diff := diff
        , stateAfter := newNote.state
        , event? := evt? }

def probeTapHoldSensorConsumers (st : GameState) (input : FrameInput) : List SensorClickConsumerProbe :=
  let newTime := st.currentTime + input.delta
  let cursor : ClickCursor := {}
  let rec probeTaps (zones : List ButtonZone) (frontiers : ButtonVec Nat) (cursor : ClickCursor)
      (acc : List SensorClickConsumerProbe) : ButtonVec Nat × ClickCursor × List SensorClickConsumerProbe :=
    match zones with
    | [] => (frontiers, cursor, acc.reverse)
    | zone :: rest =>
        let q := st.tapQueues.getD zone { notes := [] }
        match q.peek with
        | none => probeTaps rest frontiers cursor acc
        | some note =>
            let timing := note.params.effectiveTiming
            let buttonDiff := newTime - timing
            let sensorDiff := (newTime - st.touchPanelOffset) - timing
            let canConsumeClick := tapEligibleForClick note newTime && buttonQueueIndexUnlocked frontiers zone note.buttonQueueIndex
            let (usedButton, cursor1) :=
              if canConsumeClick then tryUseButtonClickAt input cursor zone else (false, cursor)
            let fallbackArea := fallbackSensorAreaForButtonNote note.lane.toButtonZone
            let (usedButton', usedSensor, cursor2) :=
              consumeButtonThenFallbackSensor input cursor canConsumeClick zone fallbackArea
            let acc :=
              if usedSensor then
                { kind := .tap
                , noteIndex := note.params.noteIndex
                , zone := zone
                , sensorArea := fallbackArea
                , queueIndex := note.buttonQueueIndex
                , frontier := frontiers.getD zone 0
                , diff := sensorDiff } :: acc
              else
                acc
            let frontiers' :=
              match tapFamilyStep note newTime (if usedButton' then buttonDiff else sensorDiff) (usedButton' || usedSensor) st.judgeStyle with
              | (newNote, some _) =>
                  match newNote.state with
                  | Lifecycle.TapState.Ended => advanceSharedButtonQueue frontiers zone
                  | _ => frontiers
              | (newNote, none) =>
                  match newNote.state with
                  | Lifecycle.TapState.Ended => advanceSharedButtonQueue frontiers zone
                  | _ => frontiers
            probeTaps rest frontiers' cursor2 acc
  let (buttonFrontiers1, cursorTap, tapConsumers) :=
    probeTaps ButtonZone.storageOrder st.buttonQueueFrontiers cursor []
  let rec probeHolds (holds : List (ButtonZone × HoldNote)) (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote)
      (cursor : ClickCursor) (acc : List SensorClickConsumerProbe) : List SensorClickConsumerProbe :=
    match holds with
    | [] => acc.reverse
    | (zone, note) :: rest =>
        let timing := note.params.effectiveTiming
        let buttonDiff := newTime - timing
        let sensorDiff := (newTime - st.touchPanelOffset) - timing
        let allowInput := queueHeadMatches (InputModel.buttonQueueAt queues zone) note && buttonQueueIndexUnlocked frontiers zone note.buttonQueueIndex
        let (usedButton, _cursor1) := if allowInput then tryUseButtonClickAt input cursor zone else (false, cursor)
        let fallbackArea := fallbackSensorAreaForButtonNote zone
        let (usedButton', usedSensor, cursor2) :=
          consumeButtonThenFallbackSensor input cursor allowInput zone fallbackArea
        let acc :=
          if usedSensor then
            { kind := .hold
            , noteIndex := note.params.noteIndex
            , zone := zone
            , sensorArea := fallbackArea
            , queueIndex := note.buttonQueueIndex
            , frontier := frontiers.getD zone 0
            , diff := sensorDiff } :: acc
          else
            acc
        let diff := if usedButton' then buttonDiff else sensorDiff
        let holdResult :=
          holdStep note newTime diff HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC (usedButton' || usedSensor)
            (holdBodyPressedFromButtonOrSensor (input.getButtonHeld zone) (fallbackSensorHeldForButtonNote input zone))
            (input.getButtonHeld zone)
            (fallbackPrevSensorHeldForButtonNote st.prevSensor zone) st.touchPanelOffset none input.delta st.judgeStyle
        let frontiers' :=
          if enteredHeadJudged note.state holdResult.1.state then
            advanceSharedButtonQueue frontiers zone
          else
            frontiers
        let queues' :=
          if enteredHeadJudged note.state holdResult.1.state then
            advanceButtonQueueIfHead queues zone note
          else
            queues
        probeHolds rest frontiers' queues' cursor2 acc
  probeHolds st.activeHolds buttonFrontiers1 st.holdQueues cursorTap tapConsumers

end LnmaiCore.Scheduler
