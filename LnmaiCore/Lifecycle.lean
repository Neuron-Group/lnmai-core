/-
  Note lifecycle state machines — pure functional models of
  Tap, Hold, Slide, and Touch note state transitions.

  Each note type has its own state machine. The Core advances
  all active notes each frame, consuming input and emitting
  JudgeEvents when a note is judged.
-/

import LnmaiCore.Types
import LnmaiCore.Areas
import LnmaiCore.Storage
import LnmaiCore.Constants
import LnmaiCore.Judge
import LnmaiCore.Convert
import LnmaiCore.Time

set_option linter.unusedVariables false

namespace LnmaiCore.Lifecycle

open Constants
open JudgeGrade
open NoteType

----------------------------------------------------------------------------
-- Common Note Parameters (set at spawn time)
----------------------------------------------------------------------------

structure CommonNoteParams where
  judgeTiming : TimePoint          -- scheduled judge time
  judgeOffset : Duration           -- user judge offset
  isBreak        : Bool := false
  isEX           : Bool := false
  noteIndex      : Nat             -- unique id in chart
deriving Inhabited, Repr

/-- Effective judge timing with user offset -/
def CommonNoteParams.effectiveTiming (p : CommonNoteParams) : TimePoint :=
  p.judgeTiming + p.judgeOffset

inductive HoldStart where
  | button (zone : ButtonZone)
  | sensor (area : SensorArea)
deriving Inhabited, Repr

def HoldStart.toRuntimePos : HoldStart → RuntimePos
  | .button zone => .button zone
  | .sensor area => .sensor area

----------------------------------------------------------------------------
-- Tap Note State
----------------------------------------------------------------------------

inductive TapState where
  | Waiting                            -- spawned, not yet in range
  | Judgeable                          -- within judgeable window, awaiting input
  | Judged (grade : JudgeGrade)        -- judged (by input or too-late)
  | Ended                              -- terminal
deriving Inhabited, Repr

structure TapNote where
  params : CommonNoteParams
  lane   : OuterSlot
  state  : TapState
deriving Inhabited, Repr

def TapNote.position (note : TapNote) : RuntimePos :=
  .button note.lane.toButtonZone

/--
  Advance a tap note one frame. Returns (new_note, optional JudgeEvent).
-/
def tapStep (note : TapNote) (currentTime : TimePoint) (judgeDiff : Duration) (inputClicked : Bool) (style : JudgeStyle) : TapNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC)
  match note.state with
  | .Waiting =>
    if currentTime > timing + tapGoodMs then
      -- too late before being judgeable — force Miss
      let raw := JudgeGrade.Miss
      let grade := Convert.convertGrade style raw
      let evt : JudgeEvent := { kind := .Tap, grade := grade, diff := Duration.fromMicros (-1000), position := note.position, noteIndex := note.params.noteIndex }
      ({ note with state := TapState.Ended }, some evt)
    else if currentTime ≥ judgeableRange.1 then
      -- entering judgeable window
      ({ note with state := TapState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    if currentTime > timing + tapGoodMs then
      -- too late → Miss
      let raw := JudgeGrade.Miss
      let grade := Convert.convertGrade style raw
      let evt : JudgeEvent := { kind := .Tap, grade := grade, diff := Duration.fromMicros (-1000), position := note.position, noteIndex := note.params.noteIndex }
      ({ note with state := TapState.Ended }, some evt)
    else if inputClicked && currentTime ≥ judgeableRange.1 then
      let raw := Judge.judgeTap judgeDiff note.params.isEX
      let grade := Convert.convertGrade style raw
      let evt : JudgeEvent := { kind := .Tap, grade := grade, diff := judgeDiff, position := note.position, noteIndex := note.params.noteIndex }
      ({ note with state := TapState.Ended }, some evt)
    else
      (note, none)
  | .Judged _ =>
    (note, none)
  | .Ended =>
    (note, none)

----------------------------------------------------------------------------
-- Hold Note State
----------------------------------------------------------------------------

inductive HoldSubState where
  | HeadWaiting
  | HeadJudgeable
  | HeadJudged (grade : JudgeGrade)    -- head hit received
  | BodyHeld                           -- holding actively
  | BodyReleased                       -- released, accumulating release time
  | Ended (grade : JudgeGrade)        -- terminal
deriving Inhabited, Repr

structure HoldNote where
  params     : CommonNoteParams
  start      : HoldStart
  state      : HoldSubState
  length     : Duration                       -- total hold length
  headDiff   : Duration := Duration.zero      -- head timing diff
  headGrade  : JudgeGrade := JudgeGrade.Miss
  playerReleaseTime : Duration := Duration.zero -- accumulated release time
  isClassic  : Bool := false
  isTouchHold : Bool := false
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Nat := 1
  touchHoldGroupTriggered : Bool := false
deriving Inhabited, Repr

def HoldNote.position (note : HoldNote) : RuntimePos :=
  note.start.toRuntimePos

/--
  Advance a hold note one frame.
  `inputPressed` = button/sensor is held this frame.
  `inputClicked` = button/sensor just pressed this frame (edge).
-/
def holdStep (note : HoldNote) (currentTime : TimePoint) (judgeDiff : Duration) (headIgnore : Duration) (tailIgnore : Duration) (inputClicked : Bool) (inputPressed : Bool) (currentButtonPressed : Bool) (prevSensorPressed : Bool) (touchPanelOffset : Duration) (sharedResult : Option (JudgeGrade × Duration)) (delta : Duration) (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let diff := currentTime - timing
  let bodyCheckStart := timing + headIgnore
  let bodyCheckEnd   := timing + note.length - tailIgnore
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC)
  let releaseOffset := if prevSensorPressed && !currentButtonPressed then Duration.zero else touchPanelOffset
  let endHold (note : HoldNote) (headGrade : JudgeGrade) (classicReleaseTiming : TimePoint) (releaseTime : Duration) : HoldNote × Option JudgeEvent :=
    let finalGrade :=
      if note.isClassic then
        Judge.judgeHoldClassicEnd headGrade timing note.length classicReleaseTiming
      else
        Judge.judgeHoldEnd headGrade note.headDiff note.length (headIgnore + tailIgnore) releaseTime
    let finalGrade' := Convert.convertGrade style finalGrade
    let eventDiff := if note.headDiff == Duration.zero && headGrade == Miss then Time.fromMillis 150 else note.headDiff
    let evt : JudgeEvent := { kind := .Hold, grade := finalGrade', diff := eventDiff, position := note.position, noteIndex := note.params.noteIndex }
    ({ note with state := HoldSubState.Ended finalGrade', touchHoldGroupTriggered := false }, some evt)
  match note.state with
  | .HeadWaiting =>
    if note.isTouchHold then
      match sharedResult with
      | some (grade, sharedDiff) =>
        ({ note with state := HoldSubState.HeadJudged grade, headDiff := sharedDiff, headGrade := grade, touchHoldGroupTriggered := true }, none)
      | none =>
        if currentTime > timing + touchGoodMs then
          ({ note with state := HoldSubState.HeadJudged Miss, headDiff := touchGoodMs, headGrade := Miss }, none)
        else if currentTime ≥ judgeableRange.1 then
          ({ note with state := HoldSubState.HeadJudgeable }, none)
        else
          (note, none)
    else if currentTime > timing + tapGoodMs then
      ({ note with state := HoldSubState.HeadJudged Miss, headDiff := tapGoodMs, headGrade := Miss }, none)
    else if currentTime ≥ judgeableRange.1 then
      ({ note with state := HoldSubState.HeadJudgeable }, none)
    else
      (note, none)
  | .HeadJudgeable =>
    if note.isTouchHold then
      match sharedResult with
      | some (grade, sharedDiff) =>
        ({ note with state := HoldSubState.HeadJudged grade, headDiff := sharedDiff, headGrade := grade, touchHoldGroupTriggered := true }, none)
      | none =>
        if inputClicked && currentTime ≥ judgeableRange.1 then
          match Judge.judgeTouch judgeDiff note.params.isEX with
          | some raw =>
            let grade := Convert.convertGrade style raw
            ({ note with state := HoldSubState.HeadJudged grade, headDiff := judgeDiff, headGrade := grade, touchHoldGroupTriggered := note.isTouchHold }, none)
          | none =>
            (note, none)
        else if currentTime > timing + touchGoodMs then
          ({ note with state := HoldSubState.HeadJudged Miss, headDiff := touchGoodMs, headGrade := Miss }, none)
        else
          (note, none)
    else if inputClicked && currentTime ≥ judgeableRange.1 then
      let raw := Judge.judgeTap judgeDiff note.params.isEX
      let grade := Convert.convertGrade style raw
      ({ note with state := HoldSubState.HeadJudged grade, headDiff := judgeDiff, headGrade := grade, touchHoldGroupTriggered := note.isTouchHold }, none)
    else if currentTime > timing + tapGoodMs then
      ({ note with state := HoldSubState.HeadJudged Miss, headDiff := tapGoodMs, headGrade := Miss }, none)
    else
      (note, none)
  | .HeadJudged headGrade =>
    if currentTime < bodyCheckStart then
      -- still in head ignore window, keep waiting
      (note, none)
    else if note.isClassic then
      if diff >= note.length + CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC || headGrade.isMissOrTooFast then
        endHold note headGrade currentTime note.playerReleaseTime
      else if inputPressed then
        ({ note with state := HoldSubState.BodyHeld, touchHoldGroupTriggered := note.isTouchHold }, none)
      else
        endHold note headGrade (currentTime - releaseOffset) note.playerReleaseTime
    else if currentTime > bodyCheckEnd then
      -- past body check window → force end
      endHold note headGrade currentTime note.playerReleaseTime
    else if inputPressed then
      ({ note with state := HoldSubState.BodyHeld, playerReleaseTime := Duration.zero, touchHoldGroupTriggered := note.isTouchHold }, none)
    else
      -- not pressed, accumulate release time if past release ignore
      let newRT := note.playerReleaseTime + delta
      if newRT ≤ DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC then
        ({ note with playerReleaseTime := newRT }, none)
      else
        ({ note with state := HoldSubState.BodyReleased, playerReleaseTime := newRT, touchHoldGroupTriggered := false }, none)
  | .BodyHeld =>
    -- check if body still active
    if note.isClassic then
      if diff >= note.length + CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC || note.headGrade.isMissOrTooFast then
        endHold note note.headGrade currentTime note.playerReleaseTime
      else if inputPressed then
        ({ note with touchHoldGroupTriggered := note.isTouchHold }, none)
      else
        endHold note note.headGrade (currentTime - releaseOffset) note.playerReleaseTime
    else if inputPressed then
      ({ note with touchHoldGroupTriggered := note.isTouchHold }, none)  -- still held
    else
      ({ note with state := HoldSubState.BodyReleased, playerReleaseTime := note.playerReleaseTime + delta, touchHoldGroupTriggered := false }, none)
  | .BodyReleased =>
    -- accumulate release time until force-end
    let newRT := note.playerReleaseTime + delta
    let _remainingTime := max Duration.zero (note.length - diff)
    if currentTime > bodyCheckEnd || diff >= note.length then
      -- force end: compute final grade
      endHold note note.headGrade currentTime newRT
    else
      ({ note with playerReleaseTime := newRT, touchHoldGroupTriggered := false }, none)
  | .Ended _ =>
    (note, none)

----------------------------------------------------------------------------
-- Touch Note State
----------------------------------------------------------------------------

inductive TouchState where
  | Waiting
  | Judgeable
  | Judged (grade : JudgeGrade)
  | Ended
deriving Inhabited, Repr

structure TouchNote where
  params       : CommonNoteParams
  state        : TouchState
  sensorPos    : SensorArea
  touchGroupId : Option Nat := none
  touchGroupSize : Nat := 1
deriving Inhabited, Repr

/--
  Advance a touch note one frame. Touch uses wider windows
  and only late-side judgments.
-/
def touchStep (note : TouchNote) (currentTime : TimePoint) (judgeDiff : Duration) (inputClicked : Bool) (sharedResult : Option (JudgeGrade × Duration)) (style : JudgeStyle) : TouchNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC + TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC)
  match note.state with
  | .Waiting =>
    match sharedResult with
    | some (grade, sharedDiff) =>
      let evt : JudgeEvent := { kind := .Touch, grade := Convert.convertGrade style grade, diff := sharedDiff, position := .sensor note.sensorPos, noteIndex := note.params.noteIndex }
      ({ note with state := TouchState.Ended }, some evt)
    | none =>
    if currentTime ≥ judgeableRange.2 then
      let evt : JudgeEvent := { kind := .Touch, grade := Miss, diff := judgeDiff, position := .sensor note.sensorPos, noteIndex := note.params.noteIndex }
      ({ note with state := TouchState.Ended }, some evt)
    else if currentTime ≥ judgeableRange.1 then
      ({ note with state := TouchState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    match sharedResult with
    | some (grade, sharedDiff) =>
      let evt : JudgeEvent := { kind := .Touch, grade := Convert.convertGrade style grade, diff := sharedDiff, position := .sensor note.sensorPos, noteIndex := note.params.noteIndex }
      ({ note with state := TouchState.Ended }, some evt)
    | none =>
    if currentTime > timing + touchGoodMs then
      let evt : JudgeEvent := { kind := .Touch, grade := Miss, diff := judgeDiff, position := .sensor note.sensorPos, noteIndex := note.params.noteIndex }
      ({ note with state := TouchState.Ended }, some evt)
    else if inputClicked then
      match Judge.judgeTouch judgeDiff note.params.isEX with
      | some raw =>
        let grade := Convert.convertGrade style raw
        let evt : JudgeEvent := { kind := .Touch, grade := grade, diff := judgeDiff, position := .sensor note.sensorPos, noteIndex := note.params.noteIndex }
        ({ note with state := TouchState.Ended }, some evt)
      | none =>
        (note, none)  -- too early, keep waiting
    else
      (note, none)
  | .Judged _ | .Ended =>
    (note, none)

----------------------------------------------------------------------------
-- Slide Note State
----------------------------------------------------------------------------

inductive SlideState where
  | Waiting
  | Active  (waitTime : Duration)          -- star traveling, wait time for end window
  | Judged  (grade : JudgeGrade) (waitTime : Duration) (judgeDiff : Duration)
  | Ended
deriving Inhabited, Repr

structure SlideArea where
  targetAreas : List SensorArea
  policy      : AreaPolicy := AreaPolicy.Or
  isLast      : Bool := false
  isSkippable : Bool := true
  arrowProgressWhenOn : Nat := 0
  arrowProgressWhenFinished : Nat := 0
  wasOn       : Bool := false
  wasOff      : Bool := false
deriving Inhabited, Repr

def SlideArea.on (area : SlideArea) : Bool :=
  area.wasOn

def SlideArea.isFinished (area : SlideArea) : Bool :=
  if area.isLast then
    area.wasOn
  else
    area.wasOn && area.wasOff

private def sensorHeldAt (sensorHeld : SensorVec Bool) (area : SensorArea) : Bool :=
  sensorHeld.getD area false

def SlideArea.check (area : SlideArea) (sensorHeld : SensorVec Bool) : SlideArea :=
  let isHeld :=
    match area.policy with
    | .Or  => area.targetAreas.any (fun target => sensorHeldAt sensorHeld target)
    | .And => area.targetAreas.all (fun target => sensorHeldAt sensorHeld target)
  if isHeld then
    { area with wasOn := true }
  else if area.wasOn then
    { area with wasOff := true }
  else
    area

abbrev SlideQueue := List SlideArea

def slideQueueRemaining (queues : List SlideQueue) : Nat :=
  let rec go (acc : Nat) : List SlideQueue → Nat
    | [] => acc
    | q :: rest => go (max acc q.length) rest
  go 0 queues

private def wifiQueueProgressRemaining (isClassic : Bool) (queues : List SlideQueue) : Nat :=
  match queues with
  | [left, center, right] =>
      if isClassic then
        if left.length ≤ 1 && center.length ≤ 1 && right.length ≤ 1 then 8
        else
          let maxLen := Nat.max left.length (Nat.max center.length right.length)
          let pick (candidates : List SlideQueue) : Nat :=
            match candidates.find? (fun queue => queue.length = maxLen) with
            | some (area :: _) => area.arrowProgressWhenFinished
            | _ => 0
          pick [left, center, right]
      else if center.isEmpty && left.length ≤ 1 && right.length ≤ 1 then
        9
      else
        let maxLen := Nat.max left.length (Nat.max center.length right.length)
        let pick (candidates : List SlideQueue) : Nat :=
          match candidates.find? (fun queue => queue.length = maxLen) with
          | some (area :: _) => area.arrowProgressWhenFinished
          | _ => 0
        pick [left, center, right]
  | _ => slideQueueRemaining queues

private def slideProgressRemaining (slideKind : SlideKind) (isClassic : Bool) (queues : List SlideQueue) : Nat :=
  match slideKind with
  | SlideKind.Wifi => wifiQueueProgressRemaining isClassic queues
  | _ => slideQueueRemaining queues

def slideQueuesCleared (queues : List SlideQueue) : Bool :=
  match queues with
  | [] => true
  | q :: rest => if q.isEmpty then slideQueuesCleared rest else false

private def slideHeadOn (queue : SlideQueue) : Bool :=
  match queue with
  | [] => false
  | area :: _ => area.on

private def slideHideBarCmd (noteIndex : Nat) (trackIndex : Option Nat) (endIndex : Nat) : RenderCommand :=
  match trackIndex with
  | none => RenderCommand.HideSlideBars noteIndex endIndex
  | some trackIndex => RenderCommand.HideSlideTrackBars noteIndex trackIndex endIndex

private def flattenRenderCmds : List (List RenderCommand) → List RenderCommand
  | [] => []
  | cmds :: rest => cmds ++ flattenRenderCmds rest

private def collectNewSlideOnTracks (index : Nat) (oldQueues newQueues : List SlideQueue) : List Nat :=
  match oldQueues, newQueues with
  | [], _ => []
  | _, [] => []
  | oldQueue :: oldRest, newQueue :: newRest =>
    let rest := collectNewSlideOnTracks (index + 1) oldRest newRest
    if slideHeadOn newQueue && !slideHeadOn oldQueue then
      index :: rest
    else
      rest

def updateSlideArea (area : SlideArea) (sensorHeld : SensorVec Bool) : SlideArea :=
  area.check sensorHeld

/-- Flatten multi-track queue specs for proof-facing single-track reasoning. -/
def flattenSlideQueues : List SlideQueue → SlideQueue
  | [] => []
  | q :: qs => q ++ flattenSlideQueues qs

/-- Pure queue traversal step, exposed for proofs and semantics-focused tests. -/
private partial def slideQueueCore
    (noteIndex : Nat) (trackIndex : Option Nat) (emitCmds : Bool) (queue : SlideQueue) (sensorHeld : SensorVec Bool) :
    SlideQueue × List RenderCommand :=
  match queue with
  | [] => ([], [])
  | first :: rest =>
    let first' := updateSlideArea first sensorHeld
    match rest with
    | [] =>
      if first'.isFinished then
        let cmds := if emitCmds then [slideHideBarCmd noteIndex trackIndex first'.arrowProgressWhenFinished] else []
        ([], cmds)
      else if first'.on then
        let cmds := if emitCmds then [slideHideBarCmd noteIndex trackIndex first'.arrowProgressWhenOn] else []
        ([first'], cmds)
      else
        ([first'], [])
    | second :: rest2 =>
      if first'.isSkippable || first'.on then
        let second' := updateSlideArea second sensorHeld
        if second'.isFinished then
          let (restQueue, restCmds) := slideQueueCore noteIndex trackIndex emitCmds rest2 sensorHeld
          let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex second'.arrowProgressWhenFinished :: restCmds else []
          (restQueue, cmds)
        else if second'.on then
          let (restQueue, restCmds) := slideQueueCore noteIndex trackIndex emitCmds (second' :: rest2) sensorHeld
          let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex second'.arrowProgressWhenOn :: restCmds else []
          (restQueue, cmds)
        else if first'.isFinished then
          let (restQueue, restCmds) := slideQueueCore noteIndex trackIndex emitCmds (second' :: rest2) sensorHeld
          let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex first'.arrowProgressWhenFinished :: restCmds else []
          (restQueue, cmds)
        else
          ([first', second'] ++ rest2, [])
      else if first'.isFinished then
        let (restQueue, restCmds) := slideQueueCore noteIndex trackIndex emitCmds rest sensorHeld
        let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex first'.arrowProgressWhenFinished :: restCmds else []
        (restQueue, cmds)
      else
        ([first'] ++ rest, [])

/-- Pure queue traversal step, exposed for proofs and semantics-focused tests. -/
partial def replaySlideQueue (queue : SlideQueue) (sensorHeld : SensorVec Bool) : SlideQueue :=
  (slideQueueCore 0 none false queue sensorHeld).1

private partial def updateSlideQueueWithCmds (noteIndex : Nat) (trackIndex : Option Nat) (queue : SlideQueue) (sensorHeld : SensorVec Bool) : SlideQueue × List RenderCommand :=
  slideQueueCore noteIndex trackIndex true queue sensorHeld

private def updateSlideQueue (noteIndex : Nat) (trackIndex : Option Nat) (queue : SlideQueue) (sensorHeld : SensorVec Bool) : SlideQueue × List RenderCommand :=
  updateSlideQueueWithCmds noteIndex trackIndex queue sensorHeld

structure SlideNote where
  params          : CommonNoteParams
  lane            : OuterSlot
  state           : SlideState
  length          : Duration            -- total slide length
  startTiming     : TimePoint           -- when slide started
  slideKind       : SlideKind := .Single
  isClassic       : Bool := false
  isConnSlide     : Bool := false
  parentNoteIndex : Option Nat := none
  isGroupPartHead : Bool := false
  isGroupPartEnd  : Bool := false
  parentFinished  : Bool := false
  parentPendingFinish : Bool := false
  initialQueueRemaining : Nat := 0
  totalJudgeQueueLen : Nat := 0
  trackCount      : Nat := 1
  isCheckable     : Bool := false
  judgeQueues     : List SlideQueue := []
deriving Inhabited, Repr

def SlideNote.position (note : SlideNote) : RuntimePos :=
  .button note.lane.toButtonZone

private def SlideNote.queueTracks (note : SlideNote) : List (Option Nat × SlideQueue) :=
  if note.trackCount = 1 then
    note.judgeQueues.map (fun queue => (none, queue))
  else
    ((List.range note.judgeQueues.length).map some).zip note.judgeQueues

private def SlideNote.trackRenderIndices (note : SlideNote) : List Nat :=
  List.range note.trackCount

private def slideProgressRenderCmds (note : SlideNote) (remaining : Nat) : List RenderCommand :=
  match note.slideKind with
  | SlideKind.Single => [RenderCommand.UpdateSlideProgress note.params.noteIndex remaining]
  | SlideKind.Wifi | SlideKind.ConnPart =>
      note.trackRenderIndices.map (fun trackIndex => RenderCommand.UpdateSlideTrackProgress note.params.noteIndex trackIndex remaining)

private def slideHideRenderCmds (note : SlideNote) : List RenderCommand :=
  match note.slideKind with
  | SlideKind.Single => [RenderCommand.HideAllSlideBars note.params.noteIndex]
  | SlideKind.Wifi | SlideKind.ConnPart =>
      [RenderCommand.HideAllSlideBars note.params.noteIndex]

/-- Advance a slide note with queue traversal. -/
def slideStep (note : SlideNote) (currentTime : TimePoint) (sensorHeld : SensorVec Bool) (touchPanelOffset : Duration) (delta : Duration) (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool) : SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand :=
  let timing := note.params.effectiveTiming
  let judgeCurrentTime := currentTime - touchPanelOffset
  let judgeDiff := judgeCurrentTime - timing
  let diff := currentTime - timing
  let oldRemaining := slideProgressRemaining note.slideKind note.isClassic note.judgeQueues
  let startTiming := currentTime - note.startTiming
  let isCheckable :=
    if note.isCheckable then
      true
    else if note.isConnSlide then
      if note.isGroupPartHead then
        startTiming >= Duration.fromMicros (-50000)
      else
        note.parentFinished || note.parentPendingFinish
    else
      startTiming >= Duration.fromMicros (-50000)
  let updatedQueuesWithCmds :=
    if isCheckable then
      note.queueTracks.map (fun (trackIndex, queue) =>
        updateSlideQueue note.params.noteIndex trackIndex queue sensorHeld)
    else
      note.judgeQueues.map (fun queue => (queue, []))
  let updatedQueues := updatedQueuesWithCmds.map Prod.fst
  let queueRenderCmds := flattenRenderCmds (updatedQueuesWithCmds.map Prod.snd)
  let queueFullyCleared := slideQueuesCleared updatedQueues
  let newRemaining := slideProgressRemaining note.slideKind note.isClassic updatedQueues
  let newTrackOns :=
    if isCheckable then collectNewSlideOnTracks 0 note.judgeQueues updatedQueues else []
  let canPlaySlideCue := note.isGroupPartHead || !note.isConnSlide
  let audioCmdsForTrackOns :=
    if canPlaySlideCue then
      newTrackOns.map (fun trackIndex => AudioCommand.PlaySlideCue note.params.noteIndex trackIndex currentTime)
    else []
  let isJudgable := note.isGroupPartEnd || !note.isConnSlide
  match note.state with
  | .Waiting =>
    let renderCmds :=
      if newRemaining != oldRemaining then
        slideProgressRenderCmds note newRemaining
      else []
    ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none, audioCmdsForTrackOns, queueRenderCmds ++ renderCmds)
  | .Active waitTime =>
    let tooLateTiming := note.startTiming + note.length + SLIDE_JUDGE_GOOD_AREA_MSEC + min note.params.judgeOffset Duration.zero
    let isTooLate := currentTime > tooLateTiming
    if !isCheckable then
      let renderCmds :=
        if newRemaining != oldRemaining then
          slideProgressRenderCmds note newRemaining
        else []
      ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none, audioCmdsForTrackOns, queueRenderCmds ++ renderCmds)
    else if isJudgable && queueFullyCleared && !isTooLate then
      let raw :=
        if note.isClassic then
          Judge.judgeSlideClassic judgeDiff
        else
          Judge.judgeSlideModern judgeDiff waitTime note.params.isEX
      let renderCmds :=
        slideProgressRenderCmds note newRemaining
      ({ note with state := SlideState.Judged raw waitTime judgeDiff, judgeQueues := updatedQueues, isCheckable := isCheckable }, none, audioCmdsForTrackOns, queueRenderCmds ++ renderCmds)
    else if isJudgable && isTooLate then
      let raw := Judge.judgeSlideTooLate (slideQueueRemaining updatedQueues)
      let grade :=
        let converted := Convert.convertGrade style raw
        if subdivideSlideJudgeGrade then converted else Judge.correctSlideGrade converted
      let evt : JudgeEvent := { kind := .Slide, grade := grade, diff := diff, position := note.position, noteIndex := note.params.noteIndex }
      ({ note with state := SlideState.Ended, judgeQueues := updatedQueues, isCheckable := isCheckable }, some evt, [], queueRenderCmds ++ slideHideRenderCmds note)
    else
      let renderCmds :=
        if newRemaining != oldRemaining then
          slideProgressRenderCmds note newRemaining
        else []
      ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none, audioCmdsForTrackOns, queueRenderCmds ++ renderCmds)
  | .Judged grade waitTime storedJudgeDiff =>
    let newWait := waitTime - delta
    if newWait ≤ Duration.zero then
      let grade :=
        let converted := Convert.convertGrade style grade
        if subdivideSlideJudgeGrade then converted else Judge.correctSlideGrade converted
      let evt : JudgeEvent := { kind := .Slide, grade := grade, diff := storedJudgeDiff, position := note.position, noteIndex := note.params.noteIndex }
      ({ note with state := SlideState.Ended, judgeQueues := updatedQueues, isCheckable := isCheckable }, some evt, [], queueRenderCmds ++ slideHideRenderCmds note)
    else
      let renderCmds :=
        if newRemaining != oldRemaining then
          slideProgressRenderCmds note newRemaining
        else []
      ({ note with state := SlideState.Judged grade newWait storedJudgeDiff, judgeQueues := updatedQueues, isCheckable := isCheckable }, none, audioCmdsForTrackOns, queueRenderCmds ++ renderCmds)
  | .Ended =>
    ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none, [], [])

end LnmaiCore.Lifecycle
