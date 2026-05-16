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

private def canEnterJudgeable (currentTime judgeableStart : TimePoint) : Bool :=
  currentTime ≥ judgeableStart

private def isTooLateForTapLike (currentTime timing lateLimit : TimePoint) : Bool :=
  currentTime > lateLimit

private def tapMissEvent (note : TapNote) (style : JudgeStyle) : JudgeEvent :=
  let grade := Convert.convertGrade style JudgeGrade.Miss
  { kind := .Tap
  , grade := grade
  , diff := Duration.fromMicros (-1000)
  , position := note.position
  , noteIndex := note.params.noteIndex }

private def tapJudgeEvent (note : TapNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Tap
  , grade := grade
  , diff := judgeDiff
  , position := note.position
  , noteIndex := note.params.noteIndex }

private def judgeTapNow (note : TapNote) (style : JudgeStyle) (judgeDiff : Duration) : TapNote × Option JudgeEvent :=
  let raw := Judge.judgeTap judgeDiff note.params.isEX
  let grade := Convert.convertGrade style raw
  ({ note with state := TapState.Ended }, some (tapJudgeEvent note grade judgeDiff))

/--
  Advance a tap note one frame. Returns (new_note, optional JudgeEvent).
-/
def tapStep (note : TapNote) (currentTime : TimePoint) (judgeDiff : Duration) (inputClicked : Bool) (style : JudgeStyle) : TapNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC)
  match note.state with
  | .Waiting =>
    if isTooLateForTapLike currentTime timing (timing + tapGoodMs) then
      ({ note with state := TapState.Ended }, some (tapMissEvent note style))
    else if canEnterJudgeable currentTime judgeableRange.1 then
      if inputClicked then
        judgeTapNow note style judgeDiff
      else
        ({ note with state := TapState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    if isTooLateForTapLike currentTime timing (timing + tapGoodMs) then
      ({ note with state := TapState.Ended }, some (tapMissEvent note style))
    else if inputClicked && canEnterJudgeable currentTime judgeableRange.1 then
      judgeTapNow note style judgeDiff
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
  touchQueueIndex : Nat := 0
  touchGroupId : Option Nat := none
  touchGroupSize : Nat := 1
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Nat := 1
  touchHoldGroupTriggered : Bool := false
deriving Inhabited, Repr

def HoldNote.position (note : HoldNote) : RuntimePos :=
  note.start.toRuntimePos

private def holdHeadJudged (note : HoldNote) (grade : JudgeGrade) (headDiff : Duration) (groupTriggered : Bool := false) : HoldNote :=
  { note with
      state := HoldSubState.HeadJudged grade
    , headDiff := headDiff
    , headGrade := grade
    , touchHoldGroupTriggered := groupTriggered }

private def holdHeadMiss (note : HoldNote) (headDiff : Duration) : HoldNote :=
  holdHeadJudged note Miss headDiff

private def holdHeadShared (note : HoldNote) (grade : JudgeGrade) (headDiff : Duration) : HoldNote :=
  holdHeadJudged note grade headDiff true

private def judgeHoldHeadTapNow (note : HoldNote) (style : JudgeStyle) (judgeDiff : Duration) : HoldNote :=
  let raw := Judge.judgeTap judgeDiff note.params.isEX
  let grade := Convert.convertGrade style raw
  holdHeadJudged note grade judgeDiff

private def judgeHoldHeadTouchNow? (note : HoldNote) (style : JudgeStyle) (judgeDiff : Duration) : HoldNote × Option JudgeEvent :=
  match Judge.judgeTouch judgeDiff note.params.isEX with
  | some raw =>
      let grade := Convert.convertGrade style raw
      (holdHeadJudged note grade judgeDiff, none)
  | none =>
      (note, none)

private def stepTouchHoldHeadWaiting
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (sharedResult : Option (JudgeGrade × Duration))
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  match sharedResult with
  | some (grade, sharedDiff) =>
      (holdHeadShared note grade sharedDiff, none)
  | none =>
      if currentTime > timing + touchGoodMs then
        (holdHeadMiss note touchGoodMs, none)
      else if canEnterJudgeable currentTime judgeableStart then
        if inputClicked then
          judgeHoldHeadTouchNow? note style judgeDiff
        else
          ({ note with state := HoldSubState.HeadJudgeable }, none)
      else
        (note, none)

private def stepTouchHoldHeadJudgeable
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (sharedResult : Option (JudgeGrade × Duration))
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  match sharedResult with
  | some (grade, sharedDiff) =>
      (holdHeadShared note grade sharedDiff, none)
  | none =>
      if inputClicked && canEnterJudgeable currentTime judgeableStart then
        judgeHoldHeadTouchNow? note style judgeDiff
      else if currentTime > timing + touchGoodMs then
        (holdHeadMiss note touchGoodMs, none)
      else
        (note, none)

private def stepRegularHoldHeadWaiting
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  if currentTime > timing + tapGoodMs then
    (holdHeadMiss note tapGoodMs, none)
  else if canEnterJudgeable currentTime judgeableStart then
    if inputClicked then
      (judgeHoldHeadTapNow note style judgeDiff, none)
    else
      ({ note with state := HoldSubState.HeadJudgeable }, none)
  else
    (note, none)

private def stepRegularHoldHeadJudgeable
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  if inputClicked && canEnterJudgeable currentTime judgeableStart then
    (judgeHoldHeadTapNow note style judgeDiff, none)
  else if currentTime > timing + tapGoodMs then
    (holdHeadMiss note tapGoodMs, none)
  else
    (note, none)

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
      stepTouchHoldHeadWaiting note currentTime timing judgeableRange.1 judgeDiff inputClicked sharedResult style
    else
      stepRegularHoldHeadWaiting note currentTime timing judgeableRange.1 judgeDiff inputClicked style
  | .HeadJudgeable =>
    if note.isTouchHold then
      stepTouchHoldHeadJudgeable note currentTime timing judgeableRange.1 judgeDiff inputClicked sharedResult style
    else
      stepRegularHoldHeadJudgeable note currentTime timing judgeableRange.1 judgeDiff inputClicked style
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
      -- MajdataPlay skips the release-ignore grace after a missed/too-fast head by seeding
      -- `_releaseTime` to a sentinel immediately on head miss, so body release starts counting at once.
      if headGrade.isMissOrTooFast then
        let newRT := note.playerReleaseTime + delta
        ({ note with state := HoldSubState.BodyReleased, playerReleaseTime := newRT, touchHoldGroupTriggered := false }, none)
      else
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
    else if currentTime > bodyCheckEnd || diff >= note.length then
      endHold note note.headGrade currentTime note.playerReleaseTime
    else if inputPressed then
      ({ note with touchHoldGroupTriggered := note.isTouchHold }, none)  -- still held
    else
      ({ note with state := HoldSubState.BodyReleased, playerReleaseTime := note.playerReleaseTime + delta, touchHoldGroupTriggered := false }, none)
  | .BodyReleased =>
    -- released body can recover back to held if input/majority returns before force-end
    if currentTime > bodyCheckEnd || diff >= note.length then
      let newRT := note.playerReleaseTime + delta
      endHold note note.headGrade currentTime newRT
    else if inputPressed then
      ({ note with state := HoldSubState.BodyHeld, touchHoldGroupTriggered := note.isTouchHold }, none)
    else
      let newRT := note.playerReleaseTime + delta
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

private def touchMissEvent (note : TouchNote) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Touch
  , grade := Miss
  , diff := judgeDiff
  , position := .sensor note.sensorPos
  , noteIndex := note.params.noteIndex }

private def touchJudgeEvent (note : TouchNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Touch
  , grade := grade
  , diff := judgeDiff
  , position := .sensor note.sensorPos
  , noteIndex := note.params.noteIndex }

private def judgeTouchNow? (note : TouchNote) (style : JudgeStyle) (judgeDiff : Duration) : TouchNote × Option JudgeEvent :=
  match Judge.judgeTouch judgeDiff note.params.isEX with
  | some raw =>
      let grade := Convert.convertGrade style raw
      ({ note with state := TouchState.Ended }, some (touchJudgeEvent note grade judgeDiff))
  | none =>
      ({ note with state := TouchState.Judgeable }, none)

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
      ({ note with state := TouchState.Ended }, some (touchJudgeEvent note (Convert.convertGrade style grade) sharedDiff))
    | none =>
    if currentTime ≥ judgeableRange.2 then
      ({ note with state := TouchState.Ended }, some (touchMissEvent note judgeDiff))
    else if canEnterJudgeable currentTime judgeableRange.1 then
      if inputClicked then
        judgeTouchNow? note style judgeDiff
      else
        ({ note with state := TouchState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    match sharedResult with
    | some (grade, sharedDiff) =>
      ({ note with state := TouchState.Ended }, some (touchJudgeEvent note (Convert.convertGrade style grade) sharedDiff))
    | none =>
    if currentTime > timing + touchGoodMs then
      ({ note with state := TouchState.Ended }, some (touchMissEvent note judgeDiff))
    else if inputClicked then
      match Judge.judgeTouch judgeDiff note.params.isEX with
      | some raw =>
        let grade := Convert.convertGrade style raw
        ({ note with state := TouchState.Ended }, some (touchJudgeEvent note grade judgeDiff))
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
  timing          : TimePoint           -- slide head timing
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

private structure SlideStepContext where
  currentTime : TimePoint
  touchPanelOffset : Duration
  delta : Duration
  style : JudgeStyle
  subdivideSlideJudgeGrade : Bool
  sensorHeld : SensorVec Bool

private structure SlideStepSemantic where
  note : SlideNote
  event : Option JudgeEvent := none
  queueRenderCmds : List RenderCommand := []
  oldRemaining : Nat := 0
  newRemaining : Nat := 0
  trackOns : List Nat := []
  progressChanged : Bool := false
  hideSlide : Bool := false
  shouldPlayTrackOns : Bool := false
  emitProgressRender : Bool := false

private def slideEffectiveJudgeGrade
    (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool) (raw : JudgeGrade) : JudgeGrade :=
  let converted := Convert.convertGrade style raw
  if subdivideSlideJudgeGrade then converted else Judge.correctSlideGrade converted

private def slideCurrentJudgeDiff (note : SlideNote) (currentTime : TimePoint) (touchPanelOffset : Duration) : Duration :=
  (currentTime - touchPanelOffset) - note.params.effectiveTiming

private def slideCurrentDiff (note : SlideNote) (currentTime : TimePoint) : Duration :=
  currentTime - note.params.effectiveTiming

private def slideShouldBeCheckable (note : SlideNote) (currentTime : TimePoint) : Bool :=
  let headTiming := currentTime - note.timing
  if note.isCheckable then
    true
  else if note.isConnSlide then
    if note.isGroupPartHead then
      headTiming >= Duration.fromMicros (-50000)
    else
      note.parentFinished || note.parentPendingFinish
  else
    headTiming >= Duration.fromMicros (-50000)

private def slideUpdatedQueuesWithCmds
    (note : SlideNote) (isCheckable : Bool) (sensorHeld : SensorVec Bool) :
    List (SlideQueue × List RenderCommand) :=
  if isCheckable then
    note.queueTracks.map (fun (trackIndex, queue) =>
      updateSlideQueue note.params.noteIndex trackIndex queue sensorHeld)
  else
    note.judgeQueues.map (fun queue => (queue, []))

private def slideTooLateTiming (note : SlideNote) : TimePoint :=
  note.startTiming + note.length + SLIDE_JUDGE_GOOD_AREA_MSEC + min note.params.judgeOffset Duration.zero

private def slideJudgeEvent (note : SlideNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Slide
  , grade := grade
  , diff := judgeDiff
  , position := note.position
  , noteIndex := note.params.noteIndex }

private def buildSlideSemanticBase
    (note : SlideNote) (updatedQueues : List SlideQueue) (queueRenderCmds : List RenderCommand)
    (oldRemaining newRemaining : Nat) (trackOns : List Nat) : SlideStepSemantic :=
  { note := { note with judgeQueues := updatedQueues }
  , queueRenderCmds := queueRenderCmds
  , oldRemaining := oldRemaining
  , newRemaining := newRemaining
  , trackOns := trackOns
  , progressChanged := newRemaining != oldRemaining }

private def slideStepSemantic (note : SlideNote) (ctx : SlideStepContext) : SlideStepSemantic :=
  let isCheckable := slideShouldBeCheckable note ctx.currentTime
  let updatedQueuesWithCmds := slideUpdatedQueuesWithCmds note isCheckable ctx.sensorHeld
  let updatedQueues := updatedQueuesWithCmds.map Prod.fst
  let queueRenderCmds := flattenRenderCmds (updatedQueuesWithCmds.map Prod.snd)
  let oldRemaining := slideProgressRemaining note.slideKind note.isClassic note.judgeQueues
  let newRemaining := slideProgressRemaining note.slideKind note.isClassic updatedQueues
  let trackOns :=
    if isCheckable then collectNewSlideOnTracks 0 note.judgeQueues updatedQueues else []
  let semanticBase :=
    buildSlideSemanticBase { note with isCheckable := isCheckable } updatedQueues queueRenderCmds oldRemaining newRemaining trackOns
  let isJudgable := note.isGroupPartEnd || !note.isConnSlide
  match note.state with
  | .Waiting =>
    { semanticBase with
      shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
      emitProgressRender := semanticBase.progressChanged }
  | .Active waitTime =>
    let isTooLate := ctx.currentTime > slideTooLateTiming note
    if !isCheckable then
      { semanticBase with
        shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
        emitProgressRender := semanticBase.progressChanged }
    else if isJudgable && slideQueuesCleared updatedQueues && !isTooLate then
      let judgeDiff := slideCurrentJudgeDiff note ctx.currentTime ctx.touchPanelOffset
      let raw :=
        if note.isClassic then
          Judge.judgeSlideClassic judgeDiff
        else
          Judge.judgeSlideModern judgeDiff waitTime note.params.isEX
      { semanticBase with
        note := { semanticBase.note with state := SlideState.Judged raw waitTime judgeDiff }
        shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
        emitProgressRender := true }
    else if isJudgable && isTooLate then
      let raw := Judge.judgeSlideTooLate (slideQueueRemaining updatedQueues)
      let grade := slideEffectiveJudgeGrade ctx.style ctx.subdivideSlideJudgeGrade raw
      { semanticBase with
        note := { semanticBase.note with state := SlideState.Ended }
        event := some (slideJudgeEvent note grade (slideCurrentDiff note ctx.currentTime))
        hideSlide := true }
    else
      { semanticBase with
        shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
        emitProgressRender := semanticBase.progressChanged }
  | .Judged grade waitTime storedJudgeDiff =>
    let newWait := waitTime - ctx.delta
    if newWait ≤ Duration.zero then
      let finalGrade := slideEffectiveJudgeGrade ctx.style ctx.subdivideSlideJudgeGrade grade
      { semanticBase with
        note := { semanticBase.note with state := SlideState.Ended }
        event := some (slideJudgeEvent note finalGrade storedJudgeDiff)
        hideSlide := true }
    else
      { semanticBase with
        note := { semanticBase.note with state := SlideState.Judged grade newWait storedJudgeDiff }
        shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
        emitProgressRender := semanticBase.progressChanged }
  | .Ended =>
      { semanticBase with note := { semanticBase.note with state := SlideState.Ended } }

private def slideSemanticAudioCmds (semantic : SlideStepSemantic) (currentTime : TimePoint) : List AudioCommand :=
  if semantic.shouldPlayTrackOns then
    semantic.trackOns.map (fun trackIndex => AudioCommand.PlaySlideCue semantic.note.params.noteIndex trackIndex currentTime)
  else []

private def slideSemanticRenderCmds (semantic : SlideStepSemantic) : List RenderCommand :=
  let progressCmds :=
    if semantic.emitProgressRender then
      slideProgressRenderCmds semantic.note semantic.newRemaining
    else []
  let hideCmds :=
    if semantic.hideSlide then
      slideHideRenderCmds semantic.note
    else []
  semantic.queueRenderCmds ++ progressCmds ++ hideCmds

/-- Advance a slide note with queue traversal. -/
def slideStep (note : SlideNote) (currentTime : TimePoint) (sensorHeld : SensorVec Bool) (touchPanelOffset : Duration) (delta : Duration) (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool) : SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand :=
  let ctx : SlideStepContext :=
    { currentTime := currentTime
    , touchPanelOffset := touchPanelOffset
    , delta := delta
    , style := style
    , subdivideSlideJudgeGrade := subdivideSlideJudgeGrade
    , sensorHeld := sensorHeld }
  let semantic := slideStepSemantic note ctx
  let audioCmds :=
    match semantic.note.state with
    | .Ended => []
    | _ => slideSemanticAudioCmds semantic currentTime
  let renderCmds :=
    match semantic.note.state with
    | .Ended =>
        if semantic.hideSlide then slideSemanticRenderCmds semantic else []
    | _ => slideSemanticRenderCmds semantic
  (semantic.note, semantic.event, audioCmds, renderCmds)

end LnmaiCore.Lifecycle
