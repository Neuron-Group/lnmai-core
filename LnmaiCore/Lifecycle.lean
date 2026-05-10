/-
  Note lifecycle state machines — pure functional models of
  Tap, Hold, Slide, and Touch note state transitions.

  Each note type has its own state machine. The Core advances
  all active notes each frame, consuming input and emitting
  JudgeEvents when a note is judged.
-/

import LnmaiCore.Types
import LnmaiCore.Constants
import LnmaiCore.Judge
import LnmaiCore.Convert

set_option linter.unusedVariables false

namespace LnmaiCore.Lifecycle

open Constants
open JudgeGrade
open NoteType

----------------------------------------------------------------------------
-- Common Note Parameters (set at spawn time)
----------------------------------------------------------------------------

structure NoteParams where
  judgeTimingSec : Float           -- scheduled judge time
  judgeOffsetSec : Float           -- user judge offset
  startPos       : Nat             -- button zone (0-7) or primary sensor area
  isBreak        : Bool := false
  isEX           : Bool := false
  noteIndex      : Nat             -- unique id in chart
deriving Inhabited, Repr

/-- Effective judge timing with user offset -/
def NoteParams.effectiveTiming (p : NoteParams) : Float :=
  p.judgeTimingSec + p.judgeOffsetSec

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
  params : NoteParams
  state  : TapState
deriving Inhabited, Repr

/--
  Advance a tap note one frame. Returns (new_note, optional JudgeEvent).
-/
def tapStep (note : TapNote) (currentSec : Float) (judgeDiffMs : Float) (inputClicked : Bool) (style : JudgeStyle) : TapNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let diffSec := currentSec - timing
  let diffMs := diffSec * 1000.0
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC)
  match note.state with
  | .Waiting =>
    if currentSec > timing + tapGoodMs / 1000.0 then
      -- too late before being judgeable — force Miss
      let raw := JudgeGrade.Miss
      let grade := Convert.convertGrade style raw
      let evt : JudgeEvent := { kind := .Tap, grade := grade, diffMs := diffMs, sensorPos := note.params.startPos, noteIndex := note.params.noteIndex }
      ({ note with state := TapState.Ended }, some evt)
    else if currentSec ≥ judgeableRange.1 then
      -- entering judgeable window
      ({ note with state := TapState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    if currentSec > timing + tapGoodMs / 1000.0 then
      -- too late → Miss
      let raw := JudgeGrade.Miss
      let grade := Convert.convertGrade style raw
      let evt : JudgeEvent := { kind := .Tap, grade := grade, diffMs := 150.0, sensorPos := note.params.startPos, noteIndex := note.params.noteIndex }
      ({ note with state := TapState.Ended }, some evt)
    else if inputClicked && currentSec ≥ judgeableRange.1 then
      let raw := Judge.judgeTap judgeDiffMs note.params.isEX
      let grade := Convert.convertGrade style raw
      let evt : JudgeEvent := { kind := .Tap, grade := grade, diffMs := judgeDiffMs, sensorPos := note.params.startPos, noteIndex := note.params.noteIndex }
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
  params     : NoteParams
  state      : HoldSubState
  lengthSec  : Float                          -- total hold length
  headDiffMs : Float := 0.0                   -- head timing diff in ms
  playerReleaseTimeSec : Float := 0.0         -- accumulated release time
  isClassic  : Bool := false
  isTouchHold : Bool := false
deriving Inhabited, Repr

/--
  Advance a hold note one frame.
  `inputPressed` = button/sensor is held this frame.
  `inputClicked` = button/sensor just pressed this frame (edge).
-/
def holdStep (note : HoldNote) (currentSec : Float) (judgeDiffMs : Float) (headIgnoreSec : Float) (tailIgnoreSec : Float) (inputClicked : Bool) (inputPressed : Bool) (deltaSec : Float) (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let diffSec := currentSec - timing
  let diffMs := diffSec * 1000.0
  let bodyCheckStart := timing + headIgnoreSec
  let bodyCheckEnd   := timing + note.lengthSec - tailIgnoreSec
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC)
  let endHold (note : HoldNote) (headGrade : JudgeGrade) (releaseTimeSec : Float) : HoldNote × Option JudgeEvent :=
    let finalGrade := Judge.judgeHoldEnd headGrade note.headDiffMs note.lengthSec (headIgnoreSec + tailIgnoreSec) releaseTimeSec
    let finalGrade' := Convert.convertGrade style finalGrade
    let evt : JudgeEvent := { kind := .Hold, grade := finalGrade', diffMs := diffMs, sensorPos := note.params.startPos, noteIndex := note.params.noteIndex }
    ({ note with state := HoldSubState.Ended finalGrade' }, some evt)
  match note.state with
  | .HeadWaiting =>
    if currentSec > timing + tapGoodMs / 1000.0 then
      -- too late for head → mark miss, but let the hold end logic run
      ({ note with state := HoldSubState.HeadJudged Miss }, none)
    else if currentSec ≥ judgeableRange.1 then
      ({ note with state := HoldSubState.HeadJudgeable }, none)
    else
      (note, none)
  | .HeadJudgeable =>
    if inputClicked && currentSec ≥ judgeableRange.1 then
      let raw := Judge.judgeTap judgeDiffMs note.params.isEX
      let grade := Convert.convertGrade style raw
      ({ note with state := HoldSubState.HeadJudged grade, headDiffMs := judgeDiffMs }, none)
    else if currentSec > timing + tapGoodMs / 1000.0 then
      ({ note with state := HoldSubState.HeadJudged Miss }, none)
    else
      (note, none)
  | .HeadJudged headGrade =>
    if currentSec < bodyCheckStart then
      -- still in head ignore window, keep waiting
      (note, none)
    else if currentSec > bodyCheckEnd then
      -- past body check window → force end
      endHold note headGrade note.playerReleaseTimeSec
    else if inputPressed then
      ({ note with state := HoldSubState.BodyHeld, playerReleaseTimeSec := 0.0 }, none)
    else
      -- not pressed, accumulate release time if past release ignore
      let newRT := note.playerReleaseTimeSec + deltaSec
      if newRT ≤ DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC then
        ({ note with playerReleaseTimeSec := newRT }, none)
      else
        ({ note with state := HoldSubState.BodyReleased, playerReleaseTimeSec := newRT }, none)
  | .BodyHeld =>
    -- check if body still active
    if inputPressed then
      (note, none)  -- still held
    else
      ({ note with state := HoldSubState.BodyReleased, playerReleaseTimeSec := note.playerReleaseTimeSec + deltaSec }, none)
  | .BodyReleased =>
    -- accumulate release time until force-end
    let newRT := note.playerReleaseTimeSec + deltaSec
    let _remainingTime := max 0.0 (note.lengthSec - diffSec)
    if currentSec > bodyCheckEnd || diffSec >= note.lengthSec then
      -- force end: compute final grade
      let headGrade := match note.state with
        | .HeadJudged g => g
        | _ => Miss
      endHold note headGrade newRT
    else
      ({ note with playerReleaseTimeSec := newRT }, none)
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
  params       : NoteParams
  state        : TouchState
  sensorPos    : Nat       -- sensor area (0-32)
deriving Inhabited, Repr

/--
  Advance a touch note one frame. Touch uses wider windows
  and only late-side judgments.
-/
def touchStep (note : TouchNote) (currentSec : Float) (judgeDiffMs : Float) (inputClicked : Bool) (style : JudgeStyle) : TouchNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let diffSec := currentSec - timing
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC + TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC)
  match note.state with
  | .Waiting =>
    if currentSec ≥ judgeableRange.2 then
      let evt : JudgeEvent := { kind := .Touch, grade := Miss, diffMs := judgeDiffMs, sensorPos := note.sensorPos, noteIndex := note.params.noteIndex }
      ({ note with state := TouchState.Ended }, some evt)
    else if currentSec ≥ judgeableRange.1 then
      ({ note with state := TouchState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    if currentSec > timing + touchGoodMs / 1000.0 then
      let evt : JudgeEvent := { kind := .Touch, grade := Miss, diffMs := judgeDiffMs, sensorPos := note.sensorPos, noteIndex := note.params.noteIndex }
      ({ note with state := TouchState.Ended }, some evt)
    else if inputClicked then
      match Judge.judgeTouch judgeDiffMs note.params.isEX with
      | some raw =>
        let grade := Convert.convertGrade style raw
        let evt : JudgeEvent := { kind := .Touch, grade := grade, diffMs := judgeDiffMs, sensorPos := note.sensorPos, noteIndex := note.params.noteIndex }
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
  | Active  (waitTimeSec : Float)          -- star traveling, wait time for end window
  | Judged  (grade : JudgeGrade) (waitTimeSec : Float)
  | Ended
deriving Inhabited, Repr

structure SlideArea where
  targetAreas : List Nat
  policy      : AreaPolicy := AreaPolicy.Or
  isLast      : Bool := false
  isSkippable : Bool := true
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

def SlideArea.check (area : SlideArea) (sensorHeld : List Bool) : SlideArea :=
  let isHeld :=
    match area.policy with
    | .Or  => area.targetAreas.any (fun target => (sensorHeld[target]?).getD false)
    | .And => area.targetAreas.all (fun target => (sensorHeld[target]?).getD false)
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

def slideQueuesCleared (queues : List SlideQueue) : Bool :=
  match queues with
  | [] => true
  | q :: rest => if q.isEmpty then slideQueuesCleared rest else false

def updateSlideArea (area : SlideArea) (sensorHeld : List Bool) : SlideArea :=
  area.check sensorHeld

partial def updateSlideQueue (queue : SlideQueue) (sensorHeld : List Bool) : SlideQueue :=
  match queue with
  | [] => []
  | first :: rest =>
    let first' := updateSlideArea first sensorHeld
    match rest with
    | [] =>
      if first'.isFinished then [] else [first']
    | second :: rest2 =>
      if first'.isSkippable || first'.on then
        let second' := updateSlideArea second sensorHeld
        if second'.isFinished then
          updateSlideQueue rest2 sensorHeld
        else if second'.on then
          second' :: rest2
        else
          first' :: second' :: rest2
      else if first'.isFinished then
        updateSlideQueue rest sensorHeld
      else
        first' :: rest

structure SlideNote where
  params          : NoteParams
  state           : SlideState
  lengthSec       : Float               -- total slide length
  startTiming     : Float               -- when slide started
  isClassic       : Bool := false
  isConnSlide     : Bool := false
  isGroupPartHead : Bool := false
  isGroupPartEnd  : Bool := false
  parentFinished  : Bool := false
  parentPendingFinish : Bool := false
  totalJudgeQueueLen : Nat := 0
  isCheckable     : Bool := false
  judgeQueues     : List SlideQueue := []
deriving Inhabited, Repr

/-- Advance a slide note with queue traversal. -/
def slideStep (note : SlideNote) (currentSec : Float) (sensorHeld : List Bool) (style : JudgeStyle) : SlideNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let diffSec := currentSec - timing
  let diffMs := diffSec * 1000.0
  let updatedQueues := note.judgeQueues.map (fun queue => updateSlideQueue queue sensorHeld)
  let queueFullyCleared := slideQueuesCleared updatedQueues
  let startTiming := currentSec - note.startTiming
  let isCheckable :=
    if note.isCheckable then
      true
    else if note.isConnSlide then
      if note.isGroupPartHead then
        startTiming >= -0.05
      else
        note.parentFinished || note.parentPendingFinish
    else
      startTiming >= -0.05
  let isJudgable := note.isGroupPartEnd || !note.isConnSlide
  match note.state with
  | .Waiting =>
    ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none)
  | .Active waitTime =>
    let isTooLate := diffSec > (SLIDE_JUDGE_GOOD_AREA_MSEC / 1000.0)
    if !isCheckable then
      ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none)
    else if isJudgable && queueFullyCleared && !isTooLate then
      let raw := Judge.judgeSlideModern diffMs (waitTime * 1000.0) note.params.isEX
      let grade := Judge.correctSlideGrade (Convert.convertGrade style raw)
      let evt : JudgeEvent := { kind := .Slide, grade := grade, diffMs := diffMs, sensorPos := note.params.startPos, noteIndex := note.params.noteIndex }
      ({ note with state := SlideState.Ended, judgeQueues := updatedQueues, isCheckable := isCheckable }, some evt)
    else if isJudgable && isTooLate then
      let raw := Judge.judgeSlideTooLate (slideQueueRemaining updatedQueues)
      let grade := Judge.correctSlideGrade (Convert.convertGrade style raw)
      let evt : JudgeEvent := { kind := .Slide, grade := grade, diffMs := diffMs, sensorPos := note.params.startPos, noteIndex := note.params.noteIndex }
      ({ note with state := SlideState.Ended, judgeQueues := updatedQueues, isCheckable := isCheckable }, some evt)
    else
      ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none)
  | .Judged _ waitTime =>
    let newWait := waitTime - Float.ofNat 1 / 60.0  -- approximate frame delta
    if newWait ≤ 0.0 then
      ({ note with state := SlideState.Ended, judgeQueues := updatedQueues, isCheckable := isCheckable }, none)
    else
      ({ note with state := SlideState.Judged (match note.state with | .Judged g _ => g | _ => Miss) newWait, judgeQueues := updatedQueues, isCheckable := isCheckable }, none)
  | .Ended =>
    ({ note with judgeQueues := updatedQueues, isCheckable := isCheckable }, none)

end LnmaiCore.Lifecycle
