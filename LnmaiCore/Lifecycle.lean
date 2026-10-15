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
import Lean.Data.Json

open Lean

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
deriving Inhabited, Repr, ToJson, FromJson

/-- Effective judge timing with user offset -/
def CommonNoteParams.effectiveTiming (p : CommonNoteParams) : TimePoint :=
  p.judgeTiming + p.judgeOffset

-- Input origin retained for hold events and body sampling.
inductive HoldStart where
  | button (zone : ButtonZone)
  | sensor (area : SensorArea)
deriving Inhabited, Repr, ToJson, FromJson

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
deriving Inhabited, Repr, ToJson, FromJson

structure TapNote where
  params : CommonNoteParams
  lane   : OuterSlot
  state  : TapState
  buttonQueueIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

def TapNote.position (note : TapNote) : RuntimePos :=
  .button note.lane.toButtonZone

-- Independently judged star head. It shares tap queues and timing; the body is a SlideNote.
structure SlideHeadNote where
  params : CommonNoteParams
  lane : OuterSlot
  state : TapState
  logicalSlideId : Nat := 0
  buttonQueueIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

-- Locate head feedback at its outer button lane.
def SlideHeadNote.position (note : SlideHeadNote) : RuntimePos :=
  .button note.lane.toButtonZone

inductive TapFamilyNote where
  | tap : TapNote → TapFamilyNote
  | slideHead : SlideHeadNote → TapFamilyNote
deriving Inhabited, Repr

private def getObjValAsD? {α : Type} [FromJson α] (json : Json) (field : String) (fallback : α) : Except String α :=
  match json.getObjValAs? α field with
  | .ok value => pure value
  | .error _ => pure fallback

private def getObjOptionalValAsD? {α : Type} [FromJson α] (json : Json) (field : String) (fallback : α) : Except String α :=
  match json.getObjVal? field with
  | .ok valueJson => fromJson? valueJson
  | .error _ => pure fallback

instance : ToJson TapFamilyNote where
  toJson
    | .tap note =>
        Json.mkObj
          [ ("kind", Json.str "tap")
          , ("params", toJson note.params)
          , ("lane", toJson note.lane)
          , ("state", toJson note.state)
          , ("buttonQueueIndex", toJson note.buttonQueueIndex) ]
    | .slideHead note =>
        Json.mkObj
          [ ("kind", Json.str "slideHead")
          , ("params", toJson note.params)
          , ("lane", toJson note.lane)
          , ("state", toJson note.state)
          , ("logicalSlideId", toJson note.logicalSlideId)
          , ("buttonQueueIndex", toJson note.buttonQueueIndex) ]

instance : FromJson TapFamilyNote where
  fromJson? json := do
    let kind ← json.getObjValAs? String "kind"
    let params ← json.getObjValAs? CommonNoteParams "params"
    let lane ← json.getObjValAs? OuterSlot "lane"
    let state ← json.getObjValAs? TapState "state"
    let buttonQueueIndex ← getObjOptionalValAsD? json "buttonQueueIndex" 0
    match kind with
    | "tap" =>
        pure <| .tap
          { params := params
          , lane := lane
          , state := state
          , buttonQueueIndex := buttonQueueIndex }
    | "slideHead" =>
        let logicalSlideId ← getObjOptionalValAsD? json "logicalSlideId" params.noteIndex
        pure <| .slideHead
          { params := params
          , lane := lane
          , state := state
          , logicalSlideId := logicalSlideId
          , buttonQueueIndex := buttonQueueIndex }
    | other =>
        throw s!"unknown TapFamilyNote kind: {other}"

def TapFamilyNote.params : TapFamilyNote → CommonNoteParams
  | .tap note => note.params
  | .slideHead note => note.params

def TapFamilyNote.lane : TapFamilyNote → OuterSlot
  | .tap note => note.lane
  | .slideHead note => note.lane

def TapFamilyNote.state : TapFamilyNote → TapState
  | .tap note => note.state
  | .slideHead note => note.state

def TapFamilyNote.buttonQueueIndex : TapFamilyNote → Nat
  | .tap note => note.buttonQueueIndex
  | .slideHead note => note.buttonQueueIndex

def TapFamilyNote.position : TapFamilyNote → RuntimePos
  | .tap note => note.position
  | .slideHead note => note.position

instance : Coe TapNote TapFamilyNote where
  coe := TapFamilyNote.tap

instance : Coe SlideHeadNote TapFamilyNote where
  coe := TapFamilyNote.slideHead

private def canEnterJudgeable (currentTime judgeableStart : TimePoint) : Bool :=
  currentTime ≥ judgeableStart

private def isTooLateForTapLike (currentTime timing lateLimit : TimePoint) : Bool :=
  currentTime > lateLimit

private def tapLikeMissEvent (params : CommonNoteParams) (lane : OuterSlot) (style : JudgeStyle) : JudgeEvent :=
  let grade := Convert.convertGrade style JudgeGrade.Miss
  { kind := .Tap
  , phase := .head
  , grade := grade
  , diff := Duration.fromMicros (-1000)
  , position := .button lane.toButtonZone
  , noteIndex := params.noteIndex
  , isBreak := params.isBreak }

private def tapLikeJudgeEvent (params : CommonNoteParams) (lane : OuterSlot) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Tap
  , phase := .head
  , grade := grade
  , diff := judgeDiff
  , position := .button lane.toButtonZone
  , noteIndex := params.noteIndex
  , isBreak := params.isBreak }

private def tapMissEvent (note : TapNote) (style : JudgeStyle) : JudgeEvent :=
  tapLikeMissEvent note.params note.lane style

private def tapJudgeEvent (note : TapNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  tapLikeJudgeEvent note.params note.lane grade judgeDiff

-- A missed star head reports a tap-family result independently of its body.
private def slideHeadMissEvent (note : SlideHeadNote) (style : JudgeStyle) : JudgeEvent :=
  tapLikeMissEvent note.params note.lane style

-- A successful star head also scores as Tap; only body results use the Slide family.
private def slideHeadJudgeEvent (note : SlideHeadNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  tapLikeJudgeEvent note.params note.lane grade judgeDiff

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

-- Resolve the star head using the accepted click or late miss, without advancing its body.
def slideHeadStep (note : SlideHeadNote) (currentTime : TimePoint) (judgeDiff : Duration) (inputClicked : Bool) (style : JudgeStyle) : SlideHeadNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let judgeableRange := (timing - JUDGABLE_RANGE_SEC, timing + JUDGABLE_RANGE_SEC)
  match note.state with
  | .Waiting =>
    if isTooLateForTapLike currentTime timing (timing + tapGoodMs) then
      ({ note with state := TapState.Ended }, some (slideHeadMissEvent note style))
    else if canEnterJudgeable currentTime judgeableRange.1 then
      if inputClicked then
        let raw := Judge.judgeTap judgeDiff note.params.isEX
        let grade := Convert.convertGrade style raw
        ({ note with state := TapState.Ended }, some (slideHeadJudgeEvent note grade judgeDiff))
      else
        ({ note with state := TapState.Judgeable }, none)
    else
      (note, none)
  | .Judgeable =>
    if isTooLateForTapLike currentTime timing (timing + tapGoodMs) then
      ({ note with state := TapState.Ended }, some (slideHeadMissEvent note style))
    else if inputClicked && canEnterJudgeable currentTime judgeableRange.1 then
      let raw := Judge.judgeTap judgeDiff note.params.isEX
      let grade := Convert.convertGrade style raw
      ({ note with state := TapState.Ended }, some (slideHeadJudgeEvent note grade judgeDiff))
    else
      (note, none)
  | .Judged _ =>
    (note, none)
  | .Ended =>
    (note, none)

def tapFamilyStep (note : TapFamilyNote) (currentTime : TimePoint) (judgeDiff : Duration) (inputClicked : Bool) (style : JudgeStyle) : TapFamilyNote × Option JudgeEvent :=
  match note with
  | .tap tap =>
      let (next, evt) := tapStep tap currentTime judgeDiff inputClicked style
      (.tap next, evt)
  | .slideHead head =>
      let (next, evt) := slideHeadStep head currentTime judgeDiff inputClicked style
      (.slideHead next, evt)

/-
  Semantic boundary for scheduler proofs.  `inputClicked = true` is the
  lifecycle's judge invocation, even when a touch judge deliberately returns
  no event for an out-of-band fast touch.  The scheduler must therefore only
  mark an input consumed when it supplies this argument as `true`.
-/
def acceptedInputEngagesJudge (inputClicked : Bool) : Bool := inputClicked

theorem accepted_input_engages_judge (h : acceptedInputEngagesJudge true = true) :
    acceptedInputEngagesJudge true = true := by
  exact h

----------------------------------------------------------------------------
-- Hold Note State
----------------------------------------------------------------------------

inductive HoldSubState where
  /-- Head is outside the input window. -/
  | HeadWaiting
  /-- Head is in its input window and awaits a press. -/
  | HeadJudgeable
  /-- Head result is fixed; body sampling starts when its check window permits. -/
  | HeadJudged (grade : JudgeGrade)
  /-- Body is currently held. -/
  | BodyHeld                           -- holding actively
  /-- Body is released; accumulated release time affects the tail grade. -/
  | BodyReleased                       -- released, accumulating release time
  /-- Terminal state carrying the final tail grade. -/
  | Ended (grade : JudgeGrade)        -- terminal
deriving Inhabited, Repr, ToJson, FromJson

/- Runtime hold note. The head is judged once, then the body samples pressed
   state through the body window; release grace and accumulated release time
   determine the final modern grade, while Classic judges the release edge. -/
structure HoldNote where
  params     : CommonNoteParams
  start      : HoldStart
  state      : HoldSubState
  length     : Duration                       -- total hold length
  buttonQueueIndex : Nat := 0
  headDiff   : Duration := Duration.zero      -- head timing diff
  headGrade  : JudgeGrade := JudgeGrade.Miss
  playerReleaseTime : Duration := Duration.zero -- accumulated release time
  releaseIgnoreTime : Duration := Duration.zero -- grace timer before release hurts score
  isClassic  : Bool := false                    -- release-edge judgment instead of press bands
  isTouchHold : Bool := false                   -- touch head rules and body ignore windows
  touchQueueIndex : Nat := 0                    -- shared sensor queue order for the head
  touchGroupId : Option Nat := none             -- group sharing head grade/diff
  touchGroupSize : Nat := 1
  touchHoldGroupId : Option Nat := none         -- separate group sharing body pressure
  touchHoldGroupSize : Nat := 1
  touchHoldGroupTriggered : Bool := false      -- member's current body-trigger flag
deriving Inhabited, Repr, ToJson, FromJson

-- Use the note's original input location for head and tail feedback.
def HoldNote.position (note : HoldNote) : RuntimePos :=
  note.start.toRuntimePos

-- Store the head result and optional touch-body trigger before entering body states.
private def holdHeadJudged (note : HoldNote) (grade : JudgeGrade) (headDiff : Duration) (groupTriggered : Bool := false) : HoldNote :=
  { note with
      state := HoldSubState.HeadJudged grade
    , headDiff := headDiff
    , headGrade := grade
    , touchHoldGroupTriggered := groupTriggered }

-- A missed head still enters the hold lifecycle so its body can affect the tail grade.
private def holdHeadMiss (note : HoldNote) (headDiff : Duration) : HoldNote :=
  holdHeadJudged note Miss headDiff

-- Adopt a touch-group head result without generating an additional head event.
private def holdHeadShared (note : HoldNote) (grade : JudgeGrade) (headDiff : Duration) : HoldNote :=
  holdHeadJudged note grade headDiff true

-- Apply tap timing and style conversion to a regular hold head.
private def judgeHoldHeadTapNow (note : HoldNote) (style : JudgeStyle) (judgeDiff : Duration) : HoldNote :=
  let raw := Judge.judgeTap judgeDiff note.params.isEX
  let grade := Convert.convertGrade style raw
  holdHeadJudged note grade judgeDiff

-- Head feedback is public; the scheduler scores the tail event for the whole hold.
private def holdHeadJudgeEvent (note : HoldNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Hold
  , phase := .head
  , grade := grade
  , diff := judgeDiff
  , position := note.position
  , noteIndex := note.params.noteIndex
  , isBreak := note.params.isBreak
  , isEX := note.params.isEX }

-- Convert the miss under the selected judge style before reporting head feedback.
private def holdHeadMissEvent (note : HoldNote) (style : JudgeStyle) (judgeDiff : Duration) : JudgeEvent :=
  holdHeadJudgeEvent note (Convert.convertGrade style JudgeGrade.Miss) judgeDiff

-- Resolve a regular head and emit its head-phase event.
private def judgeHoldHeadTapNow? (note : HoldNote) (style : JudgeStyle) (judgeDiff : Duration) : HoldNote × Option JudgeEvent :=
  let raw := Judge.judgeTap judgeDiff note.params.isEX
  let grade := Convert.convertGrade style raw
  (holdHeadJudged note grade judgeDiff, some (holdHeadJudgeEvent note grade judgeDiff))

-- Touch timing can reject an early press, leaving the head unresolved and emitting no event.
private def judgeHoldHeadTouchNow? (note : HoldNote) (style : JudgeStyle) (judgeDiff : Duration) : HoldNote × Option JudgeEvent :=
  match Judge.judgeTouch judgeDiff note.params.isEX with
  | some raw =>
      let grade := Convert.convertGrade style raw
      (holdHeadJudged note grade judgeDiff true, some (holdHeadJudgeEvent note grade judgeDiff))
  | none =>
      (note, none)

-- A waiting touch head checks timeout, shared grade, then its own click, in that order.
private def stepTouchHoldHeadWaiting
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (sharedResult : Option (JudgeGrade × Duration))
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  if currentTime > timing + touchGoodMs then
    let missed := holdHeadMiss note touchGoodMs
    (missed, some (holdHeadJudgeEvent note (Convert.convertGrade style JudgeGrade.Miss) touchGoodMs))
  else
    match sharedResult with
    | some (grade, sharedDiff) =>
        (holdHeadShared note grade sharedDiff, none)
    | none =>
        if canEnterJudgeable currentTime judgeableStart then
          if inputClicked then
            judgeHoldHeadTouchNow? note style judgeDiff
          else
            ({ note with state := HoldSubState.HeadJudgeable }, none)
        else
          (note, none)

-- An open touch head keeps the same timeout/share priority until it resolves.
private def stepTouchHoldHeadJudgeable
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (sharedResult : Option (JudgeGrade × Duration))
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  if currentTime > timing + touchGoodMs then
    let missed := holdHeadMiss note touchGoodMs
    (missed, some (holdHeadJudgeEvent note (Convert.convertGrade style JudgeGrade.Miss) touchGoodMs))
  else
    match sharedResult with
    | some (grade, sharedDiff) =>
        (holdHeadShared note grade sharedDiff, none)
    | none =>
        if inputClicked && canEnterJudgeable currentTime judgeableStart then
          judgeHoldHeadTouchNow? note style judgeDiff
        else
          (note, none)

-- Enter the regular head window, accepting a click or recording a late miss.
private def stepRegularHoldHeadWaiting
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  if currentTime > timing + tapGoodMs then
    (holdHeadMiss note tapGoodMs, some (holdHeadMissEvent note style tapGoodMs))
  else if canEnterJudgeable currentTime judgeableStart then
    if inputClicked then
      judgeHoldHeadTapNow? note style judgeDiff
    else
      ({ note with state := HoldSubState.HeadJudgeable }, none)
  else
    (note, none)

-- In this state an accepted regular-head click is checked before the late-miss branch.
private def stepRegularHoldHeadJudgeable
    (note : HoldNote)
    (currentTime timing : TimePoint)
    (judgeableStart : TimePoint)
    (judgeDiff : Duration)
    (inputClicked : Bool)
    (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  if inputClicked && canEnterJudgeable currentTime judgeableStart then
    judgeHoldHeadTapNow? note style judgeDiff
  else if currentTime > timing + tapGoodMs then
    (holdHeadMiss note tapGoodMs, some (holdHeadMissEvent note style tapGoodMs))
  else
    (note, none)

-- Off input after head resolution first spends grace, then charges the full off interval.
private def holdHeadReleaseTransition (note : HoldNote) (delta : Duration) : HoldNote × Option JudgeEvent :=
  if note.releaseIgnoreTime ≤ DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC then
    ({ note with
        releaseIgnoreTime := note.releaseIgnoreTime + delta
      , touchHoldGroupTriggered := false }, none)
  else
    ({ note with
        state := HoldSubState.BodyReleased
      , playerReleaseTime := note.playerReleaseTime + note.releaseIgnoreTime + delta
      , releaseIgnoreTime := Duration.zero
      , touchHoldGroupTriggered := false }, none)

-- Start held-body tracking from the head state, resetting release counters.
private def holdPressedTransition (note : HoldNote) : HoldNote :=
  { note with
    state := HoldSubState.BodyHeld
  , playerReleaseTime := Duration.zero
  , releaseIgnoreTime := Duration.zero
  , touchHoldGroupTriggered := note.isTouchHold }

-- A held frame cancels pending release grace while preserving previously charged release time.
private def holdKeepPressed (note : HoldNote) : HoldNote :=
  { note with releaseIgnoreTime := Duration.zero, touchHoldGroupTriggered := note.isTouchHold }

-- Leaving BodyHeld spends grace before moving to BodyReleased and charging the off interval.
private def holdReleaseTransition (note : HoldNote) (delta : Duration) : HoldNote :=
  if note.releaseIgnoreTime ≤ DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC then
    { note with releaseIgnoreTime := note.releaseIgnoreTime + delta, touchHoldGroupTriggered := false }
  else
    { note with
      state := HoldSubState.BodyReleased
    , playerReleaseTime := note.playerReleaseTime + note.releaseIgnoreTime + delta
    , releaseIgnoreTime := Duration.zero
    , touchHoldGroupTriggered := false }

-- Once grace has expired, each off frame adds directly to charged release time.
private def holdReleasedStillOff (note : HoldNote) (delta : Duration) : HoldNote :=
  { note with playerReleaseTime := note.playerReleaseTime + delta, touchHoldGroupTriggered := false }

-- Re-pressing restores BodyHeld but retains the release time already charged.
private def holdReleasedRecovered (note : HoldNote) : HoldNote :=
  { note with state := HoldSubState.BodyHeld, releaseIgnoreTime := Duration.zero, touchHoldGroupTriggered := note.isTouchHold }

/--
  Advance a hold note one frame.
  `inputPressed` = button/sensor is held this frame.
  `inputClicked` = button/sensor just pressed this frame (edge).
-/
/- One frame of hold runtime. The head is judged first; once resolved, body
   sampling starts only in the configured body window. Modern holds accumulate
   release time (with the two-frame grace), while Classic holds judge release
   timing directly. Touch-holds use the same machine but receive touch-specific
   ignore windows and may inherit a shared touch-group head result. -/
private def holdStepFuel (fuel : Nat) (note : HoldNote) (currentTime : TimePoint) (judgeDiff : Duration) (headIgnore : Duration) (tailIgnore : Duration) (inputClicked : Bool) (inputPressed : Bool) (currentButtonPressed : Bool) (prevSensorPressed : Bool) (touchPanelOffset : Duration) (sharedResult : Option (JudgeGrade × Duration)) (delta : Duration) (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  let timing := note.params.effectiveTiming
  let bodyTiming := if note.isTouchHold then note.params.judgeTiming else timing
  let diff := currentTime - timing
  let bodyCheckStart := bodyTiming + headIgnore
  let bodyCheckEnd   := bodyTiming + note.length - tailIgnore
  let classicBodyCheckStart := timing - tapGoodMs
  let bodyWindowDisabled := !note.isClassic && note.length ≤ headIgnore + tailIgnore
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
    let evt : JudgeEvent :=
      { kind := .Hold
      , phase := .tail
      , grade := finalGrade'
      , diff := eventDiff
      , position := note.position
      , noteIndex := note.params.noteIndex
      , isBreak := note.params.isBreak }
    ({ note with state := HoldSubState.Ended finalGrade', touchHoldGroupTriggered := false }, some evt)
  match note.state with
  | .HeadWaiting =>
    let (next, evt) :=
      if note.isTouchHold then
        stepTouchHoldHeadWaiting note currentTime timing judgeableRange.1 judgeDiff inputClicked sharedResult style
      else
        stepRegularHoldHeadWaiting note currentTime timing judgeableRange.1 judgeDiff inputClicked style
    match next.state with
    | .HeadWaiting | .HeadJudgeable => (next, evt)
    | .HeadJudged grade =>
        if note.isTouchHold && !inputClicked then
          (next, evt)
        else if currentTime ≥ bodyCheckStart then
          if diff ≥ note.length then endHold next grade currentTime next.playerReleaseTime
          else if inputPressed then (holdPressedTransition next, evt)
          else (holdHeadReleaseTransition { next with headGrade := grade } delta).1 |> fun n => (n, evt)
        else (next, evt)
    | .BodyHeld | .BodyReleased | .Ended _ => (next, evt)
  | .HeadJudgeable =>
    let (next, evt) :=
      if note.isTouchHold then
        stepTouchHoldHeadJudgeable note currentTime timing judgeableRange.1 judgeDiff inputClicked sharedResult style
      else
        stepRegularHoldHeadJudgeable note currentTime timing judgeableRange.1 judgeDiff inputClicked style
    match next.state with
    | .HeadWaiting | .HeadJudgeable => (next, evt)
    | .HeadJudged grade =>
        if note.isTouchHold && !inputClicked then
          (next, evt)
        else if currentTime ≥ bodyCheckStart then
          if diff ≥ note.length then endHold next grade currentTime next.playerReleaseTime
          else if inputPressed then (holdPressedTransition next, evt)
          else (holdHeadReleaseTransition { next with headGrade := grade } delta).1 |> fun n => (n, evt)
        else (next, evt)
    | .BodyHeld | .BodyReleased | .Ended _ => (next, evt)
  | .HeadJudged headGrade =>
    if note.isClassic then
      if currentTime < classicBodyCheckStart then
        (note, none)
      else if diff >= note.length + CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC || headGrade.isMissOrTooFast then
        endHold note headGrade currentTime note.playerReleaseTime
      else if inputPressed then
        ({ note with state := HoldSubState.BodyHeld, touchHoldGroupTriggered := note.isTouchHold }, none)
      else
        endHold note headGrade (currentTime - releaseOffset) note.playerReleaseTime
    else if currentTime < bodyCheckStart then
      (note, none)
    else if bodyWindowDisabled then
      if diff >= note.length then
        endHold note headGrade currentTime note.playerReleaseTime
      else
        (note, none)
    else if diff >= note.length then
      endHold note headGrade currentTime note.playerReleaseTime
    else if currentTime > bodyCheckEnd then
      (note, none)
    else if inputPressed then
      (holdPressedTransition note, none)
    else
      -- MajdataPlay seeds release-ignore away after a missed/too-fast head.
      let note := { note with headGrade := headGrade }
      holdHeadReleaseTransition note delta
  | .BodyHeld =>
    if note.isClassic then
      if diff >= note.length + CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC || note.headGrade.isMissOrTooFast then
        endHold note note.headGrade currentTime note.playerReleaseTime
      else if inputPressed then
        ({ note with touchHoldGroupTriggered := note.isTouchHold }, none)
      else
        endHold note note.headGrade (currentTime - releaseOffset) note.playerReleaseTime
    else if bodyWindowDisabled then
      if diff >= note.length then
        endHold note note.headGrade currentTime note.playerReleaseTime
      else
        (note, none)
    else if diff >= note.length then
      endHold note note.headGrade currentTime note.playerReleaseTime
    else if currentTime > bodyCheckEnd then
      (note, none)
    else if inputPressed then
      (holdKeepPressed note, none)
    else
      (holdReleaseTransition note delta, none)
  | .BodyReleased =>
    if bodyWindowDisabled then
      if diff >= note.length then
        endHold note note.headGrade currentTime note.playerReleaseTime
      else
        (note, none)
    else if diff >= note.length then
      endHold note note.headGrade currentTime note.playerReleaseTime
    else if currentTime > bodyCheckEnd then
      (note, none)
    else if inputPressed then
      (holdReleasedRecovered note, none)
    else
      (holdReleasedStillOff note delta, none)
  | .Ended _ =>
    (note, none)

/- Advance a regular hold or touch-hold. Clicks resolve heads; pressed state samples the body.
   The scheduler supplies ignore windows, source-specific offsets, and any shared head result.
   Returns the updated note and optional head/tail feedback; only the tail scores the hold. -/
def holdStep (note : HoldNote) (currentTime : TimePoint) (judgeDiff : Duration) (headIgnore : Duration) (tailIgnore : Duration) (inputClicked : Bool) (inputPressed : Bool) (currentButtonPressed : Bool) (prevSensorPressed : Bool) (touchPanelOffset : Duration) (sharedResult : Option (JudgeGrade × Duration)) (delta : Duration) (style : JudgeStyle) : HoldNote × Option JudgeEvent :=
  holdStepFuel 1 note currentTime judgeDiff headIgnore tailIgnore inputClicked inputPressed
    currentButtonPressed prevSensorPressed touchPanelOffset sharedResult delta style

----------------------------------------------------------------------------
-- Touch Note State
----------------------------------------------------------------------------

inductive TouchState where
  | Waiting
  | Judgeable
  | Judged (grade : JudgeGrade)
  | Ended
deriving Inhabited, Repr, ToJson, FromJson

structure TouchNote where
  params       : CommonNoteParams
  state        : TouchState
  sensorPos    : SensorArea
  touchQueueIndex : Nat := 0
  touchGroupId : Option Nat := none
  touchGroupSize : Nat := 1
deriving Inhabited, Repr, ToJson, FromJson

private def touchMissEvent (note : TouchNote) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Touch
  , phase := .head
  , grade := Miss
  , diff := judgeDiff
  , position := .sensor note.sensorPos
  , noteIndex := note.params.noteIndex
  , isBreak := note.params.isBreak
  , isEX := note.params.isEX }

private def touchTooLateMissEvent (note : TouchNote) : JudgeEvent :=
  -- MajdataPlay records the touch good-area boundary as the timeout diff.
  touchMissEvent note TOUCH_JUDGE_GOOD_AREA_MSEC

private def touchJudgeEvent (note : TouchNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Touch
  , phase := .head
  , grade := grade
  , diff := judgeDiff
  , position := .sensor note.sensorPos
  , noteIndex := note.params.noteIndex
  , isBreak := note.params.isBreak
  , isEX := note.params.isEX }

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
    if currentTime > timing + touchGoodMs then
      ({ note with state := TouchState.Ended }, some (touchTooLateMissEvent note))
    else
      match sharedResult with
      | some (grade, sharedDiff) =>
          ({ note with state := TouchState.Ended }, some (touchJudgeEvent note grade sharedDiff))
      | none =>
          if canEnterJudgeable currentTime judgeableRange.1 then
            if inputClicked then
              judgeTouchNow? note style judgeDiff
            else
              ({ note with state := TouchState.Judgeable }, none)
          else
            (note, none)
  | .Judgeable =>
    if currentTime > timing + touchGoodMs then
      ({ note with state := TouchState.Ended }, some (touchTooLateMissEvent note))
    else
      match sharedResult with
      | some (grade, sharedDiff) =>
          ({ note with state := TouchState.Ended }, some (touchJudgeEvent note grade sharedDiff))
      | none =>
          if inputClicked then
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
  /-- Body is dormant until its head window or connected parent unlocks it. -/
  | Waiting
  /-- Body consumes sensor frames; waitTime is the end-judge grace window. -/
  | Active  (waitTime : Duration)
  /-- Queue completion has been judged; retain the result through its grace window. -/
  | Judged  (grade : JudgeGrade) (waitTime : Duration) (judgeDiff : Duration)
  /-- Terminal state. -/
  | Ended
deriving Inhabited, Repr, ToJson, FromJson

/--
  One ordered sensor checkpoint in a slide body. `wasOn`/`wasOff` are the
  aggregate history; target histories retain each sensor's independent press
  and release state for multi-sensor AND/OR checkpoints. The next checkpoint
  can be inspected when the current checkpoint is skippable or has been pressed.
-/
structure SlideArea where
  targetAreas : List SensorArea                 -- sensors belonging to this checkpoint
  policy      : AreaPolicy := AreaPolicy.Or     -- combine per-sensor completion with OR/AND
  isLast      : Bool := false                   -- terminal checkpoints need no release
  isSkippable : Bool := true                    -- permit lookahead before this area is pressed
  arrowProgressWhenOn : Nat := 0                -- bar-hide index after a press
  arrowProgressWhenFinished : Nat := 0          -- bar-hide index after completion
  wasOn       : Bool := false                   -- latched: any target has ever been pressed
  wasOff      : Bool := false                   -- policy-combined press-and-release history
  /-- Per-target history needed by multi-sensor AND/OR areas. -/
  targetWasOn : List Bool := []
  targetWasOff : List Bool := []                -- aligned with targetAreas and targetWasOn
deriving Inhabited, Repr, ToJson, FromJson

-- Historical press flag; releasing a sensor does not reset On.
def SlideArea.on (area : SlideArea) : Bool :=
  area.wasOn

/-- A final checkpoint completes on press; other checkpoints require press and release. -/
def SlideArea.isFinished (area : SlideArea) : Bool :=
  if area.targetAreas.isEmpty then
    false
  else if area.targetWasOn.isEmpty then
    if area.isLast then area.wasOn else area.wasOn && area.wasOff
  else
    let finished := (area.targetWasOn.zip area.targetWasOff).map
      (fun pair => pair.1 && (area.isLast || pair.2))
    match area.policy with
    | .Or => finished.any id
    | .And => finished.all id

-- An absent sensor entry is treated as off.
private def sensorHeldAt (sensorHeld : SensorVec Bool) (area : SensorArea) : Bool :=
  sensorHeld.getD area false

-- Latch presses and subsequent releases independently for every target sensor.
private def slideAreaTargetHistory
    (targets : List SensorArea) (wasOn wasOff : List Bool) (sensorHeld : SensorVec Bool) :
    List Bool × List Bool :=
  let rec go (targets : List SensorArea) (wasOn wasOff : List Bool) : List Bool × List Bool :=
    match targets with
    | [] => ([], [])
    | target :: rest =>
        let oldOn := wasOn.headD false
        let oldOff := wasOff.headD false
        let held := sensorHeldAt sensorHeld target
        let newOn := oldOn || held
        let newOff := oldOff || (oldOn && !held)
        let (restOn, restOff) := go rest (wasOn.drop 1) (wasOff.drop 1)
        (newOn :: restOn, newOff :: restOff)
  go targets wasOn wasOff

/-- Accumulate this frame's held sensors into checkpoint history. -/
def SlideArea.check (area : SlideArea) (sensorHeld : SensorVec Bool) : SlideArea :=
  -- A one-target legacy state retains all the information needed to seed its history.
  let sourceWasOn := if area.targetWasOn.isEmpty && area.targetAreas.length == 1 then
    [area.wasOn] else area.targetWasOn
  let sourceWasOff := if area.targetWasOff.isEmpty && area.targetAreas.length == 1 then
    [area.wasOff] else area.targetWasOff
  let (targetWasOn, targetWasOff) :=
    slideAreaTargetHistory area.targetAreas sourceWasOn sourceWasOff sensorHeld
  -- Reference SlideArea.On is OR even when Policy is AND; Policy controls completion.
  let isOn := targetWasOn.any id
  let completedOff :=
    match area.policy with
    | .Or =>
        (targetWasOn.zip targetWasOff).any (fun pair => pair.1 && pair.2)
    | .And =>
        !area.targetAreas.isEmpty &&
          (targetWasOn.zip targetWasOff).all (fun pair => pair.1 && pair.2)
  { area with
      wasOn := isOn
      wasOff := completedOff
      targetWasOn := targetWasOn
      targetWasOff := targetWasOff }

/-- Ordered checkpoints for one slide track; WiFi and connection parts have one per track. -/
abbrev SlideQueue := List SlideArea

/-- Maximum remaining checkpoint count across tracks, used for connected-parent readiness. -/
def slideQueueRemaining (queues : List SlideQueue) : Nat :=
  let rec go (acc : Nat) : List SlideQueue → Nat
    | [] => acc
    | q :: rest => go (max acc q.length) rest
  go 0 queues

/-- WiFi bar progress follows the reference's special final-bar indices. -/
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

-- Render progress uses a WiFi bar index or a remaining-area count, depending on slide kind.
private def slideProgressRemaining (slideKind : SlideKind) (isClassic : Bool) (queues : List SlideQueue) : Nat :=
  match slideKind with
  | SlideKind.Wifi => wifiQueueProgressRemaining isClassic queues
  | _ => slideQueueRemaining queues

/-- True only when every track has consumed all of its checkpoints. -/
def slideQueuesCleared (queues : List SlideQueue) : Bool :=
  match queues with
  | [] => true
  | q :: rest => if q.isEmpty then slideQueuesCleared rest else false

-- Inspect the first remaining checkpoint of a track, rather than the separate star-head note.
private def slideHeadOn (queue : SlideQueue) : Bool :=
  match queue with
  | [] => false
  | area :: _ => area.on

-- Route a checkpoint's hide index to either the whole body or an indexed render track.
private def slideHideBarCmd (noteIndex : Nat) (trackIndex : Option Nat) (endIndex : Nat) : RenderCommand :=
  match trackIndex with
  | none => RenderCommand.HideSlideBars noteIndex endIndex
  | some trackIndex => RenderCommand.HideSlideTrackBars noteIndex trackIndex endIndex

-- Preserve track order when collecting each queue's render commands.
private def flattenRenderCmds : List (List RenderCommand) → List RenderCommand
  | [] => []
  | cmds :: rest => cmds ++ flattenRenderCmds rest

-- Cue candidates are tracks whose remaining head is now On while its old head was not On.
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

-- Public checkpoint-update boundary used by queue traversal.
def updateSlideArea (area : SlideArea) (sensorHeld : SensorVec Bool) : SlideArea :=
  area.check sensorHeld

/-- Flatten multi-track queue specs for proof-facing single-track reasoning. -/
def flattenSlideQueues : List SlideQueue → SlideQueue
  | [] => []
  | q :: qs => q ++ flattenSlideQueues qs

/--
  Consume one frame of sensor state for an ordered checkpoint queue. It updates
  the first area, may inspect the second when skipping is allowed or the first
  is active, removes completed areas, and emits bar-hide commands at the
  corresponding progress indices. Recursive advancement reuses the same sensor
  snapshot, so a frame can consume several checkpoints. Fuel bounds that traversal.
-/
private def slideQueueCoreFuel
    (fuel : Nat)
    (noteIndex : Nat) (trackIndex : Option Nat) (emitCmds : Bool) (queue : SlideQueue) (sensorHeld : SensorVec Bool) :
    SlideQueue × List RenderCommand :=
  match queue with
  | [] => ([], [])
  | first :: rest =>
    match fuel with
    | 0 => (queue, [])
    | fuel + 1 =>
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
              let (restQueue, restCmds) := slideQueueCoreFuel fuel noteIndex trackIndex emitCmds rest2 sensorHeld
              let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex second'.arrowProgressWhenFinished :: restCmds else []
              (restQueue, cmds)
            else if second'.on then
              let (restQueue, restCmds) := slideQueueCoreFuel fuel noteIndex trackIndex emitCmds (second' :: rest2) sensorHeld
              let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex second'.arrowProgressWhenOn :: restCmds else []
              (restQueue, cmds)
            else if first'.isFinished then
              let (restQueue, restCmds) := slideQueueCoreFuel fuel noteIndex trackIndex emitCmds (second' :: rest2) sensorHeld
              let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex first'.arrowProgressWhenFinished :: restCmds else []
              (restQueue, cmds)
            else
              ([first', second'] ++ rest2, [])
          else if first'.isFinished then
            let (restQueue, restCmds) := slideQueueCoreFuel fuel noteIndex trackIndex emitCmds rest sensorHeld
            let cmds := if emitCmds then slideHideBarCmd noteIndex trackIndex first'.arrowProgressWhenFinished :: restCmds else []
            (restQueue, cmds)
          else
            ([first'] ++ rest, [])

-- Queue length supplies enough fuel to traverse every removable checkpoint in this frame.
private def slideQueueCore
    (noteIndex : Nat) (trackIndex : Option Nat) (emitCmds : Bool) (queue : SlideQueue) (sensorHeld : SensorVec Bool) :
    SlideQueue × List RenderCommand :=
  slideQueueCoreFuel (queue.length + 1) noteIndex trackIndex emitCmds queue sensorHeld

/-- Replay queue progression without render commands. -/
def replaySlideQueue (queue : SlideQueue) (sensorHeld : SensorVec Bool) : SlideQueue :=
  (slideQueueCore 0 none false queue sensorHeld).1

-- Enable checkpoint bar-hide output for the runtime traversal.
private def updateSlideQueueWithCmds (noteIndex : Nat) (trackIndex : Option Nat) (queue : SlideQueue) (sensorHeld : SensorVec Bool) : SlideQueue × List RenderCommand :=
  slideQueueCore noteIndex trackIndex true queue sensorHeld

-- Advance one track and return its remaining queue together with render effects.
private def updateSlideQueue (noteIndex : Nat) (trackIndex : Option Nat) (queue : SlideQueue) (sensorHeld : SensorVec Bool) : SlideQueue × List RenderCommand :=
  updateSlideQueueWithCmds noteIndex trackIndex queue sensorHeld

/-
  Runtime slide-body note. A single-track slide has one queue; WiFi has three
  parallel queues. Connected children point at the preceding body and become
  checkable when its maximum remaining queue length is zero or one. Standalone
  bodies and group heads instead unlock 50 ms before headTiming. Checkability
  stays latched. Only standalone bodies and the final connected part judge themselves.
-/
structure SlideNote where
  params          : CommonNoteParams
  lane            : OuterSlot
  state           : SlideState
  length          : Duration            -- total slide length
  headTiming      : TimePoint           -- slide head timing anchor for body checkability
  startTiming     : TimePoint           -- when slide started
  groupStartTiming : Option TimePoint := none   -- shared start for early-clear wait adjustment
  slideKind       : SlideKind := .Single        -- queue/render routing
  isClassic       : Bool := false               -- fixed instead of extended judge windows
  isConnSlide     : Bool := false               -- member of a connected body chain
  parentNoteIndex : Option Nat := none          -- immediate preceding body, not every ancestor
  isGroupPartHead : Bool := false               -- first body uses the head-time eligibility gate
  isGroupPartEnd  : Bool := false               -- final body produces the group's result
  parentFinished  : Bool := false               -- refreshed by Scheduler: parent remaining = 0
  parentPendingFinish : Bool := false           -- refreshed by Scheduler: parent remaining = 1
  initialQueueRemaining : Nat := 0              -- initial maximum queue length, retained metadata
  totalJudgeQueueLen : Nat := 0                 -- group queue length, retained metadata
  trackCount      : Nat := 1                    -- number of render tracks
  isCheckable     : Bool := false               -- latched permission to sample body sensors
  slideSoundPlayed : Bool := false              -- records whether a cue has been observed
  multiple        : Nat := 1                    -- score multiplicity carried by the final event
  judgeQueues     : List SlideQueue := []       -- independent remaining queues and sensor history
deriving Inhabited, Repr, ToJson, FromJson

-- Body judgment feedback is anchored at the note's outer lane.
def SlideNote.position (note : SlideNote) : RuntimePos :=
  .button note.lane.toButtonZone

-- Single-track output has no track index; multi-track queues receive zero-based indices.
private def SlideNote.queueTracks (note : SlideNote) : List (Option Nat × SlideQueue) :=
  if note.trackCount = 1 then
    note.judgeQueues.map (fun queue => (none, queue))
  else
    ((List.range note.judgeQueues.length).map some).zip note.judgeQueues

-- Enumerate render destinations independently of the remaining queue lengths.
private def SlideNote.trackRenderIndices (note : SlideNote) : List Nat :=
  List.range note.trackCount

-- Publish the kind-specific progress value to the body's render destination(s).
private def slideProgressRenderCmds (note : SlideNote) (remaining : Nat) : List RenderCommand :=
  match note.slideKind with
  | SlideKind.Single => [RenderCommand.UpdateSlideProgress note.params.noteIndex remaining]
  | SlideKind.Wifi | SlideKind.ConnPart =>
      note.trackRenderIndices.map (fun trackIndex => RenderCommand.UpdateSlideTrackProgress note.params.noteIndex trackIndex remaining)

-- Ending a body hides every bar regardless of its individual track progress.
private def slideHideRenderCmds (note : SlideNote) : List RenderCommand :=
  match note.slideKind with
  | SlideKind.Single => [RenderCommand.HideAllSlideBars note.params.noteIndex]
  | SlideKind.Wifi | SlideKind.ConnPart =>
      [RenderCommand.HideAllSlideBars note.params.noteIndex]

-- Immutable frame inputs shared by the state, judge, and output phases of a slide step.
structure SlideStepContext where
  currentTime : TimePoint
  touchPanelOffset : Duration
  delta : Duration
  style : JudgeStyle
  subdivideSlideJudgeGrade : Bool
  sensorHeld : SensorVec Bool

-- Transition result before audio/render encoding; progress fields also detect bar updates.
structure SlideStepSemantic where
  note : SlideNote
  event : Option JudgeEvent := none
  queueRenderCmds : List RenderCommand := []
  oldRemaining : Nat := 0
  newRemaining : Nat := 0
  trackOns : List Nat := []                     -- track indices detected by collectNewSlideOnTracks
  progressChanged : Bool := false
  hideSlide : Bool := false
  shouldPlayTrackOns : Bool := false
  emitProgressRender : Bool := false

-- Final output applies judge-style conversion, then collapses perfect subgrades if configured.
private def slideEffectiveJudgeGrade
    (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool) (raw : JudgeGrade) : JudgeGrade :=
  let converted := Convert.convertGrade style raw
  if subdivideSlideJudgeGrade then converted else Judge.correctSlideGrade converted

-- Body judgment uses panel-adjusted time against the offset-adjusted judge anchor.
private def slideCurrentJudgeDiff (note : SlideNote) (currentTime : TimePoint) (touchPanelOffset : Duration) : Duration :=
  (currentTime - touchPanelOffset) - note.params.effectiveTiming

private def slideTooLateJudgeDiff : Duration :=
  -- MajdataPlay's SlideBase.TooLateJudge leaves NoteDrop.JudgeDiff at its default -1ms.
  Duration.fromMicros (-1000)

-- Arrival minus the stored judge anchor supplies both modern window extension and clear delay.
private def slideInitialWaitTime (note : SlideNote) : Duration :=
  note.startTiming + note.length - note.params.judgeTiming

-- Eligibility stays true once latched. New connected children use parent progress, not head time.
def slideShouldBeCheckable (note : SlideNote) (currentTime : TimePoint) : Bool :=
  let headTiming := currentTime - note.headTiming
  if note.isCheckable then
    true
  else if note.isConnSlide then
    if note.isGroupPartHead then
      headTiming >= Duration.fromMicros (-50000)
    else
      note.parentFinished || note.parentPendingFinish
  else
    headTiming >= Duration.fromMicros (-50000)

-- Uncheckable bodies preserve all queues; eligible tracks sample the same held-sensor snapshot.
def slideUpdatedQueuesWithCmds
    (note : SlideNote) (isCheckable : Bool) (sensorHeld : SensorVec Bool) :
    List (SlideQueue × List RenderCommand) :=
  if isCheckable then
    note.queueTracks.map (fun (trackIndex, queue) =>
      updateSlideQueue note.params.noteIndex trackIndex queue sensorHeld)
  else
    note.judgeQueues.map (fun queue => (queue, []))

-- Timeout is after arrival plus the Good window; only a negative judge offset moves it earlier.
private def slideTooLateTiming (note : SlideNote) : TimePoint :=
  note.startTiming + note.length + SLIDE_JUDGE_GOOD_AREA_MSEC + min note.params.judgeOffset Duration.zero

-- Early clears wait half the time until group start; very late modern clears wait only 50 ms.
private def slideAdjustedJudgedWaitTime
    (note : SlideNote) (currentTime : TimePoint) (waitTime judgeDiff : Duration) : Duration :=
  let remainingStartTime := currentTime - note.groupStartTiming.getD note.startTiming
  if remainingStartTime < Duration.zero then
    Duration.divNat (Duration.abs remainingStartTime) 2
  else if !note.isClassic && judgeDiff ≥ SLIDE_JUDGE_GOOD_AREA_MSEC then
    SLIDE_JUDGED_LATE_CLEAR_WAIT
  else
    waitTime

-- Build the body score result, preserving flags and enforcing positive score multiplicity.
private def slideJudgeEvent (note : SlideNote) (grade : JudgeGrade) (judgeDiff : Duration) : JudgeEvent :=
  { kind := .Slide
  , phase := .head
  , grade := grade
  , diff := judgeDiff
  , position := note.position
  , noteIndex := note.params.noteIndex
  , isBreak := note.params.isBreak
  , isEX := note.params.isEX
  , multiple := max 1 note.multiple }

-- Record queue changes and detect either kind-specific progress or remaining-count changes.
private def buildSlideSemanticBase
    (note : SlideNote) (updatedQueues : List SlideQueue) (queueRenderCmds : List RenderCommand)
    (oldRemaining newRemaining : Nat) (trackOns : List Nat) : SlideStepSemantic :=
  { note := { note with judgeQueues := updatedQueues }
  , queueRenderCmds := queueRenderCmds
  , oldRemaining := oldRemaining
  , newRemaining := newRemaining
  , trackOns := trackOns
  , progressChanged :=
      newRemaining != oldRemaining || slideQueueRemaining updatedQueues != slideQueueRemaining note.judgeQueues }

-- Snapshot existing queue progress for phases that do not sample sensors.
private def buildSlideStaticSemanticBase
    (note : SlideNote) (isCheckable : Bool) : SlideStepSemantic :=
  let remaining := slideProgressRemaining note.slideKind note.isClassic note.judgeQueues
  { note := { note with isCheckable := isCheckable }
  , oldRemaining := remaining
  , newRemaining := remaining }

-- Advance all eligible queues and collect their render effects, cue candidates, and progress.
private def buildSlideSensorSemanticBase
    (note : SlideNote) (ctx : SlideStepContext) (isCheckable : Bool) : SlideStepSemantic :=
  let updatedQueuesWithCmds := slideUpdatedQueuesWithCmds note isCheckable ctx.sensorHeld
  let updatedQueues := updatedQueuesWithCmds.map Prod.fst
  let queueRenderCmds := flattenRenderCmds (updatedQueuesWithCmds.map Prod.snd)
  let oldRemaining := slideProgressRemaining note.slideKind note.isClassic note.judgeQueues
  let newRemaining := slideProgressRemaining note.slideKind note.isClassic updatedQueues
  let trackOns :=
    if isCheckable then collectNewSlideOnTracks 0 note.judgeQueues updatedQueues else []
  buildSlideSemanticBase { note with isCheckable := isCheckable } updatedQueues queueRenderCmds
    oldRemaining newRemaining trackOns

-- Restore Waiting without sampling sensors or producing effects; retain the original queues.
def buildSlideDormantSemanticBase (note : SlideNote) : SlideStepSemantic :=
  { note := { note with state := SlideState.Waiting, isCheckable := false } }

-- Timeout judges pre-sensor queue counts and ends immediately, without a judged wait interval.
private def slideTooLateStepSemantic
    (note : SlideNote) (ctx : SlideStepContext) (isCheckable : Bool) : SlideStepSemantic :=
  let staticBase := buildSlideStaticSemanticBase note isCheckable
  let raw := Judge.judgeSlideTooLate (slideQueueRemaining note.judgeQueues)
  let grade := slideEffectiveJudgeGrade ctx.style ctx.subdivideSlideJudgeGrade raw
  { staticBase with
    note := { staticBase.note with state := SlideState.Ended }
    event := if note.isConnSlide && !note.isGroupPartEnd then none
      else some (slideJudgeEvent note grade slideTooLateJudgeDiff)
    hideSlide := true }

/- Check prior queue completion and timeout before sampling this frame's sensors. Newly cleared
   queues are judged on the next step. Non-final connected parts only advance their queues. -/
private def slideActiveStepSemantic
    (note : SlideNote) (ctx : SlideStepContext) (isJudgable : Bool) (waitTime : Duration) :
    SlideStepSemantic :=
  let activeNote := { note with state := SlideState.Active waitTime, isCheckable := true }
  let staticBase := buildSlideStaticSemanticBase activeNote true
  let isTooLate := ctx.currentTime > slideTooLateTiming activeNote
  if isJudgable && slideQueuesCleared activeNote.judgeQueues then
    let judgeDiff := slideCurrentJudgeDiff activeNote ctx.currentTime ctx.touchPanelOffset
    let raw :=
      if activeNote.isClassic then
        Judge.judgeSlideClassic judgeDiff
      else
        Judge.judgeSlideModern judgeDiff waitTime activeNote.params.isEX
    let storedGrade :=
      if activeNote.isClassic then
        raw
      else
        Convert.convertGrade ctx.style raw
    let judgedWaitTime := slideAdjustedJudgedWaitTime activeNote ctx.currentTime waitTime judgeDiff
    { staticBase with
      note := { staticBase.note with state := SlideState.Judged storedGrade judgedWaitTime judgeDiff }
      shouldPlayTrackOns := activeNote.isGroupPartHead || !activeNote.isConnSlide
      emitProgressRender := true }
  else if isJudgable && isTooLate then
    slideTooLateStepSemantic activeNote ctx true
  else
    let semanticBase := buildSlideSensorSemanticBase activeNote ctx true
    { semanticBase with
      shouldPlayTrackOns := activeNote.isGroupPartHead || !activeNote.isConnSlide
      emitProgressRender := semanticBase.progressChanged }

/- Select the eligibility/state branch. An uncheckable final part can still time out and end
   the chain. Judged bodies emit their result when the stored wait is already nonpositive. -/
def slideStepSemantic (note : SlideNote) (ctx : SlideStepContext) : SlideStepSemantic :=
  let isCheckable := slideShouldBeCheckable note ctx.currentTime
  let isJudgable := note.isGroupPartEnd || !note.isConnSlide
  match note.state with
  | .Waiting =>
    if isCheckable then
      slideActiveStepSemantic note ctx isJudgable (slideInitialWaitTime note)
    else if isJudgable && ctx.currentTime > slideTooLateTiming note then
      slideTooLateStepSemantic note ctx false
    else
      buildSlideDormantSemanticBase note
  | .Active waitTime =>
    if !isCheckable then
      if isJudgable && ctx.currentTime > slideTooLateTiming note then
        slideTooLateStepSemantic note ctx false
      else
        buildSlideDormantSemanticBase note
    else
      slideActiveStepSemantic note ctx isJudgable waitTime
  | .Judged grade waitTime storedJudgeDiff =>
    let staticBase := buildSlideStaticSemanticBase note isCheckable
    if waitTime ≤ Duration.zero then
      let finalGrade := slideEffectiveJudgeGrade ctx.style ctx.subdivideSlideJudgeGrade grade
      { staticBase with
        note := { staticBase.note with state := SlideState.Ended }
        event := if note.isConnSlide && !note.isGroupPartEnd then none
          else some (slideJudgeEvent note finalGrade storedJudgeDiff)
        hideSlide := true }
    else
      let newWait := waitTime - ctx.delta
      { staticBase with
        note := { staticBase.note with state := SlideState.Judged grade newWait storedJudgeDiff }
        shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
        emitProgressRender := staticBase.progressChanged }
  | .Ended =>
      let staticBase := buildSlideStaticSemanticBase note isCheckable
      { staticBase with note := { staticBase.note with state := SlideState.Ended } }

-- Encode cue candidates only for standalone slides and connected group heads.
private def slideSemanticAudioCmds (semantic : SlideStepSemantic) (currentTime : TimePoint) : List AudioCommand :=
  if semantic.shouldPlayTrackOns && !semantic.trackOns.isEmpty then
    semantic.trackOns.map
      (fun trackIndex =>
        AudioCommand.PlaySlideCue semantic.note.params.noteIndex trackIndex semantic.note.params.isBreak
          currentTime)
  else []

-- Emit checkpoint hides, then progress updates, then any whole-body hide, in that order.
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

/-
  Advance one body and return (updated note, optional result, audio commands, render commands).
  Completion and timeout are checked before sensor traversal. Normal judgment stores the result
  until its wait interval expires; timeout reports immediately. Scheduler applies parent effects.
-/
def slideStep (note : SlideNote) (currentTime : TimePoint) (sensorHeld : SensorVec Bool)
    (touchPanelOffset : Duration) (delta : Duration) (style : JudgeStyle) (subdivideSlideJudgeGrade : Bool)
    : SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand :=
  let ctx : SlideStepContext :=
    { currentTime := currentTime
    , touchPanelOffset := touchPanelOffset
    , delta := delta
    , style := style
    , subdivideSlideJudgeGrade := subdivideSlideJudgeGrade
    , sensorHeld := sensorHeld }
  let semantic := slideStepSemantic note ctx
  let shouldMarkSlideSound := semantic.shouldPlayTrackOns &&
    !semantic.note.slideSoundPlayed && !semantic.trackOns.isEmpty
  let semantic :=
    if shouldMarkSlideSound then
      { semantic with note := { semantic.note with slideSoundPlayed := true } }
    else semantic
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
