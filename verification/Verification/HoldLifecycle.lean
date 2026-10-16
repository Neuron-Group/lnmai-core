import Verification.Bridge
import LnmaiCore.Lifecycle
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 1500000

namespace Verification.HoldLifecycleProof
open Bridge LnmaiCore
open private canEnterJudgeable holdHeadJudged holdHeadMiss holdHeadShared
  holdHeadJudgeEvent holdHeadMissEvent judgeHoldHeadTapNow? judgeHoldHeadTouchNow?
  stepTouchHoldHeadWaiting stepTouchHoldHeadJudgeable
  stepRegularHoldHeadWaiting stepRegularHoldHeadJudgeable
  holdHeadReleaseTransition holdPressedTransition holdKeepPressed
  holdReleaseTransition holdReleasedStillOff holdReleasedRecovered holdStepFuel
  from LnmaiCore.Lifecycle

def toState : lifecycle.HoldSubState → Lifecycle.HoldSubState
  | .HeadWaiting => .HeadWaiting
  | .HeadJudgeable => .HeadJudgeable
  | .HeadJudged grade => .HeadJudged (toLnmJudgeGrade grade)
  | .BodyHeld => .BodyHeld
  | .BodyReleased => .BodyReleased
  | .Ended grade => .Ended (toLnmJudgeGrade grade)

def ofState : Lifecycle.HoldSubState → lifecycle.HoldSubState
  | .HeadWaiting => .HeadWaiting
  | .HeadJudgeable => .HeadJudgeable
  | .HeadJudged grade => .HeadJudged (ofLnmJudgeGrade grade)
  | .BodyHeld => .BodyHeld
  | .BodyReleased => .BodyReleased
  | .Ended grade => .Ended (ofLnmJudgeGrade grade)

@[simp] theorem toState_ofState (state : Lifecycle.HoldSubState) :
    toState (ofState state) = state := by cases state <;> simp [toState, ofState]

@[simp] theorem ofState_toState (state : lifecycle.HoldSubState) :
    ofState (toState state) = state := by cases state <;> simp [toState, ofState]

@[simp] theorem is_miss_or_too_fast_equiv (grade : JudgeGrade) :
    types.JudgeGrade.is_miss_or_too_fast (ofLnmJudgeGrade grade) =
      ok grade.isMissOrTooFast := by cases grade <;> rfl

structure Frame where
  now : TimePoint
  diff : Duration
  headIgnore : Duration
  tailIgnore : Duration
  clicked : Bool
  pressed : Bool
  currentButtonPressed : Bool
  prevSensorPressed : Bool
  touchPanelOffset : Duration
  shared : Option (JudgeGrade × Duration)
  delta : Duration
  style : JudgeStyle

/-- Interpretation of the runtime head adapter. Exact-time operations, raw
    judgments, note updates and event constructors remain adapter contracts. -/
def applyHead (note : Lifecycle.HoldNote) (f : Frame)
    (action : lifecycle.HoldHeadAction) : Lifecycle.HoldNote × Option JudgeEvent :=
  match action with
  | .Unchanged => (note, none)
  | .MakeJudgeable => ({ note with state := .HeadJudgeable }, none)
  | .Miss =>
      let diff := if note.isTouchHold then Constants.touchGoodMs else Constants.tapGoodMs
      (holdHeadMiss note diff,
        some (holdHeadJudgeEvent note (Convert.convertGrade f.style .Miss) diff))
  | .Shared => match f.shared with
      | some (grade, diff) => (holdHeadShared note grade diff, none)
      | none => (note, none)
  | .JudgeTap => judgeHoldHeadTapNow? note f.style f.diff
  | .JudgeTouch => judgeHoldHeadTouchNow? note f.style f.diff

def headStep (note : Lifecycle.HoldNote) (f : Frame) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let lateDiff := if note.isTouchHold then Constants.touchGoodMs else Constants.tapGoodMs
  let action ← lifecycle.hold_head_action (ofState note.state) note.isTouchHold
    (f.now > timing + lateDiff) (f.now ≥ timing - Constants.JUDGABLE_RANGE_SEC)
    f.clicked f.shared.isSome
  ok (applyHead note f action)

def headWaitingReference (note : Lifecycle.HoldNote) (f : Frame) :=
  let timing := note.params.effectiveTiming
  if note.isTouchHold then
    stepTouchHoldHeadWaiting note f.now timing (timing - Constants.JUDGABLE_RANGE_SEC)
      f.diff f.clicked f.shared f.style
  else
    stepRegularHoldHeadWaiting note f.now timing (timing - Constants.JUDGABLE_RANGE_SEC)
      f.diff f.clicked f.style

def headJudgeableReference (note : Lifecycle.HoldNote) (f : Frame) :=
  let timing := note.params.effectiveTiming
  if note.isTouchHold then
    stepTouchHoldHeadJudgeable note f.now timing (timing - Constants.JUDGABLE_RANGE_SEC)
      f.diff f.clicked f.shared f.style
  else
    stepRegularHoldHeadJudgeable note f.now timing (timing - Constants.JUDGABLE_RANGE_SEC)
      f.diff f.clicked f.style

theorem head_waiting_equiv (note : Lifecycle.HoldNote) (f : Frame)
    (hs : note.state = .HeadWaiting) :
    headStep note f = ok (headWaitingReference note f) := by
  cases ht : note.isTouchHold <;> cases hc : f.clicked <;> cases hshared : f.shared <;>
    simp [headStep, lifecycle.hold_head_action, hs, ofState, applyHead,
      headWaitingReference, stepTouchHoldHeadWaiting, stepRegularHoldHeadWaiting,
      canEnterJudgeable, holdHeadMissEvent, ht, hc, hshared] <;>
    split_ifs <;> simp_all

theorem head_judgeable_equiv (note : Lifecycle.HoldNote) (f : Frame)
    (hs : note.state = .HeadJudgeable) :
    headStep note f = ok (headJudgeableReference note f) := by
  cases ht : note.isTouchHold <;> cases hc : f.clicked <;> cases hshared : f.shared <;>
    simp [headStep, lifecycle.hold_head_action, hs, ofState, applyHead,
      headJudgeableReference, stepTouchHoldHeadJudgeable, stepRegularHoldHeadJudgeable,
      canEnterJudgeable, holdHeadMissEvent, ht, hc, hshared] <;>
    split_ifs <;> simp_all

def endHold (note : Lifecycle.HoldNote) (timing : TimePoint) (f : Frame)
    (grade : JudgeGrade) (releaseTiming : TimePoint) :
    Lifecycle.HoldNote × Option JudgeEvent :=
  let raw := if note.isClassic then
      Judge.judgeHoldClassicEnd grade timing note.length releaseTiming
    else Judge.judgeHoldEnd grade note.headDiff note.length
      (f.headIgnore + f.tailIgnore) note.playerReleaseTime
  let finalGrade := Convert.convertGrade f.style raw
  let eventDiff := if note.headDiff == Duration.zero && grade == .Miss then
      Time.fromMillis 150 else note.headDiff
  ({ note with state := .Ended finalGrade, touchHoldGroupTriggered := false },
    some { kind := .Hold, phase := .tail, grade := finalGrade, diff := eventDiff,
      position := note.position, noteIndex := note.params.noteIndex,
      isBreak := note.params.isBreak })

def releaseOffset (f : Frame) : Duration :=
  if f.prevSensorPressed && !f.currentButtonPressed then Duration.zero else f.touchPanelOffset

def applyBody (note : Lifecycle.HoldNote) (timing : TimePoint) (f : Frame)
    (action : lifecycle.HoldBodyAction) : Lifecycle.HoldNote × Option JudgeEvent :=
  match action with
  | .Unchanged => (note, none)
  | .EndNow grade => endHold note timing f (toLnmJudgeGrade grade) f.now
  | .EndRelease grade => endHold note timing f (toLnmJudgeGrade grade) (f.now - releaseOffset f)
  | .StartPressed => (holdPressedTransition note, none)
  | .StartClassicPressed =>
      ({ note with state := .BodyHeld, touchHoldGroupTriggered := note.isTouchHold }, none)
  | .KeepClassicPressed => ({ note with touchHoldGroupTriggered := note.isTouchHold }, none)
  | .KeepPressed => (holdKeepPressed note, none)
  | .ReleaseHead grade =>
      holdHeadReleaseTransition { note with headGrade := toLnmJudgeGrade grade } f.delta
  | .Release => (holdReleaseTransition note f.delta, none)
  | .Recover => (holdReleasedRecovered note, none)
  | .ReleasedOff => (holdReleasedStillOff note f.delta, none)

def bodyTiming (note : Lifecycle.HoldNote) : TimePoint :=
  if note.isTouchHold then note.params.judgeTiming else note.params.effectiveTiming

def afterHead (note : Lifecycle.HoldNote) (f : Frame)
    (result : Lifecycle.HoldNote × Option JudgeEvent) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let action ← lifecycle.hold_after_head_action (ofState result.1.state) note.isTouchHold
    f.clicked (f.now ≥ bodyTiming note + f.headIgnore)
    (f.now - note.params.effectiveTiming ≥ note.length) f.pressed
  let tail := applyBody result.1 note.params.effectiveTiming f action
  ok (tail.1, match tail.2 with | some event => some event | none => result.2)

def afterHeadReference (note : Lifecycle.HoldNote) (f : Frame)
    (result : Lifecycle.HoldNote × Option JudgeEvent) :
    Lifecycle.HoldNote × Option JudgeEvent :=
  let (next, event) := result
  match next.state with
  | .HeadJudged grade =>
      if note.isTouchHold && !f.clicked then (next, event)
      else if f.now ≥ bodyTiming note + f.headIgnore then
        if f.now - note.params.effectiveTiming ≥ note.length then
          endHold next note.params.effectiveTiming f grade f.now
        else if f.pressed then (holdPressedTransition next, event)
        else ((holdHeadReleaseTransition { next with headGrade := grade } f.delta).1, event)
      else (next, event)
  | _ => (next, event)

theorem after_head_equiv (note : Lifecycle.HoldNote) (f : Frame)
    (result : Lifecycle.HoldNote × Option JudgeEvent) :
    afterHead note f result = ok (afterHeadReference note f result) := by
  rcases result with ⟨next, event⟩
  cases hs : next.state <;>
    simp [afterHead, lifecycle.hold_after_head_action, hs, ofState,
      afterHeadReference, applyBody, endHold] <;>
    split_ifs <;> simp_all [holdHeadReleaseTransition]

def bodyFacts (note : Lifecycle.HoldNote) (f : Frame) : lifecycle.HoldBodyFacts :=
  let timing := note.params.effectiveTiming
  { is_classic := note.isClassic
    before_classic_start := f.now < timing - Constants.tapGoodMs
    before_body_start := f.now < bodyTiming note + f.headIgnore
    body_window_disabled := !note.isClassic && note.length ≤ f.headIgnore + f.tailIgnore
    reached_end := f.now - timing ≥ note.length
    classic_limit_reached := f.now - timing ≥
      note.length + Constants.CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC
    after_body_end := f.now > bodyTiming note + note.length - f.tailIgnore
    pressed := f.pressed }

def bodyStep (note : Lifecycle.HoldNote) (f : Frame) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let action ← lifecycle.hold_body_action (ofState note.state)
    (ofLnmJudgeGrade note.headGrade) (bodyFacts note f)
  ok (applyBody note note.params.effectiveTiming f action)

def referenceStep (note : Lifecycle.HoldNote) (f : Frame) :=
  Lifecycle.holdStep note f.now f.diff f.headIgnore f.tailIgnore f.clicked f.pressed
    f.currentButtonPressed f.prevSensorPressed f.touchPanelOffset f.shared f.delta f.style

theorem body_step_equiv (note : Lifecycle.HoldNote) (f : Frame)
    (hwaiting : note.state ≠ .HeadWaiting) (hjudgeable : note.state ≠ .HeadJudgeable) :
    bodyStep note f = ok (referenceStep note f) := by
  cases hs : note.state <;>
    simp_all [bodyStep, lifecycle.hold_body_action, ofState, bodyFacts, applyBody,
      referenceStep, Lifecycle.holdStep, holdStepFuel, bodyTiming, releaseOffset, endHold] <;>
    split_ifs <;> simp_all

def holdStep (note : Lifecycle.HoldNote) (f : Frame) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  match note.state with
  | .HeadWaiting | .HeadJudgeable =>
      let result ← headStep note f
      afterHead note f result
  | _ => bodyStep note f

/-- Single-frame equivalence of all head/body/terminal paths, including
    restored states, nonmonotone times and replacement of head feedback by a
    tail event. This composes extracted decisions with the adapter definitions
    above; it does not extract the bigint, judgment or event adapters. -/
theorem hold_step_equiv (note : Lifecycle.HoldNote) (f : Frame) :
    holdStep note f = ok (referenceStep note f) := by
  cases hs : note.state with
  | HeadWaiting =>
      simp only [holdStep, hs, head_waiting_equiv note f hs, bind_tc_ok, after_head_equiv]
      rfl
  | HeadJudgeable =>
      simp only [holdStep, hs, head_judgeable_equiv note f hs, bind_tc_ok, after_head_equiv]
      rfl
  | HeadJudged grade | BodyHeld | BodyReleased | Ended grade =>
      simpa only [holdStep, hs] using body_step_equiv note f (by simp [hs]) (by simp [hs])

def trace : Lifecycle.HoldNote → List Frame →
    Result (List (Lifecycle.HoldNote × Option JudgeEvent))
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← holdStep note frame
      let rest ← trace next.1 frames
      ok (next :: rest)

def referenceTrace : Lifecycle.HoldNote → List Frame →
    List (Lifecycle.HoldNote × Option JudgeEvent)
  | _, [] => []
  | note, frame :: frames =>
      let next := referenceStep note frame
      next :: referenceTrace next.1 frames

theorem trace_equiv (note : Lifecycle.HoldNote) (frames : List Frame) :
    trace note frames = ok (referenceTrace note frames) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih => simp [trace, referenceTrace, hold_step_equiv, ih]

end Verification.HoldLifecycleProof
