import Verification.Bridge
import LnmaiCore.Lifecycle
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.TapLifecycleProof
open Bridge
open LnmaiCore
open private canEnterJudgeable isTooLateForTapLike tapLikeMissEvent
  tapLikeJudgeEvent tapMissEvent tapJudgeEvent judgeTapNow slideHeadMissEvent
  slideHeadJudgeEvent from LnmaiCore.Lifecycle

def toState : lifecycle.TapState → Lifecycle.TapState
  | .Waiting => .Waiting
  | .Judgeable => .Judgeable
  | .Judged grade => .Judged (toLnmJudgeGrade grade)
  | .Ended => .Ended

def ofState : Lifecycle.TapState → lifecycle.TapState
  | .Waiting => .Waiting
  | .Judgeable => .Judgeable
  | .Judged grade => .Judged (ofLnmJudgeGrade grade)
  | .Ended => .Ended

@[simp] theorem toState_ofState (state : Lifecycle.TapState) :
    toState (ofState state) = state := by
  cases state <;> simp [toState, ofState]

@[simp] theorem ofState_toState (state : lifecycle.TapState) :
    ofState (toState state) = state := by
  cases state <;> simp [toState, ofState]

/-- Reference interpretation of the runtime's event adapter. The exact-number
    time and judgment adapters and event construction remain caller contracts. -/
def eventFor (action : lifecycle.TapAction) (params : Lifecycle.CommonNoteParams)
    (lane : OuterSlot) (style : JudgeStyle) (diff : Duration) : Option JudgeEvent :=
  match action with
  | .NoEvent => none
  | .Miss => some (tapLikeMissEvent params lane style)
  | .Judge => some (tapLikeJudgeEvent params lane
      (Convert.convertGrade style (Judge.judgeTap diff params.isEX)) diff)

/-- Compose the actual extracted control flow with the exact-time/event
    interpretation of the runtime adapter. -/
def tapStep (note : Lifecycle.TapNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (style : JudgeStyle) :
    Result (Lifecycle.TapNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let (state, action) ← lifecycle.tap_transition (ofState note.state)
    (now > timing + Constants.tapGoodMs)
    (now ≥ timing - Constants.JUDGABLE_RANGE_SEC) clicked
  ok ({ note with state := toState state }, eventFor action note.params note.lane style diff)

def slideHeadStep (note : Lifecycle.SlideHeadNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (style : JudgeStyle) :
    Result (Lifecycle.SlideHeadNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let (state, action) ← lifecycle.tap_transition (ofState note.state)
    (now > timing + Constants.tapGoodMs)
    (now ≥ timing - Constants.JUDGABLE_RANGE_SEC) clicked
  ok ({ note with state := toState state }, eventFor action note.params note.lane style diff)

/-- Every branch of the tap lifecycle, including restored Judged states and
    nonmonotone frame times. No reachability or fixed-size time bound is assumed. -/
theorem tap_step_equiv (note : Lifecycle.TapNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (style : JudgeStyle) :
    tapStep note now diff clicked style = ok (Lifecycle.tapStep note now diff clicked style) := by
  rcases note with ⟨params, lane, state, queueIndex⟩
  cases state <;> cases clicked <;>
    simp [tapStep, lifecycle.tap_transition, ofState, toState, eventFor,
      Lifecycle.tapStep, canEnterJudgeable, isTooLateForTapLike,
      judgeTapNow, tapMissEvent, tapJudgeEvent] <;>
    split_ifs <;> simp_all

/-- Star heads preserve their logical slide ID and queue index while using
    the same extracted control flow and Tap-family events. -/
theorem slide_head_step_equiv (note : Lifecycle.SlideHeadNote) (now : TimePoint)
    (diff : Duration) (clicked : Bool) (style : JudgeStyle) :
    slideHeadStep note now diff clicked style =
      ok (Lifecycle.slideHeadStep note now diff clicked style) := by
  rcases note with ⟨params, lane, state, slideId, queueIndex⟩
  cases state <;> cases clicked <;>
    simp [slideHeadStep, lifecycle.tap_transition, ofState, toState, eventFor,
      Lifecycle.slideHeadStep, canEnterJudgeable, isTooLateForTapLike,
      slideHeadMissEvent, slideHeadJudgeEvent] <;>
    split_ifs <;> simp_all

def tapFamilyStep (note : Lifecycle.TapFamilyNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (style : JudgeStyle) :
    Result (Lifecycle.TapFamilyNote × Option JudgeEvent) := do
  match note with
  | .tap note =>
      let (next, event) ← tapStep note now diff clicked style
      ok (.tap next, event)
  | .slideHead note =>
      let (next, event) ← slideHeadStep note now diff clicked style
      ok (.slideHead next, event)

theorem tap_family_step_equiv (note : Lifecycle.TapFamilyNote) (now : TimePoint)
    (diff : Duration) (clicked : Bool) (style : JudgeStyle) :
    tapFamilyStep note now diff clicked style =
      ok (Lifecycle.tapFamilyStep note now diff clicked style) := by
  cases note <;> simp [tapFamilyStep, tap_step_equiv, slide_head_step_equiv,
    Lifecycle.tapFamilyStep]

structure Frame where
  now : TimePoint
  diff : Duration
  clicked : Bool
  style : JudgeStyle

def trace : Lifecycle.TapFamilyNote → List Frame →
    Result (List (Lifecycle.TapFamilyNote × Option JudgeEvent))
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← tapFamilyStep note frame.now frame.diff frame.clicked frame.style
      let rest ← trace next.1 frames
      ok (next :: rest)

def referenceTrace : Lifecycle.TapFamilyNote → List Frame →
    List (Lifecycle.TapFamilyNote × Option JudgeEvent)
  | _, [] => []
  | note, frame :: frames =>
      let next := Lifecycle.tapFamilyStep note frame.now frame.diff frame.clicked frame.style
      next :: referenceTrace next.1 frames

/-- Complete intermediate notes and optional events agree at every frame,
    including empty/no-event frames, under the adapter interpretation above. -/
theorem trace_equiv (note : Lifecycle.TapFamilyNote) (frames : List Frame) :
    trace note frames = ok (referenceTrace note frames) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih =>
      simp [trace, referenceTrace, tap_family_step_equiv, ih]

theorem accepted_input_engages_judge_equiv (clicked : Bool) :
    lifecycle.accepted_input_engages_judge clicked =
      ok (Lifecycle.acceptedInputEngagesJudge clicked) := by
  rfl

end Verification.TapLifecycleProof
