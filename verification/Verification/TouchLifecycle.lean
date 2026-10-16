import Verification.Convert
import LnmaiCore.Lifecycle
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.TouchLifecycleProof
open Bridge
open LnmaiCore
open private canEnterJudgeable touchMissEvent touchTooLateMissEvent
  touchJudgeEvent judgeTouchNow? from LnmaiCore.Lifecycle

def toState : lifecycle.TouchState → Lifecycle.TouchState
  | .Waiting => .Waiting
  | .Judgeable => .Judgeable
  | .Judged grade => .Judged (toLnmJudgeGrade grade)
  | .Ended => .Ended

def ofState : Lifecycle.TouchState → lifecycle.TouchState
  | .Waiting => .Waiting
  | .Judgeable => .Judgeable
  | .Judged grade => .Judged (ofLnmJudgeGrade grade)
  | .Ended => .Ended

@[simp] theorem toState_ofState (state : Lifecycle.TouchState) :
    toState (ofState state) = state := by
  cases state <;> simp [toState, ofState]

@[simp] theorem ofState_toState (state : lifecycle.TouchState) :
    ofState (toState state) = state := by
  cases state <;> simp [toState, ofState]

theorem finish_touch_judgment_equiv (raw : Option JudgeGrade) (style : JudgeStyle) :
    lifecycle.finish_touch_judgment (raw.map ofLnmJudgeGrade) (ofLnmJudgeStyle style) =
      ok (match raw with
        | none => (lifecycle.TouchState.Judgeable, none)
        | some grade => (lifecycle.TouchState.Ended,
            some (ofLnmJudgeGrade (Convert.convertGrade style grade)))) := by
  cases raw <;> simp [lifecycle.finish_touch_judgment, ConvertProof.convert_grade_equiv]

/-- Interpret the runtime event adapters. Exact-time arithmetic and the raw
    touch judgment are caller contracts; grade conversion is extracted. -/
def finishAction (note : Lifecycle.TouchNote) (state : lifecycle.TouchState)
    (action : lifecycle.TouchAction) (diff : Duration)
    (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    Result (Lifecycle.TouchNote × Option JudgeEvent) := do
  match action with
  | .NoEvent => ok ({ note with state := toState state }, none)
  | .Miss => ok ({ note with state := toState state }, some (touchTooLateMissEvent note))
  | .Shared => ok ({ note with state := toState state },
      shared.map (fun (grade, sharedDiff) => touchJudgeEvent note grade sharedDiff))
  | .Judge =>
      let (state, grade) ← lifecycle.finish_touch_judgment
        ((Judge.judgeTouch diff note.params.isEX).map ofLnmJudgeGrade) (ofLnmJudgeStyle style)
      ok ({ note with state := toState state },
        grade.map (fun grade => touchJudgeEvent note (toLnmJudgeGrade grade) diff))

def touchStep (note : Lifecycle.TouchNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    Result (Lifecycle.TouchNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let (state, action) ← lifecycle.touch_transition (ofState note.state)
    (now > timing + Constants.touchGoodMs)
    (now ≥ timing - Constants.JUDGABLE_RANGE_SEC) clicked shared.isSome
  finishAction note state action diff shared style

/-- Covers timeout/shared/local-input precedence, early input without an event,
    every restored state, all metadata and arbitrary (also decreasing) times. -/
theorem touch_step_equiv (note : Lifecycle.TouchNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    touchStep note now diff clicked shared style =
      ok (Lifecycle.touchStep note now diff clicked shared style) := by
  rcases note with ⟨params, state, sensor, queueIndex, groupId, groupSize⟩
  cases state <;> cases clicked <;> cases shared <;>
    cases hraw : Judge.judgeTouch diff params.isEX <;>
    simp [touchStep, lifecycle.touch_transition, ofState, toState, finishAction,
      finish_touch_judgment_equiv, Lifecycle.touchStep, canEnterJudgeable,
      judgeTouchNow?, hraw] <;>
    split_ifs <;> simp_all

structure Frame where
  now : TimePoint
  diff : Duration
  clicked : Bool
  shared : Option (JudgeGrade × Duration)
  style : JudgeStyle

def trace : Lifecycle.TouchNote → List Frame →
    Result (List (Lifecycle.TouchNote × Option JudgeEvent))
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← touchStep note frame.now frame.diff frame.clicked frame.shared frame.style
      let rest ← trace next.1 frames
      ok (next :: rest)

def referenceTrace : Lifecycle.TouchNote → List Frame →
    List (Lifecycle.TouchNote × Option JudgeEvent)
  | _, [] => []
  | note, frame :: frames =>
      let next := Lifecycle.touchStep note frame.now frame.diff frame.clicked frame.shared frame.style
      next :: referenceTrace next.1 frames

theorem trace_equiv (note : Lifecycle.TouchNote) (frames : List Frame) :
    trace note frames = ok (referenceTrace note frames) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih =>
      simp [trace, referenceTrace, touch_step_equiv, ih]

end Verification.TouchLifecycleProof
