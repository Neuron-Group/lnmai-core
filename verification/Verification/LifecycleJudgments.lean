import Verification.JudgmentAdapter
import Verification.TapLifecycle
import Verification.TouchLifecycle

/-!
Compose the runtime-used exact judgment kernels with Tap/SlideHead and Touch
transition kernels. Raw judgment agreement is derived from IntegerAgrees, not
assumed as an entire callback. Time predicates, note/event representation and
the unextracted runtime facade are still interpreted at their reference boundary.
In particular, this file does not prove the concrete BigInt implementation.
-/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.LifecycleJudgmentsProof
open Bridge LnmaiCore
open private tapLikeMissEvent tapLikeJudgeEvent touchTooLateMissEvent
  touchJudgeEvent from LnmaiCore.Lifecycle

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → ℤ)
  (h : JudgmentAdapterProof.IntegerAgrees inst ops decode)
include h

/-- Same call order as runtime tap_transition_event, including extracted grade
conversion after exact raw judgment. Events retain their original exact diff. -/
def tapEvent (action : lifecycle.TapAction) (params : Lifecycle.CommonNoteParams)
    (lane : OuterSlot) (style : JudgeStyle) (diff : I) : Result (Option JudgeEvent) := do
  match action with
  | .NoEvent => ok none
  | .Miss => ok (some (tapLikeMissEvent params lane style))
  | .Judge =>
      let raw ← judgment_adapter.judge_tap inst diff params.isEX ops
      let grade ← convert.convert_grade (ofLnmJudgeStyle style) raw
      ok (some (tapLikeJudgeEvent params lane (toLnmJudgeGrade grade)
        (Duration.fromMicros (decode diff))))

theorem tap_event_equiv (action : lifecycle.TapAction)
    (params : Lifecycle.CommonNoteParams) (lane : OuterSlot)
    (style : JudgeStyle) (diff : I) :
    tapEvent inst ops decode action params lane style diff =
      ok (TapLifecycleProof.eventFor action params lane style
        (Duration.fromMicros (decode diff))) := by
  cases action <;> simp [tapEvent, TapLifecycleProof.eventFor,
    JudgmentAdapterProof.tap_equiv inst ops decode h, ConvertProof.convert_grade_equiv]

def tapStep (note : Lifecycle.TapNote) (now : TimePoint) (diff : I)
    (clicked : Bool) (style : JudgeStyle) :
    Result (Lifecycle.TapNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let (state, action) ← lifecycle.tap_transition (TapLifecycleProof.ofState note.state)
    (now > timing + Constants.tapGoodMs)
    (now ≥ timing - Constants.JUDGABLE_RANGE_SEC) clicked
  let event ← tapEvent inst ops decode action note.params note.lane style diff
  ok ({ note with state := TapLifecycleProof.toState state }, event)

theorem tap_step_equiv (note : Lifecycle.TapNote) (now : TimePoint) (diff : I)
    (clicked : Bool) (style : JudgeStyle) :
    tapStep inst ops decode note now diff clicked style =
      ok (Lifecycle.tapStep note now (Duration.fromMicros (decode diff)) clicked style) := by
  simpa only [tapStep, TapLifecycleProof.tapStep,
    tap_event_equiv inst ops decode h, bind_tc_ok] using
    TapLifecycleProof.tap_step_equiv note now (Duration.fromMicros (decode diff)) clicked style

def slideHeadStep (note : Lifecycle.SlideHeadNote) (now : TimePoint) (diff : I)
    (clicked : Bool) (style : JudgeStyle) :
    Result (Lifecycle.SlideHeadNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let (state, action) ← lifecycle.tap_transition (TapLifecycleProof.ofState note.state)
    (now > timing + Constants.tapGoodMs)
    (now ≥ timing - Constants.JUDGABLE_RANGE_SEC) clicked
  let event ← tapEvent inst ops decode action note.params note.lane style diff
  ok ({ note with state := TapLifecycleProof.toState state }, event)

theorem slide_head_step_equiv (note : Lifecycle.SlideHeadNote)
    (now : TimePoint) (diff : I) (clicked : Bool) (style : JudgeStyle) :
    slideHeadStep inst ops decode note now diff clicked style =
      ok (Lifecycle.slideHeadStep note now (Duration.fromMicros (decode diff)) clicked style) := by
  simpa only [slideHeadStep, TapLifecycleProof.slideHeadStep,
    tap_event_equiv inst ops decode h, bind_tc_ok] using
    TapLifecycleProof.slide_head_step_equiv note now
      (Duration.fromMicros (decode diff)) clicked style

def tapFamilyStep (note : Lifecycle.TapFamilyNote) (now : TimePoint) (diff : I)
    (clicked : Bool) (style : JudgeStyle) :
    Result (Lifecycle.TapFamilyNote × Option JudgeEvent) := do
  match note with
  | .tap note =>
      let (next, event) ← tapStep inst ops decode note now diff clicked style
      ok (.tap next, event)
  | .slideHead note =>
      let (next, event) ← slideHeadStep inst ops decode note now diff clicked style
      ok (.slideHead next, event)

theorem tap_family_step_equiv (note : Lifecycle.TapFamilyNote)
    (now : TimePoint) (diff : I) (clicked : Bool) (style : JudgeStyle) :
    tapFamilyStep inst ops decode note now diff clicked style =
      ok (Lifecycle.tapFamilyStep note now (Duration.fromMicros (decode diff)) clicked style) := by
  cases note with
  | tap note =>
      rcases hs : Lifecycle.tapStep note now (Duration.fromMicros (decode diff))
        clicked style with ⟨next, event⟩
      simp [tapFamilyStep, tap_step_equiv inst ops decode h, Lifecycle.tapFamilyStep, hs]
  | slideHead note =>
      rcases hs : Lifecycle.slideHeadStep note now (Duration.fromMicros (decode diff))
        clicked style with ⟨next, event⟩
      simp [tapFamilyStep, slide_head_step_equiv inst ops decode h, Lifecycle.tapFamilyStep, hs]

def touchFinishAction (note : Lifecycle.TouchNote) (state : lifecycle.TouchState)
    (action : lifecycle.TouchAction) (diff : I)
    (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    Result (Lifecycle.TouchNote × Option JudgeEvent) := do
  match action with
  | .NoEvent => ok ({ note with state := TouchLifecycleProof.toState state }, none)
  | .Miss => ok ({ note with state := TouchLifecycleProof.toState state },
      some (touchTooLateMissEvent note))
  | .Shared => ok ({ note with state := TouchLifecycleProof.toState state },
      shared.map (fun (grade, sharedDiff) => touchJudgeEvent note grade sharedDiff))
  | .Judge =>
      let raw ← judgment_adapter.judge_touch inst diff note.params.isEX ops
      let (state, grade) ← lifecycle.finish_touch_judgment raw (ofLnmJudgeStyle style)
      ok ({ note with state := TouchLifecycleProof.toState state },
        grade.map (fun grade => touchJudgeEvent note (toLnmJudgeGrade grade)
          (Duration.fromMicros (decode diff))))

theorem touch_finish_action_equiv (note : Lifecycle.TouchNote)
    (state : lifecycle.TouchState) (action : lifecycle.TouchAction) (diff : I)
    (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    touchFinishAction inst ops decode note state action diff shared style =
      TouchLifecycleProof.finishAction note state action
        (Duration.fromMicros (decode diff)) shared style := by
  cases action <;> simp only [touchFinishAction, TouchLifecycleProof.finishAction,
    JudgmentAdapterProof.touch_equiv inst ops decode h, bind_tc_ok]

def touchStep (note : Lifecycle.TouchNote) (now : TimePoint) (diff : I)
    (clicked : Bool) (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    Result (Lifecycle.TouchNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let (state, action) ← lifecycle.touch_transition (TouchLifecycleProof.ofState note.state)
    (now > timing + Constants.touchGoodMs)
    (now ≥ timing - Constants.JUDGABLE_RANGE_SEC) clicked shared.isSome
  touchFinishAction inst ops decode note state action diff shared style

theorem touch_step_equiv (note : Lifecycle.TouchNote) (now : TimePoint) (diff : I)
    (clicked : Bool) (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    touchStep inst ops decode note now diff clicked shared style =
      ok (Lifecycle.touchStep note now (Duration.fromMicros (decode diff)) clicked shared style) := by
  simpa only [touchStep, TouchLifecycleProof.touchStep,
    touch_finish_action_equiv inst ops decode h] using
    TouchLifecycleProof.touch_step_equiv note now (Duration.fromMicros (decode diff))
      clicked shared style

structure TapFrame (I : Type) where
  now : TimePoint
  diff : I
  clicked : Bool
  style : JudgeStyle

def tapFrame (frame : TapFrame I) : TapLifecycleProof.Frame :=
  ⟨frame.now, Duration.fromMicros (decode frame.diff), frame.clicked, frame.style⟩

def tapTrace : Lifecycle.TapFamilyNote → List (TapFrame I) →
    Result (List (Lifecycle.TapFamilyNote × Option JudgeEvent))
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← tapFamilyStep inst ops decode note frame.now frame.diff frame.clicked frame.style
      let rest ← tapTrace next.1 frames
      ok (next :: rest)

/-- Every intermediate note and event, for any finite trace and arbitrary exact
diffs. The property imposes no monotonicity or reachability premise. -/
theorem tap_trace_equiv (note : Lifecycle.TapFamilyNote) (frames : List (TapFrame I)) :
    tapTrace inst ops decode note frames =
      ok (TapLifecycleProof.referenceTrace note (frames.map (tapFrame decode))) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih =>
      simp [tapTrace, tap_family_step_equiv inst ops decode h,
        TapLifecycleProof.referenceTrace, tapFrame, ih]

structure TouchFrame (I : Type) where
  now : TimePoint
  diff : I
  clicked : Bool
  shared : Option (JudgeGrade × Duration)
  style : JudgeStyle

def touchFrame (frame : TouchFrame I) : TouchLifecycleProof.Frame :=
  ⟨frame.now, Duration.fromMicros (decode frame.diff), frame.clicked, frame.shared, frame.style⟩

def touchTrace : Lifecycle.TouchNote → List (TouchFrame I) →
    Result (List (Lifecycle.TouchNote × Option JudgeEvent))
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← touchStep inst ops decode note frame.now frame.diff
        frame.clicked frame.shared frame.style
      let rest ← touchTrace next.1 frames
      ok (next :: rest)

theorem touch_trace_equiv (note : Lifecycle.TouchNote) (frames : List (TouchFrame I)) :
    touchTrace inst ops decode note frames =
      ok (TouchLifecycleProof.referenceTrace note (frames.map (touchFrame decode))) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih =>
      simp [touchTrace, touch_step_equiv inst ops decode h,
        TouchLifecycleProof.referenceTrace, touchFrame, ih]

end Verification.LifecycleJudgmentsProof
