import Verification.JudgmentAdapter
import Verification.HoldLifecycle

/-!
Compose the extracted exact-integer judgments with the Hold/TouchHold lifecycle.
Both head judgments, both tail formulas and grade conversion execute the actual
extracted kernels. The modern tail also uses the atomic addition for the two
ignore windows. No complete head, tail or step correctness law is assumed.

Boundary: notes, frames, events, time predicates and release-counter updates are
interpreted using HoldLifecycle's reference data. `encode` supplies exact integer
representatives; `IntegerAgrees` supplies their atomic operation laws. This file
does not establish concrete BigInt correctness, allocation/clone correctness,
the Rust note/time representation, or extraction of the runtime facade itself.
Shared grades/diffs are supplied as typed frame inputs; touch-group selection
and scheduler ordering remain outside this lifecycle composition.
The wrappers below follow that facade's call order, including tail-event priority
when a head and tail resolve in the same frame.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 2000000

namespace Verification.HoldJudgmentsProof
open Bridge LnmaiCore
open private holdHeadJudged holdHeadMiss holdHeadShared holdHeadJudgeEvent
  judgeHoldHeadTapNow? judgeHoldHeadTouchNow? holdHeadReleaseTransition
  holdPressedTransition holdKeepPressed holdReleaseTransition
  holdReleasedStillOff holdReleasedRecovered from LnmaiCore.Lifecycle

private theorem duration_roundtrip (value : Duration) :
    Duration.fromMicros value.toMicros = value := by
  apply Duration.toMicros_injective
  rfl

private theorem point_roundtrip (value : TimePoint) :
    TimePoint.fromMicros value.toMicros = value := by
  apply TimePoint.toMicros_injective
  rfl

private theorem head_release_no_event (note : Lifecycle.HoldNote) (delta : Duration) :
    (holdHeadReleaseTransition note delta).2 = none := by
  unfold holdHeadReleaseTransition
  split_ifs <;> rfl

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → ℤ)
  (h : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (encode : ℤ → I) (hencode : ∀ value, decode (encode value) = value)
include h hencode

/-- Head action interpretation, with the tap/touch judgment and style conversion
performed by the extracted kernels. Shared grades have already been converted;
a miss stores Miss in the note while converting only its public event. -/
def applyHead (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame)
    (action : lifecycle.HoldHeadAction) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  match action with
  | .Unchanged => ok (note, none)
  | .MakeJudgeable => ok ({ note with state := .HeadJudgeable }, none)
  | .Miss =>
      let diff := if note.isTouchHold then Constants.touchGoodMs else Constants.tapGoodMs
      let grade ← convert.convert_grade (ofLnmJudgeStyle f.style) .Miss
      ok (holdHeadMiss note diff,
        some (holdHeadJudgeEvent note (toLnmJudgeGrade grade) diff))
  | .Shared => match f.shared with
      | some (grade, diff) => ok (holdHeadShared note grade diff, none)
      | none => ok (note, none)
  | .JudgeTap =>
      let raw ← judgment_adapter.judge_tap inst (encode f.diff.toMicros) note.params.isEX ops
      let grade ← convert.convert_grade (ofLnmJudgeStyle f.style) raw
      ok (holdHeadJudged note (toLnmJudgeGrade grade) f.diff false,
        some (holdHeadJudgeEvent note (toLnmJudgeGrade grade) f.diff))
  | .JudgeTouch =>
      let raw ← judgment_adapter.judge_touch inst (encode f.diff.toMicros) note.params.isEX ops
      match raw with
      | none => ok (note, none)
      | some raw =>
          let grade ← convert.convert_grade (ofLnmJudgeStyle f.style) raw
          ok (holdHeadJudged note (toLnmJudgeGrade grade) f.diff true,
            some (holdHeadJudgeEvent note (toLnmJudgeGrade grade) f.diff))

theorem apply_head_equiv (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame)
    (action : lifecycle.HoldHeadAction) :
    applyHead inst ops encode note f action = ok (HoldLifecycleProof.applyHead note f action) := by
  cases action with
  | Unchanged | MakeJudgeable => rfl
  | Miss =>
      simp [applyHead, HoldLifecycleProof.applyHead, ConvertProof.convert_grade_equiv] <;> rfl
  | Shared =>
      cases hshared : f.shared with
      | none => simp [applyHead, HoldLifecycleProof.applyHead, hshared]
      | some shared =>
          rcases shared with ⟨grade, diff⟩
          simp [applyHead, HoldLifecycleProof.applyHead, hshared]
  | JudgeTap =>
      simp [applyHead, HoldLifecycleProof.applyHead, judgeHoldHeadTapNow?,
        JudgmentAdapterProof.tap_equiv inst ops decode h, hencode, duration_roundtrip,
        ConvertProof.convert_grade_equiv]
  | JudgeTouch =>
      cases hraw : Judge.judgeTouch f.diff note.params.isEX <;>
        simp [applyHead, HoldLifecycleProof.applyHead, judgeHoldHeadTouchNow?,
          JudgmentAdapterProof.touch_equiv inst ops decode h, hencode, duration_roundtrip,
          hraw, ConvertProof.convert_grade_equiv]

/-- Exact tail inputs in runtime order. Only the modern branch combines ignore
windows; only the classic branch uses the supplied release timestamp. -/
def tailGrade (note : Lifecycle.HoldNote) (timing : TimePoint)
    (f : HoldLifecycleProof.Frame) (grade : JudgeGrade) (releaseTiming : TimePoint) :
    Result types.JudgeGrade := do
  if note.isClassic then
    judgment_adapter.judge_hold_classic_end inst (ofLnmJudgeGrade grade)
      (encode timing.toMicros) (encode note.length.toMicros) (encode releaseTiming.toMicros) ops
  else
    let ignore ← inst.add ops (encode f.headIgnore.toMicros) (encode f.tailIgnore.toMicros)
    judgment_adapter.judge_hold_end inst (ofLnmJudgeGrade grade)
      (encode note.headDiff.toMicros) (encode note.length.toMicros) ignore
      (encode note.playerReleaseTime.toMicros) ops

theorem tail_grade_equiv (note : Lifecycle.HoldNote) (timing : TimePoint)
    (f : HoldLifecycleProof.Frame) (grade : JudgeGrade) (releaseTiming : TimePoint) :
    tailGrade inst ops encode note timing f grade releaseTiming =
      ok (ofLnmJudgeGrade (if note.isClassic then
        Judge.judgeHoldClassicEnd grade timing note.length releaseTiming
      else Judge.judgeHoldEnd grade note.headDiff note.length
        (f.headIgnore + f.tailIgnore) note.playerReleaseTime)) := by
  cases hc : note.isClassic with
  | false =>
      obtain ⟨ignore, hi, hiv⟩ := spec_imp_exists
        (h.add (encode f.headIgnore.toMicros) (encode f.tailIgnore.toMicros))
      have hiVal : decode ignore = f.headIgnore.toMicros + f.tailIgnore.toMicros := by
        simpa only [hencode] using hiv
      have hiDuration : Duration.fromMicros (decode ignore) = f.headIgnore + f.tailIgnore := by
        rw [hiVal]
        rfl
      simp [tailGrade, hc, hi, JudgmentAdapterProof.modern_hold_equiv inst ops decode h,
        hencode, duration_roundtrip, hiDuration]
  | true =>
      simp [tailGrade, hc, JudgmentAdapterProof.classic_hold_equiv inst ops decode h,
        hencode, duration_roundtrip, point_roundtrip]

/-- Tail construction preserves the head's exact diff (or the 150ms missing-head
sentinel) and clears the touch-group trigger. The tail event uses the runtime's
default isEX field, unlike the head event. -/
def endHold (note : Lifecycle.HoldNote) (timing : TimePoint)
    (f : HoldLifecycleProof.Frame) (grade : JudgeGrade) (releaseTiming : TimePoint) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let raw ← tailGrade inst ops encode note timing f grade releaseTiming
  let converted ← convert.convert_grade (ofLnmJudgeStyle f.style) raw
  let finalGrade := toLnmJudgeGrade converted
  let eventDiff := if note.headDiff == Duration.zero && grade == .Miss then
      Time.fromMillis 150 else note.headDiff
  ok ({ note with state := .Ended finalGrade, touchHoldGroupTriggered := false },
    some (HoldLifecycleProof.holdTailEvent note finalGrade eventDiff))

theorem end_hold_equiv (note : Lifecycle.HoldNote) (timing : TimePoint)
    (f : HoldLifecycleProof.Frame) (grade : JudgeGrade) (releaseTiming : TimePoint) :
    endHold inst ops encode note timing f grade releaseTiming =
      ok (HoldLifecycleProof.endHold note timing f grade releaseTiming) := by
  cases hc : note.isClassic <;>
    simp [endHold, HoldLifecycleProof.endHold,
      tail_grade_equiv inst ops decode h encode hencode, hc, ConvertProof.convert_grade_equiv]

def headStep (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let timing := note.params.effectiveTiming
  let lateDiff := if note.isTouchHold then Constants.touchGoodMs else Constants.tapGoodMs
  let action ← lifecycle.hold_head_action (HoldLifecycleProof.ofState note.state) note.isTouchHold
    (f.now > timing + lateDiff) (f.now ≥ timing - Constants.JUDGABLE_RANGE_SEC)
    f.clicked f.shared.isSome
  applyHead inst ops encode note f action

theorem head_step_refines (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame) :
    headStep inst ops encode note f = HoldLifecycleProof.headStep note f := by
  simp only [headStep, HoldLifecycleProof.headStep,
    apply_head_equiv inst ops decode h encode hencode]

/-- Body actions share their counter/state interpretation with HoldLifecycle.
Every ending action calls the extracted judgments above, including the sensor
release-offset rule on EndRelease. -/
def applyBody (note : Lifecycle.HoldNote) (timing : TimePoint)
    (f : HoldLifecycleProof.Frame) (action : lifecycle.HoldBodyAction) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) :=
  match action with
  | .Unchanged => ok (note, none)
  | .EndNow grade => endHold inst ops encode note timing f (toLnmJudgeGrade grade) f.now
  | .EndRelease grade =>
      endHold inst ops encode note timing f (toLnmJudgeGrade grade)
        (f.now - HoldLifecycleProof.releaseOffset f)
  | .StartPressed => ok (holdPressedTransition note, none)
  | .StartClassicPressed =>
      ok ({ note with state := .BodyHeld, touchHoldGroupTriggered := note.isTouchHold }, none)
  | .KeepClassicPressed => ok ({ note with touchHoldGroupTriggered := note.isTouchHold }, none)
  | .KeepPressed => ok (holdKeepPressed note, none)
  | .ReleaseHead grade =>
      ok (holdHeadReleaseTransition { note with headGrade := toLnmJudgeGrade grade } f.delta)
  | .Release => ok (holdReleaseTransition note f.delta, none)
  | .Recover => ok (holdReleasedRecovered note, none)
  | .ReleasedOff => ok (holdReleasedStillOff note f.delta, none)

theorem apply_body_equiv (note : Lifecycle.HoldNote) (timing : TimePoint)
    (f : HoldLifecycleProof.Frame) (action : lifecycle.HoldBodyAction) :
    applyBody inst ops encode note timing f action =
      ok (HoldLifecycleProof.applyBody note timing f action) := by
  cases action <;> simp only [applyBody, HoldLifecycleProof.applyBody,
    end_hold_equiv inst ops decode h encode hencode]

/-- Matches runtime tail_event.or(head_event), after dispatching on the updated
head state but retaining the original note's body timing and length. -/
def afterHead (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame)
    (result : Lifecycle.HoldNote × Option JudgeEvent) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let action ← lifecycle.hold_after_head_action (HoldLifecycleProof.ofState result.1.state)
    note.isTouchHold f.clicked (f.now ≥ HoldLifecycleProof.bodyTiming note + f.headIgnore)
    (f.now - note.params.effectiveTiming ≥ note.length) f.pressed
  let applied ← applyBody inst ops encode result.1 note.params.effectiveTiming f action
  ok (applied.1, match applied.2 with
    | some event => some event
    | none => result.2)

theorem after_head_refines (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame)
    (result : Lifecycle.HoldNote × Option JudgeEvent) :
    afterHead inst ops encode note f result = HoldLifecycleProof.afterHead note f result := by
  rcases result with ⟨next, event⟩
  cases hs : next.state <;>
    simp only [afterHead, HoldLifecycleProof.afterHead,
      apply_body_equiv inst ops decode h encode hencode, lifecycle.hold_after_head_action,
      hs, HoldLifecycleProof.ofState, bind_tc_ok] <;>
    (try split_ifs) <;>
    simp_all [HoldLifecycleProof.applyBody, HoldLifecycleProof.endHold,
      head_release_no_event, bind_tc_ok]

def bodyStep (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) := do
  let action ← lifecycle.hold_body_action (HoldLifecycleProof.ofState note.state)
    (ofLnmJudgeGrade note.headGrade) (HoldLifecycleProof.bodyFacts note f)
  applyBody inst ops encode note note.params.effectiveTiming f action

theorem body_step_refines (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame) :
    bodyStep inst ops encode note f = HoldLifecycleProof.bodyStep note f := by
  simp only [bodyStep, HoldLifecycleProof.bodyStep,
    apply_body_equiv inst ops decode h encode hencode]

/-- One complete Hold or TouchHold frame. The two head states run head then
body; the other four states go directly through the extracted body dispatcher. -/
def holdStep (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame) :
    Result (Lifecycle.HoldNote × Option JudgeEvent) :=
  match note.state with
  | .HeadWaiting | .HeadJudgeable => do
      let result ← headStep inst ops encode note f
      afterHead inst ops encode note f result
  | _ => bodyStep inst ops encode note f

/-- All states, styles, hold kinds, input combinations and exact times. There is
no reachability or monotonic-time precondition and no complete judgment premise. -/
theorem hold_step_equiv (note : Lifecycle.HoldNote) (f : HoldLifecycleProof.Frame) :
    holdStep inst ops encode note f = ok (HoldLifecycleProof.referenceStep note f) := by
  calc
    holdStep inst ops encode note f = HoldLifecycleProof.holdStep note f := by
      cases hs : note.state <;>
        simp only [holdStep, HoldLifecycleProof.holdStep, hs,
          head_step_refines inst ops decode h encode hencode,
          after_head_refines inst ops decode h encode hencode,
          body_step_refines inst ops decode h encode hencode]
    _ = ok (HoldLifecycleProof.referenceStep note f) :=
      HoldLifecycleProof.hold_step_equiv note f

def trace : Lifecycle.HoldNote → List HoldLifecycleProof.Frame →
    Result (List (Lifecycle.HoldNote × Option JudgeEvent))
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← holdStep inst ops encode note frame
      let rest ← trace next.1 frames
      ok (next :: rest)

/-- Every intermediate state and optional event for any finite input sequence,
including repeated ended states and a head/tail resolved within one frame. -/
theorem trace_equiv (note : Lifecycle.HoldNote) (frames : List HoldLifecycleProof.Frame) :
    trace inst ops encode note frames = ok (HoldLifecycleProof.referenceTrace note frames) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih =>
      simp [trace, HoldLifecycleProof.referenceTrace,
        hold_step_equiv inst ops decode h encode hencode, ih]

end Verification.HoldJudgmentsProof
