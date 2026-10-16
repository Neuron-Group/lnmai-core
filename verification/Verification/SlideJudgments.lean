import Verification.JudgmentAdapter
import Verification.SlideLifecycle

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.SlideJudgmentsProof
open Bridge LnmaiCore
open private slideTooLateTiming slideCurrentJudgeDiff slideAdjustedJudgedWaitTime
  slideJudgeEvent slideTooLateJudgeDiff buildSlideStaticSemanticBase
  buildSlideSensorSemanticBase slideTooLateStepSemantic slideActiveStepSemantic
  from LnmaiCore.Lifecycle

/-!
Compose the extracted judgment_adapter calls with SlideLifecycle's transition
and output kernels. Judgment agreement follows from IntegerAgrees and the
integer encoding round trip; no whole judgment or lifecycle equation is assumed.

Runtime correspondence: lifecycle.rs:1325-1520 judges the maximum PRE-SENSOR
queue length on timeout. Completion passes the panel/anchor-adjusted diff, the
stored wait, and isEX to classic/modern judgment; only modern completion applies
the stored-grade conversion. Both timeout and delayed Finish apply effective_grade.
Completion still has priority over timeout, which has priority over sampling.

The current runtime timeout guard compares current_time with slide_too_late_timing,
rather than calling judge.rs's is_too_late_slide facade. timeout_reference below
proves the algebraic bridge to that extracted judgment call, using elapsed time
since arrival and judgeOffset, NOT the panel-adjusted completion diff. This file
does not assert that the Rust facade has been extracted or changed to this call.

Remaining boundaries: the concrete Integer/BigInt instance and encoding, exact
time arithmetic and cloning, queue metrics/sensor updates, note/semantic records,
and event/audio/render construction still use the existing reference adapters.
Unbounded counts and durations are not narrowed by the encoding assumption.
The finite trace is a single slide's complete outputs, not a GameState trace.
-/

private theorem duration_roundtrip (d : Duration) :
    Duration.fromMicros d.toMicros = d := rfl

/-- Translate the runtime's absolute deadline to the exact adapter's relative
comparison. The offset minimum stays intact; no sign or size bound is needed. -/
theorem timeout_reference (note : Lifecycle.SlideNote) (now : TimePoint) :
    Judge.isTooLateSlide (now - (note.startTiming + note.length)) note.params.judgeOffset =
      decide (now > slideTooLateTiming note) := by
  let arrival := (note.startTiming + note.length).toMicros
  let good := Constants.SLIDE_JUDGE_GOOD_AREA_MSEC.toMicros
  let offset := (min note.params.judgeOffset Duration.zero).toMicros
  change decide (good + offset < now.toMicros - arrival) =
    decide (arrival + good + offset < now.toMicros)
  have hcmp : good + offset < now.toMicros - arrival ↔
      arrival + good + offset < now.toMicros := by omega
  simp only [hcmp]

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → ℤ)
  (h : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)
include h decode_encode

/-- Encode the exact completion diff before the extracted adapter selects its
bounded representative; no input range restriction is imposed here. -/
theorem classic_grade_equiv (diff : Duration) :
    judgment_adapter.judge_slide_classic inst (encode diff.toMicros) ops =
      ok (ofLnmJudgeGrade (Judge.judgeSlideClassic diff)) := by
  simpa only [decode_encode, duration_roundtrip] using
    JudgmentAdapterProof.slide_classic_equiv inst ops decode h (encode diff.toMicros)

/-- The stored wait is encoded independently of the diff, including restored
negative waits. EX is passed through to the extracted modern judgment. -/
theorem modern_grade_equiv (diff wait : Duration) (isEX : Bool) :
    judgment_adapter.judge_slide_modern inst (encode diff.toMicros)
      (encode wait.toMicros) isEX ops =
      ok (ofLnmJudgeGrade (Judge.judgeSlideModern diff wait isEX)) := by
  simpa only [decode_encode, duration_roundtrip] using
    JudgmentAdapterProof.slide_modern_equiv inst ops decode h
      (encode diff.toMicros) (encode wait.toMicros) isEX

/-- The raw completion grade uses the same exact inputs as judge.rs. Classic
does not consume wait/isEX; modern passes both before its later style conversion. -/
def completionGrade (note : Lifecycle.SlideNote) (diff wait : Duration) :
    Result types.JudgeGrade :=
  if note.isClassic then
    judgment_adapter.judge_slide_classic inst (encode diff.toMicros) ops
  else
    judgment_adapter.judge_slide_modern inst (encode diff.toMicros)
      (encode wait.toMicros) note.params.isEX ops

theorem completion_grade_equiv (note : Lifecycle.SlideNote) (diff wait : Duration) :
    completionGrade inst ops encode note diff wait =
      ok (ofLnmJudgeGrade (if note.isClassic then Judge.judgeSlideClassic diff
        else Judge.judgeSlideModern diff wait note.params.isEX)) := by
  cases hc : note.isClassic <;>
    simp [completionGrade, hc,
      classic_grade_equiv inst ops decode h encode decode_encode,
      modern_grade_equiv inst ops decode h encode decode_encode]

/-- The Nat-to-integer boundary preserves arbitrarily large queue counts.
The extracted kernel proves the equality-with-one classification itself. -/
theorem too_late_grade_equiv (remaining : Nat) :
    judgment_adapter.judge_slide_too_late inst (encode (remaining : ℤ)) ops =
      ok (ofLnmJudgeGrade (Judge.judgeSlideTooLate remaining)) := by
  exact JudgmentAdapterProof.slide_too_late_equiv inst ops decode h
    (encode (remaining : ℤ)) remaining (decode_encode (remaining : ℤ))

def isTooLate (diff offset : Duration) : Result Bool :=
  judgment_adapter.is_too_late_slide inst (encode diff.toMicros)
    (encode offset.toMicros) ops

theorem is_too_late_equiv (diff offset : Duration) :
    isTooLate inst ops encode diff offset = ok (Judge.isTooLateSlide diff offset) := by
  simpa only [isTooLate, decode_encode, duration_roundtrip] using
    JudgmentAdapterProof.is_too_late_equiv inst ops decode h
      (encode diff.toMicros) (encode offset.toMicros)

def timeout (note : Lifecycle.SlideNote) (now : TimePoint) : Result Bool :=
  isTooLate inst ops encode (now - (note.startTiming + note.length))
    note.params.judgeOffset

theorem timeout_equiv (note : Lifecycle.SlideNote) (now : TimePoint) :
    timeout inst ops encode note now = ok (decide (now > slideTooLateTiming note)) := by
  simpa only [timeout, timeout_reference] using
    is_too_late_equiv inst ops decode h encode decode_encode
      (now - (note.startTiming + note.length)) note.params.judgeOffset

/-- Snapshot the original queues, judge their maximum remaining length, apply
effective_grade, and end immediately. Nonfinal connected parts suppress events;
the timeout event retains the runtime's -1000 microsecond sentinel. -/
def tooLateStep (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext)
    (checkable : Bool) : Result Lifecycle.SlideStepSemantic := do
  let base := buildSlideStaticSemanticBase note checkable
  let raw ← judgment_adapter.judge_slide_too_late inst
    (encode (Lifecycle.slideQueueRemaining note.judgeQueues : ℤ)) ops
  let grade ← slide_lifecycle.effective_grade (ofLnmJudgeStyle ctx.style)
    ctx.subdivideSlideJudgeGrade raw
  ok { base with
    note := { base.note with state := .Ended }
    event := if note.isConnSlide && !note.isGroupPartEnd then none
      else some (slideJudgeEvent note (toLnmJudgeGrade grade) slideTooLateJudgeDiff)
    hideSlide := true }

/-- This equality is derived, not supplied as a callback assumption. -/
theorem too_late_step_refines (note : Lifecycle.SlideNote)
    (ctx : Lifecycle.SlideStepContext) (checkable : Bool) :
    tooLateStep inst ops encode note ctx checkable =
      SlideLifecycleProof.tooLateStep note ctx checkable := by
  simp only [tooLateStep, SlideLifecycleProof.tooLateStep,
    too_late_grade_equiv inst ops decode h encode decode_encode, bind_tc_ok]

theorem too_late_step_equiv (note : Lifecycle.SlideNote)
    (ctx : Lifecycle.SlideStepContext) (checkable : Bool) :
    tooLateStep inst ops encode note ctx checkable =
      ok (slideTooLateStepSemantic note ctx checkable) := by
  rw [too_late_step_refines inst ops decode h encode decode_encode]
  exact SlideLifecycleProof.too_late_step_equiv note ctx checkable

/-- active_action still sees the queues before sensor sampling. Raw judgment
is executed only on Complete, and timeout judgment only on Timeout. Original
exact diff/wait values, not clamped representatives, are stored in the note. -/
def activeStep (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext)
    (judgable : Bool) (wait : Duration) : Result Lifecycle.SlideStepSemantic := do
  let active := { note with state := Lifecycle.SlideState.Active wait, isCheckable := true }
  let base := buildSlideStaticSemanticBase active true
  let tooLate ← timeout inst ops encode active ctx.currentTime
  let action ← slide_lifecycle.active_action judgable
    (Lifecycle.slideQueuesCleared active.judgeQueues) tooLate
  match action with
  | .Complete =>
      let diff := slideCurrentJudgeDiff active ctx.currentTime ctx.touchPanelOffset
      let raw ← completionGrade inst ops encode active diff wait
      let stored ← if active.isClassic then ok raw
        else convert.convert_grade (ofLnmJudgeStyle ctx.style) raw
      let newWait := slideAdjustedJudgedWaitTime active ctx.currentTime wait diff
      ok { base with
        note := { base.note with state := .Judged (toLnmJudgeGrade stored) newWait diff }
        shouldPlayTrackOns := active.isGroupPartHead || !active.isConnSlide
        emitProgressRender := true }
  | .Timeout => tooLateStep inst ops encode active ctx true
  | .SampleSensors =>
      let sampled := buildSlideSensorSemanticBase active ctx true
      ok { sampled with
        shouldPlayTrackOns := active.isGroupPartHead || !active.isConnSlide
        emitProgressRender := sampled.progressChanged }

theorem active_step_refines (note : Lifecycle.SlideNote)
    (ctx : Lifecycle.SlideStepContext) (judgable : Bool) (wait : Duration) :
    activeStep inst ops encode note ctx judgable wait =
      SlideLifecycleProof.activeStep note ctx judgable wait := by
  simp only [activeStep, SlideLifecycleProof.activeStep,
    timeout_equiv inst ops decode h encode decode_encode,
    completion_grade_equiv inst ops decode h encode decode_encode,
    too_late_step_refines inst ops decode h encode decode_encode, bind_tc_ok]

/-- Includes classic/modern, EX, restored negative waits, connected parts and
WiFi queues without adding reachability or timestamp monotonicity premises. -/
theorem active_step_equiv (note : Lifecycle.SlideNote)
    (ctx : Lifecycle.SlideStepContext) (judgable : Bool) (wait : Duration) :
    activeStep inst ops encode note ctx judgable wait =
      ok (slideActiveStepSemantic note ctx judgable wait) := by
  rw [active_step_refines inst ops decode h encode decode_encode]
  exact SlideLifecycleProof.active_step_equiv note ctx judgable wait

/-- All state branches retain SlideLifecycle's extracted transition and grade
conversion kernels. Only the judgment/timeout inputs are replaced by the
compositions above; queue and event adapters remain at the stated boundary. -/
def stepSemantic (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    Result Lifecycle.SlideStepSemantic := do
  let checkable ← SlideLifecycleProof.shouldBeCheckable note ctx.currentTime
  let judgable := note.isGroupPartEnd || !note.isConnSlide
  let tooLate ← timeout inst ops encode note ctx.currentTime
  let action ← slide_lifecycle.transition (SlideLifecycleProof.ofState note.state)
    (SlideLifecycleProof.initialWait note) checkable judgable tooLate
    (SlideLifecycleProof.waitExpired note.state)
  match action with
  | .Dormant => ok (Lifecycle.buildSlideDormantSemanticBase note)
  | .Activate wait => activeStep inst ops encode note ctx judgable wait
  | .Timeout => tooLateStep inst ops encode note ctx false
  | .Finish grade diff =>
      let base := buildSlideStaticSemanticBase note checkable
      let finalGrade ← slide_lifecycle.effective_grade (ofLnmJudgeStyle ctx.style)
        ctx.subdivideSlideJudgeGrade grade
      ok { base with
        note := { base.note with state := .Ended }
        event := if note.isConnSlide && !note.isGroupPartEnd then none
          else some (slideJudgeEvent note (toLnmJudgeGrade finalGrade) diff)
        hideSlide := true }
  | .Wait grade wait diff =>
      let base := buildSlideStaticSemanticBase note checkable
      ok { base with
        note := { base.note with state := .Judged (toLnmJudgeGrade grade) (wait - ctx.delta) diff }
        shouldPlayTrackOns := note.isGroupPartHead || !note.isConnSlide
        emitProgressRender := base.progressChanged }
  | .Ended =>
      let base := buildSlideStaticSemanticBase note checkable
      ok { base with note := { base.note with state := .Ended } }

theorem step_semantic_refines (note : Lifecycle.SlideNote)
    (ctx : Lifecycle.SlideStepContext) :
    stepSemantic inst ops encode note ctx = SlideLifecycleProof.stepSemantic note ctx := by
  simp only [stepSemantic, SlideLifecycleProof.stepSemantic,
    timeout_equiv inst ops decode h encode decode_encode,
    active_step_refines inst ops decode h encode decode_encode,
    too_late_step_refines inst ops decode h encode decode_encode, bind_tc_ok]

theorem step_semantic_equiv (note : Lifecycle.SlideNote)
    (ctx : Lifecycle.SlideStepContext) :
    stepSemantic inst ops encode note ctx = ok (Lifecycle.slideStepSemantic note ctx) := by
  rw [step_semantic_refines inst ops decode h encode decode_encode]
  exact SlideLifecycleProof.step_semantic_equiv note ctx

/-- Reuse the existing output_flags composition, including delayed events,
sound-played bookkeeping, audio suppression after Ended and hide rendering. -/
def step (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    Result SlideLifecycleProof.StepOutput := do
  let semantic ← stepSemantic inst ops encode note ctx
  SlideLifecycleProof.encodeOutput semantic ctx.currentTime

theorem step_refines (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    step inst ops encode note ctx = SlideLifecycleProof.step note ctx := by
  simp only [step, SlideLifecycleProof.step,
    step_semantic_refines inst ops decode h encode decode_encode]

theorem step_equiv (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    step inst ops encode note ctx =
      ok (Lifecycle.slideStep note ctx.currentTime ctx.sensorHeld ctx.touchPanelOffset
        ctx.delta ctx.style ctx.subdivideSlideJudgeGrade) := by
  rw [step_refines inst ops decode h encode decode_encode]
  exact SlideLifecycleProof.step_equiv note ctx

/-- Thread the complete resulting note, including queues, stored wait/diff and
sound flags, into each subsequent frame. Preserve every output in order. -/
def trace : Lifecycle.SlideNote → List Lifecycle.SlideStepContext →
    Result (List SlideLifecycleProof.StepOutput)
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← step inst ops encode note frame
      let rest ← trace next.1 frames
      ok (next :: rest)

theorem trace_refines (note : Lifecycle.SlideNote)
    (frames : List Lifecycle.SlideStepContext) :
    trace inst ops encode note frames = SlideLifecycleProof.trace note frames := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih =>
      simp only [trace, SlideLifecycleProof.trace,
        step_refines inst ops decode h encode decode_encode, ih]

/-- Finite trace composition for arbitrary inputs and restored states. No
entire step/trace correctness premise, time bound, or queue-count bound is used. -/
theorem trace_equiv (note : Lifecycle.SlideNote)
    (frames : List Lifecycle.SlideStepContext) :
    trace inst ops encode note frames =
      ok (SlideLifecycleProof.referenceTrace note frames) := by
  rw [trace_refines inst ops decode h encode decode_encode]
  exact SlideLifecycleProof.trace_equiv note frames

end Verification.SlideJudgmentsProof
