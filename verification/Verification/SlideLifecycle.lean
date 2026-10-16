import Verification.Convert
import LnmaiCore.Lifecycle
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.SlideLifecycleProof
open Bridge LnmaiCore
open private slideEffectiveJudgeGrade slideInitialWaitTime slideTooLateTiming
  slideCurrentJudgeDiff slideAdjustedJudgedWaitTime slideJudgeEvent
  slideTooLateJudgeDiff buildSlideStaticSemanticBase buildSlideSensorSemanticBase
  slideTooLateStepSemantic slideActiveStepSemantic slideSemanticAudioCmds
  slideSemanticRenderCmds from LnmaiCore.Lifecycle

set_option pp.maxSteps 100000

/-- The shared state stores the runtime duration without fixed-width conversion. -/
def ofState : Lifecycle.SlideState → slide_lifecycle.SlideState Duration
  | .Waiting => .Waiting
  | .Active wait => .Active wait
  | .Judged grade wait diff => .Judged (ofLnmJudgeGrade grade) wait diff
  | .Ended => .Ended

def toState : slide_lifecycle.SlideState Duration → Lifecycle.SlideState
  | .Waiting => .Waiting
  | .Active wait => .Active wait
  | .Judged grade wait diff => .Judged (toLnmJudgeGrade grade) wait diff
  | .Ended => .Ended

@[simp] theorem toState_ofState (state : Lifecycle.SlideState) :
    toState (ofState state) = state := by
  cases state <;> simp [toState, ofState]

@[simp] theorem ofState_toState (state : slide_lifecycle.SlideState Duration) :
    ofState (toState state) = state := by
  cases state <;> simp [toState, ofState]

theorem default_equiv :
    slide_lifecycle.SlideState.Insts.CoreDefaultDefault.default Duration =
      ok (ofState (default : Lifecycle.SlideState)) := rfl

/-- Runtime cloning is exact provided the opaque duration clone is exact. -/
theorem clone_equiv {D : Type} (cl : core.clone.Clone D)
    (hclone : ∀ d, cl.clone d = ok d) (state : slide_lifecycle.SlideState D) :
    slide_lifecycle.SlideState.Insts.CoreCloneClone.clone cl state = ok state := by
  cases state <;> simp [slide_lifecycle.SlideState.Insts.CoreCloneClone.clone,
    types.JudgeGrade.Insts.CoreCloneClone.clone, hclone, bind_tc_ok]

def shouldBeCheckable (note : Lifecycle.SlideNote) (now : TimePoint) : Result Bool :=
  slide_lifecycle.should_be_checkable note.isCheckable note.isConnSlide
    note.isGroupPartHead note.parentFinished note.parentPendingFinish
    (now - note.headTiming ≥ Duration.fromMicros (-50000))

theorem should_be_checkable_equiv (note : Lifecycle.SlideNote) (now : TimePoint) :
    shouldBeCheckable note now = ok (Lifecycle.slideShouldBeCheckable note now) := by
  simp [shouldBeCheckable, slide_lifecycle.should_be_checkable,
    Lifecycle.slideShouldBeCheckable] <;> split_ifs <;> simp_all

theorem effective_grade_equiv (style : JudgeStyle) (subdivide : Bool) (raw : JudgeGrade) :
    slide_lifecycle.effective_grade (ofLnmJudgeStyle style) subdivide (ofLnmJudgeGrade raw) =
      ok (ofLnmJudgeGrade (slideEffectiveJudgeGrade style subdivide raw)) := by
  cases style <;> cases raw <;> cases subdivide <;> rfl

/-- As in the runtime, arithmetic, queue snapshots and event construction are
    interpreted by their reference adapters. This is not an extraction of the
    bigint implementation or of the queue-to-render adapter. -/
def tooLateStep (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext)
    (checkable : Bool) : Result Lifecycle.SlideStepSemantic := do
  let base := buildSlideStaticSemanticBase note checkable
  let raw := Judge.judgeSlideTooLate (Lifecycle.slideQueueRemaining note.judgeQueues)
  let grade ← slide_lifecycle.effective_grade (ofLnmJudgeStyle ctx.style)
    ctx.subdivideSlideJudgeGrade (ofLnmJudgeGrade raw)
  ok { base with
    note := { base.note with state := .Ended }
    event := if note.isConnSlide && !note.isGroupPartEnd then none
      else some (slideJudgeEvent note (toLnmJudgeGrade grade) slideTooLateJudgeDiff)
    hideSlide := true }

theorem too_late_step_equiv (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext)
    (checkable : Bool) :
    tooLateStep note ctx checkable = ok (slideTooLateStepSemantic note ctx checkable) := by
  simp [tooLateStep, effective_grade_equiv, slideTooLateStepSemantic, bind_tc_ok]

def activeStep (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext)
    (judgable : Bool) (wait : Duration) : Result Lifecycle.SlideStepSemantic := do
  let active := { note with state := Lifecycle.SlideState.Active wait, isCheckable := true }
  let base := buildSlideStaticSemanticBase active true
  let action ← slide_lifecycle.active_action judgable
    (Lifecycle.slideQueuesCleared active.judgeQueues) (ctx.currentTime > slideTooLateTiming active)
  match action with
  | .Complete =>
    let diff := slideCurrentJudgeDiff active ctx.currentTime ctx.touchPanelOffset
    let raw := if active.isClassic then Judge.judgeSlideClassic diff
      else Judge.judgeSlideModern diff wait active.params.isEX
    let stored ← if active.isClassic then ok (ofLnmJudgeGrade raw)
      else convert.convert_grade (ofLnmJudgeStyle ctx.style) (ofLnmJudgeGrade raw)
    let newWait := slideAdjustedJudgedWaitTime active ctx.currentTime wait diff
    ok { base with
      note := { base.note with state := .Judged (toLnmJudgeGrade stored) newWait diff }
      shouldPlayTrackOns := active.isGroupPartHead || !active.isConnSlide
      emitProgressRender := true }
  | .Timeout => tooLateStep active ctx true
  | .SampleSensors =>
    let sampled := buildSlideSensorSemanticBase active ctx true
    ok { sampled with
      shouldPlayTrackOns := active.isGroupPartHead || !active.isConnSlide
      emitProgressRender := sampled.progressChanged }

/-- Covers completion-before-timeout-before-sampling, also for connected
    nonfinal parts and WiFi queues, without reachability assumptions. -/
theorem active_step_equiv (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext)
    (judgable : Bool) (wait : Duration) :
    activeStep note ctx judgable wait = ok (slideActiveStepSemantic note ctx judgable wait) := by
  simp [activeStep, slide_lifecycle.active_action, slideActiveStepSemantic,
    too_late_step_equiv, ConvertProof.convert_grade_equiv] <;>
    split_ifs <;> simp_all [bind_tc_ok]

def initialWait (note : Lifecycle.SlideNote) : Duration :=
  match note.state with
  | .Waiting => slideInitialWaitTime note
  | _ => Duration.zero

def waitExpired : Lifecycle.SlideState → Bool
  | .Judged _ wait _ => wait ≤ Duration.zero
  | _ => false

def stepSemantic (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    Result Lifecycle.SlideStepSemantic := do
  let checkable ← shouldBeCheckable note ctx.currentTime
  let judgable := note.isGroupPartEnd || !note.isConnSlide
  let action ← slide_lifecycle.transition (ofState note.state) (initialWait note)
    checkable judgable (ctx.currentTime > slideTooLateTiming note) (waitExpired note.state)
  match action with
  | .Dormant => ok (Lifecycle.buildSlideDormantSemanticBase note)
  | .Activate wait => activeStep note ctx judgable wait
  | .Timeout => tooLateStep note ctx false
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

/-- Every state of the extracted transition composed with the runtime adapters
    agrees with the original Lean semantic step, including restored states. -/
theorem step_semantic_equiv (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    stepSemantic note ctx = ok (Lifecycle.slideStepSemantic note ctx) := by
  cases hs : note.state <;>
    simp [stepSemantic, should_be_checkable_equiv, slide_lifecycle.transition,
      slide_lifecycle.unjudged_action, ofState, initialWait, waitExpired, hs,
      Lifecycle.slideStepSemantic, active_step_equiv, too_late_step_equiv,
      effective_grade_equiv, bind_tc_ok] <;>
    split_ifs <;> simp_all [active_step_equiv, too_late_step_equiv,
      effective_grade_equiv, bind_tc_ok]

/-- Rust maps the empty track list without Lean's redundant nonempty guard. -/
def audioCommands (s : Lifecycle.SlideStepSemantic) (now : TimePoint) : List AudioCommand :=
  if s.shouldPlayTrackOns then
    s.trackOns.map (fun track => .PlaySlideCue s.note.params.noteIndex track
      s.note.params.isBreak now)
  else []

theorem audio_commands_equiv (s : Lifecycle.SlideStepSemantic) (now : TimePoint) :
    audioCommands s now = slideSemanticAudioCmds s now := by
  cases ht : s.trackOns <;> simp [audioCommands, slideSemanticAudioCmds, ht]

def ended : Lifecycle.SlideState → Bool
  | .Ended => true
  | _ => false

def encodeOutput (s : Lifecycle.SlideStepSemantic) (now : TimePoint) :
    Result (Lifecycle.SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand) := do
  let flags ← slide_lifecycle.output_flags (ended s.note.state) s.shouldPlayTrackOns
    s.note.slideSoundPlayed (!s.trackOns.isEmpty) s.hideSlide
  let s := if flags.mark_sound_played then
    { s with note := { s.note with slideSoundPlayed := true } } else s
  let audio := if flags.emit_audio then audioCommands s now else []
  let render := if flags.emit_render then slideSemanticRenderCmds s else []
  ok (s.note, s.event, audio, render)

def referenceOutput (s : Lifecycle.SlideStepSemantic) (now : TimePoint) :
    Lifecycle.SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand :=
  let s := if s.shouldPlayTrackOns && !s.note.slideSoundPlayed && !s.trackOns.isEmpty then
    { s with note := { s.note with slideSoundPlayed := true } } else s
  let audio := match s.note.state with
    | .Ended => []
    | _ => slideSemanticAudioCmds s now
  let render := match s.note.state with
    | .Ended => if s.hideSlide then slideSemanticRenderCmds s else []
    | _ => slideSemanticRenderCmds s
  (s.note, s.event, audio, render)

theorem encode_output_equiv (s : Lifecycle.SlideStepSemantic) (now : TimePoint) :
    encodeOutput s now = ok (referenceOutput s now) := by
  cases hs : s.note.state <;> cases hp : s.shouldPlayTrackOns <;>
    cases hh : s.hideSlide <;> cases ht : s.trackOns <;>
    simp [encodeOutput, referenceOutput, slide_lifecycle.output_flags, ended,
      hs, hp, hh, ht, audioCommands, slideSemanticAudioCmds, bind_tc_ok] <;>
    split_ifs <;> simp_all [audioCommands, slideSemanticAudioCmds]

def step (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    Result (Lifecycle.SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand) := do
  let s ← stepSemantic note ctx
  encodeOutput s ctx.currentTime

theorem step_equiv (note : Lifecycle.SlideNote) (ctx : Lifecycle.SlideStepContext) :
    step note ctx = ok (Lifecycle.slideStep note ctx.currentTime ctx.sensorHeld
      ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivideSlideJudgeGrade) := by
  simp only [step, step_semantic_equiv, bind_tc_ok, encode_output_equiv]
  rfl

abbrev StepOutput :=
  Lifecycle.SlideNote × Option JudgeEvent × List AudioCommand × List RenderCommand

def trace : Lifecycle.SlideNote → List Lifecycle.SlideStepContext → Result (List StepOutput)
  | _, [] => ok []
  | note, frame :: frames => do
      let next ← step note frame
      let rest ← trace next.1 frames
      ok (next :: rest)

def referenceTrace : Lifecycle.SlideNote → List Lifecycle.SlideStepContext → List StepOutput
  | _, [] => []
  | note, frame :: frames =>
      let next := Lifecycle.slideStep note frame.currentTime frame.sensorHeld
        frame.touchPanelOffset frame.delta frame.style frame.subdivideSlideJudgeGrade
      next :: referenceTrace next.1 frames

theorem trace_equiv (note : Lifecycle.SlideNote) (frames : List Lifecycle.SlideStepContext) :
    trace note frames = ok (referenceTrace note frames) := by
  induction frames generalizing note with
  | nil => rfl
  | cons frame frames ih => simp [trace, referenceTrace, step_equiv, ih]

end Verification.SlideLifecycleProof
