import Verification.SchedulerFrame
import Verification.SlideScheduleParents

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.SchedulerFrameSlideProof
open LnmaiCore InputModel Lifecycle
open private processSlideNotes from LnmaiCore.Scheduler

/-!
Compose the extracted slide traversal and parent kernels into the frame driver.
There is no premise asserting correctness of a complete Slide pass.

The wrapper follows scheduler.rs's process_frame_phase.Slide and the empty-prefix
case of process_slide_notes_core: copy the current slides into an owned buffer,
start at zero with empty outputs, use the CURRENT frame's context, then replace
exactly slides / slide events / slide audio / slide render. In particular it
neither consumes the click cursor nor appends to the old slide output batches.

This is a composition over concrete-operation interfaces, not an extraction of
the Rust FrameWork / SlideScheduleFrame facades. Their field, copy, context and
parent-kernel wiring contracts remain explicit. OtherAtoms.step still requires
the actual per-note lifecycle and its Option-to-list event representation. The
parent kernels' universal copy/budget premises are separate from output Fits.
The frame corollary uses the existing reference finishFrame; it does not prove
Rust initialization, scoring, command conversion, or final record writes.
-/

/-- Point operations at the frame boundary. copySlides models the initial
collect/extend_from_slice allocation, separately from parent-refresh cloning.
ops captures the input and context used by SlideScheduleFrame for this frame. -/
structure FrameAdapters (W S E A C Ops : Type) where
  slides : W → S
  copySlides : S → Result S
  length : S → Usize
  emptyEvents : E
  emptyAudio : A
  emptyRender : C
  ops : W → Ops
  write : W → S → E → A → C → W

def frameContext (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    SlideScheduleProof.Context :=
  { input := input
    currentTime := frame.state.currentTime
    touchPanelOffset := frame.state.touchPanelOffset
    delta := input.delta
    style := frame.state.judgeStyle
    subdivide := frame.state.subdivideSlideJudgeGrade }

def referenceOutput (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    SlideScheduleProof.Outcome :=
  let ctx := frameContext input frame
  Scheduler.processSlideNotesCore [] frame.state.slides ctx.input ctx.currentTime
    ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide [] [] []

/-- Only four fields change; the cursor, other event batches and all other
GameState fields are retained. This is a field-write interpretation, not a pass. -/
def writeReference (frame : SchedulerFrameProof.FrameWork)
    (output : SlideScheduleProof.Outcome) : SchedulerFrameProof.FrameWork :=
  let (slides, events, audio, render) := output
  { frame with
    state := { frame.state with slides := slides }
    events := { frame.events with slide := events }
    slideAudio := audio
    slideRender := render }

theorem reference_slide (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    writeReference frame (referenceOutput input frame) =
      SchedulerFrameProof.referencePass input .Slide frame := by
  rfl

/-- Field/container laws for arbitrary supplied outputs. No law mentions the
result of a traversal, a lifecycle step, or referencePass. -/
structure FrameFieldsAgree {W S N E A C Ops : Type}
    (a : FrameAdapters W S E A C Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (repr : W → SchedulerFrameProof.FrameWork) : Prop where
  slides : ∀ frame, rep.slides (a.slides frame) = (repr frame).state.slides
  length : ∀ slides, (a.length slides).val = (rep.slides slides).length
  empty_events : rep.events a.emptyEvents = []
  empty_audio : rep.audio a.emptyAudio = []
  empty_render : rep.render a.emptyRender = []
  write : ∀ frame slides events audio render,
    repr (a.write frame slides events audio render) =
      writeReference (repr frame)
        (rep.slides slides, rep.events events, rep.audio audio, rep.render render)

def initialWork {W S E A C Ops : Type} (a : FrameAdapters W S E A C Ops)
    (slides : S) : slide_schedule.SlideWork S E A C :=
  { slides := slides
    events := a.emptyEvents
    audio := a.emptyAudio
    render := a.emptyRender
    index := 0#usize }

/-- Executable composition: the entire traversal is Generated's process, not
an opaque frame callback or a call to the Lean reference scheduler. -/
def slidePhase {W S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops) (frame : W) : Result W := do
  let slides ← a.copySlides (a.slides frame)
  let out ← slide_schedule.process call (a.length slides) (initialWork a slides) (a.ops frame)
  ok (a.write frame out.slides out.events out.audio out.render)

/-- Discharge the whole Slide phase using parent point contracts and the
remaining read/step/replace/append atoms. hcopy is only the initial container
copy; hparentCopy and the universal parent bounds are inherited unchanged from
atomic_agrees_of_kernels. other.step fixes the current context, including delta
and held sensors, and is still a per-note lifecycle obligation. -/
theorem slide_phase_refines {W S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput)
    (cap : SlideScheduleProof.Capacity)
    (parents : SlideScheduleParentsProof.Adapters S N C)
    (snapshotRepr : parents.Snapshot → List SlideNote) (id : parents.Id → Nat)
    (parentCapacity : Nat) (fields : FrameFieldsAgree a rep repr)
    (points : SlideScheduleParentsProof.PointsAgree parents rep.slides rep.render
      snapshotRepr id parentCapacity)
    (metadata : SlideScheduleParentsProof.MetadataAgrees parents rep.note id)
    (containers : SlideScheduleParentsProof.ContainersAgree parents rep.slides rep.render
      snapshotRepr)
    (hforceCapacity : 0 < parentCapacity)
    (hancestorCapacity : ∀ slides, (parents.length slides).val ≤ parentCapacity)
    (hparentCopy : ∀ slides, SlideScheduleParentsProof.CopyAgrees parents rep.slides slides)
    (frame : W)
    (hcopy : ∃ copied, a.copySlides (a.slides frame) = ok copied ∧
      rep.slides copied = rep.slides (a.slides frame))
    (facade : SlideScheduleParentsProof.FacadeCalls call (a.ops frame) parents)
    (other : SlideScheduleParentsProof.OtherAtoms call (a.ops frame) rep
      (frameContext input (repr frame)) cap)
    (hfit : SlideScheduleProof.Fits cap (referenceOutput input (repr frame))) :
    ∃ next, slidePhase call a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Slide (repr frame) := by
  obtain ⟨copied, hcopied, hcopiedRepr⟩ := hcopy
  have atoms := SlideScheduleParentsProof.atomic_agrees_of_kernels
    call (a.ops frame) rep (frameContext input (repr frame)) cap parents
    snapshotRepr id parentCapacity points metadata containers facade
    hforceCapacity hancestorCapacity hparentCopy other
  have hinit : SlideScheduleProof.interpret rep (initialWork a copied) =
      SlideScheduleProof.initialReference [] (repr frame).state.slides [] [] [] := by
    simp [SlideScheduleProof.interpret, initialWork, SlideScheduleProof.initialReference,
      fields.empty_events, fields.empty_audio, fields.empty_render, hcopiedRepr,
      fields.slides frame]
  have hlen : (rep.slides (initialWork a copied).slides).length = (a.length copied).val :=
    (fields.length copied).symm
  obtain ⟨out, hrun, houtput, _, _⟩ := spec_imp_exists
    (SlideScheduleProof.process_equiv call (a.ops frame) rep
      (frameContext input (repr frame)) cap atoms (a.length copied) (initialWork a copied)
      [] (repr frame).state.slides [] [] [] hinit hlen hfit)
  refine ⟨a.write frame out.slides out.events out.audio out.render, ?_, ?_⟩
  · simp only [slidePhase, hcopied, bind_tc_ok, hrun]
  · calc
      repr (a.write frame out.slides out.events out.audio out.render) =
          writeReference (repr frame) (SlideScheduleProof.interpret rep out).output :=
        fields.write frame out.slides out.events out.audio out.render
      _ = writeReference (repr frame) (referenceOutput input (repr frame)) :=
        congrArg (writeReference (repr frame)) houtput
      _ = SchedulerFrameProof.referencePass input .Slide (repr frame) :=
        reference_slide input (repr frame)

/-- Install the proved composition on Slide; delegate exactly the other phases
to base. The Slide branch is definitional, with no callback-equivalence premise. -/
def frameDispatch {W S N E A C Ops F : Type}
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops) (f : F)
    (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .Slide => slidePhase call a args.2
  | phase => base.call f (phase, args.2)

def frameCall {W S N E A C Ops F : Type}
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops) :
    core.ops.function.Fn F (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst := { call_once := frameDispatch base call a }
        call_mut := fun f args => do
          let out ← frameDispatch base call a f args
          ok (out, f) }
    call := frameDispatch base call a }

theorem frame_call_slide {W S N E A C Ops F : Type}
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops) (f : F) (frame : W) :
    (frameCall base call a).call f (.Slide, frame) = slidePhase call a frame := rfl

/-- The frame driver's hstep now needs whole-pass correctness only for Tap,
Hold, Touch and TouchHold. Slide uses slide_phase_refines at the actual updated
accumulator, after Tap and Hold. As in the original driver theorem, contracts
are universal over represented frames; no reachable-state restriction is
silently substituted. This installed callback is not claimed to be the
unextracted Rust process_frame_phase facade. hinitial and the reference-only
finishFrame boundary retain their original scope. -/
theorem process_frame_to_stepFrame {W S N E A C Ops F : Type}
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W) (f : F)
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (repr : W → SchedulerFrameProof.FrameWork) (initial : W)
    (st : GameState) (input : FrameInput) (cap : SlideScheduleProof.Capacity)
    (parents : SlideScheduleParentsProof.Adapters S N C)
    (snapshotRepr : parents.Snapshot → List SlideNote) (id : parents.Id → Nat)
    (parentCapacity : Nat) (fields : FrameFieldsAgree a rep repr)
    (points : SlideScheduleParentsProof.PointsAgree parents rep.slides rep.render
      snapshotRepr id parentCapacity)
    (metadata : SlideScheduleParentsProof.MetadataAgrees parents rep.note id)
    (containers : SlideScheduleParentsProof.ContainersAgree parents rep.slides rep.render
      snapshotRepr)
    (hforceCapacity : 0 < parentCapacity)
    (hancestorCapacity : ∀ slides, (parents.length slides).val ≤ parentCapacity)
    (hparentCopy : ∀ slides, SlideScheduleParentsProof.CopyAgrees parents rep.slides slides)
    (hcopy : ∀ frame, ∃ copied, a.copySlides (a.slides frame) = ok copied ∧
      rep.slides copied = rep.slides (a.slides frame))
    (facade : ∀ frame, SlideScheduleParentsProof.FacadeCalls call (a.ops frame) parents)
    (other : ∀ frame, SlideScheduleParentsProof.OtherAtoms call (a.ops frame) rep
      (frameContext input (repr frame)) cap)
    (hfit : ∀ frame, SlideScheduleProof.Fits cap (referenceOutput input (repr frame)))
    (hinitial : repr initial = SchedulerFrameProof.initialFrame st input)
    (hTap : ∀ frame, ∃ next, base.call f (.Tap, frame) = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Tap (repr frame))
    (hHold : ∀ frame, ∃ next, base.call f (.Hold, frame) = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Hold (repr frame))
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .TouchHold (repr frame)) :
    ∃ out, scheduler.process_frame (frameCall base call a) initial f = ok out ∧
      SchedulerFrameProof.finishFrame input (repr out) = Scheduler.stepFrame st input := by
  apply SchedulerFrameProof.process_frame_to_stepFrame
    (frameCall base call a) f repr initial st input hinitial
  intro phase frame
  cases phase with
  | Tap => exact hTap frame
  | Hold => exact hHold frame
  | Slide =>
      exact slide_phase_refines call a rep repr input cap parents snapshotRepr id
        parentCapacity fields points metadata containers hforceCapacity hancestorCapacity
        hparentCopy frame (hcopy frame) (facade frame) (other frame) (hfit frame)
  | Touch => exact hTouch frame
  | TouchHold => exact hTouchHold frame

end Verification.SchedulerFrameSlideProof
