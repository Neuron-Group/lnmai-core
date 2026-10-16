import Verification.SchedulerFrameSlide
import Verification.SlideScheduleJudgments
import Verification.SchedulerFrameState

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.SchedulerFrameJudgmentsProof
open LnmaiCore InputModel Lifecycle SchedulerFrameProof

/-!
Thin composition of the extracted frame driver, slide traversal, parent,
lifecycle/judgment and finalization kernels. OtherAtoms.step is derived from
SlideScheduleJudgments, and the complete Slide phase from SchedulerFrameSlide.
Only Tap/Hold/Touch/TouchHold retain whole-phase refinement premises.

Runtime wiring: scheduler.rs's process_frame_phase reads Slide's context from
the accumulator AFTER Tap and Hold. frameDispatch therefore builds installStep
at the Slide call, using frameContext input (repr CURRENT_FRAME). A callback
contains the current input and the delegate value, never an initial context.
The finite trace pairs each new input with its own callback and initializes
from the preceding frame's output state.

The result concerns these installed extracted-kernel compositions, NOT the
unextracted Rust FrameWork/SlideScheduleFrame/process_frame_phase/step_frame
facades. Initialization, exact time/field interpretation, copies, containers,
output encoding/allocation, parent metadata/point laws/facade calls and local
capacity bounds remain explicit. SlideContracts contains no lifecycle or pass
equivalence. Its universal frame/container obligations are inherited, not
proved from reachability or from the final output capacity bound.

SlideJudgments' queue/sensor/time and event/command construction adapters remain
reference interpretations. SchedulerFrameState supplies finalization from the
ORIGINAL score/settings and advanced time; its command generators, list/Vec
interpretation, score counter storage and final record writes remain boundaries.
The common IntegerAgrees and exact encode/decode laws cover both slide judgment
and score judgment; no machine-integer range restriction is substituted.
-/

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)

variable {W S N E A C Ops F : Type}
  (schedule : slide_schedule.SlideSchedule Ops S N E A C)
  (a : SchedulerFrameSlideProof.FrameAdapters W S E A C Ops)
  (rep : SlideScheduleProof.Representation S N E A C)
  (pack : SlideScheduleJudgmentsProof.OutputEncoding N E A C)
  (repr : W → FrameWork)

/-- Install the proved per-note composition with this invocation's context.
The existing phase wrapper still copies slides, starts empty output batches,
runs the extracted traversal, and writes its four resulting fields. -/
def slidePhase (input : FrameInput) (frame : W) : Result W :=
  SchedulerFrameSlideProof.slidePhase
    (SlideScheduleJudgmentsProof.installStep inst integerOps encode schedule rep pack
      (SchedulerFrameSlideProof.frameContext input (repr frame))) a frame

/-- The current accumulator is supplied by the extracted five-phase driver.
Input is fixed within one frame; its state-derived context is read at Slide. -/
def frameDispatch (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (callback : FrameInput × F) (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .Slide =>
      slidePhase inst integerOps encode schedule a rep pack repr callback.1 args.2
  | phase => base.call callback.2 (phase, args.2)

def frameCall (base : core.ops.function.Fn F (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × F) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once := frameDispatch inst integerOps encode schedule a rep pack repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps encode schedule a rep pack repr base
            callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps encode schedule a rep pack repr base }

/-- This definitional equation exposes the actual current-frame installation;
there is no context captured when frameCall itself is constructed. -/
theorem frame_call_slide
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (input : FrameInput) (f : F) (frame : W) :
    (frameCall inst integerOps encode schedule a rep pack repr base).call
      (input, f) (.Slide, frame) =
      SchedulerFrameSlideProof.slidePhase
        (SlideScheduleJudgmentsProof.installStep inst integerOps encode schedule rep pack
          (SchedulerFrameSlideProof.frameContext input (repr frame))) a frame := rfl

def processAndFinish (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (f : F) (initial : W) (state : GameState) (input : FrameInput) :
    Result SchedulerFrameStateProof.FrameOutput :=
  SchedulerFrameStateProof.processAndFinishFromInitial inst integerOps encode
    (frameCall inst integerOps encode schedule a rep pack repr base)
    (input, f) repr initial state input

/-- The driver callback is fixed, but each iteration supplies a fresh input.
Every state/event/audio/render output and the final state are retained. -/
def trace (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (callback : FrameInput → F) (initializeFrame : GameState → FrameInput → W)
    (state : GameState) (inputs : List FrameInput) :
    Result (GameState × List SchedulerFrameStateProof.FrameOutput) :=
  SchedulerFrameStateProof.traceFromInitial inst integerOps encode
    (frameCall inst integerOps encode schedule a rep pack repr base)
    (fun input => (input, callback input)) repr initializeFrame state inputs

/-- Only primitive/representation obligations. RemainingAtoms omits step;
EncodingReady encodes four independently computed output fields. Fits is only
an output-length bound, separate from temporary parent command capacity. -/
structure SlideContracts (cap : SlideScheduleProof.Capacity)
    (parents : SlideScheduleParentsProof.Adapters S N C)
    (snapshotRepr : parents.Snapshot → List SlideNote) (id : parents.Id → Nat)
    (parentCapacity : Nat) (input : FrameInput) : Prop where
  fields : SchedulerFrameSlideProof.FrameFieldsAgree a rep repr
  points : SlideScheduleParentsProof.PointsAgree parents rep.slides rep.render
    snapshotRepr id parentCapacity
  metadata : SlideScheduleParentsProof.MetadataAgrees parents rep.note id
  containers : SlideScheduleParentsProof.ContainersAgree parents rep.slides rep.render
    snapshotRepr
  force_capacity : 0 < parentCapacity
  ancestor_capacity : ∀ slides, (parents.length slides).val ≤ parentCapacity
  parent_copy : ∀ slides, SlideScheduleParentsProof.CopyAgrees parents rep.slides slides
  frame_copy : ∀ frame, ∃ copied, a.copySlides (a.slides frame) = ok copied ∧
    rep.slides copied = rep.slides (a.slides frame)
  parent_facade : ∀ frame,
    SlideScheduleParentsProof.FacadeCalls schedule (a.ops frame) parents
  remaining : ∀ frame,
    SlideScheduleJudgmentsProof.RemainingAtoms schedule (a.ops frame) rep cap
  encoding : ∀ frame,
    SlideScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack
      (SchedulerFrameSlideProof.frameContext input (repr frame))
  fits : ∀ frame,
    SlideScheduleProof.Fits cap (SchedulerFrameSlideProof.referenceOutput input (repr frame))

variable {cap : SlideScheduleProof.Capacity}
  {parents : SlideScheduleParentsProof.Adapters S N C}
  {snapshotRepr : parents.Snapshot → List SlideNote} {id : parents.Id → Nat}
  {parentCapacity : Nat}

include integerLaw decode_encode

/-- Fill OtherAtoms.step using the judgment/lifecycle proof, transport only the
unchanged parent facade calls, and reuse the existing Slide phase theorem. -/
theorem slide_phase_refines (input : FrameInput)
    (laws : SlideContracts inst integerOps encode schedule a rep pack repr
      cap parents snapshotRepr id parentCapacity input) (frame : W) :
    ∃ next, slidePhase inst integerOps encode schedule a rep pack repr input frame = ok next ∧
      repr next = referencePass input .Slide (repr frame) := by
  let ctx := SchedulerFrameSlideProof.frameContext input (repr frame)
  let installed :=
    SlideScheduleJudgmentsProof.installStep inst integerOps encode schedule rep pack ctx
  have other := SlideScheduleJudgmentsProof.other_atoms_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    schedule (a.ops frame) rep pack ctx cap (laws.encoding frame) (laws.remaining frame)
  have facade : SlideScheduleParentsProof.FacadeCalls installed (a.ops frame) parents :=
    ⟨(laws.parent_facade frame).force_direct, (laws.parent_facade frame).end_ancestors,
      (laws.parent_facade frame).refresh⟩
  exact SchedulerFrameSlideProof.slide_phase_refines installed a rep repr input cap
    parents snapshotRepr id parentCapacity laws.fields laws.points laws.metadata
    laws.containers laws.force_capacity laws.ancestor_capacity laws.parent_copy
    frame (laws.frame_copy frame) facade other (laws.fits frame)

/-- Exactly four external pass premises. The Slide branch is proved at the
actual accumulator passed to dispatch, including after Tap and Hold updates. -/
theorem frame_call_refines
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (f : F) (input : FrameInput)
    (laws : SlideContracts inst integerOps encode schedule a rep pack repr
      cap parents snapshotRepr id parentCapacity input)
    (hTap : ∀ frame, ∃ next, base.call f (.Tap, frame) = ok next ∧
      repr next = referencePass input .Tap (repr frame))
    (hHold : ∀ frame, ∃ next, base.call f (.Hold, frame) = ok next ∧
      repr next = referencePass input .Hold (repr frame))
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    ∀ phase frame, ∃ next,
      (frameCall inst integerOps encode schedule a rep pack repr base).call
        (input, f) (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame) := by
  intro phase frame
  cases phase with
  | Tap => exact hTap frame
  | Hold => exact hHold frame
  | Slide =>
      exact slide_phase_refines inst integerOps decode integerLaw encode decode_encode
        schedule a rep pack repr input laws frame
  | Touch => exact hTouch frame
  | TouchHold => exact hTouchHold frame

/-- The complete frame uses extracted ordering and judgment-aware score
folding, with the original state's score/settings and separately advanced time.
Neither finalization nor the complete Slide result is an assumed equation. -/
theorem process_and_finish_equiv
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (f : F) (initial : W) (state : GameState) (input : FrameInput)
    (laws : SlideContracts inst integerOps encode schedule a rep pack repr
      cap parents snapshotRepr id parentCapacity input)
    (hinitial : repr initial = initialFrame state input)
    (hTap : ∀ frame, ∃ next, base.call f (.Tap, frame) = ok next ∧
      repr next = referencePass input .Tap (repr frame))
    (hHold : ∀ frame, ∃ next, base.call f (.Hold, frame) = ok next ∧
      repr next = referencePass input .Hold (repr frame))
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    processAndFinish inst integerOps encode schedule a rep pack repr base f initial
      state input = ok (Scheduler.stepFrame state input) := by
  unfold processAndFinish
  exact SchedulerFrameStateProof.process_and_finish_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps encode schedule a rep pack repr base)
    (input, f) repr initial state input hinitial
    (frame_call_refines inst integerOps decode integerLaw encode decode_encode
      schedule a rep pack repr base f input laws hTap hHold hTouch hTouchHold)

/-- Finite histories preserve all outputs and the persistent final state,
including the empty history. The contracts stay universal over represented
frames/inputs; no unproved reachable-state or native allocation invariant is
introduced. Each iteration recomputes Slide's context from its current frame. -/
theorem trace_equiv
    (base : core.ops.function.Fn F (scheduler.FramePhase × W) W)
    (callback : FrameInput → F) (initializeFrame : GameState → FrameInput → W)
    (laws : ∀ input, SlideContracts inst integerOps encode schedule a rep pack repr
      cap parents snapshotRepr id parentCapacity input)
    (hinitial : ∀ state input, repr (initializeFrame state input) = initialFrame state input)
    (hTap : ∀ input frame, ∃ next, base.call (callback input) (.Tap, frame) = ok next ∧
      repr next = referencePass input .Tap (repr frame))
    (hHold : ∀ input frame, ∃ next, base.call (callback input) (.Hold, frame) = ok next ∧
      repr next = referencePass input .Hold (repr frame))
    (hTouch : ∀ input frame, ∃ next, base.call (callback input) (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ input frame, ∃ next,
      base.call (callback input) (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame))
    (state : GameState) (inputs : List FrameInput) :
    trace inst integerOps encode schedule a rep pack repr base callback initializeFrame
      state inputs = ok (SchedulerFrameStateProof.referenceTrace state inputs) := by
  unfold trace
  exact SchedulerFrameStateProof.trace_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps encode schedule a rep pack repr base)
    (fun input => (input, callback input)) repr initializeFrame hinitial
    (fun input => frame_call_refines inst integerOps decode integerLaw encode decode_encode
      schedule a rep pack repr base (callback input) input (laws input)
      (hTap input) (hHold input) (hTouch input) (hTouchHold input)) state inputs

end Verification.SchedulerFrameJudgmentsProof
