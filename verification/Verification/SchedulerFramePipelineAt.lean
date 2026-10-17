import Verification.SchedulerFramePipeline
import Verification.SchedulerFrameContractsAt

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFramePipelineAtProof
open SchedulerFrameProof SchedulerFrameInitializationProof
open SchedulerFramePipelineProof (CommandFits)

/-!
A companion entry theorem for the EXISTING Pipeline.run and Pipeline.trace.
Each contract is indexed by the actual reference input to that phase. No
all-frame field/capacity contract and no complete-phase equation is assumed.
The five equations used below are proved from installed judgments and kernels.

Tap/Touch output capacities and the +1-fuel bound follow the actual
storage-order prefixes. A full group is never required to append a fresh ID.
Concrete field/point instances and the current finite numeric bounds remain
caller obligations. Initialization, including the conditional TouchHold
rebuild PrefixFits, is reused; this module does not derive rebuild capacities.
-/

def afterTap (input : FrameInput) (initial : FrameWork) : FrameWork :=
  referencePass input .Tap initial

def afterHold (input : FrameInput) (initial : FrameWork) : FrameWork :=
  referencePass input .Hold (afterTap input initial)

def afterSlide (input : FrameInput) (initial : FrameWork) : FrameWork :=
  referencePass input .Slide (afterHold input initial)

def afterTouch (input : FrameInput) (initial : FrameWork) : FrameWork :=
  referencePass input .Touch (afterSlide input initial)

variable {I J Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int) (encode : Int → I)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (decode_encode : ∀ value, decode (encode value) = value)
  (tap : SchedulerFrameTraversalsProof.TapImplementation FrameWork)
  (hold : SchedulerFrameTraversalsProof.HoldImplementation FrameWork)
  (slide : SchedulerFrameTraversalsProof.SlideImplementation FrameWork)
  (touch : SchedulerFrameTouchJudgmentsProof.TouchImplementation FrameWork)
  (touchHold : SchedulerFrameTouchHoldProof.TouchHoldImplementation FrameWork)

/-- Five contracts at five concrete inputs. In particular TouchHold sees the
cursor and groups returned by Touch, and every context uses its current frame. -/
structure ContractsAt (input : FrameInput) (initial : FrameWork) : Prop where
  tap : SchedulerFrameContractsAtProof.TapAt
    inst integerOps decode encode tap id input initial
  hold : SchedulerFrameContractsAtProof.HoldAt
    inst integerOps encode hold id input (afterTap input initial)
  slide : SchedulerFrameContractsAtProof.SlideAt
    inst integerOps encode slide id input (afterHold input initial)
  touch : SchedulerFrameContractsAtProof.TouchAt
    inst integerOps decode encode touch id input (afterSlide input initial)
  touchHold : SchedulerFrameTouchHoldProof.ContractsAt
    inst integerOps encode touchHold id input (afterTouch input initial)

/-- Premises follow this finite history's actual source states. Neither a
universal FrameWork capacity nor a vacuous Ready predicate is introduced. -/
def TracePremisesAt (p : ParentKernel) (b : BodyKernel)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : GameState → FrameInput → FrameFields p b)
    (state : GameState) (inputs : List FrameInput) : Prop :=
  match inputs with
  | [] => True
  | input :: rest =>
      FieldsAgree p b (fields state input) state ∧
      addTime state.currentTime input.delta = ok (state.currentTime + input.delta) ∧
      (state.touchHoldGroupStates.isEmpty = true →
        TouchHoldRebuildTraversalProof.PrefixFits b.capacity state.activeTouchHolds []) ∧
      ContractsAt inst integerOps decode encode tap hold slide touch touchHold
        input (initialFrame state input) ∧
      CommandFits (processInOrder (referencePass input) (initialFrame state input)) ∧
      TracePremisesAt p b addTime fields (Scheduler.stepFrame state input).1 rest

include integerLaw decode_encode

/-- Unfold the actual generated five-call driver, using only the contracts at
its five visited inputs. This does not invoke the old universal hstep theorem. -/
theorem process_frame_equiv_at
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : Callback) (input : FrameInput) (initial : FrameWork)
    (laws : ContractsAt inst integerOps decode encode tap hold slide touch touchHold
      input initial) :
    scheduler.process_frame
      (SchedulerFrameAllTraversalsProof.frameCall inst integerOps decode encode
        tap hold slide touch touchHold id base) initial (input, callback) =
      ok (processInOrder (referencePass input) initial) := by
  let call := SchedulerFrameAllTraversalsProof.frameCall inst integerOps decode encode
    tap hold slide touch touchHold id base
  have ht : call.call (input, callback) (.Tap, initial) =
      ok (afterTap input initial) := by
    obtain ⟨next, hrun, hnext⟩ := SchedulerFrameContractsAtProof.tap_phase_refines_at
      inst integerOps decode integerLaw encode decode_encode tap id input initial laws.tap
    change next = afterTap input initial at hnext
    rw [hnext] at hrun
    exact hrun
  have hh : call.call (input, callback) (.Hold, afterTap input initial) =
      ok (afterHold input initial) := by
    obtain ⟨next, hrun, hnext⟩ := SchedulerFrameContractsAtProof.hold_phase_refines_at
      inst integerOps decode integerLaw encode decode_encode hold id input
      (afterTap input initial) laws.hold
    change next = afterHold input initial at hnext
    rw [hnext] at hrun
    exact hrun
  have hs : call.call (input, callback) (.Slide, afterHold input initial) =
      ok (afterSlide input initial) := by
    obtain ⟨next, hrun, hnext⟩ := SchedulerFrameContractsAtProof.slide_phase_refines_at
      inst integerOps decode integerLaw encode decode_encode slide id input
      (afterHold input initial) laws.slide
    change next = afterSlide input initial at hnext
    rw [hnext] at hrun
    exact hrun
  have hu : call.call (input, callback) (.Touch, afterSlide input initial) =
      ok (afterTouch input initial) := by
    obtain ⟨next, hrun, hnext⟩ := SchedulerFrameContractsAtProof.touch_phase_refines_at
      inst integerOps decode integerLaw encode decode_encode touch id input
      (afterSlide input initial) laws.touch
    change next = afterTouch input initial at hnext
    rw [hnext] at hrun
    exact hrun
  have hv : call.call (input, callback) (.TouchHold, afterTouch input initial) =
      ok (processInOrder (referencePass input) initial) := by
    obtain ⟨next, hrun, hnext⟩ := SchedulerFrameTouchHoldProof.touch_hold_phase_refines_at
      inst integerOps decode integerLaw encode decode_encode touchHold id input
      (afterTouch input initial) laws.touchHold
    change next = processInOrder (referencePass input) initial at hnext
    rw [hnext] at hrun
    exact hrun
  change scheduler.process_frame call initial (input, callback) = _
  simp only [scheduler.process_frame, ht, bind_tc_ok, hh, hs, hu, hv]

/-- Existing initialization and finalization, with only the five visited-frame
contracts in the middle. Integer, field, point and local size premises remain. -/
theorem run_equiv_at (p : ParentKernel) (b : BodyKernel)
    (parents : ParentPoints p) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b)
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : Callback) (state : GameState) (input : FrameInput)
    (hfields : FieldsAgree p b fields state)
    (htime : addTime state.currentTime input.delta = ok (state.currentTime + input.delta))
    (hgroups : state.touchHoldGroupStates.isEmpty = true →
      TouchHoldRebuildTraversalProof.PrefixFits b.capacity state.activeTouchHolds [])
    (laws : ContractsAt inst integerOps decode encode tap hold slide touch touchHold
      input (initialFrame state input))
    (fits : CommandFits (processInOrder (referencePass input) (initialFrame state input))) :
    SchedulerFramePipelineProof.run inst integerOps decode encode tap hold slide touch touchHold
      p b addTime fields base callback state input = ok (Scheduler.stepFrame state input) := by
  have hinit := initialize_equiv p b parents body addTime fields state input
    hfields htime hgroups
  have hrun := process_frame_equiv_at inst integerOps decode encode integerLaw decode_encode
    tap hold slide touch touchHold base callback input (initialFrame state input) laws
  simp only [SchedulerFramePipelineProof.run, hinit, bind_tc_ok, hrun]
  change SchedulerFramePipelineProof.finishChecked inst integerOps encode
    state (state.currentTime + input.delta) input
    (processInOrder (referencePass input) (initialFrame state input)) = _
  exact SchedulerFramePipelineProof.finish_checked_equiv
    inst integerOps decode encode integerLaw decode_encode state input fits

/-- Same runtime-interpreter trace as Pipeline.trace; each frame is discharged
at its actual source state and its own five intermediate frames. -/
theorem trace_equiv_at (p : ParentKernel) (b : BodyKernel)
    (parents : ParentPoints p) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : GameState → FrameInput → FrameFields p b)
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : FrameInput → Callback) (state : GameState) (inputs : List FrameInput)
    (ready : TracePremisesAt inst integerOps decode encode tap hold slide touch touchHold
      p b addTime fields state inputs) :
    SchedulerFramePipelineProof.trace inst integerOps decode encode tap hold slide touch touchHold
      p b addTime fields base callback state inputs =
      ok (SchedulerFrameStateProof.referenceTrace state inputs) := by
  induction inputs generalizing state with
  | nil => rfl
  | cons input rest ih =>
      rcases ready with ⟨hfields, htime, hgroups, laws, fits, hrest⟩
      simp only [SchedulerFramePipelineProof.trace,
        run_equiv_at inst integerOps decode encode integerLaw decode_encode
          tap hold slide touch touchHold p b parents body addTime (fields state input)
          base (callback input) state input hfields htime hgroups laws fits,
        bind_tc_ok, ih _ hrest, SchedulerFrameStateProof.referenceTrace]
      cases SchedulerFrameStateProof.referenceTrace (Scheduler.stepFrame state input).1 rest
      rfl

end Verification.SchedulerFramePipelineAtProof
