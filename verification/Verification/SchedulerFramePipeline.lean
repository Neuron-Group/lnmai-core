import Verification.SchedulerFrameInitialization
import Verification.SchedulerFrameAllTraversals
import Verification.FrameCommandState

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFramePipelineProof
open SchedulerFrameProof SchedulerFrameInitializationProof
open FrameCommandFinalizationProof (Buffers)
open SchedulerFrameStateProof (FrameOutput)

/-!
Compose the proved initialization kernels, all five derived scheduling passes,
and the extracted feedback selectors/collector. No complete initializer, phase,
or finalizer equation is assumed. The feedback collector input buffers are
constructed from their lists with explicit length bounds. Native capacity and
allocation refinement for final command concatenation and trace storage remains
a separate obligation.

This is a decoded interpretation of the runtime facade, not its extraction. FrameWork
fields and payloads are already decoded Lean values. Concrete Rust record,
UTF-8, bigint, allocator, and caller correspondence remain separate obligations,
as do the point contracts on the five pass implementations and initialization.
-/

/-- Usize bounds for the two feedback collector inputs. Final command and trace
storage capacities and native allocation success remain separate obligations. -/
def CommandFits (frame : FrameWork) : Prop :=
  (reportedEvents frame).length ≤ Usize.max ∧
    frame.state.activeTouchHolds.length ≤ Usize.max

/-- Decide the branch guard using its two natural-number comparisons. -/
instance commandFitsDecidable (frame : FrameWork) : Decidable (CommandFits frame) := by
  unfold CommandFits
  infer_instance

def commandBuffers (frame : FrameWork) (fits : CommandFits frame) : Buffers frame :=
  { events := ⟨reportedEvents frame, fits.1⟩
    holds := ⟨frame.state.activeTouchHolds, fits.2⟩
    events_val := rfl
    holds_val := rfl }

variable {I Ops Callback : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (integerOps : Ops) (decode : I → Int) (encode : Int → I)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (decode_encode : ∀ value, decode (encode value) = value)

def finishChecked (state : GameState) (time : TimePoint)
    (input : FrameInput) (frame : FrameWork) : Result FrameOutput :=
  if fits : CommandFits frame then
    FrameCommandStateProof.finishFromInitial inst integerOps encode state time input frame
      (commandBuffers frame fits)
  else fail .maximumSizeExceeded

variable
  (tap : SchedulerFrameTraversalsProof.TapImplementation FrameWork)
  (hold : SchedulerFrameTraversalsProof.HoldImplementation FrameWork)
  (slide : SchedulerFrameTraversalsProof.SlideImplementation FrameWork)
  (touch : SchedulerFrameTouchJudgmentsProof.TouchImplementation FrameWork)
  (touchHold : SchedulerFrameTouchHoldProof.TouchHoldImplementation FrameWork)

/-- The captured timestamp comes from the initialized state, before any pass.
Every phase receives the accumulator returned by the previous phase. -/
def run (p : ParentKernel) (b : BodyKernel)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b)
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : Callback) (state : GameState) (input : FrameInput) : Result FrameOutput := do
  let initial ← initializeFrame p b addTime fields state input
  let frame ← scheduler.process_frame
    (SchedulerFrameAllTraversalsProof.frameCall inst integerOps decode encode
      tap hold slide touch touchHold id base) initial (input, callback)
  finishChecked inst integerOps encode state initial.state.currentTime input frame

/-- Keep every complete output, including the state after each input frame. -/
def trace (p : ParentKernel) (b : BodyKernel)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : GameState → FrameInput → FrameFields p b)
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : FrameInput → Callback) (state : GameState) (inputs : List FrameInput) :
    Result (GameState × List FrameOutput) :=
  match inputs with
  | [] => ok (state, [])
  | input :: rest => do
      let out ← run inst integerOps decode encode tap hold slide touch touchHold
        p b addTime (fields state input) base (callback input) state input
      let (finalState, outputs) ← trace p b addTime fields base callback out.1 rest
      ok (finalState, out :: outputs)

/-- The field/time equations, conditional rebuild bound and CommandFits premise
are required along the reference states of this finite history. Each input also
requires the imported five-pass Contracts, whose field, point and capacity laws
retain their universal quantification over frames and operation arguments.
Concrete witnesses for those contracts remain separate obligations. -/
def TracePremises (p : ParentKernel) (b : BodyKernel)
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
      SchedulerFrameAllTraversalsProof.Contracts inst integerOps decode encode
        tap hold slide touch touchHold id input ∧
      CommandFits (processInOrder (referencePass input) (initialFrame state input)) ∧
      TracePremises p b addTime fields (Scheduler.stepFrame state input).1 rest

include integerLaw decode_encode

theorem finish_checked_equiv (state : GameState) (input : FrameInput)
    (fits : CommandFits (processInOrder (referencePass input) (initialFrame state input))) :
    finishChecked inst integerOps encode state (state.currentTime + input.delta) input
      (processInOrder (referencePass input) (initialFrame state input)) =
      ok (Scheduler.stepFrame state input) := by
  rw [finishChecked, dif_pos fits]
  exact FrameCommandStateProof.reference_finish_equiv inst integerOps decode encode
    integerLaw decode_encode state input _

/-- A complete frame result from point/representation contracts: initialization,
the five actual traversals, judgments, score, held inputs, events, audio, render.
The source result appears only on the right-hand side of the conclusion. -/
theorem run_equiv (p : ParentKernel) (b : BodyKernel)
    (parents : ParentPoints p) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b)
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : Callback) (state : GameState) (input : FrameInput)
    (hfields : FieldsAgree p b fields state)
    (htime : addTime state.currentTime input.delta = ok (state.currentTime + input.delta))
    (hgroups : state.touchHoldGroupStates.isEmpty = true →
      TouchHoldRebuildTraversalProof.PrefixFits b.capacity state.activeTouchHolds [])
    (laws : SchedulerFrameAllTraversalsProof.Contracts inst integerOps decode encode
      tap hold slide touch touchHold id input)
    (fits : CommandFits (processInOrder (referencePass input) (initialFrame state input))) :
    run inst integerOps decode encode tap hold slide touch touchHold p b addTime fields
      base callback state input = ok (Scheduler.stepFrame state input) := by
  have hinit := initialize_equiv p b parents body addTime fields state input
    hfields htime hgroups
  have hstep := SchedulerFrameAllTraversalsProof.frame_call_refines
    inst integerOps decode integerLaw encode decode_encode tap hold slide touch touchHold
    id base callback input laws
  obtain ⟨frame, hrun, hframe⟩ := process_frame_refines
    (SchedulerFrameAllTraversalsProof.frameCall inst integerOps decode encode
      tap hold slide touch touchHold id base) (input, callback) id (referencePass input)
    (initialFrame state input) hstep
  change frame = processInOrder (referencePass input) (initialFrame state input) at hframe
  rw [hframe] at hrun
  simp only [run, hinit, bind_tc_ok, hrun]
  change finishChecked inst integerOps encode state (state.currentTime + input.delta) input
    (processInOrder (referencePass input) (initialFrame state input)) = _
  exact finish_checked_equiv inst integerOps decode encode integerLaw decode_encode
    state input fits

/-- Original stepFrame at every frame, using extracted initialization, phase and
feedback kernels inside the decoded interpretation. Explicit per-state premises
follow this history; imported pass contracts and ParentPoints/BodyPoints retain
their original universal scope. Native record/scalar/container refinement and
the runtime frame-loop correspondence remain separate obligations. -/
theorem trace_equiv (p : ParentKernel) (b : BodyKernel)
    (parents : ParentPoints p) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : GameState → FrameInput → FrameFields p b)
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × FrameWork) FrameWork)
    (callback : FrameInput → Callback) (state : GameState) (inputs : List FrameInput)
    (ready : TracePremises inst integerOps decode encode tap hold slide touch touchHold
      p b addTime fields state inputs) :
    trace inst integerOps decode encode tap hold slide touch touchHold p b addTime fields
      base callback state inputs = ok (SchedulerFrameStateProof.referenceTrace state inputs) := by
  induction inputs generalizing state with
  | nil => rfl
  | cons input rest ih =>
      rcases ready with ⟨hfields, htime, hgroups, laws, fits, hrest⟩
      simp only [trace,
        run_equiv inst integerOps decode encode integerLaw decode_encode
          tap hold slide touch touchHold p b parents body addTime (fields state input)
          base (callback input) state input hfields htime hgroups laws fits,
        bind_tc_ok, ih _ hrest, SchedulerFrameStateProof.referenceTrace]

end Verification.SchedulerFramePipelineProof
