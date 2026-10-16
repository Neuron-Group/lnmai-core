import Verification.SchedulerFrameTouchTraversals
import Verification.SchedulerFrameTouchHold

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameAllTraversalsProof
open SchedulerFrameProof

/-!
Source-only handoff; compilation is reserved for the mainline worker.

Complete the existing Tap/Hold/Slide/Touch composition with the derived
TouchHold phase. All five phases use the current accumulator and the same
input. The old frame_call_refines supplies the phase proof; this file adds no
new traversal or case analysis and requires no whole-phase/body equation.

The base callback is retained for the dispatcher interface, with no laws on it:
the five installed branches cover every FramePhase. Contracts remain the
explicit field/point/encoding/integer/capacity obligations of the imported
proofs. Initialization, concrete facade identification, allocation and native
scalar/container representations remain outside this composition. The imported
finalization/trace construction is not an extraction of the runtime frame loop.
-/

variable {I J W Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (tap : SchedulerFrameTraversalsProof.TapImplementation W)
  (hold : SchedulerFrameTraversalsProof.HoldImplementation W)
  (slide : SchedulerFrameTraversalsProof.SlideImplementation W)
  (touch : SchedulerFrameTouchJudgmentsProof.TouchImplementation W)
  (touchHold : SchedulerFrameTouchHoldProof.TouchHoldImplementation W)
  (repr : W → FrameWork)

structure Contracts (input : FrameInput) : Prop where
  traversals : SchedulerFrameTouchTraversalsProof.Contracts
    inst integerOps decode encode tap hold slide touch repr input
  touchHold : SchedulerFrameTouchHoldProof.Contracts
    inst integerOps encode touchHold repr input

/-- The outer callback carries one input; both delegates receive that input. -/
def frameDispatch (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput × Callback) (args : scheduler.FramePhase × W) : Result W :=
  (SchedulerFrameTouchTraversalsProof.frameCall
    inst integerOps decode encode tap hold slide touch repr
    (SchedulerFrameTouchHoldProof.frameCall inst integerOps encode touchHold repr base)).call
      (callback.1, callback) args

def frameCall (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × Callback) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once := frameDispatch
              inst integerOps decode encode tap hold slide touch touchHold repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps decode encode tap hold slide touch touchHold
            repr base callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps decode encode tap hold slide touch touchHold repr base }

def processAndFinish (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput) :
    Result SchedulerFrameStateProof.FrameOutput :=
  SchedulerFrameStateProof.processAndFinishFromInitial inst integerOps encode
    (frameCall inst integerOps decode encode tap hold slide touch touchHold repr base)
    (input, f) repr initial state input

def trace (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput → Callback) (initializeFrame : GameState → FrameInput → W)
    (state : GameState) (inputs : List FrameInput) :
    Result (GameState × List SchedulerFrameStateProof.FrameOutput) :=
  SchedulerFrameStateProof.traceFromInitial inst integerOps encode
    (frameCall inst integerOps decode encode tap hold slide touch touchHold repr base)
    (fun input => (input, callback input)) repr initializeFrame state inputs

include integerLaw decode_encode

/-- Discharge the last complete-pass premise of TouchTraversals by construction. -/
theorem frame_call_refines
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (input : FrameInput)
    (laws : Contracts inst integerOps decode encode tap hold slide touch touchHold repr input) :
    ∀ phase frame, ∃ next,
      (frameCall inst integerOps decode encode tap hold slide touch touchHold repr base).call
        (input, f) (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame) := by
  exact SchedulerFrameTouchTraversalsProof.frame_call_refines
    inst integerOps decode integerLaw encode decode_encode tap hold slide touch repr
    (SchedulerFrameTouchHoldProof.frameCall inst integerOps encode touchHold repr base)
    (input, f) input laws.traversals
    (fun frame => SchedulerFrameTouchHoldProof.touch_hold_phase_refines
      inst integerOps decode integerLaw encode decode_encode touchHold repr input
      laws.touchHold frame)

theorem process_and_finish_equiv
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput)
    (laws : Contracts inst integerOps decode encode tap hold slide touch touchHold repr input)
    (hinitial : repr initial = initialFrame state input) :
    processAndFinish inst integerOps decode encode tap hold slide touch touchHold repr
      base f initial state input = ok (Scheduler.stepFrame state input) := by
  exact SchedulerFrameStateProof.process_and_finish_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps decode encode tap hold slide touch touchHold repr base)
    (input, f) repr initial state input hinitial
    (frame_call_refines inst integerOps decode integerLaw encode decode_encode
      tap hold slide touch touchHold repr base f input laws)

theorem trace_equiv
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput → Callback) (initializeFrame : GameState → FrameInput → W)
    (laws : ∀ input,
      Contracts inst integerOps decode encode tap hold slide touch touchHold repr input)
    (hinitial : ∀ state input, repr (initializeFrame state input) = initialFrame state input)
    (state : GameState) (inputs : List FrameInput) :
    trace inst integerOps decode encode tap hold slide touch touchHold repr
      base callback initializeFrame state inputs =
      ok (SchedulerFrameStateProof.referenceTrace state inputs) := by
  exact SchedulerFrameStateProof.trace_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps decode encode tap hold slide touch touchHold repr base)
    (fun input => (input, callback input)) repr initializeFrame hinitial
    (fun input => frame_call_refines inst integerOps decode integerLaw encode decode_encode
      tap hold slide touch touchHold repr base (callback input) input (laws input))
    state inputs

end Verification.SchedulerFrameAllTraversalsProof
