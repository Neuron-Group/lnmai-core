import Verification.SchedulerFrameTraversals
import Verification.SchedulerFrameTouchJudgments

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameTouchTraversalsProof
open SchedulerFrameProof

/-! Source-only handoff; no compiler, tests or extraction were run.

Use the existing Tap/Hold/Slide dispatcher with the proved Touch dispatcher as
its delegate. One external (input, callback) supplies BOTH nested callbacks
with the same input. Every phase still reads the actual current accumulator.
No traversal or phase case proof is duplicated: frame_call_refines below is
the existing SchedulerFrameTraversalsProof.frame_call_refines instantiated
with the derived Touch theorem. Only hTouchHold is a complete-pass premise.

Point/field/encoding/integer/capacity and initialization obligations remain
explicit and universal. The native facade and allocation boundaries of the
imported proofs remain; no reachable-state invariant is inferred here.

API snapshots (2026-10-17):
  SchedulerFrameTraversals.lean
    bf6b6c8bedcbc884b2819635033e48526c2f6af2bc2d988dbc1dfe7c6e9857ef
  SchedulerFrameState.lean
    a299ed376142799337c377857ba4861fa254a9334a205445a0ea029bf67b84b4
-/

variable {I J W Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (tap : SchedulerFrameTraversalsProof.TapImplementation W)
  (hold : SchedulerFrameTraversalsProof.HoldImplementation W)
  (slide : SchedulerFrameTraversalsProof.SlideImplementation W)
  (touch : SchedulerFrameTouchJudgmentsProof.TouchImplementation W)
  (repr : W → FrameWork)

structure Contracts (input : FrameInput) : Prop where
  traversals : SchedulerFrameTraversalsProof.Contracts
    inst integerOps decode encode tap hold slide repr input
  touch : SchedulerFrameTouchJudgmentsProof.Contracts
    inst integerOps decode encode touch repr input

/-- Adapt the callback only. The extracted frame driver keeps phase order;
the existing dispatcher routes Touch/TouchHold to its delegate. -/
def frameDispatch (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput × Callback) (args : scheduler.FramePhase × W) : Result W :=
  (SchedulerFrameTraversalsProof.frameCall inst integerOps decode encode tap hold slide repr
    (SchedulerFrameTouchJudgmentsProof.frameCall inst integerOps decode encode touch repr
      base)).call (callback.1, callback) args

def frameCall (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × Callback) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once :=
              frameDispatch inst integerOps decode encode tap hold slide touch repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps decode encode tap hold slide touch repr
            base callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps decode encode tap hold slide touch repr base }

def processAndFinish (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput) :
    Result SchedulerFrameStateProof.FrameOutput :=
  SchedulerFrameStateProof.processAndFinishFromInitial inst integerOps encode
    (frameCall inst integerOps decode encode tap hold slide touch repr base)
    (input, f) repr initial state input

/-- The imported trace driver reinitializes from the preceding output state
and pairs each new input with its own callback. It retains every full output. -/
def trace (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput → Callback) (initializeFrame : GameState → FrameInput → W)
    (state : GameState) (inputs : List FrameInput) :
    Result (GameState × List SchedulerFrameStateProof.FrameOutput) :=
  SchedulerFrameStateProof.traceFromInitial inst integerOps encode
    (frameCall inst integerOps decode encode tap hold slide touch repr base)
    (fun input => (input, callback input)) repr initializeFrame state inputs

include integerLaw decode_encode

theorem frame_call_refines
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (input : FrameInput)
    (laws : Contracts inst integerOps decode encode tap hold slide touch repr input)
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    ∀ phase frame, ∃ next,
      (frameCall inst integerOps decode encode tap hold slide touch repr base).call
        (input, f) (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame) := by
  exact SchedulerFrameTraversalsProof.frame_call_refines
    inst integerOps decode integerLaw encode decode_encode tap hold slide repr
    (SchedulerFrameTouchJudgmentsProof.frameCall inst integerOps decode encode touch repr base)
    (input, f) input laws.traversals
    (fun frame => SchedulerFrameTouchJudgmentsProof.touch_phase_refines
      inst integerOps decode integerLaw encode decode_encode touch repr input laws.touch frame)
    hTouchHold

theorem process_and_finish_equiv
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput)
    (laws : Contracts inst integerOps decode encode tap hold slide touch repr input)
    (hinitial : repr initial = initialFrame state input)
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    processAndFinish inst integerOps decode encode tap hold slide touch repr base f initial
      state input = ok (Scheduler.stepFrame state input) := by
  exact SchedulerFrameStateProof.process_and_finish_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps decode encode tap hold slide touch repr base)
    (input, f) repr initial state input hinitial
    (frame_call_refines inst integerOps decode integerLaw encode decode_encode
      tap hold slide touch repr base f input laws hTouchHold)

theorem trace_equiv
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput → Callback) (initializeFrame : GameState → FrameInput → W)
    (laws : ∀ input, Contracts inst integerOps decode encode tap hold slide touch repr input)
    (hinitial : ∀ state input, repr (initializeFrame state input) = initialFrame state input)
    (hTouchHold : ∀ input frame,
      ∃ next, base.call (callback input) (.TouchHold, frame) = ok next ∧
        repr next = referencePass input .TouchHold (repr frame))
    (state : GameState) (inputs : List FrameInput) :
    trace inst integerOps decode encode tap hold slide touch repr base callback initializeFrame
      state inputs = ok (SchedulerFrameStateProof.referenceTrace state inputs) := by
  exact SchedulerFrameStateProof.trace_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps decode encode tap hold slide touch repr base)
    (fun input => (input, callback input)) repr initializeFrame hinitial
    (fun input => frame_call_refines inst integerOps decode integerLaw encode decode_encode
      tap hold slide touch repr base (callback input) input (laws input) (hTouchHold input))
    state inputs

end Verification.SchedulerFrameTouchTraversalsProof
