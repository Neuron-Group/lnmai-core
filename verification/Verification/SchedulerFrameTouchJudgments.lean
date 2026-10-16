import Verification.SchedulerFrameTouch
import Verification.TouchSchedulePassJudgments

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameTouchJudgmentsProof
open SchedulerFrameProof

/-! Source-only handoff; no compiler, tests or extraction were run.

Install the head/automatic judgment and group composition at the CURRENT
frame. The base implementation may depend on that frame, including its
dependent representation types. Only unchanged field adapters are transported.
The automatic closure still receives the actual post-head groups.

Contracts retain field/point/encoding laws, bounded group operations and
GroupRoom, and the pass Bounds. They contain no head group/lifecycle,
automatic shared/step, or complete Touch-pass refinement assumption. Integer
operations, BigInt/usize interpretation, allocation and the concrete native
facade remain boundaries; this does not identify that facade with frameCall.

API snapshots (2026-10-17):
  SchedulerFrameTouch.lean
    694335c72ecf380a1a225a096a1f6a59b77f2c525066dfdae9e434a23ed4c63f
  TouchSchedulePassJudgments.lean
    311cf251bb41bd4485e72e56632a9d8d425699f32df569c2a7a767d2516d0a69
-/

/-- Code and adapters only; all obligations are in Contracts below. -/
structure TouchImplementation (W : Type) where
  base : W → TouchSchedulePassProof.Implementation
  adapters : SchedulerFrameTouchProof.FrameAdapters W base
  pack : (frame : W) → TouchScheduleJudgmentsProof.OutputEncoding
    (base frame).N (base frame).Event
  headCall : (frame : W) → areas.SensorArea →
    touch_hold_schedule.HeadGroups (base frame).HOps (base frame).G
      (base frame).Id (base frame).Grade (base frame).D
  groupCapacity : Nat

variable {I J W Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (touch : TouchImplementation W) (repr : W → FrameWork)

/-- Both kernels use the same installation and this invocation's context. -/
def install (input : FrameInput) (frame : W) : TouchSchedulePassProof.Implementation :=
  TouchSchedulePassJudgmentsProof.install inst integerOps decode encode (touch.base frame)
    (SchedulerFrameTouchProof.frameContext input (repr frame))
    (touch.pack frame) (touch.headCall frame)

/-- Implementation's dependent types and representations are unchanged by
installation; transfer the five field operations individually. -/
def installedAdapters (input : FrameInput) :
    SchedulerFrameTouchProof.FrameAdapters W
      (install inst integerOps decode encode touch repr input) :=
  { frontiers := touch.adapters.frontiers
    queues := touch.adapters.queues
    cursor := touch.adapters.cursor
    groups := touch.adapters.groups
    write := touch.adapters.write }

theorem fields_installed (input : FrameInput)
    (fields : SchedulerFrameTouchProof.FrameFieldsAgree touch.base touch.adapters repr) :
    SchedulerFrameTouchProof.FrameFieldsAgree
      (install inst integerOps decode encode touch repr input)
      (installedAdapters inst integerOps decode encode touch repr input) repr := by
  exact { frontiers := fields.frontiers
          queues := fields.queues
          cursor := fields.cursor
          groups := fields.groups
          write := fields.write }

/-- Uniform contracts over represented frames, not a reachability invariant.
GroupRoom is the bounded missing-id append obligation inherited from the pass. -/
structure Contracts (input : FrameInput) : Prop where
  fields : SchedulerFrameTouchProof.FrameFieldsAgree touch.base touch.adapters repr
  remaining : ∀ frame, TouchSchedulePassJudgmentsProof.RemainingAtoms (touch.base frame)
    (SchedulerFrameTouchProof.frameContext input (repr frame))
  encoding : ∀ frame, TouchSchedulePassJudgmentsProof.EncodingReady
    inst integerOps decode encode (touch.base frame)
    (SchedulerFrameTouchProof.frameContext input (repr frame)) (touch.pack frame)
  groupPoints : ∀ frame area, TouchScheduleJudgmentsProof.GroupAtoms
    (touch.headCall frame area) ((touch.base frame).headOps area)
    (touch.base frame).rep touch.groupCapacity
  groupRoom : ∀ frame, TouchScheduleJudgmentsProof.GroupRoom
    (touch.base frame).rep touch.groupCapacity
  bounds : ∀ frame, TouchSchedulePassProof.Bounds (touch.base frame)
    (SchedulerFrameTouchProof.frameContext input (repr frame))

def touchPhase (input : FrameInput) (frame : W) : Result W :=
  SchedulerFrameTouchProof.touchPhase
    (install inst integerOps decode encode touch repr input)
    (installedAdapters inst integerOps decode encode touch repr input) frame

include integerLaw decode_encode in
/-- Derive Agrees from point/judgment/group contracts, then reuse the existing
frame theorem. Its write carries queues, frontiers, cursor, groups and the
once-reversed Touch event batch together into the following TouchHold phase. -/
theorem touch_phase_refines (input : FrameInput)
    (laws : Contracts inst integerOps decode encode touch repr input) (frame : W) :
    ∃ next, touchPhase inst integerOps decode encode touch repr input frame = ok next ∧
      repr next = referencePass input .Touch (repr frame) := by
  exact SchedulerFrameTouchProof.touch_phase_refines
    (install inst integerOps decode encode touch repr input)
    (installedAdapters inst integerOps decode encode touch repr input) repr input
    (fields_installed inst integerOps decode encode touch repr input laws.fields) frame
    (TouchSchedulePassJudgmentsProof.agrees_of_judgments_groups
      inst integerOps decode integerLaw encode decode_encode (touch.base frame)
      (SchedulerFrameTouchProof.frameContext input (repr frame))
      (touch.pack frame) (touch.headCall frame) (laws.encoding frame) (laws.remaining frame)
      touch.groupCapacity (laws.groupPoints frame) (laws.groupRoom frame))
    (TouchSchedulePassJudgmentsProof.bounds_installed inst integerOps decode encode
      (touch.base frame) (SchedulerFrameTouchProof.frameContext input (repr frame))
      (touch.pack frame) (touch.headCall frame) (laws.bounds frame))

/-- Reuse the existing Touch dispatcher, supplying the fresh input and current
accumulator instead of capturing a frame context in the callback. -/
def frameDispatch (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput × Callback) (args : scheduler.FramePhase × W) : Result W :=
  SchedulerFrameTouchProof.frameDispatch base
    (install inst integerOps decode encode touch repr callback.1)
    (installedAdapters inst integerOps decode encode touch repr callback.1) callback.2 args

def frameCall (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × Callback) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once := frameDispatch inst integerOps decode encode touch repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps decode encode touch repr base callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps decode encode touch repr base }

theorem frame_call_touch
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (input : FrameInput) (f : Callback) (frame : W) :
    (frameCall inst integerOps decode encode touch repr base).call (input, f) (.Touch, frame) =
      touchPhase inst integerOps decode encode touch repr input frame := rfl

theorem frame_call_touch_hold
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (input : FrameInput) (f : Callback) (frame : W) :
    (frameCall inst integerOps decode encode touch repr base).call
      (input, f) (.TouchHold, frame) = base.call f (.TouchHold, frame) := rfl

end Verification.SchedulerFrameTouchJudgmentsProof
