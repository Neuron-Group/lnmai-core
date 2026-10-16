import Verification.SchedulerFrame
import Verification.TouchSchedulePass

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameTouchProof

/-! Install the extracted head/automatic/sensor traversal in the Touch frame
phase. All outputs are written together, so TouchHold receives the consumed
cursor, updated shared groups, queues and frontiers from this phase.
The concrete native field adapters and primitive contracts remain explicit. -/

structure FrameAdapters (W : Type) (impl : W → TouchSchedulePassProof.Implementation) where
  frontiers : (frame : W) → (impl frame).F
  queues : (frame : W) → storage.SensorVec (impl frame).Q
  cursor : (frame : W) → (impl frame).C
  groups : (frame : W) → (impl frame).G
  write : (frame : W) → (impl frame).F → storage.SensorVec (impl frame).Q →
    (impl frame).E → (impl frame).C → (impl frame).G → W

def frameContext (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    TouchSchedulePassProof.Context :=
  ⟨input, frame.state.currentTime, frame.state.judgeStyle,
    frame.state.touchPanelOffset, frame.state.buttonRingForTouch⟩

def writeReference (frame : SchedulerFrameProof.FrameWork)
    (out : TouchSchedulePassProof.Outcome) : SchedulerFrameProof.FrameWork :=
  { frame with
    state := { frame.state with touchQueueFrontiers := out.1, touchQueues := out.2.1,
      touchGroupStates := out.2.2.2.2 }
    events := { frame.events with touch := out.2.2.1 }
    cursor := out.2.2.2.1 }

theorem reference_touch (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    writeReference frame (TouchSchedulePassProof.reference (frameContext input frame)
      frame.state.touchQueueFrontiers frame.state.touchQueues frame.cursor
      frame.state.touchGroupStates) =
      SchedulerFrameProof.referencePass input .Touch frame := by
  rfl

structure FrameFieldsAgree {W : Type} (impl : W → TouchSchedulePassProof.Implementation)
    (a : FrameAdapters W impl) (repr : W → SchedulerFrameProof.FrameWork) : Prop where
  frontiers : ∀ frame, (impl frame).rep.frontiers (a.frontiers frame) =
    (repr frame).state.touchQueueFrontiers
  queues : ∀ frame, TouchSchedulePassProof.decodeQueues (impl frame) (a.queues frame) =
    (repr frame).state.touchQueues
  cursor : ∀ frame, (impl frame).rep.cursor (a.cursor frame) = (repr frame).cursor
  groups : ∀ frame, (impl frame).rep.groups (a.groups frame) =
    (repr frame).state.touchGroupStates
  write : ∀ frame frontiers queues events cursor groups,
    repr (a.write frame frontiers queues events cursor groups) = writeReference (repr frame)
      ((impl frame).rep.frontiers frontiers, TouchSchedulePassProof.decodeQueues (impl frame) queues,
        (impl frame).rep.events events, (impl frame).rep.cursor cursor, (impl frame).rep.groups groups)

def touchPhase {W : Type} (impl : W → TouchSchedulePassProof.Implementation)
    (a : FrameAdapters W impl) (frame : W) : Result W := do
  let out ← TouchSchedulePassProof.process (impl frame) (a.frontiers frame)
    (a.queues frame) (a.cursor frame) (a.groups frame)
  ok (a.write frame out.1 out.2.1 out.2.2.1 out.2.2.2.1 out.2.2.2.2)

theorem touch_phase_refines {W : Type} (impl : W → TouchSchedulePassProof.Implementation)
    (a : FrameAdapters W impl) (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput)
    (fields : FrameFieldsAgree impl a repr) (frame : W)
    (laws : TouchSchedulePassProof.Agrees (impl frame) (frameContext input (repr frame)))
    (bounds : TouchSchedulePassProof.Bounds (impl frame) (frameContext input (repr frame))) :
    ∃ next, touchPhase impl a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Touch (repr frame) := by
  obtain ⟨out, hrun, hout⟩ := TouchSchedulePassProof.process_equiv (impl frame)
    (frameContext input (repr frame)) laws bounds (a.frontiers frame) (a.queues frame)
    (a.cursor frame) (a.groups frame)
  refine ⟨a.write frame out.1 out.2.1 out.2.2.1 out.2.2.2.1 out.2.2.2.2, ?_, ?_⟩
  · simp only [touchPhase, hrun, bind_tc_ok]
  · rw [fields.write, hout, fields.frontiers, fields.queues, fields.cursor, fields.groups]
    exact reference_touch input (repr frame)

def frameDispatch {W Callback : Type}
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (impl : W → TouchSchedulePassProof.Implementation) (a : FrameAdapters W impl)
    (callback : Callback) (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .Touch => touchPhase impl a args.2
  | phase => base.call callback (phase, args.2)

def frameCall {W Callback : Type}
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (impl : W → TouchSchedulePassProof.Implementation) (a : FrameAdapters W impl) :
    core.ops.function.Fn Callback (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst := { call_once := frameDispatch base impl a }
        call_mut := fun callback args => do
          let out ← frameDispatch base impl a callback args
          ok (out, callback) }
    call := frameDispatch base impl a }

theorem frame_call_touch {W Callback : Type}
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (impl : W → TouchSchedulePassProof.Implementation) (a : FrameAdapters W impl)
    (callback : Callback) (frame : W) :
    (frameCall base impl a).call callback (.Touch, frame) = touchPhase impl a frame := rfl

end Verification.SchedulerFrameTouchProof
