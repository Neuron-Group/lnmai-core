import Verification.SchedulerFrame
import Verification.TapSchedulePass

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameTapProof

/-!
Install the actual extracted storage/Tap traversal composition in the frame
driver. The write replaces the Tap event batch and preserves all other frame
fields. Field adapters still delimit the unextracted native FrameWork facade.
There is no whole-Tap-pass equality premise. Lifecycle installation is supplied
separately by TapScheduleJudgments; the theorem here accepts its atomic result.
-/

structure FrameAdapters (W Q F C E Ops : Type) where
  frontiers : W → F
  queues : W → storage.ButtonVec Q
  cursor : W → C
  emptyEvents : E
  remaining : Q → Result Usize
  reverse : E → Result E
  ops : W → areas.ButtonZone → Ops
  write : W → F → storage.ButtonVec Q → E → C → W

def frameContext (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    TapSchedulePassProof.Context :=
  ⟨input, frame.state.currentTime, frame.state.touchPanelOffset, frame.state.judgeStyle⟩

def writeReference (frame : SchedulerFrameProof.FrameWork)
    (out : TapSchedulePassProof.Outcome) : SchedulerFrameProof.FrameWork :=
  { frame with
    state := { frame.state with buttonQueueFrontiers := out.1, tapQueues := out.2.1 }
    events := { frame.events with tap := out.2.2.1 }
    cursor := out.2.2.2 }

theorem reference_tap (input : FrameInput) (frame : SchedulerFrameProof.FrameWork) :
    writeReference frame (TapSchedulePassProof.reference (frameContext input frame)
      frame.state.buttonQueueFrontiers frame.state.tapQueues frame.cursor) =
      SchedulerFrameProof.referencePass input .Tap frame := by
  rfl

structure FrameFieldsAgree {W Q N F C E Event D Area Ops : Type}
    (a : FrameAdapters W Q F C E Ops)
    (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (repr : W → SchedulerFrameProof.FrameWork) : Prop where
  frontiers : ∀ frame, rep.frontiers (a.frontiers frame) = (repr frame).state.buttonQueueFrontiers
  queues : ∀ frame, TapSchedulePassProof.decodeQueues rep (a.queues frame) =
    (repr frame).state.tapQueues
  cursor : ∀ frame, rep.cursor (a.cursor frame) = (repr frame).cursor
  empty_events : rep.events a.emptyEvents = []
  write : ∀ frame frontiers queues events cursor,
    repr (a.write frame frontiers queues events cursor) = writeReference (repr frame)
      (rep.frontiers frontiers, TapSchedulePassProof.decodeQueues rep queues,
        rep.events events, rep.cursor cursor)

def tapPhase {W Q N F C E Event D Area Ops : Type}
    (clone : core.clone.Clone Q)
    (call : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : FrameAdapters W Q F C E Ops) (frame : W) : Result W := do
  let out ← TapSchedulePassProof.process clone (call frame) (a.ops frame)
    a.remaining a.reverse a.emptyEvents (a.frontiers frame) (a.queues frame) (a.cursor frame)
  ok (a.write frame out.1 out.2.1 out.2.2.1 out.2.2.2)

theorem tap_phase_refines {W Q N F C E Event D Area Ops : Type}
    (clone : core.clone.Clone Q)
    (call : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : FrameAdapters W Q F C E Ops)
    (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput) (capacity : Nat)
    (fields : FrameFieldsAgree a rep repr)
    (hclone : ∀ queue, clone.clone queue = ok queue)
    (hremaining : ∀ queue, ∃ fuel, a.remaining queue = ok fuel ∧
      fuel.val = TapScheduleModel.remaining (rep.queue queue))
    (hreverse : ∀ events, ∃ reversed, a.reverse events = ok reversed ∧
      rep.events reversed = (rep.events events).reverse)
    (frame : W)
    (atoms : ∀ zone, TapScheduleProof.AtomicAgrees (call frame) (a.ops frame zone) rep
      (TapSchedulePassProof.zoneContext (frameContext input (repr frame))
        (Areas.toLnmButtonZone zone)) capacity)
    (hfit : ∀ zone queue (acc : F × C × E),
      (TapScheduleModel.resume
        (TapSchedulePassProof.zoneContext (frameContext input (repr frame))
          (Areas.toLnmButtonZone zone)) (TapScheduleModel.remaining (rep.queue queue))
        ⟨rep.queue queue, rep.frontiers acc.1, rep.cursor acc.2.1, rep.events acc.2.2⟩).2.2.2.length
          ≤ capacity) :
    ∃ next, tapPhase clone call a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Tap (repr frame) := by
  obtain ⟨out, hrun, hout⟩ := TapSchedulePassProof.process_equiv clone (call frame)
    (a.ops frame) a.remaining a.reverse a.emptyEvents rep
    (frameContext input (repr frame)) capacity hclone atoms hremaining hreverse
    fields.empty_events hfit (a.frontiers frame) (a.queues frame) (a.cursor frame)
  refine ⟨a.write frame out.1 out.2.1 out.2.2.1 out.2.2.2, ?_, ?_⟩
  · simp only [tapPhase, hrun, bind_tc_ok]
  · rw [fields.write, hout, fields.frontiers, fields.queues, fields.cursor]
    exact reference_tap input (repr frame)

def frameDispatch {W Q N F C E Event D Area Ops Fn : Type}
    (base : core.ops.function.Fn Fn (scheduler.FramePhase × W) W)
    (clone : core.clone.Clone Q)
    (call : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : FrameAdapters W Q F C E Ops) (f : Fn)
    (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .Tap => tapPhase clone call a args.2
  | phase => base.call f (phase, args.2)

def frameCall {W Q N F C E Event D Area Ops Fn : Type}
    (base : core.ops.function.Fn Fn (scheduler.FramePhase × W) W)
    (clone : core.clone.Clone Q)
    (call : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : FrameAdapters W Q F C E Ops) :
    core.ops.function.Fn Fn (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst := { call_once := frameDispatch base clone call a }
        call_mut := fun f args => do
          let out ← frameDispatch base clone call a f args
          ok (out, f) }
    call := frameDispatch base clone call a }

theorem frame_call_tap {W Q N F C E Event D Area Ops Fn : Type}
    (base : core.ops.function.Fn Fn (scheduler.FramePhase × W) W)
    (clone : core.clone.Clone Q)
    (call : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : FrameAdapters W Q F C E Ops) (f : Fn) (frame : W) :
    (frameCall base clone call a).call f (.Tap, frame) = tapPhase clone call a frame := rfl

end Verification.SchedulerFrameTapProof
