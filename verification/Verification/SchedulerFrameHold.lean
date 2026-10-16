import Verification.SchedulerFrame
import Verification.HoldScheduleJudgments

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameHoldProof
open SchedulerFrameProof HoldScheduleModel HoldScheduleProof

/-!
Thin Hold-phase composition over the actual extracted hold_schedule.process.
The immutable holds source is read from the current frame; frontiers, queues
and cursor are copied, and active/events start empty. The five outputs replace
exactly the fields assigned by Rust process_frame_phase.Hold. Output order is
inherited from the proved traversal and its ordinary-Hold lifecycle composition.

installStep is built at EACH Hold invocation using that invocation's current
frame and input, including the cursor/frontiers after Tap. No initial context
is captured by frameCall. No whole Hold pass or lifecycle result is assumed.

Rust FrameWork/HoldScheduleFrame and their field/time/clone/allocation behavior
remain unextracted. Explicit contracts retain those boundaries, IntegerAgrees,
exact encoding, capacity, the non-step atoms and composite update_queue pending
its separate proof. Universal frame/copy contracts are not reachability proofs.
The final corollary uses reference finishFrame; it does not establish native
initialization or extracted score/command/final-record composition.
-/

structure FrameAdapters (W S F Q C H E Ops : Type) where
  holds : W → S
  frontiers : W → F
  queues : W → Q
  cursor : W → C
  copyFrontiers : F → Result F
  copyQueues : Q → Result Q
  copyCursor : C → Result C
  emptyActive : H
  emptyEvents : E
  ops : FrameInput → W → Ops
  write : W → F → Q → H → E → C → W

def frameContext (input : FrameInput) (frame : FrameWork) : Context :=
  { input := input, currentTime := frame.state.currentTime, delta := input.delta
    style := frame.state.judgeStyle, touchPanelOffset := frame.state.touchPanelOffset
    prevSensor := frame.state.prevSensor }

def initialReference (frame : FrameWork) : Work :=
  ⟨frame.state.buttonQueueFrontiers, frame.state.holdQueues, frame.cursor, [], []⟩

def referenceOutput (input : FrameInput) (frame : FrameWork) : Outcome :=
  reference (frameContext input frame) frame.state.activeHolds
    frame.state.buttonQueueFrontiers frame.state.holdQueues frame.cursor

def writeReference (frame : FrameWork) (output : Outcome) : FrameWork :=
  let (frontiers, queues, holds, events, cursor) := output
  { frame with
    state := { frame.state with
      buttonQueueFrontiers := frontiers, holdQueues := queues, activeHolds := holds }
    cursor := cursor
    events := { frame.events with hold := events } }

theorem reference_hold (input : FrameInput) (frame : FrameWork) :
    writeReference frame (referenceOutput input frame) = referencePass input .Hold frame := rfl

/-- Laws about individual fields/copies, with arbitrary supplied output fields.
No law refers to a traversal, referencePass, or lifecycle result. -/
structure FrameFieldsAgree {W S Z N F Q C H E Event D Area Ops : Type}
    (a : FrameAdapters W S F Q C H E Ops)
    (rep : Representation S Z N F Q C H E Event D Area) (repr : W → FrameWork) : Prop where
  holds : ∀ frame, rep.holds (a.holds frame) = (repr frame).state.activeHolds
  frontiers : ∀ frame, rep.frontiers (a.frontiers frame) = (repr frame).state.buttonQueueFrontiers
  queues : ∀ frame, rep.queues (a.queues frame) = (repr frame).state.holdQueues
  cursor : ∀ frame, rep.cursor (a.cursor frame) = (repr frame).cursor
  copy_frontiers : ∀ value, ∃ copied, a.copyFrontiers value = ok copied ∧
    rep.frontiers copied = rep.frontiers value
  copy_queues : ∀ value, ∃ copied, a.copyQueues value = ok copied ∧
    rep.queues copied = rep.queues value
  copy_cursor : ∀ value, ∃ copied, a.copyCursor value = ok copied ∧
    rep.cursor copied = rep.cursor value
  empty_active : rep.active a.emptyActive = []
  empty_events : rep.events a.emptyEvents = []
  write : ∀ frame frontiers queues holds events cursor,
    repr (a.write frame frontiers queues holds events cursor) =
      writeReference (repr frame)
        (rep.frontiers frontiers, rep.queues queues, rep.active holds,
          rep.events events, rep.cursor cursor)

def initialWork {W S F Q C H E Ops : Type}
    (a : FrameAdapters W S F Q C H E Ops) (frame : W) :
    Result (hold_schedule.HoldWork F Q C H E) := do
  let frontiers ← a.copyFrontiers (a.frontiers frame)
  let queues ← a.copyQueues (a.queues frame)
  let cursor ← a.copyCursor (a.cursor frame)
  ok ⟨frontiers, queues, cursor, a.emptyActive, a.emptyEvents⟩

theorem initial_work_spec {W S Z N F Q C H E Event D Area Ops : Type}
    (a : FrameAdapters W S F Q C H E Ops)
    (rep : Representation S Z N F Q C H E Event D Area) (repr : W → FrameWork)
    (fields : FrameFieldsAgree a rep repr) (frame : W) :
    ∃ initial, initialWork a frame = ok initial ∧
      interpret rep initial = initialReference (repr frame) := by
  obtain ⟨frontiers, hf, hfrontiers⟩ := fields.copy_frontiers (a.frontiers frame)
  obtain ⟨queues, hq, hqueues⟩ := fields.copy_queues (a.queues frame)
  obtain ⟨cursor, hc, hcursor⟩ := fields.copy_cursor (a.cursor frame)
  refine ⟨⟨frontiers, queues, cursor, a.emptyActive, a.emptyEvents⟩, ?_, ?_⟩
  · simp only [initialWork, hf, hq, hc, bind_tc_ok]
  · simp only [interpret, initialReference, hfrontiers, hqueues, hcursor,
      fields.frontiers frame, fields.queues frame, fields.cursor frame,
      fields.empty_active, fields.empty_events]

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
variable {W S Z N F Q C H E Event D Area Ops Callback : Type}
  (schedule : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area)
  (a : FrameAdapters W S F Q C H E Ops)
  (rep : Representation S Z N F Q C H E Event D Area)
  (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event) (repr : W → FrameWork)

/-- Current context is installed at the call, never at frame initialization. -/
def holdPhase (input : FrameInput) (frame : W) : Result W := do
  let initial ← initialWork a frame
  let out ← hold_schedule.process
    (HoldScheduleJudgmentsProof.installStep inst integerOps encode schedule rep pack
      (frameContext input (repr frame))) (a.holds frame) initial (a.ops input frame)
  ok (a.write frame out.frontiers out.queues out.active out.events out.cursor)

def frameDispatch (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput × Callback) (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .Hold => holdPhase inst integerOps encode schedule a rep pack repr callback.1 args.2
  | phase => base.call callback.2 (phase, args.2)

def frameCall (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × Callback) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once := frameDispatch inst integerOps encode schedule a rep pack repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps encode schedule a rep pack repr base
            callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps encode schedule a rep pack repr base }

theorem frame_call_hold (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (input : FrameInput) (f : Callback) (frame : W) :
    (frameCall inst integerOps encode schedule a rep pack repr base).call
      (input, f) (.Hold, frame) =
      holdPhase inst integerOps encode schedule a rep pack repr input frame := rfl

/-- No step/pass equation: only point/field/encoding/capacity obligations. -/
structure HoldContracts (cap : Capacity) (input : FrameInput) : Prop where
  fields : FrameFieldsAgree a rep repr
  remaining : ∀ frame,
    HoldScheduleJudgmentsProof.RemainingAtoms schedule (a.ops input frame) rep
      (frameContext input (repr frame)) cap
  encoding : ∀ frame,
    HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack
      (frameContext input (repr frame))
  fits : ∀ frame, Fits cap
    (resume (frameContext input (repr frame)) (repr frame).state.activeHolds
      (initialReference (repr frame)))

include integerLaw decode_encode

theorem hold_phase_refines (input : FrameInput) (cap : Capacity)
    (laws : HoldContracts inst integerOps encode schedule a rep pack repr cap input)
    (frame : W) :
    ∃ next, holdPhase inst integerOps encode schedule a rep pack repr input frame = ok next ∧
      repr next = referencePass input .Hold (repr frame) := by
  obtain ⟨initial, hinitial, hinit⟩ := initial_work_spec a rep repr laws.fields frame
  obtain ⟨out, hrun, houtput⟩ := spec_imp_exists
    (HoldScheduleJudgmentsProof.process_equiv inst integerOps decode integerLaw encode decode_encode
      schedule (a.ops input frame) rep pack (frameContext input (repr frame)) cap
      (laws.encoding frame) (laws.remaining frame) (a.holds frame) initial
      (repr frame).state.buttonQueueFrontiers (repr frame).state.holdQueues (repr frame).cursor
      hinit (by simpa only [laws.fields.holds frame, initialReference] using laws.fits frame))
  refine ⟨a.write frame out.frontiers out.queues out.active out.events out.cursor, ?_, ?_⟩
  · simp only [holdPhase, hinitial, bind_tc_ok, hrun]
  · calc
      repr (a.write frame out.frontiers out.queues out.active out.events out.cursor) =
          writeReference (repr frame) (interpret rep out).output :=
        laws.fields.write frame out.frontiers out.queues out.active out.events out.cursor
      _ = writeReference (repr frame) (referenceOutput input (repr frame)) :=
        congrArg (writeReference (repr frame)) (by
          simpa only [referenceOutput, laws.fields.holds frame] using houtput)
      _ = referencePass input .Hold (repr frame) := reference_hold input (repr frame)

/-- Four other phase premises; Hold is derived at the actual post-Tap frame. -/
theorem frame_call_refines
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (input : FrameInput) (cap : Capacity)
    (laws : HoldContracts inst integerOps encode schedule a rep pack repr cap input)
    (hTap : ∀ frame, ∃ next, base.call f (.Tap, frame) = ok next ∧
      repr next = referencePass input .Tap (repr frame))
    (hSlide : ∀ frame, ∃ next, base.call f (.Slide, frame) = ok next ∧
      repr next = referencePass input .Slide (repr frame))
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    ∀ phase frame, ∃ next,
      (frameCall inst integerOps encode schedule a rep pack repr base).call
        (input, f) (phase, frame) = ok next ∧ repr next = referencePass input phase (repr frame) := by
  intro phase frame
  cases phase with
  | Tap => exact hTap frame
  | Hold =>
      exact hold_phase_refines inst integerOps decode integerLaw encode decode_encode
        schedule a rep pack repr input cap laws frame
  | Slide => exact hSlide frame
  | Touch => exact hTouch frame
  | TouchHold => exact hTouchHold frame

theorem process_frame_to_stepFrame
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput) (cap : Capacity)
    (laws : HoldContracts inst integerOps encode schedule a rep pack repr cap input)
    (hinitial : repr initial = initialFrame state input)
    (hTap : ∀ frame, ∃ next, base.call f (.Tap, frame) = ok next ∧
      repr next = referencePass input .Tap (repr frame))
    (hSlide : ∀ frame, ∃ next, base.call f (.Slide, frame) = ok next ∧
      repr next = referencePass input .Slide (repr frame))
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    ∃ out, scheduler.process_frame
      (frameCall inst integerOps encode schedule a rep pack repr base) initial (input, f) = ok out ∧
      finishFrame input (repr out) = Scheduler.stepFrame state input := by
  exact SchedulerFrameProof.process_frame_to_stepFrame
    (frameCall inst integerOps encode schedule a rep pack repr base) (input, f)
    repr initial state input hinitial
    (frame_call_refines inst integerOps decode integerLaw encode decode_encode
      schedule a rep pack repr base f input cap laws hTap hSlide hTouch hTouchHold)

end Verification.SchedulerFrameHoldProof
