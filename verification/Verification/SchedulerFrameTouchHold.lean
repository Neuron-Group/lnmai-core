import Verification.SchedulerFrame
import Verification.TouchHoldScheduleQueueJudgments

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameTouchHoldProof
open SchedulerFrameProof TouchHoldScheduleModel TouchHoldScheduleProof

/-!
Source-only handoff; compilation is reserved for the mainline worker.

Run the actual extracted TouchHold traversal with judgment/lifecycle and queue
maintenance installed. Context, cursor, frontiers and BOTH group collections
come from the current frame, including the updates made by Touch. Initial-frame
body-group reconstruction belongs to initialFrame, not to this phase. All seven
outputs are written back; the forward TouchHold event batch is not reversed.

The contracts below contain only field/copy/encoding, scheduler/container point
laws and capacity bounds. Head/body group operations, the lifecycle step and
replace-all/normalize queue maintenance are supplied by imported kernel proofs.
Safe retains intermediate body-group growth bounds, not just final sizes.
There is no full item/pass equality or uniqueness premise.

Native facade identification, BigInt/usize interpretation, successful copies,
output encoding/allocation and universal point laws remain obligations. This
file does not establish reachability or the concrete Rust FrameWork adapter.
-/

structure FrameAdapters (W S F Stores C H E G B Ops : Type) where
  holds : W → S
  frontiers : W → F
  queues : W → Stores
  cursor : W → C
  groups : W → G
  bodyGroups : W → B
  copyFrontiers : F → Result F
  copyQueues : Stores → Result Stores
  copyCursor : C → Result C
  copyGroups : G → Result G
  copyBodyGroups : B → Result B
  emptyActive : H
  emptyEvents : E
  ops : FrameInput → W → Ops
  write : W → F → Stores → H → E → C → G → B → W

def frameContext (input : FrameInput) (frame : FrameWork) : Context :=
  { input := input, currentTime := frame.state.currentTime, delta := input.delta
    style := frame.state.judgeStyle, touchPanelOffset := frame.state.touchPanelOffset
    buttonRingForTouch := frame.state.buttonRingForTouch }

def initialReference (frame : FrameWork) : Work :=
  ⟨frame.state.touchQueueFrontiers, frame.state.touchHoldQueues, frame.cursor, [], [],
    frame.state.touchGroupStates, frame.state.touchHoldGroupStates⟩

def referenceOutput (input : FrameInput) (frame : FrameWork) : Outcome :=
  reference (frameContext input frame) frame.state.activeTouchHolds
    frame.state.touchQueueFrontiers frame.state.touchHoldQueues frame.cursor
    frame.state.touchGroupStates frame.state.touchHoldGroupStates

def writeReference (frame : FrameWork) (output : Outcome) : FrameWork :=
  let (frontiers, queues, holds, events, cursor, groups, bodyGroups) := output
  { frame with
    state := { frame.state with
      touchQueueFrontiers := frontiers, touchHoldQueues := queues, activeTouchHolds := holds
      touchGroupStates := groups, touchHoldGroupStates := bodyGroups }
    cursor := cursor
    events := { frame.events with touch_hold := events } }

theorem reference_touch_hold (input : FrameInput) (frame : FrameWork) :
    writeReference frame (referenceOutput input frame) =
      referencePass input .TouchHold frame := rfl

/-- Each write law accepts arbitrary supplied fields, independently of a pass. -/
structure FrameFieldsAgree {W S A N F Stores C H E Event D Z Id Grade G B Ops : Type}
    (a : FrameAdapters W S F Stores C H E G B Ops)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (repr : W → FrameWork) : Prop where
  holds : ∀ frame, rep.holds (a.holds frame) = (repr frame).state.activeTouchHolds
  frontiers : ∀ frame, rep.frontiers (a.frontiers frame) =
    (repr frame).state.touchQueueFrontiers
  queues : ∀ frame, rep.queues (a.queues frame) = (repr frame).state.touchHoldQueues
  cursor : ∀ frame, rep.cursor (a.cursor frame) = (repr frame).cursor
  groups : ∀ frame, rep.groups (a.groups frame) = (repr frame).state.touchGroupStates
  body_groups : ∀ frame, rep.bodyGroups (a.bodyGroups frame) =
    (repr frame).state.touchHoldGroupStates
  copy_frontiers : ∀ value, ∃ copied, a.copyFrontiers value = ok copied ∧
    rep.frontiers copied = rep.frontiers value
  copy_queues : ∀ value, ∃ copied, a.copyQueues value = ok copied ∧
    rep.queues copied = rep.queues value
  copy_cursor : ∀ value, ∃ copied, a.copyCursor value = ok copied ∧
    rep.cursor copied = rep.cursor value
  copy_groups : ∀ value, ∃ copied, a.copyGroups value = ok copied ∧
    rep.groups copied = rep.groups value
  copy_body_groups : ∀ value, ∃ copied, a.copyBodyGroups value = ok copied ∧
    rep.bodyGroups copied = rep.bodyGroups value
  empty_active : rep.active a.emptyActive = []
  empty_events : rep.events a.emptyEvents = []
  write : ∀ frame frontiers queues holds events cursor groups bodyGroups,
    repr (a.write frame frontiers queues holds events cursor groups bodyGroups) =
      writeReference (repr frame)
        (rep.frontiers frontiers, rep.queues queues, rep.active holds, rep.events events,
          rep.cursor cursor, rep.groups groups, rep.bodyGroups bodyGroups)

def initialWork {W S F Stores C H E G B Ops : Type}
    (a : FrameAdapters W S F Stores C H E G B Ops) (frame : W) :
    Result (touch_hold_schedule.TouchHoldWork F Stores C H E G B) := do
  let frontiers ← a.copyFrontiers (a.frontiers frame)
  let queues ← a.copyQueues (a.queues frame)
  let cursor ← a.copyCursor (a.cursor frame)
  let groups ← a.copyGroups (a.groups frame)
  let bodyGroups ← a.copyBodyGroups (a.bodyGroups frame)
  ok ⟨frontiers, queues, cursor, a.emptyActive, a.emptyEvents, groups, bodyGroups⟩

theorem initial_work_spec {W S A N F Stores C H E Event D Z Id Grade G B Ops : Type}
    (a : FrameAdapters W S F Stores C H E G B Ops)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (repr : W → FrameWork) (fields : FrameFieldsAgree a rep repr) (frame : W) :
    ∃ initial, initialWork a frame = ok initial ∧
      interpret rep initial = initialReference (repr frame) := by
  obtain ⟨frontiers, hf, hfrontiers⟩ := fields.copy_frontiers (a.frontiers frame)
  obtain ⟨queues, hq, hqueues⟩ := fields.copy_queues (a.queues frame)
  obtain ⟨cursor, hc, hcursor⟩ := fields.copy_cursor (a.cursor frame)
  obtain ⟨groups, hg, hgroups⟩ := fields.copy_groups (a.groups frame)
  obtain ⟨bodyGroups, hb, hbodyGroups⟩ := fields.copy_body_groups (a.bodyGroups frame)
  refine ⟨⟨frontiers, queues, cursor, a.emptyActive, a.emptyEvents, groups, bodyGroups⟩, ?_, ?_⟩
  · simp only [initialWork, hf, hq, hc, hg, hb, bind_tc_ok]
  · simp only [interpret, initialReference, hfrontiers, hqueues, hcursor, hgroups, hbodyGroups,
      fields.frontiers frame, fields.queues frame, fields.cursor frame, fields.groups frame,
      fields.body_groups frame, fields.empty_active, fields.empty_events]

/-- Code and representation adapters; refinement obligations live in Contracts. -/
structure TouchHoldImplementation (W : Type) where
  S : Type
  A : Type
  N : Type
  F : Type
  Stores : Type
  C : Type
  H : Type
  E : Type
  Event : Type
  D : Type
  Z : Type
  Id : Type
  Grade : Type
  G : Type
  B : Type
  Ops : Type
  Q : Type
  schedule : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade
  heads : touch_hold_schedule.HeadGroups Ops G Id Grade D
  bodies : touch_hold_schedule.BodyGroups Ops B Id
  adapters : FrameAdapters W S F Stores C H E G B Ops
  rep : Representation S A N F Stores C H E Event D Z Id Grade G B
  pack : TouchHoldScheduleJudgmentsProof.OutputEncoding N Event
  capacity : Capacity
  queueSlot : QueueMaintenanceComposition.QueueSlot Ops Stores A Q
  queueFacade : QueueMaintenanceComposition.Facade Ops Q
  replaceCall : queue_maintenance.QueueReplace Ops Q N
  cursorCall : queue_maintenance.QueueCursor Ops Q N
  queueRep : Q → ZoneQueue HoldNote
  queueCapacity : Nat

variable {I J W Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (touchHold : TouchHoldImplementation W) (repr : W → FrameWork)

/-- Bounds are required at every intermediate prefix, including bodyBefore. -/
structure Contracts (input : FrameInput) : Prop where
  fields : FrameFieldsAgree touchHold.adapters touchHold.rep repr
  remaining : ∀ frame, TouchHoldScheduleQueueJudgmentsProof.OtherAtoms
    touchHold.schedule (touchHold.adapters.ops input frame) touchHold.rep
    (frameContext input (repr frame)) touchHold.capacity
  encoding : ∀ frame, TouchHoldScheduleJudgmentsProof.EncodingReady
    inst integerOps encode touchHold.rep touchHold.pack (frameContext input (repr frame))
  queues : ∀ frame, TouchHoldScheduleQueueJudgmentsProof.QueuePoints
    touchHold.queueSlot touchHold.queueFacade touchHold.replaceCall touchHold.cursorCall
    (touchHold.adapters.ops input frame) touchHold.rep touchHold.queueRep touchHold.queueCapacity
  headPoints : ∀ frame, TouchHoldGroupsProof.HeadAtoms touchHold.heads
    (touchHold.adapters.ops input frame) (headRep touchHold.rep) touchHold.capacity.heads
  bodyPoints : ∀ frame, TouchHoldGroupsProof.BodyAtoms touchHold.bodies
    (touchHold.adapters.ops input frame) (bodyRep touchHold.rep) touchHold.capacity.bodies
  safe : ∀ frame, Safe (frameContext input (repr frame)) touchHold.capacity
    (repr frame).state.activeTouchHolds (initialReference (repr frame))

def touchHoldPhase (input : FrameInput) (frame : W) : Result W := do
  let initial ← initialWork touchHold.adapters frame
  let out ← touch_hold_schedule.process
    (TouchHoldScheduleQueueJudgmentsProof.installKernels inst integerOps encode
      touchHold.schedule touchHold.rep touchHold.pack (frameContext input (repr frame))
      touchHold.queueSlot touchHold.queueFacade touchHold.replaceCall touchHold.cursorCall)
    touchHold.heads touchHold.bodies (touchHold.adapters.holds frame) initial
    (touchHold.adapters.ops input frame)
  ok (touchHold.adapters.write frame out.frontiers out.queues out.active out.events
    out.cursor out.groups out.body_groups)

include integerLaw decode_encode in
/-- Derive the original TouchHold pass from judgments, group and queue kernels. -/
theorem touch_hold_phase_refines (input : FrameInput)
    (laws : Contracts inst integerOps encode touchHold repr input) (frame : W) :
    ∃ next, touchHoldPhase inst integerOps encode touchHold repr input frame = ok next ∧
      repr next = referencePass input .TouchHold (repr frame) := by
  obtain ⟨initial, hinitial, hinit⟩ :=
    initial_work_spec touchHold.adapters touchHold.rep repr laws.fields frame
  obtain ⟨out, hrun, houtput⟩ := spec_imp_exists
    (TouchHoldScheduleQueueJudgmentsProof.process_equiv
      inst integerOps decode integerLaw encode decode_encode
      touchHold.schedule touchHold.heads touchHold.bodies (touchHold.adapters.ops input frame)
      touchHold.rep touchHold.pack (frameContext input (repr frame)) touchHold.capacity
      touchHold.queueSlot touchHold.queueFacade touchHold.replaceCall touchHold.cursorCall
      touchHold.queueRep touchHold.queueCapacity (laws.encoding frame) (laws.remaining frame)
      (laws.queues frame) (laws.headPoints frame) (laws.bodyPoints frame)
      (touchHold.adapters.holds frame) initial (repr frame).state.touchQueueFrontiers
      (repr frame).state.touchHoldQueues (repr frame).cursor (repr frame).state.touchGroupStates
      (repr frame).state.touchHoldGroupStates hinit
      (by simpa only [laws.fields.holds frame, initialReference] using laws.safe frame))
  refine ⟨touchHold.adapters.write frame out.frontiers out.queues out.active out.events
    out.cursor out.groups out.body_groups, ?_, ?_⟩
  · simp only [touchHoldPhase, hinitial, bind_tc_ok, hrun]
  · calc
      repr (touchHold.adapters.write frame out.frontiers out.queues out.active out.events
          out.cursor out.groups out.body_groups) =
          writeReference (repr frame) (interpret touchHold.rep out).output :=
        laws.fields.write frame out.frontiers out.queues out.active out.events
          out.cursor out.groups out.body_groups
      _ = writeReference (repr frame) (referenceOutput input (repr frame)) :=
        congrArg (writeReference (repr frame)) (by
          simpa only [referenceOutput, laws.fields.holds frame] using houtput)
      _ = referencePass input .TouchHold (repr frame) := reference_touch_hold input (repr frame)

def frameDispatch (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput × Callback) (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .TouchHold => touchHoldPhase inst integerOps encode touchHold repr callback.1 args.2
  | phase => base.call callback.2 (phase, args.2)

def frameCall (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × Callback) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once := frameDispatch inst integerOps encode touchHold repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps encode touchHold repr base callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps encode touchHold repr base }

theorem frame_call_touch_hold
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (input : FrameInput) (f : Callback) (frame : W) :
    (frameCall inst integerOps encode touchHold repr base).call
      (input, f) (.TouchHold, frame) =
      touchHoldPhase inst integerOps encode touchHold repr input frame := rfl

end Verification.SchedulerFrameTouchHoldProof
