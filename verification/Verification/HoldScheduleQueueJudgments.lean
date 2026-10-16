import Verification.QueueMaintenanceComposition
import Verification.SchedulerFrameHold

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.HoldScheduleQueueJudgmentsProof
open HoldScheduleModel HoldScheduleProof QueueMaintenanceComposition
open private buttonQueueIndexUnlocked holdHeadEligibleForClick
  fallbackSensorAreaForButtonNote fallbackSensorHeldForButtonNote
  fallbackPrevSensorHeldForButtonNote advanceSharedButtonQueue holdQueueResolved
  from LnmaiCore.Scheduler

/-!
Compose the existing judgment/lifecycle and queue-maintenance proofs with the
actual extracted Hold traversal. OtherAtoms has only the eighteen point laws
outside step/update_queue. Neither a complete update, lifecycle nor pass result
is a premise. Both replaced methods execute their installed compositions;
the arbitrary base.step and base.update_queue are never used.

Queue wiring follows scheduler.rs: read one button queue, replace ALL matching
IDs with the extracted replace_all, normalize with normalize_cursor_fuel using
remaining (without +1), then store the queue. QueueMaintenanceComposition also
accounts for both clones, usize conversion and saturating subtraction. Its
proof covers duplicate IDs and arbitrary Nat cursors/IDs, preserving slot
order and length; normalization may advance the cursor.

The queue representation shares rep.note definitionally with the traversal.
QueuePoints contains only slot/facade/replace/cursor point laws and allFit;
allFit is universal because the existing update_queue contract is unguarded.
It must not be inferred from reachability or from the final active/event bound.
Capacity does not by itself establish clone/allocation success.

This proves installed extracted-kernel compositions, not the unextracted Rust
HoldScheduleFrame/hold_step or queue facades. Native fields, times/events,
IntegerAgrees/exact encoding, allocations and the eighteen other point laws
remain explicit. Queue adapters use the scheduler Ops type required by
withQueueMaintenance; connecting the stateless Rust HoldQueueMaintenance to
those adapters is still a concrete facade-wiring obligation.

The frame constructor reuses SchedulerFrameHold, installing judgment context
at each CURRENT Hold frame. It can supply that module's HoldContracts to the
mainline frame composition; no frame traversal or finalization is duplicated.
-/

/-- The original point laws with BOTH composite fields omitted. -/
structure OtherAtoms {S Z N F Q C H E Event D Area Ops : Type}
    (call : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Q C H E Event D Area)
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ holds, ∃ len, call.length ops holds = ok len ∧
    (rep.holds holds).length = len.val
  get : ∀ (holds : S) (index : Usize) (pre : List (ButtonZone × HoldNote))
    (zone : ButtonZone) (note : HoldNote) (rest : List (ButtonZone × HoldNote)),
    rep.holds holds = pre ++ (zone, note) :: rest → pre.length = index.val →
    ∃ nativeZone nativeNote, call.get ops holds index = ok (nativeZone, nativeNote) ∧
      rep.zone nativeZone = zone ∧ rep.note nativeNote = note
  button_diff : ∀ note, ∃ diff, call.button_diff ops note = ok diff ∧
    rep.duration diff = ctx.currentTime - (rep.note note).params.effectiveTiming
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = (ctx.currentTime - ctx.touchPanelOffset) -
      (rep.note note).params.effectiveTiming
  button_pressed : ∀ zone, call.button_pressed ops zone =
    ok (ctx.input.getButtonHeld (rep.zone zone))
  sensor_pressed : ∀ zone, call.sensor_pressed ops zone =
    ok (fallbackSensorHeldForButtonNote ctx.input (rep.zone zone))
  prev_sensor_pressed : ∀ zone, call.prev_sensor_pressed ops zone =
    ok (fallbackPrevSensorHeldForButtonNote ctx.prevSensor (rep.zone zone))
  phase : ∀ note, call.phase ops note = ok (HoldScheduleProof.phase (rep.note note))
  unlocked : ∀ frontiers zone note, call.unlocked ops frontiers zone note =
    ok (buttonQueueIndexUnlocked (rep.frontiers frontiers) (rep.zone zone)
      (rep.note note).buttonQueueIndex)
  eligible : ∀ note, call.eligible ops note =
    ok (holdHeadEligibleForClick (rep.note note) ctx.currentTime)
  fallback_area : ∀ zone, ∃ area, call.fallback_area ops zone = ok area ∧
    rep.area area = fallbackSensorAreaForButtonNote (rep.zone zone)
  button_available : ∀ cursor zone, call.button_available ops cursor zone =
    ok (decide ((rep.cursor cursor).buttonUsed.getD (rep.zone zone) 0 <
      ctx.input.getButtonClickCount (rep.zone zone)))
  sensor_available : ∀ cursor area, call.sensor_available ops cursor area =
    ok (decide ((rep.cursor cursor).sensorUsed.getD (rep.area area) 0 <
      ctx.input.getSensorClickCount (rep.area area)))
  use_button : ∀ cursor zone, ∃ next, call.use_button ops cursor zone = ok next ∧
    rep.cursor next = spendButton (rep.cursor cursor) (rep.zone zone)
  use_sensor : ∀ cursor area, ∃ next, call.use_sensor ops cursor area = ok next ∧
    rep.cursor next = spendSensor (rep.cursor cursor) (rep.area area)
  advance_frontier : ∀ frontiers zone, ∃ next,
    call.advance_frontier ops frontiers zone = ok next ∧
    rep.frontiers next = advanceSharedButtonQueue (rep.frontiers frontiers) (rep.zone zone)
  append_active : ∀ active zone note,
    (rep.active active).length + 1 ≤ cap.active →
    ∃ next, call.append_active ops active zone note = ok next ∧
      rep.active next = rep.active active ++ [(rep.zone zone, rep.note note)]
  append_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.append_event ops events event = ok next ∧
      rep.events next = rep.events events ++ [rep.event event]

/-- One shared note interpretation; no separate equality of note decoders. -/
def queueRepresentation {S Z N F Stores C H E Event D Area Q : Type}
    (rep : Representation S Z N F Stores C H E Event D Area)
    (queueRep : Q → ZoneQueue HoldNote) : QueueMaintenanceProof.Representation Q N HoldNote :=
  { queue := queueRep, note := rep.note }

/-- Only point contracts. In particular no field has the type of update_queue
or its complete reference result. The selected-queue capacity is explicit. -/
structure QueuePoints {S Z N F Stores C H E Event D Area Ops Q : Type}
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat) : Prop where
  slots : SlotAtomicAgrees slot ops (queueRepresentation rep queueRep) rep.queues rep.zone
    InputModel.buttonQueueAt InputModel.setButtonQueueAt queueCapacity
  facade_points : FacadeAtomicAgrees facade ops (queueRepresentation rep queueRep) queueCapacity
  replace_points : QueueMaintenanceProof.ReplaceAtomicAgrees replaceCall ops
    (queueRepresentation rep queueRep) (fun note => note.params.noteIndex) queueCapacity
  cursor_points : QueueMaintenanceProof.CursorAtomicAgrees cursorCall ops
    (queueRepresentation rep queueRep) holdQueueResolved queueCapacity
  all_fit : ∀ stores zone, QueueMaintenanceProof.Fits queueCapacity
    (InputModel.buttonQueueAt (rep.queues stores) (rep.zone zone))

/-- Fill update_queue from the real replace/normalize composition; transport
only the unchanged point laws through withQueueMaintenance. -/
theorem remaining_atoms_of_points {S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (ctx : Context) (cap : Capacity)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (other : OtherAtoms base ops rep ctx cap)
    (points : QueuePoints slot facade replaceCall cursorCall ops rep queueRep queueCapacity) :
    HoldScheduleJudgmentsProof.RemainingAtoms
      (withQueueMaintenance base slot facade replaceCall cursorCall) ops rep ctx cap := by
  refine {
    length := other.length
    get := other.get
    button_diff := other.button_diff
    sensor_diff := other.sensor_diff
    button_pressed := other.button_pressed
    sensor_pressed := other.sensor_pressed
    prev_sensor_pressed := other.prev_sensor_pressed
    phase := other.phase
    unlocked := other.unlocked
    eligible := other.eligible
    fallback_area := other.fallback_area
    button_available := other.button_available
    sensor_available := other.sensor_available
    use_button := other.use_button
    use_sensor := other.use_sensor
    advance_frontier := other.advance_frontier
    update_queue := ?_
    append_active := other.append_active
    append_event := other.append_event }
  exact hold_update_queue_from_points base slot facade replaceCall cursorCall ops
    (queueRepresentation rep queueRep) rep.queues rep.zone queueCapacity
    points.slots points.facade_points points.replace_points points.cursor_points points.all_fit

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- Both overrides are executable compositions, not callback-equality premises. -/
def installKernels {S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N) :
    hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area :=
  HoldScheduleJudgmentsProof.installStep inst integerOps encode
    (withQueueMaintenance base slot facade replaceCall cursorCall) rep pack ctx

/-- The installed queue method retains the exact point/kernel call sequence. -/
theorem install_kernels_update_queue {S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (stores : Stores) (zone : Z) (note : N) :
    (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall).update_queue
      ops stores zone note =
      updateQueueViaPoints slot facade replaceCall cursorCall ops stores zone note := rfl

section Judgments
include integerLaw decode_encode

/-- AtomicAgrees now needs neither a per-note lifecycle equality nor a whole
queue-update equality. Both fields come from existing kernel theorems. -/
theorem atomic_agrees_of_kernels {S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (ready : HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack ctx)
    (other : OtherAtoms base ops rep ctx cap)
    (points : QueuePoints slot facade replaceCall cursorCall ops rep queueRep queueCapacity) :
    AtomicAgrees
      (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall)
      ops rep ctx cap := by
  exact HoldScheduleJudgmentsProof.atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    (withQueueMaintenance base slot facade replaceCall cursorCall) ops rep pack ctx cap ready
    (remaining_atoms_of_points base ops rep ctx cap slot facade replaceCall cursorCall
      queueRep queueCapacity other points)

/-- Full extracted traversal, targeting the ORIGINAL processHoldNotes with
empty output prefixes. Input and event order inherit the existing pass proof. -/
theorem process_equiv {S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (ready : HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack ctx)
    (other : OtherAtoms base ops rep ctx cap)
    (points : QueuePoints slot facade replaceCall cursorCall ops rep queueRep queueCapacity)
    (holds : S) (initial : hold_schedule.HoldWork F Stores C H E)
    (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote) (cursor : Scheduler.ClickCursor)
    (hinit : interpret rep initial = ⟨frontiers, queues, cursor, [], []⟩)
    (hfit : Fits cap (resume ctx (rep.holds holds) ⟨frontiers, queues, cursor, [], []⟩)) :
    hold_schedule.process
      (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall)
      holds initial ops ⦃ out =>
      (interpret rep out).output = reference ctx (rep.holds holds) frontiers queues cursor ⦄ := by
  exact HoldScheduleProof.process_equiv
    (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall)
    ops rep ctx cap
    (atomic_agrees_of_kernels inst integerOps decode integerLaw encode decode_encode
      base ops rep pack ctx cap slot facade replaceCall cursorCall queueRep queueCapacity
      ready other points)
    holds initial frontiers queues cursor hinit hfit

end Judgments

/-- Construct the existing frame contracts with queue composition installed.
The current frame supplies both Ops and judgment context. No premise quantifies
over whole queue-update results or complete Hold-pass results. -/
theorem hold_contracts_of_points {W S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area)
    (a : SchedulerFrameHoldProof.FrameAdapters W S F Stores C H E Ops)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput) (cap : Capacity)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (fields : SchedulerFrameHoldProof.FrameFieldsAgree a rep repr)
    (other : ∀ frame, OtherAtoms base (a.ops input frame) rep
      (SchedulerFrameHoldProof.frameContext input (repr frame)) cap)
    (ready : ∀ frame, HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack
      (SchedulerFrameHoldProof.frameContext input (repr frame)))
    (points : ∀ frame, QueuePoints slot facade replaceCall cursorCall
      (a.ops input frame) rep queueRep queueCapacity)
    (hfit : ∀ frame, Fits cap
      (resume (SchedulerFrameHoldProof.frameContext input (repr frame)) (repr frame).state.activeHolds
        (SchedulerFrameHoldProof.initialReference (repr frame)))) :
    SchedulerFrameHoldProof.HoldContracts inst integerOps encode
      (withQueueMaintenance base slot facade replaceCall cursorCall) a rep pack repr cap input := by
  refine { fields := fields, remaining := ?_, encoding := ready, fits := hfit }
  intro frame
  exact remaining_atoms_of_points base (a.ops input frame) rep
    (SchedulerFrameHoldProof.frameContext input (repr frame)) cap
    slot facade replaceCall cursorCall queueRep queueCapacity (other frame) (points frame)

section Frame
include integerLaw decode_encode

/-- Direct frame-phase corollary. SchedulerFrameHold still installs the step
from the current accumulator; installing the queue method captures no context. -/
theorem hold_phase_refines {W S Z N F Stores C H E Event D Area Ops Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area)
    (a : SchedulerFrameHoldProof.FrameAdapters W S F Stores C H E Ops)
    (rep : Representation S Z N F Stores C H E Event D Area)
    (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput) (cap : Capacity)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (fields : SchedulerFrameHoldProof.FrameFieldsAgree a rep repr)
    (other : ∀ frame, OtherAtoms base (a.ops input frame) rep
      (SchedulerFrameHoldProof.frameContext input (repr frame)) cap)
    (ready : ∀ frame, HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack
      (SchedulerFrameHoldProof.frameContext input (repr frame)))
    (points : ∀ frame, QueuePoints slot facade replaceCall cursorCall
      (a.ops input frame) rep queueRep queueCapacity)
    (hfit : ∀ frame, Fits cap
      (resume (SchedulerFrameHoldProof.frameContext input (repr frame)) (repr frame).state.activeHolds
        (SchedulerFrameHoldProof.initialReference (repr frame)))) (frame : W) :
    ∃ next, SchedulerFrameHoldProof.holdPhase inst integerOps encode
      (withQueueMaintenance base slot facade replaceCall cursorCall) a rep pack repr input frame =
        ok next ∧ repr next = SchedulerFrameProof.referencePass input .Hold (repr frame) := by
  exact SchedulerFrameHoldProof.hold_phase_refines
    inst integerOps decode integerLaw encode decode_encode
    (withQueueMaintenance base slot facade replaceCall cursorCall) a rep pack repr input cap
    (hold_contracts_of_points inst integerOps encode base a rep pack repr input cap
      slot facade replaceCall cursorCall queueRep queueCapacity fields other ready points hfit) frame

end Frame
end Verification.HoldScheduleQueueJudgmentsProof
