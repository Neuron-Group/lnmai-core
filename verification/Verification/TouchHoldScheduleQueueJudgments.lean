import Verification.QueueMaintenanceComposition
import Verification.TouchHoldScheduleJudgments
import Verification.TouchHoldScheduleTraversal

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

namespace Verification.TouchHoldScheduleQueueJudgmentsProof
open TouchHoldScheduleModel TouchHoldScheduleProof QueueMaintenanceComposition
open private touchHoldBodyCheckActive touchQueueIndexUnlocked holdHeadEligibleForClick
  advanceSharedTouchQueue holdQueueResolved from LnmaiCore.Scheduler

/-!
Compose TouchHold judgment/lifecycle and sensor-queue maintenance from their
extracted kernels. Neither the complete lifecycle result nor the complete
queue-update result is a premise. The sensor slot contains HoldNote and uses
holdQueueResolved, including replacement of every duplicate note identifier.

The concrete scheduler facade, numeric encodings, container point laws,
allocation success, and capacity remain explicit obligations. This composition
does not establish those runtime representation laws.
-/

/-- Point laws excluding both the derived lifecycle and queue update. -/
structure OtherAtoms {S A N F Q C H E Event D Z Id Grade G B Ops : Type}
    (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
    (ops : Ops) (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ holds, ∃ len, call.length ops holds = ok len ∧
    (rep.holds holds).length = len.val
  get : ∀ holds (index : Usize) (pre : List (SensorArea × HoldNote)) area note rest,
    rep.holds holds = pre ++ (area, note) :: rest → pre.length = index.val →
    ∃ nativeArea nativeNote, call.get ops holds index = ok (nativeArea, nativeNote) ∧
      rep.area nativeArea = area ∧ rep.note nativeNote = note
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = (ctx.currentTime - ctx.touchPanelOffset) -
      (rep.note note).params.effectiveTiming
  button_diff : ∀ note, ∃ diff, call.button_diff ops note = ok diff ∧
    rep.duration diff = ctx.currentTime - (rep.note note).params.effectiveTiming
  local_pressed : ∀ area, call.local_pressed ops area = ok (ctx.input.getSensorHeld (rep.area area))
  body_check_active : ∀ note, call.body_check_active ops note =
    ok (touchHoldBodyCheckActive (rep.note note) ctx.currentTime)
  body_group_id : ∀ note, ∃ id, call.body_group_id ops note = ok id ∧
    id.map rep.id = (rep.note note).touchHoldGroupId
  head_group_id : ∀ note, ∃ id, call.head_group_id ops note = ok id ∧
    id.map rep.id = (rep.note note).touchGroupId
  note_index : ∀ note, ∃ id, call.note_index ops note = ok id ∧
    rep.id id = (rep.note note).params.noteIndex
  group_size : ∀ note, ∃ size, call.group_size ops note = ok size ∧
    rep.id size = (rep.note note).touchGroupSize
  phase : ∀ note, call.phase ops note = ok (TouchHoldScheduleProof.phase (rep.note note))
  head_grade : ∀ note, ∃ grade, call.head_grade ops note = ok grade ∧
    grade.map rep.grade = headGrade (rep.note note)
  head_diff : ∀ note, ∃ diff, call.head_diff ops note = ok diff ∧
    rep.duration diff = (rep.note note).headDiff
  grade_rejected : ∀ grade, call.grade_rejected ops grade = ok (rep.grade grade).isMissOrTooFast
  within_head_window : ∀ note, call.within_head_window ops note =
    ok (decide (ctx.currentTime ≤ (rep.note note).params.effectiveTiming + touchGoodMs))
  unlocked : ∀ frontiers area note, call.unlocked ops frontiers area note =
    ok (touchQueueIndexUnlocked (rep.frontiers frontiers) (rep.area area)
      (rep.note note).touchQueueIndex)
  eligible : ∀ note, call.eligible ops note =
    ok (holdHeadEligibleForClick (rep.note note) ctx.currentTime)
  button_ring : call.button_ring ops = ok ctx.buttonRingForTouch
  outer_button : ∀ area, ∃ zone, call.outer_button ops area = ok zone ∧
    zone.map rep.zone = (rep.area area).toOuterButtonZone?
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
  advance_frontier : ∀ frontiers area, ∃ next,
    call.advance_frontier ops frontiers area = ok next ∧
    rep.frontiers next = advanceSharedTouchQueue (rep.frontiers frontiers) (rep.area area)
  append_active : ∀ active area note, (rep.active active).length + 1 ≤ cap.active →
    ∃ next, call.append_active ops active area note = ok next ∧
    rep.active next = rep.active active ++ [(rep.area area, rep.note note)]
  append_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.append_event ops events event = ok next ∧
    rep.events next = rep.events events ++ [rep.event event]


/-- The queue and traversal use the same note interpretation. -/
def queueRepresentation {S A N F Stores C H E Event D Z Id Grade G B Q : Type}
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (queueRep : Q → ZoneQueue HoldNote) : QueueMaintenanceProof.Representation Q N HoldNote :=
  { queue := queueRep, note := rep.note }

structure QueuePoints {S A N F Stores C H E Event D Z Id Grade G B Ops Q : Type}
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat) : Prop where
  slots : SlotAtomicAgrees slot ops (queueRepresentation rep queueRep) rep.queues rep.area
    InputModel.sensorQueueAt InputModel.setSensorQueueAt queueCapacity
  facade_points : FacadeAtomicAgrees facade ops (queueRepresentation rep queueRep) queueCapacity
  replace_points : QueueMaintenanceProof.ReplaceAtomicAgrees replaceCall ops
    (queueRepresentation rep queueRep) (fun note => note.params.noteIndex) queueCapacity
  cursor_points : QueueMaintenanceProof.CursorAtomicAgrees cursorCall ops
    (queueRepresentation rep queueRep) holdQueueResolved queueCapacity
  all_fit : ∀ stores area, QueueMaintenanceProof.Fits queueCapacity
    (InputModel.sensorQueueAt (rep.queues stores) (rep.area area))

/-- Install the actual read/replace-all/normalize/write composition. -/
def withQueueMaintenance {S A N F Stores C H E Event D Z Id Grade Ops Q : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade)
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N) :
    touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade :=
  { base with update_queue := fun ops stores area note =>
      updateQueueViaPoints slot facade replaceCall cursorCall ops stores area note }

theorem remaining_atoms_of_points {S A N F Stores C H E Event D Z Id Grade G B Ops Q : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade)
    (ops : Ops) (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (ctx : Context) (cap : Capacity)
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (other : OtherAtoms base ops rep ctx cap)
    (points : QueuePoints slot facade replaceCall cursorCall ops rep queueRep queueCapacity) :
    TouchHoldScheduleJudgmentsProof.RemainingAtoms
      (withQueueMaintenance base slot facade replaceCall cursorCall) ops rep ctx cap := by
  refine {
    length := other.length
    get := other.get
    sensor_diff := other.sensor_diff
    button_diff := other.button_diff
    local_pressed := other.local_pressed
    body_check_active := other.body_check_active
    body_group_id := other.body_group_id
    head_group_id := other.head_group_id
    note_index := other.note_index
    group_size := other.group_size
    phase := other.phase
    head_grade := other.head_grade
    head_diff := other.head_diff
    grade_rejected := other.grade_rejected
    within_head_window := other.within_head_window
    unlocked := other.unlocked
    eligible := other.eligible
    button_ring := other.button_ring
    outer_button := other.outer_button
    button_available := other.button_available
    sensor_available := other.sensor_available
    use_button := other.use_button
    use_sensor := other.use_sensor
    advance_frontier := other.advance_frontier
    update_queue := ?_
    append_active := other.append_active
    append_event := other.append_event }
  intro stores area note
  exact spec_imp_exists
    (update_sensor_hold_queue_spec slot facade replaceCall cursorCall ops
      (queueRepresentation rep queueRep) rep.queues rep.area queueCapacity
      points.slots points.facade_points points.replace_points points.cursor_points
      stores area note (points.all_fit stores area))

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)

def installKernels {S A N F Stores C H E Event D Z Id Grade G B Ops Q : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (pack : TouchHoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context)
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N) :
    touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade :=
  TouchHoldScheduleJudgmentsProof.installStep inst integerOps encode
    (withQueueMaintenance base slot facade replaceCall cursorCall) rep pack ctx

theorem install_kernels_update_queue {S A N F Stores C H E Event D Z Id Grade G B Ops Q : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (pack : TouchHoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context)
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (stores : Stores) (area : A) (note : N) :
    (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall).update_queue
      ops stores area note =
      updateQueueViaPoints slot facade replaceCall cursorCall ops stores area note := rfl

include integerLaw decode_encode

/-- Both composite fields are derived from kernel theorems. -/
theorem atomic_agrees_of_kernels {S A N F Stores C H E Event D Z Id Grade G B Ops Q : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade)
    (ops : Ops) (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (pack : TouchHoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (ready : TouchHoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack ctx)
    (other : OtherAtoms base ops rep ctx cap)
    (points : QueuePoints slot facade replaceCall cursorCall ops rep queueRep queueCapacity) :
    AtomicAgrees
      (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall)
      ops rep ctx cap := by
  exact TouchHoldScheduleJudgmentsProof.atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    (withQueueMaintenance base slot facade replaceCall cursorCall) ops rep pack ctx cap ready
    (remaining_atoms_of_points base ops rep ctx cap slot facade replaceCall cursorCall
      queueRep queueCapacity other points)

/-- Complete original TouchHold pass with both composite callbacks discharged. -/
theorem process_equiv {S A N F Stores C H E Event D Z Id Grade G B Ops Q : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Stores C H E Event D Z Id Grade)
    (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (bodies : touch_hold_schedule.BodyGroups Ops B Id)
    (ops : Ops) (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (pack : TouchHoldScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (slot : QueueSlot Ops Stores A Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (queueRep : Q → ZoneQueue HoldNote) (queueCapacity : Nat)
    (ready : TouchHoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack ctx)
    (other : OtherAtoms base ops rep ctx cap)
    (points : QueuePoints slot facade replaceCall cursorCall ops rep queueRep queueCapacity)
    (headAtoms : TouchHoldGroupsProof.HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : TouchHoldGroupsProof.BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Stores C H E G B)
    (frontiers : SensorVec Nat) (queues : SensorQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) (groups : List GroupState)
    (bodyGroups : List TouchHoldBodyGroupState)
    (hinit : interpret rep initial = ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩)
    (hsafe : Safe ctx cap (rep.holds holds) ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩) :
    touch_hold_schedule.process
      (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall)
      heads bodies holds initial ops ⦃ out =>
      (interpret rep out).output =
        reference ctx (rep.holds holds) frontiers queues cursor groups bodyGroups ⦄ := by
  exact TouchHoldScheduleTraversalProof.process_equiv
    (installKernels inst integerOps encode base rep pack ctx slot facade replaceCall cursorCall)
    heads bodies ops rep ctx cap
    (atomic_agrees_of_kernels inst integerOps decode integerLaw encode decode_encode
      base ops rep pack ctx cap slot facade replaceCall cursorCall queueRep queueCapacity
      ready other points)
    headAtoms bodyAtoms holds initial frontiers queues cursor groups bodyGroups hinit hsafe

end Verification.TouchHoldScheduleQueueJudgmentsProof

