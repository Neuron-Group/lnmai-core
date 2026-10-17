import Verification.TouchSchedulePassJudgments
import Verification.TouchScheduleLocal
import Verification.StorageLocalRefinement

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TouchSchedulePassLocalProof
open TouchSchedulePassProof
open private processTouchQueueHead processTouchQueue processTouchNotes
  touchQueueResolved from LnmaiCore.Scheduler

/-! A bounded head, followed by automatic with its actual output groups, at
each visited sensor prefix. The executable Implementation and sensor loop are
unchanged. No whole-slot, whole-head or whole-pass equation is a premise. -/

def headReference {Q N F C G E Event T D Area Zone Id Grade : Type}
    (rep : TouchScheduleProof.Representation Q N F C G E Event T D Area Zone Id Grade)
    (ctx : Context) (area : areas.SensorArea) (queue : Q) (acc : Accumulator) :
    TouchScheduleModel.Outcome :=
  TouchScheduleModel.resume (headContext ctx (Areas.toLnmSensorArea area))
    (TouchScheduleModel.remaining (rep.queue queue) + 1)
    ⟨rep.queue queue, acc.1, acc.2.1, acc.2.2.1, acc.2.2.2⟩

def AutomaticFits (automaticCapacity : TouchAutomaticProof.Capacity)
    (ctx : Context) (area : areas.SensorArea) (head : TouchScheduleModel.Outcome) : Prop :=
  TouchAutomaticProof.Fits automaticCapacity
    (TouchAutomaticModel.resume
      (automaticContext ctx (Areas.toLnmSensorArea area) head.2.2.2.1)
      head.1 0 head.1.notes ⟨[], head.2.1, head.2.2.2.2⟩)

structure SlotFits {Q N F C G E Event T D Area Zone Id Grade : Type}
    (rep : TouchScheduleProof.Representation Q N F C G E Event T D Area Zone Id Grade)
    (headCapacity : TouchScheduleProof.Capacity)
    (automaticCapacity : TouchAutomaticProof.Capacity)
    (ctx : Context) (groupCapacity : Nat)
    (area : areas.SensorArea) (queue : Q) (acc : Accumulator) : Prop where
  fuel : TouchScheduleModel.remaining (rep.queue queue) + 1 ≤ Usize.max
  queueFits : QueueMaintenanceProof.Fits headCapacity.queue (rep.queue queue)
  events : (headReference rep ctx area queue acc).2.2.2.2.length ≤ headCapacity.events
  groups : (headReference rep ctx area queue acc).2.2.2.1.length ≤ groupCapacity
  automatic : AutomaticFits automaticCapacity ctx area (headReference rep ctx area queue acc)

def Ready (i : Implementation) (ctx : Context) (groupCapacity : Nat)
    (frontiers : i.F) (queues : storage.SensorVec i.Q) (cursor : i.C) (groups : i.G) : Prop :=
  StorageLocalRefinementProof.SensorVecReady i.rep.queue (referenceSlot ctx)
    (SlotFits i.rep i.headCapacity i.automaticCapacity ctx groupCapacity) queues
    (i.rep.frontiers frontiers, i.rep.cursor cursor, i.rep.groups groups, [])

structure AgreesWithin (i : Implementation) (ctx : Context) (groupCapacity : Nat) : Prop where
  clone : ∀ queue, i.clone.clone queue = ok queue
  fuel : ∀ queue, TouchScheduleModel.remaining (i.rep.queue queue) + 1 ≤ Usize.max →
    ∃ fuel, i.fuel queue = ok fuel ∧
    fuel.val = TouchScheduleModel.remaining (i.rep.queue queue) + 1
  headPoints : ∀ area, TouchScheduleProof.PointAgrees (i.head area) i.headCursor
    i.headReplace (i.headOps area) i.rep (headContext ctx (Areas.toLnmSensorArea area)) i.headCapacity
  headGroups : ∀ area, TouchScheduleLocalProof.GroupsWithin (i.head area) (i.headOps area) i.rep groupCapacity
  headLifecycle : ∀ area, TouchScheduleProof.LifecycleAgrees (i.head area) (i.headOps area) i.rep
    (headContext ctx (Areas.toLnmSensorArea area))
  automatic : ∀ area groups, TouchAutomaticProof.AtomicAgrees (i.automatic area groups)
    (i.automaticOps area groups) i.automaticRep
    (automaticContext ctx (Areas.toLnmSensorArea area) (i.rep.groups groups)) i.automaticCapacity
  setNotes : ∀ queue notes, (i.notes notes).length ≤ i.automaticCapacity.notes →
    ∃ installed, i.setNotes queue notes = ok installed ∧
      i.rep.queue installed = { i.rep.queue queue with notes := i.notes notes }
  queueFacade : QueueMaintenanceComposition.FacadeAtomicAgrees i.facade i.queueOps
    (TouchAutomaticQueueProof.queueRep i.automaticRep) i.automaticCapacity.notes
  queueCursor : QueueMaintenanceProof.CursorAtomicAgrees i.cursor i.queueOps
    (TouchAutomaticQueueProof.queueRep i.automaticRep) touchQueueResolved i.automaticCapacity.notes
  emptyNotes : i.notes i.emptyNotes = []
  emptyEvents : i.rep.events i.emptyEvents = []
  reverse : ∀ events, ∃ reversed, i.reverse events = ok reversed ∧
    i.rep.events reversed = (i.rep.events events).reverse

theorem queue_slot_refines (i : Implementation) (ctx : Context) (groupCapacity : Nat)
    (laws : AgreesWithin i ctx groupCapacity) (area : areas.SensorArea) (queue : i.Q)
    (acc : i.F × i.C × i.G × i.E)
    (bounds : SlotFits i.rep i.headCapacity i.automaticCapacity ctx groupCapacity area queue
      (decodeAcc i acc)) :
    ∃ out, queueSlot i (area, queue, acc) = ok out ∧
      (i.rep.queue out.1, decodeAcc i out.2) =
        referenceSlot ctx (Areas.toLnmSensorArea area) (i.rep.queue queue) (decodeAcc i acc) := by
  obtain ⟨fuel, hfuel, hfuelVal⟩ := laws.fuel queue bounds.fuel
  obtain ⟨head, hheadRun, _, hhead⟩ := spec_imp_exists
    (TouchScheduleLocalProof.head_process_equiv (i.head area) i.headCursor i.headReplace
      (i.headOps area) i.rep (headContext ctx (Areas.toLnmSensorArea area)) i.headCapacity
      (laws.headPoints area) (laws.headGroups area) (laws.headLifecycle area)
      fuel (headWork i queue acc) hfuelVal bounds.queueFits
      (by simpa only [hfuelVal, headWork, TouchScheduleProof.interpret, decodeAcc,
        headReference] using bounds.events)
      (by simpa only [hfuelVal, headWork, TouchScheduleProof.interpret, decodeAcc,
        headReference] using bounds.groups))
  have hheadRef : (TouchScheduleProof.interpret i.rep head).output =
      headReference i.rep ctx area queue (decodeAcc i acc) := by
    simpa only [headReference, TouchScheduleModel.resume, TouchScheduleModel.remaining,
      QueueMaintenanceModel.remaining, processTouchQueueHead, headContext, decodeAcc,
      headWork, TouchScheduleProof.interpret] using hhead
  have hauto : AutomaticFits i.automaticCapacity ctx area
      (TouchScheduleProof.interpret i.rep head).output := by
    rw [hheadRef]
    exact bounds.automatic
  obtain ⟨out, houtRun, hout⟩ := spec_imp_exists
    (TouchAutomaticQueueProof.process_equiv (i.automatic area head.groups)
      (i.automaticOps area head.groups) i.automaticRep
      (automaticContext ctx (Areas.toLnmSensorArea area) (i.rep.groups head.groups))
      i.automaticCapacity (laws.automatic area head.groups) i.setNotes laws.setNotes
      i.facade i.cursor i.queueOps laws.queueFacade laws.queueCursor head.queue
      ⟨i.emptyNotes, head.frontiers, head.events_rev⟩
      (i.rep.frontiers head.frontiers) (i.rep.events head.events_rev)
      (by simp only [TouchAutomaticProof.interpret, Implementation.automaticRep, laws.emptyNotes])
      hauto)
  refine ⟨(out.1, out.2.1, head.cursor, head.groups, out.2.2), ?_, ?_⟩
  · simp only [queueSlot, hfuel, bind_tc_ok, hheadRun, houtRun]
  · unfold referenceSlot processTouchQueue
    simp only [decodeAcc, headWork, headContext, TouchScheduleProof.interpret,
      TouchScheduleModel.Work.output] at hhead
    simp only [decodeAcc]
    rw [← hhead]
    have h := congrArg (fun result : TouchAutomaticModel.Outcome =>
      (result.1, result.2.1, i.rep.cursor head.cursor, result.2.2.1, result.2.2.2)) hout
    simpa only [decodeAcc, Implementation.automaticRep, automaticContext,
      TouchAutomaticModel.reference] using h

theorem process_equiv (i : Implementation) (ctx : Context) (groupCapacity : Nat)
    (laws : AgreesWithin i ctx groupCapacity) (frontiers : i.F) (queues : storage.SensorVec i.Q)
    (cursor : i.C) (groups : i.G)
    (ready : Ready i ctx groupCapacity frontiers queues cursor groups) :
    ∃ out, process i frontiers queues cursor groups = ok out ∧
      (i.rep.frontiers out.1, decodeQueues i out.2.1, i.rep.events out.2.2.1,
        i.rep.cursor out.2.2.2.1, i.rep.groups out.2.2.2.2) =
      reference ctx (i.rep.frontiers frontiers) (decodeQueues i queues)
        (i.rep.cursor cursor) (i.rep.groups groups) := by
  obtain ⟨mapped, hmap, hmapRep⟩ := StorageLocalRefinementProof.sensor_map_accum_refines_at
    i.clone (slotCall i) queues (frontiers, cursor, groups, i.emptyEvents) ()
    i.rep.queue (decodeAcc i) i.rep.queue (referenceSlot ctx)
    (SlotFits i.rep i.headCapacity i.automaticCapacity ctx groupCapacity) laws.clone (by
      intro area queue acc hfit
      exact queue_slot_refines i ctx groupCapacity laws area queue acc hfit)
    (by simpa only [Ready, decodeAcc, laws.emptyEvents] using ready)
  obtain ⟨events, hreverse, hevents⟩ := laws.reverse mapped.2.2.2.2
  refine ⟨(mapped.2.1, mapped.1, events, mapped.2.2.1, mapped.2.2.2.1), ?_, ?_⟩
  · simp only [process, hmap, bind_tc_ok, hreverse]
  · have h := congrArg (fun result : SensorQueueVec TouchNote × Accumulator =>
        (result.2.1, result.1, result.2.2.2.2.reverse, result.2.2.1, result.2.2.2.1)) hmapRep
    simpa only [decodeAcc, decodeQueues, reference, processTouchNotes, referenceSlot,
      laws.emptyEvents, hevents] using h

structure RemainingAtomsWithin (base : Implementation) (ctx : Context) : Prop where
  clone : ∀ queue, base.clone.clone queue = ok queue
  fuel : ∀ queue, TouchScheduleModel.remaining (base.rep.queue queue) + 1 ≤ Usize.max →
    ∃ fuel, base.fuel queue = ok fuel ∧
    fuel.val = TouchScheduleModel.remaining (base.rep.queue queue) + 1
  headPoints : ∀ area, TouchScheduleProof.PointAgrees (base.head area) base.headCursor
    base.headReplace (base.headOps area) base.rep
    (headContext ctx (Areas.toLnmSensorArea area)) base.headCapacity
  automaticPoints : ∀ area groups, TouchAutomaticGroupsProof.OtherAtoms
    (base.automatic area groups) (base.automaticOps area groups) base.automaticRep
    (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups))
    base.automaticCapacity
  setNotes : ∀ queue notes, (base.notes notes).length ≤ base.automaticCapacity.notes →
    ∃ installed, base.setNotes queue notes = ok installed ∧
      base.rep.queue installed = { base.rep.queue queue with notes := base.notes notes }
  queueFacade : QueueMaintenanceComposition.FacadeAtomicAgrees base.facade base.queueOps
    (TouchAutomaticQueueProof.queueRep base.automaticRep) base.automaticCapacity.notes
  queueCursor : QueueMaintenanceProof.CursorAtomicAgrees base.cursor base.queueOps
    (TouchAutomaticQueueProof.queueRep base.automaticRep) touchQueueResolved
    base.automaticCapacity.notes
  emptyNotes : base.notes base.emptyNotes = []
  emptyEvents : base.rep.events base.emptyEvents = []
  reverse : ∀ events, ∃ reversed, base.reverse events = ok reversed ∧
    base.rep.events reversed = (base.rep.events events).reverse

theorem automatic_group_fields (base : Implementation) (ctx : Context)
    (remaining : RemainingAtomsWithin base ctx) (area : areas.SensorArea) :
    TouchAutomaticGroupsProof.GroupFields base.automaticRep
      (TouchScheduleJudgmentsProof.headRep base.rep) (TouchSchedulePassJudgmentsProof.noteGroup base area) := by
  exact { note_group := (remaining.headPoints area).group_id, pair := fun _ => rfl }

open TouchSchedulePassJudgmentsProof

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

include integerLaw decode_encode in
theorem agrees_of_judgments_groups (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (ready : EncodingReady inst integerOps decode encode base ctx pack)
    (remaining : RemainingAtomsWithin base ctx) (groupCapacity : Nat)
    (groupPoints : ∀ area, TouchScheduleJudgmentsProof.GroupAtoms
      (headCall area) (base.headOps area) base.rep groupCapacity) :
    AgreesWithin (install inst integerOps decode encode base ctx pack headCall) ctx groupCapacity := by
  refine {
    clone := remaining.clone
    fuel := remaining.fuel
    headPoints := fun area => TouchScheduleJudgmentsProof.point_agrees_installed
      inst integerOps decode encode (base.head area) (headCall area) base.headCursor
      base.headReplace (base.headOps area) base.rep pack
      (headContext ctx (Areas.toLnmSensorArea area)) base.headCapacity (remaining.headPoints area)
    headGroups := fun area => TouchScheduleLocalProof.groups_within_installed
      inst integerOps decode encode (base.head area) (headCall area) (base.headOps area)
      base.rep pack (headContext ctx (Areas.toLnmSensorArea area)) groupCapacity (groupPoints area)
    headLifecycle := fun area => TouchScheduleJudgmentsProof.lifecycle_agrees_of_judgments
      inst integerOps decode integerLaw encode decode_encode
      (TouchScheduleJudgmentsProof.installGroups (base.head area) (headCall area))
      (base.headOps area) base.rep pack (headContext ctx (Areas.toLnmSensorArea area)) (ready area)
    automatic := ?_
    setNotes := remaining.setNotes
    queueFacade := remaining.queueFacade
    queueCursor := remaining.queueCursor
    emptyNotes := remaining.emptyNotes
    emptyEvents := remaining.emptyEvents
    reverse := remaining.reverse }
  intro area groups
  have sharedLaws := TouchAutomaticGroupsProof.remaining_atoms_of_groups
    (base.automatic area groups) (base.automaticOps area groups) base.automaticRep
    (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups))
    base.automaticCapacity (noteGroup base area) (headCall area) (base.headOps area)
    (TouchScheduleJudgmentsProof.headRep base.rep) groups groupCapacity
    (automatic_group_fields base ctx remaining area)
    (TouchScheduleJudgmentsProof.head_atoms (headCall area) (base.headOps area)
      base.rep groupCapacity (groupPoints area)) rfl (remaining.automaticPoints area groups)
  exact TouchAutomaticJudgmentsProof.atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    (TouchAutomaticGroupsProof.installShared (base.automatic area groups)
      (noteGroup base area) (headCall area) (base.headOps area) groups)
    (base.automaticOps area groups) base.automaticRep (automaticEncoding pack)
    (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups))
    base.automaticCapacity
    (automatic_encoding_ready inst integerOps decode encode base ctx pack ready area groups)
    sharedLaws

/-- The installation changes calls only; the current queues/representations
and hence the concrete prefix capacity conditions are identical. -/
theorem ready_installed (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (groupCapacity : Nat) (frontiers : base.F) (queues : storage.SensorVec base.Q)
    (cursor : base.C) (groups : base.G)
    (ready : Ready base ctx groupCapacity frontiers queues cursor groups) :
    Ready (install inst integerOps decode encode base ctx pack headCall)
      ctx groupCapacity frontiers queues cursor groups := by
  exact ready

include integerLaw decode_encode in
/-- Practical pass entry. Every size bound is evaluated at the actual initial
sensor vector and reference accumulators reached in storage order. -/
theorem process_judgments_equiv (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (encoding : EncodingReady inst integerOps decode encode base ctx pack)
    (remaining : RemainingAtomsWithin base ctx) (groupCapacity : Nat)
    (groupPoints : ∀ area, TouchScheduleJudgmentsProof.GroupAtoms
      (headCall area) (base.headOps area) base.rep groupCapacity)
    (frontiers : base.F) (queues : storage.SensorVec base.Q) (cursor : base.C) (groups : base.G)
    (ready : Ready base ctx groupCapacity frontiers queues cursor groups) :
    ∃ out, TouchSchedulePassProof.process
        (install inst integerOps decode encode base ctx pack headCall)
        frontiers queues cursor groups = ok out ∧
      (base.rep.frontiers out.1, decodeQueues base out.2.1, base.rep.events out.2.2.1,
        base.rep.cursor out.2.2.2.1, base.rep.groups out.2.2.2.2) =
      reference ctx (base.rep.frontiers frontiers) (decodeQueues base queues)
        (base.rep.cursor cursor) (base.rep.groups groups) := by
  exact process_equiv (install inst integerOps decode encode base ctx pack headCall)
    ctx groupCapacity
    (agrees_of_judgments_groups inst integerOps decode integerLaw encode decode_encode
      base ctx pack headCall encoding remaining groupCapacity groupPoints)
    frontiers queues cursor groups
    (ready_installed inst integerOps decode encode base ctx pack headCall
      groupCapacity frontiers queues cursor groups ready)

end Verification.TouchSchedulePassLocalProof
