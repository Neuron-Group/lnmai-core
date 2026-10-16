import Verification.TouchSchedule
import Verification.TouchAutomaticQueue
import Verification.StorageSensorRefinement

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TouchSchedulePassProof
open private processTouchQueueHead processTouchQueueAutomatic processTouchQueue
  processTouchNotes touchQueueResolved from LnmaiCore.Scheduler

/-! Compose both extracted Touch traversals with the extracted sensor map.
The automatic traversal uses the queue, frontiers, groups and reverse events
returned by the head traversal. Each subsequent sensor receives the complete
updated accumulator; events reverse once after the sensor traversal.

This layer accepts point, group and single-note lifecycle contracts. The
separate judgment/group installation proves the latter two. Native field,
integer, allocation, cloning and capacity obligations remain explicit; this
composition is not an extraction of the concrete scheduler.rs facade. -/

structure Context where
  input : FrameInput
  currentTime : TimePoint
  style : JudgeStyle
  touchPanelOffset : Duration
  buttonRingForTouch : Bool

def headContext (ctx : Context) (area : SensorArea) : TouchScheduleModel.Context :=
  ⟨area, ctx.input, ctx.currentTime, ctx.style, ctx.touchPanelOffset, ctx.buttonRingForTouch⟩

def automaticContext (ctx : Context) (area : SensorArea) (groups : List GroupState) :
    TouchAutomaticModel.Context :=
  ⟨area, ctx.currentTime, ctx.style, ctx.touchPanelOffset, groups⟩

abbrev Accumulator := SensorVec Nat × Scheduler.ClickCursor × List GroupState × List JudgeEvent
abbrev Outcome := SensorVec Nat × SensorQueueVec TouchNote × List JudgeEvent ×
  Scheduler.ClickCursor × List GroupState

def referenceSlot (ctx : Context) (area : SensorArea) (queue : ZoneQueue TouchNote)
    (acc : Accumulator) : ZoneQueue TouchNote × Accumulator :=
  processTouchQueue area queue acc.1 ctx.input ctx.currentTime ctx.style acc.2.1
    ctx.touchPanelOffset acc.2.2.1 ctx.buttonRingForTouch acc.2.2.2

def reference (ctx : Context) (frontiers : SensorVec Nat)
    (queues : SensorQueueVec TouchNote) (cursor : Scheduler.ClickCursor)
    (groups : List GroupState) : Outcome :=
  processTouchNotes frontiers queues ctx.input ctx.currentTime ctx.style cursor
    ctx.touchPanelOffset ctx.buttonRingForTouch groups

structure Implementation where
  Q : Type
  N : Type
  Notes : Type
  F : Type
  C : Type
  G : Type
  E : Type
  Event : Type
  T : Type
  D : Type
  Area : Type
  Zone : Type
  Id : Type
  Grade : Type
  M : Type
  HOps : Type
  AOps : Type
  QOps : Type
  clone : core.clone.Clone Q
  head : areas.SensorArea →
    touch_schedule.TouchSchedule HOps Q N F C G E Event T D Area Zone Id Grade M
  headOps : areas.SensorArea → HOps
  headCursor : queue_maintenance.QueueCursor M Q N
  headReplace : queue_maintenance.QueueReplace M Q N
  fuel : Q → Result Usize
  automatic : areas.SensorArea → G →
    touch_automatic.TouchAutomatic AOps Q N Notes F E Event D (Grade × D)
  automaticOps : areas.SensorArea → G → AOps
  setNotes : Q → Notes → Result Q
  facade : QueueMaintenanceComposition.Facade QOps Q
  cursor : queue_maintenance.QueueCursor QOps Q N
  queueOps : QOps
  emptyNotes : Notes
  emptyEvents : E
  reverse : E → Result E
  rep : TouchScheduleProof.Representation Q N F C G E Event T D Area Zone Id Grade
  notes : Notes → List TouchNote
  headCapacity : TouchScheduleProof.Capacity
  automaticCapacity : TouchAutomaticProof.Capacity

def Implementation.automaticRep (i : Implementation) :
    TouchAutomaticProof.Representation i.Q i.N i.Notes i.F i.E i.Event i.D (i.Grade × i.D) :=
  ⟨i.rep.queue, i.rep.note, i.notes, i.rep.frontiers, i.rep.events, i.rep.event,
    i.rep.duration, fun pair => (i.rep.grade pair.1, i.rep.duration pair.2)⟩

def decodeAcc (i : Implementation) (acc : i.F × i.C × i.G × i.E) : Accumulator :=
  (i.rep.frontiers acc.1, i.rep.cursor acc.2.1, i.rep.groups acc.2.2.1, i.rep.events acc.2.2.2)

def decodeQueues (i : Implementation) (queues : storage.SensorVec i.Q) : SensorQueueVec TouchNote :=
  StorageSensorRefinementProof.decodeSensors i.rep.queue (StorageProof.toLnmSensorVec queues)

def headWork (i : Implementation) (queue : i.Q) (acc : i.F × i.C × i.G × i.E) :
    touch_schedule.TouchWork i.Q i.F i.C i.G i.E :=
  ⟨queue, acc.1, acc.2.1, acc.2.2.1, acc.2.2.2⟩

structure Agrees (i : Implementation) (ctx : Context) : Prop where
  clone : ∀ queue, i.clone.clone queue = ok queue
  fuel : ∀ queue, ∃ fuel, i.fuel queue = ok fuel ∧
    fuel.val = TouchScheduleModel.remaining (i.rep.queue queue) + 1
  headPoints : ∀ area, TouchScheduleProof.PointAgrees (i.head area) i.headCursor
    i.headReplace (i.headOps area) i.rep (headContext ctx (Areas.toLnmSensorArea area)) i.headCapacity
  headGroups : ∀ area, TouchScheduleProof.GroupAgrees (i.head area) (i.headOps area) i.rep
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

/-- Capacity is a separate obligation, not inferred from successful field
reads. This uniform form can later be specialized to reachable native states. -/
structure Bounds (i : Implementation) (ctx : Context) : Prop where
  queue : ∀ queue, QueueMaintenanceProof.Fits i.headCapacity.queue (i.rep.queue queue)
  headEvents : ∀ area queue acc,
    (TouchScheduleModel.resume (headContext ctx (Areas.toLnmSensorArea area))
      (TouchScheduleModel.remaining (i.rep.queue queue) + 1)
      (TouchScheduleProof.interpret i.rep (headWork i queue acc))).2.2.2.2.length ≤ i.headCapacity.events
  automatic : ∀ area groups queue frontiers events, TouchAutomaticProof.Fits i.automaticCapacity
    (TouchAutomaticModel.resume
      (automaticContext ctx (Areas.toLnmSensorArea area) (i.rep.groups groups))
      (i.rep.queue queue) 0 (i.rep.queue queue).notes
      ⟨[], i.rep.frontiers frontiers, i.rep.events events⟩)

def queueSlot (i : Implementation) (args : areas.SensorArea × i.Q × (i.F × i.C × i.G × i.E)) :
    Result (i.Q × (i.F × i.C × i.G × i.E)) := do
  let fuel ← i.fuel args.2.1
  let head ← touch_schedule.process (i.head args.1) i.headCursor i.headReplace fuel
    (headWork i args.2.1 args.2.2) (i.headOps args.1)
  let out ← TouchAutomaticQueueProof.process (i.automatic args.1 head.groups)
    (i.automaticOps args.1 head.groups) i.setNotes i.facade i.cursor i.queueOps head.queue
    ⟨i.emptyNotes, head.frontiers, head.events_rev⟩
  ok (out.1, out.2.1, head.cursor, head.groups, out.2.2)

theorem queue_slot_refines (i : Implementation) (ctx : Context) (laws : Agrees i ctx)
    (bounds : Bounds i ctx) (area : areas.SensorArea) (queue : i.Q)
    (acc : i.F × i.C × i.G × i.E) :
    ∃ out, queueSlot i (area, queue, acc) = ok out ∧
      (i.rep.queue out.1, decodeAcc i out.2) =
        referenceSlot ctx (Areas.toLnmSensorArea area) (i.rep.queue queue) (decodeAcc i acc) := by
  obtain ⟨fuel, hfuel, hfuelVal⟩ := laws.fuel queue
  obtain ⟨head, hheadRun, _, hhead⟩ := spec_imp_exists
    (TouchScheduleProof.head_process_equiv (i.head area) i.headCursor i.headReplace
      (i.headOps area) i.rep (headContext ctx (Areas.toLnmSensorArea area)) i.headCapacity
      (laws.headPoints area) (laws.headGroups area) (laws.headLifecycle area)
      fuel (headWork i queue acc) hfuelVal (bounds.queue queue)
      (by simpa only [hfuelVal, headWork] using bounds.headEvents area queue acc))
  obtain ⟨out, houtRun, hout⟩ := spec_imp_exists
    (TouchAutomaticQueueProof.process_equiv (i.automatic area head.groups)
      (i.automaticOps area head.groups) i.automaticRep
      (automaticContext ctx (Areas.toLnmSensorArea area) (i.rep.groups head.groups))
      i.automaticCapacity (laws.automatic area head.groups) i.setNotes laws.setNotes
      i.facade i.cursor i.queueOps laws.queueFacade laws.queueCursor head.queue
      ⟨i.emptyNotes, head.frontiers, head.events_rev⟩
      (i.rep.frontiers head.frontiers) (i.rep.events head.events_rev)
      (by simp only [TouchAutomaticProof.interpret, Implementation.automaticRep, laws.emptyNotes])
      (bounds.automatic area head.groups head.queue head.frontiers head.events_rev))
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

def slotCall (i : Implementation) : core.ops.function.Fn Unit
    (areas.SensorArea × i.Q × (i.F × i.C × i.G × i.E)) (i.Q × (i.F × i.C × i.G × i.E)) :=
  { FnMutInst :=
      { FnOnceInst := { call_once := fun _ args => queueSlot i args }
        call_mut := fun unit args => do
          let out ← queueSlot i args
          ok (out, unit) }
    call := fun _ args => queueSlot i args }

def process (i : Implementation) (frontiers : i.F) (queues : storage.SensorVec i.Q)
    (cursor : i.C) (groups : i.G) : Result (i.F × storage.SensorVec i.Q × i.E × i.C × i.G) := do
  let out ← storage.SensorVec.map_accum i.clone (slotCall i) queues
    (frontiers, cursor, groups, i.emptyEvents) ()
  let events ← i.reverse out.2.2.2.2
  ok (out.2.1, out.1, events, out.2.2.1, out.2.2.2.1)

theorem process_equiv (i : Implementation) (ctx : Context) (laws : Agrees i ctx)
    (bounds : Bounds i ctx) (frontiers : i.F) (queues : storage.SensorVec i.Q)
    (cursor : i.C) (groups : i.G) :
    ∃ out, process i frontiers queues cursor groups = ok out ∧
      (i.rep.frontiers out.1, decodeQueues i out.2.1, i.rep.events out.2.2.1,
        i.rep.cursor out.2.2.2.1, i.rep.groups out.2.2.2.2) =
      reference ctx (i.rep.frontiers frontiers) (decodeQueues i queues)
        (i.rep.cursor cursor) (i.rep.groups groups) := by
  obtain ⟨mapped, hmap, hmapRep⟩ := StorageSensorRefinementProof.sensor_map_accum_refines
    i.clone (slotCall i) queues (frontiers, cursor, groups, i.emptyEvents) ()
    i.rep.queue (decodeAcc i) i.rep.queue (referenceSlot ctx) laws.clone (by
      intro area queue acc
      exact queue_slot_refines i ctx laws bounds area queue acc)
  obtain ⟨events, hreverse, hevents⟩ := laws.reverse mapped.2.2.2.2
  refine ⟨(mapped.2.1, mapped.1, events, mapped.2.2.1, mapped.2.2.2.1), ?_, ?_⟩
  · simp only [process, hmap, bind_tc_ok, hreverse]
  · have h := congrArg (fun result : SensorQueueVec TouchNote × Accumulator =>
        (result.2.1, result.1, result.2.2.2.2.reverse, result.2.2.1, result.2.2.2.1)) hmapRep
    simpa only [decodeAcc, decodeQueues, reference, processTouchNotes, referenceSlot,
      laws.emptyEvents, hevents] using h

end Verification.TouchSchedulePassProof
