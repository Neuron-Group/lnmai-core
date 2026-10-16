import Verification.TapSchedule
import Verification.StorageRefinement

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TapSchedulePassProof
open private processTapQueue processTapNotes from LnmaiCore.Scheduler

/-!
The complete Tap pass composes the actual extracted ButtonVec.map_accum and
tap_schedule.process. Each zone receives the updated frontiers, click cursor
and reverse event buffer from its predecessor. Events reverse once at the end.
The proof includes short or oversized vectors: map_accum follows the original
zip-with-storage-order stopping rule, without assuming exactly eight slots.

This wrapper has not been identified with the unextracted process_tap_notes
facade. Point representations, native clones, remaining-slot conversion and
event reversal remain explicit. The uniform intermediate output bound below
is an obligation, not a claim that arbitrary native buffers can always grow.
-/

structure Context where
  input : FrameInput
  currentTime : TimePoint
  touchPanelOffset : Duration
  style : JudgeStyle

def zoneContext (ctx : Context) (zone : ButtonZone) : TapScheduleModel.Context :=
  ⟨zone, ctx.input, ctx.currentTime, ctx.touchPanelOffset, ctx.style⟩

abbrev Accumulator := ButtonVec Nat × Scheduler.ClickCursor × List JudgeEvent
abbrev Outcome := ButtonVec Nat × ButtonQueueVec TapFamilyNote ×
  List JudgeEvent × Scheduler.ClickCursor

def referenceSlot (ctx : Context) (zone : ButtonZone)
    (queue : ZoneQueue TapFamilyNote) (acc : Accumulator) :
    ZoneQueue TapFamilyNote × Accumulator :=
  let out := processTapQueue zone queue acc.1 ctx.input ctx.currentTime
    ctx.touchPanelOffset ctx.style acc.2.1 acc.2.2
  (out.1, out.2)

def reference (ctx : Context) (frontiers : ButtonVec Nat)
    (queues : ButtonQueueVec TapFamilyNote) (cursor : Scheduler.ClickCursor) : Outcome :=
  processTapNotes frontiers queues ctx.input ctx.currentTime ctx.touchPanelOffset ctx.style cursor

variable {Q N F C E Event D Area Ops : Type}

def decodeAcc (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (acc : F × C × E) : Accumulator :=
  (rep.frontiers acc.1, rep.cursor acc.2.1, rep.events acc.2.2)

def decodeQueues (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (queues : storage.ButtonVec Q) : ButtonQueueVec TapFamilyNote :=
  StorageRefinementProof.decodeButtons rep.queue (StorageProof.toLnmButtonVec queues)

def queueSlot (call : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops) (remaining : Q → Result Usize)
    (args : areas.ButtonZone × Q × (F × C × E)) : Result (Q × (F × C × E)) := do
  let fuel ← remaining args.2.1
  let out ← tap_schedule.process call fuel
    ⟨args.2.1, args.2.2.1, args.2.2.2.1, args.2.2.2.2⟩ (ops args.1)
  ok (out.queue, out.frontiers, out.cursor, out.events_rev)

def slotCall (call : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops) (remaining : Q → Result Usize) :
    core.ops.function.Fn Unit (areas.ButtonZone × Q × (F × C × E)) (Q × (F × C × E)) :=
  { FnMutInst :=
      { FnOnceInst := { call_once := fun _ args => queueSlot call ops remaining args }
        call_mut := fun unit args => do
          let out ← queueSlot call ops remaining args
          ok (out, unit) }
    call := fun _ args => queueSlot call ops remaining args }

theorem queue_slot_refines
    (call : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops) (remaining : Q → Result Usize)
    (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (ctx : Context) (capacity : Nat) (zone : areas.ButtonZone)
    (queue : Q) (acc : F × C × E)
    (atoms : TapScheduleProof.AtomicAgrees call (ops zone) rep
      (zoneContext ctx (Areas.toLnmButtonZone zone)) capacity)
    (hremaining : ∃ fuel, remaining queue = ok fuel ∧
      fuel.val = TapScheduleModel.remaining (rep.queue queue))
    (hfit : (TapScheduleModel.resume (zoneContext ctx (Areas.toLnmButtonZone zone))
      (TapScheduleModel.remaining (rep.queue queue))
      ⟨rep.queue queue, rep.frontiers acc.1, rep.cursor acc.2.1, rep.events acc.2.2⟩).2.2.2.length
        ≤ capacity) :
    ∃ out, queueSlot call ops remaining (zone, queue, acc) = ok out ∧
      (rep.queue out.1, decodeAcc rep out.2) =
        referenceSlot ctx (Areas.toLnmButtonZone zone) (rep.queue queue) (decodeAcc rep acc) := by
  obtain ⟨fuel, hfuel, hfuelVal⟩ := hremaining
  let initial : tap_schedule.TapWork Q F C E := ⟨queue, acc.1, acc.2.1, acc.2.2⟩
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (TapScheduleProof.pending_process_equiv call (ops zone) rep
      (zoneContext ctx (Areas.toLnmButtonZone zone)) capacity atoms fuel initial
      hfuelVal (by simpa only [hfuelVal, initial, TapScheduleProof.interpret] using hfit))
  refine ⟨(out.queue, out.frontiers, out.cursor, out.events_rev), ?_, ?_⟩
  · simp only [queueSlot, hfuel, bind_tc_ok, hrun, initial]
  · exact hpost

def process (clone : core.clone.Clone Q)
    (call : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops) (remaining : Q → Result Usize)
    (reverse : E → Result E) (emptyEvents : E)
    (frontiers : F) (queues : storage.ButtonVec Q) (cursor : C) :
    Result (F × storage.ButtonVec Q × E × C) := do
  let out ← storage.ButtonVec.map_accum clone (slotCall call ops remaining)
    queues (frontiers, cursor, emptyEvents) ()
  let events ← reverse out.2.2.2
  ok (out.2.1, out.1, events, out.2.2.1)

/-- No premise asserts a queue traversal or a complete pass equation. The
intermediate capacity bound is intentionally exposed for later reachability
specialization; it must not be confused with the field/allocation laws. -/
theorem process_equiv (clone : core.clone.Clone Q)
    (call : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops) (remaining : Q → Result Usize)
    (reverse : E → Result E) (emptyEvents : E)
    (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (ctx : Context) (capacity : Nat)
    (hclone : ∀ queue, clone.clone queue = ok queue)
    (atoms : ∀ zone, TapScheduleProof.AtomicAgrees call (ops zone) rep
      (zoneContext ctx (Areas.toLnmButtonZone zone)) capacity)
    (hremaining : ∀ queue, ∃ fuel, remaining queue = ok fuel ∧
      fuel.val = TapScheduleModel.remaining (rep.queue queue))
    (hreverse : ∀ events, ∃ reversed, reverse events = ok reversed ∧
      rep.events reversed = (rep.events events).reverse)
    (hempty : rep.events emptyEvents = [])
    (hfit : ∀ zone queue (acc : F × C × E),
      (TapScheduleModel.resume (zoneContext ctx (Areas.toLnmButtonZone zone))
        (TapScheduleModel.remaining (rep.queue queue))
        ⟨rep.queue queue, rep.frontiers acc.1, rep.cursor acc.2.1, rep.events acc.2.2⟩).2.2.2.length
          ≤ capacity)
    (frontiers : F) (queues : storage.ButtonVec Q) (cursor : C) :
    ∃ out, process clone call ops remaining reverse emptyEvents frontiers queues cursor = ok out ∧
      (rep.frontiers out.1, decodeQueues rep out.2.1, rep.events out.2.2.1, rep.cursor out.2.2.2) =
        reference ctx (rep.frontiers frontiers) (decodeQueues rep queues) (rep.cursor cursor) := by
  obtain ⟨mapped, hmap, hmapRep⟩ := StorageRefinementProof.button_map_accum_refines
    clone (slotCall call ops remaining) queues (frontiers, cursor, emptyEvents) ()
    rep.queue (decodeAcc rep) rep.queue (referenceSlot ctx) hclone (by
      intro zone queue acc
      exact queue_slot_refines call ops remaining rep ctx capacity zone queue acc
        (atoms zone) (hremaining queue) (hfit zone queue acc))
  obtain ⟨events, hreverseCall, hevents⟩ := hreverse mapped.2.2.2
  refine ⟨(mapped.2.1, mapped.1, events, mapped.2.2.1), ?_, ?_⟩
  · simp only [process, hmap, bind_tc_ok, hreverseCall]
  · have h := congrArg (fun p : ButtonQueueVec TapFamilyNote × Accumulator =>
        (p.2.1, p.1, p.2.2.2.reverse, p.2.2.1)) hmapRep
    simpa only [decodeAcc, decodeQueues, reference, processTapNotes, referenceSlot, hempty,
      hevents] using h

end Verification.TapSchedulePassProof
