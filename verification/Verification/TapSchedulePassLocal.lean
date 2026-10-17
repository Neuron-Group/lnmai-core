import Verification.TapSchedulePass
import Verification.StorageLocalRefinement

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TapSchedulePassLocalProof
open TapSchedulePassProof
open private processTapNotes from LnmaiCore.Scheduler

variable {Q N F C E Event D Area Ops : Type}

def SlotFits (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (ctx : Context) (capacity : Nat) (zone : areas.ButtonZone)
    (queue : Q) (acc : Accumulator) : Prop :=
  (TapScheduleModel.resume (zoneContext ctx (Areas.toLnmButtonZone zone))
    (TapScheduleModel.remaining (rep.queue queue))
    ⟨rep.queue queue, acc.1, acc.2.1, acc.2.2⟩).2.2.2.length ≤ capacity

/-- One actual button vector, starting with the pass's empty event buffer.
The source slot determines the accumulator supplied to every next condition. -/
def Ready (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (ctx : Context) (capacity : Nat) (frontiers : F)
    (queues : storage.ButtonVec Q) (cursor : C) : Prop :=
  StorageLocalRefinementProof.ButtonVecReady rep.queue (referenceSlot ctx)
    (SlotFits rep ctx capacity) queues (rep.frontiers frontiers, rep.cursor cursor, [])

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
    (frontiers : F) (queues : storage.ButtonVec Q) (cursor : C)
    (hready : Ready rep ctx capacity frontiers queues cursor) :
    ∃ out, process clone call ops remaining reverse emptyEvents frontiers queues cursor = ok out ∧
      (rep.frontiers out.1, decodeQueues rep out.2.1, rep.events out.2.2.1, rep.cursor out.2.2.2) =
        reference ctx (rep.frontiers frontiers) (decodeQueues rep queues) (rep.cursor cursor) := by
  obtain ⟨mapped, hmap, hmapRep⟩ := StorageLocalRefinementProof.button_map_accum_refines_at
    clone (slotCall call ops remaining) queues (frontiers, cursor, emptyEvents) ()
    rep.queue (decodeAcc rep) rep.queue (referenceSlot ctx) (SlotFits rep ctx capacity) hclone (by
      intro zone queue acc hfit
      exact queue_slot_refines call ops remaining rep ctx capacity zone queue acc
        (atoms zone) (hremaining queue) hfit)
    (by simpa only [Ready, decodeAcc, hempty] using hready)
  obtain ⟨events, hreverseCall, hevents⟩ := hreverse mapped.2.2.2
  refine ⟨(mapped.2.1, mapped.1, events, mapped.2.2.1), ?_, ?_⟩
  · simp only [process, hmap, bind_tc_ok, hreverseCall]
  · have h := congrArg (fun p : ButtonQueueVec TapFamilyNote × Accumulator =>
        (p.2.1, p.1, p.2.2.2.reverse, p.2.2.1)) hmapRep
    simpa only [decodeAcc, decodeQueues, reference, processTapNotes, referenceSlot, hempty,
      hevents] using h

end Verification.TapSchedulePassLocalProof
