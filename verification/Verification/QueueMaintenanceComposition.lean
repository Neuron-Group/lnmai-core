import Verification.QueueMaintenance

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 300000

namespace Verification.QueueMaintenanceComposition

open QueueMaintenanceModel QueueMaintenanceProof
open private updateButtonHoldQueue updateSensorHoldQueue
  normalizeHoldQueueCursor normalizeTouchQueueCursor
  replaceHoldQueueNote replaceTouchQueueNote holdQueueResolved touchQueueResolved
  from LnmaiCore.Scheduler

/-!
Sources checked:
* LnmaiCore/Scheduler.lean: updateButtonHoldQueue (439),
  updateSensorHoldQueue (446), and the inline TouchNote compositions in
  processTouchQueueHeadFuel (824/838). There are no reference functions named
  updateHoldQueueForNote or updateTouchQueueForNote in that source.
* Rust scheduler.rs: replace_*_queue_note, normalize_*_queue_cursor,
  update_button_hold_queue, update_sensor_hold_queue, and
  HoldSchedule::update_queue.
* Verification.Generated.queue_maintenance and Verification.QueueMaintenance.
* Verification.HoldSchedule: AtomicAgrees.update_queue.

The definitions below are explicit facade compositions of the REAL extracted
replace_all and normalize_cursor_fuel. Neither loop is reimplemented here.
The facade compositions themselves have NOT been claimed to be extracted.
Their concrete Rust point operations and source wiring remain obligations.
There is no premise equating a complete update callback to the reference.

TouchHold uses HoldNote and holdQueueResolved even though its container is a
sensor vector. Ordinary TouchNote uses touchQueueResolved.

Capacity bounds represented slot counts only. Successful copy, container
access, allocation, BigUint conversion, and arithmetic are explicit point
contracts. No uniqueness, cursor-in-bounds, or machine bound on noteIndex is
assumed. The final Hold bridge requires every selected queue to satisfy Fits,
because the current Hold AtomicAgrees.update_queue field is unguarded.
-/

/-- Replacement changes neither length nor cursor, so the runtime needs no
    machine remaining+1 before OR after replacement. -/
theorem remaining_replace (id : α → Nat) (source : ZoneQueue α) (note : α) :
    remaining (replace id source note) = remaining source := by
  simp only [remaining, replace_preserves_length, replace_preserves_index]

/-- Point operations performed by the two runtime wrappers. `remaining` is
    only unwrap_or(usize::MAX) followed by saturating_sub; it cannot inspect
    queue entries or perform any traversal. -/
structure Facade (Ops Q : Type) where
  length : Ops → Q → Result Usize
  copy : Ops → Q → Result Q
  cursor_to_usize : Ops → Q → Result (Option Usize)
  remaining : Ops → Usize → Option Usize → Result Usize

structure FacadeAtomicAgrees {Ops Q N α : Type}
    (facade : Facade Ops Q) (ops : Ops)
    (rep : Representation Q N α) (capacity : Nat) : Prop where
  length : ∀ queue, Fits capacity (rep.queue queue) →
    ∃ len, facade.length ops queue = ok len ∧
      len.val = (rep.queue queue).notes.length
  copy : ∀ queue, Fits capacity (rep.queue queue) →
    ∃ copied, facade.copy ops queue = ok copied ∧ rep.queue copied = rep.queue queue
  cursor_to_usize : ∀ queue, Fits capacity (rep.queue queue) →
    ∃ converted, facade.cursor_to_usize ops queue = ok converted ∧
      CursorConversionAgrees (rep.queue queue).currentIndex converted
  remaining : ∀ len converted,
    ∃ fuel, facade.remaining ops len converted = ok fuel ∧
      fuel.val = remainingFacadeValue len converted

/-- Evaluation order of replace_*_queue_note: length, clone, extracted loop. -/
def replaceViaFacade {Ops Q N : Type}
    (facade : Facade Ops Q) (call : queue_maintenance.QueueReplace Ops Q N)
    (ops : Ops) (queue : Q) (note : N) : Result Q := do
  let len ← facade.length ops queue
  let copied ← facade.copy ops queue
  queue_maintenance.replace_all call len copied note ops

/-- Evaluation order of normalize_*_queue_cursor, including the clone AFTER
    computing fuel. The extracted loop still peeks before its fuel guard. -/
def normalizeViaFacade {Ops Q N : Type}
    (facade : Facade Ops Q) (call : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (queue : Q) : Result Q := do
  let len ← facade.length ops queue
  let converted ← facade.cursor_to_usize ops queue
  let fuel ← facade.remaining ops len converted
  let copied ← facade.copy ops queue
  queue_maintenance.normalize_cursor_fuel call fuel copied ops

/-- This is bind composition, not an atomic callback for a whole update. -/
def replaceThenNormalize {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (queue : Q) (note : N) : Result Q := do
  let replaced ← replaceViaFacade facade replaceCall ops queue note
  normalizeViaFacade facade cursorCall ops replaced

theorem replace_via_facade_spec {Ops Q N α : Type}
    (facade : Facade Ops Q) (call : queue_maintenance.QueueReplace Ops Q N)
    (ops : Ops) (rep : Representation Q N α) (id : α → Nat) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (points : ReplaceAtomicAgrees call ops rep id capacity)
    (queue : Q) (note : N) (hfit : Fits capacity (rep.queue queue)) :
    replaceViaFacade facade call ops queue note ⦃ out =>
      rep.queue out = replace id (rep.queue queue) (rep.note note) ∧
      Fits capacity (rep.queue out) ⦄ := by
  obtain ⟨len, hlen, hlenValue⟩ := fpoints.length queue hfit
  obtain ⟨copied, hcopy, hcopied⟩ := fpoints.copy queue hfit
  obtain ⟨out, hrun, hreference, _, _, hfitOut⟩ := spec_imp_exists
    (replace_spec call ops rep id capacity points len copied note
      (by simpa only [hcopied] using hlenValue)
      (by simpa only [hcopied] using hfit))
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, hfitOut⟩
  · simp only [replaceViaFacade, hlen, hcopy, bind_tc_ok, hrun]
  · simpa only [hcopied] using hreference

theorem normalize_via_facade_spec {Ops Q N α : Type}
    (facade : Facade Ops Q) (call : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N α) (resolved : α → Bool) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (points : CursorAtomicAgrees call ops rep resolved capacity)
    (queue : Q) (hfit : Fits capacity (rep.queue queue)) :
    normalizeViaFacade facade call ops queue ⦃ out =>
      rep.queue out = normalizeFuel resolved (remaining (rep.queue queue)) (rep.queue queue) ∧
      (rep.queue out).notes = (rep.queue queue).notes ∧
      Fits capacity (rep.queue out) ⦄ := by
  obtain ⟨len, hlen, hlenValue⟩ := fpoints.length queue hfit
  obtain ⟨converted, hconvert, hconversion⟩ := fpoints.cursor_to_usize queue hfit
  obtain ⟨fuel, hfuel, hfuelValue⟩ := fpoints.remaining len converted
  obtain ⟨copied, hcopy, hcopied⟩ := fpoints.copy queue hfit
  have hremaining : fuel.val = remaining (rep.queue queue) :=
    hfuelValue.trans (remaining_facade_value (rep.queue queue) len converted
      hlenValue hconversion)
  obtain ⟨out, hrun, hreference, hnotes, hfitOut⟩ := spec_imp_exists
    (normalize_spec call ops rep resolved capacity points copied fuel
      (by simpa only [hcopied] using hfit))
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, ?_, hfitOut⟩
  · simp only [normalizeViaFacade, hlen, hconvert, hfuel, hcopy, bind_tc_ok, hrun]
  · simpa only [hcopied, hremaining] using hreference
  · simpa only [hcopied] using hnotes

/-- Successful composition with exact reference result, all-slot map, length
    and capacity. The result uses the fuel actually computed AFTER replacement.
    remaining_replace shows this is also the original queue's remaining count. -/
theorem replace_then_normalize_spec {Ops Q N α : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N α)
    (id : α → Nat) (resolved : α → Bool) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep id capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep resolved capacity)
    (queue : Q) (note : N) (hfit : Fits capacity (rep.queue queue)) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note ⦃ out =>
      rep.queue out = normalizeFuel resolved
        (remaining (replace id (rep.queue queue) (rep.note note)))
        (replace id (rep.queue queue) (rep.note note)) ∧
      (rep.queue out).notes = (replace id (rep.queue queue) (rep.note note)).notes ∧
      (rep.queue out).notes.length = (rep.queue queue).notes.length ∧
      Fits capacity (rep.queue out) ⦄ := by
  obtain ⟨replaced, hreplace, hreplaced, hfitReplaced⟩ := spec_imp_exists
    (replace_via_facade_spec facade replaceCall ops rep id capacity fpoints rpoints
      queue note hfit)
  obtain ⟨out, hnormalize, hreference, hnotes, hfitOut⟩ := spec_imp_exists
    (normalize_via_facade_spec facade cursorCall ops rep resolved capacity fpoints cpoints
      replaced hfitReplaced)
  have hnotesOut : (rep.queue out).notes =
      (replace id (rep.queue queue) (rep.note note)).notes := by
    simpa only [hreplaced] using hnotes
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, hnotesOut, ?_, hfitOut⟩
  · simp only [replaceThenNormalize, hreplace, bind_tc_ok, hnormalize]
  · simpa only [hreplaced] using hreference
  · rw [hnotesOut, replace_preserves_length]

/-- All physical positions keep their order. Every duplicate identifier is
    replaced, even before the cursor; absent/out-of-range slots remain absent. -/
theorem replace_then_normalize_slots {Ops Q N α : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N α)
    (id : α → Nat) (resolved : α → Bool) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep id capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep resolved capacity)
    (queue : Q) (note : N) (hfit : Fits capacity (rep.queue queue)) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note ⦃ out =>
      (rep.queue out).notes.length = (rep.queue queue).notes.length ∧
      ∀ slot : Nat, (rep.queue out).notes[slot]? =
        ((rep.queue queue).notes[slot]?).map (replaceOne id (rep.note note)) ⦄ := by
  obtain ⟨out, hrun, _, hnotes, hlength, _⟩ := spec_imp_exists
    (replace_then_normalize_spec facade replaceCall cursorCall ops rep id resolved
      capacity fpoints rpoints cpoints queue note hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, hlength, ?_⟩
  intro slot
  rw [hnotes]
  exact replace_lookup id (rep.queue queue) (rep.note note) slot

/-- An out-of-bounds Nat cursor is retained exactly; replacement still scans
    all slots. This includes cursors larger than Usize.max. -/
theorem replace_then_normalize_outside {Ops Q N α : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N α)
    (id : α → Nat) (resolved : α → Bool) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep id capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep resolved capacity)
    (queue : Q) (note : N) (hfit : Fits capacity (rep.queue queue))
    (houtside : (rep.queue queue).notes.length ≤ (rep.queue queue).currentIndex) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note ⦃ out =>
      rep.queue out = replace id (rep.queue queue) (rep.note note) ∧
      (rep.queue out).currentIndex = (rep.queue queue).currentIndex ⦄ := by
  obtain ⟨out, hrun, hreference, _, _, _⟩ := spec_imp_exists
    (replace_then_normalize_spec facade replaceCall cursorCall ops rep id resolved
      capacity fpoints rpoints cpoints queue note hfit)
  have hzero : remaining (replace id (rep.queue queue) (rep.note note)) = 0 := by
    rw [remaining_replace]
    exact Nat.sub_eq_zero_of_le houtside
  have hout : rep.queue out = replace id (rep.queue queue) (rep.note note) := by
    simpa only [hzero, normalize_zero] using hreference
  apply exists_imp_spec
  refine ⟨out, hrun, hout, ?_⟩
  rw [hout, replace_preserves_index]

/-- Direct bridge used by both ordinary Hold and sensor TouchHold. -/
theorem replace_then_normalize_hold_equiv {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N HoldNote) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep
      (fun note => note.params.noteIndex) capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep holdQueueResolved capacity)
    (queue : Q) (nativeNote : N) (source : ZoneQueue HoldNote) (note : HoldNote)
    (hsource : rep.queue queue = source) (hnote : rep.note nativeNote = note)
    (hfit : Fits capacity source) :
    replaceThenNormalize facade replaceCall cursorCall ops queue nativeNote ⦃ out =>
      rep.queue out = normalizeHoldQueueCursor (replaceHoldQueueNote source note) ⦄ := by
  obtain ⟨out, hrun, hreference, _, _, _⟩ := spec_imp_exists
    (replace_then_normalize_spec facade replaceCall cursorCall ops rep
      (fun note => note.params.noteIndex) holdQueueResolved capacity
      fpoints rpoints cpoints queue nativeNote (by simpa only [hsource] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hsource, hnote, hold_pending_equiv, replace_hold_eq] using hreference

/-- This is exactly the INLINE queue expression in both touchStep result
    branches of processTouchQueueHeadFuel, not a nonexistent update helper. -/
theorem replace_then_normalize_touch_equiv {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N TouchNote) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep
      (fun note => note.params.noteIndex) capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep touchQueueResolved capacity)
    (queue : Q) (nativeNote : N) (source : ZoneQueue TouchNote) (note : TouchNote)
    (hsource : rep.queue queue = source) (hnote : rep.note nativeNote = note)
    (hfit : Fits capacity source) :
    replaceThenNormalize facade replaceCall cursorCall ops queue nativeNote ⦃ out =>
      rep.queue out = normalizeTouchQueueCursor (replaceTouchQueueNote source note) ⦄ := by
  obtain ⟨out, hrun, hreference, _, _, _⟩ := spec_imp_exists
    (replace_then_normalize_spec facade replaceCall cursorCall ops rep
      (fun note => note.params.noteIndex) touchQueueResolved capacity
      fpoints rpoints cpoints queue nativeNote (by simpa only [hsource] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hsource, hnote, touch_pending_equiv, replace_touch_eq] using hreference

/-- Single container-slot read/write only. The read includes the runtime
    get_d/default/clone behavior; the write includes its vector set/clone. -/
structure QueueSlot (Ops Stores Z Q : Type) where
  read : Ops → Stores → Z → Result Q
  write : Ops → Stores → Z → Q → Result Stores

/-- Reference read/set remain explicit so short or malformed restored vectors
    have EXACT InputModel semantics. There is no valid-zone/container-size
    assumption and no promise that a set extends a short vector. -/
structure SlotAtomicAgrees {Ops Stores Z Q N α Collection Index : Type}
    (slot : QueueSlot Ops Stores Z Q) (ops : Ops)
    (rep : Representation Q N α) (storesRep : Stores → Collection)
    (indexRep : Z → Index)
    (atQueue : Collection → Index → ZoneQueue α)
    (setQueue : Collection → Index → ZoneQueue α → Collection)
    (capacity : Nat) : Prop where
  read : ∀ stores zone, Fits capacity (atQueue (storesRep stores) (indexRep zone)) →
    ∃ queue, slot.read ops stores zone = ok queue ∧
      rep.queue queue = atQueue (storesRep stores) (indexRep zone)
  write : ∀ stores zone queue, Fits capacity (rep.queue queue) →
    ∃ out, slot.write ops stores zone queue = ok out ∧
      storesRep out = setQueue (storesRep stores) (indexRep zone) (rep.queue queue)

/-- The actual wrapper control flow: read ONE queue, run the extracted
    replace/normalize kernels with the two facade clones, write ONE queue. -/
def updateQueueViaPoints {Ops Stores Z Q N : Type}
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (stores : Stores) (zone : Z) (note : N) : Result Stores := do
  let queue ← slot.read ops stores zone
  let updated ← replaceThenNormalize facade replaceCall cursorCall ops queue note
  slot.write ops stores zone updated

theorem update_queue_via_points_spec {Ops Stores Z Q N α Collection Index : Type}
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N α)
    (storesRep : Stores → Collection) (indexRep : Z → Index)
    (atQueue : Collection → Index → ZoneQueue α)
    (setQueue : Collection → Index → ZoneQueue α → Collection)
    (id : α → Nat) (resolved : α → Bool) (capacity : Nat)
    (spoints : SlotAtomicAgrees slot ops rep storesRep indexRep atQueue setQueue capacity)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep id capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep resolved capacity)
    (stores : Stores) (zone : Z) (note : N)
    (hfit : Fits capacity (atQueue (storesRep stores) (indexRep zone))) :
    updateQueueViaPoints slot facade replaceCall cursorCall ops stores zone note ⦃ out =>
      storesRep out = setQueue (storesRep stores) (indexRep zone)
        (normalizeFuel resolved
          (remaining (replace id (atQueue (storesRep stores) (indexRep zone)) (rep.note note)))
          (replace id (atQueue (storesRep stores) (indexRep zone)) (rep.note note))) ⦄ := by
  obtain ⟨queue, hread, hqueue⟩ := spoints.read stores zone hfit
  obtain ⟨updated, hupdate, hupdated, _, _, hfitUpdated⟩ := spec_imp_exists
    (replace_then_normalize_spec facade replaceCall cursorCall ops rep id resolved
      capacity fpoints rpoints cpoints queue note (by simpa only [hqueue] using hfit))
  obtain ⟨out, hwrite, hout⟩ := spoints.write stores zone updated hfitUpdated
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simp only [updateQueueViaPoints, hread, bind_tc_ok, hupdate, hwrite]
  · simpa only [hupdated, hqueue] using hout

/-- Exact original ordinary-hold container update, without a composite
    callback premise. -/
theorem update_button_hold_queue_spec {Ops Stores Z Q N : Type}
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N HoldNote)
    (storesRep : Stores → ButtonQueueVec HoldNote) (zoneRep : Z → ButtonZone)
    (capacity : Nat)
    (spoints : SlotAtomicAgrees slot ops rep storesRep zoneRep
      InputModel.buttonQueueAt InputModel.setButtonQueueAt capacity)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep
      (fun note => note.params.noteIndex) capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep holdQueueResolved capacity)
    (stores : Stores) (zone : Z) (note : N)
    (hfit : Fits capacity (InputModel.buttonQueueAt (storesRep stores) (zoneRep zone))) :
    updateQueueViaPoints slot facade replaceCall cursorCall ops stores zone note ⦃ out =>
      storesRep out = updateButtonHoldQueue (storesRep stores) (zoneRep zone) (rep.note note) ⦄ := by
  obtain ⟨out, hrun, hreference⟩ := spec_imp_exists
    (update_queue_via_points_spec slot facade replaceCall cursorCall ops rep
      storesRep zoneRep InputModel.buttonQueueAt InputModel.setButtonQueueAt
      (fun note => note.params.noteIndex) holdQueueResolved capacity
      spoints fpoints rpoints cpoints stores zone note hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hold_pending_equiv, replace_hold_eq, updateButtonHoldQueue] using hreference

/-- Exact original TOUCH-HOLD container update. The payload and resolved
    predicate are HoldNote/holdQueueResolved, not TouchNote/touchQueueResolved. -/
theorem update_sensor_hold_queue_spec {Ops Stores Z Q N : Type}
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N HoldNote)
    (storesRep : Stores → SensorQueueVec HoldNote) (areaRep : Z → SensorArea)
    (capacity : Nat)
    (spoints : SlotAtomicAgrees slot ops rep storesRep areaRep
      InputModel.sensorQueueAt InputModel.setSensorQueueAt capacity)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep
      (fun note => note.params.noteIndex) capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep holdQueueResolved capacity)
    (stores : Stores) (area : Z) (note : N)
    (hfit : Fits capacity (InputModel.sensorQueueAt (storesRep stores) (areaRep area))) :
    updateQueueViaPoints slot facade replaceCall cursorCall ops stores area note ⦃ out =>
      storesRep out = updateSensorHoldQueue (storesRep stores) (areaRep area) (rep.note note) ⦄ := by
  obtain ⟨out, hrun, hreference⟩ := spec_imp_exists
    (update_queue_via_points_spec slot facade replaceCall cursorCall ops rep
      storesRep areaRep InputModel.sensorQueueAt InputModel.setSensorQueueAt
      (fun note => note.params.noteIndex) holdQueueResolved capacity
      spoints fpoints rpoints cpoints stores area note hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hold_pending_equiv, replace_hold_eq, updateSensorHoldQueue] using hreference

/-- Concrete projection wiring for the GENERATED HoldSchedule trait. Other
    fields come from base; update_queue is this explicit composition. This is
    deliberately NOT a premise asserting that base.update_queue is correct. -/
def withQueueMaintenance {Ops S Z N F Stores C H E Event D Area Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N) :
    hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area :=
  { base with
    update_queue := fun ops stores zone note =>
      updateQueueViaPoints slot facade replaceCall cursorCall ops stores zone note }

/-- This is precisely the existential/success shape of
    HoldScheduleProof.AtomicAgrees.update_queue. Instantiate storesRep, zoneRep
    and rep.note with the corresponding Hold representation projections.

    Every hypothesis concerns a single primitive or represented capacity.
    No whole-update equality or success hypothesis appears here. -/
theorem hold_update_queue_from_points {Ops S Z N F Stores C H E Event D Area Q : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Stores C H E Event D Area)
    (slot : QueueSlot Ops Stores Z Q) (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N HoldNote)
    (storesRep : Stores → ButtonQueueVec HoldNote) (zoneRep : Z → ButtonZone)
    (capacity : Nat)
    (spoints : SlotAtomicAgrees slot ops rep storesRep zoneRep
      InputModel.buttonQueueAt InputModel.setButtonQueueAt capacity)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep
      (fun note => note.params.noteIndex) capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep holdQueueResolved capacity)
    (allFit : ∀ stores zone,
      Fits capacity (InputModel.buttonQueueAt (storesRep stores) (zoneRep zone))) :
    ∀ stores zone note, ∃ out,
      (withQueueMaintenance base slot facade replaceCall cursorCall).update_queue
        ops stores zone note = ok out ∧
      storesRep out = updateButtonHoldQueue (storesRep stores) (zoneRep zone) (rep.note note) := by
  intro stores zone note
  change ∃ out, updateQueueViaPoints slot facade replaceCall cursorCall ops stores zone note =
      ok out ∧
    storesRep out = updateButtonHoldQueue (storesRep stores) (zoneRep zone) (rep.note note)
  exact spec_imp_exists
    (update_button_hold_queue_spec slot facade replaceCall cursorCall ops rep
      storesRep zoneRep capacity spoints fpoints rpoints cpoints stores zone note
      (allFit stores zone))

/-- Both matching and nonmatching slots are covered independently; distinct
    positions may have equal (arbitrarily large Nat) identifiers. -/
theorem replace_then_normalize_all_matches {Ops Q N α : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (rep : Representation Q N α)
    (id : α → Nat) (resolved : α → Bool) (capacity : Nat)
    (fpoints : FacadeAtomicAgrees facade ops rep capacity)
    (rpoints : ReplaceAtomicAgrees replaceCall ops rep id capacity)
    (cpoints : CursorAtomicAgrees cursorCall ops rep resolved capacity)
    (queue : Q) (note : N) (hfit : Fits capacity (rep.queue queue)) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note ⦃ out =>
      ∀ (slot : Nat) (old : α), (rep.queue queue).notes[slot]? = some old →
        (id old = id (rep.note note) → (rep.queue out).notes[slot]? = some (rep.note note)) ∧
        (id old ≠ id (rep.note note) → (rep.queue out).notes[slot]? = some old) ⦄ := by
  obtain ⟨out, hrun, _, hslots⟩ := spec_imp_exists
    (replace_then_normalize_slots facade replaceCall cursorCall ops rep id resolved
      capacity fpoints rpoints cpoints queue note hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  intro slot old hslot
  have hresult := hslots slot
  rw [hslot] at hresult
  simp only [Option.map_some] at hresult
  constructor
  · intro hmatch
    simpa [replaceOne, hmatch] using hresult
  · intro hdifferent
    simpa [replaceOne, hdifferent] using hresult

/-!
These failure/divergence equations do NOT assume successful point operations.
They preserve Result effects and evaluation order. In particular, no fuel-zero
shortcut is inserted before normalize_cursor_fuel's actual peek. Its existing
normalize_body_zero_order/normalize_body_peek_failure theorems still apply.
Pure Lean updates do not have an error result, so reference equality above is
conditional on explicit successful point contracts, not an unconditional claim
that panics or allocation failures equal a pure reference update.
-/

theorem replace_then_normalize_replace_failure {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (queue : Q) (note : N) (error : Error)
    (hfailure : replaceViaFacade facade replaceCall ops queue note = fail error) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note = fail error := by
  simp only [replaceThenNormalize, hfailure, bind_tc_fail]

theorem replace_then_normalize_replace_div {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (queue : Q) (note : N)
    (hdiv : replaceViaFacade facade replaceCall ops queue note = div) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note = div := by
  simp only [replaceThenNormalize, hdiv, bind_tc_div]

theorem replace_then_normalize_normalize_failure {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (queue replaced : Q) (note : N) (error : Error)
    (hreplace : replaceViaFacade facade replaceCall ops queue note = ok replaced)
    (hfailure : normalizeViaFacade facade cursorCall ops replaced = fail error) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note = fail error := by
  simp only [replaceThenNormalize, hreplace, bind_tc_ok, hfailure]

theorem replace_then_normalize_normalize_div {Ops Q N : Type}
    (facade : Facade Ops Q)
    (replaceCall : queue_maintenance.QueueReplace Ops Q N)
    (cursorCall : queue_maintenance.QueueCursor Ops Q N)
    (ops : Ops) (queue replaced : Q) (note : N)
    (hreplace : replaceViaFacade facade replaceCall ops queue note = ok replaced)
    (hdiv : normalizeViaFacade facade cursorCall ops replaced = div) :
    replaceThenNormalize facade replaceCall cursorCall ops queue note = div := by
  simp only [replaceThenNormalize, hreplace, bind_tc_ok, hdiv]

/-!
HANDOFF / OPEN OBLIGATIONS

1. Main worker alone elaborates this new module against accepted-v13 (or its
   successor) QueueMaintenance/Generated. No Lean, lake, Cargo, extraction or
   tests were run by this task. Audit transitive axioms after elaboration.
2. For the Hold proof, use withQueueMaintenance base ... as the call record,
   and use hold_update_queue_from_points ... for its update_queue field. The
   remaining Hold point fields are unchanged definitionally. This module does
   not import or modify the independent HoldSchedule draft.
3. Concrete Rust facade wiring is still open: discharge FacadeAtomicAgrees,
   SlotAtomicAgrees, ReplaceAtomicAgrees and CursorAtomicAgrees for the chosen
   representation, and connect HoldScheduleFrame::update_queue's method body
   to these point definitions. That connection must follow its body/definition
   (or a future extraction), NOT assume the complete reference update equality.
   Merely choosing an arbitrary base.update_queue is not covered.
4. allFit is explicit for the existing universally quantified Hold contract.
   If only reachable stores have a capacity invariant, mainline must thread
   that invariant through Hold's contract; do not silently assert allFit.
5. clone and container-set allocation success are point premises, not implied
   by Fits. The arithmetic field covers only unwrap_or(MAX)/saturating_sub.
   BigUint-to-usize success/failure must match CursorConversionAgrees exactly;
   values beyond Usize.max map to none, not to a truncated cursor or ID.

Coverage in this draft: full duplicate-ID replacement, unchanged physical
order and length, arbitrary Nat cursor/IDs, empty and out-of-bounds queues,
remaining fuel without machine +1, exact reference container getD/set,
successful composition and propagation of Result failure/divergence. No
broader Hold active-list loop or Tap code is changed.
-/

end Verification.QueueMaintenanceComposition
