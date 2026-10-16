import Verification.QueueMaintenanceModel

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 300000

namespace Verification.QueueMaintenanceProof

open QueueMaintenanceModel
open private normalizeHoldQueueCursor normalizeTouchQueueCursor
  holdQueueResolved touchQueueResolved replaceHoldQueueNote replaceTouchQueueNote
  from LnmaiCore.Scheduler

/-!
The declarations below target the actual queue_maintenance extraction installed
in Verification.Generated. No local replacement for either Rust loop is used.

Representation, individual point-operation success, and runtime facade
interpretation remain explicit premises. These are not certificates for an
arbitrary Rust facade. The capacity parameter bounds represented queue length;
allocation/clone success is still part of the individual adapter contracts.
Neither cursor values nor note identifiers have a machine-integer bound.

The concrete runtime clone/peek/resolved/advance/get/index_equal/write instances
are not proved here.
Fits is only a logical slot-capacity condition, not a theorem about allocator
success. Vec::len, BigUint::to_usize, unwrap_or(MAX), saturating_sub, and initial
copy/replacement interpretation must satisfy the facade premises below.
-/

structure Representation (Q N α : Type) where
  queue : Q → ZoneQueue α
  note : N → α

def Fits (capacity : Nat) (queue : ZoneQueue α) : Prop :=
  queue.notes.length ≤ capacity

/-- Only successful point reads, a one-note predicate, and a one-cursor update.
    In particular, no field assumes a whole normalization step or loop. -/
structure CursorAtomicAgrees {Q N Ops α : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (resolved : α → Bool) (capacity : Nat) : Prop where
  peek : ∀ queue, Fits capacity (rep.queue queue) →
    ∃ found, call.peek ops queue = ok found ∧
      found.map rep.note = (rep.queue queue).peek
  resolved : ∀ note, call.resolved ops note = ok (resolved (rep.note note))
  advance : ∀ queue, Fits capacity (rep.queue queue) →
    (rep.queue queue).peek ≠ none →
    ∃ next, call.advance ops queue = ok next ∧
      rep.queue next = (rep.queue queue).advance

private theorem decrement_fuel (fuel : Usize) (hpositive : 0 < fuel.val) :
    ∃ next, fuel - 1#usize = ok next ∧ next.val + 1 = fuel.val := by
  obtain ⟨next, hnext, hvalue, _⟩ := spec_imp_exists
    (UScalar.sub_spec (x := fuel) (y := 1#usize) (by scalar_tac))
  exact ⟨next, hnext, by scalar_tac⟩

private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hvalue⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hvalue⟩

private theorem positive_of_nonzero (fuel : Usize) (hne : fuel ≠ 0#usize) :
    0 < fuel.val := by
  by_contra h
  apply hne
  apply UScalar.eq_of_val_eq
  have hz : fuel.val = 0 := by omega
  simpa using hz

/-- One unfolding of the already-bridged reference, not another recursion. -/
theorem normalize_reference_step (resolved : α → Bool) (fuel : Nat)
    (queue : ZoneQueue α) (note : α) (hpeek : queue.peek = some note)
    (hresolved : resolved note = true) :
    normalizeFuel resolved (fuel + 1) queue =
      normalizeFuel resolved fuel queue.advance := by
  conv => lhs; unfold normalizeFuel
  simp only [hpeek, hresolved, ↓reduceIte]

theorem normalize_reference_blocked (resolved : α → Bool) (fuel : Nat)
    (queue : ZoneQueue α) (note : α) (hpeek : queue.peek = some note)
    (hresolved : resolved note = false) :
    normalizeFuel resolved fuel queue = queue := by
  cases fuel with
  | zero => exact normalize_zero resolved queue
  | succ fuel =>
      conv => lhs; unfold normalizeFuel
      simp only [hpeek, hresolved, Bool.false_eq_true, ↓reduceIte]

/-- Fuel zero still performs the actual peek. Its successful result is enough:
    neither resolved nor advance has any premise in this theorem. -/
theorem normalize_body_zero {Q N Ops : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (queue : Q) (found : Option N) (hpeek : call.peek ops queue = ok found) :
    queue_maintenance.normalize_cursor_fuel_loop.body call ops queue 0#usize =
      ok (.done queue) := by
  cases found <;>
    simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek,
      bind_tc_ok, ↓reduceIte]

/-- This equation retains the peek effect even for zero fuel: failure and
    divergence are not silently turned into a successful no-op. It assumes
    nothing about any adapter's success. -/
theorem normalize_body_zero_order {Q N Ops : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops) (queue : Q) :
    queue_maintenance.normalize_cursor_fuel_loop.body call ops queue 0#usize =
      (do let _ ← call.peek ops queue; ok (.done queue)) := by
  cases hpeek : call.peek ops queue with
  | ok found =>
      cases found <;>
        simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek,
          bind_tc_ok, ↓reduceIte]
  | fail error =>
      simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek, bind_tc_fail]
  | div =>
      simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek, bind_tc_div]

/-- In particular, an unsuccessful peek propagates before every fuel guard. -/
theorem normalize_body_peek_failure {Q N Ops : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (queue : Q) (fuel : Usize) (error : Error)
    (hpeek : call.peek ops queue = fail error) :
    queue_maintenance.normalize_cursor_fuel_loop.body call ops queue fuel = fail error := by
  simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek, bind_tc_fail]

/-- Empty peek returns before the fuel guard, for every machine fuel. -/
theorem normalize_body_empty {Q N Ops : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (queue : Q) (fuel : Usize) (hpeek : call.peek ops queue = ok none) :
    queue_maintenance.normalize_cursor_fuel_loop.body call ops queue fuel =
      ok (.done queue) := by
  simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek, bind_tc_ok]

/-- Termination is measured by the actual extracted fuel component. The
    invariant resumes the existing reference normalizeFuel at that fuel. -/
theorem normalize_loop_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (resolved : α → Bool) (capacity : Nat)
    (atoms : CursorAtomicAgrees call ops rep resolved capacity)
    (initial : Q) (fuel : Usize) (hfit : Fits capacity (rep.queue initial)) :
    queue_maintenance.normalize_cursor_fuel_loop call initial fuel ops ⦃ out =>
      rep.queue out = normalizeFuel resolved fuel.val (rep.queue initial) ⦄ := by
  unfold queue_maintenance.normalize_cursor_fuel_loop
  apply loop.spec_decr_nat (fun (p : Q × Usize) => p.2.val)
    (fun p => Fits capacity (rep.queue p.1) ∧
      normalizeFuel resolved p.2.val (rep.queue p.1) =
        normalizeFuel resolved fuel.val (rep.queue initial))
  · rintro ⟨queue, remainingFuel⟩ ⟨hcapacity, hresume⟩
    obtain ⟨found, hpeek, hfound⟩ := atoms.peek queue hcapacity
    cases found with
    | none =>
        have hreference : (rep.queue queue).peek = none := by
          simpa only [Option.map_none] using hfound.symm
        have hdone : rep.queue queue =
            normalizeFuel resolved fuel.val (rep.queue initial) :=
          (normalize_empty resolved remainingFuel.val (rep.queue queue) hreference).symm.trans
            hresume
        apply exists_imp_spec
        exact ⟨.done queue, normalize_body_empty call ops queue remainingFuel hpeek, hdone⟩
    | some note =>
        have hreference : (rep.queue queue).peek = some (rep.note note) := by
          simpa only [Option.map_some] using hfound.symm
        by_cases hzero : remainingFuel = 0#usize
        · have hdone : rep.queue queue =
              normalizeFuel resolved fuel.val (rep.queue initial) := by
            simpa [hzero, normalize_zero] using hresume
          apply exists_imp_spec
          refine ⟨.done queue, ?_, hdone⟩
          simpa only [hzero] using normalize_body_zero call ops queue (some note) hpeek
        · have hpositive := positive_of_nonzero remainingFuel hzero
          have hresolved := atoms.resolved note
          cases hcondition : resolved (rep.note note) with
          | false =>
              have hdone : rep.queue queue =
                  normalizeFuel resolved fuel.val (rep.queue initial) :=
                (normalize_reference_blocked resolved remainingFuel.val
                  (rep.queue queue) (rep.note note) hreference hcondition).symm.trans hresume
              apply exists_imp_spec
              refine ⟨.done queue, ?_, hdone⟩
              simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek,
                bind_tc_ok, hzero, ↓reduceIte, hresolved, hcondition, Bool.false_eq_true]
          | true =>
              obtain ⟨nextQueue, hadvance, hnextQueue⟩ := atoms.advance queue hcapacity (by
                rw [hreference]; simp)
              obtain ⟨nextFuel, hsubtract, hnextFuel⟩ :=
                decrement_fuel remainingFuel hpositive
              have hstep : normalizeFuel resolved remainingFuel.val (rep.queue queue) =
                  normalizeFuel resolved nextFuel.val (rep.queue queue).advance := by
                rw [← hnextFuel]
                exact normalize_reference_step resolved nextFuel.val (rep.queue queue)
                  (rep.note note) hreference hcondition
              have hnextFit : Fits capacity (rep.queue nextQueue) := by
                simpa only [Fits, hnextQueue, ZoneQueue.advance] using hcapacity
              have hnextResume : normalizeFuel resolved nextFuel.val (rep.queue nextQueue) =
                  normalizeFuel resolved fuel.val (rep.queue initial) := by
                rw [hnextQueue]
                exact hstep.symm.trans hresume
              apply exists_imp_spec
              refine ⟨.cont (nextQueue, nextFuel), ?_,
                ⟨hnextFit, hnextResume⟩, ?_⟩
              · simp only [queue_maintenance.normalize_cursor_fuel_loop.body, hpeek,
                  bind_tc_ok, hzero, ↓reduceIte, hresolved, hcondition,
                  hadvance, hsubtract]
              · dsimp only
                omega
  · exact ⟨hfit, rfl⟩

theorem normalize_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (resolved : α → Bool) (capacity : Nat)
    (atoms : CursorAtomicAgrees call ops rep resolved capacity)
    (initial : Q) (fuel : Usize) (hfit : Fits capacity (rep.queue initial)) :
    queue_maintenance.normalize_cursor_fuel call fuel initial ops ⦃ out =>
      rep.queue out = normalizeFuel resolved fuel.val (rep.queue initial) ∧
      (rep.queue out).notes = (rep.queue initial).notes ∧
      Fits capacity (rep.queue out) ⦄ := by
  obtain ⟨out, hrun, hreference⟩ := spec_imp_exists
    (normalize_loop_spec call ops rep resolved capacity atoms initial fuel hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, hreference, ?_, ?_⟩
  · rw [hreference, normalize_notes]
  · simpa only [Fits, hreference, normalize_notes] using hfit

/-- Out-of-bounds cursors are retained exactly, including arbitrary Nat values
    beyond Usize.max. There is no cursor-clamping or cursor-validity premise. -/
theorem normalize_out_of_bounds_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (resolved : α → Bool) (capacity : Nat)
    (atoms : CursorAtomicAgrees call ops rep resolved capacity)
    (initial : Q) (fuel : Usize) (hfit : Fits capacity (rep.queue initial))
    (houtside : (rep.queue initial).notes.length ≤ (rep.queue initial).currentIndex) :
    queue_maintenance.normalize_cursor_fuel call fuel initial ops ⦃ out =>
      rep.queue out = rep.queue initial ⦄ := by
  obtain ⟨out, hrun, hreference, _, _⟩ := spec_imp_exists
    (normalize_spec call ops rep resolved capacity atoms initial fuel hfit)
  have hempty := remaining_zero_peek (rep.queue initial) (by
    unfold remaining; omega)
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [normalize_empty resolved _ _ hempty] using hreference

/-- Value-level model of to_usize().unwrap_or(usize::MAX) followed by
    saturating_sub. It is not a second extracted runtime facade. -/
def remainingFacadeValue (len : Usize) (convertedCursor : Option Usize) : Nat :=
  match convertedCursor with
  | some index => len.val - index.val
  | none => len.val - UScalar.max .Usize

/-- The runtime's BigUint conversion needs its own concrete proof. -/
def CursorConversionAgrees (cursor : Nat) (converted : Option Usize) : Prop :=
  match converted with
  | some index => index.val = cursor
  | none => UScalar.max .Usize < cursor

theorem remaining_facade_value (queue : ZoneQueue α) (len : Usize)
    (converted : Option Usize) (hlen : len.val = queue.notes.length)
    (hconversion : CursorConversionAgrees queue.currentIndex converted) :
    remainingFacadeValue len converted = remaining queue := by
  have hmax : len.val ≤ UScalar.max .Usize := by scalar_tac
  cases converted with
  | none =>
      change UScalar.max .Usize < queue.currentIndex at hconversion
      simp only [remainingFacadeValue, remaining]
      omega
  | some index =>
      change index.val = queue.currentIndex at hconversion
      simp only [remainingFacadeValue, remaining, hlen, hconversion]

theorem remaining_representable (queue : ZoneQueue α) (len : Usize)
    (hlen : len.val = queue.notes.length) : remaining queue ≤ UScalar.max .Usize := by
  have hmax : len.val ≤ UScalar.max .Usize := by scalar_tac
  unfold remaining
  omega

/-- No machine remaining+1 is needed. Initial copy, length, conversion and
    saturating-sub values are facade premises; only their arithmetic is closed. -/
theorem normalize_from_facade_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (resolved : α → Bool) (capacity : Nat)
    (atoms : CursorAtomicAgrees call ops rep resolved capacity)
    (initial : Q) (source : ZoneQueue α) (len fuel : Usize)
    (converted : Option Usize) (hcopy : rep.queue initial = source)
    (hfit : Fits capacity source) (hlen : len.val = source.notes.length)
    (hconversion : CursorConversionAgrees source.currentIndex converted)
    (hfuel : fuel.val = remainingFacadeValue len converted) :
    queue_maintenance.normalize_cursor_fuel call fuel initial ops ⦃ out =>
      rep.queue out = normalizeFuel resolved (remaining source + 1) source ∧
      (rep.queue out).notes = source.notes ⦄ := by
  have hremaining : fuel.val = remaining source :=
    hfuel.trans (remaining_facade_value source len converted hlen hconversion)
  obtain ⟨out, hrun, hreference, hnotes, _⟩ := spec_imp_exists
    (normalize_spec call ops rep resolved capacity atoms initial fuel (by
      simpa only [hcopy] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_, ?_⟩
  · rw [hcopy, hremaining] at hreference
    exact hreference.trans (fuel_irrelevant resolved (remaining source)
      (remaining source + 1) source (Nat.le_refl _) (by omega))
  · simpa only [hcopy] using hnotes

theorem normalize_hold_equiv {Q N Ops : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N HoldNote) (capacity : Nat)
    (atoms : CursorAtomicAgrees call ops rep holdQueueResolved capacity)
    (initial : Q) (source : ZoneQueue HoldNote) (fuel : Usize)
    (hcopy : rep.queue initial = source) (hfit : Fits capacity source)
    (hfuel : fuel.val = remaining source) :
    queue_maintenance.normalize_cursor_fuel call fuel initial ops ⦃ out =>
      rep.queue out = normalizeHoldQueueCursor source ⦄ := by
  obtain ⟨out, hrun, hreference, _, _⟩ := spec_imp_exists
    (normalize_spec call ops rep holdQueueResolved capacity atoms initial fuel (by
      simpa only [hcopy] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hcopy, hfuel, hold_pending_equiv] using hreference

theorem normalize_touch_equiv {Q N Ops : Type}
    (call : queue_maintenance.QueueCursor Ops Q N) (ops : Ops)
    (rep : Representation Q N TouchNote) (capacity : Nat)
    (atoms : CursorAtomicAgrees call ops rep touchQueueResolved capacity)
    (initial : Q) (source : ZoneQueue TouchNote) (fuel : Usize)
    (hcopy : rep.queue initial = source) (hfit : Fits capacity source)
    (hfuel : fuel.val = remaining source) :
    queue_maintenance.normalize_cursor_fuel call fuel initial ops ⦃ out =>
      rep.queue out = normalizeTouchQueueCursor source ⦄ := by
  obtain ⟨out, hrun, hreference, _, _⟩ := spec_imp_exists
    (normalize_spec call ops rep touchQueueResolved capacity atoms initial fuel (by
      simpa only [hcopy] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hcopy, hfuel, touch_pending_equiv] using hreference

/-- The replacement payload is chosen by an unbounded reference identifier.
    No uniqueness or machine-representability premise is imposed on it. -/
def replaceOne (id : α → Nat) (replacement existing : α) : α :=
  if id existing == id replacement then replacement else existing

private theorem queue_eq_of_fields {left right : ZoneQueue α}
    (hnotes : left.notes = right.notes) (hcursor : left.currentIndex = right.currentIndex) :
    left = right := by
  cases left
  cases right
  cases hnotes
  cases hcursor
  rfl

/-- Each read/write mentions exactly one physical slot. A write retains the
    queue's entire Nat cursor and all other slots; index_equal compares the
    unbounded note identifiers. None of these fields assumes a traversal. -/
structure ReplaceAtomicAgrees {Q N Ops α : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (id : α → Nat) (capacity : Nat) : Prop where
  get : ∀ (queue : Q) (index : Usize) (pre : List α) (old : α) (rest : List α),
    Fits capacity (rep.queue queue) →
    (rep.queue queue).notes = pre ++ old :: rest → pre.length = index.val →
    ∃ found, call.get ops queue index = ok found ∧ rep.note found = old
  index_equal : ∀ existing replacement,
    call.index_equal ops existing replacement =
      ok (id (rep.note existing) == id (rep.note replacement))
  write : ∀ (queue : Q) (index : Usize) (pre : List α) (old : α) (rest : List α)
    (replacement : N),
    Fits capacity (rep.queue queue) →
    (rep.queue queue).notes = pre ++ old :: rest → pre.length = index.val →
    ∃ next, call.write ops queue index replacement = ok next ∧
      rep.queue next = { rep.queue queue with notes := pre ++ rep.note replacement :: rest }

/-- Composition of the real get, identifier comparison, conditional write,
    and machine increment. Even len = Usize.max needs no len+1 value. -/
theorem replace_body_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (id : α → Nat) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep id capacity)
    (len : Usize) (replacement : N) (queue : Q) (index : Usize)
    (pre : List α) (old : α) (rest : List α)
    (hfit : Fits capacity (rep.queue queue))
    (hnotes : (rep.queue queue).notes = pre ++ old :: rest)
    (hindex : pre.length = index.val) (hlt : index.val < len.val) :
    ∃ nextQueue nextIndex,
      queue_maintenance.replace_all_loop.body call len replacement ops queue index =
        ok (.cont (nextQueue, nextIndex)) ∧
      rep.queue nextQueue =
        { rep.queue queue with notes := pre ++ replaceOne id (rep.note replacement) old :: rest } ∧
      nextIndex.val = index.val + 1 ∧ Fits capacity (rep.queue nextQueue) := by
  obtain ⟨found, hget, hfound⟩ := atoms.get queue index pre old rest hfit hnotes hindex
  have hequal : call.index_equal ops found replacement =
      ok (id old == id (rep.note replacement)) := by
    simpa only [hfound] using atoms.index_equal found replacement
  have hupdate : ∃ nextQueue,
      (if id old == id (rep.note replacement)
        then call.write ops queue index replacement else ok queue) = ok nextQueue ∧
      rep.queue nextQueue =
        { rep.queue queue with notes := pre ++ replaceOne id (rep.note replacement) old :: rest } := by
    cases hmatch : (id old == id (rep.note replacement)) with
    | false =>
        refine ⟨queue, ?_, ?_⟩
        · simp only [hmatch, Bool.false_eq_true, ↓reduceIte]
        · apply queue_eq_of_fields
          · simpa only [replaceOne, hmatch, Bool.false_eq_true, ↓reduceIte] using hnotes
          · rfl
    | true =>
        obtain ⟨updated, hwrite, hupdated⟩ :=
          atoms.write queue index pre old rest replacement hfit hnotes hindex
        refine ⟨updated, ?_, ?_⟩
        · simpa only [hmatch, ↓reduceIte] using hwrite
        · simpa only [replaceOne, hmatch, ↓reduceIte] using hupdated
  obtain ⟨nextQueue, hupdate, hnextQueue⟩ := hupdate
  obtain ⟨nextIndex, hincrement, hnextIndex⟩ := increment_index index len hlt
  have hguard : index < len := by simpa using hlt
  have hlength : (rep.queue nextQueue).notes.length = (rep.queue queue).notes.length := by
    rw [hnextQueue, hnotes]
    simp only [List.length_append, List.length_cons]
  refine ⟨nextQueue, nextIndex, ?_, hnextQueue, hnextIndex, ?_⟩
  · simp only [queue_maintenance.replace_all_loop.body, hguard, ↓reduceIte,
      hget, bind_tc_ok, hequal, hupdate, hincrement]
  · simpa only [Fits, hlength] using hfit

/-- Stopping at len does not call get, index_equal, write, or increment. -/
theorem replace_body_done {Q N Ops : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (len : Usize) (replacement : N) (queue : Q) (index : Usize)
    (hstop : len.val ≤ index.val) :
    queue_maintenance.replace_all_loop.body call len replacement ops queue index =
      ok (.done queue) := by
  have hguard : ¬ index < len := by simpa using (Nat.not_lt.mpr hstop)
  simp only [queue_maintenance.replace_all_loop.body, hguard, ↓reduceIte]

/-- A mapped prefix and unchanged suffix cover every original physical slot.
    The queue cursor is independent of the loop index and stays unchanged. -/
def ReplaceInv {Q N α : Type} (rep : Representation Q N α) (id : α → Nat)
    (replacement : α) (capacity : Nat) (original : ZoneQueue α)
    (queue : Q) (index : Usize) : Prop :=
  Fits capacity (rep.queue queue) ∧
    ∃ pre rest : List α, original.notes = pre ++ rest ∧
      (rep.queue queue).notes = pre.map (replaceOne id replacement) ++ rest ∧
      pre.length = index.val ∧ (rep.queue queue).currentIndex = original.currentIndex

/-- The actual extracted all-slots loop refines the reference map. Matching a
    note never terminates the loop; duplicate identifiers are all visited. -/
theorem replace_loop_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (id : α → Nat) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep id capacity)
    (len : Usize) (original : ZoneQueue α) (initial : Q)
    (replacement : N) (start : Usize) (hlen : len.val = original.notes.length)
    (hinit : ReplaceInv rep id (rep.note replacement) capacity original initial start) :
    queue_maintenance.replace_all_loop call len initial replacement ops start ⦃ out =>
      rep.queue out = replace id original (rep.note replacement) ⦄ := by
  unfold queue_maintenance.replace_all_loop
  apply loop.spec_decr_nat (fun (p : Q × Usize) => len.val - p.2.val)
    (fun p => ReplaceInv rep id (rep.note replacement) capacity original p.1 p.2)
  · rintro ⟨queue, index⟩ ⟨hfit, pre, rest, horiginal, hstate, hindex, hcursor⟩
    dsimp only at hfit horiginal hstate hindex hcursor ⊢
    have hlength : len.val = pre.length + rest.length := by
      rw [hlen, horiginal, List.length_append]
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil =>
          simp only [List.length_nil, Nat.add_zero] at hlength
          omega
      | cons old rest =>
          obtain ⟨nextQueue, nextIndex, hbody, hnextQueue, hnextIndex, hnextFit⟩ :=
            replace_body_spec call ops rep id capacity atoms len replacement queue index
              (pre.map (replaceOne id (rep.note replacement))) old rest hfit hstate
              (by simpa only [List.length_map] using hindex) hlt
          have hnextNotes : (rep.queue nextQueue).notes =
              pre.map (replaceOne id (rep.note replacement)) ++
                replaceOne id (rep.note replacement) old :: rest := by
            rw [hnextQueue]
          have hnextCursor : (rep.queue nextQueue).currentIndex = original.currentIndex := by
            rw [hnextQueue]
            exact hcursor
          apply exists_imp_spec
          refine ⟨.cont (nextQueue, nextIndex), hbody, ?_, ?_⟩
          · refine ⟨hnextFit, pre ++ [old], rest, ?_, ?_, ?_, hnextCursor⟩
            · simpa only [List.append_assoc, List.singleton_append] using horiginal
            · simpa only [List.map_append, List.map_cons, List.map_nil,
                List.append_assoc, List.singleton_append] using hnextNotes
            · simp only [List.length_append, List.length_cons, List.length_nil,
                Nat.zero_add, hindex, hnextIndex]
          · dsimp only
            omega
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head tail =>
            simp only [List.length_cons] at hlength
            omega
      have hnotes : (rep.queue queue).notes =
          original.notes.map (replaceOne id (rep.note replacement)) := by
        simpa only [horiginal, hempty, List.append_nil] using hstate
      apply exists_imp_spec
      refine ⟨.done queue, replace_body_done call ops len replacement queue index hge, ?_⟩
      apply queue_eq_of_fields
      · exact hnotes
      · exact hcursor
  · exact hinit

/-- Full wrapper, starting at physical slot zero independently of currentIndex.
    hlen represents Vec::len; capacity and point-operation success are explicit. -/
theorem replace_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (id : α → Nat) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep id capacity)
    (len : Usize) (initial : Q) (replacement : N)
    (hlen : len.val = (rep.queue initial).notes.length)
    (hfit : Fits capacity (rep.queue initial)) :
    queue_maintenance.replace_all call len initial replacement ops ⦃ out =>
      rep.queue out = replace id (rep.queue initial) (rep.note replacement) ∧
      (rep.queue out).currentIndex = (rep.queue initial).currentIndex ∧
      (rep.queue out).notes.length = (rep.queue initial).notes.length ∧
      Fits capacity (rep.queue out) ⦄ := by
  have hinit : ReplaceInv rep id (rep.note replacement) capacity
      (rep.queue initial) initial 0#usize := by
    refine ⟨hfit, [], (rep.queue initial).notes, ?_, ?_, ?_, rfl⟩ <;> simp
  obtain ⟨out, hrun, hreference⟩ := spec_imp_exists
    (replace_loop_spec call ops rep id capacity atoms len (rep.queue initial) initial
      replacement 0#usize hlen hinit)
  apply exists_imp_spec
  refine ⟨out, hrun, hreference, ?_, ?_, ?_⟩
  · rw [hreference, replace_preserves_index]
  · rw [hreference, replace_preserves_length]
  · simpa only [Fits, hreference, replace_preserves_length] using hfit

private theorem map_lookup (f : α → β) (notes : List α) (index : Nat) :
    (notes.map f)[index]? = (notes[index]?).map f := by
  induction notes generalizing index with
  | nil => simp
  | cons head tail ih =>
      cases index with
      | zero => rfl
      | succ index => simpa using ih index

/-- Position-by-position correspondence, including absent/out-of-bounds slots,
    is the order statement; merely retaining length would be weaker. -/
theorem replace_lookup (id : α → Nat) (queue : ZoneQueue α)
    (replacement : α) (slot : Nat) :
    (replace id queue replacement).notes[slot]? =
      (queue.notes[slot]?).map (replaceOne id replacement) := by
  exact map_lookup (replaceOne id replacement) queue.notes slot

theorem replace_order_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (id : α → Nat) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep id capacity)
    (len : Usize) (initial : Q) (replacement : N)
    (hlen : len.val = (rep.queue initial).notes.length)
    (hfit : Fits capacity (rep.queue initial)) :
    queue_maintenance.replace_all call len initial replacement ops ⦃ out =>
      (rep.queue out).currentIndex = (rep.queue initial).currentIndex ∧
      (rep.queue out).notes.length = (rep.queue initial).notes.length ∧
      ∀ slot : Nat, (rep.queue out).notes[slot]? =
        ((rep.queue initial).notes[slot]?).map (replaceOne id (rep.note replacement)) ⦄ := by
  obtain ⟨out, hrun, hreference, hcursor, hlength, _⟩ := spec_imp_exists
    (replace_spec call ops rep id capacity atoms len initial replacement hlen hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, hcursor, hlength, ?_⟩
  intro slot
  rw [hreference]
  exact replace_lookup id (rep.queue initial) (rep.note replacement) slot

/-- Every matching position, not only a first occurrence, contains replacement.
    All nonmatching positions retain the original note. The quantifier covers
    positions before/at/after the queue cursor, which is not a loop bound. -/
theorem replace_all_matches_spec {Q N Ops α : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N α) (id : α → Nat) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep id capacity)
    (len : Usize) (initial : Q) (replacement : N)
    (hlen : len.val = (rep.queue initial).notes.length)
    (hfit : Fits capacity (rep.queue initial)) :
    queue_maintenance.replace_all call len initial replacement ops ⦃ out =>
      ∀ (slot : Nat) (old : α), (rep.queue initial).notes[slot]? = some old →
        (id old = id (rep.note replacement) →
          (rep.queue out).notes[slot]? = some (rep.note replacement)) ∧
        (id old ≠ id (rep.note replacement) →
          (rep.queue out).notes[slot]? = some old) ⦄ := by
  obtain ⟨out, hrun, _, _, hslots⟩ := spec_imp_exists
    (replace_order_spec call ops rep id capacity atoms len initial replacement hlen hfit)
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

theorem replace_hold_equiv {Q N Ops : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N HoldNote) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep (fun note => note.params.noteIndex) capacity)
    (len : Usize) (initial : Q) (replacement : N)
    (source : ZoneQueue HoldNote) (note : HoldNote)
    (hcopy : rep.queue initial = source) (hnote : rep.note replacement = note)
    (hlen : len.val = source.notes.length) (hfit : Fits capacity source) :
    queue_maintenance.replace_all call len initial replacement ops ⦃ out =>
      rep.queue out = replaceHoldQueueNote source note ⦄ := by
  obtain ⟨out, hrun, hreference, _, _, _⟩ := spec_imp_exists
    (replace_spec call ops rep (fun note => note.params.noteIndex) capacity atoms len
      initial replacement (by simpa only [hcopy] using hlen)
      (by simpa only [hcopy] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hcopy, hnote, replace_hold_eq] using hreference

theorem replace_touch_equiv {Q N Ops : Type}
    (call : queue_maintenance.QueueReplace Ops Q N) (ops : Ops)
    (rep : Representation Q N TouchNote) (capacity : Nat)
    (atoms : ReplaceAtomicAgrees call ops rep (fun note => note.params.noteIndex) capacity)
    (len : Usize) (initial : Q) (replacement : N)
    (source : ZoneQueue TouchNote) (note : TouchNote)
    (hcopy : rep.queue initial = source) (hnote : rep.note replacement = note)
    (hlen : len.val = source.notes.length) (hfit : Fits capacity source) :
    queue_maintenance.replace_all call len initial replacement ops ⦃ out =>
      rep.queue out = replaceTouchQueueNote source note ⦄ := by
  obtain ⟨out, hrun, hreference, _, _, _⟩ := spec_imp_exists
    (replace_spec call ops rep (fun note => note.params.noteIndex) capacity atoms len
      initial replacement (by simpa only [hcopy] using hlen)
      (by simpa only [hcopy] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  simpa only [hcopy, hnote, replace_touch_eq] using hreference

end Verification.QueueMaintenanceProof
