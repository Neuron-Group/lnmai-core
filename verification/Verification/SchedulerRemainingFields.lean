import Verification.QueueMaintenanceComposition

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerRemainingFieldsProof

open QueueMaintenanceModel QueueMaintenanceProof QueueMaintenanceComposition

/-!
UNVALIDATED SOURCE: main owns the only Lean worker.

Concrete scalar witnesses for the runtime queue facade. These are compositions
of the actual Aeneas unwrap_or, saturating_sub, Vec.len and checked addition,
not a second queue traversal. The outer runtime facade itself is not extracted.

Click use, input counts, current_index and shared counters in scheduler.rs use
Natural(BigUint). No theorem here identifies them with usize. Conversion from
BigUint and physical allocation success remain separate point obligations.
-/

/-- The same saturating primitive used by the runtime facade. -/
theorem saturating_sub_val (left right : Usize) :
    (core.num.Usize.saturating_sub left right).val = left.val - right.val := by
  change (BitVec.ofNat _ (max 0 (left.val - right.val))).toNat = left.val - right.val
  simp only [Nat.zero_max, BitVec.toNat_ofNat]
  exact Nat.mod_eq_of_lt (lt_of_le_of_lt (Nat.sub_le _ _) left.bv.isLt)

@[simp] theorem maximum_val : (core.num.Usize.MAX).val = Usize.max := by
  simp only [core.num.Usize.MAX, UScalar.ofNatCore_val_eq]

/-- Total even at MAX and for an unconvertible Natural cursor. -/
def remainingScalar (len : Usize) (converted : Option Usize) : Usize :=
  core.num.Usize.saturating_sub len
    (core.option.Option.unwrap_or converted core.num.Usize.MAX)

theorem remainingScalar_val (len : Usize) (converted : Option Usize) :
    (remainingScalar len converted).val = remainingFacadeValue len converted := by
  cases converted with
  | none =>
      simp only [remainingScalar, core.option.Option.unwrap_or_none,
        saturating_sub_val, maximum_val, remainingFacadeValue, UScalar.max_USize_eq]
  | some index =>
      simp only [remainingScalar, core.option.Option.unwrap_or_some,
        saturating_sub_val, remainingFacadeValue]

theorem remainingScalar_none (len : Usize) :
    (remainingScalar len none).val = 0 := by
  rw [remainingScalar_val]
  change len.val - UScalar.max .Usize = 0
  have hmax : len.val ≤ UScalar.max .Usize := by scalar_tac
  omega

/-- A real checked addition witness; the bound is supplied by this particular
scalar input, rather than by an impossible all-values successor law. -/
theorem increment_below (used supplied : Usize) (havailable : used.val < supplied.val) :
    ∃ next, used + 1#usize = ok next ∧
      next.val = used.val + 1 ∧ next.val ≤ supplied.val := by
  have hbound : used.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hrun, hvalue⟩ := spec_imp_exists
    (UScalar.add_spec (x := used) (y := 1#usize) hbound)
  have hv : next.val = used.val + 1 := by simpa only [UScalar.ofNatCore_val_eq] using hvalue
  exact ⟨next, hrun, hv, by omega⟩

/-- Runtime process_touch_queue_head uses ordinary addition after saturation. -/
def headFuelScalar (len : Usize) (converted : Option Usize) : Result Usize :=
  remainingScalar len converted + 1#usize

theorem headFuelScalar_spec (len : Usize) (converted : Option Usize)
    (headroom : remainingFacadeValue len converted + 1 ≤ Usize.max) :
    ∃ fuel, headFuelScalar len converted = ok fuel ∧
      fuel.val = remainingFacadeValue len converted + 1 := by
  have hbound : (remainingScalar len converted).val + (1#usize).val ≤
      UScalar.max .Usize := by
    simpa only [remainingScalar_val, UScalar.ofNatCore_val_eq,
      UScalar.max_USize_eq] using headroom
  obtain ⟨fuel, hrun, hvalue⟩ := spec_imp_exists
    (UScalar.add_spec (x := remainingScalar len converted) (y := 1#usize) hbound)
  refine ⟨fuel, hrun, ?_⟩
  simpa only [remainingScalar_val, UScalar.ofNatCore_val_eq] using hvalue

/-- A failed conversion denotes an out-of-range Natural cursor. Even when
len = MAX this branch has zero remaining and needs exactly fuel one. -/
theorem headFuelScalar_none_spec (len : Usize) :
    ∃ fuel, headFuelScalar len none = ok fuel ∧ fuel.val = 1 := by
  have hzero : remainingFacadeValue len none = 0 :=
    (remainingScalar_val len none).symm.trans (remainingScalar_none len)
  obtain ⟨fuel, hrun, hvalue⟩ := headFuelScalar_spec len none (by
    rw [hzero]
    scalar_tac)
  exact ⟨fuel, hrun, by simpa only [hzero, Nat.zero_add] using hvalue⟩

/-- MAX is inhabited in the actual scalar representation. MAX + 1 cannot be
made total by asking for a larger returned scalar. This concerns usize fuel,
not the runtime's Natural counters. -/
theorem headFuelScalar_max_zero_no_result :
    ¬ ∃ fuel, headFuelScalar core.num.Usize.MAX (some 0#usize) = ok fuel := by
  rintro ⟨fuel, hrun⟩
  change remainingScalar core.num.Usize.MAX (some 0#usize) + 1#usize = ok fuel at hrun
  have hsum := UScalar.add_equiv
    (remainingScalar core.num.Usize.MAX (some 0#usize)) (1#usize)
  rw [hrun] at hsum
  have hvalue : fuel.val = Usize.max + 1 := by
    simpa only [remainingScalar_val, remainingFacadeValue, maximum_val,
      UScalar.ofNatCore_val_eq, Nat.sub_zero] using hsum.2.1
  have hmax : fuel.val ≤ Usize.max := by scalar_tac
  omega

/-- Exact local criterion, given an actual finite queue. A maximum-length queue
is allowed once the cursor has passed its first slot. -/
theorem headroom_iff (queue : ZoneQueue α)
    (finite : queue.notes.length ≤ Usize.max) :
    remaining queue + 1 ≤ Usize.max ↔
      queue.notes.length < Usize.max ∨ 0 < queue.currentIndex := by
  have hpositive : 0 < Usize.max := by scalar_tac
  unfold remaining
  omega

theorem headroom_of_length (queue : ZoneQueue α)
    (short : queue.notes.length < Usize.max) :
    remaining queue + 1 ≤ Usize.max := by
  unfold remaining
  omega

theorem headroom_of_positive_cursor (queue : ZoneQueue α)
    (finite : queue.notes.length ≤ Usize.max) (positive : 0 < queue.currentIndex) :
    remaining queue + 1 ≤ Usize.max :=
  (headroom_iff queue finite).2 (Or.inr positive)

/-- This supplies an explicit Option Usize witness for every mathematical
cursor. It is NOT the missing refinement proof for BigUint::to_usize. -/
theorem checked_conversion_agrees (cursor : Nat) :
    CursorConversionAgrees cursor (UScalar.tryMkOpt .Usize cursor) := by
  have h := UScalar.tryMkOpt_eq .Usize cursor
  cases converted : UScalar.tryMkOpt .Usize cursor with
  | none =>
      simp only [converted] at h
      change UScalar.max .Usize < cursor
      change ¬ cursor < 2 ^ UScalarTy.Usize.numBits at h
      have hmax : UScalar.max .Usize + 1 = 2 ^ UScalarTy.Usize.numBits := by
        simpa only [UScalar.max_USize_eq, UScalarTy.Usize_numBits_eq] using
          Usize.max_succ_eq_pow
      omega
  | some index =>
      simp only [converted] at h
      exact h.1

/-- Projection of actual finite storage plus individually exposed operations.
The cursor projection denotes the decoded Natural value, with no usize bound.
No arbitrary List-to-Vec encoder or frame-wide size hypothesis is used. -/
structure Fields (Q N α : Type) where
  notes : Q → alloc.vec.Vec N
  note : N → α
  cursor : Q → Nat
  copy : Q → Result Q
  convert : Q → Result (Option Usize)

def source {Q N α : Type} (fields : Fields Q N α) (queue : Q) : ZoneQueue α :=
  { notes := (fields.notes queue).val.map fields.note
    currentIndex := fields.cursor queue }

def representation {Q N α : Type} (fields : Fields Q N α) : Representation Q N α :=
  { queue := source fields, note := fields.note }

theorem source_finite {Q N α : Type} (fields : Fields Q N α) (queue : Q) :
    (source fields queue).notes.length ≤ Usize.max := by
  simpa only [source, List.length_map] using (fields.notes queue).property

theorem length_value {Q N α : Type} (fields : Fields Q N α) (queue : Q) :
    (alloc.vec.Vec.len (fields.notes queue)).val = (source fields queue).notes.length := by
  simp only [source, List.length_map, alloc.vec.Vec.len_val, alloc.vec.Vec.length]

/-- Only copy and BigUint conversion remain. Length and remaining arithmetic
are constructed below, not required as whole-facade laws. -/
structure Points {Q N α : Type} (fields : Fields Q N α) (capacity : Nat) : Prop where
  copy : ∀ queue, Fits capacity (source fields queue) →
    ∃ copied, fields.copy queue = ok copied ∧ source fields copied = source fields queue
  convert : ∀ queue, Fits capacity (source fields queue) →
    ∃ converted, fields.convert queue = ok converted ∧
      CursorConversionAgrees (fields.cursor queue) converted

def facade {Ops Q N α : Type} (fields : Fields Q N α) : Facade Ops Q :=
  { length := fun _ queue => ok (alloc.vec.Vec.len (fields.notes queue))
    copy := fun _ queue => fields.copy queue
    cursor_to_usize := fun _ queue => fields.convert queue
    remaining := fun _ len converted => ok (remainingScalar len converted) }

theorem facade_agrees {Ops Q N α : Type} (fields : Fields Q N α)
    (ops : Ops) (capacity : Nat) (points : Points fields capacity) :
    FacadeAtomicAgrees (facade fields) ops (representation fields) capacity := by
  refine {
    length := ?_
    copy := points.copy
    cursor_to_usize := points.convert
    remaining := ?_ }
  · intro queue _
    exact ⟨alloc.vec.Vec.len (fields.notes queue), rfl, length_value fields queue⟩
  · intro len converted
    exact ⟨remainingScalar len converted, rfl, remainingScalar_val len converted⟩

/-- The order is the runtime order: length, cursor conversion, subtraction.
Each theorem below takes the conversion law only at this particular queue. -/
def queueRemaining {Q N α : Type} (fields : Fields Q N α) (queue : Q) : Result Usize := do
  let len := alloc.vec.Vec.len (fields.notes queue)
  let converted ← fields.convert queue
  ok (remainingScalar len converted)

def headFuel {Q N α : Type} (fields : Fields Q N α) (queue : Q) : Result Usize := do
  let len := alloc.vec.Vec.len (fields.notes queue)
  let converted ← fields.convert queue
  headFuelScalar len converted

theorem queueRemaining_spec {Q N α : Type} (fields : Fields Q N α)
    (queue : Q) (converted : Option Usize)
    (hconvert : fields.convert queue = ok converted)
    (hvalue : CursorConversionAgrees (fields.cursor queue) converted) :
    ∃ count, queueRemaining fields queue = ok count ∧
      count.val = remaining (source fields queue) := by
  refine ⟨remainingScalar (alloc.vec.Vec.len (fields.notes queue)) converted, ?_, ?_⟩
  · simp only [queueRemaining, hconvert, bind_tc_ok]
  · exact (remainingScalar_val _ _).trans
      (remaining_facade_value (source fields queue) _ converted
        (length_value fields queue) hvalue)

theorem headFuel_spec {Q N α : Type} (fields : Fields Q N α)
    (queue : Q) (converted : Option Usize)
    (hconvert : fields.convert queue = ok converted)
    (hvalue : CursorConversionAgrees (fields.cursor queue) converted)
    (headroom : remaining (source fields queue) + 1 ≤ Usize.max) :
    ∃ fuel, headFuel fields queue = ok fuel ∧
      fuel.val = remaining (source fields queue) + 1 := by
  have hremaining := remaining_facade_value (source fields queue)
    (alloc.vec.Vec.len (fields.notes queue)) converted (length_value fields queue) hvalue
  obtain ⟨fuel, hrun, hval⟩ := headFuelScalar_spec
    (alloc.vec.Vec.len (fields.notes queue)) converted (by rw [hremaining]; exact headroom)
  refine ⟨fuel, ?_, hval.trans (congrArg (fun n => n + 1) hremaining)⟩
  simp only [headFuel, hconvert, bind_tc_ok, hrun]

/-- Direct shape for TouchSchedulePassLocal.RemainingAtomsWithin.fuel. Its
only remaining law is conversion at the queue selected by the local guard.
A runtime installer sets fuel := headFuel fields and relates the queue field
projections to its Representation; this is not a whole-pass equality. -/
theorem fuel_field {Q N α : Type} (fields : Fields Q N α)
    (conversion : ∀ queue, remaining (source fields queue) + 1 ≤ Usize.max →
      ∃ converted, fields.convert queue = ok converted ∧
        CursorConversionAgrees (fields.cursor queue) converted) :
    ∀ queue, remaining (source fields queue) + 1 ≤ Usize.max →
      ∃ fuel, headFuel fields queue = ok fuel ∧
        fuel.val = remaining (source fields queue) + 1 := by
  intro queue headroom
  obtain ⟨converted, hrun, hvalue⟩ := conversion queue headroom
  exact headFuel_spec fields queue converted hrun hvalue headroom

/-- Non-vacuity witness for the scalar/finite-storage interface. The cursor is
a semantic Nat and its converter is the real scalar checked constructor.
This is not an interpretation of native BigUint or of heap allocation. -/
def representedFields (α : Type) : Fields (alloc.vec.Vec α × Nat) α α :=
  { notes := Prod.fst
    note := id
    cursor := Prod.snd
    copy := fun queue => ok queue
    convert := fun queue => ok (UScalar.tryMkOpt .Usize queue.2) }

theorem representedFields_points (α : Type) (capacity : Nat) :
    Points (representedFields α) capacity := by
  refine { copy := ?_, convert := ?_ }
  · intro queue _
    exact ⟨queue, rfl, rfl⟩
  · intro queue _
    exact ⟨UScalar.tryMkOpt .Usize queue.2, rfl, checked_conversion_agrees queue.2⟩

theorem representedFields_fuel (α : Type) (queue : alloc.vec.Vec α × Nat)
    (headroom : remaining (source (representedFields α) queue) + 1 ≤ Usize.max) :
    ∃ fuel, headFuel (representedFields α) queue = ok fuel ∧
      fuel.val = remaining (source (representedFields α) queue) + 1 := by
  exact headFuel_spec (representedFields α) queue (UScalar.tryMkOpt .Usize queue.2)
    rfl (checked_conversion_agrees queue.2) headroom

/-- An explicit inhabited finite-storage state satisfying the local guard;
this prevents non-vacuity from resting on an empty carrier or false bounds. -/
theorem empty_queue_fuel (α : Type) :
    ∃ fuel, headFuel (representedFields α) (alloc.vec.Vec.new α, 0) = ok fuel ∧
      fuel.val = 1 := by
  have hzero : remaining (source (representedFields α) (alloc.vec.Vec.new α, 0)) = 0 := rfl
  obtain ⟨fuel, hrun, hvalue⟩ := representedFields_fuel α
    (alloc.vec.Vec.new α, 0) (by rw [hzero]; scalar_tac)
  exact ⟨fuel, hrun, by simpa only [hzero, Nat.zero_add] using hvalue⟩

end Verification.SchedulerRemainingFieldsProof
