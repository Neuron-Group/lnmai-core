import Verification.TouchHoldGroups

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 800000

namespace Verification.TouchHoldBodyScansProof

open TouchHoldGroupsProof
open private scanBody scan_spec from Verification.TouchHoldGroups

/-!
Actual Generated touch_hold_body scanners, checked against extraction5 source
SHA256 61f040d6f4e25a3d202a3737085eef0fd13533994296e7570d6066ed7188c634.
This source has not been elaborated: only the main agent runs Lean.

The *_key_spec theorems use only a target-specific PartialEq.eq point law.
They inspect the actual generic record and the actual Vec.val fields, without
constructing another record. They can be instantiated with an Id -> Nat view,
but impose neither a numeric representation on Id nor injectivity of a view.
The unqualified *_spec theorems instead use LawfulPartialEq and prove first
matching RAW Id equality. Later duplicate IDs and field entries are unrestricted.

The common loop proof is the existing TouchHoldGroupsProof.scan_spec. Length
and indexing follow from the actual Slice/Vec definitions; checked increment
safety follows from index < len, including len = Usize.max. These scans allocate
and clone nothing. Natural representation, native allocation, UTF-8, the Rust
caller and other kernels are outside the theorem scopes. No Vec.is_empty or
Vec Default definition is used.
-/

abbrev Group (Id : Type) := touch_hold_body.TouchHoldBodyGroupState Id

/-- The only trait operation used by these scans is eq. Total success and
agreement with Lean equality are explicit obligations on the chosen dictionary. -/
def LawfulPartialEq {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) : Prop :=
  ∀ left right, eq.eq left right = ok (decide (left = right))

/-- First matching raw field, with the entire earlier prefix unequal.
This is a proposition about existing list entries, not a decoded record. -/
def FirstEq {α Id : Type} (field : α → Id) (items : List α) (target : Id) :
    Option Usize → Prop
  | none => ∀ item ∈ items, field item ≠ target
  | some index => ∃ pre item rest,
      At items index pre item rest ∧
      (∀ earlier ∈ pre, field earlier ≠ target) ∧ field item = target

private theorem slice_index_at {α : Type} (values : Slice α) (index : Usize)
    (pre : List α) (item : α) (rest : List α)
    (hat : At values.val index pre item rest) :
    Slice.index_usize values index = ok item := by
  have hitem : values.val[index.val]? = some item := by
    rw [hat.1, ← hat.2, List.getElem?_append_right (Nat.le_refl pre.length)]
    simp
  simp only [Slice.index_usize, Slice.getElem?_Usize_eq, hitem]

/-- Actual group scan from its wrapper's zero initial cursor. The supplied len
must be the slice's length; no len + 1 capacity premise is needed. -/
theorem find_group_loop_key_spec {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (id : Id) (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value id = ok (key value == target))
    (len : Usize) (hlen : groups.val.length = len.val) :
    touch_hold_body.find_group_loop eq groups id len 0#usize ⦃
      First (fun group : Group Id => key group.group_id) groups.val target ⦄ := by
  have hread : ∀ index pre item rest, At groups.val index pre item rest →
      touch_hold_body.group_matches eq groups index id =
        ok (key item.group_id == target) := by
    intro index pre item rest hat
    simp only [touch_hold_body.group_matches,
      slice_index_at groups index pre item rest hat, bind_tc_ok,
      core.cmp.impls.PartialEqShared.eq, heq]
  have hloop := scan_spec (fun group : Group Id => key group.group_id)
    groups.val target len
    (fun index => touch_hold_body.group_matches eq groups index id) hlen hread
  simpa only [touch_hold_body.find_group_loop,
    touch_hold_body.find_group_loop.body, scanBody] using hloop

/-- The real find_id loop has two adjacent binds (index, then compare).
Associativity regroups exactly those binds into scan_spec's point read. -/
theorem find_id_loop_key_spec {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (values : Slice Id) (note : Id) (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value note = ok (key value == target))
    (len : Usize) (hlen : values.val.length = len.val) :
    touch_hold_body.find_id_loop eq values note len 0#usize ⦃
      First key values.val target ⦄ := by
  let read : Usize → Result Bool := fun index => do
    let value ← Slice.index_usize values index
    core.cmp.impls.PartialEqShared.eq eq value note
  have hread : ∀ index pre item rest, At values.val index pre item rest →
      read index = ok (key item == target) := by
    intro index pre item rest hat
    simp only [read, slice_index_at values index pre item rest hat,
      bind_tc_ok, core.cmp.impls.PartialEqShared.eq, heq]
  have hloop := scan_spec key values.val target len read hlen hread
  have hbody :
      (fun index => touch_hold_body.find_id_loop.body eq values note len index) =
        scanBody read len := by
    funext index
    simp only [touch_hold_body.find_id_loop.body, scanBody, read, bind_assoc_eq]
  simpa only [touch_hold_body.find_id_loop, hbody] using hloop

theorem find_group_key_spec {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (id : Id) (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value id = ok (key value == target)) :
    touch_hold_body.find_group eq groups id ⦃
      First (fun group : Group Id => key group.group_id) groups.val target ⦄ := by
  simpa only [touch_hold_body.find_group, touch_hold_body.group_length, bind_tc_ok]
    using find_group_loop_key_spec eq groups id key target heq
      (Slice.len groups) (by simp)

theorem find_id_key_spec {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (values : Slice Id) (note : Id) (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value note = ok (key value == target)) :
    touch_hold_body.find_id eq values note ⦃ First key values.val target ⦄ := by
  simpa only [touch_hold_body.find_id] using
    find_id_loop_key_spec eq values note key target heq
      (Slice.len values) (by simp)

theorem find_member_key_spec {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value note = ok (key value == target))
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.find_member eq groups group note ⦃
      First key (groups.val[group.val]).member_note_indices.val target ⦄ := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  have hscan := find_id_key_spec eq
    (alloc.vec.Vec.deref (groups.val[group.val]).member_note_indices)
    note key target heq
  simpa only [touch_hold_body.find_member, hread, bind_tc_ok, hraw,
    alloc.vec.Vec.deref] using hscan

theorem find_trigger_key_spec {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value note = ok (key value == target))
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.find_trigger eq groups group note ⦃
      First key (groups.val[group.val]).triggered_note_indices.val target ⦄ := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  have hscan := find_id_key_spec eq
    (alloc.vec.Vec.deref (groups.val[group.val]).triggered_note_indices)
    note key target heq
  simpa only [touch_hold_body.find_trigger, hread, bind_tc_ok, hraw,
    alloc.vec.Vec.deref] using hscan

/-- The first-match postcondition entails the boolean existence query. -/
theorem first_isSome_eq_any {α : Type} (key : α → Nat) (items : List α)
    (target : Nat) (found : Option Usize) (h : First key items target found) :
    found.isSome = items.any (fun item => key item == target) := by
  cases found with
  | none =>
      have hany : items.any (fun item => key item == target) = false := by
        apply List.any_eq_false.mpr
        intro item hmem hmatch
        exact h item hmem (beq_iff_eq.mp hmatch)
      exact hany.symm
  | some index =>
      obtain ⟨pre, item, rest, hat, _, hmatch⟩ := h
      have hany : items.any (fun item => key item == target) = true := by
        apply List.any_eq_true.mpr
        refine ⟨item, ?_, beq_iff_eq.mpr hmatch⟩
        rw [hat.1]
        simp
      exact hany.symm

theorem has_member_key_eq {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value note = ok (key value == target))
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.has_member eq groups group note =
      ok ((groups.val[group.val]).member_note_indices.val.any
        (fun item => key item == target)) := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_member_key_spec eq groups group note key target heq hgroup)
  have hbool := first_isSome_eq_any key
    (groups.val[group.val]).member_note_indices.val target found hfirst
  simp only [touch_hold_body.has_member, hfind, bind_tc_ok,
    core.option.Option.is_some, hbool]

theorem has_trigger_key_eq {Id : Type} (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value note = ok (key value == target))
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.has_trigger eq groups group note =
      ok ((groups.val[group.val]).triggered_note_indices.val.any
        (fun item => key item == target)) := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_trigger_key_spec eq groups group note key target heq hgroup)
  have hbool := first_isSome_eq_any key
    (groups.val[group.val]).triggered_note_indices.val target found hfirst
  simp only [touch_hold_body.has_trigger, hfind, bind_tc_ok,
    core.option.Option.is_some, hbool]

/-! A one-bit key is used only to instantiate the existing Nat-keyed proof.
It is not a stored Id representation and does not restrict the type of Id. -/

private def eqKey {Id : Type} [DecidableEq Id] (target value : Id) : Nat :=
  if value = target then 1 else 0

private theorem eqKey_test {Id : Type} [DecidableEq Id] (target value : Id) :
    (eqKey target value == 1) = decide (value = target) := by
  by_cases h : value = target <;> simp [eqKey, h]

private theorem eqKey_eq_one {Id : Type} [DecidableEq Id] (target value : Id) :
    eqKey target value = 1 ↔ value = target := by
  by_cases h : value = target <;> simp [eqKey, h]

private theorem lawful_eqKey {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq) (target : Id) :
    ∀ value, eq.eq value target = ok (eqKey target value == 1) := by
  intro value
  rw [heq value target, eqKey_test]

private theorem firstEq_of_key {α Id : Type} [DecidableEq Id]
    (field : α → Id) (items : List α) (target : Id) (found : Option Usize)
    (h : First (fun item => eqKey target (field item)) items 1 found) :
    FirstEq field items target found := by
  cases found with
  | none =>
      intro item hmem hmatch
      exact h item hmem ((eqKey_eq_one target (field item)).mpr hmatch)
  | some index =>
      obtain ⟨pre, item, rest, hat, hpre, hmatch⟩ := h
      refine ⟨pre, item, rest, hat, ?_, ?_⟩
      · intro earlier hmem hearlier
        exact hpre earlier hmem ((eqKey_eq_one target (field earlier)).mpr hearlier)
      · exact (eqKey_eq_one target (field item)).mp hmatch

theorem find_group_spec {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq)
    (groups : Slice (Group Id)) (id : Id) :
    touch_hold_body.find_group eq groups id ⦃
      FirstEq (fun group : Group Id => group.group_id) groups.val id ⦄ := by
  apply spec_mono (find_group_key_spec eq groups id (eqKey id) 1
    (lawful_eqKey eq heq id))
  intro found hfound
  exact firstEq_of_key (fun group : Group Id => group.group_id)
    groups.val id found hfound

theorem find_id_spec {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq)
    (values : Slice Id) (note : Id) :
    touch_hold_body.find_id eq values note ⦃
      FirstEq (fun value => value) values.val note ⦄ := by
  apply spec_mono (find_id_key_spec eq values note (eqKey note) 1
    (lawful_eqKey eq heq note))
  intro found hfound
  exact firstEq_of_key (fun value : Id => value) values.val note found hfound

theorem find_member_spec {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.find_member eq groups group note ⦃
      FirstEq (fun value => value) (groups.val[group.val]).member_note_indices.val note ⦄ := by
  apply spec_mono (find_member_key_spec eq groups group note (eqKey note) 1
    (lawful_eqKey eq heq note) hgroup)
  intro found hfound
  exact firstEq_of_key (fun value : Id => value)
    (groups.val[group.val]).member_note_indices.val note found hfound

theorem find_trigger_spec {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.find_trigger eq groups group note ⦃
      FirstEq (fun value => value) (groups.val[group.val]).triggered_note_indices.val note ⦄ := by
  apply spec_mono (find_trigger_key_spec eq groups group note (eqKey note) 1
    (lawful_eqKey eq heq note) hgroup)
  intro found hfound
  exact firstEq_of_key (fun value : Id => value)
    (groups.val[group.val]).triggered_note_indices.val note found hfound

private theorem any_decide_eq_mem {Id : Type} [DecidableEq Id]
    (items : List Id) (note : Id) :
    items.any (fun value => decide (value = note)) = decide (note ∈ items) := by
  induction items with
  | nil => simp
  | cons item rest ih =>
      by_cases h : item = note
      · subst item
        simp
      · have hsymm : note ≠ item := Ne.symm h
        simp [h, hsymm, ih]

theorem has_member_eq {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.has_member eq groups group note =
      ok (decide (note ∈ (groups.val[group.val]).member_note_indices.val)) := by
  simpa only [eqKey_test, any_decide_eq_mem] using
    has_member_key_eq eq groups group note (eqKey note) 1
      (lawful_eqKey eq heq note) hgroup

theorem has_trigger_eq {Id : Type} [DecidableEq Id]
    (eq : core.cmp.PartialEq Id Id) (heq : LawfulPartialEq eq)
    (groups : Slice (Group Id)) (group : Usize) (note : Id)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.has_trigger eq groups group note =
      ok (decide (note ∈ (groups.val[group.val]).triggered_note_indices.val)) := by
  simpa only [eqKey_test, any_decide_eq_mem] using
    has_trigger_key_eq eq groups group note (eqKey note) 1
      (lawful_eqKey eq heq note) hgroup

end Verification.TouchHoldBodyScansProof
