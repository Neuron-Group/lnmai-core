import Verification.Generated
import LnmaiCore.Types

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyReadsProof

/-! Field reads of the actual extracted generic Rust record. Only equality of
the identifier remains a primitive contract. The decoding below projects the
real record; it does not replace its fields with a separate interpreted record.
Native allocation, Natural representation, and the Rust caller are separate
obligations. -/

abbrev Group (Id : Type) := touch_hold_body.TouchHoldBodyGroupState Id
abbrev Groups (Id : Type) := alloc.vec.Vec (Group Id)

def viewGroup {Id : Type} (decode : Id → Nat) (group : Group Id) :
    LnmaiCore.TouchHoldBodyGroupState :=
  { groupId := decode group.group_id
    memberNoteIndices := group.member_note_indices.val.map decode
    triggeredNoteIndices := group.triggered_note_indices.val.map decode }

def viewGroups {Id : Type} (decode : Id → Nat) (groups : Groups Id) :
    List LnmaiCore.TouchHoldBodyGroupState :=
  groups.val.map (viewGroup decode)

variable {Id : Type}

theorem empty_spec : touch_hold_body.empty Id ⦃ out => out.val = [] ⦄ := by
  simp [touch_hold_body.empty, alloc.vec.Vec.new]

theorem group_length_spec (groups : Slice (Group Id)) :
    touch_hold_body.group_length groups ⦃ len => len.val = groups.val.length ⦄ := by
  simp [touch_hold_body.group_length]

theorem group_matches_eq (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (index : Usize) (id : Id)
    (hindex : index.val < groups.val.length) :
    touch_hold_body.group_matches eq groups index id =
      eq.eq (groups.val[index.val]).group_id id := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups index hindex)
  simp only [touch_hold_body.group_matches, hread, bind_tc_ok, hraw,
    core.cmp.impls.PartialEqShared.eq]

theorem member_length_spec (groups : Slice (Group Id)) (group : Usize)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.member_length groups group ⦃ len =>
      len.val = (groups.val[group.val]).member_note_indices.val.length ⦄ := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  simp only [touch_hold_body.member_length, hread, bind_tc_ok, hraw, spec_ok,
    alloc.vec.Vec.len_val, alloc.vec.Vec.length]

theorem trigger_length_spec (groups : Slice (Group Id)) (group : Usize)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.trigger_length groups group ⦃ len =>
      len.val = (groups.val[group.val]).triggered_note_indices.val.length ⦄ := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  simp only [touch_hold_body.trigger_length, hread, bind_tc_ok, hraw, spec_ok,
    alloc.vec.Vec.len_val, alloc.vec.Vec.length]

theorem member_matches_eq (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (group index : Usize) (note : Id)
    (hgroup : group.val < groups.val.length)
    (hindex : index.val < (groups.val[group.val]).member_note_indices.val.length) :
    touch_hold_body.member_matches eq groups group index note =
      eq.eq ((groups.val[group.val]).member_note_indices.val[index.val]) note := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  subst raw
  obtain ⟨value, hvalue, hvalueEq⟩ := spec_imp_exists
    (alloc.vec.Vec.index_usize_spec
      (groups.val[group.val]).member_note_indices index hindex)
  simp only [touch_hold_body.member_matches, hread, bind_tc_ok,
    alloc.vec.Vec.index_slice_index, hvalue, hvalueEq,
    core.cmp.impls.PartialEqShared.eq]

theorem trigger_matches_eq (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (Group Id)) (group index : Usize) (note : Id)
    (hgroup : group.val < groups.val.length)
    (hindex : index.val < (groups.val[group.val]).triggered_note_indices.val.length) :
    touch_hold_body.trigger_matches eq groups group index note =
      eq.eq ((groups.val[group.val]).triggered_note_indices.val[index.val]) note := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  subst raw
  obtain ⟨value, hvalue, hvalueEq⟩ := spec_imp_exists
    (alloc.vec.Vec.index_usize_spec
      (groups.val[group.val]).triggered_note_indices index hindex)
  simp only [touch_hold_body.trigger_matches, hread, bind_tc_ok,
    alloc.vec.Vec.index_slice_index, hvalue, hvalueEq,
    core.cmp.impls.PartialEqShared.eq]

theorem members_empty_eq (groups : Slice (Group Id)) (group : Usize)
    (hgroup : group.val < groups.val.length) :
    touch_hold_body.members_empty groups group =
      ok (groups.val[group.val]).member_note_indices.val.isEmpty := by
  obtain ⟨raw, hread, hraw⟩ := spec_imp_exists
    (Slice.index_usize_spec groups group hgroup)
  simp only [touch_hold_body.members_empty, hread, bind_tc_ok, hraw,
    UScalar.eq_equiv, alloc.vec.Vec.len_val]
  change ok (decide ((groups.val[group.val]).member_note_indices.val.length = 0)) = _
  cases (groups.val[group.val]).member_note_indices.val <;> simp

theorem group_matches_view (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Slice (Group Id)) (index : Usize) (id : Id)
    (hindex : index.val < groups.val.length) :
    touch_hold_body.group_matches eq groups index id =
      ok ((viewGroup decode groups.val[index.val]).groupId == decode id) := by
  rw [group_matches_eq eq groups index id hindex, heq]
  rfl

theorem native_capacity (decode : Id → Nat) (groups : Groups Id) :
    (viewGroups decode groups).length ≤ Usize.max ∧
      ∀ group ∈ viewGroups decode groups,
        group.memberNoteIndices.length ≤ Usize.max ∧
        group.triggeredNoteIndices.length ≤ Usize.max := by
  constructor
  · simpa only [viewGroups, List.length_map] using groups.property
  · intro group hgroup
    obtain ⟨raw, _, rfl⟩ := List.mem_map.mp hgroup
    simpa only [viewGroup, List.length_map] using
      And.intro raw.member_note_indices.property raw.triggered_note_indices.property

end Verification.TouchHoldBodyReadsProof
