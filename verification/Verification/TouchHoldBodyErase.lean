import Verification.TouchHoldBodyScans
import Verification.TouchHoldBodyUpdates
import Init.Data.List.Erase

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyEraseProof

open TouchHoldGroupsProof TouchHoldBodyReadsProof TouchHoldBodyViewPointsProof

/-!
Composition of the actual extraction5 erase_member/erase_trigger wrappers:
key-projected FIRST scan, then the actual indexed swap-and-shrink erase.
Updates supplies decoded field writeback from Fields; no scan, erase, group
operation, or runtime-dictionary equivalence is a premise of the point laws.

The only value-operation premises are target-specific PartialEq.eq success
and primitive Default success on a hit. Clone is an arbitrary dictionary: the
shrinking Vec.resize branch never calls it. No spare-capacity premise is needed.
The miss theorems require neither Default nor Clone success and return the
literal original Vec, including every raw field.

Decoding may be non-injective; outer groups and inner entries may repeat. The
target-specific comparison law must nevertheless agree with decoded equality.
Only the first matching inner entry is removed; other fields and the complete
outer prefix/suffix are preserved. The supplied At identifies the outer group.

This proves extracted-kernel behavior under Aeneas' existing Vec/Slice model.
It does not prove the native backend, generic Drop, Natural dictionaries, or
the unextracted runtime facade. No compiler was run for this delivery.
-/

variable {Id α : Type}

/-- A key-based nonmatch entails absence in the mapped list, without injectivity. -/
theorem not_mem_map_of_noKey (key : α → Nat) (items : List α) (target : Nat)
    (hno : NoKey key items target) : target ∉ items.map key := by
  intro hmem
  obtain ⟨item, hitem, hkey⟩ := List.mem_map.mp hmem
  exact hno item hitem hkey

theorem first_none_erase_map (key : α → Nat) (items : List α) (target : Nat)
    (hfirst : First key items target none) :
    (items.map key).erase target = items.map key :=
  List.erase_of_not_mem (not_mem_map_of_noKey key items target hfirst)

/-- The scan position is valid, and indexed deletion equals decoded erase-FIRST.
The suffix may contain any number of further matches, including raw entries
different from the selected entry but with the same decoded key. -/
theorem first_some_eraseIdx_map (key : α → Nat) (items : List α) (target : Nat)
    (index : Usize) (hfirst : First key items target (some index)) :
    index.val < (items.map key).length ∧
      (items.map key).eraseIdx index.val = (items.map key).erase target := by
  obtain ⟨pre, item, rest, hat, hprefix, hmatch⟩ := hfirst
  have hindex : (pre.map key).length = index.val := by
    simpa only [List.length_map] using hat.2
  have hmiss := not_mem_map_of_noKey key pre target hprefix
  have hmap : items.map key = pre.map key ++ target :: rest.map key := by
    rw [hat.1, List.map_append, List.map_cons, hmatch]
  constructor
  · rw [hmap, List.length_append, List.length_cons]
    omega
  · rw [hmap, ← hindex, List.eraseIdx_append_of_length_le (Nat.le_refl _),
      List.erase_append_right _ hmiss]
    simp

/-- This only interprets the scan postcondition; it assumes no scan callback. -/
theorem first_eq_none_of_not_mem_map (key : α → Nat) (items : List α)
    (target : Nat) (found : Option Usize) (hfirst : First key items target found)
    (hmiss : target ∉ items.map key) : found = none := by
  cases found with
  | none => rfl
  | some index =>
      obtain ⟨pre, item, rest, hat, _, hmatch⟩ := hfirst
      have hmem : target ∈ items.map key := by
        rw [hat.1, List.map_append, List.map_cons, hmatch]
        simp
      exact False.elim (hmiss hmem)

/-- Actual erase_member, in the shape required by BodyAtoms.erase_member.
The filler is arbitrary: its decoded value is irrelevant to shrinking erase. -/
theorem erase_member_point (eqInst : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState)
    (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note filler : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (heq : ∀ value, eqInst.eq value note = ok (decode value == decode note))
    (hdefault : defaultInst.default = ok filler) :
    ∃ next, touch_hold_body.erase_member eqInst cloneInst defaultInst
        groups index note = ok next ∧
      viewGroups decode next = pre ++
        { group with memberNoteIndices :=
            group.memberNoteIndices.erase (decode note) } :: rest := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hdecoded : (groups.val[index.val]).member_note_indices.val.map decode =
      group.memberNoteIndices :=
    congrArg LnmaiCore.TouchHoldBodyGroupState.memberNoteIndices hview
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (TouchHoldBodyScansProof.find_member_key_spec eqInst
      (alloc.vec.Vec.deref groups) index note decode (decode note) heq h)
  cases found with
  | none =>
      have herase : group.memberNoteIndices.erase (decode note) =
          group.memberNoteIndices := by
        rw [← hdecoded]
        exact first_none_erase_map decode _ (decode note) hfirst
      refine ⟨groups, ?_, ?_⟩
      · simp only [touch_hold_body.erase_member, hfind, bind_tc_ok]
      · simpa only [herase] using hat.1
  | some slot =>
      obtain ⟨hslot, herase⟩ := first_some_eraseIdx_map decode
        (groups.val[index.val]).member_note_indices.val (decode note) slot hfirst
      have hslot' : slot.val < group.memberNoteIndices.length := by
        simpa only [hdecoded] using hslot
      have herase' : group.memberNoteIndices.eraseIdx slot.val =
          group.memberNoteIndices.erase (decode note) := by
        simpa only [hdecoded] using herase
      obtain ⟨next, hremove, hnext⟩ :=
        TouchHoldBodyUpdatesProof.remove_member_point cloneInst defaultInst decode
          groups index pre group rest slot filler hat hslot' hdefault
      refine ⟨next, ?_, ?_⟩
      · simp only [touch_hold_body.erase_member, hfind, bind_tc_ok, hremove]
      · simpa only [herase'] using hnext

/-- Actual erase_trigger, in the shape required by BodyAtoms.erase_trigger. -/
theorem erase_trigger_point (eqInst : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState)
    (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note filler : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (heq : ∀ value, eqInst.eq value note = ok (decode value == decode note))
    (hdefault : defaultInst.default = ok filler) :
    ∃ next, touch_hold_body.erase_trigger eqInst cloneInst defaultInst
        groups index note = ok next ∧
      viewGroups decode next = pre ++
        { group with triggeredNoteIndices :=
            group.triggeredNoteIndices.erase (decode note) } :: rest := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hdecoded : (groups.val[index.val]).triggered_note_indices.val.map decode =
      group.triggeredNoteIndices :=
    congrArg LnmaiCore.TouchHoldBodyGroupState.triggeredNoteIndices hview
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (TouchHoldBodyScansProof.find_trigger_key_spec eqInst
      (alloc.vec.Vec.deref groups) index note decode (decode note) heq h)
  cases found with
  | none =>
      have herase : group.triggeredNoteIndices.erase (decode note) =
          group.triggeredNoteIndices := by
        rw [← hdecoded]
        exact first_none_erase_map decode _ (decode note) hfirst
      refine ⟨groups, ?_, ?_⟩
      · simp only [touch_hold_body.erase_trigger, hfind, bind_tc_ok]
      · simpa only [herase] using hat.1
  | some slot =>
      obtain ⟨hslot, herase⟩ := first_some_eraseIdx_map decode
        (groups.val[index.val]).triggered_note_indices.val (decode note) slot hfirst
      have hslot' : slot.val < group.triggeredNoteIndices.length := by
        simpa only [hdecoded] using hslot
      have herase' : group.triggeredNoteIndices.eraseIdx slot.val =
          group.triggeredNoteIndices.erase (decode note) := by
        simpa only [hdecoded] using herase
      obtain ⟨next, hremove, hnext⟩ :=
        TouchHoldBodyUpdatesProof.remove_trigger_point cloneInst defaultInst decode
          groups index pre group rest slot filler hat hslot' hdefault
      refine ⟨next, ?_, ?_⟩
      · simp only [touch_hold_body.erase_trigger, hfind, bind_tc_ok, hremove]
      · simpa only [herase'] using hnext

/-- No Default-success law is needed when the member scan misses. -/
theorem erase_member_of_not_mem (eqInst : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState)
    (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (heq : ∀ value, eqInst.eq value note = ok (decode value == decode note))
    (hmiss : decode note ∉ group.memberNoteIndices) :
    touch_hold_body.erase_member eqInst cloneInst defaultInst
      groups index note = ok groups := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hdecoded : (groups.val[index.val]).member_note_indices.val.map decode =
      group.memberNoteIndices :=
    congrArg LnmaiCore.TouchHoldBodyGroupState.memberNoteIndices hview
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (TouchHoldBodyScansProof.find_member_key_spec eqInst
      (alloc.vec.Vec.deref groups) index note decode (decode note) heq h)
  have hrawMiss : decode note ∉
      (groups.val[index.val]).member_note_indices.val.map decode := by
    simpa only [hdecoded] using hmiss
  have hnone := first_eq_none_of_not_mem_map decode
    (groups.val[index.val]).member_note_indices.val (decode note) found hfirst hrawMiss
  rw [hnone] at hfind
  simp only [touch_hold_body.erase_member, hfind, bind_tc_ok]

/-- No Default-success law is needed when the trigger scan misses. -/
theorem erase_trigger_of_not_mem (eqInst : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState)
    (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (heq : ∀ value, eqInst.eq value note = ok (decode value == decode note))
    (hmiss : decode note ∉ group.triggeredNoteIndices) :
    touch_hold_body.erase_trigger eqInst cloneInst defaultInst
      groups index note = ok groups := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hdecoded : (groups.val[index.val]).triggered_note_indices.val.map decode =
      group.triggeredNoteIndices :=
    congrArg LnmaiCore.TouchHoldBodyGroupState.triggeredNoteIndices hview
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (TouchHoldBodyScansProof.find_trigger_key_spec eqInst
      (alloc.vec.Vec.deref groups) index note decode (decode note) heq h)
  have hrawMiss : decode note ∉
      (groups.val[index.val]).triggered_note_indices.val.map decode := by
    simpa only [hdecoded] using hmiss
  have hnone := first_eq_none_of_not_mem_map decode
    (groups.val[index.val]).triggered_note_indices.val (decode note) found hfirst hrawMiss
  rw [hnone] at hfind
  simp only [touch_hold_body.erase_trigger, hfind, bind_tc_ok]

end Verification.TouchHoldBodyEraseProof
