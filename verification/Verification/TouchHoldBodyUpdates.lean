import Verification.TouchHoldBodyFields
import Verification.TouchHoldBodyViewPoints

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyUpdatesProof

open TouchHoldBodyReadsProof TouchHoldBodyViewPointsProof TouchHoldGroupsProof

/-! Decoded point-update laws derived from actual extracted record mutations.
The only value-operation premises are primitive Clone/Default laws. Capacity
is required for the vector being grown. Decoding need not be injective, and
neither group identifiers nor member/trigger lists need be duplicate-free. -/

variable {Id α β : Type}

theorem map_eraseIdx (f : α → β) (items : List α) (index : Nat) :
    (items.eraseIdx index).map f = (items.map f).eraseIdx index := by
  induction items generalizing index with
  | nil => simp
  | cons item rest ih => cases index <;> simp [ih]

theorem view_set (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState)
    (hat : At (viewGroups decode groups) index pre group rest)
    (next : TouchHoldBodyReadsProof.Group Id) :
    viewGroups decode (groups.set index next) = pre ++ viewGroup decode next :: rest := by
  change (groups.val.set index.val next).map (viewGroup decode) = _
  rw [List.map_set]
  change (viewGroups decode groups).set index.val (viewGroup decode next) = _
  rw [hat.1, ← hat.2, List.set_append_right _ _ (Nat.le_refl _)]
  simp

theorem prepend_member_point (cloneInst : core.clone.Clone Id) (decode : Id → Nat)
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hroom : group.memberNoteIndices.length + 1 ≤ Usize.max)
    (hclone : cloneInst.clone note = ok note) :
    ∃ next, touch_hold_body.prepend_member cloneInst groups index note = ok next ∧
      viewGroups decode next = pre ++
        { group with memberNoteIndices := decode note :: group.memberNoteIndices } :: rest := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hlen : (groups.val[index.val]).member_note_indices.val.length =
      group.memberNoteIndices.length := by
    rw [← hview]
    simp only [viewGroup, List.length_map]
  have hrawRoom : (groups.val[index.val]).member_note_indices.val.length < Usize.max := by
    omega
  refine ⟨_, TouchHoldBodyFieldsProof.prepend_member_eq
    cloneInst groups index note h hrawRoom hclone, ?_⟩
  rw [view_set decode groups index pre group rest hat]
  have hnext : viewGroup decode
      ({ (groups.val[index.val]) with member_note_indices :=
        (TouchHoldBodyFieldsProof.prependVec (groups.val[index.val]).member_note_indices
          note hrawRoom) }) =
      { group with memberNoteIndices := decode note :: group.memberNoteIndices } := by
    rw [← hview]
    rfl
  rw [hnext]

theorem prepend_trigger_point (cloneInst : core.clone.Clone Id) (decode : Id → Nat)
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hroom : group.triggeredNoteIndices.length + 1 ≤ Usize.max)
    (hclone : cloneInst.clone note = ok note) :
    ∃ next, touch_hold_body.prepend_trigger cloneInst groups index note = ok next ∧
      viewGroups decode next = pre ++
        { group with triggeredNoteIndices := decode note :: group.triggeredNoteIndices } :: rest := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hlen : (groups.val[index.val]).triggered_note_indices.val.length =
      group.triggeredNoteIndices.length := by
    rw [← hview]
    simp only [viewGroup, List.length_map]
  have hrawRoom : (groups.val[index.val]).triggered_note_indices.val.length < Usize.max := by
    omega
  refine ⟨_, TouchHoldBodyFieldsProof.prepend_trigger_eq
    cloneInst groups index note h hrawRoom hclone, ?_⟩
  rw [view_set decode groups index pre group rest hat]
  have hnext : viewGroup decode
      ({ (groups.val[index.val]) with triggered_note_indices :=
        (TouchHoldBodyFieldsProof.prependVec (groups.val[index.val]).triggered_note_indices
          note hrawRoom) }) =
      { group with triggeredNoteIndices := decode note :: group.triggeredNoteIndices } := by
    rw [← hview]
    rfl
  rw [hnext]

theorem remove_member_point (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (slot : Usize) (filler : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hslot : slot.val < group.memberNoteIndices.length)
    (hdefault : defaultInst.default = ok filler) :
    ∃ next, touch_hold_body.remove_member cloneInst defaultInst groups index slot = ok next ∧
      viewGroups decode next = pre ++
        { group with memberNoteIndices := group.memberNoteIndices.eraseIdx slot.val } :: rest := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hrawSlot : slot.val < (groups.val[index.val]).member_note_indices.val.length := by
    simpa only [← hview, viewGroup, List.length_map] using hslot
  refine ⟨_, TouchHoldBodyFieldsProof.remove_member_eq
    cloneInst defaultInst groups index slot filler h hrawSlot hdefault, ?_⟩
  rw [view_set decode groups index pre group rest hat]
  have hnext : viewGroup decode
      ({ (groups.val[index.val]) with member_note_indices :=
        (TouchHoldBodyFieldsProof.eraseVec (groups.val[index.val]).member_note_indices
          slot hrawSlot) }) =
      { group with memberNoteIndices := group.memberNoteIndices.eraseIdx slot.val } := by
    rw [← hview]
    simp only [viewGroup, TouchHoldBodyFieldsProof.eraseVec_val, map_eraseIdx]
  rw [hnext]

theorem remove_trigger_point (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (slot : Usize) (filler : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hslot : slot.val < group.triggeredNoteIndices.length)
    (hdefault : defaultInst.default = ok filler) :
    ∃ next, touch_hold_body.remove_trigger cloneInst defaultInst groups index slot = ok next ∧
      viewGroups decode next = pre ++
        { group with triggeredNoteIndices := group.triggeredNoteIndices.eraseIdx slot.val } :: rest := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hrawSlot : slot.val < (groups.val[index.val]).triggered_note_indices.val.length := by
    simpa only [← hview, viewGroup, List.length_map] using hslot
  refine ⟨_, TouchHoldBodyFieldsProof.remove_trigger_eq
    cloneInst defaultInst groups index slot filler h hrawSlot hdefault, ?_⟩
  rw [view_set decode groups index pre group rest hat]
  have hnext : viewGroup decode
      ({ (groups.val[index.val]) with triggered_note_indices :=
        (TouchHoldBodyFieldsProof.eraseVec (groups.val[index.val]).triggered_note_indices
          slot hrawSlot) }) =
      { group with triggeredNoteIndices := group.triggeredNoteIndices.eraseIdx slot.val } := by
    rw [← hview]
    simp only [viewGroup, TouchHoldBodyFieldsProof.eraseVec_val, map_eraseIdx]
  rw [hnext]

theorem append_empty_point (cloneInst : core.clone.Clone Id) (decode : Id → Nat)
    (groups : Groups Id) (id : Id)
    (hroom : (viewGroups decode groups).length + 1 ≤ Usize.max)
    (hclone : cloneInst.clone id = ok id) :
    ∃ next, touch_hold_body.append_empty cloneInst groups id = ok next ∧
      viewGroups decode next = viewGroups decode groups ++
        [{ groupId := decode id, memberNoteIndices := [], triggeredNoteIndices := [] }] := by
  have hrawRoom : groups.val.length < Usize.max := by
    simpa only [viewGroups, List.length_map, Nat.add_one_le_iff] using hroom
  refine ⟨_, TouchHoldBodyFieldsProof.append_empty_eq cloneInst groups id hrawRoom hclone, ?_⟩
  simp only [viewGroups, TouchHoldBodyFieldsProof.snocVec_val, List.map_append,
    List.map_cons, List.map_nil, viewGroup, TouchHoldBodyFieldsProof.emptyGroup,
    alloc.vec.Vec.new]

theorem append_point (cloneInst : core.clone.Clone Id) (decode : Id → Nat)
    (groups : Groups Id) (id note : Id)
    (hroom : (viewGroups decode groups).length + 1 ≤ Usize.max)
    (hcloneId : cloneInst.clone id = ok id) (hcloneNote : cloneInst.clone note = ok note) :
    ∃ next, touch_hold_body.append cloneInst groups id note = ok next ∧
      viewGroups decode next = viewGroups decode groups ++
        [{ groupId := decode id, memberNoteIndices := [decode note],
           triggeredNoteIndices := [decode note] }] := by
  have hrawRoom : groups.val.length < Usize.max := by
    simpa only [viewGroups, List.length_map, Nat.add_one_le_iff] using hroom
  refine ⟨_, TouchHoldBodyFieldsProof.append_eq
    cloneInst groups id note hrawRoom hcloneId hcloneNote, ?_⟩
  simp only [viewGroups, TouchHoldBodyFieldsProof.snocVec_val, List.map_append,
    List.map_cons, List.map_nil, viewGroup, TouchHoldBodyFieldsProof.singletonGroup,
    TouchHoldBodyFieldsProof.singletonVec_val]

theorem remove_group_point (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (filler : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hdefault : defaultInst.default = ok filler) :
    ∃ next, touch_hold_body.remove_group cloneInst defaultInst groups index = ok next ∧
      viewGroups decode next = pre ++ rest := by
  obtain ⟨h, _⟩ := group_at decode groups index pre group rest hat
  refine ⟨_, TouchHoldBodyFieldsProof.remove_group_eq
    cloneInst defaultInst groups index filler h hdefault, ?_⟩
  change (groups.val.eraseIdx index.val).map (viewGroup decode) = _
  rw [map_eraseIdx]
  change (viewGroups decode groups).eraseIdx index.val = _
  rw [hat.1, ← hat.2, List.eraseIdx_append_of_length_le (Nat.le_refl _)]
  simp

end Verification.TouchHoldBodyUpdatesProof
