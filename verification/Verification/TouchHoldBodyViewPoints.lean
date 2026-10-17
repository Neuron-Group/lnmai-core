import Verification.TouchHoldBodyReads
import Verification.TouchHoldGroups
import Verification.TouchHoldBodyScans

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyViewPointsProof

open TouchHoldBodyReadsProof TouchHoldGroupsProof

/-! Point laws for the decoded view of the actual extracted record. The map
inversion keeps the original prefix and suffix; decoding need not be injective.
Only the identifier equality primitive remains a hypothesis of equality reads.
No interpreted replacement record or whole search/update law is assumed. -/

variable {Id : Type}

theorem at_index {α : Type} {items pre rest : List α} {item : α}
    {index : Usize} (hat : At items index pre item rest) :
    ∃ h : index.val < items.length, items[index.val]'h = item := by
  obtain ⟨hitems, hpre⟩ := hat
  have h : index.val < items.length := by
    rw [hitems, List.length_append, List.length_cons, ← hpre]
    omega
  exact ⟨h, List.getElem_of_append hitems hpre⟩

theorem any_beq_swap {α : Type} (l : List α) (f : α → Nat) (target : Nat) :
    (l.any (fun item => f item == target)) =
      (l.any (fun item => target == f item)) := by
  induction l with
  | nil => rfl
  | cons x xs ih =>
      simp only [List.any_cons, ih]
      cases h : (f x == target) <;> cases h' : (target == f x) <;>
        simp_all [beq_iff_eq]

theorem at_map_lift {α β : Type} (f : α → β) (items : List α)
    (index : Usize) (pre : List β) (item : β) (rest : List β)
    (hat : At (items.map f) index pre item rest) :
    ∃ rawPre raw rawRest, At items index rawPre raw rawRest ∧
      rawPre.map f = pre ∧ f raw = item ∧ rawRest.map f = rest := by
  obtain ⟨rawPre, tail, hsplit, hpre, htail⟩ :=
    List.map_eq_append_iff.mp hat.1
  obtain ⟨raw, rawRest, hcons, hraw, hrest⟩ :=
    List.map_eq_cons_iff.mp htail
  refine ⟨rawPre, raw, rawRest, ⟨?_, ?_⟩, hpre, hraw, hrest⟩
  · simpa only [hcons] using hsplit
  · have hlen := congrArg List.length hpre
    simpa only [List.length_map, hat.2] using hlen

theorem group_at (decode : Id → Nat) (groups : Groups Id)
    (index : Usize) (pre : List LnmaiCore.TouchHoldBodyGroupState)
    (group : LnmaiCore.TouchHoldBodyGroupState) (rest : List LnmaiCore.TouchHoldBodyGroupState)
    (hat : At (viewGroups decode groups) index pre group rest) :
    ∃ h : index.val < groups.val.length,
      viewGroup decode (groups.val[index.val]'h) = group := by
  obtain ⟨rawPre, raw, rawRest, hrawAt, _, hraw, _⟩ :=
    at_map_lift (viewGroup decode) groups.val index pre group rest hat
  obtain ⟨h, hread⟩ := at_index hrawAt
  exact ⟨h, by rw [hread]; exact hraw⟩

theorem group_matches_point (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (id : Id)
    (hat : At (viewGroups decode groups) index pre group rest) :
    touch_hold_body.group_matches eq (alloc.vec.Vec.deref groups) index id =
      ok (group.groupId == decode id) := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  simpa only [alloc.vec.Vec.deref, hview] using
    group_matches_view eq decode heq (alloc.vec.Vec.deref groups) index id h

theorem member_length_point (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState)
    (hat : At (viewGroups decode groups) index pre group rest) :
    ∃ len, touch_hold_body.member_length (alloc.vec.Vec.deref groups) index = ok len ∧
      group.memberNoteIndices.length = len.val := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  obtain ⟨len, hrun, hlen⟩ := spec_imp_exists
    (member_length_spec (alloc.vec.Vec.deref groups) index h)
  refine ⟨len, hrun, ?_⟩
  rw [← hview]
  simpa only [viewGroup, List.length_map] using hlen.symm

theorem trigger_length_point (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState)
    (hat : At (viewGroups decode groups) index pre group rest) :
    ∃ len, touch_hold_body.trigger_length (alloc.vec.Vec.deref groups) index = ok len ∧
      group.triggeredNoteIndices.length = len.val := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  obtain ⟨len, hrun, hlen⟩ := spec_imp_exists
    (trigger_length_spec (alloc.vec.Vec.deref groups) index h)
  refine ⟨len, hrun, ?_⟩
  rw [← hview]
  simpa only [viewGroup, List.length_map] using hlen.symm

theorem member_matches_point (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (index slot : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (before : List Nat) (item : Nat)
    (after : List Nat) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hslot : At group.memberNoteIndices slot before item after) :
    touch_hold_body.member_matches eq (alloc.vec.Vec.deref groups) index slot note =
      ok (item == decode note) := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hdecoded : (groups.val[index.val]).member_note_indices.val.map decode =
      group.memberNoteIndices :=
        congrArg LnmaiCore.TouchHoldBodyGroupState.memberNoteIndices hview
  have hslotMap : At ((groups.val[index.val]).member_note_indices.val.map decode)
      slot before item after := by rwa [hdecoded]
  obtain ⟨rawPre, raw, rawRest, hrawAt, _, hraw, _⟩ :=
    at_map_lift decode _ slot before item after hslotMap
  obtain ⟨hvalid, hread⟩ := at_index hrawAt
  simpa only [alloc.vec.Vec.deref, heq, hread, hraw] using
    member_matches_eq eq (alloc.vec.Vec.deref groups) index slot note h hvalid

theorem trigger_matches_point (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (index slot : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (before : List Nat) (item : Nat)
    (after : List Nat) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest)
    (hslot : At group.triggeredNoteIndices slot before item after) :
    touch_hold_body.trigger_matches eq (alloc.vec.Vec.deref groups) index slot note =
      ok (item == decode note) := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  have hdecoded : (groups.val[index.val]).triggered_note_indices.val.map decode =
      group.triggeredNoteIndices :=
        congrArg LnmaiCore.TouchHoldBodyGroupState.triggeredNoteIndices hview
  have hslotMap : At ((groups.val[index.val]).triggered_note_indices.val.map decode)
      slot before item after := by rwa [hdecoded]
  obtain ⟨rawPre, raw, rawRest, hrawAt, _, hraw, _⟩ :=
    at_map_lift decode _ slot before item after hslotMap
  obtain ⟨hvalid, hread⟩ := at_index hrawAt
  simpa only [alloc.vec.Vec.deref, heq, hread, hraw] using
    trigger_matches_eq eq (alloc.vec.Vec.deref groups) index slot note h hvalid

theorem members_empty_point (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState)
    (hat : At (viewGroups decode groups) index pre group rest) :
    touch_hold_body.members_empty (alloc.vec.Vec.deref groups) index =
      ok group.memberNoteIndices.isEmpty := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  simpa only [alloc.vec.Vec.deref, viewGroup, List.isEmpty_map, ← hview] using
    members_empty_eq (alloc.vec.Vec.deref groups) index h

theorem has_member_point (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest) :
    touch_hold_body.has_member eq (alloc.vec.Vec.deref groups) index note =
      ok (group.memberNoteIndices.contains (decode note)) := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  rw [TouchHoldBodyScansProof.has_member_key_eq eq (alloc.vec.Vec.deref groups)
    index note decode (decode note) (fun value => heq value note) h]
  simp only [alloc.vec.Vec.deref, viewGroup, List.contains_map, ← hview]
  refine congrArg ok ?_
  exact any_beq_swap _ decode (decode note)

theorem has_trigger_point (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState) (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState) (note : Id)
    (hat : At (viewGroups decode groups) index pre group rest) :
    touch_hold_body.has_trigger eq (alloc.vec.Vec.deref groups) index note =
      ok (group.triggeredNoteIndices.contains (decode note)) := by
  obtain ⟨h, hview⟩ := group_at decode groups index pre group rest hat
  rw [TouchHoldBodyScansProof.has_trigger_key_eq eq (alloc.vec.Vec.deref groups)
    index note decode (decode note) (fun value => heq value note) h]
  simp only [alloc.vec.Vec.deref, viewGroup, List.contains_map, ← hview]
  refine congrArg ok ?_
  exact any_beq_swap _ decode (decode note)

end Verification.TouchHoldBodyViewPointsProof
