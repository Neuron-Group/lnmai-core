import Verification.TouchHoldBodyUpdates
import Verification.TouchHoldRebuildCapacity

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyRebuildProof

open TouchHoldBodyReadsProof TouchHoldBodyViewPointsProof TouchHoldBodyUpdatesProof
open TouchHoldGroupsProof TouchHoldRebuildUpsertProof TouchHoldRebuildTraversalProof
open TouchHoldRebuildReverseProof TouchHoldRebuildReverseExtractedProof

/-! Rebuild composition with the real extracted group record and operations.
Only the four source-Hold reads and primitive Id operations remain hypotheses.
This dictionary checks the composition of extracted functions; it is not an
extraction of the runtime Rust trait implementation. Native allocation and the
Natural primitive implementations remain separate refinement obligations. -/

variable {Id S Ops : Type}

structure HoldFields (Ops S Id : Type) where
  length : Ops → S → Result Usize
  groupId : Ops → S → Usize → Result (Option Id)
  noteIndex : Ops → S → Usize → Result Id
  triggered : Ops → S → Usize → Result Bool

def representation (decode : Id → Nat) : BodyRepresentation (Groups Id) Id :=
  { groups := viewGroups decode, id := decode }

def capacity : BodyCapacity := TouchHoldRebuildCapacityProof.uniformCapacity Usize.max

def rebuildCall (source : HoldFields Ops S Id) (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id) :
    touch_hold_rebuild.TouchHoldRebuild Ops S (Groups Id) Id :=
  { hold_length := source.length
    hold_group_id := source.groupId
    hold_note_index := source.noteIndex
    hold_triggered := source.triggered
    empty := fun _ => touch_hold_body.empty Id
    group_length := fun _ groups => touch_hold_body.group_length (alloc.vec.Vec.deref groups)
    group_matches := fun _ groups index id =>
      touch_hold_body.group_matches eq (alloc.vec.Vec.deref groups) index id
    member_length := fun _ groups index =>
      touch_hold_body.member_length (alloc.vec.Vec.deref groups) index
    member_matches := fun _ groups index slot note =>
      touch_hold_body.member_matches eq (alloc.vec.Vec.deref groups) index slot note
    trigger_length := fun _ groups index =>
      touch_hold_body.trigger_length (alloc.vec.Vec.deref groups) index
    trigger_matches := fun _ groups index slot note =>
      touch_hold_body.trigger_matches eq (alloc.vec.Vec.deref groups) index slot note
    prepend_member := fun _ => touch_hold_body.prepend_member cloneInst
    prepend_trigger := fun _ => touch_hold_body.prepend_trigger cloneInst
    remove_trigger := fun _ => touch_hold_body.remove_trigger cloneInst defaultInst
    append_empty := fun _ => touch_hold_body.append_empty cloneInst
    swap_groups := fun _ => touch_hold_body.swap_groups }

theorem scan_atoms (source : HoldFields Ops S Id) (ops : Ops)
    (eq : core.cmp.PartialEq Id Id) (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right)) :
    ScanAtoms (rebuildCall source eq cloneInst defaultInst) ops (representation decode) := by
  constructor
  · intro groups
    obtain ⟨len, hrun, hlen⟩ := spec_imp_exists
      (group_length_spec (alloc.vec.Vec.deref groups))
    refine ⟨len, hrun, ?_⟩
    simpa only [representation, viewGroups, List.length_map] using hlen.symm
  · intro groups index pre group rest id hat
    exact group_matches_point eq decode heq groups index pre group rest id hat
  · intro groups index pre group rest hat
    exact member_length_point decode groups index pre group rest hat
  · intro groups index pre group rest slot before item after note hat hslot
    exact member_matches_point eq decode heq groups index slot
      pre group rest before item after note hat hslot
  · intro groups index pre group rest hat
    exact trigger_length_point decode groups index pre group rest hat
  · intro groups index pre group rest slot before item after note hat hslot
    exact trigger_matches_point eq decode heq groups index slot
      pre group rest before item after note hat hslot

theorem update_atoms (source : HoldFields Ops S Id) (ops : Ops)
    (eq : core.cmp.PartialEq Id Id) (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler) :
    UpdateAtoms (rebuildCall source eq cloneInst defaultInst) ops
      (representation decode) capacity := by
  refine ⟨scan_atoms source ops eq cloneInst defaultInst decode heq, ?_, ?_, ?_, ?_⟩
  · intro groups index pre group rest note hat hroom
    exact prepend_member_point cloneInst decode groups index pre group rest note
      hat hroom (hclone note)
  · intro groups index pre group rest note hat hroom
    exact prepend_trigger_point cloneInst decode groups index pre group rest note
      hat hroom (hclone note)
  · intro groups index pre group rest slot hat hslot
    exact remove_trigger_point cloneInst defaultInst decode groups index pre group rest
      slot filler hat hslot hdefault
  · intro groups id hroom
    exact append_empty_point cloneInst decode groups id hroom (hclone id)

theorem swap_points_map {α β : Type} (f : α → β) (before after : List α)
    (left right : Nat) (h : SwapPoints before after left right) :
    SwapPoints (before.map f) (after.map f) left right := by
  constructor
  · simpa only [List.length_map] using h.length_eq
  · simpa only [List.getElem?_map] using congrArg (Option.map f) h.left_eq
  · simpa only [List.getElem?_map] using congrArg (Option.map f) h.right_eq
  · intro index hleft hright
    simpa only [List.getElem?_map] using
      congrArg (Option.map f) (h.other_eq index hleft hright)

theorem reverse_atoms (source : HoldFields Ops S Id) (ops : Ops)
    (eq : core.cmp.PartialEq Id Id) (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat) :
    ReverseAtoms (rebuildCall source eq cloneInst defaultInst) ops
      (viewGroups decode) Usize.max := by
  constructor
  · intro groups _
    obtain ⟨len, hrun, hlen⟩ := spec_imp_exists
      (group_length_spec (alloc.vec.Vec.deref groups))
    exact ⟨len, hrun, by simpa only [viewGroups, List.length_map] using hlen⟩
  · intro groups left right _ hleft hright
    have hl : left.val < groups.val.length := by
      simpa only [viewGroups, List.length_map] using hleft
    have hr : right.val < groups.val.length := by
      simpa only [viewGroups, List.length_map] using hright
    obtain ⟨out, hrun, hpoints⟩ :=
      TouchHoldBodyFieldsProof.swap_groups_points groups left right hl hr
    exact ⟨out, hrun, swap_points_map (viewGroup decode) _ _ _ _ hpoints⟩

/-- Source length is read as a real Usize, so all rebuild-prefix capacities
follow from that read. No bound on IDs and no separate prefix-fit premise. -/
theorem rebuild_equiv (sourceFields : HoldFields Ops S Id) (ops : Ops)
    (eq : core.cmp.PartialEq Id Id) (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (source : S → TouchHoldRebuildModel.Holds)
    (reads : SourceAtoms (rebuildCall sourceFields eq cloneInst defaultInst) ops
      (representation decode) source) (holds : S) :
    ∃ out, touch_hold_rebuild.rebuild
        (rebuildCall sourceFields eq cloneInst defaultInst) holds ops = ok out ∧
      viewGroups decode out = TouchHoldRebuildModel.SchedulerReference.reference (source holds) ∧
      viewGroups decode out = TouchHoldRebuildModel.ChartLoaderReference.reference (source holds) ∧
      BodyFits capacity (viewGroups decode out) := by
  obtain ⟨len, _, hlen⟩ := reads.hold_length holds
  have hbound : (source holds).length ≤ Usize.max := by
    rw [hlen]
    scalar_tac
  have hfit := TouchHoldRebuildCapacityProof.prefix_fits_native (source holds) hbound
  have hempty : ∃ initial,
      (rebuildCall sourceFields eq cloneInst defaultInst).empty ops = ok initial ∧
      (representation decode).groups initial = [] := by
    refine ⟨alloc.vec.Vec.new _, rfl, rfl⟩
  exact TouchHoldRebuildTraversalProof.rebuild_equiv
    (rebuildCall sourceFields eq cloneInst defaultInst) ops (representation decode)
    source capacity reads
    (update_atoms sourceFields ops eq cloneInst defaultInst decode heq hclone filler hdefault)
    (reverse_atoms sourceFields ops eq cloneInst defaultInst decode) hempty holds hfit

end Verification.TouchHoldBodyRebuildProof
