import Verification.GeneratedTouchHoldBodyAdapter
import Verification.TouchHoldBodyErase
import Verification.TouchHoldBodyMajority

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyAdapterProof

open TouchHoldGroupsProof TouchHoldBodyReadsProof
open TouchHoldBodyViewPointsProof TouchHoldBodyUpdatesProof

open private touchHoldBodyGroupMajorityPressed
  registerTouchHoldBodyTrigger unregisterTouchHoldBodyTrigger
  exitTouchHoldBodyGroupMember from LnmaiCore.Scheduler

/-!
The dictionary below is the real stage6 extracted BodyGroups implementation for
TouchHoldBodyAdapter<Ops>. It is not an interpreted replacement dictionary.
Stage6: /tmp/lnmai-touch-hold-adapter-extraction-6/LnmaiCoreVerify.lean
SHA256: 6256a77c459e9d1b0974f172065a6bb01d9cd0f4276e06777e25f632c707af2c.
GeneratedTouchHoldBodyAdapter supplies the real stage6 additions while the
existing Generated retains its unchanged stage5 declarations.

All twelve BodyAtoms laws follow from actual record/Vec field reads and writes,
first-match scans, push/swap prepend, swap/shrink erase and guarded majority.
Only identifier PartialEq, Clone and Default are primitive premises. Capacity
is uniformly Usize.max. Decoding need not be injective; duplicate group IDs,
members and triggers are allowed, with first-match/first-erase semantics.

Register requires that its reference result fits; unregister and exit do not
grow storage. Natural's native dictionaries, allocator/backend and generic
Drop refinement remain outside this extracted-model theorem. Ops is arbitrary
and these body methods do not call it. Head/lifecycle/schedule forwarding is
proved separately. No compiler or test was run for this source delivery.
-/

variable {Id Ops : Type}

/-- Literal alias of the actual extracted Rust trait implementation. -/
abbrev bodyCall (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id) :
    touch_hold_schedule.BodyGroups
      (touch_hold_body_adapter.TouchHoldBodyAdapter Ops) (Groups Id) Id :=
  touch_hold_body_adapter.TouchHoldBodyAdapter.Insts.Lnmai_core_verifyTouch_hold_scheduleBodyGroupsVecTouchHoldBodyGroupStateId
    Ops eq cloneInst defaultInst

/-- Projection of the actual extracted three-field record and its Vec fields. -/
def representation (decode : Id → Nat) : BodyRepresentation (Groups Id) Id :=
  { groups := viewGroups decode, id := decode }

/-- All three limits come from the same native Usize bound. -/
def capacity : BodyCapacity :=
  { groups := Usize.max, members := Usize.max, triggers := Usize.max }

/-- Every existing actual Vec already satisfies the storage bound. This does
not assert spare capacity for the next prepend or append. -/
theorem native_fits (decode : Id → Nat) (groups : Groups Id) :
    BodyFits capacity ((representation decode).groups groups) := by
  exact TouchHoldBodyReadsProof.native_capacity decode groups

/-- Concrete BodyGroups dictionary, with no search/update/pass oracle.
The ops argument is the wrapped value, not its arbitrary underlying Ops field. -/
theorem body_atoms (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : touch_hold_body_adapter.TouchHoldBodyAdapter Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler) :
    BodyAtoms (bodyCall eq cloneInst defaultInst) ops
      (representation decode) capacity := by
  constructor
  · intro groups
    obtain ⟨len, hrun, hlen⟩ := spec_imp_exists
      (group_length_spec (alloc.vec.Vec.deref groups))
    refine ⟨len, hrun, ?_⟩
    simpa only [representation, viewGroups, List.length_map] using hlen.symm
  · intro groups index pre group rest id hat
    exact group_matches_point eq decode heq groups index pre group rest id hat
  · intro groups index pre group rest hat
    exact TouchHoldBodyMajorityProof.majority_point
      decode groups index pre group rest hat
  · intro groups index pre group rest note hat
    exact has_member_point eq decode heq groups index pre group rest note hat
  · intro groups index pre group rest note hat
    exact has_trigger_point eq decode heq groups index pre group rest note hat
  · intro groups index pre group rest note hat hroom
    exact prepend_member_point cloneInst decode groups index pre group rest note
      hat hroom (hclone note)
  · intro groups index pre group rest note hat hroom
    exact prepend_trigger_point cloneInst decode groups index pre group rest note
      hat hroom (hclone note)
  · intro groups index pre group rest note hat
    exact TouchHoldBodyEraseProof.erase_member_point eq cloneInst defaultInst
      decode groups index pre group rest note filler hat
      (fun value => heq value note) hdefault
  · intro groups index pre group rest note hat
    exact TouchHoldBodyEraseProof.erase_trigger_point eq cloneInst defaultInst
      decode groups index pre group rest note filler hat
      (fun value => heq value note) hdefault
  · intro groups index pre group rest hat
    exact members_empty_point decode groups index pre group rest hat
  · intro groups index pre group rest hat
    exact remove_group_point cloneInst defaultInst decode
      groups index pre group rest filler hat hdefault
  · intro groups id note hroom _ _
    exact append_point cloneInst decode groups id note hroom
      (hclone id) (hclone note)

/-- The actual wrapped search terminates and picks the first decoded match. -/
theorem find_body_spec (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : touch_hold_body_adapter.TouchHoldBodyAdapter Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (groups : Groups Id) (id : Id) :
    touch_hold_schedule.find_body (bodyCall eq cloneInst defaultInst)
      groups id ops ⦃ First LnmaiCore.TouchHoldBodyGroupState.groupId
        (viewGroups decode groups) (decode id) ⦄ := by
  exact TouchHoldGroupsProof.find_body_spec
    (bodyCall eq cloneInst defaultInst) ops (representation decode) capacity
    (body_atoms eq cloneInst defaultInst ops decode heq hclone filler hdefault)
    groups id

/-- The wrapped pressure query uses the first group's exact member/trigger
lengths. A later duplicate group never supplies the majority result. -/
theorem body_pressed_equiv (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : touch_hold_body_adapter.TouchHoldBodyAdapter Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (groups : Groups Id) (id : Id) :
    touch_hold_schedule.body_pressed (bodyCall eq cloneInst defaultInst)
      groups id ops ⦃ out =>
        out = touchHoldBodyGroupMajorityPressed (viewGroups decode groups) (decode id) ⦄ := by
  exact TouchHoldGroupsProof.body_pressed_equiv
    (bodyCall eq cloneInst defaultInst) ops (representation decode) capacity
    (body_atoms eq cloneInst defaultInst ops decode heq hclone filler hdefault)
    groups id

/-- Successful registration is bounded by the actual outer/member/trigger Vec
limits. The first matching group is updated; a missing group is appended. -/
theorem register_body_equiv (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : touch_hold_body_adapter.TouchHoldBodyAdapter Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (groups : Groups Id) (id note : Id)
    (hcapacity : BodyFits capacity (registerTouchHoldBodyTrigger
      (viewGroups decode groups) (decode id) (decode note))) :
    touch_hold_schedule.register_body (bodyCall eq cloneInst defaultInst)
      groups id note ops ⦃ out =>
        viewGroups decode out = registerTouchHoldBodyTrigger
          (viewGroups decode groups) (decode id) (decode note) ⦄ := by
  exact TouchHoldGroupsProof.register_body_equiv
    (bodyCall eq cloneInst defaultInst) ops (representation decode) capacity
    (body_atoms eq cloneInst defaultInst ops decode heq hclone filler hdefault)
    groups id note hcapacity

/-- Removes just the first matching trigger in the first matching group.
This entry does not need spare capacity. -/
theorem unregister_body_equiv (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : touch_hold_body_adapter.TouchHoldBodyAdapter Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (groups : Groups Id) (id note : Id) :
    touch_hold_schedule.unregister_body (bodyCall eq cloneInst defaultInst)
      groups id note ops ⦃ out =>
        viewGroups decode out = unregisterTouchHoldBodyTrigger
          (viewGroups decode groups) (decode id) (decode note) ⦄ := by
  exact TouchHoldGroupsProof.unregister_body_equiv
    (bodyCall eq cloneInst defaultInst) ops (representation decode) capacity
    (body_atoms eq cloneInst defaultInst ops decode heq hclone filler hdefault)
    groups id note

/-- Erases first occurrences of member and trigger, then removes only that
selected group when its remaining member Vec is empty. -/
theorem exit_body_equiv (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : touch_hold_body_adapter.TouchHoldBodyAdapter Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (groups : Groups Id) (id note : Id) :
    touch_hold_schedule.exit_body (bodyCall eq cloneInst defaultInst)
      groups id note ops ⦃ out =>
        viewGroups decode out = exitTouchHoldBodyGroupMember
          (viewGroups decode groups) (decode id) (decode note) ⦄ := by
  exact TouchHoldGroupsProof.exit_body_equiv
    (bodyCall eq cloneInst defaultInst) ops (representation decode) capacity
    (body_atoms eq cloneInst defaultInst ops decode heq hclone filler hdefault)
    groups id note

/-- The standalone runtime pressure helper is also the real extracted function.
Its narrower read-only proof needs equality only, with no Clone/Default premise. -/
theorem adapter_body_pressed_equiv (eq : core.cmp.PartialEq Id Id)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (id : Id) :
    touch_hold_body_adapter.body_pressed eq (alloc.vec.Vec.deref groups) id =
      ok (touchHoldBodyGroupMajorityPressed (viewGroups decode groups) (decode id)) := by
  exact TouchHoldBodyMajorityProof.body_pressed_view eq decode heq groups id

end Verification.TouchHoldBodyAdapterProof
