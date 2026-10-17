import Verification.GeneratedTouchHoldBodyAdapter
import Verification.TouchHoldBodyViewPoints

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyMajorityProof

open TouchHoldBodyReadsProof TouchHoldGroupsProof

open private hasStrictMajority touchHoldBodyGroupMajorityPressed
  touchHoldBodyGroupTriggeredCount touchHoldBodyGroupMemberCount
  from LnmaiCore.Scheduler

/-!
The actual stage6 extraction is
/tmp/lnmai-touch-hold-adapter-extraction-6/LnmaiCoreVerify.lean, SHA256
6256a77c459e9d1b0974f172065a6bb01d9cd0f4276e06777e25f632c707af2c.
Verification.GeneratedTouchHoldBodyAdapter must contain those actual additive
declarations; canonical Verification.Generated remains at stage5 unchanged.
This source has only been statically reviewed; main owns compilation.

The length calculation, both actual field reads, and the first-match wrapper
are unfolded from that extraction. Majority needs no equality, cloning, default,
uniqueness, vote-subset, or spare-capacity law. Its divisor is always two.
The pressed lookup additionally requires only a successful point equality law.
The decoded view is the existing projection of the actual generic record.
Natural representation, source decoding, allocation, and complete runtime
equivalence remain outside these theorem scopes.
-/

variable {Id : Type}

/-- Guarded division by two is the Scheduler's unbounded Nat majority test.
The zero-member guard is retained even when the trigger count is positive. -/
theorem strict_majority_nat_eq (triggeredLen memberLen : Nat) :
    (decide (0 < memberLen) && decide (memberLen / 2 < triggeredLen)) =
      hasStrictMajority triggeredLen memberLen := by
  simp only [hasStrictMajority,
    Nat.div_lt_iff_lt_mul (by decide : 0 < 2)]

/-- The actual extracted checked division always succeeds, including at the
maximum Usize. No bound on twice the trigger count is required. -/
theorem strict_majority_lengths_eq (triggeredLen memberLen : Usize) :
    touch_hold_body_adapter.strict_majority_lengths triggeredLen memberLen =
      ok (hasStrictMajority triggeredLen.val memberLen.val) := by
  by_cases hguard : memberLen > 0#usize
  · have hpositive : 0 < memberLen.val := by simpa using hguard
    obtain ⟨half, hdiv, hhalf⟩ :=
      UScalar.div_spec memberLen (y := 2#usize) (by simp)
    have hhalfNat : half.val = memberLen.val / 2 := by simpa using hhalf
    have hcomparison : decide (half < triggeredLen) =
        decide (memberLen.val < triggeredLen.val * 2) := by
      simp only [UScalar.lt_equiv, hhalfNat,
        Nat.div_lt_iff_lt_mul (by decide : 0 < 2)]
    simp only [touch_hold_body_adapter.strict_majority_lengths, hguard,
      ↓reduceIte, hdiv, bind_tc_ok]
    change ok (decide (half < triggeredLen)) = _
    rw [hcomparison]
    simp [hasStrictMajority, hpositive]
  · have hnonpositive : ¬0 < memberLen.val := by simpa using hguard
    simp [touch_hold_body_adapter.strict_majority_lengths, hguard,
      hasStrictMajority, hnonpositive]

/-- Two real in-bounds field reads followed by the extracted arithmetic.
No record representation or field-length equation is assumed. -/
theorem body_majority_eq (groups : Slice (TouchHoldBodyReadsProof.Group Id)) (index : Usize)
    (hindex : index.val < groups.val.length) :
    touch_hold_body_adapter.body_majority groups index =
      ok (hasStrictMajority
        (groups.val[index.val]).triggered_note_indices.val.length
        (groups.val[index.val]).member_note_indices.val.length) := by
  obtain ⟨triggeredLen, htriggers, htriggerLen⟩ := spec_imp_exists
    (trigger_length_spec groups index hindex)
  obtain ⟨memberLen, hmembers, hmemberLen⟩ := spec_imp_exists
    (member_length_spec groups index hindex)
  simp only [touch_hold_body_adapter.body_majority, htriggers, hmembers,
    bind_tc_ok, strict_majority_lengths_eq, htriggerLen, hmemberLen]

/-- Stable point API for the majority field of BodyAtoms. The outer prefix and
suffix select the existing group; decode need not be injective. -/
theorem majority_point (decode : Id → Nat) (groups : Groups Id) (index : Usize)
    (pre : List LnmaiCore.TouchHoldBodyGroupState)
    (group : LnmaiCore.TouchHoldBodyGroupState)
    (rest : List LnmaiCore.TouchHoldBodyGroupState)
    (hat : At (viewGroups decode groups) index pre group rest) :
    touch_hold_body_adapter.body_majority (alloc.vec.Vec.deref groups) index =
      ok (hasStrictMajority
        group.triggeredNoteIndices.length group.memberNoteIndices.length) := by
  obtain ⟨triggeredLen, htriggers, htriggerLen⟩ :=
    TouchHoldBodyViewPointsProof.trigger_length_point
      decode groups index pre group rest hat
  obtain ⟨memberLen, hmembers, hmemberLen⟩ :=
    TouchHoldBodyViewPointsProof.member_length_point
      decode groups index pre group rest hat
  simp only [touch_hold_body_adapter.body_majority, htriggers, hmembers, bind_tc_ok]
  simpa only [← htriggerLen, ← hmemberLen] using
    strict_majority_lengths_eq triggeredLen memberLen

/-- The real scan selects the first matching group, even if that group's
majority is false and a later duplicate would have returned true. -/
theorem body_pressed_key_eq (eq : core.cmp.PartialEq Id Id)
    (groups : Slice (TouchHoldBodyReadsProof.Group Id)) (id : Id) (key : Id → Nat) (target : Nat)
    (heq : ∀ value, eq.eq value id = ok (key value == target)) :
    touch_hold_body_adapter.body_pressed eq groups id =
      ok (match groups.val.find? (fun group => key group.group_id == target) with
        | none => false
        | some group => hasStrictMajority
            group.triggered_note_indices.val.length
            group.member_note_indices.val.length) := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (TouchHoldBodyScansProof.find_group_key_spec eq groups id key target heq)
  cases found with
  | none =>
      have href := find_none (fun group : TouchHoldBodyReadsProof.Group Id => key group.group_id)
        groups.val target hfirst
      simp only [touch_hold_body_adapter.body_pressed, hfind, bind_tc_ok, href]
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      obtain ⟨hindex, hgroup⟩ := TouchHoldBodyViewPointsProof.at_index hat
      have href := find_at (fun group : TouchHoldBodyReadsProof.Group Id => key group.group_id)
        groups.val target index pre group rest hat hprefix hmatch
      simp only [touch_hold_body_adapter.body_pressed, hfind, bind_tc_ok,
        body_majority_eq groups index hindex, hgroup, href]

/-- Scheduler lookup on the existing decoded view. Later duplicate IDs, member
entries, and trigger entries are unrestricted; only point equality is supplied. -/
theorem body_pressed_view (eq : core.cmp.PartialEq Id Id) (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (groups : Groups Id) (id : Id) :
    touch_hold_body_adapter.body_pressed eq (alloc.vec.Vec.deref groups) id =
      ok (touchHoldBodyGroupMajorityPressed (viewGroups decode groups) (decode id)) := by
  have hfind :
      (viewGroups decode groups).find? (fun group => group.groupId == decode id) =
        (groups.val.find? (fun group => decode group.group_id == decode id)).map
          (viewGroup decode) := by
    simp only [viewGroups, List.find?_map, Function.comp_def, viewGroup]
  rw [body_pressed_key_eq eq (alloc.vec.Vec.deref groups) id decode (decode id)
    (fun value => heq value id)]
  simp only [touchHoldBodyGroupMajorityPressed, hfind, alloc.vec.Vec.deref]
  cases groups.val.find? (fun group => decode group.group_id == decode id) <;>
    simp only [Option.map_none, Option.map_some, touchHoldBodyGroupTriggeredCount,
      touchHoldBodyGroupMemberCount, viewGroup, List.length_map]

end Verification.TouchHoldBodyMajorityProof
