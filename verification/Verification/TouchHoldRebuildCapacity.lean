import Verification.TouchHoldRebuildTraversal

/-! Concrete capacity witnesses for rebuilding from the original hold list.
Each processed source slot can add at most one group, member, or trigger.
The result does not require distinct group/note IDs or triggered membership.
It discharges the finite family of prefix bounds from one source-length bound;
representation, allocation, and the trait's point laws remain separate. -/

namespace Verification.TouchHoldRebuildCapacityProof

open LnmaiCore InputModel Lifecycle
open TouchHoldGroupsProof TouchHoldRebuildModel TouchHoldRebuildTraversalProof

def uniformCapacity (size : Nat) : BodyCapacity :=
  { groups := size, members := size, triggers := size }

theorem body_fits_mono (small large : BodyCapacity) (groups : Groups)
    (hgroups : small.groups ≤ large.groups)
    (hmembers : small.members ≤ large.members)
    (htriggers : small.triggers ≤ large.triggers)
    (hfit : BodyFits small groups) : BodyFits large groups := by
  refine ⟨Nat.le_trans hfit.1 hgroups, ?_⟩
  intro group hgroup
  exact ⟨Nat.le_trans (hfit.2 group hgroup).1 hmembers,
    Nat.le_trans (hfit.2 group hgroup).2 htriggers⟩

private theorem after_fields_bound (group : TouchHoldBodyGroupState)
    (note size : Nat) (triggered : Bool)
    (hmember : group.memberNoteIndices.length ≤ size)
    (htrigger : group.triggeredNoteIndices.length ≤ size) :
    (after group note triggered).memberNoteIndices.length ≤ size + 1 ∧
    (after group note triggered).triggeredNoteIndices.length ≤ size + 1 := by
  constructor
  · rw [member_length]
    split <;> omega
  · rw [trigger_length]
    split <;> split <;> omega

private theorem upsert_fields_bound (groups : Groups) (id note size : Nat)
    (triggered : Bool)
    (hfields : ∀ group ∈ groups, group.memberNoteIndices.length ≤ size ∧
      group.triggeredNoteIndices.length ≤ size) :
    ∀ group ∈ upsert id note triggered groups,
      group.memberNoteIndices.length ≤ size + 1 ∧
      group.triggeredNoteIndices.length ≤ size + 1 := by
  induction groups with
  | nil =>
      intro group hgroup
      have heq : group = fresh id note triggered := by
        simpa only [upsert, List.mem_singleton] using hgroup
      subst group
      cases triggered <;> simp [fresh]
  | cons head rest ih =>
      have hhead := hfields head (List.mem_cons_self)
      have hrest : ∀ group ∈ rest, group.memberNoteIndices.length ≤ size ∧
          group.triggeredNoteIndices.length ≤ size := by
        intro group hgroup
        exact hfields group (List.mem_cons_of_mem head hgroup)
      intro group hgroup
      by_cases hmatch : (head.groupId == id) = true
      · rw [upsert, if_pos hmatch] at hgroup
        rcases List.mem_cons.mp hgroup with heq | hmem
        · subst group
          exact after_fields_bound head note size triggered hhead.1 hhead.2
        · have hb := hrest group hmem
          exact ⟨by omega, by omega⟩
      · rw [upsert, if_neg hmatch] at hgroup
        rcases List.mem_cons.mp hgroup with heq | hmem
        · subst group
          exact ⟨by omega, by omega⟩
        · exact ih hrest group hmem

theorem step_fits (size : Nat) (note : HoldNote) (groups : Groups)
    (hfit : BodyFits (uniformCapacity size) groups) :
    BodyFits (uniformCapacity (size + 1)) (step note groups) := by
  have hlength := (step_length_bounds note groups).2
  refine ⟨Nat.le_trans hlength (Nat.add_le_add_right hfit.1 1), ?_⟩
  cases hid : note.touchHoldGroupId with
  | none =>
      intro group hgroup
      have hb := hfit.2 group (by simpa only [step, hid] using hgroup)
      exact ⟨by dsimp only [uniformCapacity] at *; omega,
        by dsimp only [uniformCapacity] at *; omega⟩
  | some id =>
      simpa only [step, hid] using
        upsert_fields_bound groups id note.params.noteIndex size
          note.touchHoldGroupTriggered hfit.2

theorem walk_fits (size : Nat) (holds : Holds) (groups : Groups)
    (hfit : BodyFits (uniformCapacity size) groups) :
    BodyFits (uniformCapacity (size + holds.length)) (walk holds groups) := by
  induction holds generalizing size groups with
  | nil => simpa only [walk, List.length_nil, Nat.add_zero] using hfit
  | cons pair rest ih =>
      rcases pair with ⟨area, note⟩
      simpa only [walk, List.length_cons, Nat.add_assoc, Nat.add_comm 1] using
        ih (size + 1) (step note groups) (step_fits size note groups hfit)

theorem walk_empty_fits (holds : Holds) :
    BodyFits (uniformCapacity holds.length) (walk holds []) := by
  have hempty : BodyFits (uniformCapacity 0) [] := by
    simp [BodyFits, uniformCapacity]
  simpa only [Nat.zero_add] using walk_fits 0 holds [] hempty

/-- Every actual prefix fits once the original source length fits all three
native buffers. Duplicate notes and absent group IDs need no extra hypotheses. -/
theorem prefix_fits_of_length (cap : BodyCapacity) (holds : Holds)
    (hgroups : holds.length ≤ cap.groups)
    (hmembers : holds.length ≤ cap.members)
    (htriggers : holds.length ≤ cap.triggers) : PrefixFits cap holds [] := by
  intro pre rest hsplit
  have hpre : pre.length ≤ holds.length := by
    rw [hsplit, List.length_append]
    omega
  exact body_fits_mono (uniformCapacity pre.length) cap (walk pre [])
    (Nat.le_trans hpre hgroups) (Nat.le_trans hpre hmembers)
    (Nat.le_trans hpre htriggers) (walk_empty_fits pre)

theorem prefix_fits_native (holds : Holds)
    (hsource : holds.length ≤ Aeneas.Std.Usize.max) :
    PrefixFits (uniformCapacity Aeneas.Std.Usize.max) holds [] :=
  prefix_fits_of_length _ holds hsource hsource hsource

end Verification.TouchHoldRebuildCapacityProof
