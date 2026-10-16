import LnmaiCore.Scheduler
import LnmaiCore.ChartLoader
import Batteries.Tactic.OpenPrivate
import Init.Data.List.Find
import Init.Data.List.Erase
import Init.Data.List.Nat.Modify
import Lean.Elab.Tactic.Omega

/-!
Reference factoring for touch_hold_rebuild.rs. No Generated interface is invented:
this file proves concrete list equations, not extraction or Rust-facade correctness.
The new extracted scans, source loop, swap reversal, and point-adapter instances
must subsequently be connected to these equations by the mainline worker.

The original Scheduler and ChartLoader recursions remain the references. There
is no uniqueness assumption, no deduplication, and no whole-upsert/rebuild law
supplied as a hypothesis. All first-match and erase-first equations below hold
for arbitrary lists, including repeated group IDs and repeated member/trigger IDs.
-/

namespace Verification.TouchHoldRebuildModel
open LnmaiCore InputModel Lifecycle

abbrev Groups := List TouchHoldBodyGroupState
abbrev Holds := List (SensorArea × HoldNote)

/-- The exact reference record update, retaining its Boolean branch order. -/
def after (group : TouchHoldBodyGroupState) (note : Nat) (triggered : Bool) :
    TouchHoldBodyGroupState :=
  { group with
    memberNoteIndices := if group.memberNoteIndices.contains note then
      group.memberNoteIndices else note :: group.memberNoteIndices
    triggeredNoteIndices := if triggered && !group.triggeredNoteIndices.contains note then
      note :: group.triggeredNoteIndices
      else if !triggered then group.triggeredNoteIndices.erase note
      else group.triggeredNoteIndices }

def fresh (id note : Nat) (triggered : Bool) : TouchHoldBodyGroupState :=
  { groupId := id, memberNoteIndices := [note]
    triggeredNoteIndices := if triggered then [note] else [] }

/-- Original first-match recursive update, used only as a proved reference model. -/
def upsert (id note : Nat) (triggered : Bool) : Groups → Groups
  | [] => [fresh id note triggered]
  | group :: rest =>
      if group.groupId == id then after group note triggered :: rest
      else group :: upsert id note triggered rest

/-- Specification of a successful explicit member scan followed by a point write. -/
def insertMissing (items : List Nat) (note : Nat) : List Nat :=
  match items.findIdx? (fun item => item == note) with
  | none => note :: items
  | some _ => items

/-- The index comes from the FIRST successful scan, not a filter over all matches. -/
def removeFirst (items : List Nat) (note : Nat) : List Nat :=
  match items.findIdx? (fun item => item == note) with
  | none => items
  | some index => items.eraseIdx index

theorem find_note_none_iff (items : List Nat) (note : Nat) :
    items.findIdx? (fun item => item == note) = none ↔ items.contains note = false := by
  induction items with
  | nil => simp
  | cons item rest ih =>
      by_cases h : item = note
      · subst item
        simp [List.findIdx?_cons]
      · simpa [List.findIdx?_cons, List.contains_cons, h, Ne.symm h] using ih

theorem insertMissing_eq (items : List Nat) (note : Nat) :
    insertMissing items note = if items.contains note then items else note :: items := by
  cases hc : items.contains note with
  | false =>
      have hf := (find_note_none_iff items note).2 hc
      simp [insertMissing, hf, hc]
  | true =>
      cases hf : items.findIdx? (fun item => item == note) with
      | none =>
          have hn := (find_note_none_iff items note).1 hf
          simp [hc] at hn
      | some index => simp [insertMissing, hf, hc]

theorem removeFirst_eq_erase (items : List Nat) (note : Nat) :
    removeFirst items note = items.erase note := by
  induction items with
  | nil => rfl
  | cons item rest ih =>
      cases hm : item == note with
      | true => simp [removeFirst, List.findIdx?_cons, List.erase_cons, hm]
      | false =>
          cases hf : rest.findIdx? (fun item => item == note) <;>
            simpa [removeFirst, List.findIdx?_cons, List.erase_cons, hm, hf] using
              congrArg (List.cons item) ih

/-- Successful scan results are always within the ORIGINAL scanned list length. -/
theorem scan_index_lt {α : Type} (items : List α) (predicate : α → Bool) (index : Nat)
    (h : items.findIdx? predicate = some index) : index < items.length :=
  (List.findIdx?_eq_some_iff_findIdx_eq.mp h).1

/-- Two independent member/trigger point edits, with no hidden group algorithm. -/
def afterPoints (group : TouchHoldBodyGroupState) (note : Nat) (triggered : Bool) :
    TouchHoldBodyGroupState :=
  let memberGroup := { group with memberNoteIndices := insertMissing group.memberNoteIndices note }
  if triggered then
    { memberGroup with triggeredNoteIndices := insertMissing memberGroup.triggeredNoteIndices note }
  else
    { memberGroup with triggeredNoteIndices := removeFirst memberGroup.triggeredNoteIndices note }

theorem afterPoints_eq (group : TouchHoldBodyGroupState) (note : Nat) (triggered : Bool) :
    afterPoints group note triggered = after group note triggered := by
  cases triggered <;> cases hc : group.triggeredNoteIndices.contains note <;>
    simp [afterPoints, after, insertMissing_eq, removeFirst_eq_erase, hc]

/-- Append-empty, prepend-member, and optional prepend-trigger in that order. -/
def freshPoints (id note : Nat) (triggered : Bool) : TouchHoldBodyGroupState :=
  let empty : TouchHoldBodyGroupState :=
    { groupId := id, memberNoteIndices := [], triggeredNoteIndices := [] }
  let member := { empty with memberNoteIndices := [note] }
  if triggered then { member with triggeredNoteIndices := [note] } else member

theorem freshPoints_eq (id note : Nat) (triggered : Bool) :
    freshPoints id note triggered = fresh id note triggered := by
  cases triggered <;> rfl

/-- The first-match indexed interpretation of the Rust upsert. The modifier is
fully defined above; it is not an assumed callback or a lifecycle operation. -/
def upsertIndexed (id note : Nat) (triggered : Bool) (groups : Groups) : Groups :=
  match groups.findIdx? (fun group => group.groupId == id) with
  | none => groups ++ [freshPoints id note triggered]
  | some index => groups.modify index (fun group => afterPoints group note triggered)

theorem upsertIndexed_eq (id note : Nat) (triggered : Bool) (groups : Groups) :
    upsertIndexed id note triggered groups = upsert id note triggered groups := by
  induction groups with
  | nil => simp [upsertIndexed, upsert, freshPoints_eq]
  | cons group rest ih =>
      by_cases hm : group.groupId = id
      · simp [upsertIndexed, upsert, List.findIdx?_cons, hm, afterPoints_eq]
      · cases hf : rest.findIdx? (fun item => item.groupId == id) <;>
          simpa [upsertIndexed, upsert, List.findIdx?_cons, hm, hf,
            afterPoints_eq, freshPoints_eq] using congrArg (List.cons group) ih

/-- No statement constrains the suffix: later equal group IDs survive verbatim. -/
def NoGroup (groups : Groups) (id : Nat) : Prop :=
  ∀ group ∈ groups, group.groupId ≠ id

theorem upsert_skip (pre rest : Groups) (id note : Nat) (triggered : Bool)
    (hpre : NoGroup pre id) :
    upsert id note triggered (pre ++ rest) = pre ++ upsert id note triggered rest := by
  revert hpre
  induction pre with
  | nil => intro _; rfl
  | cons group pre ih =>
      intro hpre
      have hhead := hpre group (by simp)
      have htail : NoGroup pre id := fun other hmem => hpre other (by simp [hmem])
      simpa [upsert, hhead] using congrArg (List.cons group) (ih htail)

theorem upsert_first (pre : Groups) (group : TouchHoldBodyGroupState) (rest : Groups)
    (id note : Nat) (triggered : Bool) (hpre : NoGroup pre id) (hmatch : group.groupId = id) :
    upsertIndexed id note triggered (pre ++ group :: rest) =
      pre ++ after group note triggered :: rest := by
  rw [upsertIndexed_eq, upsert_skip pre (group :: rest) id note triggered hpre]
  simp [upsert, hmatch]

theorem upsert_absent (groups : Groups) (id note : Nat) (triggered : Bool)
    (h : NoGroup groups id) :
    upsertIndexed id note triggered groups = groups ++ [fresh id note triggered] := by
  rw [upsertIndexed_eq]
  simpa [upsert] using upsert_skip groups [] id note triggered h

/-- Existing duplicate members keep their exact count; only absence adds one. -/
theorem member_length (group : TouchHoldBodyGroupState) (note : Nat) (triggered : Bool) :
    (after group note triggered).memberNoteIndices.length =
      if group.memberNoteIndices.contains note then group.memberNoteIndices.length
      else group.memberNoteIndices.length + 1 := by
  cases h : group.memberNoteIndices.contains note <;> simp [after, h]

/-- A false trigger deletes ONE occurrence even when the stored list has duplicates. -/
theorem trigger_length (group : TouchHoldBodyGroupState) (note : Nat) (triggered : Bool) :
    (after group note triggered).triggeredNoteIndices.length =
      if triggered then
        if group.triggeredNoteIndices.contains note then group.triggeredNoteIndices.length
        else group.triggeredNoteIndices.length + 1
      else
        if group.triggeredNoteIndices.contains note then group.triggeredNoteIndices.length - 1
        else group.triggeredNoteIndices.length := by
  cases triggered <;> by_cases h : note ∈ group.triggeredNoteIndices <;>
    simp [after, List.contains_eq_mem, List.length_erase, h]

theorem upsert_length (groups : Groups) (id note : Nat) (triggered : Bool) :
    (upsertIndexed id note triggered groups).length = groups.length +
      (if (groups.findIdx? (fun group => group.groupId == id)).isNone then 1 else 0) := by
  cases hf : groups.findIdx? (fun group => group.groupId == id) <;>
    simp [upsertIndexed, hf]

def step (note : HoldNote) (groups : Groups) : Groups :=
  match note.touchHoldGroupId with
  | none => groups
  | some id => upsert id note.params.noteIndex note.touchHoldGroupTriggered groups

def walk : Holds → Groups → Groups
  | [], groups => groups
  | (_, note) :: rest, groups => walk rest (step note groups)

def indexedStep (note : HoldNote) (groups : Groups) : Groups :=
  match note.touchHoldGroupId with
  | none => groups
  | some id => upsertIndexed id note.params.noteIndex note.touchHoldGroupTriggered groups

def indexedWalk : Holds → Groups → Groups
  | [], groups => groups
  | (_, note) :: rest, groups => indexedWalk rest (indexedStep note groups)

theorem indexedStep_eq (note : HoldNote) (groups : Groups) :
    indexedStep note groups = step note groups := by
  cases hg : note.touchHoldGroupId <;> simp [indexedStep, step, hg, upsertIndexed_eq]

theorem indexedWalk_eq (holds : Holds) (groups : Groups) :
    indexedWalk holds groups = walk holds groups := by
  induction holds generalizing groups with
  | nil => rfl
  | cons pair rest ih =>
      rcases pair with ⟨area, note⟩
      simp only [indexedWalk, walk, indexedStep_eq, ih]

/-- Reverse once, after consuming the entire remaining source list. -/
def rebuildFrom (holds : Holds) (groups : Groups) : Groups :=
  (indexedWalk holds groups).reverse

def rebuild (holds : Holds) : Groups := rebuildFrom holds []

@[simp] theorem rebuildFrom_nil (groups : Groups) : rebuildFrom [] groups = groups.reverse := rfl

theorem rebuildFrom_cons (area : SensorArea) (note : HoldNote) (rest : Holds) (groups : Groups) :
    rebuildFrom ((area, note) :: rest) groups = rebuildFrom rest (indexedStep note groups) := rfl

theorem walk_append (first rest : Holds) (groups : Groups) :
    walk (first ++ rest) groups = walk rest (walk first groups) := by
  induction first generalizing groups with
  | nil => rfl
  | cons pair first ih =>
      rcases pair with ⟨area, note⟩
      simpa only [List.cons_append, walk] using ih (step note groups)

/-- Chunked traversal must pass the UNREVERSED intermediate accumulator. -/
theorem rebuildFrom_append (first rest : Holds) (groups : Groups) :
    rebuildFrom (first ++ rest) groups = rebuildFrom rest (indexedWalk first groups) := by
  simp only [rebuildFrom, indexedWalk_eq, walk_append]

theorem step_length_bounds (note : HoldNote) (groups : Groups) :
    groups.length ≤ (step note groups).length ∧ (step note groups).length ≤ groups.length + 1 := by
  cases hg : note.touchHoldGroupId with
  | none => simp [step, hg]
  | some id =>
      rw [step, hg]
      dsimp only
      rw [← upsertIndexed_eq, upsert_length]
      cases (groups.findIdx? (fun group => group.groupId == id)).isNone <;> simp

theorem walk_length_bounds (holds : Holds) (groups : Groups) :
    groups.length ≤ (walk holds groups).length ∧
      (walk holds groups).length ≤ groups.length + holds.length := by
  induction holds generalizing groups with
  | nil => simp [walk]
  | cons pair rest ih =>
      rcases pair with ⟨area, note⟩
      have hs := step_length_bounds note groups
      have hr := ih (step note groups)
      simp only [walk, List.length_cons]
      constructor <;> omega

theorem rebuildFrom_length_bounds (holds : Holds) (groups : Groups) :
    groups.length ≤ (rebuildFrom holds groups).length ∧
      (rebuildFrom holds groups).length ≤ groups.length + holds.length := by
  simpa only [rebuildFrom, indexedWalk_eq, List.length_reverse] using
    walk_length_bounds holds groups

namespace SchedulerReference
open private touchHoldBodyGroupStatesFromHolds touchHoldBodyGroupStatesFromHolds.loop
  from LnmaiCore.Scheduler

/-- Alias, not a replacement of the original reference recursion. -/
def resume (holds : Holds) (groups : Groups) : Groups :=
  touchHoldBodyGroupStatesFromHolds.loop holds groups

def reference (holds : Holds) : Groups := touchHoldBodyGroupStatesFromHolds holds

@[simp] theorem resume_nil (groups : Groups) : resume [] groups = groups.reverse := rfl

theorem resume_cons (area : SensorArea) (note : HoldNote) (rest : Holds) (groups : Groups) :
    resume ((area, note) :: rest) groups = resume rest (step note groups) := by
  cases hg : note.touchHoldGroupId <;>
    simp only [resume, touchHoldBodyGroupStatesFromHolds.loop, step, hg] <;> rfl

theorem resume_eq_walk (holds : Holds) (groups : Groups) :
    resume holds groups = (walk holds groups).reverse := by
  induction holds generalizing groups with
  | nil => rfl
  | cons pair rest ih =>
      rcases pair with ⟨area, note⟩
      rw [resume_cons]
      exact ih (step note groups)
end SchedulerReference

namespace ChartLoaderReference
open private touchHoldBodyGroupStatesFromHolds touchHoldBodyGroupStatesFromHolds.loop
  from LnmaiCore.ChartLoader

def resume (holds : Holds) (groups : Groups) : Groups :=
  touchHoldBodyGroupStatesFromHolds.loop holds groups

def reference (holds : Holds) : Groups := touchHoldBodyGroupStatesFromHolds holds

@[simp] theorem resume_nil (groups : Groups) : resume [] groups = groups.reverse := rfl

theorem resume_cons (area : SensorArea) (note : HoldNote) (rest : Holds) (groups : Groups) :
    resume ((area, note) :: rest) groups = resume rest (step note groups) := by
  cases hg : note.touchHoldGroupId <;>
    simp only [resume, touchHoldBodyGroupStatesFromHolds.loop, step, hg] <;> rfl

theorem resume_eq_walk (holds : Holds) (groups : Groups) :
    resume holds groups = (walk holds groups).reverse := by
  induction holds generalizing groups with
  | nil => rfl
  | cons pair rest ih =>
      rcases pair with ⟨area, note⟩
      rw [resume_cons]
      exact ih (step note groups)
end ChartLoaderReference

/-- Unconditional semantic target for the future extracted rebuild_from theorem. -/
theorem model_resume_eq_scheduler (holds : Holds) (groups : Groups) :
    rebuildFrom holds groups = SchedulerReference.resume holds groups := by
  rw [rebuildFrom, indexedWalk_eq]
  exact (SchedulerReference.resume_eq_walk holds groups).symm

theorem model_rebuild_eq_scheduler (holds : Holds) :
    rebuild holds = SchedulerReference.reference holds := by
  change rebuildFrom holds [] = SchedulerReference.resume holds []
  exact model_resume_eq_scheduler holds []

theorem model_rebuild_eq_chartLoader (holds : Holds) :
    rebuild holds = ChartLoaderReference.reference holds := by
  change (indexedWalk holds []).reverse = ChartLoaderReference.resume holds []
  rw [indexedWalk_eq]
  exact (ChartLoaderReference.resume_eq_walk holds []).symm

theorem scheduler_eq_chartLoader (holds : Holds) :
    SchedulerReference.reference holds = ChartLoaderReference.reference holds := by
  rw [← model_rebuild_eq_scheduler, model_rebuild_eq_chartLoader]

end Verification.TouchHoldRebuildModel

