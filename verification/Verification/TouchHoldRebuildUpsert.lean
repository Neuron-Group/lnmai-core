import Verification.TouchHoldRebuildModel
import Verification.TouchHoldGroups

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 1000000

namespace Verification.TouchHoldRebuildUpsertProof

open TouchHoldRebuildModel TouchHoldGroupsProof
open private scanBody scan_spec from Verification.TouchHoldGroups

/-!
Actual extraction-3 scans and upsert, not replacement implementations. The
common scan proof imported above proves the real loop's first-match result and
termination from length and individual equality reads. The three local native
fragments below only regroup the binds of Generated.upsert; upsert_eq_branches
checks that regrouping before it is used.

All mutation laws describe one indexed field or one appended empty record.
remove_trigger is indexed deletion, not an assumed erase-first operation: its
first-occurrence property is derived from the actual trigger scan. Arbitrary
duplicate groups, members and triggers are permitted. Capacity bounds are on
the concrete reference output; success of allocation/cloning and exact Id/Vec
representations remain point-adapter obligations. The Rust runtime facades are
not extracted by these theorems. This file has not been compiled by its author.
-/

variable {S B Id Ops : Type}

/-- Read-only point laws. A successful length read also witnesses native-index
representability; no extra len + 1 bound is needed by the real scans. -/
structure ScanAtoms (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id)
    (ops : Ops) (rep : BodyRepresentation B Id) : Prop where
  group_length : ∀ groups, ∃ len, call.group_length ops groups = ok len ∧
    (rep.groups groups).length = len.val
  group_matches : ∀ groups index pre group rest id,
    At (rep.groups groups) index pre group rest →
    call.group_matches ops groups index id = ok (group.groupId == rep.id id)
  member_length : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    ∃ len, call.member_length ops groups index = ok len ∧
      group.memberNoteIndices.length = len.val
  member_matches : ∀ groups index pre group rest slot before item after note,
    At (rep.groups groups) index pre group rest →
    At group.memberNoteIndices slot before item after →
    call.member_matches ops groups index slot note = ok (item == rep.id note)
  trigger_length : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    ∃ len, call.trigger_length ops groups index = ok len ∧
      group.triggeredNoteIndices.length = len.val
  trigger_matches : ∀ groups index pre group rest slot before item after note,
    At (rep.groups groups) index pre group rest →
    At group.triggeredNoteIndices slot before item after →
    call.trigger_matches ops groups index slot note = ok (item == rep.id note)

/-- Only the four actual writes used by upsert, in addition to point reads. -/
structure UpdateAtoms (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id)
    (ops : Ops) (rep : BodyRepresentation B Id) (cap : BodyCapacity) : Prop where
  toScanAtoms : ScanAtoms call ops rep
  prepend_member : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    group.memberNoteIndices.length + 1 ≤ cap.members →
    ∃ next, call.prepend_member ops groups index note = ok next ∧
      rep.groups next = pre ++
        { group with memberNoteIndices := rep.id note :: group.memberNoteIndices } :: rest
  prepend_trigger : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    group.triggeredNoteIndices.length + 1 ≤ cap.triggers →
    ∃ next, call.prepend_trigger ops groups index note = ok next ∧
      rep.groups next = pre ++
        { group with triggeredNoteIndices := rep.id note :: group.triggeredNoteIndices } :: rest
  remove_trigger : ∀ groups index pre group rest (slot : Usize),
    At (rep.groups groups) index pre group rest →
    slot.val < group.triggeredNoteIndices.length →
    ∃ next, call.remove_trigger ops groups index slot = ok next ∧
      rep.groups next = pre ++
        { group with triggeredNoteIndices := group.triggeredNoteIndices.eraseIdx slot.val } :: rest
  append_empty : ∀ groups id, (rep.groups groups).length + 1 ≤ cap.groups →
    ∃ next, call.append_empty ops groups id = ok next ∧
      rep.groups next = rep.groups groups ++
        [{ groupId := rep.id id, memberNoteIndices := [], triggeredNoteIndices := [] }]

private theorem findIdx_none {α : Type} (key : α → Nat) (items : List α)
    (target : Nat) (h : NoKey key items target) :
    items.findIdx? (fun item => key item == target) = none := by
  revert h
  induction items with
  | nil => intro _; rfl
  | cons item rest ih =>
      intro h
      have hhead := h item (by simp)
      have htail : NoKey key rest target := fun x hx => h x (by simp [hx])
      simp [List.findIdx?_cons, hhead, ih htail]

private theorem findIdx_first {α : Type} (key : α → Nat)
    (pre : List α) (item : α) (rest : List α) (target : Nat)
    (hpre : NoKey key pre target) (hmatch : key item = target) :
    (pre ++ item :: rest).findIdx? (fun x => key x == target) = some pre.length := by
  revert hpre
  induction pre with
  | nil => intro _; simp [List.findIdx?_cons, hmatch]
  | cons head pre ih =>
      intro hpre
      have hhead := hpre head (by simp)
      have htail : NoKey key pre target := fun x hx => hpre x (by simp [hx])
      simp [List.findIdx?_cons, hhead, ih htail]

/-- Bridge the reused first-match predicate to the model's indexed operations. -/
theorem first_index_eq {α : Type} (key : α → Nat) (items : List α)
    (target : Nat) (found : Option Usize) (h : First key items target found) :
    items.findIdx? (fun item => key item == target) = found.map (fun i => i.val) := by
  cases found with
  | none => exact findIdx_none key items target h
  | some index =>
      obtain ⟨pre, item, rest, hat, hpre, hmatch⟩ := h
      change items.findIdx? (fun item => key item == target) = some index.val
      rw [hat.1, findIdx_first key pre item rest target hpre hmatch, hat.2]

variable (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id) (ops : Ops)
  (rep : BodyRepresentation B Id)

theorem find_group_spec (atoms : ScanAtoms call ops rep) (groups : B) (id : Id) :
    touch_hold_rebuild.find_group call groups id ops ⦃
      First TouchHoldBodyGroupState.groupId (rep.groups groups) (rep.id id) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.group_length groups
  have hloop := scan_spec TouchHoldBodyGroupState.groupId (rep.groups groups)
    (rep.id id) len (fun index => call.group_matches ops groups index id) hlen
    (fun index pre group rest hat => atoms.group_matches groups index pre group rest id hat)
  simpa only [touch_hold_rebuild.find_group, hlength, bind_tc_ok,
    touch_hold_rebuild.find_group_loop, touch_hold_rebuild.find_group_loop.body,
    scanBody] using hloop

theorem find_member_spec (atoms : ScanAtoms call ops rep) (groups : B)
    (index : Usize) (pre : Groups) (group : TouchHoldBodyGroupState)
    (rest : Groups) (note : Id) (hat : At (rep.groups groups) index pre group rest) :
    touch_hold_rebuild.find_member call groups index note ops ⦃
      First (fun n => n) group.memberNoteIndices (rep.id note) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.member_length groups index pre group rest hat
  have hloop := scan_spec (fun n => n) group.memberNoteIndices (rep.id note) len
    (fun slot => call.member_matches ops groups index slot note) hlen
    (fun slot before item after hitem =>
      atoms.member_matches groups index pre group rest slot before item after note hat hitem)
  simpa only [touch_hold_rebuild.find_member, hlength, bind_tc_ok,
    touch_hold_rebuild.find_member_loop, touch_hold_rebuild.find_member_loop.body,
    scanBody] using hloop

theorem find_trigger_spec (atoms : ScanAtoms call ops rep) (groups : B)
    (index : Usize) (pre : Groups) (group : TouchHoldBodyGroupState)
    (rest : Groups) (note : Id) (hat : At (rep.groups groups) index pre group rest) :
    touch_hold_rebuild.find_trigger call groups index note ops ⦃
      First (fun n => n) group.triggeredNoteIndices (rep.id note) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.trigger_length groups index pre group rest hat
  have hloop := scan_spec (fun n => n) group.triggeredNoteIndices (rep.id note) len
    (fun slot => call.trigger_matches ops groups index slot note) hlen
    (fun slot before item after hitem =>
      atoms.trigger_matches groups index pre group rest slot before item after note hat hitem)
  simpa only [touch_hold_rebuild.find_trigger, hlength, bind_tc_ok,
    touch_hold_rebuild.find_trigger_loop, touch_hold_rebuild.find_trigger_loop.body,
    scanBody] using hloop

def memberAfter (group : TouchHoldBodyGroupState) (note : Nat) : TouchHoldBodyGroupState :=
  { group with memberNoteIndices := insertMissing group.memberNoteIndices note }

def triggerAfter (group : TouchHoldBodyGroupState) (note : Nat) (triggered : Bool) :
    TouchHoldBodyGroupState :=
  { group with triggeredNoteIndices := if triggered then
      insertMissing group.triggeredNoteIndices note else removeFirst group.triggeredNoteIndices note }

theorem triggerAfter_memberAfter (group : TouchHoldBodyGroupState) (note : Nat)
    (triggered : Bool) :
    triggerAfter (memberAfter group note) note triggered = afterPoints group note triggered := by
  cases triggered <;> rfl

private theorem afterPoints_members (group : TouchHoldBodyGroupState) (note : Nat)
    (triggered : Bool) :
    (afterPoints group note triggered).memberNoteIndices = insertMissing group.memberNoteIndices note := by
  cases triggered <;> rfl

/-- These are literal bind fragments, with a checked factoring lemma below. -/
def memberNative (groups : B) (index : Usize) (note : Id) : Result B := do
  let found ← touch_hold_rebuild.find_member call groups index note ops
  if core.option.Option.is_none found then call.prepend_member ops groups index note
  else ok groups

def triggerNative (groups : B) (index : Usize) (note : Id) (triggered : Bool) : Result B := do
  let found ← touch_hold_rebuild.find_trigger call groups index note ops
  if triggered then
    if core.option.Option.is_none found then call.prepend_trigger ops groups index note
    else ok groups
  else
    match found with
    | none => ok groups
    | some slot => call.remove_trigger ops groups index slot

def freshNative (groups : B) (id note : Id) (triggered : Bool) : Result B := do
  let index ← call.group_length ops groups
  let empty ← call.append_empty ops groups id
  let member ← call.prepend_member ops empty index note
  if triggered then call.prepend_trigger ops member index note else ok member

theorem upsert_eq_branches (groups : B) (id note : Id) (triggered : Bool) :
    touch_hold_rebuild.upsert call groups id note triggered ops = (do
      let found ← touch_hold_rebuild.find_group call groups id ops
      match found with
      | none => freshNative call ops groups id note triggered
      | some index => do
          let middle ← memberNative call ops groups index note
          triggerNative call ops middle index note triggered) := by
  -- Compare the continuations explicitly: the extracted and factored do
  -- blocks need not elaborate to definitionally identical match functions.
  simp only [touch_hold_rebuild.upsert, freshNative, memberNative, triggerNative,
    bind_assoc_eq, bind_tc_ok, bind_eq_iff]
  all_goals
    repeat' first
      | rfl
      | (intro)
      | simp (config := { failIfUnchanged := true }) only
          [bind_assoc_eq, bind_tc_ok, bind_eq_iff]
      | split

variable (cap : BodyCapacity)

theorem member_native_equiv (atoms : UpdateAtoms call ops rep cap) (groups : B)
    (index : Usize) (pre : Groups) (group : TouchHoldBodyGroupState) (rest : Groups)
    (note : Id) (hat : At (rep.groups groups) index pre group rest)
    (hspace : (insertMissing group.memberNoteIndices (rep.id note)).length ≤ cap.members) :
    ∃ middle, memberNative call ops groups index note = ok middle ∧
      rep.groups middle = pre ++ memberAfter group (rep.id note) :: rest := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_member_spec call ops rep atoms.toScanAtoms groups index pre group rest note hat)
  have hindex := first_index_eq (fun n => n) group.memberNoteIndices (rep.id note) found hfirst
  cases found with
  | none =>
      change group.memberNoteIndices.findIdx? (fun n => n == rep.id note) = none at hindex
      have hroom : group.memberNoteIndices.length + 1 ≤ cap.members := by
        simpa only [insertMissing, hindex, List.length_cons] using hspace
      obtain ⟨middle, hwrite, hrep⟩ :=
        atoms.prepend_member groups index pre group rest note hat hroom
      refine ⟨middle, ?_, ?_⟩
      · simp only [memberNative, hfind, bind_tc_ok, core.option.Option.is_none, Option.isNone,
          ↓reduceIte, hwrite]
      · simpa only [memberAfter, insertMissing, hindex] using hrep
  | some slot =>
      change group.memberNoteIndices.findIdx? (fun n => n == rep.id note) = some slot.val at hindex
      refine ⟨groups, ?_, ?_⟩
      · simp only [memberNative, hfind, bind_tc_ok, core.option.Option.is_none, Option.isNone,
          Bool.false_eq_true, ↓reduceIte]
      · simpa only [memberAfter, insertMissing, hindex] using hat.1

theorem trigger_native_equiv (atoms : UpdateAtoms call ops rep cap) (groups : B)
    (index : Usize) (pre : Groups) (group : TouchHoldBodyGroupState) (rest : Groups)
    (note : Id) (triggered : Bool) (hat : At (rep.groups groups) index pre group rest)
    (hspace : (triggerAfter group (rep.id note) triggered).triggeredNoteIndices.length ≤ cap.triggers) :
    ∃ out, triggerNative call ops groups index note triggered = ok out ∧
      rep.groups out = pre ++ triggerAfter group (rep.id note) triggered :: rest := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_trigger_spec call ops rep atoms.toScanAtoms groups index pre group rest note hat)
  have hindex := first_index_eq (fun n => n) group.triggeredNoteIndices (rep.id note) found hfirst
  cases found with
  | none =>
      change group.triggeredNoteIndices.findIdx? (fun n => n == rep.id note) = none at hindex
      cases triggered with
      | false =>
          refine ⟨groups, ?_, ?_⟩
          · simp only [triggerNative, hfind, bind_tc_ok, Bool.false_eq_true, ↓reduceIte]
          · simpa only [triggerAfter, Bool.false_eq_true, ↓reduceIte, removeFirst, hindex] using hat.1
      | true =>
          have hroom : group.triggeredNoteIndices.length + 1 ≤ cap.triggers := by
            simpa only [triggerAfter, ↓reduceIte, insertMissing, hindex, List.length_cons] using hspace
          obtain ⟨out, hwrite, hrep⟩ := atoms.prepend_trigger groups index pre group rest note hat hroom
          refine ⟨out, ?_, ?_⟩
          · simp only [triggerNative, hfind, bind_tc_ok, core.option.Option.is_none, Option.isNone,
              ↓reduceIte, hwrite]
          · simpa only [triggerAfter, ↓reduceIte, insertMissing, hindex] using hrep
  | some slot =>
      change group.triggeredNoteIndices.findIdx? (fun n => n == rep.id note) = some slot.val at hindex
      cases triggered with
      | false =>
          have hbound : slot.val < group.triggeredNoteIndices.length :=
            scan_index_lt group.triggeredNoteIndices (fun n => n == rep.id note) slot.val hindex
          obtain ⟨out, hwrite, hrep⟩ := atoms.remove_trigger groups index pre group rest slot hat hbound
          refine ⟨out, ?_, ?_⟩
          · simp only [triggerNative, hfind, bind_tc_ok, Bool.false_eq_true, ↓reduceIte, hwrite]
          · simpa only [triggerAfter, Bool.false_eq_true, ↓reduceIte, removeFirst, hindex] using hrep
      | true =>
          refine ⟨groups, ?_, ?_⟩
          · simp only [triggerNative, hfind, bind_tc_ok, core.option.Option.is_none, Option.isNone,
              Bool.false_eq_true, ↓reduceIte]
          · simpa only [triggerAfter, ↓reduceIte, insertMissing, hindex] using hat.1

theorem fresh_native_equiv (atoms : UpdateAtoms call ops rep cap) (groups : B)
    (id note : Id) (triggered : Bool)
    (hfit : BodyFits cap (rep.groups groups ++ [fresh (rep.id id) (rep.id note) triggered])) :
    ∃ out, freshNative call ops groups id note triggered = ok out ∧
      rep.groups out = rep.groups groups ++ [fresh (rep.id id) (rep.id note) triggered] := by
  have hslot : (rep.groups groups).length + 1 ≤ cap.groups := by simpa using hfit.1
  have hlocal := hfit.2 (fresh (rep.id id) (rep.id note) triggered) (by simp)
  have hmember : 1 ≤ cap.members := by simpa only [fresh, List.length_cons, List.length_nil] using hlocal.1
  let emptyGroup : TouchHoldBodyGroupState :=
    { groupId := rep.id id, memberNoteIndices := [], triggeredNoteIndices := [] }
  let memberGroup : TouchHoldBodyGroupState :=
    { emptyGroup with memberNoteIndices := [rep.id note] }
  obtain ⟨index, hlength, hlen⟩ := atoms.toScanAtoms.group_length groups
  obtain ⟨empty, hempty, hemptyRep⟩ := atoms.append_empty groups id hslot
  have hemptyAt : At (rep.groups empty) index (rep.groups groups) emptyGroup [] :=
    ⟨hemptyRep, hlen⟩
  obtain ⟨member, hwrite, hmemberRep⟩ :=
    atoms.prepend_member empty index (rep.groups groups) emptyGroup [] note hemptyAt
      (by simpa only [emptyGroup, List.length_nil, Nat.zero_add] using hmember)
  have hmemberAt : At (rep.groups member) index (rep.groups groups) memberGroup [] :=
    ⟨hmemberRep, hlen⟩
  cases triggered with
  | false =>
      refine ⟨member, ?_, ?_⟩
      · simp only [freshNative, hlength, hempty, hwrite, bind_tc_ok, Bool.false_eq_true, ↓reduceIte]
      · simpa only [emptyGroup, fresh, Bool.false_eq_true, ↓reduceIte] using hmemberRep
  | true =>
      have htrigger : 1 ≤ cap.triggers := by
        simpa only [fresh, ↓reduceIte, List.length_cons, List.length_nil] using hlocal.2
      obtain ⟨out, hout, houtRep⟩ :=
        atoms.prepend_trigger member index (rep.groups groups) memberGroup [] note hmemberAt
          (by simpa only [memberGroup, emptyGroup, List.length_nil, Nat.zero_add] using htrigger)
      refine ⟨out, ?_, ?_⟩
      · simp only [freshNative, hlength, hempty, hwrite, hout, bind_tc_ok, ↓reduceIte]
      · simpa only [memberGroup, emptyGroup, fresh, ↓reduceIte] using houtRep

/-- Complete real upsert: three real searches, exact mutation order, and all
Boolean branches. The only non-atomic premise is a numerical output capacity. -/
theorem upsert_equiv (atoms : UpdateAtoms call ops rep cap) (groups : B)
    (id note : Id) (triggered : Bool)
    (hfit : BodyFits cap (upsert (rep.id id) (rep.id note) triggered (rep.groups groups))) :
    ∃ out, touch_hold_rebuild.upsert call groups id note triggered ops = ok out ∧
      rep.groups out = upsert (rep.id id) (rep.id note) triggered (rep.groups groups) := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_group_spec call ops rep atoms.toScanAtoms groups id)
  cases found with
  | none =>
      have href : upsert (rep.id id) (rep.id note) triggered (rep.groups groups) =
          rep.groups groups ++ [fresh (rep.id id) (rep.id note) triggered] := by
        simpa only [upsertIndexed_eq] using
          upsert_absent (rep.groups groups) (rep.id id) (rep.id note) triggered hfirst
      obtain ⟨out, hout, hrep⟩ := fresh_native_equiv call ops rep cap atoms groups id note triggered
        (by simpa only [href] using hfit)
      refine ⟨out, ?_, hrep.trans href.symm⟩
      simp only [upsert_eq_branches, hfind, bind_tc_ok, hout]
  | some index =>
      obtain ⟨pre, group, rest, hat, hpre, hmatch⟩ := hfirst
      have href : upsert (rep.id id) (rep.id note) triggered (rep.groups groups) =
          pre ++ after group (rep.id note) triggered :: rest := by
        rw [hat.1]
        simpa only [upsertIndexed_eq] using
          upsert_first pre group rest (rep.id id) (rep.id note) triggered hpre hmatch
      have hfits : BodyFits cap (pre ++ after group (rep.id note) triggered :: rest) := by
        simpa only [href] using hfit
      have hlocal := hfits.2 (after group (rep.id note) triggered) (by simp)
      have hmembers : (insertMissing group.memberNoteIndices (rep.id note)).length ≤ cap.members := by
        simpa only [← afterPoints_eq, afterPoints_members] using hlocal.1
      obtain ⟨middle, hmiddle, hmiddleRep⟩ :=
        member_native_equiv call ops rep cap atoms groups index pre group rest note hat hmembers
      have hmiddleAt : At (rep.groups middle) index pre (memberAfter group (rep.id note)) rest :=
        ⟨hmiddleRep, hat.2⟩
      have htriggers :
          (triggerAfter (memberAfter group (rep.id note)) (rep.id note) triggered).triggeredNoteIndices.length ≤
            cap.triggers := by
        simpa only [triggerAfter_memberAfter, afterPoints_eq] using hlocal.2
      obtain ⟨out, hout, hrep⟩ := trigger_native_equiv call ops rep cap atoms middle index pre
        (memberAfter group (rep.id note)) rest note triggered hmiddleAt htriggers
      refine ⟨out, ?_, ?_⟩
      · simp only [upsert_eq_branches, hfind, hmiddle, hout, bind_tc_ok]
      · rw [href]
        simpa only [triggerAfter_memberAfter, afterPoints_eq] using hrep

theorem upsert_indexed_equiv (atoms : UpdateAtoms call ops rep cap) (groups : B)
    (id note : Id) (triggered : Bool)
    (hfit : BodyFits cap (upsertIndexed (rep.id id) (rep.id note) triggered (rep.groups groups))) :
    ∃ out, touch_hold_rebuild.upsert call groups id note triggered ops = ok out ∧
      rep.groups out = upsertIndexed (rep.id id) (rep.id note) triggered (rep.groups groups) := by
  simpa only [upsertIndexed_eq] using upsert_equiv call ops rep cap atoms groups id note triggered
    (by simpa only [upsertIndexed_eq] using hfit)

end Verification.TouchHoldRebuildUpsertProof
