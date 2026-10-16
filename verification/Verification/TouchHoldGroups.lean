import Verification.Generated
import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 800000

namespace Verification.TouchHoldGroupsProof

open private hasStrictMajority groupShareResult registerTouchGroupResult
  registerTouchGroupResult.loop touchHoldBodyGroupMajorityPressed
  touchHoldBodyGroupTriggeredCount touchHoldBodyGroupMemberCount
  registerTouchHoldBodyTrigger registerTouchHoldBodyTrigger.loop
  unregisterTouchHoldBodyTrigger unregisterTouchHoldBodyTrigger.loop
  exitTouchHoldBodyGroupMember exitTouchHoldBodyGroupMember.loop
  from LnmaiCore.Scheduler

/-!
Actual Generated touch_hold_schedule group kernels, with arbitrary represented
containers and numbers. The two extracted scans reduce to the same loop body;
scan_spec proves termination and the FIRST matching index from point reads.

All mutation contracts select one indexed group/field. Membership insertion is
prepend, deletion is List.erase (FIRST occurrence). There is no uniqueness,
nonempty-group, vote-subset, or whole-group-operation equivalence premise.

Capacity conditions constrain the reference output of registration. The
individual Result.ok point laws still include successful allocation/cloning;
capacity alone is not an allocator-success theorem. Concrete Rust Vec/Natural/
time/grade facades remain unextracted adapter obligations. No new opaque
function or proof placeholder is declared. This source awaits mainline Lean
acceptance; this agent has not run a compiler or verification worker.
-/

/-- A point access describes the entire unchanged prefix and suffix. -/
def At {α : Type} (items : List α) (index : Usize)
    (pre : List α) (item : α) (rest : List α) : Prop :=
  items = pre ++ item :: rest ∧ pre.length = index.val

def NoKey {α : Type} (key : α → Nat) (items : List α) (target : Nat) : Prop :=
  ∀ item ∈ items, key item ≠ target

/-- Later duplicate IDs are unrestricted, including their majority/vote data. -/
def First {α : Type} (key : α → Nat) (items : List α) (target : Nat) :
    Option Usize → Prop
  | none => NoKey key items target
  | some index => ∃ pre item rest,
      At items index pre item rest ∧ NoKey key pre target ∧ key item = target

private theorem increment_index (index len : Usize) (h : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hvalue⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hvalue⟩

/-- Definitional common body of the two actual extracted searches. -/
private def scanBody (read : Usize → Result Bool) (len index : Usize) :
    Result (ControlFlow Usize (Option Usize)) := do
  if index < len then
    let found ← read index
    if found then ok (.done (some index))
    else
      let next ← index + 1#usize
      ok (.cont next)
  else ok (.done none)

/-- Every continued iteration advances one slot. The native increment is safe
under index < len, even when len is the largest representable Usize. -/
private theorem scan_spec {α : Type} (key : α → Nat) (items : List α)
    (target : Nat) (len : Usize) (read : Usize → Result Bool)
    (hlen : items.length = len.val)
    (hread : ∀ index pre item rest, At items index pre item rest →
      read index = ok (key item == target)) :
    loop (scanBody read len) 0#usize ⦃ First key items target ⦄ := by
  apply loop.spec_decr_nat (fun (index : Usize) => len.val - index.val)
    (fun index => ∃ pre rest, items = pre ++ rest ∧
      pre.length = index.val ∧ NoKey key pre target)
  · rintro index ⟨pre, rest, hitems, hindex, hprefix⟩
    have hlength : len.val = pre.length + rest.length := by
      rw [← hlen, hitems, List.length_append]
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons item rest =>
          have hpoint := hread index pre item rest ⟨hitems, hindex⟩
          by_cases hmatch : key item = target
          · have hreadTrue : read index = ok true := by simpa [hmatch] using hpoint
            apply exists_imp_spec
            refine ⟨.done (some index), ?_, ?_⟩
            · simp only [scanBody, hmore, ↓reduceIte, hreadTrue, bind_tc_ok]
            · exact ⟨pre, item, rest, ⟨hitems, hindex⟩, hprefix, hmatch⟩
          · have hreadFalse : read index = ok false := by
              simpa only [beq_eq_false_iff_ne.mpr hmatch] using hpoint
            obtain ⟨next, hnext, hvalue⟩ := increment_index index len hlt
            have hprefix' : NoKey key (pre ++ [item]) target := by
              intro other hmem
              simp only [List.mem_append, List.mem_singleton] at hmem
              rcases hmem with hmem | rfl
              · exact hprefix other hmem
              · exact hmatch
            apply exists_imp_spec
            refine ⟨.cont next, ?_, ?_⟩
            · simp only [scanBody, hmore, ↓reduceIte, hreadFalse, bind_tc_ok,
                Bool.false_eq_true, hnext]
            · dsimp only
              refine ⟨⟨pre ++ [item], rest, ?_, ?_, hprefix'⟩, ?_⟩
              · simpa only [List.append_assoc, List.singleton_append] using hitems
              · simp [List.length_append, hindex, hvalue]
              · omega
    · have hstop : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons item rest => simp only [List.length_cons] at hlength; omega
      have hsame : items = pre := by simpa only [hempty, List.append_nil] using hitems
      apply exists_imp_spec
      refine ⟨.done none, ?_, ?_⟩
      · simp only [scanBody, hmore, ↓reduceIte]
      · simpa only [First, hsame] using hprefix
  · exact ⟨[], items, by simp, by simp, by simp [NoKey]⟩

/-- Reference lookup skips precisely the unmatched prefix. -/
theorem find_skip {α : Type} (key : α → Nat) (pre rest : List α) (target : Nat)
    (hprefix : NoKey key pre target) :
    (pre ++ rest).find? (fun item => key item == target) =
      rest.find? (fun item => key item == target) := by
  revert hprefix
  induction pre with
  | nil => intro _; rfl
  | cons item pre ih =>
      intro hprefix
      have hhead := hprefix item (by simp)
      have htail : NoKey key pre target := fun x hx => hprefix x (by simp [hx])
      simpa [List.find?_cons, hhead] using ih htail

theorem find_none {α : Type} (key : α → Nat) (items : List α) (target : Nat)
    (h : NoKey key items target) :
    items.find? (fun item => key item == target) = none := by
  have hs := find_skip key items [] target h
  simpa using hs

theorem find_at {α : Type} (key : α → Nat) (items : List α) (target : Nat)
    (index : Usize) (pre : List α) (item : α) (rest : List α)
    (hat : At items index pre item rest) (hprefix : NoKey key pre target)
    (hmatch : key item = target) :
    items.find? (fun entry => key entry == target) = some item := by
  rw [hat.1, find_skip key pre (item :: rest) target hprefix]
  simp [hmatch]

structure HeadRepresentation (G Id Grade D : Type) where
  groups : G → List GroupState
  id : Id → Nat
  grade : Grade → JudgeGrade
  duration : D → Duration

/-- Exact reads and one indexed field write per law. Only append needs a new
outer-list slot; arithmetic and allocation success stay in these point laws. -/
structure HeadAtoms {G Id Grade D Ops : Type}
    (call : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : HeadRepresentation G Id Grade D) (capacity : Nat) : Prop where
  length : ∀ groups, ∃ len, call.head_length ops groups = ok len ∧
    (rep.groups groups).length = len.val
  «matches» : ∀ groups index pre group rest id,
    At (rep.groups groups) index pre group rest →
    call.head_matches ops groups index id = ok (group.groupId == rep.id id)
  majority : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    call.head_majority ops groups index = ok (hasStrictMajority group.count group.size)
  result : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    ∃ grade diff, call.head_result ops groups index = ok (grade, diff) ∧
      rep.grade grade = group.grade ∧ rep.duration diff = group.diff
  increment : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    ∃ next, call.head_increment ops groups index = ok next ∧
      rep.groups next = pre ++ { group with count := group.count + 1 } :: rest
  set_size : ∀ groups index pre group rest size,
    At (rep.groups groups) index pre group rest →
    ∃ next, call.head_set_size ops groups index size = ok next ∧
      rep.groups next = pre ++ { group with size := rep.id size } :: rest
  set_result : ∀ groups index pre group rest grade diff,
    At (rep.groups groups) index pre group rest →
    ∃ next, call.head_set_result ops groups index grade diff = ok next ∧
      rep.groups next = pre ++
        { group with grade := rep.grade grade, diff := rep.duration diff } :: rest
  append : ∀ groups id size grade diff,
    (rep.groups groups).length + 1 ≤ capacity →
    ∃ next, call.head_append ops groups id size grade diff = ok next ∧
      rep.groups next = rep.groups groups ++
        [{ groupId := rep.id id, count := 1, size := rep.id size,
           grade := rep.grade grade, diff := rep.duration diff }]

structure BodyRepresentation (B Id : Type) where
  groups : B → List TouchHoldBodyGroupState
  id : Id → Nat

structure BodyCapacity where
  groups : Nat
  members : Nat
  triggers : Nat

def BodyFits (cap : BodyCapacity) (groups : List TouchHoldBodyGroupState) : Prop :=
  groups.length ≤ cap.groups ∧ ∀ group ∈ groups,
    group.memberNoteIndices.length ≤ cap.members ∧
    group.triggeredNoteIndices.length ≤ cap.triggers

/-- Only indexed fields, contains, prepend, erase-first, remove, and append.
No field assumes search, registration, unregistration, exit, or majority sharing
for an entire group list. The majority law covers one group's exact Nat data. -/
structure BodyAtoms {B Id Ops : Type}
    (call : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
    (rep : BodyRepresentation B Id) (cap : BodyCapacity) : Prop where
  length : ∀ groups, ∃ len, call.body_length ops groups = ok len ∧
    (rep.groups groups).length = len.val
  «matches» : ∀ groups index pre group rest id,
    At (rep.groups groups) index pre group rest →
    call.body_matches ops groups index id = ok (group.groupId == rep.id id)
  majority : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    call.body_majority ops groups index = ok (hasStrictMajority
      group.triggeredNoteIndices.length group.memberNoteIndices.length)
  has_member : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    call.body_has_member ops groups index note = ok (group.memberNoteIndices.contains (rep.id note))
  has_trigger : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    call.body_has_trigger ops groups index note = ok (group.triggeredNoteIndices.contains (rep.id note))
  prepend_member : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    group.memberNoteIndices.length + 1 ≤ cap.members →
    ∃ next, call.body_prepend_member ops groups index note = ok next ∧
      rep.groups next = pre ++
        { group with memberNoteIndices := rep.id note :: group.memberNoteIndices } :: rest
  prepend_trigger : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    group.triggeredNoteIndices.length + 1 ≤ cap.triggers →
    ∃ next, call.body_prepend_trigger ops groups index note = ok next ∧
      rep.groups next = pre ++
        { group with triggeredNoteIndices := rep.id note :: group.triggeredNoteIndices } :: rest
  erase_member : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    ∃ next, call.body_erase_member ops groups index note = ok next ∧
      rep.groups next = pre ++
        { group with memberNoteIndices := group.memberNoteIndices.erase (rep.id note) } :: rest
  erase_trigger : ∀ groups index pre group rest note,
    At (rep.groups groups) index pre group rest →
    ∃ next, call.body_erase_trigger ops groups index note = ok next ∧
      rep.groups next = pre ++
        { group with triggeredNoteIndices := group.triggeredNoteIndices.erase (rep.id note) } :: rest
  members_empty : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    call.body_members_empty ops groups index = ok group.memberNoteIndices.isEmpty
  remove : ∀ groups index pre group rest,
    At (rep.groups groups) index pre group rest →
    ∃ next, call.body_remove ops groups index = ok next ∧ rep.groups next = pre ++ rest
  append : ∀ groups id note,
    (rep.groups groups).length + 1 ≤ cap.groups → 1 ≤ cap.members → 1 ≤ cap.triggers →
    ∃ next, call.body_append ops groups id note = ok next ∧
      rep.groups next = rep.groups groups ++
        [{ groupId := rep.id id, memberNoteIndices := [rep.id note],
           triggeredNoteIndices := [rep.id note] }]

/-- The real head wrapper performs the real length read before its scan. -/
theorem find_head_spec {G Id Grade D Ops : Type}
    (call : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : HeadRepresentation G Id Grade D) (capacity : Nat)
    (atoms : HeadAtoms call ops rep capacity) (groups : G) (id : Id) :
    touch_hold_schedule.find_head call groups id ops ⦃
      First GroupState.groupId (rep.groups groups) (rep.id id) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.length groups
  have hloop := scan_spec GroupState.groupId (rep.groups groups) (rep.id id) len
    (fun index => call.head_matches ops groups index id) hlen
    (fun index pre group rest hat => atoms.matches groups index pre group rest id hat)
  simpa only [touch_hold_schedule.find_head, hlength, bind_tc_ok,
    touch_hold_schedule.find_head_loop, touch_hold_schedule.find_head_loop.body,
    scanBody] using hloop

theorem find_body_spec {B Id Ops : Type}
    (call : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
    (rep : BodyRepresentation B Id) (cap : BodyCapacity)
    (atoms : BodyAtoms call ops rep cap) (groups : B) (id : Id) :
    touch_hold_schedule.find_body call groups id ops ⦃
      First TouchHoldBodyGroupState.groupId (rep.groups groups) (rep.id id) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.length groups
  have hloop := scan_spec TouchHoldBodyGroupState.groupId (rep.groups groups) (rep.id id) len
    (fun index => call.body_matches ops groups index id) hlen
    (fun index pre group rest hat => atoms.matches groups index pre group rest id hat)
  simpa only [touch_hold_schedule.find_body, hlength, bind_tc_ok,
    touch_hold_schedule.find_body_loop, touch_hold_schedule.find_body_loop.body,
    scanBody] using hloop

/-- Point update result in the original reference, before incrementing count. -/
def headAfter (group : GroupState) (size : Nat) (grade : JudgeGrade) (diff : Duration) :
    GroupState :=
  if hasStrictMajority group.count group.size then
    { group with count := group.count + 1, size := size }
  else
    { group with count := group.count + 1, size := size, grade := grade, diff := diff }

def bodyAfter (group : TouchHoldBodyGroupState) (note : Nat) : TouchHoldBodyGroupState :=
  { group with
    memberNoteIndices := if group.memberNoteIndices.contains note then group.memberNoteIndices
      else note :: group.memberNoteIndices
    triggeredNoteIndices := if group.triggeredNoteIndices.contains note then group.triggeredNoteIndices
      else note :: group.triggeredNoteIndices }

/-- Reference equations preserve every later duplicate unchanged. -/
theorem register_head_skip (pre rest : List GroupState) (id size : Nat)
    (grade : JudgeGrade) (diff : Duration) (hprefix : NoKey GroupState.groupId pre id) :
    registerTouchGroupResult (pre ++ rest) id size grade diff =
      pre ++ registerTouchGroupResult rest id size grade diff := by
  revert hprefix
  induction pre with
  | nil => intro _; rfl
  | cons group pre ih =>
      intro hprefix
      have hhead := hprefix group (by simp)
      have htail : NoKey GroupState.groupId pre id := fun x hx => hprefix x (by simp [hx])
      simpa [registerTouchGroupResult, registerTouchGroupResult.loop, hhead] using
        congrArg (List.cons group) (ih htail)

theorem register_head_at (pre : List GroupState) (group : GroupState) (rest : List GroupState)
    (id size : Nat) (grade : JudgeGrade) (diff : Duration)
    (hprefix : NoKey GroupState.groupId pre id) (hmatch : group.groupId = id) :
    registerTouchGroupResult (pre ++ group :: rest) id size grade diff =
      pre ++ headAfter group size grade diff :: rest := by
  rw [register_head_skip pre (group :: rest) id size grade diff hprefix]
  simp [registerTouchGroupResult, registerTouchGroupResult.loop, hmatch, headAfter]

theorem register_body_skip (pre rest : List TouchHoldBodyGroupState) (id note : Nat)
    (hprefix : NoKey TouchHoldBodyGroupState.groupId pre id) :
    registerTouchHoldBodyTrigger (pre ++ rest) id note =
      pre ++ registerTouchHoldBodyTrigger rest id note := by
  revert hprefix
  induction pre with
  | nil => intro _; rfl
  | cons group pre ih =>
      intro hprefix
      have hhead := hprefix group (by simp)
      have htail : NoKey TouchHoldBodyGroupState.groupId pre id :=
        fun x hx => hprefix x (by simp [hx])
      simpa [registerTouchHoldBodyTrigger, registerTouchHoldBodyTrigger.loop, hhead] using
        congrArg (List.cons group) (ih htail)

theorem register_body_at (pre : List TouchHoldBodyGroupState) (group : TouchHoldBodyGroupState)
    (rest : List TouchHoldBodyGroupState) (id note : Nat)
    (hprefix : NoKey TouchHoldBodyGroupState.groupId pre id) (hmatch : group.groupId = id) :
    registerTouchHoldBodyTrigger (pre ++ group :: rest) id note =
      pre ++ bodyAfter group note :: rest := by
  rw [register_body_skip pre (group :: rest) id note hprefix]
  simp [registerTouchHoldBodyTrigger, registerTouchHoldBodyTrigger.loop, hmatch, bodyAfter]

theorem unregister_body_skip (pre rest : List TouchHoldBodyGroupState) (id note : Nat)
    (hprefix : NoKey TouchHoldBodyGroupState.groupId pre id) :
    unregisterTouchHoldBodyTrigger (pre ++ rest) id note =
      pre ++ unregisterTouchHoldBodyTrigger rest id note := by
  revert hprefix
  induction pre with
  | nil => intro _; rfl
  | cons group pre ih =>
      intro hprefix
      have hhead := hprefix group (by simp)
      have htail : NoKey TouchHoldBodyGroupState.groupId pre id :=
        fun x hx => hprefix x (by simp [hx])
      simpa [unregisterTouchHoldBodyTrigger, unregisterTouchHoldBodyTrigger.loop, hhead] using
        congrArg (List.cons group) (ih htail)

theorem unregister_body_at (pre : List TouchHoldBodyGroupState) (group : TouchHoldBodyGroupState)
    (rest : List TouchHoldBodyGroupState) (id note : Nat)
    (hprefix : NoKey TouchHoldBodyGroupState.groupId pre id) (hmatch : group.groupId = id) :
    unregisterTouchHoldBodyTrigger (pre ++ group :: rest) id note =
      pre ++ { group with triggeredNoteIndices := group.triggeredNoteIndices.erase note } :: rest := by
  rw [unregister_body_skip pre (group :: rest) id note hprefix]
  simp [unregisterTouchHoldBodyTrigger, unregisterTouchHoldBodyTrigger.loop, hmatch]

theorem exit_body_skip (pre rest : List TouchHoldBodyGroupState) (id note : Nat)
    (hprefix : NoKey TouchHoldBodyGroupState.groupId pre id) :
    exitTouchHoldBodyGroupMember (pre ++ rest) id note =
      pre ++ exitTouchHoldBodyGroupMember rest id note := by
  revert hprefix
  induction pre with
  | nil => intro _; rfl
  | cons group pre ih =>
      intro hprefix
      have hhead := hprefix group (by simp)
      have htail : NoKey TouchHoldBodyGroupState.groupId pre id :=
        fun x hx => hprefix x (by simp [hx])
      simpa [exitTouchHoldBodyGroupMember, exitTouchHoldBodyGroupMember.loop, hhead] using
        congrArg (List.cons group) (ih htail)

theorem exit_body_at (pre : List TouchHoldBodyGroupState) (group : TouchHoldBodyGroupState)
    (rest : List TouchHoldBodyGroupState) (id note : Nat)
    (hprefix : NoKey TouchHoldBodyGroupState.groupId pre id) (hmatch : group.groupId = id) :
    exitTouchHoldBodyGroupMember (pre ++ group :: rest) id note =
      pre ++ (if (group.memberNoteIndices.erase note).isEmpty then rest
        else { group with memberNoteIndices := group.memberNoteIndices.erase note, triggeredNoteIndices := group.triggeredNoteIndices.erase note } :: rest) := by
  rw [exit_body_skip pre (group :: rest) id note hprefix]
  simp [exitTouchHoldBodyGroupMember, exitTouchHoldBodyGroupMember.loop, hmatch]

/-- The first matching head group alone decides sharing. Later duplicate IDs
cannot supply a result when that first group lacks a strict majority. -/
theorem shared_result_equiv {G Id Grade D Ops : Type}
    (call : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : HeadRepresentation G Id Grade D) (capacity : Nat)
    (atoms : HeadAtoms call ops rep capacity) (groups : G) (id : Id) :
    touch_hold_schedule.shared_result call groups id ops ⦃ fun out =>
      out.map (fun pair => (rep.grade pair.1, rep.duration pair.2)) =
        groupShareResult (rep.groups groups) (rep.id id) ⦄ := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_head_spec call ops rep capacity atoms groups id)
  cases found with
  | none =>
      have href := find_none GroupState.groupId (rep.groups groups) (rep.id id) hfirst
      apply exists_imp_spec
      refine ⟨none, ?_, ?_⟩
      · simp [touch_hold_schedule.shared_result, hfind, bind_tc_ok]
      · simp [groupShareResult, href]
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      have href := find_at GroupState.groupId (rep.groups groups) (rep.id id)
        index pre group rest hat hprefix hmatch
      have hmajority := atoms.majority groups index pre group rest hat
      cases hm : hasStrictMajority group.count group.size with
      | false =>
          apply exists_imp_spec
          refine ⟨none, ?_, ?_⟩
          · simp [touch_hold_schedule.shared_result, hfind, hmajority, hm, bind_tc_ok]
          · simp [groupShareResult, href, hm]
      | true =>
          obtain ⟨grade, diff, hresult, hgrade, hdiff⟩ :=
            atoms.result groups index pre group rest hat
          apply exists_imp_spec
          refine ⟨some (grade, diff), ?_, ?_⟩
          · simp [touch_hold_schedule.shared_result, hfind, hmajority, hm,
              hresult, bind_tc_ok]
          · simp [groupShareResult, href, hm, hgrade, hdiff]

/-- The stored-result decision precedes increment and size replacement, exactly
as in the reference. Only absence of the ID needs one new outer-list slot. -/
theorem register_head_equiv {G Id Grade D Ops : Type}
    (call : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : HeadRepresentation G Id Grade D) (capacity : Nat)
    (atoms : HeadAtoms call ops rep capacity)
    (groups : G) (id size : Id) (grade : Grade) (diff : D)
    (hcapacity : (registerTouchGroupResult (rep.groups groups) (rep.id id)
      (rep.id size) (rep.grade grade) (rep.duration diff)).length ≤ capacity) :
    touch_hold_schedule.register_head call groups id size grade diff ops ⦃ fun out =>
      rep.groups out = registerTouchGroupResult (rep.groups groups) (rep.id id)
        (rep.id size) (rep.grade grade) (rep.duration diff) ⦄ := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_head_spec call ops rep capacity atoms groups id)
  cases found with
  | none =>
      have href : registerTouchGroupResult (rep.groups groups) (rep.id id)
          (rep.id size) (rep.grade grade) (rep.duration diff) =
          rep.groups groups ++ [{ groupId := rep.id id, count := 1, size := rep.id size, grade := rep.grade grade, diff := rep.duration diff }] := by
        simpa [registerTouchGroupResult, registerTouchGroupResult.loop] using
          register_head_skip (rep.groups groups) [] (rep.id id) (rep.id size)
            (rep.grade grade) (rep.duration diff) hfirst
      have hslot : (rep.groups groups).length + 1 ≤ capacity := by
        simpa [href] using hcapacity
      obtain ⟨out, hout, hrep⟩ := atoms.append groups id size grade diff hslot
      apply exists_imp_spec
      refine ⟨out, ?_, ?_⟩
      · simp [touch_hold_schedule.register_head, hfind, hout, bind_tc_ok]
      · exact hrep.trans href.symm
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      have href : registerTouchGroupResult (rep.groups groups) (rep.id id)
          (rep.id size) (rep.grade grade) (rep.duration diff) =
          pre ++ headAfter group (rep.id size) (rep.grade grade) (rep.duration diff) :: rest := by
        rw [hat.1]
        exact register_head_at pre group rest (rep.id id) (rep.id size)
          (rep.grade grade) (rep.duration diff) hprefix hmatch
      have hmajority := atoms.majority groups index pre group rest hat
      obtain ⟨counted, hcounted, hcountedRep⟩ := atoms.increment groups index pre group rest hat
      have hcountedAt : At (rep.groups counted) index pre
          { group with count := group.count + 1 } rest := ⟨hcountedRep, hat.2⟩
      obtain ⟨sized, hsized, hsizedRep⟩ := atoms.set_size counted index pre
        { group with count := group.count + 1 } rest size hcountedAt
      have hsizedAt : At (rep.groups sized) index pre
          { group with count := group.count + 1, size := rep.id size } rest :=
        ⟨hsizedRep, hat.2⟩
      cases hm : hasStrictMajority group.count group.size with
      | false =>
          obtain ⟨out, hout, hrep⟩ := atoms.set_result sized index pre
            { group with count := group.count + 1, size := rep.id size } rest
            grade diff hsizedAt
          apply exists_imp_spec
          refine ⟨out, ?_, ?_⟩
          · simp [touch_hold_schedule.register_head, hfind, hmajority, hcounted,
              hsized, hm, hout, bind_tc_ok]
          · simpa [href, headAfter, hm] using hrep
      | true =>
          apply exists_imp_spec
          refine ⟨sized, ?_, ?_⟩
          · simp [touch_hold_schedule.register_head, hfind, hmajority, hcounted,
              hsized, hm, bind_tc_ok]
          · simpa [href, headAfter, hm] using hsizedRep

/-- Body pressure reads exact list lengths, including duplicate member/vote IDs. -/
theorem body_pressed_equiv {B Id Ops : Type}
    (call : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
    (rep : BodyRepresentation B Id) (cap : BodyCapacity)
    (atoms : BodyAtoms call ops rep cap) (groups : B) (id : Id) :
    touch_hold_schedule.body_pressed call groups id ops ⦃ fun out =>
      out = touchHoldBodyGroupMajorityPressed (rep.groups groups) (rep.id id) ⦄ := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_body_spec call ops rep cap atoms groups id)
  cases found with
  | none =>
      have href := find_none TouchHoldBodyGroupState.groupId
        (rep.groups groups) (rep.id id) hfirst
      apply exists_imp_spec
      refine ⟨false, ?_, ?_⟩
      · simp [touch_hold_schedule.body_pressed, hfind, bind_tc_ok]
      · simp [touchHoldBodyGroupMajorityPressed, href]
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      have href := find_at TouchHoldBodyGroupState.groupId (rep.groups groups) (rep.id id)
        index pre group rest hat hprefix hmatch
      have hmajority := atoms.majority groups index pre group rest hat
      apply exists_imp_spec
      refine ⟨hasStrictMajority group.triggeredNoteIndices.length group.memberNoteIndices.length,
        ?_, ?_⟩
      · simp [touch_hold_schedule.body_pressed, hfind, hmajority, bind_tc_ok]
      · simp [touchHoldBodyGroupMajorityPressed, href,
          touchHoldBodyGroupTriggeredCount, touchHoldBodyGroupMemberCount]

/-- Missing member and trigger IDs are prepended independently. The trigger
read occurs on the container returned by the member insertion. The final
reference bounds suffice for both intermediate allocations. -/
theorem register_body_equiv {B Id Ops : Type}
    (call : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
    (rep : BodyRepresentation B Id) (cap : BodyCapacity)
    (atoms : BodyAtoms call ops rep cap) (groups : B) (id note : Id)
    (hcapacity : BodyFits cap (registerTouchHoldBodyTrigger
      (rep.groups groups) (rep.id id) (rep.id note))) :
    touch_hold_schedule.register_body call groups id note ops ⦃ fun out =>
      rep.groups out = registerTouchHoldBodyTrigger
        (rep.groups groups) (rep.id id) (rep.id note) ⦄ := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_body_spec call ops rep cap atoms groups id)
  cases found with
  | none =>
      let fresh : TouchHoldBodyGroupState :=
        { groupId := rep.id id, memberNoteIndices := [rep.id note],
          triggeredNoteIndices := [rep.id note] }
      have href : registerTouchHoldBodyTrigger (rep.groups groups) (rep.id id) (rep.id note) =
          rep.groups groups ++ [fresh] := by
        simpa [fresh, registerTouchHoldBodyTrigger, registerTouchHoldBodyTrigger.loop] using
          register_body_skip (rep.groups groups) [] (rep.id id) (rep.id note) hfirst
      have hfits : BodyFits cap (rep.groups groups ++ [fresh]) := by
        simpa only [href] using hcapacity
      have hslot : (rep.groups groups).length + 1 ≤ cap.groups := by
        simpa using hfits.1
      have hlocal := hfits.2 fresh (by simp)
      have hmembers : 1 ≤ cap.members := by simpa [fresh] using hlocal.1
      have htriggers : 1 ≤ cap.triggers := by simpa [fresh] using hlocal.2
      obtain ⟨out, hout, hrep⟩ := atoms.append groups id note hslot hmembers htriggers
      apply exists_imp_spec
      refine ⟨out, ?_, ?_⟩
      · simp [touch_hold_schedule.register_body, hfind, hout, bind_tc_ok]
      · exact hrep.trans href.symm
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      have href : registerTouchHoldBodyTrigger (rep.groups groups) (rep.id id) (rep.id note) =
          pre ++ bodyAfter group (rep.id note) :: rest := by
        rw [hat.1]
        exact register_body_at pre group rest (rep.id id) (rep.id note) hprefix hmatch
      have hfits : BodyFits cap (pre ++ bodyAfter group (rep.id note) :: rest) := by
        simpa only [href] using hcapacity
      have hlocal := hfits.2 (bodyAfter group (rep.id note)) (by simp)
      let memberGroup : TouchHoldBodyGroupState :=
        { group with memberNoteIndices :=
            if group.memberNoteIndices.contains (rep.id note)
            then group.memberNoteIndices else rep.id note :: group.memberNoteIndices }
      have hmember := atoms.has_member groups index pre group rest note hat
      have hmiddle : ∃ mid,
          (if group.memberNoteIndices.contains (rep.id note) then ok groups
            else call.body_prepend_member ops groups index note) = ok mid ∧
          rep.groups mid = pre ++ memberGroup :: rest := by
        cases hm : group.memberNoteIndices.contains (rep.id note) with
        | false =>
            have hspace : group.memberNoteIndices.length + 1 ≤ cap.members := by
              simpa only [bodyAfter, hm, Bool.false_eq_true, ↓reduceIte, List.length_cons] using hlocal.1
            obtain ⟨mid, hmid, hrep⟩ :=
              atoms.prepend_member groups index pre group rest note hat hspace
            refine ⟨mid, ?_, ?_⟩
            · simpa [hm] using hmid
            · simpa only [memberGroup, hm, Bool.false_eq_true, ↓reduceIte] using hrep
        | true =>
            refine ⟨groups, ?_, ?_⟩
            · simp [hm]
            · simpa only [memberGroup, hm, ↓reduceIte] using hat.1
      obtain ⟨mid, hmid, hmidRep⟩ := hmiddle
      have hmidAt : At (rep.groups mid) index pre memberGroup rest := ⟨hmidRep, hat.2⟩
      have htrigger : call.body_has_trigger ops mid index note =
          ok (group.triggeredNoteIndices.contains (rep.id note)) := by
        simpa only [memberGroup] using
          atoms.has_trigger mid index pre memberGroup rest note hmidAt
      have hfinish : ∃ out,
          (if group.triggeredNoteIndices.contains (rep.id note) then ok mid
            else call.body_prepend_trigger ops mid index note) = ok out ∧
          rep.groups out = pre ++ bodyAfter group (rep.id note) :: rest := by
        cases ht : group.triggeredNoteIndices.contains (rep.id note) with
        | false =>
            have hspace : memberGroup.triggeredNoteIndices.length + 1 ≤ cap.triggers := by
              simpa only [memberGroup, bodyAfter, ht, Bool.false_eq_true, ↓reduceIte, List.length_cons] using hlocal.2
            obtain ⟨out, hout, hrep⟩ :=
              atoms.prepend_trigger mid index pre memberGroup rest note hmidAt hspace
            refine ⟨out, ?_, ?_⟩
            · simpa [ht] using hout
            · simpa only [memberGroup, bodyAfter, ht, Bool.false_eq_true, ↓reduceIte] using hrep
        | true =>
            refine ⟨mid, ?_, ?_⟩
            · simp [ht]
            · simpa only [memberGroup, bodyAfter, ht, ↓reduceIte] using hmidRep
      obtain ⟨out, hout, hrep⟩ := hfinish
      apply exists_imp_spec
      refine ⟨out, ?_, ?_⟩
      · simp only [touch_hold_schedule.register_body, hfind, bind_tc_ok,
          hmember, hmid, htrigger, hout]
      · exact hrep.trans href.symm

/-- Unregister removes the first occurrence of the vote from the first matching
group. It neither deletes membership nor normalizes duplicate IDs. -/
theorem unregister_body_equiv {B Id Ops : Type}
    (call : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
    (rep : BodyRepresentation B Id) (cap : BodyCapacity)
    (atoms : BodyAtoms call ops rep cap) (groups : B) (id note : Id) :
    touch_hold_schedule.unregister_body call groups id note ops ⦃ fun out =>
      rep.groups out = unregisterTouchHoldBodyTrigger
        (rep.groups groups) (rep.id id) (rep.id note) ⦄ := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_body_spec call ops rep cap atoms groups id)
  cases found with
  | none =>
      have href : unregisterTouchHoldBodyTrigger
          (rep.groups groups) (rep.id id) (rep.id note) = rep.groups groups := by
        simpa [unregisterTouchHoldBodyTrigger, unregisterTouchHoldBodyTrigger.loop] using
          unregister_body_skip (rep.groups groups) [] (rep.id id) (rep.id note) hfirst
      apply exists_imp_spec
      refine ⟨groups, ?_, href.symm⟩
      simp [touch_hold_schedule.unregister_body, hfind, bind_tc_ok]
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      have href : unregisterTouchHoldBodyTrigger (rep.groups groups) (rep.id id) (rep.id note) =
          pre ++ { group with triggeredNoteIndices := group.triggeredNoteIndices.erase (rep.id note) } :: rest := by
        rw [hat.1]
        exact unregister_body_at pre group rest (rep.id id) (rep.id note) hprefix hmatch
      obtain ⟨out, hout, hrep⟩ := atoms.erase_trigger groups index pre group rest note hat
      apply exists_imp_spec
      refine ⟨out, ?_, hrep.trans href.symm⟩
      simp [touch_hold_schedule.unregister_body, hfind, hout, bind_tc_ok]

/-- Exit erases one member first, then either deletes that indexed empty group
or erases one trigger. Even an already-empty matching group is deleted; no
membership, uniqueness, or nonempty-group invariant is silently imposed. -/
theorem exit_body_equiv {B Id Ops : Type}
    (call : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
    (rep : BodyRepresentation B Id) (cap : BodyCapacity)
    (atoms : BodyAtoms call ops rep cap) (groups : B) (id note : Id) :
    touch_hold_schedule.exit_body call groups id note ops ⦃ fun out =>
      rep.groups out = exitTouchHoldBodyGroupMember
        (rep.groups groups) (rep.id id) (rep.id note) ⦄ := by
  obtain ⟨found, hfind, hfirst⟩ := spec_imp_exists
    (find_body_spec call ops rep cap atoms groups id)
  cases found with
  | none =>
      have href : exitTouchHoldBodyGroupMember
          (rep.groups groups) (rep.id id) (rep.id note) = rep.groups groups := by
        simpa [exitTouchHoldBodyGroupMember, exitTouchHoldBodyGroupMember.loop] using
          exit_body_skip (rep.groups groups) [] (rep.id id) (rep.id note) hfirst
      apply exists_imp_spec
      refine ⟨groups, ?_, href.symm⟩
      simp [touch_hold_schedule.exit_body, hfind, bind_tc_ok]
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      let erased : TouchHoldBodyGroupState :=
        { group with memberNoteIndices := group.memberNoteIndices.erase (rep.id note) }
      have href : exitTouchHoldBodyGroupMember (rep.groups groups) (rep.id id) (rep.id note) =
          pre ++ (if erased.memberNoteIndices.isEmpty then rest
            else { erased with triggeredNoteIndices :=
              group.triggeredNoteIndices.erase (rep.id note) } :: rest) := by
        rw [hat.1]
        exact exit_body_at pre group rest (rep.id id) (rep.id note) hprefix hmatch
      obtain ⟨mid, hmid, hmidRep⟩ := atoms.erase_member groups index pre group rest note hat
      have hmidAt : At (rep.groups mid) index pre erased rest := ⟨hmidRep, hat.2⟩
      have hempty := atoms.members_empty mid index pre erased rest hmidAt
      cases he : erased.memberNoteIndices.isEmpty with
      | false =>
          obtain ⟨out, hout, hrep⟩ := atoms.erase_trigger mid index pre erased rest note hmidAt
          apply exists_imp_spec
          refine ⟨out, ?_, ?_⟩
          · simp [touch_hold_schedule.exit_body, hfind, hmid, hempty, he, hout, bind_tc_ok]
          · simpa [href, he, erased] using hrep
      | true =>
          obtain ⟨out, hout, hrep⟩ := atoms.remove mid index pre erased rest hmidAt
          apply exists_imp_spec
          refine ⟨out, ?_, ?_⟩
          · simp [touch_hold_schedule.exit_body, hfind, hmid, hempty, he, hout, bind_tc_ok]
          · simpa [href, he] using hrep

end Verification.TouchHoldGroupsProof
