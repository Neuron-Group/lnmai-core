import Verification.VecPrependExtracted
import Verification.VecEraseExtracted

/-!
Actual mutation proofs for the extraction5 touch_hold_body record/functions.
Canonical Generated SHA256:
61f040d6f4e25a3d202a3737085eef0fd13533994296e7570d6066ed7188c634.

All results are equalities of the actual generated record and Aeneas Vec.
The helpers below only construct values of those same types. There is no
replacement record, decoder, whole-operation premise, or assumed writeback.
The concrete index_mut back-function is proved to be Vec.set. Existing actual
prepend/erase and Slice.swap proofs supply the list effects.

Clone laws concern only the input id/note at each explicit clone call. Erasure
accepts any Clone dictionary, requires only successful primitive Id::default,
and never clones the filler. No operation here needs an Eq law or distinct IDs.
Growth requires length < Usize.max for the vector actually being grown; indexed
removal and swap only need valid indices. Native allocator/byte capacity, Drop,
Rust Natural primitive instances, and the remaining runtime wiring are outside
these extracted value-semantics results. Reads/decoding and scans are separate.

Source-only delivery: no compiler, extraction, or tests were run by this worker.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyFieldsProof

abbrev Group (Id : Type) := touch_hold_body.TouchHoldBodyGroupState Id
abbrev Groups (Id : Type) := alloc.vec.Vec (Group Id)

variable {Id α : Type}

/-- Actual Vec values used on the right-hand sides of the equalities. -/
def prependVec (values : alloc.vec.Vec α) (head : α)
    (hroom : values.val.length < Usize.max) : alloc.vec.Vec α :=
  ⟨head :: values.val, by
    simpa only [List.length_cons] using (Nat.succ_le_of_lt hroom)⟩

def eraseVec (values : alloc.vec.Vec α) (index : Usize)
    (hvalid : index.val < values.val.length) : alloc.vec.Vec α :=
  ⟨values.val.eraseIdx index.val, by
    rw [List.length_eraseIdx_of_lt hvalid]
    exact Nat.le_trans (Nat.sub_le _ _) values.property⟩

def snocVec (values : alloc.vec.Vec α) (value : α)
    (hroom : values.val.length < Usize.max) : alloc.vec.Vec α :=
  ⟨values.val ++ [value], by
    simpa only [List.length_append, List.length_singleton] using
      (Nat.succ_le_of_lt hroom)⟩

def singletonVec (value : α) : alloc.vec.Vec α :=
  ⟨[value], by simp only [List.length_singleton]; scalar_tac⟩

@[simp] theorem prependVec_val (values : alloc.vec.Vec α) (head : α)
    (hroom : values.val.length < Usize.max) :
    (prependVec values head hroom).val = head :: values.val := rfl

@[simp] theorem eraseVec_val (values : alloc.vec.Vec α) (index : Usize)
    (hvalid : index.val < values.val.length) :
    (eraseVec values index hvalid).val = values.val.eraseIdx index.val := rfl

@[simp] theorem snocVec_val (values : alloc.vec.Vec α) (value : α)
    (hroom : values.val.length < Usize.max) :
    (snocVec values value hroom).val = values.val ++ [value] := rfl

@[simp] theorem singletonVec_val (value : α) :
    (singletonVec value).val = [value] := rfl

/-- Constructors of the generated record, not alternative representations. -/
def emptyGroup (id : Id) : Group Id := {
  group_id := id
  member_note_indices := alloc.vec.Vec.new Id
  triggered_note_indices := alloc.vec.Vec.new Id
}

def singletonGroup (id note : Id) : Group Id := {
  group_id := id
  member_note_indices := singletonVec note
  triggered_note_indices := singletonVec note
}

/-- Exact concrete mutable borrow, including its actual writeback function. -/
private theorem index_mut_eq (values : alloc.vec.Vec α) (index : Usize)
    (hvalid : index.val < values.val.length) :
    alloc.vec.Vec.index_mut (core.slice.index.SliceIndexUsizeSlice α)
      values index = ok (values.val[index.val]'hvalid, values.set index) := by
  rw [alloc.vec.Vec.index_mut_slice_index]
  obtain ⟨⟨value, back⟩, hrun, hvalue, hback⟩ := spec_imp_exists
    (alloc.vec.Vec.index_mut_usize_spec values index hvalid)
  simpa only [hvalue, hback] using hrun

private theorem prepend_vec_eq (values : alloc.vec.Vec α) (head : α)
    (hroom : values.val.length < Usize.max) :
    vec_prepend.prepend values head = ok (prependVec values head hroom) := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (VecPrependExtractedProof.prepend_spec values head hroom)
  have heq : out = prependVec values head hroom := by
    apply Subtype.ext
    exact hout
  simpa only [heq] using hrun

private theorem erase_vec_eq (cloneInst : core.clone.Clone α)
    (values : alloc.vec.Vec α) (index : Usize) (filler : α)
    (hvalid : index.val < values.val.length) :
    vec_erase.erase cloneInst values index filler =
      ok (eraseVec values index hvalid) := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (VecEraseExtractedProof.erase_spec cloneInst values index filler hvalid)
  have heq : out = eraseVec values index hvalid := by
    apply Subtype.ext
    exact hout
  simpa only [heq] using hrun

private theorem push_vec_eq (values : alloc.vec.Vec α) (value : α)
    (hroom : values.val.length < Usize.max) :
    alloc.vec.Vec.push values value = ok (snocVec values value hroom) := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (alloc.vec.Vec.push_spec values value hroom)
  have heq : out = snocVec values value hroom := by
    apply Subtype.ext
    exact hout
  simpa only [heq] using hrun

/-- Only the selected member Vec changes. The original group id, trigger Vec,
all other groups, and duplicate values are retained literally. -/
theorem prepend_member_eq (cloneInst : core.clone.Clone Id)
    (groups : Groups Id) (group : Usize) (note : Id)
    (hgroup : group.val < groups.val.length)
    (hroom : (groups.val[group.val]'hgroup).member_note_indices.val.length < Usize.max)
    (hclone : cloneInst.clone note = ok note) :
    touch_hold_body.prepend_member cloneInst groups group note =
      ok (groups.set group {
        (groups.val[group.val]'hgroup) with
        member_note_indices := prependVec
          (groups.val[group.val]'hgroup).member_note_indices note hroom }) := by
  have hindex := index_mut_eq groups group hgroup
  have hprepend := prepend_vec_eq
    (groups.val[group.val]'hgroup).member_note_indices note hroom
  simp only [touch_hold_body.prepend_member, hindex, bind_tc_ok,
    uncurry_apply_pair, hclone, hprepend]

/-- Symmetric trigger-only mutation; the member Vec is retained literally. -/
theorem prepend_trigger_eq (cloneInst : core.clone.Clone Id)
    (groups : Groups Id) (group : Usize) (note : Id)
    (hgroup : group.val < groups.val.length)
    (hroom : (groups.val[group.val]'hgroup).triggered_note_indices.val.length < Usize.max)
    (hclone : cloneInst.clone note = ok note) :
    touch_hold_body.prepend_trigger cloneInst groups group note =
      ok (groups.set group {
        (groups.val[group.val]'hgroup) with
        triggered_note_indices := prependVec
          (groups.val[group.val]'hgroup).triggered_note_indices note hroom }) := by
  have hindex := index_mut_eq groups group hgroup
  have hprepend := prepend_vec_eq
    (groups.val[group.val]'hgroup).triggered_note_indices note hroom
  simp only [touch_hold_body.prepend_trigger, hindex, bind_tc_ok,
    uncurry_apply_pair, hclone, hprepend]

/-- Ordered indexed deletion, with arbitrary filler and Clone dictionary.
Default succeeds at its one primitive call; no group-level default law is used. -/
theorem remove_member_eq (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (groups : Groups Id)
    (group index : Usize) (filler : Id)
    (hgroup : group.val < groups.val.length)
    (hindex : index.val <
      (groups.val[group.val]'hgroup).member_note_indices.val.length)
    (hdefault : defaultInst.default = ok filler) :
    touch_hold_body.remove_member cloneInst defaultInst groups group index =
      ok (groups.set group {
        (groups.val[group.val]'hgroup) with
        member_note_indices := eraseVec
          (groups.val[group.val]'hgroup).member_note_indices index hindex }) := by
  have hborrow := index_mut_eq groups group hgroup
  have herase := erase_vec_eq cloneInst
    (groups.val[group.val]'hgroup).member_note_indices index filler hindex
  simp only [touch_hold_body.remove_member, hborrow, bind_tc_ok,
    uncurry_apply_pair, hdefault, herase]

theorem remove_trigger_eq (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (groups : Groups Id)
    (group index : Usize) (filler : Id)
    (hgroup : group.val < groups.val.length)
    (hindex : index.val <
      (groups.val[group.val]'hgroup).triggered_note_indices.val.length)
    (hdefault : defaultInst.default = ok filler) :
    touch_hold_body.remove_trigger cloneInst defaultInst groups group index =
      ok (groups.set group {
        (groups.val[group.val]'hgroup) with
        triggered_note_indices := eraseVec
          (groups.val[group.val]'hgroup).triggered_note_indices index hindex }) := by
  have hborrow := index_mut_eq groups group hgroup
  have herase := erase_vec_eq cloneInst
    (groups.val[group.val]'hgroup).triggered_note_indices index filler hindex
  simp only [touch_hold_body.remove_trigger, hborrow, bind_tc_ok,
    uncurry_apply_pair, hdefault, herase]

/-- Exact append of an actual record with two empty Vec fields. -/
theorem append_empty_eq (cloneInst : core.clone.Clone Id)
    (groups : Groups Id) (id : Id)
    (hroom : groups.val.length < Usize.max)
    (hcloneId : cloneInst.clone id = ok id) :
    touch_hold_body.append_empty cloneInst groups id =
      ok (snocVec groups (emptyGroup id) hroom) := by
  simp only [touch_hold_body.append_empty, hcloneId, bind_tc_ok]
  exact push_vec_eq groups (emptyGroup id) hroom

/-- The actual singleton push has no caller capacity premise: its input is
empty and 1 fits Usize. with_capacity itself is the installed Std interpretation. -/
private theorem push_singleton_eq (value : α) :
    alloc.vec.Vec.push (alloc.vec.Vec.with_capacity α 1#usize) value =
      ok (singletonVec value) := by
  have hroom : (alloc.vec.Vec.with_capacity α 1#usize).val.length < Usize.max := by
    change 0 < Usize.max
    scalar_tac
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (alloc.vec.Vec.push_spec (alloc.vec.Vec.with_capacity α 1#usize) value hroom)
  have heq : out = singletonVec value := by
    apply Subtype.ext
    simpa only [alloc.vec.Vec.with_capacity, alloc.vec.Vec.new, List.nil_append]
      using hout
  simpa only [heq] using hrun

/-- Exact singleton member/trigger fields. Generated reuses the note clone
result for both pushes; this is a value theorem, not a Clone side-effect trace. -/
theorem append_eq (cloneInst : core.clone.Clone Id)
    (groups : Groups Id) (id note : Id)
    (hroom : groups.val.length < Usize.max)
    (hcloneId : cloneInst.clone id = ok id)
    (hcloneNote : cloneInst.clone note = ok note) :
    touch_hold_body.append cloneInst groups id note =
      ok (snocVec groups (singletonGroup id note) hroom) := by
  have hone := push_singleton_eq note
  simp only [touch_hold_body.append, hcloneId, hcloneNote, bind_tc_ok, hone]
  exact push_vec_eq groups (singletonGroup id note) hroom

/-- Extraction5 constructs both empty Vec fields explicitly. Its only primitive
premise is Id::default; no opaque Vec Default or whole-record Default is assumed. -/
theorem group_default_eq (defaultInst : core.default.Default Id) (filler : Id)
    (hdefault : defaultInst.default = ok filler) :
    touch_hold_body.TouchHoldBodyGroupState.Insts.CoreDefaultDefault.default
      defaultInst = ok (emptyGroup filler) := by
  simp only [touch_hold_body.TouchHoldBodyGroupState.Insts.CoreDefaultDefault.default,
    hdefault, bind_tc_ok, emptyGroup]

/-- Exact ordered deletion of one whole group. The derived record Clone
contains Vec clones, but the shrink branch does not invoke any of them. -/
theorem remove_group_eq (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (groups : Groups Id)
    (index : Usize) (filler : Id)
    (hindex : index.val < groups.val.length)
    (hdefault : defaultInst.default = ok filler) :
    touch_hold_body.remove_group cloneInst defaultInst groups index =
      ok (eraseVec groups index hindex) := by
  have hfill := group_default_eq defaultInst filler hdefault
  have herase := erase_vec_eq
    (touch_hold_body.TouchHoldBodyGroupState.Insts.CoreCloneClone cloneInst)
    groups index (emptyGroup filler) hindex
  simp only [touch_hold_body.remove_group, hfill, bind_tc_ok, herase]

/-- The existing two-slot law uniquely determines the full list, including
when the indices are equal or the input contains repeated records/identifiers. -/
private theorem swap_points_eq (before after : List α) (left right : Nat)
    (hleft : left < before.length) (hright : right < before.length)
    (hpoints : TouchHoldRebuildReverseProof.SwapPoints before after left right) :
    after = TouchHoldRebuildReverseProof.swapAt before left right := by
  have htarget := TouchHoldRebuildReverseProof.swapAt_points before left right hleft hright
  apply List.ext_getElem?
  intro index
  by_cases hl : index = left
  · subst index
    exact hpoints.left_eq.trans htarget.left_eq.symm
  · by_cases hr : index = right
    · subst index
      exact hpoints.right_eq.trans htarget.right_eq.symm
    · exact (hpoints.other_eq index hl hr).trans (htarget.other_eq index hl hr).symm

/-- Actual deref_mut/back plus the already proved concrete Slice.swap. Two
original reads and two Vec.set writes give a literal Vec equality. -/
theorem swap_groups_eq (groups : Groups Id) (left right : Usize)
    (hleft : left.val < groups.val.length) (hright : right.val < groups.val.length) :
    touch_hold_body.swap_groups groups left right =
      ok ((groups.set left (groups.val[right.val]'hright)).set right
        (groups.val[left.val]'hleft)) := by
  let source : Slice (Group Id) := ⟨groups.val, groups.property⟩
  obtain ⟨out, hswap, hpoints⟩ := spec_imp_exists
    (TouchHoldRebuildReverseExtractedProof.slice_swap_points source left right hleft hright)
  have hswapConcrete :
      core.slice.Slice.swap (⟨groups.val, groups.property⟩ : Slice (Group Id)) left right =
        ok out := hswap
  have hlist : out.val = TouchHoldRebuildReverseProof.swapAt groups.val left.val right.val :=
    swap_points_eq groups.val out.val left.val right.val hleft hright hpoints
  have hout : (⟨out.val, out.property⟩ : Groups Id) =
      (groups.set left (groups.val[right.val]'hright)).set right
        (groups.val[left.val]'hleft) := by
    apply Subtype.ext
    change out.val =
      (groups.val.set left.val (groups.val[right.val]'hright)).set right.val
        (groups.val[left.val]'hleft)
    exact hlist.trans
      (TouchHoldRebuildReverseProof.swapAt_eq_set groups.val left.val right.val hleft hright)
  simp only [touch_hold_body.swap_groups, alloc.vec.Vec.deref_mut, lift,
    bind_tc_ok, uncurry_apply_pair, hswapConcrete, hout]

/-- Ready-to-use reverse-kernel atom; its two-slot law is a conclusion. -/
theorem swap_groups_points (groups : Groups Id) (left right : Usize)
    (hleft : left.val < groups.val.length) (hright : right.val < groups.val.length) :
    ∃ out, touch_hold_body.swap_groups groups left right = ok out ∧
      TouchHoldRebuildReverseProof.SwapPoints groups.val out.val left.val right.val := by
  refine ⟨(groups.set left (groups.val[right.val]'hright)).set right
      (groups.val[left.val]'hleft), swap_groups_eq groups left right hleft hright, ?_⟩
  have hvalue :
      ((groups.set left (groups.val[right.val]'hright)).set right
        (groups.val[left.val]'hleft)).val =
      TouchHoldRebuildReverseProof.swapAt groups.val left.val right.val := by
    exact (TouchHoldRebuildReverseProof.swapAt_eq_set
      groups.val left.val right.val hleft hright).symm
  rw [hvalue]
  exact TouchHoldRebuildReverseProof.swapAt_points groups.val left.val right.val hleft hright

end Verification.TouchHoldBodyFieldsProof
