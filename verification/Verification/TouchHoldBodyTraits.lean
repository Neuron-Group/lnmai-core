import Verification.TouchHoldBodyReads

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldBodyTraitsProof

open TouchHoldBodyReadsProof

variable {Id : Type}

/-! The real generated Default and Clone implementations. Identifier operations
are point hypotheses; neither a whole record copy nor a vector copy is assumed.
The vector theorem uses the installed Aeneas standard-library semantics. -/

theorem default_eq (defaultInst : core.default.Default Id) (value : Id)
    (hdefault : defaultInst.default = ok value) :
    touch_hold_body.TouchHoldBodyGroupState.Insts.CoreDefaultDefault.default
      defaultInst = ok
        { group_id := value
          member_note_indices := alloc.vec.Vec.new Id
          triggered_note_indices := alloc.vec.Vec.new Id } := by
  simp only [touch_hold_body.TouchHoldBodyGroupState.Insts.CoreDefaultDefault.default,
    hdefault, bind_tc_ok]

theorem default_view (defaultInst : core.default.Default Id) (decode : Id → Nat)
    (value : Id) (hdefault : defaultInst.default = ok value) (hzero : decode value = 0) :
    touch_hold_body.TouchHoldBodyGroupState.Insts.CoreDefaultDefault.default
      defaultInst ⦃ out => viewGroup decode out =
        (default : LnmaiCore.TouchHoldBodyGroupState) ⦄ := by
  rw [default_eq defaultInst value hdefault]
  simp [viewGroup, alloc.vec.Vec.new, hzero]

theorem vec_clone_eq (cloneInst : core.clone.Clone Id) (values : alloc.vec.Vec Id)
    (hclone : ∀ value ∈ values.val, cloneInst.clone value = ok value) :
    alloc.vec.CloneVec.clone cloneInst values = ok values := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists (Slice.clone_spec hclone)
  simpa only [alloc.vec.CloneVec.clone, ← hout] using hrun

theorem clone_eq (cloneInst : core.clone.Clone Id) (group : Group Id)
    (hid : cloneInst.clone group.group_id = ok group.group_id)
    (hmembers : ∀ value ∈ group.member_note_indices.val, cloneInst.clone value = ok value)
    (htriggers : ∀ value ∈ group.triggered_note_indices.val, cloneInst.clone value = ok value) :
    touch_hold_body.TouchHoldBodyGroupState.Insts.CoreCloneClone.clone
      cloneInst group = ok group := by
  simp only [touch_hold_body.TouchHoldBodyGroupState.Insts.CoreCloneClone.clone,
    hid, vec_clone_eq cloneInst group.member_note_indices hmembers,
    vec_clone_eq cloneInst group.triggered_note_indices htriggers, bind_tc_ok]

theorem groups_clone_eq (cloneInst : core.clone.Clone Id) (groups : Groups Id)
    (hclone : ∀ value, cloneInst.clone value = ok value) :
    alloc.vec.CloneVec.clone
      (touch_hold_body.TouchHoldBodyGroupState.Insts.CoreCloneClone cloneInst) groups =
      ok groups := by
  apply vec_clone_eq
  intro group _
  exact clone_eq cloneInst group (hclone group.group_id)
    (fun value _ => hclone value) (fun value _ => hclone value)

end Verification.TouchHoldBodyTraitsProof
