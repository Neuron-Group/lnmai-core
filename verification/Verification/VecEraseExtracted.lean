import Verification.VecEraseModel
import Verification.TouchHoldRebuildReverseExtracted

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.VecEraseExtractedProof

open VecEraseModel
open TouchHoldRebuildReverseProof (SwapPoints)
open TouchHoldRebuildReverseExtractedProof (slice_swap_points)

/-!
Actual vec_erase declarations from
/tmp/lnmai-prefix-prepend-erase-extraction-2/LnmaiCoreVerify.lean:17273-17317,
SHA256 7eb36feb317d79341d5db9b72625799066cef531b1ff43f3ec0f3d4670334dd3.

The real body uses checked addition, Vec.deref_mut, Slice.swap, and its concrete
back-function. The real loop terminates by last - cursor. The entry assertion
makes length - 1 safe; the loop preserves length, hence resize strictly shrinks.
The existing Std shrink branch never calls Clone, regardless of its dictionary.
No abstract swap, whole-loop, whole-erase, Clone-success, equality, or distinct-ID
premise is used. Vec's own representation supplies the Usize capacity bound.

The result is generic in the element type, including complete group records.
This proves the extracted value semantics against the installed Aeneas Std
interpretation. It does not prove native Vec/backend/allocator refinement,
observable generic Drop behavior, or the unextracted runtime field adapters.
The caller's first-match scan and field writeback remain separate compositions.

Source-only draft: no compiler, extraction, or tests were run by this author.
-/

variable {T : Type}

/-- The false guard does not increment, dereference, or swap. -/
theorem erase_body_done (values : alloc.vec.Vec T) (index last : Usize)
    (hstop : ¬index < last) :
    vec_erase.erase_loop.body last values index = ok (.done values) := by
  simp only [vec_erase.erase_loop.body, hstop, ↓reduceIte]

/-- One actual adjacent swap. Its two-slot law is proved by the existing
concrete Slice.swap theorem, not required as an adapter hypothesis. -/
theorem erase_body_step (original : List T) (erased : Nat)
    (values : alloc.vec.Vec T) (index last : Usize)
    (hlast : last.val = original.length - 1)
    (hinv : Invariant original erased index.val values.val)
    (hmore : index < last) :
    ∃ out next,
      vec_erase.erase_loop.body last values index = ok (.cont (out, next)) ∧
      Invariant original erased next.val out.val ∧ next.val = index.val + 1 := by
  have hguard : index.val < original.length - 1 := by
    simpa only [UScalar.lt_equiv, hlast] using hmore
  have hcapacity : original.length ≤ UScalar.max .Usize := by
    simpa only [UScalar.max_USize_eq, hinv.length_eq] using values.property
  obtain ⟨hindexBound, hnextBound, hnextFits⟩ :=
    body_scalar_bounds original.length index.val (UScalar.max .Usize) hguard hcapacity
  obtain ⟨next, hnext, hnextValue⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) (by simpa using hnextFits))
  have hnextNat : next.val = index.val + 1 := by simpa using hnextValue
  have hindexSlot : index.val < values.val.length := by
    simpa only [hinv.length_eq] using hindexBound
  have hnextSlot : next.val < values.val.length := by
    simpa only [hinv.length_eq, hnextNat] using hnextBound
  let source : Slice T := ⟨values.val, values.property⟩
  obtain ⟨swapped, hswap, hpoints⟩ := spec_imp_exists
    (slice_swap_points source index next hindexSlot hnextSlot)
  have hswapConcrete :
      core.slice.Slice.swap (⟨values.val, values.property⟩ : Slice T) index next =
        ok swapped := hswap
  let out : alloc.vec.Vec T := ⟨swapped.val, swapped.property⟩
  have hpointsNext : SwapPoints values.val out.val index.val (index.val + 1) := by
    simpa only [source, out, hnextNat] using hpoints
  refine ⟨out, next, ?_, ?_, hnextNat⟩
  · simp only [vec_erase.erase_loop.body, hmore, ↓reduceIte, hnext,
      alloc.vec.Vec.deref_mut, lift, bind_tc_ok, uncurry_apply_pair,
      hswapConcrete, out]
  · rw [hnextNat]
    exact invariant_step_of_points hinv hguard hpointsNext

/-- Finite execution of the real loop from any state satisfying the point
invariant. The cursor in the postcondition is the actual terminal last slot. -/
theorem erase_loop_spec (original : List T) (erased : Nat)
    (values : alloc.vec.Vec T) (index last : Usize)
    (hlast : last.val = original.length - 1)
    (hinv : Invariant original erased index.val values.val) :
    vec_erase.erase_loop values index last ⦃ out =>
      Invariant original erased (original.length - 1) out.val ⦄ := by
  unfold vec_erase.erase_loop
  apply loop.spec_decr_nat
    (fun state : alloc.vec.Vec T × Usize => last.val - state.2.val)
    (fun state => Invariant original erased state.2.val state.1.val)
  · rintro ⟨current, cursor⟩ hinvNow
    dsimp only at hinvNow ⊢
    try simp only [uncurry_apply_pair]
    by_cases hguard : cursor < last
    · obtain ⟨next, nextCursor, hbody, hinvNext, hcursor⟩ :=
        erase_body_step original erased current cursor last hlast hinvNow hguard
      apply exists_imp_spec
      refine ⟨.cont (next, nextCursor), hbody, ?_⟩
      dsimp only
      refine ⟨hinvNext, ?_⟩
      rw [hcursor]
      exact loop_measure_decreases last.val cursor.val (by simpa using hguard)
    · have hstop : last.val ≤ cursor.val := by simpa using hguard
      have hdone : original.length - 1 ≤ cursor.val := by
        simpa only [hlast] using hstop
      have hcursor := invariant_terminal_cursor hinvNow hdone
      have hfinal : Invariant original erased (original.length - 1) current.val := by
        simpa only [hcursor] using hinvNow
      apply exists_imp_spec
      exact ⟨.done current, erase_body_done current cursor last hguard, hfinal⟩
  · exact hinv

/-- The real loop keeps length fixed and moves exactly the selected original
value into the final slot. Taking last elements is therefore ordered eraseIdx. -/
theorem erase_loop_equiv (original : List T) (erased : Nat)
    (values : alloc.vec.Vec T) (index last : Usize)
    (hlast : last.val = original.length - 1)
    (hinv : Invariant original erased index.val values.val) :
    ∃ out, vec_erase.erase_loop values index last = ok out ∧
      out.val.length = original.length ∧
      out.val.take last.val = original.eraseIdx erased := by
  obtain ⟨out, hrun, hfinal⟩ := spec_imp_exists
    (erase_loop_spec original erased values index last hlast hinv)
  refine ⟨out, hrun, hfinal.length_eq, ?_⟩
  rw [hlast]
  exact invariant_done_take hfinal (Nat.le_refl _)

/-- Actual checked wrapper followed by the strictly shrinking Std resize.
The Clone dictionary is arbitrary; in particular no successful clone is assumed. -/
theorem erase_spec (cloneInst : core.clone.Clone T) (values : alloc.vec.Vec T)
    (index : Usize) (filler : T) (hvalid : index.val < values.val.length) :
    vec_erase.erase cloneInst values index filler ⦃ out =>
      out.val = values.val.eraseIdx index.val ⦄ := by
  have hguard : index < alloc.vec.Vec.len values := by
    simpa only [UScalar.lt_equiv, alloc.vec.Vec.len_val] using hvalid
  have hassert : massert (index < alloc.vec.Vec.len values) = ok () := by
    simp only [massert, hguard, ↓reduceIte]
  have hpositive : 1 ≤ (alloc.vec.Vec.len values).val := by
    rw [alloc.vec.Vec.len_val]
    change 1 ≤ values.val.length
    omega
  obtain ⟨last, hlast, hlastValue, _⟩ := spec_imp_exists
    (UScalar.sub_spec (x := alloc.vec.Vec.len values) (y := 1#usize)
      (by simpa using hpositive))
  have hlastNat : last.val = values.val.length - 1 := by
    simpa using hlastValue
  obtain ⟨moved, hloop, hmovedLength, hmovedTake⟩ :=
    erase_loop_equiv values.val index.val values index last hlastNat
      (invariant_init values.val index.val hvalid)
  have hshrink : last.val < moved.val.length := by
    rw [hlastNat, hmovedLength]
    omega
  obtain ⟨out, hresize, hout⟩ := vec_resize_shrink cloneInst moved last filler hshrink
  apply exists_imp_spec
  refine ⟨out, ?_, hout.trans hmovedTake⟩
  simp only [vec_erase.erase, hassert, hlast, hloop, hresize, bind_tc_ok]

/-- Exact value result and length drop; no spare capacity is required. -/
theorem erase_equiv (cloneInst : core.clone.Clone T) (values : alloc.vec.Vec T)
    (index : Usize) (filler : T) (hvalid : index.val < values.val.length) :
    ∃ out, vec_erase.erase cloneInst values index filler = ok out ∧
      out.val = values.val.eraseIdx index.val ∧
      out.val.length = values.val.length - 1 := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists (erase_spec cloneInst values index filler hvalid)
  refine ⟨out, hrun, hout, ?_⟩
  rw [hout, List.length_eraseIdx_of_lt hvalid]

/-- Explicit prefix/suffix order, valid even when all elements are equal. -/
theorem erase_ordered_spec (cloneInst : core.clone.Clone T) (values : alloc.vec.Vec T)
    (index : Usize) (filler : T) (hvalid : index.val < values.val.length) :
    vec_erase.erase cloneInst values index filler ⦃ out =>
      out.val = values.val.take index.val ++ values.val.drop (index.val + 1) ∧
      out.val.length + 1 = values.val.length ⦄ := by
  obtain ⟨out, hrun, hout, hlength⟩ := erase_equiv cloneInst values index filler hvalid
  apply exists_imp_spec
  refine ⟨out, hrun, ?_, ?_⟩
  · rw [hout, List.eraseIdx_eq_take_drop_succ]
  · omega

/-- Neither filler values nor Clone dictionaries affect a valid execution. -/
theorem erase_parameters_independent (leftClone rightClone : core.clone.Clone T)
    (values : alloc.vec.Vec T) (index : Usize) (left right : T)
    (hvalid : index.val < values.val.length) :
    vec_erase.erase leftClone values index left =
      vec_erase.erase rightClone values index right := by
  obtain ⟨leftOut, hleft, hleftValue⟩ := spec_imp_exists
    (erase_spec leftClone values index left hvalid)
  obtain ⟨rightOut, hright, hrightValue⟩ := spec_imp_exists
    (erase_spec rightClone values index right hvalid)
  have heq : leftOut = rightOut := by
    apply Subtype.ext
    exact hleftValue.trans hrightValue.symm
  rw [hleft, hright, heq]

/-- Outside the valid-index precondition the real entry assertion fails before
subtraction, mutation, or resize. This is an extracted Result, not a Drop trace. -/
theorem erase_invalid (cloneInst : core.clone.Clone T) (values : alloc.vec.Vec T)
    (index : Usize) (filler : T) (hbad : values.val.length ≤ index.val) :
    vec_erase.erase cloneInst values index filler = fail .assertionFailure := by
  have hnot : ¬index < alloc.vec.Vec.len values := by
    simp only [UScalar.lt_equiv, alloc.vec.Vec.len_val]
    change ¬index.val < values.val.length
    omega
  simp only [vec_erase.erase, massert, hnot, ↓reduceIte, bind_tc_fail]

end Verification.VecEraseExtractedProof
