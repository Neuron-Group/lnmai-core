import Verification.Generated
import Verification.VecPrependModel
import Verification.TouchHoldRebuildReverseExtracted

/-!
Connection to the actual Aeneas extraction:
/tmp/lnmai-prefix-prepend-extraction-1/LnmaiCoreVerify.lean
SHA256 ad80b99ecd0608f2af58a4f83fc4b11a6fa47cc207cfe090599beafc495dc1b0.
The three declarations at lines 17277-17312 are referenced directly below.
Verification.Generated must contain that extraction before this file is checked.

Rust source: lnmai-core-rs/crates/lnmai-core-verify/src/vec_prepend.rs
SHA256 15dcf2855e852b26f3b2a1ea508ef4009e73cfa76e15020cc8fe9d695c26d33b.
The original length is saved before push. The actual body uses checked
subtraction, Vec.deref_mut, Slice.swap and the mutable-borrow back function.

The proofs use the existing VecPrependModel invariant. The concrete Aeneas
push and slice-swap specifications discharge their point effects; no successful
operation, callback or whole-function equivalence law is a theorem premise.
The loop proof uses the actual index value as its decreasing measure. It does
not replace the extracted loop with the pure model's prependLoop.

Scope is the Aeneas decoded Vec/Slice and Usize interpretation. The wrapper
requires values.val.length < Usize.max for one representable extra slot. This
bound does not certify native allocator success, byte capacity, Rust Vec layout,
ownership/drop behavior, or translation correctness. No theorem here closes
Rust bigint, UTF-8, or unrelated runtime facade boundaries. No Clone, Inhabited,
DecidableEq or distinctness requirement is placed on elements.

Static draft only: this worker has run no compiler, tests or extraction.
Mainline owns elaboration against the supplied real Generated declarations.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.VecPrependExtractedProof

open VecPrependModel
open TouchHoldRebuildReverseProof (SwapPoints)

variable {α : Type}

/-- The actual false branch returns its input before any subtraction or swap. -/
theorem prepend_body_done (values : alloc.vec.Vec α) (index : Usize)
    (hstop : ¬ (index > 0#usize)) :
    vec_prepend.prepend_loop.body values index = ok (.done values) := by
  simp only [vec_prepend.prepend_loop.body, hstop, ↓reduceIte]

/-- One actual body iteration. The guard proves checked subtraction safe and
both selected slots valid. The concrete deref_mut back function retains exactly
the swapped slice; its point law preserves all five model invariant fields. -/
theorem prepend_body_step (original : List α) (head : α)
    (values : alloc.vec.Vec α) (index : Usize)
    (hinv : Invariant original head index.val values.val)
    (hmore : index > 0#usize) :
    ∃ next previous,
      vec_prepend.prepend_loop.body values index = ok (.cont (next, previous)) ∧
      Invariant original head previous.val next.val ∧
      previous.val = index.val - 1 := by
  have hmoreNat : 0 < index.val := by simpa using hmore
  have hone : 1 ≤ index.val := by omega
  obtain ⟨previous, hsub, hprevious, _⟩ := spec_imp_exists
    (UScalar.sub_spec (x := index) (y := 1#usize) (by simpa using hone))
  have hpreviousNat : previous.val = index.val - 1 := by simpa using hprevious
  obtain ⟨hindexBound, hpreviousBound, _, _⟩ := body_index_bounds hinv hmoreNat
  let slice : Slice α := (alloc.vec.Vec.deref_mut values).1
  have hindexSlice : index.val < slice.val.length := hindexBound
  have hpreviousSlice : previous.val < slice.val.length := by
    change previous.val < values.val.length
    rw [hpreviousNat]
    exact hpreviousBound
  obtain ⟨swapped, hswap, hpoints⟩ := spec_imp_exists
    (TouchHoldRebuildReverseExtractedProof.slice_swap_points
      slice index previous hindexSlice hpreviousSlice)
  let next : alloc.vec.Vec α := (alloc.vec.Vec.deref_mut values).2 swapped
  have hnextPoints : SwapPoints values.val next.val index.val (index.val - 1) := by
    have hpoints' : SwapPoints values.val next.val index.val previous.val := hpoints
    simpa only [hpreviousNat] using hpoints'
  refine ⟨next, previous, ?_, ?_, hpreviousNat⟩
  · unfold vec_prepend.prepend_loop.body
    rw [if_pos hmore]
    simp only [hsub, bind_tc_ok]
    change
      (do
        let swappedSlice ← core.slice.Slice.swap slice index previous
        ok (.cont ((alloc.vec.Vec.deref_mut values).2 swappedSlice, previous) :
          ControlFlow (alloc.vec.Vec α × Usize) (alloc.vec.Vec α))) =
      ok (.cont (next, previous))
    simp only [hswap, bind_tc_ok]
  · rw [hpreviousNat]
    exact invariant_step_of_points hinv hmoreNat hnextPoints

/-- Total execution of the actual extracted loop from any intermediate state
satisfying the invariant. No room assumption is needed after the push: the
represented vector already has its length bound, and every swap preserves it. -/
theorem prepend_loop_spec (original : List α) (head : α)
    (initial : alloc.vec.Vec α) (index : Usize)
    (hinv : Invariant original head index.val initial.val) :
    vec_prepend.prepend_loop initial index ⦃ out => out.val = head :: original ⦄ := by
  unfold vec_prepend.prepend_loop
  apply loop.spec_decr_nat
    (fun state : alloc.vec.Vec α × Usize => state.2.val)
    (fun state => Invariant original head state.2.val state.1.val)
  · rintro ⟨values, cursor⟩ hinvNow
    dsimp only at hinvNow ⊢
    try simp only [uncurry_apply_pair]
    by_cases hguard : cursor > 0#usize
    · obtain ⟨next, previous, hbody, hinvNext, hprevious⟩ :=
        prepend_body_step original head values cursor hinvNow hguard
      apply exists_imp_spec
      refine ⟨.cont (next, previous), hbody, ?_⟩
      dsimp only
      refine ⟨hinvNext, ?_⟩
      rw [hprevious]
      exact loop_measure_decreases cursor.val (by simpa using hguard)
    · have hstop : ¬ (0 < cursor.val) := by simpa using hguard
      apply exists_imp_spec
      exact ⟨.done values, prepend_body_done values cursor hguard,
        invariant_done hinvNow hstop⟩
  · exact hinv

/-- Real wrapper: snapshot the original length, perform the concrete push,
initialize the invariant at that saved length, then call the actual loop.
The empty original list and repeated values are included. -/
theorem prepend_spec (values : alloc.vec.Vec α) (head : α)
    (hroom : values.val.length < Usize.max) :
    vec_prepend.prepend values head ⦃ out => out.val = head :: values.val ⦄ := by
  obtain ⟨pushed, hpush, hpushed⟩ := spec_imp_exists
    (alloc.vec.Vec.push_spec values head hroom)
  have hinit : Invariant values.val head (alloc.vec.Vec.len values).val pushed.val := by
    simpa only [alloc.vec.Vec.len_val, alloc.vec.Vec.length] using
      (invariant_init_of_push hpushed)
  obtain ⟨out, hloop, hout⟩ := spec_imp_exists
    (prepend_loop_spec values.val head pushed (alloc.vec.Vec.len values) hinit)
  apply exists_imp_spec
  refine ⟨out, ?_, hout⟩
  simp only [vec_prepend.prepend, hpush, hloop, bind_tc_ok]

/-- Successful extraction result and exact list/length equations. The last
bound is represented length, not allocated byte capacity or native allocation. -/
theorem prepend_equiv (values : alloc.vec.Vec α) (head : α)
    (hroom : values.val.length < Usize.max) :
    ∃ out, vec_prepend.prepend values head = ok out ∧
      out.val = head :: values.val ∧
      out.val.length = values.val.length + 1 ∧ out.val.length ≤ Usize.max := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists (prepend_spec values head hroom)
  refine ⟨out, hrun, hout, ?_, out.property⟩
  simp only [hout, List.length_cons]

/-- Actual extracted execution, decoded to a list, equals the existing pure
model. This equation follows from both completed proofs and is not a premise. -/
theorem prepend_model_equiv (values : alloc.vec.Vec α) (head : α)
    (hroom : values.val.length < Usize.max) :
    (do
      let out ← vec_prepend.prepend values head
      ok out.val) = ok (VecPrependModel.prepend values.val head) := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists (prepend_spec values head hroom)
  simp only [hrun, bind_tc_ok, hout, VecPrependModel.prepend_eq_cons]

end Verification.VecPrependExtractedProof

