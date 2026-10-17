import Verification.TouchHoldRebuildReverse

/-!
List model for the actual vec_prepend.rs algorithm: snapshot the old length,
push head at the back, then swap (index, index - 1) while index > 0.

Invariant tracks the original length, the moving head, the unchanged prefix,
and the suffix shifted right by one position. A body step uses only the existing
TouchHoldRebuildReverseProof.SwapPoints contract for those two adjacent slots.
No DecidableEq, uniqueness, Clone, or Inhabited requirement is imposed on elements.

This file is only a model and reusable invariant proof. prependLoop and prepend
below are explicitly list interpretations, not generated Rust declarations.
No Generated names or signatures are assumed. Connecting the actual push,
checked subtraction, swap and extracted loop must follow the real extraction.
The scalar lemmas state length/room bounds only; native allocation success,
representation and ownership/drop behavior remain separate obligations.
This draft has not been compiled by its author; mainline owns Lean validation.
-/

namespace Verification.VecPrependModel

open TouchHoldRebuildReverseProof (SwapPoints swapAt swapAt_points)

universe u
variable {α : Type u}

/-- After the push and each swap, head occupies index. Positions strictly before
index retain their original slot; valid positions after index contain the old
preceding slot. The upper suffix bound is the original length because current
has exactly one extra element. Duplicates are unrestricted. -/
structure Invariant (original : List α) (head : α) (index : Nat)
    (current : List α) : Prop where
  index_le_length : index ≤ original.length
  length_eq : current.length = original.length + 1
  head_eq : current[index]? = some head
  prefix_eq : ∀ position, position < index →
    current[position]? = original[position]?
  suffix_eq : ∀ position, index < position → position ≤ original.length →
    current[position]? = original[position - 1]?

/-- Initial state after one successful push: old elements followed by head. -/
theorem invariant_init (original : List α) (head : α) :
    Invariant original head original.length (original ++ [head]) := by
  refine ⟨Nat.le_refl _, by simp, ?_, ?_, ?_⟩
  · rw [List.getElem?_append_right (Nat.le_refl _), Nat.sub_self]
    rfl
  · intro position hposition
    exact List.getElem?_append_left hposition
  · intro position hlow hhigh
    omega

/-- Interface for a later single-push point law; no loop equation is assumed. -/
theorem invariant_init_of_push {original current : List α} {head : α}
    (hpush : current = original ++ [head]) :
    Invariant original head original.length current := by
  rw [hpush]
  exact invariant_init original head

/-- One spare length slot suffices for representability after push. This is
arithmetic only, and does not assert that an allocator succeeds. -/
theorem push_length_fits (original : List α) (head : α) (maxValue : Nat)
    (hroom : original.length < maxValue) :
    (original ++ [head]).length ≤ maxValue := by
  simp only [List.length_append, List.length_cons, List.length_nil]
  omega

/-- Swapping preserves the post-push length bound throughout the loop. -/
theorem invariant_capacity {original current : List α} {head : α} {index maxValue : Nat}
    (hinv : Invariant original head index current)
    (hfit : original.length + 1 ≤ maxValue) : current.length ≤ maxValue := by
  rw [hinv.length_eq]
  exact hfit

/-- With a true guard both adjacent slots are valid, and the left neighbor is
still within the old list. The subtraction therefore cannot underflow. -/
theorem body_index_bounds {original current : List α} {head : α} {index : Nat}
    (hinv : Invariant original head index current) (hmore : 0 < index) :
    index < current.length ∧ index - 1 < current.length ∧
      index - 1 < index ∧ index - 1 < original.length := by
  have hindex := hinv.index_le_length
  have hlength := hinv.length_eq
  omega

/-- Natural-number facts for a future Usize bridge. No machine operation or
allocation model is introduced by this lemma. -/
theorem body_scalar_bounds (originalLength index maxValue : Nat)
    (hindex : index ≤ originalLength) (hfit : originalLength + 1 ≤ maxValue)
    (hmore : 0 < index) :
    originalLength ≤ maxValue ∧ index ≤ maxValue ∧
      1 ≤ index ∧ index - 1 ≤ maxValue := by
  omega

theorem loop_measure_decreases (index : Nat) (hmore : 0 < index) :
    index - 1 < index := by
  omega

/-- Before the swap, the right slot is head and the left slot is the untouched
old element. No element equality decision or cloning operation is used. -/
theorem invariant_body_reads {original current : List α} {head : α} {index : Nat}
    (hinv : Invariant original head index current) (hmore : 0 < index) :
    current[index]? = some head ∧
      current[index - 1]? = original[index - 1]? := by
  exact ⟨hinv.head_eq, hinv.prefix_eq (index - 1) (by omega)⟩

/-- The actual body swaps index with its left neighbor, then decrements index.
The sole mutation premise is one two-slot SwapPoints law. -/
theorem invariant_step_of_points {original before after : List α} {head : α} {index : Nat}
    (hinv : Invariant original head index before) (hmore : 0 < index)
    (hswap : SwapPoints before after index (index - 1)) :
    Invariant original head (index - 1) after := by
  have hindex := hinv.index_le_length
  obtain ⟨hhead, hprevious⟩ := invariant_body_reads hinv hmore
  refine ⟨by omega, hswap.length_eq.trans hinv.length_eq,
    hswap.right_eq.trans hhead, ?_, ?_⟩
  · intro position hposition
    have hnotIndex : position ≠ index := by omega
    have hnotPrevious : position ≠ index - 1 := by omega
    rw [hswap.other_eq position hnotIndex hnotPrevious]
    exact hinv.prefix_eq position (by omega)
  · intro position hlow hhigh
    by_cases heq : position = index
    · subst position
      exact hswap.left_eq.trans hprevious
    · have hnotPrevious : position ≠ index - 1 := by omega
      rw [hswap.other_eq position heq hnotPrevious]
      exact hinv.suffix_eq position (by omega) hhigh

/-- Concrete list-swap instance of the point-law step; its in-bounds premises
follow from the invariant, so swapAt's out-of-bounds fallback is never used. -/
theorem invariant_step {original current : List α} {head : α} {index : Nat}
    (hinv : Invariant original head index current) (hmore : 0 < index) :
    Invariant original head (index - 1) (swapAt current index (index - 1)) := by
  obtain ⟨hindex, hprevious, _, _⟩ := body_index_bounds hinv hmore
  exact invariant_step_of_points hinv hmore
    (swapAt_points current index (index - 1) hindex hprevious)

/-- A false guard leaves head at zero and every old element at its next slot.
The equality covers the whole list, including out-of-bounds reads. -/
theorem invariant_done {original current : List α} {head : α} {index : Nat}
    (hinv : Invariant original head index current) (hstop : ¬ 0 < index) :
    current = head :: original := by
  have hzero : index = 0 := by omega
  subst index
  apply List.ext_getElem?
  intro position
  cases position with
  | zero => exact hinv.head_eq
  | succ position =>
      change current[position + 1]? = original[position]?
      by_cases hposition : position < original.length
      · have hsuffix := hinv.suffix_eq (position + 1) (by omega) (by omega)
        simpa only [Nat.add_sub_cancel] using hsuffix
      · have hcurrent : current.length ≤ position + 1 := by
          rw [hinv.length_eq]
          omega
        have horiginal : original.length ≤ position := by omega
        rw [List.getElem?_eq_none hcurrent, List.getElem?_eq_none horiginal]

/-- Pure list execution with a structurally decreasing index. This definition
is not an extraction; the invariant lemmas above are the later loop interface. -/
def prependLoop : Nat → List α → List α
  | 0, current => current
  | index + 1, current => prependLoop index (swapAt current (index + 1) index)

theorem prependLoop_stop {index : Nat} {current : List α} (hstop : ¬ 0 < index) :
    prependLoop index current = current := by
  have hzero : index = 0 := by omega
  subst index
  rfl

theorem prependLoop_step {index : Nat} {current : List α} (hmore : 0 < index) :
    prependLoop index current =
      prependLoop (index - 1) (swapAt current index (index - 1)) := by
  cases index with
  | zero => omega
  | succ index => rfl

/-- Completion of the pure model from any intermediate invariant state. This
is proved by induction on index, not supplied as a whole-loop premise. -/
theorem prependLoop_complete (original : List α) (head : α) (index : Nat) :
    ∀ current, Invariant original head index current →
      prependLoop index current = head :: original := by
  induction index with
  | zero =>
      intro current hinv
      exact invariant_done hinv (by omega)
  | succ index ih =>
      intro current hinv
      change prependLoop index (swapAt current (index + 1) index) = head :: original
      apply ih
      simpa only [Nat.succ_eq_add_one, Nat.add_sub_cancel] using
        (invariant_step hinv (by omega))

/-- List interpretation of snapshot-length, push, then adjacent swaps. -/
def prepend (original : List α) (head : α) : List α :=
  prependLoop original.length (original ++ [head])

theorem prepend_eq_cons (original : List α) (head : α) :
    prepend original head = head :: original := by
  exact prependLoop_complete original head original.length
    (original ++ [head]) (invariant_init original head)

@[simp] theorem prepend_length (original : List α) (head : α) :
    (prepend original head).length = original.length + 1 := by
  rw [prepend_eq_cons, List.length_cons]

end Verification.VecPrependModel
