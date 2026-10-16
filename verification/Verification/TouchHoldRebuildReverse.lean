import Init.Data.List.Lemmas
import Lean.Elab.Tactic.Omega

/-!
List-level proof of the actual touch_hold_rebuild::reverse_groups algorithm:
snapshot len, set half := len / 2, and swap (left, len - 1 - left) while
left < half. The loop invariant specifies both completed ends and the unchanged
middle. At exit an odd-length middle consists of its own mirror position.

No reference rebuild model or Generated interface is assumed here. swapAt is
defined by two concrete List.set operations; SwapPoints exposes their point
effect for later use with the extracted swap_groups atom. The loop theorem is
proved from these point laws, never supplied as a whole-reverse hypothesis.

This is an uncompiled proof draft. The real Generated loop will be connected
after mainline extraction, using Invariant, invariant_step_of_points,
invariant_done and loop_measure_decreases. No runtime facade is claimed proved.
-/

namespace Verification.TouchHoldRebuildReverseProof

universe u
variable {α : Type u}

/-- Two original reads and two indexed writes. Invalid indices return the input;
the loop invariant proves that this fallback is never used by reverse_groups. -/
def swapAt (items : List α) (left right : Nat) : List α :=
  match items[left]?, items[right]? with
  | some leftValue, some rightValue =>
      (items.set left rightValue).set right leftValue
  | _, _ => items

@[simp] theorem swapAt_length (items : List α) (left right : Nat) :
    (swapAt items left right).length = items.length := by
  cases hl : items[left]? <;> cases hr : items[right]? <;>
    simp [swapAt, hl, hr]

theorem swapAt_eq_set (items : List α) (left right : Nat)
    (hl : left < items.length) (hr : right < items.length) :
    swapAt items left right =
      (items.set left (items[right]'hr)).set right (items[left]'hl) := by
  simp only [swapAt, List.getElem?_eq_getElem hl, List.getElem?_eq_getElem hr]

/-- Only the two selected positions change. No equality or uniqueness structure
is required on α, and both complete records are retained verbatim. -/
structure SwapPoints (before after : List α) (left right : Nat) : Prop where
  length_eq : after.length = before.length
  left_eq : after[left]? = before[right]?
  right_eq : after[right]? = before[left]?
  other_eq : ∀ index, index ≠ left → index ≠ right →
    after[index]? = before[index]?

/-- The point contract is proved for the concrete two-write list operation. -/
theorem swapAt_points (items : List α) (left right : Nat)
    (hl : left < items.length) (hr : right < items.length) :
    SwapPoints items (swapAt items left right) left right := by
  refine ⟨swapAt_length items left right, ?_, ?_, ?_⟩
  · rw [swapAt_eq_set items left right hl hr]
    by_cases heq : left = right
    · subst right
      simp [List.getElem?_set, hl, List.getElem?_eq_getElem hl]
    · simp [List.getElem?_set, hl, hr, heq, Ne.symm heq,
        List.getElem?_eq_getElem hr]
  · rw [swapAt_eq_set items left right hl hr]
    have hrSet : right < (items.set left (items[right]'hr)).length := by
      simpa only [List.length_set] using hr
    rw [List.getElem?_set_self hrSet, List.getElem?_eq_getElem hl]
  · intro index hleft hright
    rw [swapAt_eq_set items left right hl hr,
      List.getElem?_set_ne (Ne.symm hright),
      List.getElem?_set_ne (Ne.symm hleft)]

/-- The guard makes both selected indices valid and distinct. -/
theorem body_index_bounds (len left : Nat) (hmore : left < len / 2) :
    2 ≤ len ∧ left < len ∧ len - 1 - left < len ∧
      left < len - 1 - left ∧ left + 1 ≤ len / 2 := by
  omega

/-- Natural-number obligations for the two checked Usize subtractions and the
increment. A future extraction theorem supplies len ≤ maxValue from its scalar. -/
theorem body_scalar_bounds (len left maxValue : Nat)
    (hmore : left < len / 2) (hlen : len ≤ maxValue) :
    1 ≤ len ∧ left ≤ len - 1 ∧ len - 1 ≤ maxValue ∧
      len - 1 - left ≤ maxValue ∧ left + 1 ≤ maxValue := by
  omega

theorem loop_measure_decreases (half left : Nat) (hmore : left < half) :
    half - (left + 1) < half - left := by
  omega

/-- Both completed ends are mirrored; all not-yet-swapped positions retain the
original value. List length is the full original length, including duplicates. -/
structure Invariant (original : List α) (left : Nat) (current : List α) : Prop where
  length_eq : current.length = original.length
  left_le_half : left ≤ original.length / 2
  prefix_eq : ∀ index, index < left →
    current[index]? = original[original.length - 1 - index]?
  suffix_eq : ∀ index, original.length - left ≤ index → index < original.length →
    current[index]? = original[original.length - 1 - index]?
  middle_eq : ∀ index, left ≤ index → index < original.length - left →
    current[index]? = original[index]?

theorem invariant_init (original : List α) : Invariant original 0 original := by
  refine ⟨rfl, Nat.zero_le _, ?_, ?_, ?_⟩
  · intro index hindex
    omega
  · intro index hlow hhigh
    omega
  · intro index _ _
    rfl

/-- The two positions about to be swapped have not been touched previously. -/
theorem invariant_body_reads {original current : List α} {left : Nat}
    (hinv : Invariant original left current) (hmore : left < original.length / 2) :
    current[left]? = original[left]? ∧
      current[original.length - 1 - left]? = original[original.length - 1 - left]? := by
  constructor
  · exact hinv.middle_eq left (Nat.le_refl _) (by omega)
  · exact hinv.middle_eq (original.length - 1 - left) (by omega) (by omega)

/-- The reusable loop-body proof assumes only a single two-slot mutation law.
It does not assume correctness of a loop, group operation, or initializer. -/
theorem invariant_step_of_points {original before after : List α} {left : Nat}
    (hinv : Invariant original left before) (hmore : left < original.length / 2)
    (hswap : SwapPoints before after left (original.length - 1 - left)) :
    Invariant original (left + 1) after := by
  have hright : left < original.length - 1 - left := by omega
  have hmirror : original.length - 1 - (original.length - 1 - left) = left := by
    omega
  obtain ⟨hreadLeft, hreadRight⟩ := invariant_body_reads hinv hmore
  refine ⟨hswap.length_eq.trans hinv.length_eq, by omega, ?_, ?_, ?_⟩
  · intro index hindex
    by_cases heq : index = left
    · subst index
      exact hswap.left_eq.trans hreadRight
    · have hother : index ≠ original.length - 1 - left := by omega
      rw [hswap.other_eq index heq hother]
      exact hinv.prefix_eq index (by omega)
  · intro index hlow hhigh
    by_cases heq : index = original.length - 1 - left
    · subst index
      rw [hmirror]
      exact hswap.right_eq.trans hreadLeft
    · have hother : index ≠ left := by omega
      rw [hswap.other_eq index hother heq]
      exact hinv.suffix_eq index (by omega) hhigh
  · intro index hlow hhigh
    have hleft : index ≠ left := by omega
    have hrightIndex : index ≠ original.length - 1 - left := by omega
    rw [hswap.other_eq index hleft hrightIndex]
    exact hinv.middle_eq index (by omega) (by omega)

theorem invariant_step {original current : List α} {left : Nat}
    (hinv : Invariant original left current) (hmore : left < original.length / 2) :
    Invariant original (left + 1)
      (swapAt current left (original.length - 1 - left)) := by
  apply invariant_step_of_points hinv hmore
  apply swapAt_points
  · rw [hinv.length_eq]
    omega
  · rw [hinv.length_eq]
    omega

/-- At the terminal boundary any untouched position is the odd-length center,
which is its own mirror. For even lengths there is no untouched position. -/
theorem terminal_middle_fixed (len left index : Nat)
    (hleft : left = len / 2) (hlow : left ≤ index) (hhigh : index < len - left) :
    len - 1 - index = index := by
  omega

/-- The invariant plus a false guard implies exact reverse, including [] and
singleton lists. Neither uniqueness nor an assumption about reverse is used. -/
theorem invariant_done {original current : List α} {left : Nat}
    (hinv : Invariant original left current) (hstop : original.length / 2 ≤ left) :
    current = original.reverse := by
  have hleft : left = original.length / 2 := Nat.le_antisymm hinv.left_le_half hstop
  apply List.ext_getElem?
  intro index
  by_cases hindex : index < original.length
  · rw [List.getElem?_reverse hindex]
    by_cases hp : index < left
    · exact hinv.prefix_eq index hp
    · by_cases hs : original.length - left ≤ index
      · exact hinv.suffix_eq index hs hindex
      · have hlow : left ≤ index := by omega
        have hhigh : index < original.length - left := by omega
        have hfixed := terminal_middle_fixed original.length left index hleft hlow hhigh
        rw [hfixed]
        exact hinv.middle_eq index hlow hhigh
  · have hcurrent : current.length ≤ index := by
      rw [hinv.length_eq]
      omega
    have hreverse : original.reverse.length ≤ index := by
      rw [List.length_reverse]
      omega
    rw [List.getElem?_eq_none hcurrent, List.getElem?_eq_none hreverse]

/-- Pure list execution of the same loop, retaining the separately snapshotted
len and half. No generated name or signature is predicted by this definition. -/
def reverseLoop (len half left : Nat) (current : List α) : List α :=
  if hmore : left < half then
    reverseLoop len half (left + 1) (swapAt current left (len - 1 - left))
  else current
termination_by half - left
decreasing_by omega

theorem reverseLoop_stop {len half left : Nat} {current : List α}
    (hstop : half ≤ left) : reverseLoop len half left current = current := by
  have hnot : ¬left < half := by omega
  rw [reverseLoop]
  simp only [hnot, ↓reduceIte, ↓reduceDIte]

theorem reverseLoop_step {len half left : Nat} {current : List α}
    (hmore : left < half) :
    reverseLoop len half left current =
      reverseLoop len half (left + 1) (swapAt current left (len - 1 - left)) := by
  rw [reverseLoop]
  simp only [hmore, ↓reduceIte, ↓reduceDIte]

/-- Finite completion from any state satisfying the invariant. The induction
counts remaining swaps, rather than assuming termination or loop equivalence. -/
theorem reverseLoop_complete (original : List α) (remaining : Nat) :
    ∀ (left : Nat) (current : List α), original.length / 2 - left = remaining →
      Invariant original left current →
      reverseLoop original.length (original.length / 2) left current = original.reverse := by
  induction remaining with
  | zero =>
      intro left current hremaining hinv
      have hstop : original.length / 2 ≤ left := by omega
      rw [reverseLoop_stop hstop]
      exact invariant_done hinv hstop
  | succ remaining ih =>
      intro left current hremaining hinv
      have hmore : left < original.length / 2 := by omega
      rw [reverseLoop_step hmore]
      apply ih (left + 1) (swapAt current left (original.length - 1 - left))
      · omega
      · exact invariant_step hinv hmore

theorem reverseLoop_eq_reverse_of_invariant {original current : List α} {left : Nat}
    (hinv : Invariant original left current) :
    reverseLoop original.length (original.length / 2) left current = original.reverse :=
  reverseLoop_complete original (original.length / 2 - left) left current rfl hinv

theorem reverseLoop_eq_reverse (original : List α) :
    reverseLoop original.length (original.length / 2) 0 original = original.reverse :=
  reverseLoop_eq_reverse_of_invariant (invariant_init original)

/-- List interpretation of the reverse_groups entry point. -/
def reverseGroups (groups : List α) : List α :=
  reverseLoop groups.length (groups.length / 2) 0 groups

theorem reverseGroups_eq_reverse (groups : List α) :
    reverseGroups groups = groups.reverse :=
  reverseLoop_eq_reverse groups

@[simp] theorem reverseGroups_length (groups : List α) :
    (reverseGroups groups).length = groups.length := by
  rw [reverseGroups_eq_reverse, List.length_reverse]

end Verification.TouchHoldRebuildReverseProof
