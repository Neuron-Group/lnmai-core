import Verification.Generated
import LnmaiCore.Lifecycle

/-!
Queue length observations used by both lifecycle and parent scheduling.
The Rust kernels accept arbitrary checkpoint payloads and inspect only vector
lengths. The loop proofs below target the generated Rust declarations, rather
than assuming that a complete scan agrees with a separate model.

The signatures and bodies were checked against the combined extraction in
Generated.lean: `remaining_loop queues maximum index` carries `(maximum, index)`;
`cleared_loop queues index` carries `index` and branches on the Bool `count != 0`.
These proofs have passed Lean elaboration and transitive axiom checks.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore Lifecycle

set_option maxHeartbeats 300000

namespace Verification.SlideQueueMetricsProof

def remainingGo {A : Type} : List (alloc.vec.Vec A) → Nat → Nat
  | [], maximum => maximum
  | queue :: rest, maximum => remainingGo rest (max maximum queue.val.length)

def clearedForQueues {A : Type} : List (alloc.vec.Vec A) → Bool
  | [] => true
  | queue :: rest => if queue.val.isEmpty then clearedForQueues rest else false

/-- A single reference step, expressed with the length test used by Rust. -/
theorem cleared_cons_length {A : Type} (queue : alloc.vec.Vec A)
    (rest : List (alloc.vec.Vec A)) :
    clearedForQueues (queue :: rest) =
      if queue.val.length = 0 then clearedForQueues rest else false := by
  simp only [clearedForQueues, List.isEmpty_iff_length_eq_zero]

/-- The payload interpretation is arbitrary: these observations only use lengths. -/
def interpretQueues {A : Type} (decode : A → SlideArea)
    (queues : List (alloc.vec.Vec A)) : List SlideQueue :=
  queues.map (fun queue => queue.val.map decode)

theorem remaining_go_reference {A : Type} (decode : A → SlideArea)
    (queues : List (alloc.vec.Vec A)) (maximum : Nat) :
    remainingGo queues maximum =
      slideQueueRemaining.go maximum (interpretQueues decode queues) := by
  induction queues generalizing maximum with
  | nil => rfl
  | cons queue rest ih =>
      simpa only [remainingGo, interpretQueues, List.map_cons, List.length_map,
        slideQueueRemaining.go] using ih (max maximum queue.val.length)

theorem remaining_reference {A : Type} (decode : A → SlideArea)
    (queues : List (alloc.vec.Vec A)) :
    remainingGo queues 0 = slideQueueRemaining (interpretQueues decode queues) :=
  remaining_go_reference decode queues 0

theorem cleared_reference {A : Type} (decode : A → SlideArea)
    (queues : List (alloc.vec.Vec A)) :
    clearedForQueues queues = slideQueuesCleared (interpretQueues decode queues) := by
  induction queues with
  | nil => rfl
  | cons queue rest ih =>
      simpa only [clearedForQueues, interpretQueues, List.map_cons,
        slideQueuesCleared, List.isEmpty_map] using
        congrArg (fun value => if queue.val.isEmpty then value else false) ih

/-- The guard supplies the increment bound, even when `len = Usize.max`. -/
private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

/-- Actual maximum scan from any representable accumulator and index.
    The suffix invariant also covers an initial index past the end. The loop
    guard proves indexing safe; each continuation reduces the unscanned length.
    Maximum updates copy a vector length, so they require no arithmetic premise. -/
theorem remaining_loop_spec {A : Type} (queues : Slice (alloc.vec.Vec A))
    (initialMaximum initialIndex : Usize) :
    slide_queue_metrics.remaining_loop queues initialMaximum initialIndex
      ⦃ out => out.val =
        remainingGo (queues.val.drop initialIndex.val) initialMaximum.val ⦄ := by
  unfold slide_queue_metrics.remaining_loop
  apply loop.spec_decr_nat
    (fun (p : Usize × Usize) => queues.val.length - p.2.val)
    (fun p => remainingGo (queues.val.drop p.2.val) p.1.val =
      remainingGo (queues.val.drop initialIndex.val) initialMaximum.val)
  · rintro ⟨maximum, index⟩ hinv
    dsimp only at hinv ⊢
    unfold slide_queue_metrics.remaining_loop.body
    by_cases hmore : index < Slice.len queues
    · have hindex : index.val < queues.val.length := by simpa using hmore
      obtain ⟨queue, hget, hqueue⟩ := spec_imp_exists
        (Slice.index_usize_spec queues index hindex)
      obtain ⟨next, hnext, hv⟩ := increment_index index (Slice.len queues)
        (by simpa using hmore)
      have hrest :
          remainingGo (queues.val.drop (index.val + 1))
              (max maximum.val queue.val.length) =
            remainingGo (queues.val.drop initialIndex.val) initialMaximum.val := by
        rw [List.drop_eq_getElem_cons hindex, ← hqueue] at hinv
        exact hinv
      have hdecrease : queues.val.length - next.val <
          queues.val.length - index.val := by omega
      by_cases hlarger : maximum < alloc.vec.Vec.len queue
      · have hlt : maximum.val < queue.val.length := by simpa using hlarger
        have hmax : max maximum.val queue.val.length = queue.val.length :=
          Nat.max_eq_right (Nat.le_of_lt hlt)
        have hkeep : remainingGo (queues.val.drop next.val)
              (alloc.vec.Vec.len queue).val =
            remainingGo (queues.val.drop initialIndex.val) initialMaximum.val := by
          simpa only [hv, alloc.vec.Vec.len_val, alloc.vec.Vec.length, hmax] using hrest
        simpa [hmore, hget, bind_tc_ok, hlarger, hlt, hnext] using And.intro hkeep hdecrease
      · have hle : queue.val.length ≤ maximum.val := by simpa using hlarger
        have hmax : max maximum.val queue.val.length = maximum.val :=
          Nat.max_eq_left hle
        have hkeep : remainingGo (queues.val.drop next.val) maximum.val =
            remainingGo (queues.val.drop initialIndex.val) initialMaximum.val := by
          simpa only [hv, hmax] using hrest
        simpa [hmore, hget, bind_tc_ok, hlarger, Nat.not_lt.mpr hle, hnext] using
          And.intro hkeep hdecrease
    · have hdone : queues.val.length ≤ index.val := by simpa using hmore
      have hempty : queues.val.drop index.val = [] := List.drop_eq_nil_of_le hdone
      have hresult : maximum.val =
          remainingGo (queues.val.drop initialIndex.val) initialMaximum.val := by
        simpa only [hempty, remainingGo] using hinv
      simpa [hmore] using hresult
  · rfl

/-- The actual Rust entry point starts with zero maximum and index. All length
    bounds follow from the extracted Slice/Vec types; there is no scan premise. -/
theorem remaining_spec {A : Type} (queues : Slice (alloc.vec.Vec A)) :
    slide_queue_metrics.remaining queues
      ⦃ out => out.val = remainingGo queues.val 0 ⦄ := by
  unfold slide_queue_metrics.remaining
  simpa using remaining_loop_spec queues 0#usize 0#usize

/-- Actual emptiness scan, including the early return at the first nonempty
    track. Only an empty track continues, reducing the unscanned length. -/
theorem cleared_loop_spec {A : Type} (queues : Slice (alloc.vec.Vec A))
    (initialIndex : Usize) :
    slide_queue_metrics.cleared_loop queues initialIndex
      ⦃ out => out = clearedForQueues (queues.val.drop initialIndex.val) ⦄ := by
  unfold slide_queue_metrics.cleared_loop
  apply loop.spec_decr_nat
    (fun (index : Usize) => queues.val.length - index.val)
    (fun index => clearedForQueues (queues.val.drop index.val) =
      clearedForQueues (queues.val.drop initialIndex.val))
  · intro index hinv
    unfold slide_queue_metrics.cleared_loop.body
    by_cases hmore : index < Slice.len queues
    · have hindex : index.val < queues.val.length := by simpa using hmore
      obtain ⟨queue, hget, hqueue⟩ := spec_imp_exists
        (Slice.index_usize_spec queues index hindex)
      have hrest :
          (if queue.val.length = 0
            then clearedForQueues (queues.val.drop (index.val + 1)) else false) =
            clearedForQueues (queues.val.drop initialIndex.val) := by
        rw [List.drop_eq_getElem_cons hindex, ← hqueue, cleared_cons_length] at hinv
        exact hinv
      by_cases hzero : alloc.vec.Vec.len queue = 0#usize
      · have hlength : queue.val.length = 0 := by
          simpa using congrArg (fun (value : Usize) => value.val) hzero
        obtain ⟨next, hnext, hv⟩ := increment_index index (Slice.len queues)
          (by simpa using hmore)
        have hkeep : clearedForQueues (queues.val.drop next.val) =
            clearedForQueues (queues.val.drop initialIndex.val) := by
          simpa only [hlength, ↓reduceIte, hv] using hrest
        have hdecrease : queues.val.length - next.val <
            queues.val.length - index.val := by omega
        have hempty : queue.val = [] := List.eq_nil_of_length_eq_zero hlength
        simpa [hmore, hget, bind_tc_ok, bne_iff_ne, hzero, hempty, hnext] using
          And.intro hkeep hdecrease
      · have hlength : queue.val.length ≠ 0 := by
          intro hempty
          apply hzero
          apply UScalar.eq_of_val_eq
          simpa using hempty
        have hresult : false = clearedForQueues (queues.val.drop initialIndex.val) := by
          simpa only [hlength, ↓reduceIte] using hrest
        have hnonempty : queue.val ≠ [] := by
          intro hnil
          exact hlength (by simp [hnil])
        simpa [hmore, hget, bind_tc_ok, bne_iff_ne, hzero, hnonempty] using hresult
    · have hdone : queues.val.length ≤ index.val := by simpa using hmore
      have hempty : queues.val.drop index.val = [] := List.drop_eq_nil_of_le hdone
      have hresult : true = clearedForQueues (queues.val.drop initialIndex.val) := by
        simpa only [hempty, clearedForQueues] using hinv
      simpa [hmore] using hresult
  · rfl

theorem cleared_spec {A : Type} (queues : Slice (alloc.vec.Vec A)) :
    slide_queue_metrics.cleared queues
      ⦃ out => out = clearedForQueues queues.val ⦄ := by
  unfold slide_queue_metrics.cleared
  simpa using cleared_loop_spec queues 0#usize

/-- Original-reference equivalence for arbitrary checkpoint payloads.
    `decode` need not preserve fields or be injective: queue lengths suffice. -/
theorem remaining_equiv {A : Type} (decode : A → SlideArea)
    (queues : Slice (alloc.vec.Vec A)) :
    slide_queue_metrics.remaining queues
      ⦃ out => out.val = slideQueueRemaining (interpretQueues decode queues.val) ⦄ := by
  simpa only [remaining_reference decode queues.val] using remaining_spec queues

/-- Original-reference equivalence, including both successful Bool outcomes. -/
theorem cleared_equiv {A : Type} (decode : A → SlideArea)
    (queues : Slice (alloc.vec.Vec A)) :
    slide_queue_metrics.cleared queues =
      ok (slideQueuesCleared (interpretQueues decode queues.val)) := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists (cleared_spec queues)
  simpa only [hout, cleared_reference decode queues.val] using hrun

/-!
Remaining representation boundaries:
* Runtime callers must identify their checkpoint/container interpretation with
  `interpretQueues`; composing these theorems into lifecycle/parent proofs is
  external to this file. No Clone/equality/payload-field assumption is required.
* Usize bounds are supplied by the generated Slice/Vec representations. The
  proofs do not assert that unbounded reference lists fit in native containers.
-/

end Verification.SlideQueueMetricsProof
