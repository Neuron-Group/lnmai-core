import Verification.SlideArea
import Verification.SlideQueueModel

namespace Verification.SlideQueueProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open StorageProof SlideAreaProof

def toLnmQueue {P : Type} (progress : P → Nat) (queue : Slice (lifecycle.SlideArea P)) :
    LnmaiCore.Lifecycle.SlideQueue := queue.val.map (toLnmSlideArea progress)

theorem uncons_equiv {T : Type} (queue : Slice T) (first : T) (tail : List T)
    (hqueue : queue.val = first :: tail) :
    ∃ rest, Slice.index_usize queue 0#usize = ok first ∧
      core.slice.index.Slice.index (core.slice.index.SliceIndexRangeFromUsizeSlice T)
        queue { start := 1#usize } = ok rest ∧
      rest.val = tail := by
  refine ⟨queue.drop 1#usize, ?_, ?_, ?_⟩
  · simp [Slice.index_usize, hqueue]
  · simp [core.slice.index.Slice.index,
      core.slice.index.SliceIndexRangeFromUsizeSlice,
      core.slice.index.SliceIndexRangeFromUsizeSlice.index, Slice.length, hqueue]
  · simp [Slice.drop, hqueue]

theorem result_bounds {P : Type} (progress : P → Nat) (fuel : Nat) (emit : Bool)
    (queue : Slice (lifecycle.SlideArea P)) (held : storage.SensorVec Bool)
    (out : alloc.vec.Vec (lifecycle.SlideArea P) × alloc.vec.Vec P)
    (heq : (toLnmQueue progress out.1, out.2.val.map progress) =
      advanceModel fuel emit (toLnmQueue progress queue) (toLnmSensorVec held)) :
    out.1.val.length ≤ queue.val.length ∧ out.2.val.length ≤ fuel := by
  have h := advance_model_bounds fuel emit (toLnmQueue progress queue) (toLnmSensorVec held)
  rw [← heq] at h
  simpa only [toLnmQueue, List.length_map] using h

theorem extend_values_equiv {T : Type} (clone : core.clone.Clone T)
    (values : alloc.vec.Vec T) (tail : Slice T)
    (hclone : ∀ value, clone.clone value = ok value)
    (hbound : values.val.length + tail.val.length ≤ Std.Usize.max) :
    ∃ result, alloc.vec.Vec.extend_from_slice clone values tail = ok result ∧
      result.val = values.val ++ tail.val := by
  obtain ⟨copied, hcopied, heq⟩ := spec_imp_exists
    (Slice.clone_spec (s := tail) (by intro value _; exact hclone value))
  subst copied
  refine ⟨⟨values.val ++ tail.val, by simpa only [List.length_append] using hbound⟩, ?_, rfl⟩
  simp only [alloc.vec.Vec.extend_from_slice, alloc.vec.Vec.length,
    Slice.length, hbound, ↓reduceDIte, hcopied]
  split
  next copied hresult =>
    have : tail = copied := Result.ok.inj (hcopied.symm.trans hresult)
    subst copied
    rfl
  next error hresult => rw [hcopied] at hresult; cases hresult
  next hresult => rw [hcopied] at hresult; cases hresult

theorem with_head_equiv {P : Type} (clone : core.clone.Clone P)
    (head : lifecycle.SlideArea P) (tail : Slice (lifecycle.SlideArea P))
    (hclone : ∀ value, clone.clone value = ok value)
    (hbound : tail.val.length < Std.Usize.max) :
    ∃ result, slide_queue.with_head clone head tail = ok result ∧
      result.val = head :: tail.val := by
  obtain ⟨singleton, hsingleton, hs⟩ := spec_imp_exists
    (alloc.vec.Vec.push_spec (alloc.vec.Vec.new _) head (by scalar_tac))
  obtain ⟨result, hresult, hr⟩ := extend_values_equiv
    (lifecycle.SlideArea.Insts.CoreCloneClone clone) singleton tail
      (fun value => clone_equiv clone value hclone)
      (by simp only [hs, alloc.vec.Vec.new, List.nil_append, List.length_singleton]; omega)
  refine ⟨result, by simp [slide_queue.with_head, hsingleton, hresult], ?_⟩
  simpa [hs] using hr

theorem prepend_progress_equiv {P : Type} (clone : core.clone.Clone P)
    (emit : Bool) (progress : P) (rest : Slice P)
    (hclone : ∀ value, clone.clone value = ok value)
    (hbound : rest.val.length < Std.Usize.max) :
    ∃ result, slide_queue.prepend_progress clone emit progress rest = ok result ∧
      result.val = (if emit then progress :: rest.val else []) := by
  cases emit with
  | false => exact ⟨_, rfl, rfl⟩
  | true =>
      obtain ⟨singleton, hsingleton, hs⟩ := spec_imp_exists
        (alloc.vec.Vec.push_spec (alloc.vec.Vec.new _) progress (by scalar_tac))
      obtain ⟨result, hresult, hr⟩ := extend_values_equiv clone singleton rest hclone
        (by simp only [hs, alloc.vec.Vec.new, List.nil_append, List.length_singleton]; omega)
      refine ⟨result, by simp [slide_queue.prepend_progress, hclone, hsingleton, hresult], ?_⟩
      simpa [hs] using hr

end Verification.SlideQueueProof
