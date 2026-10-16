import Verification.SlideQueueTraversal

namespace Verification.SlideQueueProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open StorageProof

/-- Compose the actual extracted queue entry point over sensor snapshots.
    Keep frame boundaries in the command trace as well as command order. -/
def advanceFrames {P : Type} (clone : core.clone.Clone P) (emit : Bool) :
    Slice (lifecycle.SlideArea P) → List (storage.SensorVec Bool) →
      Result (alloc.vec.Vec (lifecycle.SlideArea P) × List (alloc.vec.Vec P))
  | queue, [] => ok (queue, [])
  | queue, held :: frames => do
      let next ← slide_queue.advance clone emit queue held
      let out ← advanceFrames clone emit next.1 frames
      ok (out.1, next.2 :: out.2)

def advanceModelFrames (emit : Bool) :
    LnmaiCore.Lifecycle.SlideQueue → List (LnmaiCore.SensorVec Bool) →
      LnmaiCore.Lifecycle.SlideQueue × List (List Nat)
  | queue, [] => (queue, [])
  | queue, held :: frames =>
      let (next, commands) := advanceModel (queue.length + 1) emit queue held
      let (remaining, trace) := advanceModelFrames emit next frames
      (remaining, commands :: trace)

/-- Every finite sequence of actual queue calls preserves remaining state,
    per-frame emission boundaries and progress order. This statement does not
    specify the scheduler that selects the snapshots or calls. -/
theorem advance_frames_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (emit : Bool) (queue : Slice (lifecycle.SlideArea P))
    (frames : List (storage.SensorVec Bool)) (hclone : ∀ x, clone.clone x = ok x)
    (hbound : queue.val.length < Std.Usize.max) :
    ∃ out, advanceFrames clone emit queue frames = ok out ∧
      (toLnmQueue progress out.1, out.2.map (fun commands => commands.val.map progress)) =
        advanceModelFrames emit (toLnmQueue progress queue) (frames.map toLnmSensorVec) := by
  induction frames generalizing queue with
  | nil => exact ⟨(queue, []), rfl, rfl⟩
  | cons held frames ih =>
      obtain ⟨next, hnext, hn⟩ := advance_equiv clone progress emit queue held hclone hbound
      have hb := (result_bounds progress (queue.val.length + 1) emit queue held next hn).1
      obtain ⟨out, hout, heq⟩ := ih next.1 (by omega)
      refine ⟨(out.1, next.2 :: out.2), ?_, ?_⟩
      · simp only [advanceFrames, hnext, bind_tc_ok, hout]
      · simp only [List.map_cons, advanceModelFrames]
        have hlength : (toLnmQueue progress queue).length = queue.val.length := by
          simp only [toLnmQueue, List.length_map]
        rw [hlength, ← hn]
        exact congrArg (fun pair : LnmaiCore.Lifecycle.SlideQueue × List (List Nat) =>
          (pair.1, next.2.val.map progress :: pair.2)) heq

end Verification.SlideQueueProof
