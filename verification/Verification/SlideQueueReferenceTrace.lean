import Verification.SlideQueueTrace

namespace Verification.SlideQueueProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open StorageProof
open private slideQueueCore slideHideBarCmd from LnmaiCore.Lifecycle

/-- An actual successful call cannot grow the remaining checkpoint queue. -/
theorem advance_output_bound {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (emit : Bool) (queue : Slice (lifecycle.SlideArea P))
    (held : storage.SensorVec Bool)
    (out : alloc.vec.Vec (lifecycle.SlideArea P) × alloc.vec.Vec P)
    (hclone : ∀ x, clone.clone x = ok x) (hbound : queue.val.length < Std.Usize.max)
    (hout : slide_queue.advance clone emit queue held = ok out) :
    out.1.val.length ≤ queue.val.length := by
  obtain ⟨next, hnext, heq⟩ := advance_equiv clone progress emit queue held hclone hbound
  have hsame : next = out := by
    rw [hout] at hnext
    exact (Result.ok.inj hnext).symm
  subst next
  exact (result_bounds progress (queue.val.length + 1) emit queue held out heq).1

/-- Record the remaining state and emissions after every actual extracted call.
    Frames with no emissions still contribute an observation. -/
def traceFrames {P : Type} (clone : core.clone.Clone P) (emit : Bool) :
    Slice (lifecycle.SlideArea P) → List (storage.SensorVec Bool) →
      Result (List (alloc.vec.Vec (lifecycle.SlideArea P) × alloc.vec.Vec P))
  | _, [] => ok []
  | queue, held :: frames => do
      let next ← slide_queue.advance clone emit queue held
      let trace ← traceFrames clone emit next.1 frames
      ok (next :: trace)

/-- Apply the original Lean queue function at each frame, with its original
    note/track routing and command order. This is not the factored queue model. -/
def referenceFrames (note : Nat) (track : Option Nat) (emit : Bool) :
    LnmaiCore.Lifecycle.SlideQueue → List (LnmaiCore.SensorVec Bool) →
      List (LnmaiCore.Lifecycle.SlideQueue × List LnmaiCore.RenderCommand)
  | _, [] => []
  | queue, held :: frames =>
      let next := slideQueueCore note track emit queue held
      next :: referenceFrames note track emit next.1 frames

/-- Intermediate checkpoint states and complete routed command lists agree
    at every frame, including empty frames and independently sized histories.
    The scheduler's choice of input snapshots is a separate obligation. -/
theorem trace_frames_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (emit : Bool) (queue : Slice (lifecycle.SlideArea P))
    (frames : List (storage.SensorVec Bool)) (note : Nat) (track : Option Nat)
    (hclone : ∀ x, clone.clone x = ok x) (hbound : queue.val.length < Std.Usize.max) :
    ∃ trace, traceFrames clone emit queue frames = ok trace ∧
      trace.map (fun out => (toLnmQueue progress out.1,
        out.2.val.map (fun p => slideHideBarCmd note track (progress p)))) =
        referenceFrames note track emit (toLnmQueue progress queue)
          (frames.map toLnmSensorVec) := by
  induction frames generalizing queue with
  | nil => exact ⟨[], rfl, rfl⟩
  | cons held frames ih =>
      obtain ⟨next, hnext, href⟩ :=
        advance_render_equiv clone progress emit queue held note track hclone hbound
      have hb := advance_output_bound clone progress emit queue held next hclone hbound hnext
      obtain ⟨trace, htrace, heq⟩ := ih next.1 (by omega)
      refine ⟨next :: trace, ?_, ?_⟩
      · simp only [traceFrames, hnext, bind_tc_ok, htrace]
      · simp only [List.map_cons, referenceFrames]
        rw [← href]
        exact congrArg (List.cons _) heq

/-- Reference replay states after each frame, using the public Lean operation. -/
def referenceReplayFrames :
    LnmaiCore.Lifecycle.SlideQueue → List (LnmaiCore.SensorVec Bool) →
      List LnmaiCore.Lifecycle.SlideQueue
  | _, [] => []
  | queue, held :: frames =>
      let next := LnmaiCore.Lifecycle.replaySlideQueue queue held
      next :: referenceReplayFrames next frames

/-- Replay preserves every intermediate state and suppresses commands in
    every frame, rather than merely returning an empty final command list. -/
theorem trace_frames_replay_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (queue : Slice (lifecycle.SlideArea P))
    (frames : List (storage.SensorVec Bool))
    (hclone : ∀ x, clone.clone x = ok x) (hbound : queue.val.length < Std.Usize.max) :
    ∃ trace, traceFrames clone false queue frames = ok trace ∧
      trace.map (fun out => toLnmQueue progress out.1) =
        referenceReplayFrames (toLnmQueue progress queue) (frames.map toLnmSensorVec) ∧
      ∀ out ∈ trace, out.2.val = [] := by
  induction frames generalizing queue with
  | nil => exact ⟨[], rfl, rfl, by simp⟩
  | cons held frames ih =>
      obtain ⟨next, hnext, href, hempty⟩ :=
        advance_replay_equiv clone progress queue held hclone hbound
      have hb := advance_output_bound clone progress false queue held next hclone hbound hnext
      obtain ⟨trace, htrace, heq, hemptyTrace⟩ := ih next.1 (by omega)
      refine ⟨next :: trace, ?_, ?_, ?_⟩
      · simp only [traceFrames, hnext, bind_tc_ok, htrace]
      · simp only [List.map_cons, referenceReplayFrames, ← href]
        exact congrArg (List.cons _) heq
      · simpa only [List.mem_cons, forall_eq_or_imp] using And.intro hempty hemptyTrace

end Verification.SlideQueueProof
