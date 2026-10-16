import Verification.SlideArea

namespace Verification.SlideAreaProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open StorageProof

/-- Compose the actual extracted checkpoint operation over a finite input
    trace. This does not model the scheduler that chooses when to call it. -/
def checkFrames {P : Type} (clone : core.clone.Clone P) :
    lifecycle.SlideArea P → List (storage.SensorVec Bool) → Result (lifecycle.SlideArea P)
  | area, [] => ok area
  | area, held :: frames => do
      let next ← lifecycle.SlideArea.check clone area held
      checkFrames clone next frames

/-- Every finite sequence of checkpoint updates refines the reference fold,
    including traces starting from malformed or legacy histories. -/
theorem check_frames_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (area : lifecycle.SlideArea P)
    (frames : List (storage.SensorVec Bool)) (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, checkFrames clone area frames = ok result ∧
      toLnmSlideArea progress result =
        (frames.map toLnmSensorVec).foldl LnmaiCore.Lifecycle.SlideArea.check
          (toLnmSlideArea progress area) := by
  induction frames generalizing area with
  | nil => exact ⟨area, rfl, rfl⟩
  | cons held frames ih =>
      obtain ⟨next, hnext, hn⟩ := check_equiv clone progress area held hclone
      obtain ⟨result, hresult, hr⟩ := ih next
      refine ⟨result, ?_, ?_⟩
      · simp only [checkFrames, hnext, bind_tc_ok, hresult]
      · simpa only [List.map_cons, List.foldl_cons, hn] using hr

/-- Public completion and press observations agree after every finite trace. -/
theorem check_frames_observations_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (area : lifecycle.SlideArea P)
    (frames : List (storage.SensorVec Bool)) (hclone : ∀ x, clone.clone x = ok x) :
    let reference := (frames.map toLnmSensorVec).foldl LnmaiCore.Lifecycle.SlideArea.check
      (toLnmSlideArea progress area)
    ∃ result, checkFrames clone area frames = ok result ∧
      lifecycle.SlideArea.on result = ok reference.on ∧
      lifecycle.SlideArea.is_finished result = ok reference.isFinished := by
  obtain ⟨result, hresult, hr⟩ := check_frames_equiv clone progress area frames hclone
  refine ⟨result, hresult, ?_, ?_⟩
  · rw [on_equiv progress, hr]
  · rw [is_finished_equiv progress, hr]

end Verification.SlideAreaProof
