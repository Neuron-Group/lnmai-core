import Verification.SlideAreaLoops

namespace Verification.SlideAreaProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas StorageProof
open Bridge (toLnmAreaPolicy)
open private slideAreaTargetHistory from LnmaiCore.Lifecycle

def toLnmSlideArea {P : Type} (progress : P → Nat) (area : lifecycle.SlideArea P) :
    LnmaiCore.Lifecycle.SlideArea :=
  { targetAreas := area.target_areas.val.map toLnmSensorArea
    policy := toLnmAreaPolicy area.policy, isLast := area.is_last
    isSkippable := area.is_skippable
    arrowProgressWhenOn := progress area.arrow_progress_when_on
    arrowProgressWhenFinished := progress area.arrow_progress_when_finished
    wasOn := area.was_on, wasOff := area.was_off
    targetWasOn := area.target_was_on.val, targetWasOff := area.target_was_off.val }

theorem default_equiv {P : Type} (defaults : core.default.Default P)
    (progress : P → Nat) (zero : P) (hdefault : defaults.default = ok zero)
    (hzero : progress zero = 0) :
    ∃ result, lifecycle.SlideArea.Insts.CoreDefaultDefault.default defaults = ok result ∧
      toLnmSlideArea progress result = (default : LnmaiCore.Lifecycle.SlideArea) := by
  unfold lifecycle.SlideArea.Insts.CoreDefaultDefault.default
  rw [hdefault]
  refine ⟨_, rfl, ?_⟩
  simp [toLnmSlideArea, toLnmAreaPolicy, hzero]
  rfl

theorem bool_vec_clone_equiv (values : alloc.vec.Vec Bool) :
    alloc.vec.CloneVec.clone core.clone.CloneBool values = ok values :=
  clone_values_equiv core.clone.CloneBool values (by intro x; rfl)

theorem clone_equiv {P : Type} (clone : core.clone.Clone P)
    (area : lifecycle.SlideArea P) (hclone : ∀ x, clone.clone x = ok x) :
    lifecycle.SlideArea.Insts.CoreCloneClone.clone clone area = ok area := by
  have htargets : alloc.vec.CloneVec.clone areas.SensorArea.Insts.CoreCloneClone
      area.target_areas = ok area.target_areas :=
    clone_values_equiv _ _ (by intro x; rfl)
  simp [lifecycle.SlideArea.Insts.CoreCloneClone.clone, htargets, hclone,
    bool_vec_clone_equiv, types.AreaPolicy.Insts.CoreCloneClone.clone,
    core.clone.impls.CloneBool.clone, lift]

theorem on_equiv {P : Type} (progress : P → Nat) (area : lifecycle.SlideArea P) :
    lifecycle.SlideArea.on area = ok (LnmaiCore.Lifecycle.SlideArea.on
      (toLnmSlideArea progress area)) := rfl

theorem history_any_result (history : Slice Bool) :
    lifecycle.history_any history = ok (history.val.any id) := by
  obtain ⟨value, hvalue, hv⟩ := spec_imp_exists (history_any_equiv history)
  simpa [hv] using hvalue

theorem history_finished_result (on off : Slice Bool) (isLast : Bool)
    (policy : types.AreaPolicy) :
    lifecycle.history_finished on off isLast policy =
      ok (combineHistory policy (finishedHistory on.val off.val isLast)) := by
  obtain ⟨value, hvalue, hv⟩ := spec_imp_exists
    (history_finished_equiv on off isLast policy)
  simpa [hv] using hvalue

theorem is_finished_equiv {P : Type} (progress : P → Nat)
    (area : lifecycle.SlideArea P) :
    lifecycle.SlideArea.is_finished area =
      ok (LnmaiCore.Lifecycle.SlideArea.isFinished (toLnmSlideArea progress area)) := by
  unfold lifecycle.SlideArea.is_finished
  simp only [alloc.vec.Vec.deref, history_finished_result]
  by_cases htargets : area.target_areas.val = [] <;>
    by_cases hhistory : area.target_was_on.val = [] <;>
    cases hp : area.policy <;> cases hl : area.is_last <;>
    cases hon : area.was_on <;> cases hoff : area.was_off <;>
    simp [LnmaiCore.Lifecycle.SlideArea.isFinished, toLnmSlideArea,
      toLnmAreaPolicy, htargets, hhistory, hp, hl, hon, hoff, combineHistory, finishedHistory,
      UScalar.eq_equiv]

/-- Legacy flags seed each empty history independently, only for one target. -/
theorem seed_history_equiv (values : alloc.vec.Vec Bool)
    (targets : alloc.vec.Vec areas.SensorArea) (legacy : Bool) :
    ∃ source,
      (if values.len = 0#usize then
        if targets.len = 1#usize then alloc.vec.Vec.push (alloc.vec.Vec.new Bool) legacy
        else alloc.vec.CloneVec.clone core.clone.CloneBool values
       else alloc.vec.CloneVec.clone core.clone.CloneBool values) = ok source ∧
      source.val = (if values.val.isEmpty && targets.val.length == 1 then [legacy] else values.val) := by
  by_cases hv : values.len = 0#usize
  · by_cases ht : targets.len = 1#usize
    · obtain ⟨source, hsource, hs⟩ := spec_imp_exists
        (alloc.vec.Vec.push_spec (alloc.vec.Vec.new Bool) legacy (by scalar_tac))
      refine ⟨source, by simp [hv, ht, hsource], ?_⟩
      simp_all [UScalar.eq_equiv]
    · exact ⟨values, by simp [hv, ht, bool_vec_clone_equiv],
        by simp_all [UScalar.eq_equiv]⟩
  · exact ⟨values, by simp [hv, bool_vec_clone_equiv],
      by simp_all [UScalar.eq_equiv]⟩

/-- Actual extracted checkpoint update. Progress is opaque and cloning must
    preserve it; sensor and history vectors need no alignment invariant. -/
theorem check_equiv {P : Type} (clone : core.clone.Clone P) (progress : P → Nat)
    (area : lifecycle.SlideArea P) (held : storage.SensorVec Bool)
    (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, lifecycle.SlideArea.check clone area held = ok result ∧
      toLnmSlideArea progress result =
        LnmaiCore.Lifecycle.SlideArea.check (toLnmSlideArea progress area)
          (toLnmSensorVec held) := by
  obtain ⟨sourceOn, hsourceOn, hsOn⟩ :=
    seed_history_equiv area.target_was_on area.target_areas area.was_on
  obtain ⟨sourceOff, hsourceOff, hsOff⟩ :=
    seed_history_equiv area.target_was_off area.target_areas area.was_off
  obtain ⟨⟨on, off⟩, hhistory, hh⟩ := spec_imp_exists
    (target_history_equiv area.target_areas sourceOn sourceOff held)
  have hmodel := target_history_model (area.target_areas.val.map toLnmSensorArea)
    sourceOn.val sourceOff.val (toLnmSensorVec held)
  rw [← hmodel] at hh
  have hOn := congrArg Prod.fst hh
  have hOff := congrArg Prod.snd hh
  dsimp only at hOn hOff
  simp only [hsOn, hsOff] at hOn hOff
  clear hh hmodel
  unfold lifecycle.SlideArea.check
  simp only [hsourceOn, hsourceOff, bind_tc_ok, alloc.vec.Vec.deref, hhistory,
    history_any_result, history_finished_result]
  cases hp : area.policy <;> by_cases hempty : area.target_areas.len = 0#usize
  all_goals
    simp [hp, hempty, clone_equiv clone _ hclone]
    simp_all [toLnmSlideArea, LnmaiCore.Lifecycle.SlideArea.check,
      toLnmAreaPolicy, combineHistory, finishedHistory, UScalar.eq_equiv]

end Verification.SlideAreaProof


