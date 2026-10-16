import Verification.Shape

open Aeneas Aeneas.Std Result lnmai_core_verify
open Verification.Areas Verification.SymmetryProof

set_option maxHeartbeats 1000000
namespace Verification.ShapeProof

open private LnmaiCore.Simai.relativeEndPos LnmaiCore.Simai.relativeEndFromTyped
  LnmaiCore.Simai.directShapeSymmetry LnmaiCore.Simai.mirroredShapeSymmetry
  LnmaiCore.Simai.outerSlotIsUpperHalf
  from LnmaiCore.Simai.Shape

theorem exists_of_project_eq {α β : Type} (m : Result α) (f : α → β) (b : β)
    (h : (do let a ← m; ok (f a)) = ok b) :
    ∃ a, m = ok a ∧ f a = b := by
  cases hm : m <;> simp_all

theorem relative_end_from_typed_equiv (s : areas.OuterSlot) (a : areas.SensorArea)
    (raw : String) :
    (do let r ← shape.relative_end_from_typed s a; ok (toLnmResult UScalar.val raw r)) =
      ok (LnmaiCore.Simai.relativeEndFromTyped (toLnmOuterSlot s) (toLnmSensorArea a)) := by
  unfold shape.relative_end_from_typed
  rw [sensorArea_to_outer_slot_equiv]
  unfold LnmaiCore.Simai.relativeEndFromTyped
  cases ho : (toLnmSensorArea a).toOuterSlot? with
  | none => simp [toLnmResult, toLnmError]
  | some e =>
    obtain ⟨si, hsi, hvsi⟩ := outerSlot_to_index_equiv s
    obtain ⟨ei, hei, hvei⟩ := outerSlot_to_index_equiv (ofLnmOuterSlot e)
    have hs : si.val ≤ 7 := by rw [hvsi]; cases s <;> decide
    have he : ei.val ≤ 7 := by rw [hvei]; cases e <;> decide
    obtain ⟨sp, hsp, hvsp⟩ := WP.spec_imp_exists
      (UScalar.add_spec (x := si) (y := 1#usize) (by scalar_tac))
    obtain ⟨ep, hep, hvep⟩ := WP.spec_imp_exists
      (UScalar.add_spec (x := ei) (y := 1#usize) (by scalar_tac))
    obtain ⟨r, hr, hvr⟩ := WP.spec_imp_exists
      (relative_end_pos_spec sp ep (by scalar_tac) (by scalar_tac))
    simp [hsi, hei, hsp, hep, hr, toLnmResult, hvr, hvsp, hvep, hvsi, hvei,
      pure, Except.pure]

theorem relative_end_from_typed_spec (s : areas.OuterSlot) (a : areas.SensorArea)
    (raw : String) :
    ∃ r, shape.relative_end_from_typed s a = ok r ∧
      toLnmResult UScalar.val raw r =
        LnmaiCore.Simai.relativeEndFromTyped (toLnmOuterSlot s) (toLnmSensorArea a) :=
  exists_of_project_eq _ _ _ (relative_end_from_typed_equiv s a raw)

theorem mk_canonical_slide_shape_spec (k : shape.SlideKind) (n : Option Usize)
    (raw : String) :
    ∃ r, shape.mk_canonical_slide_shape k n = ok r ∧
      toLnmResult toLnmCanonical raw r =
        LnmaiCore.Simai.mkCanonicalSlideShape (toLnmKind k) (n.map UScalar.val)
          raw "invalid end" := by
  obtain ⟨c, hc, he⟩ := exists_of_project_eq _ _ _ (mk_canonical_slide_shape_opt_equiv k n)
  unfold shape.mk_canonical_slide_shape LnmaiCore.Simai.mkCanonicalSlideShape
  rw [hc, ← he]
  cases c <;> simp [toLnmResult, toLnmError, pure, Except.pure]

theorem sensor_option_getD_equiv (a : Option areas.SensorArea) :
    (a.map toLnmSensorArea).getD .A1 = toLnmSensorArea (a.getD .A1) := by
  cases a <;> rfl

theorem option_unwrap_or_eq_getD {α : Type} (a : Option α) (d : α) :
    core.option.Option.unwrap_or a d = a.getD d := by
  cases a <;> rfl

/-- The extracted runtime solver agrees with the actual Lean solver for every
    body kind, lane, optional endpoint/turn area, and diagnostic raw text. -/
theorem solve_shape_equiv (k : shape.SlideBodyKind) (s : areas.OuterSlot)
    (e t : Option areas.SensorArea) (raw : String) :
    (do let r ← shape.solve_shape k s e t; ok (toLnmResult toLnmShape raw r)) =
      ok (LnmaiCore.Simai.solveSlideShape
        { rawText := raw, startLane := toLnmOuterSlot s, kind := toLnmBodyKind k,
          endArea := e.map toLnmSensorArea, turnArea := t.map toLnmSensorArea }) := by
  obtain ⟨re, hre, he⟩ := relative_end_from_typed_spec s (e.getD .A1) raw
  obtain ⟨rt, hrt, ht⟩ := relative_end_from_typed_spec s (t.getD .A1) raw
  cases k <;>
    simp only [shape.solve_shape, LnmaiCore.Simai.solveSlideShape, toLnmBodyKind,
      option_unwrap_or_eq_getD, lift, bind_tc_ok, symmetry.direct, symmetry.mirror,
      sensor_option_getD_equiv, hre, hrt, ← he, ← ht]
  all_goals cases re <;> cases rt <;>
    simp [toLnmResult, shape.mk_canonical_slide_shape, shape.mk_canonical_slide_shape_opt,
      LnmaiCore.Simai.mkCanonicalSlideShape, LnmaiCore.Simai.mkCanonicalSlideShape?,
      shape.mk_slide_shape, LnmaiCore.Simai.mkSlideShape, toLnmShape, toLnmCanonical,
      toLnmError, outer_slot_is_upper_half_equiv, toLnmSymmetry,
      LnmaiCore.Simai.directShapeSymmetry, LnmaiCore.Simai.mirroredShapeSymmetry,
      LnmaiCore.Simai.SlideSymmetry.direct, LnmaiCore.Simai.SlideSymmetry.mirror,
      shape.SlideBodyKind.Insts.CoreCmpPartialEqSlideBodyKind.eq,
      shape.SlideBodyKind.read_discriminant,
      UScalar.eq_equiv, UScalar.lt_equiv, UScalar.le_equiv,
      pure, Except.pure, Bind.bind, Except.bind, Functor.map, Except.map]
  all_goals try split_ifs
  all_goals simp_all [toLnmResult, toLnmShape, toLnmCanonical,
    toLnmError, shape.mk_slide_shape, LnmaiCore.Simai.mkSlideShape, toLnmSymmetry,
    pure, Except.pure, Bind.bind, Except.bind, Functor.map, Except.map]
  all_goals first | rfl | omega

end Verification.ShapeProof
