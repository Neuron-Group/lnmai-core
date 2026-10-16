import Verification.SlideArea
import Verification.VecEquality
import Verification.AreasPublic
import Verification.TypesPublic

namespace Verification.SlideAreaProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open Areas Bridge VecEquality

/-- Derived Rust equality compares every represented checkpoint field,
    including both independently sized history vectors. Progress equality
    must agree with equality of its natural-number representation. -/
theorem eq_equiv {P : Type} (eqInst : core.cmp.PartialEq P P)
    (progress : P → Nat) (a b : lifecycle.SlideArea P)
    (heq : ∀ x y, eqInst.eq x y = ok (decide (progress x = progress y))) :
    ∃ result, lifecycle.SlideArea.Insts.CoreCmpPartialEqSlideArea.eq eqInst a b = ok result ∧
      (result = true ↔ toLnmSlideArea progress a = toLnmSlideArea progress b) := by
  classical
  have hareas := vec_eq_equiv areas.SensorArea.Insts.CoreCmpPartialEqSensorArea
    Areas.toLnmSensorArea (by
      intro x y
      have hbeq : (Areas.toLnmSensorArea x == Areas.toLnmSensorArea y) =
          decide (Areas.toLnmSensorArea x = Areas.toLnmSensorArea y) := by
        cases x <;> cases y <;> rfl
      simpa only [hbeq] using sensorArea_eq_equiv x y)
  have hbools := vec_eq_equiv core.cmp.PartialEqBool (id : Bool → Bool)
    (by intro x y; rfl)
  refine ⟨decide (toLnmSlideArea progress a = toLnmSlideArea progress b), ?_, by simp⟩
  simp only [lifecycle.SlideArea.Insts.CoreCmpPartialEqSlideArea.eq,
    hareas, hbools, TypesProof.areaPolicy_eq_equiv, heq, bind_tc_ok, Bool.decide_coe,
    List.map_id, toLnmSlideArea, LnmaiCore.Lifecycle.SlideArea.mk.injEq]
  split_ifs <;> simp_all

end Verification.SlideAreaProof
