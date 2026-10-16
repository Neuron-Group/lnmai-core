import Verification.StoragePublic
import Verification.VecEquality

namespace Verification.StorageProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open VecEquality

/-- Storage equality compares the entire backing vector, including values
    beyond the usual eight button slots and all unequal-length cases. -/
theorem button_eq_equiv {T : Type} [DecidableEq T]
    (eqInst : core.cmp.PartialEq T T) (a b : storage.ButtonVec T)
    (heq : ∀ x y, eqInst.eq x y = ok (decide (x = y))) :
    ∃ result, storage.ButtonVec.Insts.CoreCmpPartialEqButtonVec.eq eqInst a b = ok result ∧
      (result = true ↔ toLnmButtonVec a = toLnmButtonVec b) := by
  refine ⟨decide (a.data.val = b.data.val), ?_, ?_⟩
  · simpa only [storage.ButtonVec.Insts.CoreCmpPartialEqButtonVec.eq, List.map_id] using
      vec_eq_equiv eqInst id heq a.data b.data
  · simp only [decide_eq_true_eq, toLnmButtonVec, LnmaiCore.ButtonVec.mk.injEq]

/-- Sensor storage equality likewise preserves the exact list representation;
    it does not truncate the comparison to the 33 physical sensor positions. -/
theorem sensor_eq_equiv {T : Type} [DecidableEq T]
    (eqInst : core.cmp.PartialEq T T) (a b : storage.SensorVec T)
    (heq : ∀ x y, eqInst.eq x y = ok (decide (x = y))) :
    ∃ result, storage.SensorVec.Insts.CoreCmpPartialEqSensorVec.eq eqInst a b = ok result ∧
      (result = true ↔ toLnmSensorVec a = toLnmSensorVec b) := by
  refine ⟨decide (a.data.val = b.data.val), ?_, ?_⟩
  · simpa only [storage.SensorVec.Insts.CoreCmpPartialEqSensorVec.eq, List.map_id] using
      vec_eq_equiv eqInst id heq a.data b.data
  · simp only [decide_eq_true_eq, toLnmSensorVec, LnmaiCore.SensorVec.mk.injEq]

end Verification.StorageProof
