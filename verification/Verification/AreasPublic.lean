import Verification.Symmetry

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 4000000

namespace Verification.Areas
open private LnmaiCore.rotateRingIndex LnmaiCore.rotateRingButton from LnmaiCore.Areas

/- Public finite interfaces from the extracted Rust kernel. Strings and the
   runtime Natural representation are separate obligations. -/

theorem sensorArea_default_equiv :
    (do let a ← areas.SensorArea.Insts.CoreDefaultDefault.default
        ok (toLnmSensorArea a)) = ok (default : LnmaiCore.SensorArea) := rfl

theorem sensorArea_all_equiv :
    ∃ a : Std.Array areas.SensorArea 33#usize,
      areas.SensorArea.all = ok a ∧
      a.val.map toLnmSensorArea = LnmaiCore.SensorArea.all := by
  exact ⟨_, rfl, rfl⟩

theorem sensorArea_clone_equiv (a : areas.SensorArea) :
    areas.SensorArea.Insts.CoreCloneClone.clone a = ok a := rfl

theorem sensorArea_eq_equiv (a b : areas.SensorArea) :
    areas.SensorArea.Insts.CoreCmpPartialEqSensorArea.eq a b =
      ok (toLnmSensorArea a == toLnmSensorArea b) := by
  cases a <;> cases b <;>
    simp [areas.SensorArea.Insts.CoreCmpPartialEqSensorArea.eq,
      areas.SensorArea.read_discriminant, toLnmSensorArea] <;> rfl

theorem sensorArea_cmp_equiv (a b : areas.SensorArea) :
    areas.SensorArea.Insts.CoreCmpOrd.cmp a b =
      ok (compare (toLnmSensorArea a) (toLnmSensorArea b)) := by
  cases a <;> cases b <;>
    simp [areas.SensorArea.Insts.CoreCmpOrd.cmp,
      areas.SensorArea.read_discriminant, toLnmSensorArea] <;> rfl

theorem sensorArea_partial_cmp_equiv (a b : areas.SensorArea) :
    areas.SensorArea.Insts.CoreCmpPartialOrdSensorArea.partial_cmp a b =
      ok (some (compare (toLnmSensorArea a) (toLnmSensorArea b))) := by
  simp only [areas.SensorArea.Insts.CoreCmpPartialOrdSensorArea.partial_cmp,
    sensorArea_cmp_equiv, bind_tc_ok]

theorem buttonZone_default_equiv :
    (do let a ← areas.ButtonZone.Insts.CoreDefaultDefault.default
        ok (toLnmButtonZone a)) = ok (default : LnmaiCore.ButtonZone) := rfl

theorem buttonZone_all_equiv :
    ∃ a : Std.Array areas.ButtonZone 8#usize,
      areas.ButtonZone.all = ok a ∧
      a.val.map toLnmButtonZone = LnmaiCore.ButtonZone.all := by
  exact ⟨_, rfl, rfl⟩

theorem buttonZone_clone_equiv (a : areas.ButtonZone) :
    areas.ButtonZone.Insts.CoreCloneClone.clone a = ok a := rfl

theorem buttonZone_eq_equiv (a b : areas.ButtonZone) :
    areas.ButtonZone.Insts.CoreCmpPartialEqButtonZone.eq a b =
      ok (toLnmButtonZone a == toLnmButtonZone b) := by
  cases a <;> cases b <;>
    simp [areas.ButtonZone.Insts.CoreCmpPartialEqButtonZone.eq,
      areas.ButtonZone.read_discriminant, toLnmButtonZone] <;> rfl

theorem buttonZone_cmp_equiv (a b : areas.ButtonZone) :
    areas.ButtonZone.Insts.CoreCmpOrd.cmp a b =
      ok (compare (toLnmButtonZone a) (toLnmButtonZone b)) := by
  cases a <;> cases b <;>
    simp [areas.ButtonZone.Insts.CoreCmpOrd.cmp,
      areas.ButtonZone.read_discriminant, toLnmButtonZone] <;> rfl

theorem buttonZone_partial_cmp_equiv (a b : areas.ButtonZone) :
    areas.ButtonZone.Insts.CoreCmpPartialOrdButtonZone.partial_cmp a b =
      ok (some (compare (toLnmButtonZone a) (toLnmButtonZone b))) := by
  simp only [areas.ButtonZone.Insts.CoreCmpPartialOrdButtonZone.partial_cmp,
    buttonZone_cmp_equiv, bind_tc_ok]

theorem buttonZone_storage_order_equiv :
    ∃ a : Std.Array areas.ButtonZone 8#usize,
      areas.ButtonZone.storage_order = ok a ∧
      a.val.map toLnmButtonZone = LnmaiCore.ButtonZone.storageOrder := by
  exact ⟨_, rfl, rfl⟩

theorem outerSlot_default_equiv :
    (do let a ← areas.OuterSlot.Insts.CoreDefaultDefault.default
        ok (toLnmOuterSlot a)) = ok (default : LnmaiCore.OuterSlot) := rfl

theorem outerSlot_all_equiv :
    ∃ a : Std.Array areas.OuterSlot 8#usize,
      areas.OuterSlot.all = ok a ∧
      a.val.map toLnmOuterSlot = LnmaiCore.OuterSlot.all := by
  exact ⟨_, rfl, rfl⟩

theorem outerSlot_clone_equiv (a : areas.OuterSlot) :
    areas.OuterSlot.Insts.CoreCloneClone.clone a = ok a := rfl

theorem outerSlot_eq_equiv (a b : areas.OuterSlot) :
    areas.OuterSlot.Insts.CoreCmpPartialEqOuterSlot.eq a b =
      ok (toLnmOuterSlot a == toLnmOuterSlot b) := by
  cases a <;> cases b <;>
    simp [areas.OuterSlot.Insts.CoreCmpPartialEqOuterSlot.eq,
      areas.OuterSlot.read_discriminant, toLnmOuterSlot] <;> rfl

theorem outerSlot_cmp_equiv (a b : areas.OuterSlot) :
    areas.OuterSlot.Insts.CoreCmpOrd.cmp a b =
      ok (compare (toLnmOuterSlot a) (toLnmOuterSlot b)) := by
  cases a <;> cases b <;>
    simp [areas.OuterSlot.Insts.CoreCmpOrd.cmp,
      areas.OuterSlot.read_discriminant, toLnmOuterSlot] <;> rfl

theorem outerSlot_partial_cmp_equiv (a b : areas.OuterSlot) :
    areas.OuterSlot.Insts.CoreCmpPartialOrdOuterSlot.partial_cmp a b =
      ok (some (compare (toLnmOuterSlot a) (toLnmOuterSlot b))) := by
  simp only [areas.OuterSlot.Insts.CoreCmpPartialOrdOuterSlot.partial_cmp,
    outerSlot_cmp_equiv, bind_tc_ok]

theorem outerSlot_storage_order_equiv :
    ∃ a : Std.Array areas.OuterSlot 8#usize,
      areas.OuterSlot.storage_order = ok a ∧
      a.val.map toLnmOuterSlot = LnmaiCore.OuterSlot.storageOrder := by
  exact ⟨_, rfl, rfl⟩

theorem button_rotate_mod (area : LnmaiCore.ButtonZone) (n : Nat) :
    area.rotate n = area.rotate (n % 8) := by
  cases area <;> simp [LnmaiCore.ButtonZone.rotate, LnmaiCore.rotateRingButton,
    LnmaiCore.rotateRingIndex, Nat.add_mod]

theorem outer_rotate_mod (area : LnmaiCore.OuterSlot) (n : Nat) :
    area.rotate n = area.rotate (n % 8) := by
  cases area <;> simp [LnmaiCore.OuterSlot.rotate, Nat.add_mod]

/- These composition theorems require the runtime adapter to supply the
   exact residue. They do not assume or prove a bigint implementation. -/
theorem sensorArea_rotate_unbounded_equiv
    (a : areas.SensorArea) (n : Nat) (r : Std.Usize) (hr : r.val = n % 8) :
    areas.SensorArea.rotate a r =
      ok (ofLnmSensorArea (LnmaiCore.SensorArea.rotate n (toLnmSensorArea a))) := by
  rw [sensorArea_rotate_equiv, hr, ← SymmetryProof.sensor_rotate_mod]

theorem buttonZone_rotate_unbounded_equiv
    (a : areas.ButtonZone) (n : Nat) (r : Std.Usize) (hr : r.val = n % 8) :
    areas.ButtonZone.rotate a r =
      ok (ofLnmButtonZone (LnmaiCore.ButtonZone.rotate n (toLnmButtonZone a))) := by
  rw [buttonZone_rotate_equiv, hr, ← button_rotate_mod]

theorem outerSlot_rotate_unbounded_equiv
    (a : areas.OuterSlot) (n : Nat) (r : Std.Usize) (hr : r.val = n % 8) :
    areas.OuterSlot.rotate a r =
      ok (ofLnmOuterSlot (LnmaiCore.OuterSlot.rotate n (toLnmOuterSlot a))) := by
  rw [outerSlot_rotate_equiv, hr, ← outer_rotate_mod]

theorem buttonZone_of_index_large (n : Nat) (hn : 8 ≤ n) :
    LnmaiCore.ButtonZone.ofIndex? n = none := by
  unfold LnmaiCore.ButtonZone.ofIndex?
  split <;> first | rfl | omega

theorem buttonZone_of_unbounded_index_equiv
    (n : Nat) (index : Option Std.Usize)
    (hindex : match index with | some i => i.val = n | none => 8 ≤ n) :
    (match index with
      | some i => areas.ButtonZone.of_index i
      | none => ok none) =
      ok ((LnmaiCore.ButtonZone.ofIndex? n).map ofLnmButtonZone) := by
  cases index with
  | none => simp [buttonZone_of_index_large n hindex]
  | some i => simpa only [hindex] using buttonZone_of_index_equiv i

theorem outerSlot_of_index_large (n : Nat) (hn : 8 ≤ n) :
    LnmaiCore.OuterSlot.ofIndex? n = none := by
  unfold LnmaiCore.OuterSlot.ofIndex?
  split <;> first | rfl | omega

theorem outerSlot_of_unbounded_index_equiv
    (n : Nat) (index : Option Std.Usize)
    (hindex : match index with | some i => i.val = n | none => 8 ≤ n) :
    (match index with
      | some i => areas.OuterSlot.of_index i
      | none => ok none) =
      ok ((LnmaiCore.OuterSlot.ofIndex? n).map ofLnmOuterSlot) := by
  cases index with
  | none => simp [outerSlot_of_index_large n hindex]
  | some i => simpa only [hindex] using outerSlot_of_index_equiv i

end Verification.Areas

