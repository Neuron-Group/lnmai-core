import Verification.Rotations
import LnmaiCore.Simai.Symmetry
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify
open Verification.Areas
set_option maxHeartbeats 1000000
namespace Verification.SymmetryProof

open private LnmaiCore.rotateRingIndex LnmaiCore.rotateRingSensor from LnmaiCore.Areas

def toLnmSymmetry : symmetry.SlideSymmetry → LnmaiCore.Simai.SlideSymmetry
  | .R k => .r (⟨k.val % 8, Nat.mod_lt _ (by decide)⟩ : Fin 8)
  | .Sr k => .sr (⟨k.val % 8, Nat.mod_lt _ (by decide)⟩ : Fin 8)

theorem direct_equiv :
    (do let g ← symmetry.direct; ok (toLnmSymmetry g)) =
      ok LnmaiCore.Simai.SlideSymmetry.direct := rfl

theorem mirror_equiv :
    (do let g ← symmetry.mirror; ok (toLnmSymmetry g)) =
      ok LnmaiCore.Simai.SlideSymmetry.mirror := rfl

theorem clone_equiv (g : symmetry.SlideSymmetry) :
    symmetry.SlideSymmetry.Insts.CoreCloneClone.clone g = ok g := rfl

theorem is_mirrored_equiv (g : symmetry.SlideSymmetry) :
    symmetry.is_mirrored g =
      ok (LnmaiCore.Simai.SlideSymmetry.isMirrored (toLnmSymmetry g)) := by
  cases g <;> rfl

@[step] theorem rotation_steps_spec (g : symmetry.SlideSymmetry) :
    symmetry.rotation_steps g ⦃ r =>
      r.val = LnmaiCore.Simai.SlideSymmetry.rotationSteps (toLnmSymmetry g) ⦄ := by
  match g with
  | .R k => exact UScalar.rem_spec k (y := 8#u32) (by decide)
  | .Sr k => exact UScalar.rem_spec k (y := 8#u32) (by decide)

theorem rotation_steps_equiv (g : symmetry.SlideSymmetry) :
    (do let r ← symmetry.rotation_steps g; ok r.val) =
      ok (LnmaiCore.Simai.SlideSymmetry.rotationSteps (toLnmSymmetry g)) := by
  obtain ⟨r, hr, hv⟩ := WP.spec_imp_exists (rotation_steps_spec g)
  simp [hr, hv]

theorem sensor_rotate_mod (area : LnmaiCore.SensorArea) (n : Nat) :
    area.rotate n = area.rotate (n % 8) := by
  cases area <;> simp [LnmaiCore.SensorArea.rotate, LnmaiCore.rotateRingSensor,
    LnmaiCore.rotateRingIndex, Nat.add_mod]

theorem partial_eq_equiv (g h : symmetry.SlideSymmetry) :
    symmetry.SlideSymmetry.Insts.CoreCmpPartialEqSlideSymmetry.eq g h =
      ok (decide (toLnmSymmetry g = toLnmSymmetry h)) := by
  obtain ⟨rg, hrg, hvg⟩ := WP.spec_imp_exists (rotation_steps_spec g)
  obtain ⟨rh, hrh, hvh⟩ := WP.spec_imp_exists (rotation_steps_spec h)
  unfold symmetry.SlideSymmetry.Insts.CoreCmpPartialEqSlideSymmetry.eq
  rw [is_mirrored_equiv, is_mirrored_equiv]
  cases g <;> cases h <;>
    simp_all [toLnmSymmetry, LnmaiCore.Simai.SlideSymmetry.isMirrored,
      LnmaiCore.Simai.SlideSymmetry.rotationSteps, UScalar.eq_equiv, ZMod.val]
  all_goals change _ ↔ ((⟨_, _⟩ : Fin 8) = ⟨_, _⟩)
  all_goals constructor
  all_goals intro he
  case R.R.mp => exact Fin.ext he
  case R.R.mpr => exact congrArg (fun x : Fin 8 => x.val) he
  case Sr.Sr.mp => exact Fin.ext he
  case Sr.Sr.mpr => exact congrArg (fun x : Fin 8 => x.val) he

theorem act_on_sensor_area_equiv (g : symmetry.SlideSymmetry) (area : areas.SensorArea) :
    symmetry.act_on_sensor_area g area =
      ok (ofLnmSensorArea (LnmaiCore.Simai.actOnSensorArea (toLnmSymmetry g)
        (toLnmSensorArea area))) := by
  cases g with
  | R k =>
    simp only [symmetry.act_on_sensor_area, lift, bind_tc_ok, sensorArea_rotate_equiv,
      U32.cast_Usize_val_eq]
    simpa only [toLnmSymmetry, LnmaiCore.Simai.actOnSensorArea] using
      congrArg (fun a => ok (ofLnmSensorArea a)) (sensor_rotate_mod (toLnmSensorArea area) k.val)
  | Sr k =>
    cases area <;> simp only [symmetry.act_on_sensor_area, symmetry.reflect, lift, bind_tc_ok,
      sensorArea_rotate_equiv, U32.cast_Usize_val_eq, toLnmSymmetry,
      LnmaiCore.Simai.actOnSensorArea, toLnmSensorArea, ZMod.val]
    all_goals exact congrArg (fun a => ok (ofLnmSensorArea a)) (sensor_rotate_mod _ k.val)

end Verification.SymmetryProof
