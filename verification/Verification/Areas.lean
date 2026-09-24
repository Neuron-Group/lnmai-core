-- M1a: equivalence between the Aeneas model of `lnmai-core-verify::areas`
-- and the authoritative Lean module `LnmaiCore/Areas.lean`.
import Verification.Generated
import LnmaiCore.Areas

open Aeneas Aeneas.Std Result

namespace Verification

namespace Areas

open lnmai_core_verify

/-! ## Isomorphisms -/

def toLnmSensorArea : areas.SensorArea → LnmaiCore.SensorArea
  | .A1 => .A1 | .A2 => .A2 | .A3 => .A3 | .A4 => .A4
  | .A5 => .A5 | .A6 => .A6 | .A7 => .A7 | .A8 => .A8
  | .B1 => .B1 | .B2 => .B2 | .B3 => .B3 | .B4 => .B4
  | .B5 => .B5 | .B6 => .B6 | .B7 => .B7 | .B8 => .B8
  | .C => .C
  | .D1 => .D1 | .D2 => .D2 | .D3 => .D3 | .D4 => .D4
  | .D5 => .D5 | .D6 => .D6 | .D7 => .D7 | .D8 => .D8
  | .E1 => .E1 | .E2 => .E2 | .E3 => .E3 | .E4 => .E4
  | .E5 => .E5 | .E6 => .E6 | .E7 => .E7 | .E8 => .E8

def ofLnmSensorArea : LnmaiCore.SensorArea → areas.SensorArea
  | .A1 => .A1 | .A2 => .A2 | .A3 => .A3 | .A4 => .A4
  | .A5 => .A5 | .A6 => .A6 | .A7 => .A7 | .A8 => .A8
  | .B1 => .B1 | .B2 => .B2 | .B3 => .B3 | .B4 => .B4
  | .B5 => .B5 | .B6 => .B6 | .B7 => .B7 | .B8 => .B8
  | .C => .C
  | .D1 => .D1 | .D2 => .D2 | .D3 => .D3 | .D4 => .D4
  | .D5 => .D5 | .D6 => .D6 | .D7 => .D7 | .D8 => .D8
  | .E1 => .E1 | .E2 => .E2 | .E3 => .E3 | .E4 => .E4
  | .E5 => .E5 | .E6 => .E6 | .E7 => .E7 | .E8 => .E8

def toLnmButtonZone : areas.ButtonZone → LnmaiCore.ButtonZone
  | .K1 => .K1 | .K2 => .K2 | .K3 => .K3 | .K4 => .K4
  | .K5 => .K5 | .K6 => .K6 | .K7 => .K7 | .K8 => .K8

def ofLnmButtonZone : LnmaiCore.ButtonZone → areas.ButtonZone
  | .K1 => .K1 | .K2 => .K2 | .K3 => .K3 | .K4 => .K4
  | .K5 => .K5 | .K6 => .K6 | .K7 => .K7 | .K8 => .K8

def toLnmOuterSlot : areas.OuterSlot → LnmaiCore.OuterSlot
  | .S1 => .S1 | .S2 => .S2 | .S3 => .S3 | .S4 => .S4
  | .S5 => .S5 | .S6 => .S6 | .S7 => .S7 | .S8 => .S8

def ofLnmOuterSlot : LnmaiCore.OuterSlot → areas.OuterSlot
  | .S1 => .S1 | .S2 => .S2 | .S3 => .S3 | .S4 => .S4
  | .S5 => .S5 | .S6 => .S6 | .S7 => .S7 | .S8 => .S8

@[simp] theorem ofLnmSensorArea_toLnmSensorArea (a : areas.SensorArea) :
    ofLnmSensorArea (toLnmSensorArea a) = a := by cases a <;> rfl

@[simp] theorem toLnmSensorArea_ofLnmSensorArea (a : LnmaiCore.SensorArea) :
    toLnmSensorArea (ofLnmSensorArea a) = a := by cases a <;> rfl

@[simp] theorem ofLnmButtonZone_toLnmButtonZone (z : areas.ButtonZone) :
    ofLnmButtonZone (toLnmButtonZone z) = z := by cases z <;> rfl

@[simp] theorem toLnmButtonZone_ofLnmButtonZone (z : LnmaiCore.ButtonZone) :
    toLnmButtonZone (ofLnmButtonZone z) = z := by cases z <;> rfl

@[simp] theorem ofLnmOuterSlot_toLnmOuterSlot (s : areas.OuterSlot) :
    ofLnmOuterSlot (toLnmOuterSlot s) = s := by cases s <;> rfl

@[simp] theorem toLnmOuterSlot_ofLnmOuterSlot (s : LnmaiCore.OuterSlot) :
    toLnmOuterSlot (ofLnmOuterSlot s) = s := by cases s <;> rfl

/-! ## Function equivalences -/

theorem buttonZone_to_index_equiv (z : areas.ButtonZone) :
    ∃ v : Std.Usize, areas.ButtonZone.to_index z = ok v ∧
      v.val = LnmaiCore.ButtonZone.toIndex (toLnmButtonZone z) := by
  cases z <;> simp [areas.ButtonZone.to_index, LnmaiCore.ButtonZone.toIndex,
                    toLnmButtonZone]

theorem outerSlot_to_index_equiv (s : areas.OuterSlot) :
    ∃ v : Std.Usize, areas.OuterSlot.to_index s = ok v ∧
      v.val = LnmaiCore.OuterSlot.toIndex (toLnmOuterSlot s) := by
  cases s <;> simp [areas.OuterSlot.to_index, LnmaiCore.OuterSlot.toIndex,
                    toLnmOuterSlot]

theorem buttonZone_of_index_equiv (i : Std.Usize) :
    areas.ButtonZone.of_index i =
      ok ((LnmaiCore.ButtonZone.ofIndex? i.val).map ofLnmButtonZone) := by
  unfold areas.ButtonZone.of_index LnmaiCore.ButtonZone.ofIndex?
  split <;> simp_all [ofLnmButtonZone]

theorem outerSlot_of_index_equiv (i : Std.Usize) :
    areas.OuterSlot.of_index i =
      ok ((LnmaiCore.OuterSlot.ofIndex? i.val).map ofLnmOuterSlot) := by
  unfold areas.OuterSlot.of_index LnmaiCore.OuterSlot.ofIndex?
  split <;> simp_all [ofLnmOuterSlot]

theorem outerSlot_to_button_zone_equiv (s : areas.OuterSlot) :
    areas.OuterSlot.to_button_zone s =
      ok (ofLnmButtonZone (LnmaiCore.OuterSlot.toButtonZone (toLnmOuterSlot s))) := by
  cases s <;> rfl

theorem buttonZone_to_outer_slot_equiv (z : areas.ButtonZone) :
    areas.ButtonZone.to_outer_slot z =
      ok (ofLnmOuterSlot (LnmaiCore.ButtonZone.toOuterSlot (toLnmButtonZone z))) := by
  cases z <;> rfl

theorem outerSlot_to_outer_sensor_area_equiv (s : areas.OuterSlot) :
    areas.OuterSlot.to_outer_sensor_area s =
      ok (ofLnmSensorArea (LnmaiCore.OuterSlot.toOuterSensorArea (toLnmOuterSlot s))) := by
  cases s <;> rfl

theorem buttonZone_to_outer_sensor_area_equiv (z : areas.ButtonZone) :
    areas.ButtonZone.to_outer_sensor_area z =
      ok (ofLnmSensorArea (LnmaiCore.ButtonZone.toOuterSensorArea (toLnmButtonZone z))) := by
  cases z <;> rfl

theorem sensorArea_to_outer_slot_equiv (a : areas.SensorArea) :
    areas.SensorArea.to_outer_slot a =
      ok ((LnmaiCore.SensorArea.toOuterSlot? (toLnmSensorArea a)).map ofLnmOuterSlot) := by
  cases a <;> rfl

theorem sensorArea_to_outer_button_zone_equiv (a : areas.SensorArea) :
    areas.SensorArea.to_outer_button_zone a =
      ok ((LnmaiCore.SensorArea.toOuterButtonZone? (toLnmSensorArea a)).map ofLnmButtonZone) := by
  cases a <;> rfl

end Areas

end Verification
