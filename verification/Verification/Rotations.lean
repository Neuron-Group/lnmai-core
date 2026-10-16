import Verification.Areas

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 1000000
namespace Verification.Areas

/-- Reducing steps before addition is safe for every machine-size step count
    and agrees with unbounded modular arithmetic. -/
@[step] theorem ring_index_spec (steps n : Std.Usize) (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    areas.ring_index steps n ⦃ r => r.val = ((n.val - 1 + steps.val) % 8) + 1 ⦄ := by
  unfold areas.ring_index
  step* <;> simp_all [Nat.add_mod] <;> scalar_tac

def ringSensorACore (n : Nat) : LnmaiCore.SensorArea :=
  match n with
  | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8

theorem ring_sensor_a_equiv (n : Std.Usize) :
    areas.ring_sensor_a n = ok (ofLnmSensorArea (ringSensorACore n.val)) := by
  unfold areas.ring_sensor_a ringSensorACore
  split <;> simp_all [ofLnmSensorArea]

theorem ring_sensor_a_rotate (steps n : Std.Usize)
    (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    (do let k ← areas.ring_index steps n; areas.ring_sensor_a k) =
      ok (ofLnmSensorArea (ringSensorACore ((n.val - 1 + steps.val) % 8 + 1))) := by
  obtain ⟨k, hk, hv⟩ := WP.spec_imp_exists (ring_index_spec steps n hlo hhi)
  rw [hk]
  simp only [bind_tc_ok, ring_sensor_a_equiv, hv]

def ringSensorBCore (n : Nat) : LnmaiCore.SensorArea :=
  match n with
  | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8

theorem ring_sensor_b_equiv (n : Std.Usize) :
    areas.ring_sensor_b n = ok (ofLnmSensorArea (ringSensorBCore n.val)) := by
  unfold areas.ring_sensor_b ringSensorBCore
  split <;> simp_all [ofLnmSensorArea]

theorem ring_sensor_b_rotate (steps n : Std.Usize)
    (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    (do let k ← areas.ring_index steps n; areas.ring_sensor_b k) =
      ok (ofLnmSensorArea (ringSensorBCore ((n.val - 1 + steps.val) % 8 + 1))) := by
  obtain ⟨k, hk, hv⟩ := WP.spec_imp_exists (ring_index_spec steps n hlo hhi)
  rw [hk]
  simp only [bind_tc_ok, ring_sensor_b_equiv, hv]

def ringSensorDCore (n : Nat) : LnmaiCore.SensorArea :=
  match n with
  | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8

theorem ring_sensor_d_equiv (n : Std.Usize) :
    areas.ring_sensor_d n = ok (ofLnmSensorArea (ringSensorDCore n.val)) := by
  unfold areas.ring_sensor_d ringSensorDCore
  split <;> simp_all [ofLnmSensorArea]

theorem ring_sensor_d_rotate (steps n : Std.Usize)
    (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    (do let k ← areas.ring_index steps n; areas.ring_sensor_d k) =
      ok (ofLnmSensorArea (ringSensorDCore ((n.val - 1 + steps.val) % 8 + 1))) := by
  obtain ⟨k, hk, hv⟩ := WP.spec_imp_exists (ring_index_spec steps n hlo hhi)
  rw [hk]
  simp only [bind_tc_ok, ring_sensor_d_equiv, hv]

def ringSensorECore (n : Nat) : LnmaiCore.SensorArea :=
  match n with
  | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8

theorem ring_sensor_e_equiv (n : Std.Usize) :
    areas.ring_sensor_e n = ok (ofLnmSensorArea (ringSensorECore n.val)) := by
  unfold areas.ring_sensor_e ringSensorECore
  split <;> simp_all [ofLnmSensorArea]

theorem ring_sensor_e_rotate (steps n : Std.Usize)
    (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    (do let k ← areas.ring_index steps n; areas.ring_sensor_e k) =
      ok (ofLnmSensorArea (ringSensorECore ((n.val - 1 + steps.val) % 8 + 1))) := by
  obtain ⟨k, hk, hv⟩ := WP.spec_imp_exists (ring_index_spec steps n hlo hhi)
  rw [hk]
  simp only [bind_tc_ok, ring_sensor_e_equiv, hv]

def ringButtonCore (n : Nat) : LnmaiCore.ButtonZone :=
  match n with
  | 1 => .K1 | 2 => .K2 | 3 => .K3 | 4 => .K4 | 5 => .K5 | 6 => .K6 | 7 => .K7 | _ => .K8

theorem ring_button_equiv (n : Std.Usize) :
    areas.ring_button n = ok (ofLnmButtonZone (ringButtonCore n.val)) := by
  unfold areas.ring_button ringButtonCore
  split <;> simp_all [ofLnmButtonZone]

theorem ring_button_rotate (steps n : Std.Usize)
    (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    (do let k ← areas.ring_index steps n; areas.ring_button k) =
      ok (ofLnmButtonZone (ringButtonCore ((n.val - 1 + steps.val) % 8 + 1))) := by
  obtain ⟨k, hk, hv⟩ := WP.spec_imp_exists (ring_index_spec steps n hlo hhi)
  rw [hk]
  simp only [bind_tc_ok, ring_button_equiv, hv]

def ringOuterCore (n : Nat) : LnmaiCore.OuterSlot :=
  match n with
  | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8

theorem ring_outer_equiv (n : Std.Usize) :
    areas.ring_outer n = ok (ofLnmOuterSlot (ringOuterCore n.val)) := by
  unfold areas.ring_outer ringOuterCore
  split <;> simp_all [ofLnmOuterSlot]

theorem ring_outer_rotate (steps n : Std.Usize)
    (hlo : 1 ≤ n.val) (hhi : n.val ≤ 8) :
    (do let k ← areas.ring_index steps n; areas.ring_outer k) =
      ok (ofLnmOuterSlot (ringOuterCore ((n.val - 1 + steps.val) % 8 + 1))) := by
  obtain ⟨k, hk, hv⟩ := WP.spec_imp_exists (ring_index_spec steps n hlo hhi)
  rw [hk]
  simp only [bind_tc_ok, ring_outer_equiv, hv]

theorem sensorArea_rotate_equiv (area : areas.SensorArea) (steps : Std.Usize) :
    areas.SensorArea.rotate area steps =
      ok (ofLnmSensorArea (LnmaiCore.SensorArea.rotate steps.val (toLnmSensorArea area))) := by
  cases area with
  | A1 => exact ring_sensor_a_rotate steps 1#usize (by decide) (by decide)
  | A2 => exact ring_sensor_a_rotate steps 2#usize (by decide) (by decide)
  | A3 => exact ring_sensor_a_rotate steps 3#usize (by decide) (by decide)
  | A4 => exact ring_sensor_a_rotate steps 4#usize (by decide) (by decide)
  | A5 => exact ring_sensor_a_rotate steps 5#usize (by decide) (by decide)
  | A6 => exact ring_sensor_a_rotate steps 6#usize (by decide) (by decide)
  | A7 => exact ring_sensor_a_rotate steps 7#usize (by decide) (by decide)
  | A8 => exact ring_sensor_a_rotate steps 8#usize (by decide) (by decide)
  | B1 => exact ring_sensor_b_rotate steps 1#usize (by decide) (by decide)
  | B2 => exact ring_sensor_b_rotate steps 2#usize (by decide) (by decide)
  | B3 => exact ring_sensor_b_rotate steps 3#usize (by decide) (by decide)
  | B4 => exact ring_sensor_b_rotate steps 4#usize (by decide) (by decide)
  | B5 => exact ring_sensor_b_rotate steps 5#usize (by decide) (by decide)
  | B6 => exact ring_sensor_b_rotate steps 6#usize (by decide) (by decide)
  | B7 => exact ring_sensor_b_rotate steps 7#usize (by decide) (by decide)
  | B8 => exact ring_sensor_b_rotate steps 8#usize (by decide) (by decide)
  | D1 => exact ring_sensor_d_rotate steps 1#usize (by decide) (by decide)
  | D2 => exact ring_sensor_d_rotate steps 2#usize (by decide) (by decide)
  | D3 => exact ring_sensor_d_rotate steps 3#usize (by decide) (by decide)
  | D4 => exact ring_sensor_d_rotate steps 4#usize (by decide) (by decide)
  | D5 => exact ring_sensor_d_rotate steps 5#usize (by decide) (by decide)
  | D6 => exact ring_sensor_d_rotate steps 6#usize (by decide) (by decide)
  | D7 => exact ring_sensor_d_rotate steps 7#usize (by decide) (by decide)
  | D8 => exact ring_sensor_d_rotate steps 8#usize (by decide) (by decide)
  | E1 => exact ring_sensor_e_rotate steps 1#usize (by decide) (by decide)
  | E2 => exact ring_sensor_e_rotate steps 2#usize (by decide) (by decide)
  | E3 => exact ring_sensor_e_rotate steps 3#usize (by decide) (by decide)
  | E4 => exact ring_sensor_e_rotate steps 4#usize (by decide) (by decide)
  | E5 => exact ring_sensor_e_rotate steps 5#usize (by decide) (by decide)
  | E6 => exact ring_sensor_e_rotate steps 6#usize (by decide) (by decide)
  | E7 => exact ring_sensor_e_rotate steps 7#usize (by decide) (by decide)
  | E8 => exact ring_sensor_e_rotate steps 8#usize (by decide) (by decide)
  | C => rfl

theorem buttonZone_rotate_equiv (area : areas.ButtonZone) (steps : Std.Usize) :
    areas.ButtonZone.rotate area steps =
      ok (ofLnmButtonZone (LnmaiCore.ButtonZone.rotate steps.val (toLnmButtonZone area))) := by
  cases area with
  | K1 => exact ring_button_rotate steps 1#usize (by decide) (by decide)
  | K2 => exact ring_button_rotate steps 2#usize (by decide) (by decide)
  | K3 => exact ring_button_rotate steps 3#usize (by decide) (by decide)
  | K4 => exact ring_button_rotate steps 4#usize (by decide) (by decide)
  | K5 => exact ring_button_rotate steps 5#usize (by decide) (by decide)
  | K6 => exact ring_button_rotate steps 6#usize (by decide) (by decide)
  | K7 => exact ring_button_rotate steps 7#usize (by decide) (by decide)
  | K8 => exact ring_button_rotate steps 8#usize (by decide) (by decide)

theorem outerSlot_rotate_equiv (area : areas.OuterSlot) (steps : Std.Usize) :
    areas.OuterSlot.rotate area steps =
      ok (ofLnmOuterSlot (LnmaiCore.OuterSlot.rotate steps.val (toLnmOuterSlot area))) := by
  cases area with
  | S1 => exact ring_outer_rotate steps 1#usize (by decide) (by decide)
  | S2 => exact ring_outer_rotate steps 2#usize (by decide) (by decide)
  | S3 => exact ring_outer_rotate steps 3#usize (by decide) (by decide)
  | S4 => exact ring_outer_rotate steps 4#usize (by decide) (by decide)
  | S5 => exact ring_outer_rotate steps 5#usize (by decide) (by decide)
  | S6 => exact ring_outer_rotate steps 6#usize (by decide) (by decide)
  | S7 => exact ring_outer_rotate steps 7#usize (by decide) (by decide)
  | S8 => exact ring_outer_rotate steps 8#usize (by decide) (by decide)

end Verification.Areas
