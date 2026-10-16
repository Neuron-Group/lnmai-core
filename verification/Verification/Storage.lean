import Verification.Areas
import LnmaiCore.Storage
import Batteries.Tactic.OpenPrivate
import Lean.Util.CollectAxioms
import Mathlib.Tactic.IntervalCases

namespace Verification.StorageProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas
open private sensorAreaToStorageIndex sensorAreaOfStorageIndex? listSetAt ButtonVec.getDIndex SensorVec.getDIndex from LnmaiCore.Storage

private abbrev sensorIndex := sensorAreaToStorageIndex
private abbrev sensorOfIndex := sensorAreaOfStorageIndex?

def toLnmButtonVec {T : Type} (v : storage.ButtonVec T) : LnmaiCore.ButtonVec T :=
  ⟨v.data.val⟩
def toLnmSensorVec {T : Type} (v : storage.SensorVec T) : LnmaiCore.SensorVec T :=
  ⟨v.data.val⟩

theorem sensor_area_to_storage_index_equiv (a : areas.SensorArea) :
    ∃ i, storage.sensor_area_to_storage_index a = ok i ∧
      i.val = sensorIndex (toLnmSensorArea a) := by
  cases a <;> simp [storage.sensor_area_to_storage_index, sensorIndex, sensorAreaToStorageIndex, toLnmSensorArea]

theorem sensor_area_of_storage_index_equiv (i : Std.Usize) :
    storage.sensor_area_of_storage_index i =
      ok ((sensorOfIndex i.val).map ofLnmSensorArea) := by
  unfold storage.sensor_area_of_storage_index sensorOfIndex
    sensorAreaOfStorageIndex?
  split <;> simp_all [ofLnmSensorArea]

theorem button_default_equiv (T : Type) :
    storage.ButtonVec.Insts.CoreDefaultDefault.default T =
      ok ⟨alloc.vec.Vec.new T⟩ := rfl

theorem sensor_default_equiv (T : Type) :
    storage.SensorVec.Insts.CoreDefaultDefault.default T =
      ok ⟨alloc.vec.Vec.new T⟩ := rfl

theorem clone_values_equiv {T : Type} (clone : core.clone.Clone T)
    (data : Slice T) (hclone : ∀ x, clone.clone x = ok x) :
    storage.clone_values clone data = ok data := by
  obtain ⟨result, hresult, heq⟩ := spec_imp_exists
    (alloc.slice.Slice.to_vec_spec clone data (by intro x _; exact hclone x))
  simpa [storage.clone_values, ← heq] using hresult

theorem list_set_at_equiv {T : Type} (clone : core.clone.Clone T)
    (data : Slice T) (i : Std.Usize) (value : T)
    (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, storage.list_set_at clone data i value = ok result ∧
      result.val = data.val.set i.val value := by
  unfold storage.list_set_at
  rw [clone_values_equiv clone data hclone]
  simp only [Bind.bind, Aeneas.Std.bind, alloc.vec.Vec.index_mut_slice_index]
  split
  · rename_i h
    obtain ⟨⟨old, back⟩, hback, _, hset⟩ := spec_imp_exists
      (alloc.vec.Vec.index_mut_usize_spec data i (by simpa using h))
    simp [hback, hset, alloc.vec.Vec.set]
  · rename_i h
    refine ⟨data, by simp, ?_⟩
    exact (List.set_eq_of_length_le (by simpa using h)).symm

theorem listSetAt_eq_set {T : Type} (xs : List T) (i : Nat) (x : T) :
    listSetAt xs i x = xs.set i x := by
  induction xs generalizing i with
  | nil => cases i <;> rfl
  | cons hd tl ih =>
      cases i with
      | zero => rfl
      | succ n => simpa [listSetAt, listSetAt] using congrArg (hd :: ·) (ih n)

theorem button_replicate_equiv {T : Type} (clone : core.clone.Clone T)
    (n : Std.Usize) (value : T) (hclone : clone.clone value = ok value) :
    ∃ result, storage.ButtonVec.replicate clone n value = ok result ∧
      toLnmButtonVec result = LnmaiCore.ButtonVec.replicate n.val value := by
  obtain ⟨data, hdata, hval, _⟩ := spec_imp_exists (alloc.vec.from_elem_spec clone value n hclone)
  exact ⟨⟨data⟩, by simp [storage.ButtonVec.replicate, hdata], by simp [toLnmButtonVec, LnmaiCore.ButtonVec.replicate, hval]⟩

theorem sensor_replicate_equiv {T : Type} (clone : core.clone.Clone T)
    (n : Std.Usize) (value : T) (hclone : clone.clone value = ok value) :
    ∃ result, storage.SensorVec.replicate clone n value = ok result ∧
      toLnmSensorVec result = LnmaiCore.SensorVec.replicate n.val value := by
  obtain ⟨data, hdata, hval, _⟩ := spec_imp_exists (alloc.vec.from_elem_spec clone value n hclone)
  exact ⟨⟨data⟩, by simp [storage.SensorVec.replicate, hdata], by simp [toLnmSensorVec, LnmaiCore.SensorVec.replicate, hval]⟩

theorem vec_get_equiv {T : Type} (clone : core.clone.Clone T)
    (data : alloc.vec.Vec T) (i : Std.Usize) (default : T)
    (hclone : ∀ x, clone.clone x = ok x) :
    (if i < data.len then
      Aeneas.Std.bind (alloc.vec.Vec.index (core.slice.index.SliceIndexUsizeSlice T) data i)
        clone.clone
     else ok default) = ok ((data.val[i.val]?).getD default) := by
  by_cases h : i.val < data.val.length
  · obtain ⟨v, hv, hval⟩ := spec_imp_exists (alloc.vec.Vec.index_usize_spec data i h)
    simp [h, hv, hclone, hval]
  · simp [h]

theorem button_get_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.ButtonVec T) (zone : areas.ButtonZone) (default : T)
    (hclone : ∀ x, clone.clone x = ok x) :
    storage.ButtonVec.get_d clone data zone default =
      ok (LnmaiCore.ButtonVec.getD (toLnmButtonVec data) (toLnmButtonZone zone) default) := by
  obtain ⟨i, hi, hindex⟩ := buttonZone_to_index_equiv zone
  unfold storage.ButtonVec.get_d
  rw [hi]
  simp only [Bind.bind, Aeneas.Std.bind]
  have hget := vec_get_equiv clone data.data i default hclone
  simpa only [Aeneas.Std.bind, LnmaiCore.ButtonVec.getD, ButtonVec.getDIndex,
    toLnmButtonVec, hindex] using hget

theorem sensor_get_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.SensorVec T) (area : areas.SensorArea) (default : T)
    (hclone : ∀ x, clone.clone x = ok x) :
    storage.SensorVec.get_d clone data area default =
      ok (LnmaiCore.SensorVec.getD (toLnmSensorVec data) (toLnmSensorArea area) default) := by
  obtain ⟨i, hi, hindex⟩ := sensor_area_to_storage_index_equiv area
  unfold storage.SensorVec.get_d
  rw [hi]
  simp only [Bind.bind, Aeneas.Std.bind]
  have hget := vec_get_equiv clone data.data i default hclone
  simpa only [Aeneas.Std.bind, LnmaiCore.SensorVec.getD, SensorVec.getDIndex,
    toLnmSensorVec, hindex, sensorIndex] using hget

theorem button_set_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.ButtonVec T) (zone : areas.ButtonZone) (value : T)
    (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, storage.ButtonVec.set clone data zone value = ok result ∧
      toLnmButtonVec result = LnmaiCore.ButtonVec.set (toLnmButtonVec data) (toLnmButtonZone zone) value := by
  obtain ⟨i, hi, hindex⟩ := buttonZone_to_index_equiv zone
  obtain ⟨out, hout, hval⟩ := list_set_at_equiv clone data.data i value hclone
  refine ⟨⟨out⟩, ?_, ?_⟩
  · simp [storage.ButtonVec.set, alloc.vec.Vec.deref, hi, hout]
  · simp [toLnmButtonVec, LnmaiCore.ButtonVec.set, listSetAt_eq_set, hval, hindex]

theorem sensor_set_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.SensorVec T) (area : areas.SensorArea) (value : T)
    (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, storage.SensorVec.set clone data area value = ok result ∧
      toLnmSensorVec result = LnmaiCore.SensorVec.set (toLnmSensorVec data) (toLnmSensorArea area) value := by
  obtain ⟨i, hi, hindex⟩ := sensor_area_to_storage_index_equiv area
  obtain ⟨out, hout, hval⟩ := list_set_at_equiv clone data.data i value hclone
  refine ⟨⟨out⟩, ?_, ?_⟩
  · simp [storage.SensorVec.set, alloc.vec.Vec.deref, hi, hout]
  · simp [toLnmSensorVec, LnmaiCore.SensorVec.set, listSetAt_eq_set, hval, hindex, sensorIndex]

theorem button_to_list_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.ButtonVec T) (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, storage.ButtonVec.to_list clone data = ok result ∧
      result.val = LnmaiCore.ButtonVec.toList (toLnmButtonVec data) := by
  exact ⟨data.data, clone_values_equiv clone data.data hclone, rfl⟩

theorem sensor_to_list_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.SensorVec T) (hclone : ∀ x, clone.clone x = ok x) :
    ∃ result, storage.SensorVec.to_list clone data = ok result ∧
      result.val = LnmaiCore.SensorVec.toList (toLnmSensorVec data) := by
  exact ⟨data.data, clone_values_equiv clone data.data hclone, rfl⟩

private def buttonKeys : List areas.ButtonZone :=
  [.K1, .K2, .K3, .K4, .K5, .K6, .K7, .K8]

theorem button_storage_order_equiv :
    storage.button_zone_storage_order ⦃ out =>
      out.val = LnmaiCore.ButtonZone.storageOrder.map ofLnmButtonZone ⦄ := by
  change storage.button_zone_storage_order ⦃ out => out.val = buttonKeys ⦄
  unfold storage.button_zone_storage_order storage.button_zone_storage_order_loop
  apply loop.spec_decr_nat (fun p => 8 - p.2.val)
    (fun p => p.2.val ≤ 8 ∧ p.1.val = buttonKeys.take p.2.val)
  · rintro ⟨out, i⟩ ⟨hi, hout⟩
    dsimp only
    unfold storage.button_zone_storage_order_loop.body
    split
    · rename_i hlt
      have hib : i.val < 8 := by simpa using hlt
      interval_cases h : i.val <;>
        simp_all [areas.ButtonZone.of_index, buttonKeys]
      all_goals step* <;> simp_all <;> scalar_tac
    · rename_i hge
      have hie : i.val = 8 := by scalar_tac
      simp_all [buttonKeys]
  · simp [buttonKeys]

private def sensorKeys : List areas.SensorArea :=
  [.A1, .A2, .A3, .A4, .A5, .A6, .A7, .A8,
   .D1, .D2, .D3, .D4, .D5, .D6, .D7, .D8, .C,
   .E1, .E2, .E3, .E4, .E5, .E6, .E7, .E8,
   .B1, .B2, .B3, .B4, .B5, .B6, .B7, .B8]

open private sensorAreaStorageOrder from LnmaiCore.Storage

set_option maxHeartbeats 4000000 in
theorem sensor_storage_order_equiv :
    storage.sensor_area_storage_order ⦃ out =>
      out.val = sensorAreaStorageOrder.map ofLnmSensorArea ⦄ := by
  change storage.sensor_area_storage_order ⦃ out => out.val = sensorKeys ⦄
  unfold storage.sensor_area_storage_order storage.sensor_area_storage_order_loop
  apply loop.spec_decr_nat (fun p => 33 - p.2.val)
    (fun p => p.2.val ≤ 33 ∧ p.1.val = sensorKeys.take p.2.val)
  · rintro ⟨out, i⟩ ⟨hi, hout⟩
    dsimp only
    unfold storage.sensor_area_storage_order_loop.body
    simp only [constants.SENSOR_AREA_COUNT]
    step as ⟨limit, hlimit⟩
    split
    · rename_i hlt
      have hib : i.val < 33 := by scalar_tac
      interval_cases h : i.val <;>
        simp_all [storage.sensor_area_of_storage_index, sensorKeys]
      all_goals step* <;> simp_all <;> scalar_tac
    · rename_i hge
      have hie : i.val = 33 := by scalar_tac
      simp_all [sensorKeys]
  · simp [sensorKeys]

end Verification.StorageProof
