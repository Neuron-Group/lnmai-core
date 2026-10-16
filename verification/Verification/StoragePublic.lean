import Verification.StorageAccum

namespace Verification.StorageProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas
open private sensorAreaStorageOrder buttonMapAccumList sensorMapAccumList
  ButtonVec.ofList SensorVec.ofList from LnmaiCore.Storage

theorem button_entries_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.ButtonVec T) (hclone : ∀ x, clone.clone x = ok x) :
    ∃ out, storage.ButtonVec.entries clone data = ok out ∧
      out.val.map (Prod.map toLnmButtonZone id) =
        LnmaiCore.ButtonVec.entries (toLnmButtonVec data) := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists button_storage_order_equiv
  obtain ⟨out, hout, hv⟩ := spec_imp_exists
    (button_entries_loop_equiv clone data keys hclone)
  refine ⟨out, by simp [storage.ButtonVec.entries, hkeys, hout], ?_⟩
  simp [hv, hk, List.zip_map_left, List.map_map, Prod.map, Function.comp_def,
    LnmaiCore.ButtonVec.entries, toLnmButtonVec]

theorem button_of_fn_equiv {T F : Type} (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F areas.ButtonZone T) (f : F)
    (model : LnmaiCore.ButtonZone → T)
    (hcall : ∀ key, fn.call f key = ok (model (toLnmButtonZone key))) :
    ∃ out, storage.ButtonVec.of_fn clone fn f = ok out ∧
      toLnmButtonVec out = LnmaiCore.ButtonVec.ofFn model := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists button_storage_order_equiv
  obtain ⟨out, hout, hv⟩ := spec_imp_exists
    (button_of_fn_loop_equiv fn f keys (fun key => model (toLnmButtonZone key)) hcall)
  refine ⟨⟨out⟩, by simp [storage.ButtonVec.of_fn, hkeys, hout], ?_⟩
  simp [toLnmButtonVec, LnmaiCore.ButtonVec.ofFn, hv, hk, List.map_map]

theorem button_mapAccumList_equiv {T S B : Type}
    (keys : List LnmaiCore.ButtonZone) (values : List T) (state : S)
    (f : LnmaiCore.ButtonZone → T → S → B × S) :
    mapAccumList (keys.map ofLnmButtonZone) values state
      (fun key => f (toLnmButtonZone key)) = buttonMapAccumList keys values state f := by
  induction keys generalizing values state with
  | nil => rfl
  | cons key keys ih =>
      cases values with
      | nil => rfl
      | cons value values =>
          simp [mapAccumList, buttonMapAccumList, ih]

theorem button_map_accum_equiv {T S B F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.ButtonZone × T × S) (B × S))
    (data : storage.ButtonVec T) (initial : S) (f : F)
    (model : LnmaiCore.ButtonZone → T → S → B × S)
    (hclone : ∀ x, clone.clone x = ok x)
    (hcall : ∀ key value state, fn.call f (key, value, state) =
      ok (model (toLnmButtonZone key) value state)) :
    ∃ out, storage.ButtonVec.map_accum clone fn data initial f = ok out ∧
      (toLnmButtonVec out.1, out.2) = LnmaiCore.ButtonVec.mapAccum
        (toLnmButtonVec data) initial model := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists button_storage_order_equiv
  obtain ⟨⟨state, out⟩, hout, hv⟩ := spec_imp_exists
    (button_map_accum_loop_equiv clone fn data initial f keys
      (fun key => model (toLnmButtonZone key)) hclone hcall)
  refine ⟨(⟨out⟩, state), by simp [storage.ButtonVec.map_accum, hkeys, hout], ?_⟩
  rw [hk, button_mapAccumList_equiv] at hv
  simpa [toLnmButtonVec, LnmaiCore.ButtonVec.mapAccum, ButtonVec.ofList] using
    congrArg (fun p : List B × S => (LnmaiCore.ButtonVec.mk p.1, p.2)) hv

theorem sensor_entries_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.SensorVec T) (hclone : ∀ x, clone.clone x = ok x) :
    ∃ out, storage.SensorVec.entries clone data = ok out ∧
      out.val.map (Prod.map toLnmSensorArea id) =
        LnmaiCore.SensorVec.entries (toLnmSensorVec data) := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists sensor_storage_order_equiv
  obtain ⟨out, hout, hv⟩ := spec_imp_exists
    (sensor_entries_loop_equiv clone data keys hclone)
  refine ⟨out, by simp [storage.SensorVec.entries, hkeys, hout], ?_⟩
  simp [hv, hk, List.zip_map_left, List.map_map, Prod.map, Function.comp_def,
    LnmaiCore.SensorVec.entries, toLnmSensorVec]

theorem sensor_of_fn_equiv {T F : Type} (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F areas.SensorArea T) (f : F)
    (model : LnmaiCore.SensorArea → T)
    (hcall : ∀ key, fn.call f key = ok (model (toLnmSensorArea key))) :
    ∃ out, storage.SensorVec.of_fn clone fn f = ok out ∧
      toLnmSensorVec out = LnmaiCore.SensorVec.ofFn model := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists sensor_storage_order_equiv
  obtain ⟨out, hout, hv⟩ := spec_imp_exists
    (sensor_of_fn_loop_equiv fn f keys (fun key => model (toLnmSensorArea key)) hcall)
  refine ⟨⟨out⟩, by simp [storage.SensorVec.of_fn, hkeys, hout], ?_⟩
  simp [toLnmSensorVec, LnmaiCore.SensorVec.ofFn, hv, hk, List.map_map]

theorem sensor_mapAccumList_equiv {T S B : Type}
    (keys : List LnmaiCore.SensorArea) (values : List T) (state : S)
    (f : LnmaiCore.SensorArea → T → S → B × S) :
    mapAccumList (keys.map ofLnmSensorArea) values state
      (fun key => f (toLnmSensorArea key)) = sensorMapAccumList keys values state f := by
  induction keys generalizing values state with
  | nil => rfl
  | cons key keys ih =>
      cases values with
      | nil => rfl
      | cons value values =>
          simp [mapAccumList, sensorMapAccumList, ih]

theorem sensor_map_accum_equiv {T S B F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.SensorArea × T × S) (B × S))
    (data : storage.SensorVec T) (initial : S) (f : F)
    (model : LnmaiCore.SensorArea → T → S → B × S)
    (hclone : ∀ x, clone.clone x = ok x)
    (hcall : ∀ key value state, fn.call f (key, value, state) =
      ok (model (toLnmSensorArea key) value state)) :
    ∃ out, storage.SensorVec.map_accum clone fn data initial f = ok out ∧
      (toLnmSensorVec out.1, out.2) = LnmaiCore.SensorVec.mapAccum
        (toLnmSensorVec data) initial model := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists sensor_storage_order_equiv
  obtain ⟨⟨state, out⟩, hout, hv⟩ := spec_imp_exists
    (sensor_map_accum_loop_equiv clone fn data initial f keys
      (fun key => model (toLnmSensorArea key)) hclone hcall)
  refine ⟨(⟨out⟩, state), by simp [storage.SensorVec.map_accum, hkeys, hout], ?_⟩
  rw [hk, sensor_mapAccumList_equiv] at hv
  simpa [toLnmSensorVec, LnmaiCore.SensorVec.mapAccum, SensorVec.ofList] using
    congrArg (fun p : List B × S => (LnmaiCore.SensorVec.mk p.1, p.2)) hv


theorem button_clone_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.ButtonVec T) (hclone : ∀ x, clone.clone x = ok x) :
    storage.ButtonVec.Insts.CoreCloneClone.clone clone data = ok data := by
  have hc : alloc.vec.CloneVec.clone clone data.data = ok data.data :=
    clone_values_equiv clone data.data hclone
  simp [storage.ButtonVec.Insts.CoreCloneClone.clone, hc]

theorem sensor_clone_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.SensorVec T) (hclone : ∀ x, clone.clone x = ok x) :
    storage.SensorVec.Insts.CoreCloneClone.clone clone data = ok data := by
  have hc : alloc.vec.CloneVec.clone clone data.data = ok data.data :=
    clone_values_equiv clone data.data hclone
  simp [storage.SensorVec.Insts.CoreCloneClone.clone, hc]

theorem button_default_bridge (T : Type) :
    toLnmButtonVec (⟨alloc.vec.Vec.new T⟩ : storage.ButtonVec T) = default := rfl

theorem sensor_default_bridge (T : Type) :
    toLnmSensorVec (⟨alloc.vec.Vec.new T⟩ : storage.SensorVec T) = default := rfl

end Verification.StorageProof
