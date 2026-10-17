import Verification.StorageRefinement
import Verification.StorageSensorRefinement

namespace Verification.StorageLocalRefinementProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas StorageProof
open private buttonMapAccumList sensorMapAccumList sensorAreaStorageOrder
  ButtonVec.ofList SensorVec.ofList from LnmaiCore.Storage

/-! The same Generated map_accum loops, with obligations only at the actual
reference accumulator after each visited prefix. No law about the complete
map or slot is assumed by the frame entry: slot refinement is derived below
from individual scheduler kernels. Short vectors stop at the same place. -/

def SlotsReady {K T V C : Type} (model : K → T → V → C × V)
    (need : K → T → V → Prop) (keys : List K) (values : List T) (state : V) : Prop :=
  match keys, values with
  | key :: ks, value :: vs =>
      need key value state ∧ SlotsReady model need ks vs (model key value state).2
  | _, _ => True

@[simp] theorem slotsReady_nil_right {K T V C : Type}
    (model : K → T → V → C × V) (need : K → T → V → Prop)
    (keys : List K) (state : V) : SlotsReady model need keys [] state := by
  cases keys <;> trivial

theorem button_map_accum_loop_at {T S B V C F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.ButtonZone × T × S) (B × S))
    (data : storage.ButtonVec T) (initial : S) (f : F)
    (keys : alloc.vec.Vec areas.ButtonZone) (state : S → V) (output : B → C)
    (model : areas.ButtonZone → T → V → C × V)
    (need : areas.ButtonZone → T → V → Prop)
    (hclone : ∀ x, clone.clone x = ok x)
    (hcall : ∀ key value acc, need key value (state acc) → ∃ result,
      fn.call f (key, value, acc) = ok result ∧
      (output result.1, state result.2) = model key value (state acc))
    (hready : SlotsReady model need keys.val data.data.val (state initial)) :
    storage.ButtonVec.map_accum_loop clone fn data initial f keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => (out.2.val.map output, state out.1) =
        mapAccumList keys.val data.data.val (state initial) model ⦄ := by
  unfold storage.ButtonVec.map_accum_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.2.val)
    (fun p => p.2.2.val ≤ min keys.val.length data.data.val.length ∧
      p.2.1.val.length = p.2.2.val ∧
      SlotsReady model need (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) (state p.1) ∧
      (p.2.1.val.map output ++ (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) (state p.1) model).1,
       (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) (state p.1) model).2) =
        mapAccumList keys.val data.data.val (state initial) model)
  · rintro ⟨acc, out, i⟩ ⟨hi, hlen, hr, hinv⟩
    dsimp only at *
    unfold storage.ButtonVec.map_accum_loop.body
    dsimp only
    split
    · rename_i hk
      split
      · rename_i hd
        have hki : i.val < keys.val.length := by simpa using hk
        have hdi : i.val < data.data.val.length := by simpa using hd
        rw [List.drop_eq_getElem_cons hki, List.drop_eq_getElem_cons hdi] at hr hinv
        simp only [SlotsReady] at hr
        simp only [mapAccumList] at hinv
        step as ⟨key, hkey⟩
        step as ⟨value, hvalue⟩
        have hneed : need key value (state acc) := by simpa [hkey, hvalue] using hr.1
        obtain ⟨⟨mapped, next⟩, hrun, hmodel⟩ := hcall key value acc hneed
        have hmodelAt : model keys.val[i.val] data.data.val[i.val] (state acc) =
            (output mapped, state next) := by
          simpa only [hkey, hvalue] using hmodel.symm
        rw [hmodelAt] at hr hinv
        simp only [hclone, hrun, bind_tc_ok]
        step as ⟨out', hout'⟩
        step as ⟨i', hi'⟩
        simp_all [List.append_assoc, List.map_append]
        scalar_tac
      · rename_i hd
        have hempty : data.data.val.drop i.val = [] :=
          List.drop_eq_nil_of_le (by simpa using hd)
        simpa [hempty] using hinv
    · rename_i hk
      have hempty : keys.val.drop i.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hk)
      simpa [hempty, mapAccumList] using hinv
  · simpa using hready

theorem button_input_natural {T U V C : Type}
    (input : T → U) (model : LnmaiCore.ButtonZone → U → V → C × V)
    (keys : List LnmaiCore.ButtonZone) (values : List T) (initial : V) :
    buttonMapAccumList keys values initial (fun key value => model key (input value)) =
      buttonMapAccumList keys (values.map input) initial model := by
  induction keys generalizing values initial with
  | nil => rfl
  | cons key keys ih =>
      cases values with
      | nil => rfl
      | cons value values =>
          simp only [buttonMapAccumList, List.map_cons, ih]

/-- Obligations are evaluated on a single concrete vector and its visited
reference prefix states; they do not quantify over arbitrary accumulators. -/
def ButtonVecReady {T U V C : Type} (input : T → U)
    (model : LnmaiCore.ButtonZone → U → V → C × V)
    (need : areas.ButtonZone → T → V → Prop) (data : storage.ButtonVec T) (initial : V) : Prop :=
  SlotsReady (fun key value => model (toLnmButtonZone key) (input value)) need
    (LnmaiCore.ButtonZone.storageOrder.map ofLnmButtonZone) data.data.val initial

theorem button_map_accum_refines_at {T S B U V C F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.ButtonZone × T × S) (B × S))
    (data : storage.ButtonVec T) (initial : S) (f : F)
    (input : T → U) (state : S → V) (output : B → C)
    (model : LnmaiCore.ButtonZone → U → V → C × V)
    (need : areas.ButtonZone → T → V → Prop)
    (hclone : ∀ value, clone.clone value = ok value)
    (hcall : ∀ key value acc, need key value (state acc) → ∃ result,
      fn.call f (key, value, acc) = ok result ∧
      (output result.1, state result.2) =
        model (toLnmButtonZone key) (input value) (state acc))
    (hready : ButtonVecReady input model need data (state initial)) :
    ∃ out, storage.ButtonVec.map_accum clone fn data initial f = ok out ∧
      (StorageRefinementProof.decodeButtons output (toLnmButtonVec out.1), state out.2) =
        (StorageRefinementProof.decodeButtons input (toLnmButtonVec data)).mapAccum
          (state initial) model := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists button_storage_order_equiv
  obtain ⟨⟨finalState, out⟩, hout, hv⟩ := spec_imp_exists
    (button_map_accum_loop_at clone fn data initial f keys state output
      (fun key value => model (toLnmButtonZone key) (input value)) need hclone hcall
      (by rw [hk]; exact hready))
  refine ⟨(⟨out⟩, finalState), by simp [storage.ButtonVec.map_accum, hkeys, hout], ?_⟩
  rw [hk, button_mapAccumList_equiv, button_input_natural] at hv
  simpa only [StorageRefinementProof.decodeButtons, toLnmButtonVec,
    LnmaiCore.ButtonVec.mapAccum, ButtonVec.ofList] using
    congrArg (fun p : List C × V => (LnmaiCore.ButtonVec.mk p.1, p.2)) hv

theorem sensor_map_accum_loop_at {T S B V C F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.SensorArea × T × S) (B × S))
    (data : storage.SensorVec T) (initial : S) (f : F)
    (keys : alloc.vec.Vec areas.SensorArea) (state : S → V) (output : B → C)
    (model : areas.SensorArea → T → V → C × V)
    (need : areas.SensorArea → T → V → Prop)
    (hclone : ∀ x, clone.clone x = ok x)
    (hcall : ∀ key value acc, need key value (state acc) → ∃ result,
      fn.call f (key, value, acc) = ok result ∧
      (output result.1, state result.2) = model key value (state acc))
    (hready : SlotsReady model need keys.val data.data.val (state initial)) :
    storage.SensorVec.map_accum_loop clone fn data initial f keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => (out.2.val.map output, state out.1) =
        mapAccumList keys.val data.data.val (state initial) model ⦄ := by
  unfold storage.SensorVec.map_accum_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.2.val)
    (fun p => p.2.2.val ≤ min keys.val.length data.data.val.length ∧
      p.2.1.val.length = p.2.2.val ∧
      SlotsReady model need (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) (state p.1) ∧
      (p.2.1.val.map output ++ (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) (state p.1) model).1,
       (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) (state p.1) model).2) =
        mapAccumList keys.val data.data.val (state initial) model)
  · rintro ⟨acc, out, i⟩ ⟨hi, hlen, hr, hinv⟩
    dsimp only at *
    unfold storage.SensorVec.map_accum_loop.body
    dsimp only
    split
    · rename_i hk
      split
      · rename_i hd
        have hki : i.val < keys.val.length := by simpa using hk
        have hdi : i.val < data.data.val.length := by simpa using hd
        rw [List.drop_eq_getElem_cons hki, List.drop_eq_getElem_cons hdi] at hr hinv
        simp only [SlotsReady] at hr
        simp only [mapAccumList] at hinv
        step as ⟨key, hkey⟩
        step as ⟨value, hvalue⟩
        have hneed : need key value (state acc) := by simpa [hkey, hvalue] using hr.1
        obtain ⟨⟨mapped, next⟩, hrun, hmodel⟩ := hcall key value acc hneed
        have hmodelAt : model keys.val[i.val] data.data.val[i.val] (state acc) =
            (output mapped, state next) := by
          simpa only [hkey, hvalue] using hmodel.symm
        rw [hmodelAt] at hr hinv
        simp only [hclone, hrun, bind_tc_ok]
        step as ⟨out', hout'⟩
        step as ⟨i', hi'⟩
        simp_all [List.append_assoc, List.map_append]
        scalar_tac
      · rename_i hd
        have hempty : data.data.val.drop i.val = [] :=
          List.drop_eq_nil_of_le (by simpa using hd)
        simpa [hempty] using hinv
    · rename_i hk
      have hempty : keys.val.drop i.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hk)
      simpa [hempty, mapAccumList] using hinv
  · simpa using hready

theorem sensor_input_natural {T U V C : Type}
    (input : T → U) (model : LnmaiCore.SensorArea → U → V → C × V)
    (keys : List LnmaiCore.SensorArea) (values : List T) (initial : V) :
    sensorMapAccumList keys values initial (fun key value => model key (input value)) =
      sensorMapAccumList keys (values.map input) initial model := by
  induction keys generalizing values initial with
  | nil => rfl
  | cons key keys ih =>
      cases values with
      | nil => rfl
      | cons value values =>
          simp only [sensorMapAccumList, List.map_cons, ih]

/-- Obligations are evaluated on a single concrete vector and its visited
reference prefix states; they do not quantify over arbitrary accumulators. -/
def SensorVecReady {T U V C : Type} (input : T → U)
    (model : LnmaiCore.SensorArea → U → V → C × V)
    (need : areas.SensorArea → T → V → Prop) (data : storage.SensorVec T) (initial : V) : Prop :=
  SlotsReady (fun key value => model (toLnmSensorArea key) (input value)) need
    (sensorAreaStorageOrder.map ofLnmSensorArea) data.data.val initial

theorem sensor_map_accum_refines_at {T S B U V C F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.SensorArea × T × S) (B × S))
    (data : storage.SensorVec T) (initial : S) (f : F)
    (input : T → U) (state : S → V) (output : B → C)
    (model : LnmaiCore.SensorArea → U → V → C × V)
    (need : areas.SensorArea → T → V → Prop)
    (hclone : ∀ value, clone.clone value = ok value)
    (hcall : ∀ key value acc, need key value (state acc) → ∃ result,
      fn.call f (key, value, acc) = ok result ∧
      (output result.1, state result.2) =
        model (toLnmSensorArea key) (input value) (state acc))
    (hready : SensorVecReady input model need data (state initial)) :
    ∃ out, storage.SensorVec.map_accum clone fn data initial f = ok out ∧
      (StorageSensorRefinementProof.decodeSensors output (toLnmSensorVec out.1), state out.2) =
        (StorageSensorRefinementProof.decodeSensors input (toLnmSensorVec data)).mapAccum
          (state initial) model := by
  obtain ⟨keys, hkeys, hk⟩ := spec_imp_exists sensor_storage_order_equiv
  obtain ⟨⟨finalState, out⟩, hout, hv⟩ := spec_imp_exists
    (sensor_map_accum_loop_at clone fn data initial f keys state output
      (fun key value => model (toLnmSensorArea key) (input value)) need hclone hcall
      (by rw [hk]; exact hready))
  refine ⟨(⟨out⟩, finalState), by simp [storage.SensorVec.map_accum, hkeys, hout], ?_⟩
  rw [hk, sensor_mapAccumList_equiv, sensor_input_natural] at hv
  simpa only [StorageSensorRefinementProof.decodeSensors, toLnmSensorVec,
    LnmaiCore.SensorVec.mapAccum, SensorVec.ofList] using
    congrArg (fun p : List C × V => (LnmaiCore.SensorVec.mk p.1, p.2)) hv

end Verification.StorageLocalRefinementProof
