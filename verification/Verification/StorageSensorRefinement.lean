import Verification.StoragePublic

namespace Verification.StorageSensorRefinementProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas StorageProof
open private sensorAreaStorageOrder sensorMapAccumList SensorVec.ofList from LnmaiCore.Storage

/-!
Representation-preserving composition of the real extracted SensorVec.map_accum.
The callback law concerns a single slot and the current accumulator. Neither
the complete vector traversal nor any scheduler pass is assumed. The original
storage theorem supplies the actual generated loop, including its stop at the
shorter of the key list and the input vector. Native clones remain explicit.
-/

def decodeSensors {T U : Type} (decode : T → U)
    (values : LnmaiCore.SensorVec T) : LnmaiCore.SensorVec U :=
  ⟨values.data.map decode⟩

theorem map_accum_list_natural {T S B U V C : Type}
    (input : T → U) (state : S → V) (output : B → C)
    (native : LnmaiCore.SensorArea → T → S → B × S)
    (model : LnmaiCore.SensorArea → U → V → C × V)
    (step : ∀ key value initial,
      (output (native key value initial).1, state (native key value initial).2) =
        model key (input value) (state initial))
    (keys : List LnmaiCore.SensorArea) (values : List T) (initial : S) :
    ((sensorMapAccumList keys values initial native).1.map output,
      state (sensorMapAccumList keys values initial native).2) =
      sensorMapAccumList keys (values.map input) (state initial) model := by
  induction keys generalizing values initial with
  | nil => rfl
  | cons key keys ih =>
      cases values with
      | nil => rfl
      | cons value values =>
          have hstep := step key value initial
          rcases hn : native key value initial with ⟨mapped, next⟩
          simp only [hn] at hstep
          simp only [sensorMapAccumList, List.map_cons, hn, ← hstep]
          have hrest := ih values next
          exact congrArg (fun p : List C × V => (output mapped :: p.1, p.2)) hrest

theorem sensor_map_accum_natural {T S B U V C : Type}
    (input : T → U) (state : S → V) (output : B → C)
    (native : LnmaiCore.SensorArea → T → S → B × S)
    (model : LnmaiCore.SensorArea → U → V → C × V)
    (step : ∀ key value initial,
      (output (native key value initial).1, state (native key value initial).2) =
        model key (input value) (state initial))
    (values : LnmaiCore.SensorVec T) (initial : S) :
    (decodeSensors output (values.mapAccum initial native).1,
      state (values.mapAccum initial native).2) =
      (decodeSensors input values).mapAccum (state initial) model := by
  have h := map_accum_list_natural input state output native model step
    sensorAreaStorageOrder values.data initial
  exact congrArg (fun p : List C × V => (LnmaiCore.SensorVec.mk p.1, p.2)) h

/-- Existential callback results allow different native/reference types. The
choice is proof-only: the executable result remains Generated.map_accum. -/
theorem sensor_map_accum_refines {T S B U V C F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.SensorArea × T × S) (B × S))
    (data : storage.SensorVec T) (initial : S) (f : F)
    (input : T → U) (state : S → V) (output : B → C)
    (model : LnmaiCore.SensorArea → U → V → C × V)
    (hclone : ∀ value, clone.clone value = ok value)
    (hcall : ∀ key value initial, ∃ result,
      fn.call f (key, value, initial) = ok result ∧
      (output result.1, state result.2) =
        model (toLnmSensorArea key) (input value) (state initial)) :
    ∃ out, storage.SensorVec.map_accum clone fn data initial f = ok out ∧
      (decodeSensors output (toLnmSensorVec out.1), state out.2) =
        (decodeSensors input (toLnmSensorVec data)).mapAccum (state initial) model := by
  classical
  let native : LnmaiCore.SensorArea → T → S → B × S :=
    fun key value initial => (hcall (ofLnmSensorArea key) value initial).choose
  have hnative : ∀ key value initial, fn.call f (key, value, initial) =
      ok (native (toLnmSensorArea key) value initial) := by
    intro key value initial
    simpa only [native, ofLnmSensorArea_toLnmSensorArea] using
      (hcall key value initial).choose_spec.1
  have hstep : ∀ key value initial,
      (output (native key value initial).1, state (native key value initial).2) =
        model key (input value) (state initial) := by
    intro key value initial
    simpa only [native, toLnmSensorArea_ofLnmSensorArea] using
      (hcall (ofLnmSensorArea key) value initial).choose_spec.2
  obtain ⟨out, hrun, hmodel⟩ :=
    sensor_map_accum_equiv clone fn data initial f native hclone hnative
  refine ⟨out, hrun, ?_⟩
  have h := congrArg (fun p : LnmaiCore.SensorVec B × S =>
    (decodeSensors output p.1, state p.2)) hmodel
  exact h.trans (sensor_map_accum_natural input state output native model hstep
    (toLnmSensorVec data) initial)

end Verification.StorageSensorRefinementProof
