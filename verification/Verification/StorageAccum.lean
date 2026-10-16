import Verification.StorageLoops

namespace Verification.StorageProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas

def mapAccumList {K T S B : Type} (keys : List K) (values : List T)
    (state : S) (f : K → T → S → B × S) : List B × S :=
  match keys, values with
  | key :: ks, value :: vs =>
      let (mapped, next) := f key value state
      let (rest, finalState) := mapAccumList ks vs next f
      (mapped :: rest, finalState)
  | _, _ => ([], state)

@[simp] theorem mapAccumList_nil_right {K T S B : Type} (keys : List K)
    (state : S) (f : K → T → S → B × S) :
    mapAccumList keys [] state f = ([], state) := by
  cases keys <;> rfl

theorem button_map_accum_loop_equiv {T S B F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.ButtonZone × T × S) (B × S))
    (data : storage.ButtonVec T) (initial : S) (f : F)
    (keys : alloc.vec.Vec areas.ButtonZone)
    (model : areas.ButtonZone → T → S → B × S)
    (hclone : ∀ x, clone.clone x = ok x)
    (hcall : ∀ key value state, fn.call f (key, value, state) = ok (model key value state)) :
    storage.ButtonVec.map_accum_loop clone fn data initial f keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => (out.2.val, out.1) = mapAccumList keys.val data.data.val initial model ⦄ := by
  unfold storage.ButtonVec.map_accum_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.2.val)
    (fun p => p.2.2.val ≤ min keys.val.length data.data.val.length ∧
      p.2.1.val.length = p.2.2.val ∧
      (p.2.1.val ++ (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) p.1 model).1,
       (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) p.1 model).2) =
        mapAccumList keys.val data.data.val initial model)
  · rintro ⟨state, out, i⟩ ⟨hi, hlen, hinv⟩
    dsimp only at *
    unfold storage.ButtonVec.map_accum_loop.body
    dsimp only
    split
    · rename_i hk
      split
      · rename_i hd
        have hki : i.val < keys.val.length := by simpa using hk
        have hdi : i.val < data.data.val.length := by simpa using hd
        rw [List.drop_eq_getElem_cons hki, List.drop_eq_getElem_cons hdi] at hinv
        simp only [mapAccumList] at hinv
        step as ⟨key, hkey⟩
        step as ⟨value, hvalue⟩
        simp only [hclone, hcall, bind_tc_ok]
        rcases hmodel : model key value state with ⟨mapped, next⟩
        step as ⟨out', hout'⟩
        step as ⟨i', hi'⟩
        simp_all [List.append_assoc]
        scalar_tac
      · rename_i hd
        have hempty : data.data.val.drop i.val = [] :=
          List.drop_eq_nil_of_le (by simpa using hd)
        simpa [hempty] using hinv
    · rename_i hk
      have hempty : keys.val.drop i.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hk)
      simpa [hempty, mapAccumList] using hinv
  · simp

theorem sensor_map_accum_loop_equiv {T S B F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.SensorArea × T × S) (B × S))
    (data : storage.SensorVec T) (initial : S) (f : F)
    (keys : alloc.vec.Vec areas.SensorArea)
    (model : areas.SensorArea → T → S → B × S)
    (hclone : ∀ x, clone.clone x = ok x)
    (hcall : ∀ key value state, fn.call f (key, value, state) = ok (model key value state)) :
    storage.SensorVec.map_accum_loop clone fn data initial f keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => (out.2.val, out.1) = mapAccumList keys.val data.data.val initial model ⦄ := by
  unfold storage.SensorVec.map_accum_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.2.val)
    (fun p => p.2.2.val ≤ min keys.val.length data.data.val.length ∧
      p.2.1.val.length = p.2.2.val ∧
      (p.2.1.val ++ (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) p.1 model).1,
       (mapAccumList (keys.val.drop p.2.2.val)
        (data.data.val.drop p.2.2.val) p.1 model).2) =
        mapAccumList keys.val data.data.val initial model)
  · rintro ⟨state, out, i⟩ ⟨hi, hlen, hinv⟩
    dsimp only at *
    unfold storage.SensorVec.map_accum_loop.body
    dsimp only
    split
    · rename_i hk
      split
      · rename_i hd
        have hki : i.val < keys.val.length := by simpa using hk
        have hdi : i.val < data.data.val.length := by simpa using hd
        rw [List.drop_eq_getElem_cons hki, List.drop_eq_getElem_cons hdi] at hinv
        simp only [mapAccumList] at hinv
        step as ⟨key, hkey⟩
        step as ⟨value, hvalue⟩
        simp only [hclone, hcall, bind_tc_ok]
        rcases hmodel : model key value state with ⟨mapped, next⟩
        step as ⟨out', hout'⟩
        step as ⟨i', hi'⟩
        simp_all [List.append_assoc]
        scalar_tac
      · rename_i hd
        have hempty : data.data.val.drop i.val = [] :=
          List.drop_eq_nil_of_le (by simpa using hd)
        simpa [hempty] using hinv
    · rename_i hk
      have hempty : keys.val.drop i.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hk)
      simpa [hempty, mapAccumList] using hinv
  · simp

end Verification.StorageProof
