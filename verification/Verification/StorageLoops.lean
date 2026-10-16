import Verification.Storage

namespace Verification.StorageProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas

theorem button_entries_loop_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.ButtonVec T) (keys : alloc.vec.Vec areas.ButtonZone)
    (hclone : ∀ x, clone.clone x = ok x) :
    storage.ButtonVec.entries_loop clone data keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.val = keys.val.zip data.data.val ⦄ := by
  unfold storage.ButtonVec.entries_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.val)
    (fun p => p.2.val ≤ (keys.val.zip data.data.val).length ∧
      p.1.val = (keys.val.zip data.data.val).take p.2.val)
  · rintro ⟨out, i⟩ ⟨hi, hout⟩
    dsimp only
    unfold storage.ButtonVec.entries_loop.body
    dsimp only
    split
    · rename_i hk
      split
      · rename_i hd
        step as ⟨key, hkey⟩
        step as ⟨value, hvalue⟩
        simp only [hclone, bind_tc_ok]
        step as ⟨out', hout'⟩
        step as ⟨i', hi'⟩
        have hiz : i.val < (keys.val.zip data.data.val).length := by
          simp only [List.length_zip]
          scalar_tac
        have htake := List.take_succ_eq_append_getElem hiz
        simp only [List.getElem_zip] at htake
        simp_all
        scalar_tac
      · rename_i hd
        simp_all
    · rename_i hk
      simp_all
  · simp

theorem sensor_entries_loop_equiv {T : Type} (clone : core.clone.Clone T)
    (data : storage.SensorVec T) (keys : alloc.vec.Vec areas.SensorArea)
    (hclone : ∀ x, clone.clone x = ok x) :
    storage.SensorVec.entries_loop clone data keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.val = keys.val.zip data.data.val ⦄ := by
  unfold storage.SensorVec.entries_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.val)
    (fun p => p.2.val ≤ (keys.val.zip data.data.val).length ∧
      p.1.val = (keys.val.zip data.data.val).take p.2.val)
  · rintro ⟨out, i⟩ ⟨hi, hout⟩
    dsimp only
    unfold storage.SensorVec.entries_loop.body
    dsimp only
    split
    · rename_i hk
      split
      · rename_i hd
        step as ⟨key, hkey⟩
        step as ⟨value, hvalue⟩
        simp only [hclone, bind_tc_ok]
        step as ⟨out', hout'⟩
        step as ⟨i', hi'⟩
        have hiz : i.val < (keys.val.zip data.data.val).length := by
          simp only [List.length_zip]
          scalar_tac
        have htake := List.take_succ_eq_append_getElem hiz
        simp only [List.getElem_zip] at htake
        simp_all
        scalar_tac
      · rename_i hd
        simp_all
    · rename_i hk
      simp_all
  · simp


theorem button_of_fn_loop_equiv {T F : Type}
    (fn : core.ops.function.Fn F areas.ButtonZone T) (f : F)
    (keys : alloc.vec.Vec areas.ButtonZone) (model : areas.ButtonZone → T)
    (hcall : ∀ key, fn.call f key = ok (model key)) :
    storage.ButtonVec.of_fn_loop fn f keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.val = keys.val.map model ⦄ := by
  unfold storage.ButtonVec.of_fn_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.val)
    (fun p => p.2.val ≤ keys.val.length ∧
      p.1.val = (keys.val.map model).take p.2.val)
  · rintro ⟨out, i⟩ ⟨hi, hout⟩
    dsimp only
    unfold storage.ButtonVec.of_fn_loop.body
    dsimp only
    split
    · rename_i hk
      step as ⟨key, hkey⟩
      simp only [hcall, bind_tc_ok]
      step as ⟨out', hout'⟩
      step as ⟨i', hi'⟩
      have him : i.val < (keys.val.map model).length := by
        simp only [List.length_map]
        scalar_tac
      have htake := List.take_succ_eq_append_getElem him
      simp only [List.getElem_map] at htake
      simp_all
      scalar_tac
    · rename_i hk
      simp_all
  · simp


theorem sensor_of_fn_loop_equiv {T F : Type}
    (fn : core.ops.function.Fn F areas.SensorArea T) (f : F)
    (keys : alloc.vec.Vec areas.SensorArea) (model : areas.SensorArea → T)
    (hcall : ∀ key, fn.call f key = ok (model key)) :
    storage.SensorVec.of_fn_loop fn f keys (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.val = keys.val.map model ⦄ := by
  unfold storage.SensorVec.of_fn_loop
  apply loop.spec_decr_nat (fun p => keys.val.length - p.2.val)
    (fun p => p.2.val ≤ keys.val.length ∧
      p.1.val = (keys.val.map model).take p.2.val)
  · rintro ⟨out, i⟩ ⟨hi, hout⟩
    dsimp only
    unfold storage.SensorVec.of_fn_loop.body
    dsimp only
    split
    · rename_i hk
      step as ⟨key, hkey⟩
      simp only [hcall, bind_tc_ok]
      step as ⟨out', hout'⟩
      step as ⟨i', hi'⟩
      have him : i.val < (keys.val.map model).length := by
        simp only [List.length_map]
        scalar_tac
      have htake := List.take_succ_eq_append_getElem him
      simp only [List.getElem_map] at htake
      simp_all
      scalar_tac
    · rename_i hk
      simp_all
  · simp

end Verification.StorageProof
