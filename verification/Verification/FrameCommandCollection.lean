import Verification.FrameCommandsReference

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.FrameCommandCollection

def PrefixInvariant {A T : Type} (inputs : List A) (command : A → Option T)
    (state : alloc.vec.Vec T × Usize) : Prop :=
  ∃ pref rest : List A, inputs = pref ++ rest ∧ pref.length = state.2.val ∧
    pref.filterMap command = state.1.val

/-- The callback is specified only for a single in-range element. Collection,
ordering, output length, termination and index safety are proved here. -/
theorem collect_indexed_spec {A T F : Type}
    (call : core.ops.function.Fn F Usize (Option T)) (callback : F)
    (len : Usize) (inputs : List A) (command : A → Option T)
    (hlen : len.val = inputs.length)
    (hcall : ∀ (index : Usize) (pref : List A) (item : A) (rest : List A),
      inputs = pref ++ item :: rest → pref.length = index.val →
      call.call callback index = ok (command item)) :
    frame_commands.collect_indexed call len callback
      ⦃ output => output.val = inputs.filterMap command ∧
        output.val.length ≤ inputs.length ⦄ := by
  unfold frame_commands.collect_indexed frame_commands.collect_indexed_loop
  apply loop.spec_decr_nat (fun state => len.val - state.2.val)
    (PrefixInvariant inputs command)
  · rintro ⟨buffer, index⟩ hinv
    obtain ⟨pref, rest, hinputs, hindex, hbuffer⟩ := hinv
    dsimp only at hindex hbuffer ⊢
    have hlength : len.val = pref.length + rest.length := by
      rw [hlen, hinputs, List.length_append]
    have hsize : buffer.val.length ≤ index.val := by
      rw [← hbuffer, ← hindex]
      exact List.length_filterMap_le command pref
    have hmax : len.val ≤ Usize.max := by scalar_tac
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons item rest =>
          have hcallback := hcall index pref item rest hinputs hindex
          obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
            (UScalar.add_spec (x := index) (y := 1#usize) (by scalar_tac))
          have hsucc : next.val = index.val + 1 := by simpa using hnextVal
          have hdecrease : len.val - next.val < len.val - index.val := by omega
          cases hcommand : command item with
          | none =>
              have hkeep : PrefixInvariant inputs command (buffer, next) := by
                refine ⟨pref ++ [item], rest, ?_, ?_, ?_⟩
                · simpa only [List.append_assoc, List.singleton_append] using hinputs
                · simp [List.length_append, hindex, hsucc]
                · simp [List.filterMap_append, hcommand, hbuffer]
              simpa [frame_commands.collect_indexed_loop.body, hmore, hcallback,
                hcommand, hnext, bind_tc_ok] using And.intro hkeep hdecrease
          | some value =>
              obtain ⟨nextBuffer, hpush, hpushVal⟩ := spec_imp_exists
                (alloc.vec.Vec.push_spec buffer value (by omega))
              have hkeep : PrefixInvariant inputs command (nextBuffer, next) := by
                refine ⟨pref ++ [item], rest, ?_, ?_, ?_⟩
                · simpa only [List.append_assoc, List.singleton_append] using hinputs
                · simp [List.length_append, hindex, hsucc]
                · simp [List.filterMap_append, hcommand, hbuffer, hpushVal]
              simpa [frame_commands.collect_indexed_loop.body, hmore, hcallback,
                hcommand, hpush, hnext, bind_tc_ok] using And.intro hkeep hdecrease
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons item rest => simp only [List.length_cons] at hlength; omega
      have hpref : inputs = pref := by simpa [hempty] using hinputs
      have hpost : buffer.val = inputs.filterMap command ∧
          buffer.val.length ≤ inputs.length := by
        refine ⟨by simpa [hpref] using hbuffer.symm, ?_⟩
        rw [← hbuffer, ← hpref]
        exact List.length_filterMap_le command inputs
      simpa [frame_commands.collect_indexed_loop.body, hmore] using hpost
  · exact ⟨[], inputs, by simp, by simp, by simp⟩

theorem collect_indexed_equiv {A T F : Type}
    (call : core.ops.function.Fn F Usize (Option T)) (callback : F)
    (len : Usize) (inputs : List A) (command : A → Option T)
    (hlen : len.val = inputs.length)
    (hcall : ∀ (index : Usize) (pref : List A) (item : A) (rest : List A),
      inputs = pref ++ item :: rest → pref.length = index.val →
      call.call callback index = ok (command item)) :
    (do let output ← frame_commands.collect_indexed call len callback
        ok output.val) = ok (inputs.filterMap command) := by
  obtain ⟨output, hrun, hout, _⟩ := spec_imp_exists
    (collect_indexed_spec call callback len inputs command hlen hcall)
  simp [hrun, hout, bind_tc_ok]

end Verification.FrameCommandCollection
