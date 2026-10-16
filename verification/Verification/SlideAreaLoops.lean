import Verification.Storage
import Verification.Bridge
import Verification.SlideHistory

namespace Verification.SlideAreaProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas StorageProof

theorem history_get_equiv (history : Slice Bool) (i : Std.Usize) :
    lifecycle.history_get history i = ok ((history.val[i.val]?).getD false) := by
  unfold lifecycle.history_get
  by_cases hi : i.val < history.val.length
  · obtain ⟨value, hvalue, hv⟩ := spec_imp_exists (Slice.index_usize_spec history i hi)
    simp [hi, hvalue, hv]
  · simp [hi]

theorem history_any_equiv (history : Slice Bool) :
    lifecycle.history_any history ⦃ out => out = history.val.any id ⦄ := by
  unfold lifecycle.history_any lifecycle.history_any_loop
  apply loop.spec_decr_nat (fun p => history.val.length - p.2.val)
    (fun p => p.2.val ≤ history.val.length ∧
      p.1 = (history.val.take p.2.val).any id)
  · rintro ⟨result, i⟩ ⟨hi, hresult⟩
    dsimp only
    unfold lifecycle.history_any_loop.body
    dsimp only
    split
    · rename_i hlt
      have hidx : i.val < history.val.length := by simpa using hlt
      obtain ⟨value, hvalue, hv⟩ := spec_imp_exists (Slice.index_usize_spec history i hidx)
      have htake := List.take_succ_eq_append_getElem hidx
      have hany : (history.val.take (i.val + 1)).any id =
          ((history.val.take i.val).any id || history.val[i.val]) := by
        simp only [htake, List.any_append, List.any_cons, List.any_nil, id_eq,
          Bool.or_false]
      cases result <;> simp only [Bool.false_eq_true, ↓reduceIte, hvalue, bind_tc_ok]
      all_goals
        step as ⟨i', hi'⟩
        simp only [hi', hany]
        simp_all
        scalar_tac
    · rename_i hge
      have htake : history.val.take i.val = history.val :=
        List.take_of_length_le (by simpa using hge)
      simp_all
  · simp

/-- The reference deliberately truncates both histories to their common length. -/
def finishedHistory (on off : List Bool) (isLast : Bool) : List Bool :=
  (on.zip off).map (fun pair => pair.1 && (isLast || pair.2))

def combineHistory (policy : types.AreaPolicy) (history : List Bool) : Bool :=
  match policy with
  | .Or => history.any id
  | .And => history.all id

def combineStep (policy : types.AreaPolicy) (previous current : Bool) : Bool :=
  match policy with
  | .Or => previous || current
  | .And => previous && current

theorem combine_history_append (policy : types.AreaPolicy) (history : List Bool)
    (value : Bool) :
    combineHistory policy (history ++ [value]) =
      combineStep policy (combineHistory policy history) value := by
  cases policy <;> simp only [combineHistory, combineStep, List.any_append,
    List.all_append, List.any_cons, List.all_cons, List.any_nil, List.all_nil,
    id_eq, Bool.or_false, Bool.and_true]

theorem history_finished_loop_equiv (on off : Slice Bool) (isLast : Bool)
    (policy : types.AreaPolicy) :
    lifecycle.history_finished_loop on off isLast policy (combineHistory policy []) 0#usize ⦃ out =>
      out = combineHistory policy (finishedHistory on.val off.val isLast) ⦄ := by
    unfold lifecycle.history_finished_loop
    apply loop.spec_decr_nat (fun p => on.val.length - p.2.2.val)
      (fun p => p.1 = isLast ∧
        p.2.2.val ≤ (on.val.zip off.val).length ∧
        p.2.1 = combineHistory policy
          ((finishedHistory on.val off.val isLast).take p.2.2.val))
    · rintro ⟨last, result, i⟩ ⟨hlast, hi, hresult⟩
      dsimp only at hlast hi hresult
      dsimp only
      simp only [hlast]
      unfold lifecycle.history_finished_loop.body
      dsimp only
      split
      · rename_i hon
        split
        · rename_i hoff
          have hion : i.val < on.val.length := by simpa using hon
          have hioff : i.val < off.val.length := by simpa using hoff
          have hnext : i.val + 1 ≤ (on.val.zip off.val).length := by
            simp only [List.length_zip]
            omega
          have hdecr : on.val.length - (i.val + 1) < on.val.length - i.val := by omega
          obtain ⟨vOn, hvOn, hOn⟩ := spec_imp_exists (Slice.index_usize_spec on i hion)
          obtain ⟨vOff, hvOff, hOff⟩ := spec_imp_exists (Slice.index_usize_spec off i hioff)
          have hiz : i.val < (finishedHistory on.val off.val isLast).length := by
            simp only [finishedHistory, List.length_map, List.length_zip]
            omega
          have htake :
              (finishedHistory on.val off.val isLast).take (i.val + 1) =
                (finishedHistory on.val off.val isLast).take i.val ++
                  [on.val[i.val] && (isLast || off.val[i.val])] := by
            simpa only [finishedHistory, List.getElem_map, List.getElem_zip] using
              List.take_succ_eq_append_getElem hiz
          have hcombined :
              combineHistory policy ((finishedHistory on.val off.val isLast).take (i.val + 1)) =
                combineStep policy result (vOn && (isLast || vOff)) := by
            rw [htake, combine_history_append, ← hresult, ← hOn, ← hOff]
          simp only [hvOn, hvOff, bind_tc_ok]
          cases policy <;> cases vOn <;> cases isLast <;> cases result <;> simp only [Bool.false_eq_true,
            ↓reduceIte, bind_tc_ok]
          all_goals
            step as ⟨i', hi'⟩
            simp only [hi']
            refine ⟨?_, ?_, ?_⟩
            · exact hnext
            · simpa only [combineStep, Bool.false_or, Bool.true_or, Bool.false_and,
                Bool.true_and, Bool.or_true, Bool.and_true] using hcombined.symm
            · exact hdecr
        · rename_i hoff
          have hbound : (finishedHistory on.val off.val isLast).length ≤ i.val := by
            simp only [finishedHistory, List.length_map, List.length_zip]
            have : off.val.length ≤ i.val := by simpa using hoff
            omega
          simp_all [List.take_of_length_le hbound]
      · rename_i hon
        have hbound : (finishedHistory on.val off.val isLast).length ≤ i.val := by
          simp only [finishedHistory, List.length_map, List.length_zip]
          have : on.val.length ≤ i.val := by simpa using hon
          omega
        simp_all [List.take_of_length_le hbound]
    · simp [combineHistory, finishedHistory]

theorem history_finished_equiv (on off : Slice Bool) (isLast : Bool)
    (policy : types.AreaPolicy) :
    lifecycle.history_finished on off isLast policy ⦃ out =>
      out = combineHistory policy (finishedHistory on.val off.val isLast) ⦄ := by
  have hinit : lifecycle.history_finished on off isLast policy =
      lifecycle.history_finished_loop on off isLast policy (combineHistory policy []) 0#usize := by
    cases policy <;> rfl
  rw [hinit]
  exact history_finished_loop_equiv on off isLast policy

theorem target_history_equiv (targets : Slice areas.SensorArea)
    (wasOn wasOff : Slice Bool) (held : storage.SensorVec Bool) :
    lifecycle.target_history targets wasOn wasOff held ⦃ out =>
      (out.1.val, out.2.val) =
        ((historyPairs (targets.val.map toLnmSensorArea) wasOn.val wasOff.val
          (toLnmSensorVec held)).map Prod.fst,
         (historyPairs (targets.val.map toLnmSensorArea) wasOn.val wasOff.val
          (toLnmSensorVec held)).map Prod.snd) ⦄ := by
  let expected := historyPairs (targets.val.map toLnmSensorArea) wasOn.val wasOff.val
    (toLnmSensorVec held)
  have hlen : expected.length = targets.val.length := by simp [expected, historyPairs]
  unfold lifecycle.target_history lifecycle.target_history_loop
  apply loop.spec_decr_nat (fun p => targets.val.length - p.2.2.val)
    (fun p => p.2.2.val ≤ targets.val.length ∧
      p.1.val = (expected.map Prod.fst).take p.2.2.val ∧
      p.2.1.val = (expected.map Prod.snd).take p.2.2.val)
  · rintro ⟨on, off, i⟩ ⟨hi, hon, hoff⟩
    dsimp only
    unfold lifecycle.target_history_loop.body
    dsimp only
    split
    · rename_i hlt
      have hidx : i.val < targets.val.length := by simpa using hlt
      simp only [history_get_equiv, bind_tc_ok]
      step as ⟨target, htarget⟩
      rw [sensor_get_equiv core.clone.CloneBool held target false (by intro x; rfl)]
      simp only [bind_tc_ok]
      have hiOn : i.val < (expected.map Prod.fst).length := by simp [hlen]; omega
      have hiOff : i.val < (expected.map Prod.snd).length := by simp [hlen]; omega
      have htakeOn := List.take_succ_eq_append_getElem hiOn
      have htakeOff := List.take_succ_eq_append_getElem hiOff
      simp only [expected, historyPairs, List.getElem_map, List.getElem_zipIdx,
        Nat.zero_add, nextHistory] at htakeOn htakeOff
      cases hOn : (wasOn.val[i.val]?).getD false <;>
        cases hOff : (wasOff.val[i.val]?).getD false <;>
        simp only [Bool.false_eq_true, ↓reduceIte, bind_tc_ok]
      all_goals
        step as ⟨on', hon'⟩
        step as ⟨off', hoff'⟩
        step as ⟨i', hi'⟩
        simp_all [expected, historyPairs]
        scalar_tac
    · rename_i hge
      have hiEq : i.val = targets.val.length := by scalar_tac
      have htOn : (expected.map Prod.fst).take i.val = expected.map Prod.fst :=
        List.take_of_length_le (by simp [hlen, hiEq])
      have htOff : (expected.map Prod.snd).take i.val = expected.map Prod.snd :=
        List.take_of_length_le (by simp [hlen, hiEq])
      simp_all [expected]
  · simp

end Verification.SlideAreaProof

