import Verification.StoragePublic

namespace Verification.StorageRefinementProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open Areas StorageProof
open private buttonMapAccumList ButtonVec.ofList from LnmaiCore.Storage

/-!
Representation-preserving composition of the real extracted ButtonVec.map_accum.
The callback law concerns a single slot and the current accumulator. Neither
the complete vector traversal nor any scheduler pass is assumed. The original
storage theorem supplies the actual generated loop, including its stop at the
shorter of the key list and the input vector. Native clones remain explicit.
-/

def decodeButtons {T U : Type} (decode : T → U)
    (values : LnmaiCore.ButtonVec T) : LnmaiCore.ButtonVec U :=
  ⟨values.data.map decode⟩

theorem map_accum_list_natural {T S B U V C : Type}
    (input : T → U) (state : S → V) (output : B → C)
    (native : LnmaiCore.ButtonZone → T → S → B × S)
    (model : LnmaiCore.ButtonZone → U → V → C × V)
    (step : ∀ key value initial,
      (output (native key value initial).1, state (native key value initial).2) =
        model key (input value) (state initial))
    (keys : List LnmaiCore.ButtonZone) (values : List T) (initial : S) :
    ((buttonMapAccumList keys values initial native).1.map output,
      state (buttonMapAccumList keys values initial native).2) =
      buttonMapAccumList keys (values.map input) (state initial) model := by
  induction keys generalizing values initial with
  | nil => rfl
  | cons key keys ih =>
      cases values with
      | nil => rfl
      | cons value values =>
          have hstep := step key value initial
          rcases hn : native key value initial with ⟨mapped, next⟩
          simp only [hn] at hstep
          simp only [buttonMapAccumList, List.map_cons, hn, ← hstep]
          have hrest := ih values next
          exact congrArg (fun p : List C × V => (output mapped :: p.1, p.2)) hrest

theorem button_map_accum_natural {T S B U V C : Type}
    (input : T → U) (state : S → V) (output : B → C)
    (native : LnmaiCore.ButtonZone → T → S → B × S)
    (model : LnmaiCore.ButtonZone → U → V → C × V)
    (step : ∀ key value initial,
      (output (native key value initial).1, state (native key value initial).2) =
        model key (input value) (state initial))
    (values : LnmaiCore.ButtonVec T) (initial : S) :
    (decodeButtons output (values.mapAccum initial native).1,
      state (values.mapAccum initial native).2) =
      (decodeButtons input values).mapAccum (state initial) model := by
  have h := map_accum_list_natural input state output native model step
    LnmaiCore.ButtonZone.storageOrder values.data initial
  exact congrArg (fun p : List C × V => (LnmaiCore.ButtonVec.mk p.1, p.2)) h

/-- Existential callback results allow different native/reference types. The
choice is proof-only: the executable result remains Generated.map_accum. -/
theorem button_map_accum_refines {T S B U V C F : Type}
    (clone : core.clone.Clone T)
    (fn : core.ops.function.Fn F (areas.ButtonZone × T × S) (B × S))
    (data : storage.ButtonVec T) (initial : S) (f : F)
    (input : T → U) (state : S → V) (output : B → C)
    (model : LnmaiCore.ButtonZone → U → V → C × V)
    (hclone : ∀ value, clone.clone value = ok value)
    (hcall : ∀ key value initial, ∃ result,
      fn.call f (key, value, initial) = ok result ∧
      (output result.1, state result.2) =
        model (toLnmButtonZone key) (input value) (state initial)) :
    ∃ out, storage.ButtonVec.map_accum clone fn data initial f = ok out ∧
      (decodeButtons output (toLnmButtonVec out.1), state out.2) =
        (decodeButtons input (toLnmButtonVec data)).mapAccum (state initial) model := by
  classical
  let native : LnmaiCore.ButtonZone → T → S → B × S :=
    fun key value initial => (hcall (ofLnmButtonZone key) value initial).choose
  have hnative : ∀ key value initial, fn.call f (key, value, initial) =
      ok (native (toLnmButtonZone key) value initial) := by
    intro key value initial
    simpa only [native, ofLnmButtonZone_toLnmButtonZone] using
      (hcall key value initial).choose_spec.1
  have hstep : ∀ key value initial,
      (output (native key value initial).1, state (native key value initial).2) =
        model key (input value) (state initial) := by
    intro key value initial
    simpa only [native, toLnmButtonZone_ofLnmButtonZone] using
      (hcall (ofLnmButtonZone key) value initial).choose_spec.2
  obtain ⟨out, hrun, hmodel⟩ :=
    button_map_accum_equiv clone fn data initial f native hclone hnative
  refine ⟨out, hrun, ?_⟩
  have h := congrArg (fun p : LnmaiCore.ButtonVec B × S =>
    (decodeButtons output p.1, state p.2)) hmodel
  exact h.trans (button_map_accum_natural input state output native model hstep
    (toLnmButtonVec data) initial)

end Verification.StorageRefinementProof
