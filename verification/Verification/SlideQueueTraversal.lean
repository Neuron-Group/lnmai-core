import Verification.SlideQueue

namespace Verification.SlideQueueProof
open Aeneas Aeneas.Std Result WP lnmai_core_verify
open StorageProof SlideAreaProof

set_option maxHeartbeats 2000000

/-- The actual extracted recursion returns the same remaining checkpoints and
    ordered progress values as the reference traversal, for every fuel value.
    Cloning must preserve progress; no history-alignment invariant is needed. -/
theorem advance_fuel_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (fuel : Std.Usize) (emit : Bool)
    (queue : Slice (lifecycle.SlideArea P)) (held : storage.SensorVec Bool)
    (hclone : ∀ x, clone.clone x = ok x) :
    ∃ out, slide_queue.advance_fuel clone fuel emit queue held = ok out ∧
      (toLnmQueue progress out.1, out.2.val.map progress) =
        advanceModel fuel.val emit (toLnmQueue progress queue) (toLnmSensorVec held) := by
  generalize hn : fuel.val = n
  induction n generalizing fuel queue with
  | zero =>
      have hf : fuel = 0#usize := by scalar_tac
      have hcopy : alloc.slice.Slice.to_vec
          (lifecycle.SlideArea.Insts.CoreCloneClone clone) queue = ok queue := by
        simpa only [storage.clone_values] using
          StorageProof.clone_values_equiv (lifecycle.SlideArea.Insts.CoreCloneClone clone)
            queue (fun x => clone_equiv clone x hclone)
      rw [slide_queue.advance_fuel]
      cases hq : queue.val with
      | nil =>
          refine ⟨(alloc.vec.Vec.new _, alloc.vec.Vec.new _), ?_, ?_⟩
          · simp [UScalar.eq_equiv, hq]
          · simp [toLnmQueue, alloc.vec.Vec.new, hq, advanceModel]
      | cons first tail =>
          refine ⟨(queue, alloc.vec.Vec.new _), ?_, ?_⟩
          · simp [UScalar.eq_equiv, hq, hf, hcopy, bind_tc_ok]
            rfl
          · simp [toLnmQueue, alloc.vec.Vec.new, hq, advanceModel]
  | succ n ih =>
      cases hq : queue.val with
      | nil =>
          refine ⟨(alloc.vec.Vec.new _, alloc.vec.Vec.new _), ?_, ?_⟩
          · rw [slide_queue.advance_fuel]
            simp [UScalar.eq_equiv, hq]
          · simp [toLnmQueue, alloc.vec.Vec.new, hq, advanceModel]
      | cons first tail =>
          have hf : fuel ≠ 0#usize := by scalar_tac
          have hqueueNonempty : Slice.len queue ≠ 0#usize := by
            simp [UScalar.eq_equiv, hq]
          obtain ⟨fuel1, hsub, hf1, _⟩ := spec_imp_exists
            (Usize.sub_spec (x := fuel) (y := 1#usize) (by scalar_tac))
          have hfn : fuel1.val = n := by scalar_tac
          have hnmax : n < Std.Usize.max := by scalar_tac
          obtain ⟨rest, hindex, hrest, hr⟩ := uncons_equiv queue first tail hq
          obtain ⟨checked, hcheck, hc⟩ := check_equiv clone progress first held hclone
          have hrbound : rest.val.length < Std.Usize.max := by
            have := queue.property
            simp only [hq, List.length_cons] at this
            rw [hr]
            omega
          obtain ⟨kept, hkept, hk⟩ := with_head_equiv clone checked rest hclone hrbound
          cases ht : tail with
          | nil =>
              obtain ⟨finished, hfinished, hfin⟩ := prepend_progress_equiv clone emit
                checked.arrow_progress_when_finished (alloc.vec.Vec.new P) hclone (by scalar_tac)
              obtain ⟨pressed, hpressed, hpress⟩ := prepend_progress_equiv clone emit
                checked.arrow_progress_when_on (alloc.vec.Vec.new P) hclone (by scalar_tac)
              clear ih
              have hqueueModel : toLnmQueue progress queue = [toLnmSlideArea progress first] := by
                simp only [toLnmQueue, hq, ht, List.map_cons, List.map_nil]
              have hrestEmpty : Slice.len rest = 0#usize := by
                simp [UScalar.eq_equiv, hr, ht]
              rw [slide_queue.advance_fuel]
              rw [hqueueModel, advanceModel, ← hc]
              simp only [hqueueNonempty, hf, ↓reduceIte, hsub, hindex, hcheck, hrest,
                hrestEmpty, is_finished_equiv progress, on_equiv progress,
                Std.Array.to_slice, Std.Array.empty, lift, hfinished, hpressed, hkept,
                Bind.bind, Aeneas.Std.bind]
              cases hfinished' : (toLnmSlideArea progress checked).isFinished <;>
                cases hon : (toLnmSlideArea progress checked).on <;>
                cases emit <;>
                simp [UScalar.eq_equiv, hq, ht, hf, hsub, hindex, hcheck, hrest, hr,
                  is_finished_equiv progress, on_equiv progress, hfinished', hon,
                  hfinished, hpressed, hkept, hn,
                  Std.Array.to_slice, Std.Array.empty, lift, Bind.bind, Aeneas.Std.bind] <;>
                simp [toLnmQueue, hk, hr, ht, hfin, hpress,
                  alloc.vec.Vec.new, toLnmSlideArea]
          | cons second tail2 =>
              obtain ⟨rest2, hindex2, hrest2, hr2⟩ := uncons_equiv rest second tail2
                (by simpa only [ht] using hr)
              obtain ⟨checked2, hcheck2, hc2⟩ := check_equiv clone progress second held hclone
              have hr2bound : rest2.val.length < Std.Usize.max := by
                rw [hr, ht] at hrbound
                rw [hr2]
                simp only [List.length_cons] at hrbound
                omega
              obtain ⟨next, hnext, hnextval⟩ := with_head_equiv clone checked2 rest2 hclone hr2bound
              have hnextbound : next.val.length < Std.Usize.max := by
                rw [hnextval, hr2]
                simpa only [hr, ht, List.length_cons] using hrbound
              obtain ⟨both, hboth, hbothval⟩ := with_head_equiv clone checked next hclone hnextbound
              obtain ⟨out2, hout2, ho2⟩ := ih fuel1 rest2 hfn
              obtain ⟨outNext, houtNext, hoNext⟩ := ih fuel1 next hfn
              obtain ⟨outRest, houtRest, hoRest⟩ := ih fuel1 rest hfn
              have hb2 := (result_bounds progress n emit rest2 held out2 ho2).2
              have hbNext := (result_bounds progress n emit next held outNext hoNext).2
              have hbRest := (result_bounds progress n emit rest held outRest hoRest).2
              obtain ⟨cmd2, hcmd2, hcmd2val⟩ := prepend_progress_equiv clone emit
                checked2.arrow_progress_when_finished out2.2 hclone (by omega)
              obtain ⟨cmdNext, hcmdNext, hcmdNextval⟩ := prepend_progress_equiv clone emit
                checked2.arrow_progress_when_on outNext.2 hclone (by omega)
              obtain ⟨cmdFirst, hcmdFirst, hcmdFirstval⟩ := prepend_progress_equiv clone emit
                checked.arrow_progress_when_finished outNext.2 hclone (by omega)
              obtain ⟨cmdRest, hcmdRest, hcmdRestval⟩ := prepend_progress_equiv clone emit
                checked.arrow_progress_when_finished outRest.2 hclone (by omega)
              have ho2q := congrArg Prod.fst ho2
              have ho2c := congrArg Prod.snd ho2
              have hoNextq := congrArg Prod.fst hoNext
              have hoNextc := congrArg Prod.snd hoNext
              have hoRestq := congrArg Prod.fst hoRest
              have hoRestc := congrArg Prod.snd hoRest
              dsimp only at ho2q ho2c hoNextq hoNextc hoRestq hoRestc
              clear ih ho2 hoNext hoRest
              have hqueueModel : toLnmQueue progress queue =
                  toLnmSlideArea progress first :: toLnmSlideArea progress second ::
                    tail2.map (toLnmSlideArea progress) := by
                simp only [toLnmQueue, hq, ht, List.map_cons]
              have hrestNonempty : Slice.len rest ≠ 0#usize := by
                simp [UScalar.eq_equiv, hr, ht]
              cases out2
              cases outNext
              cases outRest
              rw [slide_queue.advance_fuel]
              rw [hqueueModel, advanceModel, ← hc, ← hc2]
              simp only [hqueueNonempty, hf, ↓reduceIte, hsub, hindex, hcheck, hrest,
                hrestNonempty, hindex2, hcheck2, hrest2,
                is_finished_equiv progress, on_equiv progress,
                clone_equiv clone _ hclone, alloc.vec.Vec.deref,
                hkept, hnext, hboth, hout2, houtNext, houtRest,
                hcmd2, hcmdNext, hcmdFirst, hcmdRest, Bind.bind, Aeneas.Std.bind]
              cases hskip : checked.is_skippable <;>
                cases hon : (toLnmSlideArea progress checked).on <;>
                cases hfinished' : (toLnmSlideArea progress checked).isFinished <;>
                cases hon2 : (toLnmSlideArea progress checked2).on <;>
                cases hfinished2 : (toLnmSlideArea progress checked2).isFinished <;>
                cases emit <;>
                simp [UScalar.eq_equiv, hq, ht, hf, hsub, hindex, hcheck, hrest, hr,
                  hindex2, hcheck2, hrest2, hr2, hskip, hon, hfinished', hon2, hfinished2,
                  is_finished_equiv progress, on_equiv progress,
                  clone_equiv clone _ hclone, alloc.vec.Vec.deref,
                  hkept, hnext, hboth, hout2, houtNext, houtRest,
                  hcmd2, hcmdNext, hcmdFirst, hcmdRest, hn, Bind.bind, Aeneas.Std.bind] <;>
                (try simp only [hcmd2val, hcmdNextval, hcmdFirstval, hcmdRestval,
                  Bool.false_eq_true, ↓reduceIte, List.map_cons, List.map_nil]) <;>
                (try simp only [ho2q, ho2c, hoNextq, hoNextc, hoRestq, hoRestc]) <;>
                simp [toLnmQueue, hk, hnextval, hbothval, hr, ht, hr2,
                  toLnmSlideArea, alloc.vec.Vec.new, hskip]

theorem advance_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (emit : Bool) (queue : Slice (lifecycle.SlideArea P))
    (held : storage.SensorVec Bool) (hclone : ∀ x, clone.clone x = ok x)
    (hbound : queue.val.length < Std.Usize.max) :
    ∃ out, slide_queue.advance clone emit queue held = ok out ∧
      (toLnmQueue progress out.1, out.2.val.map progress) =
        advanceModel (queue.val.length + 1) emit
          (toLnmQueue progress queue) (toLnmSensorVec held) := by
  obtain ⟨fuel, hsum, hfuel⟩ := spec_imp_exists
    (Usize.add_spec (x := Slice.len queue) (y := 1#usize) (by scalar_tac))
  obtain ⟨out, hout, heq⟩ := advance_fuel_equiv clone progress fuel emit queue held hclone
  refine ⟨out, by simp only [slide_queue.advance, hsum, bind_tc_ok, hout], ?_⟩
  simpa [hfuel] using heq

open private slideQueueCore slideQueueCoreFuel slideHideBarCmd from LnmaiCore.Lifecycle

/-- Attach routing identifiers to the extracted progress values. This is the
    exact command list of the original queue operation, in emission order. -/
theorem advance_render_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (emit : Bool) (queue : Slice (lifecycle.SlideArea P))
    (held : storage.SensorVec Bool) (note : Nat) (track : Option Nat)
    (hclone : ∀ x, clone.clone x = ok x) (hbound : queue.val.length < Std.Usize.max) :
    ∃ out, slide_queue.advance clone emit queue held = ok out ∧
      (toLnmQueue progress out.1,
        out.2.val.map (fun p => slideHideBarCmd note track (progress p))) =
      slideQueueCore note track emit (toLnmQueue progress queue) (toLnmSensorVec held) := by
  obtain ⟨out, hout, heq⟩ := advance_equiv clone progress emit queue held hclone hbound
  refine ⟨out, hout, ?_⟩
  have mapped := congrArg (fun pair : LnmaiCore.Lifecycle.SlideQueue × List Nat =>
    (pair.1, pair.2.map (slideHideBarCmd note track))) heq
  dsimp only at mapped
  rw [advance_model_equiv] at mapped
  simpa only [slideQueueCore, toLnmQueue, List.length_map, List.map_map, Function.comp_def]
    using mapped

theorem advance_replay_equiv {P : Type} (clone : core.clone.Clone P)
    (progress : P → Nat) (queue : Slice (lifecycle.SlideArea P))
    (held : storage.SensorVec Bool) (hclone : ∀ x, clone.clone x = ok x)
    (hbound : queue.val.length < Std.Usize.max) :
    ∃ out, slide_queue.advance clone false queue held = ok out ∧
      toLnmQueue progress out.1 =
        LnmaiCore.Lifecycle.replaySlideQueue (toLnmQueue progress queue) (toLnmSensorVec held) ∧
      out.2.val = [] := by
  obtain ⟨out, hout, heq⟩ := advance_render_equiv clone progress false queue held
    0 none hclone hbound
  obtain ⟨out', hout', heq'⟩ := advance_equiv clone progress false queue held hclone hbound
  have houtEq : out' = out := by rw [hout] at hout'; exact (Result.ok.inj hout').symm
  subst out'
  refine ⟨out, hout, congrArg Prod.fst heq, ?_⟩
  have hcommands := congrArg Prod.snd heq'
  dsimp only at hcommands
  rw [advance_model_no_commands] at hcommands
  simpa only [List.map_eq_nil_iff] using hcommands

end Verification.SlideQueueProof
