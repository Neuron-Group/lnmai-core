import Verification.TouchHoldRebuildUpsert
import Verification.TouchHoldRebuildReverseExtracted

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 1000000

namespace Verification.TouchHoldRebuildTraversalProof

open TouchHoldRebuildModel TouchHoldGroupsProof TouchHoldRebuildUpsertProof
open TouchHoldRebuildReverseExtractedProof
open private increment_index from Verification.TouchHoldGroups

/-!
Real extraction-3 source body, terminating source loop, rebuild_from and rebuild.
The loop invariant threads the un-reversed accumulator through every original
source position, including duplicate notes and notes with no group. Reversal is
performed once after the entire original list, by the existing extracted swap
loop theorem. Both Scheduler and ChartLoader references are then obtained from
the already proved reference-model equations.

PrefixFits below contains ONLY group/member/trigger length inequalities for
each reference prefix. It asserts neither callback results nor an equivalence.
No opaque whole upsert, source traversal, initializer or reverse law is assumed.
SourceAtoms are four indexed source reads; UpdateAtoms and ReverseAtoms contain
the existing container point laws. The empty-buffer constructor remains one
explicit point-operation equation. Concrete Rust facades/allocation and their
representation instances are not extracted by this composition.

This file is an uncompiled source draft for the mainline's sole Lean worker.
-/

variable {S B Id Ops : Type}

/-- Source order and the complete original source length are retained. Numeric
IDs are represented independently of the Usize used to address a list slot. -/
structure SourceAtoms (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id)
    (ops : Ops) (rep : BodyRepresentation B Id) (source : S → Holds) : Prop where
  hold_length : ∀ holds, ∃ len, call.hold_length ops holds = ok len ∧
    (source holds).length = len.val
  hold_group_id : ∀ holds index pre area note rest,
    At (source holds) index pre (area, note) rest →
    ∃ id, call.hold_group_id ops holds index = ok id ∧
      id.map rep.id = note.touchHoldGroupId
  hold_note_index : ∀ holds index pre area note rest,
    At (source holds) index pre (area, note) rest →
    ∃ id, call.hold_note_index ops holds index = ok id ∧ rep.id id = note.params.noteIndex
  hold_triggered : ∀ holds index pre area note rest,
    At (source holds) index pre (area, note) rest →
    call.hold_triggered ops holds index = ok note.touchHoldGroupTriggered

/-- Exactly a finite family of capacity inequalities, including the empty
prefix. It does not constrain scan/upsert/traversal return values. -/
def PrefixFits (cap : BodyCapacity) (holds : Holds) (groups : Groups) : Prop :=
  ∀ pre rest, holds = pre ++ rest → BodyFits cap (walk pre groups)

theorem prefix_fits_here (cap : BodyCapacity) (holds : Holds) (groups : Groups)
    (hfit : PrefixFits cap holds groups) : BodyFits cap groups := by
  exact hfit [] holds rfl

theorem prefix_fits_tail (cap : BodyCapacity) (area : SensorArea) (note : HoldNote)
    (rest : Holds) (groups : Groups) (hfit : PrefixFits cap ((area, note) :: rest) groups) :
    PrefixFits cap rest (step note groups) := by
  intro pre suffix hsplit
  have hsource : (area, note) :: rest = ((area, note) :: pre) ++ suffix := by
    simp only [List.cons_append, hsplit]
  exact hfit ((area, note) :: pre) suffix hsource

theorem prefix_fits_step (cap : BodyCapacity) (area : SensorArea) (note : HoldNote)
    (rest : Holds) (groups : Groups) (hfit : PrefixFits cap ((area, note) :: rest) groups) :
    BodyFits cap (step note groups) :=
  prefix_fits_here cap rest (step note groups)
    (prefix_fits_tail cap area note rest groups hfit)

theorem prefix_fits_finish (cap : BodyCapacity) (holds : Holds) (groups : Groups)
    (hfit : PrefixFits cap holds groups) : BodyFits cap (walk holds groups) :=
  hfit holds [] (List.append_nil holds).symm

theorem body_fits_reverse (cap : BodyCapacity) (groups : Groups)
    (hfit : BodyFits cap groups) : BodyFits cap groups.reverse := by
  refine ⟨?_, ?_⟩
  · simpa only [List.length_reverse] using hfit.1
  · intro group hmem
    exact hfit.2 group (by simpa only [List.mem_reverse] using hmem)

variable (call : touch_hold_rebuild.TouchHoldRebuild Ops S B Id) (ops : Ops)
  (rep : BodyRepresentation B Id) (source : S → Holds) (cap : BodyCapacity)

/-- A false native guard returns immediately, with no source or group reads. -/
theorem rebuild_body_done (holds : S) (len : Usize) (groups : B) (index : Usize)
    (hstop : ¬index < len) :
    touch_hold_rebuild.rebuild_from_loop.body call holds ops len groups index =
      ok (.done groups) := by
  simp only [touch_hold_rebuild.rebuild_from_loop.body, hstop, ↓reduceIte]

/-- One real source iteration: group id first; note id and trigger only for
Some; then the proved real upsert, then the checked cursor increment. -/
theorem rebuild_body_step (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap) (holds : S) (len : Usize)
    (groups : B) (index : Usize) (pre : Holds) (area : SensorArea)
    (note : HoldNote) (rest : Holds)
    (hat : At (source holds) index pre (area, note) rest)
    (hmore : index.val < len.val)
    (hfit : BodyFits cap (step note (rep.groups groups))) :
    ∃ next nextIndex,
      touch_hold_rebuild.rebuild_from_loop.body call holds ops len groups index =
        ok (.cont (next, nextIndex)) ∧
      rep.groups next = step note (rep.groups groups) ∧ nextIndex.val = index.val + 1 := by
  have hguard : index < len := by simpa using hmore
  obtain ⟨nextIndex, hinc, hvalue⟩ := increment_index index len hmore
  obtain ⟨id, hread, hid⟩ := reads.hold_group_id holds index pre area note rest hat
  cases id with
  | none =>
      have hnone : note.touchHoldGroupId = none := hid.symm
      refine ⟨groups, nextIndex, ?_, ?_, hvalue⟩
      · simp only [touch_hold_rebuild.rebuild_from_loop.body, hguard, ↓reduceIte,
          hread, bind_tc_ok, hinc]
      · simp only [step, hnone]
  | some id =>
      have hsome : note.touchHoldGroupId = some (rep.id id) := hid.symm
      obtain ⟨noteId, hnote, hnoteRep⟩ := reads.hold_note_index holds index pre area note rest hat
      have htrigger := reads.hold_triggered holds index pre area note rest hat
      have hspace : BodyFits cap
          (upsert (rep.id id) (rep.id noteId) note.touchHoldGroupTriggered (rep.groups groups)) := by
        simpa only [step, hsome, hnoteRep] using hfit
      obtain ⟨next, hupsert, hnextRep⟩ := upsert_equiv call ops rep cap updates groups
        id noteId note.touchHoldGroupTriggered hspace
      refine ⟨next, nextIndex, ?_, ?_, hvalue⟩
      · simp only [touch_hold_rebuild.rebuild_from_loop.body, hguard, ↓reduceIte,
          hread, hnote, htrigger, hupsert, hinc, bind_tc_ok, uncurry_apply_pair]
      · simpa only [step, hsome, hnoteRep] using hnextRep

/-- Generalized loop theorem at any original-source prefix. Its natural measure
is len - index; no additional fuel, len + 1, or recursive Rust stack is used. -/
theorem rebuild_from_loop_spec (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap) (holds : S) (initial : B)
    (len start : Usize) (pre remaining : Holds)
    (hlen : (source holds).length = len.val)
    (hsource : source holds = pre ++ remaining) (hstart : pre.length = start.val)
    (hfit : PrefixFits cap remaining (rep.groups initial)) :
    touch_hold_rebuild.rebuild_from_loop call holds initial ops len start ⦃ out =>
      rep.groups out = walk remaining (rep.groups initial) ∧ BodyFits cap (rep.groups out) ⦄ := by
  unfold touch_hold_rebuild.rebuild_from_loop
  apply loop.spec_decr_nat
    (fun state : B × Usize => len.val - state.2.val)
    (fun state => ∃ past rest, source holds = past ++ rest ∧
      past.length = state.2.val ∧ PrefixFits cap rest (rep.groups state.1) ∧
      walk rest (rep.groups state.1) = walk remaining (rep.groups initial))
  · rintro ⟨groups, index⟩ ⟨past, rest, hsourceNow, hindex, hfitNow, hwalk⟩
    dsimp only at hsourceNow hindex hfitNow hwalk ⊢
    try simp only [uncurry_apply_pair]
    have hlength : len.val = past.length + rest.length := by
      rw [← hlen, hsourceNow, List.length_append]
    by_cases hguard : index < len
    · have hmore : index.val < len.val := by simpa using hguard
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons pair rest =>
          rcases pair with ⟨area, note⟩
          have hstepFit := prefix_fits_step cap area note rest (rep.groups groups) hfitNow
          obtain ⟨next, nextIndex, hbody, hnextRep, hnextIndex⟩ :=
            rebuild_body_step call ops rep source cap reads updates holds len groups index
              past area note rest ⟨hsourceNow, hindex⟩ hmore hstepFit
          have htailFit : PrefixFits cap rest (rep.groups next) := by
            rw [hnextRep]
            exact prefix_fits_tail cap area note rest (rep.groups groups) hfitNow
          have hnextWalk : walk rest (rep.groups next) = walk remaining (rep.groups initial) := by
            rw [hnextRep]
            exact hwalk
          apply exists_imp_spec
          refine ⟨.cont (next, nextIndex), hbody, ?_⟩
          dsimp only
          refine ⟨⟨past ++ [(area, note)], rest, ?_, ?_, htailFit, hnextWalk⟩, ?_⟩
          · simpa only [List.append_assoc, List.singleton_append] using hsourceNow
          · simp only [List.length_append, List.length_cons, List.length_nil, Nat.zero_add,
              hindex, hnextIndex]
          · omega
    · have hstop : len.val ≤ index.val := by simpa using hguard
      have hrest : rest = [] := by
        cases rest with
        | nil => rfl
        | cons item rest => simp only [List.length_cons] at hlength; omega
      have hout : rep.groups groups = walk remaining (rep.groups initial) := by
        simpa only [hrest, walk] using hwalk
      apply exists_imp_spec
      exact ⟨.done groups, rebuild_body_done call ops holds len groups index hguard,
        hout, prefix_fits_here cap rest (rep.groups groups) hfitNow⟩
  · exact ⟨pre, remaining, hsource, hstart, hfit, rfl⟩

/-- The actual wrapper reads the source length, consumes every slot from zero,
and finally calls the existing real reverse_groups theorem. -/
theorem rebuild_from_spec (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap)
    (reverse : ReverseAtoms call ops rep.groups cap.groups)
    (holds : S) (initial : B) (hfit : PrefixFits cap (source holds) (rep.groups initial)) :
    touch_hold_rebuild.rebuild_from call holds initial ops ⦃ out =>
      rep.groups out = rebuildFrom (source holds) (rep.groups initial) ∧
      BodyFits cap (rep.groups out) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := reads.hold_length holds
  obtain ⟨forward, hloop, hforward, hforwardFit⟩ := spec_imp_exists
    (rebuild_from_loop_spec call ops rep source cap reads updates holds initial len 0#usize
      [] (source holds) hlen (by simp) (by simp) hfit)
  obtain ⟨out, hreverse, hout, _, _⟩ :=
    reverse_groups_equiv call ops rep.groups cap.groups reverse forward hforwardFit.1
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, ?_⟩
  · simp only [touch_hold_rebuild.rebuild_from, hlength, hloop, hreverse, bind_tc_ok]
  · simpa only [rebuildFrom, indexedWalk_eq, hforward] using hout
  · rw [hout]
    exact body_fits_reverse cap (rep.groups forward) hforwardFit

/-- In particular the real pass returns ok, ruling out failure and divergence. -/
theorem rebuild_from_equiv (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap)
    (reverse : ReverseAtoms call ops rep.groups cap.groups)
    (holds : S) (initial : B) (hfit : PrefixFits cap (source holds) (rep.groups initial)) :
    ∃ out, touch_hold_rebuild.rebuild_from call holds initial ops = ok out ∧
      rep.groups out = rebuildFrom (source holds) (rep.groups initial) ∧
      BodyFits cap (rep.groups out) :=
  spec_imp_exists (rebuild_from_spec call ops rep source cap reads updates reverse holds initial hfit)

/-- Both original private recursions, with an arbitrary un-reversed starting
accumulator. Repeated IDs and all unchanged inner-list orders are included. -/
theorem rebuild_from_reference_equiv (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap)
    (reverse : ReverseAtoms call ops rep.groups cap.groups)
    (holds : S) (initial : B) (hfit : PrefixFits cap (source holds) (rep.groups initial)) :
    ∃ out, touch_hold_rebuild.rebuild_from call holds initial ops = ok out ∧
      rep.groups out = SchedulerReference.resume (source holds) (rep.groups initial) ∧
      rep.groups out = ChartLoaderReference.resume (source holds) (rep.groups initial) ∧
      BodyFits cap (rep.groups out) := by
  obtain ⟨out, hrun, hout, houtFit⟩ :=
    rebuild_from_equiv call ops rep source cap reads updates reverse holds initial hfit
  refine ⟨out, hrun, hout.trans (model_resume_eq_scheduler _ _), ?_, houtFit⟩
  calc
    rep.groups out = rebuildFrom (source holds) (rep.groups initial) := hout
    _ = (walk (source holds) (rep.groups initial)).reverse := by
      rw [rebuildFrom, indexedWalk_eq]
    _ = ChartLoaderReference.resume (source holds) (rep.groups initial) :=
      (ChartLoaderReference.resume_eq_walk _ _).symm

/-- Empty initialization is a concrete constructor point equation. It does not
assume anything about the full scheduler/chart-loader initializer. -/
theorem rebuild_spec (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap)
    (reverse : ReverseAtoms call ops rep.groups cap.groups)
    (hempty : ∃ initial, call.empty ops = ok initial ∧ rep.groups initial = [])
    (holds : S) (hfit : PrefixFits cap (source holds) []) :
    touch_hold_rebuild.rebuild call holds ops ⦃ out =>
      rep.groups out = rebuild (source holds) ∧ BodyFits cap (rep.groups out) ⦄ := by
  obtain ⟨initial, hinit, hinitRep⟩ := hempty
  obtain ⟨out, hrun, hout, houtFit⟩ := rebuild_from_equiv call ops rep source cap
    reads updates reverse holds initial (by simpa only [hinitRep] using hfit)
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, houtFit⟩
  · simp only [touch_hold_rebuild.rebuild, hinit, hrun, bind_tc_ok]
  · simpa only [rebuild, hinitRep] using hout

/-- Complete real rebuild connected simultaneously to the two original helpers.
Only source/container/representation/allocator point boundaries remain. -/
theorem rebuild_equiv (reads : SourceAtoms call ops rep source)
    (updates : UpdateAtoms call ops rep cap)
    (reverse : ReverseAtoms call ops rep.groups cap.groups)
    (hempty : ∃ initial, call.empty ops = ok initial ∧ rep.groups initial = [])
    (holds : S) (hfit : PrefixFits cap (source holds) []) :
    ∃ out, touch_hold_rebuild.rebuild call holds ops = ok out ∧
      rep.groups out = SchedulerReference.reference (source holds) ∧
      rep.groups out = ChartLoaderReference.reference (source holds) ∧
      BodyFits cap (rep.groups out) := by
  obtain ⟨out, hrun, hout, houtFit⟩ := spec_imp_exists
    (rebuild_spec call ops rep source cap reads updates reverse hempty holds hfit)
  exact ⟨out, hrun, hout.trans (model_rebuild_eq_scheduler _),
    hout.trans (model_rebuild_eq_chartLoader _), houtFit⟩

end Verification.TouchHoldRebuildTraversalProof
