import Verification.TouchHoldBodyAdapter
import Verification.GeneratedTouchHoldBodyAdapter
import Verification.TouchHoldScheduleTraversal

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TouchHoldScheduleAdapterProof

open TouchHoldGroupsProof TouchHoldScheduleProof TouchHoldScheduleModel

/-! Actual extracted wrapper dictionaries for the complete scheduling pass.
The wrapper forwards head and lifecycle operations without transforming their
arguments. Its entire body-storage contract is derived from actual Vec/record
proofs, including strict majority and first erasure. Other runtime field, numeric,
lifecycle and queue laws and the visited-prefix bounds remain explicit. -/

variable {S A N F Q C H E Event D Z Id Grade G B Ops : Type}

abbrev Bodies (Id : Type) := TouchHoldBodyReadsProof.Groups Id

abbrev headCall (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D) :=
  touch_hold_body_adapter.TouchHoldBodyAdapter.Insts.Lnmai_core_verifyTouch_hold_scheduleHeadGroups
    heads

abbrev scheduleCall
    (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade) :=
  touch_hold_body_adapter.TouchHoldBodyAdapter.Insts.Lnmai_core_verifyTouch_hold_scheduleTouchHoldSchedule
    call

theorem head_atoms (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (ops : Ops) (rep : HeadRepresentation G Id Grade D) (capacity : Nat)
    (atoms : HeadAtoms heads ops rep capacity) :
    HeadAtoms (headCall heads) { ops := ops } rep capacity := by
  exact ⟨atoms.length, atoms.matches, atoms.majority, atoms.result,
    atoms.increment, atoms.set_size, atoms.set_result, atoms.append⟩

theorem schedule_atoms
    (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
    (ops : Ops) (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (ctx : Context) (cap : Capacity) (atoms : AtomicAgrees call ops rep ctx cap) :
    AtomicAgrees (scheduleCall call) { ops := ops } rep ctx cap := by
  exact ⟨atoms.length, atoms.get, atoms.sensor_diff, atoms.button_diff,
    atoms.local_pressed, atoms.body_check_active, atoms.body_group_id,
    atoms.head_group_id, atoms.note_index, atoms.group_size, atoms.phase,
    atoms.head_grade, atoms.head_diff, atoms.grade_rejected, atoms.within_head_window,
    atoms.unlocked, atoms.eligible, atoms.button_ring, atoms.outer_button,
    atoms.button_available, atoms.sensor_available, atoms.use_button, atoms.use_sensor,
    atoms.step, atoms.advance_frontier, atoms.update_queue, atoms.append_active,
    atoms.append_event⟩

theorem body_atoms_for (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (ops : Ops)
    (rep : Representation S A N F Q C H E Event D Z Id Grade G (Bodies Id))
    (cap : Capacity)
    (hview : rep.bodyGroups = TouchHoldBodyReadsProof.viewGroups rep.id)
    (hcapacity : cap.bodies = TouchHoldBodyAdapterProof.capacity)
    (heq : ∀ left right, eq.eq left right = ok (rep.id left == rep.id right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler) :
    BodyAtoms (TouchHoldBodyAdapterProof.bodyCall (Ops := Ops) eq cloneInst defaultInst)
      { ops := ops } (bodyRep rep) cap.bodies := by
  have hrep : bodyRep rep = TouchHoldBodyAdapterProof.representation rep.id := by
    simp only [bodyRep, TouchHoldBodyAdapterProof.representation, hview]
  rw [hrep, hcapacity]
  exact TouchHoldBodyAdapterProof.body_atoms
    eq cloneInst defaultInst { ops := ops } rep.id heq hclone filler hdefault

/-- The actual Rust wrapper runs the complete pass. Body algorithms, storage
updates, majority and search are conclusions of the imported proofs. -/
theorem process_spec
    (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
    (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (eq : core.cmp.PartialEq Id Id) (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (ops : Ops)
    (rep : Representation S A N F Q C H E Event D Z Id Grade G (Bodies Id))
    (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (hview : rep.bodyGroups = TouchHoldBodyReadsProof.viewGroups rep.id)
    (hcapacity : cap.bodies = TouchHoldBodyAdapterProof.capacity)
    (heq : ∀ left right, eq.eq left right = ok (rep.id left == rep.id right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G (Bodies Id))
    (hsafe : Safe ctx cap (rep.holds holds) (interpret rep initial)) :
    touch_hold_schedule.process (scheduleCall call) (headCall heads)
      (TouchHoldBodyAdapterProof.bodyCall eq cloneInst defaultInst)
      holds initial { ops := ops } ⦃ out =>
        interpret rep out = resume ctx (rep.holds holds) (interpret rep initial) ⦄ := by
  exact TouchHoldScheduleTraversalProof.process_spec
    (scheduleCall call) (headCall heads)
    (TouchHoldBodyAdapterProof.bodyCall eq cloneInst defaultInst) { ops := ops }
    rep ctx cap (schedule_atoms call ops rep ctx cap atoms)
    (head_atoms heads ops (headRep rep) cap.heads headAtoms)
    (body_atoms_for eq cloneInst defaultInst ops rep cap hview hcapacity
      heq hclone filler hdefault) holds initial hsafe

/-- All seven outputs agree with the original Lean scheduler's TouchHold pass.
The statement preserves the finite prefix capacities and the non-body primitive
contracts instead of treating the runtime facade as an unconditional oracle. -/
theorem process_equiv
    (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
    (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (eq : core.cmp.PartialEq Id Id) (cloneInst : core.clone.Clone Id)
    (defaultInst : core.default.Default Id) (ops : Ops)
    (rep : Representation S A N F Q C H E Event D Z Id Grade G (Bodies Id))
    (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (hview : rep.bodyGroups = TouchHoldBodyReadsProof.viewGroups rep.id)
    (hcapacity : cap.bodies = TouchHoldBodyAdapterProof.capacity)
    (heq : ∀ left right, eq.eq left right = ok (rep.id left == rep.id right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G (Bodies Id))
    (frontiers : SensorVec Nat) (queues : SensorQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) (groups : List GroupState)
    (bodyGroups : List TouchHoldBodyGroupState)
    (hinit : interpret rep initial = ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩)
    (hsafe : Safe ctx cap (rep.holds holds)
      ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩) :
    touch_hold_schedule.process (scheduleCall call) (headCall heads)
      (TouchHoldBodyAdapterProof.bodyCall eq cloneInst defaultInst)
      holds initial { ops := ops } ⦃ out =>
        (interpret rep out).output =
          reference ctx (rep.holds holds) frontiers queues cursor groups bodyGroups ⦄ := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (process_spec call heads eq cloneInst defaultInst ops rep ctx cap atoms headAtoms
      hview hcapacity heq hclone filler hdefault holds initial
      (by simpa only [hinit] using hsafe))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  rw [hout, hinit, resume_original]

end Verification.TouchHoldScheduleAdapterProof
