import Verification.TouchAutomaticQueue
import Verification.TouchHoldGroups

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

namespace Verification.TouchAutomaticGroupsProof
open TouchAutomaticModel TouchAutomaticProof
open private touchQueueResolved advanceSharedTouchQueue from LnmaiCore.Scheduler

/-! Install the extracted first-match group lookup in the automatic Touch
traversal. The groups are the fixed post-head snapshot used by the runtime.
Together with TouchAutomaticQueue this removes the whole shared-result,
lifecycle-step and queue-normalization premises. Native field/container and
numeric operations, capacity, and concrete facade wiring remain explicit. -/

def shared {N G Id Grade D GOps : Type}
    (noteGroup : N → Result (Option Id))
    (call : touch_hold_schedule.HeadGroups GOps G Id Grade D)
    (groupOps : GOps) (groups : G) (note : N) : Result (Option (Grade × D)) := do
  let id ← noteGroup note
  match id with
  | none => ok none
  | some id => touch_hold_schedule.shared_result call groups id groupOps

def installShared {S N Notes F E Event D Grade Ops G Id GOps : Type}
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D (Grade × D))
    (noteGroup : N → Result (Option Id))
    (call : touch_hold_schedule.HeadGroups GOps G Id Grade D)
    (groupOps : GOps) (groups : G) :
    touch_automatic.TouchAutomatic Ops S N Notes F E Event D (Grade × D) :=
  { base with shared := fun _ note => shared noteGroup call groupOps groups note }

structure GroupFields {S N Notes F E Event D Grade G Id : Type}
    (rep : Representation S N Notes F E Event D (Grade × D))
    (groupRep : TouchHoldGroupsProof.HeadRepresentation G Id Grade D)
    (noteGroup : N → Result (Option Id)) : Prop where
  note_group : ∀ note, ∃ id, noteGroup note = ok id ∧
    id.map groupRep.id = (rep.note note).touchGroupId
  pair : ∀ pair, rep.shared pair = (groupRep.grade pair.1, groupRep.duration pair.2)

theorem shared_spec {S N Notes F E Event D Grade G Id GOps : Type}
    (rep : Representation S N Notes F E Event D (Grade × D))
    (groupRep : TouchHoldGroupsProof.HeadRepresentation G Id Grade D)
    (noteGroup : N → Result (Option Id))
    (call : touch_hold_schedule.HeadGroups GOps G Id Grade D)
    (groupOps : GOps) (groups : G) (ctx : Context) (capacity : Nat)
    (fields : GroupFields rep groupRep noteGroup)
    (atoms : TouchHoldGroupsProof.HeadAtoms call groupOps groupRep capacity)
    (hgroups : groupRep.groups groups = ctx.groups) (note : N) :
    ∃ out, shared noteGroup call groupOps groups note = ok out ∧
      out.map rep.shared = TouchAutomaticModel.shared ctx (rep.note note) := by
  obtain ⟨id, hid, hgroup⟩ := fields.note_group note
  cases id with
  | none =>
      refine ⟨none, ?_, ?_⟩
      · simp only [shared, hid, bind_tc_ok]
      · simp only [TouchAutomaticModel.shared, ← hgroup, Option.map_none]
  | some id =>
      obtain ⟨out, hout, hrep⟩ := spec_imp_exists
        (TouchHoldGroupsProof.shared_result_equiv call groupOps groupRep capacity
          atoms groups id)
      refine ⟨out, ?_, ?_⟩
      · simp only [shared, hid, bind_tc_ok, hout]
      · have hpairs : out.map rep.shared =
            out.map (fun pair => (groupRep.grade pair.1, groupRep.duration pair.2)) := by
          cases out <;> simp only [Option.map_none, Option.map_some, fields.pair]
        rw [hpairs]
        simpa only [TouchAutomaticModel.shared, ← hgroup, Option.map_some, hgroups] using hrep

/-- The remaining automatic traversal primitives, excluding both composed
operations shared and step. No complete-slot or pass equation is assumed. -/
structure OtherAtoms {S N Notes F E Event D Grade Ops : Type}
    (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D (Grade × D))
    (ops : Ops) (rep : Representation S N Notes F E Event D (Grade × D))
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ source, ∃ len, call.length ops source = ok len ∧
    (rep.source source).notes.length = len.val
  get : ∀ source (index : Usize) (h : index.val < (rep.source source).notes.length),
    ∃ note, call.get ops source index = ok note ∧
      rep.note note = (rep.source source).notes[index.val]'h
  before_cursor : ∀ source index, call.before_cursor ops source index =
    ok (decide (index.val < (rep.source source).currentIndex))
  resolved : ∀ note, call.resolved ops note = ok (touchQueueResolved (rep.note note))
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = difference ctx (rep.note note)
  too_late : ∀ note, call.too_late ops note = ok (tooLate ctx (rep.note note))
  advance_frontier : ∀ frontiers, ∃ next, call.advance_frontier ops frontiers = ok next ∧
    rep.frontiers next = advanceSharedTouchQueue (rep.frontiers frontiers) ctx.area
  prepend_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.prepend_event ops events event = ok next ∧
      rep.events next = rep.event event :: rep.events events
  append_note : ∀ notes note, (rep.notes notes).length + 1 ≤ cap.notes →
    ∃ next, call.append_note ops notes note = ok next ∧
      rep.notes next = rep.notes notes ++ [rep.note note]

theorem remaining_atoms_of_groups {S N Notes F E Event D Grade Ops G Id GOps : Type}
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D (Grade × D))
    (ops : Ops) (rep : Representation S N Notes F E Event D (Grade × D))
    (ctx : Context) (cap : Capacity)
    (noteGroup : N → Result (Option Id))
    (call : touch_hold_schedule.HeadGroups GOps G Id Grade D)
    (groupOps : GOps) (groupRep : TouchHoldGroupsProof.HeadRepresentation G Id Grade D)
    (groups : G) (groupCapacity : Nat)
    (fields : GroupFields rep groupRep noteGroup)
    (groupAtoms : TouchHoldGroupsProof.HeadAtoms call groupOps groupRep groupCapacity)
    (hgroups : groupRep.groups groups = ctx.groups)
    (other : OtherAtoms base ops rep ctx cap) :
    TouchAutomaticJudgmentsProof.RemainingAtoms
      (installShared base noteGroup call groupOps groups) ops rep ctx cap := by
  refine { length := other.length, get := other.get, before_cursor := other.before_cursor
           resolved := other.resolved, sensor_diff := other.sensor_diff
           too_late := other.too_late, advance_frontier := other.advance_frontier
           prepend_event := other.prepend_event, append_note := other.append_note, shared := ?_ }
  intro note
  exact shared_spec rep groupRep noteGroup call groupOps groups ctx groupCapacity
    fields groupAtoms hgroups note

/-- Actual automatic traversal, extracted sharing, extracted judgments and
extracted queue normalization composed in the same order as scheduler.rs. -/
theorem process_equiv {S N Notes F E Event D Grade Ops QOps G Id GOps I J : Type}
    (inst : judgment_adapter.IntegerOps J I) (integerOps : J) (decode : I → ℤ)
    (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
    (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D (Grade × D))
    (ops : Ops) (rep : Representation S N Notes F E Event D (Grade × D))
    (pack : TouchAutomaticJudgmentsProof.OutputEncoding N Event)
    (ctx : Context) (cap : Capacity)
    (ready : TouchAutomaticJudgmentsProof.EncodingReady inst integerOps decode encode rep pack ctx)
    (other : OtherAtoms base ops rep ctx cap)
    (noteGroup : N → Result (Option Id))
    (call : touch_hold_schedule.HeadGroups GOps G Id Grade D)
    (groupOps : GOps) (groupRep : TouchHoldGroupsProof.HeadRepresentation G Id Grade D)
    (groups : G) (groupCapacity : Nat)
    (fields : GroupFields rep groupRep noteGroup)
    (groupAtoms : TouchHoldGroupsProof.HeadAtoms call groupOps groupRep groupCapacity)
    (hgroups : groupRep.groups groups = ctx.groups)
    (setNotes : S → Notes → Result S)
    (setNotesLaw : ∀ source notes, (rep.notes notes).length ≤ cap.notes →
      ∃ installed, setNotes source notes = ok installed ∧
        rep.source installed = { rep.source source with notes := rep.notes notes })
    (facade : QueueMaintenanceComposition.Facade QOps S)
    (cursor : queue_maintenance.QueueCursor QOps S N) (queueOps : QOps)
    (facadePoints : QueueMaintenanceComposition.FacadeAtomicAgrees facade queueOps
      (TouchAutomaticQueueProof.queueRep rep) cap.notes)
    (cursorPoints : QueueMaintenanceProof.CursorAtomicAgrees cursor queueOps
      (TouchAutomaticQueueProof.queueRep rep) touchQueueResolved cap.notes)
    (source : S) (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (frontiers : SensorVec Nat) (events : List JudgeEvent)
    (hinit : interpret rep initial = ⟨[], frontiers, events⟩)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes
      ⟨[], frontiers, events⟩)) :
    TouchAutomaticQueueProof.process
      (TouchAutomaticJudgmentsProof.installStep inst integerOps decode encode
        (installShared base noteGroup call groupOps groups) rep pack ctx)
      ops setNotes facade cursor queueOps source initial ⦃ out =>
      (rep.source out.1, rep.frontiers out.2.1, ctx.groups, rep.events out.2.2) =
        reference ctx (rep.source source) frontiers events ⦄ := by
  exact TouchAutomaticQueueProof.process_judgments_equiv
    inst integerOps decode integerLaw encode decode_encode
    (installShared base noteGroup call groupOps groups) ops rep pack ctx cap ready
    (remaining_atoms_of_groups base ops rep ctx cap noteGroup call groupOps groupRep
      groups groupCapacity fields groupAtoms hgroups other)
    setNotes setNotesLaw facade cursor queueOps facadePoints cursorPoints
    source initial frontiers events hinit hfit

end Verification.TouchAutomaticGroupsProof
