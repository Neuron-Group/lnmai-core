import Verification.TouchSchedulePass
import Verification.TouchScheduleJudgments
import Verification.TouchAutomaticGroups

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TouchSchedulePassJudgmentsProof

open TouchSchedulePassProof
open private touchQueueResolved from LnmaiCore.Scheduler

/-!
UNVALIDATED SOURCE: no Lean/lake, Cargo, test or extraction was run.

Install judgment/lifecycle and extracted group operations into ONE
TouchSchedulePassProof.Implementation. Its automatic closure accepts the
actual post-head groups argument; it never closes over the initial groups.
Head and automatic share the same integer instance, note/event encoders,
group representation and per-area head-group point implementation.

Agrees.headGroups/headLifecycle and automatic.shared/step are derived. The
remaining assumptions are scheduler/container point laws, bounded head-group
point laws and GroupRoom, successful output-field encoding, exact integer
operations/encoding, and the existing pass Bounds. Neither Agrees nor a
whole queue-slot/pass equality is supplied by the caller. The sensor traversal
and final single event reverse are reused from TouchSchedulePass.

API snapshot read on 2026-10-17 (mainline owns subsequent compilation/repair):
  Verification/TouchSchedulePass.lean
    d0562ce4095f9271cbdfa0a4a211cc6a91444227310b9282b421cfe227f83525
  Verification/TouchAutomaticGroups.lean
    e04984bf56dfbec35d0e48a02f52bc93fcde7359379a99cc396f2d8731db0246
  Verification/TouchScheduleJudgments.lean
    99c571b98e15754cd4a2893b8431d64b9b8e2928a3039da8bae2bf93de3842a6
  Verification/TouchAutomaticJudgments.lean
    af7454c4546cfdd4ec62cca7fcc93257f104e04fd33c1ffd0a8359e4ada17e56
-/

/-- Reuse exactly the same field encoders in both phases. -/
def automaticEncoding {N Event : Type}
    (pack : TouchScheduleJudgmentsProof.OutputEncoding N Event) :
    TouchAutomaticJudgmentsProof.OutputEncoding N Event :=
  ⟨pack.note, pack.event⟩

/-- The already-required head group-id point read supplies the automatic
lookup's input. This adds no independent note-group encoding premise. -/
def noteGroup (base : Implementation) (area : areas.SensorArea) :
    base.N → Result (Option base.Id) :=
  (base.head area).group_id (base.headOps area)

/-- All remaining Agrees fields. The automatic field contains only the nine
primitive laws in OtherAtoms, explicitly excluding shared and step. -/
structure RemainingAtoms (base : Implementation) (ctx : Context) : Prop where
  clone : ∀ queue, base.clone.clone queue = ok queue
  fuel : ∀ queue, ∃ fuel, base.fuel queue = ok fuel ∧
    fuel.val = TouchScheduleModel.remaining (base.rep.queue queue) + 1
  headPoints : ∀ area, TouchScheduleProof.PointAgrees (base.head area) base.headCursor
    base.headReplace (base.headOps area) base.rep
    (headContext ctx (Areas.toLnmSensorArea area)) base.headCapacity
  automaticPoints : ∀ area groups, TouchAutomaticGroupsProof.OtherAtoms
    (base.automatic area groups) (base.automaticOps area groups) base.automaticRep
    (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups))
    base.automaticCapacity
  setNotes : ∀ queue notes, (base.notes notes).length ≤ base.automaticCapacity.notes →
    ∃ installed, base.setNotes queue notes = ok installed ∧
      base.rep.queue installed = { base.rep.queue queue with notes := base.notes notes }
  queueFacade : QueueMaintenanceComposition.FacadeAtomicAgrees base.facade base.queueOps
    (TouchAutomaticQueueProof.queueRep base.automaticRep) base.automaticCapacity.notes
  queueCursor : QueueMaintenanceProof.CursorAtomicAgrees base.cursor base.queueOps
    (TouchAutomaticQueueProof.queueRep base.automaticRep) touchQueueResolved
    base.automaticCapacity.notes
  emptyNotes : base.notes base.emptyNotes = []
  emptyEvents : base.rep.events base.emptyEvents = []
  reverse : ∀ events, ∃ reversed, base.reverse events = ok reversed ∧
    base.rep.events reversed = (base.rep.events events).reverse

theorem automatic_group_fields (base : Implementation) (ctx : Context)
    (remaining : RemainingAtoms base ctx) (area : areas.SensorArea) :
    TouchAutomaticGroupsProof.GroupFields base.automaticRep
      (TouchScheduleJudgmentsProof.headRep base.rep) (noteGroup base area) := by
  exact { note_group := (remaining.headPoints area).group_id, pair := fun _ => rfl }

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- Only successful output-field representation for the head composition.
The automatic encoding obligation follows by specializing clicked to false. -/
def EncodingReady (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event) : Prop :=
  ∀ area, TouchScheduleJudgmentsProof.EncodingReady inst integerOps decode encode
    base.rep pack (headContext ctx (Areas.toLnmSensorArea area))

/-- Both context constructors have the same time/style. Automatic groups only
affect its lookup, and the lifecycle receives the already-computed shared pair.
Thus the head encoding premise also covers every automatic step output. -/
theorem automatic_encoding_ready (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (ready : EncodingReady inst integerOps decode encode base ctx pack)
    (area : areas.SensorArea) (groups : base.G) :
    TouchAutomaticJudgmentsProof.EncodingReady inst integerOps decode encode
      base.automaticRep (automaticEncoding pack)
      (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups)) := by
  intro note diff shared output hstep
  have hhead : TouchScheduleJudgmentsProof.step inst integerOps decode encode
      (headContext ctx (Areas.toLnmSensorArea area)) (base.rep.note note)
      (base.rep.duration diff) false (TouchScheduleProof.sharedRep base.rep shared) =
      ok output := by
    simpa only [TouchScheduleJudgmentsProof.step, TouchAutomaticJudgmentsProof.step,
      Implementation.automaticRep, headContext, automaticContext,
      TouchScheduleProof.sharedRep] using hstep
  have fields := ready area note diff false shared output hhead
  exact { note := fields.note, event := fields.event }

/-- One record update installs both compositions. The per-area head-group
operations are reused by automatic lookup through the headOps instance, even
when HOps and AOps are different types. The groups argument is post-head. -/
def install (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D) :
    Implementation :=
  { base with
    head := fun area => TouchScheduleJudgmentsProof.install
      inst integerOps decode encode (base.head area) (headCall area) base.rep pack
      (headContext ctx (Areas.toLnmSensorArea area))
    automatic := fun area groups => TouchAutomaticJudgmentsProof.installStep
      inst integerOps decode encode
      (TouchAutomaticGroupsProof.installShared (base.automatic area groups)
        (noteGroup base area) (headCall area) (base.headOps area) groups)
      base.automaticRep (automaticEncoding pack)
      (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups)) }

/-- Existing reference capacity bounds transport through the record update;
no new traversal-capacity or whole-process equivalence premise is introduced. -/
theorem bounds_installed (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (bounds : Bounds base ctx) :
    Bounds (install inst integerOps decode encode base ctx pack headCall) ctx := by
  exact { queue := bounds.queue, headEvents := bounds.headEvents, automatic := bounds.automatic }

include integerLaw decode_encode in
theorem agrees_of_judgments_groups (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (ready : EncodingReady inst integerOps decode encode base ctx pack)
    (remaining : RemainingAtoms base ctx) (groupCapacity : Nat)
    (groupPoints : ∀ area, TouchScheduleJudgmentsProof.GroupAtoms
      (headCall area) (base.headOps area) base.rep groupCapacity)
    (groupRoom : TouchScheduleJudgmentsProof.GroupRoom base.rep groupCapacity) :
    Agrees (install inst integerOps decode encode base ctx pack headCall) ctx := by
  have headLaws := fun (area : areas.SensorArea) =>
    TouchScheduleJudgmentsProof.agrees_of_judgments_groups
      inst integerOps decode integerLaw encode decode_encode
      (base.head area) (headCall area) base.headCursor base.headReplace
      (base.headOps area) base.rep pack (headContext ctx (Areas.toLnmSensorArea area))
      base.headCapacity (ready area) (remaining.headPoints area)
      groupCapacity (groupPoints area) groupRoom
  refine {
    clone := remaining.clone
    fuel := remaining.fuel
    headPoints := fun area => (headLaws area).1
    headGroups := fun area => (headLaws area).2.1
    headLifecycle := fun area => (headLaws area).2.2
    automatic := ?_
    setNotes := remaining.setNotes
    queueFacade := remaining.queueFacade
    queueCursor := remaining.queueCursor
    emptyNotes := remaining.emptyNotes
    emptyEvents := remaining.emptyEvents
    reverse := remaining.reverse }
  intro area groups
  have sharedLaws := TouchAutomaticGroupsProof.remaining_atoms_of_groups
    (base.automatic area groups) (base.automaticOps area groups) base.automaticRep
    (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups))
    base.automaticCapacity (noteGroup base area) (headCall area) (base.headOps area)
    (TouchScheduleJudgmentsProof.headRep base.rep) groups groupCapacity
    (automatic_group_fields base ctx remaining area)
    (TouchScheduleJudgmentsProof.head_atoms (headCall area) (base.headOps area)
      base.rep groupCapacity (groupPoints area)) rfl (remaining.automaticPoints area groups)
  exact TouchAutomaticJudgmentsProof.atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    (TouchAutomaticGroupsProof.installShared (base.automatic area groups)
      (noteGroup base area) (headCall area) (base.headOps area) groups)
    (base.automaticOps area groups) base.automaticRep (automaticEncoding pack)
    (automaticContext ctx (Areas.toLnmSensorArea area) (base.rep.groups groups))
    base.automaticCapacity
    (automatic_encoding_ready inst integerOps decode encode base ctx pack ready area groups)
    sharedLaws

include integerLaw decode_encode in
/-- One sensor slot consumes the head output as automatic input, including
its updated groups and reverse events. No single-slot equation is a premise. -/
theorem queue_slot_refines (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (ready : EncodingReady inst integerOps decode encode base ctx pack)
    (remaining : RemainingAtoms base ctx) (groupCapacity : Nat)
    (groupPoints : ∀ area, TouchScheduleJudgmentsProof.GroupAtoms
      (headCall area) (base.headOps area) base.rep groupCapacity)
    (groupRoom : TouchScheduleJudgmentsProof.GroupRoom base.rep groupCapacity)
    (bounds : Bounds base ctx) (area : areas.SensorArea) (queue : base.Q)
    (acc : base.F × base.C × base.G × base.E) :
    ∃ out, TouchSchedulePassProof.queueSlot
        (install inst integerOps decode encode base ctx pack headCall)
        (area, queue, acc) = ok out ∧
      (base.rep.queue out.1, decodeAcc base out.2) =
        referenceSlot ctx (Areas.toLnmSensorArea area) (base.rep.queue queue)
          (decodeAcc base acc) := by
  exact TouchSchedulePassProof.queue_slot_refines
    (install inst integerOps decode encode base ctx pack headCall) ctx
    (agrees_of_judgments_groups inst integerOps decode integerLaw encode decode_encode
      base ctx pack headCall ready remaining groupCapacity groupPoints groupRoom)
    (bounds_installed inst integerOps decode encode base ctx pack headCall bounds)
    area queue acc

include integerLaw decode_encode in
/-- Complete sensor pass for the SAME installed Implementation: actual
Generated head + automatic + queue normalization + sensor map_accum + one
final event reverse. The reference is the original processTouchNotes.
No Agrees, headGroups/headLifecycle, shared/step or whole-pass equivalence
is required as an input. -/
theorem process_equiv (base : Implementation) (ctx : Context)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding base.N base.Event)
    (headCall : areas.SensorArea →
      touch_hold_schedule.HeadGroups base.HOps base.G base.Id base.Grade base.D)
    (ready : EncodingReady inst integerOps decode encode base ctx pack)
    (remaining : RemainingAtoms base ctx) (groupCapacity : Nat)
    (groupPoints : ∀ area, TouchScheduleJudgmentsProof.GroupAtoms
      (headCall area) (base.headOps area) base.rep groupCapacity)
    (groupRoom : TouchScheduleJudgmentsProof.GroupRoom base.rep groupCapacity)
    (bounds : Bounds base ctx) (frontiers : base.F) (queues : storage.SensorVec base.Q)
    (cursor : base.C) (groups : base.G) :
    ∃ out, TouchSchedulePassProof.process
        (install inst integerOps decode encode base ctx pack headCall)
        frontiers queues cursor groups = ok out ∧
      (base.rep.frontiers out.1, decodeQueues base out.2.1, base.rep.events out.2.2.1,
        base.rep.cursor out.2.2.2.1, base.rep.groups out.2.2.2.2) =
      reference ctx (base.rep.frontiers frontiers) (decodeQueues base queues)
        (base.rep.cursor cursor) (base.rep.groups groups) := by
  exact TouchSchedulePassProof.process_equiv
    (install inst integerOps decode encode base ctx pack headCall) ctx
    (agrees_of_judgments_groups inst integerOps decode integerLaw encode decode_encode
      base ctx pack headCall ready remaining groupCapacity groupPoints groupRoom)
    (bounds_installed inst integerOps decode encode base ctx pack headCall bounds)
    frontiers queues cursor groups

/-!
HANDOFF / BOUNDARY:

* Install this file as Verification.TouchSchedulePassJudgments after the three
  imported modules. Namespace: Verification.TouchSchedulePassJudgmentsProof.
  Main entry: process_equiv. agrees_of_judgments_groups is available separately
  if mainline needs Agrees for another composition over the same install value.
* RemainingAtoms has the existing 22 head point laws and 9 automatic point
  laws, plus clone/fuel, setNotes, normalization facade/cursor point laws,
  empty-buffer fields and event reverse. Queue traversals are proved by the
  imported modules; clone/append/write/reverse success is still a facade law.
* Eight bounded head-group point laws are reused in both phases. GroupRoom
  still quantifies over all represented groups: lists fit, and an absent id
  has one free slot. No uniqueness premise is added. Full native lists with
  an absent id need a narrower reachable-state interface in mainline; these
  theorems do not prove such a facade instance or allocation success.
* One exact integer instance/round trip and one pair of note/event encoders
  are shared. Only head EncodingReady is assumed; automatic EncodingReady
  is derived with clicked=false. BigInt, time/field encoding, native Usize
  conversion and concrete allocation remain explicit adapter obligations.
* Bounds is exactly the input Pass interface's uniform reference-capacity
  predicate. Installation changes neither its representations nor capacities.
* Actual post-head groups feed the installed automatic lookup. Updated
  frontiers/cursor/groups/events propagate between sensors; events reverse
  once at the end. This is reused from Pass, not re-proved by a second sensor
  traversal or postulated as an input equation.
* Constructing this Implementation does not prove equality with unextracted
  native scheduler.rs facade instances, initial clones or frame wiring.
  TouchHold and outer frame/GameState composition are outside this file.
* Source has not been elaborated locally. Mainline owns Model/Traversal/Pass
  compilation and acceptance of this independent composition.
-/

end Verification.TouchSchedulePassJudgmentsProof
