import Verification.SchedulerParents
import Verification.SlideSchedule
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore Lifecycle

namespace Verification.SlideScheduleParentsProof

open SchedulerParentsProof
open private forceFinishDirectParent endSlideAncestors updateSlideParentFlags
  from LnmaiCore.Scheduler

/-!
Composition of the extracted parent kernels for the three parent callbacks in
`SlideScheduleProof.AtomicAgrees`. The wrappers below call Generated's
`sensor_triggered`, `force_finish`, `end_ancestors`, and `refresh_flags`; none
uses a reference parent operation as its implementation. Their targets are
the original private Scheduler definitions.

Runtime wiring: scheduler.rs's force_finish_direct_parent first computes
slide_sensor_triggered, then calls force_finish with empty commands;
end_slide_ancestors supplies the two ended flags and an empty command buffer;
update_slide_parent_flags reads the original snapshot and writes a clone.
The SlideScheduleFrame methods return the modified owned slides and commands.

Those runtime facades have NOT been extracted. `FacadeCalls` therefore remains
an explicit boundary: the concrete trait methods must equal these kernel
compositions, including mutable-slice reborrowing and restoration to the owned
Vec. `install` constructs a trait with these calls, but is not asserted to be
the extracted SlideScheduleFrame implementation. Concrete cloning, Natural
metadata/equality, queue-head reads, and allocation are also not proved here.

No entire parent/reference equivalence is a premise of the final constructor.
All command capacity and container-length premises of SchedulerParentsProof
remain explicit. In particular AtomicAgrees asks for success on ALL inputs;
its constructor consequently requires the local bounds and copy contract for
every represented state. These obligations are not derived from the outer
scheduler's output Fits predicate or from Rust's use of Vec::new.
-/

/-- Concrete point adapters and facade argument preparation. Metadata getters
are abstract runtime operations, not calls to the reference implementation.
Snapshot reads and clone writes have separate traits from the mutation path. -/
structure Adapters (S N C : Type) where
  Id : Type
  Reader : Type
  Equal : Type
  Writer : Type
  Emitter : Type
  Snapshot : Type
  SnapshotReader : Type
  RefreshWriter : Type
  Head : Type
  readCall : slide_parents.ParentRead Reader S Id
  equalCall : slide_parents.ParentEqual Equal Id
  writeCall : core.ops.function.Fn Writer (S × Usize × slide_parents.ParentEdit) S
  emitCall : core.ops.function.Fn Emitter (C × Id) C
  snapshotReadCall : slide_parents.ParentRead SnapshotReader Snapshot Id
  refreshWriteCall : core.ops.function.Fn RefreshWriter (S × Usize × slide_parents.ParentEdit) S
  headCall : core.ops.function.Fn Head Usize Bool
  read : Reader
  equal : Equal
  write : Writer
  emit : Emitter
  snapshotRead : SnapshotReader
  refreshWrite : RefreshWriter
  length : S → Usize
  empty : C
  snapshot : S → Snapshot
  copy : S → Result S
  parent : N → Option Id
  ended : N → Bool
  phase : N → slide_parents.SensorPhase
  checkable : N → Bool
  consumed : N → N → Bool
  queueCount : N → Usize
  head : N → Head

/-- Only the existing point-read, point-write, equality, and single-push
contracts are required for traversal and mutation. IDs remain unbounded Nat
interpretations; no uniqueness, injectivity, or acyclicity premise is added. -/
structure PointsAgree {S N C : Type} (a : Adapters S N C)
    (slidesRepr : S → List SlideNote) (commandsRepr : C → List RenderCommand)
    (snapshotRepr : a.Snapshot → List SlideNote) (id : a.Id → Nat)
    (capacity : Nat) : Prop where
  read : ReadAgrees a.readCall a.read slidesRepr id
  equal : EqualAgrees a.equalCall a.equal id
  write : WriteAgrees a.writeCall a.write slidesRepr
  emit : EmitAgrees a.emitCall a.emit commandsRepr id capacity
  snapshot_read : ReadAgrees a.snapshotReadCall a.snapshotRead snapshotRepr id
  refresh_write : WriteAgrees a.refreshWriteCall a.refreshWrite slidesRepr

/-- The consumed comparison is of unbounded queue-length sums. Usize bounds
apply to queue counts and scan indices, never to the sum or to note IDs. -/
structure MetadataAgrees {S N C : Type} (a : Adapters S N C)
    (noteRepr : N → SlideNote) (id : a.Id → Nat) : Prop where
  parent : ∀ note, (a.parent note).map id = (noteRepr note).parentNoteIndex
  ended : ∀ note, a.ended note = isEnded (noteRepr note).state
  phase : ∀ note, a.phase note = sensorPhase (noteRepr note).state
  checkable : ∀ note, a.checkable note = (noteRepr note).isCheckable
  consumed : ∀ before after, a.consumed before after = decide
    (((noteRepr after).judgeQueues.map List.length).sum <
      ((noteRepr before).judgeQueues.map List.length).sum)
  queue_count : ∀ note, (a.queueCount note).val = (noteRepr note).judgeQueues.length
  head : ∀ note, HeadAgrees a.headCall (a.head note) (noteRepr note).judgeQueues

structure ContainersAgree {S N C : Type} (a : Adapters S N C)
    (slidesRepr : S → List SlideNote) (commandsRepr : C → List RenderCommand)
    (snapshotRepr : a.Snapshot → List SlideNote) : Prop where
  length : ∀ slides, (a.length slides).val = (slidesRepr slides).length
  empty : commandsRepr a.empty = []
  snapshot : ∀ slides, snapshotRepr (a.snapshot slides) = slidesRepr slides

/-- A successful container clone for this input. This is an explicit
allocation/representation obligation, not an unconditional success theorem. -/
def CopyAgrees {S N C : Type} (a : Adapters S N C)
    (slidesRepr : S → List SlideNote) (slides : S) : Prop :=
  ∃ copied, a.copy slides = ok copied ∧ slidesRepr copied = slidesRepr slides

def sensorTriggered {S N C : Type} (a : Adapters S N C) (before after : N) : Result Bool :=
  slide_parents.sensor_triggered a.headCall (a.phase before) (a.phase after)
    (a.checkable after) (a.consumed before after) (a.queueCount after) (a.head after)

def forceDirect {S N C : Type} (a : Adapters S N C)
    (slides : S) (before after : N) : Result (S × C) := do
  let triggered ← sensorTriggered a before after
  slide_parents.force_finish a.readCall a.equalCall a.writeCall a.emitCall
    (a.length slides) slides a.empty triggered (a.parent after) a.read a.equal a.write a.emit

def endAncestors {S N C : Type} (a : Adapters S N C)
    (slides : S) (before after : N) : Result (S × C) :=
  slide_parents.end_ancestors a.readCall a.equalCall a.writeCall a.emitCall
    (a.length slides) slides a.empty (a.ended before) (a.ended after) (a.parent after)
    a.read a.equal a.write a.emit

def refresh {S N C : Type} (a : Adapters S N C) (slides : S) : Result S := do
  let copied ← a.copy slides
  slide_parents.refresh_flags a.snapshotReadCall a.equalCall a.refreshWriteCall
    (a.length slides) (a.snapshot slides) copied a.snapshotRead a.equal a.refreshWrite

theorem sensor_triggered_equiv {S N C : Type} (a : Adapters S N C)
    (noteRepr : N → SlideNote) (id : a.Id → Nat)
    (metadata : MetadataAgrees a noteRepr id) (before after : N) :
    sensorTriggered a before after =
      ok (Scheduler.slideSensorTriggered (noteRepr before) (noteRepr after)) := by
  unfold sensorTriggered
  rw [metadata.phase before, metadata.phase after, metadata.checkable after]
  exact SchedulerParentsProof.sensor_triggered_spec a.headCall (a.head after)
    (a.queueCount after) (a.consumed before after) (noteRepr before) (noteRepr after)
    (metadata.queue_count after) (metadata.head after) (metadata.consumed before after)

/-- Sensor guards and head scanning are proved by the actual sensor kernel,
then composed with the actual direct-parent kernel. One free command slot is
the existing force_finish_spec premise, even on a path that emits nothing. -/
theorem force_direct_spec {S N C : Type} (a : Adapters S N C)
    (slidesRepr : S → List SlideNote) (noteRepr : N → SlideNote)
    (commandsRepr : C → List RenderCommand) (snapshotRepr : a.Snapshot → List SlideNote)
    (id : a.Id → Nat) (capacity : Nat)
    (points : PointsAgree a slidesRepr commandsRepr snapshotRepr id capacity)
    (metadata : MetadataAgrees a noteRepr id)
    (containers : ContainersAgree a slidesRepr commandsRepr snapshotRepr)
    (slides : S) (before after : N) (hcapacity : 0 < capacity) :
    forceDirect a slides before after ⦃ out =>
      (slidesRepr out.1, commandsRepr out.2) =
        forceFinishDirectParent (slidesRepr slides) (noteRepr before) (noteRepr after) ⦄ := by
  have htrigger := sensor_triggered_equiv a noteRepr id metadata before after
  have hspace : (commandsRepr a.empty).length < capacity := by
    simpa only [containers.empty, List.length_nil] using hcapacity
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (SchedulerParentsProof.force_finish_spec
      a.readCall a.equalCall a.writeCall a.emitCall a.read a.equal a.write a.emit
      slidesRepr commandsRepr id capacity (a.length slides) slides a.empty
      (Scheduler.slideSensorTriggered (noteRepr before) (noteRepr after))
      (a.parent after) (noteRepr before) (noteRepr after) rfl (metadata.parent after)
      (containers.length slides) hspace points.read points.equal points.write points.emit)
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simpa only [forceDirect, htrigger, bind_tc_ok] using hrun
  · simpa only [containers.empty, List.nil_append] using hout

/-- The command budget covers the full initial fuel (= number of slide
slots), without assuming distinct IDs, an acyclic chain, or a smaller fuel. -/
theorem end_ancestors_spec {S N C : Type} (a : Adapters S N C)
    (slidesRepr : S → List SlideNote) (noteRepr : N → SlideNote)
    (commandsRepr : C → List RenderCommand) (snapshotRepr : a.Snapshot → List SlideNote)
    (id : a.Id → Nat) (capacity : Nat)
    (points : PointsAgree a slidesRepr commandsRepr snapshotRepr id capacity)
    (metadata : MetadataAgrees a noteRepr id)
    (containers : ContainersAgree a slidesRepr commandsRepr snapshotRepr)
    (slides : S) (before after : N) (hcapacity : (a.length slides).val ≤ capacity) :
    endAncestors a slides before after ⦃ out =>
      (slidesRepr out.1, commandsRepr out.2) =
        endSlideAncestors (noteRepr before) (noteRepr after) (slidesRepr slides) ⦄ := by
  have hspace : (commandsRepr a.empty).length + (a.length slides).val ≤ capacity := by
    simpa only [containers.empty, List.length_nil, Nat.zero_add] using hcapacity
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (SchedulerParentsProof.end_ancestors_spec
      a.readCall a.equalCall a.writeCall a.emitCall a.read a.equal a.write a.emit
      slidesRepr commandsRepr id capacity (a.length slides) slides a.empty (a.parent after)
      (noteRepr before) (noteRepr after) (metadata.parent after) (containers.length slides)
      hspace points.read points.equal points.write points.emit)
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simpa only [endAncestors, metadata.ended before, metadata.ended after] using hrun
  · simpa only [containers.empty, List.nil_append] using hout

/-- Reads use the original snapshot throughout; writes target the successful
clone. CopyAgrees must be supplied for the actual input allocation. -/
theorem refresh_spec {S N C : Type} (a : Adapters S N C)
    (slidesRepr : S → List SlideNote) (commandsRepr : C → List RenderCommand)
    (snapshotRepr : a.Snapshot → List SlideNote) (id : a.Id → Nat) (capacity : Nat)
    (points : PointsAgree a slidesRepr commandsRepr snapshotRepr id capacity)
    (containers : ContainersAgree a slidesRepr commandsRepr snapshotRepr)
    (slides : S) (hcopy : CopyAgrees a slidesRepr slides) :
    refresh a slides ⦃ out => slidesRepr out = updateSlideParentFlags (slidesRepr slides) ⦄ := by
  obtain ⟨copied, hcopied, hrepr⟩ := hcopy
  have hlen : (a.length slides).val = (snapshotRepr (a.snapshot slides)).length := by
    rw [containers.snapshot slides]
    exact containers.length slides
  have hsame : slidesRepr copied = snapshotRepr (a.snapshot slides) :=
    hrepr.trans (containers.snapshot slides).symm
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (SchedulerParentsProof.refresh_flags_spec
      a.snapshotReadCall a.equalCall a.refreshWriteCall a.snapshotRead a.equal a.refreshWrite
      snapshotRepr slidesRepr id (a.length slides) (a.snapshot slides) copied hlen hsame
      points.snapshot_read points.equal points.refresh_write)
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simpa only [refresh, hcopied, bind_tc_ok] using hrun
  · simpa only [containers.snapshot slides] using hout

/-- Required implementation wiring, not a parent/reference correctness
assumption. These equations are still open for the unextracted runtime facade. -/
structure FacadeCalls {S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C) (ops : Ops)
    (a : Adapters S N C) : Prop where
  force_direct : ∀ slides before after,
    call.force_direct ops slides before after = forceDirect a slides before after
  end_ancestors : ∀ slides before after,
    call.end_ancestors ops slides before after = endAncestors a slides before after
  refresh : ∀ slides, call.refresh ops slides = SlideScheduleParentsProof.refresh a slides

/-- A generic trait construction with the real kernels installed. It makes
the required wiring equations definitional, without identifying it with the
runtime SlideScheduleFrame or changing any other operation. -/
def install {S N E A C Ops : Type}
    (base : slide_schedule.SlideSchedule Ops S N E A C) (a : Adapters S N C) :
    slide_schedule.SlideSchedule Ops S N E A C :=
  { base with
    force_direct := fun _ slides before after => forceDirect a slides before after
    end_ancestors := fun _ slides before after => endAncestors a slides before after
    refresh := fun _ slides => refresh a slides }

theorem install_calls {S N E A C Ops : Type}
    (base : slide_schedule.SlideSchedule Ops S N E A C) (ops : Ops)
    (a : Adapters S N C) : FacadeCalls (install base a) ops a := by
  exact ⟨fun _ _ _ => rfl, fun _ _ _ => rfl, fun _ => rfl⟩

/-- Exactly the three fields to be filled in AtomicAgrees. This structure is
a conclusion of parent_fields_of_kernels, not a premise of the final bridge. -/
structure ParentFields {S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C) (ops : Ops)
    (rep : SlideScheduleProof.Representation S N E A C) : Prop where
  force_direct : ∀ (slides : S) (before after : N), ∃ result commands,
    call.force_direct ops slides before after = ok (result, commands) ∧
    (rep.slides result, rep.render commands) =
      forceFinishDirectParent (rep.slides slides) (rep.note before) (rep.note after)
  end_ancestors : ∀ (slides : S) (before after : N), ∃ result commands,
    call.end_ancestors ops slides before after = ok (result, commands) ∧
    (rep.slides result, rep.render commands) =
      endSlideAncestors (rep.note before) (rep.note after) (rep.slides slides)
  refresh : ∀ (slides : S), ∃ result, call.refresh ops slides = ok result ∧
    rep.slides result = updateSlideParentFlags (rep.slides slides)

/-- Universal bounds here are required by the universal parent fields in the
existing AtomicAgrees. If only local bounds are available, use the three
per-call wrapper theorems above; this theorem does not promote local success
to unconditional facade success. The parent budget is for temporary command
buffers, separately from the scheduler's append/output capacity. -/
theorem parent_fields_of_kernels {S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C) (ops : Ops)
    (rep : SlideScheduleProof.Representation S N E A C) (a : Adapters S N C)
    (snapshotRepr : a.Snapshot → List SlideNote) (id : a.Id → Nat) (capacity : Nat)
    (points : PointsAgree a rep.slides rep.render snapshotRepr id capacity)
    (metadata : MetadataAgrees a rep.note id)
    (containers : ContainersAgree a rep.slides rep.render snapshotRepr)
    (facade : FacadeCalls call ops a) (hforceCapacity : 0 < capacity)
    (hancestorCapacity : ∀ slides, (a.length slides).val ≤ capacity)
    (hcopy : ∀ slides, CopyAgrees a rep.slides slides) : ParentFields call ops rep := by
  constructor
  · intro slides before after
    obtain ⟨out, hrun, hout⟩ := spec_imp_exists
      (force_direct_spec a rep.slides rep.note rep.render snapshotRepr id capacity
        points metadata containers slides before after hforceCapacity)
    rcases out with ⟨result, commands⟩
    exact ⟨result, commands, (facade.force_direct slides before after).trans hrun, hout⟩
  · intro slides before after
    obtain ⟨out, hrun, hout⟩ := spec_imp_exists
      (end_ancestors_spec a rep.slides rep.note rep.render snapshotRepr id capacity
        points metadata containers slides before after (hancestorCapacity slides))
    rcases out with ⟨result, commands⟩
    exact ⟨result, commands, (facade.end_ancestors slides before after).trans hrun, hout⟩
  · intro slides
    obtain ⟨out, hrun, hout⟩ := spec_imp_exists
      (refresh_spec a rep.slides rep.render snapshotRepr id capacity
        points containers slides (hcopy slides))
    exact ⟨out, (facade.refresh slides).trans hrun, hout⟩

/-- The other fields of the existing scheduler contract, kept unchanged.
This has no field for direct-parent, ancestor, or parent-flag correctness. -/
structure OtherAtoms {S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C) (ops : Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (ctx : SlideScheduleProof.Context) (cap : SlideScheduleProof.Capacity) : Prop where
  read : ∀ (slides : S) (index : Usize) (pre : List SlideNote)
    (note : SlideNote) (rest : List SlideNote),
    rep.slides slides = pre ++ note :: rest → pre.length = index.val →
    ∃ result, call.read ops slides index = ok result ∧ rep.note result = note
  step : ∀ (note : N), ∃ after events audio render,
    call.step ops note = ok (after, events, audio, render) ∧
    (rep.note after, rep.events events, rep.audio audio, rep.render render) =
      SlideScheduleProof.stepBatches ctx (rep.note note)
  replace : ∀ (slides : S) (index : Usize) (pre : List SlideNote)
    (old : SlideNote) (rest : List SlideNote) (after : N),
    rep.slides slides = pre ++ old :: rest → pre.length = index.val →
    ∃ result, call.replace ops slides index after = ok result ∧
      rep.slides result = pre ++ rep.note after :: rest
  append_events : ∀ (accumulated batch : E),
    (rep.events accumulated).length + (rep.events batch).length ≤ cap.events →
    ∃ result, call.append_events ops accumulated batch = ok result ∧
      rep.events result = rep.events accumulated ++ rep.events batch
  append_audio : ∀ (accumulated batch : A),
    (rep.audio accumulated).length + (rep.audio batch).length ≤ cap.audio →
    ∃ result, call.append_audio ops accumulated batch = ok result ∧
      rep.audio result = rep.audio accumulated ++ rep.audio batch
  append_render : ∀ (accumulated batch : C),
    (rep.render accumulated).length + (rep.render batch).length ≤ cap.render →
    ∃ result, call.append_render ops accumulated batch = ok result ∧
      rep.render result = rep.render accumulated ++ rep.render batch

/-- Build AtomicAgrees using only the remaining non-parent atoms, kernel
wiring, point adapters, metadata, container contracts, and explicit bounds.
No whole-parent reference equation is supplied as an assumption. -/
theorem atomic_agrees_of_kernels {S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C) (ops : Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (ctx : SlideScheduleProof.Context) (cap : SlideScheduleProof.Capacity)
    (a : Adapters S N C) (snapshotRepr : a.Snapshot → List SlideNote)
    (id : a.Id → Nat) (parentCapacity : Nat)
    (points : PointsAgree a rep.slides rep.render snapshotRepr id parentCapacity)
    (metadata : MetadataAgrees a rep.note id)
    (containers : ContainersAgree a rep.slides rep.render snapshotRepr)
    (facade : FacadeCalls call ops a) (hforceCapacity : 0 < parentCapacity)
    (hancestorCapacity : ∀ slides, (a.length slides).val ≤ parentCapacity)
    (hcopy : ∀ slides, CopyAgrees a rep.slides slides)
    (other : OtherAtoms call ops rep ctx cap) :
    SlideScheduleProof.AtomicAgrees call ops rep ctx cap := by
  have parents := parent_fields_of_kernels call ops rep a snapshotRepr id parentCapacity
    points metadata containers facade hforceCapacity hancestorCapacity hcopy
  exact {
    read := other.read
    step := other.step
    replace := other.replace
    force_direct := parents.force_direct
    end_ancestors := parents.end_ancestors
    refresh := parents.refresh
    append_events := other.append_events
    append_audio := other.append_audio
    append_render := other.append_render }

end Verification.SlideScheduleParentsProof
