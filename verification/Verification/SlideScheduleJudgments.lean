import Verification.SlideJudgments
import Verification.SlideScheduleParents

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore Lifecycle

namespace Verification.SlideScheduleJudgmentsProof

open SlideScheduleProof SlideScheduleParentsProof

/-!
Compose the extracted judgment_adapter and slide_lifecycle calls with the
actual extracted slide_schedule traversal. The per-note lifecycle equation in
OtherAtoms.step is a conclusion, obtained from SlideJudgmentsProof.step_equiv.

Runtime wiring: scheduler.rs:1625-1643 passes current_time, sensor_held,
touch_panel_offset, delta, style and subdivide to slide_step, then returns
(after, event.into_iter().collect(), audio, render). Context.stepContext has
those exact six fields. Our thin step wrapper changes only Option to List;
the resulting note and both command lists retain their order and contents.

The Rust SlideScheduleFrame facade, cloning, field/time interpretation and
native allocation have NOT been extracted here. installStep constructs a
trait with the proved composition installed; it does not identify that trait
with the runtime facade. The remaining lifecycle queue/time/event adapters
and concrete Integer/BigInt laws are the boundaries of SlideJudgments.

EncodingFields contains only four separate successful field encodings. Its
domain is the actual composed step's outputs, so it does not require every
unbounded Lean note/list to fit a native representation. No field asserts
judgment, lifecycle or traversal correctness. The list instance discharges
all four laws definitionally. Parent point laws, metadata, copy, capacity,
facade wiring and read/replace/append laws remain explicit below.
-/

/-- Reify each already-computed field independently; these operations do not
receive a frame context or an input note on which to run another lifecycle. -/
structure OutputEncoding (N E A R : Type) where
  note : SlideNote → Result N
  events : List JudgeEvent → Result E
  audio : List AudioCommand → Result A
  render : List RenderCommand → Result R

structure EncodingFields {S N E A R : Type}
    (pack : OutputEncoding N E A R) (rep : Representation S N E A R)
    (batch : StepBatches) : Prop where
  note : ∃ result, pack.note batch.1 = ok result ∧ rep.note result = batch.1
  events : ∃ result, pack.events batch.2.1 = ok result ∧ rep.events result = batch.2.1
  audio : ∃ result, pack.audio batch.2.2.1 = ok result ∧ rep.audio result = batch.2.2.1
  render : ∃ result, pack.render batch.2.2.2 = ok result ∧ rep.render result = batch.2.2.2

def encodeBatch {N E A R : Type} (pack : OutputEncoding N E A R)
    (batch : StepBatches) : Result (N × E × A × R) := do
  let after ← pack.note batch.1
  let events ← pack.events batch.2.1
  let audio ← pack.audio batch.2.2.1
  let render ← pack.render batch.2.2.2
  ok (after, events, audio, render)

theorem encode_batch_spec {S N E A R : Type}
    (pack : OutputEncoding N E A R) (rep : Representation S N E A R)
    (batch : StepBatches) (fields : EncodingFields pack rep batch) :
    ∃ after events audio render,
      encodeBatch pack batch = ok (after, events, audio, render) ∧
      (rep.note after, rep.events events, rep.audio audio, rep.render render) = batch := by
  obtain ⟨after, hafter, hnote⟩ := fields.note
  obtain ⟨events, hevents, heventRep⟩ := fields.events
  obtain ⟨audio, haudio, haudioRep⟩ := fields.audio
  obtain ⟨render, hrender, hrenderRep⟩ := fields.render
  refine ⟨after, events, audio, render, ?_, ?_⟩
  · simp only [encodeBatch, hafter, hevents, haudio, hrender, bind_tc_ok]
  · exact Prod.ext hnote (Prod.ext heventRep (Prod.ext haudioRep hrenderRep))

/-- A concrete Lean-list interpretation of the new step, not a new runtime.
The scheduler/parent capacity laws are not asserted for arbitrary Lean lists. -/
def listRepresentation : Representation (List SlideNote) SlideNote
    (List JudgeEvent) (List AudioCommand) (List RenderCommand) :=
  ⟨id, id, id, id, id⟩

def listEncoding : OutputEncoding SlideNote
    (List JudgeEvent) (List AudioCommand) (List RenderCommand) :=
  ⟨fun note => ok note, fun events => ok events,
    fun audio => ok audio, fun render => ok render⟩

theorem list_encoding_fields (batch : StepBatches) :
    EncodingFields listEncoding listRepresentation batch := by
  exact ⟨⟨batch.1, rfl, rfl⟩, ⟨batch.2.1, rfl, rfl⟩,
    ⟨batch.2.2.1, rfl, rfl⟩, ⟨batch.2.2.2, rfl, rfl⟩⟩

/-- The remaining scheduler atoms have neither a lifecycle/step field nor
any parent correctness field. Each append retains its capacity condition. -/
structure RemainingAtoms {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (cap : Capacity) : Prop where
  read : ∀ (slides : S) (index : Usize) (pre : List SlideNote)
    (note : SlideNote) (rest : List SlideNote),
    rep.slides slides = pre ++ note :: rest → pre.length = index.val →
    ∃ result, call.read ops slides index = ok result ∧ rep.note result = note
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
  append_render : ∀ (accumulated batch : R),
    (rep.render accumulated).length + (rep.render batch).length ≤ cap.render →
    ∃ result, call.append_render ops accumulated batch = ok result ∧
      rep.render result = rep.render accumulated ++ rep.render batch

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- The runtime's per-note tuple layout and Option-to-list conversion.
SlideJudgments.step calls the extracted judgment and lifecycle kernels. -/
def step (ctx : Context) (note : SlideNote) : Result StepBatches := do
  let (after, event, audio, render) ←
    SlideJudgmentsProof.step inst integerOps encode note ctx.stepContext
  ok (after, event.toList, audio, render)

def encodedStep {S N E A R : Type} (rep : Representation S N E A R)
    (pack : OutputEncoding N E A R) (ctx : Context) (note : N) :
    Result (N × E × A × R) := do
  let batch ← step inst integerOps encode ctx (rep.note note)
  encodeBatch pack batch

/-- Only representation/allocation of fields already returned by the actual
composition is assumed. This predicate contains no reference-step equation. -/
def EncodingReady {S N E A R : Type} (rep : Representation S N E A R)
    (pack : OutputEncoding N E A R) (ctx : Context) : Prop :=
  ∀ (note : N) (batch : StepBatches),
    step inst integerOps encode ctx (rep.note note) = ok batch →
    EncodingFields pack rep batch

theorem list_encoding_ready (ctx : Context) :
    EncodingReady inst integerOps encode listRepresentation listEncoding ctx := by
  intro _ batch _
  exact list_encoding_fields batch

/-- All other trait methods are preserved. In particular the caller cannot
supply an arbitrary per-note callback in place of the proved composition. -/
def installStep {S N E A R Ops : Type}
    (base : slide_schedule.SlideSchedule Ops S N E A R)
    (rep : Representation S N E A R) (pack : OutputEncoding N E A R)
    (ctx : Context) : slide_schedule.SlideSchedule Ops S N E A R :=
  { base with step := fun _ note => encodedStep inst integerOps encode rep pack ctx note }

include integerLaw decode_encode

theorem step_equiv (ctx : Context) (note : SlideNote) :
    step inst integerOps encode ctx note = ok (stepBatches ctx note) := by
  unfold step
  rw [SlideJudgmentsProof.step_equiv inst integerOps decode integerLaw encode decode_encode]
  rfl

/-- The required per-note result relation follows from the judgment/lifecycle
theorem and the four field encoders, not from an assumed callback equation. -/
theorem encoded_step_spec {S N E A R : Type}
    (rep : Representation S N E A R) (pack : OutputEncoding N E A R)
    (ctx : Context) (ready : EncodingReady inst integerOps encode rep pack ctx)
    (note : N) :
    ∃ after events audio render,
      encodedStep inst integerOps encode rep pack ctx note = ok (after, events, audio, render) ∧
      (rep.note after, rep.events events, rep.audio audio, rep.render render) =
        stepBatches ctx (rep.note note) := by
  have hstep := step_equiv inst integerOps decode integerLaw encode decode_encode
    ctx (rep.note note)
  obtain ⟨after, events, audio, render, hpack, hfields⟩ :=
    encode_batch_spec pack rep (stepBatches ctx (rep.note note)) (ready note _ hstep)
  refine ⟨after, events, audio, render, ?_, hfields⟩
  simpa only [encodedStep, hstep, bind_tc_ok] using hpack

/-- Fill exactly OtherAtoms.step while transporting the unchanged atomic
read/replace/append laws through the definitional installation. -/
theorem other_atoms_of_judgments {S N E A R Ops : Type}
    (base : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (pack : OutputEncoding N E A R)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep cap) :
    OtherAtoms (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap := by
  refine {
    read := remaining.read
    step := ?_
    replace := remaining.replace
    append_events := remaining.append_events
    append_audio := remaining.append_audio
    append_render := remaining.append_render }
  intro note
  exact encoded_step_spec inst integerOps decode integerLaw encode decode_encode
    rep pack ctx ready note

/-- Parent composition is reused unchanged, including its universal local
capacity/copy obligations. Neither parent nor lifecycle equivalence is a
premise; FacadeCalls still states the unextracted parent facade's wiring. -/
theorem atomic_agrees_of_judgments {S N E A R Ops : Type}
    (base : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (pack : OutputEncoding N E A R)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep cap)
    (a : Adapters S N R) (snapshotRepr : a.Snapshot → List SlideNote)
    (id : a.Id → Nat) (parentCapacity : Nat)
    (points : PointsAgree a rep.slides rep.render snapshotRepr id parentCapacity)
    (metadata : MetadataAgrees a rep.note id)
    (containers : ContainersAgree a rep.slides rep.render snapshotRepr)
    (facade : FacadeCalls base ops a) (hforceCapacity : 0 < parentCapacity)
    (hancestorCapacity : ∀ slides, (a.length slides).val ≤ parentCapacity)
    (hcopy : ∀ slides, CopyAgrees a rep.slides slides) :
    AtomicAgrees (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap := by
  have installedFacade : FacadeCalls
      (installStep inst integerOps encode base rep pack ctx) ops a :=
    ⟨facade.force_direct, facade.end_ancestors, facade.refresh⟩
  exact SlideScheduleParentsProof.atomic_agrees_of_kernels
    (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap
    a snapshotRepr id parentCapacity points metadata containers installedFacade
    hforceCapacity hancestorCapacity hcopy
    (other_atoms_of_judgments inst integerOps decode integerLaw encode decode_encode
      base ops rep pack ctx cap ready remaining)

/-- The actual extracted traversal with the composed per-note step installed.
The inherited result keeps parent updates, lifecycle/direct/ancestor render
order, forward event/audio order and exactly-once pending-slot visitation.
This is not a theorem identifying the unextracted Rust SlideScheduleFrame. -/
theorem process_equiv {S N E A R Ops : Type}
    (base : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (pack : OutputEncoding N E A R)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep cap)
    (a : Adapters S N R) (snapshotRepr : a.Snapshot → List SlideNote)
    (id : a.Id → Nat) (parentCapacity : Nat)
    (points : PointsAgree a rep.slides rep.render snapshotRepr id parentCapacity)
    (metadata : MetadataAgrees a rep.note id)
    (containers : ContainersAgree a rep.slides rep.render snapshotRepr)
    (facade : FacadeCalls base ops a) (hforceCapacity : 0 < parentCapacity)
    (hancestorCapacity : ∀ slides, (a.length slides).val ≤ parentCapacity)
    (hcopy : ∀ slides, CopyAgrees a rep.slides slides)
    (len : Usize) (initial : slide_schedule.SlideWork S E A R)
    (processedRev pending : List SlideNote) (eventsRev : List JudgeEvent)
    (audioRev : List AudioCommand) (renderRev : List RenderCommand)
    (hinit : interpret rep initial =
      initialReference processedRev pending eventsRev audioRev renderRev)
    (hlen : (rep.slides initial.slides).length = len.val)
    (hfit : Fits cap (Scheduler.processSlideNotesCore processedRev pending ctx.input
      ctx.currentTime ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide
      eventsRev audioRev renderRev)) :
    slide_schedule.process (installStep inst integerOps encode base rep pack ctx)
      len initial ops ⦃ out =>
      (interpret rep out).output =
        Scheduler.processSlideNotesCore processedRev pending ctx.input ctx.currentTime
          ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide eventsRev audioRev renderRev ∧
      out.index.val = len.val ∧
      BodyHistory (installStep inst integerOps encode base rep pack ctx) len ops initial out
        (List.range' processedRev.length pending.length) ⦄ := by
  have atoms := atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    base ops rep pack ctx cap ready remaining a snapshotRepr id parentCapacity
    points metadata containers facade hforceCapacity hancestorCapacity hcopy
  exact SlideScheduleProof.process_equiv
    (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap atoms len initial
    processedRev pending eventsRev audioRev renderRev hinit hlen hfit

end Verification.SlideScheduleJudgmentsProof
