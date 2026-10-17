import Verification.LifecycleJudgments
import Verification.TapSchedule

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TapScheduleJudgmentsProof

open TapScheduleProof TapScheduleModel
open private tapEligibleForClick buttonQueueIndexUnlocked advanceSharedButtonQueue
  listSetAt from LnmaiCore.Scheduler

/-!
Connect the extracted exact judgment and Tap/SlideHead transition kernels to
the actual Generated tap_schedule.process. AtomicAgrees.step is derived from
LifecycleJudgmentsProof.tap_family_step_equiv; no whole step, queue or pass
equation is assumed. The queue traversal is reused from TapScheduleProof.

Runtime correspondence: TapScheduleFrame.step in scheduler.rs passes the note,
current_time, diff, clicked and style directly to lifecycle.rs:tap_family_step.
The step below uses exactly those inputs, retaining Option event cardinality
and the event's exact diff. It does not rerun lifecycle during output encoding.

OutputEncoding reifies the computed note and each present event separately.
EncodingReady concerns only successful outputs of the composed step on decoded
inputs. It does not assert any reference-step equation or require an encoding
for every arbitrary unbounded Lean note/event. The reference instance proves
only these field laws, not the remaining scheduler atoms.

Remaining boundaries are explicit: all fifteen non-step point operations;
note/event/duration/area, queue/frontier/cursor interpretation; time predicates
and event construction inherited from LifecycleJudgments; IntegerAgrees and
the exact integer encoding round trip; concrete Integer/BigInt arithmetic,
cloning, containers and native allocation. The event bound concerns successful
buffers only. Field encoders and allocating atoms still require successful
representation/allocation; neither OOM behavior nor a concrete BigInt instance
is proved. No machine-width bound on decoded duration or Nat counters is added.

installStep constructs a trait with the proved composition installed. It does
not identify this trait with the unextracted Rust TapScheduleFrame facade or
prove its initial clones/context wiring. These theorems cover one queue call;
outer zone mapAccum, frame composition and GameState traces are outside this
file's scope.
-/

abbrev StepOutput := TapFamilyNote × Option JudgeEvent

/-- Field encoders receive already-computed values, with no input note or
frame context on which to run another lifecycle. -/
structure OutputEncoding (N Event : Type) where
  note : TapFamilyNote → Result N
  event : JudgeEvent → Result Event

structure EncodingFields {Q N F C E Event D Area : Type}
    (pack : OutputEncoding N Event) (rep : Representation Q N F C E Event D Area)
    (batch : StepOutput) : Prop where
  note : ∃ result, pack.note batch.1 = ok result ∧ rep.note result = batch.1
  event : ∀ event, batch.2 = some event →
    ∃ result, pack.event event = ok result ∧ rep.event result = event

/-- An absent event performs no event encoding or allocation. -/
def encodeBatch {N Event : Type} (pack : OutputEncoding N Event)
    (batch : StepOutput) : Result (N × Option Event) := do
  let after ← pack.note batch.1
  let event ← match batch.2 with
    | none => ok none
    | some event => do
        let result ← pack.event event
        ok (some result)
  ok (after, event)

theorem encode_batch_spec {Q N F C E Event D Area : Type}
    (pack : OutputEncoding N Event) (rep : Representation Q N F C E Event D Area)
    (batch : StepOutput) (fields : EncodingFields pack rep batch) :
    ∃ after event, encodeBatch pack batch = ok (after, event) ∧
      (rep.note after, event.map rep.event) = batch := by
  obtain ⟨after, hafter, hnote⟩ := fields.note
  cases hevent : batch.2 with
  | none =>
      refine ⟨after, none, ?_, ?_⟩
      · simp only [encodeBatch, hafter, hevent, bind_tc_ok]
      · apply Prod.ext
        · exact hnote
        · simp only [Option.map_none, hevent]
  | some event =>
      obtain ⟨result, hresult, heventRep⟩ := fields.event event hevent
      refine ⟨after, some result, ?_, ?_⟩
      · simp only [encodeBatch, hafter, hevent, hresult, bind_tc_ok]
      · apply Prod.ext
        · exact hnote
        · simp only [Option.map_some, heventRep, hevent]

/-- Identity field representation over the existing Lean queue/counter types.
This is not a native Rust representation or a scheduler-atom instance. -/
def referenceRepresentation : Representation (ZoneQueue TapFamilyNote) TapFamilyNote
    (ButtonVec Nat) Scheduler.ClickCursor (List JudgeEvent) JudgeEvent Duration SensorArea :=
  ⟨id, id, id, id, id, id, id, id⟩

def referenceEncoding : OutputEncoding TapFamilyNote JudgeEvent :=
  ⟨fun note => ok note, fun event => ok event⟩

theorem reference_encoding_fields (batch : StepOutput) :
    EncodingFields referenceEncoding referenceRepresentation batch := by
  refine { note := ⟨batch.1, rfl, rfl⟩, event := ?_ }
  intro event _
  exact ⟨event, rfl, rfl⟩

/-- Exactly the non-step fields of AtomicAgrees. Each law concerns one point
operation; in particular no lifecycle, queue traversal or pass law occurs. -/
structure RemainingAtoms {Q N F C E Event D Area Ops : Type}
    (call : tap_schedule.TapSchedule Ops Q N F C E Event D Area) (ops : Ops)
    (rep : Representation Q N F C E Event D Area)
    (ctx : Context) (capacity : Nat) : Prop where
  peek : ∀ q, ∃ note, call.peek ops q = ok note ∧ note.map rep.note = (rep.queue q).peek
  button_diff : ∀ n, ∃ d, call.button_diff ops n = ok d ∧
    rep.duration d = difference ctx (rep.note n) false
  sensor_diff : ∀ n, ∃ d, call.sensor_diff ops n = ok d ∧
    rep.duration d = difference ctx (rep.note n) true
  eligible : ∀ n, call.eligible ops n = ok (tapEligibleForClick (rep.note n) ctx.currentTime)
  unlocked : ∀ f n, call.unlocked ops f n =
    ok (buttonQueueIndexUnlocked (rep.frontiers f) ctx.zone (rep.note n).buttonQueueIndex)
  fallback_area : ∀ n, ∃ a, call.fallback_area ops n = ok a ∧ rep.area a = fallback (rep.note n)
  button_available : ∀ c, call.button_available ops c = ok (buttonAvailable ctx (rep.cursor c))
  sensor_available : ∀ c a,
    call.sensor_available ops c a = ok (sensorAvailable ctx (rep.cursor c) (rep.area a))
  /-- Only an available click is spent. Natural arithmetic/field allocation
  is required at the selected update, never for an unavailable counter. -/
  use_button : ∀ c, buttonAvailable ctx (rep.cursor c) = true →
    ∃ out, call.use_button ops c = ok out ∧
      rep.cursor out = useButton ctx (rep.cursor c)
  use_sensor : ∀ c a, sensorAvailable ctx (rep.cursor c) (rep.area a) = true →
    ∃ out, call.use_sensor ops c a = ok out ∧
      rep.cursor out = useSensor (rep.cursor c) (rep.area a)
  ended : ∀ n, call.ended ops n = ok (TapScheduleModel.ended (rep.note n))
  prepend_event : ∀ events event, (rep.events events).length + 1 ≤ capacity →
    ∃ out, call.prepend_event ops events event = ok out ∧
      rep.events out = rep.event event :: rep.events events
  advance : ∀ q, ∃ out, call.advance ops q = ok out ∧ rep.queue out = (rep.queue q).advance
  advance_frontier : ∀ f, ∃ out, call.advance_frontier ops f = ok out ∧
    rep.frontiers out = advanceSharedButtonQueue (rep.frontiers f) ctx.zone
  replace_current : ∀ q n, ∃ out, call.replace_current ops q n = ok out ∧
    rep.queue out = { rep.queue q with
      notes := listSetAt (rep.queue q).notes (rep.queue q).currentIndex (rep.note n) }

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- The actual judgment/lifecycle composition, with the exact duration encoded
before the extracted judgment adapter selects its bounded representative. -/
def step (ctx : Context) (note : TapFamilyNote) (diff : Duration) (clicked : Bool) :
    Result StepOutput :=
  LifecycleJudgmentsProof.tapFamilyStep inst integerOps decode note ctx.currentTime
    (encode diff.toMicros) clicked ctx.style

def encodedStep {Q N F C E Event D Area : Type}
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (note : N) (diff : D) (clicked : Bool) : Result (N × Option Event) := do
  let batch ← step inst integerOps decode encode ctx (rep.note note) (rep.duration diff) clicked
  encodeBatch pack batch

/-- Only field representation/allocation on actual successful outputs of the
composition. No reference lifecycle or complete-step equality is a premise. -/
def EncodingReady {Q N F C E Event D Area : Type}
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) : Prop :=
  ∀ (note : N) (diff : D) (clicked : Bool) (batch : StepOutput),
    step inst integerOps decode encode ctx (rep.note note) (rep.duration diff) clicked = ok batch →
    EncodingFields pack rep batch

theorem reference_encoding_ready (ctx : Context) :
    EncodingReady inst integerOps decode encode referenceRepresentation referenceEncoding ctx := by
  intro _ _ _ batch _
  exact reference_encoding_fields batch

/-- Every other trait method is preserved; base.step is discarded and is not
an assumed callback equation. The context and integer instance are fixed. -/
def installStep {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) : tap_schedule.TapSchedule Ops Q N F C E Event D Area :=
  { base with step := fun _ note diff clicked =>
      encodedStep inst integerOps decode encode rep pack ctx note diff clicked }

include integerLaw decode_encode

theorem step_equiv (ctx : Context) (note : TapFamilyNote) (diff : Duration) (clicked : Bool) :
    step inst integerOps decode encode ctx note diff clicked =
      ok (Lifecycle.tapFamilyStep note ctx.currentTime diff clicked ctx.style) := by
  have hdiff : Duration.fromMicros diff.toMicros = diff := rfl
  simpa only [step, decode_encode, hdiff] using
    LifecycleJudgmentsProof.tap_family_step_equiv inst integerOps decode integerLaw
      note ctx.currentTime (encode diff.toMicros) clicked ctx.style

/-- The exact relation required by AtomicAgrees.step follows from the proved
judgment/lifecycle equation and independent output field encodings. -/
theorem encoded_step_spec {Q N F C E Event D Area : Type}
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (note : N) (diff : D) (clicked : Bool) :
    ∃ after event,
      encodedStep inst integerOps decode encode rep pack ctx note diff clicked = ok (after, event) ∧
      (rep.note after, event.map rep.event) =
        Lifecycle.tapFamilyStep (rep.note note) ctx.currentTime (rep.duration diff) clicked ctx.style := by
  have hstep := step_equiv inst integerOps decode integerLaw encode decode_encode
    ctx (rep.note note) (rep.duration diff) clicked
  obtain ⟨after, event, hpack, hfields⟩ :=
    encode_batch_spec pack rep _ (ready note diff clicked _ hstep)
  refine ⟨after, event, ?_, hfields⟩
  simpa only [encodedStep, hstep, bind_tc_ok] using hpack

/-- Fill AtomicAgrees.step with the derived theorem, transporting the other
fifteen fields through the definitionally unchanged trait projections. -/
theorem atomic_agrees_of_judgments {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area) (ops : Ops)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (capacity : Nat)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx capacity) :
    AtomicAgrees (installStep inst integerOps decode encode base rep pack ctx)
      ops rep ctx capacity := by
  refine {
    peek := remaining.peek
    button_diff := remaining.button_diff
    sensor_diff := remaining.sensor_diff
    eligible := remaining.eligible
    unlocked := remaining.unlocked
    fallback_area := remaining.fallback_area
    button_available := remaining.button_available
    sensor_available := remaining.sensor_available
    use_button := remaining.use_button
    use_sensor := remaining.use_sensor
    step := ?_
    ended := remaining.ended
    prepend_event := remaining.prepend_event
    advance := remaining.advance
    advance_frontier := remaining.advance_frontier
    replace_current := remaining.replace_current }
  intro note diff clicked
  exact encoded_step_spec inst integerOps decode integerLaw encode decode_encode
    rep pack ctx ready note diff clicked

/-- The actual Generated process with the composed step installed. The result
is the original Scheduler fuel recursion, including exact reverse event order,
click consumption, stop/advance behavior and zero-event Ended transitions. -/
theorem process_equiv {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area) (ops : Ops)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (capacity : Nat)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx capacity)
    (fuel : Usize) (initial : tap_schedule.TapWork Q F C E)
    (hfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.length ≤ capacity) :
    tap_schedule.process (installStep inst integerOps decode encode base rep pack ctx)
      fuel initial ops ⦃ out =>
      (interpret rep out).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  have atoms := atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    base ops rep pack ctx capacity ready remaining
  exact TapScheduleProof.process_equiv
    (installStep inst integerOps decode encode base rep pack ctx)
    ops rep ctx capacity atoms fuel initial hfit

/-- Reuse the established remaining-count fuel bridge for the same generated
queue call. This adds no outer zone traversal or frame assumption. -/
theorem pending_process_equiv {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area) (ops : Ops)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (capacity : Nat)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx capacity)
    (fuel : Usize) (initial : tap_schedule.TapWork Q F C E)
    (hfuel : fuel.val = TapScheduleModel.remaining (rep.queue initial.queue))
    (hfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.length ≤ capacity) :
    tap_schedule.process (installStep inst integerOps decode encode base rep pack ctx)
      fuel initial ops ⦃ out =>
      (interpret rep out).output = resume ctx
        (TapScheduleModel.remaining (rep.queue initial.queue) + 1) (interpret rep initial) ⦄ := by
  have atoms := atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    base ops rep pack ctx capacity ready remaining
  exact TapScheduleProof.pending_process_equiv
    (installStep inst integerOps decode encode base rep pack ctx)
    ops rep ctx capacity atoms fuel initial hfuel hfit

end Verification.TapScheduleJudgmentsProof
