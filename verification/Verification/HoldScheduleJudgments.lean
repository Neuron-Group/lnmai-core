import Verification.HoldJudgments
import Verification.HoldSchedule

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

namespace Verification.HoldScheduleJudgmentsProof
open HoldScheduleModel HoldScheduleProof
open private buttonQueueIndexUnlocked holdHeadEligibleForClick
  fallbackSensorAreaForButtonNote fallbackSensorHeldForButtonNote
  fallbackPrevSensorHeldForButtonNote advanceSharedButtonQueue updateButtonHoldQueue
  from LnmaiCore.Scheduler

/-!
Install the concrete HoldJudgments step in the extracted hold_schedule trait.
The runtime HoldScheduleFrame.step supplies the two constant ignore windows,
shared = none, current frame time/delta/style/offset, and the four booleans
computed by the traversal. Its event stays Option; the traversal appends it
only when present. No lifecycle or whole-pass equality is assumed here.

Only independently computed output fields are encoded. IntegerAgrees and
exact encode/decode, the inherited time/note/event interpretation, container
and counter atoms, successful field encoding and capacity remain boundaries.
update_queue explicitly retains replace-all-matching-IDs + normalize + store;
its separate composition is pending. The installed trait is a thin composition
of extracted kernels, not an extraction of Rust HoldScheduleFrame/hold_step.
-/

structure OutputEncoding (N Event : Type) where
  note : HoldNote → Result N
  event : JudgeEvent → Result Event

structure EncodingFields {S Z N F Q C H E Event D Area : Type}
    (pack : OutputEncoding N Event) (rep : Representation S Z N F Q C H E Event D Area)
    (output : HoldNote × Option JudgeEvent) : Prop where
  note : ∃ after, pack.note output.1 = ok after ∧ rep.note after = output.1
  event : ∀ event, output.2 = some event →
    ∃ encoded, pack.event event = ok encoded ∧ rep.event encoded = event

def encodeOutput {N Event : Type} (pack : OutputEncoding N Event)
    (output : HoldNote × Option JudgeEvent) : Result (N × Option Event) := do
  let after ← pack.note output.1
  let event ← match output.2 with
    | none => ok none
    | some event => do
        let encoded ← pack.event event
        ok (some encoded)
  ok (after, event)

theorem encode_output_spec {S Z N F Q C H E Event D Area : Type}
    (pack : OutputEncoding N Event) (rep : Representation S Z N F Q C H E Event D Area)
    (output : HoldNote × Option JudgeEvent) (fields : EncodingFields pack rep output) :
    ∃ after event, encodeOutput pack output = ok (after, event) ∧
      (rep.note after, event.map rep.event) = output := by
  obtain ⟨after, hafter, hnote⟩ := fields.note
  cases hevent : output.2 with
  | none =>
      refine ⟨after, none, ?_, ?_⟩
      · simp only [encodeOutput, hafter, hevent, bind_tc_ok]
      · exact Prod.ext hnote (by simp only [Option.map_none, hevent])
  | some event =>
      obtain ⟨encoded, hencoded, heventRep⟩ := fields.event event hevent
      refine ⟨after, some encoded, ?_, ?_⟩
      · simp only [encodeOutput, hafter, hevent, hencoded, bind_tc_ok]
      · exact Prod.ext hnote (by simp only [Option.map_some, hevent, heventRep])

/-- All scheduler laws except step. Queue synchronization is the one retained
composite contract; it is not asserted proved by this file. -/
structure RemainingAtoms {S Z N F Q C H E Event D Area Ops : Type}
    (call : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Q C H E Event D Area)
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ holds, ∃ len, call.length ops holds = ok len ∧
    (rep.holds holds).length = len.val
  get : ∀ (holds : S) (index : Usize) (pre : List (ButtonZone × HoldNote))
    (zone : ButtonZone) (note : HoldNote) (rest : List (ButtonZone × HoldNote)),
    rep.holds holds = pre ++ (zone, note) :: rest → pre.length = index.val →
    ∃ nativeZone nativeNote, call.get ops holds index = ok (nativeZone, nativeNote) ∧
      rep.zone nativeZone = zone ∧ rep.note nativeNote = note
  button_diff : ∀ note, ∃ diff, call.button_diff ops note = ok diff ∧
    rep.duration diff = ctx.currentTime - (rep.note note).params.effectiveTiming
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = (ctx.currentTime - ctx.touchPanelOffset) -
      (rep.note note).params.effectiveTiming
  button_pressed : ∀ zone, call.button_pressed ops zone =
    ok (ctx.input.getButtonHeld (rep.zone zone))
  sensor_pressed : ∀ zone, call.sensor_pressed ops zone =
    ok (fallbackSensorHeldForButtonNote ctx.input (rep.zone zone))
  prev_sensor_pressed : ∀ zone, call.prev_sensor_pressed ops zone =
    ok (fallbackPrevSensorHeldForButtonNote ctx.prevSensor (rep.zone zone))
  phase : ∀ note, call.phase ops note = ok (HoldScheduleProof.phase (rep.note note))
  unlocked : ∀ frontiers zone note, call.unlocked ops frontiers zone note =
    ok (buttonQueueIndexUnlocked (rep.frontiers frontiers) (rep.zone zone)
      (rep.note note).buttonQueueIndex)
  eligible : ∀ note, call.eligible ops note =
    ok (holdHeadEligibleForClick (rep.note note) ctx.currentTime)
  fallback_area : ∀ zone, ∃ area, call.fallback_area ops zone = ok area ∧
    rep.area area = fallbackSensorAreaForButtonNote (rep.zone zone)
  button_available : ∀ cursor zone, call.button_available ops cursor zone =
    ok (decide ((rep.cursor cursor).buttonUsed.getD (rep.zone zone) 0 <
      ctx.input.getButtonClickCount (rep.zone zone)))
  sensor_available : ∀ cursor area, call.sensor_available ops cursor area =
    ok (decide ((rep.cursor cursor).sensorUsed.getD (rep.area area) 0 <
      ctx.input.getSensorClickCount (rep.area area)))
  use_button : ∀ cursor zone, ∃ next, call.use_button ops cursor zone = ok next ∧
    rep.cursor next = spendButton (rep.cursor cursor) (rep.zone zone)
  use_sensor : ∀ cursor area, ∃ next, call.use_sensor ops cursor area = ok next ∧
    rep.cursor next = spendSensor (rep.cursor cursor) (rep.area area)
  advance_frontier : ∀ frontiers zone, ∃ next,
    call.advance_frontier ops frontiers zone = ok next ∧
    rep.frontiers next = advanceSharedButtonQueue (rep.frontiers frontiers) (rep.zone zone)
  update_queue : ∀ queues zone note, ∃ next, call.update_queue ops queues zone note = ok next ∧
    rep.queues next = updateButtonHoldQueue (rep.queues queues) (rep.zone zone) (rep.note note)
  append_active : ∀ active zone note,
    (rep.active active).length + 1 ≤ cap.active →
    ∃ next, call.append_active ops active zone note = ok next ∧
      rep.active next = rep.active active ++ [(rep.zone zone, rep.note note)]
  append_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.append_event ops events event = ok next ∧
      rep.events next = rep.events events ++ [rep.event event]

def stepFrame (ctx : Context) (diff : Duration)
    (clicked pressed currentButton prevSensor : Bool) : HoldLifecycleProof.Frame :=
  { now := ctx.currentTime, diff := diff
    headIgnore := HOLD_HEAD_IGNORE_LENGTH_SEC, tailIgnore := HOLD_TAIL_IGNORE_LENGTH_SEC
    clicked := clicked, pressed := pressed, currentButtonPressed := currentButton
    prevSensorPressed := prevSensor, touchPanelOffset := ctx.touchPanelOffset
    shared := none, delta := ctx.delta, style := ctx.style }

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- Actual extracted judgment/lifecycle dispatch, with runtime's fixed inputs. -/
def step (ctx : Context) (note : HoldNote) (diff : Duration)
    (clicked pressed currentButton prevSensor : Bool) : Result (HoldNote × Option JudgeEvent) :=
  HoldJudgmentsProof.holdStep inst integerOps encode note
    (stepFrame ctx diff clicked pressed currentButton prevSensor)

def encodedStep {S Z N F Q C H E Event D Area : Type}
    (rep : Representation S Z N F Q C H E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (note : N) (diff : D) (clicked pressed currentButton prevSensor : Bool) :
    Result (N × Option Event) := do
  let output ← step inst integerOps encode ctx (rep.note note) (rep.duration diff)
    clicked pressed currentButton prevSensor
  encodeOutput pack output

/-- Representation of successful outputs, without a reference-step equation. -/
def EncodingReady {S Z N F Q C H E Event D Area : Type}
    (rep : Representation S Z N F Q C H E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) : Prop :=
  ∀ (note : N) (diff : D) (clicked pressed currentButton prevSensor : Bool)
    (output : HoldNote × Option JudgeEvent),
    step inst integerOps encode ctx (rep.note note) (rep.duration diff)
      clicked pressed currentButton prevSensor = ok output → EncodingFields pack rep output

def installStep {S Z N F Q C H E Event D Area Ops : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area)
    (rep : Representation S Z N F Q C H E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area :=
  { base with
    step := fun _ note diff clicked pressed currentButton prevSensor =>
      encodedStep inst integerOps encode rep pack ctx note diff
        clicked pressed currentButton prevSensor }

include integerLaw decode_encode

theorem step_equiv (ctx : Context) (note : HoldNote) (diff : Duration)
    (clicked pressed currentButton prevSensor : Bool) :
    step inst integerOps encode ctx note diff clicked pressed currentButton prevSensor =
      ok (Lifecycle.holdStep note ctx.currentTime diff
        HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked pressed
        currentButton prevSensor ctx.touchPanelOffset none ctx.delta ctx.style) := by
  unfold step
  simpa only [HoldLifecycleProof.referenceStep, stepFrame] using
    HoldJudgmentsProof.hold_step_equiv inst integerOps decode integerLaw encode decode_encode
      note (stepFrame ctx diff clicked pressed currentButton prevSensor)

theorem encoded_step_spec {S Z N F Q C H E Event D Area : Type}
    (rep : Representation S Z N F Q C H E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (ready : EncodingReady inst integerOps encode rep pack ctx)
    (note : N) (diff : D) (clicked pressed currentButton prevSensor : Bool) :
    ∃ after event,
      encodedStep inst integerOps encode rep pack ctx note diff
        clicked pressed currentButton prevSensor = ok (after, event) ∧
      (rep.note after, event.map rep.event) =
        Lifecycle.holdStep (rep.note note) ctx.currentTime (rep.duration diff)
          HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked pressed
          currentButton prevSensor ctx.touchPanelOffset none ctx.delta ctx.style := by
  have hstep := step_equiv inst integerOps decode integerLaw encode decode_encode
    ctx (rep.note note) (rep.duration diff) clicked pressed currentButton prevSensor
  obtain ⟨after, event, hpack, hfields⟩ := encode_output_spec pack rep _
    (ready note diff clicked pressed currentButton prevSensor _ hstep)
  refine ⟨after, event, ?_, hfields⟩
  simpa only [encodedStep, hstep, bind_tc_ok] using hpack

/-- Derive AtomicAgrees.step; every other method keeps its original point law. -/
theorem atomic_agrees_of_judgments {S Z N F Q C H E Event D Area Ops : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Q C H E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx cap) :
    AtomicAgrees (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap := by
  refine {
    length := remaining.length
    get := remaining.get
    button_diff := remaining.button_diff
    sensor_diff := remaining.sensor_diff
    button_pressed := remaining.button_pressed
    sensor_pressed := remaining.sensor_pressed
    prev_sensor_pressed := remaining.prev_sensor_pressed
    phase := remaining.phase
    unlocked := remaining.unlocked
    eligible := remaining.eligible
    fallback_area := remaining.fallback_area
    button_available := remaining.button_available
    sensor_available := remaining.sensor_available
    use_button := remaining.use_button
    use_sensor := remaining.use_sensor
    step := ?_
    advance_frontier := remaining.advance_frontier
    update_queue := remaining.update_queue
    append_active := remaining.append_active
    append_event := remaining.append_event }
  intro note diff clicked pressed currentButton prevSensor
  exact encoded_step_spec inst integerOps decode integerLaw encode decode_encode
    rep pack ctx ready note diff clicked pressed currentButton prevSensor

/-- Reuse the proved extracted traversal to reach the ORIGINAL processHoldNotes.
No whole-item, whole-lifecycle or whole-pass equation is a premise. -/
theorem process_equiv {S Z N F Q C H E Event D Area Ops : Type}
    (base : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area) (ops : Ops)
    (rep : Representation S Z N F Q C H E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx cap)
    (holds : S) (initial : hold_schedule.HoldWork F Q C H E)
    (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor)
    (hinit : interpret rep initial = ⟨frontiers, queues, cursor, [], []⟩)
    (hfit : Fits cap (resume ctx (rep.holds holds) ⟨frontiers, queues, cursor, [], []⟩)) :
    hold_schedule.process (installStep inst integerOps encode base rep pack ctx)
      holds initial ops ⦃ out =>
      (interpret rep out).output = reference ctx (rep.holds holds) frontiers queues cursor ⦄ := by
  exact HoldScheduleProof.process_equiv
    (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap
    (atomic_agrees_of_judgments inst integerOps decode integerLaw encode decode_encode
      base ops rep pack ctx cap ready remaining)
    holds initial frontiers queues cursor hinit hfit

end Verification.HoldScheduleJudgmentsProof
