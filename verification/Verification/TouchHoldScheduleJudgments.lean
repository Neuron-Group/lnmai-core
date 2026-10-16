import Verification.HoldJudgments
import Verification.TouchHoldSchedule

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

namespace Verification.TouchHoldScheduleJudgmentsProof
open TouchHoldScheduleModel TouchHoldScheduleProof
open private touchHoldBodyCheckActive holdHeadAwaitsResolution touchQueueIndexUnlocked
  holdHeadEligibleForClick advanceSharedTouchQueue updateSensorHoldQueue
  from LnmaiCore.Scheduler

/-!
Install the extracted Hold judgment/lifecycle composition into the TouchHold
scheduler. Both click sources share one diff chosen by the traversal. Body
pressure already includes the updated body-group majority, and the shared head
result is decoded and passed through unchanged. The two TouchHold ignore windows
and both false physical-button-history flags match the runtime facade.

This discharges the single-note step equation, not the concrete Rust facade.
Exact integer operations, output encodings, scheduler/container point laws,
queue synchronization and capacity remain explicit obligations.
-/

structure OutputEncoding (N Event : Type) where
  note : HoldNote → Result N
  event : JudgeEvent → Result Event

structure EncodingFields {S A N F Q C H E Event D Z Id Grade G B : Type}
    (pack : OutputEncoding N Event) (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
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

theorem encode_output_spec {S A N F Q C H E Event D Z Id Grade G B : Type}
    (pack : OutputEncoding N Event) (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
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

/-- Native point laws, excluding the derived lifecycle equation. -/
structure RemainingAtoms {S A N F Q C H E Event D Z Id Grade G B Ops : Type}
    (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
    (ops : Ops) (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ holds, ∃ len, call.length ops holds = ok len ∧
    (rep.holds holds).length = len.val
  get : ∀ holds (index : Usize) (pre : List (SensorArea × HoldNote)) area note rest,
    rep.holds holds = pre ++ (area, note) :: rest → pre.length = index.val →
    ∃ nativeArea nativeNote, call.get ops holds index = ok (nativeArea, nativeNote) ∧
      rep.area nativeArea = area ∧ rep.note nativeNote = note
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = (ctx.currentTime - ctx.touchPanelOffset) -
      (rep.note note).params.effectiveTiming
  button_diff : ∀ note, ∃ diff, call.button_diff ops note = ok diff ∧
    rep.duration diff = ctx.currentTime - (rep.note note).params.effectiveTiming
  local_pressed : ∀ area, call.local_pressed ops area = ok (ctx.input.getSensorHeld (rep.area area))
  body_check_active : ∀ note, call.body_check_active ops note =
    ok (touchHoldBodyCheckActive (rep.note note) ctx.currentTime)
  body_group_id : ∀ note, ∃ id, call.body_group_id ops note = ok id ∧
    id.map rep.id = (rep.note note).touchHoldGroupId
  head_group_id : ∀ note, ∃ id, call.head_group_id ops note = ok id ∧
    id.map rep.id = (rep.note note).touchGroupId
  note_index : ∀ note, ∃ id, call.note_index ops note = ok id ∧
    rep.id id = (rep.note note).params.noteIndex
  group_size : ∀ note, ∃ size, call.group_size ops note = ok size ∧
    rep.id size = (rep.note note).touchGroupSize
  phase : ∀ note, call.phase ops note = ok (TouchHoldScheduleProof.phase (rep.note note))
  head_grade : ∀ note, ∃ grade, call.head_grade ops note = ok grade ∧
    grade.map rep.grade = headGrade (rep.note note)
  head_diff : ∀ note, ∃ diff, call.head_diff ops note = ok diff ∧
    rep.duration diff = (rep.note note).headDiff
  grade_rejected : ∀ grade, call.grade_rejected ops grade = ok (rep.grade grade).isMissOrTooFast
  within_head_window : ∀ note, call.within_head_window ops note =
    ok (decide (ctx.currentTime ≤ (rep.note note).params.effectiveTiming + touchGoodMs))
  unlocked : ∀ frontiers area note, call.unlocked ops frontiers area note =
    ok (touchQueueIndexUnlocked (rep.frontiers frontiers) (rep.area area)
      (rep.note note).touchQueueIndex)
  eligible : ∀ note, call.eligible ops note =
    ok (holdHeadEligibleForClick (rep.note note) ctx.currentTime)
  button_ring : call.button_ring ops = ok ctx.buttonRingForTouch
  outer_button : ∀ area, ∃ zone, call.outer_button ops area = ok zone ∧
    zone.map rep.zone = (rep.area area).toOuterButtonZone?
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
  advance_frontier : ∀ frontiers area, ∃ next,
    call.advance_frontier ops frontiers area = ok next ∧
    rep.frontiers next = advanceSharedTouchQueue (rep.frontiers frontiers) (rep.area area)
  update_queue : ∀ queues area note, ∃ next, call.update_queue ops queues area note = ok next ∧
    rep.queues next = updateSensorHoldQueue (rep.queues queues) (rep.area area) (rep.note note)
  append_active : ∀ active area note, (rep.active active).length + 1 ≤ cap.active →
    ∃ next, call.append_active ops active area note = ok next ∧
    rep.active next = rep.active active ++ [(rep.area area, rep.note note)]
  append_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.append_event ops events event = ok next ∧
    rep.events next = rep.events events ++ [rep.event event]

def stepFrame (ctx : Context) (diff : Duration)
    (clicked pressed : Bool) (shared : Option (JudgeGrade × Duration)) : HoldLifecycleProof.Frame :=
  { now := ctx.currentTime, diff := diff
    headIgnore := TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC, tailIgnore := TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
    clicked := clicked, pressed := pressed, currentButtonPressed := false
    prevSensorPressed := false, touchPanelOffset := ctx.touchPanelOffset
    shared := shared, delta := ctx.delta, style := ctx.style }

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- Actual extracted judgment/lifecycle dispatch, with runtime's fixed inputs. -/
def step (ctx : Context) (note : HoldNote) (diff : Duration)
    (clicked pressed : Bool) (shared : Option (JudgeGrade × Duration)) : Result (HoldNote × Option JudgeEvent) :=
  HoldJudgmentsProof.holdStep inst integerOps encode note
    (stepFrame ctx diff clicked pressed shared)

def encodedStep {S A N F Q C H E Event D Z Id Grade G B : Type}
    (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (pack : OutputEncoding N Event) (ctx : Context) (note : N) (diff : D)
    (clicked pressed : Bool) (shared : Option (Grade × D)) : Result (N × Option Event) := do
  let output ← step inst integerOps encode ctx (rep.note note) (rep.duration diff)
    clicked pressed (shared.map (decodeShared rep))
  encodeOutput pack output

/-- Representation of successful outputs, without a reference-step equation. -/
def EncodingReady {S A N F Q C H E Event D Z Id Grade G B : Type}
    (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (pack : OutputEncoding N Event) (ctx : Context) : Prop :=
  ∀ (note : N) (diff : D) (clicked pressed : Bool) (shared : Option (Grade × D))
    (output : HoldNote × Option JudgeEvent),
    step inst integerOps encode ctx (rep.note note) (rep.duration diff)
      clicked pressed (shared.map (decodeShared rep)) = ok output →
        EncodingFields pack rep output

def installStep {S A N F Q C H E Event D Z Id Grade G B Ops : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
    (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (pack : OutputEncoding N Event) (ctx : Context) :
    touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade :=
  { base with step := fun _ note diff clicked pressed shared =>
      encodedStep inst integerOps encode rep pack ctx note diff clicked pressed shared }

include integerLaw decode_encode

theorem step_equiv (ctx : Context) (note : HoldNote) (diff : Duration)
    (clicked pressed : Bool) (shared : Option (JudgeGrade × Duration)) :
    step inst integerOps encode ctx note diff clicked pressed shared =
      ok (Lifecycle.holdStep note ctx.currentTime diff
        TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC clicked pressed
        false false ctx.touchPanelOffset shared ctx.delta ctx.style) := by
  unfold step
  simpa only [HoldLifecycleProof.referenceStep, stepFrame] using
    HoldJudgmentsProof.hold_step_equiv inst integerOps decode integerLaw encode decode_encode
      note (stepFrame ctx diff clicked pressed shared)

theorem encoded_step_spec {S A N F Q C H E Event D Z Id Grade G B : Type}
    (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (pack : OutputEncoding N Event) (ctx : Context)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (note : N) (diff : D) (clicked pressed : Bool) (shared : Option (Grade × D)) :
    ∃ after event,
      encodedStep inst integerOps encode rep pack ctx note diff clicked pressed shared =
        ok (after, event) ∧
      (rep.note after, event.map rep.event) =
        Lifecycle.holdStep (rep.note note) ctx.currentTime (rep.duration diff)
          TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
          clicked pressed false false ctx.touchPanelOffset (shared.map (decodeShared rep))
          ctx.delta ctx.style := by
  have hstep := step_equiv inst integerOps decode integerLaw encode decode_encode
    ctx (rep.note note) (rep.duration diff) clicked pressed (shared.map (decodeShared rep))
  obtain ⟨after, event, hpack, hfields⟩ := encode_output_spec pack rep _
    (ready note diff clicked pressed shared _ hstep)
  refine ⟨after, event, ?_, hfields⟩
  simpa only [encodedStep, hstep, bind_tc_ok] using hpack

/-- Derive AtomicAgrees.step; every other method keeps its original point law. -/
theorem atomic_agrees_of_judgments {S A N F Q C H E Event D Z Id Grade G B Ops : Type}
    (base : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade) (ops : Ops)
    (rep : Representation S A N F Q C H E Event D Z Id Grade G B) (pack : OutputEncoding N Event)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx cap) :
    AtomicAgrees (installStep inst integerOps encode base rep pack ctx) ops rep ctx cap := by
  refine {
    length := remaining.length
    get := remaining.get
    sensor_diff := remaining.sensor_diff
    button_diff := remaining.button_diff
    local_pressed := remaining.local_pressed
    body_check_active := remaining.body_check_active
    body_group_id := remaining.body_group_id
    head_group_id := remaining.head_group_id
    note_index := remaining.note_index
    group_size := remaining.group_size
    phase := remaining.phase
    head_grade := remaining.head_grade
    head_diff := remaining.head_diff
    grade_rejected := remaining.grade_rejected
    within_head_window := remaining.within_head_window
    unlocked := remaining.unlocked
    eligible := remaining.eligible
    button_ring := remaining.button_ring
    outer_button := remaining.outer_button
    button_available := remaining.button_available
    sensor_available := remaining.sensor_available
    use_button := remaining.use_button
    use_sensor := remaining.use_sensor
    advance_frontier := remaining.advance_frontier
    update_queue := remaining.update_queue
    append_active := remaining.append_active
    append_event := remaining.append_event
    step := ?_ }
  intro note diff clicked pressed shared
  exact encoded_step_spec inst integerOps decode integerLaw encode decode_encode
    rep pack ctx ready note diff clicked pressed shared

end Verification.TouchHoldScheduleJudgmentsProof
