import Verification.HoldScheduleModel
import Verification.SchedulerClicks

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

set_option maxHeartbeats 800000

namespace Verification.HoldScheduleProof
open HoldScheduleModel
open private holdHeadAwaitsResolution buttonQueueIndexUnlocked holdHeadEligibleForClick
  fallbackSensorAreaForButtonNote fallbackSensorHeldForButtonNote
  fallbackPrevSensorHeldForButtonNote holdBodyPressedFromButtonOrSensor
  isHeadJudgedState enteredHeadJudged keepHoldActive advanceSharedButtonQueue
  updateButtonHoldQueue from LnmaiCore.Scheduler

/-!
Targets the ACTUAL hold_schedule declarations extracted in the scheduler batch
(LnmaiCoreVerify.lean, trait/body/loop at 1649/1702/1829), now installed in
Verification.Generated. The generated loop state is (HoldWork, Usize); its
return is HoldWork, not the tuple layout used by slide_schedule.

The continuation is HoldScheduleModel.resume, which calls the original private
Scheduler.processHoldNotes. No assumption supplies an entire item or pass.
The single-note lifecycle and the composite update_queue contract remain
explicit boundaries; this file does not claim the Rust facade was extracted.
Judgment and queue-maintenance compositions discharge these two boundaries
separately; the point-operation laws remain explicit.
-/

structure Representation (S Z N F Q C H E Event D Area : Type) where
  holds : S → List (ButtonZone × HoldNote)
  zone : Z → ButtonZone
  note : N → HoldNote
  frontiers : F → ButtonVec Nat
  queues : Q → ButtonQueueVec HoldNote
  cursor : C → Scheduler.ClickCursor
  active : H → List (ButtonZone × HoldNote)
  events : E → List JudgeEvent
  event : Event → JudgeEvent
  duration : D → Duration
  area : Area → SensorArea

def interpret {S Z N F Q C H E Event D Area : Type}
    (rep : Representation S Z N F Q C H E Event D Area)
    (w : hold_schedule.HoldWork F Q C H E) : Work :=
  ⟨rep.frontiers w.frontiers, rep.queues w.queues, rep.cursor w.cursor,
    rep.active w.active, rep.events w.events⟩

structure Capacity where
  active : Nat
  events : Nat

def Fits (cap : Capacity) (w : Work) : Prop :=
  w.active.length ≤ cap.active ∧ w.events.length ≤ cap.events

/-- A final bound also bounds the buffers immediately after this item. -/
theorem step_fits (ctx : Context) (cap : Capacity) (w : Work)
    (zone : ButtonZone) (note : HoldNote) (rest : List (ButtonZone × HoldNote))
    (hfit : Fits cap (resume ctx ((zone, note) :: rest) w)) :
    Fits cap (step ctx w zone note) := by
  rw [resume_step] at hfit
  have h := resume_output_growth ctx rest (step ctx w zone note)
  exact ⟨Nat.le_trans h.1 hfit.1, Nat.le_trans h.2 hfit.2⟩

/-- Grades do not affect the phase, in particular HeadJudged Miss. -/
def phase (note : HoldNote) : hold_schedule.HoldPhase :=
  match note.state with
  | .HeadWaiting => .HeadWaiting
  | .HeadJudgeable => .HeadJudgeable
  | .HeadJudged _ => .HeadJudged
  | .BodyHeld => .BodyHeld
  | .BodyReleased => .BodyReleased
  | .Ended _ => .Ended

def spendButton (cursor : Scheduler.ClickCursor) (zone : ButtonZone) : Scheduler.ClickCursor :=
  { cursor with buttonUsed := cursor.buttonUsed.set zone (cursor.buttonUsed.getD zone 0 + 1) }

def spendSensor (cursor : Scheduler.ClickCursor) (area : SensorArea) : Scheduler.ClickCursor :=
  { cursor with sensorUsed := cursor.sensorUsed.set area (cursor.sensorUsed.getD area 0 + 1) }

/-- Each field is a point operation except the explicitly retained single-note
lifecycle and queue synchronization boundary. Length/get concern the immutable
original list. The append contracts are conditional on capacity, not universal
allocation-success laws. Field/counter/time representations and successful
clones or allocations must be established for the concrete Rust facade.
update_queue still means replace ALL matching IDs, normalize the resolved head
prefix, and store the zone queue; the separate Queue proof must discharge it. -/
structure AtomicAgrees {S Z N F Q C H E Event D Area Ops : Type}
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
  step : ∀ note diff clicked pressed currentButton prevSensor,
    ∃ after event, call.step ops note diff clicked pressed currentButton prevSensor =
      ok (after, event) ∧
      (rep.note after, event.map rep.event) =
        holdStep (rep.note note) ctx.currentTime (rep.duration diff)
          HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked pressed
          currentButton prevSensor ctx.touchPanelOffset none ctx.delta ctx.style
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

private theorem phase_await (note : HoldNote) :
    (match phase note with
      | .HeadWaiting => ok true | .HeadJudgeable => ok true
      | .HeadJudged => ok false | .BodyHeld => ok false
      | .BodyReleased => ok false | .Ended => ok false) =
      (ok (holdHeadAwaitsResolution note.state) : Result Bool) := by
  cases hs : note.state <;> simp [phase, hs, holdHeadAwaitsResolution]

private theorem phase_judged (note : HoldNote) :
    (match phase note with
      | .HeadWaiting => ok false | .HeadJudgeable => ok false
      | .HeadJudged => ok true | .BodyHeld => ok false
      | .BodyReleased => ok false | .Ended => ok false) =
      (ok (isHeadJudgedState note.state) : Result Bool) := by
  cases hs : note.state <;> simp [phase, hs, isHeadJudgedState]

private theorem phase_ended (note : HoldNote) :
    (match phase note with
      | .HeadWaiting => ok false | .HeadJudgeable => ok false
      | .HeadJudged => ok false | .BodyHeld => ok false
      | .BodyReleased => ok false | .Ended => ok true) =
      (ok (!keepHoldActive note) : Result Bool) := by
  cases hs : note.state <;> simp [phase, hs, keepHoldActive]

private theorem admission_ok {W : Type} (w : W) (head unlocked eligible : Bool) :
    (if head then
      (do
        let b ← if unlocked then ok eligible else ok false
        ok (w, b))
      else ok (w, false)) = (ok (w, head && unlocked && eligible) : Result (W × Bool)) := by
  cases head <;> cases unlocked <;> rfl

private theorem or_ok (a b : Bool) :
    (if a then ok true else ok b) = (ok (a || b) : Result Bool) := by
  cases a <;> rfl

/-- Pure name for the table of the real shared consume_click kernel. -/
def clickPlan (allow button sensor : Bool) : scheduler.ClickUse :=
  if allow then
    if button then ⟨true, false⟩ else if sensor then ⟨false, true⟩ else ⟨false, false⟩
  else ⟨false, false⟩

theorem click_plan_spec (allow button sensor : Bool) :
    scheduler.consume_click allow button sensor = ok (clickPlan allow button sensor) := by
  cases allow <;> cases button <;> cases sensor <;> rfl

def spentCursor (plan : scheduler.ClickUse) (cursor : Scheduler.ClickCursor)
    (zone : ButtonZone) (area : SensorArea) : Scheduler.ClickCursor :=
  { cursor with
    buttonUsed := if plan.used_button then
      cursor.buttonUsed.set zone (cursor.buttonUsed.getD zone 0 + 1) else cursor.buttonUsed
    sensorUsed := if plan.used_sensor then
      cursor.sensorUsed.set area (cursor.sensorUsed.getD area 0 + 1) else cursor.sensorUsed }

/-- Reuse the proved reference click adapter; both availability reads use the
old cursor, and a button suppresses the fallback sensor. -/
theorem plan_consumes (ctx : Context) (cursor : Scheduler.ClickCursor)
    (zone : ButtonZone) (allow : Bool) :
    let area := fallbackSensorAreaForButtonNote zone
    let plan := clickPlan allow
      (decide (cursor.buttonUsed.getD zone 0 < ctx.input.getButtonClickCount zone))
      (decide (cursor.sensorUsed.getD area 0 < ctx.input.getSensorClickCount area))
    (plan.used_button, plan.used_sensor, spentCursor plan cursor zone area) =
      consume ctx zone allow cursor := by
  have h := SchedulerClickProof.consume_click_equiv ctx.input cursor allow zone
    (fallbackSensorAreaForButtonNote zone)
  simp only [SchedulerClickProof.consumeClickRuntime, click_plan_spec, bind_tc_ok,
    uncurry_apply_pair] at h
  exact Result.ok.inj h

/-- index < len makes the increment safe even at the maximum native length. -/
private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

section Extracted
variable {S Z N F Q C H E Event D Area Ops : Type}
variable (call : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area) (ops : Ops)
variable (rep : Representation S Z N F Q C H E Event D Area)
variable (ctx : Context) (cap : Capacity)

/-- Successful continuation of the actual extracted body. Admission, shared
click consumption, diff selection (sensor even without a click), frontier,
queue synchronization and both append choices are proved from point laws. -/
theorem process_body_spec (atoms : AtomicAgrees call ops rep ctx cap) (holds : S) (len index : Usize)
    (w : hold_schedule.HoldWork F Q C H E) (pre : List (ButtonZone × HoldNote))
    (zone : ButtonZone) (note : HoldNote) (rest : List (ButtonZone × HoldNote))
    (hholds : rep.holds holds = pre ++ (zone, note) :: rest)
    (hindex : pre.length = index.val) (hmore : index.val < len.val)
    (hfit : Fits cap (step ctx (interpret rep w) zone note)) :
    ∃ next nextIndex,
      hold_schedule.process_loop.body call holds ops len w index = ok (.cont (next, nextIndex)) ∧
      interpret rep next = step ctx (interpret rep w) zone note ∧
      nextIndex.val = index.val + 1 := by
  obtain ⟨nativeZone, before, hget, hzone, hbefore⟩ :=
    atoms.get holds index pre zone note rest hholds hindex
  obtain ⟨bd, hbd, hbdRep⟩ := atoms.button_diff before
  obtain ⟨sd, hsd, hsdRep⟩ := atoms.sensor_diff before
  obtain ⟨area, harea, hareaRep⟩ := atoms.fallback_area nativeZone
  have hareaRef : rep.area area = fallbackSensorAreaForButtonNote zone := by
    simpa only [hzone] using hareaRep
  let allow := eligible ctx (rep.frontiers w.frontiers) zone note
  let plan := clickPlan allow
    (decide ((rep.cursor w.cursor).buttonUsed.getD zone 0 < ctx.input.getButtonClickCount zone))
    (decide ((rep.cursor w.cursor).sensorUsed.getD (fallbackSensorAreaForButtonNote zone) 0 <
      ctx.input.getSensorClickCount (fallbackSensorAreaForButtonNote zone)))
  have hadmission : (if holdHeadAwaitsResolution note.state then
      (do
        let b ← if buttonQueueIndexUnlocked (rep.frontiers w.frontiers) zone note.buttonQueueIndex
          then ok (holdHeadEligibleForClick note ctx.currentTime) else ok false
        ok (w, b))
      else ok (w, false)) = (ok (w, allow) : Result (hold_schedule.HoldWork F Q C H E × Bool)) :=
    admission_ok w _ _ _
  have hbuttonAvailable := atoms.button_available w.cursor nativeZone
  have hsensorAvailable := atoms.sensor_available w.cursor area
  rw [hzone] at hbuttonAvailable
  rw [hareaRef] at hsensorAvailable
  have hplan : scheduler.consume_click allow
      (decide ((rep.cursor w.cursor).buttonUsed.getD zone 0 < ctx.input.getButtonClickCount zone))
      (decide ((rep.cursor w.cursor).sensorUsed.getD (fallbackSensorAreaForButtonNote zone) 0 <
        ctx.input.getSensorClickCount (fallbackSensorAreaForButtonNote zone))) = ok plan :=
    click_plan_spec _ _ _
  obtain ⟨spentButton, hbWrite, hbRep⟩ := atoms.use_button w.cursor nativeZone
  let cursor1 := if plan.used_button then spentButton else w.cursor
  obtain ⟨spentSensor, hsWrite, hsRep⟩ := atoms.use_sensor cursor1 area
  let cursor2 := if plan.used_sensor then spentSensor else cursor1
  have hbUse : (if plan.used_button then
      (do let next ← call.use_button ops w.cursor nativeZone; ok (next, true))
      else ok (w.cursor, false)) = (ok (cursor1, plan.used_button) : Result (C × Bool)) := by
    cases hb : plan.used_button <;> simp [hb, hbWrite, cursor1, bind_tc_ok]
  have hsUse : (if plan.used_sensor then
      (do let next ← call.use_sensor ops cursor1 area; ok (next, true))
      else ok (cursor1, false)) = (ok (cursor2, plan.used_sensor) : Result (C × Bool)) := by
    cases hs : plan.used_sensor <;> simp [hs, hsWrite, cursor2, bind_tc_ok]
  have hcRep : rep.cursor cursor2 =
      spentCursor plan (rep.cursor w.cursor) zone (fallbackSensorAreaForButtonNote zone) := by
    cases hb : plan.used_button <;> cases hs : plan.used_sensor <;>
      simp [cursor2, cursor1, hb, hs, hsRep, hbRep, hzone, hareaRef,
        spentCursor, spendButton, spendSensor]
  have hconsume : consume ctx zone allow (rep.cursor w.cursor) =
      (plan.used_button, plan.used_sensor, rep.cursor cursor2) := by
    rw [hcRep]
    exact (plan_consumes ctx (rep.cursor w.cursor) zone allow).symm
  let diff := if plan.used_button then bd else sd
  have hdiffCall : (if plan.used_button then ok bd else ok sd) = (ok diff : Result D) := by
    cases hb : plan.used_button <;> simp [diff, hb]
  have hdiff : rep.duration diff = difference ctx note plan.used_button := by
    cases hb : plan.used_button <;>
      simp [diff, difference, hb, hbdRep, hsdRep, hbefore]
  obtain ⟨after, event, hstep, hstepRep⟩ := atoms.step before diff
    (plan.used_button || plan.used_sensor)
    (ctx.input.getButtonHeld zone || fallbackSensorHeldForButtonNote ctx.input zone)
    (ctx.input.getButtonHeld zone)
    (fallbackPrevSensorHeldForButtonNote ctx.prevSensor zone)
  have hj : judgment ctx zone note plan.used_button plan.used_sensor =
      (rep.note after, event.map rep.event) := by
    simpa only [judgment, holdBodyPressedFromButtonOrSensor, hbefore, hdiff] using hstepRep.symm
  have hmodel : step ctx (interpret rep w) zone note =
      advance (interpret rep w) zone note (rep.note after) (event.map rep.event)
        (rep.cursor cursor2) := by
    unfold step
    change (let (button, sensor, cursor) := consume ctx zone allow (rep.cursor w.cursor)
      let (after, event) := judgment ctx zone note button sensor
      advance (interpret rep w) zone note after event cursor) = _
    simp only [hconsume, hj]
  have hparts :
      (rep.active w.active).length + (retained zone (rep.note after)).length ≤ cap.active ∧
      (rep.events w.events).length + (event.map rep.event).toList.length ≤ cap.events := by
    have hfit' := hfit
    unfold Fits at hfit'
    rw [hmodel] at hfit'
    simpa only [advance, interpret, List.length_append] using hfit'
  obtain ⟨advanced, hadvance, hadvanceRep⟩ := atoms.advance_frontier w.frontiers nativeZone
  let frontiers := if enteredHeadJudged note.state (rep.note after).state then advanced
    else w.frontiers
  have hfront : (if isHeadJudgedState note.state then ok (w.frontiers, phase (rep.note after))
      else (do
        let next ← if isHeadJudgedState (rep.note after).state
          then call.advance_frontier ops w.frontiers nativeZone else ok w.frontiers
        ok (next, phase (rep.note after)))) =
      (ok (frontiers, phase (rep.note after)) : Result (F × hold_schedule.HoldPhase)) := by
    cases hb : isHeadJudgedState note.state <;>
      cases ha : isHeadJudgedState (rep.note after).state <;>
      simp [hb, ha, hadvance, frontiers, enteredHeadJudged, bind_tc_ok]
  have hfrontRep : rep.frontiers frontiers =
      if enteredHeadJudged note.state (rep.note after).state
      then advanceSharedButtonQueue (rep.frontiers w.frontiers) zone else rep.frontiers w.frontiers := by
    unfold frontiers
    split <;> simp only [hadvanceRep, hzone]
  obtain ⟨queues, hqueue, hqueueRep⟩ := atoms.update_queue w.queues nativeZone after
  have hactive : ∃ active,
      (if !keepHoldActive (rep.note after) then ok w.active
        else call.append_active ops w.active nativeZone after) = ok active ∧
      rep.active active = rep.active w.active ++ retained zone (rep.note after) := by
    cases hk : keepHoldActive (rep.note after) with
    | false =>
        refine ⟨w.active, ?_, ?_⟩
        · simp [hk]
        · simp [retained, hk]
    | true =>
        obtain ⟨active, hcall, hrep⟩ := atoms.append_active w.active nativeZone after (by
          simpa [retained, hk] using hparts.1)
        refine ⟨active, ?_, ?_⟩
        · simpa [hk] using hcall
        · simpa [retained, hk, hzone] using hrep
  obtain ⟨active, hactiveCall, hactiveRep⟩ := hactive
  have hevents : ∃ events,
      (match event with
        | none => ok w.events
        | some event => call.append_event ops w.events event) = ok events ∧
      rep.events events = rep.events w.events ++ (event.map rep.event).toList := by
    cases event with
    | none =>
        refine ⟨w.events, rfl, ?_⟩
        simp
    | some event =>
        obtain ⟨events, hcall, hrep⟩ := atoms.append_event w.events event (by
          simpa using hparts.2)
        exact ⟨events, hcall, by simpa using hrep⟩
  obtain ⟨events, heventsCall, heventsRep⟩ := hevents
  obtain ⟨nextIndex, hi, hiVal⟩ := increment_index index len hmore
  let next : hold_schedule.HoldWork F Q C H E := ⟨frontiers, queues, cursor2, active, events⟩
  refine ⟨next, nextIndex, ?_, ?_, hiVal⟩
  · have hguard : index < len := by simpa using hmore
    simp only [hold_schedule.process_loop.body, hguard, ↓reduceIte, hget,
      hbd, hsd, atoms.button_pressed, atoms.sensor_pressed, atoms.prev_sensor_pressed,
      atoms.phase, hbefore, hzone, phase_await, atoms.unlocked, atoms.eligible,
      hadmission, harea, hbuttonAvailable, hsensorAvailable, hplan,
      hbUse, hsUse, or_ok, hdiffCall, hstep, phase_judged, hfront, hqueue,
      phase_ended, hactiveCall, heventsCall, hi, bind_tc_ok, uncurry_apply_pair, next]
    have hawait := phase_await note
    have hbeforeJudged := phase_judged note
    have hafterJudged := phase_judged (rep.note after)
    have hafterEnded := phase_ended (rep.note after)
    cases hbPhase : phase note <;> cases haPhase : phase (rep.note after) <;>
      simp only [hbPhase, haPhase, Result.ok.injEq] at hawait hbeforeJudged hafterJudged hafterEnded <;>
      simp only [← hawait, ← hbeforeJudged, ← hafterJudged, ← hafterEnded, hbPhase, haPhase] at hadmission hfront hactiveCall <;>
      simp only [hbPhase, haPhase, bind_tc_ok, hadmission, hbuttonAvailable, hsensorAvailable,
        hplan, hbUse, hsUse, or_ok, hdiffCall, hstep, atoms.phase, hfront, hqueue,
        hactiveCall, heventsCall, hi, uncurry_apply_pair, next] <;>
      cases event <;>
      simp only [Result.ok.injEq] at heventsCall <;>
      simp only [heventsCall, bind_tc_ok]
  · rw [hmodel]
    simp only [next, interpret, advance, hfrontRep, hqueueRep, hzone, hactiveRep, heventsRep]

end Extracted

/-- The history records successful calls of the ACTUAL generated body, not a
reference fold. The source handle is fixed throughout the trace. -/
inductive BodyHistory {S Z N F Q C H E Event D Area Ops : Type}
    (call : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area)
    (ops : Ops) (holds : S) (len : Usize) :
    (hold_schedule.HoldWork F Q C H E × Usize) →
    (hold_schedule.HoldWork F Q C H E × Usize) → List Nat → Prop
  | refl (state) : BodyHistory call ops holds len state state []
  | extend {initial current next visited}
      (past : BodyHistory call ops holds len initial current visited)
      (active : current.2.val < len.val)
      (body : hold_schedule.process_loop.body call holds ops len current.1 current.2 =
        ok (.cont next)) :
      BodyHistory call ops holds len initial next (visited ++ [current.2.val])

section Extracted
variable {S Z N F Q C H E Event D Area Ops : Type}
variable (call : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area) (ops : Ops)
variable (rep : Representation S Z N F Q C H E Event D Area)
variable (ctx : Context) (cap : Capacity)

/-- The stop branch is independent of all trait operations. -/
theorem process_body_done (holds : S) (len index : Usize)
    (w : hold_schedule.HoldWork F Q C H E) (hstop : len.val ≤ index.val) :
    hold_schedule.process_loop.body call holds ops len w index = ok (.done w) := by
  have hguard : ¬ index < len := by simpa using Nat.not_lt.mpr hstop
  simp only [hold_schedule.process_loop.body, hguard, ↓reduceIte]

/-- Total finite traversal under the point contracts. The measure is len-index;
there is no source-list replacement, extra fuel unit, or recursive Rust stack.
The postcondition also records the exact ordered range of visited indices. -/
theorem process_loop_spec (atoms : AtomicAgrees call ops rep ctx cap) (holds : S) (len : Usize)
    (initial : hold_schedule.HoldWork F Q C H E) (start : Usize)
    (hlen : (rep.holds holds).length = len.val) (hstart : start.val ≤ len.val)
    (hfit : Fits cap (resume ctx ((rep.holds holds).drop start.val) (interpret rep initial))) :
    hold_schedule.process_loop call holds initial ops len start ⦃ out =>
      interpret rep out = resume ctx ((rep.holds holds).drop start.val) (interpret rep initial) ∧
      ∃ finishIndex : Usize, finishIndex.val = len.val ∧
        BodyHistory call ops holds len (initial, start) (out, finishIndex)
          (List.range' start.val (len.val - start.val)) ⦄ := by
  unfold hold_schedule.process_loop
  apply loop.spec_decr_nat
    (fun state : hold_schedule.HoldWork F Q C H E × Usize => len.val - state.2.val)
    (fun state => start.val ≤ state.2.val ∧ state.2.val ≤ len.val ∧
      resume ctx ((rep.holds holds).drop state.2.val) (interpret rep state.1) =
        resume ctx ((rep.holds holds).drop start.val) (interpret rep initial) ∧
      BodyHistory call ops holds len (initial, start) state
        (List.range' start.val (state.2.val - start.val)))
  · rintro ⟨w, index⟩ ⟨hlow, hupp, hresume, history⟩
    dsimp only at hlow hupp hresume history ⊢
    try simp only [uncurry_apply_pair]
    by_cases hguard : index < len
    · have hmore : index.val < len.val := by simpa using hguard
      have hslot : index.val < (rep.holds holds).length := by omega
      let pre := (rep.holds holds).take index.val
      let item := (rep.holds holds)[index.val]'hslot
      let rest := (rep.holds holds).drop (index.val + 1)
      have hpending : (rep.holds holds).drop index.val = item :: rest :=
        List.drop_eq_getElem_cons hslot
      have hsplit : rep.holds holds = pre ++ (item.1, item.2) :: rest := by
        change rep.holds holds = pre ++ item :: rest
        rw [← hpending]
        exact (List.take_append_drop _ _).symm
      have hpre : pre.length = index.val := List.length_take_of_le (Nat.le_of_lt hslot)
      have hready := step_fits ctx cap (interpret rep w) item.1 item.2 rest (by
        change Fits cap (resume ctx (item :: rest) (interpret rep w))
        rw [← hpending, hresume]
        exact hfit)
      obtain ⟨next, nextIndex, hbody, hnextRep, hnextIndex⟩ :=
        process_body_spec call ops rep ctx cap atoms holds len index w pre item.1 item.2 rest
          hsplit hpre hmore hready
      have hnextResume :
          resume ctx ((rep.holds holds).drop nextIndex.val) (interpret rep next) =
            resume ctx ((rep.holds holds).drop start.val) (interpret rep initial) := by
        rw [hnextIndex, hnextRep]
        calc
          resume ctx ((rep.holds holds).drop (index.val + 1))
              (step ctx (interpret rep w) item.1 item.2) =
              resume ctx ((rep.holds holds).drop index.val) (interpret rep w) :=
            (resume_drop_step ctx (rep.holds holds) index.val (interpret rep w) hslot).symm
          _ = _ := hresume
      have hcount : nextIndex.val - start.val = (index.val - start.val) + 1 := by omega
      have hsum : start.val + (index.val - start.val) = index.val := by omega
      have hhistory : BodyHistory call ops holds len (initial, start) (next, nextIndex)
          (List.range' start.val (nextIndex.val - start.val)) := by
        rw [hcount, List.range'_concat]
        simpa only [Nat.one_mul, hsum] using BodyHistory.extend history hmore hbody
      apply exists_imp_spec
      refine ⟨.cont (next, nextIndex), hbody, ?_⟩
      dsimp only
      exact ⟨⟨by omega, by omega, hnextResume, hhistory⟩, by omega⟩
    · have hstop : len.val ≤ index.val := by simpa using hguard
      have heq : index.val = len.val := by omega
      have hdone := resume_drop_done ctx (rep.holds holds) index.val (interpret rep w) (by omega)
      apply exists_imp_spec
      refine ⟨.done w, process_body_done call ops holds len index w hstop,
        hdone.symm.trans hresume, index, heq, ?_⟩
      simpa only [heq] using history
  · dsimp only
    refine ⟨Nat.le_refl _, hstart, rfl, ?_⟩
    simpa only [Nat.sub_self, List.range'_zero] using
      BodyHistory.refl (call := call) (ops := ops) (holds := holds) (len := len) (initial, start)

/-- Full extracted wrapper, with its real initial length read and zero index.
Capacity is required only for the final model buffers. -/
theorem process_spec (atoms : AtomicAgrees call ops rep ctx cap) (holds : S)
    (initial : hold_schedule.HoldWork F Q C H E)
    (hfit : Fits cap (resume ctx (rep.holds holds) (interpret rep initial))) :
    hold_schedule.process call holds initial ops ⦃ out =>
      interpret rep out = resume ctx (rep.holds holds) (interpret rep initial) ∧
      ∃ len finishIndex : Usize,
        (rep.holds holds).length = len.val ∧ finishIndex.val = len.val ∧
        BodyHistory call ops holds len (initial, 0#usize) (out, finishIndex)
          (List.range' 0 (rep.holds holds).length) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.length holds
  obtain ⟨out, hrun, hout, finishIndex, hfinish, history⟩ := spec_imp_exists
    (process_loop_spec call ops rep ctx cap atoms holds len initial 0#usize hlen
      (by simp) (by simpa using hfit))
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, len, finishIndex, hlen, hfinish, ?_⟩
  · simp only [hold_schedule.process, hlength, hrun, bind_tc_ok, uncurry_apply_pair]
  · simpa using hout
  · simpa [hlen] using history

/-- The extracted traversal visits each original slot once, in order, even if
its note was already Body/Ended or earlier items changed the head queues. -/
theorem process_visits_once (atoms : AtomicAgrees call ops rep ctx cap) (holds : S)
    (initial : hold_schedule.HoldWork F Q C H E)
    (hfit : Fits cap (resume ctx (rep.holds holds) (interpret rep initial))) :
    hold_schedule.process call holds initial ops ⦃ out =>
      ∃ (len finishIndex : Usize) (visited : List Nat),
        BodyHistory call ops holds len (initial, 0#usize) (out, finishIndex) visited ∧
        visited.Nodup ∧ (∀ index, index ∈ visited ↔ index < (rep.holds holds).length) ∧
        visited.length = (rep.holds holds).length ⦄ := by
  obtain ⟨out, hrun, _, len, finishIndex, _, _, history⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms holds initial hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, len, finishIndex, List.range' 0 (rep.holds holds).length,
    history, List.nodup_range', ?_, List.length_range'⟩
  intro index
  simp only [List.mem_range'_1, Nat.zero_add, Nat.zero_le, true_and]

/-- Equality to the finite left-to-right model fold, retaining arbitrary old
output prefixes. This follows from the original recursion's model theorem. -/
theorem process_model_equiv (atoms : AtomicAgrees call ops rep ctx cap) (holds : S)
    (initial : hold_schedule.HoldWork F Q C H E)
    (hfit : Fits cap (resume ctx (rep.holds holds) (interpret rep initial))) :
    hold_schedule.process call holds initial ops ⦃ out =>
      interpret rep out = traverse ctx (rep.holds holds) (interpret rep initial) ⦄ := by
  obtain ⟨out, hrun, hout, _⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms holds initial hfit)
  apply exists_imp_spec
  exact ⟨out, hrun, hout.trans (traverse_eq_resume ctx _ _).symm⟩

/-- Runtime-facing empty-buffer initialization gives the ORIGINAL private
Scheduler.processHoldNotes tuple, in frontier/queue/active/event/cursor order.
It remains conditional on native initialization and the above facade laws;
Generated contains the shared kernel, not the concrete HoldScheduleFrame. -/
theorem process_equiv (atoms : AtomicAgrees call ops rep ctx cap) (holds : S)
    (initial : hold_schedule.HoldWork F Q C H E)
    (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor)
    (hinit : interpret rep initial = ⟨frontiers, queues, cursor, [], []⟩)
    (hfit : Fits cap (resume ctx (rep.holds holds) ⟨frontiers, queues, cursor, [], []⟩)) :
    hold_schedule.process call holds initial ops ⦃ out =>
      (interpret rep out).output = reference ctx (rep.holds holds) frontiers queues cursor ⦄ := by
  obtain ⟨out, hrun, hout, _⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms holds initial (by simpa only [hinit] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  rw [hout, hinit, resume_original]

end Extracted
end Verification.HoldScheduleProof
