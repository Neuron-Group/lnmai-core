import Verification.TouchScheduleModel
import Verification.QueueMaintenance
import Verification.SchedulerClicks

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 1600000

namespace Verification.TouchScheduleProof
open TouchScheduleModel
open QueueMaintenanceProof (Fits CursorAtomicAgrees ReplaceAtomicAgrees)
open private normalizeTouchQueueCursor replaceTouchQueueNote touchQueueResolved
  touchClickCanEngageJudge touchQueueIndexUnlocked advanceSharedTouchQueue
  tryUseButtonClickAt tryUseSensorClickAt hasUnusedTouchInputAt
  groupShareResult registerTouchGroupResult processTouchQueueHead from LnmaiCore.Scheduler

/-!
UNVALIDATED PROOF DRAFT. No Lean/lake, Cargo, tests or extraction were run.

Targets the ACTUAL Generated touch_schedule.process_loop.body/process_loop/
process installed from the touch/parser batch. In this extraction queue_ops
is called once per body and its returned M is reused for both normalizations
and replacement; group_id is also cached. The equations below retain that
order. The loop returns a five-tuple; process rebuilds TouchWork from it.

PointAgrees is NOT a whole-step contract. Queue traversals are discharged by
QueueMaintenanceProof, and click selection is derived from counter reads and
updates. GroupAgrees explicitly names two COMPOSITE group subcalls, and
LifecycleAgrees names only one note's lifecycle. These two boundaries still
need their independent proofs/encoding connections. No whole body, head pass,
automatic pass or frame equality is an assumption.

This file imports the separately frozen TouchScheduleModel.lean. Its source
SHA-256 is ad628ade27aea49cfad83e0119fd9e4145de12c4f820bd3cd2c0d34bfea884e8.
Mainline owns elaboration and any necessary repairs to its installed copy.
-/

structure Representation (Q N F C G E Event T D Area Zone Id Grade : Type) where
  queue : Q → ZoneQueue TouchNote
  note : N → TouchNote
  frontiers : F → SensorVec Nat
  cursor : C → Scheduler.ClickCursor
  groups : G → List GroupState
  events : E → List JudgeEvent
  event : Event → JudgeEvent
  time : T → TimePoint
  duration : D → Duration
  area : Area → SensorArea
  zone : Zone → ButtonZone
  id : Id → Nat
  grade : Grade → JudgeGrade

structure Capacity where
  queue : Nat
  events : Nat

variable {Q N F C G E Event T D Area Zone Id Grade M Ops : Type}

abbrev Call := touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M
abbrev RawWork := touch_schedule.TouchWork Q F C G E
abbrev RawOutput := Q × F × C × G × E

def queueRep (rep : Representation Q N F C G E Event T D Area Zone Id Grade) :
    QueueMaintenanceProof.Representation Q N TouchNote := ⟨rep.queue, rep.note⟩

def interpret (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E)) : Work :=
  ⟨rep.queue w.queue, rep.frontiers w.frontiers, rep.cursor w.cursor,
    rep.groups w.groups, rep.events w.events_rev⟩

def restore (out : RawOutput (Q := Q) (F := F) (C := C) (G := G) (E := E)) :
    RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E) :=
  ⟨out.1, out.2.1, out.2.2.1, out.2.2.2.1, out.2.2.2.2⟩

def sharedRep (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (shared : Option (Grade × D)) : Option (JudgeGrade × Duration) :=
  shared.map (fun p => (rep.grade p.1, rep.duration p.2))

def buttonAvailable (ctx : Context) (cursor : Scheduler.ClickCursor) (zone : ButtonZone) : Bool :=
  decide (cursor.buttonUsed.getD zone 0 < ctx.input.getButtonClickCount zone)

def sensorAvailable (ctx : Context) (cursor : Scheduler.ClickCursor) (area : SensorArea) : Bool :=
  decide (cursor.sensorUsed.getD area 0 < ctx.input.getSensorClickCount area)

def useButton (cursor : Scheduler.ClickCursor) (zone : ButtonZone) : Scheduler.ClickCursor :=
  { cursor with buttonUsed := cursor.buttonUsed.set zone (cursor.buttonUsed.getD zone 0 + 1) }

def useSensor (cursor : Scheduler.ClickCursor) (area : SensorArea) : Scheduler.ClickCursor :=
  { cursor with sensorUsed := cursor.sensorUsed.set area (cursor.sensorUsed.getD area 0 + 1) }

/-- Only point operations. queue_ops returns a maintenance instance with the
already-proved cursor/replacement POINT laws, never a whole normalize law.
Capacity bounds slots/event length; successful allocation is still explicit. -/
structure PointAgrees
    (call : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
      (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
      (Id := Id) (Grade := Grade) (M := M))
    (cursorCall : queue_maintenance.QueueCursor M Q N)
    (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (ctx : Context) (cap : Capacity) : Prop where
  queue_ops : ∃ m, call.queue_ops ops = ok m ∧
    CursorAtomicAgrees cursorCall m (queueRep rep) touchQueueResolved cap.queue ∧
    ReplaceAtomicAgrees replaceCall m (queueRep rep) (fun n => n.params.noteIndex) cap.queue
  queue_remaining : ∀ q, Fits cap.queue (rep.queue q) →
    ∃ fuel, call.queue_remaining ops q = ok fuel ∧ fuel.val = remaining (rep.queue q)
  queue_len : ∀ q, Fits cap.queue (rep.queue q) →
    ∃ len, call.queue_len ops q = ok len ∧ len.val = (rep.queue q).notes.length
  timing : ∀ n, ∃ t, call.timing ops n = ok t ∧
    rep.time t = (rep.note n).params.effectiveTiming
  sensor_diff : ∀ t, ∃ d, call.sensor_diff ops t = ok d ∧
    rep.duration d = (ctx.currentTime - ctx.touchPanelOffset) - rep.time t
  button_diff : ∀ t, ∃ d, call.button_diff ops t = ok d ∧
    rep.duration d = ctx.currentTime - rep.time t
  group_id : ∀ n, ∃ id, call.group_id ops n = ok id ∧
    id.map rep.id = (rep.note n).touchGroupId
  group_size : ∀ n, ∃ size, call.group_size ops n = ok size ∧
    rep.id size = (rep.note n).touchGroupSize
  eligible : ∀ n, call.eligible ops n =
    ok (touchClickCanEngageJudge (rep.note n) ctx.currentTime ctx.touchPanelOffset)
  unlocked : ∀ f n, call.unlocked ops f n =
    ok (touchQueueIndexUnlocked (rep.frontiers f) ctx.area (rep.note n).touchQueueIndex)
  button_ring_for_touch : call.button_ring_for_touch ops = ok ctx.buttonRingForTouch
  sensor_area : ∀ n, ∃ a, call.sensor_area ops n = ok a ∧ rep.area a = (rep.note n).sensorPos
  outer_button : ∀ a, ∃ z, call.outer_button ops a = ok z ∧
    z.map rep.zone = (rep.area a).toOuterButtonZone?
  button_available : ∀ c z, call.button_available ops c z =
    ok (buttonAvailable ctx (rep.cursor c) (rep.zone z))
  sensor_available : ∀ c a, call.sensor_available ops c a =
    ok (sensorAvailable ctx (rep.cursor c) (rep.area a))
  use_button : ∀ c z, ∃ out, call.use_button ops c z = ok out ∧
    rep.cursor out = useButton (rep.cursor c) (rep.zone z)
  use_sensor : ∀ c a, ∃ out, call.use_sensor ops c a = ok out ∧
    rep.cursor out = useSensor (rep.cursor c) (rep.area a)
  ended : ∀ n, call.ended ops n = ok (ended (rep.note n))
  event_grade : ∀ e, ∃ g, call.event_grade ops e = ok g ∧ rep.grade g = (rep.event e).grade
  is_miss_or_too_fast : ∀ g, call.is_miss_or_too_fast ops g = ok (rep.grade g).isMissOrTooFast
  prepend_event : ∀ es e, (rep.events es).length + 1 ≤ cap.events →
    ∃ out, call.prepend_event ops es e = ok out ∧ rep.events out = rep.event e :: rep.events es
  advance_frontier : ∀ f, ∃ out, call.advance_frontier ops f = ok out ∧
    rep.frontiers out = advanceSharedTouchQueue (rep.frontiers f) ctx.area

/-- Explicit COMPOSITE boundaries: first-match group lookup and registration.
These must be connected to the existing group-plan traversal and its point
laws. Universal successful calls here are assumptions, not allocator results. -/
structure GroupAgrees
    (call : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
      (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
      (Id := Id) (Grade := Grade) (M := M)) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade) : Prop where
  shared_result : ∀ gs id, ∃ shared, call.shared_result ops gs id = ok shared ∧
    sharedRep rep shared = groupShareResult (rep.groups gs) (rep.id id)
  register_group : ∀ gs id size grade diff, ∃ out,
    call.register_group ops gs id size grade diff = ok out ∧
    rep.groups out = registerTouchGroupResult (rep.groups gs) (rep.id id)
      (rep.id size) (rep.grade grade) (rep.duration diff)

/-- Only a single TouchNote, with exact time/diff/shared-result/output encoding.
No queue, click-counter, group-vector or Work update is hidden in this law. -/
structure LifecycleAgrees
    (call : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
      (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
      (Id := Id) (Grade := Grade) (M := M)) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade) (ctx : Context) : Prop where
  step : ∀ n d clicked shared, ∃ after event,
    call.step ops n d clicked shared = ok (after, event) ∧
    (rep.note after, event.map rep.event) = touchStep (rep.note n) ctx.currentTime
      (rep.duration d) clicked (sharedRep rep shared) ctx.style

variable (call : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
  (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
  (Id := Id) (Grade := Grade) (M := M))
  (cursorCall : queue_maintenance.QueueCursor M Q N)
  (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
  (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
  (ctx : Context) (cap : Capacity)
  (points : PointAgrees call cursorCall replaceCall ops rep ctx cap)
  (groups : GroupAgrees call ops rep) (life : LifecycleAgrees call ops rep ctx)

/-- The GENERATED prefix, including normalization before the head-fuel guard. -/
def enter (queue : Q) : Result (M × Q) := do
  let pending ← call.queue_remaining ops queue
  let m ← call.queue_ops ops
  let next ← queue_maintenance.normalize_cursor_fuel cursorCall pending queue m
  ok (m, next)

def normalizeQueue (m : M) (queue : Q) : Result Q := do
  let pending ← call.queue_remaining ops queue
  queue_maintenance.normalize_cursor_fuel cursorCall pending queue m

def replaceThenNormalize (m : M) (queue : Q) (after : N) : Result Q := do
  let len ← call.queue_len ops queue
  let replaced ← queue_maintenance.replace_all replaceCall len queue after m
  normalizeQueue call cursorCall ops m replaced

include points in
theorem normalize_queue_spec (m : M)
    (cpoints : CursorAtomicAgrees cursorCall m (queueRep rep) touchQueueResolved cap.queue)
    (queue : Q) (hfit : Fits cap.queue (rep.queue queue)) :
    ∃ out, normalizeQueue call cursorCall ops m queue = ok out ∧
      rep.queue out = normalizeTouchQueueCursor (rep.queue queue) ∧
      Fits cap.queue (rep.queue out) := by
  obtain ⟨pending, hp, hpRep⟩ := points.queue_remaining queue hfit
  obtain ⟨out, hrun, href, _, houtFit⟩ := spec_imp_exists
    (QueueMaintenanceProof.normalize_spec cursorCall m (queueRep rep) touchQueueResolved
      cap.queue cpoints queue pending hfit)
  refine ⟨out, ?_, ?_, houtFit⟩
  · simpa only [normalizeQueue, hp, bind_tc_ok] using hrun
  · simpa only [queueRep, hpRep, QueueMaintenanceModel.touch_pending_equiv] using href

include points in
theorem enter_spec (queue : Q) (hfit : Fits cap.queue (rep.queue queue)) :
    ∃ m out, enter call cursorCall ops queue = ok (m, out) ∧
      rep.queue out = normalizeTouchQueueCursor (rep.queue queue) ∧
      Fits cap.queue (rep.queue out) ∧
      CursorAtomicAgrees cursorCall m (queueRep rep) touchQueueResolved cap.queue ∧
      ReplaceAtomicAgrees replaceCall m (queueRep rep) (fun n => n.params.noteIndex) cap.queue := by
  obtain ⟨pending, hp, hpRep⟩ := points.queue_remaining queue hfit
  obtain ⟨m, hm, hc, hr⟩ := points.queue_ops
  obtain ⟨out, hrun, href, _, houtFit⟩ := spec_imp_exists
    (QueueMaintenanceProof.normalize_spec cursorCall m (queueRep rep) touchQueueResolved
      cap.queue hc queue pending hfit)
  refine ⟨m, out, ?_, ?_, houtFit, hc, hr⟩
  · simp only [enter, hp, hm, hrun, bind_tc_ok]
  · simpa only [queueRep, hpRep, QueueMaintenanceModel.touch_pending_equiv] using href

include points in
theorem replace_then_normalize_spec (m : M)
    (cpoints : CursorAtomicAgrees cursorCall m (queueRep rep) touchQueueResolved cap.queue)
    (rpoints : ReplaceAtomicAgrees replaceCall m (queueRep rep)
      (fun n => n.params.noteIndex) cap.queue)
    (queue : Q) (after : N) (hfit : Fits cap.queue (rep.queue queue)) :
    ∃ out, replaceThenNormalize call cursorCall replaceCall ops m queue after = ok out ∧
      rep.queue out = normalizeTouchQueueCursor
        (replaceTouchQueueNote (rep.queue queue) (rep.note after)) ∧
      Fits cap.queue (rep.queue out) := by
  obtain ⟨len, hl, hlRep⟩ := points.queue_len queue hfit
  obtain ⟨replaced, hr, hrRep, _, _, hrFit⟩ := spec_imp_exists
    (QueueMaintenanceProof.replace_spec replaceCall m (queueRep rep)
      (fun n => n.params.noteIndex) cap.queue rpoints len queue after hlRep hfit)
  obtain ⟨out, hn, hnRep, hnFit⟩ := normalize_queue_spec call cursorCall replaceCall ops
    rep ctx cap points m cpoints replaced hrFit
  refine ⟨out, ?_, ?_, hnFit⟩
  · simp only [replaceThenNormalize, hl, hr, hn, bind_tc_ok]
  · rw [hnRep]
    simpa only [queueRep, QueueMaintenanceModel.replace_touch_eq] using
      congrArg normalizeTouchQueueCursor hrRep

def attemptButton (cursor : C) (zone : Zone) : Result (C × Bool) := do
  let available ← call.button_available ops cursor zone
  let next ← if available then call.use_button ops cursor zone else ok cursor
  ok (next, available)

def attemptSensor (cursor : C) (area : Area) : Result (C × Bool) := do
  let available ← call.sensor_available ops cursor area
  let next ← if available then call.use_sensor ops cursor area else ok cursor
  ok (next, available)

theorem try_button_reference (cursor : Scheduler.ClickCursor) (zone : ButtonZone) :
    tryUseButtonClickAt ctx.input cursor zone =
      if buttonAvailable ctx cursor zone then (true, useButton cursor zone) else (false, cursor) := by
  simp [tryUseButtonClickAt, buttonAvailable, useButton]

theorem try_sensor_reference (cursor : Scheduler.ClickCursor) (area : SensorArea) :
    tryUseSensorClickAt ctx.input cursor area =
      if sensorAvailable ctx cursor area then (true, useSensor cursor area) else (false, cursor) := by
  simp [tryUseSensorClickAt, sensorAvailable, useSensor]

include points in
theorem attempt_button_spec (cursor : C) (zone : Zone) :
    ∃ next clicked, attemptButton call ops cursor zone = ok (next, clicked) ∧
      (clicked, rep.cursor next) = tryUseButtonClickAt ctx.input (rep.cursor cursor) (rep.zone zone) := by
  obtain ⟨next, hn, hnRep⟩ := points.use_button cursor zone
  cases ha : buttonAvailable ctx (rep.cursor cursor) (rep.zone zone)
  · refine ⟨cursor, false, ?_, ?_⟩
    · simp [attemptButton, points.button_available, ha]
    · simp [try_button_reference, ha]
  · refine ⟨next, true, ?_, ?_⟩
    · simp [attemptButton, points.button_available, ha, hn]
    · simp [try_button_reference, ha, hnRep]

include points in
theorem attempt_sensor_spec (cursor : C) (area : Area) :
    ∃ next clicked, attemptSensor call ops cursor area = ok (next, clicked) ∧
      (clicked, rep.cursor next) = tryUseSensorClickAt ctx.input (rep.cursor cursor) (rep.area area) := by
  obtain ⟨next, hn, hnRep⟩ := points.use_sensor cursor area
  cases ha : sensorAvailable ctx (rep.cursor cursor) (rep.area area)
  · refine ⟨cursor, false, ?_, ?_⟩
    · simp [attemptSensor, points.sensor_available, ha]
    · simp [try_sensor_reference, ha]
  · refine ⟨next, true, ?_, ?_⟩
    · simp [attemptSensor, points.sensor_available, ha, hn]
    · simp [try_sensor_reference, ha, hnRep]

def buttonChoice (ctx : Context) (cursor : Scheduler.ClickCursor)
    (area : SensorArea) (allow : Bool) : Bool × Scheduler.ClickCursor :=
  if allow && ctx.buttonRingForTouch then
    match area.toOuterButtonZone? with
    | some zone => tryUseButtonClickAt ctx.input cursor zone
    | none => (false, cursor)
  else (false, cursor)

def chooseButton (allow : Bool) (cursor : C) (area : Area) : Result (C × Bool) := do
  if allow then
    let ring ← call.button_ring_for_touch ops
    if ring then
      let zone ← call.outer_button ops area
      match zone with
      | none => ok (cursor, false)
      | some zone => attemptButton call ops cursor zone
    else ok (cursor, false)
  else ok (cursor, false)

include points in
theorem choose_button_spec (allow : Bool) (cursor : C) (area : Area) :
    ∃ next clicked, chooseButton call ops allow cursor area = ok (next, clicked) ∧
      (clicked, rep.cursor next) = buttonChoice ctx (rep.cursor cursor) (rep.area area) allow := by
  cases allow with
  | false => exact ⟨cursor, false, rfl, rfl⟩
  | true =>
      cases hring : ctx.buttonRingForTouch with
      | false =>
          refine ⟨cursor, false, ?_, ?_⟩ <;>
            simp [chooseButton, points.button_ring_for_touch, buttonChoice, hring]
      | true =>
          obtain ⟨zone, hz, hzRep⟩ := points.outer_button area
          cases zone with
          | none =>
              refine ⟨cursor, false, ?_, ?_⟩ <;>
                simp [chooseButton, points.button_ring_for_touch, hring, hz,
                  buttonChoice, ← hzRep]
          | some zone =>
              obtain ⟨next, clicked, hn, hnRep⟩ := attempt_button_spec call cursorCall replaceCall
                ops rep ctx cap points cursor zone
              refine ⟨next, clicked, ?_, ?_⟩
              · simp [chooseButton, points.button_ring_for_touch, hring, hz, hn]
              · simpa [buttonChoice, hring, ← hzRep] using hnRep

/-- The tuple shuffles are the extractor's, not a change of effect order. -/
def consumeClicks (allow : Bool) (cursor : C) (area : Area) : Result (Bool × Bool × C) := do
  let (cursor, button) ← chooseButton call ops allow cursor area
  let (cursor, button, sensor) ← if allow then do
    let (next, sensor) ← if button then ok (cursor, false)
      else attemptSensor call ops cursor area
    ok (next, button, sensor)
    else ok (cursor, button, false)
  ok (button, sensor, cursor)

include points in
theorem consume_clicks_spec (allow : Bool) (cursor : C) (area : Area)
    (before : TouchNote) (harea : rep.area area = before.sensorPos) :
    ∃ button sensor next, consumeClicks call ops allow cursor area = ok (button, sensor, next) ∧
      (button, sensor, rep.cursor next) = consume ctx before allow (rep.cursor cursor) := by
  obtain ⟨cb, button, hb, hbRep⟩ := choose_button_spec call cursorCall replaceCall ops
    rep ctx cap points allow cursor area
  obtain ⟨cs, sensor, hs, hsRep⟩ := attempt_sensor_spec call cursorCall replaceCall ops
    rep ctx cap points cb area
  have href : consume ctx before allow (rep.cursor cursor) =
      let (sensor, next) := if allow && !button then
        tryUseSensorClickAt ctx.input (rep.cursor cb) before.sensorPos else (false, rep.cursor cb)
      (button, sensor, next) := by
    change (let (button, cb) := buttonChoice ctx (rep.cursor cursor) before.sensorPos allow
      let (sensor, next) := if allow && !button then
        tryUseSensorClickAt ctx.input cb before.sensorPos else (false, cb)
      (button, sensor, next)) = _
    rw [← harea, ← hbRep]
  rw [harea] at hsRep
  cases allow <;> cases button
  · refine ⟨false, false, cb, ?_, ?_⟩ <;> simp [consumeClicks, hb, href]
  · refine ⟨true, false, cb, ?_, ?_⟩ <;> simp [consumeClicks, hb, href]
  · refine ⟨false, sensor, cs, ?_, ?_⟩ <;> simp [consumeClicks, hb, hs, href, ← hsRep]
  · refine ⟨true, false, cb, ?_, ?_⟩ <;> simp [consumeClicks, hb, href]

def readShared (gs : G) (id : Option Id) : Result (Option (Grade × D)) :=
  match id with
  | none => ok none
  | some id => call.shared_result ops gs id

include groups in
theorem read_shared_spec (gs : G) (id : Option Id) (note : TouchNote)
    (hid : id.map rep.id = note.touchGroupId) :
    ∃ shared, readShared call ops gs id = ok shared ∧
      sharedRep rep shared = sharedResult (rep.groups gs) note := by
  cases id with
  | none =>
      refine ⟨none, rfl, ?_⟩
      simp [sharedRep, sharedResult, ← hid]
  | some id =>
      obtain ⟨shared, hs, hsRep⟩ := groups.shared_result gs id
      refine ⟨shared, hs, ?_⟩
      simpa [sharedResult, ← hid] using hsRep

theorem shared_is_none (shared : Option (Grade × D)) :
    core.option.Option.is_none shared = (sharedRep rep shared).isNone := by
  cases shared <;> rfl

structure Prepared (N C Event D Area Id : Type) where
  groupId : Option Id
  area : Area
  cursor : C
  diff : D
  clicked : Bool
  after : N
  event : Option Event

def prepare (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E)) (before : N) :
    Result (Prepared N C Event D Area Id) := do
  let timing ← call.timing ops before
  let sensorDiff ← call.sensor_diff ops timing
  let id ← call.group_id ops before
  let shared ← readShared call ops w.groups id
  let allow ← if core.option.Option.is_none shared then do
    let eligible ← call.eligible ops before
    if eligible then call.unlocked ops w.frontiers before else ok false
    else ok false
  let area ← call.sensor_area ops before
  let (button, sensor, cursor) ← consumeClicks call ops allow w.cursor area
  let clicked ← if button then ok true else ok sensor
  let diff ← if button then call.button_diff ops timing else ok sensorDiff
  let (after, event) ← call.step ops before diff clicked shared
  ok ⟨id, area, cursor, diff, clicked, after, event⟩

include points groups life in
theorem prepare_spec (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E)) (before : N) :
    ∃ p button sensor, prepare call ops w before = ok p ∧
      p.groupId.map rep.id = (rep.note before).touchGroupId ∧
      rep.area p.area = (rep.note before).sensorPos ∧
      (button, sensor, rep.cursor p.cursor) = consume ctx (rep.note before)
        (eligible ctx (rep.frontiers w.frontiers) (rep.groups w.groups) (rep.note before))
        (rep.cursor w.cursor) ∧
      p.clicked = (button || sensor) ∧
      rep.duration p.diff = difference ctx (rep.note before) button ∧
      (rep.note p.after, p.event.map rep.event) =
        judgment ctx (rep.groups w.groups) (rep.note before) button sensor := by
  obtain ⟨timing, ht, htRep⟩ := points.timing before
  obtain ⟨ds, hds, hdsRep⟩ := points.sensor_diff timing
  obtain ⟨db, hdb, hdbRep⟩ := points.button_diff timing
  obtain ⟨id, hi, hiRep⟩ := points.group_id before
  obtain ⟨shared, hshared, hsharedRep⟩ := read_shared_spec call ops rep groups
    w.groups id (rep.note before) hiRep
  obtain ⟨area, ha, haRep⟩ := points.sensor_area before
  let allow := eligible ctx (rep.frontiers w.frontiers) (rep.groups w.groups) (rep.note before)
  obtain ⟨button, sensor, cursor, hc, hcRep⟩ := consume_clicks_spec call cursorCall replaceCall
    ops rep ctx cap points allow w.cursor area (rep.note before) haRep
  let diff := if button then db else ds
  obtain ⟨after, event, hj, hjRep⟩ := life.step before diff (button || sensor) shared
  have hdRep : rep.duration diff = difference ctx (rep.note before) button := by
    cases button <;> simp [diff, difference, hdbRep, hdsRep, htRep]
  have hallow : (if core.option.Option.is_none shared then do
      let eligible ← call.eligible ops before
      if eligible then call.unlocked ops w.frontiers before else ok false
      else ok false) = ok allow := by
    rw [shared_is_none rep shared, hsharedRep]
    cases hshare : (sharedResult (rep.groups w.groups) (rep.note before)).isNone <;>
      cases helig : touchClickCanEngageJudge (rep.note before) ctx.currentTime ctx.touchPanelOffset <;>
      simp [allow, eligible, hshare, helig, points.eligible, points.unlocked]
  refine ⟨⟨id, area, cursor, diff, button || sensor, after, event⟩, button, sensor,
    ?_, hiRep, haRep, hcRep, rfl, hdRep, ?_⟩
  · simp only [prepare, ht, hds, hi, hshared, hallow, ha, hc, bind_tc_ok]
    have hbound := congrArg (fun r => r >>= fun (a, e) =>
      ok (Prepared.mk id area cursor diff (button || sensor) a e)) hj
    cases button <;> cases sensor <;>
      simpa only [diff, Bool.false_eq_true, Bool.true_eq, ↓reduceIte,
        Bool.false_or, Bool.true_or, hdb, bind_tc_ok] using hbound
  · simpa only [judgment, hdRep, hsharedRep] using hjRep

/-- The cached group id is read before judgment. The selected diff, not the
event's recorded diff, is passed to registration, as in Generated. -/
def registerVote (gs : G) (before : N) (id : Option Id)
    (event : Event) (clicked : Bool) (diff : D) : Result G := do
  if clicked then
    let grade ← call.event_grade ops event
    let rejected ← call.is_miss_or_too_fast ops grade
    if rejected then ok gs else
      match id with
      | none => ok gs
      | some id =>
          let size ← call.group_size ops before
          call.register_group ops gs id size grade diff
  else ok gs

include points groups in
theorem register_vote_spec (gs : G) (before : N) (id : Option Id)
    (event : Event) (clicked : Bool) (diff : D)
    (hid : id.map rep.id = (rep.note before).touchGroupId) :
    ∃ out, registerVote call ops gs before id event clicked diff = ok out ∧
      rep.groups out = updateGroups (rep.groups gs) (rep.note before)
        (some (rep.event event)) clicked (rep.duration diff) := by
  cases clicked with
  | false =>
      refine ⟨gs, rfl, ?_⟩
      simp only [updateGroups, Bool.not_false, Bool.true_or, Bool.true_eq, ↓reduceIte]
  | true =>
      obtain ⟨grade, hg, hgRep⟩ := points.event_grade event
      cases hrejected : (rep.event event).grade.isMissOrTooFast with
      | true =>
          refine ⟨gs, ?_, ?_⟩
          · simp [registerVote, hg, points.is_miss_or_too_fast, hgRep, hrejected]
          · simp [updateGroups, hrejected]
      | false =>
          cases id with
          | none =>
              refine ⟨gs, ?_, ?_⟩
              · simp [registerVote, hg, points.is_miss_or_too_fast, hgRep, hrejected]
              · simp [updateGroups, hrejected, ← hid]
          | some id =>
              obtain ⟨size, hs, hsRep⟩ := points.group_size before
              obtain ⟨out, ho, hoRep⟩ := groups.register_group gs id size grade diff
              refine ⟨out, ?_, ?_⟩
              · simp [registerVote, hg, points.is_miss_or_too_fast, hgRep,
                  hrejected, hs, ho]
              · simpa [updateGroups, hrejected, ← hid, hsRep, hgRep] using hoRep

/-- Prepend first, then conditionally register. No reversal occurs here. -/
def recordEvent (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (before : N) (p : Prepared N C Event D Area Id) : Result (G × E) :=
  match p.event with
  | none => ok (w.groups, w.events_rev)
  | some event => do
      let es ← call.prepend_event ops w.events_rev event
      let gs ← registerVote call ops w.groups before p.groupId event p.clicked p.diff
      ok (gs, es)

include points groups in
theorem record_event_spec
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (before : N) (p : Prepared N C Event D Area Id)
    (hid : p.groupId.map rep.id = (rep.note before).touchGroupId)
    (hefit : (advance ctx (interpret rep w) (rep.note before) (rep.note p.after)
      (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)).eventsRev.length
        ≤ cap.events) :
    ∃ gs es, recordEvent call ops w before p = ok (gs, es) ∧
      rep.groups gs = updateGroups (rep.groups w.groups) (rep.note before)
        (p.event.map rep.event) p.clicked (rep.duration p.diff) ∧
      rep.events es = (p.event.map rep.event).toList ++ rep.events w.events_rev := by
  cases hevent : p.event with
  | none =>
      refine ⟨w.groups, w.events_rev, ?_, ?_, ?_⟩ <;>
        simp [recordEvent, updateGroups, hevent]
  | some event =>
      have hcapacity : (rep.events w.events_rev).length + 1 ≤ cap.events := by
        simpa only [advance, interpret, hevent, Option.map_some, List.length_cons] using hefit
      obtain ⟨es, he, heRep⟩ := points.prepend_event w.events_rev event hcapacity
      obtain ⟨gs, hg, hgRep⟩ := register_vote_spec call cursorCall replaceCall ops
        rep ctx cap points groups w.groups before p.groupId event p.clicked p.diff hid
      refine ⟨gs, es, ?_, ?_, ?_⟩
      · simp only [recordEvent, hevent, he, hg, bind_tc_ok]
      · simpa only [hevent, Option.map_some] using hgRep
      · simpa [hevent] using heRep

/-- Generated probes the outer button before reading the ring-enable flag,
even when that flag will be false. Only an Ended note reaches this helper. -/
def unusedInput (cursor : C) (area : Area) : Result Bool := do
  let zone ← call.outer_button ops area
  let ring ← match zone with
    | none => ok false
    | some zone => call.button_available ops cursor zone
  let enabled ← call.button_ring_for_touch ops
  let sensor ← call.sensor_available ops cursor area
  scheduler.touch_input_available enabled sensor (some ring)

include points in
theorem unused_input_spec (cursor : C) (area : Area) :
    unusedInput call ops cursor area =
      ok (hasUnusedTouchInputAt ctx.input (rep.cursor cursor) (rep.area area)
        ctx.buttonRingForTouch) := by
  obtain ⟨zone, hz, hzRep⟩ := points.outer_button area
  have href := SchedulerClickProof.touch_input_equiv ctx.input (rep.cursor cursor)
    (rep.area area) ctx.buttonRingForTouch
  have hreturn (r : Result Bool) : (do let b ← r; ok b) = r := by
    cases r <;> rfl
  cases zone with
  | none =>
      simpa [unusedInput, hz, points.button_ring_for_touch, points.sensor_available,
        SchedulerClickProof.touchInputRuntime, ← hzRep, sensorAvailable,
        scheduler.touch_input_available, bind_tc_ok, hreturn] using href
  | some zone =>
      simpa [unusedInput, hz, points.button_available, points.button_ring_for_touch,
        points.sensor_available, SchedulerClickProof.touchInputRuntime, ← hzRep,
        buttonAvailable, sensorAvailable, scheduler.touch_input_available, bind_tc_ok, hreturn] using href

def interpretFlow (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (result : ControlFlow
      (Usize × RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
      (RawOutput (Q := Q) (F := F) (C := C) (G := G) (E := E))) :
    ControlFlow (Nat × Work) Outcome :=
  match result with
  | .cont (fuel, w) => .cont (fuel.val, interpret rep w)
  | .done out => .done (interpret rep (restore out)).output

def FlowFits (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat)
    (result : ControlFlow
      (Usize × RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
      (RawOutput (Q := Q) (F := F) (C := C) (G := G) (E := E))) : Prop :=
  match result with
  | .cont (_, w) => Fits capacity (rep.queue w.queue)
  | .done out => Fits capacity (rep.queue out.1)

/-- Names the exact suffix after lifecycle, retaining the SAME maintenance
instance and BEFORE note. Newly resolved and Ended are separate predicates. -/
def finish (m : M) (fuel : Usize)
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (before : N) (p : Prepared N C Event D Area Id) :
    Result (ControlFlow
      (Usize × RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
      (RawOutput (Q := Q) (F := F) (C := C) (G := G) (E := E))) := do
  let wasResolved ← cursorCall.resolved m before
  let newlyResolved ← if wasResolved then ok false else cursorCall.resolved m p.after
  let (gs, es) ← recordEvent call ops w before p
  let queue ← replaceThenNormalize call cursorCall replaceCall ops m w.queue p.after
  let frontiers ← if newlyResolved then call.advance_frontier ops w.frontiers else ok w.frontiers
  let isEnded ← call.ended ops p.after
  if isEnded then
    let more ← unusedInput call ops p.cursor p.area
    if more then
      let nextFuel ← fuel - 1#usize
      ok (.cont (nextFuel, ⟨queue, frontiers, p.cursor, gs, es⟩))
    else ok (.done (queue, frontiers, p.cursor, gs, es))
  else ok (.done (queue, frontiers, p.cursor, gs, es))

/-- A proved reassociation of ACTUAL Generated code. There is no abstract
body callback, replacement loop callback, or body-equivalence premise. -/
theorem body_reassociated (fuel : Usize)
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E)) :
    touch_schedule.process_loop.body call cursorCall replaceCall ops fuel w = (do
      let (m, queue) ← enter call cursorCall ops w.queue
      let base : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E) := { w with queue := queue }
      if fuel = 0#usize then
        ok (.done (queue, w.frontiers, w.cursor, w.groups, w.events_rev))
      else
        let found ← cursorCall.peek m queue
        match found with
        | none => ok (.done (queue, w.frontiers, w.cursor, w.groups, w.events_rev))
        | some before =>
            let p ← prepare call ops base before
            finish call cursorCall replaceCall ops m fuel base before p) := by
  simp only [touch_schedule.process_loop.body, enter, prepare, consumeClicks,
    chooseButton, attemptButton, attemptSensor, readShared, finish, recordEvent,
    registerVote, replaceThenNormalize, normalizeQueue, unusedInput,
    bind_assoc_eq, bind_tc_ok, bind_eq_iff, Prod.forall, uncurry_apply_pair]
  all_goals
    repeat' first
      | rfl
      | (intro)
      | simp (config := { failIfUnchanged := true }) only
          [bind_assoc_eq, bind_tc_ok, bind_eq_iff, Prod.forall, uncurry_apply_pair]
      | split

include points groups in
theorem finish_spec (m : M)
    (cpoints : CursorAtomicAgrees cursorCall m (queueRep rep) touchQueueResolved cap.queue)
    (rpoints : ReplaceAtomicAgrees replaceCall m (queueRep rep)
      (fun n => n.params.noteIndex) cap.queue)
    (fuel : Usize) (hfuel : 0 < fuel.val)
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (before : N) (p : Prepared N C Event D Area Id)
    (hid : p.groupId.map rep.id = (rep.note before).touchGroupId)
    (harea : rep.area p.area = (rep.note before).sensorPos)
    (hqfit : Fits cap.queue (rep.queue w.queue))
    (hefit : (advance ctx (interpret rep w) (rep.note before) (rep.note p.after)
      (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)).eventsRev.length
        ≤ cap.events) :
    ∃ result, finish call cursorCall replaceCall ops m fuel w before p = ok result ∧
      interpretFlow rep result =
        (let next := advance ctx (interpret rep w) (rep.note before) (rep.note p.after)
          (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)
        if ended (rep.note p.after) && unused ctx (rep.note before) (rep.cursor p.cursor)
        then .cont (fuel.val - 1, next) else .done next.output) ∧
      FlowFits rep cap.queue result := by
  obtain ⟨gs, es, he, hgRep, heRep⟩ := record_event_spec call cursorCall replaceCall
    ops rep ctx cap points groups w before p hid hefit
  obtain ⟨queue, hq, hqRep, hqFit⟩ := replace_then_normalize_spec call cursorCall
    replaceCall ops rep ctx cap points m cpoints rpoints w.queue p.after hqfit
  obtain ⟨advanced, ha, haRep⟩ := points.advance_frontier w.frontiers
  obtain ⟨less, hless, hlessRep⟩ := spec_imp_exists
    (UScalar.sub_spec (x := fuel) (y := 1#usize) (by scalar_tac))
  let newly := !touchQueueResolved (rep.note before) && touchQueueResolved (rep.note p.after)
  let frontiers := if newly then advanced else w.frontiers
  let nextRaw : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E) :=
    ⟨queue, frontiers, p.cursor, gs, es⟩
  let next := advance ctx (interpret rep w) (rep.note before) (rep.note p.after)
    (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)
  have hb : cursorCall.resolved m before = ok (touchQueueResolved (rep.note before)) :=
    cpoints.resolved before
  have hafter : cursorCall.resolved m p.after = ok (touchQueueResolved (rep.note p.after)) :=
    cpoints.resolved p.after
  have hnewly : (if touchQueueResolved (rep.note before) then (ok false : Result Bool)
      else ok (touchQueueResolved (rep.note p.after))) = ok newly := by
    cases hr : touchQueueResolved (rep.note before) <;> simp [newly, hr]
  have hf : (if newly then call.advance_frontier ops w.frontiers else ok w.frontiers) =
      ok frontiers := by
    cases hr : newly <;> simp [frontiers, hr, ha]
  have hfRep : rep.frontiers frontiers =
      (if newly then advanceSharedTouchQueue (rep.frontiers w.frontiers) ctx.area
        else rep.frontiers w.frontiers) := by
    cases hr : newly <;> simp [frontiers, hr, haRep]
  have hnext : interpret rep nextRaw = next := by
    dsimp only [interpret, nextRaw, next, advance]
    rw [hqRep, hfRep, hgRep, heRep]
    cases p.event <;> rfl
  have hout : (interpret rep (restore (queue, frontiers, p.cursor, gs, es))).output =
      next.output := congrArg Work.output hnext
  have hu : unusedInput call ops p.cursor p.area =
      ok (unused ctx (rep.note before) (rep.cursor p.cursor)) := by
    simpa only [unused, harea] using
      unused_input_spec call cursorCall replaceCall ops rep ctx cap points p.cursor p.area
  have hrun : finish call cursorCall replaceCall ops m fuel w before p = ok
      (if ended (rep.note p.after) && unused ctx (rep.note before) (rep.cursor p.cursor)
      then .cont (less, nextRaw) else .done (queue, frontiers, p.cursor, gs, es)) := by
    cases hend : ended (rep.note p.after) <;>
      cases hmore : unused ctx (rep.note before) (rep.cursor p.cursor) <;>
      simp only [finish, hb, hafter, bind_tc_ok, hnewly, he, hq, hf,
        points.ended, hend, hu, hmore, hless, bind_tc_ok, nextRaw,
        Bool.false_and, Bool.true_and, Bool.false_eq_true, Bool.true_eq, ↓reduceIte] <;> rfl
  refine ⟨_, hrun, ?_, ?_⟩
  · cases hcontinue : (ended (rep.note p.after) &&
        unused ctx (rep.note before) (rep.cursor p.cursor)) <;>
      simp only [hcontinue, Bool.false_eq_true, Bool.true_eq, ↓reduceIte,
        interpretFlow, hlessRep.1, hnext, hout] <;> rfl
  · cases hcontinue : (ended (rep.note p.after) &&
        unused ctx (rep.note before) (rep.cursor p.cursor)) <;>
      simpa only [hcontinue, Bool.false_eq_true, Bool.true_eq, ↓reduceIte,
        FlowFits, nextRaw] using hqFit

include points groups life in
/-- Each continuation preserves the result of the ORIGINAL reference
recursion and strictly decreases the Usize fuel. Fits is also carried through
both normalization loops and the full replacement loop, including every exit. -/
theorem body_spec (fuel : Usize)
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hqfit : Fits cap.queue (rep.queue w.queue))
    (hefit : (resume ctx fuel.val (interpret rep w)).2.2.2.2.length ≤ cap.events) :
    touch_schedule.process_loop.body call cursorCall replaceCall ops fuel w ⦃ result =>
      match result with
      | .cont (nextFuel, next) =>
          Fits cap.queue (rep.queue next.queue) ∧
          resume ctx nextFuel.val (interpret rep next) = resume ctx fuel.val (interpret rep w) ∧
          nextFuel.val < fuel.val
      | .done out =>
          Fits cap.queue (rep.queue out.1) ∧
          (interpret rep (restore out)).output = resume ctx fuel.val (interpret rep w) ⦄ := by
  obtain ⟨m, queue, he, hqRep, hqFit, cpoints, rpoints⟩ :=
    enter_spec call cursorCall replaceCall ops rep ctx cap points w.queue hqfit
  let base : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E) := { w with queue := queue }
  have hbase : interpret rep base = normalize (interpret rep w) := by
    simp only [interpret, base, TouchScheduleModel.normalize, hqRep]
  have hout : (interpret rep (restore
      (queue, w.frontiers, w.cursor, w.groups, w.events_rev))).output =
      (normalize (interpret rep w)).output := congrArg Work.output hbase
  by_cases hzero : fuel = 0#usize
  · subst fuel
    apply exists_imp_spec
    refine ⟨.done (queue, w.frontiers, w.cursor, w.groups, w.events_rev), ?_, hqFit, ?_⟩
    · rw [body_reassociated call cursorCall replaceCall ops, he]
      simp only [bind_tc_ok, ↓reduceIte] <;> rfl
    · exact hout.trans (resume_zero ctx (interpret rep w)).symm
  · have hpositive : 0 < fuel.val := by scalar_tac
    have hsucc : fuel.val = (fuel.val - 1) + 1 := by omega
    obtain ⟨found, hp, hpRep⟩ := cpoints.peek queue hqFit
    cases found with
    | none =>
        have hempty : (normalize (interpret rep w)).queue.peek = none := by
          rw [← hbase]
          exact hpRep.symm
        apply exists_imp_spec
        refine ⟨.done (queue, w.frontiers, w.cursor, w.groups, w.events_rev), ?_, hqFit, ?_⟩
        · rw [body_reassociated call cursorCall replaceCall ops, he]
          simp only [bind_tc_ok, uncurry_apply_pair, hzero, ↓reduceIte, hp]
        · exact hout.trans (resume_empty ctx fuel.val (interpret rep w) hempty).symm
    | some before =>
        have hpeek : (normalize (interpret rep w)).queue.peek = some (rep.note before) := by
          rw [← hbase]
          exact hpRep.symm
        have href := resume_step ctx (fuel.val - 1) (interpret rep w) (rep.note before) hpeek
        rw [← hsucc, ← hbase] at href
        obtain ⟨p, button, sensor, hprepare, hid, harea, hc, hclicked, hdiff, hj⟩ :=
          prepare_spec call cursorCall replaceCall ops rep ctx cap points groups life base before
        dsimp only [interpret] at href
        rw [← hc] at href
        try dsimp only at href
        rw [← hj] at href
        try dsimp only at href
        rw [← hclicked, ← hdiff] at href
        let next := advance ctx (interpret rep base) (rep.note before) (rep.note p.after)
          (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)
        change resume ctx fuel.val (interpret rep w) =
          (if ended (rep.note p.after) && unused ctx (rep.note before) (rep.cursor p.cursor)
            then resume ctx (fuel.val - 1) next else next.output) at href
        have hnextFit : next.eventsRev.length ≤ cap.events := by
          rw [href] at hefit
          split at hefit
          · exact Nat.le_trans (resume_events_growth ctx (fuel.val - 1) next) hefit
          · exact hefit
        obtain ⟨result, hfinish, hresult, hresultFit⟩ :=
          finish_spec call cursorCall replaceCall ops rep ctx cap points groups m
            cpoints rpoints fuel hpositive base before p hid harea hqFit hnextFit
        apply exists_imp_spec
        refine ⟨result, ?_, ?_⟩
        · rw [body_reassociated call cursorCall replaceCall ops, he]
          simpa only [bind_tc_ok, uncurry_apply_pair, hzero, ↓reduceIte, hp,
            hprepare, base] using hfinish
        · cases result with
          | cont state =>
              rcases state with ⟨nextFuel, nextWork⟩
              dsimp only [interpretFlow] at hresult
              split at hresult
              · rename_i hcontinue
                have heq := ControlFlow.cont.inj hresult
                have hFuel : nextFuel.val = fuel.val - 1 := congrArg Prod.fst heq
                have hWork : interpret rep nextWork = next := congrArg Prod.snd heq
                refine ⟨hresultFit, ?_, ?_⟩
                · rw [hFuel, hWork, href]
                  simp only [hcontinue, ↓reduceIte, next]
                · rw [hFuel]
                  omega
              · contradiction
          | done out =>
              dsimp only [interpretFlow] at hresult
              split at hresult
              · contradiction
              · rename_i hstop
                have heq := ControlFlow.done.inj hresult
                refine ⟨hresultFit, ?_⟩
                rw [heq, href]
                simp [hstop, next]

include points groups life in
theorem process_loop_spec (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hqfit : Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events) :
    touch_schedule.process_loop call cursorCall replaceCall fuel initial ops ⦃ out =>
      Fits cap.queue (rep.queue out.1) ∧
      (interpret rep (restore out)).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  unfold touch_schedule.process_loop
  apply loop.spec_decr_nat (fun state => state.1.val)
    (fun state => Fits cap.queue (rep.queue state.2.queue) ∧
      resume ctx state.1.val (interpret rep state.2) =
        resume ctx fuel.val (interpret rep initial))
  · rintro ⟨remainingFuel, work⟩ ⟨hworkFit, hinv⟩
    obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
      (body_spec call cursorCall replaceCall ops rep ctx cap points groups life
        remainingFuel work hworkFit (by rw [hinv]; exact hefit))
    apply exists_imp_spec
    refine ⟨out, hrun, ?_⟩
    cases out with
    | cont state => exact ⟨⟨hpost.1, hpost.2.1.trans hinv⟩, hpost.2.2⟩
    | done result => exact ⟨hpost.1, hpost.2.trans hinv⟩
  · exact ⟨hqfit, rfl⟩

include points groups life in
/-- Real Generated process; resume is definitionally the original private
Scheduler.processTouchQueueHeadFuel. No atomic/body/pass equivalence premise
is introduced in lifting body_spec through loop and output reconstruction. -/
theorem process_equiv (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hqfit : Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events) :
    touch_schedule.process call cursorCall replaceCall fuel initial ops ⦃ out =>
      Fits cap.queue (rep.queue out.queue) ∧
      (interpret rep out).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (process_loop_spec call cursorCall replaceCall ops rep ctx cap points groups life
      fuel initial hqfit hefit)
  rcases out with ⟨queue, frontiers, cursor, gs, es⟩
  apply exists_imp_spec
  refine ⟨⟨queue, frontiers, cursor, gs, es⟩, ?_, hpost⟩
  simp only [touch_schedule.process, hrun, bind_tc_ok] <;> rfl

include points groups life in
/-- The unchanged runtime head wrapper supplies remaining + 1. The equation
for its Usize argument is explicit: native conversion/addition and allocation
success have not been inferred from this generic kernel theorem. -/
theorem head_process_equiv (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hfuel : fuel.val = remaining (rep.queue initial.queue) + 1)
    (hqfit : Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events) :
    touch_schedule.process call cursorCall replaceCall fuel initial ops ⦃ out =>
      Fits cap.queue (rep.queue out.queue) ∧
      (interpret rep out).output = processTouchQueueHead ctx.area
        (rep.queue initial.queue) (rep.frontiers initial.frontiers) ctx.input ctx.currentTime
        ctx.style (rep.cursor initial.cursor) ctx.touchPanelOffset (rep.groups initial.groups)
        ctx.buttonRingForTouch (rep.events initial.events_rev) ⦄ := by
  obtain ⟨out, hrun, hfit, href⟩ := spec_imp_exists
    (process_equiv call cursorCall replaceCall ops rep ctx cap points groups life
      fuel initial hqfit hefit)
  apply exists_imp_spec
  refine ⟨out, hrun, hfit, ?_⟩
  simpa only [resume, interpret, hfuel, processTouchQueueHead,
    remaining, QueueMaintenanceModel.remaining] using href

/-!
HANDOFF BOUNDARY (unvalidated source, mainline owns the Lean worker):

* Install/import TouchScheduleModel first, then this file as
  Verification.TouchSchedule. Target signatures were read from the SECOND
  extraction's actual touch_schedule declarations, without running extraction.
* PointAgrees still requires exact queue/field/time/duration/area/button/id/grade
  encodings; availability reads and one-counter increments; eligibility and
  frontier predicates/updates; event prepending; ended/resolved interpretation.
  queue_ops must supply CursorAtomicAgrees and ReplaceAtomicAgrees for ONE M.
  Those are slot reads/writes, peek, id equality, resolved and cursor advance.
  The normalize and replacement LOOPS are composed from QueueMaintenanceProof,
  including all matching ids and arbitrary out-of-range reference cursors.
* GroupAgrees.shared_result/register_group are explicitly retained COMPOSITE
  subcall obligations, to connect to separately proved group traversals and
  first-match/update/clone encodings. They are not described as primitive laws.
* LifecycleAgrees.step is ONE note's actual touchStep, including shared/timeout
  judgment with no click. Connect LifecycleJudgments and output fields here.
  No assumption identifies an entire queue iteration, head pass or frame.
* BigInt/BigUint fields, exact differences, counters and ids remain facade
  obligations. Usize subtraction is proved for positive fuel; queue len and
  remaining conversions, wrapper +1 representability, Vec clone/write/prepend
  and group allocation success are not discharged. Fits only bounds length.
* Event equality is in reverse-accumulator order, including its initial suffix.
  The final facade reverse and head/automatic/normalize composition are outside
  this file. No automatic, TouchHold, outer sensor pass, or frame theorem is
  claimed here. No uniqueness or reachable-note-state assumption is added.
-/

end Verification.TouchScheduleProof
