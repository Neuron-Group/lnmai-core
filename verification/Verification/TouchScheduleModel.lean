import Verification.QueueMaintenanceModel
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

namespace Verification.TouchScheduleModel
open LnmaiCore InputModel Lifecycle
open private processTouchQueueHeadFuel processTouchQueueHead
  normalizeTouchQueueCursorFuel normalizeTouchQueueCursor replaceTouchQueueNote
  touchQueueResolved touchQueueIndexUnlocked advanceSharedTouchQueue
  touchClickCanEngageJudge tryUseButtonClickAt tryUseSensorClickAt
  hasUnusedTouchInputAt groupShareResult registerTouchGroupResult
  from LnmaiCore.Scheduler
open private judgeTouchNow? from LnmaiCore.Lifecycle

/-!
UNVALIDATED SOURCE DRAFT: no local Lean worker was started for this file.
Revised after mainline's first batch log: preserve tuple equations, tolerate
already-reduced projections, and expose the common remaining-count notation
to arithmetic. This revision still requires mainline elaboration.

The continuation is the ORIGINAL private processTouchQueueHeadFuel, not a new
recursive specification. This file gives its zero/empty/step equations and a
fuel bound. It neither asserts a theorem about a nonexistent Generated Touch
kernel nor postulates equivalence of an entire scheduler iteration.

The proposed Rust kernel composes existing queue_maintenance loops. The
QueueMaintenanceModel.touch_pending_equiv theorem accounts for their remaining
fuel versus the reference normalizer's remaining + 1. The head wrapper itself
is deliberately unchanged in the runtime patch.

No uniqueness, valid-state, or in-bounds cursor premise is imposed. In
particular, replaceTouchQueueNote maps ALL matching identifiers.
-/

structure Context where
  area : SensorArea
  input : FrameInput
  currentTime : TimePoint
  style : JudgeStyle
  touchPanelOffset : Duration
  buttonRingForTouch : Bool

structure Work where
  queue : ZoneQueue TouchNote
  frontiers : SensorVec Nat
  cursor : Scheduler.ClickCursor
  groups : List GroupState
  eventsRev : List JudgeEvent

abbrev Outcome := ZoneQueue TouchNote × SensorVec Nat × Scheduler.ClickCursor ×
  List GroupState × List JudgeEvent

def Work.output (w : Work) : Outcome :=
  (w.queue, w.frontiers, w.cursor, w.groups, w.eventsRev)

def normalize (w : Work) : Work :=
  { w with queue := normalizeTouchQueueCursor w.queue }

def resume (ctx : Context) (fuel : Nat) (w : Work) : Outcome :=
  processTouchQueueHeadFuel fuel ctx.area w.queue w.frontiers ctx.input
    ctx.currentTime ctx.style w.cursor ctx.touchPanelOffset w.groups
    ctx.buttonRingForTouch w.eventsRev

abbrev remaining (queue : ZoneQueue TouchNote) : Nat :=
  QueueMaintenanceModel.remaining queue

def sharedResult (groups : List GroupState) (note : TouchNote) :
    Option (JudgeGrade × Duration) :=
  match note.touchGroupId with
  | some id => groupShareResult groups id
  | none => none

def eligible (ctx : Context) (frontiers : SensorVec Nat)
    (groups : List GroupState) (note : TouchNote) : Bool :=
  (sharedResult groups note).isNone &&
    touchClickCanEngageJudge note ctx.currentTime ctx.touchPanelOffset &&
    touchQueueIndexUnlocked frontiers ctx.area note.touchQueueIndex

def consume (ctx : Context) (note : TouchNote) (allowInput : Bool)
    (cursor : Scheduler.ClickCursor) : Bool × Bool × Scheduler.ClickCursor :=
  let (button, cursorButton) :=
    if allowInput && ctx.buttonRingForTouch then
      match note.sensorPos.toOuterButtonZone? with
      | some zone => tryUseButtonClickAt ctx.input cursor zone
      | none => (false, cursor)
    else (false, cursor)
  let (sensor, cursor2) :=
    if allowInput && !button then
      tryUseSensorClickAt ctx.input cursorButton note.sensorPos
    else (false, cursorButton)
  (button, sensor, cursor2)

/-- Sensor offset applies even when neither input was consumed. -/
def difference (ctx : Context) (note : TouchNote) (usedButton : Bool) : Duration :=
  if usedButton then ctx.currentTime - note.params.effectiveTiming
  else (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming

/-- Always call the lifecycle: shared and timeout judgments need no click. -/
def judgment (ctx : Context) (groups : List GroupState) (note : TouchNote)
    (button sensor : Bool) : TouchNote × Option JudgeEvent :=
  touchStep note ctx.currentTime (difference ctx note button) (button || sensor)
    (sharedResult groups note) ctx.style

def ended (note : TouchNote) : Bool :=
  match note.state with | .Ended => true | _ => false

def unused (ctx : Context) (before : TouchNote) (cursor : Scheduler.ClickCursor) : Bool :=
  hasUnusedTouchInputAt ctx.input cursor before.sensorPos ctx.buttonRingForTouch

/-- Register only a local successful judgment, using BEFORE metadata and the
selected input diff. Shared, timeout, no-event, Miss and TooFast do not vote. -/
def updateGroups (groups : List GroupState) (before : TouchNote)
    (event : Option JudgeEvent) (clicked : Bool) (diff : Duration) : List GroupState :=
  match event with
  | none => groups
  | some event =>
      if !clicked || event.grade.isMissOrTooFast then groups
      else
        match before.touchGroupId with
        | some id => registerTouchGroupResult groups id before.touchGroupSize event.grade diff
        | none => groups

/-- Newly resolved advances the shared frontier; Ended separately controls
continuation. Queue replacement and normalization happen even without events. -/
def advance (ctx : Context) (w : Work) (before after : TouchNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor)
    (clicked : Bool) (diff : Duration) : Work :=
  { queue := normalizeTouchQueueCursor (replaceTouchQueueNote w.queue after)
    frontiers := if !touchQueueResolved before && touchQueueResolved after then
      advanceSharedTouchQueue w.frontiers ctx.area else w.frontiers
    cursor := cursor
    groups := updateGroups w.groups before event clicked diff
    eventsRev := match event with | some event => event :: w.eventsRev | none => w.eventsRev }

/-- One positive-fuel head after entry normalization. No fuel is hidden here.
The bool means continue with one less fuel, not merely resolved. -/
def step (ctx : Context) (w : Work) (note : TouchNote) : Bool × Work :=
  let (button, sensor, cursor) :=
    consume ctx note (eligible ctx w.frontiers w.groups note) w.cursor
  let (after, event) := judgment ctx w.groups note button sensor
  let next := advance ctx w note after event cursor (button || sensor)
    (difference ctx note button)
  (ended after && unused ctx note cursor, next)

theorem resume_zero (ctx : Context) (w : Work) :
    resume ctx 0 w = (normalize w).output := by
  rfl

theorem resume_empty (ctx : Context) (fuel : Nat) (w : Work)
    (hempty : (normalize w).queue.peek = none) :
    resume ctx fuel w = (normalize w).output := by
  change (normalizeTouchQueueCursor w.queue).peek = none at hempty
  cases fuel <;> simp only [resume, processTouchQueueHeadFuel, hempty, normalize, Work.output]

/-- One unfolding of the original recursion, including the event/no-event
branches and the extra normalization performed on a continuing zero-fuel call. -/
theorem resume_step (ctx : Context) (fuel : Nat) (w : Work)
    (note : TouchNote) (hpeek : (normalize w).queue.peek = some note) :
    resume ctx (fuel + 1) w =
      let base := normalize w
      let (button, sensor, cursor) :=
        consume ctx note (eligible ctx base.frontiers base.groups note) base.cursor
      let (after, event) := judgment ctx base.groups note button sensor
      let next := advance ctx base note after event cursor (button || sensor)
        (difference ctx note button)
      if ended after && unused ctx note cursor then resume ctx fuel next else next.output := by
  change (normalizeTouchQueueCursor w.queue).peek = some note at hpeek
  unfold resume
  conv => lhs; unfold processTouchQueueHeadFuel
  simp only [hpeek]
  unfold consume eligible judgment difference sharedResult
  dsimp only [normalize]
  cases hg : note.touchGroupId <;> cases hz : note.sensorPos.toOuterButtonZone?
  all_goals
    simp only [hg, hz]
    rcases hb : (if
        ((match note.touchGroupId with
          | some id => groupShareResult w.groups id
          | none => none).isNone &&
          touchClickCanEngageJudge note ctx.currentTime ctx.touchPanelOffset &&
          touchQueueIndexUnlocked w.frontiers ctx.area note.touchQueueIndex) &&
            ctx.buttonRingForTouch then
        match note.sensorPos.toOuterButtonZone? with
        | some zone => tryUseButtonClickAt ctx.input w.cursor zone
        | none => (false, w.cursor)
      else (false, w.cursor)) with ⟨button, cursorButton⟩
    simp only [hg, hz] at hb
    simp only [hb]
    rcases hs : (if
        ((match note.touchGroupId with
          | some id => groupShareResult w.groups id
          | none => none).isNone &&
          touchClickCanEngageJudge note ctx.currentTime ctx.touchPanelOffset &&
          touchQueueIndexUnlocked w.frontiers ctx.area note.touchQueueIndex) && !button then
        tryUseSensorClickAt ctx.input cursorButton note.sensorPos
      else (false, cursorButton)) with ⟨sensor, cursor⟩
    simp only [hg] at hs
    simp only [hs]
    rcases hj : touchStep note ctx.currentTime
        (if button then ctx.currentTime - note.params.effectiveTiming
         else (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming)
        (button || sensor)
        (match note.touchGroupId with
          | some id => groupShareResult w.groups id
          | none => none) ctx.style with ⟨after, event⟩
    simp only [hg] at hj
    simp only [hj]
    cases button <;> cases sensor <;> cases event <;> cases ha : after.state <;>
      simp [advance, updateGroups, ended, unused, ha, hg, hz, Work.output, normalize]

theorem resume_step_pair (ctx : Context) (fuel : Nat) (w : Work)
    (note : TouchNote) (hpeek : (normalize w).queue.peek = some note) :
    resume ctx (fuel + 1) w =
      let next := step ctx (normalize w) note
      if next.1 then resume ctx fuel next.2 else next.2.output := by
  rw [resume_step ctx fuel w note hpeek]
  unfold step
  rcases hc : consume ctx note
      (eligible ctx (normalize w).frontiers (normalize w).groups note)
      (normalize w).cursor with ⟨button, sensor, cursor⟩
  rcases hj : judgment ctx (normalize w).groups note button sensor with ⟨after, event⟩
  simp only [hc, hj]

/-- Keep the recursive-entry normalization explicit even for one unit of fuel. -/
theorem resume_one (ctx : Context) (w : Work)
    (note : TouchNote) (hpeek : (normalize w).queue.peek = some note) :
    resume ctx 1 w =
      let next := step ctx (normalize w) note
      if next.1 then (normalize next.2).output else next.2.output := by
  rw [resume_step_pair ctx 0 w note hpeek]
  simp only [resume_zero]

/-- Metadata preservation is proved from the actual lifecycle, not assumed as
a property of an arbitrary step callback. It is needed for the fuel decrease. -/
theorem touch_step_params (note : TouchNote) (now : TimePoint) (diff : Duration)
    (clicked : Bool) (shared : Option (JudgeGrade × Duration)) (style : JudgeStyle) :
    (touchStep note now diff clicked shared style).1.params = note.params := by
  rcases note with ⟨params, state, sensor, queueIndex, groupId, groupSize⟩
  cases state <;> cases clicked <;> cases shared <;>
    cases hraw : Judge.judgeTouch diff params.isEX <;>
    simp [touchStep, judgeTouchNow?, hraw] <;> split_ifs <;> simp_all

theorem judgment_note_index (ctx : Context) (groups : List GroupState)
    (note : TouchNote) (button sensor : Bool) :
    (judgment ctx groups note button sensor).1.params.noteIndex = note.params.noteIndex := by
  exact congrArg (fun params : CommonNoteParams => params.noteIndex)
    (touch_step_params note ctx.currentTime (difference ctx note button)
      (button || sensor) (sharedResult groups note) ctx.style)

theorem normalize_fuel_index_mono (fuel : Nat) (queue : ZoneQueue TouchNote) :
    queue.currentIndex ≤
      (QueueMaintenanceModel.normalizeFuel touchQueueResolved fuel queue).currentIndex := by
  induction fuel generalizing queue with
  | zero => simp only [QueueMaintenanceModel.normalize_zero, Nat.le_refl]
  | succ fuel ih =>
      cases hp : queue.peek with
      | none =>
          simp only [QueueMaintenanceModel.normalize_empty touchQueueResolved _ queue hp,
            Nat.le_refl]
      | some note =>
          conv => rhs; unfold QueueMaintenanceModel.normalizeFuel
          simp only [hp]
          split
          · exact Nat.le_trans (by simp [ZoneQueue.advance]) (ih queue.advance)
          · exact Nat.le_refl _

theorem normalize_fuel_remaining_le (fuel : Nat) (queue : ZoneQueue TouchNote) :
    remaining (QueueMaintenanceModel.normalizeFuel touchQueueResolved fuel queue) ≤
      remaining queue := by
  have hn := QueueMaintenanceModel.normalize_notes touchQueueResolved fuel queue
  have hi := normalize_fuel_index_mono fuel queue
  unfold remaining QueueMaintenanceModel.remaining
  rw [hn]
  omega

theorem normalize_remaining_le (queue : ZoneQueue TouchNote) :
    remaining (normalizeTouchQueueCursor queue) ≤ remaining queue := by
  rw [← QueueMaintenanceModel.touch_pending_equiv]
  exact normalize_fuel_remaining_le _ queue

theorem normalize_remaining_lt (queue : ZoneQueue TouchNote) (note : TouchNote)
    (hpeek : queue.peek = some note) (hresolved : touchQueueResolved note = true) :
    remaining (normalizeTouchQueueCursor queue) < remaining queue := by
  have hpositive : 0 < remaining queue :=
    QueueMaintenanceModel.peek_remaining_positive queue note hpeek
  have heq : normalizeTouchQueueCursor queue =
      QueueMaintenanceModel.normalizeFuel touchQueueResolved (remaining queue) queue.advance := by
    change normalizeTouchQueueCursorFuel (remaining queue + 1) queue = _
    rw [← QueueMaintenanceModel.touch_fuel_eq]
    conv => lhs; unfold QueueMaintenanceModel.normalizeFuel
    simp only [hpeek, hresolved, Bool.true_eq, if_true]
  rw [heq]
  have hle := normalize_fuel_remaining_le (remaining queue) queue.advance
  have hadv : remaining queue.advance = remaining queue - 1 :=
    QueueMaintenanceModel.remaining_advance queue
  omega

private theorem map_lookup (f : α → β) (xs : List α) (index : Nat) :
    (xs.map f)[index]? = (xs[index]?).map f := by
  induction xs generalizing index with
  | nil => simp
  | cons head tail ih =>
      cases index with
      | zero => rfl
      | succ index => simpa using ih index

/-- Duplicate identifiers elsewhere do not affect this head-slot fact. -/
theorem replace_peek (queue : ZoneQueue TouchNote) (before after : TouchNote)
    (hpeek : queue.peek = some before)
    (hid : after.params.noteIndex = before.params.noteIndex) :
    (replaceTouchQueueNote queue after).peek = some after := by
  change queue.notes[queue.currentIndex]? = some before at hpeek
  change (queue.notes.map (fun existing =>
    if existing.params.noteIndex == after.params.noteIndex then after else existing))[queue.currentIndex]? = some after
  rw [map_lookup, hpeek]
  simp [hid]

theorem remaining_replace (queue : ZoneQueue TouchNote) (after : TouchNote) :
    remaining (replaceTouchQueueNote queue after) = remaining queue := by
  simp [remaining, QueueMaintenanceModel.remaining, replaceTouchQueueNote]

theorem advance_remaining_lt (ctx : Context) (w : Work) (before after : TouchNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) (clicked : Bool) (diff : Duration)
    (hpeek : w.queue.peek = some before)
    (hid : after.params.noteIndex = before.params.noteIndex) (hend : ended after = true) :
    remaining (advance ctx w before after event cursor clicked diff).queue < remaining w.queue := by
  have hr : touchQueueResolved after = true := by
    cases hs : after.state <;> simp_all [ended, touchQueueResolved]
  have hp := replace_peek w.queue before after hpeek hid
  have hlt := normalize_remaining_lt (replaceTouchQueueNote w.queue after) after hp hr
  simpa only [advance, remaining_replace] using hlt

theorem normalize_empty_of_remaining_zero (w : Work) (hzero : remaining w.queue = 0) :
    (normalize w).queue.peek = none := by
  have hempty := QueueMaintenanceModel.remaining_zero_peek w.queue hzero
  change (normalizeTouchQueueCursor w.queue).peek = none
  rw [← QueueMaintenanceModel.touch_pending_equiv]
  change (QueueMaintenanceModel.normalizeFuel touchQueueResolved (remaining w.queue) w.queue).peek = none
  rw [hzero, QueueMaintenanceModel.normalize_zero]
  exact hempty

/-- At most one pending position is needed per continuing head call. Both
normalizations may skip further resolved slots. The result holds for arbitrary
restored note states, duplicate identifiers, and out-of-range Nat cursors. -/
theorem resume_fuel_irrelevant (ctx : Context) (fuel other : Nat) (w : Work)
    (hfuel : remaining w.queue ≤ fuel) (hother : remaining w.queue ≤ other) :
    resume ctx fuel w = resume ctx other w := by
  induction fuel generalizing other w with
  | zero =>
      have hempty := normalize_empty_of_remaining_zero w (by omega)
      rw [resume_empty ctx 0 w hempty, resume_empty ctx other w hempty]
  | succ fuel ih =>
      cases hpeek : (normalize w).queue.peek with
      | none => rw [resume_empty ctx _ w hpeek, resume_empty ctx _ w hpeek]
      | some note =>
          have hpositive : 0 < remaining (normalize w).queue :=
            QueueMaintenanceModel.peek_remaining_positive (normalize w).queue note hpeek
          have hnorm : remaining (normalize w).queue ≤ remaining w.queue :=
            normalize_remaining_le w.queue
          cases other with
          | zero => omega
          | succ other =>
              rw [resume_step ctx fuel w note hpeek, resume_step ctx other w note hpeek]
              rcases hc : consume ctx note
                  (eligible ctx (normalize w).frontiers (normalize w).groups note)
                  (normalize w).cursor with ⟨button, sensor, cursor⟩
              simp only [hc]
              rcases hj : judgment ctx (normalize w).groups note button sensor with ⟨after, event⟩
              simp only [hj]
              split
              · rename_i hcontinue
                have hend : ended after = true := (Bool.and_eq_true_iff.mp hcontinue).1
                have hid : after.params.noteIndex = note.params.noteIndex := by
                  have h := judgment_note_index ctx (normalize w).groups note button sensor
                  simpa only [hj] using h
                have hdecrease := advance_remaining_lt ctx (normalize w) note after event cursor
                  (button || sensor) (difference ctx note button) hpeek hid hend
                apply ih <;> omega
              · rfl

/-- Mathematical permission to use pending length for HEAD fuel. The supplied
runtime patch does not take this further refactor; its wrapper keeps +1. -/
theorem pending_fuel_equiv (ctx : Context) (w : Work) :
    resume ctx (remaining w.queue) w =
      processTouchQueueHead ctx.area w.queue w.frontiers ctx.input ctx.currentTime
        ctx.style w.cursor ctx.touchPanelOffset w.groups ctx.buttonRingForTouch w.eventsRev := by
  exact resume_fuel_irrelevant ctx (remaining w.queue) (remaining w.queue + 1) w
    (Nat.le_refl _) (by omega)

/-- Events are prepended, never reversed here. An old reverse accumulator is
preserved as a suffix, including no-event Ended transitions. -/
theorem advance_events (ctx : Context) (w : Work) (before after : TouchNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) (clicked : Bool) (diff : Duration) :
    (advance ctx w before after event cursor clicked diff).eventsRev = event.toList ++ w.eventsRev := by
  cases event <;> rfl

theorem advance_events_growth (ctx : Context) (w : Work) (before after : TouchNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) (clicked : Bool) (diff : Duration) :
    w.eventsRev.length ≤ (advance ctx w before after event cursor clicked diff).eventsRev.length := by
  cases event <;> simp [advance]

theorem resume_events_growth (ctx : Context) (fuel : Nat) (w : Work) :
    w.eventsRev.length ≤ (resume ctx fuel w).2.2.2.2.length := by
  induction fuel generalizing w with
  | zero => simp only [resume_zero, normalize, Work.output, Nat.le_refl]
  | succ fuel ih =>
      cases hpeek : (normalize w).queue.peek with
      | none => simp only [resume_empty ctx _ w hpeek, normalize, Work.output, Nat.le_refl]
      | some note =>
          rw [resume_step ctx fuel w note hpeek]
          rcases hc : consume ctx note
              (eligible ctx (normalize w).frontiers (normalize w).groups note)
              (normalize w).cursor with ⟨button, sensor, cursor⟩
          simp only [hc]
          rcases hj : judgment ctx (normalize w).groups note button sensor with ⟨after, event⟩
          simp only [hj]
          have hg := advance_events_growth ctx (normalize w) note after event cursor
            (button || sensor) (difference ctx note button)
          split
          · exact Nat.le_trans hg (ih _)
          · exact hg

end Verification.TouchScheduleModel
