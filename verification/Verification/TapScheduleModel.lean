import Verification.SchedulerClicks
import Verification.LifecycleJudgments
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.TapScheduleModel
open LnmaiCore InputModel Lifecycle
open private processTapQueueFuel processTapQueue tapEligibleForClick
  buttonQueueIndexUnlocked advanceSharedButtonQueue consumeButtonThenFallbackSensor
  fallbackSensorAreaForButtonNote tapFamilyQueueHasUnusedClick listSetAt
  from LnmaiCore.Scheduler

/-!
One-step equations for the original scheduler, with no replacement recursive
model. The extracted traversal proof uses these equations as its invariant.
Queue indices and click/frontier counters remain arbitrary natural numbers.
-/

structure Context where
  zone : ButtonZone
  input : FrameInput
  currentTime : TimePoint
  touchPanelOffset : Duration
  style : JudgeStyle

structure Work where
  queue : ZoneQueue TapFamilyNote
  frontiers : ButtonVec Nat
  cursor : Scheduler.ClickCursor
  eventsRev : List JudgeEvent

abbrev Outcome :=
  ZoneQueue TapFamilyNote × ButtonVec Nat × Scheduler.ClickCursor × List JudgeEvent

def Work.output (w : Work) : Outcome :=
  (w.queue, w.frontiers, w.cursor, w.eventsRev)

def resume (ctx : Context) (fuel : Nat) (w : Work) : Outcome :=
  processTapQueueFuel fuel ctx.zone w.queue w.frontiers ctx.input ctx.currentTime
    ctx.touchPanelOffset ctx.style w.cursor w.eventsRev

def remaining (queue : ZoneQueue TapFamilyNote) : Nat :=
  queue.notes.length - queue.currentIndex

def fallback (note : TapFamilyNote) : SensorArea :=
  fallbackSensorAreaForButtonNote note.lane.toButtonZone

def eligible (ctx : Context) (frontiers : ButtonVec Nat) (note : TapFamilyNote) : Bool :=
  tapEligibleForClick note ctx.currentTime &&
    buttonQueueIndexUnlocked frontiers ctx.zone note.buttonQueueIndex

def consume (ctx : Context) (note : TapFamilyNote) (allowInput : Bool)
    (cursor : Scheduler.ClickCursor) : Bool × Bool × Scheduler.ClickCursor :=
  consumeButtonThenFallbackSensor ctx.input cursor allowInput ctx.zone (fallback note)

def difference (ctx : Context) (note : TapFamilyNote) (usedSensor : Bool) : Duration :=
  if usedSensor then
    (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming
  else ctx.currentTime - note.params.effectiveTiming

/-- A button always selects the unshifted time, even for an arbitrary pair of
flags. The real consume kernel additionally excludes spending both clicks. -/
def judgment (ctx : Context) (note : TapFamilyNote) (usedButton usedSensor : Bool) :
    TapFamilyNote × Option JudgeEvent :=
  tapFamilyStep note ctx.currentTime (difference ctx note (!usedButton && usedSensor))
    (usedButton || usedSensor) ctx.style

def ended (note : TapFamilyNote) : Bool :=
  match note.state with | .Ended => true | _ => false

def unused (ctx : Context) (note : TapFamilyNote) (cursor : Scheduler.ClickCursor) : Bool :=
  tapFamilyQueueHasUnusedClick ctx.input cursor ctx.zone (fallback note)

/-- Only an ended note advances the queue and shared frontier. Events are
prepended even when the note remains at the same queue position. -/
def advance (ctx : Context) (w : Work) (after : TapFamilyNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) : Work :=
  { queue := if ended after then w.queue.advance else
      { w.queue with notes := listSetAt w.queue.notes w.queue.currentIndex after }
    frontiers := if ended after then advanceSharedButtonQueue w.frontiers ctx.zone
      else w.frontiers
    cursor := cursor
    eventsRev := match event with | some event => event :: w.eventsRev | none => w.eventsRev }

theorem resume_zero (ctx : Context) (w : Work) :
    resume ctx 0 w = w.output := by
  unfold resume processTapQueueFuel
  cases w.queue.peek <;> rfl

theorem resume_empty (ctx : Context) (fuel : Nat) (w : Work)
    (hempty : w.queue.peek = none) :
    resume ctx fuel w = w.output := by
  simp only [resume, processTapQueueFuel, hempty, Work.output]

/-- Exactly one unfolding of the original fuel recursion; this includes a
zero-event Ended transition and the stop when no unused click remains. -/
theorem resume_step (ctx : Context) (fuel : Nat) (w : Work)
    (note : TapFamilyNote) (hpeek : w.queue.peek = some note) :
    resume ctx (fuel + 1) w =
      let (button, sensor, cursor) := consume ctx note (eligible ctx w.frontiers note) w.cursor
      let (after, event) := judgment ctx note button sensor
      let next := advance ctx w after event cursor
      if ended after && unused ctx note cursor then resume ctx fuel next else next.output := by
  unfold resume
  conv => lhs; unfold processTapQueueFuel
  simp only [hpeek]
  unfold consume eligible judgment difference fallback unused
  rcases hc : consumeButtonThenFallbackSensor ctx.input w.cursor
      (tapEligibleForClick note ctx.currentTime &&
        buttonQueueIndexUnlocked w.frontiers ctx.zone note.buttonQueueIndex)
      ctx.zone (fallbackSensorAreaForButtonNote note.lane.toButtonZone) with
    ⟨button, sensor, cursor⟩
  cases button <;> cases sensor <;> simp only [Bool.not_false, Bool.not_true,
    Bool.false_and, Bool.true_and, Bool.false_or, Bool.true_or, Bool.false_eq_true,
    Bool.true_eq, if_true, if_false]
  all_goals
    rcases hj : tapFamilyStep note ctx.currentTime _ _ ctx.style with ⟨after, event⟩
    cases event <;> cases hs : after.state <;>
      simp [advance, ended, hs, Work.output, fallback]

theorem remaining_zero_peek (queue : ZoneQueue TapFamilyNote)
    (h : remaining queue = 0) : queue.peek = none := by
  apply List.getElem?_eq_none
  dsimp only [remaining] at h
  omega

theorem remaining_advance (queue : ZoneQueue TapFamilyNote) :
    remaining queue.advance = remaining queue - 1 := by
  simp only [remaining, ZoneQueue.advance]
  omega

theorem peek_remaining_positive (queue : ZoneQueue TapFamilyNote)
    (note : TapFamilyNote) (h : queue.peek = some note) : 0 < remaining queue := by
  by_contra hn
  have hz : remaining queue = 0 := by omega
  rw [remaining_zero_peek queue hz] at h
  contradiction

theorem advance_ended_queue (ctx : Context) (w : Work) (after : TapFamilyNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor)
    (h : ended after = true) :
    (advance ctx w after event cursor).queue = w.queue.advance := by
  simp [advance, h]

theorem advance_events_growth (ctx : Context) (w : Work) (after : TapFamilyNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) :
    w.eventsRev.length ≤ (advance ctx w after event cursor).eventsRev.length := by
  cases event <;> simp [advance]

/-- A capacity bound on the final event buffer also bounds every intermediate
buffer. No universal allocation-success assumption is needed for prepending. -/
theorem resume_events_growth (ctx : Context) (fuel : Nat) (w : Work) :
    w.eventsRev.length ≤ (resume ctx fuel w).2.2.2.length := by
  induction fuel generalizing w with
  | zero => simp only [resume_zero, Work.output, Nat.le_refl]
  | succ fuel ih =>
      cases hpeek : w.queue.peek with
      | none => simp only [resume_empty ctx _ w hpeek, Work.output, Nat.le_refl]
      | some note =>
          rw [resume_step ctx fuel w note hpeek]
          rcases hc : consume ctx note (eligible ctx w.frontiers note) w.cursor with
            ⟨button, sensor, cursor⟩
          dsimp only
          rcases hj : judgment ctx note button sensor with ⟨after, event⟩
          dsimp only
          split
          · exact Nat.le_trans (advance_events_growth ctx w after event cursor) (ih _)
          · exact advance_events_growth ctx w after event cursor

/-- Extra fuel cannot change the result after enough queue positions have
been budgeted. No reachability assumption is made about stored note states. -/
theorem resume_fuel_irrelevant (ctx : Context) (fuel other : Nat) (w : Work)
    (hfuel : remaining w.queue ≤ fuel) (hother : remaining w.queue ≤ other) :
    resume ctx fuel w = resume ctx other w := by
  induction fuel generalizing other w with
  | zero =>
      have hempty := remaining_zero_peek w.queue (by omega)
      rw [resume_empty ctx 0 w hempty, resume_empty ctx other w hempty]
  | succ fuel ih =>
      cases hpeek : w.queue.peek with
      | none => rw [resume_empty ctx _ w hpeek, resume_empty ctx _ w hpeek]
      | some note =>
          have hpositive := peek_remaining_positive w.queue note hpeek
          cases other with
          | zero => omega
          | succ other =>
              rw [resume_step ctx fuel w note hpeek, resume_step ctx other w note hpeek]
              rcases hc : consume ctx note (eligible ctx w.frontiers note) w.cursor with
                ⟨button, sensor, cursor⟩
              dsimp only
              rcases hj : judgment ctx note button sensor with ⟨after, event⟩
              dsimp only
              split
              · rename_i hcontinue
                have hend : ended after = true := (Bool.and_eq_true_iff.mp hcontinue).1
                have hremaining :
                    remaining (advance ctx w after event cursor).queue = remaining w.queue - 1 := by
                  rw [advance_ended_queue ctx w after event cursor hend, remaining_advance]
                apply ih
                · rw [hremaining]; omega
                · rw [hremaining]; omega
              · rfl

/-- The runtime can use pending length directly, avoiding `pending + 1`
overflow; the original reference's extra fuel only observes an empty queue. -/
theorem pending_fuel_equiv (ctx : Context) (w : Work) :
    resume ctx (remaining w.queue) w =
      processTapQueue ctx.zone w.queue w.frontiers ctx.input ctx.currentTime
        ctx.touchPanelOffset ctx.style w.cursor w.eventsRev := by
  exact resume_fuel_irrelevant ctx (remaining w.queue) (remaining w.queue + 1) w
    (Nat.le_refl _) (by omega)

end Verification.TapScheduleModel
