import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

namespace Verification.HoldScheduleModel
open LnmaiCore InputModel Lifecycle Constants
open private processHoldNotes holdHeadAwaitsResolution buttonQueueIndexUnlocked
  holdHeadEligibleForClick fallbackSensorAreaForButtonNote
  consumeButtonThenFallbackSensor fallbackSensorHeldForButtonNote
  fallbackPrevSensorHeldForButtonNote holdBodyPressedFromButtonOrSensor
  enteredHeadJudged advanceSharedButtonQueue updateButtonHoldQueue keepHoldActive
  from LnmaiCore.Scheduler

/-!
Ordinary-hold traversal model. The continuation calls the ORIGINAL private
Scheduler.processHoldNotes; it is not a replacement recursive specification.
The pending list is immutable. Queue synchronization changes only Work.queues.
Active notes and events are forward buffers, including arbitrary old prefixes.
This file has no dependency on generated Rust declarations.
-/

structure Context where
  input : FrameInput
  currentTime : TimePoint
  delta : Duration
  style : JudgeStyle
  touchPanelOffset : Duration
  prevSensor : SensorVec Bool

/-- Field order follows the fixed Rust HoldWork interface. -/
structure Work where
  frontiers : ButtonVec Nat
  queues : ButtonQueueVec HoldNote
  cursor : Scheduler.ClickCursor
  active : List (ButtonZone × HoldNote)
  events : List JudgeEvent

abbrev Outcome := ButtonVec Nat × ButtonQueueVec HoldNote ×
  List (ButtonZone × HoldNote) × List JudgeEvent × Scheduler.ClickCursor

def Work.output (w : Work) : Outcome :=
  (w.frontiers, w.queues, w.active, w.events, w.cursor)

/-- Public name for the original recursion, without changing its fuel or order. -/
def reference (ctx : Context) (pending : List (ButtonZone × HoldNote))
    (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) : Outcome :=
  processHoldNotes frontiers queues pending ctx.input ctx.currentTime ctx.delta
    ctx.style ctx.touchPanelOffset ctx.prevSensor cursor

def resume (ctx : Context) (pending : List (ButtonZone × HoldNote)) (w : Work) : Work :=
  let out := reference ctx pending w.frontiers w.queues w.cursor
  { frontiers := out.1
    queues := out.2.1
    cursor := out.2.2.2.2
    active := w.active ++ out.2.2.1
    events := w.events ++ out.2.2.2.1 }

def withPrefix (active : List (ButtonZone × HoldNote)) (events : List JudgeEvent)
    (w : Work) : Work :=
  { w with active := active ++ w.active, events := events ++ w.events }

def eligible (ctx : Context) (frontiers : ButtonVec Nat) (zone : ButtonZone)
    (note : HoldNote) : Bool :=
  holdHeadAwaitsResolution note.state &&
    buttonQueueIndexUnlocked frontiers zone note.buttonQueueIndex &&
    holdHeadEligibleForClick note ctx.currentTime

def consume (ctx : Context) (zone : ButtonZone) (allowInput : Bool)
    (cursor : Scheduler.ClickCursor) : Bool × Bool × Scheduler.ClickCursor :=
  consumeButtonThenFallbackSensor ctx.input cursor allowInput zone
    (fallbackSensorAreaForButtonNote zone)

/-- No button means sensor diff, including the case where neither click is used. -/
def difference (ctx : Context) (note : HoldNote) (usedButton : Bool) : Duration :=
  if usedButton then ctx.currentTime - note.params.effectiveTiming
  else (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming

def judgment (ctx : Context) (zone : ButtonZone) (note : HoldNote)
    (usedButton usedSensor : Bool) : HoldNote × Option JudgeEvent :=
  holdStep note ctx.currentTime (difference ctx note usedButton)
    HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC (usedButton || usedSensor)
    (holdBodyPressedFromButtonOrSensor (ctx.input.getButtonHeld zone)
      (fallbackSensorHeldForButtonNote ctx.input zone))
    (ctx.input.getButtonHeld zone)
    (fallbackPrevSensorHeldForButtonNote ctx.prevSensor zone)
    ctx.touchPanelOffset none ctx.delta ctx.style

def retained (zone : ButtonZone) (note : HoldNote) : List (ButtonZone × HoldNote) :=
  if keepHoldActive note then [(zone, note)] else []

/-- The final state controls the frontier and active output independently of
whether an event exists. HeadJudged Miss is included; direct Body/Ended is not. -/
def advance (w : Work) (zone : ButtonZone) (before after : HoldNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) : Work :=
  { frontiers := if enteredHeadJudged before.state after.state then
      advanceSharedButtonQueue w.frontiers zone else w.frontiers
    queues := updateButtonHoldQueue w.queues zone after
    cursor := cursor
    active := w.active ++ retained zone after
    events := w.events ++ event.toList }

def step (ctx : Context) (w : Work) (zone : ButtonZone) (note : HoldNote) : Work :=
  let (button, sensor, cursor) := consume ctx zone (eligible ctx w.frontiers zone note) w.cursor
  let (after, event) := judgment ctx zone note button sensor
  advance w zone note after event cursor

/-- A finite fold used to describe prefixes of the immutable source list.
The theorem below relates this fold to the original recursion. -/
def traverse (ctx : Context) (original : List (ButtonZone × HoldNote)) (w : Work) : Work :=
  original.foldl (fun state item => step ctx state item.1 item.2) w

theorem resume_nil (ctx : Context) (w : Work) : resume ctx [] w = w := by
  cases w
  simp [resume, reference, processHoldNotes]

theorem resume_original (ctx : Context) (pending : List (ButtonZone × HoldNote))
    (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) :
    (resume ctx pending ⟨frontiers, queues, cursor, [], []⟩).output =
      reference ctx pending frontiers queues cursor := by
  rfl

/-- Unfold exactly one constructor of the original recursion. No assumption
about a full item, queue traversal, or lifecycle result is used here. -/
theorem resume_step (ctx : Context) (w : Work) (zone : ButtonZone) (note : HoldNote)
    (rest : List (ButtonZone × HoldNote)) :
    resume ctx ((zone, note) :: rest) w = resume ctx rest (step ctx w zone note) := by
  unfold resume reference step
  conv => lhs; unfold processHoldNotes
  unfold consume eligible judgment difference
  rcases hc : consumeButtonThenFallbackSensor ctx.input w.cursor
      (holdHeadAwaitsResolution note.state &&
        buttonQueueIndexUnlocked w.frontiers zone note.buttonQueueIndex &&
        holdHeadEligibleForClick note ctx.currentTime)
      zone (fallbackSensorAreaForButtonNote zone) with ⟨button, sensor, cursor⟩
  dsimp only
  rcases hj : holdStep note ctx.currentTime
      (if button then ctx.currentTime - note.params.effectiveTiming
        else (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming)
      HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC (button || sensor)
      (holdBodyPressedFromButtonOrSensor (ctx.input.getButtonHeld zone)
        (fallbackSensorHeldForButtonNote ctx.input zone))
      (ctx.input.getButtonHeld zone)
      (fallbackPrevSensorHeldForButtonNote ctx.prevSensor zone)
      ctx.touchPanelOffset none ctx.delta ctx.style with ⟨after, event⟩
  dsimp only [advance]
  rcases hr : processHoldNotes
      (if enteredHeadJudged note.state after.state then
        advanceSharedButtonQueue w.frontiers zone else w.frontiers)
      (updateButtonHoldQueue w.queues zone after) rest ctx.input ctx.currentTime
      ctx.delta ctx.style ctx.touchPanelOffset ctx.prevSensor cursor with
    ⟨frontiers, queues, active, events, nextCursor⟩
  cases event <;> cases hk : keepHoldActive after <;>
    simp [hc, hj, hr, retained, hk, Option.toList, List.append_assoc]

theorem resume_singleton (ctx : Context) (w : Work) (zone : ButtonZone)
    (note : HoldNote) : resume ctx [(zone, note)] w = step ctx w zone note := by
  rw [resume_step, resume_nil]

/-- Existing output prefixes are preserved, in their original order. -/
theorem resume_prefix (ctx : Context) (pending : List (ButtonZone × HoldNote))
    (w : Work) (active : List (ButtonZone × HoldNote)) (events : List JudgeEvent) :
    resume ctx pending (withPrefix active events w) = withPrefix active events (resume ctx pending w) := by
  simp only [resume, withPrefix, List.append_assoc]

theorem resume_output_prefixes (ctx : Context) (pending : List (ButtonZone × HoldNote))
    (w : Work) : ∃ active events,
    (resume ctx pending w).active = w.active ++ active ∧
    (resume ctx pending w).events = w.events ++ events := by
  exact ⟨_, _, rfl, rfl⟩

theorem advance_output_lengths (w : Work) (zone : ButtonZone) (before after : HoldNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) :
    (advance w zone before after event cursor).active.length =
        w.active.length + (retained zone after).length ∧
    (advance w zone before after event cursor).events.length =
        w.events.length + event.toList.length := by
  exact ⟨List.length_append, List.length_append⟩

theorem retained_length_le_one (zone : ButtonZone) (note : HoldNote) :
    (retained zone note).length ≤ 1 := by
  unfold retained
  split <;> simp

theorem advance_output_bounds (w : Work) (zone : ButtonZone) (before after : HoldNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor) :
    w.active.length ≤ (advance w zone before after event cursor).active.length ∧
    (advance w zone before after event cursor).active.length ≤ w.active.length + 1 ∧
    w.events.length ≤ (advance w zone before after event cursor).events.length ∧
    (advance w zone before after event cursor).events.length ≤ w.events.length + 1 := by
  have hl := advance_output_lengths w zone before after event cursor
  have hr := retained_length_le_one zone after
  have he : event.toList.length ≤ 1 := by cases event <;> simp
  omega

theorem step_output_bounds (ctx : Context) (w : Work) (zone : ButtonZone) (note : HoldNote) :
    w.active.length ≤ (step ctx w zone note).active.length ∧
    (step ctx w zone note).active.length ≤ w.active.length + 1 ∧
    w.events.length ≤ (step ctx w zone note).events.length ∧
    (step ctx w zone note).events.length ≤ w.events.length + 1 := by
  unfold step
  rcases consume ctx zone (eligible ctx w.frontiers zone note) w.cursor with
    ⟨button, sensor, cursor⟩
  dsimp only
  rcases judgment ctx zone note button sensor with ⟨after, event⟩
  exact advance_output_bounds w zone note after event cursor

theorem resume_output_growth (ctx : Context) (pending : List (ButtonZone × HoldNote))
    (w : Work) :
    w.active.length ≤ (resume ctx pending w).active.length ∧
    w.events.length ≤ (resume ctx pending w).events.length := by
  simp only [resume, List.length_append]
  constructor <;> omega

theorem resume_output_bounds (ctx : Context) (pending : List (ButtonZone × HoldNote))
    (w : Work) :
    (resume ctx pending w).active.length ≤ w.active.length + pending.length ∧
    (resume ctx pending w).events.length ≤ w.events.length + pending.length := by
  induction pending generalizing w with
  | nil => simp [resume_nil]
  | cons item rest ih =>
      rcases item with ⟨zone, note⟩
      rw [resume_step]
      have hnext := step_output_bounds ctx w zone note
      have hrest := ih (step ctx w zone note)
      simp only [List.length_cons]
      omega

theorem traverse_nil (ctx : Context) (w : Work) : traverse ctx [] w = w := rfl

theorem traverse_cons (ctx : Context) (w : Work) (zone : ButtonZone) (note : HoldNote)
    (rest : List (ButtonZone × HoldNote)) :
    traverse ctx ((zone, note) :: rest) w = traverse ctx rest (step ctx w zone note) := rfl

/-- Every original item is visited, including Body/Ended entries and entries
whose queue storage was changed by a previous iteration. -/
theorem traverse_eq_resume (ctx : Context) (original : List (ButtonZone × HoldNote))
    (w : Work) : traverse ctx original w = resume ctx original w := by
  induction original generalizing w with
  | nil => rw [traverse_nil, resume_nil]
  | cons item rest ih =>
      rcases item with ⟨zone, note⟩
      rw [traverse_cons, resume_step, ih]

/-- Appending input lists appends each segment's outputs in forward order. -/
theorem resume_append (ctx : Context) (first rest : List (ButtonZone × HoldNote))
    (w : Work) :
    resume ctx (first ++ rest) w = resume ctx rest (resume ctx first w) := by
  induction first generalizing w with
  | nil => simp only [List.nil_append, resume_nil]
  | cons item tail ih =>
      rcases item with ⟨zone, note⟩
      simp only [List.cons_append, resume_step, ih]

theorem resume_split_at (ctx : Context) (original : List (ButtonZone × HoldNote))
    (index : Nat) (w : Work) :
    resume ctx original w =
      resume ctx (original.drop index) (traverse ctx (original.take index) w) := by
  rw [traverse_eq_resume, ← resume_append, List.take_append_drop]

theorem resume_drop_step (ctx : Context) (original : List (ButtonZone × HoldNote))
    (index : Nat) (w : Work) (h : index < original.length) :
    resume ctx (original.drop index) w =
      resume ctx (original.drop (index + 1))
        (step ctx w (original[index]).1 (original[index]).2) := by
  rw [List.drop_eq_getElem_cons h]
  exact resume_step ctx w (original[index]).1 (original[index]).2 _

theorem resume_drop_done (ctx : Context) (original : List (ButtonZone × HoldNote))
    (index : Nat) (w : Work) (h : original.length ≤ index) :
    resume ctx (original.drop index) w = w := by
  rw [List.drop_of_length_le h, resume_nil]

/-- The final capacity bound suffices for every finite traversal prefix. -/
theorem traversal_prefix_lengths (ctx : Context) (original : List (ButtonZone × HoldNote))
    (index : Nat) (w : Work) :
    (traverse ctx (original.take index) w).active.length ≤ (resume ctx original w).active.length ∧
    (traverse ctx (original.take index) w).events.length ≤ (resume ctx original w).events.length := by
  rw [resume_split_at ctx original index w]
  exact resume_output_growth ctx _ _

theorem traverse_original (ctx : Context) (original : List (ButtonZone × HoldNote))
    (frontiers : ButtonVec Nat) (queues : ButtonQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) :
    (traverse ctx original ⟨frontiers, queues, cursor, [], []⟩).output =
      reference ctx original frontiers queues cursor := by
  rw [traverse_eq_resume, resume_original]

end Verification.HoldScheduleModel
