import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

namespace Verification.TouchAutomaticModel
open LnmaiCore InputModel Lifecycle Constants
open private processTouchQueueAutomatic processTouchQueueAutomatic.loop
  touchQueueResolved groupShareResult advanceSharedTouchQueue normalizeTouchQueueCursor
  from LnmaiCore.Scheduler

/-!
Reference equations for the original automatic Touch traversal. Groups are a
fixed snapshot; every original slot is visited once, including slots before
the current queue cursor. Only unresolved eligible notes run touchStep, always
with clicked=false. The forward note buffer is related to the original reverse
accumulator, while event order remains reverse. No generated Rust is modeled
or claimed proved in this file.
-/

structure Context where
  area : SensorArea
  currentTime : TimePoint
  style : JudgeStyle
  touchPanelOffset : Duration
  groups : List GroupState

structure Work where
  notes : List TouchNote
  frontiers : SensorVec Nat
  eventsRev : List JudgeEvent

abbrev Outcome := ZoneQueue TouchNote × SensorVec Nat × List GroupState × List JudgeEvent

def shared (ctx : Context) (note : TouchNote) : Option (JudgeGrade × Duration) :=
  match note.touchGroupId with
  | some id => groupShareResult ctx.groups id
  | none => none

def difference (ctx : Context) (note : TouchNote) : Duration :=
  (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming

def tooLate (ctx : Context) (note : TouchNote) : Bool :=
  decide (ctx.currentTime > note.params.effectiveTiming + touchGoodMs)

def keep (w : Work) (note : TouchNote) : Work :=
  { w with notes := w.notes ++ [note] }

def update (ctx : Context) (w : Work) (before after : TouchNote)
    (event : Option JudgeEvent) : Work :=
  { notes := w.notes ++ [after]
    frontiers := if !touchQueueResolved before && touchQueueResolved after then
      advanceSharedTouchQueue w.frontiers ctx.area else w.frontiers
    eventsRev := match event with | none => w.eventsRev | some event => event :: w.eventsRev }

def step (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (note : TouchNote) (w : Work) : Work :=
  if decide (index < queue.currentIndex) || touchQueueResolved note then keep w note
  else
    let result := shared ctx note
    if result.isNone && !tooLate ctx note then keep w note
    else
      let (after, event) := touchStep note ctx.currentTime (difference ctx note) false result ctx.style
      update ctx w note after event

/-- The continuation invokes the original local recursion directly. -/
def resume (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (pending : List TouchNote) (w : Work) : Work :=
  let (notes, frontiers, events) := processTouchQueueAutomatic.loop ctx.area queue
    ctx.currentTime ctx.style ctx.touchPanelOffset ctx.groups index pending
    w.notes.reverse w.frontiers w.eventsRev
  ⟨notes, frontiers, events⟩

def finish (ctx : Context) (queue : ZoneQueue TouchNote) (w : Work) : Outcome :=
  (normalizeTouchQueueCursor { queue with notes := w.notes },
    w.frontiers, ctx.groups, w.eventsRev)

def reference (ctx : Context) (queue : ZoneQueue TouchNote) (frontiers : SensorVec Nat)
    (events : List JudgeEvent) : Outcome :=
  processTouchQueueAutomatic ctx.area queue frontiers ctx.currentTime ctx.style
    ctx.touchPanelOffset ctx.groups events

theorem resume_nil (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat) (w : Work) :
    resume ctx queue index [] w = w := by
  cases w
  simp [resume, processTouchQueueAutomatic.loop]

theorem resume_step (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (note : TouchNote) (rest : List TouchNote) (w : Work) :
    resume ctx queue index (note :: rest) w =
      resume ctx queue (index + 1) rest (step ctx queue index note w) := by
  unfold resume step
  conv => lhs; unfold processTouchQueueAutomatic.loop
  dsimp only
  by_cases hskip : (decide (index < queue.currentIndex) || touchQueueResolved note) = true
  · simp [hskip, keep]
  · simp only [hskip, Bool.false_eq_true, if_false]
    unfold shared difference tooLate
    by_cases hwait : ((match note.touchGroupId with
        | some id => groupShareResult ctx.groups id | none => none).isNone &&
        !decide (ctx.currentTime > note.params.effectiveTiming + touchGoodMs)) = true
    · simp only [hwait, if_true, keep, List.reverse_append, List.reverse_cons,
        List.reverse_nil, List.nil_append, List.singleton_append]
      cases hg : note.touchGroupId <;> simp_all
    · simp only [hwait, Bool.false_eq_true, if_false]
      rcases hj : touchStep note ctx.currentTime
          ((ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming) false
          (match note.touchGroupId with
            | some id => groupShareResult ctx.groups id | none => none) ctx.style with
        ⟨after, event⟩
      cases event <;> simp only [update, List.reverse_append, List.reverse_cons,
        List.reverse_nil, List.nil_append, List.singleton_append]
      all_goals cases hg : note.touchGroupId <;> simp_all <;> split <;> simp_all

theorem reference_eq_resume (ctx : Context) (queue : ZoneQueue TouchNote)
    (frontiers : SensorVec Nat) (events : List JudgeEvent) :
    finish ctx queue (resume ctx queue 0 queue.notes ⟨[], frontiers, events⟩) =
      reference ctx queue frontiers events := by
  rfl

theorem step_notes_length (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (note : TouchNote) (w : Work) :
    (step ctx queue index note w).notes.length = w.notes.length + 1 := by
  unfold step
  split
  · simp [keep]
  · dsimp only
    split
    · simp [keep]
    · rcases touchStep note ctx.currentTime (difference ctx note) false (shared ctx note)
        ctx.style with ⟨after, event⟩
      simp [update]

theorem step_events_bound (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (note : TouchNote) (w : Work) :
    w.eventsRev.length ≤ (step ctx queue index note w).eventsRev.length ∧
    (step ctx queue index note w).eventsRev.length ≤ w.eventsRev.length + 1 := by
  unfold step
  split
  · simp [keep]
  · dsimp only
    split
    · simp [keep]
    · rcases touchStep note ctx.currentTime (difference ctx note) false (shared ctx note)
        ctx.style with ⟨after, event⟩
      cases event <;> simp [update]

theorem resume_notes_length (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (pending : List TouchNote) (w : Work) :
    (resume ctx queue index pending w).notes.length = w.notes.length + pending.length := by
  induction pending generalizing index w with
  | nil => simp [resume_nil]
  | cons note rest ih =>
      rw [resume_step, ih, step_notes_length]
      simp only [List.length_cons]
      omega

theorem resume_events_bound (ctx : Context) (queue : ZoneQueue TouchNote) (index : Nat)
    (pending : List TouchNote) (w : Work) :
    w.eventsRev.length ≤ (resume ctx queue index pending w).eventsRev.length ∧
    (resume ctx queue index pending w).eventsRev.length ≤ w.eventsRev.length + pending.length := by
  induction pending generalizing index w with
  | nil => simp [resume_nil]
  | cons note rest ih =>
      rw [resume_step]
      have hs := step_events_bound ctx queue index note w
      have hi := ih (index + 1) (step ctx queue index note w)
      simp only [List.length_cons]
      omega

/-- The automatic pass never registers a group result. -/
theorem finish_groups (ctx : Context) (queue : ZoneQueue TouchNote) (w : Work) :
    (finish ctx queue w).2.2.1 = ctx.groups := rfl

theorem resume_drop_step (ctx : Context) (queue : ZoneQueue TouchNote)
    (index : Nat) (w : Work) (h : index < queue.notes.length) :
    resume ctx queue index (queue.notes.drop index) w =
      resume ctx queue (index + 1) (queue.notes.drop (index + 1))
        (step ctx queue index (queue.notes[index]'h) w) := by
  rw [List.drop_eq_getElem_cons h, resume_step]

theorem resume_drop_done (ctx : Context) (queue : ZoneQueue TouchNote)
    (index : Nat) (w : Work) (h : queue.notes.length ≤ index) :
    resume ctx queue index (queue.notes.drop index) w = w := by
  rw [List.drop_eq_nil_iff.mpr h, resume_nil]

end Verification.TouchAutomaticModel
