import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

namespace Verification.TouchHoldScheduleModel
open LnmaiCore InputModel Lifecycle Constants
open private processTouchHoldNotes touchHoldBodyCheckActive
  registerTouchHoldBodyTrigger unregisterTouchHoldBodyTrigger
  touchHoldBodyGroupMajorityPressed groupShareResult holdHeadAwaitsResolution
  touchQueueIndexUnlocked holdHeadEligibleForClick tryUseButtonClickAt
  tryUseSensorClickAt enteredHeadJudged advanceSharedTouchQueue
  updateSensorHoldQueue registerTouchGroupResult exitTouchHoldBodyGroupMember
  keepHoldActive from LnmaiCore.Scheduler

/-!
TouchHold reference traversal model. The continuation calls the ORIGINAL
private Scheduler.processTouchHoldNotes. The immutable pending list is separate
from mutable queue storage, current head groups, and current body groups.
Forward output buffers may already have prefixes. No Generated interface or
full-pass/lifecycle equivalence premise is introduced here.

This file is a source draft awaiting the mainline Lean worker. The extracted
loops, concrete Rust facade, scalar/container representations, queue composition,
and one-note judgment wiring require separate proofs.
-/

structure Context where
  input : FrameInput
  currentTime : TimePoint
  delta : Duration
  style : JudgeStyle
  touchPanelOffset : Duration
  buttonRingForTouch : Bool

/-- Field order follows the frozen Rust TouchHoldWork interface. -/
structure Work where
  frontiers : SensorVec Nat
  queues : SensorQueueVec HoldNote
  cursor : Scheduler.ClickCursor
  active : List (SensorArea × HoldNote)
  events : List JudgeEvent
  groups : List GroupState
  bodyGroups : List TouchHoldBodyGroupState

abbrev Outcome := SensorVec Nat × SensorQueueVec HoldNote ×
  List (SensorArea × HoldNote) × List JudgeEvent × Scheduler.ClickCursor ×
    List GroupState × List TouchHoldBodyGroupState

def Work.output (w : Work) : Outcome :=
  (w.frontiers, w.queues, w.active, w.events, w.cursor, w.groups, w.bodyGroups)

/-- Alias of the original reference recursion, including its seven outputs. -/
def reference (ctx : Context) (pending : List (SensorArea × HoldNote))
    (frontiers : SensorVec Nat) (queues : SensorQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) (groups : List GroupState)
    (bodyGroups : List TouchHoldBodyGroupState) : Outcome :=
  processTouchHoldNotes frontiers queues pending ctx.input ctx.currentTime ctx.delta
    ctx.style ctx.touchPanelOffset ctx.buttonRingForTouch cursor groups bodyGroups

def resume (ctx : Context) (pending : List (SensorArea × HoldNote)) (w : Work) : Work :=
  let out := reference ctx pending w.frontiers w.queues w.cursor w.groups w.bodyGroups
  { frontiers := out.1
    queues := out.2.1
    cursor := out.2.2.2.2.1
    active := w.active ++ out.2.2.1
    events := w.events ++ out.2.2.2.1
    groups := out.2.2.2.2.2.1
    bodyGroups := out.2.2.2.2.2.2 }

def withPrefix (active : List (SensorArea × HoldNote)) (events : List JudgeEvent)
    (w : Work) : Work :=
  { w with active := active ++ w.active, events := events ++ w.events }

/-- Local vote update precedes the majority lookup for this same note. -/
def bodyBefore (ctx : Context) (area : SensorArea) (note : HoldNote)
    (groups : List TouchHoldBodyGroupState) : List TouchHoldBodyGroupState :=
  if touchHoldBodyCheckActive note ctx.currentTime then
    match note.touchHoldGroupId with
    | some groupId =>
        if ctx.input.getSensorHeld area then
          registerTouchHoldBodyTrigger groups groupId note.params.noteIndex
        else
          unregisterTouchHoldBodyTrigger groups groupId note.params.noteIndex
    | none => groups
  else
    groups

def effectivePressed (ctx : Context) (area : SensorArea) (note : HoldNote)
    (updatedBodyGroups : List TouchHoldBodyGroupState) : Bool :=
  ctx.input.getSensorHeld area ||
    match note.touchHoldGroupId with
    | some groupId => touchHoldBodyGroupMajorityPressed updatedBodyGroups groupId
    | none => false

def sharedResult (groups : List GroupState) (note : HoldNote) :
    Option (JudgeGrade × Duration) :=
  match note.touchGroupId with
  | some groupId => groupShareResult groups groupId
  | none => none

def eligible (ctx : Context) (frontiers : SensorVec Nat) (area : SensorArea)
    (note : HoldNote) (shared : Option (JudgeGrade × Duration)) : Bool :=
  shared.isNone && ctx.currentTime <= note.params.effectiveTiming + touchGoodMs &&
    holdHeadAwaitsResolution note.state &&
    touchQueueIndexUnlocked frontiers area note.touchQueueIndex &&
    holdHeadEligibleForClick note ctx.currentTime

def useButton (ctx : Context) (area : SensorArea) (allowInput : Bool)
    (cursor : Scheduler.ClickCursor) : Bool × Scheduler.ClickCursor :=
  if allowInput && ctx.buttonRingForTouch then
    match area.toOuterButtonZone? with
    | some zone => tryUseButtonClickAt ctx.input cursor zone
    | none => (false, cursor)
  else
    (false, cursor)

def useSensor (ctx : Context) (area : SensorArea) (allowInput usedButton : Bool)
    (cursor : Scheduler.ClickCursor) : Bool × Scheduler.ClickCursor :=
  if allowInput then
    if usedButton then (false, cursor) else tryUseSensorClickAt ctx.input cursor area
  else
    (false, cursor)

/-- Button priority, followed by sensor fallback on the updated cursor. -/
def consume (ctx : Context) (area : SensorArea) (allowInput : Bool)
    (cursor : Scheduler.ClickCursor) : Bool × Bool × Scheduler.ClickCursor :=
  let (button, cursorButton) := useButton ctx area allowInput cursor
  let (sensor, cursorSensor) := useSensor ctx area allowInput button cursorButton
  (button, sensor, cursorSensor)

/-- No button means sensor diff, including the case with no consumed click. -/
def difference (ctx : Context) (note : HoldNote) (usedButton : Bool) : Duration :=
  if usedButton then ctx.currentTime - note.params.effectiveTiming
  else (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming

def judgment (ctx : Context) (note : HoldNote) (usedButton usedSensor pressed : Bool)
    (shared : Option (JudgeGrade × Duration)) : HoldNote × Option JudgeEvent :=
  holdStep note ctx.currentTime (difference ctx note usedButton)
    TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
    (usedButton || usedSensor) pressed false false ctx.touchPanelOffset shared
    ctx.delta ctx.style

/-- Preserve the original state-based registration condition. In particular,
no extra clicked or not-shared guard is silently added. -/
def headGroupsAfter (groups : List GroupState) (before after : HoldNote) :
    List GroupState :=
  if enteredHeadJudged before.state after.state then
    match after.state, before.touchGroupId with
    | HoldSubState.HeadJudged grade, some groupId =>
        if grade.isMissOrTooFast then groups
        else registerTouchGroupResult groups groupId before.touchGroupSize grade after.headDiff
    | _, _ => groups
  else
    groups

/-- Ended removes the original member ID from its original body group. -/
def bodyGroupsAfter (groups : List TouchHoldBodyGroupState) (before after : HoldNote) :
    List TouchHoldBodyGroupState :=
  match before.touchHoldGroupId with
  | some groupId =>
      if keepHoldActive after then groups
      else exitTouchHoldBodyGroupMember groups groupId before.params.noteIndex
  | none => groups

def retained (area : SensorArea) (note : HoldNote) : List (SensorArea × HoldNote) :=
  if keepHoldActive note then [(area, note)] else []

/-- Frontier transition, unconditional queue synchronization, current-group
writeback, and forward output append are independent of event presence. -/
def advance (w : Work) (area : SensorArea) (before after : HoldNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor)
    (bodyGroups1 : List TouchHoldBodyGroupState) : Work :=
  { frontiers := if enteredHeadJudged before.state after.state then
      advanceSharedTouchQueue w.frontiers area else w.frontiers
    queues := updateSensorHoldQueue w.queues area after
    cursor := cursor
    active := w.active ++ retained area after
    events := w.events ++ event.toList
    groups := headGroupsAfter w.groups before after
    bodyGroups := bodyGroupsAfter bodyGroups1 before after }

def step (ctx : Context) (w : Work) (area : SensorArea) (note : HoldNote) : Work :=
  let bodyGroups1 := bodyBefore ctx area note w.bodyGroups
  let pressed := effectivePressed ctx area note bodyGroups1
  let shared := sharedResult w.groups note
  let (button, sensor, cursor) :=
    consume ctx area (eligible ctx w.frontiers area note shared) w.cursor
  let (after, event) := judgment ctx note button sensor pressed shared
  advance w area note after event cursor bodyGroups1

/-- Finite original-list traversal; queue mutation does not replace its source. -/
def traverse (ctx : Context) (original : List (SensorArea × HoldNote)) (w : Work) : Work :=
  original.foldl (fun state item => step ctx state item.1 item.2) w

theorem resume_nil (ctx : Context) (w : Work) : resume ctx [] w = w := by
  cases w
  simp [resume, reference, processTouchHoldNotes]

theorem resume_original (ctx : Context) (pending : List (SensorArea × HoldNote))
    (frontiers : SensorVec Nat) (queues : SensorQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) (groups : List GroupState)
    (bodyGroups : List TouchHoldBodyGroupState) :
    (resume ctx pending ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩).output =
      reference ctx pending frontiers queues cursor groups bodyGroups := by
  rfl

/-- Unfold one constructor of the ORIGINAL recursion. The recursive tail sees
all five updated state fields, including both groups and the consumed cursor. -/
theorem resume_step (ctx : Context) (w : Work) (area : SensorArea) (note : HoldNote)
    (rest : List (SensorArea × HoldNote)) :
    resume ctx ((area, note) :: rest) w = resume ctx rest (step ctx w area note) := by
  cases hg : note.touchGroupId <;> cases hz : area.toOuterButtonZone? <;>
    cases hbg : note.touchHoldGroupId
  all_goals
    rcases hb : useButton ctx area
        (eligible ctx w.frontiers area note (sharedResult w.groups note))
        w.cursor with ⟨button, cursorButton⟩
    rcases hs : useSensor ctx area
        (eligible ctx w.frontiers area note (sharedResult w.groups note))
        button cursorButton with ⟨sensor, cursor⟩
    rcases hj : judgment ctx note button sensor
        (effectivePressed ctx area note (bodyBefore ctx area note w.bodyGroups))
        (sharedResult w.groups note) with ⟨after, event⟩
    let next := advance w area note after event cursor (bodyBefore ctx area note w.bodyGroups)
    rcases hr : reference ctx rest next.frontiers next.queues next.cursor
        next.groups next.bodyGroups with
      ⟨frontiers, queues, active, events, nextCursor, groups, bodyGroups⟩
    unfold resume reference step
    conv => lhs; unfold processTouchHoldNotes
    dsimp only [next, reference, consume, useButton, useSensor, eligible, sharedResult,
      judgment, difference, bodyBefore, effectivePressed, advance, headGroupsAfter,
      bodyGroupsAfter] at hb hs hj hr ⊢
    -- Rewrite the concrete pairs before simp normalizes Boolean guards into
    -- propositions; otherwise hb/hs no longer match the expanded reference.
    simp only [hg, hz, hbg] at hb hs hj hr ⊢
    simp only [hb, hs, hj, hr]
    cases event <;> cases hk : keepHoldActive after <;>
      simp [retained, hk, hg, hz, hbg, Option.toList, List.append_assoc]

theorem resume_singleton (ctx : Context) (w : Work) (area : SensorArea)
    (note : HoldNote) : resume ctx [(area, note)] w = step ctx w area note := by
  rw [resume_step, resume_nil]

/-- The second note reads the first note's updated groups/frontiers/cursor. -/
theorem resume_two (ctx : Context) (w : Work) (area₁ area₂ : SensorArea)
    (note₁ note₂ : HoldNote) :
    resume ctx [(area₁, note₁), (area₂, note₂)] w =
      step ctx (step ctx w area₁ note₁) area₂ note₂ := by
  rw [resume_step, resume_singleton]

theorem consume_disabled (ctx : Context) (area : SensorArea)
    (cursor : Scheduler.ClickCursor) :
    consume ctx area false cursor = (false, false, cursor) := by
  simp [consume, useButton, useSensor]

/-- A stored shared result blocks BOTH physical click counters. -/
theorem shared_blocks_clicks (ctx : Context) (frontiers : SensorVec Nat)
    (area : SensorArea) (note : HoldNote) (grade : JudgeGrade) (diff : Duration)
    (cursor : Scheduler.ClickCursor) :
    consume ctx area (eligible ctx frontiers area note (some (grade, diff))) cursor =
      (false, false, cursor) := by
  simp [eligible, consume, useButton, useSensor]

theorem difference_without_button (ctx : Context) (note : HoldNote) :
    difference ctx note false =
      (ctx.currentTime - ctx.touchPanelOffset) - note.params.effectiveTiming := rfl

/-- Old output prefixes remain first, in their original order. -/
theorem resume_prefix (ctx : Context) (pending : List (SensorArea × HoldNote))
    (w : Work) (active : List (SensorArea × HoldNote)) (events : List JudgeEvent) :
    resume ctx pending (withPrefix active events w) =
      withPrefix active events (resume ctx pending w) := by
  simp only [resume, withPrefix, List.append_assoc]

theorem resume_output_prefixes (ctx : Context) (pending : List (SensorArea × HoldNote))
    (w : Work) : ∃ active events,
    (resume ctx pending w).active = w.active ++ active ∧
    (resume ctx pending w).events = w.events ++ events := by
  exact ⟨_, _, rfl, rfl⟩

theorem advance_output_lengths (w : Work) (area : SensorArea) (before after : HoldNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor)
    (bodyGroups1 : List TouchHoldBodyGroupState) :
    (advance w area before after event cursor bodyGroups1).active.length =
        w.active.length + (retained area after).length ∧
    (advance w area before after event cursor bodyGroups1).events.length =
        w.events.length + event.toList.length := by
  exact ⟨List.length_append, List.length_append⟩

theorem retained_length_le_one (area : SensorArea) (note : HoldNote) :
    (retained area note).length ≤ 1 := by
  unfold retained
  split <;> simp

theorem advance_output_bounds (w : Work) (area : SensorArea) (before after : HoldNote)
    (event : Option JudgeEvent) (cursor : Scheduler.ClickCursor)
    (bodyGroups1 : List TouchHoldBodyGroupState) :
    w.active.length ≤ (advance w area before after event cursor bodyGroups1).active.length ∧
    (advance w area before after event cursor bodyGroups1).active.length ≤ w.active.length + 1 ∧
    w.events.length ≤ (advance w area before after event cursor bodyGroups1).events.length ∧
    (advance w area before after event cursor bodyGroups1).events.length ≤ w.events.length + 1 := by
  have hl := advance_output_lengths w area before after event cursor bodyGroups1
  have hr := retained_length_le_one area after
  have he : event.toList.length ≤ 1 := by cases event <;> simp
  omega

theorem step_output_bounds (ctx : Context) (w : Work) (area : SensorArea) (note : HoldNote) :
    w.active.length ≤ (step ctx w area note).active.length ∧
    (step ctx w area note).active.length ≤ w.active.length + 1 ∧
    w.events.length ≤ (step ctx w area note).events.length ∧
    (step ctx w area note).events.length ≤ w.events.length + 1 := by
  rcases hc : consume ctx area (eligible ctx w.frontiers area note (sharedResult w.groups note))
      w.cursor with ⟨button, sensor, cursor⟩
  rcases hj : judgment ctx note button sensor
      (effectivePressed ctx area note (bodyBefore ctx area note w.bodyGroups))
      (sharedResult w.groups note) with ⟨after, event⟩
  simpa only [step, hc, hj] using
    advance_output_bounds w area note after event cursor (bodyBefore ctx area note w.bodyGroups)

theorem resume_output_growth (ctx : Context) (pending : List (SensorArea × HoldNote))
    (w : Work) :
    w.active.length ≤ (resume ctx pending w).active.length ∧
    w.events.length ≤ (resume ctx pending w).events.length := by
  simp only [resume, List.length_append]
  constructor <;> omega

theorem resume_output_bounds (ctx : Context) (pending : List (SensorArea × HoldNote))
    (w : Work) :
    (resume ctx pending w).active.length ≤ w.active.length + pending.length ∧
    (resume ctx pending w).events.length ≤ w.events.length + pending.length := by
  induction pending generalizing w with
  | nil => simp [resume_nil]
  | cons item rest ih =>
      rcases item with ⟨area, note⟩
      rw [resume_step]
      have hnext := step_output_bounds ctx w area note
      have hrest := ih (step ctx w area note)
      simp only [List.length_cons]
      omega

theorem traverse_nil (ctx : Context) (w : Work) : traverse ctx [] w = w := rfl

theorem traverse_cons (ctx : Context) (w : Work) (area : SensorArea) (note : HoldNote)
    (rest : List (SensorArea × HoldNote)) :
    traverse ctx ((area, note) :: rest) w = traverse ctx rest (step ctx w area note) := rfl

/-- Every source item is visited, including Body/Ended and no-event entries. -/
theorem traverse_eq_resume (ctx : Context) (original : List (SensorArea × HoldNote))
    (w : Work) : traverse ctx original w = resume ctx original w := by
  induction original generalizing w with
  | nil => rw [traverse_nil, resume_nil]
  | cons item rest ih =>
      rcases item with ⟨area, note⟩
      rw [traverse_cons, resume_step, ih]

/-- Concatenated source segments preserve forward active/event order and thread
both group lists from the first segment into the second. -/
theorem resume_append (ctx : Context) (first rest : List (SensorArea × HoldNote))
    (w : Work) :
    resume ctx (first ++ rest) w = resume ctx rest (resume ctx first w) := by
  induction first generalizing w with
  | nil => simp only [List.nil_append, resume_nil]
  | cons item tail ih =>
      rcases item with ⟨area, note⟩
      simp only [List.cons_append, resume_step, ih]

theorem resume_split_at (ctx : Context) (original : List (SensorArea × HoldNote))
    (index : Nat) (w : Work) :
    resume ctx original w =
      resume ctx (original.drop index) (traverse ctx (original.take index) w) := by
  rw [traverse_eq_resume, ← resume_append, List.take_append_drop]

theorem resume_drop_step (ctx : Context) (original : List (SensorArea × HoldNote))
    (index : Nat) (w : Work) (h : index < original.length) :
    resume ctx (original.drop index) w =
      resume ctx (original.drop (index + 1))
        (step ctx w (original[index]).1 (original[index]).2) := by
  rw [List.drop_eq_getElem_cons h]
  exact resume_step ctx w (original[index]).1 (original[index]).2 _

theorem resume_drop_done (ctx : Context) (original : List (SensorArea × HoldNote))
    (index : Nat) (w : Work) (h : original.length ≤ index) :
    resume ctx (original.drop index) w = w := by
  rw [List.drop_of_length_le h, resume_nil]

/-- Final output lengths bound every finite source-prefix traversal. -/
theorem traversal_prefix_lengths (ctx : Context) (original : List (SensorArea × HoldNote))
    (index : Nat) (w : Work) :
    (traverse ctx (original.take index) w).active.length ≤ (resume ctx original w).active.length ∧
    (traverse ctx (original.take index) w).events.length ≤ (resume ctx original w).events.length := by
  rw [resume_split_at ctx original index w]
  exact resume_output_growth ctx _ _

theorem traverse_original (ctx : Context) (original : List (SensorArea × HoldNote))
    (frontiers : SensorVec Nat) (queues : SensorQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) (groups : List GroupState)
    (bodyGroups : List TouchHoldBodyGroupState) :
    (traverse ctx original ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩).output =
      reference ctx original frontiers queues cursor groups bodyGroups := by
  rw [traverse_eq_resume, resume_original]

end Verification.TouchHoldScheduleModel
