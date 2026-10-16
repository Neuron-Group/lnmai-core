import Verification.TouchHoldScheduleModel
import Verification.TouchHoldGroups

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

set_option maxHeartbeats 1200000

namespace Verification.TouchHoldScheduleProof
open TouchHoldScheduleModel TouchHoldGroupsProof
open private touchHoldBodyCheckActive registerTouchHoldBodyTrigger
  unregisterTouchHoldBodyTrigger touchHoldBodyGroupMajorityPressed groupShareResult
  holdHeadAwaitsResolution touchQueueIndexUnlocked holdHeadEligibleForClick
  tryUseButtonClickAt tryUseSensorClickAt enteredHeadJudged isHeadJudgedState
  advanceSharedTouchQueue updateSensorHoldQueue registerTouchGroupResult
  exitTouchHoldBodyGroupMember keepHoldActive from LnmaiCore.Scheduler

/-!
The real extracted TouchHold traversal, with the original recursion as its
continuation. Native fragments below merely name contiguous pieces of Generated;
body_eq_item proves their expansion is the actual extracted body. No contract
assumes a whole item, pass, or group operation agrees.

The concrete Rust facade is NOT extracted. Field/time/ID/container laws,
single-note holdStep, and updateSensorHoldQueue (replace-all + normalize) remain
explicit. HeadAtoms/BodyAtoms discharge the group algorithms, including duplicate
IDs and first-occurrence erasure. No uniqueness invariant is imposed here.

Source draft: compilation is reserved for the mainline worker.
-/

structure Representation (S A N F Q C H E Event D Z Id Grade G B : Type) where
  holds : S → List (SensorArea × HoldNote)
  area : A → SensorArea
  note : N → HoldNote
  frontiers : F → SensorVec Nat
  queues : Q → SensorQueueVec HoldNote
  cursor : C → Scheduler.ClickCursor
  active : H → List (SensorArea × HoldNote)
  events : E → List JudgeEvent
  event : Event → JudgeEvent
  duration : D → Duration
  zone : Z → ButtonZone
  id : Id → Nat
  grade : Grade → JudgeGrade
  groups : G → List GroupState
  bodyGroups : B → List TouchHoldBodyGroupState

variable {S A N F Q C H E Event D Z Id Grade G B Ops : Type}

def headRep (rep : Representation S A N F Q C H E Event D Z Id Grade G B) :
    HeadRepresentation G Id Grade D :=
  ⟨rep.groups, rep.id, rep.grade, rep.duration⟩

def bodyRep (rep : Representation S A N F Q C H E Event D Z Id Grade G B) :
    BodyRepresentation B Id := ⟨rep.bodyGroups, rep.id⟩

def decodeShared (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (pair : Grade × D) : JudgeGrade × Duration :=
  (rep.grade pair.1, rep.duration pair.2)

def interpret (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) : Work :=
  ⟨rep.frontiers w.frontiers, rep.queues w.queues, rep.cursor w.cursor,
    rep.active w.active, rep.events w.events, rep.groups w.groups,
    rep.bodyGroups w.body_groups⟩

structure Capacity where
  active : Nat
  events : Nat
  heads : Nat
  bodies : BodyCapacity

def Fits (cap : Capacity) (w : Work) : Prop :=
  w.active.length ≤ cap.active ∧ w.events.length ≤ cap.events ∧
    w.groups.length ≤ cap.heads

/-- A body registration can grow a container that the same item later shrinks.
The intermediate bound therefore cannot be replaced by a final-pass bound. -/
def StepFits (ctx : Context) (cap : Capacity) (w : Work)
    (area : SensorArea) (note : HoldNote) : Prop :=
  BodyFits cap.bodies (bodyBefore ctx area note w.bodyGroups) ∧
    Fits cap (step ctx w area note)

/-- Only size bounds on the model prefixes, not an execution/equivalence oracle. -/
def Safe (ctx : Context) (cap : Capacity) : List (SensorArea × HoldNote) → Work → Prop
  | [], _ => True
  | (area, note) :: rest, w =>
      StepFits ctx cap w area note ∧ Safe ctx cap rest (step ctx w area note)

def phase (note : HoldNote) : hold_schedule.HoldPhase :=
  match note.state with
  | .HeadWaiting => .HeadWaiting
  | .HeadJudgeable => .HeadJudgeable
  | .HeadJudged _ => .HeadJudged
  | .BodyHeld => .BodyHeld
  | .BodyReleased => .BodyReleased
  | .Ended _ => .Ended

def headGrade (note : HoldNote) : Option JudgeGrade :=
  match note.state with | .HeadJudged grade => some grade | _ => none

def spendButton (cursor : Scheduler.ClickCursor) (zone : ButtonZone) : Scheduler.ClickCursor :=
  { cursor with buttonUsed := cursor.buttonUsed.set zone (cursor.buttonUsed.getD zone 0 + 1) }

def spendSensor (cursor : Scheduler.ClickCursor) (area : SensorArea) : Scheduler.ClickCursor :=
  { cursor with sensorUsed := cursor.sensorUsed.set area (cursor.sensorUsed.getD area 0 + 1) }

/-- All fields are point laws except the named one-note lifecycle and queue
composition boundaries. Success includes bounded arithmetic, clones and writes;
append success is required only under the corresponding capacity bound. -/
structure AtomicAgrees
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
  step : ∀ note diff clicked pressed shared, ∃ after event,
    call.step ops note diff clicked pressed shared = ok (after, event) ∧
    (rep.note after, event.map rep.event) =
      holdStep (rep.note note) ctx.currentTime (rep.duration diff)
        TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC
        clicked pressed false false ctx.touchPanelOffset (shared.map (decodeShared rep))
        ctx.delta ctx.style
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

section Native
variable (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
variable (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
variable (bodies : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)

/-- Exact fragments of the extracted body. Carried tuples deliberately follow
Generated, including unchanged fields, to make body_eq_item purely structural. -/
def voteNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (localPressed : Bool) (bodyId : Option Id) (noteId : Id) (active : Bool) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B × Bool × Option Id) := do
  if active then
    let (w1, local1) ← match bodyId with
      | none => ok (w, localPressed)
      | some id => do
          let next ← if localPressed then
              touch_hold_schedule.register_body bodies w.body_groups id noteId ops
            else touch_hold_schedule.unregister_body bodies w.body_groups id noteId ops
          ok ({ w with body_groups := next }, localPressed)
    ok (w1, local1, bodyId)
  else ok (w, localPressed, bodyId)

def pressureNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (localPressed : Bool) (bodyId : Option Id) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B × Option Id × Bool) := do
  if localPressed then ok (w, bodyId, true)
  else
    let (w1, pressed) ← match bodyId with
      | none => ok (w, false)
      | some id => do
          let pressed ← touch_hold_schedule.body_pressed bodies w.body_groups id ops
          ok (w, pressed)
    ok (w1, bodyId, pressed)

def sharedNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (headId : Option Id) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B × Option (Grade × D)) := do
  match headId with
  | none => ok (w, none)
  | some id => do
      let shared ← touch_hold_schedule.shared_result heads w.groups id ops
      ok (w, shared)

def awaitNative (p : hold_schedule.HoldPhase) : Result Bool :=
  match p with
  | .HeadWaiting => ok true | .HeadJudgeable => ok true | .HeadJudged => ok false
  | .BodyHeld => ok false | .BodyReleased => ok false | .Ended => ok false

def judgedNative (p : hold_schedule.HoldPhase) : Result Bool :=
  match p with
  | .HeadWaiting => ok false | .HeadJudgeable => ok false | .HeadJudged => ok true
  | .BodyHeld => ok false | .BodyReleased => ok false | .Ended => ok false

def endedNative (p : hold_schedule.HoldPhase) : Result Bool :=
  match p with
  | .HeadWaiting => ok false | .HeadJudgeable => ok false | .HeadJudged => ok false
  | .BodyHeld => ok false | .BodyReleased => ok false | .Ended => ok true

def admissionNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (area : A) (before : N) (p : hold_schedule.HoldPhase) (shared : Option (Grade × D)) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B × hold_schedule.HoldPhase × Bool) := do
  if core.option.Option.is_none shared then
    let within ← call.within_head_window ops before
    if within then
      let awaits ← awaitNative p
      let (w1, allow) ← if awaits then do
          let unlocked ← call.unlocked ops w.frontiers area before
          let eligible ← if unlocked then call.eligible ops before else ok false
          ok (w, eligible)
        else ok (w, false)
      ok (w1, p, allow)
    else ok (w, p, false)
  else ok (w, p, false)

def clicksAfterOuter (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (area : A) (allow : Bool) (outer : Option Z) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B × Bool × Bool) := do
  let (w1, buttonAvailable) ← match outer with
    | none => ok (w, false)
    | some zone => do
        let available ← call.button_available ops w.cursor zone
        ok (w, available)
  let (w2, ba, sa) ← if allow then do
      let (w3, available) ← if buttonAvailable then ok (w1, false) else do
        let available ← call.sensor_available ops w1.cursor area
        ok (w1, available)
      ok (w3, buttonAvailable, available)
    else ok (w1, buttonAvailable, false)
  let plan ← scheduler.consume_click allow ba sa
  let (w3, button) ← if plan.used_button then do
      let w4 ← match outer with
        | none => ok w2
        | some zone => do
            let cursor ← call.use_button ops w2.cursor zone
            ok { w2 with cursor := cursor }
      ok (w4, true)
    else ok (w2, false)
  let (w4, sensor) ← if plan.used_sensor then do
      let cursor ← call.use_sensor ops w3.cursor area
      ok ({ w3 with cursor := cursor }, true)
    else ok (w3, false)
  ok (w4, button, sensor)

def clicksNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (area : A) (allow : Bool) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B × Bool × Bool) := do
  let outer ← if allow then do
      let ring ← call.button_ring ops
      if ring then call.outer_button ops area else ok none
    else ok none
  clicksAfterOuter call ops w area allow outer

def enteredNative (before after : hold_schedule.HoldPhase) : Result (hold_schedule.HoldPhase × Bool) := do
  let wasJudged ← judgedNative before
  if wasJudged then ok (after, false) else do
    let nowJudged ← judgedNative after
    ok (after, nowJudged)

def headWriteNative (groups : G) (before after : N) (headId : Option Id)
    (entered : Bool) : Result G := do
  if entered then
    match headId with
    | none => ok groups
    | some id => do
        let grade ← call.head_grade ops after
        match grade with
        | none => ok groups
        | some grade =>
            let rejected ← call.grade_rejected ops grade
            if rejected then ok groups else do
              let size ← call.group_size ops before
              let diff ← call.head_diff ops after
              touch_hold_schedule.register_head heads groups id size grade diff ops
  else ok groups

def bodyExitNative (groups : B) (bodyId : Option Id) (noteId : Id) (ended : Bool) :
    Result (B × Bool) := do
  if ¬ ended then ok (groups, true) else do
    let groups1 ← match bodyId with
      | none => ok groups
      | some id => touch_hold_schedule.exit_body bodies groups id noteId ops
    ok (groups1, false)

def finishNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (area : A) (before after : N) (event : Option Event)
    (bodyId : Option Id) (noteId : Id) (headId : Option Id)
    (beforePhase : hold_schedule.HoldPhase) :
    Result (touch_hold_schedule.TouchHoldWork F Q C H E G B) := do
  let afterPhase ← call.phase ops after
  let (afterPhase1, entered) ← enteredNative beforePhase afterPhase
  let w1 ← if entered then do
      let frontiers ← call.advance_frontier ops w.frontiers area
      ok { w with frontiers := frontiers }
    else ok w
  let queues ← call.update_queue ops w1.queues area after
  let groups ← headWriteNative call heads ops w1.groups before after headId entered
  let ended ← endedNative afterPhase1
  let (bodyGroups, keep) ← bodyExitNative bodies ops w1.body_groups bodyId noteId ended
  let active ← if keep then call.append_active ops w1.active area after else ok w1.active
  let events ← match event with
    | none => ok w1.events
    | some event => call.append_event ops w1.events event
  ok { w1 with queues := queues, groups := groups, body_groups := bodyGroups,
    active := active, events := events }

def itemNative (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (area : A) (before : N) : Result (touch_hold_schedule.TouchHoldWork F Q C H E G B) := do
  let sensorDiff ← call.sensor_diff ops before
  let localPressed ← call.local_pressed ops area
  let bodyId ← call.body_group_id ops before
  let noteId ← call.note_index ops before
  let bodyActive ← call.body_check_active ops before
  let (w1, local1, bodyId1) ← voteNative bodies ops w localPressed bodyId noteId bodyActive
  let (w2, bodyId2, pressed) ← pressureNative bodies ops w1 local1 bodyId1
  let headId ← call.head_group_id ops before
  let (w3, shared) ← sharedNative heads ops w2 headId
  let beforePhase ← call.phase ops before
  let (w4, beforePhase1, allow) ← admissionNative call ops w3 area before beforePhase shared
  let (w5, button, sensor) ← clicksNative call ops w4 area allow
  let diff ← if button then call.button_diff ops before else ok sensorDiff
  let clicked ← if button then ok true else ok sensor
  let (after, event) ← call.step ops before diff clicked pressed shared
  finishNative call heads bodies ops w5 area before after event bodyId2 noteId headId beforePhase1

/-- Pure expansion/reassociation, before imposing any representation contract. -/
theorem body_eq_item (holds : S) (len index : Usize)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) :
    touch_hold_schedule.process_loop.body call heads bodies holds ops len w index =
      (if index < len then do
        let (area, before) ← call.get ops holds index
        let next ← itemNative call heads bodies ops w area before
        let nextIndex ← index + 1#usize
        ok (.cont (next, nextIndex))
      else ok (.done w)) := by
  simp only [touch_hold_schedule.process_loop.body, itemNative, voteNative,
    pressureNative, sharedNative, admissionNative, clicksNative, clicksAfterOuter,
    finishNative, headWriteNative, bodyExitNative, enteredNative, awaitNative,
    judgedNative, endedNative, bind_assoc_eq, bind_tc_ok, uncurry_apply_pair,
    bind_eq_iff, Prod.forall]
  all_goals (intros; rfl)

end Native

private theorem await_phase (note : HoldNote) :
    awaitNative (phase note) = ok (holdHeadAwaitsResolution note.state) := by
  cases h : note.state <;> simp [phase, h, awaitNative, holdHeadAwaitsResolution]

private theorem entered_phase (before after : HoldNote) :
    enteredNative (phase before) (phase after) =
      ok (phase after, enteredHeadJudged before.state after.state) := by
  cases hb : before.state <;> cases ha : after.state <;>
    simp [enteredNative, judgedNative, phase, hb, ha, enteredHeadJudged,
      isHeadJudgedState, bind_tc_ok]

private theorem ended_phase (note : HoldNote) :
    endedNative (phase note) = ok (!keepHoldActive note) := by
  cases h : note.state <;> simp [endedNative, phase, h, keepHoldActive]

private theorem head_after_eq (groups : List GroupState) (before after : HoldNote) :
    headGroupsAfter groups before after =
      if enteredHeadJudged before.state after.state then
        match before.touchGroupId, headGrade after with
        | some id, some grade => if grade.isMissOrTooFast then groups
            else registerTouchGroupResult groups id before.touchGroupSize grade after.headDiff
        | _, _ => groups
      else groups := by
  cases hs : after.state <;> cases before.touchGroupId <;>
    simp only [headGroupsAfter, headGrade, hs]

section Fragments
variable (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
variable (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
variable (bodies : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
variable (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
variable (ctx : Context) (cap : Capacity)

theorem vote_native_spec (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (area : A) (before : N)
    (bodyId : Option Id) (noteId : Id)
    (hid : bodyId.map rep.id = (rep.note before).touchHoldGroupId)
    (hnote : rep.id noteId = (rep.note before).params.noteIndex)
    (hfit : BodyFits cap.bodies
      (bodyBefore ctx (rep.area area) (rep.note before) (rep.bodyGroups w.body_groups))) :
    ∃ groups,
      voteNative bodies ops w (ctx.input.getSensorHeld (rep.area area)) bodyId noteId
        (touchHoldBodyCheckActive (rep.note before) ctx.currentTime) =
          ok ({ w with body_groups := groups }, ctx.input.getSensorHeld (rep.area area), bodyId) ∧
      rep.bodyGroups groups =
        bodyBefore ctx (rep.area area) (rep.note before) (rep.bodyGroups w.body_groups) := by
  cases hc : touchHoldBodyCheckActive (rep.note before) ctx.currentTime with
  | false =>
      refine ⟨w.body_groups, ?_, ?_⟩
      · simp [voteNative, hc]
      · simp [bodyBefore, hc]
  | true =>
      cases bodyId with
      | none =>
          have hid' : (rep.note before).touchHoldGroupId = none := hid.symm
          refine ⟨w.body_groups, ?_, ?_⟩
          · simp [voteNative, hc, bind_tc_ok, uncurry_apply_pair]
          · simp [bodyBefore, hc, hid']
      | some id =>
          have hid' : (rep.note before).touchHoldGroupId = some (rep.id id) := hid.symm
          cases hl : ctx.input.getSensorHeld (rep.area area) with
          | false =>
              obtain ⟨groups, hrun, hrep⟩ := spec_imp_exists
                (unregister_body_equiv bodies ops (bodyRep rep) cap.bodies bodyAtoms
                  w.body_groups id noteId)
              refine ⟨groups, ?_, ?_⟩
              · simp [voteNative, hc, hl, hrun, bind_tc_ok, uncurry_apply_pair]
              · simpa only [bodyBefore, hc, hl, hid', Bool.true_eq, Bool.false_eq_true,
                  ↓reduceIte, bodyRep, hnote] using hrep
          | true =>
              have hcapacity : BodyFits cap.bodies (registerTouchHoldBodyTrigger
                  (rep.bodyGroups w.body_groups) (rep.id id) (rep.id noteId)) := by
                simpa [bodyBefore, hc, hl, hid', hnote] using hfit
              obtain ⟨groups, hrun, hrep⟩ := spec_imp_exists
                (register_body_equiv bodies ops (bodyRep rep) cap.bodies bodyAtoms
                  w.body_groups id noteId hcapacity)
              refine ⟨groups, ?_, ?_⟩
              · simp [voteNative, hc, hl, hrun, bind_tc_ok, uncurry_apply_pair]
              · simpa [bodyBefore, hc, hl, hid', bodyRep, hnote] using hrep

theorem pressure_native_spec (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (area : A) (before : N)
    (bodyId : Option Id) (hid : bodyId.map rep.id = (rep.note before).touchHoldGroupId) :
    pressureNative bodies ops w (ctx.input.getSensorHeld (rep.area area)) bodyId =
      ok (w, bodyId, effectivePressed ctx (rep.area area) (rep.note before)
        (rep.bodyGroups w.body_groups)) := by
  cases hl : ctx.input.getSensorHeld (rep.area area) with
  | true => simp [pressureNative, effectivePressed, hl]
  | false =>
      cases bodyId with
      | none =>
          have hid' : (rep.note before).touchHoldGroupId = none := hid.symm
          simp [pressureNative, effectivePressed, hl, hid', bind_tc_ok, uncurry_apply_pair]
      | some id =>
          have hid' : (rep.note before).touchHoldGroupId = some (rep.id id) := hid.symm
          obtain ⟨pressed, hrun, hrep⟩ := spec_imp_exists
            (body_pressed_equiv bodies ops (bodyRep rep) cap.bodies bodyAtoms w.body_groups id)
          simp only [bodyRep] at hrep
          simp [pressureNative, effectivePressed, hl, hid', hrun, hrep,
            bind_tc_ok, uncurry_apply_pair]

theorem shared_native_spec (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (before : N)
    (headId : Option Id) (hid : headId.map rep.id = (rep.note before).touchGroupId) :
    ∃ shared, sharedNative heads ops w headId = ok (w, shared) ∧
      shared.map (decodeShared rep) = sharedResult (rep.groups w.groups) (rep.note before) := by
  cases headId with
  | none =>
      have hid' : (rep.note before).touchGroupId = none := hid.symm
      exact ⟨none, rfl, by simp [sharedResult, hid']⟩
  | some id =>
      have hid' : (rep.note before).touchGroupId = some (rep.id id) := hid.symm
      obtain ⟨shared, hrun, hrep⟩ := spec_imp_exists
        (shared_result_equiv heads ops (headRep rep) cap.heads headAtoms w.groups id)
      refine ⟨shared, ?_, ?_⟩
      · simp only [sharedNative, hrun, bind_tc_ok]
      · simpa only [headRep, decodeShared, sharedResult, hid'] using hrep

theorem admission_native_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (area : A) (before : N)
    (shared : Option (Grade × D)) :
    admissionNative call ops w area before (phase (rep.note before)) shared =
      ok (w, phase (rep.note before), eligible ctx (rep.frontiers w.frontiers)
        (rep.area area) (rep.note before) (shared.map (decodeShared rep))) := by
  have hn : core.option.Option.is_none shared = (shared.map (decodeShared rep)).isNone := by
    cases shared <;> rfl
  simp only [admissionNative, hn, atoms.within_head_window, atoms.unlocked, atoms.eligible,
    await_phase, bind_tc_ok, uncurry_apply_pair, eligible]
  cases hs : (shared.map (decodeShared rep)).isNone <;>
    cases hw : decide (ctx.currentTime ≤ (rep.note before).params.effectiveTiming + touchGoodMs) <;>
    cases ha : holdHeadAwaitsResolution (rep.note before).state <;>
    cases hu : touchQueueIndexUnlocked (rep.frontiers w.frontiers) (rep.area area)
      (rep.note before).touchQueueIndex <;>
    simp [hs, hw, ha, hu, bind_tc_ok, uncurry_apply_pair]

theorem head_write_native_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (groups : G) (before after : N) (headId : Option Id)
    (hid : headId.map rep.id = (rep.note before).touchGroupId)
    (hfit : (headGroupsAfter (rep.groups groups) (rep.note before) (rep.note after)).length ≤
      cap.heads) :
    ∃ next, headWriteNative call heads ops groups before after headId
      (enteredHeadJudged (rep.note before).state (rep.note after).state) = ok next ∧
      rep.groups next = headGroupsAfter (rep.groups groups) (rep.note before) (rep.note after) := by
  cases he : enteredHeadJudged (rep.note before).state (rep.note after).state with
  | false => exact ⟨groups, by simp [headWriteNative, he], by simp [head_after_eq, he]⟩
  | true =>
      cases headId with
      | none =>
          have hid' : (rep.note before).touchGroupId = none := hid.symm
          exact ⟨groups, by simp [headWriteNative, he], by simp [head_after_eq, he, hid']⟩
      | some id =>
          have hid' : (rep.note before).touchGroupId = some (rep.id id) := hid.symm
          obtain ⟨grade, hg, hgRep⟩ := atoms.head_grade after
          cases grade with
          | none =>
              have hg' : headGrade (rep.note after) = none := hgRep.symm
              refine ⟨groups, ?_, ?_⟩
              · simp [headWriteNative, he, hg, bind_tc_ok]
              · simp [head_after_eq, he, hid', hg']
          | some grade =>
              have hg' : headGrade (rep.note after) = some (rep.grade grade) := hgRep.symm
              cases hr : (rep.grade grade).isMissOrTooFast with
              | true =>
                  refine ⟨groups, ?_, ?_⟩
                  · simp [headWriteNative, he, hg, atoms.grade_rejected, hr, bind_tc_ok]
                  · simp [head_after_eq, he, hid', hg', hr]
              | false =>
                  obtain ⟨size, hsize, hsizeRep⟩ := atoms.group_size before
                  obtain ⟨diff, hdiff, hdiffRep⟩ := atoms.head_diff after
                  have hcapacity : (registerTouchGroupResult (rep.groups groups) (rep.id id)
                      (rep.id size) (rep.grade grade) (rep.duration diff)).length ≤ cap.heads := by
                    simpa [head_after_eq, he, hid', hg', hr, hsizeRep, hdiffRep] using hfit
                  obtain ⟨next, hrun, hrep⟩ := spec_imp_exists
                    (register_head_equiv heads ops (headRep rep) cap.heads headAtoms
                      groups id size grade diff hcapacity)
                  refine ⟨next, ?_, ?_⟩
                  · simp [headWriteNative, he, hg, atoms.grade_rejected, hr, hsize, hdiff,
                      hrun, bind_tc_ok]
                  · simpa [headRep, head_after_eq, he, hid', hg', hr, hsizeRep, hdiffRep] using hrep

theorem body_exit_native_spec (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (groups : B) (before after : N) (bodyId : Option Id) (noteId : Id)
    (hid : bodyId.map rep.id = (rep.note before).touchHoldGroupId)
    (hnote : rep.id noteId = (rep.note before).params.noteIndex) :
    ∃ next, bodyExitNative bodies ops groups bodyId noteId (!keepHoldActive (rep.note after)) =
      ok (next, keepHoldActive (rep.note after)) ∧
      rep.bodyGroups next = bodyGroupsAfter (rep.bodyGroups groups) (rep.note before) (rep.note after) := by
  cases hk : keepHoldActive (rep.note after) with
  | true =>
      refine ⟨groups, by simp [bodyExitNative, hk], ?_⟩
      cases (rep.note before).touchHoldGroupId <;> simp [bodyGroupsAfter, hk]
  | false =>
      cases bodyId with
      | none =>
          have hid' : (rep.note before).touchHoldGroupId = none := hid.symm
          refine ⟨groups, ?_, ?_⟩
          · simp [bodyExitNative, hk, bind_tc_ok]
          · simp [bodyGroupsAfter, hid']
      | some id =>
          have hid' : (rep.note before).touchHoldGroupId = some (rep.id id) := hid.symm
          obtain ⟨next, hrun, hrep⟩ := spec_imp_exists
            (exit_body_equiv bodies ops (bodyRep rep) cap.bodies bodyAtoms groups id noteId)
          refine ⟨next, ?_, ?_⟩
          · simp [bodyExitNative, hk, hrun, bind_tc_ok]
          · simpa [bodyRep, bodyGroupsAfter, hid', hk, hnote] using hrep

end Fragments

end Verification.TouchHoldScheduleProof
