import Verification.TouchHoldSchedule

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

set_option maxHeartbeats 1200000

namespace Verification.TouchHoldScheduleTraversalProof
open TouchHoldScheduleModel TouchHoldScheduleProof TouchHoldGroupsProof
open private tryUseButtonClickAt tryUseSensorClickAt enteredHeadJudged
  isHeadJudgedState keepHoldActive advanceSharedTouchQueue updateSensorHoldQueue
  from LnmaiCore.Scheduler

/-!
Continuation of TouchHoldSchedule: the missing click/finish assembly, followed
by the actual Generated body, decreasing loop, wrapper and original pass output.
The old model and fragments are imported unchanged. No whole-item or whole-pass
agreement premise is introduced. Safe contains only the existing per-prefix
capacity bounds, including the body container before the lifecycle can shrink it.

Remaining boundaries are AtomicAgrees (notably one-note holdStep and the queue
replace-all/normalize composition), HeadAtoms/BodyAtoms, representations and
initialization. The concrete Rust facade remains unextracted. This source has
not been compiled here; validation belongs to the mainline worker.
-/

variable {S A N F Q C H E Event D Z Id Grade G B Ops : Type}

/-- Pure click adapter for an already selected optional outer button. -/
private def consumeOuter (ctx : Context) (area : SensorArea) (allow : Bool)
    (outer : Option ButtonZone) (cursor : Scheduler.ClickCursor) :
    Bool × Bool × Scheduler.ClickCursor :=
  let (button, cursor1) := if allow then
      match outer with
      | none => (false, cursor)
      | some zone => tryUseButtonClickAt ctx.input cursor zone
    else (false, cursor)
  let (sensor, cursor2) := useSensor ctx area allow button cursor1
  (button, sensor, cursor2)

private theorem consume_outer_eq (ctx : Context) (area : SensorArea)
    (allow : Bool) (cursor : Scheduler.ClickCursor) :
    consumeOuter ctx area allow
      (if allow && ctx.buttonRingForTouch then area.toOuterButtonZone? else none) cursor =
        consume ctx area allow cursor := by
  cases allow <;> cases hr : ctx.buttonRingForTouch <;>
    simp [consumeOuter, consume, useButton, hr]

section Assembly
variable (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
variable (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
variable (bodies : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
variable (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
variable (ctx : Context) (cap : Capacity)

/-- The real consume_click table, including button suppression of sensor reads
and writes. Availability uses the current cursor of this item. -/
private theorem clicks_after_outer_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (area : A)
    (allow : Bool) (outer : Option Z) :
    ∃ button sensor cursor,
      clicksAfterOuter call ops w area allow outer =
        ok ({ w with cursor := cursor }, button, sensor) ∧
      (button, sensor, rep.cursor cursor) =
        consumeOuter ctx (rep.area area) allow (outer.map rep.zone) (rep.cursor w.cursor) := by
  cases allow with
  | false =>
      refine ⟨false, false, w.cursor, ?_, ?_⟩
      · cases outer <;>
          simp [clicksAfterOuter, atoms.button_available, scheduler.consume_click,
            bind_tc_ok, uncurry_apply_pair]
      · simp [consumeOuter, useSensor]
  | true =>
      cases outer with
      | none =>
          by_cases hs : (rep.cursor w.cursor).sensorUsed.getD (rep.area area) 0 <
              ctx.input.getSensorClickCount (rep.area area)
          · obtain ⟨cursor, huse, hrep⟩ := atoms.use_sensor w.cursor area
            refine ⟨false, true, cursor, ?_, ?_⟩
            · simp [clicksAfterOuter, atoms.sensor_available, hs, scheduler.consume_click,
                huse, bind_tc_ok, uncurry_apply_pair]
            · simp [consumeOuter, useSensor, tryUseSensorClickAt, hs, hrep, spendSensor]
          · refine ⟨false, false, w.cursor, ?_, ?_⟩
            · simp [clicksAfterOuter, atoms.sensor_available, hs, scheduler.consume_click,
                bind_tc_ok, uncurry_apply_pair]
            · simp [consumeOuter, useSensor, tryUseSensorClickAt, hs]
      | some zone =>
          by_cases hb : (rep.cursor w.cursor).buttonUsed.getD (rep.zone zone) 0 <
              ctx.input.getButtonClickCount (rep.zone zone)
          · obtain ⟨cursor, huse, hrep⟩ := atoms.use_button w.cursor zone
            refine ⟨true, false, cursor, ?_, ?_⟩
            · simp [clicksAfterOuter, atoms.button_available, hb, scheduler.consume_click,
                huse, bind_tc_ok, uncurry_apply_pair]
            · simp [consumeOuter, useSensor, tryUseButtonClickAt, hb, hrep, spendButton]
          · by_cases hs : (rep.cursor w.cursor).sensorUsed.getD (rep.area area) 0 <
                ctx.input.getSensorClickCount (rep.area area)
            · obtain ⟨cursor, huse, hrep⟩ := atoms.use_sensor w.cursor area
              refine ⟨false, true, cursor, ?_, ?_⟩
              · simp [clicksAfterOuter, atoms.button_available, atoms.sensor_available,
                  hb, hs, scheduler.consume_click, huse, bind_tc_ok, uncurry_apply_pair]
              · simp [consumeOuter, useSensor, tryUseButtonClickAt, tryUseSensorClickAt,
                  hb, hs, hrep, spendSensor]
            · refine ⟨false, false, w.cursor, ?_, ?_⟩
              · simp [clicksAfterOuter, atoms.button_available, atoms.sensor_available,
                  hb, hs, scheduler.consume_click, bind_tc_ok, uncurry_apply_pair]
              · simp [consumeOuter, useSensor, tryUseButtonClickAt, tryUseSensorClickAt, hb, hs]

theorem clicks_native_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (area : A) (allow : Bool) :
    ∃ button sensor cursor,
      clicksNative call ops w area allow = ok ({ w with cursor := cursor }, button, sensor) ∧
      (button, sensor, rep.cursor cursor) =
        consume ctx (rep.area area) allow (rep.cursor w.cursor) := by
  obtain ⟨outer, houter, houterRep⟩ := atoms.outer_button area
  let selected := if allow && ctx.buttonRingForTouch then outer else none
  have hselect : (if allow then do
      let ring ← call.button_ring ops
      if ring then call.outer_button ops area else ok none
    else ok none) = (ok selected : Result (Option Z)) := by
    cases allow <;> cases hr : ctx.buttonRingForTouch <;>
      simp [selected, atoms.button_ring, hr, houter, bind_tc_ok]
  have hselected : selected.map rep.zone =
      if allow && ctx.buttonRingForTouch then (rep.area area).toOuterButtonZone? else none := by
    unfold selected
    split <;> simp only [Option.map_none, houterRep]
  obtain ⟨button, sensor, cursor, hrun, hrep⟩ :=
    clicks_after_outer_spec call ops rep ctx cap atoms w area allow selected
  refine ⟨button, sensor, cursor, ?_, ?_⟩
  · simp only [clicksNative, hselect, bind_tc_ok, hrun]
  · rw [hselected] at hrep
    exact hrep.trans (consume_outer_eq ctx (rep.area area) allow (rep.cursor w.cursor))

private theorem entered_phase (before after : HoldNote) :
    enteredNative (phase before) (phase after) =
      ok (phase after, enteredHeadJudged before.state after.state) := by
  cases hb : before.state <;> cases ha : after.state <;>
    simp [enteredNative, judgedNative, phase, hb, ha, enteredHeadJudged,
      isHeadJudgedState, bind_tc_ok]

private theorem ended_phase (note : HoldNote) :
    endedNative (phase note) = ok (!keepHoldActive note) := by
  cases h : note.state <;> simp [endedNative, phase, h, keepHoldActive]

/-- Queue synchronization always occurs. Head registration precedes body exit;
active and optional event are appended in original traversal order. -/
theorem finish_native_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (area : A) (before after : N) (event : Option Event)
    (bodyId : Option Id) (noteId : Id) (headId : Option Id)
    (hbody : bodyId.map rep.id = (rep.note before).touchHoldGroupId)
    (hnote : rep.id noteId = (rep.note before).params.noteIndex)
    (hhead : headId.map rep.id = (rep.note before).touchGroupId)
    (hfit : Fits cap (advance (interpret rep w) (rep.area area) (rep.note before)
      (rep.note after) (event.map rep.event) (rep.cursor w.cursor) (rep.bodyGroups w.body_groups))) :
    ∃ next, finishNative call heads bodies ops w area before after event bodyId noteId headId
      (phase (rep.note before)) = ok next ∧
      interpret rep next = advance (interpret rep w) (rep.area area) (rep.note before)
        (rep.note after) (event.map rep.event) (rep.cursor w.cursor) (rep.bodyGroups w.body_groups) := by
  obtain ⟨advanced, ha, haRep⟩ := atoms.advance_frontier w.frontiers area
  let entered := enteredHeadJudged (rep.note before).state (rep.note after).state
  let frontiers := if entered then advanced else w.frontiers
  have hfront : (if entered then do
      let next ← call.advance_frontier ops w.frontiers area
      ok { w with frontiers := next }
    else ok w) = (ok { w with frontiers := frontiers } :
      Result (touch_hold_schedule.TouchHoldWork F Q C H E G B)) := by
    cases he : entered <;> simp [frontiers, he, ha, bind_tc_ok]
  have hfrontRep : rep.frontiers frontiers =
      if entered then advanceSharedTouchQueue (rep.frontiers w.frontiers) (rep.area area)
      else rep.frontiers w.frontiers := by
    unfold frontiers
    split <;> simp only [haRep]
  obtain ⟨queues, hq, hqRep⟩ := atoms.update_queue w.queues area after
  obtain ⟨groups, hg, hgRep⟩ :=
    head_write_native_spec (call := call) (heads := heads) (ops := ops) (rep := rep)
      (ctx := ctx) (cap := cap) atoms headAtoms w.groups before after headId hhead hfit.2.2
  obtain ⟨bodyGroups, hb, hbRep⟩ :=
    body_exit_native_spec (bodies := bodies) (ops := ops) (rep := rep) (cap := cap)
      bodyAtoms w.body_groups before after bodyId noteId hbody hnote
  have hactive : ∃ active,
      (if keepHoldActive (rep.note after) then call.append_active ops w.active area after
        else ok w.active) = ok active ∧
      rep.active active = rep.active w.active ++ retained (rep.area area) (rep.note after) := by
    cases hk : keepHoldActive (rep.note after) with
    | false => exact ⟨w.active, by simp [hk], by simp [retained, hk]⟩
    | true =>
        obtain ⟨active, hrun, hrep⟩ := atoms.append_active w.active area after (by
          simpa [advance, interpret, retained, hk] using hfit.1)
        exact ⟨active, by simpa [hk] using hrun, by simpa [retained, hk] using hrep⟩
  obtain ⟨active, hactive, hactiveRep⟩ := hactive
  have hevents : ∃ events,
      (match event with
        | none => ok w.events
        | some event => call.append_event ops w.events event) = ok events ∧
      rep.events events = rep.events w.events ++ (event.map rep.event).toList := by
    cases event with
    | none => exact ⟨w.events, rfl, by simp⟩
    | some event =>
        obtain ⟨events, hrun, hrep⟩ := atoms.append_event w.events event (by
          simpa [advance, interpret] using hfit.2.1)
        exact ⟨events, hrun, by simpa using hrep⟩
  obtain ⟨events, hevents, heventsRep⟩ := hevents
  let next : touch_hold_schedule.TouchHoldWork F Q C H E G B :=
    { w with frontiers := frontiers, queues := queues, groups := groups,
      body_groups := bodyGroups, active := active, events := events }
  refine ⟨next, ?_, ?_⟩
  · simp only [finishNative, atoms.phase, entered_phase, hfront, hq, hg, ended_phase,
      hb, hactive, hevents, bind_tc_ok, uncurry_apply_pair, next]
  · simp only [interpret, next, advance, hfrontRep, hqRep, hgRep, hbRep,
      hactiveRep, heventsRep, entered]

private theorem or_ok (button sensor : Bool) :
    (if button then ok true else ok sensor) = (ok (button || sensor) : Result Bool) := by
  cases button <;> rfl

/-- The actual item uses this item's updated body votes and current head
groups. Only the one-note lifecycle and queue composition remain atomic. -/
theorem item_native_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (area : A) (before : N)
    (hfit : StepFits ctx cap (interpret rep w) (rep.area area) (rep.note before)) :
    ∃ next, itemNative call heads bodies ops w area before = ok next ∧
      interpret rep next = step ctx (interpret rep w) (rep.area area) (rep.note before) := by
  obtain ⟨sensorDiff, hsensorDiff, hsensorRep⟩ := atoms.sensor_diff before
  obtain ⟨bodyId, hbodyId, hbodyIdRep⟩ := atoms.body_group_id before
  obtain ⟨noteId, hnoteId, hnoteIdRep⟩ := atoms.note_index before
  obtain ⟨headId, hheadId, hheadIdRep⟩ := atoms.head_group_id before
  obtain ⟨bodyGroups, hvote, hbodyRep⟩ :=
    vote_native_spec (bodies := bodies) (ops := ops) (rep := rep) (ctx := ctx)
      (cap := cap) bodyAtoms w area before bodyId noteId hbodyIdRep hnoteIdRep hfit.1
  let w1 : touch_hold_schedule.TouchHoldWork F Q C H E G B :=
    { w with body_groups := bodyGroups }
  let pressed := effectivePressed ctx (rep.area area) (rep.note before) (rep.bodyGroups bodyGroups)
  have hpressure : pressureNative bodies ops w1 (ctx.input.getSensorHeld (rep.area area))
      bodyId = ok (w1, bodyId, pressed) :=
    pressure_native_spec (bodies := bodies) (ops := ops) (rep := rep) (ctx := ctx)
      (cap := cap) bodyAtoms w1 area before bodyId hbodyIdRep
  obtain ⟨shared, hshared, hsharedRep⟩ :=
    shared_native_spec (heads := heads) (ops := ops) (rep := rep) (cap := cap)
      headAtoms w1 before headId hheadIdRep
  let allow := eligible ctx (rep.frontiers w.frontiers) (rep.area area) (rep.note before)
    (shared.map (decodeShared rep))
  have hadmit : admissionNative call ops w1 area before (phase (rep.note before)) shared =
      ok (w1, phase (rep.note before), allow) :=
    admission_native_spec (call := call) (ops := ops) (rep := rep) (ctx := ctx)
      (cap := cap) atoms w1 area before shared
  obtain ⟨button, sensor, cursor, hclick, hclickRep⟩ :=
    clicks_native_spec call ops rep ctx cap atoms w1 area allow
  let w2 : touch_hold_schedule.TouchHoldWork F Q C H E G B :=
    { w1 with cursor := cursor }
  have hconsume : consume ctx (rep.area area)
      (eligible ctx (rep.frontiers w.frontiers) (rep.area area) (rep.note before)
        (sharedResult (rep.groups w.groups) (rep.note before))) (rep.cursor w.cursor) =
      (button, sensor, rep.cursor cursor) := by
    simpa only [allow, w1, hsharedRep] using hclickRep.symm
  obtain ⟨buttonDiff, hbuttonDiff, hbuttonRep⟩ := atoms.button_diff before
  let diff := if button then buttonDiff else sensorDiff
  have hdiff : (if button then call.button_diff ops before else ok sensorDiff) =
      (ok diff : Result D) := by
    cases button <;> simp [diff, hbuttonDiff]
  have hdiffRep : rep.duration diff = difference ctx (rep.note before) button := by
    cases button <;> simp [diff, difference, hbuttonRep, hsensorRep]
  obtain ⟨after, event, hstep, hstepRep⟩ :=
    atoms.step before diff (button || sensor) pressed shared
  have hj : judgment ctx (rep.note before) button sensor
      (effectivePressed ctx (rep.area area) (rep.note before)
        (bodyBefore ctx (rep.area area) (rep.note before) (rep.bodyGroups w.body_groups)))
      (sharedResult (rep.groups w.groups) (rep.note before)) =
        (rep.note after, event.map rep.event) := by
    simpa only [judgment, hdiffRep, pressed, hbodyRep, hsharedRep, w1] using hstepRep.symm
  have hmodel : step ctx (interpret rep w) (rep.area area) (rep.note before) =
      advance (interpret rep w2) (rep.area area) (rep.note before) (rep.note after)
        (event.map rep.event) (rep.cursor w2.cursor) (rep.bodyGroups w2.body_groups) := by
    simp only [step, interpret, hconsume, hj]
    simp only [w2, w1, advance, interpret, hbodyRep]
  obtain ⟨next, hfinish, hnext⟩ :=
    finish_native_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms
      w2 area before after event bodyId noteId headId hbodyIdRep hnoteIdRep hheadIdRep (by
        rw [← hmodel]
        exact hfit.2)
  refine ⟨next, ?_, hnext.trans hmodel.symm⟩
  simp only [itemNative, hsensorDiff, atoms.local_pressed, hbodyId, hnoteId,
    atoms.body_check_active, hvote, hpressure, hheadId, hshared, atoms.phase, hadmit,
    hclick, hdiff, or_ok, hstep, hfinish, bind_tc_ok, uncurry_apply_pair]

end Assembly

/-- index < len implies a safe increment even when len is usize::MAX. -/
private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

section Traversal
variable (call : touch_hold_schedule.TouchHoldSchedule Ops S A N F Q C H E Event D Z Id Grade)
variable (heads : touch_hold_schedule.HeadGroups Ops G Id Grade D)
variable (bodies : touch_hold_schedule.BodyGroups Ops B Id) (ops : Ops)
variable (rep : Representation S A N F Q C H E Event D Z Id Grade G B)
variable (ctx : Context) (cap : Capacity)

/-- A real generated body step reads exactly the original source slot and
increments its index once, irrespective of whether the note emits an event. -/
theorem process_body_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (len index : Usize) (w : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (pre : List (SensorArea × HoldNote)) (area : SensorArea) (note : HoldNote)
    (rest : List (SensorArea × HoldNote))
    (hholds : rep.holds holds = pre ++ (area, note) :: rest)
    (hindex : pre.length = index.val) (hmore : index.val < len.val)
    (hfit : StepFits ctx cap (interpret rep w) area note) :
    ∃ next nextIndex,
      touch_hold_schedule.process_loop.body call heads bodies holds ops len w index =
        ok (.cont (next, nextIndex)) ∧
      interpret rep next = step ctx (interpret rep w) area note ∧
      nextIndex.val = index.val + 1 := by
  obtain ⟨nativeArea, before, hget, harea, hbefore⟩ :=
    atoms.get holds index pre area note rest hholds hindex
  obtain ⟨next, hitem, hnext⟩ :=
    item_native_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms
      w nativeArea before (by simpa only [harea, hbefore] using hfit)
  obtain ⟨nextIndex, hi, hiVal⟩ := increment_index index len hmore
  refine ⟨next, nextIndex, ?_, ?_, hiVal⟩
  · have hguard : index < len := by simpa using hmore
    rw [body_eq_item]
    simp only [hguard, ↓reduceIte, hget, hitem, hi, bind_tc_ok, uncurry_apply_pair]
  · simpa only [harea, hbefore] using hnext

/-- Stopping performs no trait call and leaves all seven work fields intact. -/
theorem process_body_done (holds : S) (len index : Usize)
    (w : touch_hold_schedule.TouchHoldWork F Q C H E G B) (hstop : len.val ≤ index.val) :
    touch_hold_schedule.process_loop.body call heads bodies holds ops len w index =
      ok (.done w) := by
  have hguard : ¬ index < len := by simpa using Nat.not_lt.mpr hstop
  rw [body_eq_item]
  simp only [hguard, ↓reduceIte]

/-- Total finite execution of the actual extracted loop. The measure len-index
strictly decreases; its source handle never changes. Safe supplies capacities
at every remaining prefix, including intermediate body-group growth. -/
theorem process_loop_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (len : Usize) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (start : Usize) (hlen : (rep.holds holds).length = len.val)
    (hstart : start.val ≤ len.val)
    (hsafe : Safe ctx cap ((rep.holds holds).drop start.val) (interpret rep initial)) :
    touch_hold_schedule.process_loop call heads bodies holds initial ops len start ⦃ out =>
      interpret rep out = resume ctx ((rep.holds holds).drop start.val) (interpret rep initial) ⦄ := by
  unfold touch_hold_schedule.process_loop
  apply loop.spec_decr_nat
    (fun state : touch_hold_schedule.TouchHoldWork F Q C H E G B × Usize => len.val - state.2.val)
    (fun state => start.val ≤ state.2.val ∧ state.2.val ≤ len.val ∧
      Safe ctx cap ((rep.holds holds).drop state.2.val) (interpret rep state.1) ∧
      resume ctx ((rep.holds holds).drop state.2.val) (interpret rep state.1) =
        resume ctx ((rep.holds holds).drop start.val) (interpret rep initial))
  · rintro ⟨w, index⟩ ⟨hlow, hupp, hsafeNow, hresume⟩
    dsimp only at hlow hupp hsafeNow hresume ⊢
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
      have hready : StepFits ctx cap (interpret rep w) item.1 item.2 ∧
          Safe ctx cap rest (step ctx (interpret rep w) item.1 item.2) := by
        rw [hpending] at hsafeNow
        change Safe ctx cap ((item.1, item.2) :: rest) (interpret rep w) at hsafeNow
        exact hsafeNow
      obtain ⟨next, nextIndex, hbody, hnextRep, hnextIndex⟩ :=
        process_body_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms
          holds len index w pre item.1 item.2 rest hsplit hpre hmore hready.1
      have hsafeNext : Safe ctx cap ((rep.holds holds).drop nextIndex.val)
          (interpret rep next) := by
        rw [hnextIndex, hnextRep]
        exact hready.2
      have hnextResume : resume ctx ((rep.holds holds).drop nextIndex.val) (interpret rep next) =
          resume ctx ((rep.holds holds).drop start.val) (interpret rep initial) := by
        rw [hnextIndex, hnextRep]
        calc
          resume ctx ((rep.holds holds).drop (index.val + 1))
              (step ctx (interpret rep w) item.1 item.2) =
              resume ctx ((rep.holds holds).drop index.val) (interpret rep w) :=
            (resume_drop_step ctx (rep.holds holds) index.val (interpret rep w) hslot).symm
          _ = _ := hresume
      apply exists_imp_spec
      refine ⟨.cont (next, nextIndex), hbody, ?_⟩
      dsimp only
      exact ⟨⟨by omega, by omega, hsafeNext, hnextResume⟩, by omega⟩
    · have hstop : len.val ≤ index.val := by simpa using hguard
      have hdone := resume_drop_done ctx (rep.holds holds) index.val (interpret rep w) (by omega)
      apply exists_imp_spec
      exact ⟨.done w, process_body_done call heads bodies ops holds len index w hstop,
        hdone.symm.trans hresume⟩
  · dsimp only
    exact ⟨Nat.le_refl _, hstart, hsafe, rfl⟩

/-- Real wrapper: read the immutable source length, then start the loop at zero. -/
theorem process_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (hsafe : Safe ctx cap (rep.holds holds) (interpret rep initial)) :
    touch_hold_schedule.process call heads bodies holds initial ops ⦃ out =>
      interpret rep out = resume ctx (rep.holds holds) (interpret rep initial) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.length holds
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (process_loop_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms
      holds len initial 0#usize hlen (by simp) (by simpa using hsafe))
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simp only [touch_hold_schedule.process, hlength, hrun, bind_tc_ok, uncurry_apply_pair]
  · simpa using hout

/-- Existence of an ok result rules out divergence/failure under these contracts. -/
theorem process_terminates (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (hsafe : Safe ctx cap (rep.holds holds) (interpret rep initial)) :
    ∃ out, touch_hold_schedule.process call heads bodies holds initial ops = ok out := by
  obtain ⟨out, hrun, _⟩ := spec_imp_exists
    (process_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms holds initial hsafe)
  exact ⟨out, hrun⟩

/-- The complete result is the finite original-list fold, including arbitrary
existing output prefixes and both group lists threaded between successive notes. -/
theorem process_model_equiv (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (hsafe : Safe ctx cap (rep.holds holds) (interpret rep initial)) :
    touch_hold_schedule.process call heads bodies holds initial ops ⦃ out =>
      interpret rep out = traverse ctx (rep.holds holds) (interpret rep initial) ⦄ := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (process_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms holds initial hsafe)
  apply exists_imp_spec
  exact ⟨out, hrun, hout.trans (traverse_eq_resume ctx _ _).symm⟩

/-- Runtime empty-buffer initialization gives all seven ORIGINAL pass outputs:
frontiers, queues, active, events, cursor, head groups, body groups, in that order.
This concerns extracted kernels; the concrete Rust facade is still a boundary. -/
theorem process_equiv (atoms : AtomicAgrees call ops rep ctx cap)
    (headAtoms : HeadAtoms heads ops (headRep rep) cap.heads)
    (bodyAtoms : BodyAtoms bodies ops (bodyRep rep) cap.bodies)
    (holds : S) (initial : touch_hold_schedule.TouchHoldWork F Q C H E G B)
    (frontiers : SensorVec Nat) (queues : SensorQueueVec HoldNote)
    (cursor : Scheduler.ClickCursor) (groups : List GroupState)
    (bodyGroups : List TouchHoldBodyGroupState)
    (hinit : interpret rep initial = ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩)
    (hsafe : Safe ctx cap (rep.holds holds) ⟨frontiers, queues, cursor, [], [], groups, bodyGroups⟩) :
    touch_hold_schedule.process call heads bodies holds initial ops ⦃ out =>
      (interpret rep out).output =
        reference ctx (rep.holds holds) frontiers queues cursor groups bodyGroups ⦄ := by
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (process_spec call heads bodies ops rep ctx cap atoms headAtoms bodyAtoms holds initial
      (by simpa only [hinit] using hsafe))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  rw [hout, hinit, resume_original]

end Traversal

end Verification.TouchHoldScheduleTraversalProof
