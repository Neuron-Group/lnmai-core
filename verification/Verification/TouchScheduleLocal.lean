import Verification.TouchScheduleJudgments

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 1600000

namespace Verification.TouchScheduleLocalProof
open TouchScheduleModel TouchScheduleProof
open QueueMaintenanceProof (Fits CursorAtomicAgrees ReplaceAtomicAgrees)
open private normalizeTouchQueueCursor replaceTouchQueueNote touchQueueResolved
  touchClickCanEngageJudge touchQueueIndexUnlocked advanceSharedTouchQueue
  tryUseButtonClickAt tryUseSensorClickAt hasUnusedTouchInputAt
  groupShareResult registerTouchGroupResult registerTouchGroupResult.loop
  processTouchQueueHead from LnmaiCore.Scheduler

/-! Capacity-sensitive companion of TouchSchedule. It executes the identical
Generated body, queue maintenance and note lifecycle. Registration succeeds
only when its reference output fits; the caller supplies the bound for this
head invocation, not room for every represented group and every absent ID.
No compiler or test has been run on this new module. -/

theorem register_length_mono (gs : List GroupState) (id size : Nat)
    (grade : JudgeGrade) (diff : Duration) :
    gs.length ≤ (registerTouchGroupResult gs id size grade diff).length := by
  induction gs with
  | nil => simp [registerTouchGroupResult, registerTouchGroupResult.loop]
  | cons g gs ih =>
      have heq : registerTouchGroupResult (g :: gs) id size grade diff =
          (if g.groupId == id then TouchHoldGroupsProof.headAfter g size grade diff :: gs
           else g :: registerTouchGroupResult gs id size grade diff) := rfl
      rw [heq]
      split
      · simp only [List.length_cons, Nat.le_refl]
      · exact Nat.succ_le_succ ih

theorem update_groups_length_mono (gs : List GroupState) (before : TouchNote)
    (event : Option JudgeEvent) (clicked : Bool) (diff : Duration) :
    gs.length ≤ (updateGroups gs before event clicked diff).length := by
  cases event with
  | none => exact Nat.le_refl _
  | some event =>
      unfold updateGroups
      by_cases hcond : (!clicked || event.grade.isMissOrTooFast) = true
      · simp only [hcond, ↓reduceIte, Nat.le_refl]
      · have hfalse : (!clicked || event.grade.isMissOrTooFast) = false := by
          cases h : (!clicked || event.grade.isMissOrTooFast) <;> simp_all
        simp only [hfalse, Bool.false_eq_true, ↓reduceIte]
        cases hid : before.touchGroupId with
        | none => exact Nat.le_refl _
        | some id =>
            exact register_length_mono gs id before.touchGroupSize event.grade diff

theorem resume_groups_growth (ctx : Context) (fuel : Nat) (w : Work) :
    w.groups.length ≤ (resume ctx fuel w).2.2.2.1.length := by
  induction fuel generalizing w with
  | zero =>
      rw [resume_zero]
      exact Nat.le_refl _
  | succ fuel ih =>
      cases hpeek : (normalize w).queue.peek with
      | none =>
          simp only [resume_empty ctx _ w hpeek, Work.output, TouchScheduleModel.normalize]
          exact Nat.le_refl _
      | some note =>
          rw [resume_step ctx fuel w note hpeek]
          rcases hc : consume ctx note
              (eligible ctx (normalize w).frontiers (normalize w).groups note)
              (normalize w).cursor with ⟨button, sensor, cursor⟩
          simp only [hc]
          rcases hj : judgment ctx (normalize w).groups note button sensor with ⟨after, event⟩
          simp only [hj]
          have hg := update_groups_length_mono (normalize w).groups note event
            (button || sensor) (difference ctx note button)
          split
          · exact Nat.le_trans hg
              (ih (advance ctx (normalize w) note after event cursor (button || sensor)
                (difference ctx note button)))
          · exact hg

variable {Q N F C G E Event T D Area Zone Id Grade M Ops : Type}

structure GroupsWithin
    (call : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
      (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
      (Id := Id) (Grade := Grade) (M := M)) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat) : Prop where
  shared_result : ∀ gs id, ∃ shared, call.shared_result ops gs id = ok shared ∧
    sharedRep rep shared = groupShareResult (rep.groups gs) (rep.id id)
  register_group : ∀ gs id size grade diff,
    (registerTouchGroupResult (rep.groups gs) (rep.id id) (rep.id size)
      (rep.grade grade) (rep.duration diff)).length ≤ capacity →
    ∃ out, call.register_group ops gs id size grade diff = ok out ∧
      rep.groups out = registerTouchGroupResult (rep.groups gs) (rep.id id)
        (rep.id size) (rep.grade grade) (rep.duration diff)

variable {groupCapacity : Nat}
  (call : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
    (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
    (Id := Id) (Grade := Grade) (M := M))
  (cursorCall : queue_maintenance.QueueCursor M Q N)
  (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
  (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
  (ctx : Context) (cap : Capacity)
  (points : PointAgrees call cursorCall replaceCall ops rep ctx cap)
  (groups : GroupsWithin call ops rep groupCapacity)
  (life : LifecycleAgrees call ops rep ctx)

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

include points groups in
theorem register_vote_spec (gs : G) (before : N) (id : Option Id)
    (event : Event) (clicked : Bool) (diff : D)
    (hid : id.map rep.id = (rep.note before).touchGroupId)
    (hgfit : (updateGroups (rep.groups gs) (rep.note before)
      (some (rep.event event)) clicked (rep.duration diff)).length ≤ groupCapacity) :
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
                (by simpa [updateGroups, hrejected, ← hid, hsRep, hgRep] using hgfit)
              refine ⟨out, ?_, ?_⟩
              · simp [registerVote, hg, points.is_miss_or_too_fast, hgRep,
                  hrejected, hs, ho]
              · simpa [updateGroups, hrejected, ← hid, hsRep, hgRep] using hoRep

include points groups in
theorem record_event_spec
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (before : N) (p : Prepared N C Event D Area Id)
    (hid : p.groupId.map rep.id = (rep.note before).touchGroupId)
    (hefit : (advance ctx (interpret rep w) (rep.note before) (rep.note p.after)
      (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)).eventsRev.length
        ≤ cap.events)
    (hgfit : (updateGroups (rep.groups w.groups) (rep.note before)
      (p.event.map rep.event) p.clicked (rep.duration p.diff)).length ≤ groupCapacity) :
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
        rep ctx cap points groups w.groups before p.groupId event p.clicked p.diff hid (by simpa only [hevent, Option.map_some] using hgfit)
      refine ⟨gs, es, ?_, ?_, ?_⟩
      · simp only [recordEvent, hevent, he, hg, bind_tc_ok]
      · simpa only [hevent, Option.map_some] using hgRep
      · simpa [hevent] using heRep

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
        ≤ cap.events)
    (hgfit : (updateGroups (rep.groups w.groups) (rep.note before)
      (p.event.map rep.event) p.clicked (rep.duration p.diff)).length ≤ groupCapacity) :
    ∃ result, finish call cursorCall replaceCall ops m fuel w before p = ok result ∧
      interpretFlow rep result =
        (let next := advance ctx (interpret rep w) (rep.note before) (rep.note p.after)
          (p.event.map rep.event) (rep.cursor p.cursor) p.clicked (rep.duration p.diff)
        if ended (rep.note p.after) && unused ctx (rep.note before) (rep.cursor p.cursor)
        then .cont (fuel.val - 1, next) else .done next.output) ∧
      FlowFits rep cap.queue result := by
  obtain ⟨gs, es, he, hgRep, heRep⟩ := record_event_spec call cursorCall replaceCall
    ops rep ctx cap points groups w before p hid hefit hgfit
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
theorem body_spec (fuel : Usize)
    (w : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hqfit : Fits cap.queue (rep.queue w.queue))
    (hefit : (resume ctx fuel.val (interpret rep w)).2.2.2.2.length ≤ cap.events)
    (hgfit : (resume ctx fuel.val (interpret rep w)).2.2.2.1.length ≤ groupCapacity) :
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
        have hnextGroupFit : next.groups.length ≤ groupCapacity := by
          rw [href] at hgfit
          split at hgfit
          · exact Nat.le_trans (resume_groups_growth ctx (fuel.val - 1) next) hgfit
          · exact hgfit
        obtain ⟨result, hfinish, hresult, hresultFit⟩ :=
          finish_spec call cursorCall replaceCall ops rep ctx cap points groups m
            cpoints rpoints fuel hpositive base before p hid harea hqFit hnextFit hnextGroupFit
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
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events)
    (hgfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.1.length ≤ groupCapacity) :
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
        remainingFuel work hworkFit (by rw [hinv]; exact hefit)
        (by rw [hinv]; exact hgfit))
    apply exists_imp_spec
    refine ⟨out, hrun, ?_⟩
    cases out with
    | cont state => exact ⟨⟨hpost.1, hpost.2.1.trans hinv⟩, hpost.2.2⟩
    | done result => exact ⟨hpost.1, hpost.2.trans hinv⟩
  · exact ⟨hqfit, rfl⟩

include points groups life in
theorem process_equiv (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hqfit : Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events)
    (hgfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.1.length ≤ groupCapacity) :
    touch_schedule.process call cursorCall replaceCall fuel initial ops ⦃ out =>
      Fits cap.queue (rep.queue out.queue) ∧
      (interpret rep out).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (process_loop_spec call cursorCall replaceCall ops rep ctx cap points groups life
      fuel initial hqfit hefit hgfit)
  rcases out with ⟨queue, frontiers, cursor, gs, es⟩
  apply exists_imp_spec
  refine ⟨⟨queue, frontiers, cursor, gs, es⟩, ?_, hpost⟩
  simp only [touch_schedule.process, hrun, bind_tc_ok] <;> rfl

include points groups life in
theorem head_process_equiv (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (G := G) (E := E))
    (hfuel : fuel.val = remaining (rep.queue initial.queue) + 1)
    (hqfit : Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events)
    (hgfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.1.length ≤ groupCapacity) :
    touch_schedule.process call cursorCall replaceCall fuel initial ops ⦃ out =>
      Fits cap.queue (rep.queue out.queue) ∧
      (interpret rep out).output = processTouchQueueHead ctx.area
        (rep.queue initial.queue) (rep.frontiers initial.frontiers) ctx.input ctx.currentTime
        ctx.style (rep.cursor initial.cursor) ctx.touchPanelOffset (rep.groups initial.groups)
        ctx.buttonRingForTouch (rep.events initial.events_rev) ⦄ := by
  obtain ⟨out, hrun, hfit, href⟩ := spec_imp_exists
    (process_equiv call cursorCall replaceCall ops rep ctx cap points groups life
      fuel initial hqfit hefit hgfit)
  apply exists_imp_spec
  refine ⟨out, hrun, hfit, ?_⟩
  simpa only [resume, interpret, hfuel, processTouchQueueHead,
    remaining, QueueMaintenanceModel.remaining] using href

/-- Bounded registration is proved by the real first-match group kernel.
No GroupRoom or unbounded registration callback is used. -/
theorem groups_within_installed {I J : Type}
    (inst : judgment_adapter.IntegerOps J I) (integerOps : J)
    (decode : I → Int) (encode : Int → I)
    (base : Call (Q := Q) (N := N) (F := F) (C := C) (G := G) (E := E)
      (Event := Event) (T := T) (D := D) (Area := Area) (Zone := Zone)
      (Id := Id) (Grade := Grade) (M := M))
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : TouchScheduleJudgmentsProof.OutputEncoding N Event) (ctx : Context)
    (capacity : Nat)
    (atoms : TouchScheduleJudgmentsProof.GroupAtoms headCall ops rep capacity) :
    GroupsWithin (TouchScheduleJudgmentsProof.install inst integerOps decode encode
      base headCall rep pack ctx) ops rep capacity := by
  have headAtoms := TouchScheduleJudgmentsProof.head_atoms headCall ops rep capacity atoms
  constructor
  · intro gs id
    obtain ⟨out, hrun, hrep⟩ := spec_imp_exists
      (TouchHoldGroupsProof.shared_result_equiv headCall ops
        (TouchScheduleJudgmentsProof.headRep rep) capacity headAtoms gs id)
    exact ⟨out, hrun, hrep⟩
  · intro gs id size grade diff hfit
    obtain ⟨out, hrun, hrep⟩ := spec_imp_exists
      (TouchHoldGroupsProof.register_head_equiv headCall ops
        (TouchScheduleJudgmentsProof.headRep rep) capacity headAtoms gs id size grade diff hfit)
    exact ⟨out, hrun, hrep⟩

end Verification.TouchScheduleLocalProof
