import Verification.TapScheduleModel

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 800000

namespace Verification.TapScheduleProof
open TapScheduleModel
open private tapEligibleForClick buttonQueueIndexUnlocked advanceSharedButtonQueue
  listSetAt consumeButtonThenFallbackSensor from LnmaiCore.Scheduler

/-!
The actual extracted Tap queue loop, related to the original Scheduler fuel
recursion. Every facade obligation is a point operation; queue traversal and
click selection are proved here. No complete queue/pass equation is assumed.
Concrete runtime field, bigint, clone/container and allocation refinement is
still required. Capacity concerns successful event buffers, not OOM traces.
-/

structure Representation (Q N F C E Event D Area : Type) where
  queue : Q → ZoneQueue TapFamilyNote
  note : N → TapFamilyNote
  frontiers : F → ButtonVec Nat
  cursor : C → Scheduler.ClickCursor
  events : E → List JudgeEvent
  event : Event → JudgeEvent
  duration : D → Duration
  area : Area → SensorArea

variable {Q N F C E Event D Area Ops : Type}

abbrev Call := tap_schedule.TapSchedule Ops Q N F C E Event D Area
abbrev RawWork := tap_schedule.TapWork Q F C E
abbrev RawOutput := Q × F × C × E

def interpret (rep : Representation Q N F C E Event D Area)
    (w : RawWork (Q := Q) (F := F) (C := C) (E := E)) : Work :=
  ⟨rep.queue w.queue, rep.frontiers w.frontiers, rep.cursor w.cursor, rep.events w.events_rev⟩

def restore (out : RawOutput (Q := Q) (F := F) (C := C) (E := E)) :
    RawWork (Q := Q) (F := F) (C := C) (E := E) :=
  ⟨out.1, out.2.1, out.2.2.1, out.2.2.2⟩

def buttonAvailable (ctx : Context) (cursor : Scheduler.ClickCursor) : Bool :=
  decide (cursor.buttonUsed.getD ctx.zone 0 < ctx.input.getButtonClickCount ctx.zone)

def sensorAvailable (ctx : Context) (cursor : Scheduler.ClickCursor) (area : SensorArea) : Bool :=
  decide (cursor.sensorUsed.getD area 0 < ctx.input.getSensorClickCount area)

def useButton (ctx : Context) (cursor : Scheduler.ClickCursor) : Scheduler.ClickCursor :=
  { cursor with buttonUsed := cursor.buttonUsed.set ctx.zone (cursor.buttonUsed.getD ctx.zone 0 + 1) }

def useSensor (cursor : Scheduler.ClickCursor) (area : SensorArea) : Scheduler.ClickCursor :=
  { cursor with sensorUsed := cursor.sensorUsed.set area (cursor.sensorUsed.getD area 0 + 1) }

structure AtomicAgrees (call : Call (Q := Q) (N := N) (F := F) (C := C)
    (E := E) (Event := Event) (D := D) (Area := Area)) (ops : Ops)
    (rep : Representation Q N F C E Event D Area) (ctx : Context) (capacity : Nat) : Prop where
  peek : ∀ q, ∃ note, call.peek ops q = ok note ∧ note.map rep.note = (rep.queue q).peek
  button_diff : ∀ n, ∃ d, call.button_diff ops n = ok d ∧
    rep.duration d = difference ctx (rep.note n) false
  sensor_diff : ∀ n, ∃ d, call.sensor_diff ops n = ok d ∧
    rep.duration d = difference ctx (rep.note n) true
  eligible : ∀ n, call.eligible ops n = ok (tapEligibleForClick (rep.note n) ctx.currentTime)
  unlocked : ∀ f n, call.unlocked ops f n =
    ok (buttonQueueIndexUnlocked (rep.frontiers f) ctx.zone (rep.note n).buttonQueueIndex)
  fallback_area : ∀ n, ∃ a, call.fallback_area ops n = ok a ∧ rep.area a = fallback (rep.note n)
  button_available : ∀ c, call.button_available ops c = ok (buttonAvailable ctx (rep.cursor c))
  sensor_available : ∀ c a,
    call.sensor_available ops c a = ok (sensorAvailable ctx (rep.cursor c) (rep.area a))
  use_button : ∀ c, ∃ out, call.use_button ops c = ok out ∧
    rep.cursor out = useButton ctx (rep.cursor c)
  use_sensor : ∀ c a, ∃ out, call.use_sensor ops c a = ok out ∧
    rep.cursor out = useSensor (rep.cursor c) (rep.area a)
  step : ∀ n d clicked, ∃ after event, call.step ops n d clicked = ok (after, event) ∧
    (rep.note after, event.map rep.event) =
      tapFamilyStep (rep.note n) ctx.currentTime (rep.duration d) clicked ctx.style
  ended : ∀ n, call.ended ops n = ok (ended (rep.note n))
  prepend_event : ∀ events event, (rep.events events).length + 1 ≤ capacity →
    ∃ out, call.prepend_event ops events event = ok out ∧
      rep.events out = rep.event event :: rep.events events
  advance : ∀ q, ∃ out, call.advance ops q = ok out ∧ rep.queue out = (rep.queue q).advance
  advance_frontier : ∀ f, ∃ out, call.advance_frontier ops f = ok out ∧
    rep.frontiers out = advanceSharedButtonQueue (rep.frontiers f) ctx.zone
  replace_current : ∀ q n, ∃ out, call.replace_current ops q n = ok out ∧
    rep.queue out = { rep.queue q with
      notes := listSetAt (rep.queue q).notes (rep.queue q).currentIndex (rep.note n) }

variable (call : Call (Q := Q) (N := N) (F := F) (C := C)
  (E := E) (Event := Event) (D := D) (Area := Area)) (ops : Ops)
  (rep : Representation Q N F C E Event D Area) (ctx : Context) (capacity : Nat)
  (atoms : AtomicAgrees call ops rep ctx capacity)

/-- Reassociate exactly the two cursor updates in the generated body. Both
availability reads precede either update. The booleans retain the extractor's
explicit true/false result markers. -/
def consumeClicks (allow : Bool) (cursor : C) (area : Area) : Result (Bool × Bool × C) := do
  let b ← call.button_available ops cursor
  let s ← call.sensor_available ops cursor area
  let plan ← scheduler.consume_click allow b s
  let (c, usedButton) ← if plan.used_button then do
    let c ← call.use_button ops cursor
    ok (c, true)
    else ok (cursor, false)
  let (c, usedSensor) ← if plan.used_sensor then do
    let c ← call.use_sensor ops c area
    ok (c, true)
    else ok (c, false)
  ok (usedButton, usedSensor, c)

include atoms in
theorem consume_clicks_spec (allow : Bool) (cursor : C) (area : Area) :
    ∃ button sensor next, consumeClicks call ops allow cursor area = ok (button, sensor, next) ∧
      (button, sensor, rep.cursor next) =
        consumeButtonThenFallbackSensor ctx.input (rep.cursor cursor) allow
          ctx.zone (rep.area area) := by
  obtain ⟨cb, hcb, hcbRep⟩ := atoms.use_button cursor
  obtain ⟨cs, hcs, hcsRep⟩ := atoms.use_sensor cursor area
  have href := SchedulerClickProof.consume_click_equiv ctx.input (rep.cursor cursor)
    allow ctx.zone (rep.area area)
  cases allow <;>
    cases hb : buttonAvailable ctx (rep.cursor cursor) <;>
    cases hs : sensorAvailable ctx (rep.cursor cursor) (rep.area area)
  all_goals
    have hb' := hb
    have hs' := hs
    dsimp only [buttonAvailable] at hb'
    dsimp only [sensorAvailable] at hs'
    simp only [
      SchedulerClickProof.consumeClickRuntime, hb', hs', scheduler.consume_click,
      Bool.false_eq_true, Bool.true_eq, ↓reduceIte, bind_tc_ok, Result.ok.injEq] at href
    simp [consumeClicks, atoms.button_available, atoms.sensor_available, hb, hs,
      scheduler.consume_click, hcb, hcs, hcbRep, hcsRep, ← href,
      useButton, useSensor]

/-- Names the straight-line prefix of the actual body, ending after lifecycle. -/
def prepare (w : RawWork (Q := Q) (F := F) (C := C) (E := E)) (note : N) :
    Result (Area × C × N × Option Event) := do
  let buttonDiff ← call.button_diff ops note
  let sensorDiff ← call.sensor_diff ops note
  let b ← call.eligible ops note
  let allow ← if b then call.unlocked ops w.frontiers note else ok false
  let area ← call.fallback_area ops note
  let (button, sensor, cursor) ← consumeClicks call ops allow w.cursor area
  let (clicked, diff) ← if button then ok (true, buttonDiff) else do
    let diff ← if sensor then ok sensorDiff else ok buttonDiff
    ok (sensor, diff)
  let (after, event) ← call.step ops note diff clicked
  ok (area, cursor, after, event)

/-- Names the rest of that body, including prepend-before-advance and the
unconditional frontier advance for every Ended result, even with no event. -/
def finish (fuel : Usize) (w : RawWork (Q := Q) (F := F) (C := C) (E := E))
    (area : Area) (cursor : C) (after : N) (event : Option Event) :
    Result (ControlFlow (Usize × RawWork (Q := Q) (F := F) (C := C) (E := E))
      (RawOutput (Q := Q) (F := F) (C := C) (E := E))) := do
  let isEnded ← call.ended ops after
  let events ← match event with
    | none => ok w.events_rev
    | some event => call.prepend_event ops w.events_rev event
  if isEnded then
    let queue ← call.advance ops w.queue
    let frontiers ← call.advance_frontier ops w.frontiers
    let b ← call.button_available ops cursor
    let s ← call.sensor_available ops cursor area
    let more ← scheduler.tap_queue_has_unused_click b s
    if more then
      let fuel ← fuel - 1#usize
      ok (.cont (fuel, ⟨queue, frontiers, cursor, events⟩))
    else ok (.done (queue, frontiers, cursor, events))
  else
    let queue ← call.replace_current ops w.queue after
    ok (.done (queue, w.frontiers, cursor, events))

theorem body_reassociated (fuel : Usize)
    (w : RawWork (Q := Q) (F := F) (C := C) (E := E)) (note : N)
    (hpeek : call.peek ops w.queue = ok (some note)) (hfuel : fuel ≠ 0#usize) :
    tap_schedule.process_loop.body call ops fuel w = (do
      let (area, cursor, after, event) ← prepare call ops w note
      finish call ops fuel w area cursor after event) := by
  simp only [tap_schedule.process_loop.body, hpeek, hfuel, if_false,
    prepare, consumeClicks, finish, bind_assoc_eq, bind_tc_ok, bind_eq_iff,
    Prod.forall, uncurry_apply_pair]
  all_goals (intros; rfl)


include atoms in
theorem prepare_spec (w : RawWork (Q := Q) (F := F) (C := C) (E := E)) (note : N) :
    ∃ area cursor after event button sensor,
      prepare call ops w note = ok (area, cursor, after, event) ∧
      rep.area area = fallback (rep.note note) ∧
      (button, sensor, rep.cursor cursor) =
        consume ctx (rep.note note) (eligible ctx (rep.frontiers w.frontiers) (rep.note note))
          (rep.cursor w.cursor) ∧
      (rep.note after, event.map rep.event) = judgment ctx (rep.note note) button sensor := by
  obtain ⟨db, hdb, hdbRep⟩ := atoms.button_diff note
  obtain ⟨ds, hds, hdsRep⟩ := atoms.sensor_diff note
  obtain ⟨area, ha, haRep⟩ := atoms.fallback_area note
  obtain ⟨button, sensor, cursor, hc, hcRep⟩ := consume_clicks_spec call ops rep ctx capacity atoms
    (eligible ctx (rep.frontiers w.frontiers) (rep.note note)) w.cursor area
  let diff := if button then db else if sensor then ds else db
  obtain ⟨after, event, hstep, hstepRep⟩ := atoms.step note diff (button || sensor)
  have hprefix : prepare call ops w note = (do
      let (b, s, c) ← consumeClicks call ops
        (eligible ctx (rep.frontiers w.frontiers) (rep.note note)) w.cursor area
      let (clicked, d) ← if b then ok (true, db) else do
        let d ← if s then ok ds else ok db
        ok (s, d)
      let (a, e) ← call.step ops note d clicked
      ok (area, c, a, e)) := by
    simp only [prepare, hdb, hds, atoms.eligible, atoms.unlocked, ha, bind_tc_ok]
    cases heligible : tapEligibleForClick (rep.note note) ctx.currentTime <;>
      simp [eligible, heligible, bind_tc_ok]
  refine ⟨area, cursor, after, event, button, sensor, ?_, haRep, ?_, ?_⟩
  · rw [hprefix, hc]
    cases button <;> cases sensor <;>
      simpa only [diff, Bool.false_eq_true, Bool.true_eq, ↓reduceIte,
        Bool.false_or, Bool.true_or, bind_tc_ok] using
        congrArg (fun r => r >>= fun (a, e) => ok (area, cursor, a, e)) hstep
  · simpa only [consume, haRep] using hcRep
  · cases button <;> cases sensor <;>
      simpa only [judgment, diff, Bool.not_false, Bool.not_true,
        Bool.true_and, Bool.false_and, Bool.false_eq_true, Bool.true_eq, ↓reduceIte,
        hdbRep, hdsRep] using hstepRep

def interpretFlow (rep : Representation Q N F C E Event D Area)
    (out : ControlFlow (Usize × RawWork (Q := Q) (F := F) (C := C) (E := E))
      (RawOutput (Q := Q) (F := F) (C := C) (E := E))) :
    ControlFlow (Nat × Work) Outcome :=
  match out with
  | .cont (fuel, w) => .cont (fuel.val, interpret rep w)
  | .done result => .done (interpret rep (restore result)).output

include atoms in
theorem finish_spec (fuel : Usize) (hfuel : 0 < fuel.val)
    (w : RawWork (Q := Q) (F := F) (C := C) (E := E)) (before : TapFamilyNote)
    (area : Area) (cursor : C) (after : N) (event : Option Event)
    (harea : rep.area area = fallback before)
    (hfit : (advance ctx (interpret rep w) (rep.note after) (event.map rep.event)
      (rep.cursor cursor)).eventsRev.length ≤ capacity) :
    ∃ result, finish call ops fuel w area cursor after event = ok result ∧
      interpretFlow rep result =
        let next := advance ctx (interpret rep w) (rep.note after)
          (event.map rep.event) (rep.cursor cursor)
        if ended (rep.note after) && unused ctx before (rep.cursor cursor)
        then .cont (fuel.val - 1, next) else .done next.output := by
  have hevents : ∃ events,
      (match event with
        | none => ok w.events_rev
        | some event => call.prepend_event ops w.events_rev event) = ok events ∧
      rep.events events =
        match event.map rep.event with
        | none => rep.events w.events_rev
        | some event => event :: rep.events w.events_rev := by
    cases event with
    | none => exact ⟨w.events_rev, rfl, rfl⟩
    | some event =>
        obtain ⟨events, hrun, hevents⟩ := atoms.prepend_event w.events_rev event (by
          simpa only [advance, interpret, Option.map_some, List.length_cons] using hfit)
        exact ⟨events, hrun, hevents⟩
  obtain ⟨events, he, heRep⟩ := hevents
  obtain ⟨queue, hq, hqRep⟩ := atoms.advance w.queue
  obtain ⟨frontiers, hf, hfRep⟩ := atoms.advance_frontier w.frontiers
  obtain ⟨replaced, hr, hrRep⟩ := atoms.replace_current w.queue after
  obtain ⟨less, hless, hlessRep⟩ := spec_imp_exists
    (UScalar.sub_spec (x := fuel) (y := 1#usize) (by scalar_tac))
  have hmore : scheduler.tap_queue_has_unused_click
      (buttonAvailable ctx (rep.cursor cursor)) (sensorAvailable ctx (rep.cursor cursor) (rep.area area)) =
      ok (unused ctx before (rep.cursor cursor)) := by
    simpa only [buttonAvailable, sensorAvailable, unused, harea] using
      SchedulerClickProof.tap_queue_click_equiv ctx.input (rep.cursor cursor) ctx.zone (rep.area area)
  cases hend : ended (rep.note after) <;> cases hu : unused ctx before (rep.cursor cursor)
  all_goals
    simp only [finish, atoms.ended, hend, he, bind_tc_ok, Bool.false_eq_true,
      Bool.true_eq, ↓reduceIte, hq, hf, hr, atoms.button_available,
      atoms.sensor_available, hmore, hu, hless, bind_tc_ok,
      Result.ok.injEq, exists_eq_left]
    simp [interpretFlow, interpret, restore, advance, Work.output, hend, hu,
      hqRep, hfRep, hrRep, heRep, hlessRep.1] <;> cases event <;> rfl

/- The invariant for the actual body resumes the ORIGINAL fuel recursion.
A continuing iteration strictly decreases fuel; every exit returns its result. -/
include atoms in
theorem body_spec (fuel : Usize)
    (w : RawWork (Q := Q) (F := F) (C := C) (E := E))
    (hfit : (resume ctx fuel.val (interpret rep w)).2.2.2.length ≤ capacity) :
    tap_schedule.process_loop.body call ops fuel w ⦃ result =>
      match result with
      | .cont (nextFuel, next) =>
          resume ctx nextFuel.val (interpret rep next) = resume ctx fuel.val (interpret rep w) ∧
          nextFuel.val < fuel.val
      | .done out => (interpret rep (restore out)).output = resume ctx fuel.val (interpret rep w) ⦄ := by
  obtain ⟨peeked, hpeek, hpeekRep⟩ := atoms.peek w.queue
  cases peeked with
  | none =>
      have hempty : (rep.queue w.queue).peek = none := hpeekRep.symm
      apply exists_imp_spec
      refine ⟨.done (w.queue, w.frontiers, w.cursor, w.events_rev), ?_, ?_⟩
      · simp only [tap_schedule.process_loop.body, hpeek, bind_tc_ok]
      · exact (resume_empty ctx fuel.val (interpret rep w) hempty).symm
  | some note =>
      by_cases hzero : fuel = 0#usize
      · apply exists_imp_spec
        refine ⟨.done (w.queue, w.frontiers, w.cursor, w.events_rev), ?_, ?_⟩
        · simp only [tap_schedule.process_loop.body, hpeek, hzero, ↓reduceIte, bind_tc_ok]
        · subst fuel
          exact (resume_zero ctx (interpret rep w)).symm
      · have hpositive : 0 < fuel.val := by scalar_tac
        have hsucc : fuel.val = (fuel.val - 1) + 1 := by omega
        have href := resume_step ctx (fuel.val - 1) (interpret rep w) (rep.note note) hpeekRep.symm
        rw [← hsucc] at href
        obtain ⟨area, cursor, after, event, button, sensor, hp, ha, hc, hj⟩ :=
          prepare_spec call ops rep ctx capacity atoms w note
        dsimp only [interpret] at href
        rw [← hc] at href
        dsimp only at href
        rw [← hj] at href
        dsimp only at href
        let next := advance ctx (interpret rep w) (rep.note after) (event.map rep.event) (rep.cursor cursor)
        change resume ctx fuel.val (interpret rep w) =
          (if ended (rep.note after) && unused ctx (rep.note note) (rep.cursor cursor)
            then resume ctx (fuel.val - 1) next else next.output) at href
        have hnextFit : next.eventsRev.length ≤ capacity := by
          rw [href] at hfit
          split at hfit
          · exact Nat.le_trans (resume_events_growth ctx (fuel.val - 1) next) hfit
          · exact hfit
        obtain ⟨result, hfinish, hresult⟩ :=
          finish_spec call ops rep ctx capacity atoms fuel hpositive w (rep.note note)
            area cursor after event ha hnextFit
        apply exists_imp_spec
        refine ⟨result, ?_, ?_⟩
        · rw [body_reassociated call ops fuel w note hpeek hzero, hp]
          simpa only [bind_tc_ok] using hfinish
        · cases result with
          | cont pair =>
              rcases pair with ⟨nextFuel, nextWork⟩
              dsimp only [interpretFlow] at hresult
              split at hresult
              · rename_i hcontinue
                have heq := ControlFlow.cont.inj hresult
                have hfuel := congrArg Prod.fst heq
                have hwork := congrArg Prod.snd heq
                dsimp only at hfuel hwork ⊢
                constructor
                · rw [hfuel, hwork, href]
                  simp only [hcontinue, ↓reduceIte, next]
                · rw [hfuel]
                  omega
              · contradiction
          | done out =>
              dsimp only [interpretFlow] at hresult
              split at hresult
              · contradiction
              · rename_i hstop
                have heq := ControlFlow.done.inj hresult
                dsimp only
                rw [heq, href]
                simp only [hstop, Bool.false_eq_true, ↓reduceIte, next]

include atoms in
theorem process_loop_spec (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (E := E))
    (hfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.length ≤ capacity) :
    tap_schedule.process_loop call fuel initial ops ⦃ out =>
      (interpret rep (restore out)).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  unfold tap_schedule.process_loop
  apply loop.spec_decr_nat (fun state => state.1.val)
    (fun state => resume ctx state.1.val (interpret rep state.2) =
      resume ctx fuel.val (interpret rep initial))
  · rintro ⟨remaining, work⟩ hinv
    obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
      (body_spec call ops rep ctx capacity atoms remaining work (by rw [hinv]; exact hfit))
    apply exists_imp_spec
    refine ⟨out, hrun, ?_⟩
    cases out with
    | cont state => exact ⟨hpost.1.trans hinv, hpost.2⟩
    | done result => exact hpost.trans hinv
  · rfl

include atoms in
theorem process_equiv (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (E := E))
    (hfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.length ≤ capacity) :
    tap_schedule.process call fuel initial ops ⦃ out =>
      (interpret rep out).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (process_loop_spec call ops rep ctx capacity atoms fuel initial hfit)
  rcases out with ⟨queue, frontiers, cursor, events⟩
  apply exists_imp_spec
  refine ⟨⟨queue, frontiers, cursor, events⟩, ?_, hpost⟩
  simp only [tap_schedule.process, hrun, bind_tc_ok]
  rfl

/- The runtime supplies pending count, without the reference's extra one.
This theorem also covers an initially out-of-range arbitrary-Nat cursor. -/
include atoms in
theorem pending_process_equiv (fuel : Usize)
    (initial : RawWork (Q := Q) (F := F) (C := C) (E := E))
    (hfuel : fuel.val = remaining (rep.queue initial.queue))
    (hfit : (resume ctx fuel.val (interpret rep initial)).2.2.2.length ≤ capacity) :
    tap_schedule.process call fuel initial ops ⦃ out =>
      (interpret rep out).output = resume ctx
        (remaining (rep.queue initial.queue) + 1) (interpret rep initial) ⦄ := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (process_equiv call ops rep ctx capacity atoms fuel initial hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, hpost.trans ?_⟩
  apply resume_fuel_irrelevant
  · change remaining (rep.queue initial.queue) ≤ fuel.val
    omega
  · simp [interpret]

end Verification.TapScheduleProof
