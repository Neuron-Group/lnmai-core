import Verification.TouchAutomaticModel
import Verification.Generated

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

set_option maxHeartbeats 800000

namespace Verification.TouchAutomaticProof
open TouchAutomaticModel
open private touchQueueResolved advanceSharedTouchQueue from LnmaiCore.Scheduler

/-! The actual extracted automatic traversal, related to the original private
Lean recursion. No premise supplies a whole slot or pass. Single-note lifecycle,
group lookup, scalar/field representations and bounded allocation remain
explicit contracts. The source queue is immutable for the entire traversal. -/

structure Representation (S N Notes F E Event D Shared : Type) where
  source : S → ZoneQueue TouchNote
  note : N → TouchNote
  notes : Notes → List TouchNote
  frontiers : F → SensorVec Nat
  events : E → List JudgeEvent
  event : Event → JudgeEvent
  duration : D → Duration
  shared : Shared → JudgeGrade × Duration

def interpret {S N Notes F E Event D Shared : Type}
    (rep : Representation S N Notes F E Event D Shared)
    (w : touch_automatic.TouchAutomaticWork Notes F E) : Work :=
  ⟨rep.notes w.notes, rep.frontiers w.frontiers, rep.events w.events_rev⟩

structure Capacity where
  notes : Nat
  events : Nat

def Fits (cap : Capacity) (w : Work) : Prop :=
  w.notes.length ≤ cap.notes ∧ w.eventsRev.length ≤ cap.events

theorem step_fits (ctx : Context) (queue : ZoneQueue TouchNote) (cap : Capacity)
    (index : Nat) (note : TouchNote) (rest : List TouchNote) (w : Work)
    (hfit : Fits cap (resume ctx queue index (note :: rest) w)) :
    Fits cap (step ctx queue index note w) := by
  rw [resume_step] at hfit
  have hn := resume_notes_length ctx queue (index + 1) rest (step ctx queue index note w)
  have he := resume_events_bound ctx queue (index + 1) rest (step ctx queue index note w)
  exact ⟨by have := hfit.1; omega, Nat.le_trans he.1 hfit.2⟩

structure AtomicAgrees {S N Notes F E Event D Shared Ops : Type}
    (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (rep : Representation S N Notes F E Event D Shared)
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ source, ∃ len, call.length ops source = ok len ∧
    (rep.source source).notes.length = len.val
  get : ∀ source (index : Usize) (h : index.val < (rep.source source).notes.length),
    ∃ note, call.get ops source index = ok note ∧
      rep.note note = (rep.source source).notes[index.val]'h
  before_cursor : ∀ source index, call.before_cursor ops source index =
    ok (decide (index.val < (rep.source source).currentIndex))
  resolved : ∀ note, call.resolved ops note = ok (touchQueueResolved (rep.note note))
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = difference ctx (rep.note note)
  shared : ∀ note, ∃ result, call.shared ops note = ok result ∧
    result.map rep.shared = TouchAutomaticModel.shared ctx (rep.note note)
  too_late : ∀ note, call.too_late ops note = ok (tooLate ctx (rep.note note))
  step : ∀ note diff result, ∃ after event, call.step ops note diff result = ok (after, event) ∧
    (rep.note after, event.map rep.event) =
      touchStep (rep.note note) ctx.currentTime (rep.duration diff) false
        (result.map rep.shared) ctx.style
  advance_frontier : ∀ frontiers, ∃ next, call.advance_frontier ops frontiers = ok next ∧
    rep.frontiers next = advanceSharedTouchQueue (rep.frontiers frontiers) ctx.area
  prepend_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.prepend_event ops events event = ok next ∧
      rep.events next = rep.event event :: rep.events events
  append_note : ∀ notes note, (rep.notes notes).length + 1 ≤ cap.notes →
    ∃ next, call.append_note ops notes note = ok next ∧
      rep.notes next = rep.notes notes ++ [rep.note note]

section Extracted
variable {S N Notes F E Event D Shared Ops : Type}
variable (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
variable (rep : Representation S N Notes F E Event D Shared) (ctx : Context) (cap : Capacity)

/-- An exact abbreviation of the generated post-judgment branch. -/
def finishNative (w : touch_automatic.TouchAutomaticWork Notes F E)
    (wasResolved : Bool) (after : N) (event : Option Event) : Result (Notes × F × E) := do
  let resolvedNow ← if wasResolved then ok false else call.resolved ops after
  let work1 ← if resolvedNow then do
      let next ← call.advance_frontier ops w.frontiers
      ok { w with frontiers := next }
    else ok w
  let work2 ← match event with
    | none => ok work1
    | some event => do
        let next ← call.prepend_event ops work1.events_rev event
        ok { work1 with events_rev := next }
  let notes ← call.append_note ops work2.notes after
  ok (notes, work2.frontiers, work2.events_rev)

theorem finish_native_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (w : touch_automatic.TouchAutomaticWork Notes F E) (before after : N)
    (event : Option Event)
    (hfit : Fits cap (update ctx (interpret rep w) (rep.note before) (rep.note after)
      (event.map rep.event))) :
    ∃ notes frontiers events,
      finishNative call ops w (touchQueueResolved (rep.note before)) after event =
        ok (notes, frontiers, events) ∧
      interpret rep ⟨notes, frontiers, events⟩ =
        update ctx (interpret rep w) (rep.note before) (rep.note after) (event.map rep.event) := by
  obtain ⟨advanced, hadvance, hadvanceRep⟩ := atoms.advance_frontier w.frontiers
  let resolvedNow := !touchQueueResolved (rep.note before) && touchQueueResolved (rep.note after)
  let frontiers := if resolvedNow then advanced else w.frontiers
  have hfront : (do
      let resolved ← if touchQueueResolved (rep.note before) then ok false
        else call.resolved ops after
      if resolved then do
        let next ← call.advance_frontier ops w.frontiers
        ok { w with frontiers := next }
      else ok w) = (ok { w with frontiers := frontiers } :
        Result (touch_automatic.TouchAutomaticWork Notes F E)) := by
    cases hb : touchQueueResolved (rep.note before) <;>
      cases ha : touchQueueResolved (rep.note after) <;>
      simp [atoms.resolved, hb, ha, hadvance, frontiers, resolvedNow, bind_tc_ok]
  have hfrontRep : rep.frontiers frontiers =
      if resolvedNow then advanceSharedTouchQueue (rep.frontiers w.frontiers) ctx.area
      else rep.frontiers w.frontiers := by
    unfold frontiers
    split <;> simp only [hadvanceRep]
  obtain ⟨notes, hnotes, hnotesRep⟩ := atoms.append_note w.notes after (by
    simpa [Fits, update, interpret] using hfit.1)
  have hevents : ∃ events, (match event with
      | none => ok ({ w with frontiers := frontiers })
      | some event => do
          let next ← call.prepend_event ops w.events_rev event
          ok { w with frontiers := frontiers, events_rev := next }) = ok
        ({ w with frontiers := frontiers, events_rev := events }) ∧
      rep.events events = match event.map rep.event with
        | none => rep.events w.events_rev
        | some evt => evt :: rep.events w.events_rev := by
    cases event with
    | none => exact ⟨w.events_rev, rfl, rfl⟩
    | some event =>
        obtain ⟨events, hcall, hrep⟩ := atoms.prepend_event w.events_rev event (by
          simpa [Fits, update, interpret] using hfit.2)
        exact ⟨events, by simp only [hcall, bind_tc_ok], hrep⟩
  obtain ⟨events, hevents, heventsRep⟩ := hevents
  refine ⟨notes, frontiers, events, ?_, ?_⟩
  · unfold finishNative
    rw [← bind_assoc, hfront]
    simp only [bind_tc_ok, hevents, hnotes]
  · simp only [interpret, update, hnotesRep, hfrontRep, heventsRep, resolvedNow]
    rfl

private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

theorem process_body_spec (atoms : AtomicAgrees call ops rep ctx cap)
    (source : S) (len index : Usize) (w : touch_automatic.TouchAutomaticWork Notes F E)
    (hlen : (rep.source source).notes.length = len.val) (hmore : index.val < len.val)
    (hfit : Fits cap (step ctx (rep.source source) index.val
      ((rep.source source).notes[index.val]'(by omega)) (interpret rep w))) :
    ∃ next nextIndex,
      touch_automatic.process_loop.body call source ops len w index = ok (.cont (next, nextIndex)) ∧
      interpret rep next = step ctx (rep.source source) index.val
        ((rep.source source).notes[index.val]'(by omega)) (interpret rep w) ∧
      nextIndex.val = index.val + 1 := by
  obtain ⟨before, hget, hbefore⟩ := atoms.get source index (by omega)
  obtain ⟨nextIndex, hi, hiVal⟩ := increment_index index len hmore
  have hguard : index < len := by simpa using hmore
  rw [← hbefore] at hfit ⊢
  have hn : (rep.notes w.notes).length + 1 ≤ cap.notes := by
    have h := step_notes_length ctx (rep.source source) index.val (rep.note before) (interpret rep w)
    have := hfit.1
    rw [h] at this
    exact this
  obtain ⟨kept, hkeep, hkeepRep⟩ := atoms.append_note w.notes before hn
  by_cases hskip : (decide (index.val < (rep.source source).currentIndex) ||
      touchQueueResolved (rep.note before)) = true
  · refine ⟨⟨kept, w.frontiers, w.events_rev⟩, nextIndex, ?_, ?_, hiVal⟩
    · cases hp : decide (index.val < (rep.source source).currentIndex) <;>
        cases hr : touchQueueResolved (rep.note before) <;>
        simp_all only [Bool.or_self, Bool.or_false, Bool.false_or, Bool.true_eq_false,
          Bool.false_eq_true, touch_automatic.process_loop.body, hguard, ↓reduceIte, hget,
          atoms.before_cursor, atoms.resolved, hkeep, hi, bind_tc_ok, uncurry_apply_pair]
    · simp only [step, hskip, ↓reduceIte, keep, interpret, hkeepRep]
  · have hp : decide (index.val < (rep.source source).currentIndex) = false := by
      cases hh : decide (index.val < (rep.source source).currentIndex) <;> simp_all
    have hr : touchQueueResolved (rep.note before) = false := by
      cases hh : touchQueueResolved (rep.note before) <;> simp_all
    obtain ⟨diff, hd, hdRep⟩ := atoms.sensor_diff before
    obtain ⟨result, hs, hsRep⟩ := atoms.shared before
    have hsNone : core.option.Option.is_none result =
        (TouchAutomaticModel.shared ctx (rep.note before)).isNone := by
      rw [← hsRep]
      cases result <;> rfl
    by_cases hwait : ((TouchAutomaticModel.shared ctx (rep.note before)).isNone &&
        !tooLate ctx (rep.note before)) = true
    · have hnone : (TouchAutomaticModel.shared ctx (rep.note before)).isNone = true := by
        cases hh : (TouchAutomaticModel.shared ctx (rep.note before)).isNone <;> simp_all
      have hlate : tooLate ctx (rep.note before) = false := by
        cases hh : tooLate ctx (rep.note before) <;> simp_all
      refine ⟨⟨kept, w.frontiers, w.events_rev⟩, nextIndex, ?_, ?_, hiVal⟩
      · simp only [touch_automatic.process_loop.body, hguard, ↓reduceIte, hget,
          atoms.before_cursor, hp, atoms.resolved, hr, hd, hs, atoms.too_late, hsNone,
          hnone, hlate, hkeep, hi, bind_tc_ok, uncurry_apply_pair, Bool.false_eq_true]
      · simp only [step, hskip, hwait, ↓reduceIte, keep, interpret, hkeepRep, Bool.false_eq_true]
    · obtain ⟨after, event, hj, hjRep⟩ := atoms.step before diff result
      have hmodel : step ctx (rep.source source) index.val (rep.note before) (interpret rep w) =
          update ctx (interpret rep w) (rep.note before) (rep.note after) (event.map rep.event) := by
        simp only [step, hskip, Bool.false_eq_true, ↓reduceIte, hwait]
        rw [← hdRep, ← hsRep, ← hjRep]
      obtain ⟨notes, frontiers, events, hpost, hpostRep⟩ := finish_native_spec call ops rep ctx cap
        atoms w before after event (by simpa only [hmodel] using hfit)
      refine ⟨⟨notes, frontiers, events⟩, nextIndex, ?_, hpostRep.trans hmodel.symm, hiVal⟩
      simp only [touch_automatic.process_loop.body, hguard, ↓reduceIte, hget,
        atoms.before_cursor, hp, atoms.resolved, hr, hd, hs, atoms.too_late, hsNone,
        bind_tc_ok, Bool.false_eq_true]
      have hcont : (do
          let (notes, frontiers, events) ← finishNative call ops w false after event
          let nextIndex ← index + 1#usize
          ok (ControlFlow.cont (β := touch_automatic.TouchAutomaticWork Notes F E)
            ({ notes := notes, frontiers := frontiers, events_rev := events }, nextIndex))) =
          ok (ControlFlow.cont (
            (⟨notes, frontiers, events⟩ : touch_automatic.TouchAutomaticWork Notes F E), nextIndex)) := by
        rw [hr] at hpost
        simp only [hpost, hi, bind_tc_ok, uncurry_apply_pair]
      cases hn : (TouchAutomaticModel.shared ctx (rep.note before)).isNone <;>
        cases hl : tooLate ctx (rep.note before) <;>
        simp only [hn, hl, Bool.not_true, Bool.not_false, Bool.and_self,
          Bool.false_and, Bool.and_false, Bool.false_eq_true, not_false_eq_true] at hwait ⊢
      all_goals try contradiction
      all_goals simpa only [hj, bind_tc_ok, uncurry_apply_pair, finishNative, hr,
        atoms.resolved, ↓reduceIte] using hcont

theorem process_body_done (source : S) (len index : Usize)
    (w : touch_automatic.TouchAutomaticWork Notes F E) (hstop : len.val ≤ index.val) :
    touch_automatic.process_loop.body call source ops len w index = ok (.done w) := by
  have hguard : ¬ index < len := by simpa using Nat.not_lt.mpr hstop
  simp only [touch_automatic.process_loop.body, hguard, ↓reduceIte]

end Extracted

inductive BodyHistory {S N Notes F E Event D Shared Ops : Type}
    (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared)
    (ops : Ops) (source : S) (len : Usize) :
    (touch_automatic.TouchAutomaticWork Notes F E × Usize) →
    (touch_automatic.TouchAutomaticWork Notes F E × Usize) → List Nat → Prop
  | refl (state) : BodyHistory call ops source len state state []
  | extend {initial current next visited}
      (past : BodyHistory call ops source len initial current visited)
      (active : current.2.val < len.val)
      (body : touch_automatic.process_loop.body call source ops len current.1 current.2 =
        ok (.cont next)) :
      BodyHistory call ops source len initial next (visited ++ [current.2.val])

section Extracted
variable {S N Notes F E Event D Shared Ops : Type}
variable (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
variable (rep : Representation S N Notes F E Event D Shared) (ctx : Context) (cap : Capacity)

theorem process_loop_spec (atoms : AtomicAgrees call ops rep ctx cap) (source : S) (len : Usize)
    (initial : touch_automatic.TouchAutomaticWork Notes F E) (start : Usize)
    (hlen : (rep.source source).notes.length = len.val) (hstart : start.val ≤ len.val)
    (hfit : Fits cap (resume ctx (rep.source source) start.val
      ((rep.source source).notes.drop start.val) (interpret rep initial))) :
    touch_automatic.process_loop call source initial ops len start ⦃ out =>
      interpret rep out = resume ctx (rep.source source) start.val
        ((rep.source source).notes.drop start.val) (interpret rep initial) ∧
      ∃ finishIndex : Usize, finishIndex.val = len.val ∧
        BodyHistory call ops source len (initial, start) (out, finishIndex)
          (List.range' start.val (len.val - start.val)) ⦄ := by
  unfold touch_automatic.process_loop
  apply loop.spec_decr_nat
    (fun state : touch_automatic.TouchAutomaticWork Notes F E × Usize => len.val - state.2.val)
    (fun state => start.val ≤ state.2.val ∧ state.2.val ≤ len.val ∧
      resume ctx (rep.source source) state.2.val ((rep.source source).notes.drop state.2.val)
          (interpret rep state.1) =
        resume ctx (rep.source source) start.val ((rep.source source).notes.drop start.val)
          (interpret rep initial) ∧
      BodyHistory call ops source len (initial, start) state
        (List.range' start.val (state.2.val - start.val)))
  · rintro ⟨w, index⟩ ⟨hlow, hupp, hresume, history⟩
    dsimp only at hlow hupp hresume history ⊢
    try simp only [uncurry_apply_pair]
    by_cases hguard : index < len
    · have hmore : index.val < len.val := by simpa using hguard
      have hslot : index.val < (rep.source source).notes.length := by omega
      let note := (rep.source source).notes[index.val]'hslot
      let rest := (rep.source source).notes.drop (index.val + 1)
      have hpending : (rep.source source).notes.drop index.val = note :: rest :=
        List.drop_eq_getElem_cons hslot
      have hready := step_fits ctx (rep.source source) cap index.val note rest (interpret rep w) (by
        rw [← hpending, hresume]
        exact hfit)
      obtain ⟨next, nextIndex, hbody, hnextRep, hnextIndex⟩ :=
        process_body_spec call ops rep ctx cap atoms source len index w hlen hmore hready
      have hnextResume : resume ctx (rep.source source) nextIndex.val
          ((rep.source source).notes.drop nextIndex.val) (interpret rep next) =
          resume ctx (rep.source source) start.val ((rep.source source).notes.drop start.val)
            (interpret rep initial) := by
        rw [hnextIndex, hnextRep]
        exact (resume_drop_step ctx (rep.source source) index.val (interpret rep w) hslot).symm.trans hresume
      have hcount : nextIndex.val - start.val = (index.val - start.val) + 1 := by omega
      have hsum : start.val + (index.val - start.val) = index.val := by omega
      have hhistory : BodyHistory call ops source len (initial, start) (next, nextIndex)
          (List.range' start.val (nextIndex.val - start.val)) := by
        rw [hcount, List.range'_concat]
        simpa only [Nat.one_mul, hsum] using BodyHistory.extend history hmore hbody
      apply exists_imp_spec
      refine ⟨.cont (next, nextIndex), hbody, ?_⟩
      dsimp only
      exact ⟨⟨by omega, by omega, hnextResume, hhistory⟩, by omega⟩
    · have hstop : len.val ≤ index.val := by simpa using hguard
      have heq : index.val = len.val := by omega
      have hdone := resume_drop_done ctx (rep.source source) index.val (interpret rep w) (by omega)
      apply exists_imp_spec
      refine ⟨.done w, process_body_done call ops source len index w hstop,
        hdone.symm.trans hresume, index, heq, ?_⟩
      simpa only [heq] using history
  · dsimp only
    refine ⟨Nat.le_refl _, hstart, rfl, ?_⟩
    simpa only [Nat.sub_self, List.range'_zero] using
      BodyHistory.refl (call := call) (ops := ops) (source := source) (len := len) (initial, start)

theorem process_spec (atoms : AtomicAgrees call ops rep ctx cap) (source : S)
    (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes (interpret rep initial))) :
    touch_automatic.process call source initial ops ⦃ out =>
      interpret rep out = resume ctx (rep.source source) 0 (rep.source source).notes (interpret rep initial) ∧
      ∃ len finishIndex : Usize,
        (rep.source source).notes.length = len.val ∧ finishIndex.val = len.val ∧
        BodyHistory call ops source len (initial, 0#usize) (out, finishIndex)
          (List.range' 0 (rep.source source).notes.length) ⦄ := by
  obtain ⟨len, hlength, hlen⟩ := atoms.length source
  obtain ⟨out, hrun, hout, finishIndex, hfinish, history⟩ := spec_imp_exists
    (process_loop_spec call ops rep ctx cap atoms source len initial 0#usize hlen
      (by simp) (by simpa using hfit))
  apply exists_imp_spec
  refine ⟨out, ?_, ?_, len, finishIndex, hlen, hfinish, ?_⟩
  · simp only [touch_automatic.process, hlength, hrun, bind_tc_ok]
  · simpa using hout
  · simpa [hlen] using history

theorem process_visits_once (atoms : AtomicAgrees call ops rep ctx cap) (source : S)
    (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes (interpret rep initial))) :
    touch_automatic.process call source initial ops ⦃ out =>
      ∃ (len finishIndex : Usize) (visited : List Nat),
        BodyHistory call ops source len (initial, 0#usize) (out, finishIndex) visited ∧
        visited.Nodup ∧ (∀ index, index ∈ visited ↔ index < (rep.source source).notes.length) ∧
        visited.length = (rep.source source).notes.length ⦄ := by
  obtain ⟨out, hrun, _, len, finishIndex, _, _, history⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms source initial hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, len, finishIndex, List.range' 0 (rep.source source).notes.length,
    history, List.nodup_range', ?_, List.length_range'⟩
  intro index
  simp only [List.mem_range'_1, Nat.zero_add, Nat.zero_le, true_and]

/-- Finishing still uses the reference queue normalization. Its concrete
installation and the initial empty native note buffer are separate obligations. -/
theorem process_equiv (atoms : AtomicAgrees call ops rep ctx cap) (source : S)
    (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (frontiers : SensorVec Nat) (events : List JudgeEvent)
    (hinit : interpret rep initial = ⟨[], frontiers, events⟩)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes ⟨[], frontiers, events⟩)) :
    touch_automatic.process call source initial ops ⦃ out =>
      finish ctx (rep.source source) (interpret rep out) =
        reference ctx (rep.source source) frontiers events ⦄ := by
  obtain ⟨out, hrun, hout, _⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms source initial (by simpa only [hinit] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_⟩
  rw [hout, hinit, reference_eq_resume]

end Extracted
end Verification.TouchAutomaticProof
