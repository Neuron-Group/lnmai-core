import Verification.SlideLifecycle
import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

set_option maxHeartbeats 300000

namespace Verification.SlideScheduleProof

open private processSlideNotesCoreFuel forceFinishDirectParent endSlideAncestorsFuel
  endSlideAncestors updateSlideParentFlags updateSlideParentFlags_length from LnmaiCore.Scheduler

/-!
Equivalence proofs for the shared slide_schedule::process loop. The runtime
delegates its process_slide_notes_core traversal to this kernel. The invariant
resumes the ORIGINAL Scheduler fuel recursion. The definition 'advance' only
names its one-step expression; it is neither a second recursive scheduler nor
an assumed equivalent callback. Read, lifecycle, replacement, the three parent
operations, and each append have separate adapter contracts.

The installed extraction was inspected: the body continues with SlideWork,
but its done branch and process_loop return (slides, events, audio, render,
index). The process wrapper reconstructs SlideWork from that tuple. Trait
parameters and call order below follow those actual generated declarations.
Acceptance requires proof compilation and a transitive axiom audit against
the synchronized generated source.
-/

structure Context where
  input : FrameInput
  currentTime : TimePoint
  touchPanelOffset : Duration
  delta : Duration
  style : JudgeStyle
  subdivide : Bool

def Context.stepContext (ctx : Context) : SlideStepContext :=
  { currentTime := ctx.currentTime
    touchPanelOffset := ctx.touchPanelOffset
    delta := ctx.delta
    style := ctx.style
    subdivideSlideJudgeGrade := ctx.subdivide
    sensorHeld := ctx.input.sensorHeld }

def lifecycle (ctx : Context) (note : SlideNote) :=
  slideStep note ctx.currentTime ctx.input.sensorHeld ctx.touchPanelOffset ctx.delta
    ctx.style ctx.subdivide

abbrev Outcome :=
  List SlideNote × List JudgeEvent × List AudioCommand × List RenderCommand

abbrev StepBatches :=
  SlideNote × List JudgeEvent × List AudioCommand × List RenderCommand

def stepBatches (ctx : Context) (note : SlideNote) : StepBatches :=
  let (after, event, audio, render) := lifecycle ctx note
  (after, event.toList, audio, render)

/-- Reuse the existing lifecycle theorem, including its output encoder.
    Runtime note/time/container interpretation is still an adapter. -/
theorem lifecycle_batches_from_existing (ctx : Context) (note : SlideNote) :
    (do
      let (after, event, audio, render) ←
        SlideLifecycleProof.step note ctx.stepContext
      ok (after, event.toList, audio, render)) = ok (stepBatches ctx note) := by
  rw [SlideLifecycleProof.step_equiv]
  rfl

structure RefWork where
  slides : List SlideNote
  events : List JudgeEvent
  audio : List AudioCommand
  render : List RenderCommand
  index : Nat

def RefWork.output (w : RefWork) : Outcome := (w.slides, w.events, w.audio, w.render)

/-- Alias of the original private definition, not a new recursion. -/
def fuelReference (ctx : Context) (fuel : Nat) (processedRev pending : List SlideNote)
    (eventsRev : List JudgeEvent) (audioRev : List AudioCommand)
    (renderRev : List RenderCommand) : Outcome :=
  processSlideNotesCoreFuel fuel processedRev pending ctx.input ctx.currentTime
    ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide eventsRev audioRev renderRev

def resume (ctx : Context) (w : RefWork) : Outcome :=
  fuelReference ctx (w.slides.length - w.index + 1)
    (w.slides.take w.index).reverse (w.slides.drop w.index)
    w.events.reverse w.audio.reverse w.render.reverse

/-- One original reference step, retaining the whole list after BOTH parent
    operations and refresh. No old processed/pending data is copied back over
    the parent updates. Output buffers are in forward order. -/
def advance (ctx : Context) (pre : List SlideNote) (note : SlideNote)
    (rest : List SlideNote) (events : List JudgeEvent) (audio : List AudioCommand)
    (render : List RenderCommand) : RefWork :=
  let (after, event, audioCmds, renderCmds) := lifecycle ctx note
  let (forced, directCmds) := forceFinishDirectParent (pre ++ after :: rest) note after
  let (ended, ancestorCmds) := endSlideAncestors note after forced
  { slides := updateSlideParentFlags ended
    events := events ++ event.toList
    audio := audio ++ audioCmds
    render := render ++ renderCmds ++ directCmds ++ ancestorCmds
    index := pre.length + 1 }

theorem force_length (slides : List SlideNote) (before after : SlideNote) :
    (forceFinishDirectParent slides before after).1.length = slides.length := by
  unfold forceFinishDirectParent
  split
  · rfl
  · cases hp : after.parentNoteIndex with
    | none => rfl
    | some target =>
        simp only [hp]
        cases hf : slides.find? (fun slide => slide.params.noteIndex == target) with
        | none => simp only [hf]
        | some found =>
            simp only [hf]
            split <;> simp only [List.length_map]

/-- Unfold one fuel constructor; never recursively simp arbitrary fuel. -/
theorem ancestors_fuel_length (fuel : Nat) (slides : List SlideNote)
    (parent : Option Nat) :
    (endSlideAncestorsFuel fuel slides parent).1.length = slides.length := by
  induction fuel generalizing slides parent with
  | zero => rfl
  | succ fuel ih =>
      cases parent with
      | none => rfl
      | some target =>
          conv => lhs; unfold endSlideAncestorsFuel
          cases hf : slides.find? (fun slide => slide.params.noteIndex == target) with
          | none => rfl
          | some found =>
              cases hs : found.state <;> simp only [hs]
              all_goals first
                | rfl
                | (
                    let updated := slides.map (fun slide =>
                      if slide.params.noteIndex == target
                      then { slide with state := .Ended } else slide)
                    have hrec := ih updated found.parentNoteIndex
                    cases hpair : endSlideAncestorsFuel fuel updated found.parentNoteIndex
                    simpa only [hpair, updated, List.length_map] using hrec)

theorem ancestors_length (slides : List SlideNote) (before after : SlideNote) :
    (endSlideAncestors before after slides).1.length = slides.length := by
  unfold endSlideAncestors
  cases before.state <;> cases after.state <;>
    first | rfl | exact ancestors_fuel_length _ _ _

theorem advance_length (ctx : Context) (pre : List SlideNote) (note : SlideNote)
    (rest : List SlideNote) (events : List JudgeEvent) (audio : List AudioCommand)
    (render : List RenderCommand) :
    (advance ctx pre note rest events audio render).slides.length =
      pre.length + 1 + rest.length := by
  unfold advance
  rcases hs : lifecycle ctx note with ⟨after, event, ac, rc⟩
  simp only [updateSlideParentFlags_length, ancestors_length, force_length,
    List.length_append, List.length_cons]
  omega

/-- Exact normalization of the original reverse accumulators. -/
theorem fuel_step (ctx : Context) (fuel : Nat) (pre : List SlideNote)
    (note : SlideNote) (rest : List SlideNote) (events : List JudgeEvent)
    (audio : List AudioCommand) (render : List RenderCommand) :
    fuelReference ctx (fuel + 1) pre.reverse (note :: rest)
        events.reverse audio.reverse render.reverse =
      let next := advance ctx pre note rest events audio render
      fuelReference ctx fuel (next.slides.take next.index).reverse
        (next.slides.drop next.index) next.events.reverse next.audio.reverse
        next.render.reverse := by
  unfold fuelReference
  conv => lhs; unfold processSlideNotesCoreFuel
  change (match lifecycle ctx note with
    | (after, event, ac, rc) =>
      let (forced, dc) := forceFinishDirectParent (pre.reverse.reverse ++ after :: rest) note after
      let (ended, anc) := endSlideAncestors note after forced
      let refreshed := updateSlideParentFlags ended
      processSlideNotesCoreFuel fuel
        (refreshed.take (pre.reverse.length + 1)).reverse
        (refreshed.drop (pre.reverse.length + 1)) ctx.input ctx.currentTime
        ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide
        (match event with | some e => e :: events.reverse | none => events.reverse)
        (ac.reverse ++ audio.reverse)
        (anc.reverse ++ dc.reverse ++ rc.reverse ++ render.reverse)) = _
  rcases hs : lifecycle ctx note with ⟨after, event, ac, rc⟩
  cases event <;>
    simp only [advance, hs, List.reverse_reverse, List.length_reverse,
      Option.toList, List.reverse_append, List.reverse_cons, List.reverse_nil,
      List.nil_append, List.append_nil, List.append_assoc, List.cons_append]

theorem resume_step (ctx : Context) (pre : List SlideNote) (note : SlideNote)
    (rest : List SlideNote) (events : List JudgeEvent) (audio : List AudioCommand)
    (render : List RenderCommand) :
    resume ctx ⟨pre ++ note :: rest, events, audio, render, pre.length⟩ =
      resume ctx (advance ctx pre note rest events audio render) := by
  have hlen := advance_length ctx pre note rest events audio render
  have hfuel : (pre ++ note :: rest).length - pre.length + 1 = (rest.length + 1) + 1 := by
    simp only [List.length_append, List.length_cons]
    omega
  have hnext : (advance ctx pre note rest events audio render).slides.length -
      (advance ctx pre note rest events audio render).index + 1 = rest.length + 1 := by
    change (advance ctx pre note rest events audio render).slides.length -
      (pre.length + 1) + 1 = _
    omega
  unfold resume
  simp only [hfuel, hnext, List.take_left, List.drop_left]
  exact fuel_step ctx (rest.length + 1) pre note rest events audio render

theorem resume_done (ctx : Context) (w : RefWork) (hindex : w.slides.length ≤ w.index) :
    resume ctx w = w.output := by
  have hdrop := List.drop_of_length_le hindex
  have htake := List.take_of_length_le hindex
  simp only [resume, hdrop, htake, fuelReference, processSlideNotesCoreFuel,
    List.reverse_reverse, RefWork.output]

/-- The reference consumes no output. Intermediate append bounds follow from
    the final reference output bound; arbitrary pairs of native vectors are
    NOT assumed always to fit. -/
theorem fuel_output_bounds (ctx : Context) (fuel : Nat)
    (pre pending : List SlideNote) (events : List JudgeEvent)
    (audio : List AudioCommand) (render : List RenderCommand) :
    let out := fuelReference ctx fuel pre.reverse pending
      events.reverse audio.reverse render.reverse
    events.length ≤ out.2.1.length ∧ audio.length ≤ out.2.2.1.length ∧
      render.length ≤ out.2.2.2.length := by
  induction fuel generalizing pre pending events audio render with
  | zero =>
      cases pending <;>
        simp only [fuelReference, processSlideNotesCoreFuel, List.length_reverse,
          Nat.le_refl, and_self]
  | succ fuel ih =>
      cases pending with
      | nil =>
          simp only [fuelReference, processSlideNotesCoreFuel, List.length_reverse,
            Nat.le_refl, and_self]
      | cons note rest =>
          rw [fuel_step]
          let next := advance ctx pre note rest events audio render
          have h := ih (next.slides.take next.index) (next.slides.drop next.index)
            next.events next.audio next.render
          have hgrowth : events.length ≤ next.events.length ∧
              audio.length ≤ next.audio.length ∧ render.length ≤ next.render.length := by
            dsimp only [next, advance]
            rcases hs : lifecycle ctx note with ⟨after, event, ac, rc⟩
            simp only [List.length_append]
            omega
          exact ⟨Nat.le_trans hgrowth.1 h.1,
            Nat.le_trans hgrowth.2.1 h.2.1, Nat.le_trans hgrowth.2.2 h.2.2⟩

theorem resume_output_bounds (ctx : Context) (w : RefWork) :
    w.events.length ≤ (resume ctx w).2.1.length ∧
      w.audio.length ≤ (resume ctx w).2.2.1.length ∧
      w.render.length ≤ (resume ctx w).2.2.2.length :=
  fuel_output_bounds ctx _ _ _ _ _ _

structure Capacity where
  events : Nat
  audio : Nat
  render : Nat

def Fits (cap : Capacity) (out : Outcome) : Prop :=
  out.2.1.length ≤ cap.events ∧ out.2.2.1.length ≤ cap.audio ∧
    out.2.2.2.length ≤ cap.render

theorem advance_fits (ctx : Context) (cap : Capacity) (pre : List SlideNote)
    (note : SlideNote) (rest : List SlideNote) (events : List JudgeEvent)
    (audio : List AudioCommand) (render : List RenderCommand)
    (hfit : Fits cap (resume ctx ⟨pre ++ note :: rest, events, audio, render, pre.length⟩)) :
    Fits cap (advance ctx pre note rest events audio render).output := by
  rw [resume_step] at hfit
  have hbound := resume_output_bounds ctx (advance ctx pre note rest events audio render)
  exact ⟨Nat.le_trans hbound.1 hfit.1, Nat.le_trans hbound.2.1 hfit.2.1,
    Nat.le_trans hbound.2.2 hfit.2.2⟩

/-- Parent effects survive on both sides of the new cursor. This is a split of
    the refreshed list, never of the old prefix or old suffix. -/
theorem refreshed_split (ctx : Context) (pre : List SlideNote) (note : SlideNote)
    (rest : List SlideNote) (events : List JudgeEvent) (audio : List AudioCommand)
    (render : List RenderCommand) :
    let next := advance ctx pre note rest events audio render
    (next.slides.take next.index).reverse.reverse ++ next.slides.drop next.index = next.slides := by
  simp only [List.reverse_reverse, List.take_append_drop]

/-! The following declarations target the installed extracted trait and loop.
    No local Lean stand-in for slide_schedule.process is provided. -/

structure Representation (S N E A R : Type) where
  slides : S → List SlideNote
  note : N → SlideNote
  events : E → List JudgeEvent
  audio : A → List AudioCommand
  render : R → List RenderCommand

def interpret {S N E A R : Type} (rep : Representation S N E A R)
    (w : slide_schedule.SlideWork S E A R) : RefWork :=
  ⟨rep.slides w.slides, rep.events w.events, rep.audio w.audio, rep.render w.render,
    w.index.val⟩

/-- Actual generated loop return layout; this only reconstructs its wrapper
    record and carries no semantic assumption. -/
abbrev LoopResult (S E A R : Type) := S × E × A × R × Usize

def restore {S E A R : Type} (out : LoopResult S E A R) :
    slide_schedule.SlideWork S E A R :=
  ⟨out.1, out.2.1, out.2.2.1, out.2.2.2.1, out.2.2.2.2⟩

/-- Atomic success/representation premises. No field asserts correctness of a
    complete scheduler iteration or traversal. The three parent fields can
    use SchedulerParentsProof.force_finish_spec, end_ancestors_spec and
    refresh_flags_spec after composing their runtime adapters. The lifecycle
    field uses SlideLifecycleProof.step_equiv, as demonstrated above. -/
structure AtomicAgrees {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (ctx : Context) (cap : Capacity) : Prop where
  read : ∀ (slides : S) (index : Usize) (pre : List SlideNote)
    (note : SlideNote) (rest : List SlideNote),
    rep.slides slides = pre ++ note :: rest → pre.length = index.val →
    ∃ result, call.read ops slides index = ok result ∧ rep.note result = note
  step : ∀ (note : N), ∃ after events audio render,
    call.step ops note = ok (after, events, audio, render) ∧
    (rep.note after, rep.events events, rep.audio audio, rep.render render) =
      stepBatches ctx (rep.note note)
  replace : ∀ (slides : S) (index : Usize) (pre : List SlideNote)
    (old : SlideNote) (rest : List SlideNote) (after : N),
    rep.slides slides = pre ++ old :: rest → pre.length = index.val →
    ∃ result, call.replace ops slides index after = ok result ∧
      rep.slides result = pre ++ rep.note after :: rest
  force_direct : ∀ (slides : S) (before after : N), ∃ result commands,
    call.force_direct ops slides before after = ok (result, commands) ∧
    (rep.slides result, rep.render commands) =
      forceFinishDirectParent (rep.slides slides) (rep.note before) (rep.note after)
  end_ancestors : ∀ (slides : S) (before after : N), ∃ result commands,
    call.end_ancestors ops slides before after = ok (result, commands) ∧
    (rep.slides result, rep.render commands) =
      endSlideAncestors (rep.note before) (rep.note after) (rep.slides slides)
  refresh : ∀ (slides : S), ∃ result, call.refresh ops slides = ok result ∧
    rep.slides result = updateSlideParentFlags (rep.slides slides)
  append_events : ∀ (accumulated batch : E),
    (rep.events accumulated).length + (rep.events batch).length ≤ cap.events →
    ∃ result, call.append_events ops accumulated batch = ok result ∧
      rep.events result = rep.events accumulated ++ rep.events batch
  append_audio : ∀ (accumulated batch : A),
    (rep.audio accumulated).length + (rep.audio batch).length ≤ cap.audio →
    ∃ result, call.append_audio ops accumulated batch = ok result ∧
      rep.audio result = rep.audio accumulated ++ rep.audio batch
  append_render : ∀ (accumulated batch : R),
    (rep.render accumulated).length + (rep.render batch).length ≤ cap.render →
    ∃ result, call.append_render ops accumulated batch = ok result ∧
      rep.render result = rep.render accumulated ++ rep.render batch

private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

/-- Prove the extracted body by composing its individual calls. Its output
    retains the whole parent-mutated list and lifecycle/direct/ancestor order.
    The increment is safe even if len is the largest representable Usize. -/
theorem process_body_spec {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap) (len : Usize)
    (w : slide_schedule.SlideWork S E A R) (pre : List SlideNote)
    (note : SlideNote) (rest : List SlideNote)
    (hslides : rep.slides w.slides = pre ++ note :: rest)
    (hindex : pre.length = w.index.val) (hmore : w.index.val < len.val)
    (hfit : Fits cap (advance ctx pre note rest (rep.events w.events)
      (rep.audio w.audio) (rep.render w.render)).output) :
    ∃ next, slide_schedule.process_loop.body call len ops w = ok (.cont next) ∧
      interpret rep next = advance ctx pre note rest (rep.events w.events)
        (rep.audio w.audio) (rep.render w.render) ∧
      next.index.val = w.index.val + 1 := by
  obtain ⟨before, hread, hbefore⟩ := atoms.read w.slides w.index pre note rest hslides hindex
  obtain ⟨after, events, audio, render, hstep, hstepRep⟩ := atoms.step before
  rcases hs : lifecycle ctx note with ⟨newNote, event, ac, rc⟩
  have hcomponents : rep.note after = newNote ∧ rep.events events = event.toList ∧
      rep.audio audio = ac ∧ rep.render render = rc := by
    simpa only [hbefore, stepBatches, hs, Prod.mk.injEq] using hstepRep
  rcases hcomponents with ⟨hafter, hevents, haudio, hrender⟩
  obtain ⟨replaced, hreplace, hreplaced⟩ :=
    atoms.replace w.slides w.index pre note rest after hslides hindex
  rw [hafter] at hreplaced
  obtain ⟨forced, direct, hforce, hforceRep⟩ := atoms.force_direct replaced before after
  have hforceRef : forceFinishDirectParent (pre ++ newNote :: rest) note newNote =
      (rep.slides forced, rep.render direct) := by
    simpa only [hreplaced, hbefore, hafter] using hforceRep.symm
  obtain ⟨ended, ancestors, hend, hendRep⟩ := atoms.end_ancestors forced before after
  have hendRef : endSlideAncestors note newNote (rep.slides forced) =
      (rep.slides ended, rep.render ancestors) := by
    simpa only [hbefore, hafter] using hendRep.symm
  obtain ⟨refreshed, hrefresh, hrefreshed⟩ := atoms.refresh ended
  have hfitParts :
      (rep.events w.events).length + event.toList.length ≤ cap.events ∧
      (rep.audio w.audio).length + ac.length ≤ cap.audio ∧
      (rep.render w.render).length + rc.length + (rep.render direct).length +
        (rep.render ancestors).length ≤ cap.render := by
    simpa only [Fits, RefWork.output, advance, hs, hforceRef, hendRef,
      List.length_append] using hfit
  obtain ⟨render1, hr1, hr1Rep⟩ := atoms.append_render w.render render (by
    rw [hrender]; omega)
  obtain ⟨render2, hr2, hr2Rep⟩ := atoms.append_render render1 direct (by
    rw [hr1Rep, hrender, List.length_append]; omega)
  obtain ⟨render3, hr3, hr3Rep⟩ := atoms.append_render render2 ancestors (by
    rw [hr2Rep, hr1Rep, hrender]
    simpa only [List.length_append] using hfitParts.2.2)
  obtain ⟨events1, he1, he1Rep⟩ := atoms.append_events w.events events (by
    simpa only [hevents] using hfitParts.1)
  obtain ⟨audio1, ha1, ha1Rep⟩ := atoms.append_audio w.audio audio (by
    simpa only [haudio] using hfitParts.2.1)
  obtain ⟨index1, hi1, hi1val⟩ := increment_index w.index len hmore
  let next : slide_schedule.SlideWork S E A R :=
    ⟨refreshed, events1, audio1, render3, index1⟩
  refine ⟨next, ?_, ?_, hi1val⟩
  · have hguard : w.index < len := by simpa using hmore
    simp only [slide_schedule.process_loop.body, hguard, ↓reduceIte, hread,
      hstep, hreplace, hr1, hforce, hr2, hend, hr3, hrefresh, he1, ha1, hi1,
      bind_tc_ok, next]
    change (do
      let replaced ← call.replace ops w.slides w.index after
      let render1 ← call.append_render ops w.render render
      let (forced, direct) ← call.force_direct ops replaced before after
      let render2 ← call.append_render ops render1 direct
      let (ended, ancestors) ← call.end_ancestors ops forced before after
      let render3 ← call.append_render ops render2 ancestors
      let refreshed ← call.refresh ops ended
      let events1 ← call.append_events ops w.events events
      let audio1 ← call.append_audio ops w.audio audio
      ok (.cont ⟨refreshed, events1, audio1, render3, index1⟩) :
      Result (ControlFlow (slide_schedule.SlideWork S E A R) (LoopResult S E A R))) =
      ok (.cont next)
    simp only [hreplace, hr1, hforce, bind_tc_ok]
    change (do
      let render2 ← call.append_render ops render1 direct
      let (ended, ancestors) ← call.end_ancestors ops forced before after
      let render3 ← call.append_render ops render2 ancestors
      let refreshed ← call.refresh ops ended
      let events1 ← call.append_events ops w.events events
      let audio1 ← call.append_audio ops w.audio audio
      ok (.cont ⟨refreshed, events1, audio1, render3, index1⟩) :
      Result (ControlFlow (slide_schedule.SlideWork S E A R) (LoopResult S E A R))) =
      ok (.cont next)
    simp only [hr2, hend, bind_tc_ok]
    change (do
      let render3 ← call.append_render ops render2 ancestors
      let refreshed ← call.refresh ops ended
      let events1 ← call.append_events ops w.events events
      let audio1 ← call.append_audio ops w.audio audio
      ok (.cont ⟨refreshed, events1, audio1, render3, index1⟩) :
      Result (ControlFlow (slide_schedule.SlideWork S E A R) (LoopResult S E A R))) =
      ok (.cont next)
    simp only [hr3, hrefresh, he1, ha1, bind_tc_ok, next]
  · simp only [next, interpret, advance, hs, hforceRef, hendRef, hrefreshed,
      he1Rep, ha1Rep, hr3Rep, hr2Rep, hr1Rep, hevents, haudio, hrender,
      hi1val, hindex]

/-- A ghost history of successful continuations of the ACTUAL extracted body.
    Each recorded index is the slot whose read/lifecycle call occurs in that
    body. This carries no correctness assumption about a whole iteration. -/
inductive BodyHistory {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (len : Usize) (ops : Ops) :
    slide_schedule.SlideWork S E A R → slide_schedule.SlideWork S E A R → List Nat → Prop
  | refl (w) : BodyHistory call len ops w w []
  | extend {initial current next visited}
      (past : BodyHistory call len ops initial current visited)
      (active : current.index.val < len.val)
      (body : slide_schedule.process_loop.body call len ops current = ok (.cont next)) :
      BodyHistory call len ops initial next (visited ++ [current.index.val])

/-- Finite traversal: length is invariant, index only increases, and the
    original reference continuation is invariant. The additional history
    proves exactly the pending index range was stepped, once and in order. -/
theorem process_loop_spec {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap) (len : Usize)
    (initial : slide_schedule.SlideWork S E A R)
    (hlen : (rep.slides initial.slides).length = len.val)
    (hstart : initial.index.val ≤ len.val)
    (hfit : Fits cap (resume ctx (interpret rep initial))) :
    slide_schedule.process_loop call len initial ops ⦃ result =>
      let out := restore result
      out.index.val = len.val ∧
      (interpret rep out).output = resume ctx (interpret rep initial) ∧
      BodyHistory call len ops initial out
        (List.range' initial.index.val (len.val - initial.index.val)) ⦄ := by
  unfold slide_schedule.process_loop
  apply loop.spec_decr_nat
    (fun (w : slide_schedule.SlideWork S E A R) => len.val - w.index.val)
    (fun w => (rep.slides w.slides).length = len.val ∧
      initial.index.val ≤ w.index.val ∧ w.index.val ≤ len.val ∧
      resume ctx (interpret rep w) = resume ctx (interpret rep initial) ∧
      BodyHistory call len ops initial w
        (List.range' initial.index.val (w.index.val - initial.index.val)))
  · intro w hinv
    rcases hinv with ⟨hwlen, hlow, hupp, hresume, history⟩
    by_cases hguard : w.index < len
    · have hmore : w.index.val < len.val := by simpa using hguard
      have hslot : w.index.val < (rep.slides w.slides).length := by omega
      let pre := (rep.slides w.slides).take w.index.val
      let note := (rep.slides w.slides)[w.index.val]'hslot
      let rest := (rep.slides w.slides).drop (w.index.val + 1)
      have hsplit : rep.slides w.slides = pre ++ note :: rest := by
        dsimp only [pre, note, rest]
        rw [← List.drop_eq_getElem_cons hslot]
        exact (List.take_append_drop _ _).symm
      have hpre : pre.length = w.index.val := List.length_take_of_le (Nat.le_of_lt hslot)
      have hinterp : interpret rep w =
          ⟨pre ++ note :: rest, rep.events w.events, rep.audio w.audio,
            rep.render w.render, pre.length⟩ := by
        unfold interpret
        rw [hsplit, hpre]
      have hready := advance_fits ctx cap pre note rest
        (rep.events w.events) (rep.audio w.audio) (rep.render w.render)
        (by rw [← hinterp, hresume]; exact hfit)
      obtain ⟨next, hbody, hnextRep, hnextIndex⟩ :=
        process_body_spec call ops rep ctx cap atoms len w pre note rest hsplit hpre hmore hready
      have hnextLen : (rep.slides next.slides).length = len.val := by
        have h := congrArg (fun w : RefWork => w.slides.length) hnextRep
        dsimp only [interpret] at h
        rw [advance_length] at h
        have htotal := congrArg List.length hsplit
        simp only [List.length_append, List.length_cons] at htotal
        omega
      have hnextResume : resume ctx (interpret rep next) = resume ctx (interpret rep initial) := by
        rw [hnextRep, ← resume_step, ← hinterp, hresume]
      have hcount : next.index.val - initial.index.val =
          (w.index.val - initial.index.val) + 1 := by omega
      have hsum : initial.index.val + (w.index.val - initial.index.val) = w.index.val := by omega
      have hhistory : BodyHistory call len ops initial next
          (List.range' initial.index.val (next.index.val - initial.index.val)) := by
        rw [hcount, List.range'_concat]
        simpa only [Nat.one_mul, hsum] using BodyHistory.extend history hmore hbody
      apply exists_imp_spec
      refine ⟨.cont next, hbody, ?_⟩
      exact ⟨⟨hnextLen, by omega, by omega, hnextResume, hhistory⟩, by omega⟩
    · have hstop : len.val ≤ w.index.val := by simpa using hguard
      have heq : w.index.val = len.val := by omega
      have hdone := resume_done ctx (interpret rep w) (by
        change (rep.slides w.slides).length ≤ w.index.val
        omega)
      apply exists_imp_spec
      refine ⟨.done (w.slides, w.events, w.audio, w.render, w.index),
        ?_, heq, hdone.symm.trans hresume, ?_⟩
      · simp only [slide_schedule.process_loop.body, hguard, ↓reduceIte]
      · simpa only [heq] using history
  · refine ⟨hlen, Nat.le_refl _, hstart, rfl, ?_⟩
    simpa only [Nat.sub_self, List.range'_zero] using
      BodyHistory.refl (call := call) (len := len) (ops := ops) initial

theorem process_spec {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap) (len : Usize)
    (initial : slide_schedule.SlideWork S E A R)
    (hlen : (rep.slides initial.slides).length = len.val)
    (hstart : initial.index.val ≤ len.val)
    (hfit : Fits cap (resume ctx (interpret rep initial))) :
    slide_schedule.process call len initial ops ⦃ out =>
      out.index.val = len.val ∧
      (interpret rep out).output = resume ctx (interpret rep initial) ∧
      BodyHistory call len ops initial out
        (List.range' initial.index.val (len.val - initial.index.val)) ⦄ := by
  obtain ⟨result, hrun, hpost⟩ := spec_imp_exists
    (process_loop_spec call ops rep ctx cap atoms len initial hlen hstart hfit)
  rcases result with ⟨slides, events, audio, render, index⟩
  apply exists_imp_spec
  refine ⟨restore (slides, events, audio, render, index), ?_, hpost⟩
  simp only [slide_schedule.process, hrun, bind_tc_ok, restore]
  rfl

/-- The runtime prepares forward buffers from reverse reference inputs. -/
def initialReference (processedRev pending : List SlideNote)
    (eventsRev : List JudgeEvent) (audioRev : List AudioCommand)
    (renderRev : List RenderCommand) : RefWork :=
  ⟨processedRev.reverse ++ pending, eventsRev.reverse, audioRev.reverse,
    renderRev.reverse, processedRev.length⟩

theorem initial_reference (ctx : Context) (processedRev pending : List SlideNote)
    (eventsRev : List JudgeEvent) (audioRev : List AudioCommand)
    (renderRev : List RenderCommand) :
    resume ctx (initialReference processedRev pending eventsRev audioRev renderRev) =
      Scheduler.processSlideNotesCore processedRev pending ctx.input ctx.currentTime
        ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide eventsRev audioRev renderRev := by
  have htake : (processedRev.reverse ++ pending).take processedRev.length = processedRev.reverse :=
    List.take_left' List.length_reverse
  have hdrop : (processedRev.reverse ++ pending).drop processedRev.length = pending :=
    List.drop_left' List.length_reverse
  have hfuel : (processedRev.reverse ++ pending).length - processedRev.length + 1 =
      pending.length + 1 := by simp only [List.length_append, List.length_reverse, Nat.add_sub_cancel_left]
  simp only [initialReference, resume, hfuel, htake, hdrop, List.reverse_reverse,
    fuelReference, Scheduler.processSlideNotesCore]

/-- Public-wrapper equivalence. Only initial representation and individual
    atomic adapters are premises. The pending-count fuel, list traversal,
    command ordering and all reference splits are proved in this file. -/
theorem process_equiv {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap) (len : Usize)
    (initial : slide_schedule.SlideWork S E A R)
    (processedRev pending : List SlideNote) (eventsRev : List JudgeEvent)
    (audioRev : List AudioCommand) (renderRev : List RenderCommand)
    (hinit : interpret rep initial = initialReference processedRev pending eventsRev audioRev renderRev)
    (hlen : (rep.slides initial.slides).length = len.val)
    (hfit : Fits cap (Scheduler.processSlideNotesCore processedRev pending ctx.input ctx.currentTime
      ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide eventsRev audioRev renderRev)) :
    slide_schedule.process call len initial ops ⦃ out =>
      (interpret rep out).output =
        Scheduler.processSlideNotesCore processedRev pending ctx.input ctx.currentTime
          ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide eventsRev audioRev renderRev ∧
      out.index.val = len.val ∧
      BodyHistory call len ops initial out (List.range' processedRev.length pending.length) ⦄ := by
  have hindex : initial.index.val = processedRev.length := congrArg RefWork.index hinit
  have hlist := congrArg (fun w : RefWork => w.slides.length) hinit
  dsimp only [interpret, initialReference] at hlist
  simp only [List.length_append, List.length_reverse] at hlist
  have hstart : initial.index.val ≤ len.val := by omega
  have hremaining : len.val - initial.index.val = pending.length := by omega
  have hreference : resume ctx (interpret rep initial) =
      Scheduler.processSlideNotesCore processedRev pending ctx.input ctx.currentTime
        ctx.touchPanelOffset ctx.delta ctx.style ctx.subdivide eventsRev audioRev renderRev := by
    rw [hinit, initial_reference]
  obtain ⟨out, hrun, hdone, houtput, hhistory⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms len initial hlen hstart
      (by simpa only [hreference] using hfit))
  apply exists_imp_spec
  refine ⟨out, hrun, ?_, hdone, ?_⟩
  · exact houtput.trans hreference
  · rw [hremaining, hindex] at hhistory
    exact hhistory

/-- A trace of actual body calls contains no earlier prefix slot and no
    duplicate slot, including when parent operations modify earlier slides. -/
theorem process_visits_once {S N E A R Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A R) (ops : Ops)
    (rep : Representation S N E A R) (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap) (len : Usize)
    (initial : slide_schedule.SlideWork S E A R)
    (hlen : (rep.slides initial.slides).length = len.val)
    (hstart : initial.index.val ≤ len.val)
    (hfit : Fits cap (resume ctx (interpret rep initial))) :
    slide_schedule.process call len initial ops ⦃ out =>
      ∃ visited, BodyHistory call len ops initial out visited ∧ visited.Nodup ∧
        (∀ index, index ∈ visited ↔ initial.index.val ≤ index ∧ index < len.val) ∧
        visited.length = len.val - initial.index.val ⦄ := by
  obtain ⟨out, hrun, _, _, hhistory⟩ := spec_imp_exists
    (process_spec call ops rep ctx cap atoms len initial hlen hstart hfit)
  apply exists_imp_spec
  refine ⟨out, hrun, List.range' initial.index.val (len.val - initial.index.val),
    hhistory, List.nodup_range', ?_, List.length_range'⟩
  intro index
  rw [List.mem_range'_1]
  have hsum : initial.index.val + (len.val - initial.index.val) = len.val := by omega
  rw [hsum]

end Verification.SlideScheduleProof
