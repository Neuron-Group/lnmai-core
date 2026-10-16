import Verification.Bridge
import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.SchedulerFrameProof
open LnmaiCore InputModel Lifecycle
open private updateSlideParentFlags touchHoldBodyGroupStatesFromHolds
  processTapNotes processHoldNotes processSlideNotes processTouchNotes processTouchHoldNotes
  foldEventsIntoScore eventsToAudioCommands eventsToRenderCommands touchHoldBodyAudioCmds
  hasStrictMajority groupShareResult registerTouchGroupResult
  from LnmaiCore.Scheduler
open private registerTouchGroupResult.loop from LnmaiCore.Scheduler

/-!
The runtime calls the extracted kernels proved here. The callbacks are explicit
adapter boundaries, not extracted Rust pass bodies: `hstep` must cover every
queue/note iteration and the representation of the complete accumulator; append
hypotheses must cover the Rust vector moves and allocation bounds. The reference
callbacks below invoke the original private scheduler functions. In particular,
the final bridge targets `Scheduler.stepFrame`, not a separate lifecycle model.

Touch-group selection below separately requires the runtime probe's identifier
equality, strict-majority arithmetic, and slice representation. Counts, group
identifiers, sizes, and durations are not narrowed to machine integers.
-/

/-- Order of actual processing, with the updated accumulator at each boundary. -/
def processInOrder {S : Type} (step : scheduler.FramePhase → S → S) (s : S) : S :=
  step .TouchHold (step .Touch (step .Slide (step .Hold (step .Tap s))))

/-- A representation-changing refinement, so runtime payloads need not be Lean
    lists or `GameState`. Pass correctness is a hypothesis, not a conclusion. -/
theorem process_frame_refines {S R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (f : F) (repr : R → S) (step : scheduler.FramePhase → S → S) (initial : R)
    (hstep : ∀ phase state, ∃ next, call.call f (phase, state) = ok next ∧
      repr next = step phase (repr state)) :
    ∃ out, scheduler.process_frame call initial f = ok out ∧
      repr out = processInOrder step (repr initial) := by
  obtain ⟨tap, ht, hrt⟩ := hstep .Tap initial
  obtain ⟨hold, hh, hrh⟩ := hstep .Hold tap
  obtain ⟨slide, hs, hrs⟩ := hstep .Slide hold
  obtain ⟨touch, hu, hru⟩ := hstep .Touch slide
  obtain ⟨touchHold, hv, hrv⟩ := hstep .TouchHold touch
  refine ⟨touchHold, ?_, ?_⟩
  · simp [scheduler.process_frame, ht, hh, hs, hu, hv, bind_tc_ok]
  · simp [processInOrder, hrv, hru, hrs, hrh, hrt]

/-- Reported event order is tap, hold, touch, touch-hold, slide. The same batch
    is used by the runtime for reporting, scoring, and judgment commands. -/
theorem combine_events_refines {E T F : Type}
    (call : core.ops.function.Fn F (E × E) E) (f : F) (repr : E → List T)
    (events : scheduler.FrameEvents E)
    (happend : ∀ a b, ∃ out, call.call f (a, b) = ok out ∧
      repr out = repr a ++ repr b) :
    ∃ out, scheduler.combine_events call events f = ok out ∧
      repr out = repr events.tap ++ repr events.hold ++ repr events.touch ++
        repr events.touch_hold ++ repr events.slide := by
  obtain ⟨a, ha, hra⟩ := happend events.tap events.hold
  obtain ⟨b, hb, hrb⟩ := happend a events.touch
  obtain ⟨c, hc, hrc⟩ := happend b events.touch_hold
  obtain ⟨d, hd, hrd⟩ := happend c events.slide
  refine ⟨d, ?_, ?_⟩
  · simp [scheduler.combine_events, ha, hb, hc, hd, bind_tc_ok]
  · simp [hrd, hrc, hrb, hra, List.append_assoc]

/-- Commands retain both inter-batch and intra-batch order, conditional on the
    two concrete append adapters. Producing each batch is a separate obligation. -/
theorem combine_commands_refines {A R TA TR FA FR : Type}
    (audioCall : core.ops.function.Fn FA (A × A) A)
    (renderCall : core.ops.function.Fn FR (R × R) R)
    (fa : FA) (fr : FR) (audioRepr : A → List TA) (renderRepr : R → List TR)
    (slideAudio bodyAudio eventAudio : A) (slideRender eventRender : R)
    (ha : ∀ a b, ∃ out, audioCall.call fa (a, b) = ok out ∧
      audioRepr out = audioRepr a ++ audioRepr b)
    (hr : ∀ a b, ∃ out, renderCall.call fr (a, b) = ok out ∧
      renderRepr out = renderRepr a ++ renderRepr b) :
    ∃ audio render,
      scheduler.combine_commands audioCall renderCall slideAudio bodyAudio eventAudio
        slideRender eventRender fa fr = ok (audio, render) ∧
      audioRepr audio = audioRepr slideAudio ++ audioRepr bodyAudio ++ audioRepr eventAudio ∧
      renderRepr render = renderRepr slideRender ++ renderRepr eventRender := by
  obtain ⟨a, ha1, hra1⟩ := ha slideAudio bodyAudio
  obtain ⟨b, ha2, hra2⟩ := ha a eventAudio
  obtain ⟨r, hr1, hrr1⟩ := hr slideRender eventRender
  refine ⟨b, r, ?_, ?_, hrr1⟩
  · simp [scheduler.combine_commands, ha1, ha2, hr1, bind_tc_ok]
  · simp [hra2, hra1, List.append_assoc]

/-- Interpretation of the runtime's `FrameWork`; no pass state is discarded. -/
structure FrameWork where
  state : GameState
  cursor : Scheduler.ClickCursor
  events : scheduler.FrameEvents (List JudgeEvent)
  slideAudio : List AudioCommand
  slideRender : List RenderCommand

def initialFrame (st : GameState) (input : FrameInput) : FrameWork :=
  { state := { st with
      currentTime := st.currentTime + input.delta
      slides := updateSlideParentFlags st.slides
      touchHoldGroupStates := if st.touchHoldGroupStates.isEmpty then
          touchHoldBodyGroupStatesFromHolds st.activeTouchHolds else st.touchHoldGroupStates }
    cursor := {}
    events := { tap := [], hold := [], slide := [], touch := [], touch_hold := [] }
    slideAudio := []
    slideRender := [] }

/-- These are the original scheduler passes, arranged exactly as in the Rust
    callback `process_frame_phase`. Proving that callback requires the pass
    adapters, including all note/queue iteration and storage invariants. -/
def referencePass (input : FrameInput) (phase : scheduler.FramePhase)
    (frame : FrameWork) : FrameWork :=
  let st := frame.state
  match phase with
  | .Tap =>
      let (frontiers, queues, events, cursor) := processTapNotes
        st.buttonQueueFrontiers st.tapQueues input st.currentTime st.touchPanelOffset
        st.judgeStyle frame.cursor
      { frame with
        state := { st with buttonQueueFrontiers := frontiers, tapQueues := queues }
        cursor := cursor
        events := { frame.events with tap := events } }
  | .Hold =>
      let (frontiers, queues, holds, events, cursor) := processHoldNotes
        st.buttonQueueFrontiers st.holdQueues st.activeHolds input st.currentTime input.delta
        st.judgeStyle st.touchPanelOffset st.prevSensor frame.cursor
      { frame with
        state := { st with
          buttonQueueFrontiers := frontiers
          holdQueues := queues
          activeHolds := holds }
        cursor := cursor
        events := { frame.events with hold := events } }
  | .Slide =>
      let (slides, events, audio, render) := processSlideNotes st.slides input st.currentTime
        st.touchPanelOffset input.delta st.judgeStyle st.subdivideSlideJudgeGrade
      { frame with
        state := { st with slides := slides }
        events := { frame.events with slide := events }
        slideAudio := audio
        slideRender := render }
  | .Touch =>
      let (frontiers, queues, events, cursor, groups) := processTouchNotes
        st.touchQueueFrontiers st.touchQueues input st.currentTime st.judgeStyle frame.cursor
        st.touchPanelOffset st.buttonRingForTouch st.touchGroupStates
      { frame with
        state := { st with
          touchQueueFrontiers := frontiers
          touchQueues := queues
          touchGroupStates := groups }
        cursor := cursor
        events := { frame.events with touch := events } }
  | .TouchHold =>
      let (frontiers, queues, holds, events, cursor, groups, bodyGroups) :=
        processTouchHoldNotes st.touchQueueFrontiers st.touchHoldQueues st.activeTouchHolds
          input st.currentTime input.delta st.judgeStyle st.touchPanelOffset
          st.buttonRingForTouch frame.cursor st.touchGroupStates st.touchHoldGroupStates
      { frame with
        state := { st with
          touchQueueFrontiers := frontiers
          touchHoldQueues := queues
          activeTouchHolds := holds
          touchGroupStates := groups
          touchHoldGroupStates := bodyGroups }
        cursor := cursor
        events := { frame.events with touch_hold := events } }

def reportedEvents (frame : FrameWork) : List JudgeEvent :=
  frame.events.tap ++ frame.events.hold ++ frame.events.touch ++
    frame.events.touch_hold ++ frame.events.slide

/-- Reference interpretation of runtime finalization. The append kernels above
    establish its batch order. Score folding, body audio, event conversion, and
    final record/vector representation still need their runtime adapter proofs. -/
def finishFrame (input : FrameInput) (frame : FrameWork) :
    GameState × List JudgeEvent × List AudioCommand × List RenderCommand :=
  let st := frame.state
  let events := reportedEvents frame
  let score := foldEventsIntoScore st.noteFastLateDisplay st.breakFastLateDisplay st.score events
  let audio := frame.slideAudio ++
    touchHoldBodyAudioCmds st.activeTouchHolds st.currentTime input st.touchHoldGroupStates ++
    eventsToAudioCommands events st.currentTime
  let render := frame.slideRender ++ eventsToRenderCommands st.displayHoldHeadJudgeResult events
  ({ st with prevButton := input.buttonHeld, prevSensor := input.sensorHeld, score := score },
    events, audio, render)

/-- Repacking the *original* passes is definitionally the original `stepFrame`,
    including the touch pass's updated groups being fed to touch-hold heads. -/
theorem reference_frame_equiv (st : GameState) (input : FrameInput) :
    finishFrame input (processInOrder (referencePass input) (initialFrame st input)) =
      Scheduler.stepFrame st input := by
  simp only [finishFrame, processInOrder, referencePass, initialFrame,
    reportedEvents, Scheduler.stepFrame]

/-- Compositional bridge to the original scheduler. This proves the extracted
    driver's sequencing under `hstep` and `hinitial`; it does not discharge the
    runtime pass/initialization/finalization adapter obligations. -/
theorem process_frame_to_stepFrame {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R) (f : F)
    (repr : R → FrameWork) (initial : R) (st : GameState) (input : FrameInput)
    (hinitial : repr initial = initialFrame st input)
    (hstep : ∀ phase state, ∃ next, call.call f (phase, state) = ok next ∧
      repr next = referencePass input phase (repr state)) :
    ∃ out, scheduler.process_frame call initial f = ok out ∧
      finishFrame input (repr out) = Scheduler.stepFrame st input := by
  obtain ⟨out, hout, hrepr⟩ :=
    process_frame_refines call f repr (referencePass input) initial hstep
  refine ⟨out, hout, ?_⟩
  rw [hrepr, hinitial]
  exact reference_frame_equiv st input

/-- Every group already passed by the loop has a different identifier. No
    uniqueness assumption is made about the full group list. -/
def NoGroup (groups : List GroupState) (groupId : Nat) : Prop :=
  ∀ group ∈ groups, group.groupId ≠ groupId

/-- Interpretation of the actual runtime probe, including short-circuiting
    the majority calculation on nonmatching groups. -/
def groupProbe (group : GroupState) (groupId : Nat) : scheduler.TouchGroupProbe :=
  { matches_group := group.groupId == groupId
    has_majority := (group.groupId == groupId) &&
      hasStrictMajority group.count group.size }

/-- The callback obligation addresses exactly the element at `index`. The
    decomposition exposes both index validity and the slice/list boundary;
    identifier equality and majority arithmetic remain runtime obligations. -/
def ProbeAgrees {F : Type}
    (call : core.ops.function.Fn F Usize scheduler.TouchGroupProbe) (probe : F)
    (groups : List GroupState) (groupId : Nat) : Prop :=
  ∀ (index : Usize) (pref : List GroupState) (group : GroupState)
    (suffix : List GroupState),
    groups = pref ++ group :: suffix → pref.length = index.val →
    call.call probe index = ok (groupProbe group groupId)

/-- An update selects the first match, and its flag records the majority
    *before* the count/size update. Append is possible exactly when no ID matches. -/
def FirstGroupPlan (groups : List GroupState) (groupId : Nat) :
    scheduler.TouchGroupAction → Prop
  | .Append => NoGroup groups groupId
  | .Update index keep =>
      ∃ (pref : List GroupState) (group : GroupState) (suffix : List GroupState),
        groups = pref ++ group :: suffix ∧ pref.length = index.val ∧
        NoGroup pref groupId ∧ group.groupId = groupId ∧
        keep = hasStrictMajority group.count group.size

/-- The extracted bounded loop terminates without index overflow and selects
    precisely the first matching group. Only the probe/length representations
    are assumed; the search, its bound, and first-match behavior are proved here. -/
theorem touch_group_plan_spec {F : Type}
    (call : core.ops.function.Fn F Usize scheduler.TouchGroupProbe) (probe : F)
    (len : Usize) (groups : List GroupState) (groupId : Nat)
    (hlen : len.val = groups.length)
    (hprobe : ProbeAgrees call probe groups groupId) :
    scheduler.touch_group_plan call len probe ⦃ FirstGroupPlan groups groupId ⦄ := by
  unfold scheduler.touch_group_plan scheduler.touch_group_plan_loop
  apply loop.spec_decr_nat (fun index => len.val - index.val)
    (fun index => ∃ pref rest : List GroupState,
      groups = pref ++ rest ∧ pref.length = index.val ∧ NoGroup pref groupId)
  · rintro index ⟨pref, rest, hgroups, hindex, hprefix⟩
    have hlength : len.val = pref.length + rest.length := by
      rw [hlen, hgroups, List.length_append]
    unfold scheduler.touch_group_plan_loop.body
    by_cases hmore : index < len
    · simp only [hmore, ↓reduceIte]
      have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil =>
          simp only [List.length_nil, Nat.add_zero] at hlength
          omega
      | cons group suffix =>
          rw [hprobe index pref group suffix hgroups hindex]
          simp only [bind_tc_ok]
          by_cases hmatch : group.groupId = groupId
          · have hdone : FirstGroupPlan groups groupId
                (.Update index (hasStrictMajority group.count group.size)) :=
              ⟨pref, group, suffix, hgroups, hindex, hprefix, hmatch, rfl⟩
            simpa [groupProbe, hmatch] using hdone
          · have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
            obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
              (UScalar.add_spec (x := index) (y := 1#usize) hbound)
            have hsucc : next.val = index.val + 1 := by simpa using hnextVal
            have hcontinue :
                (∃ before after : List GroupState, groups = before ++ after ∧
                  before.length = next.val ∧ NoGroup before groupId) ∧
                len.val - next.val < len.val - index.val := by
              constructor
              · refine ⟨pref ++ [group], suffix, ?_, ?_, ?_⟩
                · simpa only [List.append_assoc, List.singleton_append] using hgroups
                · simp [List.length_append, hindex, hsucc]
                · intro previous hmem
                  rcases List.mem_append.mp hmem with hmem | hmem
                  · exact hprefix previous hmem
                  · have heq : previous = group := by simpa using hmem
                    simpa only [heq] using hmatch
              · omega
            simpa [groupProbe, hmatch, hnext, bind_tc_ok] using hcontinue
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head tail => simp only [List.length_cons] at hlength; omega
      have hnone : NoGroup groups groupId := by
        rw [hgroups, hempty, List.append_nil]
        exact hprefix
      simpa [hmore, FirstGroupPlan] using hnone
  · exact ⟨[], groups, by simp, by simp, by simp [NoGroup]⟩

/-- In particular, a selected slot is valid for the runtime's indexed read/write. -/
theorem first_group_plan_index_bound (groups : List GroupState) (groupId : Nat)
    (index : Usize) (keep : Bool)
    (hplan : FirstGroupPlan groups groupId (.Update index keep)) :
    index.val < groups.length := by
  obtain ⟨pref, group, suffix, hgroups, hindex, _, _, _⟩ := hplan
  simp only [hgroups, List.length_append, List.length_cons]
  omega

/-- Interpretation of `group_share_result` after the extracted plan. The
    successful-plan theorem rules out the optional lookup's invalid-index case. -/
def shareFromPlan (groups : List GroupState) :
    scheduler.TouchGroupAction → Option (JudgeGrade × Duration)
  | .Append => none
  | .Update index keep =>
      if keep then (groups[index.val]?).map (fun group => (group.grade, group.diff))
      else none

/-- Interpretation of the runtime's field updates, retaining the *old* flag.
    The runtime's Natural addition, cloning, and field representation are adapters. -/
def registeredGroup (group : GroupState) (groupSize : Nat) (grade : JudgeGrade)
    (diff : Duration) (keep : Bool) : GroupState :=
  if keep then { group with count := group.count + 1, size := groupSize }
  else { group with count := group.count + 1, size := groupSize, grade := grade, diff := diff }

/-- List interpretation of the runtime clone/index-mutate/append adapter. -/
def registerFromPlan (groups : List GroupState) (groupId groupSize : Nat)
    (grade : JudgeGrade) (diff : Duration) : scheduler.TouchGroupAction → List GroupState
  | .Append => groups ++
      [{ groupId := groupId, count := 1, size := groupSize, grade := grade, diff := diff }]
  | .Update index keep =>
      groups.modify index.val (fun group => registeredGroup group groupSize grade diff keep)

theorem selected_group_get (pref : List GroupState) (group : GroupState)
    (suffix : List GroupState) :
    (pref ++ group :: suffix)[pref.length]? = some group := by
  induction pref with
  | nil => rfl
  | cons previous pref ih =>
      simpa only [List.cons_append, List.length_cons, List.getElem?_cons_succ] using ih

theorem selected_group_modify (pref : List GroupState) (group : GroupState)
    (suffix : List GroupState) (f : GroupState → GroupState) :
    (pref ++ group :: suffix).modify pref.length f = pref ++ f group :: suffix := by
  induction pref with
  | nil => rfl
  | cons previous pref ih =>
      simp only [List.cons_append, List.length_cons, List.modify_succ_cons, ih]

/-- Skip exactly an unmatched pref in the original reference's lookup. -/
theorem find_group_skip (pref rest : List GroupState) (groupId : Nat)
    (hprefix : NoGroup pref groupId) :
    (pref ++ rest).find? (fun group => group.groupId == groupId) =
      rest.find? (fun group => group.groupId == groupId) := by
  revert hprefix
  induction pref with
  | nil => intro _; rfl
  | cons previous pref ih =>
      intro hprefix
      have hhead : previous.groupId ≠ groupId := hprefix previous (by simp)
      have htail : NoGroup pref groupId :=
        fun group hmem => hprefix group (by simp [hmem])
      simpa [List.find?_cons, hhead] using ih htail

/-- Skip exactly an unmatched pref in the original reference's registration. -/
theorem register_group_skip (pref rest : List GroupState)
    (groupId groupSize : Nat) (grade : JudgeGrade) (diff : Duration)
    (hprefix : NoGroup pref groupId) :
    registerTouchGroupResult (pref ++ rest) groupId groupSize grade diff =
      pref ++ registerTouchGroupResult rest groupId groupSize grade diff := by
  revert hprefix
  induction pref with
  | nil => intro _; rfl
  | cons previous pref ih =>
      intro hprefix
      have hhead : previous.groupId ≠ groupId := hprefix previous (by simp)
      have htail : NoGroup pref groupId :=
        fun group hmem => hprefix group (by simp [hmem])
      simpa [registerTouchGroupResult, registerTouchGroupResult.loop, hhead] using
        congrArg (List.cons previous) (ih htail)

/-- The plan interpretation agrees with the original `groupShareResult`, even
    when later duplicate groups have a majority but the first match does not. -/
theorem share_plan_equiv (groups : List GroupState) (groupId : Nat)
    (action : scheduler.TouchGroupAction) (hplan : FirstGroupPlan groups groupId action) :
    shareFromPlan groups action = groupShareResult groups groupId := by
  cases action with
  | Append =>
      have hfind : groups.find? (fun group => group.groupId == groupId) = none := by
        apply List.find?_eq_none.mpr
        intro group hmem
        simpa using hplan group hmem
      simp [shareFromPlan, groupShareResult, hfind]
  | Update index keep =>
      obtain ⟨pref, group, suffix, hgroups, hindex, hprefix, hmatch, hkeep⟩ := hplan
      have hfind : groups.find? (fun entry => entry.groupId == groupId) = some group := by
        rw [hgroups, find_group_skip pref (group :: suffix) groupId hprefix]
        simp [hmatch]
      have hget : groups[index.val]? = some group := by
        rw [hgroups, ← hindex]
        exact selected_group_get pref group suffix
      simp [shareFromPlan, groupShareResult, hfind, hget, hkeep]

/-- The original recursive registration equals the runtime plan's indexed
    mutation or append. Prefix and suffix are preserved, including duplicates. -/
theorem register_plan_equiv (groups : List GroupState) (groupId groupSize : Nat)
    (grade : JudgeGrade) (diff : Duration) (action : scheduler.TouchGroupAction)
    (hplan : FirstGroupPlan groups groupId action) :
    registerFromPlan groups groupId groupSize grade diff action =
      registerTouchGroupResult groups groupId groupSize grade diff := by
  cases action with
  | Append =>
      have hskip := register_group_skip groups [] groupId groupSize grade diff hplan
      simpa [registerFromPlan, registerTouchGroupResult, registerTouchGroupResult.loop] using
        hskip.symm
  | Update index keep =>
      obtain ⟨pref, group, suffix, hgroups, hindex, hprefix, hmatch, hkeep⟩ := hplan
      simp only [registerFromPlan]
      rw [hgroups, ← hindex, selected_group_modify,
        register_group_skip pref (group :: suffix) groupId groupSize grade diff hprefix]
      simp [registeredGroup, registerTouchGroupResult, registerTouchGroupResult.loop, hmatch, hkeep]

/-- Combined extracted-loop/original-reference result. The same selected slot
    supports both sharing and every registration payload. These equalities
    concern the documented list adapters, not unextracted Rust memory operations. -/
theorem touch_group_plan_to_reference {F : Type}
    (call : core.ops.function.Fn F Usize scheduler.TouchGroupProbe) (probe : F)
    (len : Usize) (groups : List GroupState) (groupId : Nat)
    (hlen : len.val = groups.length) (hprobe : ProbeAgrees call probe groups groupId) :
    ∃ action, scheduler.touch_group_plan call len probe = ok action ∧
      FirstGroupPlan groups groupId action ∧
      shareFromPlan groups action = groupShareResult groups groupId ∧
      ∀ groupSize grade diff, registerFromPlan groups groupId groupSize grade diff action =
        registerTouchGroupResult groups groupId groupSize grade diff := by
  obtain ⟨action, haction, hplan⟩ := spec_imp_exists
    (touch_group_plan_spec call probe len groups groupId hlen hprobe)
  exact ⟨action, haction, hplan, share_plan_equiv groups groupId action hplan,
    fun groupSize grade diff => register_plan_equiv groups groupId groupSize grade diff action hplan⟩

/-!
Remaining runtime obligations (not claims established by this module):

* `process_frame_phase` versus `referencePass`: every note/queue iteration,
  iteration fuel, cursor/frontier ownership, storage order, and lifecycle result.
* `initialFrame`: time arithmetic, clone/record representation, cursor defaults,
  slide-parent resolution, and touch-hold body-group reconstruction.
* `ProbeAgrees`/`hlen`: runtime slice/list representation and index access,
  Natural identifier equality and strict-majority arithmetic.
* `shareFromPlan`/`registerFromPlan`: clone preservation, indexed read/write,
  unbounded count increment, size/result assignments, and append capacity.
* The append callback hypotheses in `combine_events_refines` and
  `combine_commands_refines`: Vec moves/append, allocation bounds, and batch
  representation for the actual callbacks and all intermediate buffers.
* `finishFrame`: score fold, event-to-audio/render conversion, body-audio
  selection/order, and final state/output representations.
* Connected slide-parent effects and slide lifecycle remain separate adapters.

See SOURCE_AUDIT.md for checked extraction versions and axiom-audit evidence.
These adapter contracts are not a whole-runtime equivalence claim.
-/

end Verification.SchedulerFrameProof
