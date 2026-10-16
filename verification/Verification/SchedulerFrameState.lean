import Verification.SchedulerFrameFinalization

/-!
The runtime finalizer reads score/display settings from the original state and
uses its separately computed `new_time`. The existing interpreted finalizer
reads these values from the processed frame. Only the five relevant fields are
projected here: preservation follows from the actual reference pass updates,
without assuming equality of the complete pre/post-pass state.

The extracted driver still requires explicit initialization and per-pass
contracts. Rust records, Vec allocation/movement, cloning, time representation
and addition, and command batch generation remain facade/representation
obligations. The score fold retains its integer-operation/encoding contracts
and the counter-storage boundaries documented in RuntimeScoreJudgments.
-/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.SchedulerFrameStateProof
open LnmaiCore InputModel SchedulerFrameProof SchedulerFrameFinalizationProof
open private eventsToAudioCommands eventsToRenderCommands touchHoldBodyAudioCmds
  from LnmaiCore.Scheduler

/-- Exactly the fields whose sources differ between the two finalizers. Score
is preserved during the passes, then replaced by the final score fold. -/
structure FinalizationSources where
  noteFastLateDisplay : JudgeDisplayOption
  breakFastLateDisplay : JudgeDisplayOption
  score : ScoreState
  displayHoldHeadJudgeResult : Bool
  currentTime : TimePoint

def sources (state : GameState) : FinalizationSources :=
  { noteFastLateDisplay := state.noteFastLateDisplay
    breakFastLateDisplay := state.breakFastLateDisplay
    score := state.score
    displayHoldHeadJudgeResult := state.displayHoldHeadJudgeResult
    currentTime := state.currentTime }

/-- All five phases preserve these fields, for every accumulator, independently
of the values produced by the note/queue operations. -/
theorem reference_pass_sources (input : FrameInput) (phase : scheduler.FramePhase)
    (frame : FrameWork) :
    sources (referencePass input phase frame).state = sources frame.state := by
  cases phase <;> rfl

theorem process_in_order_sources (input : FrameInput) (frame : FrameWork) :
    sources (processInOrder (referencePass input) frame).state = sources frame.state := by
  simp only [processInOrder, reference_pass_sources]

/-- Initialization advances time and preserves the original score/settings.
Slide-parent resolution and touch-hold group initialization do not affect them. -/
theorem initial_frame_sources (state : GameState) (input : FrameInput) :
    sources (initialFrame state input).state =
      { sources state with currentTime := state.currentTime + input.delta } := by
  rfl

theorem processed_frame_sources (state : GameState) (input : FrameInput) :
    sources (processInOrder (referencePass input) (initialFrame state input)).state =
      { sources state with currentTime := state.currentTime + input.delta } := by
  rw [process_in_order_sources, initial_frame_sources]

/-- Export the same invariant at the extracted driver's output. The pass
contract remains explicit; the source-field invariant is derived from it. -/
theorem process_frame_sources {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (f : F) (repr : R → FrameWork) (initial : R)
    (state : GameState) (input : FrameInput)
    (hinitial : repr initial = initialFrame state input)
    (hstep : ∀ phase frame, ∃ next, call.call f (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame)) :
    ∃ out, scheduler.process_frame call initial f = ok out ∧
      sources (repr out).state =
        { sources state with currentTime := state.currentTime + input.delta } := by
  obtain ⟨out, hout, hrepr⟩ :=
    process_frame_refines call f repr (referencePass input) initial hstep
  refine ⟨out, hout, ?_⟩
  rw [hrepr, hinitial]
  exact processed_frame_sources state input

abbrev FrameOutput := GameState × List JudgeEvent × List AudioCommand × List RenderCommand

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → Int) (encode : Int → I)
  (h : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (hencode : ∀ value, decode (encode value) = value)

/-- Match the sources in Rust `step_frame`: score/display settings come from
`state`, timestamps from `newTime`, and body holds/groups plus slide commands
from the final frame. The ordering kernels are extracted calls; scoring uses
RuntimeScoreJudgments' composed fold. Command generators, counter storage and
list representation retain their existing interpretation boundaries. -/
def finishFromInitial (state : GameState) (newTime : TimePoint)
    (input : FrameInput) (frame : FrameWork) : Result FrameOutput := do
  let events ← scheduler.combine_events appendCall frame.events ()
  let score ← RuntimeScoreJudgmentsProof.foldEvents inst ops encode
    state.noteFastLateDisplay state.breakFastLateDisplay state.score events
  let (audio, render) ← scheduler.combine_commands appendCall appendCall
    frame.slideAudio
    (touchHoldBodyAudioCmds frame.state.activeTouchHolds newTime input
      frame.state.touchHoldGroupStates)
    (eventsToAudioCommands events newTime) frame.slideRender
    (eventsToRenderCommands state.displayHoldHeadJudgeResult events) () ()
  ok ({ frame.state with
      prevButton := input.buttonHeld
      prevSensor := input.sensorHeld
      score := score }, events, audio, render)

/-- Source substitution needs only these five equalities, not equality of the
whole state and not an assumed score-fold/finalization equivalence. -/
theorem finish_from_initial_eq_finish (state : GameState) (newTime : TimePoint)
    (input : FrameInput) (frame : FrameWork)
    (hsources : sources frame.state = { sources state with currentTime := newTime }) :
    finishFromInitial inst ops encode state newTime input frame =
      SchedulerFrameFinalizationProof.finish inst ops encode input frame := by
  have hn : frame.state.noteFastLateDisplay = state.noteFastLateDisplay :=
    congrArg FinalizationSources.noteFastLateDisplay hsources
  have hb : frame.state.breakFastLateDisplay = state.breakFastLateDisplay :=
    congrArg FinalizationSources.breakFastLateDisplay hsources
  have hs : frame.state.score = state.score :=
    congrArg FinalizationSources.score hsources
  have hd : frame.state.displayHoldHeadJudgeResult = state.displayHoldHeadJudgeResult :=
    congrArg FinalizationSources.displayHoldHeadJudgeResult hsources
  have ht : frame.state.currentTime = newTime :=
    congrArg FinalizationSources.currentTime hsources
  simp only [finishFromInitial, SchedulerFrameFinalizationProof.finish, hn, hb, hs, hd, ht]

/-- Use the original state even after the extracted driver returns its updated
accumulator. Agreement of Rust time arithmetic/initialization with this represented
time is retained in the concrete facade and initialization obligations. -/
def processAndFinishFromInitial {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (f : F) (repr : R → FrameWork) (initial : R)
    (state : GameState) (input : FrameInput) : Result FrameOutput := do
  let frame ← scheduler.process_frame call initial f
  finishFromInitial inst ops encode state (state.currentTime + input.delta) input (repr frame)

/-- Keep every full frame output and the final persistent state, including the
initial state when there are no inputs. The reference is the public stepFrame. -/
def referenceTrace : GameState → List FrameInput → GameState × List FrameOutput
  | state, [] => (state, [])
  | state, input :: inputs =>
      let out := Scheduler.stepFrame state input
      let rest := referenceTrace out.1 inputs
      (rest.1, out :: rest.2)

/-- Reinitialize from each preceding output state. The callback value can
capture the current input; no pass/initializer correctness is built into this
definition. All extracted driver/finalizer failures propagate. -/
def traceFromInitial {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (callback : FrameInput → F) (repr : R → FrameWork)
    (initializeFrame : GameState → FrameInput → R) :
    GameState → List FrameInput → Result (GameState × List FrameOutput)
  | state, [] => ok (state, [])
  | state, input :: inputs => do
      let out ← processAndFinishFromInitial inst ops encode call (callback input) repr
        (initializeFrame state input) state input
      let rest ← traceFromInitial call callback repr initializeFrame out.1 inputs
      ok (rest.1, out :: rest.2)

include h hencode

/-- The reference five-pass composition satisfies the source invariant without
any additional state-equality hypothesis. Its runtime-style finalization gives
the complete original frame result. -/
theorem reference_finish_from_initial_equiv (state : GameState) (input : FrameInput) :
    finishFromInitial inst ops encode state (state.currentTime + input.delta) input
      (processInOrder (referencePass input) (initialFrame state input)) =
      ok (Scheduler.stepFrame state input) := by
  rw [finish_from_initial_eq_finish inst ops encode state (state.currentTime + input.delta)
    input _ (processed_frame_sources state input)]
  rw [SchedulerFrameFinalizationProof.finish_equiv inst ops decode encode h hencode,
    reference_frame_equiv]

/-- Runtime-style source selection plus the extracted five-phase driver agrees
with stepFrame under the original explicit pass/initialization contracts. -/
theorem process_and_finish_from_initial_equiv {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (f : F) (repr : R → FrameWork) (initial : R)
    (state : GameState) (input : FrameInput)
    (hinitial : repr initial = initialFrame state input)
    (hstep : ∀ phase frame, ∃ next, call.call f (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame)) :
    processAndFinishFromInitial inst ops encode call f repr initial state input =
      ok (Scheduler.stepFrame state input) := by
  obtain ⟨out, hout, hrepr⟩ :=
    process_frame_refines call f repr (referencePass input) initial hstep
  simp only [processAndFinishFromInitial, hout, bind_tc_ok, hrepr, hinitial]
  exact reference_finish_from_initial_equiv inst ops decode encode h hencode state input

/-- Arbitrary finite frame histories agree in every state/event/audio/render
observation and the final state. Initialization and each single-phase hstep
are still hypotheses for every frame; this is their composition, not a proof
of the concrete runtime pass bodies or of an unextracted Rust frame loop. -/
theorem trace_from_initial_equiv {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (callback : FrameInput → F) (repr : R → FrameWork)
    (initializeFrame : GameState → FrameInput → R)
    (hinitial : ∀ state input, repr (initializeFrame state input) = initialFrame state input)
    (hstep : ∀ input phase frame, ∃ next,
      call.call (callback input) (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame))
    (state : GameState) (inputs : List FrameInput) :
    traceFromInitial inst ops encode call callback repr initializeFrame state inputs =
      ok (referenceTrace state inputs) := by
  induction inputs generalizing state with
  | nil => rfl
  | cons input inputs ih =>
      simp only [traceFromInitial,
        process_and_finish_from_initial_equiv inst ops decode encode h hencode call
          (callback input) repr (initializeFrame state input) state input
          (hinitial state input) (hstep input),
        bind_tc_ok, ih, referenceTrace]

end Verification.SchedulerFrameStateProof
