import Verification.FrameCommandFinalization
import Verification.SchedulerFrameState

/-! Match the actual Rust finalizer sources: original score/settings and the
new timestamp, with final active holds and slide commands. The extracted
selectors/collector replace all three source command generators. Record and
unbounded numeric representation remain explicit boundaries. -/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.FrameCommandStateProof
open LnmaiCore InputModel SchedulerFrameProof SchedulerFrameStateProof
open SchedulerFrameFinalizationProof FrameCommandsProof
open FrameCommandFinalizationProof (Buffers)

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → Int) (encode : Int → I)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (decode_encode : ∀ value, decode (encode value) = value)

def finishFromInitial (state : GameState) (newTime : TimePoint)
    (input : FrameInput) (frame : FrameWork) (buffers : Buffers frame) :
    Result FrameOutput := do
  let events ← scheduler.combine_events appendCall frame.events ()
  let score ← RuntimeScoreJudgmentsProof.foldEvents inst ops encode
    state.noteFastLateDisplay state.breakFastLateDisplay state.score events
  let body ← collectSlice buffers.holds
    (bodyRun newTime input frame.state.touchHoldGroupStates)
  let eventAudio ← collectSlice buffers.events (audioRun newTime)
  let eventRender ← collectSlice buffers.events (renderRun state.displayHoldHeadJudgeResult)
  let (audio, render) ← scheduler.combine_commands appendCall appendCall
    frame.slideAudio body eventAudio frame.slideRender eventRender () ()
  ok ({ frame.state with prevButton := input.buttonHeld, prevSensor := input.sensorHeld,
    score := score }, events, audio, render)

theorem finish_from_initial_eq_finish (state : GameState) (newTime : TimePoint)
    (input : FrameInput) (frame : FrameWork) (buffers : Buffers frame)
    (hsources : sources frame.state = { sources state with currentTime := newTime }) :
    finishFromInitial inst ops encode state newTime input frame buffers =
      FrameCommandFinalizationProof.finish inst ops encode input frame buffers := by
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
  simp only [finishFromInitial, FrameCommandFinalizationProof.finish, hn, hb, hs, hd, ht]

include integerLaw decode_encode

theorem finish_from_initial_equiv (state : GameState) (newTime : TimePoint)
    (input : FrameInput) (frame : FrameWork) (buffers : Buffers frame)
    (hsources : sources frame.state = { sources state with currentTime := newTime }) :
    finishFromInitial inst ops encode state newTime input frame buffers =
      ok (finishFrame input frame) := by
  rw [finish_from_initial_eq_finish inst ops encode state newTime input frame buffers hsources]
  exact FrameCommandFinalizationProof.finish_equiv inst ops decode encode
    integerLaw decode_encode input frame buffers

/-- The five source-field equalities are derived from the proved passes. No
whole-finalizer or command-generation equation is supplied by the caller. -/
theorem reference_finish_equiv (state : GameState) (input : FrameInput)
    (buffers : Buffers (processInOrder (referencePass input) (initialFrame state input))) :
    finishFromInitial inst ops encode state (state.currentTime + input.delta) input
      (processInOrder (referencePass input) (initialFrame state input)) buffers =
      ok (Scheduler.stepFrame state input) := by
  rw [finish_from_initial_equiv inst ops decode encode integerLaw decode_encode state
    (state.currentTime + input.delta) input _ buffers (processed_frame_sources state input),
    reference_frame_equiv]

end Verification.FrameCommandStateProof
