import Verification.FrameCommands
import Verification.SchedulerFrameFinalization

/-! Frame feedback now uses the actual extracted selectors and ordered
collector. Buffers carry their pointwise record/list representation; arithmetic,
group-majority lookup, payload copying and allocation remain explicit boundaries. -/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.FrameCommandFinalizationProof
open LnmaiCore InputModel Lifecycle SchedulerFrameProof FrameCommandsProof
open SchedulerFrameFinalizationProof

/-- Representable buffers at the output of the five scheduling passes.
No command-generator equation is a field. -/
structure Buffers (frame : FrameWork) where
  events : Slice JudgeEvent
  holds : Slice (SensorArea × HoldNote)
  events_val : events.val = reportedEvents frame
  holds_val : holds.val = frame.state.activeTouchHolds

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → Int) (encode : Int → I)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (decode_encode : ∀ value, decode (encode value) = value)

def finish (input : FrameInput) (frame : FrameWork) (buffers : Buffers frame) :
    Result (GameState × List JudgeEvent × List AudioCommand × List RenderCommand) := do
  let state := frame.state
  let events ← scheduler.combine_events appendCall frame.events ()
  let score ← RuntimeScoreJudgmentsProof.foldEvents inst ops encode
    state.noteFastLateDisplay state.breakFastLateDisplay state.score events
  let body ← collectSlice buffers.holds
    (bodyRun state.currentTime input state.touchHoldGroupStates)
  let eventAudio ← collectSlice buffers.events (audioRun state.currentTime)
  let eventRender ← collectSlice buffers.events (renderRun state.displayHoldHeadJudgeResult)
  let (audio, render) ← scheduler.combine_commands appendCall appendCall
    frame.slideAudio body eventAudio frame.slideRender eventRender () ()
  ok ({ state with prevButton := input.buttonHeld, prevSensor := input.sensorHeld, score := score },
    events, audio, render)

include integerLaw decode_encode

/-- Exact score, event order, body/event feedback order and final held-input
fields. The source feedback generators occur only in the reference result. -/
theorem finish_equiv (input : FrameInput) (frame : FrameWork) (buffers : Buffers frame) :
    finish inst ops encode input frame buffers = ok (finishFrame input frame) := by
  simp only [finish, reported_events_equiv,
    RuntimeScoreJudgmentsProof.fold_events_equiv inst ops decode encode integerLaw decode_encode,
    body_commands_equiv, audio_commands_equiv, render_commands_equiv,
    buffers.events_val, buffers.holds_val, bind_tc_ok]
  simp [scheduler.combine_commands, appendCall, finishFrame]

end Verification.FrameCommandFinalizationProof
