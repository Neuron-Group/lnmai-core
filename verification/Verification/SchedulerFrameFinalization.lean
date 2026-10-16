import Verification.SchedulerFrame
import Verification.RuntimeScoreJudgments

/-!
Connect the extracted event/command ordering kernels and exact-judgment score
fold to frame finalization. Lists interpret runtime buffers; command generation
still uses the original reference operations, and is not claimed extracted.
Likewise, record writes and score counter arithmetic remain representation
boundaries. This file removes the separate score-fold and batch-order gaps in
the interpreted finishFrame, not the remaining concrete runtime obligations.
-/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.SchedulerFrameFinalizationProof
open LnmaiCore InputModel SchedulerFrameProof
open private eventsToAudioCommands eventsToRenderCommands touchHoldBodyAudioCmds
  from LnmaiCore.Scheduler

/-- List interpretation of the runtime append callback. Concrete Vec movement,
allocation and capacity refinement are separate obligations. -/
def appendCall {T : Type} :
    core.ops.function.Fn Unit (List T × List T) (List T) :=
  { FnMutInst :=
      { FnOnceInst := { call_once := fun _ pair => ok (pair.1 ++ pair.2) }
        call_mut := fun f pair => ok (pair.1 ++ pair.2, f) }
    call := fun _ pair => ok (pair.1 ++ pair.2) }

theorem reported_events_equiv (frame : FrameWork) :
    scheduler.combine_events appendCall frame.events () = ok (reportedEvents frame) := by
  simp [scheduler.combine_events, appendCall, reportedEvents]

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → Int) (encode : Int → I)
  (h : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (hencode : ∀ value, decode (encode value) = value)
include h hencode

def finish (input : FrameInput) (frame : FrameWork) :
    Result (GameState × List JudgeEvent × List AudioCommand × List RenderCommand) := do
  let state := frame.state
  let events ← scheduler.combine_events appendCall frame.events ()
  let score ← RuntimeScoreJudgmentsProof.foldEvents inst ops encode
    state.noteFastLateDisplay state.breakFastLateDisplay state.score events
  let (audio, render) ← scheduler.combine_commands appendCall appendCall
    frame.slideAudio
    (touchHoldBodyAudioCmds state.activeTouchHolds state.currentTime input state.touchHoldGroupStates)
    (eventsToAudioCommands events state.currentTime) frame.slideRender
    (eventsToRenderCommands state.displayHoldHeadJudgeResult events) () ()
  ok ({ state with prevButton := input.buttonHeld, prevSensor := input.sensorHeld, score := score },
    events, audio, render)

theorem finish_equiv (input : FrameInput) (frame : FrameWork) :
    finish inst ops encode input frame = ok (finishFrame input frame) := by
  simp only [finish, reported_events_equiv,
    RuntimeScoreJudgmentsProof.fold_events_equiv inst ops decode encode h hencode, bind_tc_ok]
  simp [scheduler.combine_commands, appendCall, finishFrame]

def processAndFinish {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (f : F) (repr : R → FrameWork) (initial : R) (input : FrameInput) :
    Result (GameState × List JudgeEvent × List AudioCommand × List RenderCommand) := do
  let frame ← scheduler.process_frame call initial f
  finish inst ops encode input (repr frame)

/-- The driver and finalizer return the original full frame result. The
initialization and per-phase contracts are retained explicitly; this theorem
does not prove the unextracted pass callbacks. -/
theorem process_and_finish_equiv {R F : Type}
    (call : core.ops.function.Fn F (scheduler.FramePhase × R) R)
    (f : F) (repr : R → FrameWork) (initial : R) (state : GameState) (input : FrameInput)
    (hinitial : repr initial = initialFrame state input)
    (hstep : ∀ phase frame, ∃ next, call.call f (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame)) :
    processAndFinish inst ops encode call f repr initial input =
      ok (Scheduler.stepFrame state input) := by
  obtain ⟨out, hout, hresult⟩ := SchedulerFrameProof.process_frame_to_stepFrame
    call f repr initial state input hinitial hstep
  simp only [processAndFinish, hout, bind_tc_ok,
    finish_equiv inst ops decode encode h hencode, hresult]

end Verification.SchedulerFrameFinalizationProof
