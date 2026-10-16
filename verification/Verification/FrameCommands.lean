import Verification.FrameCommandCollection

/-! The actual extracted selectors and collector are composed with source
event construction. Runtime record/UTF-8/bigint representation is not supplied
by interpreting the payload with Lean types. -/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.FrameCommandsProof
open LnmaiCore InputModel Bridge TypesProof
open FrameCommandsReference FrameCommandCollection
open private eventsToAudioCommands eventsToRenderCommands touchHoldBodyAudioCmds
  touchHoldBodyGroupMajorityPressed from LnmaiCore.Scheduler

def ofKind : JudgeEventKind → types.JudgeEventKind
  | .Tap => .Tap | .Hold => .Hold | .Slide => .Slide | .Touch => .Touch | .Break => .Break

def ofPhase : JudgePhase → types.JudgePhase
  | .head => .Head | .tail => .Tail

def ofPosition : RuntimePos → types.RuntimePos
  | .button zone => .Button (ofLnmButtonZone zone)
  | .sensor area => .Sensor (ofLnmSensorArea area)

def audioKindRun (kind : JudgeEventKind) (phase : JudgePhase) (grade : JudgeGrade)
    (position : RuntimePos) : Result (Option JudgeEventKind) := do
  let output ← frame_commands.event_audio_kind (ofKind kind) (ofPhase phase)
    (ofLnmJudgeGrade grade) (ofPosition position)
  ok (output.map toLnmJudgeEventKind)

theorem audio_kind_equiv (kind : JudgeEventKind) (phase : JudgePhase) (grade : JudgeGrade)
    (position : RuntimePos) :
    audioKindRun kind phase grade position = ok (audioKind kind phase grade position) := by
  cases kind <;> cases phase <;> cases position <;> cases grade <;> rfl

theorem render_visible_equiv (kind : JudgeEventKind) (phase : JudgePhase)
    (display : Bool) :
    frame_commands.event_render_visible (ofKind kind) (ofPhase phase) display =
      ok (renderVisible kind phase display) := by
  cases kind <;> cases phase <;> cases display <;> rfl

theorem body_active_equiv (state : Lifecycle.HoldSubState) (localPressed groupPressed : Bool) :
    frame_commands.touch_hold_body_active (HoldLifecycleProof.ofState state) localPressed groupPressed =
      ok (bodyActive state localPressed groupPressed) := by
  cases state with
  | HeadJudged grade =>
      cases grade <;> cases localPressed <;> cases groupPressed <;> rfl
  | HeadWaiting | HeadJudgeable | BodyHeld | BodyReleased | Ended _ =>
      cases localPressed <;> cases groupPressed <;> rfl

def audioRun (time : TimePoint) (event : JudgeEvent) : Result (Option AudioCommand) := do
  let kind ← audioKindRun event.kind event.phase event.grade event.position
  ok (kind.map fun kind => .PlayJudgeSfx kind event.grade event.isBreak time event.noteIndex)

def renderRun (display : Bool) (event : JudgeEvent) : Result (Option RenderCommand) := do
  let visible ← frame_commands.event_render_visible (ofKind event.kind) (ofPhase event.phase) display
  ok (if visible then
    some (.ShowJudgeResult event.kind event.grade event.isBreak event.diff event.noteIndex)
    else none)

def bodyRun (time : TimePoint) (input : FrameInput)
    (groups : List TouchHoldBodyGroupState) (entry : SensorArea × Lifecycle.HoldNote) :
    Result (Option AudioCommand) := do
  let note := entry.2
  let group := match note.touchHoldGroupId with
    | some id => touchHoldBodyGroupMajorityPressed groups id
    | none => false
  let active ← frame_commands.touch_hold_body_active (HoldLifecycleProof.ofState note.state)
    (input.getSensorHeld entry.1) group
  ok (if active then some (.PlayTouchHoldBody note.params.noteIndex time) else none)

theorem audio_run_equiv (time : TimePoint) (event : JudgeEvent) :
    audioRun time event = ok (audioCommand time event) := by
  simp [audioRun, audio_kind_equiv, audioCommand, bind_tc_ok]

theorem render_run_equiv (display : Bool) (event : JudgeEvent) :
    renderRun display event = ok (renderCommand display event) := by
  simp [renderRun, render_visible_equiv, renderCommand, bind_tc_ok]

theorem body_run_equiv (time : TimePoint) (input : FrameInput)
    (groups : List TouchHoldBodyGroupState) (entry : SensorArea × Lifecycle.HoldNote) :
    bodyRun time input groups entry = ok (bodyCommand time input groups entry) := by
  simp only [bodyRun, body_active_equiv, bodyCommand, bind_tc_ok] <;> rfl

def sliceCallback {A T : Type} (source : Slice A) (action : A → Result (Option T))
    (index : Usize) : Result (Option T) := do
  let value ← Slice.index_usize source index
  action value

theorem slice_callback_equiv {A T : Type} (source : Slice A)
    (action : A → Result (Option T)) (command : A → Option T)
    (haction : ∀ value ∈ source.val, action value = ok (command value))
    (index : Usize) (pref : List A) (value : A) (rest : List A)
    (hsource : source.val = pref ++ value :: rest) (hindex : pref.length = index.val) :
    sliceCallback source action index = ok (command value) := by
  have hbound : index.val < source.val.length := by
    rw [hsource, List.length_append, List.length_cons]
    omega
  obtain ⟨item, hread, hitem⟩ := spec_imp_exists (Slice.index_usize_spec source index hbound)
  have heq : item = value := by
    rw [hitem]
    simp only [hsource, ← hindex, List.getElem_append_right (Nat.le_refl _),
      Nat.sub_self, List.getElem_cons_zero]
  rw [heq] at hread
  simp only [sliceCallback, hread, bind_tc_ok]
  exact haction value (by rw [hsource]; simp)

def collectSlice {A T : Type} (source : Slice A) (action : A → Result (Option T)) :
    Result (List T) := do
  let out ← frame_commands.collect_indexed (BuiltinFn Usize (Option T))
    (Slice.len source) (sliceCallback source action)
  ok out.val

theorem collect_slice_equiv {A T : Type} (source : Slice A)
    (action : A → Result (Option T)) (command : A → Option T)
    (haction : ∀ value ∈ source.val, action value = ok (command value)) :
    collectSlice source action = ok (source.val.filterMap command) := by
  apply collect_indexed_equiv _ _ _ _ _ (by simp)
  intro index pref value rest hsource hindex
  exact slice_callback_equiv source action command haction index pref value rest hsource hindex

theorem audio_commands_equiv (time : TimePoint) (events : Slice JudgeEvent) :
    collectSlice events (audioRun time) = ok (eventsToAudioCommands events.val time) := by
  rw [collect_slice_equiv events (audioRun time) (audioCommand time)
    (fun event _ => audio_run_equiv time event), audio_commands_original]

theorem render_commands_equiv (display : Bool) (events : Slice JudgeEvent) :
    collectSlice events (renderRun display) = ok (eventsToRenderCommands display events.val) := by
  rw [collect_slice_equiv events (renderRun display) (renderCommand display)
    (fun event _ => render_run_equiv display event), render_commands_original]

theorem body_commands_equiv (time : TimePoint) (input : FrameInput)
    (groups : List TouchHoldBodyGroupState) (holds : Slice (SensorArea × Lifecycle.HoldNote)) :
    collectSlice holds (bodyRun time input groups) =
      ok (touchHoldBodyAudioCmds holds.val time input groups) := by
  rw [collect_slice_equiv holds (bodyRun time input groups) (bodyCommand time input groups)
    (fun entry _ => body_run_equiv time input groups entry), body_command_original]

end Verification.FrameCommandsProof
