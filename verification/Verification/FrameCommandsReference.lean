import Verification.HoldLifecycle
import Verification.TypesPublic
import LnmaiCore.Scheduler

/-! Source factoring for the frame's feedback. These definitions use the
original unbounded event/note values; the extracted branch and collection
proofs are separate. No complete command-generator equality is assumed. -/

namespace Verification.FrameCommandsReference
open LnmaiCore InputModel Lifecycle
open private eventToAudioCommands eventToRenderCommands
  eventsToAudioCommands eventsToRenderCommands touchHoldBodyAudioCmds
  touchHoldBodyGroupMajorityPressed from LnmaiCore.Scheduler

def audioKind (kind : JudgeEventKind) (phase : JudgePhase) (grade : JudgeGrade)
    (position : RuntimePos) : Option JudgeEventKind :=
  if kind == .Hold && phase == .head && position.sensorArea?.isSome then none
  else if kind == .Hold && phase == .head && grade.isMissOrTooFast then none
  else if kind == .Hold && phase == .head then some .Tap
  else some kind

def audioCommand (time : TimePoint) (event : JudgeEvent) : Option AudioCommand :=
  (audioKind event.kind event.phase event.grade event.position).map fun kind =>
    .PlayJudgeSfx kind event.grade event.isBreak time event.noteIndex

def renderVisible (kind : JudgeEventKind) (phase : JudgePhase)
    (displayHoldHead : Bool) : Bool :=
  !(kind == .Hold && phase == .head && !displayHoldHead)

def renderCommand (displayHoldHead : Bool) (event : JudgeEvent) : Option RenderCommand :=
  if renderVisible event.kind event.phase displayHoldHead then
    some (.ShowJudgeResult event.kind event.grade event.isBreak event.diff event.noteIndex)
  else none

def bodyActive (state : HoldSubState) (localPressed groupedPressed : Bool) : Bool :=
  let active := match state with | .Ended _ => false | _ => true
  let headAccepted := match state with
    | .HeadJudged grade => !grade.isMissOrTooFast
    | .BodyHeld | .BodyReleased => true
    | _ => false
  active && headAccepted && (localPressed || groupedPressed)

def bodyCommand (time : TimePoint) (input : FrameInput)
    (groups : List TouchHoldBodyGroupState) (entry : SensorArea × HoldNote) :
    Option AudioCommand :=
  let note := entry.2
  let groupedPressed := match note.touchHoldGroupId with
    | some groupId => touchHoldBodyGroupMajorityPressed groups groupId
    | none => false
  if bodyActive note.state (input.getSensorHeld entry.1) groupedPressed then
    some (.PlayTouchHoldBody note.params.noteIndex time)
  else none

theorem audio_command_original (time : TimePoint) (event : JudgeEvent) :
    (audioCommand time event).toList = eventToAudioCommands event time := by
  simp only [audioCommand, audioKind, eventToAudioCommands]
  split <;> simp_all only [Option.map_none, Option.toList_none]
  split <;> simp_all only [Option.map_none, Option.toList_none]
  split <;> simp_all

theorem render_command_original (display : Bool) (event : JudgeEvent) :
    (renderCommand display event).toList = eventToRenderCommands display event := by
  simp only [renderCommand, renderVisible, eventToRenderCommands]
  cases h : (event.kind == .Hold && event.phase == .head && !display) <;> simp [h]

theorem body_command_original (time : TimePoint) (input : FrameInput)
    (groups : List TouchHoldBodyGroupState) (holds : List (SensorArea × HoldNote)) :
    holds.filterMap (bodyCommand time input groups) =
      touchHoldBodyAudioCmds holds time input groups := by
  induction holds with
  | nil => rfl
  | cons entry rest ih =>
      simp only [List.filterMap_cons, touchHoldBodyAudioCmds, List.foldr_cons,
        bodyCommand, bodyActive] at *
      split <;> simp_all

theorem filter_map_to_list {A B : Type} (f : A → Option B) (values : List A) :
    values.filterMap f = values.flatMap (fun value => (f value).toList) := by
  induction values with
  | nil => rfl
  | cons value rest ih => cases h : f value <;> simp [h, ih]

theorem audio_commands_original (time : TimePoint) (events : List JudgeEvent) :
    events.filterMap (audioCommand time) = eventsToAudioCommands events time := by
  rw [filter_map_to_list]
  induction events with
  | nil => rfl
  | cons event rest ih =>
      simp only [List.flatMap_cons, audio_command_original, eventsToAudioCommands, ih]

theorem render_commands_original (display : Bool) (events : List JudgeEvent) :
    events.filterMap (renderCommand display) = eventsToRenderCommands display events := by
  rw [filter_map_to_list]
  induction events with
  | nil => rfl
  | cons event rest ih =>
      simp only [List.flatMap_cons, render_command_original, eventsToRenderCommands, ih]

end Verification.FrameCommandsReference
