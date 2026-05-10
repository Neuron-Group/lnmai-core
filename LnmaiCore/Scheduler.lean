/-
  Frame Scheduler — step one frame of gameplay.

  The Core receives all active notes and frame input, advances each
  note's lifecycle, and returns updated notes plus a list of JudgeEvents.
-/

import LnmaiCore.Types
import LnmaiCore.Constants
import LnmaiCore.Judge
import LnmaiCore.Convert
import LnmaiCore.Score
import LnmaiCore.Lifecycle
import LnmaiCore.InputModel

set_option linter.unusedVariables false

namespace LnmaiCore.Scheduler

open Constants
open InputModel
open Lifecycle
open Score

structure ClickCursor where
  buttonUsed : List Nat := List.replicate BUTTON_ZONE_COUNT 0
  sensorUsed : List Nat := List.replicate SENSOR_AREA_COUNT 0
deriving Inhabited

private def getUsed (xs : List Nat) (i : Nat) : Nat :=
  (xs[i]?).getD 0

private def setUsed : List Nat → Nat → Nat → List Nat
  | [], _, _ => []
  | _ :: rest, 0, v => v :: rest
  | x :: rest, i+1, v => x :: setUsed rest i v

private def tryUseButtonClick (input : FrameInput) (cursor : ClickCursor) (zone : Nat) : Bool × ClickCursor :=
  let used := getUsed cursor.buttonUsed zone
  let available := input.getButtonClickCount zone
  if used < available then
    (true, { cursor with buttonUsed := setUsed cursor.buttonUsed zone (used + 1) })
  else
    (false, cursor)

private def tryUseSensorClick (input : FrameInput) (cursor : ClickCursor) (area : Nat) : Bool × ClickCursor :=
  let used := getUsed cursor.sensorUsed area
  let available := input.getSensorClickCount area
  if used < available then
    (true, { cursor with sensorUsed := setUsed cursor.sensorUsed area (used + 1) })
  else
    (false, cursor)

----------------------------------------------------------------------------
-- Active Notes (all types pooled together for one frame)
----------------------------------------------------------------------------

inductive ActiveNote where
  | tapNote   : TapNote → ActiveNote
  | holdNote  : HoldNote → ActiveNote
  | touchNote : TouchNote → ActiveNote
  | slideNote : SlideNote → ActiveNote
deriving Inhabited

----------------------------------------------------------------------------
-- Process tap notes
----------------------------------------------------------------------------

private def processTapNotes (queues : List (ZoneQueue TapNote)) (input : FrameInput) (currentSec : Float) (style : JudgeStyle) (cursor : ClickCursor) : List (ZoneQueue TapNote) × List JudgeEvent × ClickCursor :=
  let rec loop (i : Nat) (qs : List (ZoneQueue TapNote)) (evs : List JudgeEvent) (cursor : ClickCursor) :=
    match qs with
    | [] => ([], evs, cursor)
    | q :: rest =>
      match q.peek with
      | none => loop (i+1) rest evs cursor
      | some note =>
        let timing := note.params.effectiveTiming
        let buttonDiffMs := (currentSec - timing) * 1000.0
        let sensorDiffMs := buttonDiffMs
        let (usedButton, cursor1) := tryUseButtonClick input cursor i
        let (clicked, diffMs, cursor2) :=
          if usedButton then (true, buttonDiffMs, cursor1)
          else
            let (usedSensor, cursorS) := tryUseSensorClick input cursor1 note.params.startPos
            if usedSensor then (true, sensorDiffMs, cursorS) else (false, buttonDiffMs, cursorS)
        match tapStep note currentSec diffMs clicked style with
        | (_, some evt) =>
          let (restQs, restEvs, cursor3) := loop (i+1) rest evs cursor2
          (q.advance :: restQs, evt :: restEvs, cursor3)
        | (_, none) =>
          let (restQs, restEvs, cursor3) := loop (i+1) rest evs cursor2
          (q :: restQs, restEvs, cursor3)
  loop 0 queues [] cursor

----------------------------------------------------------------------------
-- Process hold notes
----------------------------------------------------------------------------

private def processHoldNotes (holds : List (Nat × HoldNote)) (input : FrameInput) (currentSec : Float) (deltaSec : Float) (style : JudgeStyle) (cursor : ClickCursor) : List (Nat × HoldNote) × List JudgeEvent × ClickCursor :=
  match holds with
  | [] => ([], [], cursor)
  | (zone, note) :: rest =>
    let isPressed := input.getButtonHeld zone || input.getSensorHeld note.params.startPos
    let timing := note.params.effectiveTiming
    let buttonDiffMs := (currentSec - timing) * 1000.0
    let sensorDiffMs := buttonDiffMs
    let (usedButton, cursor1) := tryUseButtonClick input cursor zone
    let (clicked, diffMs, cursor2) :=
      if usedButton then (true, buttonDiffMs, cursor1)
      else
        let (usedSensor, cursorS) := tryUseSensorClick input cursor1 note.params.startPos
        if usedSensor then (true, sensorDiffMs, cursorS) else (false, buttonDiffMs, cursorS)
    match holdStep note currentSec diffMs HOLD_HEAD_IGNORE_LENGTH_SEC HOLD_TAIL_IGNORE_LENGTH_SEC clicked isPressed deltaSec style with
    | (newNote, some evt) =>
      let (restHolds, restEvs, cursor3) := processHoldNotes rest input currentSec deltaSec style cursor2
      (restHolds, evt :: restEvs, cursor3)
    | (newNote, none) =>
      let (restHolds, restEvs, cursor3) := processHoldNotes rest input currentSec deltaSec style cursor2
      ((zone, newNote) :: restHolds, restEvs, cursor3)

private def processTouchHoldNotes (holds : List (Nat × HoldNote)) (input : FrameInput) (currentSec : Float) (deltaSec : Float) (style : JudgeStyle) (cursor : ClickCursor) : List (Nat × HoldNote) × List JudgeEvent × ClickCursor :=
  match holds with
  | [] => ([], [], cursor)
  | (area, note) :: rest =>
    let isPressed := input.getSensorHeld area
    let timing := note.params.effectiveTiming
    let sensorDiffMs := (currentSec - timing) * 1000.0
    let (usedSensor, cursor1) := tryUseSensorClick input cursor area
    match holdStep note currentSec sensorDiffMs TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC usedSensor isPressed deltaSec style with
    | (newNote, some evt) =>
      let (restHolds, restEvs, cursor2) := processTouchHoldNotes rest input currentSec deltaSec style cursor1
      (restHolds, evt :: restEvs, cursor2)
    | (newNote, none) =>
      let (restHolds, restEvs, cursor2) := processTouchHoldNotes rest input currentSec deltaSec style cursor1
      ((area, newNote) :: restHolds, restEvs, cursor2)

----------------------------------------------------------------------------
-- Process touch notes
----------------------------------------------------------------------------

private def processTouchNotes (queues : List (ZoneQueue TouchNote)) (input : FrameInput) (currentSec : Float) (style : JudgeStyle) (cursor : ClickCursor) : List (ZoneQueue TouchNote) × List JudgeEvent × ClickCursor :=
  let rec loop (i : Nat) (qs : List (ZoneQueue TouchNote)) (evs : List JudgeEvent) (cursor : ClickCursor) :=
    match qs with
    | [] => ([], evs, cursor)
    | q :: rest =>
      match q.peek with
      | none => loop (i+1) rest evs cursor
      | some note =>
        let timing := note.params.effectiveTiming
        let diffMs := (currentSec - timing) * 1000.0
        let (usedSensor, cursor1) := tryUseSensorClick input cursor i
        match touchStep note currentSec diffMs usedSensor style with
        | (_, some evt) =>
          let (restQs, restEvs, cursor2) := loop (i+1) rest evs cursor1
          (q.advance :: restQs, evt :: restEvs, cursor2)
        | (_, none) =>
          let (restQs, restEvs, cursor2) := loop (i+1) rest evs cursor1
          (q :: restQs, restEvs, cursor2)
  loop 0 queues [] cursor

----------------------------------------------------------------------------
-- Process slide notes
----------------------------------------------------------------------------

private def processSlideNotes (slides : List SlideNote) (input : FrameInput) (currentSec : Float) (style : JudgeStyle) : List SlideNote × List JudgeEvent :=
  match slides with
  | [] => ([], [])
  | note :: rest =>
    match slideStep note currentSec input.sensorHeld style with
    | (newNote, some evt) =>
      let (restSlides, restEvs) := processSlideNotes rest input currentSec style
      (newNote :: restSlides, evt :: restEvs)
    | (newNote, none) =>
      let (restSlides, restEvs) := processSlideNotes rest input currentSec style
      (newNote :: restSlides, restEvs)

----------------------------------------------------------------------------
-- Score Accumulation from Events
----------------------------------------------------------------------------

private def foldEventIntoScore (s : ScoreState) (evt : JudgeEvent) : ScoreState :=
  let multiple : Int := 1
  let comboDelta := Score.updateCombo s.combo s.pCombo s.cPCombo s.dxScore evt.grade multiple
  let counts := match evt.kind with
    | .Tap   => { s.counts with tapCount   := λ g => if g == evt.grade then s.counts.tapCount g + 1 else s.counts.tapCount g }
    | .Hold  => { s.counts with holdCount  := λ g => if g == evt.grade then s.counts.holdCount g + 1 else s.counts.holdCount g }
    | .Slide => { s.counts with slideCount := λ g => if g == evt.grade then s.counts.slideCount g + 1 else s.counts.slideCount g }
    | .Touch => { s.counts with touchCount := λ g => if g == evt.grade then s.counts.touchCount g + 1 else s.counts.touchCount g }
    | .Break => { s.counts with breakCount := λ g => if g == evt.grade then s.counts.breakCount g + 1 else s.counts.breakCount g }
  { s with
    combo       := comboDelta.combo
    pCombo      := comboDelta.pCombo
    cPCombo     := comboDelta.cPCombo
    dxScore     := comboDelta.dXScoreLost
    counts      := counts
  }

private def foldEventsIntoScore (s : ScoreState) (events : List JudgeEvent) : ScoreState :=
  match events with
  | [] => s
  | evt :: rest => foldEventsIntoScore (foldEventIntoScore s evt) rest

----------------------------------------------------------------------------
-- Frame Step: advance all active notes one frame (entry point)
----------------------------------------------------------------------------

def stepFrame (st : GameState) (input : FrameInput) : GameState × List JudgeEvent :=
  let newTime := st.currentTime + input.deltaSec
  let cursor : ClickCursor := {}

  let (tapNotes, tapEvents) :=
    let (notes, evs, _) := processTapNotes st.tapQueues input newTime st.judgeStyle cursor
    (notes, evs)
  let (holdNotes, holdEvents) :=
    let (notes, evs, _) := processHoldNotes st.activeHolds input newTime input.deltaSec st.judgeStyle cursor
    (notes, evs)
  let (touchHoldNotes, touchHoldEvents) :=
    let (notes, evs, _) := processTouchHoldNotes st.activeTouchHolds input newTime input.deltaSec st.judgeStyle cursor
    (notes, evs)
  let (touchNotes, touchEvents) :=
    let (notes, evs, _) := processTouchNotes st.touchQueues input newTime st.judgeStyle cursor
    (notes, evs)
  let (slideNotes, slideEvents) :=
    processSlideNotes st.slides input newTime st.judgeStyle

  let allEvents := tapEvents ++ holdEvents ++ touchHoldEvents ++ touchEvents ++ slideEvents
  let newScore := foldEventsIntoScore st.score allEvents

  ({ st with
      currentTime := newTime
    , prevButton  := input.buttonHeld
    , prevSensor  := input.sensorHeld
    , score       := newScore
    , slides      := slideNotes
    , activeHolds := holdNotes
    , activeTouchHolds := touchHoldNotes
  }, allEvents)

end LnmaiCore.Scheduler
