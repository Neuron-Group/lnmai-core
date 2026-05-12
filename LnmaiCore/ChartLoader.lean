/-
  Declarative chart loader for the lean runtime.

  This mirrors the reference loader's job at a structural level: it takes a
  chart description and materializes the runtime note queues/state used by the
  scheduler.
-/

import LnmaiCore.Types
import LnmaiCore.Constants
import LnmaiCore.Lifecycle
import LnmaiCore.InputModel
import LnmaiCore.Simai.Syntax
import LnmaiCore.Simai.Shape
import LnmaiCore.Simai.SlideTables
import LnmaiCore.Simai.SlideParser
import Lean.Data.Json

open Lean

namespace LnmaiCore.ChartLoader

open Lean (Json)

open Constants
open InputModel
open Lifecycle

structure TapChartNote where
  timingSec : Float
  lane      : Nat
  isBreak   : Bool := false
  isEX      : Bool := false
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure HoldChartNote where
  timingSec : Float
  lane      : Nat
  lengthSec : Float
  isBreak   : Bool := false
  isEX      : Bool := false
  isTouch   : Bool := false
  isClassic : Option Bool := none
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure TouchChartNote where
  timingSec : Float
  sensorPos : Nat
  isBreak   : Bool := false
  touchGroupId : Option Nat := none
  touchGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

abbrev SlideAreaSpec := Simai.SlideAreaSpec

structure SlideChartNote where
  timingSec     : Float
  lane          : Nat
  lengthSec     : Float
  startTimingSec : Float := 0.0
  slideKind     : SlideKind := .Single
  isClassic     : Bool := false
  isConnSlide   : Bool := false
  parentNoteIndex : Option Nat := none
  isGroupHead   : Bool := false
  isGroupEnd    : Bool := false
  parentFinished : Bool := false
  parentPendingFinish : Bool := false
  totalJudgeQueueLen : Nat := 0
  trackCount    : Nat := 1
  judgeAtSec    : Option Float := none
  isBreak       : Bool := false
  isEX          : Bool := false
  noteIndex     : Nat := 0
  judgeQueues   : List (List SlideAreaSpec) := []
  debugSimai : Option (String × String × Bool) := none
deriving Inhabited, Repr, ToJson, FromJson

structure ChartSpec where
  taps       : List TapChartNote := []
  holds      : List HoldChartNote := []
  touches    : List TouchChartNote := []
  touchHolds : List HoldChartNote := []
  slides     : List SlideChartNote := []
  slideSkipping : Option Bool := none
deriving Inhabited, Repr, ToJson, FromJson

private def insertByTiming {α : Type} (getTiming : α → Float) (item : α) : List α → List α
  | [] => [item]
  | head :: rest =>
    if getTiming item ≤ getTiming head then
      item :: head :: rest
    else
      head :: insertByTiming getTiming item rest

private def sortByTiming {α : Type} (getTiming : α → Float) (items : List α) : List α :=
  items.foldl (fun acc item => insertByTiming getTiming item acc) []

private def buildTap (note : TapChartNote) : TapNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, startPos := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , state := TapState.Waiting }

private def buildHold (note : HoldChartNote) : HoldNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, startPos := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , state := HoldSubState.HeadWaiting
  , lengthSec := note.lengthSec
  , headDiffMs := 0.0
  , headGrade := .Miss
  , playerReleaseTimeSec := 0.0
  , isClassic := note.isClassic.getD false
  , isTouchHold := note.isTouch
  , touchHoldGroupId := note.touchHoldGroupId
  , touchHoldGroupSize := note.touchHoldGroupSize.getD 1
  , touchHoldGroupTriggered := false }

private def buildTouch (note : TouchChartNote) : TouchNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, startPos := note.sensorPos, isBreak := note.isBreak, isEX := false, noteIndex := note.noteIndex }
  , state := TouchState.Waiting
  , sensorPos := note.sensorPos
  , touchGroupId := note.touchGroupId
  , touchGroupSize := note.touchGroupSize.getD 1 }

private def buildSlideArea (spec : SlideAreaSpec) : SlideArea :=
  { targetAreas := spec.targetAreas
  , policy := spec.policy
  , isLast := spec.isLast
  , isSkippable := spec.isSkippable
  , arrowProgressWhenOn := spec.arrowProgressWhenOn
  , arrowProgressWhenFinished := spec.arrowProgressWhenFinished
  }

private def buildSlideAreasFromSimai (spec : Simai.SlideAreaSpec) : SlideArea :=
  { targetAreas := spec.targetAreas
  , policy := spec.policy
  , isLast := spec.isLast
  , isSkippable := spec.isSkippable
  , arrowProgressWhenOn := spec.arrowProgressWhenOn
  , arrowProgressWhenFinished := spec.arrowProgressWhenFinished
  }

private def disableSlideSkipping (queues : List (List SlideArea)) : List (List SlideArea) :=
  queues.map (fun queue => queue.map (fun area => { area with isSkippable := false }))

private def applySingleTrackConnRules (note : SlideChartNote) (queue : List SlideArea) : List SlideArea :=
  if !note.isConnSlide then
    queue
  else if note.totalJudgeQueueLen < 4 then
    match queue with
    | [] => []
    | first :: second :: rest =>
      let first' := { first with isSkippable := note.isGroupHead }
      let second' := { second with isSkippable := note.isGroupEnd }
      first' :: second' :: rest
    | only => only
  else
    queue.map (fun area => { area with isSkippable := true })

theorem shortConnSlide_applySingleTrackConnRules
    (note : SlideChartNote) (first second : SlideArea) (rest : List SlideArea)
    (hConn : note.isConnSlide = true) (hShort : note.totalJudgeQueueLen < 4) :
    applySingleTrackConnRules note (first :: second :: rest) =
      ({ first with isSkippable := note.isGroupHead } ::
        { second with isSkippable := note.isGroupEnd } :: rest) := by
  simp [applySingleTrackConnRules, hConn, hShort]

private def touchHoldNeighbors : Nat → List Nat
  | 0  => [17, 18, 25, 26, 8]
  | 1  => [18, 19, 26, 27, 9]
  | 2  => [19, 20, 27, 28, 10]
  | 3  => [20, 21, 28, 29, 11]
  | 4  => [21, 22, 29, 30, 12]
  | 5  => [22, 23, 30, 31, 13]
  | 6  => [23, 24, 31, 32, 14]
  | 7  => [24, 17, 32, 25, 15]
  | 8  => [0, 7, 25]
  | 9  => [1, 0, 26]
  | 10 => [2, 1, 27]
  | 11 => [3, 2, 28]
  | 12 => [4, 3, 29]
  | 13 => [5, 4, 30]
  | 14 => [6, 5, 31]
  | 15 => [7, 6, 32]
  | 16 => [8, 9, 10, 11, 12, 13, 14, 15]
  | 17 => [0, 7, 25]
  | 18 => [0, 1, 26]
  | 19 => [1, 2, 27]
  | 20 => [2, 3, 28]
  | 21 => [3, 4, 29]
  | 22 => [4, 5, 30]
  | 23 => [5, 6, 31]
  | 24 => [6, 7, 32]
  | 25 => [17, 0, 26, 8, 16]
  | 26 => [18, 1, 25, 27, 9, 16]
  | 27 => [19, 2, 26, 28, 10, 16]
  | 28 => [20, 3, 27, 29, 11, 16]
  | 29 => [21, 4, 28, 30, 12, 16]
  | 30 => [22, 5, 29, 31, 13, 16]
  | 31 => [23, 6, 30, 32, 14, 16]
  | 32 => [24, 7, 31, 15, 16]
  | _  => []

private def containsNat (items : List Nat) (value : Nat) : Bool :=
  items.any (fun item => item == value)

private def removeNat (items : List Nat) (value : Nat) : List Nat :=
  items.filter (fun item => item != value)

partial def collectTouchHoldComponent (pending : List Nat) (remaining : List Nat) (component : List Nat) : List Nat :=
  match pending with
  | [] => component
  | area :: rest =>
    if containsNat component area then
      collectTouchHoldComponent rest remaining component
    else
      let neighbors := touchHoldNeighbors area
      let newlyReached := remaining.filter (fun candidate => containsNat neighbors candidate)
      let remaining' := remaining.filter (fun candidate => candidate != area && !containsNat neighbors candidate)
      collectTouchHoldComponent (rest ++ newlyReached) remaining' (area :: component)

partial def assignTouchHoldGroupsLoop (allNotes : List HoldChartNote) (remaining : List Nat) (groupId : Nat) (acc : List HoldChartNote) : List HoldChartNote :=
  match remaining with
  | [] => acc
  | area :: rest =>
    let component := collectTouchHoldComponent [area] remaining []
    let componentSize := List.length (allNotes.filter (fun note => containsNat component note.lane))
      let nextAcc := acc.map (fun note =>
        if containsNat component note.lane then
          { note with touchHoldGroupId := some groupId, touchHoldGroupSize := some componentSize }
        else
          note)
    let remaining' := rest.filter (fun candidate => !containsNat component candidate)
    assignTouchHoldGroupsLoop allNotes remaining' (groupId + 1) nextAcc

private def assignTouchHoldGroups (notes : List HoldChartNote) : List HoldChartNote :=
  let sensorTypes := notes.foldl (fun acc note => if containsNat acc note.lane then acc else note.lane :: acc) []
  assignTouchHoldGroupsLoop notes sensorTypes 0 notes

partial def assignTouchGroupsLoop (allNotes : List TouchChartNote) (remaining : List Nat) (groupId : Nat) (acc : List TouchChartNote) : List TouchChartNote :=
  match remaining with
  | [] => acc
  | area :: rest =>
    let component := collectTouchHoldComponent [area] remaining []
    let componentSize := List.length (allNotes.filter (fun note => containsNat component note.sensorPos))
    let nextAcc := acc.map (fun note =>
      if containsNat component note.sensorPos then
        { note with touchGroupId := some groupId, touchGroupSize := some componentSize }
      else
        note)
    let remaining' := rest.filter (fun candidate => !containsNat component candidate)
    assignTouchGroupsLoop allNotes remaining' (groupId + 1) nextAcc

private def assignTouchGroups (notes : List TouchChartNote) : List TouchChartNote :=
  let sensorTypes := notes.foldl (fun acc note => if containsNat acc note.sensorPos then acc else note.sensorPos :: acc) []
  assignTouchGroupsLoop notes sensorTypes 0 notes

private def buildSlide (slideSkipping : Bool) (note : SlideChartNote) : SlideNote :=
  let judgeQueues :=
    let queues := note.judgeQueues.map (fun queue => queue.map buildSlideArea)
    if !slideSkipping then
      disableSlideSkipping queues
    else
      match queues with
      | [queue] => [applySingleTrackConnRules note queue]
      | _ => queues
  let judgeTimingSec := note.judgeAtSec.getD note.timingSec
  let waitTimeSec := max 0.0 (note.startTimingSec + note.lengthSec - judgeTimingSec)
  let rec maxQueueLength : List (List SlideArea) → Nat
    | [] => 0
    | queue :: rest => Nat.max queue.length (maxQueueLength rest)
  { params := { judgeTimingSec := judgeTimingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, startPos := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , state := SlideState.Active waitTimeSec
  , lengthSec := note.lengthSec
  , startTiming := note.startTimingSec
  , slideKind := note.slideKind
  , isClassic := note.isClassic
  , isConnSlide := note.isConnSlide
  , parentNoteIndex := note.parentNoteIndex
  , isGroupPartHead := note.isGroupHead
  , isGroupPartEnd := note.isGroupEnd
  , parentFinished := note.parentFinished
  , parentPendingFinish := note.parentPendingFinish
  , initialQueueRemaining := maxQueueLength judgeQueues
  , totalJudgeQueueLen := note.totalJudgeQueueLen
  , trackCount := note.trackCount
  , isCheckable := false
  , judgeQueues := judgeQueues }

def buildGameState (chart : ChartSpec) : GameState :=
  let tapQueues : List (ZoneQueue TapNote) :=
    (List.range BUTTON_ZONE_COUNT).map (fun zone =>
      let notes := (sortByTiming (fun note => note.timingSec) (chart.taps.filter (fun note => note.lane == zone))).map buildTap
      { notes := notes })
  let holdQueues : List (ZoneQueue HoldNote) :=
    (List.range BUTTON_ZONE_COUNT).map (fun zone =>
      let notes := (sortByTiming (fun note => note.timingSec) (chart.holds.filter (fun note => note.lane == zone))).map buildHold
      { notes := notes })
  let touchHoldNotes := assignTouchHoldGroups chart.touchHolds
  let touchNotesGrouped := assignTouchGroups chart.touches
  let touchHoldQueues : List (ZoneQueue HoldNote) :=
    (List.range SENSOR_AREA_COUNT).map (fun area =>
      let notes := (sortByTiming (fun note => note.timingSec) (touchHoldNotes.filter (fun note => note.lane == area))).map buildHold
      { notes := notes })
  let touchQueues : List (ZoneQueue TouchNote) :=
    (List.range SENSOR_AREA_COUNT).map (fun area =>
      let notes := (sortByTiming (fun note => note.timingSec) (touchNotesGrouped.filter (fun note => note.sensorPos == area))).map buildTouch
      { notes := notes })
  let activeHolds :=
    (chart.holds.filter (fun note => note.isTouch = false)).map (fun note => (note.lane, buildHold note))
  let activeTouchHolds :=
    (touchHoldNotes.map (fun note => (note.lane, buildHold note)))
  {
    currentTime := 0.0,
    prevButton := List.replicate BUTTON_ZONE_COUNT false,
    prevSensor := List.replicate SENSOR_AREA_COUNT false,
    tapQueues := tapQueues,
    holdQueues := holdQueues,
    touchHoldQueues := touchHoldQueues,
    touchQueues := touchQueues,
    slides := chart.slides.map (buildSlide (chart.slideSkipping.getD true)),
    activeHolds := activeHolds,
    activeTouchHolds := activeTouchHolds,
    touchGroupStates := [],
    touchHoldGroupStates := [],
    currentBatch := {},
    score := {},
    judgeStyle := JudgeStyle.Default,
    touchPanelOffsetSec := Constants.TOUCH_PANEL_OFFSET_SEC
    , useButtonRingForTouch := Constants.USE_BUTTON_RING_FOR_TOUCH
    , subdivideSlideJudgeGrade := Constants.SUBDIVIDE_SLIDE_JUDGE_GRADE
  }

def parseChartJson (json : Json) : Except String ChartSpec :=
  Lean.fromJson? json

def parseChartJsonString (content : String) : Except String ChartSpec :=
  match Json.parse content with
  | Except.ok json => parseChartJson json
  | Except.error err => Except.error err

def loadChartFile (path : System.FilePath) : IO (Except String ChartSpec) := do
  let content ← IO.FS.readFile path
  pure <| parseChartJsonString content

end LnmaiCore.ChartLoader
