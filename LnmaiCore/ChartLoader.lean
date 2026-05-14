/-
  Declarative chart loader for the lean runtime.

  This mirrors the reference loader's job at a structural level: it takes a
  chart description and materializes the runtime note queues/state used by the
  scheduler.
-/

import LnmaiCore.Types
import LnmaiCore.Areas
import LnmaiCore.Storage
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
  lane      : ButtonZone
  isBreak   : Bool := false
  isEX      : Bool := false
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure HoldChartNote where
  timingSec : Float
  lane      : ButtonZone
  lengthSec : Float
  isBreak   : Bool := false
  isEX      : Bool := false
  isTouch   : Bool := false
  isClassic : Option Bool := none
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure TouchHoldChartNote where
  timingSec : Float
  sensorPos : SensorArea
  lengthSec : Float
  isBreak   : Bool := false
  isEX      : Bool := false
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure TouchChartNote where
  timingSec : Float
  sensorPos : SensorArea
  isBreak   : Bool := false
  touchGroupId : Option Nat := none
  touchGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

abbrev SlideAreaSpec := Simai.SlideAreaSpec

structure SlideChartNote where
  timingSec     : Float
  lane          : ButtonZone
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
  touchHolds : List TouchHoldChartNote := []
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
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , lane := note.lane
  , state := TapState.Waiting }

private def buildHold (note : HoldChartNote) : HoldNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , start := .button note.lane
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

private def buildTouchHold (note : TouchHoldChartNote) : HoldNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , start := .sensor note.sensorPos
  , state := HoldSubState.HeadWaiting
  , lengthSec := note.lengthSec
  , headDiffMs := 0.0
  , headGrade := .Miss
  , playerReleaseTimeSec := 0.0
  , isClassic := false
  , isTouchHold := true
  , touchHoldGroupId := note.touchHoldGroupId
  , touchHoldGroupSize := note.touchHoldGroupSize.getD 1
  , touchHoldGroupTriggered := false }

private def buildTouch (note : TouchChartNote) : TouchNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, isBreak := note.isBreak, isEX := false, noteIndex := note.noteIndex }
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

private def touchHoldNeighbors : SensorArea → List SensorArea
  | .A1 => [.D1, .D2, .E1, .E2, .B1]
  | .A2 => [.D2, .D3, .E2, .E3, .B2]
  | .A3 => [.D3, .D4, .E3, .E4, .B3]
  | .A4 => [.D4, .D5, .E4, .E5, .B4]
  | .A5 => [.D5, .D6, .E5, .E6, .B5]
  | .A6 => [.D6, .D7, .E6, .E7, .B6]
  | .A7 => [.D7, .D8, .E7, .E8, .B7]
  | .A8 => [.D8, .D1, .E8, .E1, .B8]
  | .D1 => [.A1, .A8, .E1]
  | .D2 => [.A2, .A1, .E2]
  | .D3 => [.A3, .A2, .E3]
  | .D4 => [.A4, .A3, .E4]
  | .D5 => [.A5, .A4, .E5]
  | .D6 => [.A6, .A5, .E6]
  | .D7 => [.A7, .A6, .E7]
  | .D8 => [.A8, .A7, .E8]
  | .E1 => [.D1, .A1, .A8, .B1, .B8]
  | .E2 => [.D2, .A2, .A1, .B2, .B1]
  | .E3 => [.D3, .A3, .A2, .B3, .B2]
  | .E4 => [.D4, .A4, .A3, .B4, .B3]
  | .E5 => [.D5, .A5, .A4, .B5, .B4]
  | .E6 => [.D6, .A6, .A5, .B6, .B5]
  | .E7 => [.D7, .A7, .A6, .B7, .B6]
  | .E8 => [.D8, .A8, .A7, .B8, .B7]
  | .B1 => [.E1, .E2, .B8, .B2, .A1, .C]
  | .B2 => [.E2, .E3, .B1, .B3, .A2, .C]
  | .B3 => [.E3, .E4, .B2, .B4, .A3, .C]
  | .B4 => [.E4, .E5, .B3, .B5, .A4, .C]
  | .B5 => [.E5, .E6, .B4, .B6, .A5, .C]
  | .B6 => [.E6, .E7, .B5, .B7, .A6, .C]
  | .B7 => [.E7, .E8, .B6, .B8, .A7, .C]
  | .B8 => [.E8, .E1, .B7, .B1, .A8, .C]
  | .C  => [.B1, .B2, .B3, .B4, .B5, .B6, .B7, .B8]

private def containsArea (items : List SensorArea) (value : SensorArea) : Bool :=
  items.any (fun item => item == value)

partial def collectTouchHoldComponent (pending : List SensorArea) (remaining : List SensorArea) (component : List SensorArea) : List SensorArea :=
  match pending with
  | [] => component
  | area :: rest =>
    if containsArea component area then
      collectTouchHoldComponent rest remaining component
    else
      let neighbors := touchHoldNeighbors area
      let newlyReached := remaining.filter (fun candidate => containsArea neighbors candidate)
      let remaining' := remaining.filter (fun candidate => candidate != area && !containsArea neighbors candidate)
      collectTouchHoldComponent (rest ++ newlyReached) remaining' (area :: component)

partial def assignTouchHoldGroupsLoop (allNotes : List TouchHoldChartNote) (remaining : List SensorArea) (groupId : Nat) (acc : List TouchHoldChartNote) : List TouchHoldChartNote :=
  match remaining with
  | [] => acc
  | area :: rest =>
    let component := collectTouchHoldComponent [area] remaining []
    let componentSize := List.length (allNotes.filter (fun note => containsArea component note.sensorPos))
    let nextAcc := acc.map (fun note =>
      if containsArea component note.sensorPos then
        { note with touchHoldGroupId := some groupId, touchHoldGroupSize := some componentSize }
      else
        note)
    let remaining' := rest.filter (fun candidate => !containsArea component candidate)
    assignTouchHoldGroupsLoop allNotes remaining' (groupId + 1) nextAcc

private def assignTouchHoldGroups (notes : List TouchHoldChartNote) : List TouchHoldChartNote :=
  let sensorTypes := notes.foldl (fun acc note => if containsArea acc note.sensorPos then acc else note.sensorPos :: acc) []
  assignTouchHoldGroupsLoop notes sensorTypes 0 notes

partial def assignTouchGroupsLoop (allNotes : List TouchChartNote) (remaining : List SensorArea) (groupId : Nat) (acc : List TouchChartNote) : List TouchChartNote :=
  match remaining with
  | [] => acc
  | area :: rest =>
    let component := collectTouchHoldComponent [area] remaining []
    let componentSize := List.length (allNotes.filter (fun note => containsArea component note.sensorPos))
    let nextAcc := acc.map (fun note =>
      if containsArea component note.sensorPos then
        { note with touchGroupId := some groupId, touchGroupSize := some componentSize }
      else
        note)
    let remaining' := rest.filter (fun candidate => !containsArea component candidate)
    assignTouchGroupsLoop allNotes remaining' (groupId + 1) nextAcc

private def assignTouchGroups (notes : List TouchChartNote) : List TouchChartNote :=
  let sensorTypes := notes.foldl (fun acc note => if containsArea acc note.sensorPos then acc else note.sensorPos :: acc) []
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
  { params := { judgeTimingSec := judgeTimingSec, judgeOffsetSec := Constants.JUDGE_OFFSET_SEC, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , lane := note.lane
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
  let tapQueues : ButtonQueueVec TapNote :=
    ButtonVec.ofFn (fun zone =>
      let notes := (sortByTiming (fun note => note.timingSec) (chart.taps.filter (fun note => note.lane == zone))).map buildTap
      { notes := notes })
  let holdQueues : ButtonQueueVec HoldNote :=
    ButtonVec.ofFn (fun zone =>
      let notes := (sortByTiming (fun note => note.timingSec) (chart.holds.filter (fun note => note.lane == zone))).map buildHold
      { notes := notes })
  let touchHoldNotes := assignTouchHoldGroups chart.touchHolds
  let touchNotesGrouped := assignTouchGroups chart.touches
  let touchHoldQueues : SensorQueueVec HoldNote :=
    SensorVec.ofFn (fun area =>
      let notes := (sortByTiming (fun note => note.timingSec) (touchHoldNotes.filter (fun note => note.sensorPos == area))).map buildTouchHold
      { notes := notes })
  let touchQueues : SensorQueueVec TouchNote :=
    SensorVec.ofFn (fun area =>
      let notes := (sortByTiming (fun note => note.timingSec) (touchNotesGrouped.filter (fun note => note.sensorPos == area))).map buildTouch
      { notes := notes })
  let activeHolds :=
    (chart.holds.map (fun note => (note.lane, buildHold note)))
  let activeTouchHolds :=
    (touchHoldNotes.map (fun note => (note.sensorPos, buildTouchHold note)))
  {
    currentTime := 0.0,
    prevButton := ButtonVec.replicate BUTTON_ZONE_COUNT false,
    prevSensor := SensorVec.replicate SENSOR_AREA_COUNT false,
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
