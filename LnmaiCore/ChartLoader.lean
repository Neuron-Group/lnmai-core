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
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure TouchChartNote where
  timingSec : Float
  sensorPos : Nat
  isBreak   : Bool := false
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure SlideAreaSpec where
  targetAreas : List Nat
  policy      : AreaPolicy := .Or
  isLast      : Bool := false
  isSkippable : Bool := true
  arrowProgressWhenOn : Nat := 0
  arrowProgressWhenFinished : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

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
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := 0.0, startPos := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , state := TapState.Waiting }

private def buildHold (note : HoldChartNote) : HoldNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := 0.0, startPos := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , state := HoldSubState.HeadWaiting
  , lengthSec := note.lengthSec
  , headDiffMs := 0.0
  , headGrade := .Miss
  , playerReleaseTimeSec := 0.0
  , isClassic := false
  , isTouchHold := note.isTouch }

private def buildTouch (note : TouchChartNote) : TouchNote :=
  { params := { judgeTimingSec := note.timingSec, judgeOffsetSec := 0.0, startPos := note.sensorPos, isBreak := note.isBreak, isEX := false, noteIndex := note.noteIndex }
  , state := TouchState.Waiting
  , sensorPos := note.sensorPos }

private def buildSlideArea (spec : SlideAreaSpec) : SlideArea :=
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
  let rec maxQueueLength : List (List SlideAreaSpec) → Nat
    | [] => 0
    | queue :: rest => Nat.max queue.length (maxQueueLength rest)
  { params := { judgeTimingSec := judgeTimingSec, judgeOffsetSec := 0.0, startPos := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
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
  , initialQueueRemaining := maxQueueLength note.judgeQueues
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
  let touchQueues : List (ZoneQueue TouchNote) :=
    (List.range SENSOR_AREA_COUNT).map (fun area =>
      let notes := (sortByTiming (fun note => note.timingSec) (chart.touches.filter (fun note => note.sensorPos == area))).map buildTouch
      { notes := notes })
  let activeHolds :=
    (chart.holds.filter (fun note => note.isTouch = false)).map (fun note => (note.lane, buildHold note))
  let activeTouchHolds :=
    (chart.touchHolds.map (fun note => (note.lane, buildHold note)))
  {
    currentTime := 0.0,
    prevButton := List.replicate BUTTON_ZONE_COUNT false,
    prevSensor := List.replicate SENSOR_AREA_COUNT false,
    tapQueues := tapQueues,
    holdQueues := holdQueues,
    touchQueues := touchQueues,
    slides := chart.slides.map (buildSlide (chart.slideSkipping.getD true)),
    activeHolds := activeHolds,
    activeTouchHolds := activeTouchHolds,
    currentBatch := {},
    score := {},
    judgeStyle := JudgeStyle.Default
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
