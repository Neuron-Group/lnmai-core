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
import LnmaiCore.Time
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

private def getObjValAsD? {α : Type} [FromJson α] (json : Json) (field : String) (fallback : α) : Except String α :=
  match json.getObjValAs? α field with
  | .ok value => pure value
  | .error _ => pure fallback

private def getObjOptionalValAsD? {α : Type} [FromJson α] (json : Json) (field : String) (fallback : α) : Except String α :=
  match json.getObjVal? field with
  | .ok valueJson => fromJson? valueJson
  | .error _ => pure fallback

structure TapChartNote where
  timing : TimePoint
  slot      : OuterSlot
  isBreak   : Bool := false
  isEX      : Bool := false
  buttonQueueIndex : Nat := 0
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure HoldChartNote where
  timing : TimePoint
  slot      : OuterSlot
  length : Duration
  isBreak   : Bool := false
  isEX      : Bool := false
  isTouch   : Bool := false
  isClassic : Option Bool := none
  buttonQueueIndex : Nat := 0
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure TouchHoldChartNote where
  timing : TimePoint
  sensorPos : SensorArea
  length : Duration
  isBreak   : Bool := false
  isEX      : Bool := false
  sourceGroupId : Option Nat := none
  sourceGroupIndex : Option Nat := none
  sourceGroupSize : Option Nat := none
  touchQueueIndex : Nat := 0
  touchGroupId : Option Nat := none
  touchGroupSize : Option Nat := none
  touchHoldGroupId : Option Nat := none
  touchHoldGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

structure TouchChartNote where
  timing : TimePoint
  sensorPos : SensorArea
  isBreak   : Bool := false
  sourceGroupId : Option Nat := none
  sourceGroupIndex : Option Nat := none
  sourceGroupSize : Option Nat := none
  touchQueueIndex : Nat := 0
  touchGroupId : Option Nat := none
  touchGroupSize : Option Nat := none
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson, FromJson

abbrev SlideAreaSpec := Simai.SlideAreaSpec

structure SlideHeadChartNote where
  timing : TimePoint
  slot : OuterSlot
  isBreak : Bool := false
  isEX : Bool := false
  logicalSlideId : Nat := 0
  noteIndex : Nat := 0
deriving Inhabited, Repr, ToJson

instance : FromJson SlideHeadChartNote where
  fromJson? json := do
    let timing ← json.getObjValAs? TimePoint "timing"
    let slot ← json.getObjValAs? OuterSlot "slot"
    let isBreak ← getObjValAsD? json "isBreak" false
    let isEX ← getObjValAsD? json "isEX" false
    let noteIndex ← getObjValAsD? json "noteIndex" 0
    let logicalSlideId ← getObjOptionalValAsD? json "logicalSlideId" noteIndex
    pure
      { timing := timing
      , slot := slot
      , isBreak := isBreak
      , isEX := isEX
      , logicalSlideId := logicalSlideId
      , noteIndex := noteIndex }

structure SlideChartNote where
  headTiming    : TimePoint
  slot          : OuterSlot
  length        : Duration
  startTiming   : TimePoint := TimePoint.zero
  slideKind     : SlideKind := .Single
  isClassic     : Bool := false
  isSlideNoHead : Bool := false
  isConnSlide   : Bool := false
  parentNoteIndex : Option Nat := none
  isGroupHead   : Bool := false
  isGroupEnd    : Bool := false
  parentFinished : Bool := false
  parentPendingFinish : Bool := false
  totalJudgeQueueLen : Nat := 0
  trackCount    : Nat := 1
  judgeAt       : Option TimePoint := none
  isBreak       : Bool := false
  isEX          : Bool := false
  multiple      : Nat := 1
  logicalSlideId : Nat := 0
  noteIndex     : Nat := 0
  judgeQueues   : List (List SlideAreaSpec) := []
  debugSimai : Option (String × String × Bool) := none
deriving Inhabited, Repr, ToJson

instance : FromJson SlideChartNote where
  fromJson? json := do
    let headTiming ← json.getObjValAs? TimePoint "headTiming"
    let slot ← json.getObjValAs? OuterSlot "slot"
    let length ← json.getObjValAs? Duration "length"
    let startTiming ← getObjValAsD? json "startTiming" TimePoint.zero
    let slideKind ← getObjValAsD? json "slideKind" .Single
    let isClassic ← getObjValAsD? json "isClassic" false
    let isSlideNoHead ← getObjValAsD? json "isSlideNoHead" false
    let isConnSlide ← getObjValAsD? json "isConnSlide" false
    let parentNoteIndex ← getObjValAsD? json "parentNoteIndex" none
    let isGroupHead ← getObjValAsD? json "isGroupHead" false
    let isGroupEnd ← getObjValAsD? json "isGroupEnd" false
    let parentFinished ← getObjValAsD? json "parentFinished" false
    let parentPendingFinish ← getObjValAsD? json "parentPendingFinish" false
    let totalJudgeQueueLen ← getObjValAsD? json "totalJudgeQueueLen" 0
    let trackCount ← getObjValAsD? json "trackCount" 1
    let judgeAt ← getObjValAsD? json "judgeAt" none
    let isBreak ← getObjValAsD? json "isBreak" false
    let isEX ← getObjValAsD? json "isEX" false
    let multiple ← getObjValAsD? json "multiple" 1
    let noteIndex ← getObjValAsD? json "noteIndex" 0
    let logicalSlideId ← getObjOptionalValAsD? json "logicalSlideId" noteIndex
    let judgeQueues ← getObjValAsD? json "judgeQueues" []
    let debugSimai ← getObjValAsD? json "debugSimai" none
    pure
      { headTiming := headTiming
      , slot := slot
      , length := length
      , startTiming := startTiming
      , slideKind := slideKind
      , isClassic := isClassic
      , isSlideNoHead := isSlideNoHead
      , isConnSlide := isConnSlide
      , parentNoteIndex := parentNoteIndex
      , isGroupHead := isGroupHead
      , isGroupEnd := isGroupEnd
      , parentFinished := parentFinished
      , parentPendingFinish := parentPendingFinish
      , totalJudgeQueueLen := totalJudgeQueueLen
      , trackCount := trackCount
      , judgeAt := judgeAt
      , isBreak := isBreak
      , isEX := isEX
      , multiple := multiple
      , logicalSlideId := logicalSlideId
      , noteIndex := noteIndex
      , judgeQueues := judgeQueues
      , debugSimai := debugSimai }

structure ChartSpec where
  taps       : List TapChartNote := []
  holds      : List HoldChartNote := []
  touches    : List TouchChartNote := []
  touchHolds : List TouchHoldChartNote := []
  slideHeads : List SlideHeadChartNote := []
  slides     : List SlideChartNote := []
  slideSkipping : Option Bool := none
deriving Inhabited, Repr, ToJson, FromJson

private def insertByTiming {α : Type} (getTiming : α → TimePoint) (item : α) : List α → List α
  | [] => [item]
  | head :: rest =>
    if getTiming item ≤ getTiming head then
      item :: head :: rest
    else
      head :: insertByTiming getTiming item rest

private def sortByTiming {α : Type} (getTiming : α → TimePoint) (items : List α) : List α :=
  items.foldl (fun acc item => insertByTiming getTiming item acc) []

private def insertByNatKey {α : Type} (getKey : α → Nat) (item : α) : List α → List α
  | [] => [item]
  | head :: rest =>
    if getKey item ≤ getKey head then
      item :: head :: rest
    else
      head :: insertByNatKey getKey item rest

private def sortByNatKey {α : Type} (getKey : α → Nat) (items : List α) : List α :=
  items.foldl (fun acc item => insertByNatKey getKey item acc) []

private def findButtonQueueIndexByNoteIndex (notes : List TapChartNote) (noteIndex : Nat) : Nat :=
  match notes.find? (fun note => note.noteIndex = noteIndex) with
  | some note => note.buttonQueueIndex
  | none => 0

private def buildTap (note : TapChartNote) : TapFamilyNote :=
  .tap
    { params := { judgeTiming := note.timing, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
    , lane := note.slot
    , state := TapState.Waiting
    , buttonQueueIndex := note.buttonQueueIndex }

private def buildSlideHead (note : SlideHeadChartNote) : TapFamilyNote :=
  .slideHead
    { params := { judgeTiming := note.timing, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
    , lane := note.slot
    , state := TapState.Waiting
    , logicalSlideId := note.logicalSlideId
    , buttonQueueIndex := 0 }

private def buildHold (note : HoldChartNote) : HoldNote :=
  { params := { judgeTiming := note.timing, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , start := .button note.slot.toButtonZone
  , state := HoldSubState.HeadWaiting
  , length := note.length
  , buttonQueueIndex := note.buttonQueueIndex
  , headDiff := Duration.zero
  , headGrade := .Miss
  , playerReleaseTime := Duration.zero
  , isClassic := note.isClassic.getD false
  , isTouchHold := note.isTouch
  , touchHoldGroupId := note.touchHoldGroupId
  , touchHoldGroupSize := note.touchHoldGroupSize.getD 1
  , touchHoldGroupTriggered := false }

private def buildTouchHold (note : TouchHoldChartNote) : HoldNote :=
  { params := { judgeTiming := note.timing, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , start := .sensor note.sensorPos
  , state := HoldSubState.HeadWaiting
  , length := note.length
  , headDiff := Duration.zero
  , headGrade := .Miss
  , playerReleaseTime := Duration.zero
  , isClassic := false
  , isTouchHold := true
  , touchQueueIndex := note.touchQueueIndex
  , touchGroupId := note.touchGroupId
  , touchGroupSize := note.touchGroupSize.getD 1
  , touchHoldGroupId := note.touchHoldGroupId
  , touchHoldGroupSize := note.touchHoldGroupSize.getD 1
  , touchHoldGroupTriggered := false }

private def buildTouch (note : TouchChartNote) : TouchNote :=
  { params := { judgeTiming := note.timing, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := false, noteIndex := note.noteIndex }
  , state := TouchState.Waiting
  , sensorPos := note.sensorPos
  , touchQueueIndex := note.touchQueueIndex
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

private partial def assignTouchHoldGroupsLoop (allNotes : List TouchHoldChartNote) (remaining : List SensorArea) (groupId : Nat) (acc : List TouchHoldChartNote) : List TouchHoldChartNote :=
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

private def assignTouchHoldGroupsForBatch (notes : List TouchHoldChartNote) : List TouchHoldChartNote :=
  let sensorTypes := notes.foldl (fun acc note => if containsArea acc note.sensorPos then acc else note.sensorPos :: acc) []
  assignTouchHoldGroupsLoop notes sensorTypes 0 notes

private partial def assignTouchGroupsLoop (allNotes : List TouchChartNote) (remaining : List SensorArea) (groupId : Nat) (acc : List TouchChartNote) : List TouchChartNote :=
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

private def assignTouchGroupsForBatch (notes : List TouchChartNote) : List TouchChartNote :=
  let sensorTypes := notes.foldl (fun acc note => if containsArea acc note.sensorPos then acc else note.sensorPos :: acc) []
  assignTouchGroupsLoop notes sensorTypes 0 notes

private def isEachSourceGroupId? (sourceGroupId : Option Nat) (sourceGroupSize : Option Nat) : Option Nat :=
  match sourceGroupId, sourceGroupSize with
  | some groupId, some size =>
      if size > 1 then some groupId else none
  | _, _ => none

private def rewriteTouchGroupIds (notes : List TouchChartNote) (groupIdMap : List (Nat × Nat)) : List TouchChartNote :=
  notes.map (fun note =>
    match note.touchGroupId, note.sourceGroupId with
    | some localId, some sourceGroupId =>
        match groupIdMap.find? (fun entry => entry.1 = sourceGroupId) with
        | some (_, globalId) => { note with touchGroupId := some (globalId + localId) }
        | none => note
    | _, _ => note)

private def rewriteTouchHoldGroupIds (notes : List TouchHoldChartNote) (groupIdMap : List (Nat × Nat)) : List TouchHoldChartNote :=
  notes.map (fun note =>
    match note.touchHoldGroupId, note.sourceGroupId with
    | some localId, some sourceGroupId =>
        match groupIdMap.find? (fun entry => entry.1 = sourceGroupId) with
        | some (_, globalId) => { note with touchHoldGroupId := some (globalId + localId) }
        | none => note
    | _, _ => note)

private def assignTouchGroupsBySourceGroup
    (touches : List TouchChartNote)
    (touchHolds : List TouchHoldChartNote) : List TouchChartNote × List TouchHoldChartNote :=
  let sourceGroupIds :=
    ((touches.foldl (fun acc note =>
      match isEachSourceGroupId? note.sourceGroupId note.sourceGroupSize with
      | some groupId => if acc.contains groupId then acc else groupId :: acc
      | none => acc) [])
      |> fun acc => touchHolds.foldl (fun acc note =>
        match isEachSourceGroupId? note.sourceGroupId note.sourceGroupSize with
        | some groupId => if acc.contains groupId then acc else groupId :: acc
        | none => acc) acc).reverse
  let rec loop (remaining : List Nat) (nextTouchGroupId nextTouchHoldGroupId : Nat)
      (touchAcc : List TouchChartNote) (touchHoldAcc : List TouchHoldChartNote) :
      List TouchChartNote × List TouchHoldChartNote :=
    match remaining with
    | [] => (touchAcc, touchHoldAcc)
    | sourceGroupId :: rest =>
        let touchBatch :=
          touches.filter (fun note => isEachSourceGroupId? note.sourceGroupId note.sourceGroupSize = some sourceGroupId)
        let touchHoldBatch :=
          touchHolds.filter (fun note => isEachSourceGroupId? note.sourceGroupId note.sourceGroupSize = some sourceGroupId)
        let combinedTouchBatch :=
          touchBatch ++ (touchHoldBatch.map (fun note =>
            { timing := note.timing
            , sensorPos := note.sensorPos
            , isBreak := note.isBreak
            , sourceGroupId := note.sourceGroupId
            , sourceGroupIndex := note.sourceGroupIndex
            , sourceGroupSize := note.sourceGroupSize
            , noteIndex := note.noteIndex }))
        let combinedGrouped := assignTouchGroupsForBatch combinedTouchBatch
        let touchGrouped :=
          combinedGrouped.filterMap (fun note =>
            if touchBatch.any (fun original => original.noteIndex = note.noteIndex) then some note else none)
        let touchHoldGroupMetadata :=
          combinedGrouped.filterMap (fun note =>
            if touchHoldBatch.any (fun original => original.noteIndex = note.noteIndex) then
              some (note.noteIndex, note.touchGroupId, note.touchGroupSize)
            else
              none)
        let touchHoldBodyGrouped := assignTouchHoldGroupsForBatch touchHoldBatch
        let touchGroupBase := nextTouchGroupId
        let touchHoldGroupBase := nextTouchHoldGroupId
        let touchComponentCount :=
          ((touchGrouped.filterMap (fun note => note.touchGroupId)).eraseDups).length
        let touchHoldComponentCount :=
          ((touchHoldBodyGrouped.filterMap (fun note => note.touchHoldGroupId)).eraseDups).length
        let touchGrouped' :=
          touchGrouped.map (fun note =>
            match note.touchGroupId with
            | some gid => { note with touchGroupId := some (touchGroupBase + gid) }
            | none => note)
        let touchHoldWithTouchGroup :=
          touchHoldBatch.map (fun note =>
            match touchHoldGroupMetadata.find? (fun entry => entry.1 = note.noteIndex) with
            | some (_, gid, size) => { note with touchGroupId := gid.map (fun id => touchGroupBase + id), touchGroupSize := size }
            | none => note)
        let touchHoldGrouped' :=
          (assignTouchHoldGroupsForBatch touchHoldWithTouchGroup).map (fun note =>
            match note.touchHoldGroupId with
            | some gid => { note with touchHoldGroupId := some (touchHoldGroupBase + gid) }
            | none => note)
        let touchAcc' :=
          touchAcc.map (fun note =>
            match touchGrouped'.find? (fun grouped => grouped.noteIndex = note.noteIndex) with
            | some grouped => grouped
            | none => note)
        let touchHoldAcc' :=
          touchHoldAcc.map (fun note =>
            match touchHoldGrouped'.find? (fun grouped => grouped.noteIndex = note.noteIndex) with
            | some grouped => grouped
            | none => note)
        loop rest (touchGroupBase + touchComponentCount) (touchHoldGroupBase + touchHoldComponentCount) touchAcc' touchHoldAcc'
  loop sourceGroupIds 0 0 touches touchHolds

private def uniqueAreasFromTouches (touches : List TouchChartNote) (touchHolds : List TouchHoldChartNote) : List SensorArea :=
  let fromTouches := touches.foldl (fun acc note => if containsArea acc note.sensorPos then acc else note.sensorPos :: acc) []
  touchHolds.foldl (fun acc note => if containsArea acc note.sensorPos then acc else note.sensorPos :: acc) fromTouches

private def assignSharedTouchQueueIndices (touches : List TouchChartNote) (touchHolds : List TouchHoldChartNote) : List TouchChartNote × List TouchHoldChartNote :=
  let allAreas := uniqueAreasFromTouches touches touchHolds
  let rec mergeAssign (index : Nat) (ts : List TouchChartNote) (hs : List TouchHoldChartNote) (accT : List TouchChartNote) (accH : List TouchHoldChartNote) : List TouchChartNote × List TouchHoldChartNote :=
    match ts, hs with
    | [], [] => (accT, accH)
    | t :: ts', [] => mergeAssign (index + 1) ts' [] ({ t with touchQueueIndex := index } :: accT) accH
    | [], h :: hs' => mergeAssign (index + 1) [] hs' accT ({ h with touchQueueIndex := index } :: accH)
    | t :: ts', h :: hs' =>
      if t.timing ≤ h.timing then
        mergeAssign (index + 1) ts' (h :: hs') ({ t with touchQueueIndex := index } :: accT) accH
      else
        mergeAssign (index + 1) (t :: ts') hs' accT ({ h with touchQueueIndex := index } :: accH)
  let rec loop (areas : List SensorArea) (accT : List TouchChartNote) (accH : List TouchHoldChartNote) : List TouchChartNote × List TouchHoldChartNote :=
    match areas with
    | [] => (accT, accH)
    | area :: rest =>
      let ts := sortByTiming (fun note => note.timing) (touches.filter (fun note => note.sensorPos == area))
      let hs := sortByTiming (fun note => note.timing) (touchHolds.filter (fun note => note.sensorPos == area))
      let (accT', accH') := mergeAssign 0 ts hs accT accH
      loop rest accT' accH'
  let (touches', touchHolds') := loop allAreas [] []
  (touches'.reverse, touchHolds'.reverse)

private def assignSharedButtonQueueIndices (taps : List TapChartNote) (holds : List HoldChartNote) : List TapChartNote × List HoldChartNote :=
  let allZones :=
    let fromTaps := taps.foldl (fun acc note => if acc.contains note.slot.toButtonZone then acc else note.slot.toButtonZone :: acc) []
    holds.foldl (fun acc note => if acc.contains note.slot.toButtonZone then acc else note.slot.toButtonZone :: acc) fromTaps
  let rec mergeAssign (index : Nat) (ts : List TapChartNote) (hs : List HoldChartNote) (accT : List TapChartNote) (accH : List HoldChartNote) : List TapChartNote × List HoldChartNote :=
    match ts, hs with
    | [], [] => (accT, accH)
    | t :: ts', [] => mergeAssign (index + 1) ts' [] ({ t with buttonQueueIndex := index } :: accT) accH
    | [], h :: hs' => mergeAssign (index + 1) [] hs' accT ({ h with buttonQueueIndex := index } :: accH)
    | t :: ts', h :: hs' =>
      if t.timing ≤ h.timing then
        mergeAssign (index + 1) ts' (h :: hs') ({ t with buttonQueueIndex := index } :: accT) accH
      else
        mergeAssign (index + 1) (t :: ts') hs' accT ({ h with buttonQueueIndex := index } :: accH)
  let rec loop (zones : List ButtonZone) (accT : List TapChartNote) (accH : List HoldChartNote) : List TapChartNote × List HoldChartNote :=
    match zones with
    | [] => (accT, accH)
    | zone :: rest =>
      let ts := sortByTiming (fun note => note.timing) (taps.filter (fun note => note.slot.toButtonZone == zone))
      let hs := sortByTiming (fun note => note.timing) (holds.filter (fun note => note.slot.toButtonZone == zone))
      let (accT', accH') := mergeAssign 0 ts hs accT accH
      loop rest accT' accH'
  let (taps', holds') := loop allZones [] []
  (taps'.reverse, holds'.reverse)

private def buildSlide (slideSkipping : Bool) (note : SlideChartNote) : SlideNote :=
  let judgeQueues :=
    let queues := note.judgeQueues.map (fun queue => queue.map buildSlideArea)
    if !slideSkipping then
      disableSlideSkipping queues
    else
      match queues with
      | [queue] => [applySingleTrackConnRules note queue]
      | _ => queues
  let judgeTiming := note.judgeAt.getD note.headTiming
  let waitTime := note.startTiming + note.length - judgeTiming
  let rec maxQueueLength : List (List SlideArea) → Nat
    | [] => 0
    | queue :: rest => Nat.max queue.length (maxQueueLength rest)
  { params := { judgeTiming := judgeTiming, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
  , lane := note.slot
  , state := SlideState.Active waitTime
  , length := note.length
  , headTiming := note.headTiming
  , startTiming := note.startTiming
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
  , multiple := max 1 note.multiple
  , judgeQueues := judgeQueues }

private def touchHoldBodyGroupStatesFromHolds
    (holds : List (SensorArea × HoldNote)) : List TouchHoldBodyGroupState :=
  let rec loop (remaining : List (SensorArea × HoldNote)) (acc : List TouchHoldBodyGroupState) :
      List TouchHoldBodyGroupState :=
    match remaining with
    | [] => acc.reverse
    | (_, note) :: rest =>
        match note.touchHoldGroupId with
        | none => loop rest acc
        | some groupId =>
            let noteIndex := note.params.noteIndex
            let triggered := note.touchHoldGroupTriggered
            let rec upsert (items : List TouchHoldBodyGroupState) : List TouchHoldBodyGroupState :=
              match items with
              | [] =>
                  let triggeredNoteIndices := if triggered then [noteIndex] else []
                  [{ groupId := groupId, memberNoteIndices := [noteIndex], triggeredNoteIndices := triggeredNoteIndices }]
              | item :: tail =>
                  if item.groupId == groupId then
                    let memberNoteIndices :=
                      if item.memberNoteIndices.contains noteIndex then item.memberNoteIndices
                      else noteIndex :: item.memberNoteIndices
                    let triggeredNoteIndices :=
                      if triggered && !item.triggeredNoteIndices.contains noteIndex then
                        noteIndex :: item.triggeredNoteIndices
                      else if !triggered then
                        item.triggeredNoteIndices.erase noteIndex
                      else
                        item.triggeredNoteIndices
                    { item with
                        memberNoteIndices := memberNoteIndices
                      , triggeredNoteIndices := triggeredNoteIndices } :: tail
                  else
                    item :: upsert tail
            loop rest (upsert acc)
  loop holds []

private structure ChartScoreTotals where
  totalBase : Nat := 0
  totalExtra : Nat := 0
  noteCount : Nat := 0

private def addScoreTotal
    (totals : ChartScoreTotals) (kind : NoteType) (isBreak : Bool) (multiple : Nat := 1) :
    ChartScoreTotals :=
  let multiple := max 1 multiple
  let scoreKind := if isBreak then NoteType.Break else kind
  { totalBase := totals.totalBase + NoteType.baseScore scoreKind * multiple
  , totalExtra := totals.totalExtra + NoteType.extraScore scoreKind * multiple
  , noteCount := totals.noteCount + multiple }

private def chartScoreTotals (chart : ChartSpec) : ChartScoreTotals :=
  let totals := chart.taps.foldl (fun totals note => addScoreTotal totals .Tap note.isBreak) {}
  let totals := chart.slideHeads.foldl (fun totals note => addScoreTotal totals .Tap note.isBreak) totals
  let totals := chart.holds.foldl (fun totals note => addScoreTotal totals .Hold note.isBreak) totals
  let totals :=
    chart.touchHolds.foldl (fun totals note => addScoreTotal totals .Hold note.isBreak) totals
  let totals := chart.touches.foldl (fun totals note => addScoreTotal totals .Touch note.isBreak) totals
  chart.slides.foldl
    (fun totals note =>
      if note.isConnSlide && !note.isGroupEnd then totals
      else addScoreTotal totals .Slide note.isBreak note.multiple)
    totals

def buildGameState (chart : ChartSpec) : GameState :=
  let scoreTotals := chartScoreTotals chart
  let tapFamilyHeads := chart.taps ++ chart.slideHeads.map (fun note =>
    { timing := note.timing
    , slot := note.slot
    , isBreak := note.isBreak
    , isEX := note.isEX
    , noteIndex := note.noteIndex })
  let (tapsWithIndices, holdsWithIndices) := assignSharedButtonQueueIndices tapFamilyHeads chart.holds
  let tapQueues : ButtonQueueVec TapFamilyNote :=
    ButtonVec.ofFn (fun zone =>
      let taps :=
        (sortByNatKey (fun note => note.buttonQueueIndex)
          (tapsWithIndices.filter (fun note => note.slot.toButtonZone == zone && !(chart.slideHeads.any (fun head => head.noteIndex = note.noteIndex))))).map buildTap
      let slideHeads :=
        (sortByNatKey (fun note => findButtonQueueIndexByNoteIndex tapsWithIndices note.noteIndex)
          (chart.slideHeads.filter (fun note => note.slot.toButtonZone == zone))).map (fun note =>
          let buttonQueueIndex := findButtonQueueIndexByNoteIndex tapsWithIndices note.noteIndex
          .slideHead
            { params := { judgeTiming := note.timing, judgeOffset := Constants.JUDGE_OFFSET, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex }
            , lane := note.slot
            , state := TapState.Waiting
            , logicalSlideId := note.logicalSlideId
            , buttonQueueIndex := buttonQueueIndex })
      let notes := sortByNatKey (fun note => note.buttonQueueIndex) (taps ++ slideHeads)
      { notes := notes })
  let holdQueues : ButtonQueueVec HoldNote :=
    ButtonVec.ofFn (fun zone =>
      let notes := (sortByNatKey (fun note => note.buttonQueueIndex) (holdsWithIndices.filter (fun note => note.slot.toButtonZone == zone))).map buildHold
      { notes := notes })
  let (touchNotesGrouped0, touchHoldNotes0) := assignTouchGroupsBySourceGroup chart.touches chart.touchHolds
  let (touchNotesGrouped, touchHoldNotesQueued) := assignSharedTouchQueueIndices touchNotesGrouped0 touchHoldNotes0
  let touchHoldNotes :=
    touchHoldNotesQueued.map (fun note =>
      match touchNotesGrouped.find? (fun touch => touch.touchQueueIndex == note.touchQueueIndex && touch.sensorPos == note.sensorPos) with
      | some touch => { note with touchGroupId := touch.touchGroupId, touchGroupSize := touch.touchGroupSize }
      | none => note)
  let touchHoldQueues : SensorQueueVec HoldNote :=
    SensorVec.ofFn (fun area =>
      let notes := (sortByNatKey (fun note => note.touchQueueIndex) (touchHoldNotes.filter (fun note => note.sensorPos == area))).map buildTouchHold
      { notes := notes })
  let touchQueues : SensorQueueVec TouchNote :=
    SensorVec.ofFn (fun area =>
      let notes := (sortByNatKey (fun note => note.touchQueueIndex) (touchNotesGrouped.filter (fun note => note.sensorPos == area))).map buildTouch
      { notes := notes })
  let activeHolds : List (ButtonZone × HoldNote) :=
    ButtonZone.all.foldr (fun zone acc =>
      let queue := holdQueues.getD zone { notes := [] }
      let entries := queue.notes.map (fun note => (zone, note))
      entries ++ acc) []
  let activeTouchHolds : List (SensorArea × HoldNote) :=
    touchHoldQueues.entries.foldr (fun entry acc =>
      let entries := entry.2.notes.map (fun note => (entry.1, note))
      entries ++ acc) []
  let touchHoldGroupStates := touchHoldBodyGroupStatesFromHolds activeTouchHolds
  {
    currentTime := TimePoint.zero,
    prevButton := ButtonVec.replicate BUTTON_ZONE_COUNT false,
    prevSensor := SensorVec.replicate SENSOR_AREA_COUNT false,
    buttonQueueFrontiers := ButtonVec.replicate BUTTON_ZONE_COUNT 0,
    touchQueueFrontiers := SensorVec.replicate SENSOR_AREA_COUNT 0,
    tapQueues := tapQueues,
    holdQueues := holdQueues,
    touchHoldQueues := touchHoldQueues,
    touchQueues := touchQueues,
    slides := chart.slides.map (buildSlide (chart.slideSkipping.getD true)),
    activeHolds := activeHolds,
    activeTouchHolds := activeTouchHolds,
    touchGroupStates := [],
    touchHoldGroupStates := touchHoldGroupStates,
    currentBatch := {},
    score :=
      { totalBase := scoreTotals.totalBase
      , totalExtra := scoreTotals.totalExtra
      , maxDxScore := scoreTotals.noteCount * 3 },
    judgeStyle := JudgeStyle.Default,
    touchPanelOffset := Constants.TOUCH_PANEL_OFFSET
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
