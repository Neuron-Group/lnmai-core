/-
  Core domain types for the LnMai game judgment engine.

  Pure data types for grades, notes, difficulty modes, lifecycle states,
  score tracking, and events. Floats prevent deriving DecidableEq on
  some structures; manual Eq instances are provided where needed.
-/

import Mathlib
import Lean.Data.Json
import LnmaiCore.Areas
import LnmaiCore.Time

open Lean

set_option linter.unusedVariables false

namespace LnmaiCore

private def getObjValAsD? {α : Type} [FromJson α] (json : Json) (field : String) (fallback : α) :
    Except String α :=
  match json.getObjValAs? α field with
  | .ok value => pure value
  | .error _ => pure fallback

----------------------------------------------------------------------------
-- Typed runtime note/event positions
----------------------------------------------------------------------------

inductive RuntimePos where
  | button (zone : ButtonZone)
  | sensor (area : SensorArea)
deriving DecidableEq, Repr, Inhabited

def RuntimePos.buttonZone? : RuntimePos → Option ButtonZone
  | .button zone => some zone
  | .sensor _ => none

def RuntimePos.sensorArea? : RuntimePos → Option SensorArea
  | .button _ => none
  | .sensor area => some area

instance : ToJson RuntimePos where
  toJson
    | .button zone => Json.mkObj [("button", toJson zone)]
    | .sensor area => Json.mkObj [("sensor", toJson area)]

instance : FromJson RuntimePos where
  fromJson?
    | json@(Json.obj _) =>
        match json.getObjVal? "button" with
        | .ok buttonJson => RuntimePos.button <$> fromJson? buttonJson
        | .error _ =>
            match json.getObjVal? "sensor" with
            | .ok sensorJson => RuntimePos.sensor <$> fromJson? sensorJson
            | .error _ => .error "invalid RuntimePos"
    | _ => .error "invalid RuntimePos"

----------------------------------------------------------------------------
-- Judgment Grades (15-tier lattice, ordered by quality ascending)
----------------------------------------------------------------------------

inductive JudgeGrade where
  | Miss
  | LateGood
  | LateGreat3rd
  | LateGreat2nd
  | LateGreat
  | LatePerfect3rd
  | LatePerfect2nd
  | Perfect          -- Critical Perfect: center of the timing window
  | FastPerfect2nd
  | FastPerfect3rd
  | FastGreat
  | FastGreat2nd
  | FastGreat3rd
  | FastGood
  | TooFast
deriving DecidableEq, Ord, Repr, Inhabited, ToJson, FromJson

instance : ToString JudgeGrade where
  toString
    | JudgeGrade.Miss           => "Miss"
    | JudgeGrade.LateGood       => "LateGood"
    | JudgeGrade.LateGreat3rd   => "LateGreat3rd"
    | JudgeGrade.LateGreat2nd   => "LateGreat2nd"
    | JudgeGrade.LateGreat      => "LateGreat"
    | JudgeGrade.LatePerfect3rd => "LatePerfect3rd"
    | JudgeGrade.LatePerfect2nd => "LatePerfect2nd"
    | JudgeGrade.Perfect        => "Perfect"
    | JudgeGrade.FastPerfect2nd => "FastPerfect2nd"
    | JudgeGrade.FastPerfect3rd => "FastPerfect3rd"
    | JudgeGrade.FastGreat      => "FastGreat"
    | JudgeGrade.FastGreat2nd   => "FastGreat2nd"
    | JudgeGrade.FastGreat3rd   => "FastGreat3rd"
    | JudgeGrade.FastGood       => "FastGood"
    | JudgeGrade.TooFast        => "TooFast"

namespace JudgeGrade

def isMissOrTooFast : JudgeGrade → Bool
  | Miss    => true
  | TooFast => true
  | _       => false

def isFast : JudgeGrade → Bool
  | FastPerfect2nd | FastPerfect3rd | FastGreat
  | FastGreat2nd   | FastGreat3rd   | FastGood
  | TooFast => true
  | _       => false

def isLate : JudgeGrade → Bool
  | LateGood  | LateGreat3rd | LateGreat2nd | LateGreat
  | LatePerfect3rd | LatePerfect2nd | Miss => true
  | _ => false

/-- Distance from Critical Perfect (0 = Perfect, 7 = Miss/TooFast) -/
def distFromPerfect : JudgeGrade → Nat
  | Miss            => 7
  | LateGood        => 6
  | LateGreat3rd    => 5
  | LateGreat2nd    => 4
  | LateGreat       => 3
  | LatePerfect3rd  => 2
  | LatePerfect2nd  => 1
  | Perfect         => 0
  | FastPerfect2nd  => 1
  | FastPerfect3rd  => 2
  | FastGreat       => 3
  | FastGreat2nd    => 4
  | FastGreat3rd    => 5
  | FastGood        => 6
  | TooFast         => 7

def isPerfectGrade : JudgeGrade → Bool
  | Perfect | LatePerfect2nd | LatePerfect3rd | FastPerfect2nd | FastPerfect3rd => true
  | _ => false

def isGreatGrade : JudgeGrade → Bool
  | LateGreat | LateGreat2nd | LateGreat3rd | FastGreat | FastGreat2nd | FastGreat3rd => true
  | _ => false

def isGoodGrade : JudgeGrade → Bool
  | LateGood | FastGood => true
  | _ => false

end JudgeGrade

----------------------------------------------------------------------------
-- Judge display options
----------------------------------------------------------------------------

/--
  Display/counting option mirrored from MajdataPlay's `JudgeDisplayOption`.

  The runtime uses this for fast/late statistic folding. MajdataPlay's
  `ObjectCounter.UpdateFastLateCount` treats `BelowGR` and `Disable` like
  `BelowP` for statistics, while `MissOnly` contributes no fast/late count.
-/
inductive JudgeDisplayOption where
  | All
  | BelowCP
  | BelowP
  | BelowGR
  | MissOnly
  | Disable
deriving DecidableEq, Repr, Inhabited, ToJson, FromJson

----------------------------------------------------------------------------
-- Note Type
----------------------------------------------------------------------------

inductive NoteType where
  | Tap | Hold | Slide | Touch | Break
deriving DecidableEq, Repr, Inhabited, BEq, ToJson, FromJson

inductive SlideKind where
  | Single | Wifi | ConnPart
deriving DecidableEq, Repr, Inhabited, BEq, ToJson, FromJson

inductive AreaPolicy where
  | Or | And
deriving DecidableEq, Repr, Inhabited, BEq, ToJson, FromJson

namespace NoteType

def baseScore : NoteType → Nat
  | Tap   => 500
  | Hold  => 1000
  | Slide => 1500
  | Touch => 500
  | Break => 2500

def extraScore : NoteType → Nat
  | Break => 100
  | _     => 0

end NoteType

----------------------------------------------------------------------------
-- Difficulty / Judgment Style
----------------------------------------------------------------------------

inductive JudgeStyle where
  | Default | Maji | Gachi | Gori
deriving DecidableEq, Repr, Inhabited, BEq, ToJson, FromJson

----------------------------------------------------------------------------
-- Note Lifecycle Status
----------------------------------------------------------------------------

inductive NoteStatus where
  | Start | Inited | Scaling | Running | Arrived | End
deriving DecidableEq, Repr, Inhabited

namespace NoteStatus

def leq : NoteStatus → NoteStatus → Bool
  | Start,    _         => true
  | Inited,   Start     => false
  | Inited,   _         => true
  | Scaling,  Start     => false
  | Scaling,  Inited    => false
  | Scaling,  _         => true
  | Running,  Start     => false
  | Running,  Inited    => false
  | Running,  Scaling   => false
  | Running,  _         => true
  | Arrived,  Start     => false
  | Arrived,  Inited    => false
  | Arrived,  Scaling   => false
  | Arrived,  Running   => false
  | Arrived,  _         => true
  | End,      End       => true
  | End,      _         => false

end NoteStatus

----------------------------------------------------------------------------
-- Combo State
----------------------------------------------------------------------------

inductive ComboState where
  | None | FC | FCPlus | AP | APPlus
deriving DecidableEq, Ord, Repr, Inhabited

----------------------------------------------------------------------------
-- A single note's judgment result
----------------------------------------------------------------------------

structure NoteJudgeResult where
  grade    : JudgeGrade
  diff     : Duration
  isBreak  : Bool := false
  isEX     : Bool := false
deriving Repr, Inhabited, ToJson, FromJson

structure GroupState where
  groupId : Nat
  count   : Nat
  size    : Nat
  grade   : JudgeGrade
  diff    : Duration
deriving Repr, Inhabited, ToJson, FromJson

structure TouchHoldBodyGroupState where
  groupId : Nat
  memberNoteIndices : List Nat := []
  triggeredNoteIndices : List Nat := []
deriving Repr, Inhabited, ToJson, FromJson

namespace NoteJudgeResult

def isFast (r : NoteJudgeResult) : Bool := r.diff < Duration.zero

def isMissOrTooFast (r : NoteJudgeResult) : Bool :=
  r.grade.isMissOrTooFast

end NoteJudgeResult

----------------------------------------------------------------------------
-- Per-note-type judge counts
----------------------------------------------------------------------------

abbrev JudgeCounts := JudgeGrade → Nat

instance : Inhabited JudgeCounts := ⟨λ _ => 0⟩
instance : Repr JudgeCounts where
  reprPrec _ _ := "<JudgeCounts>"

def emptyJudgeCounts : JudgeCounts := λ _ => 0

structure NoteTypeJudgeCounts where
  tapCount    : JudgeCounts := emptyJudgeCounts
  holdCount   : JudgeCounts := emptyJudgeCounts
  slideCount  : JudgeCounts := emptyJudgeCounts
  touchCount  : JudgeCounts := emptyJudgeCounts
  breakCount  : JudgeCounts := emptyJudgeCounts
deriving Inhabited, Repr

def emptyNoteTypeJudgeCounts : NoteTypeJudgeCounts where
  tapCount   := emptyJudgeCounts
  holdCount  := emptyJudgeCounts
  slideCount := emptyJudgeCounts
  touchCount := emptyJudgeCounts
  breakCount := emptyJudgeCounts

private def judgeCountsGrades : List JudgeGrade :=
  [ .Miss
  , .LateGood
  , .LateGreat3rd
  , .LateGreat2nd
  , .LateGreat
  , .LatePerfect3rd
  , .LatePerfect2nd
  , .Perfect
  , .FastPerfect2nd
  , .FastPerfect3rd
  , .FastGreat
  , .FastGreat2nd
  , .FastGreat3rd
  , .FastGood
  , .TooFast ]

private def judgeCountsToJson (counts : JudgeCounts) : Json :=
  Json.mkObj <| judgeCountsGrades.map (fun grade => (toString grade, toJson (counts grade)))

private def judgeCountsFromJson? (json : Json) : Except String JudgeCounts := do
  let fields ← judgeCountsGrades.mapM (fun grade => do
    let value ← json.getObjValAs? Nat (toString grade)
    pure (grade, value))
  pure <| fun grade =>
    match fields.find? (fun entry => entry.1 == grade) with
    | some (_, value) => value
    | none => 0

instance : ToJson NoteTypeJudgeCounts where
  toJson counts :=
    Json.mkObj
      [ ("tapCount", judgeCountsToJson counts.tapCount)
      , ("holdCount", judgeCountsToJson counts.holdCount)
      , ("slideCount", judgeCountsToJson counts.slideCount)
      , ("touchCount", judgeCountsToJson counts.touchCount)
      , ("breakCount", judgeCountsToJson counts.breakCount) ]

instance : FromJson NoteTypeJudgeCounts where
  fromJson? json := do
    let tapCount ← judgeCountsFromJson? (← json.getObjVal? "tapCount")
    let holdCount ← judgeCountsFromJson? (← json.getObjVal? "holdCount")
    let slideCount ← judgeCountsFromJson? (← json.getObjVal? "slideCount")
    let touchCount ← judgeCountsFromJson? (← json.getObjVal? "touchCount")
    let breakCount ← judgeCountsFromJson? (← json.getObjVal? "breakCount")
    pure
      { tapCount := tapCount
      , holdCount := holdCount
      , slideCount := slideCount
      , touchCount := touchCount
      , breakCount := breakCount }

def NoteTypeJudgeCounts.gradeCount (counts : NoteTypeJudgeCounts) (grade : JudgeGrade) : Nat :=
  counts.tapCount grade + counts.holdCount grade + counts.slideCount grade +
    counts.touchCount grade + counts.breakCount grade

def NoteTypeJudgeCounts.gradeCountWhere
    (counts : NoteTypeJudgeCounts) (pred : JudgeGrade → Bool) : Nat :=
  judgeCountsGrades.foldl
    (fun acc grade => if pred grade then acc + counts.gradeCount grade else acc)
    0

def NoteTypeJudgeCounts.breakGradeCountWhere
    (counts : NoteTypeJudgeCounts) (pred : JudgeGrade → Bool) : Nat :=
  judgeCountsGrades.foldl
    (fun acc grade => if pred grade then acc + counts.breakCount grade else acc)
    0

----------------------------------------------------------------------------
-- Score accumulation state
----------------------------------------------------------------------------

structure ScoreState where
  combo       : Nat := 0
  pCombo      : Nat := 0
  cPCombo     : Nat := 0
  totalBase   : Nat := 0
  totalExtra  : Nat := 0
  earnedBase  : Nat := 0
  earnedExtra : Nat := 0
  earnedClassicExtra : Nat := 0
  lostBase    : Nat := 0
  lostExtra   : Nat := 0
  lostClassicExtra : Nat := 0
  dxScore     : ℤ := 0
  maxDxScore  : Nat := 0
  fastCount   : Nat := 0
  lateCount   : Nat := 0
  counts      : NoteTypeJudgeCounts := emptyNoteTypeJudgeCounts
deriving Inhabited, Repr, ToJson

private def getObjValAsDStrict? {α : Type} [FromJson α] (json : Json) (field : String)
    (fallback : α) : Except String α :=
  match json.getObjVal? field with
  | .ok value => fromJson? value
  | .error _ => pure fallback

instance : FromJson ScoreState where
  fromJson?
    | json@(Json.obj _) => do
        let combo ← getObjValAsDStrict? json "combo" 0
        let pCombo ← getObjValAsDStrict? json "pCombo" 0
        let cPCombo ← getObjValAsDStrict? json "cPCombo" 0
        let totalBase ← getObjValAsDStrict? json "totalBase" 0
        let totalExtra ← getObjValAsDStrict? json "totalExtra" 0
        let earnedBase ← getObjValAsDStrict? json "earnedBase" 0
        let earnedExtra ← getObjValAsDStrict? json "earnedExtra" 0
        let earnedClassicExtra ← getObjValAsDStrict? json "earnedClassicExtra" 0
        let lostBase ← getObjValAsDStrict? json "lostBase" 0
        let lostExtra ← getObjValAsDStrict? json "lostExtra" 0
        let lostClassicExtra ← getObjValAsDStrict? json "lostClassicExtra" 0
        let dxScore ← getObjValAsDStrict? json "dxScore" 0
        let maxDxScore ← getObjValAsDStrict? json "maxDxScore" 0
        let fastCount ← getObjValAsDStrict? json "fastCount" 0
        let lateCount ← getObjValAsDStrict? json "lateCount" 0
        let counts ← getObjValAsDStrict? json "counts" emptyNoteTypeJudgeCounts
        pure
          { combo := combo
          , pCombo := pCombo
          , cPCombo := cPCombo
          , totalBase := totalBase
          , totalExtra := totalExtra
          , earnedBase := earnedBase
          , earnedExtra := earnedExtra
          , earnedClassicExtra := earnedClassicExtra
          , lostBase := lostBase
          , lostExtra := lostExtra
          , lostClassicExtra := lostClassicExtra
          , dxScore := dxScore
          , maxDxScore := maxDxScore
          , fastCount := fastCount
          , lateCount := lateCount
          , counts := counts }
    | _ => .error "invalid ScoreState"

----------------------------------------------------------------------------
-- Combo display result
----------------------------------------------------------------------------

def comboState (s : ScoreState) : ComboState :=
  let critical := s.counts.gradeCount .Perfect
  let perfect := s.counts.gradeCountWhere (fun grade =>
    grade == .LatePerfect3rd || grade == .LatePerfect2nd ||
      grade == .FastPerfect2nd || grade == .FastPerfect3rd)
  let great := s.counts.gradeCountWhere JudgeGrade.isGreatGrade
  let good := s.counts.gradeCountWhere JudgeGrade.isGoodGrade
  let miss := s.counts.gradeCountWhere JudgeGrade.isMissOrTooFast
  let allNonMiss := critical + perfect + great + good
  let isFullCombo := allNonMiss != 0 && miss == 0
  let isFullComboPlus := isFullCombo && good == 0
  let isAllPerfect := isFullComboPlus && great == 0
  let breakCritical := s.counts.breakCount .Perfect
  let breakPerfect := s.counts.breakGradeCountWhere (fun grade =>
    grade == .LatePerfect3rd || grade == .LatePerfect2nd ||
      grade == .FastPerfect2nd || grade == .FastPerfect3rd)
  let breakGreat := s.counts.breakGradeCountWhere JudgeGrade.isGreatGrade
  let breakGood := s.counts.breakGradeCountWhere JudgeGrade.isGoodGrade
  let breakMiss := s.counts.breakGradeCountWhere JudgeGrade.isMissOrTooFast
  let breakAllNonMiss := breakCritical + breakPerfect + breakGreat + breakGood
  let breakIsAllPerfect := breakAllNonMiss != 0 && breakMiss == 0 && breakGood == 0 &&
    breakGreat == 0
  let breakIsTheoretical := breakIsAllPerfect && breakPerfect == 0
  if !isFullCombo then ComboState.None
  else if isAllPerfect then
    if breakIsTheoretical then ComboState.APPlus else ComboState.AP
  else if isFullComboPlus then ComboState.FCPlus
  else ComboState.FC

----------------------------------------------------------------------------
-- Judge Event (emitted by Core → consumed by host for rendering)
----------------------------------------------------------------------------

inductive JudgeEventKind where
  | Tap    | Hold | Slide | Touch | Break
deriving DecidableEq, Repr, Inhabited, ToJson, FromJson

structure JudgeEvent where
  kind      : JudgeEventKind
  grade     : JudgeGrade
  diff      : Duration
  position  : RuntimePos
  noteIndex : Nat
  isBreak   : Bool := false
  multiple  : Nat := 1
deriving Repr, Inhabited, ToJson

instance : FromJson JudgeEvent where
  fromJson? json := do
    let kind ← json.getObjValAs? JudgeEventKind "kind"
    let grade ← json.getObjValAs? JudgeGrade "grade"
    let diff ← json.getObjValAs? Duration "diff"
    let position ← json.getObjValAs? RuntimePos "position"
    let noteIndex ← json.getObjValAs? Nat "noteIndex"
    let isBreak ← getObjValAsD? json "isBreak" false
    let multiple ← getObjValAsD? json "multiple" 1
    pure { kind := kind
         , grade := grade
         , diff := diff
         , position := position
         , noteIndex := noteIndex
         , isBreak := isBreak
         , multiple := multiple }

inductive AudioCommand where
  | PlayJudgeSfx (kind : JudgeEventKind) (grade : JudgeGrade) (isBreak : Bool)
      (atTime : TimePoint) (noteIndex : Nat)
  | PlaySlideCue (noteIndex : Nat) (trackIndex : Nat) (isBreak : Bool) (atTime : TimePoint)
deriving Repr, Inhabited, ToJson, FromJson

inductive RenderCommand where
  | ShowJudgeResult (kind : JudgeEventKind) (grade : JudgeGrade) (isBreak : Bool)
      (diff : Duration) (noteIndex : Nat)
  | UpdateSlideProgress (noteIndex : Nat) (remaining : Nat)
  | UpdateSlideTrackProgress (noteIndex : Nat) (trackIndex : Nat) (remaining : Nat)
  | HideAllSlideBars (noteIndex : Nat)
  | HideSlideBars (noteIndex : Nat) (endIndex : Nat)
  | HideSlideTrackBars (noteIndex : Nat) (trackIndex : Nat) (endIndex : Nat)
deriving Repr, Inhabited, ToJson, FromJson

end LnmaiCore
