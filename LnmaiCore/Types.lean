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
deriving Repr, Inhabited

structure GroupState where
  groupId : Nat
  count   : Nat
  size    : Nat
  grade   : JudgeGrade
  diff    : Duration
deriving Repr, Inhabited

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
  lostBase    : Nat := 0
  lostExtra   : Nat := 0
  dxScore     : ℤ := 0
  maxDxScore  : Nat := 0
  fastCount   : Nat := 0
  lateCount   : Nat := 0
  counts      : NoteTypeJudgeCounts := emptyNoteTypeJudgeCounts
deriving Inhabited, Repr

----------------------------------------------------------------------------
-- Combo display result
----------------------------------------------------------------------------

def comboState (s : ScoreState) : ComboState :=
  if s.combo == 0 then ComboState.None
  else if s.pCombo == s.combo && s.cPCombo == s.combo then ComboState.APPlus
  else if s.pCombo == s.combo then ComboState.AP
  else if s.combo == s.pCombo then ComboState.FCPlus
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
deriving Repr, Inhabited, ToJson, FromJson

inductive AudioCommand where
  | PlayJudgeSfx (kind : JudgeEventKind) (grade : JudgeGrade) (atTime : TimePoint) (noteIndex : Nat)
  | PlaySlideCue (noteIndex : Nat) (trackIndex : Nat) (atTime : TimePoint)
deriving Repr, Inhabited

inductive RenderCommand where
  | ShowJudgeResult (kind : JudgeEventKind) (grade : JudgeGrade) (diff : Duration) (noteIndex : Nat)
  | UpdateSlideProgress (noteIndex : Nat) (remaining : Nat)
  | UpdateSlideTrackProgress (noteIndex : Nat) (trackIndex : Nat) (remaining : Nat)
  | HideAllSlideBars (noteIndex : Nat)
  | HideSlideBars (noteIndex : Nat) (endIndex : Nat)
  | HideSlideTrackBars (noteIndex : Nat) (trackIndex : Nat) (endIndex : Nat)
deriving Repr, Inhabited

end LnmaiCore
