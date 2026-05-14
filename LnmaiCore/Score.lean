import Mathlib

/-
  Score and combo computation — faithful transcription of
  ObjectCounter.UpdateComboCount() and UpdateNoteScoreCount().
-/

import LnmaiCore.Types

namespace LnmaiCore.Score

open JudgeGrade
open NoteType

----------------------------------------------------------------------------
-- Base Score per Note Type
----------------------------------------------------------------------------

def baseScore (nt : NoteType) : Nat :=
  match nt with
  | Tap   => 500
  | Hold  => 1000
  | Slide => 1500
  | Touch => 500
  | Break => 2500

def extraScore : Nat := 100  -- Break notes only (DX extra)

----------------------------------------------------------------------------
-- Non-Break Note Score
--   Returns (earned, lost) base score for this note × multiple
----------------------------------------------------------------------------

/--
  Score a non-Break note. Returns (baseEarned, baseLost).
  - Miss/TooFast: 0% earned, 100% lost
  - Good:         50% earned, 50% lost
  - Great*:       80% earned, 20% lost
  - Perfect*:     100% earned
-/
def scoreNonBreak (baseScore : Nat) (grade : JudgeGrade) (multiple : Nat := 1) : Nat × Nat :=
  let b := baseScore * multiple
  match grade with
  | Miss    => (0, b)
  | TooFast => (0, b)
  | LateGood  => (b / 2,           b - b / 2)
  | FastGood  => (b / 2,           b - b / 2)
  | LateGreat | LateGreat2nd | LateGreat3rd
  | FastGreat | FastGreat2nd | FastGreat3rd =>
    (b * 4 / 5, b - b * 4 / 5)
  | LatePerfect3rd | LatePerfect2nd | JudgeGrade.Perfect
  | FastPerfect2nd | FastPerfect3rd =>
    (b, 0)

----------------------------------------------------------------------------
-- Break Note Score (most complex)
--   Returns (baseEarned, extraEarned, classicExtraEarned,
--            baseLost, extraLost, classicExtraLost)
----------------------------------------------------------------------------

/--
  Score a Break note (2500 base + 100 extra).
  Extra score has two tracks: DX and Classic.
  Classic extra is stricter (only Perfect2nd and Perfect earn any).
-/
def scoreBreak (grade : JudgeGrade) (multiple : Nat := 1) : Nat × Nat × Nat × Nat × Nat × Nat :=
  let m := multiple
  match grade with
  | Miss | TooFast =>
    (0,        0,  0,    -- earned: base, extraDX, extraClassic
     2500 * m, 100 * m, 100 * m)  -- lost: base, extraDX, extraClassic
  | LateGood | FastGood =>
    (1000 * m, 30 * m,  0,
     1500 * m, 70 * m,  100 * m)
  | LateGreat3rd | FastGreat3rd =>
    (1250 * m, 40 * m,  0,
     1250 * m, 60 * m,  100 * m)
  | LateGreat2nd | FastGreat2nd =>
    (1500 * m, 40 * m,  0,
     1000 * m, 60 * m,  100 * m)
  | LateGreat | FastGreat =>
    (2000 * m, 40 * m,  0,
      500 * m, 60 * m,  100 * m)
  | LatePerfect3rd | FastPerfect3rd =>
    (2500 * m, 50 * m,  0,
         0,     50 * m,  100 * m)
  | LatePerfect2nd | FastPerfect2nd =>
    (2500 * m, 75 * m,  50 * m,
         0,     25 * m,  50 * m)
  | JudgeGrade.Perfect =>
    (2500 * m, 100 * m, 100 * m,
         0,         0,       0)

----------------------------------------------------------------------------
-- Combo State Update
--   Returns (newCombo, newPCombo, newCPCombo, dXScoreLostDelta)
--   called AFTER combo has already been incremented for non-miss
----------------------------------------------------------------------------

structure ComboDelta where
  combo      : Nat
  pCombo     : Nat
  cPCombo    : Nat
  dXScoreLost : ℤ  -- negative = lost score (subtracted from total)
deriving Repr, Inhabited

/--
  Update combo counters for a single note judgment.
  This is the pure version of ObjectCounter.UpdateComboCount().
  Input: current combo state, the grade, and multiple.
  Output: new combo state.

  Note: the C# code increments _combo BEFORE calling UpdateComboCount
  for non-Miss grades. The combo reset for Miss/TooFast overrides.
-/
def updateCombo (combo : Nat) (pCombo : Nat) (cPCombo : Nat) (dXScoreLost : ℤ) (grade : JudgeGrade) (multiple : Nat := 1) : ComboDelta :=
  let m := multiple
  match grade with
  | JudgeGrade.Perfect =>
    { combo       := combo + m
    , pCombo      := pCombo + m
    , cPCombo     := cPCombo + m
    , dXScoreLost := dXScoreLost
    }
  | LatePerfect2nd | FastPerfect2nd | LatePerfect3rd | FastPerfect3rd =>
    { combo       := combo + m
    , pCombo      := pCombo + m
    , cPCombo     := 0
    , dXScoreLost := dXScoreLost - Int.ofNat (1 * m)
    }
  | LateGreat3rd | LateGreat2nd | LateGreat
  | FastGreat | FastGreat2nd | FastGreat3rd =>
    { combo       := combo + m
    , pCombo      := 0
    , cPCombo     := 0
    , dXScoreLost := dXScoreLost - Int.ofNat (2 * m)
    }
  | LateGood | FastGood =>
    { combo       := combo + m
    , pCombo      := 0
    , cPCombo     := 0
    , dXScoreLost := dXScoreLost - Int.ofNat (3 * m)
    }
  | Miss | TooFast =>
    { combo       := 0
    , pCombo      := 0
    , cPCombo     := 0
    , dXScoreLost := dXScoreLost - Int.ofNat (3 * m)
    }

----------------------------------------------------------------------------
-- Fast / Late Counting
--   Based on |7 - (int)grade| distance from Perfect.
--   Only counts grades with distance > 2 for "Below Perfect" display,
--   or all non-Perfected for "Below CP". Miss/TooFast excluded.
----------------------------------------------------------------------------

/--
  Returns (isFast, isLate) increment flags for this grade,
  given a display option (matching JudgeDisplayOption).
-/
inductive FastLateDisplay where
  | All     -- count all non-zero-diff, non-miss
  | BelowCP -- count everything except Perfect (CP), Miss, TooFast
  | BelowP  -- count only Great and Good (distance from Perfect > 2)
deriving DecidableEq, Repr

def countFastLate (grade : JudgeGrade) (diff : Duration) (display : FastLateDisplay) : (Bool × Bool) :=
  if grade.isMissOrTooFast then
    (false, false)
  else
    let d := grade.distFromPerfect
    match display with
    | FastLateDisplay.All =>
      if diff == Duration.zero then (false, false)
      else if diff < Duration.zero then (true, false)
      else (false, true)
    | FastLateDisplay.BelowCP =>
      if grade == JudgeGrade.Perfect then (false, false)
      else if diff < Duration.zero then (true, false)
      else (false, true)
    | FastLateDisplay.BelowP =>
      if d ≤ 2 then (false, false)  -- skip Perfect, Perfect2nd, Perfect3rd
      else if diff < Duration.zero then (true, false)
      else (false, true)

----------------------------------------------------------------------------
-- DX Score Rank
--   Based on percentage of maxDXScore achieved.
----------------------------------------------------------------------------

/--
  DXScore ranks: 5=SSS+, 4=SSS, 3=SS+, 2=SS, 1=S, 0=none
  Thresholds: 97%, 95%, 93%, 90%, 85%
-/
def dxScoreRank (achievedDxScore : Nat) (maxDxScore : Nat) : Nat :=
  if maxDxScore == 0 then 0
  else
    let meetsPercent (threshold : Nat) : Bool :=
      achievedDxScore * 100 ≥ maxDxScore * threshold
    if meetsPercent 97 then 5
    else if meetsPercent 95 then 4
    else if meetsPercent 93 then 3
    else if meetsPercent 90 then 2
    else if meetsPercent 85 then 1
    else 0

----------------------------------------------------------------------------
-- Accuracy Rate Computation
--   The 5 accuracy formulas from ObjectCounter.UpdateAccRate()
----------------------------------------------------------------------------

structure AccRates where
  classicAccPlus    : Rat  -- [0]: classic acc (+)  = CurrentNoteScoreClassic / TotalBase * 100
  classicAccMinus   : Rat  -- [1]: classic acc (-)  = (TotalBase - LostBase + CurrentExtraClassic) / TotalBase * 100
  dxAccMinus101     : Rat  -- [2]: acc 101(-)       = (earnedBase/totalBase + earnedExtra/(totalExtra*100)) * 100
  dxAccMinus100     : Rat  -- [3]: acc 100(-)       = (earnedBase/totalBase + currentExtra/(totalExtra*100)) * 100
  dxAccPlus         : Rat  -- [4]: acc (+)          = (currentBase/totalBase + currentExtra/(totalExtra*100)) * 100
deriving Repr

def computeAccRates (totalBase : Nat) (currentBase : Nat) (lostBase : Nat)
                    (totalExtra : Nat) (currentExtra : Nat) (lostExtra : Nat)
                    (currentExtraClassic : Nat) : AccRates :=
  let tb : Rat := Int.ofNat totalBase
  let cb : Rat := Int.ofNat currentBase
  let lb : Rat := Int.ofNat lostBase
  let te : Rat := Int.ofNat (max 1 totalExtra)
  let ce : Rat := Int.ofNat currentExtra
  let le : Rat := Int.ofNat lostExtra
  let cc : Rat := Int.ofNat currentExtraClassic
  let hundred : Rat := 100
  let earnedBase := tb - lb
  let earnedExtra := te - le
  if totalBase == 0 then
    { classicAccPlus    := 0
    , classicAccMinus   := 0
    , dxAccMinus101     := 0
    , dxAccMinus100     := 0
    , dxAccPlus         := 0
    }
  else
    { classicAccPlus    := (cb + cc) / tb * hundred
    , classicAccMinus   := (earnedBase + cc) / tb * hundred
    , dxAccMinus101     := (earnedBase / tb + earnedExtra / (te * hundred)) * hundred
    , dxAccMinus100     := (earnedBase / tb + ce / (te * hundred)) * hundred
    , dxAccPlus         := (cb / tb + ce / (te * hundred)) * hundred
    }

end LnmaiCore.Score
