/-
  Pure judgment functions — map timing error to JudgeGrade.

  Faithful transcriptions of:
    NoteDrop.Judge()         — Tap/Hold head
    TouchDrop.Judge()        — Touch (late-only)
    SlideBase.Judge()        — Modern slide (dynamic extension)
    SlideBase.JudgeClassic() — Classic slide (fixed windows)
    NoteLongDrop.HoldEndJudge()        — Hold release quality (press-band table)
    NoteLongDrop.HoldClassicEndJudge() — Classic hold release judgment

  All functions compute raw grades. Grade conversion (ConvertJudgeGrade)
  and subgrade correction (JudgeGradeCorrection) are separate passes.
-/

import LnmaiCore.Types
import LnmaiCore.Constants
import LnmaiCore.Time

namespace LnmaiCore.Judge

open Constants
open JudgeGrade
open LnmaiCore

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

private def absDiff (diff : Duration) : Duration :=
  Duration.abs diff

----------------------------------------------------------------------------
-- Tap / Hold Head Judgment
--   From NoteDrop.Judge(), lines 262-290
----------------------------------------------------------------------------

/--
  Judge a Tap or Hold-head note based on timing difference in ms.
  `diffMs` = (currentSec - JudgeTimingWithOffset) * 1000
  Returns the raw (unconverted) grade.
-/
def judgeTap (diff : Duration) (isEX : Bool := false) : JudgeGrade :=
  if isEX then
    Perfect  -- EX notes always Critical Perfect
  else
    let isFast := diff < Duration.zero
    let diffMSec := absDiff diff
    if diffMSec ≤ tapPerfect1Ms then
      Perfect
    else if diffMSec ≤ tapPerfect2Ms then
      if isFast then FastPerfect2nd else LatePerfect2nd
    else if diffMSec ≤ tapPerfect3Ms then
      if isFast then FastPerfect3rd else LatePerfect3rd
    else if diffMSec ≤ tapGreat1Ms then
      if isFast then FastGreat else LateGreat
    else if diffMSec ≤ tapGreat2Ms then
      if isFast then FastGreat2nd else LateGreat2nd
    else if diffMSec ≤ tapGreat3Ms then
      if isFast then FastGreat3rd else LateGreat3rd
    else
      if isFast then FastGood else LateGood

----------------------------------------------------------------------------
-- Touch Judgment (late-only, no fast side)
--   From TouchDrop.Judge(), lines ~100-130
----------------------------------------------------------------------------

/--
  Judge a Touch note. Touch has no fast-side judgments —
  if input is too early (fast && beyond 1st perfect window), bails.
  Unlike tap/hold-head notes, touch EX notes are not auto-promoted.
  Returns `none` if the hit is too early to count (caller should ignore).
-/
def judgeTouch (diff : Duration) (isEX : Bool := false) : Option JudgeGrade :=
  let _ := isEX
  let isFast := diff < Duration.zero
  let diffMSec := absDiff diff
  -- Touch: if fast and beyond 1st perfect, too early → no judgment
  if isFast && diffMSec > touchPerfect1Ms then
    none
  else
    let grade :=
      if diffMSec ≤ touchPerfect1Ms then
        Perfect
      else if diffMSec ≤ touchPerfect2Ms then
        LatePerfect2nd
      else if diffMSec ≤ touchPerfect3Ms then
        LatePerfect3rd
      else if diffMSec ≤ touchGreat1Ms then
        LateGreat
      else if diffMSec ≤ touchGreat2Ms then
        LateGreat2nd
      else if diffMSec ≤ touchGreat3Ms then
        LateGreat3rd
      else
        LateGood
    some grade

----------------------------------------------------------------------------
-- Modern Slide Judgment (dynamic extension)
--   From SlideBase.Judge(), lines 224-273
----------------------------------------------------------------------------

/--
  Judge a modern (deluxe) slide. The 3rd-perfect window is dynamically extended
  based on `stayTimeMs` (last wait time at slide end, in ms).
  The 1st and 2nd perfect windows are 1/3 and 2/3 of the 3rd window respectively.
  Slide EX notes are judged normally; they are not auto-promoted.
-/
def judgeSlideModern (diff : Duration) (stayTime : Duration) (isEX : Bool := false) : JudgeGrade :=
  let _ := isEX
  let isFast := diff < Duration.zero
  let diffMSec := absDiff diff
  -- Dynamic extension: ext = min(stayTimeMs / 4, 22-frame max)
  let ext := min (Duration.divNat stayTime 4) SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC
  let seg3rdPerfect := SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC + ext
  let seg1stPerfect := Duration.divNat seg3rdPerfect 3
  let seg2ndPerfect := Duration.divNat (Duration.scaleNat seg3rdPerfect 2) 3
  if diffMSec ≤ seg1stPerfect then
    Perfect
  else if diffMSec ≤ seg2ndPerfect then
    if isFast then FastPerfect2nd else LatePerfect2nd
  else if diffMSec ≤ seg3rdPerfect then
    if isFast then FastPerfect3rd else LatePerfect3rd
  else if diffMSec ≤ SLIDE_JUDGE_SEG_1ST_GREAT_MSEC then
    if isFast then FastGreat else LateGreat
  else if diffMSec ≤ SLIDE_JUDGE_SEG_2ND_GREAT_MSEC then
    if isFast then FastGreat2nd else LateGreat2nd
  else if diffMSec ≤ SLIDE_JUDGE_SEG_3RD_GREAT_MSEC then
    if isFast then FastGreat3rd else LateGreat3rd
  else
    if isFast then FastGood else LateGood

----------------------------------------------------------------------------
-- Classic Slide Judgment (fixed windows, separate fast/late thresholds)
--   From SlideBase.JudgeClassic(), lines 274-327
----------------------------------------------------------------------------

/-- Classic slide: fast-side thresholds (ms) -/
def slideClassicFastThresholds : List (Duration × JudgeGrade) :=
  [ (SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC, Perfect)
  , (SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC, FastPerfect2nd)
  , (SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC, FastPerfect3rd)
  , (SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC,   FastGreat)
  , (SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC,   FastGreat2nd)
  , (SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC,   FastGreat3rd)
  ]

/-- Classic slide: late-side thresholds (ms) -/
def slideClassicLateThresholds : List (Duration × JudgeGrade) :=
  [ (SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC, Perfect)
  , (SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC, LatePerfect2nd)
  , (SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC, LatePerfect3rd)
  , (SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC,   LateGreat)
  , (SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC,   LateGreat2nd)
  , (SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC,   LateGreat3rd)
  ]

private def pickGrade (diffMSec : Duration) (thresholds : List (Duration × JudgeGrade)) (fallback : JudgeGrade) : JudgeGrade :=
  match thresholds with
  | []                  => fallback
  | (limit, g) :: rest  =>
      if diffMSec ≤ limit then g else pickGrade diffMSec rest fallback

/--
  Judge a classic-mode slide. Uses fixed windows that are symmetrical in
  frame count but stored as separate fast/late constant sets.
-/
def judgeSlideClassic (diff : Duration) : JudgeGrade :=
  let isFast := diff < Duration.zero
  let diffMSec := absDiff diff
  if isFast then
    pickGrade diffMSec slideClassicFastThresholds FastGood
  else
    pickGrade diffMSec slideClassicLateThresholds LateGood

/--
  Slide judge grade correction from `SlideBase.JudgeGradeCorrection()`.

  This collapses subdivided slide grades into the coarser grades used by the
  default result flow when slide subgrades are not displayed separately.
-/
def correctSlideGrade : JudgeGrade → JudgeGrade
  | LatePerfect3rd | LatePerfect2nd | FastPerfect2nd | FastPerfect3rd => Perfect
  | grade => grade

----------------------------------------------------------------------------
-- Hold End Judgment (Deluxe/Modern) — press-band lookup table
--   From NoteLongDrop.HoldEndJudge(), lines 66-255
----------------------------------------------------------------------------

/--
  Compute the press band index from held percentage:
    0: >= 100%    1: [67%, 100%)   2: [33%, 67%)
    3: [5%, 33%)  4: [0%, 5%)
-/
private def pressBandMicros (heldMicros realityMicros : Int) : Nat :=
  if heldMicros >= realityMicros then 0
  else if heldMicros * 100 >= realityMicros * 67 then 1
  else if heldMicros * 100 >= realityMicros * 33 then 2
  else if heldMicros * 100 >= realityMicros * 5 then 3
  else 4

/--
  HoldEndJudge: computes the final hold grade from head grade and how
  long the player held the button.

  Parameters:
    headGrade            — grade from the tap head judgment
    judgeDiff            — judge diff in ms (from head judgment; negative = fast)
    lengthSec            — total hold length in seconds
    ignoreTimeSec        — head + tail ignore duration (6f+12f=0.3s for regular hold, 15f+12f=0.45s for touch hold)
    playerReleaseTimeSec — accumulated release time in seconds
-/
def judgeHoldEnd (headGrade : JudgeGrade) (judgeDiff : Duration) (length : Duration) (ignoreTime : Duration) (playerReleaseTime : Duration) : JudgeGrade :=
  -- offset: 0 if fast-side head, otherwise = judgeDiff
  let offset := if headGrade.isFast then Duration.zero else judgeDiff
  -- realityHT = effective hold time (minus ignores, minus late offset, clamped)
  let realityHTRaw := length - ignoreTime - offset
  let realityHTMax := length - Duration.fromMicros 300000
  let realityHT := max Duration.zero (min realityHTRaw realityHTMax)
  if realityHT ≤ Duration.zero then
    headGrade
  else
    let held := max Duration.zero (realityHT - playerReleaseTime)
    let band := pressBandMicros held.toMicros realityHT.toMicros
    match band with
    | 0 =>  -- ≥ 100%: release never or very late
      match headGrade with
      | LatePerfect3rd | LatePerfect2nd | JudgeGrade.Perfect | FastPerfect2nd | FastPerfect3rd =>
        headGrade
      | LateGood | LateGreat3rd | LateGreat2nd | LateGreat =>
        LateGreat
      | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
        FastGreat
      | Miss   => LateGood
      | TooFast => FastGood
    | 1 =>  -- [67%, 100%): release slightly early
      match headGrade with
      | JudgeGrade.Perfect =>
        if judgeDiff ≥ Duration.zero then LatePerfect2nd else FastPerfect2nd
      | LatePerfect3rd | LatePerfect2nd | FastPerfect2nd | FastPerfect3rd =>
        headGrade
      | LateGood | LateGreat3rd | LateGreat2nd | LateGreat =>
        LateGreat
      | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
        FastGreat
      | Miss   => LateGood
      | TooFast => FastGood
    | 2 =>  -- [33%, 67%): release moderately early
      match headGrade with
      | JudgeGrade.Perfect =>
        if judgeDiff ≥ Duration.zero then LateGreat2nd else FastGreat2nd
      | LateGood | LateGreat3rd | LateGreat2nd | LateGreat | LatePerfect3rd | LatePerfect2nd =>
        LateGreat
      | FastPerfect2nd | FastPerfect3rd | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
        FastGreat
      | Miss   => LateGood
      | TooFast => FastGood
    | 3 =>  -- [5%, 33%): release very early
      match headGrade with
      | JudgeGrade.Perfect =>
        if judgeDiff ≥ Duration.zero then LateGood else FastGood
      | Miss | LateGood | LateGreat3rd | LateGreat2nd | LateGreat | LatePerfect3rd | LatePerfect2nd =>
        LateGood
      | FastPerfect2nd | FastPerfect3rd | FastGreat | FastGreat2nd | FastGreat3rd | FastGood | TooFast =>
        FastGood
    | _ =>  -- [0%, 5%): release almost immediately
      match headGrade with
      | JudgeGrade.Perfect =>
        if judgeDiff ≥ Duration.zero then LateGood else FastGood
      | LateGood | LateGreat3rd | LateGreat2nd | LateGreat | LatePerfect3rd | LatePerfect2nd =>
        LateGood
      | FastPerfect2nd | FastPerfect3rd | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
        FastGood
      | Miss | TooFast =>
        headGrade
    -- band is always 0-4

----------------------------------------------------------------------------
-- Hold Classic End Judgment
--   From NoteLongDrop.HoldClassicEndJudge(), lines 257-307
----------------------------------------------------------------------------

/--
  Classic hold end judge: evaluates release timing independently,
  then takes the WORSE of head grade vs end grade.
  Comparison uses |7 - (int)grade| distance from Perfect.
-/
def judgeHoldClassicEnd
    (headGrade : JudgeGrade) (timing : TimePoint) (length : Duration)
    (releaseTiming : TimePoint) : JudgeGrade :=
  -- If head is already Miss or TooFast, no improvement possible
  if headGrade.isMissOrTooFast then
    headGrade
  else
    let diff := timing + length - releaseTiming
    let isFast := diff > Duration.zero
    let diffMSec := absDiff diff
    -- End grade: Perfect if within window, else Good
    let endGrade :=
      if isFast then
        if diffMSec < HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC then Perfect
        else FastGood
      else
        if diffMSec < HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC then Perfect
        else LateGood
    -- Take worst: compare distance from 7 (Perfect)
    let headDist := headGrade.distFromPerfect
    let endDist  := endGrade.distFromPerfect
    if endDist > headDist then endGrade else headGrade

----------------------------------------------------------------------------
-- Slide Too-Late Judge
--   From SlideBase.TooLateJudge(): if queueRemaining == 1 → LateGood, else Miss
----------------------------------------------------------------------------

def judgeSlideTooLate (queueRemaining : Nat) : JudgeGrade :=
  if queueRemaining == 1 then LateGood else Miss

----------------------------------------------------------------------------
-- Stub: judgeSlideTooLateCheck
--  In the real game: timing > SLIDE_JUDGE_GOOD_AREA_MSEC / 1000 + min(user_offset, 0)
----------------------------------------------------------------------------

def isTooLateSlide (diff : Duration) (userOffset : Duration := Duration.zero) : Bool :=
  let threshold := SLIDE_JUDGE_GOOD_AREA_MSEC + min userOffset Duration.zero
  diff > threshold

end LnmaiCore.Judge
