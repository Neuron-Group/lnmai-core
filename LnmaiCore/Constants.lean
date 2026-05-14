/-
  Exact timing constants for the game judgment engine.

  Values are represented in exact microsecond durations.
  Naming matches the C# source from NoteDrop.cs and NoteLongDrop.cs.
-/

import LnmaiCore.Time

namespace LnmaiCore.Constants

open LnmaiCore

def FRAME_LENGTH : Duration := Duration.fromMicros 16667
def FRAME_LENGTH_MSEC : Duration := FRAME_LENGTH

----------------------------------------------------------------------------
-- Tap Judgment
----------------------------------------------------------------------------

def TAP_JUDGE_SEG_1ST_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 1
def TAP_JUDGE_SEG_2ND_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 2
def TAP_JUDGE_SEG_3RD_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 3
def TAP_JUDGE_SEG_1ST_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 4
def TAP_JUDGE_SEG_2ND_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 5
def TAP_JUDGE_SEG_3RD_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 6
def TAP_JUDGE_GOOD_AREA_MSEC       : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 9

def tapPerfect1Ms  : Duration := TAP_JUDGE_SEG_1ST_PERFECT_MSEC
def tapPerfect2Ms  : Duration := TAP_JUDGE_SEG_2ND_PERFECT_MSEC
def tapPerfect3Ms  : Duration := TAP_JUDGE_SEG_3RD_PERFECT_MSEC
def tapGreat1Ms    : Duration := TAP_JUDGE_SEG_1ST_GREAT_MSEC
def tapGreat2Ms    : Duration := TAP_JUDGE_SEG_2ND_GREAT_MSEC
def tapGreat3Ms    : Duration := TAP_JUDGE_SEG_3RD_GREAT_MSEC
def tapGoodMs      : Duration := TAP_JUDGE_GOOD_AREA_MSEC

----------------------------------------------------------------------------
-- Touch Judgment
----------------------------------------------------------------------------

def TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 9
def TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC : Duration := Duration.fromMicros 175004
def TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 12
def TOUCH_JUDGE_SEG_1ST_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 13
def TOUCH_JUDGE_SEG_2ND_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 14
def TOUCH_JUDGE_SEG_3RD_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 15
def TOUCH_JUDGE_GOOD_AREA_MSEC       : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 18

def touchPerfect1Ms : Duration := TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC
def touchPerfect2Ms : Duration := TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC
def touchPerfect3Ms : Duration := TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC
def touchGreat1Ms   : Duration := TOUCH_JUDGE_SEG_1ST_GREAT_MSEC
def touchGreat2Ms   : Duration := TOUCH_JUDGE_SEG_2ND_GREAT_MSEC
def touchGreat3Ms   : Duration := TOUCH_JUDGE_SEG_3RD_GREAT_MSEC
def touchGoodMs     : Duration := TOUCH_JUDGE_GOOD_AREA_MSEC

----------------------------------------------------------------------------
-- Hold Constants
----------------------------------------------------------------------------

def HOLD_HEAD_IGNORE_LENGTH_SEC       : Duration := Duration.scaleNat FRAME_LENGTH 6
def HOLD_TAIL_IGNORE_LENGTH_SEC       : Duration := Duration.scaleNat FRAME_LENGTH 12
def TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC : Duration := Duration.scaleNat FRAME_LENGTH 15
def TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC : Duration := Duration.scaleNat FRAME_LENGTH 12
def DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC : Duration := Duration.scaleNat FRAME_LENGTH 2
def CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC  : Duration := Duration.scaleNat FRAME_LENGTH 20
def JUDGE_OFFSET : Duration := Duration.zero
def TOUCH_PANEL_OFFSET : Duration := Duration.zero
def USE_BUTTON_RING_FOR_TOUCH : Bool := false
def SUBDIVIDE_SLIDE_JUDGE_GRADE : Bool := false

def HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 9
def HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 12

def holdHeadIgnoreMs : Duration := HOLD_HEAD_IGNORE_LENGTH_SEC
def holdTailIgnoreMs : Duration := HOLD_TAIL_IGNORE_LENGTH_SEC

----------------------------------------------------------------------------
-- Slide Constants
----------------------------------------------------------------------------

def SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 22
def SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC       : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 14
def SLIDE_JUDGE_SEG_1ST_GREAT_MSEC              : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 21
def SLIDE_JUDGE_SEG_2ND_GREAT_MSEC              : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 25
def SLIDE_JUDGE_SEG_3RD_GREAT_MSEC              : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 29
def SLIDE_JUDGE_GOOD_AREA_MSEC                  : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 36

def SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 4
def SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 8
def SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 12
def SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 16
def SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 20
def SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 24

def SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 4
def SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 8
def SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 12
def SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 16
def SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 20
def SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC   : Duration := Duration.scaleNat FRAME_LENGTH_MSEC 24

----------------------------------------------------------------------------
-- Zone / Sensor counts
----------------------------------------------------------------------------

def BUTTON_ZONE_COUNT : Nat := 8
def SENSOR_AREA_COUNT : Nat := 33

----------------------------------------------------------------------------
-- Judgeable ranges
----------------------------------------------------------------------------

def JUDGABLE_RANGE_SEC : Duration := Duration.fromMicros 150000
def TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC : Duration := Duration.scaleNat FRAME_LENGTH 10

end LnmaiCore.Constants
