/-
  Timing constants for the game judgment engine.
  All values in milliseconds. Derived from frame units × FRAME_LENGTH_MSEC.

  Naming matches the C# source from NoteDrop.cs and NoteLongDrop.cs.
-/

namespace LnmaiCore.Constants

def FRAME_LENGTH_MSEC : Float := 1000.0 / 60.0  -- ~16.667ms per frame
def FRAME_LENGTH_SEC  : Float := 1.0 / 60.0

----------------------------------------------------------------------------
-- Tap Judgment (frame units → ms)
----------------------------------------------------------------------------

def TAP_JUDGE_SEG_1ST_PERFECT_MSEC : Float := 1.0 * FRAME_LENGTH_MSEC
def TAP_JUDGE_SEG_2ND_PERFECT_MSEC : Float := 2.0 * FRAME_LENGTH_MSEC
def TAP_JUDGE_SEG_3RD_PERFECT_MSEC : Float := 3.0 * FRAME_LENGTH_MSEC
def TAP_JUDGE_SEG_1ST_GREAT_MSEC   : Float := 4.0 * FRAME_LENGTH_MSEC
def TAP_JUDGE_SEG_2ND_GREAT_MSEC   : Float := 5.0 * FRAME_LENGTH_MSEC
def TAP_JUDGE_SEG_3RD_GREAT_MSEC   : Float := 6.0 * FRAME_LENGTH_MSEC
def TAP_JUDGE_GOOD_AREA_MSEC       : Float := 9.0 * FRAME_LENGTH_MSEC

def tapPerfect1Ms  : Float := TAP_JUDGE_SEG_1ST_PERFECT_MSEC
def tapPerfect2Ms  : Float := TAP_JUDGE_SEG_2ND_PERFECT_MSEC
def tapPerfect3Ms  : Float := TAP_JUDGE_SEG_3RD_PERFECT_MSEC
def tapGreat1Ms    : Float := TAP_JUDGE_SEG_1ST_GREAT_MSEC
def tapGreat2Ms    : Float := TAP_JUDGE_SEG_2ND_GREAT_MSEC
def tapGreat3Ms    : Float := TAP_JUDGE_SEG_3RD_GREAT_MSEC
def tapGoodMs      : Float := TAP_JUDGE_GOOD_AREA_MSEC

----------------------------------------------------------------------------
-- Touch Judgment
----------------------------------------------------------------------------

def TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC : Float := 9.0 * FRAME_LENGTH_MSEC
def TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC : Float := 10.5 * FRAME_LENGTH_MSEC
def TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC : Float := 12.0 * FRAME_LENGTH_MSEC
def TOUCH_JUDGE_SEG_1ST_GREAT_MSEC   : Float := 13.0 * FRAME_LENGTH_MSEC
def TOUCH_JUDGE_SEG_2ND_GREAT_MSEC   : Float := 14.0 * FRAME_LENGTH_MSEC
def TOUCH_JUDGE_SEG_3RD_GREAT_MSEC   : Float := 15.0 * FRAME_LENGTH_MSEC
def TOUCH_JUDGE_GOOD_AREA_MSEC       : Float := 18.0 * FRAME_LENGTH_MSEC

def touchPerfect1Ms : Float := TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC
def touchPerfect2Ms : Float := TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC
def touchPerfect3Ms : Float := TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC
def touchGreat1Ms   : Float := TOUCH_JUDGE_SEG_1ST_GREAT_MSEC
def touchGreat2Ms   : Float := TOUCH_JUDGE_SEG_2ND_GREAT_MSEC
def touchGreat3Ms   : Float := TOUCH_JUDGE_SEG_3RD_GREAT_MSEC
def touchGoodMs     : Float := TOUCH_JUDGE_GOOD_AREA_MSEC

----------------------------------------------------------------------------
-- Hold Constants
----------------------------------------------------------------------------

def HOLD_HEAD_IGNORE_LENGTH_SEC     : Float := 6.0 * FRAME_LENGTH_SEC
def HOLD_TAIL_IGNORE_LENGTH_SEC     : Float := 12.0 * FRAME_LENGTH_SEC
def TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC : Float := 15.0 * FRAME_LENGTH_SEC
def TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC : Float := 12.0 * FRAME_LENGTH_SEC
def DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC : Float := 2.0 * FRAME_LENGTH_SEC
def CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC : Float := 20.0 * FRAME_LENGTH_SEC

def HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC : Float := 9.0 * FRAME_LENGTH_MSEC
def HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC : Float := 12.0 * FRAME_LENGTH_MSEC

def holdHeadIgnoreMs  : Float := HOLD_HEAD_IGNORE_LENGTH_SEC * 1000.0
def holdTailIgnoreMs  : Float := HOLD_TAIL_IGNORE_LENGTH_SEC * 1000.0

----------------------------------------------------------------------------
-- Slide Constants (Modern)
----------------------------------------------------------------------------

def SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC : Float := 22.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC       : Float := 14.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_SEG_1ST_GREAT_MSEC              : Float := 21.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_SEG_2ND_GREAT_MSEC              : Float := 25.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_SEG_3RD_GREAT_MSEC              : Float := 29.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_GOOD_AREA_MSEC                  : Float := 36.0 * FRAME_LENGTH_MSEC

----------------------------------------------------------------------------
-- Slide Constants (Classic — fast side)
----------------------------------------------------------------------------

def SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC : Float := 4.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC : Float := 8.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC : Float := 12.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC   : Float := 16.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC   : Float := 20.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC   : Float := 24.0 * FRAME_LENGTH_MSEC

----------------------------------------------------------------------------
-- Slide Constants (Classic — late side)
----------------------------------------------------------------------------

def SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC : Float := 4.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC : Float := 8.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC : Float := 12.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC   : Float := 16.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC   : Float := 20.0 * FRAME_LENGTH_MSEC
def SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC   : Float := 24.0 * FRAME_LENGTH_MSEC

----------------------------------------------------------------------------
-- Zone / Sensor counts
----------------------------------------------------------------------------

def BUTTON_ZONE_COUNT : Nat := 8
def SENSOR_AREA_COUNT : Nat := 33

----------------------------------------------------------------------------
-- Judgeable range (in seconds)
----------------------------------------------------------------------------

def JUDGABLE_RANGE_SEC : Float := 0.15
def TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC : Float := FRAME_LENGTH_SEC * 10.0

end LnmaiCore.Constants
