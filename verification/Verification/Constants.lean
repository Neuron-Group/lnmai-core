import Verification.Bridge

open Aeneas Aeneas.Std lnmai_core_verify
namespace Verification.ConstantsProof
open Bridge

theorem frame_length_equiv :
    toLnmDuration constants.FRAME_LENGTH = LnmaiCore.Constants.FRAME_LENGTH := by
  unfold constants.FRAME_LENGTH
  decide

theorem tap_perfect_1st_equiv :
    toLnmDuration constants.TAP_PERFECT_1ST = LnmaiCore.Constants.TAP_JUDGE_SEG_1ST_PERFECT_MSEC := by
  unfold constants.TAP_PERFECT_1ST
  decide

theorem tap_perfect_2nd_equiv :
    toLnmDuration constants.TAP_PERFECT_2ND = LnmaiCore.Constants.TAP_JUDGE_SEG_2ND_PERFECT_MSEC := by
  unfold constants.TAP_PERFECT_2ND
  decide

theorem tap_perfect_3rd_equiv :
    toLnmDuration constants.TAP_PERFECT_3RD = LnmaiCore.Constants.TAP_JUDGE_SEG_3RD_PERFECT_MSEC := by
  unfold constants.TAP_PERFECT_3RD
  decide

theorem tap_great_1st_equiv :
    toLnmDuration constants.TAP_GREAT_1ST = LnmaiCore.Constants.TAP_JUDGE_SEG_1ST_GREAT_MSEC := by
  unfold constants.TAP_GREAT_1ST
  decide

theorem tap_great_2nd_equiv :
    toLnmDuration constants.TAP_GREAT_2ND = LnmaiCore.Constants.TAP_JUDGE_SEG_2ND_GREAT_MSEC := by
  unfold constants.TAP_GREAT_2ND
  decide

theorem tap_great_3rd_equiv :
    toLnmDuration constants.TAP_GREAT_3RD = LnmaiCore.Constants.TAP_JUDGE_SEG_3RD_GREAT_MSEC := by
  unfold constants.TAP_GREAT_3RD
  decide

theorem tap_good_equiv :
    toLnmDuration constants.TAP_GOOD = LnmaiCore.Constants.TAP_JUDGE_GOOD_AREA_MSEC := by
  unfold constants.TAP_GOOD
  decide

theorem touch_perfect_1st_equiv :
    toLnmDuration constants.TOUCH_PERFECT_1ST = LnmaiCore.Constants.TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC := by
  unfold constants.TOUCH_PERFECT_1ST
  decide

theorem touch_perfect_2nd_equiv :
    toLnmDuration constants.TOUCH_PERFECT_2ND = LnmaiCore.Constants.TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC := by
  unfold constants.TOUCH_PERFECT_2ND
  decide

theorem touch_perfect_3rd_equiv :
    toLnmDuration constants.TOUCH_PERFECT_3RD = LnmaiCore.Constants.TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC := by
  unfold constants.TOUCH_PERFECT_3RD
  decide

theorem touch_great_1st_equiv :
    toLnmDuration constants.TOUCH_GREAT_1ST = LnmaiCore.Constants.TOUCH_JUDGE_SEG_1ST_GREAT_MSEC := by
  unfold constants.TOUCH_GREAT_1ST
  decide

theorem touch_great_2nd_equiv :
    toLnmDuration constants.TOUCH_GREAT_2ND = LnmaiCore.Constants.TOUCH_JUDGE_SEG_2ND_GREAT_MSEC := by
  unfold constants.TOUCH_GREAT_2ND
  decide

theorem touch_great_3rd_equiv :
    toLnmDuration constants.TOUCH_GREAT_3RD = LnmaiCore.Constants.TOUCH_JUDGE_SEG_3RD_GREAT_MSEC := by
  unfold constants.TOUCH_GREAT_3RD
  decide

theorem touch_good_equiv :
    toLnmDuration constants.TOUCH_GOOD = LnmaiCore.Constants.TOUCH_JUDGE_GOOD_AREA_MSEC := by
  unfold constants.TOUCH_GOOD
  decide

theorem slide_perfect_3rd_equiv :
    toLnmDuration constants.SLIDE_PERFECT_3RD = LnmaiCore.Constants.SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC := by
  unfold constants.SLIDE_PERFECT_3RD
  decide

theorem slide_great_1st_equiv :
    toLnmDuration constants.SLIDE_GREAT_1ST = LnmaiCore.Constants.SLIDE_JUDGE_SEG_1ST_GREAT_MSEC := by
  unfold constants.SLIDE_GREAT_1ST
  decide

theorem slide_great_2nd_equiv :
    toLnmDuration constants.SLIDE_GREAT_2ND = LnmaiCore.Constants.SLIDE_JUDGE_SEG_2ND_GREAT_MSEC := by
  unfold constants.SLIDE_GREAT_2ND
  decide

theorem slide_great_3rd_equiv :
    toLnmDuration constants.SLIDE_GREAT_3RD = LnmaiCore.Constants.SLIDE_JUDGE_SEG_3RD_GREAT_MSEC := by
  unfold constants.SLIDE_GREAT_3RD
  decide

theorem slide_good_equiv :
    toLnmDuration constants.SLIDE_GOOD = LnmaiCore.Constants.SLIDE_JUDGE_GOOD_AREA_MSEC := by
  unfold constants.SLIDE_GOOD
  decide

theorem slide_max_ext_equiv :
    toLnmDuration constants.SLIDE_MAX_EXT = LnmaiCore.Constants.SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC := by
  unfold constants.SLIDE_MAX_EXT
  decide

theorem hold_head_ignore_equiv :
    toLnmDuration constants.HOLD_HEAD_IGNORE = LnmaiCore.Constants.HOLD_HEAD_IGNORE_LENGTH_SEC := by
  unfold constants.HOLD_HEAD_IGNORE
  decide

theorem hold_tail_ignore_equiv :
    toLnmDuration constants.HOLD_TAIL_IGNORE = LnmaiCore.Constants.HOLD_TAIL_IGNORE_LENGTH_SEC := by
  unfold constants.HOLD_TAIL_IGNORE
  decide

theorem touch_hold_head_ignore_equiv :
    toLnmDuration constants.TOUCH_HOLD_HEAD_IGNORE = LnmaiCore.Constants.TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC := by
  unfold constants.TOUCH_HOLD_HEAD_IGNORE
  decide

theorem touch_hold_tail_ignore_equiv :
    toLnmDuration constants.TOUCH_HOLD_TAIL_IGNORE = LnmaiCore.Constants.TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC := by
  unfold constants.TOUCH_HOLD_TAIL_IGNORE
  decide

theorem deluxe_hold_release_ignore_time_equiv :
    toLnmDuration constants.DELUXE_HOLD_RELEASE_IGNORE_TIME = LnmaiCore.Constants.DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC := by
  unfold constants.DELUXE_HOLD_RELEASE_IGNORE_TIME
  decide

theorem classic_hold_allow_over_length_equiv :
    toLnmDuration constants.CLASSIC_HOLD_ALLOW_OVER_LENGTH = LnmaiCore.Constants.CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC := by
  unfold constants.CLASSIC_HOLD_ALLOW_OVER_LENGTH
  decide

theorem hold_classic_end_judge_perfect_fast_equiv :
    toLnmDuration constants.HOLD_CLASSIC_END_JUDGE_PERFECT_FAST = LnmaiCore.Constants.HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC := by
  unfold constants.HOLD_CLASSIC_END_JUDGE_PERFECT_FAST
  decide

theorem hold_classic_end_judge_perfect_late_equiv :
    toLnmDuration constants.HOLD_CLASSIC_END_JUDGE_PERFECT_LATE = LnmaiCore.Constants.HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC := by
  unfold constants.HOLD_CLASSIC_END_JUDGE_PERFECT_LATE
  decide

theorem slide_judged_late_clear_wait_equiv :
    toLnmDuration constants.SLIDE_JUDGED_LATE_CLEAR_WAIT = LnmaiCore.Constants.SLIDE_JUDGED_LATE_CLEAR_WAIT := by
  unfold constants.SLIDE_JUDGED_LATE_CLEAR_WAIT
  decide

theorem judgable_range_equiv :
    toLnmDuration constants.JUDGABLE_RANGE = LnmaiCore.Constants.JUDGABLE_RANGE_SEC := by
  unfold constants.JUDGABLE_RANGE
  decide

theorem touch_judgable_range_late_extra_equiv :
    toLnmDuration constants.TOUCH_JUDGABLE_RANGE_LATE_EXTRA = LnmaiCore.Constants.TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC := by
  unfold constants.TOUCH_JUDGABLE_RANGE_LATE_EXTRA
  decide

theorem button_zone_count_equiv :
    constants.BUTTON_ZONE_COUNT.val = LnmaiCore.Constants.BUTTON_ZONE_COUNT := by
  unfold constants.BUTTON_ZONE_COUNT
  decide

theorem sensor_area_count_equiv :
    constants.SENSOR_AREA_COUNT.val = LnmaiCore.Constants.SENSOR_AREA_COUNT := by
  unfold constants.SENSOR_AREA_COUNT
  decide

theorem subdivide_slide_judge_grade_equiv :
    constants.SUBDIVIDE_SLIDE_JUDGE_GRADE = LnmaiCore.Constants.SUBDIVIDE_SLIDE_JUDGE_GRADE := by
  unfold constants.SUBDIVIDE_SLIDE_JUDGE_GRADE
  decide

end Verification.ConstantsProof
