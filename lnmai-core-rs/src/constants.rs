//! Timing windows and frame constants for the game judgment engine.
//!
//! Values are represented in exact microsecond durations.
//! Naming matches the C# source from NoteDrop.cs and NoteLongDrop.cs.

use super::time::Duration;

/// Frame length in microseconds (60fps = 16667μs per frame)
pub const FRAME_LENGTH: Duration = Duration { ticks: super::time::TimeTick { val: 16667 } };

/// Frame length in milliseconds (alias for FRAME_LENGTH)
pub const FRAME_LENGTH_MSEC: Duration = FRAME_LENGTH;

// ============================================================================
// Tap Judgment Windows
// ============================================================================

/// 1st Perfect segment (1 frame)
pub const TAP_JUDGE_SEG_1ST_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 16667 } };

/// 2nd Perfect segment (2 frames)
pub const TAP_JUDGE_SEG_2ND_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 33334 } };

/// 3rd Perfect segment (3 frames)
pub const TAP_JUDGE_SEG_3RD_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 50001 } };

/// 1st Great segment (4 frames)
pub const TAP_JUDGE_SEG_1ST_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 66668 } };

/// 2nd Great segment (5 frames)
pub const TAP_JUDGE_SEG_2ND_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 83335 } };

/// 3rd Great segment (6 frames)
pub const TAP_JUDGE_SEG_3RD_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 100002 } };

/// Good area (9 frames)
pub const TAP_JUDGE_GOOD_AREA_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 150003 } };

// ============================================================================
// Touch Judgment Windows
// ============================================================================

/// 1st Perfect segment (9 frames)
pub const TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 150003 } };

/// 2nd Perfect segment
pub const TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 175004 } };

/// 3rd Perfect segment (12 frames)
pub const TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 200004 } };

/// 1st Great segment (13 frames)
pub const TOUCH_JUDGE_SEG_1ST_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 216671 } };

/// 2nd Great segment (14 frames)
pub const TOUCH_JUDGE_SEG_2ND_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 233338 } };

/// 3rd Great segment (15 frames)
pub const TOUCH_JUDGE_SEG_3RD_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 250005 } };

/// Good area (18 frames)
pub const TOUCH_JUDGE_GOOD_AREA_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 300006 } };

// ============================================================================
// Hold Constants
// ============================================================================

/// Hold head ignore length (6 frames)
pub const HOLD_HEAD_IGNORE_LENGTH_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 100002 } };

/// Hold tail ignore length (12 frames)
pub const HOLD_TAIL_IGNORE_LENGTH_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 200004 } };

/// Touch hold head ignore length (15 frames)
pub const TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 250005 } };

/// Touch hold tail ignore length (12 frames)
pub const TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 200004 } };

/// Deluxe hold release ignore time (2 frames)
pub const DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 33334 } };

/// Classic hold allow over length (20 frames)
pub const CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 333340 } };

/// Judge offset (default 0)
pub const JUDGE_OFFSET: Duration = Duration { ticks: super::time::TimeTick { val: 0 } };

/// Touch panel offset (default 0)
pub const TOUCH_PANEL_OFFSET: Duration = Duration { ticks: super::time::TimeTick { val: 0 } };

/// Use button ring for touch (default false)
pub const USE_BUTTON_RING_FOR_TOUCH: bool = false;

/// Subdivide slide judge grade (default false)
pub const SUBDIVIDE_SLIDE_JUDGE_GRADE: bool = false;

/// Hold classic end judge perfect fast (9 frames)
pub const HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 150003 } };

/// Hold classic end judge perfect late (12 frames)
pub const HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 200004 } };

// ============================================================================
// Slide Constants
// ============================================================================

/// Slide judge maximum allowed extension length (22 frames)
pub const SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 366674 } };

/// Slide judge base 3rd perfect (14 frames)
pub const SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 233338 } };

/// Slide judge 1st great (21 frames)
pub const SLIDE_JUDGE_SEG_1ST_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 350007 } };

/// Slide judge 2nd great (25 frames)
pub const SLIDE_JUDGE_SEG_2ND_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 416675 } };

/// Slide judge 3rd great (29 frames)
pub const SLIDE_JUDGE_SEG_3RD_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 483343 } };

/// Slide judge good area (36 frames)
pub const SLIDE_JUDGE_GOOD_AREA_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 600012 } };

// ============================================================================
// Slide Classic Fast Windows
// ============================================================================

/// Slide classic fast 1st perfect (4 frames)
pub const SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 66668 } };

/// Slide classic fast 2nd perfect (8 frames)
pub const SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 133336 } };

/// Slide classic fast 3rd perfect (12 frames)
pub const SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 200004 } };

/// Slide classic fast 1st great (16 frames)
pub const SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 266672 } };

/// Slide classic fast 2nd great (20 frames)
pub const SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 333340 } };

/// Slide classic fast 3rd great (24 frames)
pub const SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 400008 } };

// ============================================================================
// Slide Classic Late Windows
// ============================================================================

/// Slide classic late 1st perfect (4 frames)
pub const SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 66668 } };

/// Slide classic late 2nd perfect (8 frames)
pub const SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 133336 } };

/// Slide classic late 3rd perfect (12 frames)
pub const SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 200004 } };

/// Slide classic late 1st great (16 frames)
pub const SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 266672 } };

/// Slide classic late 2nd great (20 frames)
pub const SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 333340 } };

/// Slide classic late 3rd great (24 frames)
pub const SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC: Duration = Duration { ticks: super::time::TimeTick { val: 400008 } };

// ============================================================================
// Zone / Sensor Counts
// ============================================================================

/// Number of button zones (K1-K8)
pub const BUTTON_ZONE_COUNT: usize = 8;

/// Number of sensor areas (A1-A8, B1-B8, C, D1-D8, E1-E8)
pub const SENSOR_AREA_COUNT: usize = 33;

// ============================================================================
// Judgeable Ranges
// ============================================================================

/// Judgable range in seconds (150ms)
pub const JUDGABLE_RANGE_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 150000 } };

/// Touch judgable range late extra (10 frames)
pub const TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC: Duration = Duration { ticks: super::time::TimeTick { val: 166670 } };
