//! Exact-time adapters to the finite judgment tables.
//!
//! Values outside the last threshold can use a representative with the same
//! sign. Hold percentages are computed exactly before calling the shared
//! grade table. No unbounded input is cast to a machine integer.

use crate::{
    integer::Integer,
    time::Duration,
    types::{JudgeDisplayOption, JudgeGrade},
};
use lnmai_core_verify::{judge as kernel, time::Duration as BoundedDuration};

pub use kernel::correct_slide_grade;

fn bounded(value: &Integer, limit: i64) -> i64 {
    if value < &Integer::from(-limit) {
        -limit
    } else if value > &Integer::from(limit) {
        limit
    } else {
        value
            .to_i64()
            .expect("value lies in the checked i64 interval")
    }
}
fn diff_rep(value: &Duration) -> BoundedDuration {
    BoundedDuration::from_micros(bounded(&value.micros, 1_000_000))
}
pub fn judge_tap(diff: Duration, is_ex: bool) -> JudgeGrade {
    kernel::judge_tap(diff_rep(&diff), is_ex)
}
pub fn judge_touch(diff: Duration, is_ex: bool) -> Option<JudgeGrade> {
    kernel::judge_touch(diff_rep(&diff), is_ex)
}

/// `Judge.slideClassicFastThresholds`, in first-matching threshold order.
pub fn slide_classic_fast_thresholds() -> Vec<(Duration, JudgeGrade)> {
    use crate::constants::*;
    use JudgeGrade::*;
    vec![
        (SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC, Perfect),
        (
            SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC,
            FastPerfect2nd,
        ),
        (
            SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC,
            FastPerfect3rd,
        ),
        (SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC, FastGreat),
        (SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC, FastGreat2nd),
        (SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC, FastGreat3rd),
    ]
}

/// `Judge.slideClassicLateThresholds`, in first-matching threshold order.
pub fn slide_classic_late_thresholds() -> Vec<(Duration, JudgeGrade)> {
    use crate::constants::*;
    use JudgeGrade::*;
    vec![
        (SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC, Perfect),
        (
            SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC,
            LatePerfect2nd,
        ),
        (
            SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC,
            LatePerfect3rd,
        ),
        (SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC, LateGreat),
        (SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC, LateGreat2nd),
        (SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC, LateGreat3rd),
    ]
}

pub fn judge_slide_classic(diff: Duration) -> JudgeGrade {
    kernel::judge_slide_classic(diff_rep(&diff))
}
pub fn judge_slide_modern(diff: Duration, stay: Duration, is_ex: bool) -> JudgeGrade {
    // At the lower bound all three perfect windows are negative; at the
    // upper bound the extension has already reached its cap.
    kernel::judge_slide_modern(
        diff_rep(&diff),
        BoundedDuration::from_micros(bounded(&stay.micros, 4_000_000)),
        is_ex,
    )
}
pub fn judge_slide_too_late(remaining: impl Into<Integer>) -> JudgeGrade {
    if remaining.into() == 1 {
        JudgeGrade::LateGood
    } else {
        JudgeGrade::Miss
    }
}
pub fn is_too_late_slide(diff: Duration, user_offset: Duration) -> bool {
    diff.micros > Integer::from(600012) + user_offset.micros.min(Integer::small(0))
}
pub fn judge_hold_end(
    head: JudgeGrade,
    diff: Duration,
    length: Duration,
    ignore: Duration,
    release: Duration,
) -> JudgeGrade {
    let maximum = (&length.micros - 300000).max(Integer::small(0));
    let reality = (length.micros - ignore.micros)
        .max(Integer::small(0))
        .min(maximum);
    if reality <= 0 {
        return head;
    }
    let held = (&reality - release.micros).max(Integer::small(0));
    let band = if held >= reality {
        0
    } else if &held * 100 >= &reality * 67 {
        1
    } else if &held * 100 >= &reality * 33 {
        2
    } else if &held * 100 >= &reality * 5 {
        3
    } else {
        4
    };
    kernel::hold_band_grade(head, diff.micros >= 0, band)
}
pub fn judge_hold_classic_end(
    head: JudgeGrade,
    timing: impl Into<Integer>,
    length: Duration,
    release: impl Into<Integer>,
) -> JudgeGrade {
    let diff = timing.into() + length.micros - release.into();
    kernel::judge_hold_classic_end(head, bounded(&diff, 1_000_000), BoundedDuration::zero(), 0)
}
pub fn count_fast_late(
    grade: JudgeGrade,
    diff: Duration,
    display: JudgeDisplayOption,
) -> (bool, bool) {
    lnmai_core_verify::score::count_fast_late(grade, diff_rep(&diff), display)
}
