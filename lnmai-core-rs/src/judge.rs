//! Pure judgment functions — map timing error to JudgeGrade.
//!
//! Faithful transcriptions of:
//!   NoteDrop.Judge()         — Tap/Hold head
//!   TouchDrop.Judge()        — Touch (late-only)
//!   SlideBase.Judge()        — Modern slide (dynamic extension)
//!   SlideBase.JudgeClassic() — Classic slide (fixed windows)
//!   NoteLongDrop.HoldEndJudge()        — Hold release quality (press-band table)
//!   NoteLongDrop.HoldClassicEndJudge() — Classic hold release judgment
//!
//! All functions compute raw grades. Grade conversion (ConvertJudgeGrade)
//! and subgrade correction (JudgeGradeCorrection) are separate passes.

use super::constants::*;
use super::time::Duration;
use super::types::JudgeGrade;

/// Helper: absolute difference
fn abs_diff(diff: Duration) -> Duration {
    diff.abs()
}

// ============================================================================
// Tap / Hold Head Judgment
// From NoteDrop.Judge(), lines 262-290
// ============================================================================

/// Judge a Tap or Hold-head note based on timing difference in ms.
/// `diff` = (currentSec - JudgeTimingWithOffset) * 1000
/// Returns the raw (unconverted) grade.
pub fn judge_tap(diff: Duration, is_ex: bool) -> JudgeGrade {
    if is_ex {
        return JudgeGrade::Perfect; // EX notes always Critical Perfect
    }

    let is_fast = diff < Duration::zero();
    let diff_msec = abs_diff(diff);

    if diff_msec <= TAP_JUDGE_SEG_1ST_PERFECT_MSEC {
        JudgeGrade::Perfect
    } else if diff_msec <= TAP_JUDGE_SEG_2ND_PERFECT_MSEC {
        if is_fast { JudgeGrade::FastPerfect2nd } else { JudgeGrade::LatePerfect2nd }
    } else if diff_msec <= TAP_JUDGE_SEG_3RD_PERFECT_MSEC {
        if is_fast { JudgeGrade::FastPerfect3rd } else { JudgeGrade::LatePerfect3rd }
    } else if diff_msec <= TAP_JUDGE_SEG_1ST_GREAT_MSEC {
        if is_fast { JudgeGrade::FastGreat } else { JudgeGrade::LateGreat }
    } else if diff_msec <= TAP_JUDGE_SEG_2ND_GREAT_MSEC {
        if is_fast { JudgeGrade::FastGreat2nd } else { JudgeGrade::LateGreat2nd }
    } else if diff_msec <= TAP_JUDGE_SEG_3RD_GREAT_MSEC {
        if is_fast { JudgeGrade::FastGreat3rd } else { JudgeGrade::LateGreat3rd }
    } else {
        if is_fast { JudgeGrade::FastGood } else { JudgeGrade::LateGood }
    }
}

// ============================================================================
// Touch Judgment (late-only, no fast side)
// From TouchDrop.Judge(), lines ~100-130
// ============================================================================

/// Judge a Touch note. Touch has no fast-side judgments —
/// if input is too early (fast && beyond 1st perfect window), bails.
/// Unlike tap/hold-head notes, touch EX notes are not auto-promoted.
/// Returns `None` if the hit is too early to count (caller should ignore).
pub fn judge_touch(diff: Duration, _is_ex: bool) -> Option<JudgeGrade> {
    let is_fast = diff < Duration::zero();
    let diff_msec = abs_diff(diff);

    // Touch: if fast and beyond 1st perfect, too early → no judgment
    if is_fast && diff_msec > TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC {
        return None;
    }

    let grade = if diff_msec <= TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC {
        JudgeGrade::Perfect
    } else if diff_msec <= TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC {
        JudgeGrade::LatePerfect2nd
    } else if diff_msec <= TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC {
        JudgeGrade::LatePerfect3rd
    } else if diff_msec <= TOUCH_JUDGE_SEG_1ST_GREAT_MSEC {
        JudgeGrade::LateGreat
    } else if diff_msec <= TOUCH_JUDGE_SEG_2ND_GREAT_MSEC {
        JudgeGrade::LateGreat2nd
    } else if diff_msec <= TOUCH_JUDGE_SEG_3RD_GREAT_MSEC {
        JudgeGrade::LateGreat3rd
    } else {
        JudgeGrade::LateGood
    };

    Some(grade)
}

// ============================================================================
// Modern Slide Judgment (dynamic extension)
// From SlideBase.Judge(), lines 224-273
// ============================================================================

/// Judge a modern (deluxe) slide. The 3rd-perfect window is dynamically extended
/// based on `stay_time` (last wait time at slide end, in ms).
/// The 1st and 2nd perfect windows are 1/3 and 2/3 of the 3rd window respectively.
/// Slide EX notes are judged normally; they are not auto-promoted.
pub fn judge_slide_modern(diff: Duration, stay_time: Duration, _is_ex: bool) -> JudgeGrade {
    let is_fast = diff < Duration::zero();
    let diff_msec = abs_diff(diff);

    // Dynamic extension: ext = min(stayTimeMs / 4, 22-frame max)
    let ext = stay_time.div_nat(4).min(SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC);
    let seg_3rd_perfect = SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC + ext;
    let seg_1st_perfect = seg_3rd_perfect.div_nat(3);
    let seg_2nd_perfect = (seg_3rd_perfect.scale_nat(2)).div_nat(3);

    if diff_msec <= seg_1st_perfect {
        JudgeGrade::Perfect
    } else if diff_msec <= seg_2nd_perfect {
        if is_fast { JudgeGrade::FastPerfect2nd } else { JudgeGrade::LatePerfect2nd }
    } else if diff_msec <= seg_3rd_perfect {
        if is_fast { JudgeGrade::FastPerfect3rd } else { JudgeGrade::LatePerfect3rd }
    } else if diff_msec <= SLIDE_JUDGE_SEG_1ST_GREAT_MSEC {
        if is_fast { JudgeGrade::FastGreat } else { JudgeGrade::LateGreat }
    } else if diff_msec <= SLIDE_JUDGE_SEG_2ND_GREAT_MSEC {
        if is_fast { JudgeGrade::FastGreat2nd } else { JudgeGrade::LateGreat2nd }
    } else if diff_msec <= SLIDE_JUDGE_SEG_3RD_GREAT_MSEC {
        if is_fast { JudgeGrade::FastGreat3rd } else { JudgeGrade::LateGreat3rd }
    } else {
        if is_fast { JudgeGrade::FastGood } else { JudgeGrade::LateGood }
    }
}

// ============================================================================
// Classic Slide Judgment (fixed windows, separate fast/late thresholds)
// From SlideBase.JudgeClassic(), lines 274-327
// ============================================================================

/// Classic slide: fast-side thresholds
const SLIDE_CLASSIC_FAST_THRESHOLDS: &[(Duration, JudgeGrade)] = &[
    (SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_PERFECT_MSEC, JudgeGrade::Perfect),
    (SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_PERFECT_MSEC, JudgeGrade::FastPerfect2nd),
    (SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_PERFECT_MSEC, JudgeGrade::FastPerfect3rd),
    (SLIDE_JUDGE_CLASSIC_FAST_SEG_1ST_GREAT_MSEC, JudgeGrade::FastGreat),
    (SLIDE_JUDGE_CLASSIC_FAST_SEG_2ND_GREAT_MSEC, JudgeGrade::FastGreat2nd),
    (SLIDE_JUDGE_CLASSIC_FAST_SEG_3RD_GREAT_MSEC, JudgeGrade::FastGreat3rd),
];

/// Classic slide: late-side thresholds
const SLIDE_CLASSIC_LATE_THRESHOLDS: &[(Duration, JudgeGrade)] = &[
    (SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_PERFECT_MSEC, JudgeGrade::Perfect),
    (SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_PERFECT_MSEC, JudgeGrade::LatePerfect2nd),
    (SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_PERFECT_MSEC, JudgeGrade::LatePerfect3rd),
    (SLIDE_JUDGE_CLASSIC_LATE_SEG_1ST_GREAT_MSEC, JudgeGrade::LateGreat),
    (SLIDE_JUDGE_CLASSIC_LATE_SEG_2ND_GREAT_MSEC, JudgeGrade::LateGreat2nd),
    (SLIDE_JUDGE_CLASSIC_LATE_SEG_3RD_GREAT_MSEC, JudgeGrade::LateGreat3rd),
];

fn pick_grade(diff_msec: Duration, thresholds: &[(Duration, JudgeGrade)], fallback: JudgeGrade) -> JudgeGrade {
    for &(limit, grade) in thresholds {
        if diff_msec <= limit {
            return grade;
        }
    }
    fallback
}

/// Judge a classic-mode slide. Uses fixed windows that are symmetrical in
/// frame count but stored as separate fast/late constant sets.
pub fn judge_slide_classic(diff: Duration) -> JudgeGrade {
    let is_fast = diff < Duration::zero();
    let diff_msec = abs_diff(diff);

    if is_fast {
        pick_grade(diff_msec, SLIDE_CLASSIC_FAST_THRESHOLDS, JudgeGrade::FastGood)
    } else {
        pick_grade(diff_msec, SLIDE_CLASSIC_LATE_THRESHOLDS, JudgeGrade::LateGood)
    }
}

/// Slide judge grade correction from `SlideBase.JudgeGradeCorrection()`.
///
/// This collapses subdivided slide grades into the coarser grades used by the
/// default result flow when slide subgrades are not displayed separately.
pub fn correct_slide_grade(grade: JudgeGrade) -> JudgeGrade {
    match grade {
        JudgeGrade::LatePerfect3rd
        | JudgeGrade::LatePerfect2nd
        | JudgeGrade::FastPerfect2nd
        | JudgeGrade::FastPerfect3rd => JudgeGrade::Perfect,
        _ => grade,
    }
}

// ============================================================================
// Hold End Judgment (Deluxe/Modern) — press-band lookup table
// From NoteLongDrop.HoldEndJudge(), lines 66-255
// ============================================================================

/// Compute the press band index from held percentage:
///   0: >= 100%    1: [67%, 100%)   2: [33%, 67%)
///   3: [5%, 33%)  4: [0%, 5%)
fn press_band_micros(held_micros: i64, reality_micros: i64) -> u32 {
    if held_micros >= reality_micros {
        0
    } else if held_micros * 100 >= reality_micros * 67 {
        1
    } else if held_micros * 100 >= reality_micros * 33 {
        2
    } else if held_micros * 100 >= reality_micros * 5 {
        3
    } else {
        4
    }
}

/// HoldEndJudge: computes the final hold grade from head grade and how
/// long the player held the button.
///
/// Parameters:
///   head_grade            — grade from the tap head judgment
///   judge_diff            — judge diff in ms (from head judgment; negative = fast)
///   length                — total hold length in seconds
///   ignore_time           — head + tail ignore duration (6f+12f=0.3s for regular hold, 15f+12f=0.45s for touch hold)
///   player_release_time   — accumulated release time in seconds
pub fn judge_hold_end(
    head_grade: JudgeGrade,
    judge_diff: Duration,
    length: Duration,
    ignore_time: Duration,
    player_release_time: Duration,
) -> JudgeGrade {
    // offset: 0 if fast-side head, otherwise = judgeDiff
    let offset = if head_grade.is_fast() { Duration::zero() } else { judge_diff };
    // realityHT = effective hold time (minus ignores, minus late offset, clamped)
    let reality_ht_raw = length - ignore_time - offset;
    let reality_ht_max = length - Duration::from_micros(300000);
    let reality_ht = reality_ht_raw.max(Duration::zero()).min(reality_ht_max);

    if reality_ht <= Duration::zero() {
        return head_grade;
    }

    let held = (reality_ht - player_release_time).max(Duration::zero());
    let band = press_band_micros(held.to_micros(), reality_ht.to_micros());

    match band {
        0 => {
            // >= 100%: release never or very late
            match head_grade {
                JudgeGrade::LatePerfect3rd
                | JudgeGrade::LatePerfect2nd
                | JudgeGrade::Perfect
                | JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd => head_grade,
                JudgeGrade::LateGood
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat => JudgeGrade::LateGreat,
                JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
                | JudgeGrade::FastGood => JudgeGrade::FastGreat,
                JudgeGrade::Miss => JudgeGrade::LateGood,
                JudgeGrade::TooFast => JudgeGrade::FastGood,
            }
        }
        1 => {
            // [67%, 100%): release slightly early
            match head_grade {
                JudgeGrade::Perfect => {
                    if judge_diff >= Duration::zero() {
                        JudgeGrade::LatePerfect2nd
                    } else {
                        JudgeGrade::FastPerfect2nd
                    }
                }
                JudgeGrade::LatePerfect3rd
                | JudgeGrade::LatePerfect2nd
                | JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd => head_grade,
                JudgeGrade::LateGood
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat => JudgeGrade::LateGreat,
                JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
                | JudgeGrade::FastGood => JudgeGrade::FastGreat,
                JudgeGrade::Miss => JudgeGrade::LateGood,
                JudgeGrade::TooFast => JudgeGrade::FastGood,
            }
        }
        2 => {
            // [33%, 67%): release moderately early
            match head_grade {
                JudgeGrade::Perfect => {
                    if judge_diff >= Duration::zero() {
                        JudgeGrade::LateGreat2nd
                    } else {
                        JudgeGrade::FastGreat2nd
                    }
                }
                JudgeGrade::LateGood
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat
                | JudgeGrade::LatePerfect3rd
                | JudgeGrade::LatePerfect2nd => JudgeGrade::LateGreat,
                JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd
                | JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
                | JudgeGrade::FastGood => JudgeGrade::FastGreat,
                JudgeGrade::Miss => JudgeGrade::LateGood,
                JudgeGrade::TooFast => JudgeGrade::FastGood,
            }
        }
        3 => {
            // [5%, 33%): release very early
            match head_grade {
                JudgeGrade::Perfect => {
                    if judge_diff >= Duration::zero() {
                        JudgeGrade::LateGood
                    } else {
                        JudgeGrade::FastGood
                    }
                }
                JudgeGrade::Miss
                | JudgeGrade::LateGood
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat
                | JudgeGrade::LatePerfect3rd
                | JudgeGrade::LatePerfect2nd => JudgeGrade::LateGood,
                JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd
                | JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
                | JudgeGrade::FastGood
                | JudgeGrade::TooFast => JudgeGrade::FastGood,
            }
        }
        _ => {
            // [0%, 5%): release almost immediately
            match head_grade {
                JudgeGrade::Perfect => {
                    if judge_diff >= Duration::zero() {
                        JudgeGrade::LateGood
                    } else {
                        JudgeGrade::FastGood
                    }
                }
                JudgeGrade::LateGood
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat
                | JudgeGrade::LatePerfect3rd
                | JudgeGrade::LatePerfect2nd => JudgeGrade::LateGood,
                JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd
                | JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
                | JudgeGrade::FastGood => JudgeGrade::FastGood,
                JudgeGrade::Miss | JudgeGrade::TooFast => head_grade,
            }
        }
    }
}

// ============================================================================
// Hold Classic End Judgment
// From NoteLongDrop.HoldClassicEndJudge(), lines 257-307
// ============================================================================

/// Classic hold end judge: evaluates release timing independently,
/// then takes the WORSE of head grade vs end grade.
/// Comparison uses |7 - (int)grade| distance from Perfect.
pub fn judge_hold_classic_end(
    head_grade: JudgeGrade,
    timing: i64, // TimePoint in microseconds
    length: Duration,
    release_timing: i64, // TimePoint in microseconds
) -> JudgeGrade {
    // If head is already Miss or TooFast, no improvement possible
    if head_grade.is_miss_or_too_fast() {
        return head_grade;
    }

    let diff = Duration::from_micros(timing + length.to_micros() - release_timing);
    let is_fast = diff > Duration::zero();
    let diff_msec = abs_diff(diff);

    // End grade: Perfect if within window, else Good
    let end_grade = if is_fast {
        if diff_msec < HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC {
            JudgeGrade::Perfect
        } else {
            JudgeGrade::FastGood
        }
    } else {
        if diff_msec < HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC {
            JudgeGrade::Perfect
        } else {
            JudgeGrade::LateGood
        }
    };

    // Take worst: compare distance from 7 (Perfect)
    let head_dist = head_grade.dist_from_perfect();
    let end_dist = end_grade.dist_from_perfect();
    if end_dist > head_dist { end_grade } else { head_grade }
}

// ============================================================================
// Slide Too-Late Judge
// From SlideBase.TooLateJudge(): if queueRemaining == 1 → LateGood, else Miss
// ============================================================================

/// Judge a slide that is too late.
/// If only 1 segment remaining → LateGood, else Miss
pub fn judge_slide_too_late(queue_remaining: u32) -> JudgeGrade {
    if queue_remaining == 1 {
        JudgeGrade::LateGood
    } else {
        JudgeGrade::Miss
    }
}

/// Check if a slide is too late
pub fn is_too_late_slide(diff: Duration, user_offset: Duration) -> bool {
    let threshold = SLIDE_JUDGE_GOOD_AREA_MSEC + user_offset.min(Duration::zero());
    diff > threshold
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_judge_tap_perfect() {
        // Within 1st perfect window
        let diff = Duration::from_micros(10000); // 10ms
        assert_eq!(judge_tap(diff, false), JudgeGrade::Perfect);
    }

    #[test]
    fn test_judge_tap_ex() {
        // EX notes always Perfect
        let diff = Duration::from_micros(100000); // 100ms
        assert_eq!(judge_tap(diff, true), JudgeGrade::Perfect);
    }

    #[test]
    fn test_judge_tap_fast() {
        // Fast side
        let diff = Duration::from_micros(-20000); // -20ms
        assert_eq!(judge_tap(diff, false), JudgeGrade::FastPerfect2nd);
    }

    #[test]
    fn test_judge_tap_late() {
        // Late side
        let diff = Duration::from_micros(20000); // 20ms
        assert_eq!(judge_tap(diff, false), JudgeGrade::LatePerfect2nd);
    }

    #[test]
    fn test_judge_touch_too_fast() {
        // Too fast: beyond 1st perfect window
        let diff = Duration::from_micros(-200000); // -200ms
        assert_eq!(judge_touch(diff, false), None);
    }

    #[test]
    fn test_judge_touch_perfect() {
        // Within 1st perfect window
        let diff = Duration::from_micros(100000); // 100ms
        assert_eq!(judge_touch(diff, false), Some(JudgeGrade::Perfect));
    }

    #[test]
    fn test_judge_slide_too_late() {
        assert_eq!(judge_slide_too_late(1), JudgeGrade::LateGood);
        assert_eq!(judge_slide_too_late(2), JudgeGrade::Miss);
        assert_eq!(judge_slide_too_late(0), JudgeGrade::Miss);
    }

    #[test]
    fn test_correct_slide_grade() {
        assert_eq!(correct_slide_grade(JudgeGrade::LatePerfect3rd), JudgeGrade::Perfect);
        assert_eq!(correct_slide_grade(JudgeGrade::LatePerfect2nd), JudgeGrade::Perfect);
        assert_eq!(correct_slide_grade(JudgeGrade::FastPerfect2nd), JudgeGrade::Perfect);
        assert_eq!(correct_slide_grade(JudgeGrade::FastPerfect3rd), JudgeGrade::Perfect);
        assert_eq!(correct_slide_grade(JudgeGrade::Perfect), JudgeGrade::Perfect);
        assert_eq!(correct_slide_grade(JudgeGrade::LateGreat), JudgeGrade::LateGreat);
    }
}
