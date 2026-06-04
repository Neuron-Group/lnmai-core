//! Aeneas-friendly judge module
//!
//! Pure judgment functions

use super::constants::*;
use super::time::Duration;
use super::types::JudgeGrade;

/// Judge a tap note
pub fn judge_tap(diff: Duration, is_ex: bool) -> JudgeGrade {
    if is_ex {
        return JudgeGrade::Perfect;
    }

    let is_fast = diff.micros < 0;
    let abs_diff = diff.abs();

    if abs_diff.micros <= TAP_PERFECT_1ST.micros {
        JudgeGrade::Perfect
    } else if abs_diff.micros <= TAP_PERFECT_2ND.micros {
        if is_fast { JudgeGrade::FastPerfect2nd } else { JudgeGrade::LatePerfect2nd }
    } else if abs_diff.micros <= TAP_PERFECT_3RD.micros {
        if is_fast { JudgeGrade::FastPerfect3rd } else { JudgeGrade::LatePerfect3rd }
    } else if abs_diff.micros <= TAP_GREAT_1ST.micros {
        if is_fast { JudgeGrade::FastGreat } else { JudgeGrade::LateGreat }
    } else if abs_diff.micros <= TAP_GREAT_2ND.micros {
        if is_fast { JudgeGrade::FastGreat2nd } else { JudgeGrade::LateGreat2nd }
    } else if abs_diff.micros <= TAP_GREAT_3RD.micros {
        if is_fast { JudgeGrade::FastGreat3rd } else { JudgeGrade::LateGreat3rd }
    } else {
        if is_fast { JudgeGrade::FastGood } else { JudgeGrade::LateGood }
    }
}

/// Judge a touch note
pub fn judge_touch(diff: Duration, _is_ex: bool) -> Option<JudgeGrade> {
    let is_fast = diff.micros < 0;
    let abs_diff = diff.abs();

    if is_fast && abs_diff.micros > TOUCH_PERFECT_1ST.micros {
        return None;
    }

    let grade = if abs_diff.micros <= TOUCH_PERFECT_1ST.micros {
        JudgeGrade::Perfect
    } else if abs_diff.micros <= TOUCH_PERFECT_2ND.micros {
        JudgeGrade::LatePerfect2nd
    } else if abs_diff.micros <= TOUCH_PERFECT_3RD.micros {
        JudgeGrade::LatePerfect3rd
    } else if abs_diff.micros <= TOUCH_GREAT_1ST.micros {
        JudgeGrade::LateGreat
    } else if abs_diff.micros <= TOUCH_GREAT_2ND.micros {
        JudgeGrade::LateGreat2nd
    } else if abs_diff.micros <= TOUCH_GREAT_3RD.micros {
        JudgeGrade::LateGreat3rd
    } else {
        JudgeGrade::LateGood
    };

    Some(grade)
}

/// Judge a classic slide
pub fn judge_slide_classic(diff: Duration) -> JudgeGrade {
    let is_fast = diff.micros < 0;
    let abs_diff = diff.abs();

    if is_fast {
        if abs_diff.micros <= 66668 {
            JudgeGrade::Perfect
        } else if abs_diff.micros <= 133336 {
            JudgeGrade::FastPerfect2nd
        } else if abs_diff.micros <= 200004 {
            JudgeGrade::FastPerfect3rd
        } else if abs_diff.micros <= 266672 {
            JudgeGrade::FastGreat
        } else if abs_diff.micros <= 333340 {
            JudgeGrade::FastGreat2nd
        } else if abs_diff.micros <= 400008 {
            JudgeGrade::FastGreat3rd
        } else {
            JudgeGrade::FastGood
        }
    } else {
        if abs_diff.micros <= 66668 {
            JudgeGrade::Perfect
        } else if abs_diff.micros <= 133336 {
            JudgeGrade::LatePerfect2nd
        } else if abs_diff.micros <= 200004 {
            JudgeGrade::LatePerfect3rd
        } else if abs_diff.micros <= 266672 {
            JudgeGrade::LateGreat
        } else if abs_diff.micros <= 333340 {
            JudgeGrade::LateGreat2nd
        } else if abs_diff.micros <= 400008 {
            JudgeGrade::LateGreat3rd
        } else {
            JudgeGrade::LateGood
        }
    }
}

/// Correct slide grade (collapse subdivided grades)
pub fn correct_slide_grade(grade: JudgeGrade) -> JudgeGrade {
    match grade {
        JudgeGrade::LatePerfect3rd
        | JudgeGrade::LatePerfect2nd
        | JudgeGrade::FastPerfect2nd
        | JudgeGrade::FastPerfect3rd => JudgeGrade::Perfect,
        _ => grade,
    }
}

/// Judge slide too late
pub fn judge_slide_too_late(queue_remaining: u32) -> JudgeGrade {
    if queue_remaining == 1 {
        JudgeGrade::LateGood
    } else {
        JudgeGrade::Miss
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_judge_tap_perfect() {
        assert_eq!(judge_tap(Duration::from_micros(0), false), JudgeGrade::Perfect);
        assert_eq!(judge_tap(Duration::from_micros(10000), false), JudgeGrade::Perfect);
    }

    #[test]
    fn test_judge_tap_ex() {
        assert_eq!(judge_tap(Duration::from_micros(100000), true), JudgeGrade::Perfect);
    }

    #[test]
    fn test_judge_slide_too_late() {
        assert_eq!(judge_slide_too_late(1), JudgeGrade::LateGood);
        assert_eq!(judge_slide_too_late(2), JudgeGrade::Miss);
    }

    #[test]
    fn test_correct_slide_grade() {
        assert_eq!(correct_slide_grade(JudgeGrade::LatePerfect2nd), JudgeGrade::Perfect);
        assert_eq!(correct_slide_grade(JudgeGrade::LateGreat), JudgeGrade::LateGreat);
    }
}
