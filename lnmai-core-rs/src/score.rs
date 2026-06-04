//! Score and combo computation — faithful transcription of
//! ObjectCounter.UpdateComboCount() and UpdateNoteScoreCount().

use super::types::{JudgeGrade, NoteType, ScoreState};

/// Base Score per Note Type
pub fn base_score(nt: NoteType) -> u32 {
    match nt {
        NoteType::Tap => 500,
        NoteType::Hold => 1000,
        NoteType::Slide => 1500,
        NoteType::Touch => 500,
        NoteType::Break => 2500,
    }
}

/// Extra score for Break notes (DX extra)
pub fn extra_score(_nt: NoteType) -> u32 {
    100
}

/// Score a non-Break note. Returns (baseEarned, baseLost).
/// - Miss/TooFast: 0% earned, 100% lost
/// - Good:         50% earned, 50% lost
/// - Great*:       80% earned, 20% lost
/// - Perfect*:     100% earned
pub fn score_non_break(base_score: u32, grade: JudgeGrade, multiple: u32) -> (u32, u32) {
    let b = base_score * multiple;
    match grade {
        JudgeGrade::Miss | JudgeGrade::TooFast => (0, b),
        JudgeGrade::LateGood | JudgeGrade::FastGood => (b / 2, b - b / 2),
        JudgeGrade::LateGreat
        | JudgeGrade::LateGreat2nd
        | JudgeGrade::LateGreat3rd
        | JudgeGrade::FastGreat
        | JudgeGrade::FastGreat2nd
        | JudgeGrade::FastGreat3rd => (b * 4 / 5, b - b * 4 / 5),
        JudgeGrade::LatePerfect3rd
        | JudgeGrade::LatePerfect2nd
        | JudgeGrade::Perfect
        | JudgeGrade::FastPerfect2nd
        | JudgeGrade::FastPerfect3rd => (b, 0),
    }
}

/// Score a Break note (2500 base + 100 extra).
/// Extra score has two tracks: DX and Classic.
/// Classic extra is stricter (only Perfect2nd and Perfect earn any).
///
/// Returns (baseEarned, extraEarned, classicExtraEarned,
///          baseLost, extraLost, classicExtraLost)
pub fn score_break(grade: JudgeGrade, multiple: u32) -> (u32, u32, u32, u32, u32, u32) {
    let m = multiple;
    match grade {
        JudgeGrade::Miss | JudgeGrade::TooFast => (
            0, 0, 0,           // earned: base, extraDX, extraClassic
            2500 * m, 100 * m, 100 * m, // lost: base, extraDX, extraClassic
        ),
        JudgeGrade::LateGood | JudgeGrade::FastGood => (
            1000 * m, 30 * m, 0,
            1500 * m, 70 * m, 100 * m,
        ),
        JudgeGrade::LateGreat3rd | JudgeGrade::FastGreat3rd => (
            1250 * m, 40 * m, 0,
            1250 * m, 60 * m, 100 * m,
        ),
        JudgeGrade::LateGreat2nd | JudgeGrade::FastGreat2nd => (
            1500 * m, 40 * m, 0,
            1000 * m, 60 * m, 100 * m,
        ),
        JudgeGrade::LateGreat | JudgeGrade::FastGreat => (
            2000 * m, 40 * m, 0,
            500 * m, 60 * m, 100 * m,
        ),
        JudgeGrade::LatePerfect3rd | JudgeGrade::FastPerfect3rd => (
            2500 * m, 50 * m, 0,
            0, 50 * m, 100 * m,
        ),
        JudgeGrade::LatePerfect2nd | JudgeGrade::FastPerfect2nd => (
            2500 * m, 75 * m, 50 * m,
            0, 25 * m, 50 * m,
        ),
        JudgeGrade::Perfect => (
            2500 * m, 100 * m, 100 * m,
            0, 0, 0,
        ),
    }
}

/// Combo state update result
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ComboDelta {
    pub combo: u32,
    pub p_combo: u32,
    pub c_p_combo: u32,
    pub d_x_score_lost: i32, // negative = lost score (subtracted from total)
}

/// Update combo counters for a single note judgment.
/// This is the pure version of ObjectCounter.UpdateComboCount().
/// Input: current combo state, the grade, and multiple.
/// Output: new combo state.
///
/// Note: the C# code increments _combo BEFORE calling UpdateComboCount
/// for non-Miss grades. The combo reset for Miss/TooFast overrides.
pub fn update_combo(
    combo: u32,
    p_combo: u32,
    c_p_combo: u32,
    d_x_score_lost: i32,
    grade: JudgeGrade,
    multiple: u32,
) -> ComboDelta {
    let m = multiple;
    match grade {
        JudgeGrade::Perfect => ComboDelta {
            combo: combo + m,
            p_combo: p_combo + m,
            c_p_combo: c_p_combo + m,
            d_x_score_lost,
        },
        JudgeGrade::LatePerfect2nd
        | JudgeGrade::FastPerfect2nd
        | JudgeGrade::LatePerfect3rd
        | JudgeGrade::FastPerfect3rd => ComboDelta {
            combo: combo + m,
            p_combo: p_combo + m,
            c_p_combo: 0,
            d_x_score_lost: d_x_score_lost - (1 * m) as i32,
        },
        JudgeGrade::LateGreat3rd
        | JudgeGrade::LateGreat2nd
        | JudgeGrade::LateGreat
        | JudgeGrade::FastGreat
        | JudgeGrade::FastGreat2nd
        | JudgeGrade::FastGreat3rd => ComboDelta {
            combo: combo + m,
            p_combo: 0,
            c_p_combo: 0,
            d_x_score_lost: d_x_score_lost - (2 * m) as i32,
        },
        JudgeGrade::LateGood | JudgeGrade::FastGood => ComboDelta {
            combo: combo + m,
            p_combo: 0,
            c_p_combo: 0,
            d_x_score_lost: d_x_score_lost - (3 * m) as i32,
        },
        JudgeGrade::Miss | JudgeGrade::TooFast => ComboDelta {
            combo: 0,
            p_combo: 0,
            c_p_combo: 0,
            d_x_score_lost: d_x_score_lost - (3 * m) as i32,
        },
    }
}

/// Fast/Late display option
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FastLateDisplay {
    /// Count all non-zero-diff, non-miss
    All,
    /// Count everything except Perfect (CP), Miss, TooFast
    BelowCP,
    /// Count only Great and Good (distance from Perfect > 2)
    BelowP,
}

/// Returns (isFast, isLate) increment flags for this grade,
/// given a display option (matching JudgeDisplayOption).
pub fn count_fast_late(grade: JudgeGrade, diff: i64, display: FastLateDisplay) -> (bool, bool) {
    if grade.is_miss_or_too_fast() {
        return (false, false);
    }

    let d = grade.dist_from_perfect();
    match display {
        FastLateDisplay::All => {
            if diff == 0 {
                (false, false)
            } else if diff < 0 {
                (true, false)
            } else {
                (false, true)
            }
        }
        FastLateDisplay::BelowCP => {
            if grade == JudgeGrade::Perfect {
                (false, false)
            } else if diff < 0 {
                (true, false)
            } else {
                (false, true)
            }
        }
        FastLateDisplay::BelowP => {
            if d <= 2 {
                (false, false) // skip Perfect, Perfect2nd, Perfect3rd
            } else if diff < 0 {
                (true, false)
            } else {
                (false, true)
            }
        }
    }
}

/// DXScore ranks: 5=SSS+, 4=SSS, 3=SS+, 2=SS, 1=S, 0=none
/// Thresholds: 97%, 95%, 93%, 90%, 85%
pub fn dx_score_rank(achieved_dx_score: u32, max_dx_score: u32) -> u32 {
    if max_dx_score == 0 {
        return 0;
    }

    let meets_percent = |threshold: u32| -> bool {
        achieved_dx_score * 100 >= max_dx_score * threshold
    };

    if meets_percent(97) {
        5
    } else if meets_percent(95) {
        4
    } else if meets_percent(93) {
        3
    } else if meets_percent(90) {
        2
    } else if meets_percent(85) {
        1
    } else {
        0
    }
}

/// Accuracy rate computation result
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AccRates {
    /// Classic acc (+) = CurrentNoteScoreClassic / TotalBase * 100
    pub classic_acc_plus: f64,
    /// Classic acc (-) = (TotalBase - LostBase + CurrentExtraClassic) / TotalBase * 100
    pub classic_acc_minus: f64,
    /// Acc 101(-) = (earnedBase/totalBase + earnedExtra/(totalExtra*100)) * 100
    pub dx_acc_minus_101: f64,
    /// Acc 100(-) = (earnedBase/totalBase + currentExtra/(totalExtra*100)) * 100
    pub dx_acc_minus_100: f64,
    /// Acc (+) = (currentBase/totalBase + currentExtra/(totalExtra*100)) * 100
    pub dx_acc_plus: f64,
}

/// Compute accuracy rates
pub fn compute_acc_rates(score: &ScoreState) -> AccRates {
    let tb = score.total_base as f64;
    let cb = score.earned_base as f64;
    let lb = score.lost_base as f64;
    let te = (score.total_extra.max(1)) as f64;
    let ce = score.earned_extra as f64;
    let le = score.lost_extra as f64;
    let cc = score.counts.break_count as f64; // Using break_count as classicExtraEarned
    let hundred = 100.0;
    let earned_base = tb - lb;
    let earned_extra = te - le;

    if score.total_base == 0 {
        AccRates {
            classic_acc_plus: 0.0,
            classic_acc_minus: 0.0,
            dx_acc_minus_101: 0.0,
            dx_acc_minus_100: 0.0,
            dx_acc_plus: 0.0,
        }
    } else {
        AccRates {
            classic_acc_plus: (cb + cc) / tb * hundred,
            classic_acc_minus: (earned_base + cc) / tb * hundred,
            dx_acc_minus_101: (earned_base / tb + earned_extra / (te * hundred)) * hundred,
            dx_acc_minus_100: (earned_base / tb + ce / (te * hundred)) * hundred,
            dx_acc_plus: (cb / tb + ce / (te * hundred)) * hundred,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base_score() {
        assert_eq!(base_score(NoteType::Tap), 500);
        assert_eq!(base_score(NoteType::Hold), 1000);
        assert_eq!(base_score(NoteType::Slide), 1500);
        assert_eq!(base_score(NoteType::Touch), 500);
        assert_eq!(base_score(NoteType::Break), 2500);
    }

    #[test]
    fn test_score_non_break_perfect() {
        let (earned, lost) = score_non_break(500, JudgeGrade::Perfect, 1);
        assert_eq!(earned, 500);
        assert_eq!(lost, 0);
    }

    #[test]
    fn test_score_non_break_miss() {
        let (earned, lost) = score_non_break(500, JudgeGrade::Miss, 1);
        assert_eq!(earned, 0);
        assert_eq!(lost, 500);
    }

    #[test]
    fn test_score_non_break_good() {
        let (earned, lost) = score_non_break(500, JudgeGrade::LateGood, 1);
        assert_eq!(earned, 250);
        assert_eq!(lost, 250);
    }

    #[test]
    fn test_score_non_break_great() {
        let (earned, lost) = score_non_break(500, JudgeGrade::LateGreat, 1);
        assert_eq!(earned, 400);
        assert_eq!(lost, 100);
    }

    #[test]
    fn test_score_break_perfect() {
        let (base, extra, classic, base_lost, extra_lost, classic_lost) =
            score_break(JudgeGrade::Perfect, 1);
        assert_eq!(base, 2500);
        assert_eq!(extra, 100);
        assert_eq!(classic, 100);
        assert_eq!(base_lost, 0);
        assert_eq!(extra_lost, 0);
        assert_eq!(classic_lost, 0);
    }

    #[test]
    fn test_dx_score_rank() {
        assert_eq!(dx_score_rank(0, 0), 0);
        assert_eq!(dx_score_rank(100, 100), 5); // 100% >= 97%
        assert_eq!(dx_score_rank(96, 100), 4);  // 96% >= 95%
        assert_eq!(dx_score_rank(94, 100), 3);  // 94% >= 93%
        assert_eq!(dx_score_rank(91, 100), 2);  // 91% >= 90%
        assert_eq!(dx_score_rank(86, 100), 1);  // 86% >= 85%
        assert_eq!(dx_score_rank(80, 100), 0);  // 80% < 85%
    }

    #[test]
    fn test_update_combo_perfect() {
        let delta = update_combo(10, 5, 3, 0, JudgeGrade::Perfect, 1);
        assert_eq!(delta.combo, 11);
        assert_eq!(delta.p_combo, 6);
        assert_eq!(delta.c_p_combo, 4);
        assert_eq!(delta.d_x_score_lost, 0);
    }

    #[test]
    fn test_update_combo_miss() {
        let delta = update_combo(10, 5, 3, 0, JudgeGrade::Miss, 1);
        assert_eq!(delta.combo, 0);
        assert_eq!(delta.p_combo, 0);
        assert_eq!(delta.c_p_combo, 0);
        assert_eq!(delta.d_x_score_lost, -3);
    }
}
