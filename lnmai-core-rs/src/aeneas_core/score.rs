//! Aeneas-friendly score module
//!
//! Score computation functions

use super::types::{JudgeGrade, NoteType, ScoreState};

/// Base score for note type
pub fn base_score(nt: NoteType) -> u32 {
    match nt {
        NoteType::Tap => 500,
        NoteType::Hold => 1000,
        NoteType::Slide => 1500,
        NoteType::Touch => 500,
        NoteType::Break => 2500,
    }
}

/// Score a non-break note
pub fn score_non_break(base: u32, grade: JudgeGrade, multiple: u32) -> (u32, u32) {
    let b = base * multiple;
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

/// Combo delta
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ComboDelta {
    pub combo: u32,
    pub p_combo: u32,
    pub c_p_combo: u32,
    pub dx_score_lost: i32,
}

/// Update combo state
pub fn update_combo(
    combo: u32,
    p_combo: u32,
    c_p_combo: u32,
    dx_score_lost: i32,
    grade: JudgeGrade,
    multiple: u32,
) -> ComboDelta {
    let m = multiple;
    match grade {
        JudgeGrade::Perfect => ComboDelta {
            combo: combo + m,
            p_combo: p_combo + m,
            c_p_combo: c_p_combo + m,
            dx_score_lost,
        },
        JudgeGrade::LatePerfect2nd
        | JudgeGrade::FastPerfect2nd
        | JudgeGrade::LatePerfect3rd
        | JudgeGrade::FastPerfect3rd => ComboDelta {
            combo: combo + m,
            p_combo: p_combo + m,
            c_p_combo: 0,
            dx_score_lost: dx_score_lost - (1 * m) as i32,
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
            dx_score_lost: dx_score_lost - (2 * m) as i32,
        },
        JudgeGrade::LateGood | JudgeGrade::FastGood => ComboDelta {
            combo: combo + m,
            p_combo: 0,
            c_p_combo: 0,
            dx_score_lost: dx_score_lost - (3 * m) as i32,
        },
        JudgeGrade::Miss | JudgeGrade::TooFast => ComboDelta {
            combo: 0,
            p_combo: 0,
            c_p_combo: 0,
            dx_score_lost: dx_score_lost - (3 * m) as i32,
        },
    }
}

/// DX score rank
pub fn dx_score_rank(achieved: u32, max: u32) -> u32 {
    if max == 0 {
        return 0;
    }
    let meets = |threshold: u32| -> bool {
        achieved * 100 >= max * threshold
    };
    if meets(97) { 5 }
    else if meets(95) { 4 }
    else if meets(93) { 3 }
    else if meets(90) { 2 }
    else if meets(85) { 1 }
    else { 0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base_score() {
        assert_eq!(base_score(NoteType::Tap), 500);
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
    fn test_dx_score_rank() {
        assert_eq!(dx_score_rank(100, 100), 5);
        assert_eq!(dx_score_rank(80, 100), 0);
    }
}
