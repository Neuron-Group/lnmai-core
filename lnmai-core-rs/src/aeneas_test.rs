//! Minimal Aeneas-compatible test module
//!
//! This module contains only the pure functional core that Aeneas can translate.
//! No String, no iterators, no complex trait objects.

/// Simple grade enum
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Grade {
    Miss,
    Good,
    Great,
    Perfect,
}

/// Simple note type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteType {
    Tap,
    Hold,
    Slide,
}

/// Grade distance from Perfect
pub fn grade_dist(g: Grade) -> u32 {
    match g {
        Grade::Perfect => 0,
        Grade::Great => 1,
        Grade::Good => 2,
        Grade::Miss => 3,
    }
}

/// Base score for note type
pub fn base_score(nt: NoteType) -> u32 {
    match nt {
        NoteType::Tap => 500,
        NoteType::Hold => 1000,
        NoteType::Slide => 1500,
    }
}

/// Score computation
pub fn compute_score(base: u32, grade: Grade) -> (u32, u32) {
    match grade {
        Grade::Perfect => (base, 0),
        Grade::Great => (base * 4 / 5, base - base * 4 / 5),
        Grade::Good => (base / 2, base - base / 2),
        Grade::Miss => (0, base),
    }
}

/// Duration type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration {
    pub micros: i64,
}

impl Duration {
    pub fn from_micros(micros: i64) -> Self {
        Self { micros }
    }

    pub fn to_micros(&self) -> i64 {
        self.micros
    }
}

/// Time point type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TimePoint {
    pub micros: i64,
}

impl TimePoint {
    pub fn from_micros(micros: i64) -> Self {
        Self { micros }
    }

    pub fn to_micros(&self) -> i64 {
        self.micros
    }
}

/// Judge a tap note
pub fn judge_tap(diff: Duration) -> Grade {
    let abs_diff = if diff.micros < 0 { -diff.micros } else { diff.micros };
    if abs_diff <= 16667 {
        Grade::Perfect
    } else if abs_diff <= 50001 {
        Grade::Great
    } else if abs_diff <= 150003 {
        Grade::Good
    } else {
        Grade::Miss
    }
}

/// Score state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScoreState {
    pub combo: u32,
    pub perfect_combo: u32,
    pub total_score: u32,
}

impl ScoreState {
    pub fn new() -> Self {
        Self {
            combo: 0,
            perfect_combo: 0,
            total_score: 0,
        }
    }
}

/// Update score state
pub fn update_score(state: &ScoreState, base: u32, grade: Grade) -> ScoreState {
    let (earned, _lost) = compute_score(base, grade);
    ScoreState {
        combo: if grade == Grade::Miss { 0 } else { state.combo + 1 },
        perfect_combo: if grade == Grade::Perfect { state.perfect_combo + 1 } else { 0 },
        total_score: state.total_score + earned,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grade_dist() {
        assert_eq!(grade_dist(Grade::Perfect), 0);
        assert_eq!(grade_dist(Grade::Great), 1);
        assert_eq!(grade_dist(Grade::Good), 2);
        assert_eq!(grade_dist(Grade::Miss), 3);
    }

    #[test]
    fn test_base_score() {
        assert_eq!(base_score(NoteType::Tap), 500);
        assert_eq!(base_score(NoteType::Hold), 1000);
        assert_eq!(base_score(NoteType::Slide), 1500);
    }

    #[test]
    fn test_compute_score() {
        assert_eq!(compute_score(500, Grade::Perfect), (500, 0));
        assert_eq!(compute_score(500, Grade::Miss), (0, 500));
    }

    #[test]
    fn test_judge_tap() {
        assert_eq!(judge_tap(Duration::from_micros(0)), Grade::Perfect);
        assert_eq!(judge_tap(Duration::from_micros(10000)), Grade::Perfect);
        assert_eq!(judge_tap(Duration::from_micros(30000)), Grade::Great);
        assert_eq!(judge_tap(Duration::from_micros(100000)), Grade::Good);
        assert_eq!(judge_tap(Duration::from_micros(200000)), Grade::Miss);
    }

    #[test]
    fn test_update_score() {
        let state = ScoreState::new();
        let new_state = update_score(&state, 500, Grade::Perfect);
        assert_eq!(new_state.combo, 1);
        assert_eq!(new_state.perfect_combo, 1);
        assert_eq!(new_state.total_score, 500);
    }
}
