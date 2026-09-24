//! Aeneas-friendly types module
//!
//! Design decisions:
//! - No serde derives
//! - No String usage
//! - Simple enums and structs

use super::areas::{SensorArea, ButtonZone, OuterSlot};

/// Judgment grades (15-tier)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum JudgeGrade {
    Miss,
    LateGood,
    LateGreat3rd,
    LateGreat2nd,
    LateGreat,
    LatePerfect3rd,
    LatePerfect2nd,
    Perfect,
    FastPerfect2nd,
    FastPerfect3rd,
    FastGreat,
    FastGreat2nd,
    FastGreat3rd,
    FastGood,
    TooFast,
}

impl JudgeGrade {
    pub fn is_miss_or_too_fast(&self) -> bool {
        matches!(self, JudgeGrade::Miss | JudgeGrade::TooFast)
    }

    pub fn is_fast(&self) -> bool {
        matches!(
            self,
            JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd
                | JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
                | JudgeGrade::FastGood
                | JudgeGrade::TooFast
        )
    }

    pub fn is_late(&self) -> bool {
        matches!(
            self,
            JudgeGrade::LateGood
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat
                | JudgeGrade::LatePerfect3rd
                | JudgeGrade::LatePerfect2nd
                | JudgeGrade::Miss
        )
    }

    pub fn dist_from_perfect(&self) -> u32 {
        match self {
            JudgeGrade::Miss => 7,
            JudgeGrade::LateGood => 6,
            JudgeGrade::LateGreat3rd => 5,
            JudgeGrade::LateGreat2nd => 4,
            JudgeGrade::LateGreat => 3,
            JudgeGrade::LatePerfect3rd => 2,
            JudgeGrade::LatePerfect2nd => 1,
            JudgeGrade::Perfect => 0,
            JudgeGrade::FastPerfect2nd => 1,
            JudgeGrade::FastPerfect3rd => 2,
            JudgeGrade::FastGreat => 3,
            JudgeGrade::FastGreat2nd => 4,
            JudgeGrade::FastGreat3rd => 5,
            JudgeGrade::FastGood => 6,
            JudgeGrade::TooFast => 7,
        }
    }

    pub fn is_perfect_grade(&self) -> bool {
        matches!(
            self,
            JudgeGrade::Perfect
                | JudgeGrade::LatePerfect2nd
                | JudgeGrade::LatePerfect3rd
                | JudgeGrade::FastPerfect2nd
                | JudgeGrade::FastPerfect3rd
        )
    }
}

/// Note types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteType {
    Tap,
    Hold,
    Slide,
    Touch,
    Break,
}

impl NoteType {
    pub fn base_score(&self) -> u32 {
        match self {
            NoteType::Tap => 500,
            NoteType::Hold => 1000,
            NoteType::Slide => 1500,
            NoteType::Touch => 500,
            NoteType::Break => 2500,
        }
    }
}

/// Judge styles
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JudgeStyle {
    Default,
    Maji,
    Gachi,
    Gori,
}

/// Slide kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideKind {
    Single,
    Wifi,
    ConnPart,
}

/// Area policy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AreaPolicy {
    Or,
    And,
}

/// Runtime position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimePos {
    ButtonZonePos(ButtonZone),
    SensorAreaPos(SensorArea),
}

/// Judge event
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JudgeEvent {
    pub kind: JudgeEventKind,
    pub grade: JudgeGrade,
    pub diff: i64,
    pub position: RuntimePos,
    pub note_index: u32,
}

/// Judge event kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JudgeEventKind {
    Tap,
    Hold,
    Slide,
    Touch,
    Break,
}

/// Score state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScoreState {
    pub combo: u32,
    pub p_combo: u32,
    pub c_p_combo: u32,
    pub total_base: u32,
    pub earned_base: u32,
    pub lost_base: u32,
}

impl Default for ScoreState {
    fn default() -> Self {
        Self {
            combo: 0,
            p_combo: 0,
            c_p_combo: 0,
            total_base: 0,
            earned_base: 0,
            lost_base: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sensor_area_roundtrip() {
        for area in SensorArea::ALL {
            let index = area.to_index();
            let recovered = SensorArea::from_index(index).unwrap();
            assert_eq!(*area, recovered);
        }
    }

    #[test]
    fn test_button_zone_roundtrip() {
        for zone in ButtonZone::ALL {
            let index = zone.to_index();
            let recovered = ButtonZone::from_index(index).unwrap();
            assert_eq!(*zone, recovered);
        }
    }

    #[test]
    fn test_outer_slot_to_button_zone() {
        for slot in OuterSlot::ALL {
            let zone = slot.to_button_zone();
            assert_eq!(slot.to_index(), zone.to_index());
        }
    }
}
