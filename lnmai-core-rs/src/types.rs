//! Core domain types for the LnMai game judgment engine.
//!
//! Pure data types for grades, notes, difficulty modes, lifecycle states,
//! score tracking, and events.

use serde::{Deserialize, Serialize};

use super::areas::{ButtonZone, SensorArea};

/// Typed runtime note/event positions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RuntimePos {
    ButtonZonePos(ButtonZone),
    SensorAreaPos(SensorArea),
}

impl RuntimePos {
    pub fn button_zone(&self) -> Option<ButtonZone> {
        match self {
            RuntimePos::ButtonZonePos(zone) => Some(*zone),
            _ => None,
        }
    }

    pub fn sensor_area(&self) -> Option<SensorArea> {
        match self {
            RuntimePos::SensorAreaPos(area) => Some(*area),
            _ => None,
        }
    }
}

impl From<ButtonZone> for RuntimePos {
    fn from(zone: ButtonZone) -> Self {
        RuntimePos::ButtonZonePos(zone)
    }
}

impl From<SensorArea> for RuntimePos {
    fn from(area: SensorArea) -> Self {
        RuntimePos::SensorAreaPos(area)
    }
}

/// Judgment Grades (15-tier lattice, ordered by quality ascending)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum JudgeGrade {
    Miss,
    LateGood,
    LateGreat3rd,
    LateGreat2nd,
    LateGreat,
    LatePerfect3rd,
    LatePerfect2nd,
    Perfect, // Critical Perfect: center of the timing window
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

    /// Distance from Critical Perfect (0 = Perfect, 7 = Miss/TooFast)
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

    pub fn is_great_grade(&self) -> bool {
        matches!(
            self,
            JudgeGrade::LateGreat
                | JudgeGrade::LateGreat2nd
                | JudgeGrade::LateGreat3rd
                | JudgeGrade::FastGreat
                | JudgeGrade::FastGreat2nd
                | JudgeGrade::FastGreat3rd
        )
    }

    pub fn is_good_grade(&self) -> bool {
        matches!(self, JudgeGrade::LateGood | JudgeGrade::FastGood)
    }
}

/// Note Type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

    pub fn extra_score(&self) -> u32 {
        match self {
            NoteType::Break => 100,
            _ => 0,
        }
    }
}

/// Slide Kind
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SlideKind {
    Single,
    Wifi,
    ConnPart,
}

/// Area Policy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AreaPolicy {
    Or,
    And,
}

/// Difficulty / Judgment Style
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JudgeStyle {
    Default,
    Maji,
    Gachi,
    Gori,
}

/// Note Lifecycle Status
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum NoteStatus {
    Start,
    Inited,
    Scaling,
    Running,
    Arrived,
    End,
}

impl NoteStatus {
    pub fn leq(&self, other: &NoteStatus) -> bool {
        match (self, other) {
            (NoteStatus::Start, _) => true,
            (NoteStatus::Inited, NoteStatus::Start) => false,
            (NoteStatus::Inited, _) => true,
            (NoteStatus::Scaling, NoteStatus::Start | NoteStatus::Inited) => false,
            (NoteStatus::Scaling, _) => true,
            (NoteStatus::Running, NoteStatus::Start | NoteStatus::Inited | NoteStatus::Scaling) => false,
            (NoteStatus::Running, _) => true,
            (NoteStatus::Arrived, NoteStatus::End) => true,
            (NoteStatus::Arrived, _) => false,
            (NoteStatus::End, NoteStatus::End) => true,
            (NoteStatus::End, _) => false,
        }
    }
}

/// Combo State
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ComboState {
    None,
    FC,
    FCPlus,
    AP,
    APPlus,
}

/// A single note's judgment result
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct NoteJudgeResult {
    pub grade: JudgeGrade,
    pub diff: i64, // Duration in microseconds
    pub is_break: bool,
    pub is_ex: bool,
}

impl NoteJudgeResult {
    pub fn is_fast(&self) -> bool {
        self.diff < 0
    }

    pub fn is_miss_or_too_fast(&self) -> bool {
        self.grade.is_miss_or_too_fast()
    }
}

/// Group state for touch notes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct GroupState {
    pub group_id: u32,
    pub count: u32,
    pub size: u32,
    pub grade: JudgeGrade,
    pub diff: i64, // Duration in microseconds
}

/// Per-note-type judge counts
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct NoteTypeJudgeCounts {
    pub tap_count: u32,
    pub hold_count: u32,
    pub slide_count: u32,
    pub touch_count: u32,
    pub break_count: u32,
}

impl Default for NoteTypeJudgeCounts {
    fn default() -> Self {
        Self {
            tap_count: 0,
            hold_count: 0,
            slide_count: 0,
            touch_count: 0,
            break_count: 0,
        }
    }
}

/// Score accumulation state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScoreState {
    pub combo: u32,
    pub p_combo: u32,
    pub c_p_combo: u32,
    pub total_base: u32,
    pub total_extra: u32,
    pub earned_base: u32,
    pub earned_extra: u32,
    pub lost_base: u32,
    pub lost_extra: u32,
    pub dx_score: i32,
    pub max_dx_score: u32,
    pub fast_count: u32,
    pub late_count: u32,
    pub counts: NoteTypeJudgeCounts,
}

impl Default for ScoreState {
    fn default() -> Self {
        Self {
            combo: 0,
            p_combo: 0,
            c_p_combo: 0,
            total_base: 0,
            total_extra: 0,
            earned_base: 0,
            earned_extra: 0,
            lost_base: 0,
            lost_extra: 0,
            dx_score: 0,
            max_dx_score: 0,
            fast_count: 0,
            late_count: 0,
            counts: NoteTypeJudgeCounts::default(),
        }
    }
}

/// Combo display result
pub fn combo_state(s: &ScoreState) -> ComboState {
    if s.combo == 0 {
        ComboState::None
    } else if s.p_combo == s.combo && s.c_p_combo == s.combo {
        ComboState::APPlus
    } else if s.p_combo == s.combo {
        ComboState::AP
    } else if s.combo == s.p_combo {
        ComboState::FCPlus
    } else {
        ComboState::FC
    }
}

/// Judge Event (emitted by Core → consumed by host for rendering)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JudgeEventKind {
    Tap,
    Hold,
    Slide,
    Touch,
    Break,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct JudgeEvent {
    pub kind: JudgeEventKind,
    pub grade: JudgeGrade,
    pub diff: i64, // Duration in microseconds
    pub position: RuntimePos,
    pub note_index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AudioCommand {
    PlayJudgeSfx {
        kind: JudgeEventKind,
        grade: JudgeGrade,
        at_time: i64, // TimePoint in microseconds
        note_index: u32,
    },
    PlaySlideCue {
        note_index: u32,
        track_index: u32,
        at_time: i64, // TimePoint in microseconds
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RenderCommand {
    ShowJudgeResult {
        kind: JudgeEventKind,
        grade: JudgeGrade,
        diff: i64, // Duration in microseconds
        note_index: u32,
    },
    UpdateSlideProgress {
        note_index: u32,
        remaining: u32,
    },
    UpdateSlideTrackProgress {
        note_index: u32,
        track_index: u32,
        remaining: u32,
    },
    HideAllSlideBars {
        note_index: u32,
    },
    HideSlideBars {
        note_index: u32,
        end_index: u32,
    },
    HideSlideTrackBars {
        note_index: u32,
        track_index: u32,
        end_index: u32,
    },
}
