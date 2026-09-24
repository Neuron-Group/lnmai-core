//! Source/IR data types for the Simai pipeline.
//!
//! Mirrors `LnmaiCore/Simai/Syntax.lean`. JSON instances are not ported here
//! (they live at the FFI boundary, M4); the focus is the pure data model.

use crate::areas::{OuterSlot, SensorArea};
use crate::rat::Rat;
use crate::symmetry::SlideSymmetry;
use crate::time::{Duration, TimePoint};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SourceSpan {
    pub start: SourcePos,
    pub stop: SourcePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    InvalidSyntax,
    InvalidShape,
    InvalidEndPosition,
    InvalidTurnPosition,
    InvalidChainTiming,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub raw_text: String,
    pub message: String,
    pub span: Option<SourceSpan>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideKind {
    Line,
    Circle,
    V,
    Turn,
    Pq,
    Ppqq,
    S,
    Wifi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalSlideShape {
    Line(usize),
    Circle(usize),
    V(usize),
    Turn(usize),
    Pq(usize),
    Ppqq(usize),
    S,
    Wifi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SlideShape {
    pub canonical: CanonicalSlideShape,
    pub symmetry: SlideSymmetry,
}

impl Default for SlideShape {
    fn default() -> Self {
        SlideShape { canonical: CanonicalSlideShape::S, symmetry: crate::symmetry::direct() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideBodyKind {
    Line,
    CircleRight,
    CircleLeft,
    CircleUp,
    V,
    Pp,
    Qq,
    P,
    Q,
    S,
    Z,
    Turn,
    Wifi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedSlideBody {
    pub raw_text: String,
    pub start_lane: OuterSlot,
    pub kind: SlideBodyKind,
    pub end_area: Option<SensorArea>,
    pub turn_area: Option<SensorArea>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SlideNoteSemantics {
    pub raw_text: String,
    pub start_slot: OuterSlot,
    pub end_area: SensorArea,
    pub shape: SlideShape,
    pub is_just_right: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimingPointSemantics {
    pub timing: TimePoint,
    pub bpm: Rat,
    pub h_speed: Rat,
    pub notes: Vec<SlideNoteSemantics>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SimaiChartSemantics {
    pub timing_points: Vec<TimingPointSemantics>,
    pub comma_timings: Vec<TimePoint>,
    pub title: String,
    pub designer: String,
    pub level: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RawNoteKind {
    Tap,
    Hold,
    Slide,
    Touch,
    TouchHold,
    Rest,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MaidataMetadata {
    pub fields: Vec<(String, String)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MaidataChartBlock {
    pub level_index: usize,
    pub raw_body: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MaidataFile {
    pub metadata: MaidataMetadata,
    pub charts: Vec<MaidataChartBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawNoteToken {
    pub raw_text: String,
    pub kind: RawNoteKind,
    pub timing: TimePoint,
    pub bpm: Rat,
    pub h_speed: Rat,
    pub divisor: usize,
    pub slot: Option<OuterSlot>,
    pub sensor_pos: Option<SensorArea>,
    pub slide_body: Option<ParsedSlideBody>,
    pub length: Option<Duration>,
    pub star_wait: Option<Duration>,
    pub is_break: bool,
    pub is_ex: bool,
    pub is_hanabi: bool,
    pub is_slide_no_head: bool,
    pub is_force_star: bool,
    pub is_fake_rotate: bool,
    pub is_slide_break: bool,
    pub source_group_id: Option<usize>,
    pub source_group_index: Option<usize>,
    pub source_group_size: Option<usize>,
    pub source_pos: Option<SourceSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceNote {
    pub token: RawNoteToken,
    pub source_pos: Option<SourceSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceEvent {
    pub timing: TimePoint,
    pub bpm: Rat,
    pub h_speed: Rat,
    pub divisor: usize,
    pub notes: Vec<SourceNote>,
    pub source_pos: Option<SourceSpan>,
}

impl Default for SourceEvent {
    fn default() -> Self {
        SourceEvent {
            timing: TimePoint::zero(),
            bpm: Rat::zero(),
            h_speed: Rat::one(),
            divisor: 4,
            notes: Vec::new(),
            source_pos: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SourceChart {
    pub events: Vec<SourceEvent>,
}
