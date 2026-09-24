//! Normalized chart IR.
//!
//! Mirrors `LnmaiCore/Simai/IR.lean`.

use crate::areas::{OuterSlot, SensorArea};
use crate::chart_loader::ChartSpec;
use crate::rat::Rat;
use crate::simai::slide_tables::SlideAreaSpec;
use crate::simai::syntax::{
    MaidataChartBlock, MaidataMetadata, RawNoteToken, SlideNoteSemantics, SlideShape, SourceChart,
};
use crate::time::{Duration, TimePoint};
use crate::types::SlideKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedSlideDebug {
    pub note_index: usize,
    pub raw_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedTap {
    pub timing: TimePoint,
    pub slot: OuterSlot,
    pub is_break: bool,
    pub is_ex: bool,
    pub is_hanabi: bool,
    pub is_force_star: bool,
    pub note_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedHold {
    pub timing: TimePoint,
    pub slot: OuterSlot,
    pub length: Duration,
    pub is_break: bool,
    pub is_ex: bool,
    pub is_hanabi: bool,
    pub note_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedTouchHold {
    pub timing: TimePoint,
    pub sensor_pos: SensorArea,
    pub length: Duration,
    pub is_break: bool,
    pub is_ex: bool,
    pub is_hanabi: bool,
    pub source_group_id: Option<usize>,
    pub source_group_index: Option<usize>,
    pub source_group_size: Option<usize>,
    pub note_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedTouch {
    pub timing: TimePoint,
    pub sensor_pos: SensorArea,
    pub is_break: bool,
    pub is_hanabi: bool,
    pub source_group_id: Option<usize>,
    pub source_group_index: Option<usize>,
    pub source_group_size: Option<usize>,
    pub note_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedSlide {
    pub head_timing: TimePoint,
    pub slot: OuterSlot,
    pub length: Duration,
    pub start_timing: TimePoint,
    pub group_start_timing: Option<TimePoint>,
    pub h_speed: Rat,
    pub slide_kind: SlideKind,
    pub is_classic: bool,
    pub track_count: usize,
    pub judge_at: Option<TimePoint>,
    pub is_break: bool,
    pub is_ex: bool,
    pub is_hanabi: bool,
    pub has_head_note: bool,
    pub has_body: bool,
    pub is_slide_no_head: bool,
    pub is_force_star: bool,
    pub is_fake_rotate: bool,
    pub is_slide_break: bool,
    pub is_conn_slide: bool,
    pub parent_note_index: Option<usize>,
    pub is_group_head: bool,
    pub is_group_end: bool,
    pub total_judge_queue_len: usize,
    pub judge_queues: Vec<Vec<SlideAreaSpec>>,
    pub source_group_id: Option<usize>,
    pub source_group_index: Option<usize>,
    pub source_group_size: Option<usize>,
    pub multiple: usize,
    pub note_index: usize,
    pub simai_shape: SlideShape,
}

impl Default for NormalizedSlide {
    fn default() -> Self {
        NormalizedSlide {
            head_timing: TimePoint::zero(),
            slot: OuterSlot::S1,
            length: Duration::zero(),
            start_timing: TimePoint::zero(),
            group_start_timing: None,
            h_speed: Rat::one(),
            slide_kind: SlideKind::Single,
            is_classic: false,
            track_count: 1,
            judge_at: None,
            is_break: false,
            is_ex: false,
            is_hanabi: false,
            has_head_note: true,
            has_body: true,
            is_slide_no_head: false,
            is_force_star: false,
            is_fake_rotate: false,
            is_slide_break: false,
            is_conn_slide: false,
            parent_note_index: None,
            is_group_head: false,
            is_group_end: false,
            total_judge_queue_len: 0,
            judge_queues: Vec::new(),
            source_group_id: None,
            source_group_index: None,
            source_group_size: None,
            multiple: 1,
            note_index: 0,
            simai_shape: SlideShape::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedChart {
    pub taps: Vec<NormalizedTap>,
    pub holds: Vec<NormalizedHold>,
    pub touches: Vec<NormalizedTouch>,
    pub touch_holds: Vec<NormalizedTouchHold>,
    pub slides: Vec<NormalizedSlide>,
    pub slide_debug: Vec<NormalizedSlideDebug>,
    pub slide_skipping: bool,
}

impl Default for NormalizedChart {
    fn default() -> Self {
        NormalizedChart {
            taps: Vec::new(),
            holds: Vec::new(),
            touches: Vec::new(),
            touch_holds: Vec::new(),
            slides: Vec::new(),
            slide_debug: Vec::new(),
            slide_skipping: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontendChartInspection {
    pub metadata: MaidataMetadata,
    pub chart: MaidataChartBlock,
    pub source: SourceChart,
    pub tokens: Vec<RawNoteToken>,
    pub slide_notes: Vec<SlideNoteSemantics>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontendSemanticChart {
    pub normalized: NormalizedChart,
    pub lowered: ChartSpec,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontendChartResult {
    pub semantic: FrontendSemanticChart,
    pub inspection: FrontendChartInspection,
}

impl Default for NormalizedSlideDebug {
    fn default() -> Self {
        NormalizedSlideDebug { note_index: 0, raw_text: String::new() }
    }
}

impl Default for NormalizedTap {
    fn default() -> Self {
        NormalizedTap {
            timing: TimePoint::zero(),
            slot: OuterSlot::S1,
            is_break: false,
            is_ex: false,
            is_hanabi: false,
            is_force_star: false,
            note_index: 0,
        }
    }
}

impl Default for NormalizedHold {
    fn default() -> Self {
        NormalizedHold {
            timing: TimePoint::zero(),
            slot: OuterSlot::S1,
            length: Duration::zero(),
            is_break: false,
            is_ex: false,
            is_hanabi: false,
            note_index: 0,
        }
    }
}

impl Default for NormalizedTouchHold {
    fn default() -> Self {
        NormalizedTouchHold {
            timing: TimePoint::zero(),
            sensor_pos: SensorArea::C,
            length: Duration::zero(),
            is_break: false,
            is_ex: false,
            is_hanabi: false,
            source_group_id: None,
            source_group_index: None,
            source_group_size: None,
            note_index: 0,
        }
    }
}

impl Default for NormalizedTouch {
    fn default() -> Self {
        NormalizedTouch {
            timing: TimePoint::zero(),
            sensor_pos: SensorArea::C,
            is_break: false,
            is_hanabi: false,
            source_group_id: None,
            source_group_index: None,
            source_group_size: None,
            note_index: 0,
        }
    }
}
