//! Declarative chart-to-runtime loader.
//!
//! Converts ChartSpec JSON to runtime GameState.

use serde::{Deserialize, Serialize};

use super::areas::*;
use super::input_model::*;
use super::lifecycle::*;
use super::types::*;

/// Tap chart note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TapChartNote {
    pub timing: i64, // TimePoint in microseconds
    pub slot: OuterSlot,
    pub is_break: bool,
    pub is_ex: bool,
    pub button_queue_index: u32,
    pub note_index: u32,
}

/// Hold chart note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HoldChartNote {
    pub timing: i64, // TimePoint in microseconds
    pub slot: OuterSlot,
    pub length: i64, // Duration in microseconds
    pub is_break: bool,
    pub is_ex: bool,
    pub is_touch: bool,
    pub is_classic: bool,
    pub button_queue_index: u32,
    pub touch_hold_group_id: Option<u32>,
    pub touch_hold_group_size: Option<u32>,
    pub note_index: u32,
}

/// Touch hold chart note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TouchHoldChartNote {
    pub timing: i64, // TimePoint in microseconds
    pub sensor_pos: SensorArea,
    pub length: i64, // Duration in microseconds
    pub is_break: bool,
    pub is_ex: bool,
    pub touch_queue_index: u32,
    pub touch_group_id: Option<u32>,
    pub touch_group_size: Option<u32>,
    pub note_index: u32,
}

/// Touch chart note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TouchChartNote {
    pub timing: i64, // TimePoint in microseconds
    pub sensor_pos: SensorArea,
    pub is_break: bool,
    pub touch_queue_index: u32,
    pub touch_group_id: Option<u32>,
    pub touch_group_size: Option<u32>,
    pub note_index: u32,
}

/// Slide chart note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlideChartNote {
    pub timing: i64, // TimePoint in microseconds
    pub slot: OuterSlot,
    pub length: i64, // Duration in microseconds
    pub start_timing: i64, // TimePoint in microseconds
    pub slide_kind: SlideKind,
    pub is_classic: bool,
    pub is_conn_slide: bool,
    pub parent_index: Option<u32>,
    pub group_indices: Vec<u32>,
    pub track_count: u32,
    pub judge_at: Vec<i64>, // TimePoint in microseconds
    pub is_break: bool,
    pub is_ex: bool,
    pub note_index: u32,
    pub judge_queues: Vec<Vec<SlideArea>>,
    pub debug_simai: Option<String>,
}

/// Chart specification
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ChartSpec {
    pub taps: Vec<TapChartNote>,
    pub holds: Vec<HoldChartNote>,
    pub touches: Vec<TouchChartNote>,
    pub touch_holds: Vec<TouchHoldChartNote>,
    pub slides: Vec<SlideChartNote>,
    pub slide_skipping: bool,
}

impl Default for ChartSpec {
    fn default() -> Self {
        Self {
            taps: Vec::new(),
            holds: Vec::new(),
            touches: Vec::new(),
            touch_holds: Vec::new(),
            slides: Vec::new(),
            slide_skipping: false,
        }
    }
}

/// Build GameState from ChartSpec
pub fn build_game_state(chart: &ChartSpec) -> GameState {
    let mut state = GameState::default();

    // Build tap queues
    for tap in &chart.taps {
        let zone = tap.slot.to_button_zone();
        let note = TapFamilyNote::TapNote(TapNote {
            params: CommonNoteParams {
                judge_timing: tap.timing,
                judge_offset: 0,
                is_break: tap.is_break,
                is_ex: tap.is_ex,
                note_index: tap.note_index,
            },
            lane: tap.slot,
            state: TapState::Waiting,
            button_queue_index: tap.button_queue_index,
        });
        let queue = state.tap_queues.get_d(zone, ZoneQueue::default());
        let mut new_queue = queue.clone();
        new_queue.notes.push(note);
        state.tap_queues = state.tap_queues.set(zone, new_queue);
    }

    // Build hold queues
    for hold in &chart.holds {
        let zone = hold.slot.to_button_zone();
        let note = HoldNote {
            params: CommonNoteParams {
                judge_timing: hold.timing,
                judge_offset: 0,
                is_break: hold.is_break,
                is_ex: hold.is_ex,
                note_index: hold.note_index,
            },
            start: HoldStart::HoldButton(zone),
            state: HoldSubState::HeadWaiting,
            length: hold.length,
            head_diff: None,
            head_grade: None,
            player_release_time: None,
            is_classic: hold.is_classic,
            is_touch_hold: hold.is_touch,
            touch_group_id: hold.touch_hold_group_id,
            touch_group_size: hold.touch_hold_group_size,
            touch_group_count: None,
        };
        let queue = state.hold_queues.get_d(zone, ZoneQueue::default());
        let mut new_queue = queue.clone();
        new_queue.notes.push(note);
        state.hold_queues = state.hold_queues.set(zone, new_queue);
    }

    // Build touch queues
    for touch in &chart.touches {
        let area = touch.sensor_pos;
        let note = TouchNote {
            params: CommonNoteParams {
                judge_timing: touch.timing,
                judge_offset: 0,
                is_break: touch.is_break,
                is_ex: false,
                note_index: touch.note_index,
            },
            state: TouchState::TouchWaiting,
            sensor_pos: area,
            touch_group_id: touch.touch_group_id,
            touch_group_size: touch.touch_group_size,
            touch_group_count: None,
        };
        let queue = state.touch_queues.get_d(area, ZoneQueue::default());
        let mut new_queue = queue.clone();
        new_queue.notes.push(note);
        state.touch_queues = state.touch_queues.set(area, new_queue);
    }

    // Build touch hold queues
    for touch_hold in &chart.touch_holds {
        let area = touch_hold.sensor_pos;
        let note = HoldNote {
            params: CommonNoteParams {
                judge_timing: touch_hold.timing,
                judge_offset: 0,
                is_break: touch_hold.is_break,
                is_ex: touch_hold.is_ex,
                note_index: touch_hold.note_index,
            },
            start: HoldStart::HoldSensor(area),
            state: HoldSubState::HeadWaiting,
            length: touch_hold.length,
            head_diff: None,
            head_grade: None,
            player_release_time: None,
            is_classic: false,
            is_touch_hold: true,
            touch_group_id: touch_hold.touch_group_id,
            touch_group_size: touch_hold.touch_group_size,
            touch_group_count: None,
        };
        let queue = state.touch_hold_queues.get_d(area, ZoneQueue::default());
        let mut new_queue = queue.clone();
        new_queue.notes.push(note);
        state.touch_hold_queues = state.touch_hold_queues.set(area, new_queue);
    }

    // Build slides
    for slide in &chart.slides {
        let note = SlideNote {
            params: CommonNoteParams {
                judge_timing: slide.timing,
                judge_offset: 0,
                is_break: slide.is_break,
                is_ex: slide.is_ex,
                note_index: slide.note_index,
            },
            lane: slide.slot,
            state: SlideState::Waiting,
            length: slide.length,
            timing: slide.timing,
            start_timing: slide.start_timing,
            slide_kind: slide.slide_kind,
            is_classic: slide.is_classic,
            is_conn_slide: slide.is_conn_slide,
            parent_index: slide.parent_index,
            group_indices: slide.group_indices.clone(),
            judge_queues: slide.judge_queues.clone(),
        };
        state.slides.push(note);
    }

    state
}

/// Parse JSON to ChartSpec
pub fn parse_chart_json(json: &serde_json::Value) -> Result<ChartSpec, String> {
    serde_json::from_value(json.clone()).map_err(|e| e.to_string())
}

/// Parse JSON string to ChartSpec
pub fn parse_chart_json_string(s: &str) -> Result<ChartSpec, String> {
    serde_json::from_str(s).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_empty_game_state() {
        let chart = ChartSpec::default();
        let state = build_game_state(&chart);
        assert_eq!(state.slides.len(), 0);
    }
}
