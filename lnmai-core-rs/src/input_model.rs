//! Per-frame input model and per-zone note queues.
//!
//! Mirrors LnmaiCore/InputModel.lean.
//! Uses storage::ButtonVec and storage::SensorVec (Lean storage order).

use serde::{Deserialize, Serialize};

use super::areas::*;
use super::constants::*;
use super::lifecycle::*;
use super::storage::*;
use super::types::*;

/// Button vector (8 elements) — re-export from storage
pub use super::storage::ButtonVec;

/// Sensor vector (33 elements) — re-export from storage
pub use super::storage::SensorVec;

/// Frame input (read-only snapshot for one frame)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FrameInput {
    pub button_clicked: ButtonVec<bool>,
    pub button_held: ButtonVec<bool>,
    pub sensor_clicked: SensorVec<bool>,
    pub sensor_held: SensorVec<bool>,
    pub button_click_count: ButtonVec<u32>,
    pub sensor_click_count: SensorVec<u32>,
    pub delta: i64, // Duration in microseconds
}

impl Default for FrameInput {
    fn default() -> Self {
        Self {
            button_clicked: ButtonVec::replicate(false),
            button_held: ButtonVec::replicate(false),
            sensor_clicked: SensorVec::replicate(false),
            sensor_held: SensorVec::replicate(false),
            button_click_count: ButtonVec::replicate(0),
            sensor_click_count: SensorVec::replicate(0),
            delta: 0,
        }
    }
}

impl FrameInput {
    pub fn get_button_held(&self, zone: ButtonZone) -> bool {
        self.button_held.get_d(zone, false)
    }

    pub fn get_button_clicked(&self, zone: ButtonZone) -> bool {
        self.button_clicked.get_d(zone, false)
    }

    pub fn get_sensor_held(&self, area: SensorArea) -> bool {
        self.sensor_held.get_d(area, false)
    }

    pub fn get_sensor_clicked(&self, area: SensorArea) -> bool {
        self.sensor_clicked.get_d(area, false)
    }

    pub fn get_button_click_count(&self, zone: ButtonZone) -> u32 {
        let count = self.button_click_count.get_d(zone, 0);
        if count > 0 { count } else if self.get_button_clicked(zone) { 1 } else { 0 }
    }

    pub fn get_sensor_click_count(&self, area: SensorArea) -> u32 {
        let count = self.sensor_click_count.get_d(area, 0);
        if count > 0 { count } else if self.get_sensor_clicked(area) { 1 } else { 0 }
    }
}

/// Timed input event
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimedInputEvent {
    ButtonClick {
        tp: i64, // TimePoint in microseconds
        zone: ButtonZone,
    },
    ButtonHold {
        tp: i64, // TimePoint in microseconds
        zone: ButtonZone,
        is_down: bool,
    },
    SensorClick {
        tp: i64, // TimePoint in microseconds
        area: SensorArea,
    },
    SensorHold {
        tp: i64, // TimePoint in microseconds
        area: SensorArea,
        is_down: bool,
    },
}

impl TimedInputEvent {
    pub fn at(&self) -> i64 {
        match self {
            TimedInputEvent::ButtonClick { tp, .. } => *tp,
            TimedInputEvent::ButtonHold { tp, .. } => *tp,
            TimedInputEvent::SensorClick { tp, .. } => *tp,
            TimedInputEvent::SensorHold { tp, .. } => *tp,
        }
    }
}

/// Timed input batch
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TimedInputBatch {
    pub current_time: i64, // TimePoint in microseconds
    pub events: Vec<TimedInputEvent>,
}

impl Default for TimedInputBatch {
    fn default() -> Self {
        Self {
            current_time: 0,
            events: Vec::new(),
        }
    }
}

/// Frame window
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FrameWindow {
    pub prev_time: i64, // TimePoint in microseconds
    pub current_time: i64, // TimePoint in microseconds
}

impl FrameWindow {
    pub fn of_delta(current_time: i64, delta: i64) -> Self {
        Self {
            prev_time: current_time - delta,
            current_time,
        }
    }

    /// Frame inclusion policy for timed inputs.
    ///
    /// - zero-duration frame: exact-point inclusion `{currentTime}`
    /// - positive-duration frame: left-open, right-closed `(prevTime, currentTime]`
    pub fn contains_event_time(&self, event_time: i64) -> bool {
        if self.prev_time == self.current_time {
            event_time == self.current_time
        } else {
            event_time > self.prev_time && event_time <= self.current_time
        }
    }
}

impl TimedInputBatch {
    /// Convert a TimedInputBatch into a FrameInput by folding events within the frame window.
    ///
    /// Mirrors LnmaiCore.InputModel.TimedInputBatch.toFrameInput.
    pub fn to_frame_input(
        &self,
        delta: i64,
        prev_button_held: &ButtonVec<bool>,
        prev_sensor_held: &SensorVec<bool>,
    ) -> FrameInput {
        let window = FrameWindow::of_delta(self.current_time, delta);
        let within_frame = |evt: &TimedInputEvent| -> bool {
            window.contains_event_time(evt.at())
        };

        let mut fi = FrameInput {
            button_held: prev_button_held.clone(),
            sensor_held: prev_sensor_held.clone(),
            delta,
            ..Default::default()
        };

        for evt in &self.events {
            match evt {
                TimedInputEvent::ButtonClick { tp: _, zone } => {
                    if within_frame(evt) {
                        fi.button_clicked = fi.button_clicked.set(*zone, true);
                        let count = fi.button_click_count.get_d(*zone, 0);
                        fi.button_click_count = fi.button_click_count.set(*zone, count + 1);
                    }
                }
                TimedInputEvent::SensorClick { tp: _, area } => {
                    if within_frame(evt) {
                        fi.sensor_clicked = fi.sensor_clicked.set(*area, true);
                        let count = fi.sensor_click_count.get_d(*area, 0);
                        fi.sensor_click_count = fi.sensor_click_count.set(*area, count + 1);
                    }
                }
                TimedInputEvent::ButtonHold { tp: _, zone, is_down } => {
                    fi.button_held = fi.button_held.set(*zone, *is_down);
                }
                TimedInputEvent::SensorHold { tp: _, area, is_down } => {
                    fi.sensor_held = fi.sensor_held.set(*area, *is_down);
                }
            }
        }

        fi
    }
}

/// Per-zone note queue
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ZoneQueue<T> {
    pub notes: Vec<T>,
    pub current_index: u32,
}

impl<T> Default for ZoneQueue<T> {
    fn default() -> Self {
        Self {
            notes: Vec::new(),
            current_index: 0,
        }
    }
}

impl<T: Clone> ZoneQueue<T> {
    pub fn is_empty(&self) -> bool {
        self.current_index as usize >= self.notes.len()
    }

    pub fn peek(&self) -> Option<&T> {
        self.notes.get(self.current_index as usize)
    }

    pub fn advance(&mut self) {
        self.current_index += 1;
    }

    /// Replace the current note and advance
    pub fn replace_and_advance(&mut self, note: T) {
        self.notes[self.current_index as usize] = note;
        self.current_index += 1;
    }

    /// Replace the current note without advancing
    pub fn replace_current(&mut self, note: T) {
        self.notes[self.current_index as usize] = note;
    }
}

/// Button queue vector
pub type ButtonQueueVec<T> = ButtonVec<ZoneQueue<T>>;

/// Sensor queue vector
pub type SensorQueueVec<T> = SensorVec<ZoneQueue<T>>;

/// Game state (held across frames)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GameState {
    pub current_time: i64, // TimePoint in microseconds
    pub prev_button: ButtonVec<bool>,
    pub prev_sensor: SensorVec<bool>,
    pub button_queue_frontiers: ButtonVec<u32>,
    pub touch_queue_frontiers: SensorVec<u32>,
    pub tap_queues: ButtonQueueVec<TapFamilyNote>,
    pub hold_queues: ButtonQueueVec<HoldNote>,
    pub touch_hold_queues: SensorQueueVec<HoldNote>,
    pub touch_queues: SensorQueueVec<TouchNote>,
    pub slides: Vec<SlideNote>,
    pub active_holds: Vec<(ButtonZone, HoldNote)>,
    pub active_touch_holds: Vec<(SensorArea, HoldNote)>,
    pub touch_group_states: Vec<GroupState>,
    pub touch_hold_group_states: Vec<GroupState>,
    pub current_batch: Option<TimedInputBatch>,
    pub score: ScoreState,
    pub judge_style: JudgeStyle,
    pub touch_panel_offset: i64, // Duration in microseconds
    pub use_button_ring_for_touch: bool,
    pub subdivide_slide_judge_grade: bool,
}

impl Default for GameState {
    fn default() -> Self {
        Self {
            current_time: 0,
            prev_button: ButtonVec::replicate(false),
            prev_sensor: SensorVec::replicate(false),
            button_queue_frontiers: ButtonVec::replicate(0),
            touch_queue_frontiers: SensorVec::replicate(0),
            tap_queues: ButtonVec::replicate(ZoneQueue::default()),
            hold_queues: ButtonVec::replicate(ZoneQueue::default()),
            touch_hold_queues: SensorVec::replicate(ZoneQueue::default()),
            touch_queues: SensorVec::replicate(ZoneQueue::default()),
            slides: Vec::new(),
            active_holds: Vec::new(),
            active_touch_holds: Vec::new(),
            touch_group_states: Vec::new(),
            touch_hold_group_states: Vec::new(),
            current_batch: None,
            score: ScoreState::default(),
            judge_style: JudgeStyle::Default,
            touch_panel_offset: 0,
            use_button_ring_for_touch: USE_BUTTON_RING_FOR_TOUCH,
            subdivide_slide_judge_grade: SUBDIVIDE_SLIDE_JUDGE_GRADE,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_button_vec_get_set() {
        let mut vec = ButtonVec::replicate(false);
        assert_eq!(vec.get_d(ButtonZone::K1, true), false);
        vec = vec.set(ButtonZone::K1, true);
        assert_eq!(vec.get_d(ButtonZone::K1, false), true);
        assert_eq!(vec.get_d(ButtonZone::K2, false), false);
    }

    #[test]
    fn test_sensor_vec_get_set() {
        let mut vec = SensorVec::replicate(false);
        assert_eq!(vec.get_d(SensorArea::A1, true), false);
        vec = vec.set(SensorArea::A1, true);
        assert_eq!(vec.get_d(SensorArea::A1, false), true);
        assert_eq!(vec.get_d(SensorArea::A2, false), false);
    }

    #[test]
    fn test_zone_queue() {
        let mut queue = ZoneQueue {
            notes: vec![1, 2, 3],
            current_index: 0,
        };
        assert_eq!(queue.peek(), Some(&1));
        assert!(!queue.is_empty());

        queue.advance();
        assert_eq!(queue.peek(), Some(&2));
        assert!(!queue.is_empty());
    }

    #[test]
    fn test_frame_window() {
        let window = FrameWindow::of_delta(100, 10);
        assert_eq!(window.prev_time, 90);
        assert_eq!(window.current_time, 100);

        assert!(window.contains_event_time(95));
        assert!(window.contains_event_time(100));
        assert!(!window.contains_event_time(90));
        assert!(!window.contains_event_time(101));
    }

    #[test]
    fn test_timed_batch_to_frame_input() {
        let batch = TimedInputBatch {
            current_time: 100,
            events: vec![
                TimedInputEvent::ButtonClick { tp: 95, zone: ButtonZone::K1 },
                TimedInputEvent::SensorClick { tp: 98, area: SensorArea::A1 },
                TimedInputEvent::ButtonHold { tp: 90, zone: ButtonZone::K2, is_down: true },
            ],
        };
        let prev_button = ButtonVec::replicate(false);
        let prev_sensor = SensorVec::replicate(false);
        let fi = batch.to_frame_input(10, &prev_button, &prev_sensor);

        assert!(fi.get_button_clicked(ButtonZone::K1));
        assert!(fi.get_sensor_clicked(SensorArea::A1));
        assert!(fi.get_button_held(ButtonZone::K2));
        assert!(!fi.get_button_clicked(ButtonZone::K2));
    }
}
