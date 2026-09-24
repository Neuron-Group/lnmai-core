//! Per-frame input model, per-zone note queues, and the cross-frame game state.
//!
//! Mirrors `LnmaiCore/InputModel.lean`.

use crate::areas::{ButtonZone, OuterSlot, SensorArea};
use crate::constants::{BUTTON_ZONE_COUNT, SENSOR_AREA_COUNT, SUBDIVIDE_SLIDE_JUDGE_GRADE};
use crate::lifecycle::{HoldNote, SlideNote, TapFamilyNote, TouchNote};
use crate::storage::{ButtonVec, SensorVec};
use crate::time::{time_point_sub_duration, Duration, TimePoint};
use crate::runtime_score::RuntimeScoreState;
use crate::types::{GroupState, JudgeDisplayOption, JudgeStyle};

/// `TouchHoldBodyGroupState` (from `LnmaiCore.Types`).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TouchHoldBodyGroupState {
    pub group_id: usize,
    pub member_note_indices: Vec<usize>,
    pub triggered_note_indices: Vec<usize>,
}

fn button_vec_bool_default() -> ButtonVec<bool> {
    ButtonVec::replicate(BUTTON_ZONE_COUNT as usize, false)
}

fn sensor_vec_bool_default() -> SensorVec<bool> {
    SensorVec::replicate(SENSOR_AREA_COUNT as usize, false)
}

fn button_vec_nat_default() -> ButtonVec<usize> {
    ButtonVec::replicate(BUTTON_ZONE_COUNT as usize, 0)
}

fn sensor_vec_nat_default() -> SensorVec<usize> {
    SensorVec::replicate(SENSOR_AREA_COUNT as usize, 0)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrameInput {
    pub button_clicked: ButtonVec<bool>,
    pub button_held: ButtonVec<bool>,
    pub sensor_clicked: SensorVec<bool>,
    pub sensor_held: SensorVec<bool>,
    pub button_click_count: ButtonVec<usize>,
    pub sensor_click_count: SensorVec<usize>,
    pub delta: Duration,
}

impl Default for FrameInput {
    fn default() -> Self {
        FrameInput {
            button_clicked: button_vec_bool_default(),
            button_held: button_vec_bool_default(),
            sensor_clicked: sensor_vec_bool_default(),
            sensor_held: sensor_vec_bool_default(),
            button_click_count: button_vec_nat_default(),
            sensor_click_count: sensor_vec_nat_default(),
            delta: Duration::zero(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimedInputEvent {
    ButtonClick(TimePoint, ButtonZone),
    ButtonHold(TimePoint, ButtonZone, bool),
    SensorClick(TimePoint, SensorArea),
    SensorHold(TimePoint, SensorArea, bool),
}

impl TimedInputEvent {
    pub fn at(&self) -> TimePoint {
        match self {
            TimedInputEvent::ButtonClick(tp, _) => *tp,
            TimedInputEvent::ButtonHold(tp, _, _) => *tp,
            TimedInputEvent::SensorClick(tp, _) => *tp,
            TimedInputEvent::SensorHold(tp, _, _) => *tp,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimedInputBatch {
    pub current_time: TimePoint,
    pub events: Vec<TimedInputEvent>,
}

impl Default for TimedInputBatch {
    fn default() -> Self {
        TimedInputBatch { current_time: TimePoint::zero(), events: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FrameWindow {
    pub prev_time: TimePoint,
    pub current_time: TimePoint,
}

impl FrameWindow {
    pub fn of_delta(current_time: TimePoint, delta: Duration) -> FrameWindow {
        FrameWindow { prev_time: time_point_sub_duration(current_time, delta), current_time }
    }

    pub fn contains_event_time(&self, event_time: TimePoint) -> bool {
        if self.prev_time.to_micros() == self.current_time.to_micros() {
            event_time.to_micros() == self.current_time.to_micros()
        } else {
            event_time.to_micros() > self.prev_time.to_micros()
                && event_time.to_micros() <= self.current_time.to_micros()
        }
    }
}

impl FrameInput {
    pub fn get_button_held(&self, zone: ButtonZone) -> bool {
        self.button_held.get_d(zone, false)
    }
    pub fn get_button_held_list(&self) -> Vec<bool> {
        self.button_held.to_list()
    }
    pub fn get_button_clicked(&self, zone: ButtonZone) -> bool {
        self.button_clicked.get_d(zone, false)
    }
    pub fn get_sensor_held(&self, area: SensorArea) -> bool {
        self.sensor_held.get_d(area, false)
    }
    pub fn get_sensor_held_list(&self) -> Vec<bool> {
        self.sensor_held.to_list()
    }
    pub fn get_sensor_clicked(&self, area: SensorArea) -> bool {
        self.sensor_clicked.get_d(area, false)
    }
    pub fn get_button_click_count(&self, zone: ButtonZone) -> usize {
        let count = self.button_click_count.get_d(zone, 0);
        if count > 0 {
            count
        } else if self.get_button_clicked(zone) {
            1
        } else {
            0
        }
    }
    pub fn get_sensor_click_count(&self, area: SensorArea) -> usize {
        let count = self.sensor_click_count.get_d(area, 0);
        if count > 0 {
            count
        } else if self.get_sensor_clicked(area) {
            1
        } else {
            0
        }
    }
}

impl TimedInputBatch {
    pub fn to_frame_input(
        &self,
        delta: Duration,
        prev_button_held: ButtonVec<bool>,
        prev_sensor_held: SensorVec<bool>,
    ) -> FrameInput {
        let window = FrameWindow::of_delta(self.current_time, delta);
        let mut fi = FrameInput {
            button_held: prev_button_held,
            sensor_held: prev_sensor_held,
            delta,
            ..FrameInput::default()
        };
        for evt in &self.events {
            if !window.contains_event_time(evt.at()) {
                continue;
            }
            match evt {
                TimedInputEvent::ButtonClick(_, zone) => {
                    fi.button_clicked = fi.button_clicked.set(*zone, true);
                    let next = fi.button_click_count.get_d(*zone, 0) + 1;
                    fi.button_click_count = fi.button_click_count.set(*zone, next);
                }
                TimedInputEvent::SensorClick(_, area) => {
                    fi.sensor_clicked = fi.sensor_clicked.set(*area, true);
                    let next = fi.sensor_click_count.get_d(*area, 0) + 1;
                    fi.sensor_click_count = fi.sensor_click_count.set(*area, next);
                }
                TimedInputEvent::ButtonHold(_, zone, is_down) => {
                    fi.button_held = fi.button_held.set(*zone, *is_down);
                }
                TimedInputEvent::SensorHold(_, area, is_down) => {
                    fi.sensor_held = fi.sensor_held.set(*area, *is_down);
                }
            }
        }
        fi.delta = delta;
        fi
    }
}

// ---------------------------------------------------------------------------
// Per-zone queues
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZoneQueue<T> {
    pub notes: Vec<T>,
    pub current_index: usize,
}

impl<T> Default for ZoneQueue<T> {
    fn default() -> Self {
        ZoneQueue { notes: Vec::new(), current_index: 0 }
    }
}

impl<T: Clone> ZoneQueue<T> {
    pub fn is_empty(&self) -> bool {
        self.current_index >= self.notes.len()
    }

    pub fn peek(&self) -> Option<T> {
        self.notes.get(self.current_index).cloned()
    }

    pub fn advance(&self) -> ZoneQueue<T> {
        ZoneQueue { current_index: self.current_index + 1, ..self.clone() }
    }
}

pub type ButtonQueueVec<T> = ButtonVec<ZoneQueue<T>>;
pub type SensorQueueVec<T> = SensorVec<ZoneQueue<T>>;

pub fn button_queue_vec_replicate<T: Clone>(n: usize, queue: ZoneQueue<T>) -> ButtonQueueVec<T> {
    ButtonVec::replicate(n, queue)
}

pub fn sensor_queue_vec_replicate<T: Clone>(n: usize, queue: ZoneQueue<T>) -> SensorQueueVec<T> {
    SensorVec::replicate(n, queue)
}

pub fn button_queue_at<T: Clone>(queues: &ButtonQueueVec<T>, zone: ButtonZone) -> ZoneQueue<T> {
    queues.get_d(zone, ZoneQueue::default())
}

pub fn sensor_queue_at<T: Clone>(queues: &SensorQueueVec<T>, area: SensorArea) -> ZoneQueue<T> {
    queues.get_d(area, ZoneQueue::default())
}

pub fn set_button_queue_at<T: Clone>(
    queues: &ButtonQueueVec<T>,
    zone: ButtonZone,
    queue: ZoneQueue<T>,
) -> ButtonQueueVec<T> {
    queues.set(zone, queue)
}

pub fn set_sensor_queue_at<T: Clone>(
    queues: &SensorQueueVec<T>,
    area: SensorArea,
    queue: ZoneQueue<T>,
) -> SensorQueueVec<T> {
    queues.set(area, queue)
}

// ---------------------------------------------------------------------------
// Game state
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GameState {
    pub current_time: TimePoint,
    pub prev_button: ButtonVec<bool>,
    pub prev_sensor: SensorVec<bool>,
    pub button_queue_frontiers: ButtonVec<usize>,
    pub touch_queue_frontiers: SensorVec<usize>,
    pub tap_queues: ButtonQueueVec<TapFamilyNote>,
    pub hold_queues: ButtonQueueVec<HoldNote>,
    pub touch_hold_queues: SensorQueueVec<HoldNote>,
    pub touch_queues: SensorQueueVec<TouchNote>,
    pub slides: Vec<SlideNote>,
    pub active_holds: Vec<(ButtonZone, HoldNote)>,
    pub active_touch_holds: Vec<(SensorArea, HoldNote)>,
    pub touch_group_states: Vec<GroupState>,
    pub touch_hold_group_states: Vec<TouchHoldBodyGroupState>,
    pub current_batch: TimedInputBatch,
    pub score: RuntimeScoreState,
    pub judge_style: JudgeStyle,
    pub touch_panel_offset: Duration,
    pub subdivide_slide_judge_grade: bool,
    pub note_fast_late_display: JudgeDisplayOption,
    pub break_fast_late_display: JudgeDisplayOption,
}

impl Default for GameState {
    fn default() -> Self {
        GameState {
            current_time: TimePoint::zero(),
            prev_button: button_vec_bool_default(),
            prev_sensor: sensor_vec_bool_default(),
            button_queue_frontiers: button_vec_nat_default(),
            touch_queue_frontiers: sensor_vec_nat_default(),
            tap_queues: button_queue_vec_replicate(BUTTON_ZONE_COUNT as usize, ZoneQueue::default()),
            hold_queues: button_queue_vec_replicate(BUTTON_ZONE_COUNT as usize, ZoneQueue::default()),
            touch_hold_queues: sensor_queue_vec_replicate(SENSOR_AREA_COUNT as usize, ZoneQueue::default()),
            touch_queues: sensor_queue_vec_replicate(SENSOR_AREA_COUNT as usize, ZoneQueue::default()),
            slides: Vec::new(),
            active_holds: Vec::new(),
            active_touch_holds: Vec::new(),
            touch_group_states: Vec::new(),
            touch_hold_group_states: Vec::new(),
            current_batch: TimedInputBatch::default(),
            score: RuntimeScoreState::default(),
            judge_style: JudgeStyle::Default,
            touch_panel_offset: Duration::zero(),
            subdivide_slide_judge_grade: SUBDIVIDE_SLIDE_JUDGE_GRADE,
            note_fast_late_display: JudgeDisplayOption::All,
            break_fast_late_display: JudgeDisplayOption::Disable,
        }
    }
}

#[allow(dead_code)]
fn _unused_outer(_o: OuterSlot) {}
