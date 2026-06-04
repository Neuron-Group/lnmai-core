//! Note lifecycle state machines — pure functional models of
//! Tap, Hold, Slide, and Touch note state transitions.
//!
//! Each note type has its own state machine. The Core advances
//! all active notes each frame, consuming input and emitting
//! JudgeEvents when a note is judged.

use serde::{Deserialize, Serialize};

use super::areas::{ButtonZone, OuterSlot, SensorArea};
use super::constants::*;
use super::judge;
use super::time::Duration;
use super::types::*;

/// Common note parameters (set at spawn time)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct CommonNoteParams {
    /// Scheduled judge time
    pub judge_timing: i64, // TimePoint in microseconds
    /// User judge offset
    pub judge_offset: i64, // Duration in microseconds
    /// Is break note
    pub is_break: bool,
    /// Is EX note
    pub is_ex: bool,
    /// Unique id in chart
    pub note_index: u32,
}

impl CommonNoteParams {
    /// Effective judge timing with user offset
    pub fn effective_timing(&self) -> i64 {
        self.judge_timing + self.judge_offset
    }
}

/// Hold start position
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HoldStart {
    HoldButton(ButtonZone),
    HoldSensor(SensorArea),
}

impl HoldStart {
    pub fn to_runtime_pos(&self) -> RuntimePos {
        match self {
            HoldStart::HoldButton(zone) => RuntimePos::ButtonZonePos(*zone),
            HoldStart::HoldSensor(area) => RuntimePos::SensorAreaPos(*area),
        }
    }
}

/// Tap note state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TapState {
    /// Spawned, not yet in range
    Waiting,
    /// Within judgeable window, awaiting input
    Judgeable,
    /// Judged (by input or too-late)
    TapJudged(JudgeGrade),
    /// Terminal
    TapEnded,
}

/// Tap note
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TapNote {
    pub params: CommonNoteParams,
    pub lane: OuterSlot,
    pub state: TapState,
    pub button_queue_index: u32,
}

impl TapNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::ButtonZonePos(self.lane.to_button_zone())
    }
}

/// Slide head note
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlideHeadNote {
    pub params: CommonNoteParams,
    pub lane: OuterSlot,
    pub state: TapState,
    pub logical_slide_id: u32,
    pub button_queue_index: u32,
}

impl SlideHeadNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::ButtonZonePos(self.lane.to_button_zone())
    }
}

/// Tap family note (tap or slide head)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TapFamilyNote {
    TapNote(TapNote),
    SlideHeadNote(SlideHeadNote),
}

impl TapFamilyNote {
    pub fn params(&self) -> &CommonNoteParams {
        match self {
            TapFamilyNote::TapNote(note) => &note.params,
            TapFamilyNote::SlideHeadNote(note) => &note.params,
        }
    }

    pub fn lane(&self) -> OuterSlot {
        match self {
            TapFamilyNote::TapNote(note) => note.lane,
            TapFamilyNote::SlideHeadNote(note) => note.lane,
        }
    }

    pub fn state(&self) -> &TapState {
        match self {
            TapFamilyNote::TapNote(note) => &note.state,
            TapFamilyNote::SlideHeadNote(note) => &note.state,
        }
    }
}

/// Hold sub-state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HoldSubState {
    HeadWaiting,
    HeadJudgeable,
    HeadJudged,
    BodyHeld,
    BodyReleased,
    Ended,
}

/// Hold note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HoldNote {
    pub params: CommonNoteParams,
    pub start: HoldStart,
    pub state: HoldSubState,
    pub length: i64, // Duration in microseconds
    pub head_diff: Option<i64>, // Duration in microseconds
    pub head_grade: Option<JudgeGrade>,
    pub player_release_time: Option<i64>, // TimePoint in microseconds
    pub is_classic: bool,
    pub is_touch_hold: bool,
    pub touch_group_id: Option<u32>,
    pub touch_group_size: Option<u32>,
    pub touch_group_count: Option<u32>,
}

impl HoldNote {
    pub fn position(&self) -> RuntimePos {
        self.start.to_runtime_pos()
    }
}

/// Touch note state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TouchState {
    TouchWaiting,
    TouchJudgeable,
    TouchJudged(JudgeGrade),
    TouchEnded,
}

/// Touch note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TouchNote {
    pub params: CommonNoteParams,
    pub state: TouchState,
    pub sensor_pos: SensorArea,
    pub touch_group_id: Option<u32>,
    pub touch_group_size: Option<u32>,
    pub touch_group_count: Option<u32>,
}

impl TouchNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::SensorAreaPos(self.sensor_pos)
    }
}

/// Slide state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SlideState {
    Waiting,
    Active {
        wait_time: i64, // Duration in microseconds
    },
    Judged {
        grade: JudgeGrade,
        wait_time: i64, // Duration in microseconds
        judge_diff: i64, // Duration in microseconds
    },
    Ended,
}

/// Slide area
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlideArea {
    pub target_areas: Vec<SensorArea>,
    pub policy: AreaPolicy,
    pub is_last: bool,
    pub is_skippable: bool,
    pub arrow_progress_when_on: Option<i64>, // TimePoint in microseconds
    pub arrow_progress_when_finished: Option<i64>, // TimePoint in microseconds
    pub was_on: bool,
    pub was_off: bool,
}

/// Slide note
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlideNote {
    pub params: CommonNoteParams,
    pub lane: OuterSlot,
    pub state: SlideState,
    pub length: i64, // Duration in microseconds
    pub timing: i64, // TimePoint in microseconds
    pub start_timing: i64, // TimePoint in microseconds
    pub slide_kind: SlideKind,
    pub is_classic: bool,
    pub is_conn_slide: bool,
    pub parent_index: Option<u32>,
    pub group_indices: Vec<u32>,
    pub judge_queues: Vec<Vec<SlideArea>>,
}

impl SlideNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::ButtonZonePos(self.lane.to_button_zone())
    }
}

/// Slide step context
#[derive(Debug, Clone, Copy)]
pub struct SlideStepContext {
    pub current_time: i64, // TimePoint in microseconds
    pub touch_panel_offset: i64, // Duration in microseconds
    pub delta: i64, // Duration in microseconds
    pub style: JudgeStyle,
    pub subdivide_slide_judge_grade: bool,
    pub sensor_held: [bool; 33], // SensorVec<bool>
}

/// One frame advance for TapNote
///
/// Returns (new_note, optional_judge_event)
pub fn tap_step(
    note: &TapNote,
    current_time: i64,
    judge_diff: i64,
    input_clicked: bool,
    style: JudgeStyle,
) -> (TapNote, Option<JudgeEvent>) {
    let effective_timing = note.params.effective_timing();
    let time_diff = current_time - effective_timing;

    match note.state {
        TapState::Waiting => {
            // Check if we're within judgable range
            if time_diff >= -JUDGABLE_RANGE_SEC.to_micros() {
                let mut new_note = *note;
                new_note.state = TapState::Judgeable;
                (new_note, None)
            } else {
                (*note, None)
            }
        }
        TapState::Judgeable => {
            if input_clicked {
                // Judge the note
                let grade = judge::judge_tap(Duration::from_micros(judge_diff), note.params.is_ex);
                let converted_grade = super::convert::convert_grade(style, grade);
                let mut new_note = *note;
                new_note.state = TapState::TapJudged(converted_grade);
                let event = JudgeEvent {
                    kind: JudgeEventKind::Tap,
                    grade: converted_grade,
                    diff: judge_diff,
                    position: note.position(),
                    note_index: note.params.note_index,
                };
                (new_note, Some(event))
            } else if time_diff > JUDGABLE_RANGE_SEC.to_micros() {
                // Too late
                let mut new_note = *note;
                new_note.state = TapState::TapJudged(JudgeGrade::Miss);
                let event = JudgeEvent {
                    kind: JudgeEventKind::Tap,
                    grade: JudgeGrade::Miss,
                    diff: judge_diff,
                    position: note.position(),
                    note_index: note.params.note_index,
                };
                (new_note, Some(event))
            } else {
                (*note, None)
            }
        }
        TapState::TapJudged(_) => {
            // Move to ended
            let mut new_note = *note;
            new_note.state = TapState::TapEnded;
            (new_note, None)
        }
        TapState::TapEnded => (*note, None),
    }
}

/// One frame advance for TouchNote
///
/// Returns (new_note, optional_judge_event)
pub fn touch_step(
    note: &TouchNote,
    current_time: i64,
    judge_diff: i64,
    input_clicked: bool,
    _shared_result: Option<JudgeGrade>,
    style: JudgeStyle,
) -> (TouchNote, Option<JudgeEvent>) {
    let effective_timing = note.params.effective_timing();
    let time_diff = current_time - effective_timing;

    match note.state {
        TouchState::TouchWaiting => {
            if time_diff >= -JUDGABLE_RANGE_SEC.to_micros() {
                let mut new_note = note.clone();
                new_note.state = TouchState::TouchJudgeable;
                (new_note, None)
            } else {
                (note.clone(), None)
            }
        }
        TouchState::TouchJudgeable => {
            if input_clicked {
                let grade_opt = judge::judge_touch(Duration::from_micros(judge_diff), note.params.is_ex);
                match grade_opt {
                    Some(grade) => {
                        let converted_grade = super::convert::convert_grade(style, grade);
                        let mut new_note = note.clone();
                        new_note.state = TouchState::TouchJudged(converted_grade);
                        let event = JudgeEvent {
                            kind: JudgeEventKind::Touch,
                            grade: converted_grade,
                            diff: judge_diff,
                            position: note.position(),
                            note_index: note.params.note_index,
                        };
                        (new_note, Some(event))
                    }
                    None => {
                        // Too early, ignore
                        (note.clone(), None)
                    }
                }
            } else if time_diff > JUDGABLE_RANGE_SEC.to_micros() + TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC.to_micros() {
                // Too late
                let mut new_note = note.clone();
                new_note.state = TouchState::TouchJudged(JudgeGrade::Miss);
                let event = JudgeEvent {
                    kind: JudgeEventKind::Touch,
                    grade: JudgeGrade::Miss,
                    diff: judge_diff,
                    position: note.position(),
                    note_index: note.params.note_index,
                };
                (new_note, Some(event))
            } else {
                (note.clone(), None)
            }
        }
        TouchState::TouchJudged(_) => {
            let mut new_note = note.clone();
            new_note.state = TouchState::TouchEnded;
            (new_note, None)
        }
        TouchState::TouchEnded => (note.clone(), None),
    }
}

/// Advance a hold note one frame.
///
/// Mirrors LnmaiCore.Lifecycle.holdStep.
/// `input_clicked` = button/sensor just pressed this frame (edge).
/// `input_pressed` = button/sensor is held this frame.
pub fn hold_step(
    note: &HoldNote,
    current_time: i64,
    judge_diff: i64,
    head_ignore: i64,
    tail_ignore: i64,
    input_clicked: bool,
    input_pressed: bool,
    current_button_pressed: bool,
    prev_sensor_pressed: bool,
    touch_panel_offset: i64,
    shared_result: Option<(JudgeGrade, i64)>,
    delta: i64,
    style: JudgeStyle,
) -> (HoldNote, Option<JudgeEvent>) {
    let timing = note.params.effective_timing();
    let diff = current_time - timing;
    let body_check_start = timing + head_ignore;
    let body_check_end = timing + note.length - tail_ignore;
    let body_window_disabled = !note.is_classic && note.length <= head_ignore + tail_ignore;
    let release_offset = if prev_sensor_pressed && !current_button_pressed { 0 } else { touch_panel_offset };

    let end_hold = |note: &HoldNote, head_grade: JudgeGrade, classic_release_timing: i64, release_time: i64| -> (HoldNote, Option<JudgeEvent>) {
        let final_grade = if note.is_classic {
            super::judge::judge_hold_classic_end(
                head_grade,
                timing,
                Duration::from_micros(note.length),
                classic_release_timing,
            )
        } else {
            super::judge::judge_hold_end(
                head_grade,
                Duration::from_micros(note.head_diff.unwrap_or(0)),
                Duration::from_micros(note.length),
                Duration::from_micros(head_ignore + tail_ignore),
                Duration::from_micros(release_time),
            )
        };
        let final_grade = super::convert::convert_grade(style, final_grade);
        let event_diff = if note.head_diff.unwrap_or(0) == 0 && head_grade == JudgeGrade::Miss {
            150000 // 150ms
        } else {
            note.head_diff.unwrap_or(0)
        };
        let event = JudgeEvent {
            kind: JudgeEventKind::Hold,
            grade: final_grade,
            diff: event_diff,
            position: note.position(),
            note_index: note.params.note_index,
        };
        let mut ended = note.clone();
        ended.state = HoldSubState::Ended;
        (ended, Some(event))
    };

    let judgeable_start = timing - JUDGABLE_RANGE_SEC.to_micros();

    match note.state {
        HoldSubState::HeadWaiting => {
            let too_late_threshold = if note.is_touch_hold {
                timing + TOUCH_JUDGE_GOOD_AREA_MSEC.to_micros()
            } else {
                timing + TAP_JUDGE_GOOD_AREA_MSEC.to_micros()
            };

            if let Some((grade, shared_diff)) = shared_result {
                let mut updated = note.clone();
                updated.state = HoldSubState::HeadJudged;
                updated.head_grade = Some(grade);
                updated.head_diff = Some(shared_diff);
                (updated, None)
            } else if current_time > too_late_threshold {
                let mut updated = note.clone();
                updated.state = HoldSubState::HeadJudged;
                updated.head_grade = Some(JudgeGrade::Miss);
                (updated, None)
            } else if current_time >= judgeable_start {
                if input_clicked {
                    if note.is_touch_hold {
                        let grade_opt = super::judge::judge_touch(
                            Duration::from_micros(judge_diff),
                            note.params.is_ex,
                        );
                        match grade_opt {
                            Some(raw) => {
                                let grade = super::convert::convert_grade(style, raw);
                                let mut updated = note.clone();
                                updated.state = HoldSubState::HeadJudged;
                                updated.head_grade = Some(grade);
                                updated.head_diff = Some(judge_diff);
                                (updated, None)
                            }
                            None => (note.clone(), None),
                        }
                    } else {
                        let raw = super::judge::judge_tap(
                            Duration::from_micros(judge_diff),
                            note.params.is_ex,
                        );
                        let grade = super::convert::convert_grade(style, raw);
                        let mut updated = note.clone();
                        updated.state = HoldSubState::HeadJudged;
                        updated.head_grade = Some(grade);
                        updated.head_diff = Some(judge_diff);
                        (updated, None)
                    }
                } else {
                    let mut updated = note.clone();
                    updated.state = HoldSubState::HeadJudgeable;
                    (updated, None)
                }
            } else {
                (note.clone(), None)
            }
        }
        HoldSubState::HeadJudgeable => {
            let too_late_threshold = if note.is_touch_hold {
                timing + TOUCH_JUDGE_GOOD_AREA_MSEC.to_micros()
            } else {
                timing + TAP_JUDGE_GOOD_AREA_MSEC.to_micros()
            };

            if let Some((grade, shared_diff)) = shared_result {
                let mut updated = note.clone();
                updated.state = HoldSubState::HeadJudged;
                updated.head_grade = Some(grade);
                updated.head_diff = Some(shared_diff);
                (updated, None)
            } else if input_clicked && current_time >= judgeable_start {
                if note.is_touch_hold {
                    let grade_opt = super::judge::judge_touch(
                        Duration::from_micros(judge_diff),
                        note.params.is_ex,
                    );
                    match grade_opt {
                        Some(raw) => {
                            let grade = super::convert::convert_grade(style, raw);
                            let mut updated = note.clone();
                            updated.state = HoldSubState::HeadJudged;
                            updated.head_grade = Some(grade);
                            updated.head_diff = Some(judge_diff);
                            (updated, None)
                        }
                        None => (note.clone(), None),
                    }
                } else {
                    let raw = super::judge::judge_tap(
                        Duration::from_micros(judge_diff),
                        note.params.is_ex,
                    );
                    let grade = super::convert::convert_grade(style, raw);
                    let mut updated = note.clone();
                    updated.state = HoldSubState::HeadJudged;
                    updated.head_grade = Some(grade);
                    updated.head_diff = Some(judge_diff);
                    (updated, None)
                }
            } else if current_time > too_late_threshold {
                let mut updated = note.clone();
                updated.state = HoldSubState::HeadJudged;
                updated.head_grade = Some(JudgeGrade::Miss);
                (updated, None)
            } else {
                (note.clone(), None)
            }
        }
        HoldSubState::HeadJudged => {
            let head_grade = note.head_grade.unwrap_or(JudgeGrade::Miss);
            if current_time < body_check_start {
                (note.clone(), None)
            } else if note.is_classic {
                if diff >= note.length + CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC.to_micros()
                    || head_grade.is_miss_or_too_fast()
                {
                    end_hold(note, head_grade, current_time, note.player_release_time.unwrap_or(0))
                } else if input_pressed {
                    let mut updated = note.clone();
                    updated.state = HoldSubState::BodyHeld;
                    (updated, None)
                } else {
                    end_hold(note, head_grade, current_time - release_offset, note.player_release_time.unwrap_or(0))
                }
            } else if body_window_disabled {
                if diff >= note.length {
                    end_hold(note, head_grade, current_time, note.player_release_time.unwrap_or(0))
                } else {
                    (note.clone(), None)
                }
            } else if current_time > body_check_end {
                end_hold(note, head_grade, current_time, note.player_release_time.unwrap_or(0))
            } else if input_pressed {
                let mut updated = note.clone();
                updated.state = HoldSubState::BodyHeld;
                updated.player_release_time = Some(0);
                (updated, None)
            } else {
                if head_grade.is_miss_or_too_fast() {
                    let new_rt = note.player_release_time.unwrap_or(0) + delta;
                    let mut updated = note.clone();
                    updated.state = HoldSubState::BodyReleased;
                    updated.player_release_time = Some(new_rt);
                    (updated, None)
                } else {
                    let new_rt = note.player_release_time.unwrap_or(0) + delta;
                    if new_rt <= DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC.to_micros() {
                        let mut updated = note.clone();
                        updated.player_release_time = Some(new_rt);
                        (updated, None)
                    } else {
                        let mut updated = note.clone();
                        updated.state = HoldSubState::BodyReleased;
                        updated.player_release_time = Some(new_rt);
                        (updated, None)
                    }
                }
            }
        }
        HoldSubState::BodyHeld => {
            let head_grade = note.head_grade.unwrap_or(JudgeGrade::Miss);
            if note.is_classic {
                if diff >= note.length + CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC.to_micros()
                    || head_grade.is_miss_or_too_fast()
                {
                    end_hold(note, head_grade, current_time, note.player_release_time.unwrap_or(0))
                } else if input_pressed {
                    (note.clone(), None)
                } else {
                    end_hold(note, head_grade, current_time - release_offset, note.player_release_time.unwrap_or(0))
                }
            } else if body_window_disabled {
                if diff >= note.length {
                    end_hold(note, head_grade, current_time, note.player_release_time.unwrap_or(0))
                } else {
                    (note.clone(), None)
                }
            } else if current_time > body_check_end || diff >= note.length {
                end_hold(note, head_grade, current_time, note.player_release_time.unwrap_or(0))
            } else if input_pressed {
                (note.clone(), None)
            } else {
                let mut updated = note.clone();
                updated.state = HoldSubState::BodyReleased;
                updated.player_release_time = Some(note.player_release_time.unwrap_or(0) + delta);
                (updated, None)
            }
        }
        HoldSubState::BodyReleased => {
            let head_grade = note.head_grade.unwrap_or(JudgeGrade::Miss);
            if body_window_disabled {
                if diff >= note.length {
                    let new_rt = note.player_release_time.unwrap_or(0) + delta;
                    end_hold(note, head_grade, current_time, new_rt)
                } else {
                    (note.clone(), None)
                }
            } else if current_time > body_check_end || diff >= note.length {
                let new_rt = note.player_release_time.unwrap_or(0) + delta;
                end_hold(note, head_grade, current_time, new_rt)
            } else if input_pressed {
                let mut updated = note.clone();
                updated.state = HoldSubState::BodyHeld;
                (updated, None)
            } else {
                let new_rt = note.player_release_time.unwrap_or(0) + delta;
                let mut updated = note.clone();
                updated.player_release_time = Some(new_rt);
                (updated, None)
            }
        }
        HoldSubState::Ended => (note.clone(), None),
    }
}

/// Advance a slide note one frame with queue traversal.
///
/// Mirrors LnmaiCore.Lifecycle.slideStep.
pub fn slide_step(
    note: &SlideNote,
    current_time: i64,
    sensor_held: &[bool; 33],
    touch_panel_offset: i64,
    delta: i64,
    style: JudgeStyle,
    subdivide_slide_judge_grade: bool,
) -> (SlideNote, Option<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>) {
    let effective_grade = |raw: JudgeGrade| -> JudgeGrade {
        let converted = super::convert::convert_grade(style, raw);
        if subdivide_slide_judge_grade { converted } else { super::judge::correct_slide_grade(converted) }
    };

    let judge_diff = (current_time - touch_panel_offset) - note.params.effective_timing();
    let current_diff = current_time - note.params.effective_timing();

    let too_late_timing = note.start_timing + note.length
        + SLIDE_JUDGE_GOOD_AREA_MSEC.to_micros()
        + note.params.judge_offset.min(0);

    // Check sensor held for each area
    let sensor_held_at = |area: SensorArea| -> bool {
        let idx = area.to_index();
        sensor_held.get(idx).copied().unwrap_or(false)
    };

    // Update slide area based on sensor held
    let update_area = |area: &SlideArea| -> SlideArea {
        let is_held = match area.policy {
            AreaPolicy::Or => area.target_areas.iter().any(|a| sensor_held_at(*a)),
            AreaPolicy::And => area.target_areas.iter().all(|a| sensor_held_at(*a)),
        };
        let mut updated = area.clone();
        if is_held {
            updated.was_on = true;
        } else if area.was_on {
            updated.was_off = true;
        }
        updated
    };

    // Process a single judge queue
    let process_queue = |queue: &[SlideArea]| -> (Vec<SlideArea>, Vec<RenderCommand>) {
        let mut cmds = Vec::new();
        let mut result = Vec::new();
        let mut areas: Vec<SlideArea> = queue.iter().map(|a| update_area(a)).collect();

        let mut i = 0;
        while i < areas.len() {
            let area = &areas[i];
            if i + 1 < areas.len() {
                let next = &areas[i + 1];
                let next_updated = update_area(next);
                if area.is_skippable || area.was_on {
                    if next_updated.was_on && next_updated.was_off && next_updated.is_last {
                        // next is finished (last area)
                        cmds.push(RenderCommand::HideSlideBars {
                            note_index: note.params.note_index,
                            end_index: next_updated.arrow_progress_when_finished.unwrap_or(0) as u32,
                        });
                        i += 2;
                        continue;
                    } else if next_updated.was_on {
                        cmds.push(RenderCommand::HideSlideBars {
                            note_index: note.params.note_index,
                            end_index: next_updated.arrow_progress_when_on.unwrap_or(0) as u32,
                        });
                        result.push(next_updated);
                        i += 2;
                        continue;
                    } else if area.was_on && area.was_off && area.is_last {
                        cmds.push(RenderCommand::HideSlideBars {
                            note_index: note.params.note_index,
                            end_index: area.arrow_progress_when_finished.unwrap_or(0) as u32,
                        });
                        result.push(next_updated);
                        i += 2;
                        continue;
                    }
                }
                result.push(areas[i].clone());
                i += 1;
            } else {
                if area.was_on && area.was_off && area.is_last {
                    cmds.push(RenderCommand::HideSlideBars {
                        note_index: note.params.note_index,
                        end_index: area.arrow_progress_when_finished.unwrap_or(0) as u32,
                    });
                } else {
                    result.push(area.clone());
                }
                i += 1;
            }
        }
        (result, cmds)
    };

    let old_queues: Vec<Vec<SlideArea>> = note.judge_queues.clone();
    let mut new_queues: Vec<Vec<SlideArea>> = Vec::new();
    let mut render_cmds = Vec::new();

    for queue in &old_queues {
        let (new_queue, cmds) = process_queue(queue);
        new_queues.push(new_queue);
        render_cmds.extend(cmds);
    }

    let queue_remaining = |queues: &[Vec<SlideArea>]| -> u32 {
        queues.iter().map(|q| q.len() as u32).max().unwrap_or(0)
    };
    let all_cleared = |queues: &[Vec<SlideArea>]| -> bool {
        queues.iter().all(|q| q.is_empty())
    };

    let old_remaining = queue_remaining(&old_queues);
    let new_remaining = queue_remaining(&new_queues);

    let mut updated_note = note.clone();
    updated_note.judge_queues = new_queues.clone();

    let progress_changed = new_remaining != old_remaining;

    match note.state {
        SlideState::Waiting => {
            let mut audio = Vec::new();
            if progress_changed {
                render_cmds.push(RenderCommand::UpdateSlideProgress {
                    note_index: note.params.note_index,
                    remaining: new_remaining,
                });
            }
            (updated_note, None, audio, render_cmds)
        }
        SlideState::Active { wait_time } => {
            let is_too_late = current_time > too_late_timing;
            let mut audio = Vec::new();

            if all_cleared(&updated_note.judge_queues) && !is_too_late {
                let raw = if note.is_classic {
                    super::judge::judge_slide_classic(Duration::from_micros(judge_diff))
                } else {
                    super::judge::judge_slide_modern(
                        Duration::from_micros(judge_diff),
                        Duration::from_micros(wait_time),
                        note.params.is_ex,
                    )
                };
                updated_note.state = SlideState::Judged {
                    grade: raw,
                    wait_time,
                    judge_diff,
                };
                if progress_changed {
                    render_cmds.push(RenderCommand::UpdateSlideProgress {
                        note_index: note.params.note_index,
                        remaining: new_remaining,
                    });
                }
                (updated_note, None, audio, render_cmds)
            } else if all_cleared(&updated_note.judge_queues) && is_too_late {
                let raw = super::judge::judge_slide_too_late(queue_remaining(&updated_note.judge_queues));
                let grade = effective_grade(raw);
                updated_note.state = SlideState::Ended;
                let event = JudgeEvent {
                    kind: JudgeEventKind::Slide,
                    grade,
                    diff: current_diff,
                    position: note.position(),
                    note_index: note.params.note_index,
                };
                render_cmds.push(RenderCommand::HideAllSlideBars {
                    note_index: note.params.note_index,
                });
                (updated_note, Some(event), audio, render_cmds)
            } else {
                if progress_changed {
                    render_cmds.push(RenderCommand::UpdateSlideProgress {
                        note_index: note.params.note_index,
                        remaining: new_remaining,
                    });
                }
                (updated_note, None, audio, render_cmds)
            }
        }
        SlideState::Judged { grade, wait_time, judge_diff: stored_judge_diff } => {
            let new_wait = wait_time - delta;
            let mut audio = Vec::new();
            if new_wait <= 0 {
                let final_grade = effective_grade(grade);
                updated_note.state = SlideState::Ended;
                let event = JudgeEvent {
                    kind: JudgeEventKind::Slide,
                    grade: final_grade,
                    diff: stored_judge_diff,
                    position: note.position(),
                    note_index: note.params.note_index,
                };
                render_cmds.push(RenderCommand::HideAllSlideBars {
                    note_index: note.params.note_index,
                });
                (updated_note, Some(event), audio, render_cmds)
            } else {
                updated_note.state = SlideState::Judged {
                    grade,
                    wait_time: new_wait,
                    judge_diff: stored_judge_diff,
                };
                if progress_changed {
                    render_cmds.push(RenderCommand::UpdateSlideProgress {
                        note_index: note.params.note_index,
                        remaining: new_remaining,
                    });
                }
                (updated_note, None, audio, render_cmds)
            }
        }
        SlideState::Ended => {
            (updated_note, None, Vec::new(), Vec::new())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tap_note_position() {
        let note = TapNote {
            params: CommonNoteParams {
                judge_timing: 1000000,
                judge_offset: 0,
                is_break: false,
                is_ex: false,
                note_index: 0,
            },
            lane: OuterSlot::S1,
            state: TapState::Waiting,
            button_queue_index: 0,
        };
        assert_eq!(note.position(), RuntimePos::ButtonZonePos(ButtonZone::K1));
    }

    #[test]
    fn test_hold_note_position() {
        let note = HoldNote {
            params: CommonNoteParams {
                judge_timing: 1000000,
                judge_offset: 0,
                is_break: false,
                is_ex: false,
                note_index: 0,
            },
            start: HoldStart::HoldButton(ButtonZone::K1),
            state: HoldSubState::HeadWaiting,
            length: 1000000,
            head_diff: None,
            head_grade: None,
            player_release_time: None,
            is_classic: false,
            is_touch_hold: false,
            touch_group_id: None,
            touch_group_size: None,
            touch_group_count: None,
        };
        assert_eq!(note.position(), RuntimePos::ButtonZonePos(ButtonZone::K1));
    }

    #[test]
    fn test_touch_note_position() {
        let note = TouchNote {
            params: CommonNoteParams {
                judge_timing: 1000000,
                judge_offset: 0,
                is_break: false,
                is_ex: false,
                note_index: 0,
            },
            state: TouchState::TouchWaiting,
            sensor_pos: SensorArea::A1,
            touch_group_id: None,
            touch_group_size: None,
            touch_group_count: None,
        };
        assert_eq!(note.position(), RuntimePos::SensorAreaPos(SensorArea::A1));
    }
}
