//! Note lifecycle state machines (Tap, Hold, Touch, Slide).
//!
//! Mirrors `LnmaiCore/Lifecycle.lean`.

use crate::areas::{ButtonZone, OuterSlot, SensorArea};
use crate::constants as C;
use crate::convert::convert_grade;
use crate::events::{AudioCommand, RenderCommand};
use crate::judge::{
    correct_slide_grade, judge_hold_classic_end, judge_hold_end, judge_slide_classic,
    judge_slide_modern, judge_slide_too_late, judge_tap, judge_touch,
};
use crate::storage::SensorVec;
use crate::time::{self, time_point_add_duration, time_point_sub_duration, Duration, TimePoint};
use crate::types::{
    AreaPolicy, JudgeEvent, JudgeEventKind, JudgeGrade, JudgeStyle, RuntimePos, SlideKind,
};

fn jd_event(
    kind: JudgeEventKind,
    grade: JudgeGrade,
    diff: Duration,
    position: RuntimePos,
    note_index: usize,
    is_break: bool,
) -> JudgeEvent {
    JudgeEvent { kind, grade, diff, position, note_index, is_break, multiple: 1 }
}

// ---------------------------------------------------------------------------
// Common parameters
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CommonNoteParams {
    pub judge_timing: TimePoint,
    pub judge_offset: Duration,
    pub is_break: bool,
    pub is_ex: bool,
    pub note_index: usize,
}

impl CommonNoteParams {
    pub fn effective_timing(&self) -> TimePoint {
        time_point_add_duration(self.judge_timing, self.judge_offset)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HoldStart {
    Button(ButtonZone),
    Sensor(SensorArea),
}

impl HoldStart {
    pub fn to_runtime_pos(&self) -> RuntimePos {
        match self {
            HoldStart::Button(z) => RuntimePos::Button(*z),
            HoldStart::Sensor(a) => RuntimePos::Sensor(*a),
        }
    }
}

// ---------------------------------------------------------------------------
// Tap family
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TapState {
    Waiting,
    Judgeable,
    Judged(JudgeGrade),
    Ended,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TapNote {
    pub params: CommonNoteParams,
    pub lane: OuterSlot,
    pub state: TapState,
    pub button_queue_index: usize,
}

impl TapNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::Button(self.lane.to_button_zone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SlideHeadNote {
    pub params: CommonNoteParams,
    pub lane: OuterSlot,
    pub state: TapState,
    pub logical_slide_id: usize,
    pub button_queue_index: usize,
}

impl SlideHeadNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::Button(self.lane.to_button_zone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TapFamilyNote {
    Tap(TapNote),
    SlideHead(SlideHeadNote),
}

impl TapFamilyNote {
    pub fn params(&self) -> CommonNoteParams {
        match self {
            TapFamilyNote::Tap(n) => n.params,
            TapFamilyNote::SlideHead(n) => n.params,
        }
    }
    pub fn lane(&self) -> OuterSlot {
        match self {
            TapFamilyNote::Tap(n) => n.lane,
            TapFamilyNote::SlideHead(n) => n.lane,
        }
    }
    pub fn state(&self) -> TapState {
        match self {
            TapFamilyNote::Tap(n) => n.state,
            TapFamilyNote::SlideHead(n) => n.state,
        }
    }
    pub fn button_queue_index(&self) -> usize {
        match self {
            TapFamilyNote::Tap(n) => n.button_queue_index,
            TapFamilyNote::SlideHead(n) => n.button_queue_index,
        }
    }
    pub fn position(&self) -> RuntimePos {
        match self {
            TapFamilyNote::Tap(n) => n.position(),
            TapFamilyNote::SlideHead(n) => n.position(),
        }
    }
}

fn can_enter_judgeable(current_time: TimePoint, judgeable_start: TimePoint) -> bool {
    current_time.to_micros() >= judgeable_start.to_micros()
}

fn is_too_late_for_tap_like(current_time: TimePoint, late_limit: TimePoint) -> bool {
    current_time.to_micros() > late_limit.to_micros()
}

fn tap_like_miss_event(params: CommonNoteParams, lane: OuterSlot, style: JudgeStyle) -> JudgeEvent {
    let grade = convert_grade(style, JudgeGrade::Miss);
    jd_event(
        JudgeEventKind::Tap,
        grade,
        Duration::from_micros(-1000),
        RuntimePos::Button(lane.to_button_zone()),
        params.note_index,
        params.is_break,
    )
}

fn tap_like_judge_event(
    params: CommonNoteParams,
    lane: OuterSlot,
    grade: JudgeGrade,
    judge_diff: Duration,
) -> JudgeEvent {
    jd_event(
        JudgeEventKind::Tap,
        grade,
        judge_diff,
        RuntimePos::Button(lane.to_button_zone()),
        params.note_index,
        params.is_break,
    )
}

fn judge_tap_now(note: &TapNote, style: JudgeStyle, judge_diff: Duration) -> (TapNote, Option<JudgeEvent>) {
    let raw = judge_tap(judge_diff, note.params.is_ex);
    let grade = convert_grade(style, raw);
    (
        TapNote { state: TapState::Ended, ..*note },
        Some(tap_like_judge_event(note.params, note.lane, grade, judge_diff)),
    )
}

/// `tapStep`.
pub fn tap_step(
    note: &TapNote,
    current_time: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    style: JudgeStyle,
) -> (TapNote, Option<JudgeEvent>) {
    let timing = note.params.effective_timing();
    let judgeable_start = time_point_sub_duration(timing, C::JUDGABLE_RANGE);
    let late_limit = time_point_add_duration(timing, C::TAP_GOOD);
    match note.state {
        TapState::Waiting => {
            if is_too_late_for_tap_like(current_time, late_limit) {
                (
                    TapNote { state: TapState::Ended, ..*note },
                    Some(tap_like_miss_event(note.params, note.lane, style)),
                )
            } else if can_enter_judgeable(current_time, judgeable_start) {
                if input_clicked {
                    judge_tap_now(note, style, judge_diff)
                } else {
                    (TapNote { state: TapState::Judgeable, ..*note }, None)
                }
            } else {
                (*note, None)
            }
        }
        TapState::Judgeable => {
            if is_too_late_for_tap_like(current_time, late_limit) {
                (
                    TapNote { state: TapState::Ended, ..*note },
                    Some(tap_like_miss_event(note.params, note.lane, style)),
                )
            } else if input_clicked && can_enter_judgeable(current_time, judgeable_start) {
                judge_tap_now(note, style, judge_diff)
            } else {
                (*note, None)
            }
        }
        TapState::Judged(_) | TapState::Ended => (*note, None),
    }
}

/// `slideHeadStep`.
pub fn slide_head_step(
    note: &SlideHeadNote,
    current_time: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    style: JudgeStyle,
) -> (SlideHeadNote, Option<JudgeEvent>) {
    let timing = note.params.effective_timing();
    let judgeable_start = time_point_sub_duration(timing, C::JUDGABLE_RANGE);
    let late_limit = time_point_add_duration(timing, C::TAP_GOOD);
    let judge_now = |note: &SlideHeadNote| -> (SlideHeadNote, Option<JudgeEvent>) {
        let raw = judge_tap(judge_diff, note.params.is_ex);
        let grade = convert_grade(style, raw);
        (
            SlideHeadNote { state: TapState::Ended, ..*note },
            Some(tap_like_judge_event(note.params, note.lane, grade, judge_diff)),
        )
    };
    match note.state {
        TapState::Waiting => {
            if is_too_late_for_tap_like(current_time, late_limit) {
                (
                    SlideHeadNote { state: TapState::Ended, ..*note },
                    Some(tap_like_miss_event(note.params, note.lane, style)),
                )
            } else if can_enter_judgeable(current_time, judgeable_start) {
                if input_clicked {
                    judge_now(note)
                } else {
                    (SlideHeadNote { state: TapState::Judgeable, ..*note }, None)
                }
            } else {
                (*note, None)
            }
        }
        TapState::Judgeable => {
            if is_too_late_for_tap_like(current_time, late_limit) {
                (
                    SlideHeadNote { state: TapState::Ended, ..*note },
                    Some(tap_like_miss_event(note.params, note.lane, style)),
                )
            } else if input_clicked && can_enter_judgeable(current_time, judgeable_start) {
                judge_now(note)
            } else {
                (*note, None)
            }
        }
        TapState::Judged(_) | TapState::Ended => (*note, None),
    }
}

/// `tapFamilyStep`.
pub fn tap_family_step(
    note: &TapFamilyNote,
    current_time: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    style: JudgeStyle,
) -> (TapFamilyNote, Option<JudgeEvent>) {
    match note {
        TapFamilyNote::Tap(tap) => {
            let (next, evt) = tap_step(tap, current_time, judge_diff, input_clicked, style);
            (TapFamilyNote::Tap(next), evt)
        }
        TapFamilyNote::SlideHead(head) => {
            let (next, evt) = slide_head_step(head, current_time, judge_diff, input_clicked, style);
            (TapFamilyNote::SlideHead(next), evt)
        }
    }
}

// ---------------------------------------------------------------------------
// Hold
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HoldSubState {
    HeadWaiting,
    HeadJudgeable,
    HeadJudged(JudgeGrade),
    BodyHeld,
    BodyReleased,
    Ended(JudgeGrade),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HoldNote {
    pub params: CommonNoteParams,
    pub start: HoldStart,
    pub state: HoldSubState,
    pub length: Duration,
    pub button_queue_index: usize,
    pub head_diff: Duration,
    pub head_grade: JudgeGrade,
    pub player_release_time: Duration,
    pub release_ignore_time: Duration,
    pub is_classic: bool,
    pub is_touch_hold: bool,
    pub touch_queue_index: usize,
    pub touch_group_id: Option<usize>,
    pub touch_group_size: usize,
    pub touch_hold_group_id: Option<usize>,
    pub touch_hold_group_size: usize,
    pub touch_hold_group_triggered: bool,
}

impl HoldNote {
    pub fn position(&self) -> RuntimePos {
        self.start.to_runtime_pos()
    }
}

fn hold_head_judged(note: &HoldNote, grade: JudgeGrade, head_diff: Duration, group_triggered: bool) -> HoldNote {
    HoldNote {
        state: HoldSubState::HeadJudged(grade),
        head_diff,
        head_grade: grade,
        touch_hold_group_triggered: group_triggered,
        ..*note
    }
}

fn hold_head_miss(note: &HoldNote, head_diff: Duration) -> HoldNote {
    hold_head_judged(note, JudgeGrade::Miss, head_diff, false)
}

fn hold_head_shared(note: &HoldNote, grade: JudgeGrade, head_diff: Duration) -> HoldNote {
    hold_head_judged(note, grade, head_diff, true)
}

fn judge_hold_head_tap_now(note: &HoldNote, style: JudgeStyle, judge_diff: Duration) -> HoldNote {
    let raw = judge_tap(judge_diff, note.params.is_ex);
    let grade = convert_grade(style, raw);
    hold_head_judged(note, grade, judge_diff, false)
}

fn judge_hold_head_touch_now(
    note: &HoldNote,
    style: JudgeStyle,
    judge_diff: Duration,
) -> (HoldNote, Option<JudgeEvent>) {
    match judge_touch(judge_diff, note.params.is_ex) {
        Some(raw) => {
            let grade = convert_grade(style, raw);
            (hold_head_judged(note, grade, judge_diff, false), None)
        }
        None => (*note, None),
    }
}

fn step_touch_hold_head_waiting(
    note: &HoldNote,
    current_time: TimePoint,
    timing: TimePoint,
    judgeable_start: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    shared_result: Option<(JudgeGrade, Duration)>,
    style: JudgeStyle,
) -> (HoldNote, Option<JudgeEvent>) {
    let late = time_point_add_duration(timing, C::TOUCH_GOOD);
    if current_time.to_micros() > late.to_micros() {
        (hold_head_miss(note, C::TOUCH_GOOD), None)
    } else {
        match shared_result {
            Some((grade, diff)) => (hold_head_shared(note, grade, diff), None),
            None => {
                if can_enter_judgeable(current_time, judgeable_start) {
                    if input_clicked {
                        judge_hold_head_touch_now(note, style, judge_diff)
                    } else {
                        (HoldNote { state: HoldSubState::HeadJudgeable, ..*note }, None)
                    }
                } else {
                    (*note, None)
                }
            }
        }
    }
}

fn step_touch_hold_head_judgeable(
    note: &HoldNote,
    current_time: TimePoint,
    timing: TimePoint,
    judgeable_start: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    shared_result: Option<(JudgeGrade, Duration)>,
    style: JudgeStyle,
) -> (HoldNote, Option<JudgeEvent>) {
    let late = time_point_add_duration(timing, C::TOUCH_GOOD);
    if current_time.to_micros() > late.to_micros() {
        (hold_head_miss(note, C::TOUCH_GOOD), None)
    } else {
        match shared_result {
            Some((grade, diff)) => (hold_head_shared(note, grade, diff), None),
            None => {
                if input_clicked && can_enter_judgeable(current_time, judgeable_start) {
                    judge_hold_head_touch_now(note, style, judge_diff)
                } else {
                    (*note, None)
                }
            }
        }
    }
}

fn step_regular_hold_head_waiting(
    note: &HoldNote,
    current_time: TimePoint,
    timing: TimePoint,
    judgeable_start: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    style: JudgeStyle,
) -> (HoldNote, Option<JudgeEvent>) {
    let late = time_point_add_duration(timing, C::TAP_GOOD);
    if current_time.to_micros() > late.to_micros() {
        (hold_head_miss(note, C::TAP_GOOD), None)
    } else if can_enter_judgeable(current_time, judgeable_start) {
        if input_clicked {
            (judge_hold_head_tap_now(note, style, judge_diff), None)
        } else {
            (HoldNote { state: HoldSubState::HeadJudgeable, ..*note }, None)
        }
    } else {
        (*note, None)
    }
}

fn step_regular_hold_head_judgeable(
    note: &HoldNote,
    current_time: TimePoint,
    timing: TimePoint,
    judgeable_start: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    style: JudgeStyle,
) -> (HoldNote, Option<JudgeEvent>) {
    let late = time_point_add_duration(timing, C::TAP_GOOD);
    if input_clicked && can_enter_judgeable(current_time, judgeable_start) {
        (judge_hold_head_tap_now(note, style, judge_diff), None)
    } else if current_time.to_micros() > late.to_micros() {
        (hold_head_miss(note, C::TAP_GOOD), None)
    } else {
        (*note, None)
    }
}

fn head_judged_should_bypass_release_ignore(head_grade: JudgeGrade) -> bool {
    head_grade.is_miss_or_too_fast()
}

fn hold_head_release_transition(note: &HoldNote, delta: Duration) -> (HoldNote, Option<JudgeEvent>) {
    if head_judged_should_bypass_release_ignore(note.head_grade) {
        (
            HoldNote {
                state: HoldSubState::BodyReleased,
                player_release_time: note.player_release_time + delta,
                release_ignore_time: Duration::zero(),
                touch_hold_group_triggered: false,
                ..*note
            },
            None,
        )
    } else if note.release_ignore_time.to_micros() <= C::DELUXE_HOLD_RELEASE_IGNORE_TIME.to_micros() {
        (
            HoldNote {
                release_ignore_time: note.release_ignore_time + delta,
                touch_hold_group_triggered: false,
                ..*note
            },
            None,
        )
    } else {
        (
            HoldNote {
                state: HoldSubState::BodyReleased,
                player_release_time: note.player_release_time + delta,
                touch_hold_group_triggered: false,
                ..*note
            },
            None,
        )
    }
}

fn hold_pressed_transition(note: &HoldNote) -> HoldNote {
    HoldNote {
        state: HoldSubState::BodyHeld,
        player_release_time: Duration::zero(),
        release_ignore_time: Duration::zero(),
        touch_hold_group_triggered: note.is_touch_hold,
        ..*note
    }
}

fn hold_keep_pressed(note: &HoldNote) -> HoldNote {
    HoldNote {
        release_ignore_time: Duration::zero(),
        touch_hold_group_triggered: note.is_touch_hold,
        ..*note
    }
}

fn hold_release_transition(note: &HoldNote, delta: Duration) -> HoldNote {
    if note.release_ignore_time.to_micros() <= C::DELUXE_HOLD_RELEASE_IGNORE_TIME.to_micros() {
        HoldNote {
            release_ignore_time: note.release_ignore_time + delta,
            touch_hold_group_triggered: false,
            ..*note
        }
    } else {
        HoldNote {
            state: HoldSubState::BodyReleased,
            player_release_time: note.player_release_time + delta,
            touch_hold_group_triggered: false,
            ..*note
        }
    }
}

fn hold_released_still_off(note: &HoldNote, delta: Duration) -> HoldNote {
    HoldNote {
        player_release_time: note.player_release_time + delta,
        touch_hold_group_triggered: false,
        ..*note
    }
}

fn hold_released_recovered(note: &HoldNote) -> HoldNote {
    HoldNote {
        state: HoldSubState::BodyHeld,
        release_ignore_time: Duration::zero(),
        touch_hold_group_triggered: note.is_touch_hold,
        ..*note
    }
}

/// `holdStep`.
#[allow(clippy::too_many_arguments)]
pub fn hold_step(
    note: &HoldNote,
    current_time: TimePoint,
    judge_diff: Duration,
    head_ignore: Duration,
    tail_ignore: Duration,
    input_clicked: bool,
    input_pressed: bool,
    current_button_pressed: bool,
    prev_sensor_pressed: bool,
    touch_panel_offset: Duration,
    shared_result: Option<(JudgeGrade, Duration)>,
    delta: Duration,
    style: JudgeStyle,
) -> (HoldNote, Option<JudgeEvent>) {
    let timing = note.params.effective_timing();
    let body_timing = if note.is_touch_hold { note.params.judge_timing } else { timing };
    let diff = (current_time - timing).to_micros();
    let _body_check_start = time_point_add_duration(body_timing, head_ignore);
    let body_check_end = time_point_sub_duration(time_point_add_duration(body_timing, note.length), tail_ignore);
    let classic_body_check_start = time_point_sub_duration(timing, C::TAP_GOOD);
    let body_window_disabled =
        !note.is_classic && note.length.to_micros() <= head_ignore.to_micros() + tail_ignore.to_micros();
    let judgeable_start = time_point_sub_duration(timing, C::JUDGABLE_RANGE);
    let release_offset = if prev_sensor_pressed && !current_button_pressed {
        Duration::zero()
    } else {
        touch_panel_offset
    };

    let end_hold = |note: &HoldNote, head_grade: JudgeGrade, classic_release_timing: TimePoint, release_time: Duration| {
        let final_grade = if note.is_classic {
            judge_hold_classic_end(
                head_grade,
                timing.to_micros(),
                note.length,
                classic_release_timing.to_micros(),
            )
        } else {
            judge_hold_end(
                head_grade,
                note.head_diff,
                note.length,
                head_ignore + tail_ignore,
                release_time,
            )
        };
        let final_grade = convert_grade(style, final_grade);
        let event_diff = if note.head_diff.to_micros() == 0 && head_grade == JudgeGrade::Miss {
            time::from_millis(150)
        } else {
            note.head_diff
        };
        let evt = jd_event(
            JudgeEventKind::Hold,
            final_grade,
            event_diff,
            note.position(),
            note.params.note_index,
            note.params.is_break,
        );
        (
            HoldNote { state: HoldSubState::Ended(final_grade), touch_hold_group_triggered: false, ..*note },
            Some(evt),
        )
    };

    match note.state {
        HoldSubState::HeadWaiting => {
            if note.is_touch_hold {
                step_touch_hold_head_waiting(
                    note, current_time, timing, judgeable_start, judge_diff, input_clicked, shared_result,
                    style,
                )
            } else {
                step_regular_hold_head_waiting(
                    note, current_time, timing, judgeable_start, judge_diff, input_clicked, style,
                )
            }
        }
        HoldSubState::HeadJudgeable => {
            if note.is_touch_hold {
                step_touch_hold_head_judgeable(
                    note, current_time, timing, judgeable_start, judge_diff, input_clicked, shared_result,
                    style,
                )
            } else {
                step_regular_hold_head_judgeable(
                    note, current_time, timing, judgeable_start, judge_diff, input_clicked, style,
                )
            }
        }
        HoldSubState::HeadJudged(head_grade) => {
            if note.is_classic {
                if current_time.to_micros() < classic_body_check_start.to_micros() {
                    (*note, None)
                } else if diff >= note.length.to_micros() + C::CLASSIC_HOLD_ALLOW_OVER_LENGTH.to_micros()
                    || head_grade.is_miss_or_too_fast()
                {
                    end_hold(note, head_grade, current_time, note.player_release_time)
                } else if input_pressed {
                    (
                        HoldNote { state: HoldSubState::BodyHeld, touch_hold_group_triggered: note.is_touch_hold, ..*note },
                        None,
                    )
                } else {
                    end_hold(
                        note,
                        head_grade,
                        time_point_sub_duration(current_time, release_offset),
                        note.player_release_time,
                    )
                }
            } else if current_time.to_micros() < _body_check_start.to_micros() {
                (*note, None)
            } else if body_window_disabled {
                if diff >= note.length.to_micros() {
                    end_hold(note, head_grade, current_time, note.player_release_time)
                } else {
                    (*note, None)
                }
            } else if diff >= note.length.to_micros() {
                end_hold(note, head_grade, current_time, note.player_release_time)
            } else if current_time.to_micros() > body_check_end.to_micros() {
                (*note, None)
            } else if input_pressed {
                (hold_pressed_transition(note), None)
            } else {
                let note = HoldNote { head_grade, ..*note };
                hold_head_release_transition(&note, delta)
            }
        }
        HoldSubState::BodyHeld => {
            if note.is_classic {
                if diff >= note.length.to_micros() + C::CLASSIC_HOLD_ALLOW_OVER_LENGTH.to_micros()
                    || note.head_grade.is_miss_or_too_fast()
                {
                    end_hold(note, note.head_grade, current_time, note.player_release_time)
                } else if input_pressed {
                    (HoldNote { touch_hold_group_triggered: note.is_touch_hold, ..*note }, None)
                } else {
                    end_hold(
                        note,
                        note.head_grade,
                        time_point_sub_duration(current_time, release_offset),
                        note.player_release_time,
                    )
                }
            } else if body_window_disabled {
                if diff >= note.length.to_micros() {
                    end_hold(note, note.head_grade, current_time, note.player_release_time)
                } else {
                    (*note, None)
                }
            } else if diff >= note.length.to_micros() {
                end_hold(note, note.head_grade, current_time, note.player_release_time)
            } else if current_time.to_micros() > body_check_end.to_micros() {
                (*note, None)
            } else if input_pressed {
                (hold_keep_pressed(note), None)
            } else {
                (hold_release_transition(note, delta), None)
            }
        }
        HoldSubState::BodyReleased => {
            if body_window_disabled {
                if diff >= note.length.to_micros() {
                    end_hold(note, note.head_grade, current_time, note.player_release_time)
                } else {
                    (*note, None)
                }
            } else if diff >= note.length.to_micros() {
                end_hold(note, note.head_grade, current_time, note.player_release_time)
            } else if current_time.to_micros() > body_check_end.to_micros() {
                (*note, None)
            } else if input_pressed {
                (hold_released_recovered(note), None)
            } else {
                (hold_released_still_off(note, delta), None)
            }
        }
        HoldSubState::Ended(_) => (*note, None),
    }
}

// ---------------------------------------------------------------------------
// Touch
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TouchState {
    Waiting,
    Judgeable,
    Judged(JudgeGrade),
    Ended,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TouchNote {
    pub params: CommonNoteParams,
    pub state: TouchState,
    pub sensor_pos: SensorArea,
    pub touch_queue_index: usize,
    pub touch_group_id: Option<usize>,
    pub touch_group_size: usize,
}

fn touch_miss_event(note: &TouchNote, judge_diff: Duration) -> JudgeEvent {
    jd_event(
        JudgeEventKind::Touch,
        JudgeGrade::Miss,
        judge_diff,
        RuntimePos::Sensor(note.sensor_pos),
        note.params.note_index,
        note.params.is_break,
    )
}

fn touch_judge_event(note: &TouchNote, grade: JudgeGrade, judge_diff: Duration) -> JudgeEvent {
    jd_event(
        JudgeEventKind::Touch,
        grade,
        judge_diff,
        RuntimePos::Sensor(note.sensor_pos),
        note.params.note_index,
        note.params.is_break,
    )
}

fn judge_touch_now(
    note: &TouchNote,
    style: JudgeStyle,
    judge_diff: Duration,
) -> (TouchNote, Option<JudgeEvent>) {
    match judge_touch(judge_diff, note.params.is_ex) {
        Some(raw) => {
            let grade = convert_grade(style, raw);
            (
                TouchNote { state: TouchState::Ended, ..*note },
                Some(touch_judge_event(note, grade, judge_diff)),
            )
        }
        None => (TouchNote { state: TouchState::Judgeable, ..*note }, None),
    }
}

/// `touchStep`.
pub fn touch_step(
    note: &TouchNote,
    current_time: TimePoint,
    judge_diff: Duration,
    input_clicked: bool,
    shared_result: Option<(JudgeGrade, Duration)>,
    style: JudgeStyle,
) -> (TouchNote, Option<JudgeEvent>) {
    let timing = note.params.effective_timing();
    let judgeable_start = time_point_sub_duration(timing, C::JUDGABLE_RANGE);
    let late = time_point_add_duration(timing, C::TOUCH_GOOD);
    match note.state {
        TouchState::Waiting => {
            if current_time.to_micros() > late.to_micros() {
                (
                    TouchNote { state: TouchState::Ended, ..*note },
                    Some(touch_miss_event(note, Duration::from_micros(-1000))),
                )
            } else {
                match shared_result {
                    Some((grade, diff)) => (
                        TouchNote { state: TouchState::Ended, ..*note },
                        Some(touch_judge_event(note, grade, diff)),
                    ),
                    None => {
                        if can_enter_judgeable(current_time, judgeable_start) {
                            if input_clicked {
                                judge_touch_now(note, style, judge_diff)
                            } else {
                                (TouchNote { state: TouchState::Judgeable, ..*note }, None)
                            }
                        } else {
                            (*note, None)
                        }
                    }
                }
            }
        }
        TouchState::Judgeable => {
            if current_time.to_micros() > late.to_micros() {
                (
                    TouchNote { state: TouchState::Ended, ..*note },
                    Some(touch_miss_event(note, Duration::from_micros(-1000))),
                )
            } else {
                match shared_result {
                    Some((grade, diff)) => (
                        TouchNote { state: TouchState::Ended, ..*note },
                        Some(touch_judge_event(note, grade, diff)),
                    ),
                    None => {
                        if input_clicked {
                            match judge_touch(judge_diff, note.params.is_ex) {
                                Some(raw) => {
                                    let grade = convert_grade(style, raw);
                                    (
                                        TouchNote { state: TouchState::Ended, ..*note },
                                        Some(touch_judge_event(note, grade, judge_diff)),
                                    )
                                }
                                None => (*note, None),
                            }
                        } else {
                            (*note, None)
                        }
                    }
                }
            }
        }
        TouchState::Judged(_) | TouchState::Ended => (*note, None),
    }
}

// ---------------------------------------------------------------------------
// Slide queue core
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideState {
    Waiting,
    Active(Duration),
    Judged(JudgeGrade, Duration, Duration),
    Ended,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SlideArea {
    pub target_areas: Vec<SensorArea>,
    pub policy: AreaPolicy,
    pub is_last: bool,
    pub is_skippable: bool,
    pub arrow_progress_when_on: usize,
    pub arrow_progress_when_finished: usize,
    pub was_on: bool,
    pub was_off: bool,
}

impl SlideArea {
    pub fn on(&self) -> bool {
        self.was_on
    }

    pub fn is_finished(&self) -> bool {
        if self.is_last {
            self.was_on
        } else {
            self.was_on && self.was_off
        }
    }

    pub fn check(&self, sensor_held: &SensorVec<bool>) -> SlideArea {
        let is_held = match self.policy {
            AreaPolicy::Or => self.target_areas.iter().any(|t| sensor_held.get_d(*t, false)),
            AreaPolicy::And => self.target_areas.iter().all(|t| sensor_held.get_d(*t, false)),
        };
        if is_held {
            SlideArea { was_on: true, ..self.clone() }
        } else if self.was_on {
            SlideArea { was_off: true, ..self.clone() }
        } else {
            self.clone()
        }
    }
}

pub type SlideQueue = Vec<SlideArea>;

pub fn slide_queue_remaining(queues: &[SlideQueue]) -> usize {
    queues.iter().map(|q| q.len()).max().unwrap_or(0)
}

fn wifi_queue_progress_remaining(is_classic: bool, queues: &[SlideQueue]) -> usize {
    match queues {
        [left, center, right] => {
            if is_classic {
                if left.len() <= 1 && center.len() <= 1 && right.len() <= 1 {
                    8
                } else {
                    let max_len = left.len().max(center.len()).max(right.len());
                    let pick = |cands: [&SlideQueue; 3]| -> usize {
                        cands
                            .iter()
                            .find(|q| q.len() == max_len)
                            .and_then(|q| q.first())
                            .map(|area| area.arrow_progress_when_finished)
                            .unwrap_or(0)
                    };
                    pick([left, center, right])
                }
            } else if center.is_empty() && left.len() <= 1 && right.len() <= 1 {
                9
            } else {
                let max_len = left.len().max(center.len()).max(right.len());
                let pick = |cands: [&SlideQueue; 3]| -> usize {
                    cands
                        .iter()
                        .find(|q| q.len() == max_len)
                        .and_then(|q| q.first())
                        .map(|area| area.arrow_progress_when_finished)
                        .unwrap_or(0)
                };
                pick([left, center, right])
            }
        }
        _ => slide_queue_remaining(queues),
    }
}

fn slide_progress_remaining(slide_kind: SlideKind, is_classic: bool, queues: &[SlideQueue]) -> usize {
    match slide_kind {
        SlideKind::Wifi => wifi_queue_progress_remaining(is_classic, queues),
        _ => slide_queue_remaining(queues),
    }
}

pub fn slide_queues_cleared(queues: &[SlideQueue]) -> bool {
    match queues {
        [] => true,
        [q, rest @ ..] => {
            if q.is_empty() {
                slide_queues_cleared(rest)
            } else {
                false
            }
        }
    }
}

fn slide_head_on(queue: &SlideQueue) -> bool {
    queue.first().map(|a| a.on()).unwrap_or(false)
}

fn slide_hide_bar_cmd(note_index: usize, track_index: Option<usize>, end_index: usize) -> RenderCommand {
    match track_index {
        None => RenderCommand::HideSlideBars { note_index, end_index },
        Some(track_index) => RenderCommand::HideSlideTrackBars { note_index, track_index, end_index },
    }
}

fn flatten_render_cmds(cmds: Vec<Vec<RenderCommand>>) -> Vec<RenderCommand> {
    cmds.into_iter().flatten().collect()
}

fn collect_new_slide_on_tracks(index: usize, old_queues: &[SlideQueue], new_queues: &[SlideQueue]) -> Vec<usize> {
    match (old_queues.split_first(), new_queues.split_first()) {
        (Some((old_q, old_rest)), Some((new_q, new_rest))) => {
            let mut rest = collect_new_slide_on_tracks(index + 1, old_rest, new_rest);
            if slide_head_on(new_q) && !slide_head_on(old_q) {
                let mut out = vec![index];
                out.append(&mut rest);
                out
            } else {
                rest
            }
        }
        _ => Vec::new(),
    }
}

pub fn update_slide_area(area: &SlideArea, sensor_held: &SensorVec<bool>) -> SlideArea {
    area.check(sensor_held)
}

pub fn flatten_slide_queues(queues: &[SlideQueue]) -> SlideQueue {
    queues.iter().flatten().cloned().collect()
}

fn slide_queue_core_fuel(
    fuel: usize,
    note_index: usize,
    track_index: Option<usize>,
    emit_cmds: bool,
    queue: &[SlideArea],
    sensor_held: &SensorVec<bool>,
) -> (SlideQueue, Vec<RenderCommand>) {
    let (first, rest) = match queue.split_first() {
        None => return (Vec::new(), Vec::new()),
        Some((f, r)) => (f, r),
    };
    if fuel == 0 {
        return (queue.to_vec(), Vec::new());
    }
    let first = update_slide_area(first, sensor_held);
    match rest.split_first() {
        None => {
            if first.is_finished() {
                let cmds = if emit_cmds {
                    vec![slide_hide_bar_cmd(note_index, track_index, first.arrow_progress_when_finished)]
                } else {
                    Vec::new()
                };
                (Vec::new(), cmds)
            } else if first.on() {
                let cmds = if emit_cmds {
                    vec![slide_hide_bar_cmd(note_index, track_index, first.arrow_progress_when_on)]
                } else {
                    Vec::new()
                };
                (vec![first], cmds)
            } else {
                (vec![first], Vec::new())
            }
        }
        Some((second, rest2)) => {
            if first.is_skippable || first.on() {
                let second = update_slide_area(second, sensor_held);
                if second.is_finished() {
                    let (rest_queue, rest_cmds) =
                        slide_queue_core_fuel(fuel, note_index, track_index, emit_cmds, rest2, sensor_held);
                    let cmds = if emit_cmds {
                        let mut c =
                            vec![slide_hide_bar_cmd(note_index, track_index, second.arrow_progress_when_finished)];
                        c.extend(rest_cmds);
                        c
                    } else {
                        rest_cmds
                    };
                    (rest_queue, cmds)
                } else if second.on() {
                    let mut next: SlideQueue = vec![second.clone()];
                    next.extend_from_slice(rest2);
                    let (rest_queue, rest_cmds) =
                        slide_queue_core_fuel(fuel, note_index, track_index, emit_cmds, &next, sensor_held);
                    let cmds = if emit_cmds {
                        let mut c = vec![slide_hide_bar_cmd(note_index, track_index, second.arrow_progress_when_on)];
                        c.extend(rest_cmds);
                        c
                    } else {
                        rest_cmds
                    };
                    (rest_queue, cmds)
                } else if first.is_finished() {
                    let mut next: SlideQueue = vec![second.clone()];
                    next.extend_from_slice(rest2);
                    let (rest_queue, rest_cmds) =
                        slide_queue_core_fuel(fuel, note_index, track_index, emit_cmds, &next, sensor_held);
                    let cmds = if emit_cmds {
                        let mut c =
                            vec![slide_hide_bar_cmd(note_index, track_index, first.arrow_progress_when_finished)];
                        c.extend(rest_cmds);
                        c
                    } else {
                        rest_cmds
                    };
                    (rest_queue, cmds)
                } else {
                    let mut out = vec![first, second];
                    out.extend_from_slice(rest2);
                    (out, Vec::new())
                }
            } else if first.is_finished() {
                let (rest_queue, rest_cmds) =
                    slide_queue_core_fuel(fuel, note_index, track_index, emit_cmds, rest, sensor_held);
                let cmds = if emit_cmds {
                    let mut c = vec![slide_hide_bar_cmd(note_index, track_index, first.arrow_progress_when_finished)];
                    c.extend(rest_cmds);
                    c
                } else {
                    rest_cmds
                };
                (rest_queue, cmds)
            } else {
                let mut out = vec![first];
                out.extend_from_slice(rest);
                (out, Vec::new())
            }
        }
    }
}

fn slide_queue_core(
    note_index: usize,
    track_index: Option<usize>,
    emit_cmds: bool,
    queue: &SlideQueue,
    sensor_held: &SensorVec<bool>,
) -> (SlideQueue, Vec<RenderCommand>) {
    slide_queue_core_fuel(queue.len() + 1, note_index, track_index, emit_cmds, queue, sensor_held)
}

pub fn replay_slide_queue(queue: &SlideQueue, sensor_held: &SensorVec<bool>) -> SlideQueue {
    slide_queue_core(0, None, false, queue, sensor_held).0
}

fn update_slide_queue(
    note_index: usize,
    track_index: Option<usize>,
    queue: &SlideQueue,
    sensor_held: &SensorVec<bool>,
) -> (SlideQueue, Vec<RenderCommand>) {
    slide_queue_core(note_index, track_index, true, queue, sensor_held)
}

// ---------------------------------------------------------------------------
// Slide note stepping
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SlideNote {
    pub params: CommonNoteParams,
    pub lane: OuterSlot,
    pub state: SlideState,
    pub length: Duration,
    pub head_timing: TimePoint,
    pub start_timing: TimePoint,
    pub group_start_timing: Option<TimePoint>,
    pub slide_kind: SlideKind,
    pub is_classic: bool,
    pub is_conn_slide: bool,
    pub parent_note_index: Option<usize>,
    pub is_group_part_head: bool,
    pub is_group_part_end: bool,
    pub parent_finished: bool,
    pub parent_pending_finish: bool,
    pub initial_queue_remaining: usize,
    pub total_judge_queue_len: usize,
    pub track_count: usize,
    pub is_checkable: bool,
    pub multiple: usize,
    pub judge_queues: Vec<SlideQueue>,
}

impl SlideNote {
    pub fn position(&self) -> RuntimePos {
        RuntimePos::Button(self.lane.to_button_zone())
    }

    fn queue_tracks(&self) -> Vec<(Option<usize>, SlideQueue)> {
        if self.track_count == 1 {
            self.judge_queues.iter().map(|q| (None, q.clone())).collect()
        } else {
            (0..self.judge_queues.len()).map(Some).zip(self.judge_queues.clone()).collect()
        }
    }

    fn track_render_indices(&self) -> Vec<usize> {
        (0..self.track_count).collect()
    }
}

fn slide_progress_render_cmds(note: &SlideNote, remaining: usize) -> Vec<RenderCommand> {
    match note.slide_kind {
        SlideKind::Single => vec![RenderCommand::UpdateSlideProgress {
            note_index: note.params.note_index,
            remaining,
        }],
        SlideKind::Wifi | SlideKind::ConnPart => note
            .track_render_indices()
            .into_iter()
            .map(|track_index| RenderCommand::UpdateSlideTrackProgress {
                note_index: note.params.note_index,
                track_index,
                remaining,
            })
            .collect(),
    }
}

fn slide_hide_render_cmds(note: &SlideNote) -> Vec<RenderCommand> {
    vec![RenderCommand::HideAllSlideBars { note_index: note.params.note_index }]
}

struct SlideStepContext<'a> {
    current_time: TimePoint,
    touch_panel_offset: Duration,
    delta: Duration,
    style: JudgeStyle,
    subdivide_slide_judge_grade: bool,
    sensor_held: &'a SensorVec<bool>,
}

#[derive(Clone)]
#[allow(dead_code)]
struct SlideStepSemantic {
    note: SlideNote,
    event: Option<JudgeEvent>,
    queue_render_cmds: Vec<RenderCommand>,
    old_remaining: usize,
    new_remaining: usize,
    track_ons: Vec<usize>,
    progress_changed: bool,
    hide_slide: bool,
    should_play_track_ons: bool,
    emit_progress_render: bool,
}

fn slide_effective_judge_grade(style: JudgeStyle, subdivide: bool, raw: JudgeGrade) -> JudgeGrade {
    let converted = convert_grade(style, raw);
    if subdivide {
        converted
    } else {
        correct_slide_grade(converted)
    }
}

fn slide_current_judge_diff(note: &SlideNote, current_time: TimePoint, touch_panel_offset: Duration) -> Duration {
    time_point_sub_duration(current_time, touch_panel_offset) - note.params.effective_timing()
}

fn slide_initial_wait_time(note: &SlideNote) -> Duration {
    time_point_add_duration(note.start_timing, note.length) - note.params.judge_timing
}

fn slide_should_be_checkable(note: &SlideNote, current_time: TimePoint) -> bool {
    let head_timing = current_time - note.head_timing;
    if note.is_checkable {
        true
    } else if note.is_conn_slide {
        if note.is_group_part_head {
            head_timing.to_micros() >= -50000
        } else {
            note.parent_finished || note.parent_pending_finish
        }
    } else {
        head_timing.to_micros() >= -50000
    }
}

fn slide_updated_queues_with_cmds(
    note: &SlideNote,
    is_checkable: bool,
    sensor_held: &SensorVec<bool>,
) -> Vec<(SlideQueue, Vec<RenderCommand>)> {
    if is_checkable {
        note.queue_tracks()
            .into_iter()
            .map(|(track_index, queue)| update_slide_queue(note.params.note_index, track_index, &queue, sensor_held))
            .collect()
    } else {
        note.judge_queues.iter().map(|q| (q.clone(), Vec::new())).collect()
    }
}

fn slide_too_late_timing(note: &SlideNote) -> TimePoint {
    let base = time_point_add_duration(time_point_add_duration(note.start_timing, note.length), C::SLIDE_GOOD);
    let min_offset = if note.params.judge_offset.to_micros() < 0 {
        note.params.judge_offset
    } else {
        Duration::zero()
    };
    time_point_add_duration(base, min_offset)
}

fn slide_adjusted_judged_wait_time(
    note: &SlideNote,
    current_time: TimePoint,
    wait_time: Duration,
    judge_diff: Duration,
) -> Duration {
    let start = note.group_start_timing.unwrap_or(note.start_timing);
    let remaining_start_time = current_time - start;
    if remaining_start_time.to_micros() < 0 {
        Duration::div_nat(Duration::abs(remaining_start_time), 2)
    } else if !note.is_classic && judge_diff.to_micros() >= C::SLIDE_GOOD.to_micros() {
        C::SLIDE_JUDGED_LATE_CLEAR_WAIT
    } else {
        wait_time
    }
}

fn slide_judge_event(note: &SlideNote, grade: JudgeGrade, judge_diff: Duration) -> JudgeEvent {
    JudgeEvent {
        kind: JudgeEventKind::Slide,
        grade,
        diff: judge_diff,
        position: note.position(),
        note_index: note.params.note_index,
        is_break: note.params.is_break,
        multiple: note.multiple.max(1),
    }
}

fn build_slide_semantic_base(
    note: &SlideNote,
    updated_queues: Vec<SlideQueue>,
    queue_render_cmds: Vec<RenderCommand>,
    old_remaining: usize,
    new_remaining: usize,
    track_ons: Vec<usize>,
) -> SlideStepSemantic {
    let progress_changed = new_remaining != old_remaining
        || slide_queue_remaining(&updated_queues) != slide_queue_remaining(&note.judge_queues);
    SlideStepSemantic {
        note: SlideNote { judge_queues: updated_queues, ..note.clone() },
        event: None,
        queue_render_cmds,
        old_remaining,
        new_remaining,
        track_ons,
        progress_changed,
        hide_slide: false,
        should_play_track_ons: false,
        emit_progress_render: false,
    }
}

fn build_slide_static_semantic_base(note: &SlideNote, is_checkable: bool) -> SlideStepSemantic {
    let remaining = slide_progress_remaining(note.slide_kind, note.is_classic, &note.judge_queues);
    SlideStepSemantic {
        note: SlideNote { is_checkable, ..note.clone() },
        event: None,
        queue_render_cmds: Vec::new(),
        old_remaining: remaining,
        new_remaining: remaining,
        track_ons: Vec::new(),
        progress_changed: false,
        hide_slide: false,
        should_play_track_ons: false,
        emit_progress_render: false,
    }
}

fn build_slide_sensor_semantic_base(
    note: &SlideNote,
    ctx: &SlideStepContext,
    is_checkable: bool,
) -> SlideStepSemantic {
    let updated = slide_updated_queues_with_cmds(note, is_checkable, ctx.sensor_held);
    let updated_queues: Vec<SlideQueue> = updated.iter().map(|(q, _)| q.clone()).collect();
    let queue_render_cmds = flatten_render_cmds(updated.iter().map(|(_, c)| c.clone()).collect());
    let old_remaining = slide_progress_remaining(note.slide_kind, note.is_classic, &note.judge_queues);
    let new_remaining = slide_progress_remaining(note.slide_kind, note.is_classic, &updated_queues);
    let track_ons = if is_checkable {
        collect_new_slide_on_tracks(0, &note.judge_queues, &updated_queues)
    } else {
        Vec::new()
    };
    let note_with = SlideNote { is_checkable, ..note.clone() };
    build_slide_semantic_base(&note_with, updated_queues, queue_render_cmds, old_remaining, new_remaining, track_ons)
}

fn build_slide_dormant_semantic_base(note: &SlideNote) -> SlideStepSemantic {
    SlideStepSemantic {
        note: SlideNote { state: SlideState::Waiting, is_checkable: false, ..note.clone() },
        event: None,
        queue_render_cmds: Vec::new(),
        old_remaining: 0,
        new_remaining: 0,
        track_ons: Vec::new(),
        progress_changed: false,
        hide_slide: false,
        should_play_track_ons: false,
        emit_progress_render: false,
    }
}

fn slide_active_step_semantic(
    note: &SlideNote,
    ctx: &SlideStepContext,
    is_judgable: bool,
    wait_time: Duration,
) -> SlideStepSemantic {
    let active_note = SlideNote { state: SlideState::Active(wait_time), is_checkable: true, ..note.clone() };
    let static_base = build_slide_static_semantic_base(&active_note, true);
    let is_too_late = ctx.current_time.to_micros() > slide_too_late_timing(&active_note).to_micros();
    if is_judgable && slide_queues_cleared(&active_note.judge_queues) {
        let judge_diff = slide_current_judge_diff(&active_note, ctx.current_time, ctx.touch_panel_offset);
        let raw = if active_note.is_classic {
            judge_slide_classic(judge_diff)
        } else {
            judge_slide_modern(judge_diff, wait_time, active_note.params.is_ex)
        };
        let stored_grade = if active_note.is_classic { raw } else { convert_grade(ctx.style, raw) };
        let judged_wait_time = slide_adjusted_judged_wait_time(&active_note, ctx.current_time, wait_time, judge_diff);
        SlideStepSemantic {
            note: SlideNote {
                state: SlideState::Judged(stored_grade, judged_wait_time, judge_diff),
                ..static_base.note.clone()
            },
            should_play_track_ons: active_note.is_group_part_head || !active_note.is_conn_slide,
            emit_progress_render: true,
            ..static_base
        }
    } else if is_judgable && is_too_late {
        let raw = judge_slide_too_late(slide_queue_remaining(&active_note.judge_queues) as u32);
        let grade = slide_effective_judge_grade(ctx.style, ctx.subdivide_slide_judge_grade, raw);
        SlideStepSemantic {
            note: SlideNote { state: SlideState::Ended, ..static_base.note.clone() },
            event: Some(slide_judge_event(&active_note, grade, Duration::from_micros(-1000))),
            hide_slide: true,
            ..static_base
        }
    } else {
        let semantic_base = build_slide_sensor_semantic_base(&active_note, ctx, true);
        SlideStepSemantic {
            should_play_track_ons: active_note.is_group_part_head || !active_note.is_conn_slide,
            emit_progress_render: semantic_base.progress_changed,
            ..semantic_base
        }
    }
}

fn slide_step_semantic(note: &SlideNote, ctx: &SlideStepContext) -> SlideStepSemantic {
    let is_checkable = slide_should_be_checkable(note, ctx.current_time);
    let is_judgable = note.is_group_part_end || !note.is_conn_slide;
    match note.state {
        SlideState::Waiting => {
            if is_checkable {
                slide_active_step_semantic(note, ctx, is_judgable, slide_initial_wait_time(note))
            } else {
                build_slide_dormant_semantic_base(note)
            }
        }
        SlideState::Active(wait_time) => {
            if !is_checkable {
                build_slide_dormant_semantic_base(note)
            } else {
                slide_active_step_semantic(note, ctx, is_judgable, wait_time)
            }
        }
        SlideState::Judged(grade, wait_time, stored_judge_diff) => {
            let static_base = build_slide_static_semantic_base(note, is_checkable);
            if wait_time.to_micros() <= 0 {
                let final_grade = slide_effective_judge_grade(ctx.style, ctx.subdivide_slide_judge_grade, grade);
                SlideStepSemantic {
                    note: SlideNote { state: SlideState::Ended, ..static_base.note.clone() },
                    event: Some(slide_judge_event(note, final_grade, stored_judge_diff)),
                    hide_slide: true,
                    ..static_base
                }
            } else {
                let new_wait = wait_time - ctx.delta;
                SlideStepSemantic {
                    note: SlideNote {
                        state: SlideState::Judged(grade, new_wait, stored_judge_diff),
                        ..static_base.note.clone()
                    },
                    should_play_track_ons: note.is_group_part_head || !note.is_conn_slide,
                    emit_progress_render: static_base.progress_changed,
                    ..static_base
                }
            }
        }
        SlideState::Ended => {
            let static_base = build_slide_static_semantic_base(note, is_checkable);
            SlideStepSemantic {
                note: SlideNote { state: SlideState::Ended, ..static_base.note.clone() },
                ..static_base
            }
        }
    }
}

fn slide_semantic_audio_cmds(semantic: &SlideStepSemantic, current_time: TimePoint) -> Vec<AudioCommand> {
    if semantic.should_play_track_ons {
        semantic
            .track_ons
            .iter()
            .map(|track_index| AudioCommand::PlaySlideCue {
                note_index: semantic.note.params.note_index,
                track_index: *track_index,
                is_break: semantic.note.params.is_break,
                at_time: current_time,
            })
            .collect()
    } else {
        Vec::new()
    }
}

fn slide_semantic_render_cmds(semantic: &SlideStepSemantic) -> Vec<RenderCommand> {
    let progress_cmds = if semantic.emit_progress_render {
        slide_progress_render_cmds(&semantic.note, semantic.new_remaining)
    } else {
        Vec::new()
    };
    let hide_cmds = if semantic.hide_slide {
        slide_hide_render_cmds(&semantic.note)
    } else {
        Vec::new()
    };
    let mut out = semantic.queue_render_cmds.clone();
    out.extend(progress_cmds);
    out.extend(hide_cmds);
    out
}

/// `slideStep`.
pub fn slide_step(
    note: &SlideNote,
    current_time: TimePoint,
    sensor_held: &SensorVec<bool>,
    touch_panel_offset: Duration,
    delta: Duration,
    style: JudgeStyle,
    subdivide_slide_judge_grade: bool,
) -> (SlideNote, Option<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>) {
    let ctx = SlideStepContext {
        current_time,
        touch_panel_offset,
        delta,
        style,
        subdivide_slide_judge_grade,
        sensor_held,
    };
    let semantic = slide_step_semantic(note, &ctx);
    let audio_cmds = match semantic.note.state {
        SlideState::Ended => Vec::new(),
        _ => slide_semantic_audio_cmds(&semantic, current_time),
    };
    let render_cmds = match semantic.note.state {
        SlideState::Ended => {
            if semantic.hide_slide {
                slide_semantic_render_cmds(&semantic)
            } else {
                Vec::new()
            }
        }
        _ => slide_semantic_render_cmds(&semantic),
    };
    (semantic.note, semantic.event, audio_cmds, render_cmds)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn params(timing_us: i64) -> CommonNoteParams {
        CommonNoteParams {
            judge_timing: TimePoint::from_micros(timing_us),
            judge_offset: Duration::zero(),
            is_break: false,
            is_ex: false,
            note_index: 1,
        }
    }

    #[test]
    fn tap_clicks_perfect() {
        let note = TapNote {
            params: params(1_000_000),
            lane: OuterSlot::S1,
            state: TapState::Waiting,
            button_queue_index: 0,
        };
        let (next, evt) = tap_step(&note, TimePoint::from_micros(1_000_000), Duration::zero(), true, JudgeStyle::Default);
        assert_eq!(next.state, TapState::Ended);
        assert_eq!(evt.unwrap().grade, JudgeGrade::Perfect);
    }

    #[test]
    fn tap_too_late_is_miss() {
        let note = TapNote {
            params: params(0),
            lane: OuterSlot::S2,
            state: TapState::Waiting,
            button_queue_index: 0,
        };
        let (_next, evt) = tap_step(&note, TimePoint::from_micros(400_000), Duration::zero(), false, JudgeStyle::Default);
        assert_eq!(evt.unwrap().grade, JudgeGrade::Miss);
    }

    #[test]
    fn touch_too_early_stays() {
        let note = TouchNote {
            params: params(0),
            state: TouchState::Judgeable,
            sensor_pos: SensorArea::A1,
            touch_queue_index: 0,
            touch_group_id: None,
            touch_group_size: 1,
        };
        // Very fast hit beyond the 1st perfect window → judgeTouch returns none.
        let (_next, evt) = touch_step(&note, TimePoint::from_micros(-200_000), Duration::from_micros(-200_000), true, None, JudgeStyle::Default);
        assert_eq!(evt, None);
    }

    #[test]
    fn slide_queue_remaining_uses_max() {
        let mk = |n: usize| -> SlideQueue { (0..n).map(|_| SlideArea {
            target_areas: vec![SensorArea::A1],
            policy: AreaPolicy::Or,
            is_last: false,
            is_skippable: true,
            arrow_progress_when_on: 0,
            arrow_progress_when_finished: 0,
            was_on: false,
            was_off: false,
        }).collect() };
        assert_eq!(slide_queue_remaining(&[mk(3), mk(5), mk(1)]), 5);
    }
}
