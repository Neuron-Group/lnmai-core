//! Frame scheduler: advance all active notes one frame.
//!
//! Mirrors `LnmaiCore/Scheduler.lean`. The subsystem processing order
//! (tap → hold → touch → touch-hold → slide) is part of the observable
//! semantics and is preserved exactly.

use crate::areas::{ButtonZone, SensorArea};
use crate::constants as C;
use crate::events::{AudioCommand, RenderCommand};
use crate::input_model::{
    button_queue_at, sensor_queue_at, set_button_queue_at, set_sensor_queue_at, FrameInput,
    GameState, TimedInputBatch, ZoneQueue,
};
use crate::lifecycle::{
    hold_step, slide_queue_remaining, slide_step, tap_family_step, touch_step, HoldNote,
    HoldSubState, SlideNote, TapFamilyNote, TapState, TouchNote, TouchState,
};
use crate::runtime_score::{fold_events_into_score, RuntimeScoreState};
use crate::storage::{ButtonVec, SensorVec};
use crate::time::{time_point_add_duration, time_point_sub_duration, Duration, TimePoint};
use crate::types::{GroupState, JudgeEvent, JudgeGrade};
use crate::input_model::TouchHoldBodyGroupState;

fn list_set_at<T: Clone>(items: &[T], index: usize, value: T) -> Vec<T> {
    let mut out = items.to_vec();
    if index < out.len() {
        out[index] = value;
    }
    out
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClickCursor {
    pub button_used: ButtonVec<usize>,
    pub sensor_used: SensorVec<usize>,
}

impl Default for ClickCursor {
    fn default() -> Self {
        ClickCursor {
            button_used: ButtonVec::replicate(C::BUTTON_ZONE_COUNT as usize, 0),
            sensor_used: SensorVec::replicate(C::SENSOR_AREA_COUNT as usize, 0),
        }
    }
}

fn try_use_button_click_at(input: &FrameInput, cursor: &ClickCursor, zone: ButtonZone) -> (bool, ClickCursor) {
    let used = cursor.button_used.get_d(zone, 0);
    let available = input.get_button_click_count(zone);
    if used < available {
        (true, ClickCursor { button_used: cursor.button_used.set(zone, used + 1), ..cursor.clone() })
    } else {
        (false, cursor.clone())
    }
}

fn try_use_sensor_click_at(input: &FrameInput, cursor: &ClickCursor, area: SensorArea) -> (bool, ClickCursor) {
    let used = cursor.sensor_used.get_d(area, 0);
    let available = input.get_sensor_click_count(area);
    if used < available {
        (true, ClickCursor { sensor_used: cursor.sensor_used.set(area, used + 1), ..cursor.clone() })
    } else {
        (false, cursor.clone())
    }
}

fn has_unused_button_click_at(input: &FrameInput, cursor: &ClickCursor, zone: ButtonZone) -> bool {
    cursor.button_used.get_d(zone, 0) < input.get_button_click_count(zone)
}

fn has_unused_sensor_click_at(input: &FrameInput, cursor: &ClickCursor, area: SensorArea) -> bool {
    cursor.sensor_used.get_d(area, 0) < input.get_sensor_click_count(area)
}

fn fallback_sensor_area_for_button_note(zone: ButtonZone) -> SensorArea {
    zone.to_outer_sensor_area()
}

fn fallback_sensor_held_for_button_note(input: &FrameInput, zone: ButtonZone) -> bool {
    input.get_sensor_held(fallback_sensor_area_for_button_note(zone))
}

fn fallback_prev_sensor_held_for_button_note(prev_sensor: &SensorVec<bool>, zone: ButtonZone) -> bool {
    prev_sensor.get_d(fallback_sensor_area_for_button_note(zone), false)
}

fn hold_body_pressed_from_button_or_sensor(cur_btn: bool, cur_sensor: bool) -> bool {
    cur_btn || cur_sensor
}

fn consume_button_then_fallback_sensor(
    input: &FrameInput,
    cursor: &ClickCursor,
    allow_input: bool,
    zone: ButtonZone,
    sensor_area: SensorArea,
) -> (bool, bool, ClickCursor) {
    let (used_button, cursor1) = if allow_input {
        try_use_button_click_at(input, cursor, zone)
    } else {
        (false, cursor.clone())
    };
    let (used_sensor, cursor2) = if allow_input && !used_button {
        try_use_sensor_click_at(input, &cursor1, sensor_area)
    } else {
        (false, cursor1)
    };
    (used_button, used_sensor, cursor2)
}

fn tap_family_queue_has_unused_click(
    input: &FrameInput,
    cursor: &ClickCursor,
    zone: ButtonZone,
    fallback_area: SensorArea,
) -> bool {
    has_unused_button_click_at(input, cursor, zone) || has_unused_sensor_click_at(input, cursor, fallback_area)
}

fn slide_remaining(slide: &SlideNote) -> usize {
    slide_queue_remaining(&slide.judge_queues)
}

fn empty_slide_queues(slide: &SlideNote) -> SlideNote {
    SlideNote {
        judge_queues: slide.judge_queues.iter().map(|_| Vec::new()).collect(),
        ..slide.clone()
    }
}

fn update_slide_parent_flags(slides: &[SlideNote]) -> Vec<SlideNote> {
    let statuses: Vec<(usize, usize)> = slides.iter().map(|s| (s.params.note_index, slide_remaining(s))).collect();
    let find_remaining = |note_index: usize| -> Option<usize> {
        statuses.iter().find(|(idx, _)| *idx == note_index).map(|(_, r)| *r)
    };
    slides
        .iter()
        .map(|slide| match slide.parent_note_index {
            None => SlideNote { parent_finished: false, parent_pending_finish: false, ..slide.clone() },
            Some(parent_index) => match find_remaining(parent_index) {
                None => SlideNote { parent_finished: false, parent_pending_finish: false, ..slide.clone() },
                Some(remaining) => SlideNote {
                    parent_finished: remaining == 0,
                    parent_pending_finish: remaining == 1,
                    ..slide.clone()
                },
            },
        })
        .collect()
}

fn force_finish_parent_slides(slides: &[SlideNote]) -> Vec<SlideNote> {
    let mut child_requests: Vec<usize> = Vec::new();
    for child in slides {
        if let Some(parent_index) = child.parent_note_index {
            if slide_remaining(child) < child.initial_queue_remaining && !child.parent_finished {
                child_requests.push(parent_index);
            }
        }
    }
    slides
        .iter()
        .map(|slide| {
            if slide.is_conn_slide
                && !slide.is_group_part_end
                && child_requests.contains(&slide.params.note_index)
            {
                empty_slide_queues(slide)
            } else {
                slide.clone()
            }
        })
        .collect()
}

fn hide_slide_render_cmds(slide: &SlideNote) -> Vec<RenderCommand> {
    vec![RenderCommand::HideAllSlideBars { note_index: slide.params.note_index }]
}

fn force_finish_render_cmds(before: &[SlideNote], after: &[SlideNote]) -> Vec<RenderCommand> {
    let mut out = Vec::new();
    for (b, a) in before.iter().zip(after.iter()) {
        if b.is_conn_slide && !b.is_group_part_end && slide_remaining(b) > 0 && slide_remaining(a) == 0 {
            out.extend(hide_slide_render_cmds(a));
        }
    }
    out
}

fn tap_eligible_for_click(note: &TapFamilyNote, current_time: TimePoint) -> bool {
    let timing = note.params().effective_timing();
    current_time.to_micros() >= time_point_sub_duration(timing, C::JUDGABLE_RANGE).to_micros()
        && current_time.to_micros() <= time_point_add_duration(timing, C::TAP_GOOD).to_micros()
}

fn button_queue_index_unlocked(frontiers: &ButtonVec<usize>, zone: ButtonZone, index: usize) -> bool {
    index <= frontiers.get_d(zone, 0)
}

fn advance_shared_button_queue(frontiers: &ButtonVec<usize>, zone: ButtonZone) -> ButtonVec<usize> {
    frontiers.set(zone, frontiers.get_d(zone, 0) + 1)
}

fn touch_eligible_for_click(note: &TouchNote, current_time: TimePoint) -> bool {
    let timing = note.params.effective_timing();
    current_time.to_micros() >= time_point_sub_duration(timing, C::JUDGABLE_RANGE).to_micros()
        && current_time.to_micros() <= time_point_add_duration(timing, C::TOUCH_GOOD).to_micros()
}

fn hold_head_eligible_for_click(note: &HoldNote, current_time: TimePoint) -> bool {
    let timing = note.params.effective_timing();
    let late_limit = if note.is_touch_hold {
        time_point_add_duration(
            time_point_add_duration(timing, C::JUDGABLE_RANGE),
            C::TOUCH_JUDGABLE_RANGE_LATE_EXTRA,
        )
    } else {
        time_point_add_duration(timing, C::JUDGABLE_RANGE)
    };
    current_time.to_micros() >= time_point_sub_duration(timing, C::JUDGABLE_RANGE).to_micros()
        && current_time.to_micros() <= late_limit.to_micros()
}

// ---------------------------------------------------------------------------
// Tap processing
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn process_tap_queue_fuel(
    fuel: usize,
    zone: ButtonZone,
    queue: &ZoneQueue<TapFamilyNote>,
    frontiers: &ButtonVec<usize>,
    input: &FrameInput,
    current_time: TimePoint,
    touch_panel_offset: Duration,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
    evs_rev: &[JudgeEvent],
) -> (ZoneQueue<TapFamilyNote>, ButtonVec<usize>, ClickCursor, Vec<JudgeEvent>) {
    let note = match queue.peek() {
        None => return (queue.clone(), frontiers.clone(), cursor.clone(), evs_rev.to_vec()),
        Some(n) => n,
    };
    if fuel == 0 {
        return (queue.clone(), frontiers.clone(), cursor.clone(), evs_rev.to_vec());
    }
    let timing = note.params().effective_timing();
    let button_diff = current_time - timing;
    let sensor_diff = time_point_sub_duration(current_time, touch_panel_offset) - timing;
    let can_consume_click =
        tap_eligible_for_click(&note, current_time) && button_queue_index_unlocked(frontiers, zone, note.button_queue_index());
    let fallback_area = fallback_sensor_area_for_button_note(note.lane().to_button_zone());
    let (used_button, used_sensor, cursor2) =
        consume_button_then_fallback_sensor(input, cursor, can_consume_click, zone, fallback_area);
    let (clicked, diff) = if used_button {
        (true, button_diff)
    } else if used_sensor {
        (true, sensor_diff)
    } else {
        (false, button_diff)
    };
    let (new_note, evt) = tap_family_step(&note, current_time, diff, clicked, style);
    let ended = matches!(new_note.state(), TapState::Ended);
    let mut evs = evs_rev.to_vec();
    if let Some(e) = evt {
        evs.insert(0, e);
    }
    if ended {
        let queue2 = queue.advance();
        let frontiers2 = advance_shared_button_queue(frontiers, zone);
        if tap_family_queue_has_unused_click(input, &cursor2, zone, fallback_area) {
            process_tap_queue_fuel(
                fuel - 1, zone, &queue2, &frontiers2, input, current_time, touch_panel_offset, style,
                &cursor2, &evs,
            )
        } else {
            (queue2, frontiers2, cursor2, evs)
        }
    } else {
        let queue2 = ZoneQueue {
            notes: list_set_at(&queue.notes, queue.current_index, new_note),
            ..queue.clone()
        };
        (queue2, frontiers.clone(), cursor2, evs)
    }
}

#[allow(clippy::too_many_arguments)]
fn process_tap_queue(
    zone: ButtonZone,
    queue: &ZoneQueue<TapFamilyNote>,
    frontiers: &ButtonVec<usize>,
    input: &FrameInput,
    current_time: TimePoint,
    touch_panel_offset: Duration,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
    evs_rev: &[JudgeEvent],
) -> (ZoneQueue<TapFamilyNote>, ButtonVec<usize>, ClickCursor, Vec<JudgeEvent>) {
    let fuel = queue.notes.len().saturating_sub(queue.current_index) + 1;
    process_tap_queue_fuel(
        fuel, zone, queue, frontiers, input, current_time, touch_panel_offset, style, cursor, evs_rev,
    )
}

#[allow(clippy::too_many_arguments)]
fn process_tap_notes(
    frontiers: &ButtonVec<usize>,
    queues: &crate::input_model::ButtonQueueVec<TapFamilyNote>,
    input: &FrameInput,
    current_time: TimePoint,
    touch_panel_offset: Duration,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
) -> (ButtonVec<usize>, crate::input_model::ButtonQueueVec<TapFamilyNote>, Vec<JudgeEvent>, ClickCursor) {
    let (next_queues, (frontiers2, cursor2, mut evs_rev)) = queues.map_accum(
        (frontiers.clone(), cursor.clone(), Vec::<JudgeEvent>::new()),
        |zone, q, state| {
            let (frontiers, cursor, evs_rev) = state;
            let (next_queue, frontiers2, cursor2, evs_rev2) = process_tap_queue(
                zone, &q, &frontiers, input, current_time, touch_panel_offset, style, &cursor, &evs_rev,
            );
            (next_queue, (frontiers2, cursor2, evs_rev2))
        },
    );
    evs_rev.reverse();
    (frontiers2, next_queues, evs_rev, cursor2)
}

// ---------------------------------------------------------------------------
// Hold processing
// ---------------------------------------------------------------------------

fn is_head_judged_state(state: HoldSubState) -> bool {
    matches!(state, HoldSubState::HeadJudged(_))
}

fn entered_head_judged(before: HoldSubState, after: HoldSubState) -> bool {
    !is_head_judged_state(before) && is_head_judged_state(after)
}

fn keep_hold_active(note: &HoldNote) -> bool {
    !matches!(note.state, HoldSubState::Ended(_))
}

fn queue_head_matches(queue: &ZoneQueue<HoldNote>, note: &HoldNote) -> bool {
    match queue.peek() {
        Some(head) => head.params.note_index == note.params.note_index,
        None => false,
    }
}

fn hold_head_awaits_resolution(state: HoldSubState) -> bool {
    matches!(state, HoldSubState::HeadWaiting | HoldSubState::HeadJudgeable)
}

fn hold_queue_resolved(note: &HoldNote) -> bool {
    !hold_head_awaits_resolution(note.state)
}

fn normalize_hold_queue_cursor(queue: &ZoneQueue<HoldNote>) -> ZoneQueue<HoldNote> {
    let fuel = queue.notes.len().saturating_sub(queue.current_index) + 1;
    let mut q = queue.clone();
    for _ in 0..fuel {
        match q.peek() {
            Some(note) if hold_queue_resolved(&note) => q = q.advance(),
            _ => break,
        }
    }
    q
}

fn replace_hold_queue_note(queue: &ZoneQueue<HoldNote>, note: &HoldNote) -> ZoneQueue<HoldNote> {
    ZoneQueue {
        notes: queue
            .notes
            .iter()
            .map(|existing| {
                if existing.params.note_index == note.params.note_index {
                    note.clone()
                } else {
                    existing.clone()
                }
            })
            .collect(),
        ..queue.clone()
    }
}

fn update_button_hold_queue(
    queues: &crate::input_model::ButtonQueueVec<HoldNote>,
    zone: ButtonZone,
    note: &HoldNote,
) -> crate::input_model::ButtonQueueVec<HoldNote> {
    let queue = button_queue_at(queues, zone);
    set_button_queue_at(queues, zone, normalize_hold_queue_cursor(&replace_hold_queue_note(&queue, note)))
}

fn update_sensor_hold_queue(
    queues: &crate::input_model::SensorQueueVec<HoldNote>,
    area: SensorArea,
    note: &HoldNote,
) -> crate::input_model::SensorQueueVec<HoldNote> {
    let queue = sensor_queue_at(queues, area);
    set_sensor_queue_at(queues, area, normalize_hold_queue_cursor(&replace_hold_queue_note(&queue, note)))
}

fn has_strict_majority(count: usize, size: usize) -> bool {
    size > 0 && count * 2 > size
}

fn group_share_result(groups: &[GroupState], group_id: usize) -> Option<(JudgeGrade, Duration)> {
    match groups.iter().find(|g| g.group_id == group_id) {
        Some(group) => {
            if has_strict_majority(group.count, group.size) {
                Some((group.grade, group.diff))
            } else {
                None
            }
        }
        None => None,
    }
}

fn register_touch_group_result(
    groups: &[GroupState],
    group_id: usize,
    group_size: usize,
    grade: JudgeGrade,
    diff: Duration,
) -> Vec<GroupState> {
    let mut out = Vec::new();
    let mut found = false;
    for group in groups {
        if group.group_id == group_id {
            found = true;
            let keep_stored = has_strict_majority(group.count, group.size);
            let next = if keep_stored {
                GroupState { count: group.count + 1, size: group_size, ..group.clone() }
            } else {
                GroupState { count: group.count + 1, size: group_size, grade, diff, ..group.clone() }
            };
            out.push(next);
        } else {
            out.push(group.clone());
        }
    }
    if !found {
        out.push(GroupState { group_id, count: 1, size: group_size, grade, diff });
    }
    out
}

fn touch_hold_body_group_states_from_holds(holds: &[(SensorArea, HoldNote)]) -> Vec<TouchHoldBodyGroupState> {
    let mut acc: Vec<TouchHoldBodyGroupState> = Vec::new();
    for (_, note) in holds {
        let group_id = match note.touch_hold_group_id {
            None => continue,
            Some(gid) => gid,
        };
        let note_index = note.params.note_index;
        let triggered = note.touch_hold_group_triggered;
        if let Some(item) = acc.iter_mut().find(|g| g.group_id == group_id) {
            if !item.member_note_indices.contains(&note_index) {
                item.member_note_indices.insert(0, note_index);
            }
            if triggered && !item.triggered_note_indices.contains(&note_index) {
                item.triggered_note_indices.insert(0, note_index);
            } else if !triggered {
                item.triggered_note_indices.retain(|i| *i != note_index);
            }
        } else {
            let triggered_note_indices = if triggered { vec![note_index] } else { Vec::new() };
            acc.push(TouchHoldBodyGroupState {
                group_id,
                member_note_indices: vec![note_index],
                triggered_note_indices,
            });
        }
    }
    acc
}

fn touch_hold_body_group_majority_pressed(groups: &[TouchHoldBodyGroupState], group_id: usize) -> bool {
    match groups.iter().find(|g| g.group_id == group_id) {
        Some(group) => has_strict_majority(group.triggered_note_indices.len(), group.member_note_indices.len()),
        None => false,
    }
}

fn register_touch_hold_body_trigger(
    groups: &[TouchHoldBodyGroupState],
    group_id: usize,
    note_index: usize,
) -> Vec<TouchHoldBodyGroupState> {
    let mut out = Vec::new();
    let mut found = false;
    for group in groups {
        if group.group_id == group_id {
            found = true;
            let mut g = group.clone();
            if !g.member_note_indices.contains(&note_index) {
                g.member_note_indices.insert(0, note_index);
            }
            if !g.triggered_note_indices.contains(&note_index) {
                g.triggered_note_indices.insert(0, note_index);
            }
            out.push(g);
        } else {
            out.push(group.clone());
        }
    }
    if !found {
        out.push(TouchHoldBodyGroupState {
            group_id,
            member_note_indices: vec![note_index],
            triggered_note_indices: vec![note_index],
        });
    }
    out
}

fn unregister_touch_hold_body_trigger(
    groups: &[TouchHoldBodyGroupState],
    group_id: usize,
    note_index: usize,
) -> Vec<TouchHoldBodyGroupState> {
    groups
        .iter()
        .map(|group| {
            if group.group_id == group_id {
                TouchHoldBodyGroupState {
                    triggered_note_indices: group.triggered_note_indices.iter().cloned().filter(|i| *i != note_index).collect(),
                    ..group.clone()
                }
            } else {
                group.clone()
            }
        })
        .collect()
}

fn exit_touch_hold_body_group_member(
    groups: &[TouchHoldBodyGroupState],
    group_id: usize,
    note_index: usize,
) -> Vec<TouchHoldBodyGroupState> {
    let mut out = Vec::new();
    for group in groups {
        if group.group_id == group_id {
            let members: Vec<usize> = group.member_note_indices.iter().cloned().filter(|i| *i != note_index).collect();
            if members.is_empty() {
                continue;
            }
            out.push(TouchHoldBodyGroupState {
                member_note_indices: members,
                triggered_note_indices: group.triggered_note_indices.iter().cloned().filter(|i| *i != note_index).collect(),
                ..group.clone()
            });
        } else {
            out.push(group.clone());
        }
    }
    out
}

fn touch_hold_body_check_active(note: &HoldNote, current_time: TimePoint) -> bool {
    let timing = note.params.judge_timing;
    let body_check_start = time_point_add_duration(timing, C::TOUCH_HOLD_HEAD_IGNORE);
    let body_check_end = time_point_sub_duration(
        time_point_add_duration(timing, note.length),
        C::TOUCH_HOLD_TAIL_IGNORE,
    );
    let body_window_disabled = !note.is_classic
        && note.length.to_micros() <= C::TOUCH_HOLD_HEAD_IGNORE.to_micros() + C::TOUCH_HOLD_TAIL_IGNORE.to_micros();
    !body_window_disabled
        && current_time.to_micros() >= body_check_start.to_micros()
        && current_time.to_micros() <= body_check_end.to_micros()
}

#[allow(clippy::too_many_arguments)]
fn process_hold_notes(
    frontiers: &ButtonVec<usize>,
    queues: &crate::input_model::ButtonQueueVec<HoldNote>,
    holds: &[(ButtonZone, HoldNote)],
    input: &FrameInput,
    current_time: TimePoint,
    delta: Duration,
    style: crate::types::JudgeStyle,
    touch_panel_offset: Duration,
    prev_sensor: &SensorVec<bool>,
    cursor: &ClickCursor,
) -> (
    ButtonVec<usize>,
    crate::input_model::ButtonQueueVec<HoldNote>,
    Vec<(ButtonZone, HoldNote)>,
    Vec<JudgeEvent>,
    ClickCursor,
) {
    match holds.split_first() {
        None => (frontiers.clone(), queues.clone(), Vec::new(), Vec::new(), cursor.clone()),
        Some(((zone, note), rest)) => {
            let zone = *zone;
            let timing = note.params.effective_timing();
            let button_diff = current_time - timing;
            let sensor_diff = time_point_sub_duration(current_time, touch_panel_offset) - timing;
            let current_button_pressed = input.get_button_held(zone);
            let current_sensor_pressed = fallback_sensor_held_for_button_note(input, zone);
            let prev_sensor_pressed = fallback_prev_sensor_held_for_button_note(prev_sensor, zone);
            let allow_input = hold_head_awaits_resolution(note.state)
                && queue_head_matches(&normalize_hold_queue_cursor(&button_queue_at(queues, zone)), note)
                && button_queue_index_unlocked(frontiers, zone, note.button_queue_index)
                && hold_head_eligible_for_click(note, current_time);
            let fallback_area = fallback_sensor_area_for_button_note(zone);
            let (used_button, used_sensor, cursor2) =
                consume_button_then_fallback_sensor(input, cursor, allow_input, zone, fallback_area);
            let clicked = used_button || used_sensor;
            let diff = if used_button { button_diff } else { sensor_diff };
            let (new_note, evt) = hold_step(
                note,
                current_time,
                diff,
                C::HOLD_HEAD_IGNORE,
                C::HOLD_TAIL_IGNORE,
                clicked,
                hold_body_pressed_from_button_or_sensor(current_button_pressed, current_sensor_pressed),
                current_button_pressed,
                prev_sensor_pressed,
                touch_panel_offset,
                None,
                delta,
                style,
            );
            let frontiers2 = if entered_head_judged(note.state, new_note.state) {
                advance_shared_button_queue(frontiers, zone)
            } else {
                frontiers.clone()
            };
            let queues2 = update_button_hold_queue(queues, zone, &new_note);
            let (rest_frontiers, rest_queues, mut rest_notes, mut rest_evs, cursor3) = process_hold_notes(
                &frontiers2, &queues2, rest, input, current_time, delta, style, touch_panel_offset,
                prev_sensor, &cursor2,
            );
            if keep_hold_active(&new_note) {
                rest_notes.insert(0, (zone, new_note));
            }
            if let Some(e) = evt {
                rest_evs.insert(0, e);
            }
            (rest_frontiers, rest_queues, rest_notes, rest_evs, cursor3)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn process_touch_hold_notes(
    touch_frontiers: &SensorVec<usize>,
    queues: &crate::input_model::SensorQueueVec<HoldNote>,
    holds: &[(SensorArea, HoldNote)],
    input: &FrameInput,
    current_time: TimePoint,
    delta: Duration,
    style: crate::types::JudgeStyle,
    touch_panel_offset: Duration,
    cursor: &ClickCursor,
    touch_group_states: &[GroupState],
    touch_hold_body_groups: &[TouchHoldBodyGroupState],
) -> (
    SensorVec<usize>,
    crate::input_model::SensorQueueVec<HoldNote>,
    Vec<(SensorArea, HoldNote)>,
    Vec<JudgeEvent>,
    ClickCursor,
    Vec<GroupState>,
    Vec<TouchHoldBodyGroupState>,
) {
    match holds.split_first() {
        None => (
            touch_frontiers.clone(),
            queues.clone(),
            Vec::new(),
            Vec::new(),
            cursor.clone(),
            touch_group_states.to_vec(),
            touch_hold_body_groups.to_vec(),
        ),
        Some(((area, note), rest)) => {
            let area = *area;
            let timing = note.params.effective_timing();
            let sensor_diff = time_point_sub_duration(current_time, touch_panel_offset) - timing;
            let local_body_pressed = input.get_sensor_held(area);
            let body_groups1 = if touch_hold_body_check_active(note, current_time) {
                match note.touch_hold_group_id {
                    Some(group_id) => {
                        if local_body_pressed {
                            register_touch_hold_body_trigger(touch_hold_body_groups, group_id, note.params.note_index)
                        } else {
                            unregister_touch_hold_body_trigger(touch_hold_body_groups, group_id, note.params.note_index)
                        }
                    }
                    None => touch_hold_body_groups.to_vec(),
                }
            } else {
                touch_hold_body_groups.to_vec()
            };
            let effective_pressed = local_body_pressed
                || match note.touch_hold_group_id {
                    Some(group_id) => touch_hold_body_group_majority_pressed(&body_groups1, group_id),
                    None => false,
                };
            let shared_result = match note.touch_group_id {
                Some(group_id) => group_share_result(touch_group_states, group_id),
                None => None,
            };
            let allow_input = shared_result.is_none()
                && current_time.to_micros() <= time_point_add_duration(timing, C::TOUCH_GOOD).to_micros()
                && hold_head_awaits_resolution(note.state)
                && queue_head_matches(&normalize_hold_queue_cursor(&sensor_queue_at(queues, area)), note)
                && touch_queue_index_unlocked(touch_frontiers, area, note.touch_queue_index)
                && hold_head_eligible_for_click(note, current_time);
            let (used_sensor, cursor1) = if allow_input {
                try_use_sensor_click_at(input, cursor, area)
            } else {
                (false, cursor.clone())
            };
            let (new_note, evt) = hold_step(
                note,
                current_time,
                sensor_diff,
                C::TOUCH_HOLD_HEAD_IGNORE,
                C::TOUCH_HOLD_TAIL_IGNORE,
                used_sensor,
                effective_pressed,
                false,
                false,
                touch_panel_offset,
                shared_result,
                delta,
                style,
            );
            let touch_frontiers2 = if entered_head_judged(note.state, new_note.state) {
                advance_shared_touch_queue(touch_frontiers, area)
            } else {
                touch_frontiers.clone()
            };
            let queues2 = update_sensor_hold_queue(queues, area, &new_note);
            let touch_group_states2 = if used_sensor && entered_head_judged(note.state, new_note.state) {
                match (new_note.state, note.touch_group_id) {
                    (HoldSubState::HeadJudged(grade), Some(group_id)) => {
                        if grade.is_miss_or_too_fast() {
                            touch_group_states.to_vec()
                        } else {
                            register_touch_group_result(
                                touch_group_states,
                                group_id,
                                note.touch_group_size,
                                grade,
                                new_note.head_diff,
                            )
                        }
                    }
                    _ => touch_group_states.to_vec(),
                }
            } else {
                touch_group_states.to_vec()
            };
            let body_groups2 = match note.touch_hold_group_id {
                Some(group_id) => {
                    if keep_hold_active(&new_note) {
                        body_groups1
                    } else {
                        exit_touch_hold_body_group_member(&body_groups1, group_id, note.params.note_index)
                    }
                }
                None => body_groups1,
            };
            let (rest_frontiers, rest_queues, mut rest_notes, mut rest_evs, cursor2, rest_groups, rest_body) =
                process_touch_hold_notes(
                    &touch_frontiers2, &queues2, rest, input, current_time, delta, style,
                    touch_panel_offset, &cursor1, &touch_group_states2, &body_groups2,
                );
            if keep_hold_active(&new_note) {
                rest_notes.insert(0, (area, new_note));
            }
            if let Some(e) = evt {
                rest_evs.insert(0, e);
            }
            (rest_frontiers, rest_queues, rest_notes, rest_evs, cursor2, rest_groups, rest_body)
        }
    }
}

fn touch_queue_index_unlocked(frontiers: &SensorVec<usize>, area: SensorArea, index: usize) -> bool {
    index <= frontiers.get_d(area, 0)
}

fn advance_shared_touch_queue(frontiers: &SensorVec<usize>, area: SensorArea) -> SensorVec<usize> {
    frontiers.set(area, frontiers.get_d(area, 0) + 1)
}

fn touch_queue_resolved(note: &TouchNote) -> bool {
    matches!(note.state, TouchState::Judged(_) | TouchState::Ended)
}

fn normalize_touch_queue_cursor(queue: &ZoneQueue<TouchNote>) -> ZoneQueue<TouchNote> {
    let fuel = queue.notes.len().saturating_sub(queue.current_index) + 1;
    let mut q = queue.clone();
    for _ in 0..fuel {
        match q.peek() {
            Some(note) if touch_queue_resolved(&note) => q = q.advance(),
            _ => break,
        }
    }
    q
}

fn replace_touch_queue_note(queue: &ZoneQueue<TouchNote>, note: &TouchNote) -> ZoneQueue<TouchNote> {
    ZoneQueue {
        notes: queue
            .notes
            .iter()
            .map(|existing| {
                if existing.params.note_index == note.params.note_index {
                    note.clone()
                } else {
                    existing.clone()
                }
            })
            .collect(),
        ..queue.clone()
    }
}

#[allow(clippy::too_many_arguments)]
fn process_touch_queue_head_fuel(
    fuel: usize,
    area: SensorArea,
    queue: &ZoneQueue<TouchNote>,
    frontiers: &SensorVec<usize>,
    input: &FrameInput,
    current_time: TimePoint,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
    touch_panel_offset: Duration,
    groups: &[GroupState],
    evs_rev: &[JudgeEvent],
) -> (ZoneQueue<TouchNote>, SensorVec<usize>, ClickCursor, Vec<GroupState>, Vec<JudgeEvent>) {
    let queue = normalize_touch_queue_cursor(queue);
    if fuel == 0 {
        return (queue, frontiers.clone(), cursor.clone(), groups.to_vec(), evs_rev.to_vec());
    }
    let note = match queue.peek() {
        None => return (queue, frontiers.clone(), cursor.clone(), groups.to_vec(), evs_rev.to_vec()),
        Some(n) => n,
    };
    let timing = note.params.effective_timing();
    let sensor_diff = time_point_sub_duration(current_time, touch_panel_offset) - timing;
    let shared_result = match note.touch_group_id {
        Some(group_id) => group_share_result(groups, group_id),
        None => None,
    };
    let can_consume_click = shared_result.is_none()
        && touch_eligible_for_click(&note, current_time)
        && touch_queue_index_unlocked(frontiers, area, note.touch_queue_index);
    let (used_sensor, cursor2) = if can_consume_click {
        try_use_sensor_click_at(input, cursor, note.sensor_pos)
    } else {
        (false, cursor.clone())
    };
    let clicked = used_sensor;
    let diff = sensor_diff;
    let (new_note, evt) = touch_step(&note, current_time, diff, clicked, shared_result, style);
    let resolved_now = !touch_queue_resolved(&note) && touch_queue_resolved(&new_note);
    let mut groups2 = groups.to_vec();
    let mut evs = evs_rev.to_vec();
    if let Some(e) = &evt {
        evs.insert(0, e.clone());
        if !(!clicked || e.grade.is_miss_or_too_fast()) {
            if let Some(group_id) = note.touch_group_id {
                groups2 = register_touch_group_result(groups, group_id, note.touch_group_size, e.grade, diff);
            }
        }
    }
    let queue2 = normalize_touch_queue_cursor(&replace_touch_queue_note(&queue, &new_note));
    let frontiers2 = if resolved_now {
        advance_shared_touch_queue(frontiers, area)
    } else {
        frontiers.clone()
    };
    if matches!(new_note.state, TouchState::Ended)
        && has_unused_sensor_click_at(input, &cursor2, note.sensor_pos)
    {
        process_touch_queue_head_fuel(
            fuel - 1, area, &queue2, &frontiers2, input, current_time, style, &cursor2,
            touch_panel_offset, &groups2, &evs,
        )
    } else {
        (queue2, frontiers2, cursor2, groups2, evs)
    }
}

#[allow(clippy::too_many_arguments)]
fn process_touch_queue_head(
    area: SensorArea,
    queue: &ZoneQueue<TouchNote>,
    frontiers: &SensorVec<usize>,
    input: &FrameInput,
    current_time: TimePoint,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
    touch_panel_offset: Duration,
    groups: &[GroupState],
    evs_rev: &[JudgeEvent],
) -> (ZoneQueue<TouchNote>, SensorVec<usize>, ClickCursor, Vec<GroupState>, Vec<JudgeEvent>) {
    let fuel = queue.notes.len().saturating_sub(queue.current_index) + 1;
    process_touch_queue_head_fuel(
        fuel, area, queue, frontiers, input, current_time, style, cursor, touch_panel_offset, groups, evs_rev,
    )
}

#[allow(clippy::too_many_arguments)]
fn process_touch_queue_automatic(
    area: SensorArea,
    queue: &ZoneQueue<TouchNote>,
    frontiers: &SensorVec<usize>,
    current_time: TimePoint,
    style: crate::types::JudgeStyle,
    touch_panel_offset: Duration,
    groups: &[GroupState],
    evs_rev: &[JudgeEvent],
) -> (ZoneQueue<TouchNote>, SensorVec<usize>, Vec<GroupState>, Vec<JudgeEvent>) {
    let mut notes: Vec<TouchNote> = Vec::new();
    let mut frontiers = frontiers.clone();
    let mut evs = evs_rev.to_vec();
    for (index, note) in queue.notes.iter().enumerate() {
        if index < queue.current_index || touch_queue_resolved(note) {
            notes.push(note.clone());
            continue;
        }
        let timing = note.params.effective_timing();
        let sensor_diff = time_point_sub_duration(current_time, touch_panel_offset) - timing;
        let shared_result = match note.touch_group_id {
            Some(group_id) => group_share_result(groups, group_id),
            None => None,
        };
        let too_late = current_time.to_micros() > time_point_add_duration(timing, C::TOUCH_GOOD).to_micros();
        if shared_result.is_none() && !too_late {
            notes.push(note.clone());
        } else {
            let (new_note, evt) = touch_step(note, current_time, sensor_diff, false, shared_result, style);
            let resolved_now = !touch_queue_resolved(note) && touch_queue_resolved(&new_note);
            if resolved_now {
                frontiers = advance_shared_touch_queue(&frontiers, area);
            }
            if let Some(e) = evt {
                evs.insert(0, e);
            }
            notes.push(new_note);
        }
    }
    let q = normalize_touch_queue_cursor(&ZoneQueue { notes, ..queue.clone() });
    (q, frontiers, groups.to_vec(), evs)
}

#[allow(clippy::too_many_arguments)]
fn process_touch_queue(
    area: SensorArea,
    queue: &ZoneQueue<TouchNote>,
    frontiers: &SensorVec<usize>,
    input: &FrameInput,
    current_time: TimePoint,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
    touch_panel_offset: Duration,
    groups: &[GroupState],
    evs_rev: &[JudgeEvent],
) -> (ZoneQueue<TouchNote>, SensorVec<usize>, ClickCursor, Vec<GroupState>, Vec<JudgeEvent>) {
    let (queue1, frontiers1, cursor1, groups1, evs1) = process_touch_queue_head(
        area, queue, frontiers, input, current_time, style, cursor, touch_panel_offset, groups, evs_rev,
    );
    let (queue2, frontiers2, groups2, evs2) = process_touch_queue_automatic(
        area, &queue1, &frontiers1, current_time, style, touch_panel_offset, &groups1, &evs1,
    );
    (queue2, frontiers2, cursor1, groups2, evs2)
}

#[allow(clippy::too_many_arguments)]
fn process_touch_notes(
    frontiers: &SensorVec<usize>,
    queues: &crate::input_model::SensorQueueVec<TouchNote>,
    input: &FrameInput,
    current_time: TimePoint,
    style: crate::types::JudgeStyle,
    cursor: &ClickCursor,
    touch_panel_offset: Duration,
    group_states: &[GroupState],
) -> (
    SensorVec<usize>,
    crate::input_model::SensorQueueVec<TouchNote>,
    Vec<JudgeEvent>,
    ClickCursor,
    Vec<GroupState>,
) {
    let (next_queues, (frontiers2, cursor2, groups2, mut evs_rev)) = queues.map_accum(
        (frontiers.clone(), cursor.clone(), group_states.to_vec(), Vec::<JudgeEvent>::new()),
        |area, q, state| {
            let (frontiers, cursor, groups, evs_rev) = state;
            let (next_queue, frontiers2, cursor2, groups2, evs_rev2) = process_touch_queue(
                area, &q, &frontiers, input, current_time, style, &cursor, touch_panel_offset, &groups, &evs_rev,
            );
            (next_queue, (frontiers2, cursor2, groups2, evs_rev2))
        },
    );
    evs_rev.reverse();
    (frontiers2, next_queues, evs_rev, cursor2, groups2)
}

// ---------------------------------------------------------------------------
// Slide processing
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn process_slide_notes_core(
    slides: &[SlideNote],
    input: &FrameInput,
    current_time: TimePoint,
    touch_panel_offset: Duration,
    delta: Duration,
    style: crate::types::JudgeStyle,
    subdivide_slide_judge_grade: bool,
) -> (Vec<SlideNote>, Vec<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>) {
    let mut processed: Vec<SlideNote> = Vec::new();
    let mut pending: Vec<SlideNote> = slides.to_vec();
    let mut events: Vec<JudgeEvent> = Vec::new();
    let mut audio: Vec<AudioCommand> = Vec::new();
    let mut render: Vec<RenderCommand> = Vec::new();

    while let Some(note) = pending.first().cloned() {
        let rest = pending[1..].to_vec();
        let (new_note, evt, audio_cmds, render_cmds) = slide_step(
            &note, current_time, &input.sensor_held, touch_panel_offset, delta, style,
            subdivide_slide_judge_grade,
        );
        let mut updated: Vec<SlideNote> = vec![new_note.clone()];
        updated.extend(rest);
        let updated = update_slide_parent_flags(&updated);
        pending = if updated.is_empty() { Vec::new() } else { updated[1..].to_vec() };
        processed.push(new_note);
        if let Some(e) = evt {
            events.push(e);
        }
        for c in audio_cmds.into_iter().rev() {
            audio.insert(0, c);
        }
        for c in render_cmds.into_iter().rev() {
            render.insert(0, c);
        }
    }
    (processed, events, audio, render)
}

fn event_to_audio_commands(evt: &JudgeEvent, time_point: TimePoint) -> Vec<AudioCommand> {
    vec![AudioCommand::PlayJudgeSfx {
        kind: evt.kind,
        grade: evt.grade,
        is_break: evt.is_break,
        at_time: time_point,
        note_index: evt.note_index,
    }]
}

fn event_to_render_commands(evt: &JudgeEvent) -> Vec<RenderCommand> {
    vec![RenderCommand::ShowJudgeResult {
        kind: evt.kind,
        grade: evt.grade,
        is_break: evt.is_break,
        diff: evt.diff,
        note_index: evt.note_index,
    }]
}

fn events_to_audio_commands(events: &[JudgeEvent], time_point: TimePoint) -> Vec<AudioCommand> {
    events.iter().flat_map(|e| event_to_audio_commands(e, time_point)).collect()
}

fn events_to_render_commands(events: &[JudgeEvent]) -> Vec<RenderCommand> {
    events.iter().flat_map(event_to_render_commands).collect()
}

/// `stepFrame`.
pub fn step_frame(
    st: &GameState,
    input: &FrameInput,
) -> (GameState, Vec<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>) {
    let new_time = time_point_add_duration(st.current_time, input.delta);
    let cursor = ClickCursor::default();
    let resolved_slides = update_slide_parent_flags(&st.slides);
    let touch_hold_body_groups = if st.touch_hold_group_states.is_empty() {
        touch_hold_body_group_states_from_holds(&st.active_touch_holds)
    } else {
        st.touch_hold_group_states.clone()
    };

    let (button_frontiers1, tap_notes, tap_events, cursor_tap) = process_tap_notes(
        &st.button_queue_frontiers, &st.tap_queues, input, new_time, st.touch_panel_offset, st.judge_style, &cursor,
    );
    let (button_frontiers2, hold_queues, hold_notes, hold_events, cursor1) = process_hold_notes(
        &button_frontiers1, &st.hold_queues, &st.active_holds, input, new_time, input.delta,
        st.judge_style, st.touch_panel_offset, &st.prev_sensor, &cursor_tap,
    );
    let (touch_frontiers1, touch_notes, touch_events, cursor2, touch_group_states) = process_touch_notes(
        &st.touch_queue_frontiers, &st.touch_queues, input, new_time, st.judge_style, &cursor1,
        st.touch_panel_offset, &st.touch_group_states,
    );
    let (touch_frontiers2, touch_hold_queues, touch_hold_notes, touch_hold_events, _cursor3, touch_group_states2, touch_hold_group_states) =
        process_touch_hold_notes(
            &touch_frontiers1, &st.touch_hold_queues, &st.active_touch_holds, input, new_time,
            input.delta, st.judge_style, st.touch_panel_offset, &cursor2, &touch_group_states,
            &touch_hold_body_groups,
        );
    let (slide_notes, slide_events, slide_audio_commands, slide_render_commands) = process_slide_notes_core(
        &resolved_slides, input, new_time, st.touch_panel_offset, input.delta, st.judge_style,
        st.subdivide_slide_judge_grade,
    );
    let slide_notes = force_finish_parent_slides(&slide_notes);
    let slide_notes = update_slide_parent_flags(&slide_notes);
    let force_finish_commands = force_finish_render_cmds(&resolved_slides, &slide_notes);

    let mut all_events: Vec<JudgeEvent> = Vec::new();
    all_events.extend(tap_events);
    all_events.extend(hold_events);
    all_events.extend(touch_hold_events);
    all_events.extend(touch_events);
    all_events.extend(slide_events);

    let new_score = fold_events_into_score(
        st.note_fast_late_display,
        st.break_fast_late_display,
        &st.score,
        &all_events,
    );
    let mut audio_commands = slide_audio_commands;
    audio_commands.extend(events_to_audio_commands(&all_events, new_time));
    let mut render_commands = slide_render_commands;
    render_commands.extend(force_finish_commands);
    render_commands.extend(events_to_render_commands(&all_events));

    let next_state = GameState {
        current_time: new_time,
        prev_button: input.button_held.clone(),
        prev_sensor: input.sensor_held.clone(),
        button_queue_frontiers: button_frontiers2,
        tap_queues: tap_notes,
        hold_queues,
        touch_queue_frontiers: touch_frontiers2,
        touch_hold_queues,
        touch_queues: touch_notes,
        score: new_score,
        slides: slide_notes,
        active_holds: hold_notes,
        active_touch_holds: touch_hold_notes,
        touch_group_states: touch_group_states2,
        touch_hold_group_states,
        ..st.clone()
    };
    (next_state, all_events, audio_commands, render_commands)
}

/// `stepFrameTimed`.
pub fn step_frame_timed(
    st: &GameState,
    batch: &TimedInputBatch,
) -> (GameState, Vec<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>) {
    let delta = batch.current_time - st.current_time;
    let input = batch.to_frame_input(delta, st.prev_button.clone(), st.prev_sensor.clone());
    let st_with_batch = GameState { current_batch: batch.clone(), ..st.clone() };
    let (next_state, events, audio, render) = step_frame(&st_with_batch, &input);
    (GameState { current_batch: batch.clone(), ..next_state }, events, audio, render)
}

#[allow(dead_code)]
fn _unused_runtime_score(_s: RuntimeScoreState) {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chart_loader::ChartSpec;
    use crate::input_model::TimedInputEvent;
    use crate::simai::frontend::frontend_lowered_chart;

    const MAIDATA: &str = "&first=0\n&inote_1=\n(120)\n1,2,3,\n";

    #[test]
    fn step_frame_advances_time_and_scores_misses() {
        let spec: ChartSpec = frontend_lowered_chart(MAIDATA, 1).unwrap();
        let state = crate::chart_loader::build_game_state(&spec);
        // Jump far past all notes with no input → they should miss.
        let mut input = FrameInput { delta: Duration::from_micros(3_000_000), ..FrameInput::default() };
        input.button_held = ButtonVec::replicate(8, false);
        let (next, events, _audio, _render) = step_frame(&state, &input);
        assert_eq!(next.current_time.to_micros(), 3_000_000);
        assert!(!events.is_empty());
        assert_eq!(next.score.combo, 0);
    }

    #[test]
    fn timed_batch_click_scores_perfect() {
        let spec: ChartSpec = frontend_lowered_chart(MAIDATA, 1).unwrap();
        let state = crate::chart_loader::build_game_state(&spec);
        let mut batch = TimedInputBatch { current_time: TimePoint::zero(), ..Default::default() };
        batch.events.push(TimedInputEvent::ButtonClick(TimePoint::zero(), ButtonZone::K1));
        let (next, events, _audio, _render) = step_frame_timed(&state, &batch);
        // First tap judged at time 0 (delta 0 window contains exact point).
        assert!(!events.is_empty());
        assert!(next.score.combo >= 1);
    }
}
