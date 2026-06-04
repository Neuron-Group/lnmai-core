//! Frame scheduler — one function per frame.
//!
//! The Scheduler processes notes in a fixed order: tap → hold → touch → touch-hold → slide.
//! This is semantically meaningful and must be preserved exactly.
//!
//! Mirrors LnmaiCore/Scheduler.lean.

use super::areas::*;
use super::constants::*;
use super::convert;
use super::input_model::*;
use super::judge;
use super::lifecycle::*;
use super::score;
use super::storage::*;
use super::time::Duration;
use super::types::*;

/// Click cursor for tracking input consumption within a frame
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClickCursor {
    pub button_used: ButtonVec<u32>,
    pub sensor_used: SensorVec<u32>,
}

impl Default for ClickCursor {
    fn default() -> Self {
        Self {
            button_used: ButtonVec::replicate(0),
            sensor_used: SensorVec::replicate(0),
        }
    }
}

impl ClickCursor {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn consume_button_click(&mut self, input: &FrameInput, zone: ButtonZone) -> bool {
        let used = self.button_used.get_d(zone, 0);
        let available = input.get_button_click_count(zone);
        if used < available {
            self.button_used = self.button_used.set(zone, used + 1);
            true
        } else {
            false
        }
    }

    pub fn consume_sensor_click(&mut self, input: &FrameInput, area: SensorArea) -> bool {
        let used = self.sensor_used.get_d(area, 0);
        let available = input.get_sensor_click_count(area);
        if used < available {
            self.sensor_used = self.sensor_used.set(area, used + 1);
            true
        } else {
            false
        }
    }
}

/// Step result
#[derive(Debug, Clone, PartialEq)]
pub struct StepResult {
    pub state: GameState,
    pub events: Vec<JudgeEvent>,
    pub audio_commands: Vec<AudioCommand>,
    pub render_commands: Vec<RenderCommand>,
}

// Helper: fallback sensor area for a button zone
fn fallback_sensor_area_for_button_note(zone: ButtonZone) -> SensorArea {
    zone.to_outer_sensor_area()
}

fn fallback_sensor_held_for_button_note(input: &FrameInput, zone: ButtonZone) -> bool {
    input.get_sensor_held(fallback_sensor_area_for_button_note(zone))
}

fn fallback_prev_sensor_held_for_button_note(prev_sensor: &SensorVec<bool>, zone: ButtonZone) -> bool {
    prev_sensor.get_d(fallback_sensor_area_for_button_note(zone), false)
}

// Queue index unlocked check
fn button_queue_index_unlocked(frontiers: &ButtonVec<u32>, index: u32) -> bool {
    true // simplified — always unlocked for now
}

fn touch_queue_index_unlocked(frontiers: &SensorVec<u32>, index: u32) -> bool {
    true // simplified — always unlocked for now
}

fn advance_shared_button_queue(frontiers: &ButtonVec<u32>, zone: ButtonZone) -> ButtonVec<u32> {
    frontiers.set(zone, frontiers.get_d(zone, 0) + 1)
}

fn advance_shared_touch_queue(frontiers: &SensorVec<u32>, area: SensorArea) -> SensorVec<u32> {
    frontiers.set(area, frontiers.get_d(area, 0) + 1)
}

// Strict majority check
fn has_strict_majority(count: u32, size: u32) -> bool {
    size > 0 && count * 2 > size
}

// Group share result
fn group_share_result(groups: &[GroupState], group_id: u32) -> Option<(JudgeGrade, i64)> {
    groups.iter()
        .find(|g| g.group_id == group_id)
        .and_then(|group| {
            if has_strict_majority(group.count, group.size) {
                Some((group.grade, group.diff))
            } else {
                None
            }
        })
}

fn update_group_state(groups: &mut Vec<GroupState>, group_id: u32, group_size: u32, grade: JudgeGrade, diff: i64) {
    if let Some(group) = groups.iter_mut().find(|g| g.group_id == group_id) {
        let keep_stored = has_strict_majority(group.count, group.size);
        group.count += 1;
        if !keep_stored {
            group.grade = grade;
            group.diff = diff;
        }
        group.size = group_size;
    } else {
        groups.push(GroupState {
            group_id,
            count: 1,
            size: group_size,
            grade,
            diff,
        });
    }
}

/// Process tap queues
fn process_tap_queues(
    state: &mut GameState,
    input: &FrameInput,
    cursor: &mut ClickCursor,
    events: &mut Vec<JudgeEvent>,
) {
    let new_time = state.current_time + input.delta;
    for zone in ButtonZone::ALL {
        let queue = state.tap_queues.get_d(*zone, ZoneQueue::default());
        if queue.is_empty() {
            continue;
        }

        let mut q = queue.clone();
        let mut processed = true;

        while processed && !q.is_empty() {
            processed = false;
            if let Some(note) = q.peek() {
                let note_ref = note.clone();
                let timing = note_ref.params().effective_timing();
                let button_diff = new_time - timing;
                let sensor_diff = (new_time - state.touch_panel_offset) - timing;

                let can_consume = new_time >= timing - JUDGABLE_RANGE_SEC.to_micros()
                    && button_queue_index_unlocked(&state.button_queue_frontiers, 0u32);

                let used_button = if can_consume { cursor.consume_button_click(input, *zone) } else { false };
                let (clicked, diff) = if used_button {
                    (true, button_diff)
                } else {
                    let fallback_area = fallback_sensor_area_for_button_note(*zone);
                    let used_sensor = if can_consume { cursor.consume_sensor_click(input, fallback_area) } else { false };
                    (used_sensor, if used_sensor { sensor_diff } else { button_diff })
                };

                match note_ref.state() {
                    TapState::Waiting => {
                        if new_time >= timing - JUDGABLE_RANGE_SEC.to_micros() {
                            let mut updated = note_ref.clone();
                            match &mut updated {
                                TapFamilyNote::TapNote(n) => n.state = TapState::Judgeable,
                                TapFamilyNote::SlideHeadNote(n) => n.state = TapState::Judgeable,
                            }
                            q.replace_current(updated);
                            processed = true;
                        }
                    }
                    TapState::Judgeable => {
                        if clicked {
                            let raw = judge::judge_tap(Duration::from_micros(diff), note_ref.params().is_ex);
                            let grade = convert::convert_grade(state.judge_style, raw);
                            let mut updated = note_ref.clone();
                            match &mut updated {
                                TapFamilyNote::TapNote(n) => n.state = TapState::TapEnded,
                                TapFamilyNote::SlideHeadNote(n) => n.state = TapState::TapEnded,
                            }
                            let event = JudgeEvent {
                                kind: JudgeEventKind::Tap,
                                grade,
                                diff,
                                position: note_ref.lane().to_button_zone().into(),
                                note_index: note_ref.params().note_index,
                            };
                            events.push(event);
                            state.button_queue_frontiers = advance_shared_button_queue(&state.button_queue_frontiers, *zone);
                            q.replace_and_advance(updated);
                            processed = true;
                        } else if new_time > timing + TAP_JUDGE_GOOD_AREA_MSEC.to_micros() {
                            let mut updated = note_ref.clone();
                            match &mut updated {
                                TapFamilyNote::TapNote(n) => n.state = TapState::TapEnded,
                                TapFamilyNote::SlideHeadNote(n) => n.state = TapState::TapEnded,
                            }
                            let event = JudgeEvent {
                                kind: JudgeEventKind::Tap,
                                grade: JudgeGrade::Miss,
                                diff,
                                position: note_ref.lane().to_button_zone().into(),
                                note_index: note_ref.params().note_index,
                            };
                            events.push(event);
                            state.button_queue_frontiers = advance_shared_button_queue(&state.button_queue_frontiers, *zone);
                            q.replace_and_advance(updated);
                            processed = true;
                        }
                    }
                    TapState::TapJudged(_) => {
                        q.advance();
                        processed = true;
                    }
                    TapState::TapEnded => {
                        q.advance();
                        processed = true;
                    }
                }
            }
        }
        state.tap_queues = state.tap_queues.set(*zone, q);
    }
}

/// Process hold queues (head judgment)
fn process_hold_queues(
    state: &mut GameState,
    input: &FrameInput,
    cursor: &mut ClickCursor,
    events: &mut Vec<JudgeEvent>,
) {
    let new_time = state.current_time + input.delta;
    for zone in ButtonZone::ALL {
        let queue = state.hold_queues.get_d(*zone, ZoneQueue::default());
        if queue.is_empty() {
            continue;
        }

        let mut q = queue.clone();
        let mut processed = true;

        while processed && !q.is_empty() {
            processed = false;
            if let Some(note) = q.peek() {
                let note_ref = note.clone();
                let timing = note_ref.params.effective_timing();
                let diff = new_time - timing;
                let current_button_pressed = input.get_button_held(*zone);
                let current_sensor_pressed = fallback_sensor_held_for_button_note(input, *zone);
                let prev_sensor_pressed = fallback_prev_sensor_held_for_button_note(&state.prev_sensor, *zone);

                let can_consume = new_time >= timing - JUDGABLE_RANGE_SEC.to_micros()
                    && button_queue_index_unlocked(&state.button_queue_frontiers, 0u32);

                let used_button = if can_consume { cursor.consume_button_click(input, *zone) } else { false };
                let used_sensor = if can_consume && !used_button {
                    let fallback_area = fallback_sensor_area_for_button_note(*zone);
                    cursor.consume_sensor_click(input, fallback_area)
                } else {
                    false
                };
                let clicked = used_button || used_sensor;

                let (updated_note, evt) = hold_step(
                    &note_ref,
                    new_time,
                    diff,
                    HOLD_HEAD_IGNORE_LENGTH_SEC.to_micros(),
                    HOLD_TAIL_IGNORE_LENGTH_SEC.to_micros(),
                    clicked,
                    current_button_pressed || current_sensor_pressed,
                    current_button_pressed,
                    prev_sensor_pressed,
                    state.touch_panel_offset,
                    None,
                    input.delta,
                    state.judge_style,
                );

                let entered_head_judged = matches!(note_ref.state, HoldSubState::HeadWaiting | HoldSubState::HeadJudgeable)
                    && matches!(updated_note.state, HoldSubState::HeadJudged);

                if entered_head_judged {
                    state.button_queue_frontiers = advance_shared_button_queue(&state.button_queue_frontiers, *zone);
                    q.replace_and_advance(updated_note.clone());
                } else if matches!(updated_note.state, HoldSubState::Ended) {
                    q.advance();
                } else {
                    q.replace_current(updated_note.clone());
                }

                if matches!(updated_note.state, HoldSubState::HeadJudged | HoldSubState::BodyHeld | HoldSubState::BodyReleased) {
                    state.active_holds.push((*zone, updated_note));
                }

                if let Some(e) = evt {
                    events.push(e);
                }
                processed = true;
            }
        }
        state.hold_queues = state.hold_queues.set(*zone, q);
    }
}

/// Process active holds
fn process_active_holds(
    state: &mut GameState,
    input: &FrameInput,
    cursor: &mut ClickCursor,
    events: &mut Vec<JudgeEvent>,
) {
    let new_time = state.current_time + input.delta;
    let mut new_active_holds = Vec::new();

    for (zone, note) in &state.active_holds {
        let note_ref = note.clone();
        let timing = note_ref.params.effective_timing();
        let diff = new_time - timing;
        let current_button_pressed = input.get_button_held(*zone);
        let current_sensor_pressed = fallback_sensor_held_for_button_note(input, *zone);
        let prev_sensor_pressed = fallback_prev_sensor_held_for_button_note(&state.prev_sensor, *zone);

        let (updated_note, evt) = hold_step(
            &note_ref,
            new_time,
            diff,
            HOLD_HEAD_IGNORE_LENGTH_SEC.to_micros(),
            HOLD_TAIL_IGNORE_LENGTH_SEC.to_micros(),
            false, // active holds don't get new clicks
            current_button_pressed || current_sensor_pressed,
            current_button_pressed,
            prev_sensor_pressed,
            state.touch_panel_offset,
            None,
            input.delta,
            state.judge_style,
        );

        if matches!(updated_note.state, HoldSubState::HeadJudged | HoldSubState::BodyHeld | HoldSubState::BodyReleased) {
            new_active_holds.push((*zone, updated_note));
        } else {
            // Ended - check for new entry via head-judged in queue
            // The hold was already advanced, don't re-add
        }

        if let Some(e) = evt {
            events.push(e);
        }
    }

    state.active_holds = new_active_holds;
}

/// Process touch queues
fn process_touch_queues(
    state: &mut GameState,
    input: &FrameInput,
    cursor: &mut ClickCursor,
    events: &mut Vec<JudgeEvent>,
) {
    let new_time = state.current_time + input.delta;
    for area in SensorArea::ALL {
        let queue = state.touch_queues.get_d(*area, ZoneQueue::default());
        if queue.is_empty() {
            continue;
        }

        let mut q = queue.clone();
        let mut processed = true;

        while processed && !q.is_empty() {
            processed = false;
            if let Some(note) = q.peek() {
                let note_ref = note.clone();
                let timing = note_ref.params.effective_timing();
                let button_diff = new_time - timing;
                let sensor_diff = (new_time - state.touch_panel_offset) - timing;

                let can_consume = new_time >= timing - JUDGABLE_RANGE_SEC.to_micros()
                    && touch_queue_index_unlocked(&state.touch_queue_frontiers, 0u32);

                let used_button = if can_consume && state.use_button_ring_for_touch {
                    match area.to_outer_button_zone() {
                        Some(zone) => cursor.consume_button_click(input, zone),
                        None => false,
                    }
                } else {
                    false
                };

                let used_sensor = if used_button {
                    false
                } else if can_consume {
                    cursor.consume_sensor_click(input, note_ref.sensor_pos)
                } else {
                    false
                };

                let clicked = used_button || used_sensor;
                let diff = if used_button { button_diff } else { sensor_diff };

                let shared_result = note_ref.touch_group_id
                    .and_then(|gid| group_share_result(&state.touch_group_states, gid));

                let (updated_note, evt) = touch_step(
                    &note_ref,
                    new_time,
                    diff,
                    clicked,
                    shared_result.map(|(g, _)| g),
                    state.judge_style,
                );

                match evt {
                    Some(ref e) => {
                        if !e.grade.is_miss_or_too_fast() {
                            if let Some(gid) = note_ref.touch_group_id {
                                update_group_state(
                                    &mut state.touch_group_states,
                                    gid,
                                    note_ref.touch_group_size.unwrap_or(1),
                                    e.grade,
                                    diff,
                                );
                            }
                        }
                        state.touch_queue_frontiers = advance_shared_touch_queue(&state.touch_queue_frontiers, *area);
                        q.advance();
                        events.push(e.clone());
                    }
                    None => {
                        match updated_note.state {
                            TouchState::TouchEnded => {
                                state.touch_queue_frontiers = advance_shared_touch_queue(&state.touch_queue_frontiers, *area);
                                q.advance();
                            }
                            TouchState::TouchJudged(grade) => {
                                if !grade.is_miss_or_too_fast() {
                                    if let Some(gid) = note_ref.touch_group_id {
                                        update_group_state(
                                            &mut state.touch_group_states,
                                            gid,
                                            note_ref.touch_group_size.unwrap_or(1),
                                            grade,
                                            diff,
                                        );
                                    }
                                }
                                // still advance since judged
                            }
                            _ => {
                                q.replace_current(updated_note);
                            }
                        }
                    }
                }
                processed = true;
            }
        }
        state.touch_queues = state.touch_queues.set(*area, q);
    }
}

/// Process touch hold queues (head judgment + active)
fn process_touch_hold_queues(
    state: &mut GameState,
    input: &FrameInput,
    cursor: &mut ClickCursor,
    events: &mut Vec<JudgeEvent>,
) {
    let new_time = state.current_time + input.delta;
    // Process queued touch holds
    for area in SensorArea::ALL {
        let queue = state.touch_hold_queues.get_d(*area, ZoneQueue::default());
        if queue.is_empty() {
            continue;
        }

        let mut q = queue.clone();
        let mut processed = true;

        while processed && !q.is_empty() {
            processed = false;
            if let Some(note) = q.peek() {
                let note_ref = note.clone();
                let timing = note_ref.params.effective_timing();
                let button_diff = new_time - timing;
                let sensor_diff = (new_time - state.touch_panel_offset) - timing;

                let effective_pressed = input.get_sensor_held(*area);

                let can_consume = touch_queue_index_unlocked(&state.touch_queue_frontiers, 0u32)
                    && new_time >= timing - JUDGABLE_RANGE_SEC.to_micros();

                let used_button = if can_consume && state.use_button_ring_for_touch {
                    match area.to_outer_button_zone() {
                        Some(zone) => cursor.consume_button_click(input, zone),
                        None => false,
                    }
                } else {
                    false
                };

                let used_sensor = if used_button {
                    false
                } else if can_consume {
                    cursor.consume_sensor_click(input, *area)
                } else {
                    false
                };

                let head_diff = if used_button { button_diff } else { sensor_diff };

                let shared_result = note_ref.touch_group_id
                    .and_then(|gid| group_share_result(&state.touch_hold_group_states, gid));

                let (updated_note, evt) = hold_step(
                    &note_ref,
                    new_time,
                    head_diff,
                    TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC.to_micros(),
                    TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC.to_micros(),
                    used_button || used_sensor,
                    effective_pressed,
                    used_button,
                    false,
                    state.touch_panel_offset,
                    shared_result,
                    input.delta,
                    state.judge_style,
                );

                let entered_head_judged = matches!(note_ref.state, HoldSubState::HeadWaiting | HoldSubState::HeadJudgeable)
                    && matches!(updated_note.state, HoldSubState::HeadJudged);

                if entered_head_judged {
                    state.touch_queue_frontiers = advance_shared_touch_queue(&state.touch_queue_frontiers, *area);
                    q.replace_and_advance(updated_note.clone());
                } else if matches!(updated_note.state, HoldSubState::Ended) {
                    q.advance();
                } else {
                    q.replace_current(updated_note.clone());
                }

                if matches!(updated_note.state, HoldSubState::HeadJudged | HoldSubState::BodyHeld | HoldSubState::BodyReleased) {
                    state.active_touch_holds.push((*area, updated_note));
                }

                if let Some(ref e) = evt {
                    if !e.grade.is_miss_or_too_fast() {
                        if let Some(gid) = note_ref.touch_group_id {
                            update_group_state(
                                &mut state.touch_hold_group_states,
                                gid,
                                note_ref.touch_group_size.unwrap_or(1),
                                e.grade,
                                note_ref.head_diff.unwrap_or(head_diff),
                            );
                        }
                    }
                    events.push(e.clone());
                }
                processed = true;
            }
        }
        state.touch_hold_queues = state.touch_hold_queues.set(*area, q);
    }

    // Process active touch holds
    let mut new_active_touch_holds = Vec::new();
    for (area, note) in &state.active_touch_holds {
        let note_ref = note.clone();
        let timing = note_ref.params.effective_timing();
        let diff = new_time - timing;

        let effective_pressed = input.get_sensor_held(*area);

        let shared_result = note_ref.touch_group_id
            .and_then(|gid| group_share_result(&state.touch_hold_group_states, gid));

        let (updated_note, evt) = hold_step(
            &note_ref,
            new_time,
            diff,
            TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC.to_micros(),
            TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC.to_micros(),
            false,
            effective_pressed,
            false,
            false,
            state.touch_panel_offset,
            shared_result,
            input.delta,
            state.judge_style,
        );

        if matches!(updated_note.state, HoldSubState::HeadJudged | HoldSubState::BodyHeld | HoldSubState::BodyReleased) {
            new_active_touch_holds.push((*area, updated_note));
        }

        if let Some(ref e) = evt {
            if !e.grade.is_miss_or_too_fast() {
                if let Some(gid) = note_ref.touch_group_id {
                    update_group_state(
                        &mut state.touch_hold_group_states,
                        gid,
                        note_ref.touch_group_size.unwrap_or(1),
                        e.grade,
                        note_ref.head_diff.unwrap_or(diff),
                    );
                }
            }
            events.push(e.clone());
        }
    }
    state.active_touch_holds = new_active_touch_holds;
}

/// Process slides
fn process_slides(
    state: &mut GameState,
    input: &FrameInput,
    events: &mut Vec<JudgeEvent>,
    audio_commands: &mut Vec<AudioCommand>,
    render_commands: &mut Vec<RenderCommand>,
) {
    let new_time = state.current_time + input.delta;
    let mut new_slides = Vec::new();

    // Build sensor_held array from FrameInput using Lean storage order
    let mut sensor_held = [false; 33];
    for area in SensorArea::ALL {
        let idx = sensor_area_to_storage_index(*area);
        sensor_held[idx] = input.get_sensor_held(*area);
    }

    for slide in &state.slides {
        let (updated_slide, evt, audio_cmds, render_cmds) = slide_step(
            slide,
            new_time,
            &sensor_held,
            state.touch_panel_offset,
            input.delta,
            state.judge_style,
            state.subdivide_slide_judge_grade,
        );

        if matches!(updated_slide.state, SlideState::Ended) {
            // Slide done, emit commands
        } else {
            new_slides.push(updated_slide);
        }

        if let Some(e) = evt {
            events.push(e);
        }
        audio_commands.extend(audio_cmds);
        render_commands.extend(render_cmds);
    }

    state.slides = new_slides;
}

// Score accumulation from events
fn fold_event_into_score(s: &ScoreState, evt: &JudgeEvent) -> ScoreState {
    let multiple: u32 = 1;
    let delta = score::update_combo(
        s.combo,
        s.p_combo,
        s.c_p_combo,
        s.dx_score,
        evt.grade,
        multiple,
    );
    ScoreState {
        combo: delta.combo,
        p_combo: delta.p_combo,
        c_p_combo: delta.c_p_combo,
        dx_score: delta.d_x_score_lost,
        counts: s.counts, // simplified — full implementation would update per-note-type counts
        ..*s
    }
}

fn fold_events_into_score(s: &ScoreState, events: &[JudgeEvent]) -> ScoreState {
    let mut score = *s;
    for evt in events {
        score = fold_event_into_score(&score, evt);
    }
    score
}

fn events_to_audio_commands(events: &[JudgeEvent], time_point: i64) -> Vec<AudioCommand> {
    events.iter().map(|evt| {
        AudioCommand::PlayJudgeSfx {
            kind: evt.kind,
            grade: evt.grade,
            at_time: time_point,
            note_index: evt.note_index,
        }
    }).collect()
}

fn events_to_render_commands(events: &[JudgeEvent]) -> Vec<RenderCommand> {
    events.iter().map(|evt| {
        RenderCommand::ShowJudgeResult {
            kind: evt.kind,
            grade: evt.grade,
            diff: evt.diff,
            note_index: evt.note_index,
        }
    }).collect()
}

/// Main entry point: advance all notes one frame.
///
/// Processes notes in fixed order: tap → hold → touch → touch-hold → slide
pub fn step_frame(state: &mut GameState, input: &FrameInput) -> StepResult {
    let new_time = state.current_time + input.delta;
    let prev_score = state.score;
    state.current_time = new_time;

    let mut events = Vec::new();
    let mut audio_commands = Vec::new();
    let mut render_commands = Vec::new();
    let mut cursor = ClickCursor::new();

    // Process in semantic order
    process_tap_queues(state, input, &mut cursor, &mut events);
    process_hold_queues(state, input, &mut cursor, &mut events);
    process_active_holds(state, input, &mut cursor, &mut events);
    process_touch_queues(state, input, &mut cursor, &mut events);
    process_touch_hold_queues(state, input, &mut cursor, &mut events);
    process_slides(state, input, &mut events, &mut audio_commands, &mut render_commands);

    // Update score from events
    state.score = fold_events_into_score(&prev_score, &events);

    // Generate audio/render commands from events
    audio_commands.extend(events_to_audio_commands(&events, new_time));
    render_commands.extend(events_to_render_commands(&events));

    StepResult {
        state: state.clone(),
        events,
        audio_commands,
        render_commands,
    }
}

/// Convenience wrapper with timed input
pub fn step_frame_timed(state: &mut GameState, batch: &TimedInputBatch) -> StepResult {
    let input = batch.to_frame_input(
        batch.current_time - state.current_time,
        &state.prev_button,
        &state.prev_sensor,
    );
    state.current_batch = Some(batch.clone());
    step_frame(state, &input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_click_cursor() {
        let input = FrameInput {
            button_click_count: ButtonVec::replicate(0).set(ButtonZone::K1, 2),
            ..Default::default()
        };
        let mut cursor = ClickCursor::new();

        assert!(cursor.consume_button_click(&input, ButtonZone::K1));
        assert!(cursor.consume_button_click(&input, ButtonZone::K1));
        assert!(!cursor.consume_button_click(&input, ButtonZone::K1));
        assert!(!cursor.consume_button_click(&input, ButtonZone::K2));
    }

    #[test]
    fn test_step_frame_empty() {
        let mut state = GameState::default();
        let input = FrameInput::default();
        let result = step_frame(&mut state, &input);

        assert_eq!(result.events.len(), 0);
        assert_eq!(result.state.current_time, input.delta);
    }

    #[test]
    fn test_group_share_result() {
        let mut groups = Vec::new();
        update_group_state(&mut groups, 1, 2, JudgeGrade::Perfect, 1000);
        assert!(group_share_result(&groups, 1).is_none()); // 1/2 not strict majority

        update_group_state(&mut groups, 1, 2, JudgeGrade::Perfect, 1000);
        assert!(group_share_result(&groups, 1).is_some()); // 2/2 = strict majority
    }
}
