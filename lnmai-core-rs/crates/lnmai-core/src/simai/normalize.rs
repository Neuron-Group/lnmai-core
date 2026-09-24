//! Lower raw tokens into the normalized chart IR and then into `ChartSpec`.
//!
//! Mirrors `LnmaiCore/Simai/Normalize.lean`.

use crate::chart_loader::{
    ChartSpec, HoldChartNote, SlideChartNote, SlideHeadChartNote, TapChartNote, TouchChartNote,
    TouchHoldChartNote,
};
use crate::rat::Rat;
use crate::simai::ir::{
    NormalizedChart, NormalizedHold, NormalizedSlide, NormalizedSlideDebug, NormalizedTap,
    NormalizedTouch, NormalizedTouchHold,
};
use crate::simai::shape::{shape_key, shape_kind};
use crate::simai::slide_parser::{
    parse_slide_just_text, parse_slide_note, parse_slide_note_from_body, parse_terminal_end_area,
};
use crate::simai::slide_tables::{
    judge_queues_for_shape, rotate_judge_queues, SlideAreaSpec,
};
use crate::simai::syntax::{RawNoteKind, RawNoteToken, SlideKind as ShapeSlideKind, SlideNoteSemantics};
use crate::simai::timing::note_timing_increment;
use crate::time::{time_point_add_duration, Duration, TimePoint};
use crate::types::SlideKind;

fn total_judge_queue_len(queues: &[Vec<SlideAreaSpec>]) -> usize {
    queues.iter().map(|q| q.len()).max().unwrap_or(0)
}

fn apply_single_track_conn_rules_normalized(
    slide: &NormalizedSlide,
    queue: &[SlideAreaSpec],
) -> Vec<SlideAreaSpec> {
    if !slide.is_conn_slide {
        queue.to_vec()
    } else if slide.total_judge_queue_len < 4 {
        match queue {
            [] => Vec::new(),
            [single] => {
                vec![SlideAreaSpec {
                    is_skippable: slide.is_group_head || slide.is_group_end,
                    ..single.clone()
                }]
            }
            [first, second, rest @ ..] => {
                let mut out = Vec::with_capacity(queue.len());
                out.push(SlideAreaSpec { is_skippable: slide.is_group_head, ..first.clone() });
                out.push(SlideAreaSpec { is_skippable: slide.is_group_end, ..second.clone() });
                out.extend_from_slice(rest);
                out
            }
        }
    } else {
        queue.to_vec()
    }
}

fn attach_judge_queues(slide: &NormalizedSlide) -> NormalizedSlide {
    let raw_queues = judge_queues_for_shape(slide.simai_shape, slide.is_classic).unwrap_or_default();
    let placed_queues = rotate_judge_queues(slide.slot.to_index() as usize, &raw_queues);
    let with_len = NormalizedSlide {
        total_judge_queue_len: total_judge_queue_len(&placed_queues),
        ..slide.clone()
    };
    let queues: Vec<Vec<SlideAreaSpec>> = match placed_queues.as_slice() {
        [queue] => vec![apply_single_track_conn_rules_normalized(&with_len, queue)],
        _ => placed_queues,
    };
    NormalizedSlide {
        judge_queues: queues.clone(),
        total_judge_queue_len: total_judge_queue_len(&queues),
        ..with_len
    }
}

fn slide_debug_for(chart: &NormalizedChart, note_index: usize) -> Option<NormalizedSlideDebug> {
    chart.slide_debug.iter().find(|dbg| dbg.note_index == note_index).cloned()
}

/// `lowerSlideToken`.
pub fn lower_slide_token(
    note_index: usize,
    token: &RawNoteToken,
) -> Option<(NormalizedSlide, SlideNoteSemantics)> {
    let slot = token.slot?;
    let end_area = parse_terminal_end_area(&token.raw_text).ok()?;
    let parsed = match &token.slide_body {
        Some(body) => parse_slide_note_from_body(&token.raw_text, body, end_area),
        None => parse_slide_note(&token.raw_text, slot, end_area),
    }
    .ok()?;
    let is_wifi = shape_kind(parsed.shape) == ShapeSlideKind::Wifi;
    let length = token
        .length
        .unwrap_or_else(|| note_timing_increment(token.bpm, token.divisor.max(1)));
    let star_wait = token.star_wait.unwrap_or_else(Duration::zero);
    let start_timing = time_point_add_duration(token.timing, star_wait);
    let judge_at = time_point_add_duration(start_timing, length);
    let slide = NormalizedSlide {
        head_timing: token.timing,
        slot,
        length,
        start_timing,
        h_speed: token.h_speed,
        slide_kind: if is_wifi { SlideKind::Wifi } else { SlideKind::Single },
        is_classic: false,
        track_count: if is_wifi { 3 } else { 1 },
        judge_at: Some(judge_at),
        is_break: token.is_break,
        is_ex: token.is_ex,
        is_hanabi: token.is_hanabi,
        has_head_note: !token.is_slide_no_head,
        has_body: true,
        is_slide_no_head: token.is_slide_no_head,
        is_force_star: token.is_force_star,
        is_fake_rotate: token.is_fake_rotate,
        is_slide_break: token.is_slide_break,
        source_group_id: token.source_group_id,
        source_group_index: token.source_group_index,
        source_group_size: token.source_group_size,
        multiple: 1,
        note_index,
        simai_shape: parsed.shape,
        ..NormalizedSlide::default()
    };
    Some((slide, parsed))
}

fn apply_connected_slide_metadata(slides: &[NormalizedSlide]) -> Vec<NormalizedSlide> {
    let mut acc: Vec<NormalizedSlide> = Vec::new();
    let mut current_group_id: Option<usize> = None;
    let mut parent_note_index: Option<usize> = None;
    let mut parent_end_timing: Option<TimePoint> = None;
    let mut group_start_timing_state: Option<TimePoint> = None;

    for slide in slides {
        match (slide.source_group_id, slide.source_group_index, slide.source_group_size) {
            (Some(gid), Some(idx), Some(size)) => {
                let actual_parent = if idx == 0 { None } else { parent_note_index };
                let is_conn = size > 1;
                let slide_kind = if !is_conn {
                    slide.slide_kind
                } else if slide.slide_kind == SlideKind::Wifi {
                    SlideKind::Wifi
                } else {
                    SlideKind::ConnPart
                };
                let start_timing = if idx == 0 {
                    slide.start_timing
                } else {
                    parent_end_timing.unwrap_or(slide.start_timing)
                };
                let group_start_timing = if idx == 0 {
                    slide.start_timing
                } else {
                    group_start_timing_state.unwrap_or(slide.start_timing)
                };
                let stored_group_start = if is_conn { Some(group_start_timing) } else { None };
                let start_shift = slide.start_timing - start_timing;
                let judge_at = slide.judge_at.map(|tp| time_point_add_duration(tp, start_shift));
                let updated = NormalizedSlide {
                    start_timing,
                    group_start_timing: stored_group_start,
                    judge_at,
                    slide_kind,
                    is_conn_slide: is_conn,
                    parent_note_index: actual_parent,
                    is_group_head: idx == 0,
                    is_group_end: idx + 1 == size,
                    ..slide.clone()
                };
                let next_parent_end = time_point_add_duration(updated.start_timing, updated.length);
                let is_end = idx + 1 == size;
                let reset_chain = current_group_id != Some(gid) && idx != 0;
                if reset_chain {
                    acc.push(NormalizedSlide {
                        is_conn_slide: false,
                        is_group_head: false,
                        is_group_end: false,
                        parent_note_index: None,
                        ..slide.clone()
                    });
                    current_group_id = None;
                    parent_note_index = None;
                    parent_end_timing = None;
                    group_start_timing_state = None;
                } else {
                    acc.push(updated.clone());
                    current_group_id = if is_end { None } else { Some(gid) };
                    parent_note_index = if is_end { None } else { Some(updated.note_index) };
                    parent_end_timing = if is_end { None } else { Some(next_parent_end) };
                    group_start_timing_state = if is_end { None } else { Some(group_start_timing) };
                }
            }
            _ => {
                acc.push(NormalizedSlide {
                    is_conn_slide: false,
                    is_group_head: false,
                    is_group_end: false,
                    parent_note_index: None,
                    ..slide.clone()
                });
                current_group_id = None;
                parent_note_index = None;
                parent_end_timing = None;
                group_start_timing_state = None;
            }
        }
    }
    acc
}

fn slide_fold_fields_match(left: &NormalizedSlide, right: &NormalizedSlide) -> bool {
    left.head_timing == right.head_timing
        && left.slot == right.slot
        && left.length == right.length
        && left.start_timing == right.start_timing
        && left.h_speed == right.h_speed
        && left.slide_kind == right.slide_kind
        && left.is_classic == right.is_classic
        && left.track_count == right.track_count
        && left.judge_at == right.judge_at
        && left.is_break == right.is_break
        && left.is_ex == right.is_ex
        && left.is_hanabi == right.is_hanabi
        && left.has_head_note == right.has_head_note
        && left.has_body == right.has_body
        && left.is_slide_no_head == right.is_slide_no_head
        && left.is_force_star == right.is_force_star
        && left.is_fake_rotate == right.is_fake_rotate
        && left.is_slide_break == right.is_slide_break
        && left.source_group_index == right.source_group_index
        && left.source_group_size == right.source_group_size
        && left.simai_shape == right.simai_shape
}

fn take_same_source_group(gid: usize, slides: &[NormalizedSlide]) -> (Vec<NormalizedSlide>, Vec<NormalizedSlide>) {
    let mut group = Vec::new();
    let mut i = 0;
    while i < slides.len() && slides[i].source_group_id == Some(gid) {
        group.push(slides[i].clone());
        i += 1;
    }
    (group, slides[i..].to_vec())
}

fn split_slide_multiplicity_units(slides: &[NormalizedSlide]) -> Vec<Vec<NormalizedSlide>> {
    let mut units = Vec::new();
    let mut rest: Vec<NormalizedSlide> = slides.to_vec();
    while !rest.is_empty() {
        match rest[0].source_group_id {
            None => {
                units.push(vec![rest[0].clone()]);
                rest.remove(0);
            }
            Some(gid) => {
                let (group_tail, remaining) = take_same_source_group(gid, &rest[1..]);
                let mut unit = vec![rest[0].clone()];
                unit.extend(group_tail);
                units.push(unit);
                rest = remaining;
            }
        }
    }
    units
}

fn slide_multiplicity_units_can_fold(left: &[NormalizedSlide], right: &[NormalizedSlide]) -> bool {
    match (left.first(), right.first()) {
        (Some(lh), Some(rh)) => match (lh.source_group_id, rh.source_group_id) {
            (None, None) => match (left, right) {
                ([ls], [rs]) => slide_fold_fields_match(ls, rs),
                _ => false,
            },
            (Some(lg), Some(rg)) => {
                lg != rg
                    && left.len() == right.len()
                    && left.iter().zip(right.iter()).all(|(l, r)| slide_fold_fields_match(l, r))
            }
            _ => false,
        },
        _ => false,
    }
}

fn merge_slide_multiplicity_unit(
    left: &[NormalizedSlide],
    right: &[NormalizedSlide],
) -> Vec<NormalizedSlide> {
    left.iter()
        .zip(right.iter())
        .map(|(l, r)| NormalizedSlide { multiple: l.multiple + r.multiple, ..l.clone() })
        .collect()
}

fn fold_slide_multiplicity(slides: &[NormalizedSlide]) -> Vec<NormalizedSlide> {
    let mut units: Vec<Vec<NormalizedSlide>> = Vec::new();
    for unit in split_slide_multiplicity_units(slides) {
        if let Some(pos) = units.iter().position(|u| slide_multiplicity_units_can_fold(u, &unit)) {
            let merged = merge_slide_multiplicity_unit(&units[pos], &unit);
            units[pos] = merged;
        } else {
            units.push(unit);
        }
    }
    units.into_iter().flatten().collect()
}

/// `lowerRawTokens`.
pub fn lower_raw_tokens(
    measure_dur_sec: &dyn Fn(Rat) -> Duration,
    tokens: &[RawNoteToken],
) -> (NormalizedChart, Vec<SlideNoteSemantics>) {
    let mut note_index: usize = 1;
    let mut taps: Vec<NormalizedTap> = Vec::new();
    let mut holds: Vec<NormalizedHold> = Vec::new();
    let mut touches: Vec<NormalizedTouch> = Vec::new();
    let mut touch_holds: Vec<NormalizedTouchHold> = Vec::new();
    let mut slides: Vec<NormalizedSlide> = Vec::new();
    let mut slide_debug: Vec<NormalizedSlideDebug> = Vec::new();
    let mut slide_semantics: Vec<SlideNoteSemantics> = Vec::new();

    for token in tokens {
        match token.kind {
            RawNoteKind::Tap => {
                if let Some(slot) = token.slot {
                    taps.push(NormalizedTap {
                        timing: token.timing,
                        slot,
                        is_break: token.is_break,
                        is_ex: token.is_ex,
                        is_hanabi: token.is_hanabi,
                        is_force_star: token.is_force_star,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            RawNoteKind::Hold => {
                if let Some(slot) = token.slot {
                    holds.push(NormalizedHold {
                        timing: token.timing,
                        slot,
                        length: token.length.unwrap_or_else(|| measure_dur_sec(token.bpm)),
                        is_break: token.is_break,
                        is_ex: token.is_ex,
                        is_hanabi: token.is_hanabi,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            RawNoteKind::Touch => {
                if let Some(sensor_pos) = token.sensor_pos {
                    touches.push(NormalizedTouch {
                        timing: token.timing,
                        sensor_pos,
                        is_break: token.is_break,
                        is_hanabi: token.is_hanabi,
                        source_group_id: token.source_group_id,
                        source_group_index: token.source_group_index,
                        source_group_size: token.source_group_size,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            RawNoteKind::TouchHold => {
                if let Some(sensor_pos) = token.sensor_pos {
                    touch_holds.push(NormalizedTouchHold {
                        timing: token.timing,
                        sensor_pos,
                        length: token.length.unwrap_or_else(|| measure_dur_sec(token.bpm)),
                        is_break: token.is_break,
                        is_ex: token.is_ex,
                        is_hanabi: token.is_hanabi,
                        source_group_id: token.source_group_id,
                        source_group_index: token.source_group_index,
                        source_group_size: token.source_group_size,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            RawNoteKind::Slide => {
                if let Some((slide, parsed)) = lower_slide_token(note_index, token) {
                    slides.push(slide);
                    slide_debug.push(NormalizedSlideDebug {
                        note_index,
                        raw_text: token.raw_text.clone(),
                    });
                    slide_semantics.push(parsed);
                    note_index += 1;
                }
            }
            _ => {}
        }
    }

    let folded = fold_slide_multiplicity(&slides);
    let folded = apply_connected_slide_metadata(&folded);
    let lowered_slides: Vec<NormalizedSlide> = folded.iter().map(attach_judge_queues).collect();

    (
        NormalizedChart {
            taps,
            holds,
            touches,
            touch_holds,
            slides: lowered_slides,
            slide_debug,
            slide_skipping: true,
        },
        slide_semantics,
    )
}

/// `toChartSpec`.
pub fn to_chart_spec(chart: &NormalizedChart) -> ChartSpec {
    let mut max_note_index = 0usize;
    for note in &chart.taps {
        max_note_index = max_note_index.max(note.note_index);
    }
    for note in &chart.holds {
        max_note_index = max_note_index.max(note.note_index);
    }
    for note in &chart.touches {
        max_note_index = max_note_index.max(note.note_index);
    }
    for note in &chart.touch_holds {
        max_note_index = max_note_index.max(note.note_index);
    }
    for note in &chart.slides {
        max_note_index = max_note_index.max(note.note_index);
    }

    let mut next_note_index = max_note_index + 1;
    let mut slide_heads_rev: Vec<SlideHeadChartNote> = Vec::new();
    for note in &chart.slides {
        if !note.has_head_note {
            continue;
        }
        let count = note.multiple.max(1);
        let mut generated: Vec<SlideHeadChartNote> = (0..count)
            .map(|offset| SlideHeadChartNote {
                timing: note.head_timing,
                slot: note.slot,
                is_break: note.is_break,
                is_ex: note.is_ex,
                logical_slide_id: note.note_index,
                note_index: next_note_index + offset,
            })
            .collect();
        generated.reverse();
        generated.extend(slide_heads_rev);
        slide_heads_rev = generated;
        next_note_index += count;
    }
    slide_heads_rev.reverse();

    ChartSpec {
        taps: chart
            .taps
            .iter()
            .map(|note| TapChartNote {
                timing: note.timing,
                slot: note.slot,
                is_break: note.is_break,
                is_ex: note.is_ex,
                note_index: note.note_index,
                ..TapChartNote::default()
            })
            .collect(),
        holds: chart
            .holds
            .iter()
            .map(|note| HoldChartNote {
                timing: note.timing,
                slot: note.slot,
                length: note.length,
                is_break: note.is_break,
                is_ex: note.is_ex,
                is_touch: false,
                note_index: note.note_index,
                ..HoldChartNote::default()
            })
            .collect(),
        touches: chart
            .touches
            .iter()
            .map(|note| TouchChartNote {
                timing: note.timing,
                sensor_pos: note.sensor_pos,
                is_break: note.is_break,
                source_group_id: note.source_group_id,
                source_group_index: note.source_group_index,
                source_group_size: note.source_group_size,
                note_index: note.note_index,
                ..TouchChartNote::default()
            })
            .collect(),
        touch_holds: chart
            .touch_holds
            .iter()
            .map(|note| TouchHoldChartNote {
                timing: note.timing,
                sensor_pos: note.sensor_pos,
                length: note.length,
                is_break: note.is_break,
                is_ex: note.is_ex,
                source_group_id: note.source_group_id,
                source_group_index: note.source_group_index,
                source_group_size: note.source_group_size,
                note_index: note.note_index,
                ..TouchHoldChartNote::default()
            })
            .collect(),
        slide_heads: slide_heads_rev,
        slides: chart
            .slides
            .iter()
            .map(|note| SlideChartNote {
                head_timing: note.head_timing,
                slot: note.slot,
                length: note.length,
                start_timing: note.start_timing,
                group_start_timing: note.group_start_timing,
                slide_kind: note.slide_kind,
                is_classic: note.is_classic,
                is_slide_no_head: note.is_slide_no_head,
                is_conn_slide: note.is_conn_slide,
                parent_note_index: note.parent_note_index,
                is_group_head: note.is_group_head,
                is_group_end: note.is_group_end,
                total_judge_queue_len: note.total_judge_queue_len,
                track_count: note.track_count,
                judge_at: note.judge_at,
                is_break: note.is_slide_break,
                is_ex: note.is_ex,
                multiple: note.multiple,
                logical_slide_id: note.note_index,
                note_index: note.note_index,
                judge_queues: note.judge_queues.clone(),
                debug_simai: slide_debug_for(chart, note.note_index).map(|dbg| {
                    let just = parse_slide_just_text(&dbg.raw_text).unwrap_or(false);
                    (dbg.raw_text, shape_key(note.simai_shape), just)
                }),
                ..SlideChartNote::default()
            })
            .collect(),
        slide_skipping: Some(chart.slide_skipping),
    }
}
