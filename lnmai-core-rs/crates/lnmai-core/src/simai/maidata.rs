//! maidata.txt front-end and chart lowering.
//!
//! Mirrors `LnmaiCore/Simai/Source/Maidata.lean`.

use crate::rat::Rat;
use crate::simai::ir::{
    FrontendChartInspection, FrontendChartResult, FrontendSemanticChart,
};
use crate::simai::normalize::{lower_raw_tokens, to_chart_spec};
use crate::simai::syntax::{
    MaidataChartBlock, MaidataFile, MaidataMetadata, ParseError, ParseErrorKind, RawNoteKind,
    RawNoteToken, SourceChart, SourceEvent, SourceNote,
};
use crate::simai::timing::{parse_rat_def, trim};
use crate::simai::tokenize::{parse_segments, strip_comments};
use crate::simai::typecheck::typecheck_slides;
use crate::time::{self, TimePoint};

fn same_event_key(token: &RawNoteToken, timing: TimePoint, bpm: Rat, h_speed: Rat, divisor: usize) -> bool {
    token.timing == timing && token.bpm == bpm && token.h_speed == h_speed && token.divisor == divisor
}

fn is_expanded_slide_group_child(token: &RawNoteToken) -> bool {
    token.kind == RawNoteKind::Slide && matches!(token.source_group_index, Some(i) if i != 0)
}

fn majdata_timing_note_count(tokens: &[RawNoteToken]) -> usize {
    tokens.iter().filter(|t| !is_expanded_slide_group_child(t)).count()
}

fn majdata_no_head_slide_count(tokens: &[RawNoteToken]) -> usize {
    tokens
        .iter()
        .filter(|t| !is_expanded_slide_group_child(t) && t.kind == RawNoteKind::Slide && t.is_slide_no_head)
        .count()
}

fn tag_touch_each_group(group_id: usize, tokens: &[RawNoteToken]) -> Vec<RawNoteToken> {
    let timing_note_count = majdata_timing_note_count(tokens);
    let non_no_head_count = timing_note_count.saturating_sub(majdata_no_head_slide_count(tokens));
    let touch_is_each = timing_note_count > 1 && non_no_head_count != 1;
    let touch_hold_is_each = timing_note_count > 1;
    let mut out = Vec::with_capacity(tokens.len());
    let mut index = 0usize;
    for token in tokens {
        let should_tag = match token.kind {
            RawNoteKind::Touch => touch_is_each,
            RawNoteKind::TouchHold => touch_hold_is_each,
            _ => false,
        };
        if should_tag {
            out.push(RawNoteToken {
                source_group_id: Some(group_id),
                source_group_index: Some(index),
                source_group_size: Some(timing_note_count),
                ..token.clone()
            });
            index += 1;
        } else {
            out.push(token.clone());
        }
    }
    out
}

fn take_same_event_rest(
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    tokens: &[RawNoteToken],
) -> (Vec<RawNoteToken>, Vec<RawNoteToken>) {
    let mut same = Vec::new();
    let mut i = 0;
    while i < tokens.len() && same_event_key(&tokens[i], timing, bpm, h_speed, divisor) {
        same.push(tokens[i].clone());
        i += 1;
    }
    (same, tokens[i..].to_vec())
}

fn annotate_touch_each_groups_from(group_id: usize, tokens: &[RawNoteToken]) -> Vec<RawNoteToken> {
    let mut out = Vec::new();
    let mut rest: Vec<RawNoteToken> = tokens.to_vec();
    let mut gid = group_id;
    while let Some(token) = rest.first().cloned() {
        let (same, remaining) =
            take_same_event_rest(token.timing, token.bpm, token.h_speed, token.divisor, &rest[1..]);
        let mut event_tokens = vec![token];
        event_tokens.extend(same);
        out.extend(tag_touch_each_group(gid, &event_tokens));
        rest = remaining;
        gid += 1;
    }
    out
}

fn annotate_touch_each_groups(tokens: &[RawNoteToken]) -> Vec<RawNoteToken> {
    annotate_touch_each_groups_from(0, tokens)
}

fn source_chart_from_tokens(tokens: &[RawNoteToken]) -> SourceChart {
    let mut events: Vec<SourceEvent> = Vec::new();
    let mut current: Option<SourceEvent> = None;
    for token in tokens {
        let note = SourceNote { token: token.clone(), source_pos: token.source_pos };
        match &mut current {
            None => {
                current = Some(SourceEvent {
                    timing: token.timing,
                    bpm: token.bpm,
                    h_speed: token.h_speed,
                    divisor: token.divisor,
                    notes: vec![note],
                    source_pos: None,
                });
            }
            Some(event)
                if same_event_key(token, event.timing, event.bpm, event.h_speed, event.divisor) =>
            {
                event.notes.push(note);
            }
            Some(event) => {
                events.push(event.clone());
                current = Some(SourceEvent {
                    timing: token.timing,
                    bpm: token.bpm,
                    h_speed: token.h_speed,
                    divisor: token.divisor,
                    notes: vec![note],
                    source_pos: None,
                });
            }
        }
    }
    if let Some(event) = current {
        events.push(event);
    }
    SourceChart { events }
}

fn starts_with_amp(s: &str) -> bool {
    s.chars().next() == Some('&')
}

fn parse_key_value_line(line: &str) -> Option<(String, String)> {
    let key = match line.find('=') {
        Some(i) => trim(&line[..i]),
        None => trim(line),
    };
    let value = match line.find('=') {
        Some(i) => trim(&line[i + 1..]),
        None => String::new(),
    };
    Some((key, value))
}

fn collect_chart_body(lines: &[String], mut acc: Vec<String>) -> (Vec<String>, Vec<String>) {
    let mut i = 0usize;
    while i < lines.len() {
        if starts_with_amp(&lines[i]) {
            break;
        }
        acc.push(lines[i].clone());
        i += 1;
    }
    // `acc` is already in source order (`Lean` prepends then reverses; here we
    // append, so no reversal is needed).
    (acc, lines[i..].to_vec())
}

fn parse_maidata_lines(lines: &[String], fields: Vec<(String, String)>, charts: Vec<MaidataChartBlock>) -> MaidataFile {
    let mut fields = fields;
    let mut charts = charts;
    let mut rest: Vec<String> = lines.to_vec();
    while let Some(line) = rest.first().cloned() {
        if trim(&line).is_empty() {
            rest.remove(0);
            continue;
        }
        if starts_with_amp(&line) {
            match parse_key_value_line(&line) {
                Some((key, value)) => {
                    if key.starts_with("&inote_") {
                        let level_index = key.chars().skip(7).collect::<String>().parse::<usize>().unwrap_or(0);
                        let (body_lines, remaining) = collect_chart_body(&rest[1..], vec![value]);
                        let body = body_lines.join("\n");
                        charts.push(MaidataChartBlock { level_index, raw_body: body });
                        rest = remaining;
                    } else {
                        fields.push((key, value));
                        rest.remove(0);
                    }
                }
                None => {
                    rest.remove(0);
                }
            }
        } else {
            rest.remove(0);
        }
    }
    // `fields`/`charts` are appended in source order above; Lean builds them by
    // prepending and then reverses, so no extra reversal is needed here.
    MaidataFile { metadata: MaidataMetadata { fields }, charts }
}

fn metadata_field(md: &MaidataMetadata, key: &str) -> Option<String> {
    md.fields.iter().find(|(k, _)| k == key).map(|(_, v)| v.clone())
}

/// `parseSourceMaidata`.
pub fn parse_source_maidata(content: &str) -> Result<MaidataFile, ParseError> {
    let lines: Vec<String> = content.split('\n').map(|s| s.to_string()).collect();
    Ok(parse_maidata_lines(&lines, Vec::new(), Vec::new()))
}

/// `lowerSourceChartBlock`.
pub fn lower_source_chart_block(
    file: &MaidataFile,
    block: &MaidataChartBlock,
) -> Result<FrontendChartResult, ParseError> {
    let base_bpm = parse_rat_def(&metadata_field(&file.metadata, "&wholebpm").unwrap_or_else(|| "120".to_string()), Rat::from_int(120));
    let first_offset = match time::parse_seconds_point_string(
        &metadata_field(&file.metadata, "&first").unwrap_or_else(|| "0".to_string()),
    ) {
        Some(value) => value,
        None => TimePoint::zero(),
    };
    let cleaned_body = strip_comments(&block.raw_body);
    let segments: Vec<String> = cleaned_body.split(',').map(|s| s.to_string()).collect();
    let raw_tokens = parse_segments(&segments, first_offset, base_bpm, Rat::one(), 4)?;
    let tokens = annotate_touch_each_groups(&raw_tokens);
    let _typed = typecheck_slides(&tokens)?;
    let source = source_chart_from_tokens(&tokens);
    let measure_dur_sec = |bpm: Rat| time::duration_from_rat_micros(time::bpm_measure_micros_rat(bpm));
    let (normalized, slide_notes) = lower_raw_tokens(&measure_dur_sec, &tokens);
    let lowered = to_chart_spec(&normalized);
    Ok(FrontendChartResult {
        semantic: FrontendSemanticChart { normalized, lowered },
        inspection: FrontendChartInspection {
            metadata: file.metadata.clone(),
            chart: block.clone(),
            source,
            tokens,
            slide_notes,
        },
    })
}

/// `lowerSourceChartByLevel`.
pub fn lower_source_chart_by_level(file: &MaidataFile, level_index: usize) -> Result<FrontendChartResult, ParseError> {
    match file.charts.iter().find(|block| block.level_index == level_index) {
        Some(block) => lower_source_chart_block(file, block),
        None => Err(ParseError {
            kind: ParseErrorKind::InvalidSyntax,
            raw_text: String::new(),
            message: format!("missing inote block {}", level_index),
            span: None,
        }),
    }
}

/// `parseAndLowerSourceMaidata`.
pub fn parse_and_lower_source_maidata(content: &str, level_index: usize) -> Result<FrontendChartResult, ParseError> {
    let file = parse_source_maidata(content)?;
    lower_source_chart_by_level(&file, level_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::areas::OuterSlot;

    const MAIDATA: &str = "&title=Test\n&wholebpm=120\n&first=0\n&inote_1=\n(120)\n1,2,3,\n";

    #[test]
    fn parse_and_lower() {
        let file = parse_source_maidata(MAIDATA).unwrap();
        assert_eq!(file.charts.len(), 1);
        assert_eq!(file.charts[0].level_index, 1);
        let result = parse_and_lower_source_maidata(MAIDATA, 1).unwrap();
        assert_eq!(result.semantic.lowered.taps.len(), 3);
        assert_eq!(result.semantic.normalized.taps.len(), 3);
    }

    #[test]
    fn key_value_line() {
        assert_eq!(parse_key_value_line("&title=A=B"), Some(("&title".to_string(), "A=B".to_string())));
        assert_eq!(parse_key_value_line("bare"), Some(("bare".to_string(), "".to_string())));
    }

    #[test]
    fn multi_measure_body_keeps_source_order() {
        // Regression: `collect_chart_body` used to reverse the body lines, so
        // later measures were lowered before earlier ones. Expected timings are
        // the ones Lean (`compileRuntimeChartSection`) produces for this body.
        let content = "&first=0\n&inote_1=\n(210){4},{8},,,,,,,,6,\n5,4,3,4,5,\n";
        let result = parse_and_lower_source_maidata(content, 1).unwrap();
        let taps = &result.semantic.lowered.taps;
        assert_eq!(taps.len(), 6);
        let slots: Vec<_> = taps.iter().map(|t| t.slot).collect();
        assert_eq!(
            slots,
            vec![OuterSlot::S6, OuterSlot::S5, OuterSlot::S4, OuterSlot::S3, OuterSlot::S4, OuterSlot::S5]
        );
        let times: Vec<i64> = taps.iter().map(|t| t.timing.to_micros()).collect();
        assert_eq!(times, vec![1_428_570, 1_571_427, 1_714_284, 1_857_141, 1_999_998, 2_142_855]);
    }
}
