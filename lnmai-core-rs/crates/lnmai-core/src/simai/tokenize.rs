//! Simai tokenization: segments → `RawNoteToken`s.
//!
//! Mirrors `LnmaiCore/Simai/Tokenize.lean`.

use crate::areas::{OuterSlot, SensorArea};
use crate::rat::Rat;
use crate::simai::shape::{detect_shape_from_text, digit_to_nat, parse_slide_body_from_text};
use crate::simai::slide_tables::judge_queues_for_shape;
use crate::simai::syntax::{ParseError, ParseErrorKind, RawNoteKind, RawNoteToken};
use crate::simai::timing::{
    note_timing_increment, parse_duration_spec, parse_nat_def, parse_rat_def,
    parse_star_wait_spec, pseudo_increment, trim,
};
use crate::time::{time_point_add_duration, Duration, TimePoint};

fn syntax_error(raw_text: &str, message: &str) -> ParseError {
    ParseError {
        kind: ParseErrorKind::InvalidSyntax,
        raw_text: raw_text.to_string(),
        message: message.to_string(),
        span: None,
    }
}

/// `firstDigit?`.
pub fn first_digit(s: &str) -> Option<usize> {
    s.chars().find_map(digit_to_nat)
}

/// `leadingDigit?`.
pub fn leading_digit(s: &str) -> Option<usize> {
    s.chars().next().and_then(digit_to_nat)
}

/// `leadingTouchPos?`.
pub fn leading_touch_pos(s: &str) -> Option<usize> {
    let mut it = s.chars();
    match it.next() {
        Some('C') => Some(8),
        Some(_) => it.next().and_then(digit_to_nat),
        None => None,
    }
}

/// `touchAreaToSensorArea?`.
pub fn touch_area_to_sensor_area(s: &str) -> Option<SensorArea> {
    let mut it = s.chars();
    let area = it.next()?;
    if area == 'C' {
        return Some(SensorArea::C);
    }
    let d = it.next()?;
    let n = digit_to_nat(d)?;
    let pick = |a1, a2, a3, a4, a5, a6, a7, a8| match n {
        1 => a1,
        2 => a2,
        3 => a3,
        4 => a4,
        5 => a5,
        6 => a6,
        7 => a7,
        _ => a8,
    };
    match area {
        'A' => Some(pick(
            SensorArea::A1, SensorArea::A2, SensorArea::A3, SensorArea::A4,
            SensorArea::A5, SensorArea::A6, SensorArea::A7, SensorArea::A8,
        )),
        'D' => Some(pick(
            SensorArea::D1, SensorArea::D2, SensorArea::D3, SensorArea::D4,
            SensorArea::D5, SensorArea::D6, SensorArea::D7, SensorArea::D8,
        )),
        'E' => Some(pick(
            SensorArea::E1, SensorArea::E2, SensorArea::E3, SensorArea::E4,
            SensorArea::E5, SensorArea::E6, SensorArea::E7, SensorArea::E8,
        )),
        'B' => Some(pick(
            SensorArea::B1, SensorArea::B2, SensorArea::B3, SensorArea::B4,
            SensorArea::B5, SensorArea::B6, SensorArea::B7, SensorArea::B8,
        )),
        _ => None,
    }
}

/// `stripComments`.
pub fn strip_comments(s: &str) -> String {
    s.split('\n')
        .map(|line| line.split("||").next().unwrap_or(line))
        .collect::<Vec<_>>()
        .join("\n")
}

/// `stripPrefixDirectives`.
pub fn strip_prefix_directives(token: &str) -> String {
    let t = trim(token);
    if t.is_empty() {
        t
    } else if t.starts_with('{') {
        // drop through the first '}', then trim.
        match t.split_once('}') {
            Some((_, rest)) => trim(rest),
            None => t,
        }
    } else if t.starts_with('(') {
        match t.split_once(')') {
            Some((_, rest)) => trim(rest),
            None => t,
        }
    } else if t.starts_with('<') {
        match t.split_once('>') {
            Some((_, rest)) => trim(rest),
            None => t,
        }
    } else {
        t
    }
}

/// `sanitizeSlideToken`.
pub fn sanitize_slide_token(token: &str) -> String {
    strip_prefix_directives(token)
        .chars()
        .filter(|c| !matches!(c, 'b' | 'x' | 'f' | '!' | '?' | '$'))
        .collect()
}

/// `isTouchAreaChar`.
pub fn is_touch_area_char(c: char) -> bool {
    matches!(c, 'A' | 'B' | 'C' | 'D' | 'E')
}

/// `isSlideMarkChar`.
pub fn is_slide_mark_char(c: char) -> bool {
    matches!(c, '-' | '^' | 'v' | '<' | '>' | 'V' | 'p' | 'q' | 's' | 'z' | 'w')
}

/// `isSlideText`.
pub fn is_slide_text(t: &str) -> bool {
    t.chars().any(is_slide_mark_char)
}

/// `inferKind`.
pub fn infer_kind(token: &str) -> RawNoteKind {
    let t = strip_prefix_directives(token);
    if t.is_empty() {
        RawNoteKind::Rest
    } else if leading_digit(&t).is_some() {
        if is_slide_text(&t) {
            RawNoteKind::Slide
        } else if t.contains('h') {
            RawNoteKind::Hold
        } else {
            RawNoteKind::Tap
        }
    } else {
        match t.chars().next() {
            Some(area) if is_touch_area_char(area) => {
                if t.contains('h') { RawNoteKind::TouchHold } else { RawNoteKind::Touch }
            }
            _ => RawNoteKind::Unknown,
        }
    }
}

/// `splitTopLevel`.
pub fn split_top_level(sep: char, s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut depth: usize = 0;
    let mut current = String::new();
    for c in s.chars() {
        match c {
            '[' => {
                depth += 1;
                current.push('[');
            }
            ']' => {
                depth = depth.saturating_sub(1);
                current.push(']');
            }
            _ if c == sep && depth == 0 => {
                result.push(std::mem::take(&mut current));
            }
            _ => current.push(c),
        }
    }
    result.push(current);
    result
}

/// `splitEntryTokens`.
pub fn split_entry_tokens(entry: &str) -> Vec<String> {
    split_top_level('/', entry).into_iter().map(|t| trim(&t)).filter(|t| !t.is_empty()).collect()
}

fn take_until_slide_mark(text: &str) -> String {
    let mut acc = String::new();
    for c in text.chars() {
        if is_slide_mark_char(c) {
            break;
        }
        acc.push(c);
    }
    acc
}

/// `parseHeadBreak`.
pub fn parse_head_break(token: &str) -> bool {
    let t = strip_prefix_directives(token);
    if is_slide_text(&t) {
        take_until_slide_mark(&t).contains('b')
    } else {
        t.contains('b')
    }
}

/// `parseSlideSegmentBreak`.
pub fn parse_slide_segment_break(token: &str) -> bool {
    let t = strip_prefix_directives(token);
    if !is_slide_text(&t) {
        false
    } else {
        match t.split('[').next() {
            Some(head) => head.ends_with('b'),
            None => false,
        }
    }
}

/// `parseHSpeedDirective`.
pub fn parse_h_speed_directive(text: &str, current: Rat) -> Rat {
    let t = trim(text);
    if !t.starts_with("<H") {
        current
    } else {
        let body = t.split('>').next().unwrap_or("");
        let body = body.chars().skip(2).collect::<String>();
        let value_text = if body.starts_with("S*") {
            body.chars().skip(2).collect::<String>()
        } else {
            body
        };
        parse_rat_def(&value_text, current)
    }
}

/// `applyInlineDirective`.
pub fn apply_inline_directive(
    bpm: Rat,
    divisor: usize,
    h_speed: Rat,
    segment: &str,
) -> (Rat, usize, Rat, String) {
    let mut bpm = bpm;
    let mut divisor = divisor;
    let mut h_speed = h_speed;
    let mut t = trim(segment);
    loop {
        if t.starts_with('(') {
            let after = t.chars().skip(1).collect::<String>();
            match after.split_once(')') {
                Some((inside, rest)) => {
                    bpm = parse_rat_def(inside, bpm);
                    t = trim(rest);
                }
                None => break,
            }
        } else if t.starts_with('{') {
            let after = t.chars().skip(1).collect::<String>();
            match after.split_once('}') {
                Some((inside, rest)) => {
                    divisor = parse_nat_def(inside, divisor);
                    t = trim(rest);
                }
                None => break,
            }
        } else if t.starts_with("<H") {
            let after = match t.split_once('>') {
                Some((_, rest)) => rest.to_string(),
                None => t.clone(),
            };
            h_speed = parse_h_speed_directive(&t, h_speed);
            t = after;
        } else {
            break;
        }
    }
    (bpm, divisor, h_speed, t)
}

/// `mkRawToken`.
pub fn mk_raw_token(
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    token: &str,
) -> RawNoteToken {
    let t = trim(token);
    let kind = infer_kind(&t);
    let parsed_text = if kind == RawNoteKind::Slide { sanitize_slide_token(&t) } else { t.clone() };
    let slot = leading_digit(&parsed_text).and_then(|n| OuterSlot::of_index(n.saturating_sub(1)));
    let sensor_pos = touch_area_to_sensor_area(&t);
    let slide_body = if kind == RawNoteKind::Slide {
        parse_slide_body_from_text(&parsed_text).ok()
    } else {
        None
    };
    let length = parse_duration_spec(bpm, &t);
    let star_wait = if kind == RawNoteKind::Slide { parse_star_wait_spec(bpm, &t) } else { None };
    let is_break = parse_head_break(&t);
    let is_ex = t.contains('x');
    let is_hanabi = t.contains('f');
    let is_slide_no_head = t.contains('!') || t.contains('?');
    let is_force_star = t.contains('$');
    let is_fake_rotate = t.chars().filter(|c| *c == '$').count() >= 2;
    let is_slide_break = parse_slide_segment_break(&t);
    RawNoteToken {
        raw_text: parsed_text,
        kind,
        timing,
        bpm,
        h_speed,
        divisor,
        slot,
        sensor_pos,
        slide_body,
        length,
        star_wait,
        is_break,
        is_ex,
        is_hanabi,
        is_slide_no_head,
        is_force_star,
        is_fake_rotate,
        is_slide_break,
        source_group_id: None,
        source_group_index: None,
        source_group_size: None,
        source_pos: None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ContinuousChainSegment {
    raw_text: String,
    has_timing: bool,
}

fn read_digit_char(raw_text: &str, chars: &[char]) -> Result<(char, Vec<char>), ParseError> {
    match chars.split_first() {
        Some((c, rest)) if c.is_ascii_digit() => Ok((*c, rest.to_vec())),
        _ => Err(syntax_error(raw_text, "invalid connected slide syntax")),
    }
}

fn read_bracket_suffix(raw_text: &str, chars: &[char]) -> Result<(String, Vec<char>), ParseError> {
    let mut acc = String::from("[");
    let mut rest = chars;
    loop {
        match rest.split_first() {
            None => return Err(syntax_error(raw_text, "unterminated slide timing spec")),
            Some((c, tail)) => {
                acc.push(*c);
                if *c == ']' {
                    return Ok((acc, tail.to_vec()));
                }
                rest = tail;
            }
        }
    }
}

fn parse_slide_shape_chars(
    raw_text: &str,
    op: char,
    rest: &[char],
) -> Result<(String, Vec<char>), ParseError> {
    if op == 'V' {
        let (middle, rest) = read_digit_char(raw_text, rest)?;
        let (finish, rest) = read_digit_char(raw_text, &rest)?;
        Ok((format!("V{}{}", middle, finish), rest))
    } else {
        let (shape_text, rest) = if op == 'p' || op == 'q' {
            match rest.split_first() {
                Some((next, tail)) if *next == op => (format!("{}{}", op, next), tail.to_vec()),
                _ => (op.to_string(), rest.to_vec()),
            }
        } else {
            (op.to_string(), rest.to_vec())
        };
        let (finish, rest) = read_digit_char(raw_text, &rest)?;
        Ok((format!("{}{}", shape_text, finish), rest))
    }
}

fn parse_continuous_slide_segments_core(
    raw_text: &str,
    current_start: char,
    chars: &[char],
) -> Result<Vec<ContinuousChainSegment>, ParseError> {
    match chars.split_first() {
        None => Ok(Vec::new()),
        Some((c, rest)) => {
            if c.is_ascii_digit() {
                return Err(syntax_error(
                    raw_text,
                    "connected slide chain cannot contain a fresh numeric head",
                ));
            }
            if !is_slide_mark_char(*c) {
                return Err(syntax_error(raw_text, "invalid connected slide syntax"));
            }
            let (shape_and_end, rest) = parse_slide_shape_chars(raw_text, *c, rest)?;
            let segment_core = format!("{}{}", current_start, shape_and_end);
            let (timing_suffix, rest, has_timing) = match rest.split_first() {
                Some(('[', tail)) => {
                    let (suffix, rest) = read_bracket_suffix(raw_text, tail)?;
                    (suffix, rest, true)
                }
                _ => (String::new(), rest.clone(), false),
            };
            let end_char = shape_and_end.chars().last().unwrap_or(current_start);
            let mut tail = parse_continuous_slide_segments_core(raw_text, end_char, &rest)?;
            let mut out = vec![ContinuousChainSegment {
                raw_text: format!("{}{}", segment_core, timing_suffix),
                has_timing,
            }];
            out.append(&mut tail);
            Ok(out)
        }
    }
}

fn parse_continuous_slide_segments(
    token: &str,
) -> Result<Option<Vec<ContinuousChainSegment>>, ParseError> {
    let sanitized = sanitize_slide_token(token);
    let chars: Vec<char> = sanitized.chars().collect();
    match chars.split_first() {
        None => Ok(None),
        Some((start, rest)) => {
            if !start.is_ascii_digit() {
                Ok(None)
            } else {
                let segments = parse_continuous_slide_segments_core(&sanitized, *start, rest)?;
                if segments.len() <= 1 {
                    Ok(None)
                } else {
                    Ok(Some(segments))
                }
            }
        }
    }
}

fn segment_bar_count(raw_text: &str) -> Result<usize, ParseError> {
    let shape = detect_shape_from_text(raw_text)?;
    let queues = judge_queues_for_shape(shape, false).unwrap_or_default();
    let count = queues.iter().map(|q| q.len()).max().unwrap_or(0);
    if count == 0 {
        Err(syntax_error(raw_text, "missing slide table for connected slide segment"))
    } else {
        Ok(count)
    }
}

fn apply_shared_slide_flags(base: &RawNoteToken, token: &RawNoteToken, is_headless: bool) -> RawNoteToken {
    RawNoteToken {
        is_break: base.is_break,
        is_ex: base.is_ex,
        is_hanabi: base.is_hanabi,
        is_slide_no_head: is_headless,
        is_force_star: base.is_force_star,
        is_fake_rotate: base.is_fake_rotate,
        is_slide_break: base.is_slide_break,
        ..token.clone()
    }
}

fn tag_connected_group(group_id: usize, size: usize, tokens: Vec<RawNoteToken>) -> Vec<RawNoteToken> {
    tokens
        .into_iter()
        .enumerate()
        .map(|(index, tok)| RawNoteToken {
            source_group_id: Some(group_id),
            source_group_index: Some(index),
            source_group_size: Some(size),
            ..tok
        })
        .collect()
}

fn build_per_segment_chain_tokens(
    group_id: usize,
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    base: &RawNoteToken,
    segments: &[ContinuousChainSegment],
) -> Result<Vec<RawNoteToken>, ParseError> {
    let tokens: Vec<RawNoteToken> = segments
        .iter()
        .enumerate()
        .map(|(i, segment)| {
            let tok = mk_raw_token(timing, bpm, h_speed, divisor, &segment.raw_text);
            let headless = if i == 0 { base.is_slide_no_head } else { true };
            apply_shared_slide_flags(base, &tok, headless)
        })
        .collect();
    let size = tokens.len();
    Ok(tag_connected_group(group_id, size, tokens))
}

fn build_whole_duration_chain_tokens(
    group_id: usize,
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    base: &RawNoteToken,
    segments: &[ContinuousChainSegment],
) -> Result<Vec<RawNoteToken>, ParseError> {
    let total_length = base.length.ok_or_else(|| {
        syntax_error(&base.raw_text, "connected slide chain requires an explicit timing spec")
    })?;
    let mut bar_counts = Vec::with_capacity(segments.len());
    for segment in segments {
        bar_counts.push(segment_bar_count(&segment.raw_text)?);
    }
    let total_bars: usize = bar_counts.iter().sum();
    if total_bars == 0 {
        return Err(syntax_error(&base.raw_text, "connected slide chain has no measurable segments"));
    }
    let base_micros = total_length.to_micros() as i128;
    let tokens: Vec<RawNoteToken> = segments
        .iter()
        .zip(bar_counts.iter())
        .enumerate()
        .map(|(i, (segment, bars))| {
            let seg_tok = mk_raw_token(timing, bpm, h_speed, divisor, &segment.raw_text);
            let seg_len = Duration::from_micros((base_micros * (*bars as i128) / total_bars as i128) as i64);
            let seg_wait = if i == 0 { base.star_wait } else { None };
            let tok = RawNoteToken { length: Some(seg_len), star_wait: seg_wait, ..seg_tok };
            let headless = if i == 0 { base.is_slide_no_head } else { true };
            apply_shared_slide_flags(base, &tok, headless)
        })
        .collect();
    let size = tokens.len();
    Ok(tag_connected_group(group_id, size, tokens))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChainTimingLayout {
    PerSegment,
    OverallFinal,
}

fn classify_chain_timing_layout(
    raw_text: &str,
    segments: &[ContinuousChainSegment],
) -> Result<ChainTimingLayout, ParseError> {
    let flags: Vec<bool> = segments.iter().map(|s| s.has_timing).collect();
    if flags.iter().all(|f| *f) {
        Ok(ChainTimingLayout::PerSegment)
    } else {
        match flags.last() {
            Some(true) => {
                let rest_all_false = flags[..flags.len() - 1].iter().all(|f| !*f);
                if rest_all_false {
                    Ok(ChainTimingLayout::OverallFinal)
                } else {
                    Err(syntax_error(raw_text, "invalid connected slide timing layout"))
                }
            }
            Some(false) => {
                if flags.iter().all(|f| !*f) {
                    Err(syntax_error(
                        raw_text,
                        "connected slide chain requires either per-segment timing or a final overall timing spec",
                    ))
                } else {
                    Err(syntax_error(raw_text, "invalid connected slide timing layout"))
                }
            }
            None => Err(syntax_error(raw_text, "invalid connected slide timing layout")),
        }
    }
}

fn expand_continuous_chain_token(
    group_id: usize,
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    token: &str,
) -> Result<Vec<RawNoteToken>, ParseError> {
    let base = mk_raw_token(timing, bpm, h_speed, divisor, token);
    if base.kind != RawNoteKind::Slide {
        return Ok(vec![base]);
    }
    match parse_continuous_slide_segments(token)? {
        None => Ok(vec![base]),
        Some(segments) => match classify_chain_timing_layout(&base.raw_text, &segments)? {
            ChainTimingLayout::PerSegment => {
                build_per_segment_chain_tokens(group_id, timing, bpm, h_speed, divisor, &base, &segments)
            }
            ChainTimingLayout::OverallFinal => {
                build_whole_duration_chain_tokens(group_id, timing, bpm, h_speed, divisor, &base, &segments)
            }
        },
    }
}

fn same_head_group_parts(token: &str) -> Vec<String> {
    split_top_level('*', token).into_iter().map(|t| trim(&t)).filter(|t| !t.is_empty()).collect()
}

fn same_head_head_prefix(token: &str) -> String {
    let t = trim(&strip_prefix_directives(token));
    let chars: Vec<char> = t.chars().collect();
    match chars.split_first() {
        None => String::new(),
        Some((first, rest)) => {
            if is_touch_area_char(*first) {
                match (first, rest.split_first()) {
                    ('C', _) => "C".to_string(),
                    (_, Some((digit, _))) if digit.is_ascii_digit() => {
                        format!("{}{}", first, digit)
                    }
                    _ => first.to_string(),
                }
            } else if first.is_ascii_digit() {
                first.to_string()
            } else {
                String::new()
            }
        }
    }
}

fn expand_same_head_group_rest(
    group_id: usize,
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    head_prefix: &str,
    size: usize,
    start_idx: usize,
    parts: &[String],
) -> Result<Vec<RawNoteToken>, ParseError> {
    let mut out = Vec::with_capacity(parts.len());
    for (offset, part) in parts.iter().enumerate() {
        let rebuilt = if head_prefix.is_empty() { part.clone() } else { format!("{}{}", head_prefix, part) };
        let tok = mk_raw_token(timing, bpm, h_speed, divisor, &rebuilt);
        out.push(RawNoteToken {
            is_slide_no_head: true,
            source_group_id: Some(group_id),
            source_group_index: Some(start_idx + offset),
            source_group_size: Some(size),
            ..tok
        });
    }
    Ok(out)
}

fn expand_same_head_group(
    group_id: usize,
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    token: &str,
) -> Result<Vec<RawNoteToken>, ParseError> {
    let parts = same_head_group_parts(token);
    match parts.split_first() {
        None => Ok(Vec::new()),
        Some((first, rest)) => {
            let head_prefix = same_head_head_prefix(first);
            let mut first_tok = mk_raw_token(timing, bpm, h_speed, divisor, first);
            let first_is_grouped_slide = first_tok.kind == RawNoteKind::Slide;
            let grouped_slide_count = (if first_is_grouped_slide { 1 } else { 0 }) + rest.len();
            if first_is_grouped_slide {
                first_tok.source_group_id = Some(group_id);
                first_tok.source_group_index = Some(0);
                first_tok.source_group_size = Some(grouped_slide_count);
            }
            let rest_start_index = if first_is_grouped_slide { 1 } else { 0 };
            let rest_toks = expand_same_head_group_rest(
                group_id, timing, bpm, h_speed, divisor, &head_prefix, grouped_slide_count,
                rest_start_index, rest,
            )?;
            let mut out = vec![first_tok];
            out.extend(rest_toks);
            Ok(out)
        }
    }
}

fn expand_token_list(
    base_group_id: usize,
    timing: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
    texts: &[String],
) -> Result<Vec<RawNoteToken>, ParseError> {
    let mut out = Vec::new();
    for (idx, tok_text) in texts.iter().enumerate() {
        let current = if tok_text.contains('*') {
            expand_same_head_group(base_group_id + idx, timing, bpm, h_speed, divisor, tok_text)?
        } else {
            expand_continuous_chain_token(base_group_id + idx, timing, bpm, h_speed, divisor, tok_text)?
        };
        out.extend(current);
    }
    Ok(out)
}

/// `parseSegmentNotes`.
pub fn parse_segment_notes(
    segment: &str,
    time: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
) -> Result<Vec<RawNoteToken>, ParseError> {
    let normalized = trim(&segment.replace('\n', ""));
    if normalized.is_empty() {
        Ok(Vec::new())
    } else if normalized.contains('`') {
        let parts: Vec<&str> = normalized.split('`').collect();
        let mut current_time = time;
        let mut acc: Vec<RawNoteToken> = Vec::new();
        for part in parts {
            let tokens = expand_token_list(0, current_time, bpm, h_speed, divisor, &split_entry_tokens(part))?;
            current_time = time_point_add_duration(current_time, pseudo_increment(bpm));
            acc.extend(tokens);
        }
        Ok(acc)
    } else {
        expand_token_list(0, time, bpm, h_speed, divisor, &split_entry_tokens(&normalized))
    }
}

/// `parseSegments`.
pub fn parse_segments(
    segments: &[String],
    time: TimePoint,
    bpm: Rat,
    h_speed: Rat,
    divisor: usize,
) -> Result<Vec<RawNoteToken>, ParseError> {
    let mut acc: Vec<RawNoteToken> = Vec::new();
    let mut time = time;
    let mut bpm = bpm;
    let mut h_speed = h_speed;
    let mut divisor = divisor;
    for segment in segments {
        let clean = trim(segment);
        let (bpm2, divisor2, h_speed2, body) = apply_inline_directive(bpm, divisor, h_speed, &clean);
        let new_tokens = parse_segment_notes(&body, time, bpm2, h_speed2, divisor2)?;
        let mut rev: Vec<RawNoteToken> = new_tokens.into_iter().rev().collect();
        rev.extend(acc);
        acc = rev;
        time = time_point_add_duration(time, note_timing_increment(bpm2, divisor2));
        bpm = bpm2;
        h_speed = h_speed2;
        divisor = divisor2;
    }
    acc.reverse();
    Ok(acc)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn kind_inference() {
        assert_eq!(infer_kind("1"), RawNoteKind::Tap);
        assert_eq!(infer_kind("1h[4:1]"), RawNoteKind::Hold);
        assert_eq!(infer_kind("1-3"), RawNoteKind::Slide);
        assert_eq!(infer_kind("A1"), RawNoteKind::Touch);
        assert_eq!(infer_kind("Ch"), RawNoteKind::TouchHold);
        assert_eq!(infer_kind(""), RawNoteKind::Rest);
    }

    #[test]
    fn touch_area() {
        assert_eq!(touch_area_to_sensor_area("C"), Some(SensorArea::C));
        assert_eq!(touch_area_to_sensor_area("A3"), Some(SensorArea::A3));
        assert_eq!(touch_area_to_sensor_area("B8"), Some(SensorArea::B8));
        assert_eq!(touch_area_to_sensor_area("A9"), None);
    }

    #[test]
    fn split_entry() {
        assert_eq!(split_entry_tokens("1/2/3"), vec!["1", "2", "3"]);
        assert_eq!(split_entry_tokens("1-3[4:1]/2"), vec!["1-3[4:1]", "2"]);
    }

    #[test]
    fn segment_notes_tap() {
        let toks = parse_segment_notes("1", TimePoint::zero(), Rat::from_int(120), Rat::one(), 4).unwrap();
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].kind, RawNoteKind::Tap);
        assert_eq!(toks[0].slot, Some(OuterSlot::S1));
    }

    #[test]
    fn segments_advance_time() {
        let toks = parse_segments(
            &["1".to_string(), "2".to_string()],
            TimePoint::zero(),
            Rat::from_int(120),
            Rat::one(),
            4,
        )
        .unwrap();
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[1].timing.to_micros(), 500_000);
    }

    #[test]
    fn inline_bpm_directive() {
        let (bpm, _, _, body) = apply_inline_directive(Rat::from_int(120), 4, Rat::one(), "(240)1");
        assert_eq!(bpm, Rat::from_int(240));
        assert_eq!(body, "1");
    }
}
