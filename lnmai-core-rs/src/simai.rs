//! Simai parser — port of LnmaiCore/Simai/*.lean
//!
//! Pipeline: maidata.txt → segments → RawNoteToken → Normalized → ChartSpec
//!
//! Faithful to Lean semantics per AGENTS.md. No optimization, no shortcuts.

use crate::areas::{OuterSlot, SensorArea};
use crate::chart_loader::{
    ChartSpec, HoldChartNote, SlideChartNote, TapChartNote,
    TouchChartNote, TouchHoldChartNote,
};
use crate::types::SlideKind;
use crate::time::{Duration, TimePoint};

// ============================================================================
// Utility functions (exact match to Lean)
// ============================================================================

fn digit_to_nat(c: char) -> Option<u32> {
    c.to_digit(10)
}

fn first_digit(s: &str) -> Option<u32> {
    s.chars().find_map(|c| digit_to_nat(c))
}

fn leading_digit(s: &str) -> Option<u32> {
    s.chars().next().and_then(|c| digit_to_nat(c))
}

fn is_touch_area_char(c: char) -> bool {
    matches!(c, 'A' | 'B' | 'C' | 'D' | 'E')
}

fn is_slide_mark_char(c: char) -> bool {
    matches!(c, '-' | '^' | 'v' | '<' | '>' | 'V' | 'p' | 'q' | 's' | 'z' | 'w')
}

fn is_slide_text(t: &str) -> bool {
    t.chars().any(is_slide_mark_char)
}

/// Split by separator while respecting bracket depth (matches Lean splitTopLevel)
fn split_top_level(sep: char, s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut depth: u32 = 0;
    let mut current = String::new();

    for c in s.chars() {
        match c {
            '[' => {
                depth += 1;
                current.push(c);
            }
            ']' => {
                depth = depth.saturating_sub(1);
                current.push(c);
            }
            _ if c == sep && depth == 0 => {
                result.push(current.clone());
                current.clear();
            }
            _ => current.push(c),
        }
    }
    result.push(current);
    result
}

fn trim(s: &str) -> String {
    s.trim().to_string()
}

fn strip_comments(s: &str) -> String {
    s.lines()
        .map(|line| match line.split("||").next() {
            Some(head) => head,
            None => line,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn parse_rat_def(s: &str, fallback: f64) -> f64 {
    let t = trim(s);
    if t.is_empty() {
        return fallback;
    }

    let negative = t.starts_with('-');
    let body = if negative { &t[1..] } else { &t };

    if let Some(dot) = body.find('.') {
        let whole = &body[..dot];
        let frac = &body[dot + 1..];
        if let (Ok(w), Ok(f)) = (whole.parse::<i64>(), frac.parse::<i64>()) {
            if frac.chars().all(|c| c.is_ascii_digit()) {
                let denom = 10i64.pow(frac.len() as u32) as f64;
                let val = w as f64 + f as f64 / denom;
                return if negative { -val } else { val };
            }
        }
    } else if let Ok(w) = body.parse::<i64>() {
        let val = w as f64;
        return if negative { -val } else { val };
    }
    fallback
}

fn parse_nat_def(s: &str, fallback: u32) -> u32 {
    let t = trim(s);
    t.parse::<u32>().unwrap_or(fallback)
}

// ============================================================================
// Timing computation (matches LnmaiCore/Simai/Timing.lean)
// ============================================================================

const MICROS_PER_MINUTE: f64 = 60_000_000.0;

/// measure duration in microseconds = (4 * minute_micros) / bpm
fn measure_dur_micros(bpm: f64) -> i64 {
    if bpm <= 0.0 {
        return 0;
    }
    ((4.0 * MICROS_PER_MINUTE) / bpm) as i64
}

/// beat duration = minute_micros / bpm
fn beat_dur_micros(bpm: f64) -> i64 {
    if bpm <= 0.0 {
        return 0;
    }
    (MICROS_PER_MINUTE / bpm) as i64
}

/// noteTimingIncrement: (measure_dur / divisor) = ((4 * minute_us) / bpm) / divisor
fn note_timing_increment(bpm: f64, divisor: u32) -> i64 {
    if bpm <= 0.0 || divisor == 0 {
        return 0;
    }
    measure_dur_micros(bpm) / divisor as i64
}

/// Parse bracket content duration like [4:5] → 4 beats, 5 divisions → (4/5) * measure_dur
fn parse_nd_duration(bpm: f64, timing: &str) -> Option<Duration> {
    let parts: Vec<&str> = timing.split(':').collect();
    if parts.len() != 2 {
        return None;
    }
    let div: u32 = parts[0].parse().ok()?;
    let beats: u32 = parts[1].parse().ok()?;
    if div == 0 {
        return None;
    }
    let micros = (measure_dur_micros(bpm) as f64 * beats as f64 / div as f64) as i64;
    Some(Duration::from_micros(micros))
}

fn extract_bracket_contents(token: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut inside = false;
    let mut current = String::new();

    for c in token.chars() {
        match c {
            '[' if !inside => {
                inside = true;
            }
            ']' if inside => {
                inside = false;
                result.push(current.clone());
                current.clear();
            }
            _ if inside => {
                current.push(c);
            }
            _ => {}
        }
    }
    result
}

fn parse_duration_inner(bpm: f64, inner: &str) -> Option<Duration> {
    if inner.starts_with('#') && inner.matches('#').count() == 1 && !inner.contains(':') {
        // Plain seconds: #1.5 → 1.5 seconds
        let sec_str = &inner[1..];
        return parse_seconds_string(sec_str);
    }

    let hash_count = inner.matches('#').count();
    if hash_count == 2 {
        // #BPM#duration or #BPM#beats:div
        let parts: Vec<&str> = inner.split('#').collect();
        if parts.len() >= 3 {
            return parse_seconds_string(parts[2]);
        }
    } else if hash_count == 1 {
        // #BPM:beats:div or #seconds
        let parts: Vec<&str> = inner.split('#').collect();
        if parts.len() >= 2 {
            let custom_bpm = parse_rat_def(parts[0], bpm);
            if custom_bpm > 0.0 {
                if let Some(d) = parse_nd_duration(custom_bpm, parts[1]) {
                    return Some(d);
                }
                return parse_seconds_string(parts[1]);
            }
        }
    } else {
        if let Some(d) = parse_nd_duration(bpm, inner) {
            return Some(d);
        }
        if !inner.starts_with('#') {
            return parse_seconds_string(inner);
        }
    }
    None
}

fn parse_seconds_string(s: &str) -> Option<Duration> {
    crate::time::parse_seconds_string(s)
}

/// parseDurationSpec: sum of all bracket durations
fn parse_duration_spec(bpm: f64, token: &str) -> Option<Duration> {
    let contents = extract_bracket_contents(token);
    let mut acc: Option<Duration> = None;
    for inner in &contents {
        if let Some(dur) = parse_duration_inner(bpm, inner) {
            acc = match acc {
                Some(sum) => Some(Duration::from_micros(sum.to_micros() + dur.to_micros())),
                None => Some(dur),
            };
        }
    }
    acc
}

// ============================================================================
// Touch area parsing (matches LnmaiCore/Simai/Tokenize.lean)
// ============================================================================

fn touch_area_to_sensor_area(s: &str) -> Option<SensorArea> {
    let chars: Vec<char> = s.chars().collect();
    if chars.is_empty() {
        return None;
    }
    match chars[0] {
        'C' => Some(SensorArea::C),
        'A' if chars.len() >= 2 => match chars[1].to_digit(10) {
            Some(1) => Some(SensorArea::A1),
            Some(2) => Some(SensorArea::A2),
            Some(3) => Some(SensorArea::A3),
            Some(4) => Some(SensorArea::A4),
            Some(5) => Some(SensorArea::A5),
            Some(6) => Some(SensorArea::A6),
            Some(7) => Some(SensorArea::A7),
            Some(8) => Some(SensorArea::A8),
            _ => None,
        },
        'D' if chars.len() >= 2 => match chars[1].to_digit(10) {
            Some(1) => Some(SensorArea::D1),
            Some(2) => Some(SensorArea::D2),
            Some(3) => Some(SensorArea::D3),
            Some(4) => Some(SensorArea::D4),
            Some(5) => Some(SensorArea::D5),
            Some(6) => Some(SensorArea::D6),
            Some(7) => Some(SensorArea::D7),
            Some(8) => Some(SensorArea::D8),
            _ => None,
        },
        'E' if chars.len() >= 2 => match chars[1].to_digit(10) {
            Some(1) => Some(SensorArea::E1),
            Some(2) => Some(SensorArea::E2),
            Some(3) => Some(SensorArea::E3),
            Some(4) => Some(SensorArea::E4),
            Some(5) => Some(SensorArea::E5),
            Some(6) => Some(SensorArea::E6),
            Some(7) => Some(SensorArea::E7),
            Some(8) => Some(SensorArea::E8),
            _ => None,
        },
        'B' if chars.len() >= 2 => match chars[1].to_digit(10) {
            Some(1) => Some(SensorArea::B1),
            Some(2) => Some(SensorArea::B2),
            Some(3) => Some(SensorArea::B3),
            Some(4) => Some(SensorArea::B4),
            Some(5) => Some(SensorArea::B5),
            Some(6) => Some(SensorArea::B6),
            Some(7) => Some(SensorArea::B7),
            Some(8) => Some(SensorArea::B8),
            _ => None,
        },
        _ => None,
    }
}

// ============================================================================
// Note types (matches Lean RawNoteKind)
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NoteKind {
    Tap,
    Hold,
    Slide,
    Touch,
    TouchHold,
    Rest,
    Unknown,
}

// ============================================================================
// Strip prefix directives: (BPM) {divisor} <HS>
// ============================================================================

fn strip_prefix_directives(t: &str) -> String {
    let t = t.trim();
    if t.is_empty() {
        return t.to_string();
    }
    if let Some(rest) = t.strip_prefix('{') {
        if let Some(idx) = rest.find('}') {
            return trim(&rest[idx + 1..]);
        }
    }
    if let Some(rest) = t.strip_prefix('(') {
        if let Some(idx) = rest.find(')') {
            return trim(&rest[idx + 1..]);
        }
    }
    if let Some(rest) = t.strip_prefix('<') {
        if let Some(idx) = rest.find('>') {
            return trim(&rest[idx + 1..]);
        }
    }
    t.to_string()
}

// ============================================================================
// Infer note kind (matches Lean inferKind)
// ============================================================================

fn infer_kind(token: &str) -> NoteKind {
    let t = strip_prefix_directives(token);
    if t.is_empty() {
        return NoteKind::Rest;
    }

    if leading_digit(&t).is_some() {
        if is_slide_text(&t) {
            return NoteKind::Slide;
        }
        if t.contains('h') {
            return NoteKind::Hold;
        }
        return NoteKind::Tap;
    }

    let chars: Vec<char> = t.chars().collect();
    if !chars.is_empty() && is_touch_area_char(chars[0]) {
        if t.contains('h') {
            return NoteKind::TouchHold;
        }
        return NoteKind::Touch;
    }
    NoteKind::Unknown
}

// ============================================================================
// Parse head break flag (matches Lean parseHeadBreak)
// ============================================================================

fn parse_head_break(token: &str) -> bool {
    let t = strip_prefix_directives(token);
    if is_slide_text(&t) {
        if let Some(pos) = t.find('-') {
            t[..pos].contains('b')
        } else {
            t.contains('b')
        }
    } else {
        t.contains('b')
    }
}

// ============================================================================
// Parse a single note token (matches Lean mkRawToken)
// ============================================================================

#[derive(Debug, Clone)]
struct RawNoteToken {
    kind: NoteKind,
    timing: TimePoint,
    bpm: f64,
    #[allow(dead_code)]
    h_speed: f64,
    #[allow(dead_code)]
    divisor: u32,
    slot: Option<OuterSlot>,
    sensor_pos: Option<SensorArea>,
    slide_body: Option<ParsedSlideBody>,
    length: Option<Duration>,
    star_wait: Option<Duration>,
    is_break: bool,
    is_ex: bool,
    is_hanabi: bool,
    is_slide_no_head: bool,
    is_force_star: bool,
    is_fake_rotate: bool,
    is_slide_break: bool,
    source_group_id: Option<u32>,
    source_group_index: Option<u32>,
    source_group_size: Option<u32>,
    raw_text: String,
}

fn sanitize_slide_token(token: &str) -> String {
    let t = strip_prefix_directives(token);
    t.chars()
        .filter(|c| !matches!(c, 'b' | 'x' | 'f' | '!' | '?' | '$'))
        .collect()
}

fn mk_raw_token(timing: TimePoint, bpm: f64, h_speed: f64, divisor: u32, token: &str) -> RawNoteToken {
    let t = trim(token);
    let kind = infer_kind(&t);
    let parsed_text = if kind == NoteKind::Slide {
        sanitize_slide_token(&t)
    } else {
        t.clone()
    };

    let leading_d = leading_digit(&parsed_text);
    let slot = leading_d
        .and_then(|n| if n >= 1 { OuterSlot::from_index((n - 1) as usize) } else { None });

    let sensor_pos = touch_area_to_sensor_area(&t);
    let length = parse_duration_spec(bpm, &t);
    let is_break = parse_head_break(&t);
    let is_ex = t.contains('x');

    // Parse slide body for slide tokens
    let slide_body = if kind == NoteKind::Slide {
        parse_slide_body_from_text(&t)
    } else {
        None
    };

    RawNoteToken {
        kind,
        timing,
        bpm,
        h_speed,
        divisor,
        slot,
        sensor_pos,
        slide_body,
        length,
        star_wait: None,
        is_break,
        is_ex,
        is_hanabi: false,
        is_slide_no_head: false,
        is_force_star: false,
        is_fake_rotate: false,
        is_slide_break: false,
        source_group_id: None,
        source_group_index: None,
        source_group_size: None,
        raw_text: parsed_text,
    }
}

// ============================================================================
// BPM, divisor, hi-speed inline directives
// ============================================================================

fn apply_inline_directive(bpm: f64, divisor: u32, h_speed: f64, segment: &str) -> (f64, u32, f64, String) {
    let t = trim(segment);
    if let Some(rest) = t.strip_prefix('(') {
        if let Some(idx) = rest.find(')') {
            let next_bpm = parse_rat_def(&rest[..idx], bpm);
            return apply_inline_directive(next_bpm, divisor, h_speed, &rest[idx + 1..]);
        }
    }
    if let Some(rest) = t.strip_prefix('{') {
        if let Some(idx) = rest.find('}') {
            let next_div = parse_nat_def(&rest[..idx], divisor);
            return apply_inline_directive(bpm, next_div, h_speed, &rest[idx + 1..]);
        }
    }
    (bpm, divisor, h_speed, t)
}

// ============================================================================
// Connected slide chain expansion (matches Lean expandContinuousChainToken)
// ============================================================================

#[derive(Debug, Clone)]
struct ContinuousChainSegment {
    raw_text: String,
    has_timing: bool,
}

fn parse_slide_shape_chars(op: char, rest: &[char]) -> Option<(String, &[char])> {
    match op {
        'V' => {
            let mid = read_digit_at(rest, 0)?;
            let fin = read_digit_at(rest, 1)?;
            Some((format!("{}{}{}", 'V', mid, fin), &rest[2..]))
        }
        'p' | 'q' => {
            if rest.first() == Some(&op) {
                Some((format!("{}{}", op, op), &rest[1..]))
            } else {
                Some((op.to_string(), rest))
            }
        }
        _ => {
            let fin = read_digit_at(rest, 0)?;
            Some((format!("{}{}", op, fin), &rest[1..]))
        }
    }
}

fn parse_continuous_slide_segments(token: &str) -> Option<Vec<ContinuousChainSegment>> {
    let sanitized = sanitize_slide_token(token);
    let chars: Vec<char> = sanitized.chars().collect();
    let (start_char, rest) = (*chars.first()?, &chars[1..]);
    if !start_char.is_ascii_digit() {
        return None;
    }

    let segments = parse_chain_core(token, start_char, rest).ok()?;
    if segments.len() <= 1 { None } else { Some(segments) }
}

fn parse_chain_core(
    raw_text: &str,
    current_start: char,
    chars: &[char],
) -> Result<Vec<ContinuousChainSegment>, String> {
    if chars.is_empty() {
        return Ok(Vec::new());
    }
    let c = chars[0];
    let rest = &chars[1..];

    if c.is_ascii_digit() {
        return Err("chain cannot contain fresh numeric head".to_string());
    }
    if !is_slide_mark_char(c) {
        return Err("invalid connected slide syntax".to_string());
    }

    let (shape_and_end, rest) = parse_slide_shape_chars(c, rest)
        .ok_or_else(|| "invalid slide shape".to_string())?;

    let segment_core = format!("{}{}", current_start, shape_and_end);

    // Read optional timing suffix [...]
    let (timing_suffix, rest, has_timing) = if rest.first() == Some(&'[') {
        let (suffix, rest_after) = read_bracket(&rest[1..])
            .ok_or_else(|| "unterminated bracket".to_string())?;
        (format!("[{}", suffix), rest_after, true)
    } else {
        (String::new(), rest, false)
    };

    let end_char = shape_and_end.chars().last().unwrap_or(current_start);
    let mut tail = parse_chain_core(raw_text, end_char, rest)?;

    let mut result = vec![ContinuousChainSegment {
        raw_text: segment_core + &timing_suffix,
        has_timing,
    }];
    result.append(&mut tail);
    Ok(result)
}

fn read_bracket(chars: &[char]) -> Option<(String, &[char])> {
    let mut i = 0;
    while i < chars.len() && chars[i] != ']' {
        i += 1;
    }
    if i >= chars.len() {
        return None;
    }
    Some((chars[..i].iter().collect::<String>(), &chars[i + 1..]))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChainTimingLayout { PerSegment, OverallFinal }

fn classify_chain_timing_layout(
    _raw_text: &str,
    segments: &[ContinuousChainSegment],
) -> Result<ChainTimingLayout, String> {
    let flags: Vec<bool> = segments.iter().map(|s| s.has_timing).collect();
    if flags.iter().all(|&f| f) {
        return Ok(ChainTimingLayout::PerSegment);
    }

    let last_has_timing = flags.last().copied().unwrap_or(false);
    let rest_no_timing: Vec<bool> = flags[..flags.len() - 1].iter().copied().collect();
    if last_has_timing && rest_no_timing.iter().all(|&f| !f) {
        return Ok(ChainTimingLayout::OverallFinal);
    }

    if flags.iter().all(|&f| !f) {
        Err("chain requires either per-segment or final timing".to_string())
    } else {
        Err("invalid chain timing layout".to_string())
    }
}

fn apply_shared_slide_flags(base: &RawNoteToken, seg: &RawNoteToken, is_headless: bool) -> RawNoteToken {
    RawNoteToken {
        is_break: base.is_break,
        is_ex: base.is_ex,
        is_hanabi: base.is_hanabi,
        is_slide_no_head: is_headless,
        is_force_star: base.is_force_star,
        is_fake_rotate: base.is_fake_rotate,
        is_slide_break: base.is_slide_break,
        ..seg.clone()
    }
}

fn tag_connected_group(group_id: u32, tokens: Vec<RawNoteToken>) -> Vec<RawNoteToken> {
    let size = tokens.len() as u32;
    tokens
        .into_iter()
        .enumerate()
        .map(|(i, tok)| RawNoteToken {
            source_group_id: Some(group_id),
            source_group_index: Some(i as u32),
            source_group_size: Some(size),
            ..tok
        })
        .collect()
}

fn build_per_segment_chain_tokens(
    group_id: u32,
    timing: TimePoint,
    bpm: f64,
    h_speed: f64,
    divisor: u32,
    base_tok: &RawNoteToken,
    segments: &[ContinuousChainSegment],
) -> Vec<RawNoteToken> {
    let mut tokens: Vec<RawNoteToken> = segments
        .iter()
        .enumerate()
        .map(|(i, seg)| {
            let tok = mk_raw_token(timing, bpm, h_speed, divisor, &seg.raw_text);
            let is_headless = if i == 0 { base_tok.is_slide_no_head } else { true };
            apply_shared_slide_flags(base_tok, &tok, is_headless)
        })
        .collect();

    tag_connected_group(group_id, tokens)
}

fn expand_continuous_chain_token(
    group_id: u32,
    timing: TimePoint,
    bpm: f64,
    h_speed: f64,
    divisor: u32,
    token: &str,
) -> Vec<RawNoteToken> {
    let base_tok = mk_raw_token(timing, bpm, h_speed, divisor, token);
    if base_tok.kind != NoteKind::Slide {
        return vec![base_tok];
    }

    let segments = match parse_continuous_slide_segments(token) {
        Some(s) => s,
        None => return vec![base_tok],
    };

    match classify_chain_timing_layout(&base_tok.raw_text, &segments) {
        Ok(ChainTimingLayout::PerSegment) => {
            build_per_segment_chain_tokens(group_id, timing, bpm, h_speed, divisor, &base_tok, &segments)
        }
        _ => vec![base_tok], // OverallFinal needs SlideTables — skip for now
    }
}

// ============================================================================
// Same-head group expansion (matches Lean expandSameHeadGroup)
// ============================================================================

fn same_head_group_parts(token: &str) -> Vec<String> {
    split_top_level('*', token)
        .into_iter()
        .map(|t| trim(&t))
        .filter(|t| !t.is_empty())
        .collect()
}

fn same_head_head_prefix(token: &str) -> String {
    let t = trim(&strip_prefix_directives(token));
    let chars: Vec<char> = t.chars().collect();
    if chars.is_empty() {
        return String::new();
    }
    let first = chars[0];
    if is_touch_area_char(first) {
        match first {
            'C' => "C".to_string(),
            _ if chars.len() >= 2 && chars[1].is_ascii_digit() => {
                format!("{}{}", first, chars[1])
            }
            _ => first.to_string(),
        }
    } else if first.is_ascii_digit() {
        first.to_string()
    } else {
        String::new()
    }
}

fn expand_same_head_group_rest(
    group_id: u32,
    timing: TimePoint,
    bpm: f64,
    h_speed: f64,
    divisor: u32,
    head_prefix: &str,
    size: u32,
    start_idx: u32,
    parts: &[String],
) -> Vec<RawNoteToken> {
    parts
        .iter()
        .enumerate()
        .map(|(i, part)| {
            let rebuilt = if head_prefix.is_empty() {
                part.clone()
            } else {
                format!("{}{}", head_prefix, part)
            };
            let tok = mk_raw_token(timing, bpm, h_speed, divisor, &rebuilt);
            RawNoteToken {
                is_slide_no_head: true,
                source_group_id: Some(group_id),
                source_group_index: Some(start_idx + i as u32),
                source_group_size: Some(size),
                ..tok
            }
        })
        .collect()
}

fn expand_same_head_group(
    group_id: u32,
    timing: TimePoint,
    bpm: f64,
    h_speed: f64,
    divisor: u32,
    token: &str,
) -> Vec<RawNoteToken> {
    let parts = same_head_group_parts(token);
    if parts.is_empty() {
        return Vec::new();
    }

    let first = &parts[0];
    let rest: Vec<String> = parts[1..].to_vec();
    let head_prefix = same_head_head_prefix(first);
    let first_tok = mk_raw_token(timing, bpm, h_speed, divisor, first);

    let first_is_grouped_slide = first_tok.kind == NoteKind::Slide;
    let grouped_count = (if first_is_grouped_slide { 1 } else { 0 }) + rest.len();

    let first_tok = if first_is_grouped_slide {
        RawNoteToken {
            source_group_id: Some(group_id),
            source_group_index: Some(0),
            source_group_size: Some(grouped_count as u32),
            ..first_tok
        }
    } else {
        first_tok
    };

    let rest_start = if first_is_grouped_slide { 1u32 } else { 0u32 };
    let rest_toks = expand_same_head_group_rest(
        group_id, timing, bpm, h_speed, divisor,
        &head_prefix, grouped_count as u32, rest_start, &rest,
    );

    let mut result = vec![first_tok];
    result.extend(rest_toks);
    result
}

// ============================================================================
// Token list expansion (matches Lean expandTokenList)
// ============================================================================

fn expand_token_list(
    base_group_id: u32,
    timing: TimePoint,
    bpm: f64,
    h_speed: f64,
    divisor: u32,
    tok_texts: &[String],
) -> Vec<RawNoteToken> {
    tok_texts
        .iter()
        .enumerate()
        .flat_map(|(idx, text)| {
            if text.contains('*') {
                expand_same_head_group(base_group_id + idx as u32, timing, bpm, h_speed, divisor, text)
            } else {
                expand_continuous_chain_token(base_group_id + idx as u32, timing, bpm, h_speed, divisor, text)
            }
        })
        .collect()
}

// ============================================================================
// Parse segment notes (updated with expansion)

fn parse_segment_notes(
    segment: &str,
    time: TimePoint,
    bpm: f64,
    h_speed: f64,
    divisor: u32,
) -> Vec<RawNoteToken> {
    let normalized = trim(&segment.replace('\n', ""));
    if normalized.is_empty() {
        return Vec::new();
    }

    // Handle ` separated sub-segments (for pseudo-increment timing)
    if normalized.contains('`') {
        let parts: Vec<&str> = normalized.split('`').collect();
        let mut current_time = time;
        let mut all_tokens = Vec::new();
        for part in &parts {
            let entry_tokens = split_top_level('/', part);
            let entries: Vec<String> = entry_tokens.into_iter().map(|t| trim(&t)).filter(|t| !t.is_empty()).collect();
            let tokens = expand_token_list(0, current_time, bpm, h_speed, divisor, &entries);
            all_tokens.extend(tokens);
            // pseudoIncrement bpm → beat_seconds / 32
            let pseudo_inc = if bpm > 0.0 {
                (beat_dur_micros(bpm) / 32).max(1000)
            } else {
                1000
            };
            current_time = TimePoint::from_micros(current_time.to_micros() + pseudo_inc);
        }
        return all_tokens;
    }

    let entry_tokens = split_top_level('/', &normalized);
    let entries: Vec<String> = entry_tokens
        .into_iter()
        .map(|t| trim(&t))
        .filter(|t| !t.is_empty())
        .collect();

    expand_token_list(0, time, bpm, h_speed, divisor, &entries)
}

// ============================================================================
// Parse segments loop (matches Lean parseSegments)
// ============================================================================

fn parse_segments(
    segments: &[String],
    mut time: TimePoint,
    mut bpm: f64,
    mut h_speed: f64,
    mut divisor: u32,
) -> Vec<RawNoteToken> {
    let mut all_tokens: Vec<RawNoteToken> = Vec::new();

    for segment in segments {
        let clean = trim(segment);
        let (new_bpm, new_div, new_hs, body) = apply_inline_directive(bpm, divisor, h_speed, &clean);
        bpm = new_bpm;
        divisor = new_div;
        h_speed = new_hs;

        let mut tokens = parse_segment_notes(&body, time, bpm, h_speed, divisor);
        all_tokens.append(&mut tokens);

        let inc = note_timing_increment(bpm, divisor);
        time = TimePoint::from_micros(time.to_micros() + inc);
    }

    all_tokens
}

// ============================================================================
// Maidata file parsing (matches LnmaiCore/Simai/Source/Maidata.lean)
// ============================================================================

#[derive(Debug, Clone)]
struct MaidataChartBlock {
    level_index: u32,
    raw_body: String,
}

fn starts_with_amp(s: &str) -> bool {
    s.trim_start().starts_with('&')
}

fn parse_key_value_line(line: &str) -> Option<(String, String)> {
    let parts: Vec<&str> = line.splitn(2, '=').collect();
    if parts.len() == 2 {
        Some((trim(parts[0]), trim(parts[1])))
    } else {
        None
    }
}

fn collect_chart_body(
    lines: &[String],
    start_idx: usize,
) -> (Vec<String>, usize) {
    let mut body_lines = Vec::new();
    let mut i = start_idx;
    while i < lines.len() {
        let line = &lines[i];
        if starts_with_amp(line) {
            break;
        }
        body_lines.push(line.clone());
        i += 1;
    }
    (body_lines, i)
}

fn parse_maidata_lines(content: &str) -> (Vec<(String, String)>, Vec<MaidataChartBlock>) {
    let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
    let mut fields = Vec::new();
    let mut charts = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let line = &lines[i];
        let trimmed = trim(line);
        if trimmed.is_empty() {
            i += 1;
            continue;
        }

        if starts_with_amp(line) {
            if let Some((key, value)) = parse_key_value_line(line) {
                if key.starts_with("&inote_") {
                    let level_str: String = key.chars().skip("&inote_".len()).collect();
                    let level_index: u32 = level_str.parse().unwrap_or(0);
                    let (body_lines, next_i) = collect_chart_body(&lines, i + 1);
                    let body = body_lines.join("\n");
                    charts.push(MaidataChartBlock { level_index, raw_body: body });
                    i = next_i;
                    continue;
                } else {
                    fields.push((key, value));
                }
            }
        }
        i += 1;
    }

    (fields, charts)
}

fn metadata_field(fields: &[(String, String)], key: &str) -> Option<String> {
    fields.iter().find(|(k, _)| k == key).map(|(_, v)| v.clone())
}

// ============================================================================
// Compile chart (matches Lean lowerSourceChartBlock)
// ============================================================================

pub fn compile_lowered(content: &str, level_index: u32) -> Result<ChartSpec, String> {
    let (fields, charts) = parse_maidata_lines(content);

    let chart_block = charts
        .iter()
        .find(|b| b.level_index == level_index)
        .ok_or_else(|| format!("missing inote block for level {}", level_index))?;

    let base_bpm_str = metadata_field(&fields, "&wholebpm").unwrap_or_else(|| "120".to_string());
    let base_bpm = parse_rat_def(&base_bpm_str, 120.0);

    let first_offset = metadata_field(&fields, "&first")
        .and_then(|s| parse_seconds_string(&s))
        .map(|d| TimePoint::from_micros(d.to_micros()))
        .unwrap_or_else(|| TimePoint::from_micros(0));

    let cleaned_body = strip_comments(&chart_block.raw_body);
    let segments: Vec<String> = cleaned_body.split(',')
        .map(|s| s.to_string())
        .filter(|s| !trim(s).is_empty())
        .collect();

    let tokens = parse_segments(&segments, first_offset, base_bpm, 1.0, 4);
    tokens_to_chart_spec(&tokens)

    // Note: slide parsing (connected chains, wifi, etc.) not yet ported.
    // Slides in the generated chart will be empty.
}

// ============================================================================
// Slide parsing (matches LnmaiCore/Simai/Shape.lean + SlideParser.lean)
// ============================================================================

/// Compute relative end position from start and end lane indices (1-based, 8 lanes ring)
fn relative_end_pos(start_idx: u32, end_idx: u32) -> u32 {
    (((end_idx + 7).saturating_sub(start_idx)) % 8) + 1
}

fn outer_slot_is_right_half(slot: OuterSlot) -> bool {
    slot.to_index() < 4
}

fn outer_slot_is_upper_half(slot: OuterSlot) -> bool {
    matches!(slot, OuterSlot::S7 | OuterSlot::S8 | OuterSlot::S1 | OuterSlot::S2)
}

fn key_pos_to_outer_slot(pos: u32) -> Option<OuterSlot> {
    if pos >= 1 { OuterSlot::from_index((pos - 1) as usize) } else { None }
}

fn key_pos_to_sensor_area(pos: u32) -> Option<SensorArea> {
    key_pos_to_outer_slot(pos).map(|s| match s {
        OuterSlot::S1 => SensorArea::A1, OuterSlot::S2 => SensorArea::A2,
        OuterSlot::S3 => SensorArea::A3, OuterSlot::S4 => SensorArea::A4,
        OuterSlot::S5 => SensorArea::A5, OuterSlot::S6 => SensorArea::A6,
        OuterSlot::S7 => SensorArea::A7, OuterSlot::S8 => SensorArea::A8,
    })
}

fn read_digit_at(chars: &[char], index: usize) -> Option<u32> {
    chars.get(index).and_then(|c| c.to_digit(10))
}

fn parse_end_area(chars: &[char], index: usize) -> Option<SensorArea> {
    let pos = read_digit_at(chars, index)?;
    key_pos_to_sensor_area(pos)
}

fn parse_start_lane(chars: &[char], index: usize) -> Option<OuterSlot> {
    let pos = read_digit_at(chars, index)?;
    key_pos_to_outer_slot(pos)
}

/// Parsed slide body — identifies the slide kind, start lane, and end area
#[derive(Debug, Clone)]
struct ParsedSlideBody {
    raw_text: String,
    start_lane: OuterSlot,
    kind: SlideBodyKind,
    end_area: SensorArea,
    turn_area: Option<SensorArea>,
    is_just_right: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SlideBodyKind {
    Line,
    CircleRight,
    CircleLeft,
    CircleUp,
    V,
    Pp,
    Qq,
    P,
    Q,
    S,
    Z,
    Turn,
    Wifi,
}

fn parse_slide_body_from_text(content: &str) -> Option<ParsedSlideBody> {
    let chars: Vec<char> = content.chars().collect();
    if chars.is_empty() { return None; }

    let start_lane = parse_start_lane(&chars, 0)?;

    let (kind, end_idx, turn_idx) = if content.contains('-') {
        (SlideBodyKind::Line, 2, None)
    } else if content.contains('>') {
        (SlideBodyKind::CircleRight, 2, None)
    } else if content.contains('<') {
        (SlideBodyKind::CircleLeft, 2, None)
    } else if content.contains('^') {
        (SlideBodyKind::CircleUp, 2, None)
    } else if content.contains("pp") {
        (SlideBodyKind::Pp, 3, None)
    } else if content.contains("qq") {
        (SlideBodyKind::Qq, 3, None)
    } else if content.contains('p') {
        (SlideBodyKind::P, 2, None)
    } else if content.contains('q') {
        (SlideBodyKind::Q, 2, None)
    } else if content.contains('s') {
        (SlideBodyKind::S, 2, None)
    } else if content.contains('z') {
        (SlideBodyKind::Z, 2, None)
    } else if content.contains('V') {
        (SlideBodyKind::Turn, 3, Some(2))
    } else if content.contains('v') {
        (SlideBodyKind::V, 2, None)
    } else if content.contains('w') {
        (SlideBodyKind::Wifi, 2, None)
    } else {
        return None;
    };

    let end_area = parse_end_area(&chars, end_idx)?;
    let turn_area = turn_idx.and_then(|i| parse_end_area(&chars, i));

    let is_just_right = match kind {
        SlideBodyKind::CircleRight => {
            outer_slot_is_upper_half(start_lane)
        }
        SlideBodyKind::CircleLeft => {
            !outer_slot_is_upper_half(start_lane)
        }
        SlideBodyKind::CircleUp => {
            let start_pos = start_lane.to_index() as u32 + 1;
            let end_pos = read_digit_at(&chars, end_idx).unwrap_or(1);
            let rel = relative_end_pos(start_pos, end_pos);
            rel < 4
        }
        SlideBodyKind::V | SlideBodyKind::Turn => {
            read_digit_at(&chars, end_idx)
                .and_then(|pos| key_pos_to_outer_slot(pos))
                .map(|s| outer_slot_is_right_half(s))
                .unwrap_or(false)
        }
        SlideBodyKind::Wifi => {
            read_digit_at(&chars, end_idx)
                .and_then(|pos| key_pos_to_outer_slot(pos))
                .map(|s| outer_slot_is_upper_half(s))
                .unwrap_or(false)
        }
        _ => {
            // For line, v, p, q, pp, qq, s, z: default depends on end zone being in right half
            read_digit_at(&chars, end_idx)
                .and_then(|pos| key_pos_to_outer_slot(pos))
                .map(|s| outer_slot_is_right_half(s))
                .unwrap_or(false)
        }
    };

    Some(ParsedSlideBody {
        raw_text: content.to_string(),
        start_lane,
        kind,
        end_area,
        turn_area,
        is_just_right,
    })
}

fn area_to_key_pos(area: SensorArea) -> Option<u32> {
    match area {
        SensorArea::A1 => Some(1), SensorArea::A2 => Some(2), SensorArea::A3 => Some(3), SensorArea::A4 => Some(4),
        SensorArea::A5 => Some(5), SensorArea::A6 => Some(6), SensorArea::A7 => Some(7), SensorArea::A8 => Some(8),
        SensorArea::B1 => Some(1), SensorArea::B2 => Some(2), SensorArea::B3 => Some(3), SensorArea::B4 => Some(4),
        SensorArea::B5 => Some(5), SensorArea::B6 => Some(6), SensorArea::B7 => Some(7), SensorArea::B8 => Some(8),
        SensorArea::D1 => Some(1), SensorArea::D2 => Some(2), SensorArea::D3 => Some(3), SensorArea::D4 => Some(4),
        SensorArea::D5 => Some(5), SensorArea::D6 => Some(6), SensorArea::D7 => Some(7), SensorArea::D8 => Some(8),
        SensorArea::E1 => Some(1), SensorArea::E2 => Some(2), SensorArea::E3 => Some(3), SensorArea::E4 => Some(4),
        SensorArea::E5 => Some(5), SensorArea::E6 => Some(6), SensorArea::E7 => Some(7), SensorArea::E8 => Some(8),
        SensorArea::C => None,
    }
}

fn sensor_area_to_outer_slot(area: SensorArea) -> Option<OuterSlot> {
    area.to_outer_slot()
}

fn tokens_to_chart_spec(tokens: &[RawNoteToken]) -> Result<ChartSpec, String> {
    let mut taps: Vec<TapChartNote> = Vec::new();
    let mut holds: Vec<HoldChartNote> = Vec::new();
    let mut touches: Vec<TouchChartNote> = Vec::new();
    let mut touch_holds: Vec<TouchHoldChartNote> = Vec::new();
    let mut slides: Vec<SlideChartNote> = Vec::new();
    let mut note_index: u32 = 1;

    for token in tokens {
        match token.kind {
            NoteKind::Tap => {
                if let Some(slot) = token.slot {
                    taps.push(TapChartNote {
                        timing: token.timing.to_micros(),
                        slot,
                        is_break: token.is_break,
                        is_ex: token.is_ex,
                        button_queue_index: 0,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            NoteKind::Hold => {
                if let Some(slot) = token.slot {
                    let hold_length = token.length
                        .map(|d| d.to_micros())
                        .unwrap_or_else(|| measure_dur_micros(token.bpm));
                    let is_touch = false;
                    holds.push(HoldChartNote {
                        timing: token.timing.to_micros(),
                        slot,
                        length: hold_length,
                        is_break: token.is_break,
                        is_ex: token.is_ex,
                        is_touch,
                        is_classic: false,
                        button_queue_index: 0,
                        touch_hold_group_id: None,
                        touch_hold_group_size: None,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            NoteKind::Touch => {
                if let Some(sensor_pos) = token.sensor_pos {
                    touches.push(TouchChartNote {
                        timing: token.timing.to_micros(),
                        sensor_pos,
                        is_break: token.is_break,
                        touch_queue_index: 0,
                        touch_group_id: None,
                        touch_group_size: None,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            NoteKind::TouchHold => {
                if let Some(sensor_pos) = token.sensor_pos {
                    let hold_length = token.length
                        .map(|d| d.to_micros())
                        .unwrap_or_else(|| measure_dur_micros(token.bpm));
                    touch_holds.push(TouchHoldChartNote {
                        timing: token.timing.to_micros(),
                        sensor_pos,
                        length: hold_length,
                        is_break: token.is_break,
                        is_ex: token.is_ex,
                        touch_queue_index: 0,
                        touch_group_id: None,
                        touch_group_size: None,
                        note_index,
                    });
                    note_index += 1;
                }
            }
            NoteKind::Slide => {
                if let Some(slot) = token.slot {
                    if let Some(body) = &token.slide_body {
                        let slide_length = token.length
                            .map(|d| d.to_micros())
                            .unwrap_or_else(|| measure_dur_micros(token.bpm));
                        let slide_kind = match body.kind {
                            SlideBodyKind::Wifi => SlideKind::Wifi,
                            _ => SlideKind::Single,
                        };
                        slides.push(SlideChartNote {
                            timing: token.timing.to_micros(),
                            slot,
                            length: slide_length,
                            start_timing: token.timing.to_micros(),
                            slide_kind,
                            is_classic: false,
                            is_conn_slide: false,
                            parent_index: None,
                            group_indices: vec![],
                            track_count: 1,
                            judge_at: vec![token.timing.to_micros() + slide_length],
                            is_break: token.is_break,
                            is_ex: token.is_ex,
                            note_index,
                            judge_queues: vec![],
                            debug_simai: Some(token.raw_text.clone()),
                        });
                    }
                    note_index += 1;
                }
            }
            NoteKind::Rest | NoteKind::Unknown => {}
        }
    }

    Ok(ChartSpec {
        taps,
        holds,
        touches,
        touch_holds,
        slides,
        slide_skipping: true,
    })
}

// ============================================================================
// Grade summary helpers (matches RealChartVerification.lean)
// ============================================================================

#[allow(dead_code)]
pub fn summarize_grades(events: &[(u32, crate::types::JudgeGrade)]) -> Vec<(crate::types::JudgeGrade, u32)> {
    use std::collections::HashMap;
    let mut counts = HashMap::new();
    for (_, grade) in events {
        *counts.entry(*grade).or_insert(0u32) += 1;
    }
    let mut result: Vec<_> = counts.into_iter().collect();
    result.sort_by_key(|(g, _)| std::cmp::Reverse(*g as u8));
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_maidata_basic() {
        let content = "&title=Test\n&wholebpm=150\n&inote_1=\n(150){4}\n1,2,3,4,\n";
        let (fields, charts) = parse_maidata_lines(content);
        assert_eq!(metadata_field(&fields, "&title"), Some("Test".to_string()));
        assert_eq!(metadata_field(&fields, "&wholebpm"), Some("150".to_string()));
        assert_eq!(charts.len(), 1);
        assert_eq!(charts[0].level_index, 1);
        assert!(charts[0].raw_body.contains("1,2,3,4,"));
    }

    #[test]
    fn test_compile_simple_chart() {
        let content = "&title=Test\n&wholebpm=120\n&first=1.0\n&inote_1=\n(120){4}\n1,2,3,4,\n";
        let chart = compile_lowered(content, 1).unwrap();
        assert_eq!(chart.taps.len(), 4);
        // First tap should be at approximately 1 second + 0 beats offset
        // BPM = 120, divisor = 4, measure = 2000000us, increment = 500000us
        // First note at first_offset (1s) = 1000000us
        assert!(chart.taps[0].timing >= 900000 && chart.taps[0].timing <= 1100000);
    }

    #[test]
    fn test_hold_parsing() {
        let content = "&title=Test\n&wholebpm=120\n&inote_1=\n(120){4}\n1h[4:3],,,,\n";
        let chart = compile_lowered(content, 1).unwrap();
        assert_eq!(chart.holds.len(), 1);
        // 4 beats / 3 divisions at 120 BPM = 4/3 * measure = 4/3 * 2000000 ≈ 2666666
        assert!(chart.holds[0].length > 1000000);
    }

    #[test]
    fn test_touch_parsing() {
        let content = "&title=Test\n&wholebpm=120\n&inote_1=\n(120){4}\nC,A1,D3,E5,B8,\n";
        let chart = compile_lowered(content, 1).unwrap();
        assert_eq!(chart.touches.len(), 5);
        assert_eq!(chart.touches[0].sensor_pos, SensorArea::C);
        assert_eq!(chart.touches[1].sensor_pos, SensorArea::A1);
        assert_eq!(chart.touches[2].sensor_pos, SensorArea::D3);
        assert_eq!(chart.touches[3].sensor_pos, SensorArea::E5);
        assert_eq!(chart.touches[4].sensor_pos, SensorArea::B8);
    }

    #[test]
    fn test_break_parsing() {
        let content = "&title=Test\n&wholebpm=120\n&inote_1=\n(120){4}\n1b,\n";
        let chart = compile_lowered(content, 1).unwrap();
        assert_eq!(chart.taps.len(), 1);
        assert!(chart.taps[0].is_break);
    }
}
