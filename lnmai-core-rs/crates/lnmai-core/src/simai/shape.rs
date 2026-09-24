//! Simai slide-shape classification.
//!
//! Mirrors `LnmaiCore/Simai/Shape.lean`.

use crate::areas::{OuterSlot, SensorArea};
use crate::simai::syntax::{
    CanonicalSlideShape, ParseError, ParseErrorKind, ParsedSlideBody, SlideBodyKind, SlideKind,
    SlideShape,
};
use crate::symmetry::{self, SlideSymmetry};

fn error(kind: ParseErrorKind, raw_text: &str, message: &str) -> ParseError {
    ParseError { kind, raw_text: raw_text.to_string(), message: message.to_string(), span: None }
}

/// `SlideShape.kind`.
pub fn shape_kind(shape: SlideShape) -> SlideKind {
    match shape.canonical {
        CanonicalSlideShape::Line(_) => SlideKind::Line,
        CanonicalSlideShape::Circle(_) => SlideKind::Circle,
        CanonicalSlideShape::V(_) => SlideKind::V,
        CanonicalSlideShape::Turn(_) => SlideKind::Turn,
        CanonicalSlideShape::Pq(_) => SlideKind::Pq,
        CanonicalSlideShape::Ppqq(_) => SlideKind::Ppqq,
        CanonicalSlideShape::S => SlideKind::S,
        CanonicalSlideShape::Wifi => SlideKind::Wifi,
    }
}

/// `SlideShape.relEnd`.
pub fn shape_rel_end(shape: SlideShape) -> Option<usize> {
    match shape.canonical {
        CanonicalSlideShape::Line(n)
        | CanonicalSlideShape::Circle(n)
        | CanonicalSlideShape::V(n)
        | CanonicalSlideShape::Turn(n)
        | CanonicalSlideShape::Pq(n)
        | CanonicalSlideShape::Ppqq(n) => Some(n),
        CanonicalSlideShape::S | CanonicalSlideShape::Wifi => None,
    }
}

/// `SlideShape.mirrored`.
pub fn shape_mirrored(shape: SlideShape) -> bool {
    symmetry::is_mirrored(shape.symmetry)
}

/// `mkCanonicalSlideShape?`.
pub fn mk_canonical_slide_shape_opt(
    kind: SlideKind,
    rel_end: Option<usize>,
) -> Option<CanonicalSlideShape> {
    match (kind, rel_end) {
        (SlideKind::Line, Some(n)) => {
            if (2..=8).contains(&n) { Some(CanonicalSlideShape::Line(n)) } else { None }
        }
        (SlideKind::Circle, Some(n)) => Some(CanonicalSlideShape::Circle(n)),
        (SlideKind::V, Some(n)) => if n != 5 { Some(CanonicalSlideShape::V(n)) } else { None },
        (SlideKind::Turn, Some(n)) => {
            if (2..=8).contains(&n) { Some(CanonicalSlideShape::Turn(n)) } else { None }
        }
        (SlideKind::Pq, Some(n)) => Some(CanonicalSlideShape::Pq(n)),
        (SlideKind::Ppqq, Some(n)) => Some(CanonicalSlideShape::Ppqq(n)),
        (SlideKind::S, None) => Some(CanonicalSlideShape::S),
        (SlideKind::Wifi, None) => Some(CanonicalSlideShape::Wifi),
        _ => None,
    }
}

/// `mkCanonicalSlideShape`.
pub fn mk_canonical_slide_shape(
    kind: SlideKind,
    rel_end: Option<usize>,
    raw_text: &str,
    message: &str,
) -> Result<CanonicalSlideShape, ParseError> {
    match mk_canonical_slide_shape_opt(kind, rel_end) {
        Some(shape) => Ok(shape),
        None => Err(error(ParseErrorKind::InvalidEndPosition, raw_text, message)),
    }
}

/// `mkCanonicalSlideShapeUnchecked`.
pub fn mk_canonical_slide_shape_unchecked(
    kind: SlideKind,
    rel_end: Option<usize>,
) -> CanonicalSlideShape {
    match (kind, rel_end) {
        (SlideKind::Line, Some(n)) => CanonicalSlideShape::Line(n),
        (SlideKind::Circle, Some(n)) => CanonicalSlideShape::Circle(n),
        (SlideKind::V, Some(n)) => CanonicalSlideShape::V(n),
        (SlideKind::Turn, Some(n)) => CanonicalSlideShape::Turn(n),
        (SlideKind::Pq, Some(n)) => CanonicalSlideShape::Pq(n),
        (SlideKind::Ppqq, Some(n)) => CanonicalSlideShape::Ppqq(n),
        (SlideKind::S, _) => CanonicalSlideShape::S,
        (SlideKind::Wifi, _) => CanonicalSlideShape::Wifi,
        (SlideKind::Line, None) => CanonicalSlideShape::Line(3),
        (SlideKind::Circle, None) => CanonicalSlideShape::Circle(2),
        (SlideKind::V, None) => CanonicalSlideShape::V(1),
        (SlideKind::Turn, None) => CanonicalSlideShape::Turn(2),
        (SlideKind::Pq, None) => CanonicalSlideShape::Pq(1),
        (SlideKind::Ppqq, None) => CanonicalSlideShape::Ppqq(1),
    }
}

/// `mkSlideShape`.
pub fn mk_slide_shape(canonical: CanonicalSlideShape, sym: SlideSymmetry) -> SlideShape {
    SlideShape { canonical, symmetry: sym }
}

/// `getAt?`.
pub fn get_at(xs: &[char], n: usize) -> Option<char> {
    xs.get(n).copied()
}

/// `digitToNat?`.
pub fn digit_to_nat(c: char) -> Option<usize> {
    match c {
        '1' => Some(1),
        '2' => Some(2),
        '3' => Some(3),
        '4' => Some(4),
        '5' => Some(5),
        '6' => Some(6),
        '7' => Some(7),
        '8' => Some(8),
        _ => None,
    }
}

fn key_pos_to_outer_slot(pos: usize) -> Option<OuterSlot> {
    OuterSlot::of_index(pos.saturating_sub(1))
}

fn key_pos_to_outer_sensor_area(pos: usize) -> Option<SensorArea> {
    OuterSlot::of_index(pos.saturating_sub(1)).map(OuterSlot::to_outer_sensor_area)
}

fn mirror_key(n: usize) -> usize {
    match n {
        1 => 1,
        2 => 8,
        3 => 7,
        4 => 6,
        5 => 5,
        6 => 4,
        7 => 3,
        8 => 2,
        n => n,
    }
}

/// `canonicalRelEnd`.
pub fn canonical_rel_end(sym: SlideSymmetry, rel_end: usize) -> usize {
    if symmetry::is_mirrored(sym) { mirror_key(rel_end) } else { rel_end }
}

/// `baseRelEnd`.
pub fn base_rel_end(sym: SlideSymmetry, rel_end: usize) -> usize {
    canonical_rel_end(sym, rel_end)
}

/// `canonicalShapeKey`.
pub fn canonical_shape_key(shape: SlideShape) -> String {
    let sym = shape.symmetry;
    match shape.canonical {
        CanonicalSlideShape::Line(n) => format!("line{}", n),
        CanonicalSlideShape::Circle(n) => format!("circle{}", base_rel_end(sym, n)),
        CanonicalSlideShape::V(n) => format!("v{}", n),
        CanonicalSlideShape::Turn(n) => format!("L{}", base_rel_end(sym, n)),
        CanonicalSlideShape::Pq(n) => format!("pq{}", base_rel_end(sym, n)),
        CanonicalSlideShape::Ppqq(n) => format!("ppqq{}", base_rel_end(sym, n)),
        CanonicalSlideShape::S => "s".to_string(),
        CanonicalSlideShape::Wifi => "wifi".to_string(),
    }
}

/// `displayShapeKey`.
pub fn display_shape_key(shape: SlideShape) -> String {
    let base = canonical_shape_key(shape);
    if base == "wifi" || base.is_empty() || !symmetry::is_mirrored(shape.symmetry) {
        base
    } else {
        format!("-{}", base)
    }
}

/// `shapeKey`.
pub fn shape_key(shape: SlideShape) -> String {
    display_shape_key(shape)
}

fn relative_end_pos(start_pos: usize, end_pos: usize) -> usize {
    (((end_pos - 1) + 8 - (start_pos - 1)) % 8) + 1
}

fn outer_slot_is_right_half(slot: OuterSlot) -> bool {
    slot.to_index() < 4
}

fn outer_slot_is_upper_half(slot: OuterSlot) -> bool {
    matches!(slot, OuterSlot::S7 | OuterSlot::S8 | OuterSlot::S1 | OuterSlot::S2)
}

fn relative_end_from_typed(
    start_lane: OuterSlot,
    end_area: SensorArea,
) -> Result<usize, ParseError> {
    match end_area.to_outer_slot() {
        Some(end_zone) => Ok(relative_end_pos(
            start_lane.to_index() as usize + 1,
            end_zone.to_index() as usize + 1,
        )),
        None => Err(error(
            ParseErrorKind::InvalidEndPosition,
            "",
            "slide end must be on outer A-ring",
        )),
    }
}

fn read_digit_at(content: &[char], index: usize) -> Result<usize, ParseError> {
    match get_at(content, index) {
        Some(c) => match digit_to_nat(c) {
            Some(n) => Ok(n),
            None => Err(error(
                ParseErrorKind::InvalidSyntax,
                &content.iter().collect::<String>(),
                &format!("expected digit at {}", index),
            )),
        },
        None => Err(error(
            ParseErrorKind::InvalidSyntax,
            &content.iter().collect::<String>(),
            &format!("missing digit at {}", index),
        )),
    }
}

fn read_start_lane_at(content: &[char], index: usize) -> Result<OuterSlot, ParseError> {
    let pos = read_digit_at(content, index)?;
    match key_pos_to_outer_slot(pos) {
        Some(zone) => Ok(zone),
        None => Err(error(
            ParseErrorKind::InvalidSyntax,
            &content.iter().collect::<String>(),
            &format!("invalid start lane at {}", index),
        )),
    }
}

fn read_end_area_at(content: &[char], index: usize) -> Result<SensorArea, ParseError> {
    let pos = read_digit_at(content, index)?;
    match key_pos_to_outer_sensor_area(pos) {
        Some(area) => Ok(area),
        None => Err(error(
            ParseErrorKind::InvalidSyntax,
            &content.iter().collect::<String>(),
            &format!("invalid end area at {}", index),
        )),
    }
}

/// `parseSlideBodyFromText`.
pub fn parse_slide_body_from_text(content: &str) -> Result<ParsedSlideBody, ParseError> {
    let cs: Vec<char> = content.chars().collect();
    let start_lane = read_start_lane_at(&cs, 0)?;
    let end = |index: usize| -> Result<Option<SensorArea>, ParseError> {
        Ok(Some(read_end_area_at(&cs, index)?))
    };
    let body = |kind: SlideBodyKind,
                end_area: Option<SensorArea>,
                turn_area: Option<SensorArea>| ParsedSlideBody {
        raw_text: content.to_string(),
        start_lane,
        kind,
        end_area,
        turn_area,
    };
    if content.contains('-') {
        Ok(body(SlideBodyKind::Line, end(2)?, None))
    } else if content.contains('>') {
        Ok(body(SlideBodyKind::CircleRight, end(2)?, None))
    } else if content.contains('<') {
        Ok(body(SlideBodyKind::CircleLeft, end(2)?, None))
    } else if content.contains('^') {
        Ok(body(SlideBodyKind::CircleUp, end(2)?, None))
    } else if content.contains('v') {
        Ok(body(SlideBodyKind::V, end(2)?, None))
    } else if content.contains("pp") {
        Ok(body(SlideBodyKind::Pp, end(3)?, None))
    } else if content.contains("qq") {
        Ok(body(SlideBodyKind::Qq, end(3)?, None))
    } else if content.contains('p') {
        Ok(body(SlideBodyKind::P, end(2)?, None))
    } else if content.contains('q') {
        Ok(body(SlideBodyKind::Q, end(2)?, None))
    } else if content.contains('s') {
        Ok(body(SlideBodyKind::S, end(2)?, None))
    } else if content.contains('z') {
        Ok(body(SlideBodyKind::Z, end(2)?, None))
    } else if content.contains('V') {
        let turn_area = read_end_area_at(&cs, 2)?;
        let end_area = read_end_area_at(&cs, 3)?;
        Ok(body(SlideBodyKind::Turn, Some(end_area), Some(turn_area)))
    } else if content.contains('w') {
        Ok(body(SlideBodyKind::Wifi, end(2)?, None))
    } else {
        Err(error(ParseErrorKind::InvalidShape, content, "unrecognized Simai slide shape"))
    }
}

/// `solveSlideShape`.
pub fn solve_slide_shape(body: &ParsedSlideBody) -> Result<SlideShape, ParseError> {
    let end_area = body.end_area.unwrap_or(SensorArea::A1);
    let direct = symmetry::direct();
    let mirror = symmetry::mirror();
    let raw = body.raw_text.as_str();
    match body.kind {
        SlideBodyKind::Line => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Line, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, direct))
        }
        SlideBodyKind::CircleRight => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Circle, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(
                canonical,
                if outer_slot_is_upper_half(body.start_lane) { direct } else { mirror },
            ))
        }
        SlideBodyKind::CircleLeft => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Circle, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(
                canonical,
                if !outer_slot_is_upper_half(body.start_lane) { direct } else { mirror },
            ))
        }
        SlideBodyKind::CircleUp => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Circle, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, if rel_end < 5 { direct } else { mirror }))
        }
        SlideBodyKind::V => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::V, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, direct))
        }
        SlideBodyKind::Pp => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Ppqq, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, direct))
        }
        SlideBodyKind::Qq => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Ppqq, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, mirror))
        }
        SlideBodyKind::P => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Pq, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, direct))
        }
        SlideBodyKind::Q => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            let canonical = mk_canonical_slide_shape(SlideKind::Pq, Some(rel_end), raw, "invalid end")?;
            Ok(mk_slide_shape(canonical, mirror))
        }
        SlideBodyKind::S | SlideBodyKind::Z => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            if rel_end != 5 {
                Err(error(ParseErrorKind::InvalidEndPosition, raw, "invalid end"))
            } else {
                let canonical = mk_canonical_slide_shape(SlideKind::S, None, raw, "invalid end")?;
                let sym = if body.kind == SlideBodyKind::Z { mirror } else { direct };
                Ok(mk_slide_shape(canonical, sym))
            }
        }
        SlideBodyKind::Turn => {
            let turn_area = body.turn_area.unwrap_or(SensorArea::A1);
            let turn_rel = relative_end_from_typed(body.start_lane, turn_area)?;
            let end_rel = relative_end_from_typed(body.start_lane, end_area)?;
            if turn_rel == 7 {
                if end_rel < 2 || end_rel > 5 {
                    Err(error(ParseErrorKind::InvalidTurnPosition, raw, "invalid end"))
                } else {
                    let canonical = mk_canonical_slide_shape(SlideKind::Turn, Some(end_rel), raw, "invalid end")?;
                    Ok(mk_slide_shape(canonical, direct))
                }
            } else if turn_rel == 3 {
                if end_rel < 5 {
                    Err(error(ParseErrorKind::InvalidTurnPosition, raw, "invalid end"))
                } else {
                    let canonical = mk_canonical_slide_shape(SlideKind::Turn, Some(end_rel), raw, "invalid end")?;
                    Ok(mk_slide_shape(canonical, mirror))
                }
            } else {
                Err(error(ParseErrorKind::InvalidTurnPosition, raw, "invalid turn"))
            }
        }
        SlideBodyKind::Wifi => {
            let rel_end = relative_end_from_typed(body.start_lane, end_area)?;
            if rel_end != 5 {
                Err(error(ParseErrorKind::InvalidEndPosition, raw, "invalid end"))
            } else {
                let canonical = mk_canonical_slide_shape(SlideKind::Wifi, None, raw, "invalid end")?;
                Ok(mk_slide_shape(canonical, direct))
            }
        }
    }
}

/// `detectShapeFromText`.
pub fn detect_shape_from_text(content: &str) -> Result<SlideShape, ParseError> {
    let body = parse_slide_body_from_text(content)?;
    solve_slide_shape(&body)
}

/// `detectJustType`.
pub fn detect_just_type(content: &str) -> Result<bool, ParseError> {
    let cs: Vec<char> = content.chars().collect();
    let need_outer = || {
        error(ParseErrorKind::InvalidEndPosition, content, "slide end must be on outer A-ring")
    };
    if content.contains('>') {
        let start_lane = read_start_lane_at(&cs, 0)?;
        let _end = read_end_area_at(&cs, 2)?;
        Ok(outer_slot_is_upper_half(start_lane))
    } else if content.contains('<') {
        let start_lane = read_start_lane_at(&cs, 0)?;
        let _end = read_end_area_at(&cs, 2)?;
        Ok(!outer_slot_is_upper_half(start_lane))
    } else if content.contains('^') {
        let start_lane = read_start_lane_at(&cs, 0)?;
        let end_area = read_end_area_at(&cs, 2)?;
        let rel_end = relative_end_from_typed(start_lane, end_area)?;
        Ok(rel_end < 4)
    } else if content.contains('V') {
        let _start = read_start_lane_at(&cs, 0)?;
        let end_area = read_end_area_at(&cs, 3)?;
        let end_zone = end_area.to_outer_slot().ok_or_else(need_outer)?;
        Ok(outer_slot_is_right_half(end_zone))
    } else if content.contains('w') {
        let _start = read_start_lane_at(&cs, 0)?;
        let end_area = read_end_area_at(&cs, 2)?;
        let end_zone = end_area.to_outer_slot().ok_or_else(need_outer)?;
        Ok(outer_slot_is_upper_half(end_zone))
    } else {
        let end_area = if content.contains("qq") || content.contains("pp") {
            read_end_area_at(&cs, 3)?
        } else {
            read_end_area_at(&cs, 2)?
        };
        let end_zone = end_area.to_outer_slot().ok_or_else(need_outer)?;
        Ok(outer_slot_is_right_half(end_zone))
    }
}

/// `parseStartLaneAt`.
pub fn parse_start_lane_at(content: &[char], index: usize) -> Result<OuterSlot, ParseError> {
    read_start_lane_at(content, index)
}

/// `parseEndAreaAt`.
pub fn parse_end_area_at(content: &[char], index: usize) -> Result<SensorArea, ParseError> {
    read_end_area_at(content, index)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn straight_line() {
        let shape = detect_shape_from_text("1-3").unwrap();
        assert_eq!(shape.canonical, CanonicalSlideShape::Line(3));
        assert_eq!(canonical_shape_key(shape), "line3");
    }

    #[test]
    fn invalid_v_at_5() {
        assert!(detect_shape_from_text("1v5").is_err());
        assert!(detect_shape_from_text("1v3").is_ok());
    }

    #[test]
    fn wifi_requires_center() {
        let shape = detect_shape_from_text("1w5").unwrap();
        assert_eq!(shape.canonical, CanonicalSlideShape::Wifi);
        assert!(detect_shape_from_text("1w3").is_err());
    }

    #[test]
    fn just_type_ishape() {
        // "1>3": circle right from S1 (upper half) → isJustRight true.
        assert_eq!(detect_just_type("1>3").unwrap(), true);
    }
}
