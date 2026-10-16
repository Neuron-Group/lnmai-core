//! Simai slide-shape classification.
//!
//! Mirrors `LnmaiCore/Simai/Shape.lean`.

use crate::areas::{OuterSlot, SensorArea};
use crate::natural::Natural;
use crate::simai::syntax::{
    CanonicalSlideShape, ParseError, ParseErrorKind, ParsedSlideBody, SlideBodyKind, SlideKind,
    SlideShape,
};
use crate::symmetry;
use lnmai_core_verify::shape as kernel;

pub use kernel::digit_to_nat;
use kernel::{outer_slot_is_right_half, outer_slot_is_upper_half};

pub fn shape_kind(shape: &SlideShape) -> SlideKind {
    match &shape.canonical {
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

pub fn shape_rel_end(shape: &SlideShape) -> Option<Natural> {
    match &shape.canonical {
        CanonicalSlideShape::Line(n)
        | CanonicalSlideShape::Circle(n)
        | CanonicalSlideShape::V(n)
        | CanonicalSlideShape::Turn(n)
        | CanonicalSlideShape::Pq(n)
        | CanonicalSlideShape::Ppqq(n) => Some(n.clone()),
        CanonicalSlideShape::S | CanonicalSlideShape::Wifi => None,
    }
}

pub fn shape_mirrored(shape: &SlideShape) -> bool {
    symmetry::is_mirrored(shape.symmetry)
}

pub fn mk_canonical_slide_shape_opt(
    kind: SlideKind,
    rel_end: Option<Natural>,
) -> Option<CanonicalSlideShape> {
    match rel_end {
        None => kernel::mk_canonical_slide_shape_opt(kind, None).map(Into::into),
        Some(n) => match n.to_usize() {
            Some(small) => kernel::mk_canonical_slide_shape_opt(kind, Some(small)).map(Into::into),
            // Only these constructors accept endpoints beyond the machine range.
            None => match kind {
                SlideKind::Circle => Some(CanonicalSlideShape::Circle(n)),
                SlideKind::V => Some(CanonicalSlideShape::V(n)),
                SlideKind::Pq => Some(CanonicalSlideShape::Pq(n)),
                SlideKind::Ppqq => Some(CanonicalSlideShape::Ppqq(n)),
                _ => None,
            },
        },
    }
}

pub fn mk_canonical_slide_shape_unchecked(
    kind: SlideKind,
    rel_end: Option<Natural>,
) -> CanonicalSlideShape {
    match rel_end {
        None => kernel::mk_canonical_slide_shape_unchecked(kind, None).into(),
        Some(n) => match n.to_usize() {
            Some(small) => kernel::mk_canonical_slide_shape_unchecked(kind, Some(small)).into(),
            None => match kind {
                SlideKind::Line => CanonicalSlideShape::Line(n),
                SlideKind::Circle => CanonicalSlideShape::Circle(n),
                SlideKind::V => CanonicalSlideShape::V(n),
                SlideKind::Turn => CanonicalSlideShape::Turn(n),
                SlideKind::Pq => CanonicalSlideShape::Pq(n),
                SlideKind::Ppqq => CanonicalSlideShape::Ppqq(n),
                SlideKind::S => CanonicalSlideShape::S,
                SlideKind::Wifi => CanonicalSlideShape::Wifi,
            },
        },
    }
}

pub fn mk_slide_shape(
    canonical: CanonicalSlideShape,
    symmetry: symmetry::SlideSymmetry,
) -> SlideShape {
    SlideShape {
        canonical,
        symmetry,
    }
}

pub fn canonical_rel_end(sym: symmetry::SlideSymmetry, rel_end: impl Into<Natural>) -> Natural {
    let rel_end = rel_end.into();
    match rel_end.to_usize() {
        Some(n) => kernel::canonical_rel_end(sym, n).into(),
        // Lean's mirrorKey is the identity outside 1..=8.
        None => rel_end,
    }
}

pub fn base_rel_end(sym: symmetry::SlideSymmetry, rel_end: impl Into<Natural>) -> Natural {
    canonical_rel_end(sym, rel_end)
}

fn shape_error(raw_text: &str, failure: kernel::ShapeError) -> ParseError {
    match failure {
        kernel::ShapeError::EndNotOuter => error(
            ParseErrorKind::InvalidEndPosition,
            "",
            "slide end must be on outer A-ring",
        ),
        kernel::ShapeError::InvalidEnd => {
            error(ParseErrorKind::InvalidEndPosition, raw_text, "invalid end")
        }
        kernel::ShapeError::InvalidTurnEnd => {
            error(ParseErrorKind::InvalidTurnPosition, raw_text, "invalid end")
        }
        kernel::ShapeError::InvalidTurn => error(
            ParseErrorKind::InvalidTurnPosition,
            raw_text,
            "invalid turn",
        ),
    }
}

fn relative_end_from_typed(
    start_lane: OuterSlot,
    end_area: SensorArea,
) -> Result<usize, ParseError> {
    kernel::relative_end_from_typed(start_lane, end_area).map_err(|err| shape_error("", err))
}

/// solveSlideShape: use the same pure implementation that is extracted for Lean.
pub fn solve_slide_shape(body: &ParsedSlideBody) -> Result<SlideShape, ParseError> {
    kernel::solve_shape(body.kind, body.start_lane, body.end_area, body.turn_area)
        .map(Into::into)
        .map_err(|err| shape_error(&body.raw_text, err))
}

fn error(kind: ParseErrorKind, raw_text: &str, message: &str) -> ParseError {
    ParseError {
        kind,
        raw_text: raw_text.to_string(),
        message: message.to_string(),
        span: None,
    }
}

/// `mkCanonicalSlideShape`.
pub fn mk_canonical_slide_shape(
    kind: SlideKind,
    rel_end: Option<Natural>,
    raw_text: &str,
    message: &str,
) -> Result<CanonicalSlideShape, ParseError> {
    match mk_canonical_slide_shape_opt(kind, rel_end) {
        Some(shape) => Ok(shape),
        None => Err(error(ParseErrorKind::InvalidEndPosition, raw_text, message)),
    }
}

/// `getAt?`.
pub fn get_at(xs: &[char], n: impl Into<Natural>) -> Option<char> {
    xs.get(n.into().to_usize()?).copied()
}

fn key_pos_to_outer_slot(pos: usize) -> Option<OuterSlot> {
    OuterSlot::of_index(pos.saturating_sub(1))
}

fn key_pos_to_outer_sensor_area(pos: usize) -> Option<SensorArea> {
    OuterSlot::of_index(pos.saturating_sub(1)).map(OuterSlot::to_outer_sensor_area)
}

/// `canonicalShapeKey`.
pub fn canonical_shape_key(shape: &SlideShape) -> String {
    let sym = shape.symmetry;
    match &shape.canonical {
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
pub fn display_shape_key(shape: &SlideShape) -> String {
    let base = canonical_shape_key(shape);
    if base == "wifi" || base.is_empty() || !symmetry::is_mirrored(shape.symmetry) {
        base
    } else {
        format!("-{}", base)
    }
}

/// `shapeKey`.
pub fn shape_key(shape: &SlideShape) -> String {
    display_shape_key(shape)
}

fn read_digit_at(content: &[char], index: &Natural) -> Result<usize, ParseError> {
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

fn read_start_lane_at(
    content: &[char],
    index: impl Into<Natural>,
) -> Result<OuterSlot, ParseError> {
    let index = index.into();
    let pos = read_digit_at(content, &index)?;
    match key_pos_to_outer_slot(pos) {
        Some(zone) => Ok(zone),
        None => Err(error(
            ParseErrorKind::InvalidSyntax,
            &content.iter().collect::<String>(),
            &format!("invalid start lane at {}", index),
        )),
    }
}

fn read_end_area_at(content: &[char], index: impl Into<Natural>) -> Result<SensorArea, ParseError> {
    let index = index.into();
    let pos = read_digit_at(content, &index)?;
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
        Err(error(
            ParseErrorKind::InvalidShape,
            content,
            "unrecognized Simai slide shape",
        ))
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
        error(
            ParseErrorKind::InvalidEndPosition,
            content,
            "slide end must be on outer A-ring",
        )
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
pub fn parse_start_lane_at(
    content: &[char],
    index: impl Into<Natural>,
) -> Result<OuterSlot, ParseError> {
    read_start_lane_at(content, index)
}

/// `parseEndAreaAt`.
pub fn parse_end_area_at(
    content: &[char],
    index: impl Into<Natural>,
) -> Result<SensorArea, ParseError> {
    read_end_area_at(content, index)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn straight_line() {
        let shape = detect_shape_from_text("1-3").unwrap();
        assert_eq!(shape.canonical, CanonicalSlideShape::Line(3.into()));
        assert_eq!(canonical_shape_key(&shape), "line3");
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
        assert!(detect_just_type("1>3").unwrap());
    }
}
