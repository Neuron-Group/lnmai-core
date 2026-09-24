//! Authoritative slide topology tables.
//!
//! Mirrors `LnmaiCore/Simai/SlideTables.lean`.

use crate::areas::SensorArea;
use crate::simai::shape::canonical_shape_key;
use crate::simai::syntax::SlideShape;
use crate::symmetry::{self, SlideSymmetry};
use crate::types::AreaPolicy;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SlideAreaSpec {
    pub target_areas: Vec<SensorArea>,
    pub policy: AreaPolicy,
    pub is_last: bool,
    pub is_skippable: bool,
    pub arrow_progress_when_on: usize,
    pub arrow_progress_when_finished: usize,
}

fn mk_area_spec(
    areas: Vec<SensorArea>,
    on: usize,
    finished: usize,
    is_skippable: bool,
    is_last: bool,
) -> SlideAreaSpec {
    SlideAreaSpec {
        target_areas: areas,
        policy: AreaPolicy::Or,
        is_last,
        is_skippable,
        arrow_progress_when_on: on,
        arrow_progress_when_finished: finished,
    }
}

/// Lean `one` (list of areas).
fn one(
    areas: Vec<SensorArea>,
    on: usize,
    finished: usize,
    is_skippable: bool,
    is_last: bool,
) -> SlideAreaSpec {
    mk_area_spec(areas, on, finished, is_skippable, is_last)
}

/// Lean `one'` (single area).
fn one1(area: SensorArea, on: usize, finish: usize, is_skippable: bool, is_last: bool) -> SlideAreaSpec {
    one(vec![area], on, finish, is_skippable, is_last)
}

/// Lean `track`.
fn track(steps: Vec<SlideAreaSpec>) -> Vec<Vec<SlideAreaSpec>> {
    vec![steps]
}

/// `rotateAreaSpec`.
pub fn rotate_area_spec(steps: usize, spec: &SlideAreaSpec) -> SlideAreaSpec {
    SlideAreaSpec {
        target_areas: spec.target_areas.iter().map(|a| a.rotate(steps)).collect(),
        ..spec.clone()
    }
}

/// `transformAreaSpec`.
pub fn transform_area_spec(g: SlideSymmetry, spec: &SlideAreaSpec) -> SlideAreaSpec {
    SlideAreaSpec {
        target_areas: spec.target_areas.iter().map(|a| symmetry::act_on_sensor_area(g, *a)).collect(),
        ..spec.clone()
    }
}

/// `mirrorAreaSpec`.
pub fn mirror_area_spec(spec: &SlideAreaSpec) -> SlideAreaSpec {
    transform_area_spec(symmetry::mirror(), spec)
}

/// `mirrorJudgeQueues`.
pub fn mirror_judge_queues(queues: &[Vec<SlideAreaSpec>]) -> Vec<Vec<SlideAreaSpec>> {
    queues.iter().map(|q| q.iter().map(mirror_area_spec).collect()).collect()
}

/// `transformJudgeQueues`.
pub fn transform_judge_queues(
    g: SlideSymmetry,
    queues: &[Vec<SlideAreaSpec>],
) -> Vec<Vec<SlideAreaSpec>> {
    queues.iter().map(|q| q.iter().map(|s| transform_area_spec(g, s)).collect()).collect()
}

/// `rotateJudgeQueues`.
pub fn rotate_judge_queues(steps: usize, queues: &[Vec<SlideAreaSpec>]) -> Vec<Vec<SlideAreaSpec>> {
    queues.iter().map(|q| q.iter().map(|s| rotate_area_spec(steps, s)).collect()).collect()
}

/// `stripMirrorPrefix`.
pub fn strip_mirror_prefix(s: &str) -> String {
    if s.is_empty() {
        String::new()
    } else if s.starts_with('-') {
        s[1..].to_string()
    } else {
        s.to_string()
    }
}

/// `parseShapeKeySymmetry`.
pub fn parse_shape_key_symmetry(shape_key: &str) -> SlideSymmetry {
    if shape_key.starts_with('-') { symmetry::mirror() } else { symmetry::direct() }
}

fn wifi_left() -> Vec<SlideAreaSpec> {
    vec![
        one1(SensorArea::A1, 0, 0, true, false),
        one1(SensorArea::B8, 2, 2, true, false),
        one1(SensorArea::B7, 4, 4, true, false),
        one(vec![SensorArea::A6, SensorArea::D6], 7, 7, true, true),
    ]
}

fn wifi_center(is_classic: bool) -> Vec<SlideAreaSpec> {
    if is_classic {
        vec![
            one1(SensorArea::A1, 0, 0, true, false),
            one1(SensorArea::B1, 2, 2, true, false),
            one1(SensorArea::C, 7, 7, true, false),
        ]
    } else {
        vec![
            one1(SensorArea::A1, 0, 0, true, false),
            one1(SensorArea::B1, 2, 2, true, false),
            one1(SensorArea::C, 4, 4, true, false),
            one(vec![SensorArea::A5, SensorArea::B5], 7, 7, true, true),
        ]
    }
}

fn wifi_right() -> Vec<SlideAreaSpec> {
    vec![
        one1(SensorArea::A1, 0, 0, true, false),
        one1(SensorArea::B2, 2, 2, true, false),
        one1(SensorArea::B3, 4, 4, true, false),
        one(vec![SensorArea::A4, SensorArea::D5], 7, 7, true, true),
    ]
}

fn ordinary() -> Vec<(&'static str, Vec<Vec<SlideAreaSpec>>)> {
    vec![
        ("circle2", track(vec![one1(SensorArea::A1, 0, 3, false, false), one1(SensorArea::A2, 5, 7, true, true)])),
        ("circle3", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, false, false), one1(SensorArea::A3, 13, 15, true, true)])),
        ("circle4", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, true, false), one1(SensorArea::A3, 14, 19, true, false), one1(SensorArea::A4, 21, 23, true, true)])),
        ("circle5", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, true, false), one1(SensorArea::A3, 14, 19, true, false), one1(SensorArea::A4, 23, 27, true, false), one1(SensorArea::A5, 29, 31, true, true)])),
        ("circle6", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, true, false), one1(SensorArea::A3, 14, 19, true, false), one1(SensorArea::A4, 23, 27, true, false), one1(SensorArea::A5, 31, 35, true, false), one1(SensorArea::A6, 37, 39, true, true)])),
        ("circle7", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, true, false), one1(SensorArea::A3, 14, 19, true, false), one1(SensorArea::A4, 23, 27, true, false), one1(SensorArea::A5, 31, 35, true, false), one1(SensorArea::A6, 39, 43, true, false), one1(SensorArea::A7, 45, 47, true, true)])),
        ("circle8", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, true, false), one1(SensorArea::A3, 14, 19, true, false), one1(SensorArea::A4, 23, 27, true, false), one1(SensorArea::A5, 31, 35, true, false), one1(SensorArea::A6, 39, 43, true, false), one1(SensorArea::A7, 46, 51, true, false), one1(SensorArea::A8, 53, 55, true, true)])),
        ("circle1", track(vec![one1(SensorArea::A1, 0, 3, true, false), one1(SensorArea::A2, 7, 11, true, false), one1(SensorArea::A3, 14, 19, true, false), one1(SensorArea::A4, 23, 27, true, false), one1(SensorArea::A5, 31, 35, true, false), one1(SensorArea::A6, 39, 43, true, false), one1(SensorArea::A7, 46, 51, true, false), one1(SensorArea::A8, 54, 59, true, false), one1(SensorArea::A1, 61, 63, true, true)])),
        ("line2", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::A2, 6, 9, true, true)]]),
        ("line3", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one(vec![SensorArea::A2, SensorArea::B2], 6, 9, false, false)], vec![one1(SensorArea::A3, 10, 13, true, true)]]),
        ("line4", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B2, 6, 9, true, false)], vec![one1(SensorArea::B3, 11, 14, true, false)], vec![one1(SensorArea::A4, 15, 18, true, true)]]),
        ("line5", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 10, 12, true, false)], vec![one1(SensorArea::B5, 13, 16, true, false)], vec![one1(SensorArea::A5, 17, 19, true, true)]]),
        ("line6", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 6, 9, true, false)], vec![one1(SensorArea::B7, 11, 14, true, false)], vec![one1(SensorArea::A6, 15, 18, true, true)]]),
        ("line7", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one(vec![SensorArea::A8, SensorArea::B8], 6, 9, false, false)], vec![one1(SensorArea::A7, 10, 13, true, true)]]),
        ("line8", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::A1, 6, 9, true, true)]]),
        ("v1", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B1, 14, 16, true, false)], vec![one1(SensorArea::A1, 17, 19, true, true)]]),
        ("v2", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B2, 14, 16, true, false)], vec![one1(SensorArea::A2, 17, 19, true, true)]]),
        ("v3", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B3, 14, 16, true, false)], vec![one1(SensorArea::A3, 17, 19, true, true)]]),
        ("v4", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B4, 14, 16, true, false)], vec![one1(SensorArea::A4, 17, 19, true, true)]]),
        ("v6", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B6, 14, 16, true, false)], vec![one1(SensorArea::A6, 17, 19, true, true)]]),
        ("v7", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B7, 14, 16, true, false)], vec![one1(SensorArea::A7, 17, 19, true, true)]]),
        ("v8", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 8, 13, true, false)], vec![one1(SensorArea::B8, 14, 16, true, false)], vec![one1(SensorArea::A8, 17, 19, true, true)]]),
        ("ppqq1", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 10, 13, true, false)], vec![one1(SensorArea::B4, 15, 17, true, false)], vec![one1(SensorArea::A3, 21, 26, true, false)], vec![one1(SensorArea::A2, 29, 32, true, false)], vec![one1(SensorArea::A1, 33, 35, true, true)]]),
        ("ppqq2", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 20, 25, true, false)], vec![one1(SensorArea::A2, 26, 28, true, true)]]),
        ("ppqq3", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 4, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 19, 22, true, true)]]),
        ("ppqq4", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 20, 25, true, false)], vec![one1(SensorArea::A2, 28, 33, true, false)], vec![one1(SensorArea::B1, 34, 37, true, false)], vec![one1(SensorArea::C, 39, 43, true, false)], vec![one1(SensorArea::B4, 44, 46, true, false)], vec![one1(SensorArea::A4, 47, 49, true, true)]]),
        ("ppqq5", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 20, 25, true, false)], vec![one1(SensorArea::A2, 28, 33, true, false)], vec![one1(SensorArea::B1, 34, 37, true, false)], vec![one1(SensorArea::C, 39, 43, true, false)], vec![one1(SensorArea::B5, 44, 46, true, false)], vec![one1(SensorArea::A5, 47, 49, true, true)]]),
        ("ppqq6", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 20, 25, true, false)], vec![one1(SensorArea::A2, 28, 33, true, false)], vec![one1(SensorArea::B1, 34, 37, true, false)], vec![one(vec![SensorArea::C, SensorArea::B8], 38, 40, true, false)], vec![one(vec![SensorArea::B7, SensorArea::B6], 42, 44, true, false)], vec![one1(SensorArea::A6, 46, 48, true, true)]]),
        ("ppqq7", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 20, 25, true, false)], vec![one1(SensorArea::A2, 28, 33, true, false)], vec![one1(SensorArea::B1, 34, 37, true, false)], vec![one1(SensorArea::B8, 38, 42, true, false)], vec![one1(SensorArea::A7, 43, 46, true, true)]]),
        ("ppqq8", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one1(SensorArea::B1, 5, 7, true, false)], vec![one1(SensorArea::C, 9, 13, true, false)], vec![one1(SensorArea::B4, 14, 17, true, false)], vec![one1(SensorArea::A3, 20, 25, true, false)], vec![one1(SensorArea::A2, 28, 33, true, false)], vec![one(vec![SensorArea::B1, SensorArea::A1], 35, 37, true, false)], vec![one1(SensorArea::A8, 38, 41, true, true)]]),
        ("L2", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one(vec![SensorArea::B8, SensorArea::A8], 6, 10, false, false)], vec![one1(SensorArea::A7, 12, 19, true, false)], vec![one1(SensorArea::B8, 21, 24, true, false)], vec![one1(SensorArea::B1, 25, 28, true, false)], vec![one1(SensorArea::A2, 29, 32, true, true)]]),
        ("L3", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one(vec![SensorArea::B8, SensorArea::A8], 6, 10, false, false)], vec![one1(SensorArea::A7, 12, 18, true, false)], vec![one1(SensorArea::B7, 20, 22, true, false)], vec![one1(SensorArea::C, 25, 27, true, false)], vec![one1(SensorArea::B3, 28, 31, true, false)], vec![one1(SensorArea::A3, 32, 34, true, true)]]),
        ("L4", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one(vec![SensorArea::B8, SensorArea::A8], 6, 10, false, false)], vec![one1(SensorArea::A7, 12, 19, true, false)], vec![one1(SensorArea::B6, 21, 24, true, false)], vec![one1(SensorArea::B5, 25, 28, true, false)], vec![one1(SensorArea::A4, 29, 32, true, true)]]),
        ("L5", vec![vec![one1(SensorArea::A1, 0, 3, true, false)], vec![one(vec![SensorArea::B8, SensorArea::A8], 6, 10, false, false)], vec![one1(SensorArea::A7, 12, 18, true, false)], vec![one(vec![SensorArea::B6, SensorArea::A6], 21, 24, false, false)], vec![one1(SensorArea::A5, 27, 28, true, true)]]),
        ("s", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 7, 9, true, false)], vec![one1(SensorArea::B7, 10, 12, true, false)], vec![one1(SensorArea::C, 14, 17, true, false)], vec![one1(SensorArea::B3, 19, 21, true, false)], vec![one1(SensorArea::B4, 22, 25, true, false)], vec![one1(SensorArea::A5, 27, 30, true, true)]]),
        ("pq1", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 11, true, false)], vec![one1(SensorArea::B6, 12, 14, true, false)], vec![one1(SensorArea::B5, 15, 17, true, false)], vec![one1(SensorArea::B4, 19, 21, true, false)], vec![one1(SensorArea::B3, 22, 24, true, false)], vec![one1(SensorArea::B2, 25, 29, true, false)], vec![one1(SensorArea::A1, 30, 33, true, true)]]),
        ("pq2", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 11, true, false)], vec![one1(SensorArea::B6, 12, 14, true, false)], vec![one1(SensorArea::B5, 16, 18, true, false)], vec![one1(SensorArea::B4, 19, 21, true, false)], vec![one1(SensorArea::B3, 22, 26, true, false)], vec![one1(SensorArea::A2, 27, 30, true, true)]]),
        ("pq3", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 11, true, false)], vec![one1(SensorArea::B6, 12, 14, true, false)], vec![one1(SensorArea::B5, 16, 18, true, false)], vec![one1(SensorArea::B4, 20, 23, true, false)], vec![one1(SensorArea::A3, 25, 27, true, true)]]),
        ("pq4", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 11, true, false)], vec![one1(SensorArea::B6, 12, 14, true, false)], vec![one1(SensorArea::B5, 16, 20, true, false)], vec![one1(SensorArea::A4, 22, 24, true, true)]]),
        ("pq5", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 12, true, false)], vec![one1(SensorArea::B6, 14, 17, true, false)], vec![one1(SensorArea::A5, 19, 21, true, true)]]),
        ("pq6", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 11, true, false)], vec![one1(SensorArea::B6, 13, 15, true, false)], vec![one1(SensorArea::B5, 16, 18, true, false)], vec![one1(SensorArea::B4, 19, 21, true, false)], vec![one1(SensorArea::B3, 22, 24, true, false)], vec![one1(SensorArea::B2, 25, 27, true, false)], vec![one1(SensorArea::B1, 28, 30, true, false)], vec![one1(SensorArea::B8, 31, 33, true, false)], vec![one1(SensorArea::B7, 35, 38, true, false)], vec![one1(SensorArea::A6, 40, 42, true, true)]]),
        ("pq7", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 7, 9, true, false)], vec![one1(SensorArea::B7, 10, 12, true, false)], vec![one1(SensorArea::B6, 13, 15, true, false)], vec![one1(SensorArea::B5, 16, 18, true, false)], vec![one1(SensorArea::B4, 20, 22, true, false)], vec![one1(SensorArea::B3, 23, 25, true, false)], vec![one1(SensorArea::B2, 26, 28, true, false)], vec![one1(SensorArea::B1, 30, 32, true, false)], vec![one1(SensorArea::B8, 33, 36, true, false)], vec![one1(SensorArea::A7, 37, 40, true, true)]]),
        ("pq8", vec![vec![one1(SensorArea::A1, 0, 4, true, false)], vec![one1(SensorArea::B8, 5, 8, true, false)], vec![one1(SensorArea::B7, 9, 11, true, false)], vec![one1(SensorArea::B6, 12, 14, true, false)], vec![one1(SensorArea::B5, 15, 17, true, false)], vec![one1(SensorArea::B4, 19, 21, true, false)], vec![one1(SensorArea::B3, 22, 24, true, false)], vec![one1(SensorArea::B2, 25, 27, true, false)], vec![one1(SensorArea::B1, 28, 32, true, false)], vec![one1(SensorArea::A8, 33, 36, true, true)]]),
    ]
}

/// `judgeQueuesForShapeKey`.
pub fn judge_queues_for_shape_key(
    shape_key: &str,
    is_classic: bool,
) -> Option<Vec<Vec<SlideAreaSpec>>> {
    let key = strip_mirror_prefix(shape_key);
    let sym = parse_shape_key_symmetry(shape_key);
    let base: Option<Vec<Vec<SlideAreaSpec>>> =
        if let Some((_, queues)) = ordinary().into_iter().find(|(k, _)| *k == key) {
            Some(vec![queues.into_iter().flatten().collect()])
        } else if key == "wifi" {
            Some(vec![wifi_left(), wifi_center(is_classic), wifi_right()])
        } else {
            None
        };
    base.map(|b| transform_judge_queues(sym, &b))
}

/// `judgeQueuesForShape`.
pub fn judge_queues_for_shape(
    shape: SlideShape,
    is_classic: bool,
) -> Option<Vec<Vec<SlideAreaSpec>>> {
    let base = judge_queues_for_shape_key(&canonical_shape_key(shape), is_classic)?;
    Some(transform_judge_queues(shape.symmetry, &base))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::simai::shape::detect_shape_from_text;

    #[test]
    fn line3_table_shape() {
        let queues = judge_queues_for_shape_key("line3", false).unwrap();
        // ordinary shapes are flattened into a single queue.
        assert_eq!(queues.len(), 1);
        assert_eq!(queues[0].len(), 3);
        assert_eq!(queues[0][1].target_areas, vec![SensorArea::A2, SensorArea::B2]);
    }

    #[test]
    fn wifi_has_three_queues() {
        let queues = judge_queues_for_shape_key("wifi", false).unwrap();
        assert_eq!(queues.len(), 3);
        assert_eq!(queues[1].len(), 4);
        let classic = judge_queues_for_shape_key("wifi", true).unwrap();
        assert_eq!(classic[1].len(), 3);
    }

    #[test]
    fn unknown_key_none() {
        assert_eq!(judge_queues_for_shape_key("nope", false), None);
    }

    #[test]
    fn shape_lookup() {
        let shape = detect_shape_from_text("1-3").unwrap();
        let queues = judge_queues_for_shape(shape, false).unwrap();
        assert_eq!(queues[0].len(), 3);
    }

    #[test]
    fn mirror_prefix() {
        assert_eq!(strip_mirror_prefix("-line3"), "line3");
        assert_eq!(strip_mirror_prefix("line3"), "line3");
        assert!(matches!(parse_shape_key_symmetry("-line3"), SlideSymmetry::Sr(_)));
    }
}
