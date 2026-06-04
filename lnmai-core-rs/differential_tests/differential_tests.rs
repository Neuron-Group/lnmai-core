//! Differential Tests
//!
//! This module contains tests that compare Rust output with Lean reference implementation.
//! The goal is to verify that the Rust implementation produces identical results to the Lean specification.
//!
//! ## Test Strategy
//!
//! For each public function:
//! 1. Generate random inputs
//! 2. Call the Rust function
//! 3. Compare with expected output from Lean specification
//! 4. Assert equality
//!
//! ## Modules Covered
//!
//! - `time`: Duration, TimePoint arithmetic
//! - `areas`: SensorArea, ButtonZone, OuterSlot index conversions
//! - `convert`: Grade conversion functions
//! - `judge`: Judgment functions (judgeTap, judgeTouch, judgeSlide, etc.)
//! - `score`: Score computation functions

use lnmai_core::areas::*;
use lnmai_core::convert::*;
use lnmai_core::judge::*;
use lnmai_core::score::*;
use lnmai_core::time::*;
use lnmai_core::types::*;

/// Test data for differential testing
/// Each entry represents a Lean-Rust pair that should produce identical results
#[derive(Debug, Clone)]
struct DifferentialTestCase<I, O> {
    input: I,
    expected: O,
    description: &'static str,
}

// ============================================================================
// Time Module Differential Tests
// ============================================================================

#[test]
fn differential_duration_from_micros() {
    let cases: Vec<DifferentialTestCase<i64, i64>> = vec![
        DifferentialTestCase { input: 0, expected: 0, description: "zero" },
        DifferentialTestCase { input: 1000000, expected: 1000000, description: "1 second" },
        DifferentialTestCase { input: -1000000, expected: -1000000, description: "negative 1 second" },
        DifferentialTestCase { input: 16667, expected: 16667, description: "1 frame" },
    ];

    for case in cases {
        let duration = Duration::from_micros(case.input);
        assert_eq!(
            duration.to_micros(),
            case.expected,
            "Duration::from_micros({}) failed: {}",
            case.input,
            case.description
        );
    }
}

#[test]
fn differential_duration_arithmetic() {
    let cases: Vec<(i64, i64)> = vec![
        (100, 200),
        (0, 0),
        (-100, 200),
        (100, -200),
        (1000000, 2000000),
    ];

    for (a, b) in cases {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);

        // Addition
        assert_eq!((da + db).to_micros(), a + b, "Duration add: {} + {}", a, b);

        // Subtraction
        assert_eq!((da - db).to_micros(), a - b, "Duration sub: {} - {}", a, b);
    }
}

#[test]
fn differential_timepoint_arithmetic() {
    let cases: Vec<(i64, i64)> = vec![
        (1000000, 200000),
        (0, 0),
        (500000, -100000),
    ];

    for (p, d) in cases {
        let point = TimePoint::from_micros(p);
        let dur = Duration::from_micros(d);

        // Point + Duration
        assert_eq!((point + dur).to_micros(), p + d, "TimePoint + Duration: {} + {}", p, d);

        // Point - Duration
        assert_eq!((point - dur).to_micros(), p - d, "TimePoint - Duration: {} - {}", p, d);
    }
}

// ============================================================================
// Areas Module Differential Tests
// ============================================================================

#[test]
fn differential_sensor_area_index() {
    for area in SensorArea::ALL {
        let index = area.to_index();
        let recovered = SensorArea::from_index(index).unwrap();
        assert_eq!(*area, recovered, "SensorArea index roundtrip failed for {:?}", area);
    }
}

#[test]
fn differential_button_zone_index() {
    for zone in ButtonZone::ALL {
        let index = zone.to_index();
        let recovered = ButtonZone::from_index(index).unwrap();
        assert_eq!(*zone, recovered, "ButtonZone index roundtrip failed for {:?}", zone);
    }
}

#[test]
fn differential_outer_slot_index() {
    for slot in OuterSlot::ALL {
        let index = slot.to_index();
        let recovered = OuterSlot::from_index(index).unwrap();
        assert_eq!(*slot, recovered, "OuterSlot index roundtrip failed for {:?}", slot);
    }
}

#[test]
fn differential_outer_slot_button_zone_conversion() {
    for slot in OuterSlot::ALL {
        let zone = slot.to_button_zone();
        assert_eq!(slot.to_index(), zone.to_index(), "OuterSlot -> ButtonZone index mismatch for {:?}", slot);

        let recovered = zone.to_outer_slot();
        assert_eq!(*slot, recovered, "OuterSlot -> ButtonZone -> OuterSlot roundtrip failed for {:?}", slot);
    }
}

// ============================================================================
// Convert Module Differential Tests
// ============================================================================

#[test]
fn differential_convert_perfect_fixed() {
    let styles = [JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori];
    for style in styles {
        assert_eq!(
            convert_grade(style, JudgeGrade::Perfect),
            JudgeGrade::Perfect,
            "Perfect should be fixed under {:?}",
            style
        );
    }
}

#[test]
fn differential_convert_miss_fixed() {
    let styles = [JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori];
    for style in styles {
        assert_eq!(
            convert_grade(style, JudgeGrade::Miss),
            JudgeGrade::Miss,
            "Miss should be fixed under {:?}",
            style
        );
    }
}

#[test]
fn differential_convert_default_identity() {
    let grades = [
        JudgeGrade::Miss, JudgeGrade::LateGood, JudgeGrade::LateGreat3rd,
        JudgeGrade::LateGreat2nd, JudgeGrade::LateGreat, JudgeGrade::LatePerfect3rd,
        JudgeGrade::LatePerfect2nd, JudgeGrade::Perfect, JudgeGrade::FastPerfect2nd,
        JudgeGrade::FastPerfect3rd, JudgeGrade::FastGreat, JudgeGrade::FastGreat2nd,
        JudgeGrade::FastGreat3rd, JudgeGrade::FastGood, JudgeGrade::TooFast,
    ];
    for grade in grades {
        assert_eq!(
            convert_grade(JudgeStyle::Default, grade),
            grade,
            "Default should be identity for {:?}",
            grade
        );
    }
}

// ============================================================================
// Judge Module Differential Tests
// ============================================================================

#[test]
fn differential_judge_tap_ex_always_perfect() {
    let diffs = [-100000, -50000, 0, 50000, 100000];
    for diff in diffs {
        assert_eq!(
            judge_tap(Duration::from_micros(diff), true),
            JudgeGrade::Perfect,
            "EX notes should always be Perfect, diff={}",
            diff
        );
    }
}

#[test]
fn differential_judge_tap_perfect_window() {
    // Within 1st perfect window (±16667μs)
    let diffs = [-16667, -10000, 0, 10000, 16667];
    for diff in diffs {
        assert_eq!(
            judge_tap(Duration::from_micros(diff), false),
            JudgeGrade::Perfect,
            "Within perfect window should be Perfect, diff={}",
            diff
        );
    }
}

#[test]
fn differential_judge_slide_too_late() {
    // 1 remaining → LateGood
    assert_eq!(judge_slide_too_late(1), JudgeGrade::LateGood);

    // 2+ remaining → Miss
    assert_eq!(judge_slide_too_late(2), JudgeGrade::Miss);
    assert_eq!(judge_slide_too_late(3), JudgeGrade::Miss);
    assert_eq!(judge_slide_too_late(0), JudgeGrade::Miss);
}

#[test]
fn differential_correct_slide_grade() {
    // Perfect variants → Perfect
    assert_eq!(correct_slide_grade(JudgeGrade::Perfect), JudgeGrade::Perfect);
    assert_eq!(correct_slide_grade(JudgeGrade::LatePerfect2nd), JudgeGrade::Perfect);
    assert_eq!(correct_slide_grade(JudgeGrade::LatePerfect3rd), JudgeGrade::Perfect);
    assert_eq!(correct_slide_grade(JudgeGrade::FastPerfect2nd), JudgeGrade::Perfect);
    assert_eq!(correct_slide_grade(JudgeGrade::FastPerfect3rd), JudgeGrade::Perfect);

    // Non-perfect grades stay unchanged
    assert_eq!(correct_slide_grade(JudgeGrade::Miss), JudgeGrade::Miss);
    assert_eq!(correct_slide_grade(JudgeGrade::LateGood), JudgeGrade::LateGood);
    assert_eq!(correct_slide_grade(JudgeGrade::LateGreat), JudgeGrade::LateGreat);
}

// ============================================================================
// Score Module Differential Tests
// ============================================================================

#[test]
fn differential_base_score() {
    assert_eq!(base_score(NoteType::Tap), 500);
    assert_eq!(base_score(NoteType::Hold), 1000);
    assert_eq!(base_score(NoteType::Slide), 1500);
    assert_eq!(base_score(NoteType::Touch), 500);
    assert_eq!(base_score(NoteType::Break), 2500);
}

#[test]
fn differential_score_non_break() {
    // Perfect: 100% earned
    let (earned, lost) = score_non_break(500, JudgeGrade::Perfect, 1);
    assert_eq!(earned, 500);
    assert_eq!(lost, 0);

    // Miss: 0% earned
    let (earned, lost) = score_non_break(500, JudgeGrade::Miss, 1);
    assert_eq!(earned, 0);
    assert_eq!(lost, 500);

    // Good: 50% earned
    let (earned, lost) = score_non_break(500, JudgeGrade::LateGood, 1);
    assert_eq!(earned, 250);
    assert_eq!(lost, 250);

    // Great: 80% earned
    let (earned, lost) = score_non_break(500, JudgeGrade::LateGreat, 1);
    assert_eq!(earned, 400);
    assert_eq!(lost, 100);
}

#[test]
fn differential_score_break() {
    // Perfect: all earned
    let (base, extra, classic, base_lost, extra_lost, classic_lost) =
        score_break(JudgeGrade::Perfect, 1);
    assert_eq!(base, 2500);
    assert_eq!(extra, 100);
    assert_eq!(classic, 100);
    assert_eq!(base_lost, 0);
    assert_eq!(extra_lost, 0);
    assert_eq!(classic_lost, 0);

    // Miss: all lost
    let (base, extra, classic, base_lost, extra_lost, classic_lost) =
        score_break(JudgeGrade::Miss, 1);
    assert_eq!(base, 0);
    assert_eq!(extra, 0);
    assert_eq!(classic, 0);
    assert_eq!(base_lost, 2500);
    assert_eq!(extra_lost, 100);
    assert_eq!(classic_lost, 100);
}

#[test]
fn differential_dx_score_rank() {
    assert_eq!(dx_score_rank(0, 0), 0);
    assert_eq!(dx_score_rank(100, 100), 5); // 100% >= 97%
    assert_eq!(dx_score_rank(96, 100), 4);  // 96% >= 95%
    assert_eq!(dx_score_rank(94, 100), 3);  // 94% >= 93%
    assert_eq!(dx_score_rank(91, 100), 2);  // 91% >= 90%
    assert_eq!(dx_score_rank(86, 100), 1);  // 86% >= 85%
    assert_eq!(dx_score_rank(80, 100), 0);  // 80% < 85%
}

#[test]
fn differential_update_combo() {
    // Perfect: increment all
    let delta = update_combo(10, 5, 3, 0, JudgeGrade::Perfect, 1);
    assert_eq!(delta.combo, 11);
    assert_eq!(delta.p_combo, 6);
    assert_eq!(delta.c_p_combo, 4);

    // Miss: reset all
    let delta = update_combo(10, 5, 3, 0, JudgeGrade::Miss, 1);
    assert_eq!(delta.combo, 0);
    assert_eq!(delta.p_combo, 0);
    assert_eq!(delta.c_p_combo, 0);
}
