//! Real chart verification tests
//!
//! These tests mirror the Lean tests from Apps/RealChartVerification.lean
//! and LnmaiCore/RuntimeTests.lean.

use lnmai_core::areas::*;
use lnmai_core::constants::*;
use lnmai_core::convert::*;
use lnmai_core::input_model::*;
use lnmai_core::judge::*;
use lnmai_core::lifecycle::*;
use lnmai_core::scheduler::*;
use lnmai_core::score::*;
use lnmai_core::time::*;
use lnmai_core::types::*;

// ============================================================================
// Helper Functions
// ============================================================================

fn tp(micros: i64) -> TimePoint {
    TimePoint::from_micros(micros)
}

fn dur(micros: i64) -> Duration {
    Duration::from_micros(micros)
}

fn secs(seconds: i64) -> TimePoint {
    tp(seconds * 1000000)
}

fn button_flag_vec(pressed: &[ButtonZone]) -> ButtonVec<bool> {
    let mut vec = ButtonVec::replicate(false);
    for zone in pressed {
        vec = vec.set(*zone, true);
    }
    vec
}

fn sensor_flag_vec(pressed: &[SensorArea]) -> SensorVec<bool> {
    let mut vec = SensorVec::replicate(false);
    for area in pressed {
        vec = vec.set(*area, true);
    }
    vec
}

fn button_count_vec(clicks: &[ButtonZone]) -> ButtonVec<u32> {
    let mut vec = ButtonVec::replicate(0);
    for zone in clicks {
        let count = vec.get_d(*zone, 0);
        vec = vec.set(*zone, count + 1);
    }
    vec
}

fn sensor_count_vec(clicks: &[SensorArea]) -> SensorVec<u32> {
    let mut vec = SensorVec::replicate(0);
    for area in clicks {
        let count = vec.get_d(*area, 0);
        vec = vec.set(*area, count + 1);
    }
    vec
}

fn mk_button_frame_input(
    button_clicks: &[ButtonZone],
    button_held: &[ButtonZone],
    sensor_clicks: &[SensorArea],
    sensor_held: &[SensorArea],
    delta: Duration,
) -> FrameInput {
    FrameInput {
        button_clicked: button_flag_vec(button_clicks),
        button_held: button_flag_vec(button_held),
        sensor_clicked: sensor_flag_vec(sensor_clicks),
        sensor_held: sensor_flag_vec(sensor_held),
        button_click_count: button_count_vec(button_clicks),
        sensor_click_count: sensor_count_vec(sensor_clicks),
        delta: delta.to_micros(),
    }
}

// ============================================================================
// Test Cases from RuntimeTests.lean
// ============================================================================

#[test]
fn test_button_tap_can_use_matching_a_sensor() {
    // Setup: A tap note at K1, judgeable
    let tap = TapNote {
        params: CommonNoteParams {
            judge_timing: secs(1).to_micros(),
            judge_offset: 0,
            is_break: false,
            is_ex: false,
            note_index: 1,
        },
        lane: OuterSlot::S1,
        state: TapState::Judgeable,
        button_queue_index: 0,
    };

    let mut tap_queues = ButtonVec::replicate(ZoneQueue::default());
    tap_queues = tap_queues.set(
        ButtonZone::K1,
        ZoneQueue {
            notes: vec![TapFamilyNote::TapNote(tap)],
            current_index: 0,
        },
    );

    let mut state = GameState {
        current_time: tp(984000).to_micros(),
        tap_queues,
        ..Default::default()
    };

    // Input: A1 sensor clicked
    let input = mk_button_frame_input(&[], &[], &[SensorArea::A1], &[], dur(16000));

    let result = step_frame(&mut state, &input);

    // Expect: One tap event at K1
    assert_eq!(result.events.len(), 1, "expected one tap event");
    assert_eq!(result.events[0].kind, JudgeEventKind::Tap);
    assert_eq!(result.events[0].position, RuntimePos::ButtonZonePos(ButtonZone::K1));
}

#[test]
fn test_classic_hold_matching_a_sensor_keeps_body_pressed() {
    // Setup: A classic hold at K1, body held
    let hold = HoldNote {
        params: CommonNoteParams {
            judge_timing: secs(1).to_micros(),
            judge_offset: 0,
            is_break: false,
            is_ex: false,
            note_index: 2,
        },
        start: HoldStart::HoldButton(ButtonZone::K1),
        state: HoldSubState::BodyHeld,
        length: dur(200000).to_micros(),
        head_diff: Some(0),
        head_grade: Some(JudgeGrade::Perfect),
        player_release_time: None,
        is_classic: true,
        is_touch_hold: false,
        touch_group_id: None,
        touch_group_size: None,
        touch_group_count: None,
    };

    let mut state = GameState {
        current_time: tp(1050000).to_micros(),
        active_holds: vec![(ButtonZone::K1, hold)],
        prev_sensor: sensor_flag_vec(&[SensorArea::A1]),
        ..Default::default()
    };

    // Input: A1 sensor held
    let input = mk_button_frame_input(&[], &[], &[], &[SensorArea::A1], dur(16000));

    let result = step_frame(&mut state, &input);

    // Expect: Hold remains active, no events
    assert_eq!(result.events.len(), 0, "expected no events");
    assert_eq!(result.state.active_holds.len(), 1, "expected hold to remain active");
}

#[test]
fn test_modern_hold_head_miss_can_end_as_late_good() {
    // Setup: A modern hold at K1, head missed, body held
    let hold = HoldNote {
        params: CommonNoteParams {
            judge_timing: secs(1).to_micros(),
            judge_offset: 0,
            is_break: false,
            is_ex: false,
            note_index: 3,
        },
        start: HoldStart::HoldButton(ButtonZone::K1),
        state: HoldSubState::BodyHeld,
        length: dur(800000).to_micros(),
        head_diff: Some(dur(150000).to_micros()),
        head_grade: Some(JudgeGrade::Miss),
        player_release_time: Some(0),
        is_classic: false,
        is_touch_hold: false,
        touch_group_id: None,
        touch_group_size: None,
        touch_group_count: None,
    };

    let mut state = GameState {
        current_time: tp(1700000).to_micros(),
        active_holds: vec![(ButtonZone::K1, hold)],
        ..Default::default()
    };

    // Input: K1 button held
    let input = mk_button_frame_input(&[], &[ButtonZone::K1], &[], &[], dur(16000));

    let result = step_frame(&mut state, &input);

    // Expect: Hold ends with LateGood grade
    assert_eq!(result.state.active_holds.len(), 0, "expected hold to end");
    assert_eq!(result.events.len(), 1, "expected one hold event");
    assert_eq!(result.events[0].kind, JudgeEventKind::Hold);
    assert_eq!(result.events[0].grade, JudgeGrade::LateGood);
}

// ============================================================================
// Judge Function Tests
// ============================================================================

#[test]
fn test_judge_tap_perfect_window() {
    // Within 1st perfect window (±16667μs)
    assert_eq!(judge_tap(dur(0), false), JudgeGrade::Perfect);
    assert_eq!(judge_tap(dur(10000), false), JudgeGrade::Perfect);
    assert_eq!(judge_tap(dur(-10000), false), JudgeGrade::Perfect);
    assert_eq!(judge_tap(dur(16667), false), JudgeGrade::Perfect);
    assert_eq!(judge_tap(dur(-16667), false), JudgeGrade::Perfect);
}

#[test]
fn test_judge_tap_ex_always_perfect() {
    // EX notes always Perfect
    assert_eq!(judge_tap(dur(0), true), JudgeGrade::Perfect);
    assert_eq!(judge_tap(dur(100000), true), JudgeGrade::Perfect);
    assert_eq!(judge_tap(dur(-100000), true), JudgeGrade::Perfect);
}

#[test]
fn test_judge_tap_great_window() {
    // Within great window (16667-50001μs)
    assert_eq!(judge_tap(dur(20000), false), JudgeGrade::LatePerfect2nd);
    assert_eq!(judge_tap(dur(-20000), false), JudgeGrade::FastPerfect2nd);
    assert_eq!(judge_tap(dur(40000), false), JudgeGrade::LatePerfect3rd);
    assert_eq!(judge_tap(dur(-40000), false), JudgeGrade::FastPerfect3rd);
}

#[test]
fn test_judge_slide_too_late() {
    assert_eq!(judge_slide_too_late(1), JudgeGrade::LateGood);
    assert_eq!(judge_slide_too_late(2), JudgeGrade::Miss);
    assert_eq!(judge_slide_too_late(0), JudgeGrade::Miss);
}

#[test]
fn test_correct_slide_grade() {
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
// Convert Function Tests
// ============================================================================

#[test]
fn test_convert_perfect_fixed() {
    let styles = [JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori];
    for style in styles {
        assert_eq!(convert_grade(style, JudgeGrade::Perfect), JudgeGrade::Perfect);
    }
}

#[test]
fn test_convert_miss_fixed() {
    let styles = [JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori];
    for style in styles {
        assert_eq!(convert_grade(style, JudgeGrade::Miss), JudgeGrade::Miss);
    }
}

#[test]
fn test_convert_default_identity() {
    let grades = [
        JudgeGrade::Miss, JudgeGrade::LateGood, JudgeGrade::LateGreat3rd,
        JudgeGrade::LateGreat2nd, JudgeGrade::LateGreat, JudgeGrade::LatePerfect3rd,
        JudgeGrade::LatePerfect2nd, JudgeGrade::Perfect, JudgeGrade::FastPerfect2nd,
        JudgeGrade::FastPerfect3rd, JudgeGrade::FastGreat, JudgeGrade::FastGreat2nd,
        JudgeGrade::FastGreat3rd, JudgeGrade::FastGood, JudgeGrade::TooFast,
    ];
    for grade in grades {
        assert_eq!(convert_grade(JudgeStyle::Default, grade), grade);
    }
}

// ============================================================================
// Score Function Tests
// ============================================================================

#[test]
fn test_base_score() {
    assert_eq!(base_score(NoteType::Tap), 500);
    assert_eq!(base_score(NoteType::Hold), 1000);
    assert_eq!(base_score(NoteType::Slide), 1500);
    assert_eq!(base_score(NoteType::Touch), 500);
    assert_eq!(base_score(NoteType::Break), 2500);
}

#[test]
fn test_score_non_break_perfect() {
    let (earned, lost) = score_non_break(500, JudgeGrade::Perfect, 1);
    assert_eq!(earned, 500);
    assert_eq!(lost, 0);
}

#[test]
fn test_score_non_break_miss() {
    let (earned, lost) = score_non_break(500, JudgeGrade::Miss, 1);
    assert_eq!(earned, 0);
    assert_eq!(lost, 500);
}

#[test]
fn test_score_non_break_conservation() {
    let grades = [
        JudgeGrade::Perfect, JudgeGrade::LateGreat, JudgeGrade::LateGood,
        JudgeGrade::Miss, JudgeGrade::TooFast,
    ];
    for grade in grades {
        let (earned, lost) = score_non_break(500, grade, 1);
        assert_eq!(earned + lost, 500, "conservation failed for {:?}", grade);
    }
}

#[test]
fn test_score_break_perfect() {
    let (base, extra, classic, base_lost, extra_lost, classic_lost) =
        score_break(JudgeGrade::Perfect, 1);
    assert_eq!(base, 2500);
    assert_eq!(extra, 100);
    assert_eq!(classic, 100);
    assert_eq!(base_lost, 0);
    assert_eq!(extra_lost, 0);
    assert_eq!(classic_lost, 0);
}

#[test]
fn test_dx_score_rank() {
    assert_eq!(dx_score_rank(0, 0), 0);
    assert_eq!(dx_score_rank(100, 100), 5); // 100% >= 97%
    assert_eq!(dx_score_rank(96, 100), 4);  // 96% >= 95%
    assert_eq!(dx_score_rank(94, 100), 3);  // 94% >= 93%
    assert_eq!(dx_score_rank(91, 100), 2);  // 91% >= 90%
    assert_eq!(dx_score_rank(86, 100), 1);  // 86% >= 85%
    assert_eq!(dx_score_rank(80, 100), 0);  // 80% < 85%
}

#[test]
fn test_update_combo_perfect() {
    let delta = update_combo(10, 5, 3, 0, JudgeGrade::Perfect, 1);
    assert_eq!(delta.combo, 11);
    assert_eq!(delta.p_combo, 6);
    assert_eq!(delta.c_p_combo, 4);
}

#[test]
fn test_update_combo_miss() {
    let delta = update_combo(10, 5, 3, 0, JudgeGrade::Miss, 1);
    assert_eq!(delta.combo, 0);
    assert_eq!(delta.p_combo, 0);
    assert_eq!(delta.c_p_combo, 0);
}

// ============================================================================
// Area Index Roundtrip Tests
// ============================================================================

#[test]
fn test_sensor_area_index_roundtrip() {
    for area in SensorArea::ALL {
        let index = area.to_index();
        let recovered = SensorArea::from_index(index).unwrap();
        assert_eq!(*area, recovered);
    }
}

#[test]
fn test_button_zone_index_roundtrip() {
    for zone in ButtonZone::ALL {
        let index = zone.to_index();
        let recovered = ButtonZone::from_index(index).unwrap();
        assert_eq!(*zone, recovered);
    }
}

#[test]
fn test_outer_slot_index_roundtrip() {
    for slot in OuterSlot::ALL {
        let index = slot.to_index();
        let recovered = OuterSlot::from_index(index).unwrap();
        assert_eq!(*slot, recovered);
    }
}

#[test]
fn test_outer_slot_button_zone_conversion() {
    for slot in OuterSlot::ALL {
        let zone = slot.to_button_zone();
        assert_eq!(slot.to_index(), zone.to_index());
        let recovered = zone.to_outer_slot();
        assert_eq!(*slot, recovered);
    }
}

// ============================================================================
// Time Module Tests
// ============================================================================

#[test]
fn test_duration_injective() {
    let a = Duration::from_micros(100);
    let b = Duration::from_micros(100);
    assert_eq!(a, b);

    let c = Duration::from_micros(200);
    assert_ne!(a, c);
}

#[test]
fn test_duration_order_preserving() {
    let a = Duration::from_micros(100);
    let b = Duration::from_micros(200);
    assert!(a < b);
    assert!(a.to_micros() < b.to_micros());
}

#[test]
fn test_timepoint_injective() {
    let a = TimePoint::from_micros(100);
    let b = TimePoint::from_micros(100);
    assert_eq!(a, b);

    let c = TimePoint::from_micros(200);
    assert_ne!(a, c);
}

#[test]
fn test_timepoint_order_preserving() {
    let a = TimePoint::from_micros(100);
    let b = TimePoint::from_micros(200);
    assert!(a < b);
    assert!(a.to_micros() < b.to_micros());
}

#[test]
fn test_duration_arithmetic() {
    let a = Duration::from_micros(100);
    let b = Duration::from_micros(200);
    assert_eq!((a + b).to_micros(), 300);
    assert_eq!((b - a).to_micros(), 100);
    assert_eq!((-a).to_micros(), -100);
}

#[test]
fn test_timepoint_arithmetic() {
    let p = TimePoint::from_micros(1000);
    let d = Duration::from_micros(200);
    assert_eq!((p + d).to_micros(), 1200);
    assert_eq!((p - d).to_micros(), 800);
}

#[test]
fn test_quantize_seconds_string() {
    assert_eq!(quantize_seconds_string("1"), Some(1_000_000));
    assert_eq!(quantize_seconds_string("1.5"), Some(1_500_000));
    assert_eq!(quantize_seconds_string("-1.5"), Some(-1_500_000));
    assert_eq!(quantize_seconds_string("0.001"), Some(1000));
    assert_eq!(quantize_seconds_string(""), None);
    assert_eq!(quantize_seconds_string("abc"), None);
}

// ============================================================================
// Lifecycle Tests
// ============================================================================

#[test]
fn test_tap_note_position() {
    let note = TapNote {
        params: CommonNoteParams {
            judge_timing: 1000000,
            judge_offset: 0,
            is_break: false,
            is_ex: false,
            note_index: 0,
        },
        lane: OuterSlot::S1,
        state: TapState::Waiting,
        button_queue_index: 0,
    };
    assert_eq!(note.position(), RuntimePos::ButtonZonePos(ButtonZone::K1));
}

#[test]
fn test_hold_note_position() {
    let note = HoldNote {
        params: CommonNoteParams {
            judge_timing: 1000000,
            judge_offset: 0,
            is_break: false,
            is_ex: false,
            note_index: 0,
        },
        start: HoldStart::HoldButton(ButtonZone::K1),
        state: HoldSubState::HeadWaiting,
        length: 1000000,
        head_diff: None,
        head_grade: None,
        player_release_time: None,
        is_classic: false,
        is_touch_hold: false,
        touch_group_id: None,
        touch_group_size: None,
        touch_group_count: None,
    };
    assert_eq!(note.position(), RuntimePos::ButtonZonePos(ButtonZone::K1));
}

#[test]
fn test_touch_note_position() {
    let note = TouchNote {
        params: CommonNoteParams {
            judge_timing: 1000000,
            judge_offset: 0,
            is_break: false,
            is_ex: false,
            note_index: 0,
        },
        state: TouchState::TouchWaiting,
        sensor_pos: SensorArea::A1,
        touch_group_id: None,
        touch_group_size: None,
        touch_group_count: None,
    };
    assert_eq!(note.position(), RuntimePos::SensorAreaPos(SensorArea::A1));
}
