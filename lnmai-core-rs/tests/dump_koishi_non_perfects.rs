//! DumpKoishiNonPerfects - 使用真实 simulation pipeline
//!
//! 对应 Lean 的 Apps/DumpKoishiNonPerfects.lean
//! 使用 build_game_state + step_frame 进行真实模拟

use lnmai_core::areas::*;
use lnmai_core::chart_loader::*;
use lnmai_core::input_model::*;
use lnmai_core::judge::*;
use lnmai_core::lifecycle::*;
use lnmai_core::scheduler::*;
use lnmai_core::types::*;

use std::collections::BTreeMap;

// ============================================================================
// Simulation helpers
// ============================================================================

/// Convert a list of timed tactic events into batches grouped by time
fn events_to_batches(events: &[TimedInputEvent]) -> Vec<TimedInputBatch> {
    let mut map: BTreeMap<i64, Vec<TimedInputEvent>> = BTreeMap::new();
    for &evt in events {
        map.entry(evt.at()).or_default().push(evt);
    }
    map.into_iter()
        .map(|(time, evts)| TimedInputBatch {
            current_time: time,
            events: evts,
        })
        .collect()
}

/// Convert a TimedInputBatch into a FrameInput
fn batch_to_frame_input(batch: &TimedInputBatch) -> FrameInput {
    let mut btn_clicked = ButtonVec::replicate(false);
    let mut btn_held = ButtonVec::replicate(false);
    let mut sen_clicked = SensorVec::replicate(false);
    let mut sen_held = SensorVec::replicate(false);
    let mut btn_count = ButtonVec::replicate(0u32);
    let mut sen_count = SensorVec::replicate(0u32);

    for evt in &batch.events {
        match evt {
            TimedInputEvent::ButtonClick { zone, .. } => {
                btn_clicked = btn_clicked.set(*zone, true);
                let cur = btn_count.get_d(*zone, 0);
                btn_count = btn_count.set(*zone, cur + 1);
            }
            TimedInputEvent::ButtonHold { zone, is_down, .. } => {
                btn_held = btn_held.set(*zone, *is_down);
            }
            TimedInputEvent::SensorClick { area, .. } => {
                sen_clicked = sen_clicked.set(*area, true);
                let cur = sen_count.get_d(*area, 0);
                sen_count = sen_count.set(*area, cur + 1);
            }
            TimedInputEvent::SensorHold { area, is_down, .. } => {
                sen_held = sen_held.set(*area, *is_down);
            }
        }
    }

    FrameInput {
        button_clicked: btn_clicked,
        button_held: btn_held,
        sensor_clicked: sen_clicked,
        sensor_held: sen_held,
        button_click_count: btn_count,
        sensor_click_count: sen_count,
        delta: 16667, // FRAME_LENGTH
    }
}

/// Generate default tactic events for a chart
fn default_tactic(chart: &ChartSpec) -> Vec<TimedInputEvent> {
    let mut events = Vec::new();

    for tap in &chart.taps {
        let zone = tap.slot.to_button_zone();
        events.push(TimedInputEvent::ButtonClick {
            tp: tap.timing,
            zone,
        });
    }

    for hold in &chart.holds {
        let zone = hold.slot.to_button_zone();
        events.push(TimedInputEvent::ButtonClick {
            tp: hold.timing,
            zone,
        });
        events.push(TimedInputEvent::ButtonHold {
            tp: hold.timing,
            zone,
            is_down: true,
        });
        events.push(TimedInputEvent::ButtonHold {
            tp: hold.timing + hold.length,
            zone,
            is_down: false,
        });
    }

    for touch in &chart.touches {
        events.push(TimedInputEvent::SensorClick {
            tp: touch.timing,
            area: touch.sensor_pos,
        });
    }

    events.sort_by_key(|e| e.at());
    events
}

/// Run full simulation for a chart with tactic events
fn simulate_chart(chart: &ChartSpec, tactic_events: &[TimedInputEvent]) -> Vec<JudgeEvent> {
    let state = build_game_state(chart);
    let batches = events_to_batches(tactic_events);
    let mut all_events: Vec<JudgeEvent> = Vec::new();
    let mut current_state = state;

    for batch in batches {
        current_state.current_time = batch.current_time;
        let input = batch_to_frame_input(&batch);
        let result = step_frame(&mut current_state, &input);
        all_events.extend(result.events);
    }

    // Run settle frames: simulate empty frames for ~2000ms beyond last note
    if let Some(last_time) = tactic_events.last().map(|e| e.at()) {
        let end_time = last_time + 2000000; // 2 seconds settle
        let frame_length: i64 = 16667;
        let mut t = last_time + frame_length;
        while t < end_time {
            current_state.current_time = t;
            let input = FrameInput {
                delta: frame_length,
                ..Default::default()
            };
            let result = step_frame(&mut current_state, &input);
            all_events.extend(result.events);
            t += frame_length;
        }
    }

    all_events
}

/// Filter non-perfect events
fn non_perfects(events: &[JudgeEvent]) -> Vec<(u32, JudgeGrade)> {
    events
        .iter()
        .filter(|evt| evt.grade != JudgeGrade::Perfect)
        .map(|evt| (evt.note_index, evt.grade))
        .collect()
}

/// Check AP achievement
fn achieves_ap(events: &[JudgeEvent]) -> bool {
    events.iter().all(|evt| evt.grade.is_perfect_grade())
}

// ============================================================================
// Chart generation (matching 小石DISCO structure)
// ============================================================================

fn generate_chart() -> ChartSpec {
    let mut taps = Vec::new();
    let mut holds = Vec::new();
    let mut touches = Vec::new();

    // 生成 taps (类似小石DISCO 的 tap 数量)
    for i in 0..300 {
        taps.push(TapChartNote {
            timing: (1000000 + i * 50000) as i64, // 每50ms一个tap
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            is_break: i % 20 == 0,
            is_ex: false,
            button_queue_index: (i % 8) as u32,
            note_index: i as u32,
        });
    }

    // 生成 holds — timing 在所有 taps 之后，避免与 tap 在同一 zone 重叠
    let tap_end = 1000000 + 300 * 50000;
    for i in 0..50 {
        holds.push(HoldChartNote {
            timing: (tap_end + 500000 + i * 200000) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            length: 500000, // 500ms hold
            is_break: i % 10 == 0,
            is_ex: false,
            is_touch: i % 4 == 0,
            is_classic: false,
            button_queue_index: (i % 8) as u32,
            touch_hold_group_id: None,
            touch_hold_group_size: None,
            note_index: (300 + i) as u32,
        });
    }

    // 生成 touches — timing 在 holds 之后
    let hold_end = tap_end + 500000 + 50 * 200000 + 500000;
    for i in 0..30 {
        touches.push(TouchChartNote {
            timing: (hold_end + i * 100000) as i64,
            sensor_pos: SensorArea::from_index((i % 33) as usize).unwrap(),
            is_break: false,
            touch_queue_index: i as u32,
            touch_group_id: None,
            touch_group_size: None,
            note_index: (350 + i) as u32,
        });
    }

    ChartSpec {
        taps,
        holds,
        touches,
        touch_holds: vec![],
        slides: vec![],
        slide_skipping: false,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_simulate_default_tactic() {
    let chart = generate_chart();
    let tactic = default_tactic(&chart);

    println!("Chart: {} taps, {} holds, {} touches", chart.taps.len(), chart.holds.len(), chart.touches.len());
    println!("Tactic events: {}", tactic.len());

    let events = simulate_chart(&chart, &tactic);
    let non_p = non_perfects(&events);

    println!("Judged events: {}", events.len());
    println!("Non-perfect count: {}", non_p.len());
    for (idx, grade) in &non_p {
        println!("  note {}: {:?}", idx, grade);
    }
    println!("Achieves AP: {}", achieves_ap(&events));

    // 默认策略下，有 imperfect timing 的 hold notes 可能产生非 Perfect
    // 但大部分笔记应该是 Perfect 的
    assert!(events.len() > 0, "Should have judged events");
}

#[test]
fn test_perfect_tactic_achieves_ap() {
    let chart = generate_chart();

    // 构建完美策略：所有 tap 和 touch 精确命中
    let mut perfect_events = Vec::new();
    for tap in &chart.taps {
        perfect_events.push(TimedInputEvent::ButtonClick {
            tp: tap.timing,
            zone: tap.slot.to_button_zone(),
        });
    }
    for hold in &chart.holds {
        let zone = hold.slot.to_button_zone();
        perfect_events.push(TimedInputEvent::ButtonClick {
            tp: hold.timing,
            zone,
        });
        perfect_events.push(TimedInputEvent::ButtonHold {
            tp: hold.timing,
            zone,
            is_down: true,
        });
        perfect_events.push(TimedInputEvent::ButtonHold {
            tp: hold.timing + hold.length,
            zone,
            is_down: false,
        });
    }
    for touch in &chart.touches {
        perfect_events.push(TimedInputEvent::SensorClick {
            tp: touch.timing,
            area: touch.sensor_pos,
        });
    }
    perfect_events.sort_by_key(|e| e.at());

    let events = simulate_chart(&chart, &perfect_events);
    let non_p = non_perfects(&events);

    println!("Perfect tactic: {} judged, {} non-perfect", events.len(), non_p.len());

    // 完美输入应该产生全部 Perfect 判定
    assert_eq!(non_p.len(), 0, "Perfect tactic should produce no non-perfect grades");
    assert!(achieves_ap(&events), "Perfect tactic should achieve AP");
}

#[test]
fn test_imperfect_tactic_produces_non_perfect() {
    let chart = generate_chart();

    // 故意偏移 tap 的点击时间
    let mut imperfect_events = Vec::new();
    for tap in &chart.taps {
        // 故意偏移 30ms (超过 1st perfect window)
        let offset_timing = if tap.note_index % 3 == 0 {
            tap.timing + 30000 // late
        } else if tap.note_index % 3 == 1 {
            tap.timing - 20000 // slightly fast
        } else {
            tap.timing // perfect
        };
        imperfect_events.push(TimedInputEvent::ButtonClick {
            tp: offset_timing,
            zone: tap.slot.to_button_zone(),
        });
    }
    // holds 和 touches 保持完美
    for hold in &chart.holds {
        if hold.is_touch {
            let area = SensorArea::from_index(0).unwrap();
            imperfect_events.push(TimedInputEvent::SensorClick {
                tp: hold.timing,
                area,
            });
            imperfect_events.push(TimedInputEvent::SensorHold {
                tp: hold.timing,
                area,
                is_down: true,
            });
            imperfect_events.push(TimedInputEvent::SensorHold {
                tp: hold.timing + hold.length,
                area,
                is_down: false,
            });
        } else {
            imperfect_events.push(TimedInputEvent::ButtonClick {
                tp: hold.timing,
                zone: hold.slot.to_button_zone(),
            });
            imperfect_events.push(TimedInputEvent::ButtonHold {
                tp: hold.timing,
                zone: hold.slot.to_button_zone(),
                is_down: true,
            });
            imperfect_events.push(TimedInputEvent::ButtonHold {
                tp: hold.timing + hold.length,
                zone: hold.slot.to_button_zone(),
                is_down: false,
            });
        }
    }
    for touch in &chart.touches {
        imperfect_events.push(TimedInputEvent::SensorClick {
            tp: touch.timing,
            area: touch.sensor_pos,
        });
    }
    imperfect_events.sort_by_key(|e| e.at());

    let events = simulate_chart(&chart, &imperfect_events);
    let non_p = non_perfects(&events);

    println!("Imperfect tactic: {} judged, {} non-perfect", events.len(), non_p.len());
    for (idx, grade) in &non_p {
        println!("  note {}: {:?}", idx, grade);
    }

    // 有偏移的输入应该产生非 Perfect
    assert!(non_p.len() > 0, "Imperfect tactic should produce some non-perfect grades");
    assert!(!achieves_ap(&events), "Imperfect tactic should NOT achieve AP");
}

#[test]
fn test_non_perfects_filter() {
    let events = vec![
        JudgeEvent {
            kind: JudgeEventKind::Tap,
            grade: JudgeGrade::Perfect,
            diff: 0,
            position: RuntimePos::ButtonZonePos(ButtonZone::K1),
            note_index: 0,
        },
        JudgeEvent {
            kind: JudgeEventKind::Tap,
            grade: JudgeGrade::LateGreat,
            diff: 30000,
            position: RuntimePos::ButtonZonePos(ButtonZone::K2),
            note_index: 1,
        },
        JudgeEvent {
            kind: JudgeEventKind::Tap,
            grade: JudgeGrade::Perfect,
            diff: 0,
            position: RuntimePos::ButtonZonePos(ButtonZone::K3),
            note_index: 2,
        },
    ];

    let non_perfects = non_perfects(&events);
    assert_eq!(non_perfects.len(), 1);
    assert_eq!(non_perfects[0], (1, JudgeGrade::LateGreat));
}

#[test]
fn test_achieves_ap() {
    let perfect_events = vec![
        JudgeEvent {
            kind: JudgeEventKind::Tap,
            grade: JudgeGrade::Perfect,
            diff: 0,
            position: RuntimePos::ButtonZonePos(ButtonZone::K1),
            note_index: 0,
        },
    ];
    assert!(achieves_ap(&perfect_events));

    let mixed_events = vec![
        JudgeEvent {
            kind: JudgeEventKind::Tap,
            grade: JudgeGrade::LatePerfect2nd,
            diff: 20000,
            position: RuntimePos::ButtonZonePos(ButtonZone::K1),
            note_index: 0,
        },
    ];
    assert!(achieves_ap(&mixed_events)); // Perfect2nd counts as AP

    let non_ap_events = vec![
        JudgeEvent {
            kind: JudgeEventKind::Tap,
            grade: JudgeGrade::LateGreat,
            diff: 70000,
            position: RuntimePos::ButtonZonePos(ButtonZone::K1),
            note_index: 0,
        },
    ];
    assert!(!achieves_ap(&non_ap_events));
}

// ============================================================================
// Direct comparison test: calls pure judge/score functions directly
// These are the core functions proven equivalent in verification/Equiv.lean
// ============================================================================

#[test]
fn test_dump_non_perfects_direct() {
    // Simulate 5 tap notes with various timing offsets
    // Note index 0: Perfect (0μs diff)
    // Note index 1: LatePerfect2nd (+20000μs)
    // Note index 2: FastGreat (-70000μs)
    // Note index 3: LateGood (+120000μs)
    // Note index 4: Perfect (0μs diff)

    let diffs = [
        (0, lnmai_core::time::Duration::from_micros(0)),
        (1, lnmai_core::time::Duration::from_micros(20000)),
        (2, lnmai_core::time::Duration::from_micros(-70000)),
        (3, lnmai_core::time::Duration::from_micros(120000)),
        (4, lnmai_core::time::Duration::from_micros(0)),
    ];

    println!("=== Rust DumpKoishiNonPerfects (direct judge functions) ===");
    println!("Judged events:");
    for (idx, diff) in &diffs {
        let grade = lnmai_core::judge::judge_tap(*diff, false);
        let is_perfect = grade == JudgeGrade::Perfect;
        println!("  note {}: {:?} | diff: {}μs | perfect: {}",
            idx, grade, diff.to_micros(), is_perfect);
    }

    let non_p: Vec<_> = diffs.iter()
        .map(|(idx, diff)| (*idx, lnmai_core::judge::judge_tap(*diff, false)))
        .filter(|(_, g)| *g != JudgeGrade::Perfect)
        .collect();

    println!("\nNon-perfect events: {:?}", non_p);
    println!("Expected: note 1=LatePerfect2nd, note 2=FastGreat, note 3=LateGood");
    assert_eq!(non_p.len(), 3);
}
