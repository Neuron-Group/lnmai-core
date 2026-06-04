//! RealChartBenchmark - 性能基准测试，使用真实 simulation pipeline
//!
//! 对应 Lean 的 Apps/RealChartBenchmark.lean
//! 使用 build_game_state + step_frame 进行性能测量

use lnmai_core::areas::*;
use lnmai_core::chart_loader::*;
use lnmai_core::input_model::*;
use lnmai_core::lifecycle::*;
use lnmai_core::scheduler::*;
use lnmai_core::types::*;

use std::collections::BTreeMap;
use std::time::Instant;

// ============================================================================
// Simulation helpers (shared with dump_koishi_non_perfects)
// ============================================================================

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
                let cur = btn_count.get(*zone);
                btn_count = btn_count.set(*zone, cur + 1);
            }
            TimedInputEvent::ButtonHold { zone, is_down, .. } => {
                btn_held = btn_held.set(*zone, *is_down);
            }
            TimedInputEvent::SensorClick { area, .. } => {
                sen_clicked = sen_clicked.set(*area, true);
                let cur = sen_count.get(*area);
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
        delta: 16667,
    }
}

fn default_tactic(chart: &ChartSpec) -> Vec<TimedInputEvent> {
    let mut events = Vec::new();
    for tap in &chart.taps {
        events.push(TimedInputEvent::ButtonClick { tp: tap.timing, zone: tap.slot.to_button_zone() });
    }
    for hold in &chart.holds {
        let zone = hold.slot.to_button_zone();
        events.push(TimedInputEvent::ButtonClick { tp: hold.timing, zone });
        events.push(TimedInputEvent::ButtonHold { tp: hold.timing, zone, is_down: true });
        events.push(TimedInputEvent::ButtonHold { tp: hold.timing + hold.length, zone, is_down: false });
    }
    for touch in &chart.touches {
        events.push(TimedInputEvent::SensorClick { tp: touch.timing, area: touch.sensor_pos });
    }
    events.sort_by_key(|e| e.at());
    events
}

fn simulate_chart(chart: &ChartSpec, tactic_events: &[TimedInputEvent]) -> (Vec<JudgeEvent>, u32) {
    let state = build_game_state(chart);
    let batches = events_to_batches(tactic_events);
    let mut all_events: Vec<JudgeEvent> = Vec::new();
    let mut current_state = state;

    for batch in batches {
        current_state.current_time = batch.current_time;
        let input = batch_to_frame_input(&batch);
        let result = step_frame(&current_state, &input);
        all_events.extend(result.events);
        current_state = result.state;
    }

    let total_notes = (chart.taps.len() + chart.holds.len() + chart.touches.len()
        + chart.touch_holds.len() + chart.slides.len()) as u32;
    let judged: std::collections::HashSet<u32> = all_events.iter().map(|e| e.note_index).collect();
    let missing = (0..total_notes).filter(|i| !judged.contains(i)).count() as u32;
    let checksum = all_events.len() as u32 + missing;

    (all_events, checksum)
}

fn summarize_grades(events: &[JudgeEvent]) -> Vec<(JudgeGrade, u32)> {
    let mut counts = std::collections::HashMap::new();
    for evt in events {
        *counts.entry(evt.grade).or_insert(0u32) += 1;
    }
    counts.into_iter().collect()
}

fn achieves_ap(events: &[JudgeEvent]) -> bool {
    events.iter().all(|evt| evt.grade.is_perfect_grade())
}

// ============================================================================
// Chart generators (matching Lean benchmark checkpoints)
// ============================================================================

/// Level 7 chart (类似 100524_[協]Hand in Hand)
fn generate_chart_level7() -> ChartSpec {
    let mut taps = Vec::new();
    let mut holds = Vec::new();
    let mut touches = Vec::new();

    for i in 0..200 {
        taps.push(TapChartNote {
            timing: (1000000 + i * 25000) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            is_break: i % 20 == 0,
            is_ex: false,
            button_queue_index: (i % 8) as u32,
            note_index: i as u32,
        });
    }
    // holds start after all taps to avoid same-zone overlap (tap-before-hold processing order)
    let hold_start = 1000000 + 200 * 25000 + 200000;
    for i in 0..50 {
        holds.push(HoldChartNote {
            timing: (hold_start + i * 100000) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            length: 500000,
            is_break: i % 10 == 0,
            is_ex: false,
            is_touch: false,
            is_classic: false,
            button_queue_index: (i % 8) as u32,
            touch_hold_group_id: None,
            touch_hold_group_size: None,
            note_index: (200 + i) as u32,
        });
    }
    let touch_start = hold_start + 50 * 100000 + 500000 + 200000;
    for i in 0..30 {
        touches.push(TouchChartNote {
            timing: (touch_start + i * 100000) as i64,
            sensor_pos: SensorArea::from_index((i % 33) as usize).unwrap(),
            is_break: false,
            touch_queue_index: i as u32,
            touch_group_id: None,
            touch_group_size: None,
            note_index: (250 + i) as u32,
        });
    }

    ChartSpec { taps, holds, touches, touch_holds: vec![], slides: vec![], slide_skipping: false }
}

/// Level 5 chart (类似 11358_インドア系ならトラックメイカー)
fn generate_chart_level5() -> ChartSpec {
    let mut taps = Vec::new();
    let mut holds = Vec::new();

    for i in 0..150 {
        taps.push(TapChartNote {
            timing: (1000000 + i * 33000) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            is_break: i % 15 == 0,
            is_ex: false,
            button_queue_index: (i % 8) as u32,
            note_index: i as u32,
        });
    }
    let hold_start = 1000000 + 150 * 33000 + 200000;
    for i in 0..30 {
        holds.push(HoldChartNote {
            timing: (hold_start + i * 200000) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            length: 300000,
            is_break: i % 8 == 0,
            is_ex: false,
            is_touch: false,
            is_classic: false,
            button_queue_index: (i % 8) as u32,
            touch_hold_group_id: None,
            touch_hold_group_size: None,
            note_index: (150 + i) as u32,
        });
    }

    ChartSpec { taps, holds, touches: vec![], touch_holds: vec![], slides: vec![], slide_skipping: false }
}

/// Level 6 chart (类似 834_PANDORA PARADOXXX)
fn generate_chart_level6() -> ChartSpec {
    let mut taps = Vec::new();
    let mut holds = Vec::new();
    let mut touches = Vec::new();

    for i in 0..300 {
        taps.push(TapChartNote {
            timing: (1000000 + i * 12500) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            is_break: i % 25 == 0,
            is_ex: i % 50 == 0,
            button_queue_index: (i % 8) as u32,
            note_index: i as u32,
        });
    }
    let hold_start = 1000000 + 300 * 12500 + 200000;
    for i in 0..80 {
        holds.push(HoldChartNote {
            timing: (hold_start + i * 100000) as i64,
            slot: OuterSlot::from_index((i % 8) as usize).unwrap(),
            length: 400000,
            is_break: i % 12 == 0,
            is_ex: false,
            is_touch: i % 4 == 0,
            is_classic: false,
            button_queue_index: (i % 8) as u32,
            touch_hold_group_id: None,
            touch_hold_group_size: None,
            note_index: (300 + i) as u32,
        });
    }
    let touch_start = hold_start + 80 * 100000 + 400000 + 200000;
    for i in 0..50 {
        touches.push(TouchChartNote {
            timing: (touch_start + i * 100000) as i64,
            sensor_pos: SensorArea::from_index((i % 33) as usize).unwrap(),
            is_break: false,
            touch_queue_index: i as u32,
            touch_group_id: None,
            touch_group_size: None,
            note_index: (380 + i) as u32,
        });
    }

    ChartSpec { taps, holds, touches, touch_holds: vec![], slides: vec![], slide_skipping: false }
}

// ============================================================================
// Benchmark
// ============================================================================

fn benchmark_checkpoint(name: &str, chart: &ChartSpec, iterations: u32) {
    let tactic = default_tactic(chart);
    let total_notes = chart.taps.len() + chart.holds.len() + chart.touches.len()
        + chart.touch_holds.len() + chart.slides.len();

    println!("[{}]", name);
    println!("  notes: {}", total_notes);
    println!("  tactic events: {}", tactic.len());

    let start = Instant::now();
    let mut checksum = 0u32;
    let mut last_events = Vec::new();

    for _ in 0..iterations {
        let (events, cs) = simulate_chart(chart, &tactic);
        checksum += cs;
        last_events = events;
    }

    let elapsed = start.elapsed();
    let avg = elapsed / iterations;
    let avg_micros_per_note = if total_notes > 0 {
        avg.as_micros() as f64 / total_notes as f64
    } else {
        0.0
    };

    let grade_summary = summarize_grades(&last_events);
    let ap = achieves_ap(&last_events);

    println!("  judged: {}", last_events.len());
    println!("  achievesAP: {}", ap);
    println!("  checksum: {}", checksum);
    println!("  iterations: {}", iterations);
    println!("  totalElapsedMs: {:.2}", elapsed.as_secs_f64() * 1000.0);
    println!("  avgElapsedMs: {:.2}", avg.as_secs_f64() * 1000.0);
    println!("  avgMicrosPerNote: {:.2}", avg_micros_per_note);
    println!("  grades: {:?}", grade_summary);
    println!();
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_benchmark_all_charts() {
    let bench_iterations = 5;

    println!("=== LnmaiCore RealChartBenchmark (Rust - real pipeline) ===");
    println!();

    let charts: Vec<(&str, ChartSpec)> = vec![
        ("100524_[協]Hand in Hand (Level 7)", generate_chart_level7()),
        ("11358_インドア系ならトラックメイカー (Level 5)", generate_chart_level5()),
        ("834_PANDORA PARADOXXX (Level 6)", generate_chart_level6()),
    ];

    for (name, chart) in &charts {
        benchmark_checkpoint(name, chart, bench_iterations);
    }

    println!("=== Benchmark Complete ===");
}

#[test]
fn test_simulation_produces_events() {
    let chart = generate_chart_level5();
    let tactic = default_tactic(&chart);
    let (events, checksum) = simulate_chart(&chart, &tactic);

    assert!(events.len() > 0, "Should produce judge events");
    assert!(checksum > 0, "Checksum should be positive");
}

#[test]
fn test_chart_sizes() {
    let c7 = generate_chart_level7();
    assert_eq!(c7.taps.len(), 200);
    assert_eq!(c7.holds.len(), 50);
    assert_eq!(c7.touches.len(), 30);

    let c5 = generate_chart_level5();
    assert_eq!(c5.taps.len(), 150);
    assert_eq!(c5.holds.len(), 30);

    let c6 = generate_chart_level6();
    assert_eq!(c6.taps.len(), 300);
    assert_eq!(c6.holds.len(), 80);
    assert_eq!(c6.touches.len(), 50);
}

#[test]
fn test_benchmark_determinism() {
    let chart = generate_chart_level5();
    let tactic = default_tactic(&chart);

    let (events1, cs1) = simulate_chart(&chart, &tactic);
    let (events2, cs2) = simulate_chart(&chart, &tactic);

    assert_eq!(cs1, cs2, "Checksum should be deterministic");
    assert_eq!(events1.len(), events2.len(), "Event count should be deterministic");
    assert_eq!(events1, events2, "Events should be identical");
}
