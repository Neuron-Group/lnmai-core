//! RealChartBenchmark - 读取真实谱面文件进行性能测试
//!
//! 对应 Lean 的 Apps/RealChartBenchmark.lean
//! 使用 simai parser 读取 .txt 文件 + build_game_state + step_frame

use lnmai_core::areas::*;
use lnmai_core::chart_loader::*;
use lnmai_core::input_model::*;
use lnmai_core::scheduler::*;
use lnmai_core::simai::compile_lowered;
use lnmai_core::types::*;

use std::collections::BTreeMap;
use std::time::Instant;

fn events_to_batches(events: &[TimedInputEvent]) -> Vec<TimedInputBatch> {
    let mut map: BTreeMap<i64, Vec<TimedInputEvent>> = BTreeMap::new();
    for evt in events {
        map.entry(evt.at()).or_default().push(*evt);
    }
    map.into_iter()
        .map(|(time, evts)| TimedInputBatch { current_time: time, events: evts })
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
        button_clicked: btn_clicked, button_held: btn_held,
        sensor_clicked: sen_clicked, sensor_held: sen_held,
        button_click_count: btn_count, sensor_click_count: sen_count,
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
        let result = step_frame(&mut current_state, &input);
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

fn benchmark_checkpoint(name: &str, chart: &ChartSpec, iterations: u32) {
    let tactic = default_tactic(chart);
    let total_notes = chart.taps.len() + chart.holds.len() + chart.touches.len()
        + chart.touch_holds.len() + chart.slides.len();

    println!("[{}]", name);
    println!("  taps: {} holds: {} touches: {} touchHolds: {} slides: {}",
        chart.taps.len(), chart.holds.len(), chart.touches.len(),
        chart.touch_holds.len(), chart.slides.len());
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

/// Load chart from maidata.txt file, fall back to synthetic if file not found
fn load_chart(path: &str, level: u32, _name: &str) -> Option<ChartSpec> {
    match std::fs::read_to_string(path) {
        Ok(content) => match compile_lowered(&content, level) {
            Ok(c) => Some(c),
            Err(e) => {
                println!("  parse error: {}", e);
                None
            }
        },
        Err(_) => None,
    }
}

// ============================================================================
// Benchmarks
// ============================================================================

#[test]
fn test_benchmark_real_charts() {
    let bench_iterations = 3;

    println!("=== LnmaiCore RealChartBenchmark (Rust - real .txt files via simai) ===");
    println!();

    let checkpoints: Vec<(&str, &str, u32)> = vec![
        ("100524_[協]Hand in Hand", "../tools/assets/100524_[協]Hand in Hand/maidata.txt", 7),
        ("11358_インドア系ならトラックメイカー", "../tools/assets/11358_インドア系ならトラックメイカー/maidata.txt", 5),
        ("834_PANDORA PARADOXXX", "../tools/assets/834_PANDORA PARADOXXX/maidata.txt", 6),
    ];

    let mut loaded = 0u32;
    for (name, path, level) in &checkpoints {
        if let Some(chart) = load_chart(path, *level, name) {
            benchmark_checkpoint(name, &chart, bench_iterations);
            loaded += 1;
        } else {
            println!("[{}] SKIPPED (file not found)", name);
        }
    }

    println!("=== Loaded {} / {} charts ===", loaded, checkpoints.len());
}

#[test]
fn test_simulation_produces_events() {
    // Use the smallest real chart
    if let Some(chart) = load_chart("../tools/assets/11358_インドア系ならトラックメイカー/maidata.txt", 5, "test") {
        let tactic = default_tactic(&chart);
        let (events, checksum) = simulate_chart(&chart, &tactic);
        assert!(events.len() > 0, "Should produce judge events");
        assert!(checksum > 0, "Checksum should be positive");
    }
}

#[test]
fn test_benchmark_determinism() {
    if let Some(chart) = load_chart("../tools/assets/11358_インドア系ならトラックメイカー/maidata.txt", 5, "test") {
        let tactic = default_tactic(&chart);
        let (events1, cs1) = simulate_chart(&chart, &tactic);
        let (events2, cs2) = simulate_chart(&chart, &tactic);
        assert_eq!(cs1, cs2, "Checksum should be deterministic");
        assert_eq!(events1.len(), events2.len(), "Event count should be deterministic");
    }
}
