//! FFI JSON API and process-local session registry.
//!
//! Mirrors the exported surface of `LnmaiCore/FFI.lean`: every entry point
//! returns a JSON envelope (`{"ok":true,"result":...}` or
//! `{"ok":false,"error":{...}}`). The Lean implementation stores sessions in a
//! `Std.Mutex`-guarded registry; this port uses a `Mutex<HashMap>`.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use serde_json::{json, Value};

use crate::areas::{ButtonZone, OuterSlot, SensorArea};
use crate::chart_loader::ChartSpec;
use crate::input_model::{GameState, TimedInputBatch, TimedInputEvent, ZoneQueue};
use crate::lifecycle::{
    HoldNote, HoldSubState, SlideNote, TapFamilyNote, TapState, TouchNote, TouchState,
};
use crate::runtime_score::{grade_index, RuntimeScoreState};
use crate::time::{Duration, TimePoint};
use crate::types::{JudgeEvent, JudgeEventKind, JudgeGrade, RuntimePos};

// ---------------------------------------------------------------------------
// Envelope helpers
// ---------------------------------------------------------------------------

pub fn ok_json(payload: Value) -> String {
    json!({ "ok": true, "result": payload }).to_string()
}

pub fn error_json(code: &str, message: &str) -> String {
    json!({ "ok": false, "error": { "code": code, "message": message } }).to_string()
}

// ---------------------------------------------------------------------------
// Scalar encoders
// ---------------------------------------------------------------------------

fn rat_json(r: crate::rat::Rat) -> Value {
    json!({ "num": r.num() as i64, "den": r.den() as i64, "decimal": r.to_decimal_string() })
}

fn duration_json(d: Duration) -> Value {
    json!(d.to_micros())
}

fn time_point_json(t: TimePoint) -> Value {
    json!(t.to_micros())
}

fn grade_str(g: JudgeGrade) -> &'static str {
    match g {
        JudgeGrade::Miss => "Miss",
        JudgeGrade::LateGood => "LateGood",
        JudgeGrade::LateGreat3rd => "LateGreat3rd",
        JudgeGrade::LateGreat2nd => "LateGreat2nd",
        JudgeGrade::LateGreat => "LateGreat",
        JudgeGrade::LatePerfect3rd => "LatePerfect3rd",
        JudgeGrade::LatePerfect2nd => "LatePerfect2nd",
        JudgeGrade::Perfect => "Perfect",
        JudgeGrade::FastPerfect2nd => "FastPerfect2nd",
        JudgeGrade::FastPerfect3rd => "FastPerfect3rd",
        JudgeGrade::FastGreat => "FastGreat",
        JudgeGrade::FastGreat2nd => "FastGreat2nd",
        JudgeGrade::FastGreat3rd => "FastGreat3rd",
        JudgeGrade::FastGood => "FastGood",
        JudgeGrade::TooFast => "TooFast",
    }
}

fn event_kind_str(k: JudgeEventKind) -> &'static str {
    match k {
        JudgeEventKind::Tap => "Tap",
        JudgeEventKind::Hold => "Hold",
        JudgeEventKind::Slide => "Slide",
        JudgeEventKind::Touch => "Touch",
        JudgeEventKind::Break => "Break",
    }
}

fn sensor_str(a: SensorArea) -> &'static str {
    match a {
        SensorArea::A1 => "A1", SensorArea::A2 => "A2", SensorArea::A3 => "A3", SensorArea::A4 => "A4",
        SensorArea::A5 => "A5", SensorArea::A6 => "A6", SensorArea::A7 => "A7", SensorArea::A8 => "A8",
        SensorArea::B1 => "B1", SensorArea::B2 => "B2", SensorArea::B3 => "B3", SensorArea::B4 => "B4",
        SensorArea::B5 => "B5", SensorArea::B6 => "B6", SensorArea::B7 => "B7", SensorArea::B8 => "B8",
        SensorArea::C => "C",
        SensorArea::D1 => "D1", SensorArea::D2 => "D2", SensorArea::D3 => "D3", SensorArea::D4 => "D4",
        SensorArea::D5 => "D5", SensorArea::D6 => "D6", SensorArea::D7 => "D7", SensorArea::D8 => "D8",
        SensorArea::E1 => "E1", SensorArea::E2 => "E2", SensorArea::E3 => "E3", SensorArea::E4 => "E4",
        SensorArea::E5 => "E5", SensorArea::E6 => "E6", SensorArea::E7 => "E7", SensorArea::E8 => "E8",
    }
}

fn button_str(z: ButtonZone) -> &'static str {
    match z {
        ButtonZone::K1 => "K1", ButtonZone::K2 => "K2", ButtonZone::K3 => "K3", ButtonZone::K4 => "K4",
        ButtonZone::K5 => "K5", ButtonZone::K6 => "K6", ButtonZone::K7 => "K7", ButtonZone::K8 => "K8",
    }
}

fn slot_str(s: OuterSlot) -> &'static str {
    match s {
        OuterSlot::S1 => "S1", OuterSlot::S2 => "S2", OuterSlot::S3 => "S3", OuterSlot::S4 => "S4",
        OuterSlot::S5 => "S5", OuterSlot::S6 => "S6", OuterSlot::S7 => "S7", OuterSlot::S8 => "S8",
    }
}

fn pos_json(p: RuntimePos) -> Value {
    match p {
        RuntimePos::Button(z) => json!({ "button": button_str(z) }),
        RuntimePos::Sensor(a) => json!({ "sensor": sensor_str(a) }),
    }
}

fn judge_event_json(e: &JudgeEvent) -> Value {
    json!({
        "kind": event_kind_str(e.kind),
        "grade": grade_str(e.grade),
        "diff": duration_json(e.diff),
        "position": pos_json(e.position),
        "noteIndex": e.note_index,
        "isBreak": e.is_break,
        "multiple": e.multiple,
    })
}

fn audio_cmd_json(c: &crate::events::AudioCommand) -> Value {
    match c {
        crate::events::AudioCommand::PlayJudgeSfx { kind, grade, is_break, at_time, note_index } => json!({
            "PlayJudgeSfx": {
                "kind": event_kind_str(*kind),
                "grade": grade_str(*grade),
                "isBreak": is_break,
                "atTime": time_point_json(*at_time),
                "noteIndex": note_index,
            }
        }),
        crate::events::AudioCommand::PlaySlideCue { note_index, track_index, is_break, at_time } => json!({
            "PlaySlideCue": {
                "noteIndex": note_index,
                "trackIndex": track_index,
                "isBreak": is_break,
                "atTime": time_point_json(*at_time),
            }
        }),
    }
}

fn render_cmd_json(c: &crate::events::RenderCommand) -> Value {
    match c {
        crate::events::RenderCommand::ShowJudgeResult { kind, grade, is_break, diff, note_index } => json!({
            "ShowJudgeResult": {
                "kind": event_kind_str(*kind),
                "grade": grade_str(*grade),
                "isBreak": is_break,
                "diff": duration_json(*diff),
                "noteIndex": note_index,
            }
        }),
        crate::events::RenderCommand::UpdateSlideProgress { note_index, remaining } => json!({
            "UpdateSlideProgress": { "noteIndex": note_index, "remaining": remaining }
        }),
        crate::events::RenderCommand::UpdateSlideTrackProgress { note_index, track_index, remaining } => json!({
            "UpdateSlideTrackProgress": {
                "noteIndex": note_index, "trackIndex": track_index, "remaining": remaining
            }
        }),
        crate::events::RenderCommand::HideAllSlideBars { note_index } => json!({
            "HideAllSlideBars": { "noteIndex": note_index }
        }),
        crate::events::RenderCommand::HideSlideBars { note_index, end_index } => json!({
            "HideSlideBars": { "noteIndex": note_index, "endIndex": end_index }
        }),
        crate::events::RenderCommand::HideSlideTrackBars { note_index, track_index, end_index } => json!({
            "HideSlideTrackBars": {
                "noteIndex": note_index, "trackIndex": track_index, "endIndex": end_index
            }
        }),
    }
}

fn grade_counts_json(counts: &[usize; 15]) -> Value {
    let names = [
        "Miss", "LateGood", "LateGreat3rd", "LateGreat2nd", "LateGreat",
        "LatePerfect3rd", "LatePerfect2nd", "Perfect", "FastPerfect2nd", "FastPerfect3rd",
        "FastGreat", "FastGreat2nd", "FastGreat3rd", "FastGood", "TooFast",
    ];
    let mut obj = serde_json::Map::new();
    for (i, name) in names.iter().enumerate() {
        obj.insert((*name).to_string(), json!(counts[i]));
    }
    Value::Object(obj)
}

fn score_json(s: &RuntimeScoreState) -> Value {
    json!({
        "combo": s.combo,
        "pCombo": s.p_combo,
        "cPCombo": s.c_p_combo,
        "totalBase": s.total_base,
        "totalExtra": s.total_extra,
        "earnedBase": s.earned_base,
        "earnedExtra": s.earned_extra,
        "earnedClassicExtra": s.earned_classic_extra,
        "lostBase": s.lost_base,
        "lostExtra": s.lost_extra,
        "lostClassicExtra": s.lost_classic_extra,
        "dxScore": s.dx_score,
        "maxDxScore": s.max_dx_score,
        "fastCount": s.fast_count,
        "lateCount": s.late_count,
        "counts": {
            "tapCount": grade_counts_json(&s.counts.tap),
            "holdCount": grade_counts_json(&s.counts.hold),
            "slideCount": grade_counts_json(&s.counts.slide),
            "touchCount": grade_counts_json(&s.counts.touch),
            "breakCount": grade_counts_json(&s.counts.break_),
        },
    })
}

// ---------------------------------------------------------------------------
// Chart IR encoders
// ---------------------------------------------------------------------------

fn slide_area_spec_json(spec: &crate::simai::slide_tables::SlideAreaSpec) -> Value {
    json!({
        "targetAreas": spec.target_areas.iter().map(|a| sensor_str(*a)).collect::<Vec<_>>(),
        "policy": match spec.policy { crate::types::AreaPolicy::Or => "Or", crate::types::AreaPolicy::And => "And" },
        "isLast": spec.is_last,
        "isSkippable": spec.is_skippable,
        "arrowProgressWhenOn": spec.arrow_progress_when_on,
        "arrowProgressWhenFinished": spec.arrow_progress_when_finished,
    })
}

fn slide_kind_str(k: crate::types::SlideKind) -> &'static str {
    match k {
        crate::types::SlideKind::Single => "Single",
        crate::types::SlideKind::Wifi => "Wifi",
        crate::types::SlideKind::ConnPart => "ConnPart",
    }
}

pub fn chart_spec_json(spec: &ChartSpec) -> Value {
    json!({
        "taps": spec.taps.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "slot": slot_str(n.slot),
            "isBreak": n.is_break, "isEX": n.is_ex,
            "buttonQueueIndex": n.button_queue_index, "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "holds": spec.holds.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "slot": slot_str(n.slot),
            "length": duration_json(n.length), "isBreak": n.is_break, "isEX": n.is_ex,
            "isTouch": n.is_touch, "isClassic": n.is_classic,
            "buttonQueueIndex": n.button_queue_index,
            "touchHoldGroupId": n.touch_hold_group_id, "touchHoldGroupSize": n.touch_hold_group_size,
            "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "touches": spec.touches.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "sensorPos": sensor_str(n.sensor_pos),
            "isBreak": n.is_break, "sourceGroupId": n.source_group_id,
            "sourceGroupIndex": n.source_group_index, "sourceGroupSize": n.source_group_size,
            "touchQueueIndex": n.touch_queue_index, "touchGroupId": n.touch_group_id,
            "touchGroupSize": n.touch_group_size, "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "touchHolds": spec.touch_holds.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "sensorPos": sensor_str(n.sensor_pos),
            "length": duration_json(n.length), "isBreak": n.is_break, "isEX": n.is_ex,
            "sourceGroupId": n.source_group_id, "sourceGroupIndex": n.source_group_index,
            "sourceGroupSize": n.source_group_size, "touchQueueIndex": n.touch_queue_index,
            "touchGroupId": n.touch_group_id, "touchGroupSize": n.touch_group_size,
            "touchHoldGroupId": n.touch_hold_group_id, "touchHoldGroupSize": n.touch_hold_group_size,
            "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "slideHeads": spec.slide_heads.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "slot": slot_str(n.slot),
            "isBreak": n.is_break, "isEX": n.is_ex,
            "logicalSlideId": n.logical_slide_id, "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "slides": spec.slides.iter().map(slide_chart_note_json).collect::<Vec<_>>(),
        "slideSkipping": spec.slide_skipping,
    })
}

fn slide_chart_note_json(n: &crate::chart_loader::SlideChartNote) -> Value {
    json!({
        "headTiming": time_point_json(n.head_timing), "slot": slot_str(n.slot),
        "length": duration_json(n.length), "startTiming": time_point_json(n.start_timing),
        "groupStartTiming": n.group_start_timing.map(time_point_json),
        "slideKind": slide_kind_str(n.slide_kind), "isClassic": n.is_classic,
        "isSlideNoHead": n.is_slide_no_head, "isConnSlide": n.is_conn_slide,
        "parentNoteIndex": n.parent_note_index, "isGroupHead": n.is_group_head,
        "isGroupEnd": n.is_group_end, "parentFinished": n.parent_finished,
        "parentPendingFinish": n.parent_pending_finish,
        "totalJudgeQueueLen": n.total_judge_queue_len, "trackCount": n.track_count,
        "judgeAt": n.judge_at.map(time_point_json), "isBreak": n.is_break, "isEX": n.is_ex,
        "multiple": n.multiple, "logicalSlideId": n.logical_slide_id, "noteIndex": n.note_index,
        "judgeQueues": n.judge_queues.iter().map(|q|
            q.iter().map(slide_area_spec_json).collect::<Vec<_>>()).collect::<Vec<_>>(),
        // Lean tuples are nested `Prod`, so a 3-tuple serializes as `[a,[b,c]]`.
        "debugSimai": n.debug_simai.as_ref().map(|(raw, key, just)| json!([raw, [key, just]])),
    })
}

pub fn normalized_chart_json(chart: &crate::simai::ir::NormalizedChart) -> Value {
    json!({
        "taps": chart.taps.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "slot": slot_str(n.slot),
            "isBreak": n.is_break, "isEX": n.is_ex, "isHanabi": n.is_hanabi,
            "isForceStar": n.is_force_star, "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "holds": chart.holds.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "slot": slot_str(n.slot),
            "length": duration_json(n.length), "isBreak": n.is_break, "isEX": n.is_ex,
            "isHanabi": n.is_hanabi, "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "touches": chart.touches.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "sensorPos": sensor_str(n.sensor_pos),
            "isBreak": n.is_break, "isHanabi": n.is_hanabi, "sourceGroupId": n.source_group_id,
            "sourceGroupIndex": n.source_group_index, "sourceGroupSize": n.source_group_size,
            "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "touchHolds": chart.touch_holds.iter().map(|n| json!({
            "timing": time_point_json(n.timing), "sensorPos": sensor_str(n.sensor_pos),
            "length": duration_json(n.length), "isBreak": n.is_break, "isEX": n.is_ex,
            "isHanabi": n.is_hanabi, "sourceGroupId": n.source_group_id,
            "sourceGroupIndex": n.source_group_index, "sourceGroupSize": n.source_group_size,
            "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "slides": chart.slides.iter().map(|n| json!({
            "headTiming": time_point_json(n.head_timing), "slot": slot_str(n.slot),
            "length": duration_json(n.length), "startTiming": time_point_json(n.start_timing),
            "groupStartTiming": n.group_start_timing.map(time_point_json),
            "hSpeed": rat_json(n.h_speed), "slideKind": slide_kind_str(n.slide_kind),
            "isClassic": n.is_classic, "trackCount": n.track_count,
            "judgeAt": n.judge_at.map(time_point_json), "isBreak": n.is_break, "isEX": n.is_ex,
            "isHanabi": n.is_hanabi, "hasHeadNote": n.has_head_note, "hasBody": n.has_body,
            "isSlideNoHead": n.is_slide_no_head, "isForceStar": n.is_force_star,
            "isFakeRotate": n.is_fake_rotate, "isSlideBreak": n.is_slide_break,
            "isConnSlide": n.is_conn_slide, "parentNoteIndex": n.parent_note_index,
            "isGroupHead": n.is_group_head, "isGroupEnd": n.is_group_end,
            "totalJudgeQueueLen": n.total_judge_queue_len, "multiple": n.multiple,
            "noteIndex": n.note_index,
        })).collect::<Vec<_>>(),
        "slideDebug": chart.slide_debug.iter().map(|d| json!({
            "noteIndex": d.note_index, "rawText": d.raw_text,
        })).collect::<Vec<_>>(),
        "slideSkipping": chart.slide_skipping,
    })
}

// ---------------------------------------------------------------------------
// TimedInputBatch decoder
// ---------------------------------------------------------------------------

fn parse_sensor(s: &str) -> Option<SensorArea> {
    Some(match s {
        "A1" => SensorArea::A1, "A2" => SensorArea::A2, "A3" => SensorArea::A3, "A4" => SensorArea::A4,
        "A5" => SensorArea::A5, "A6" => SensorArea::A6, "A7" => SensorArea::A7, "A8" => SensorArea::A8,
        "B1" => SensorArea::B1, "B2" => SensorArea::B2, "B3" => SensorArea::B3, "B4" => SensorArea::B4,
        "B5" => SensorArea::B5, "B6" => SensorArea::B6, "B7" => SensorArea::B7, "B8" => SensorArea::B8,
        "C" => SensorArea::C,
        "D1" => SensorArea::D1, "D2" => SensorArea::D2, "D3" => SensorArea::D3, "D4" => SensorArea::D4,
        "D5" => SensorArea::D5, "D6" => SensorArea::D6, "D7" => SensorArea::D7, "D8" => SensorArea::D8,
        "E1" => SensorArea::E1, "E2" => SensorArea::E2, "E3" => SensorArea::E3, "E4" => SensorArea::E4,
        "E5" => SensorArea::E5, "E6" => SensorArea::E6, "E7" => SensorArea::E7, "E8" => SensorArea::E8,
        _ => return None,
    })
}

fn parse_button(s: &str) -> Option<ButtonZone> {
    Some(match s {
        "K1" => ButtonZone::K1, "K2" => ButtonZone::K2, "K3" => ButtonZone::K3, "K4" => ButtonZone::K4,
        "K5" => ButtonZone::K5, "K6" => ButtonZone::K6, "K7" => ButtonZone::K7, "K8" => ButtonZone::K8,
        _ => return None,
    })
}

pub fn parse_timed_input_batch(value: &Value) -> Result<TimedInputBatch, String> {
    let current_time = value.get("currentTime").and_then(Value::as_i64).unwrap_or(0);
    let mut events = Vec::new();
    if let Some(arr) = value.get("events").and_then(Value::as_array) {
        for ev in arr {
            // Accept both `{"tag":"buttonClick", ...}` and Lean's derived
            // constructor-keyed encoding `{"buttonClick": {...}}`.
            let (tag, fields): (String, &Value) = if let Some(t) = ev.get("tag").and_then(Value::as_str) {
                (t.to_string(), ev)
            } else if let Some(obj) = ev.as_object() {
                match obj.iter().next() {
                    Some((k, v)) => (k.clone(), v),
                    None => return Err("empty input event".to_string()),
                }
            } else {
                return Err("invalid input event".to_string());
            };
            let tp = TimePoint::from_micros(fields.get("tp").and_then(Value::as_i64).unwrap_or(0));
            match tag.as_str() {
                "buttonClick" => {
                    let zone = fields.get("zone").and_then(Value::as_str).and_then(parse_button)
                        .ok_or_else(|| "invalid zone".to_string())?;
                    events.push(TimedInputEvent::ButtonClick(tp, zone));
                }
                "buttonHold" => {
                    let zone = fields.get("zone").and_then(Value::as_str).and_then(parse_button)
                        .ok_or_else(|| "invalid zone".to_string())?;
                    let is_down = fields.get("isDown").and_then(Value::as_bool).unwrap_or(true);
                    events.push(TimedInputEvent::ButtonHold(tp, zone, is_down));
                }
                "sensorClick" => {
                    let area = fields.get("area").and_then(Value::as_str).and_then(parse_sensor)
                        .ok_or_else(|| "invalid area".to_string())?;
                    events.push(TimedInputEvent::SensorClick(tp, area));
                }
                "sensorHold" => {
                    let area = fields.get("area").and_then(Value::as_str).and_then(parse_sensor)
                        .ok_or_else(|| "invalid area".to_string())?;
                    let is_down = fields.get("isDown").and_then(Value::as_bool).unwrap_or(true);
                    events.push(TimedInputEvent::SensorHold(tp, area, is_down));
                }
                other => return Err(format!("unknown input event tag: {}", other)),
            }
        }
    }
    Ok(TimedInputBatch { current_time: TimePoint::from_micros(current_time), events })
}

// ---------------------------------------------------------------------------
// Session registry
// ---------------------------------------------------------------------------

enum Session {
    Empty,
    Loaded { spec: ChartSpec, state: GameState },
}

fn registry() -> &'static Mutex<HashMap<u64, Session>> {
    static REG: OnceLock<Mutex<HashMap<u64, Session>>> = OnceLock::new();
    REG.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_handle() -> u64 {
    static NEXT: OnceLock<Mutex<u64>> = OnceLock::new();
    let m = NEXT.get_or_init(|| Mutex::new(1));
    let mut g = m.lock().unwrap();
    let h = *g;
    *g += 1;
    h
}

fn loaded_summary_json(spec: &ChartSpec) -> Value {
    json!({
        "tapCount": spec.taps.len(),
        "holdCount": spec.holds.len(),
        "touchCount": spec.touches.len(),
        "touchHoldCount": spec.touch_holds.len(),
        "slideHeadCount": spec.slide_heads.len(),
        "slideCount": spec.slides.len(),
    })
}

fn step_result_json(
    events: &[JudgeEvent],
    audio: &[crate::events::AudioCommand],
    render: &[crate::events::RenderCommand],
    score: &RuntimeScoreState,
    current_time: TimePoint,
) -> Value {
    json!({
        "events": events.iter().map(judge_event_json).collect::<Vec<_>>(),
        "audioCommands": audio.iter().map(audio_cmd_json).collect::<Vec<_>>(),
        "renderCommands": render.iter().map(render_cmd_json).collect::<Vec<_>>(),
        "score": score_json(score),
        "currentTime": time_point_json(current_time),
    })
}

// ---------------------------------------------------------------------------
// Exported entry points (Rust API; thin C ABI can wrap these)
// ---------------------------------------------------------------------------

pub const FFI_ABI_VERSION: u64 = 1;

pub fn ffi_version_json() -> String {
    ok_json(json!({ "abiVersion": FFI_ABI_VERSION }))
}

/// `lnmai_parse_lowered_chart_json`.
pub fn parse_lowered_chart_json(content: &str, level_index: u32) -> String {
    match crate::simai::frontend::frontend_lowered_chart(content, level_index as usize) {
        Ok(spec) => ok_json(chart_spec_json(&spec)),
        Err(e) => error_json("parse_error", &e.message),
    }
}

/// `lnmai_parse_normalized_chart_json`.
pub fn parse_normalized_chart_json(content: &str, level_index: u32) -> String {
    match crate::simai::frontend::frontend_normalized_chart(content, level_index as usize) {
        Ok(chart) => ok_json(normalized_chart_json(&chart)),
        Err(e) => error_json("parse_error", &e.message),
    }
}

/// `lnmai_parse_frontend_semantic_chart_json`.
pub fn parse_frontend_semantic_chart_json(content: &str, level_index: u32) -> String {
    match crate::simai::frontend::parse_frontend_semantic_chart(content, level_index as usize) {
        Ok(semantic) => ok_json(json!({
            "normalized": normalized_chart_json(&semantic.normalized),
            "lowered": chart_spec_json(&semantic.lowered),
        })),
        Err(e) => error_json("parse_error", &e.message),
    }
}

/// `lnmai_default_tactic_from_chart_json`.
///
/// Decodes a `ChartSpec` JSON (as produced by `lnmai_parse_lowered_chart_json`)
/// and returns the default autoplay tactic:
/// `{"ok":true,"result":{"events":[ ...TimedInputEvent... ]}}`.
pub fn default_tactic_from_chart_json(chart_spec_json: &str) -> String {
    match parse_chart_spec_json(chart_spec_json) {
        Ok(spec) => {
            let events = crate::default_tactic::default_tactic_from_chart(&spec);
            ok_json(json!({
                "events": events.iter().map(timed_event_json).collect::<Vec<_>>(),
            }))
        }
        Err(e) => error_json("invalid_chart_spec_json", &e),
    }
}

/// `lnmai_create_empty_session_handle`.
pub fn create_empty_session_handle() -> String {
    let handle = next_handle();
    registry().lock().unwrap().insert(handle, Session::Empty);
    ok_json(json!({ "handle": handle, "state": "empty" }))
}

/// `lnmai_load_chart_into_session_from_text`.
pub fn load_chart_into_session_from_text(handle: u64, content: &str, level_index: u32) -> String {
    let spec = match crate::simai::frontend::frontend_lowered_chart(content, level_index as usize) {
        Ok(spec) => spec,
        Err(e) => return error_json("parse_error", &e.message),
    };
    let state = crate::chart_loader::build_game_state(&spec);
    let mut reg = registry().lock().unwrap();
    match reg.get_mut(&handle) {
        None => error_json("invalid_session_state", "unknown session handle"),
        Some(slot) => {
            *slot = Session::Loaded { spec: spec.clone(), state };
            ok_json(json!({
                "handle": handle,
                "state": "loaded",
                "summary": loaded_summary_json(&spec),
            }))
        }
    }
}

/// `lnmai_load_chart_into_session_from_json` (expects a `ChartSpec` JSON produced
/// by `parse_lowered_chart_json`).
pub fn load_chart_into_session_from_json(handle: u64, chart_spec_json: &str) -> String {
    match parse_chart_spec_json(chart_spec_json) {
        Err(e) => error_json("invalid_chart_spec_json", &e),
        Ok(spec) => {
            let state = crate::chart_loader::build_game_state(&spec);
            let mut reg = registry().lock().unwrap();
            match reg.get_mut(&handle) {
                None => error_json("invalid_session_state", "unknown session handle"),
                Some(slot) => {
                    *slot = Session::Loaded { spec: spec.clone(), state };
                    ok_json(json!({
                        "handle": handle,
                        "state": "loaded",
                        "summary": loaded_summary_json(&spec),
                    }))
                }
            }
        }
    }
}

/// `lnmai_unload_chart_from_session`.
pub fn unload_chart_from_session(handle: u64) -> String {
    let mut reg = registry().lock().unwrap();
    match reg.get_mut(&handle) {
        None => error_json("invalid_session_state", "unknown session handle"),
        Some(slot) => {
            *slot = Session::Empty;
            ok_json(json!({ "handle": handle, "state": "empty" }))
        }
    }
}

/// `lnmai_get_lowered_chart_json_by_handle`.
pub fn get_lowered_chart_json_by_handle(handle: u64) -> String {
    let reg = registry().lock().unwrap();
    match reg.get(&handle) {
        Some(Session::Loaded { spec, .. }) => ok_json(chart_spec_json(spec)),
        _ => error_json("invalid_session_state", "session is not loaded"),
    }
}

/// `lnmai_free_game_state_handle`.
pub fn free_game_state_handle(handle: u64) -> String {
    let removed = registry().lock().unwrap().remove(&handle).is_some();
    if removed {
        ok_json(json!({ "freed": true }))
    } else {
        error_json("invalid_runtime_handle", "unknown handle")
    }
}

/// `lnmai_step_game_state_handle_light`.
pub fn step_game_state_handle_light(handle: u64, batch_json: &str) -> String {
    let value: Value = match serde_json::from_str(batch_json) {
        Ok(v) => v,
        Err(e) => return error_json("invalid_runtime_json", &e.to_string()),
    };
    let batch = match parse_timed_input_batch(&value) {
        Ok(b) => b,
        Err(e) => return error_json("invalid_runtime_json", &e),
    };
    let mut reg = registry().lock().unwrap();
    match reg.get_mut(&handle) {
        Some(Session::Loaded { state, .. }) => {
            let (next, events, audio, render) = crate::scheduler::step_frame_timed(state, &batch);
            let result = step_result_json(&events, &audio, &render, &next.score, next.current_time);
            *state = next;
            ok_json(result)
        }
        _ => error_json("invalid_runtime_handle", "session is not loaded"),
    }
}

// ---------------------------------------------------------------------------
// ChartSpec JSON decoder (for `load_..._from_json`)
// ---------------------------------------------------------------------------

fn as_i64(v: &Value) -> Option<i64> {
    v.as_i64()
}

fn parse_slot(s: &str) -> Option<OuterSlot> {
    Some(match s {
        "S1" => OuterSlot::S1, "S2" => OuterSlot::S2, "S3" => OuterSlot::S3, "S4" => OuterSlot::S4,
        "S5" => OuterSlot::S5, "S6" => OuterSlot::S6, "S7" => OuterSlot::S7, "S8" => OuterSlot::S8,
        _ => return None,
    })
}

fn parse_slide_area_spec(v: &Value) -> Result<crate::simai::slide_tables::SlideAreaSpec, String> {
    let mut target_areas = Vec::new();
    if let Some(arr) = v.get("targetAreas").and_then(Value::as_array) {
        for a in arr {
            target_areas.push(a.as_str().and_then(parse_sensor).ok_or("invalid targetAreas")?);
        }
    }
    let policy = match v.get("policy").and_then(Value::as_str) {
        Some("And") => crate::types::AreaPolicy::And,
        _ => crate::types::AreaPolicy::Or,
    };
    Ok(crate::simai::slide_tables::SlideAreaSpec {
        target_areas,
        policy,
        is_last: v.get("isLast").and_then(Value::as_bool).unwrap_or(false),
        is_skippable: v.get("isSkippable").and_then(Value::as_bool).unwrap_or(false),
        arrow_progress_when_on: v
            .get("arrowProgressWhenOn")
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize,
        arrow_progress_when_finished: v
            .get("arrowProgressWhenFinished")
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize,
    })
}

fn parse_chart_spec_json(content: &str) -> Result<ChartSpec, String> {
    let v: Value = serde_json::from_str(content).map_err(|e| e.to_string())?;
    // Accept either the envelope or the bare ChartSpec.
    let v = v.get("result").cloned().unwrap_or(v);
    let mut spec = ChartSpec::default();
    if let Some(arr) = v.get("taps").and_then(Value::as_array) {
        for n in arr {
            spec.taps.push(crate::chart_loader::TapChartNote {
                timing: TimePoint::from_micros(as_i64(&n["timing"]).unwrap_or(0)),
                slot: n["slot"].as_str().and_then(parse_slot).ok_or("invalid slot")?,
                is_break: n["isBreak"].as_bool().unwrap_or(false),
                is_ex: n["isEX"].as_bool().unwrap_or(false),
                button_queue_index: n["buttonQueueIndex"].as_u64().unwrap_or(0) as usize,
                note_index: n["noteIndex"].as_u64().unwrap_or(0) as usize,
            });
        }
    }
    if let Some(arr) = v.get("holds").and_then(Value::as_array) {
        for n in arr {
            spec.holds.push(crate::chart_loader::HoldChartNote {
                timing: TimePoint::from_micros(as_i64(&n["timing"]).unwrap_or(0)),
                slot: n["slot"].as_str().and_then(parse_slot).ok_or("invalid slot")?,
                length: Duration::from_micros(as_i64(&n["length"]).unwrap_or(0)),
                is_break: n["isBreak"].as_bool().unwrap_or(false),
                is_ex: n["isEX"].as_bool().unwrap_or(false),
                is_touch: n["isTouch"].as_bool().unwrap_or(false),
                is_classic: n["isClassic"].as_bool(),
                button_queue_index: n["buttonQueueIndex"].as_u64().unwrap_or(0) as usize,
                touch_hold_group_id: n["touchHoldGroupId"].as_u64().map(|x| x as usize),
                touch_hold_group_size: n["touchHoldGroupSize"].as_u64().map(|x| x as usize),
                note_index: n["noteIndex"].as_u64().unwrap_or(0) as usize,
            });
        }
    }
    if let Some(arr) = v.get("touches").and_then(Value::as_array) {
        for n in arr {
            spec.touches.push(crate::chart_loader::TouchChartNote {
                timing: TimePoint::from_micros(as_i64(&n["timing"]).unwrap_or(0)),
                sensor_pos: n["sensorPos"].as_str().and_then(parse_sensor).ok_or("invalid sensorPos")?,
                is_break: n["isBreak"].as_bool().unwrap_or(false),
                source_group_id: n["sourceGroupId"].as_u64().map(|x| x as usize),
                source_group_index: n["sourceGroupIndex"].as_u64().map(|x| x as usize),
                source_group_size: n["sourceGroupSize"].as_u64().map(|x| x as usize),
                touch_queue_index: n["touchQueueIndex"].as_u64().unwrap_or(0) as usize,
                touch_group_id: n["touchGroupId"].as_u64().map(|x| x as usize),
                touch_group_size: n["touchGroupSize"].as_u64().map(|x| x as usize),
                note_index: n["noteIndex"].as_u64().unwrap_or(0) as usize,
            });
        }
    }
    if let Some(arr) = v.get("touchHolds").and_then(Value::as_array) {
        for n in arr {
            spec.touch_holds.push(crate::chart_loader::TouchHoldChartNote {
                timing: TimePoint::from_micros(as_i64(&n["timing"]).unwrap_or(0)),
                sensor_pos: n["sensorPos"].as_str().and_then(parse_sensor).ok_or("invalid sensorPos")?,
                length: Duration::from_micros(as_i64(&n["length"]).unwrap_or(0)),
                is_break: n["isBreak"].as_bool().unwrap_or(false),
                is_ex: n["isEX"].as_bool().unwrap_or(false),
                source_group_id: n["sourceGroupId"].as_u64().map(|x| x as usize),
                source_group_index: n["sourceGroupIndex"].as_u64().map(|x| x as usize),
                source_group_size: n["sourceGroupSize"].as_u64().map(|x| x as usize),
                touch_queue_index: n["touchQueueIndex"].as_u64().unwrap_or(0) as usize,
                touch_group_id: n["touchGroupId"].as_u64().map(|x| x as usize),
                touch_group_size: n["touchGroupSize"].as_u64().map(|x| x as usize),
                touch_hold_group_id: n["touchHoldGroupId"].as_u64().map(|x| x as usize),
                touch_hold_group_size: n["touchHoldGroupSize"].as_u64().map(|x| x as usize),
                note_index: n["noteIndex"].as_u64().unwrap_or(0) as usize,
            });
        }
    }
    if let Some(arr) = v.get("slideHeads").and_then(Value::as_array) {
        for n in arr {
            spec.slide_heads.push(crate::chart_loader::SlideHeadChartNote {
                timing: TimePoint::from_micros(as_i64(&n["timing"]).unwrap_or(0)),
                slot: n["slot"].as_str().and_then(parse_slot).ok_or("invalid slot")?,
                is_break: n["isBreak"].as_bool().unwrap_or(false),
                is_ex: n["isEX"].as_bool().unwrap_or(false),
                logical_slide_id: n["logicalSlideId"].as_u64().unwrap_or(0) as usize,
                note_index: n["noteIndex"].as_u64().unwrap_or(0) as usize,
            });
        }
    }
    if let Some(arr) = v.get("slides").and_then(Value::as_array) {
        for n in arr {
            let mut note = crate::chart_loader::SlideChartNote::default();
            note.head_timing = TimePoint::from_micros(as_i64(&n["headTiming"]).unwrap_or(0));
            note.slot = n["slot"].as_str().and_then(parse_slot).ok_or("invalid slot")?;
            note.length = Duration::from_micros(as_i64(&n["length"]).unwrap_or(0));
            note.start_timing = TimePoint::from_micros(as_i64(&n["startTiming"]).unwrap_or(0));
            note.slide_kind = match n["slideKind"].as_str() {
                Some("Wifi") => crate::types::SlideKind::Wifi,
                Some("ConnPart") => crate::types::SlideKind::ConnPart,
                _ => crate::types::SlideKind::Single,
            };
            note.is_classic = n["isClassic"].as_bool().unwrap_or(false);
            note.is_slide_no_head = n["isSlideNoHead"].as_bool().unwrap_or(false);
            note.is_conn_slide = n["isConnSlide"].as_bool().unwrap_or(false);
            note.parent_note_index = n["parentNoteIndex"].as_u64().map(|x| x as usize);
            note.is_group_head = n["isGroupHead"].as_bool().unwrap_or(false);
            note.is_group_end = n["isGroupEnd"].as_bool().unwrap_or(false);
            note.total_judge_queue_len = n["totalJudgeQueueLen"].as_u64().unwrap_or(0) as usize;
            note.track_count = n["trackCount"].as_u64().unwrap_or(1) as usize;
            note.is_break = n["isBreak"].as_bool().unwrap_or(false);
            note.is_ex = n["isEX"].as_bool().unwrap_or(false);
            note.multiple = n["multiple"].as_u64().unwrap_or(1) as usize;
            note.logical_slide_id = n["logicalSlideId"].as_u64().unwrap_or(0) as usize;
            note.note_index = n["noteIndex"].as_u64().unwrap_or(0) as usize;
            note.judge_at = n.get("judgeAt").and_then(Value::as_i64).map(TimePoint::from_micros);
            if let Some(queues) = n.get("judgeQueues").and_then(Value::as_array) {
                for q in queues {
                    let mut queue = Vec::new();
                    if let Some(specs) = q.as_array() {
                        for a in specs {
                            queue.push(parse_slide_area_spec(a)?);
                        }
                    }
                    note.judge_queues.push(queue);
                }
            }
            spec.slides.push(note);
        }
    }
    spec.slide_skipping = v.get("slideSkipping").and_then(Value::as_bool);
    Ok(spec)
}

// ---------------------------------------------------------------------------
// Handler build helpers (used by tests and future C ABI)
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Full GameState encoder (Rust schema)
// ---------------------------------------------------------------------------

fn judge_style_str(s: crate::types::JudgeStyle) -> &'static str {
    match s {
        crate::types::JudgeStyle::Default => "Default",
        crate::types::JudgeStyle::Maji => "Maji",
        crate::types::JudgeStyle::Gachi => "Gachi",
        crate::types::JudgeStyle::Gori => "Gori",
    }
}

fn display_option_str(d: crate::types::JudgeDisplayOption) -> &'static str {
    match d {
        crate::types::JudgeDisplayOption::All => "All",
        crate::types::JudgeDisplayOption::BelowCP => "BelowCP",
        crate::types::JudgeDisplayOption::BelowP => "BelowP",
        crate::types::JudgeDisplayOption::BelowGR => "BelowGR",
        crate::types::JudgeDisplayOption::MissOnly => "MissOnly",
        crate::types::JudgeDisplayOption::Disable => "Disable",
    }
}

fn tap_state_json(s: TapState) -> Value {
    match s {
        TapState::Waiting => json!("Waiting"),
        TapState::Judgeable => json!("Judgeable"),
        TapState::Judged(g) => json!({ "Judged": grade_str(g) }),
        TapState::Ended => json!("Ended"),
    }
}

fn hold_sub_state_json(s: HoldSubState) -> Value {
    match s {
        HoldSubState::HeadWaiting => json!("HeadWaiting"),
        HoldSubState::HeadJudgeable => json!("HeadJudgeable"),
        HoldSubState::HeadJudged(g) => json!({ "HeadJudged": grade_str(g) }),
        HoldSubState::BodyHeld => json!("BodyHeld"),
        HoldSubState::BodyReleased => json!("BodyReleased"),
        HoldSubState::Ended(g) => json!({ "Ended": grade_str(g) }),
    }
}

fn touch_state_json(s: TouchState) -> Value {
    match s {
        TouchState::Waiting => json!("Waiting"),
        TouchState::Judgeable => json!("Judgeable"),
        TouchState::Judged(g) => json!({ "Judged": grade_str(g) }),
        TouchState::Ended => json!("Ended"),
    }
}

fn slide_state_json(s: crate::lifecycle::SlideState) -> Value {
    use crate::lifecycle::SlideState;
    match s {
        SlideState::Waiting => json!("Waiting"),
        SlideState::Active(w) => json!({ "Active": duration_json(w) }),
        SlideState::Judged(g, w, d) => {
            json!({ "Judged": [grade_str(g), duration_json(w), duration_json(d)] })
        }
        SlideState::Ended => json!("Ended"),
    }
}

fn common_params_json(p: &crate::lifecycle::CommonNoteParams) -> Value {
    json!({
        "judgeTiming": time_point_json(p.judge_timing),
        "judgeOffset": duration_json(p.judge_offset),
        "isBreak": p.is_break,
        "isEX": p.is_ex,
        "noteIndex": p.note_index,
    })
}

fn tap_family_json(n: &TapFamilyNote) -> Value {
    match n {
        TapFamilyNote::Tap(t) => json!({
            "kind": "tap",
            "params": common_params_json(&t.params),
            "lane": slot_str(t.lane),
            "state": tap_state_json(t.state),
            "buttonQueueIndex": t.button_queue_index,
        }),
        TapFamilyNote::SlideHead(h) => json!({
            "kind": "slideHead",
            "params": common_params_json(&h.params),
            "lane": slot_str(h.lane),
            "state": tap_state_json(h.state),
            "logicalSlideId": h.logical_slide_id,
            "buttonQueueIndex": h.button_queue_index,
        }),
    }
}

fn hold_note_json(n: &HoldNote) -> Value {
    json!({
        "params": common_params_json(&n.params),
        "start": match n.start {
            crate::lifecycle::HoldStart::Button(z) => json!({ "button": button_str(z) }),
            crate::lifecycle::HoldStart::Sensor(a) => json!({ "sensor": sensor_str(a) }),
        },
        "state": hold_sub_state_json(n.state),
        "length": duration_json(n.length),
        "buttonQueueIndex": n.button_queue_index,
        "headDiff": duration_json(n.head_diff),
        "headGrade": grade_str(n.head_grade),
        "playerReleaseTime": duration_json(n.player_release_time),
        "releaseIgnoreTime": duration_json(n.release_ignore_time),
        "isClassic": n.is_classic,
        "isTouchHold": n.is_touch_hold,
        "touchQueueIndex": n.touch_queue_index,
        "touchGroupId": n.touch_group_id,
        "touchGroupSize": n.touch_group_size,
        "touchHoldGroupId": n.touch_hold_group_id,
        "touchHoldGroupSize": n.touch_hold_group_size,
        "touchHoldGroupTriggered": n.touch_hold_group_triggered,
    })
}

fn touch_note_json(n: &TouchNote) -> Value {
    json!({
        "params": common_params_json(&n.params),
        "state": touch_state_json(n.state),
        "sensorPos": sensor_str(n.sensor_pos),
        "touchQueueIndex": n.touch_queue_index,
        "touchGroupId": n.touch_group_id,
        "touchGroupSize": n.touch_group_size,
    })
}

fn slide_note_json(n: &SlideNote) -> Value {
    json!({
        "params": common_params_json(&n.params),
        "lane": slot_str(n.lane),
        "state": slide_state_json(n.state),
        "length": duration_json(n.length),
        "headTiming": time_point_json(n.head_timing),
        "startTiming": time_point_json(n.start_timing),
        "groupStartTiming": n.group_start_timing.map(time_point_json),
        "slideKind": slide_kind_str(n.slide_kind),
        "isClassic": n.is_classic,
        "isConnSlide": n.is_conn_slide,
        "parentNoteIndex": n.parent_note_index,
        "isGroupPartHead": n.is_group_part_head,
        "isGroupPartEnd": n.is_group_part_end,
        "parentFinished": n.parent_finished,
        "parentPendingFinish": n.parent_pending_finish,
        "initialQueueRemaining": n.initial_queue_remaining,
        "totalJudgeQueueLen": n.total_judge_queue_len,
        "trackCount": n.track_count,
        "isCheckable": n.is_checkable,
        "multiple": n.multiple,
        "judgeQueues": n.judge_queues.iter().map(|q| q.iter().map(|a| json!({
            "targetAreas": a.target_areas.iter().map(|x| sensor_str(*x)).collect::<Vec<_>>(),
            "isLast": a.is_last, "isSkippable": a.is_skippable,
            "wasOn": a.was_on, "wasOff": a.was_off,
            "arrowProgressWhenOn": a.arrow_progress_when_on,
            "arrowProgressWhenFinished": a.arrow_progress_when_finished,
        })).collect::<Vec<_>>()).collect::<Vec<_>>(),
    })
}

fn timed_event_json(e: &TimedInputEvent) -> Value {
    match e {
        TimedInputEvent::ButtonClick(tp, z) => {
            json!({ "buttonClick": { "tp": time_point_json(*tp), "zone": button_str(*z) } })
        }
        TimedInputEvent::ButtonHold(tp, z, down) => {
            json!({ "buttonHold": { "tp": time_point_json(*tp), "zone": button_str(*z), "isDown": down } })
        }
        TimedInputEvent::SensorClick(tp, a) => {
            json!({ "sensorClick": { "tp": time_point_json(*tp), "area": sensor_str(*a) } })
        }
        TimedInputEvent::SensorHold(tp, a, down) => {
            json!({ "sensorHold": { "tp": time_point_json(*tp), "area": sensor_str(*a), "isDown": down } })
        }
    }
}

fn batch_json(b: &TimedInputBatch) -> Value {
    json!({
        "currentTime": time_point_json(b.current_time),
        "events": b.events.iter().map(timed_event_json).collect::<Vec<_>>(),
    })
}

pub fn game_state_json(st: &GameState) -> Value {
    let button_vec = |v: &crate::storage::ButtonVec<bool>| -> Value {
        Value::Array(v.entries().into_iter().map(|(_, b)| json!(b)).collect())
    };
    let sensor_vec = |v: &crate::storage::SensorVec<bool>| -> Value {
        Value::Array(v.entries().into_iter().map(|(_, b)| json!(b)).collect())
    };
    let button_frontiers = Value::Array(
        st.button_queue_frontiers.entries().into_iter().map(|(_, n)| json!(n)).collect(),
    );
    let touch_frontiers = Value::Array(
        st.touch_queue_frontiers.entries().into_iter().map(|(_, n)| json!(n)).collect(),
    );
    let zone_queues = |q: &crate::storage::ButtonVec<ZoneQueue<TapFamilyNote>>| -> Value {
        Value::Array(q.entries().into_iter().map(|(_, zq)| json!({
            "notes": zq.notes.iter().map(tap_family_json).collect::<Vec<_>>(),
            "currentIndex": zq.current_index,
        })).collect())
    };
    let hold_queues = Value::Array(st.hold_queues.entries().into_iter().map(|(_, zq)| json!({
        "notes": zq.notes.iter().map(hold_note_json).collect::<Vec<_>>(),
        "currentIndex": zq.current_index,
    })).collect());
    let touch_hold_queues = Value::Array(st.touch_hold_queues.entries().into_iter().map(|(_, zq)| json!({
        "notes": zq.notes.iter().map(hold_note_json).collect::<Vec<_>>(),
        "currentIndex": zq.current_index,
    })).collect());
    let touch_queues = Value::Array(st.touch_queues.entries().into_iter().map(|(_, zq)| json!({
        "notes": zq.notes.iter().map(touch_note_json).collect::<Vec<_>>(),
        "currentIndex": zq.current_index,
    })).collect());

    json!({
        "currentTime": time_point_json(st.current_time),
        "prevButton": button_vec(&st.prev_button),
        "prevSensor": sensor_vec(&st.prev_sensor),
        "buttonQueueFrontiers": button_frontiers,
        "touchQueueFrontiers": touch_frontiers,
        "tapQueues": zone_queues(&st.tap_queues),
        "holdQueues": hold_queues,
        "touchHoldQueues": touch_hold_queues,
        "touchQueues": touch_queues,
        "slides": st.slides.iter().map(slide_note_json).collect::<Vec<_>>(),
        "activeHolds": st.active_holds.iter().map(|(z, n)| json!({
            "zone": button_str(*z), "note": hold_note_json(n)
        })).collect::<Vec<_>>(),
        "activeTouchHolds": st.active_touch_holds.iter().map(|(a, n)| json!({
            "sensor": sensor_str(*a), "note": hold_note_json(n)
        })).collect::<Vec<_>>(),
        "touchGroupStates": st.touch_group_states.iter().map(|g| json!({
            "groupId": g.group_id, "count": g.count, "size": g.size,
            "grade": grade_str(g.grade), "diff": duration_json(g.diff),
        })).collect::<Vec<_>>(),
        "touchHoldGroupStates": st.touch_hold_group_states.iter().map(|g| json!({
            "groupId": g.group_id,
            "memberNoteIndices": g.member_note_indices,
            "triggeredNoteIndices": g.triggered_note_indices,
        })).collect::<Vec<_>>(),
        "currentBatch": batch_json(&st.current_batch),
        "score": score_json(&st.score),
        "judgeStyle": judge_style_str(st.judge_style),
        "touchPanelOffset": duration_json(st.touch_panel_offset),
        "subdivideSlideJudgeGrade": st.subdivide_slide_judge_grade,
        "noteFastLateDisplay": display_option_str(st.note_fast_late_display),
        "breakFastLateDisplay": display_option_str(st.break_fast_late_display),
    })
}

/// `lnmai_get_game_state_json_by_handle`.
pub fn get_game_state_json_by_handle(handle: u64) -> String {
    let reg = registry().lock().unwrap();
    match reg.get(&handle) {
        Some(Session::Loaded { state, .. }) => ok_json(game_state_json(state)),
        _ => error_json("invalid_runtime_handle", "session is not loaded"),
    }
}

/// `lnmai_step_game_state_handle` (full: returns the whole `GameState`).
pub fn step_game_state_handle(handle: u64, batch_json: &str) -> String {
    let value: Value = match serde_json::from_str(batch_json) {
        Ok(v) => v,
        Err(e) => return error_json("invalid_runtime_json", &e.to_string()),
    };
    let batch = match parse_timed_input_batch(&value) {
        Ok(b) => b,
        Err(e) => return error_json("invalid_runtime_json", &e),
    };
    let mut reg = registry().lock().unwrap();
    match reg.get_mut(&handle) {
        Some(Session::Loaded { state, .. }) => {
            let (next, events, audio, render) = crate::scheduler::step_frame_timed(state, &batch);
            let result = json!({
                "state": game_state_json(&next),
                "events": events.iter().map(judge_event_json).collect::<Vec<_>>(),
                "audioCommands": audio.iter().map(audio_cmd_json).collect::<Vec<_>>(),
                "renderCommands": render.iter().map(render_cmd_json).collect::<Vec<_>>(),
            });
            *state = next;
            ok_json(result)
        }
        _ => error_json("invalid_runtime_handle", "session is not loaded"),
    }
}

#[allow(dead_code)]
fn _unused(_: &TapFamilyNote, _: TapState, _: &TouchNote, _: TouchState, _: &HoldNote, _: &SlideNote, _: usize, _: usize) {}

#[allow(dead_code)]
fn _unused_grade_index(g: JudgeGrade) -> usize {
    grade_index(g)
}

#[cfg(test)]
mod tests {
    use super::*;

    const MAIDATA: &str = "&first=0\n&inote_1=\n(120)\n1,2,3,\n";

    fn parse(s: &str) -> Value {
        serde_json::from_str(s).unwrap()
    }

    #[test]
    fn version_ok() {
        let v = parse(&ffi_version_json());
        assert_eq!(v["ok"], true);
        assert_eq!(v["result"]["abiVersion"], 1);
    }

    #[test]
    fn parse_lowered_ok() {
        let v = parse(&parse_lowered_chart_json(MAIDATA, 1));
        assert_eq!(v["ok"], true);
        assert_eq!(v["result"]["taps"].as_array().unwrap().len(), 3);
        assert_eq!(v["result"]["taps"][0]["slot"], "S1");
    }

    #[test]
    fn parse_error_envelope() {
        let v = parse(&parse_lowered_chart_json("&inote_1=\n(120)\n1-9,", 1));
        assert_eq!(v["ok"], false);
        assert_eq!(v["error"]["code"], "parse_error");
    }

    #[test]
    fn session_roundtrip_and_step() {
        let created = parse(&create_empty_session_handle());
        let handle = created["result"]["handle"].as_u64().unwrap();
        assert_eq!(created["result"]["state"], "empty");

        let loaded = parse(&load_chart_into_session_from_text(handle, MAIDATA, 1));
        assert_eq!(loaded["result"]["state"], "loaded");
        assert_eq!(loaded["result"]["summary"]["tapCount"], 3);

        let lowered = parse(&get_lowered_chart_json_by_handle(handle));
        assert_eq!(lowered["ok"], true);

        // Step with a click at time 0 on K1 → first tap judged Perfect.
        let batch = r#"{"currentTime":0,"events":[{"tag":"buttonClick","tp":0,"zone":"K1"}]}"#;
        let stepped = parse(&step_game_state_handle_light(handle, batch));
        assert_eq!(stepped["ok"], true);
        assert_eq!(stepped["result"]["events"].as_array().unwrap().len(), 1);
        assert_eq!(stepped["result"]["events"][0]["grade"], "Perfect");
        assert_eq!(stepped["result"]["score"]["combo"], 1);
        assert_eq!(stepped["result"]["score"]["counts"]["tapCount"]["Perfect"], 1);

        let unloaded = parse(&unload_chart_from_session(handle));
        assert_eq!(unloaded["result"]["state"], "empty");
        let freed = parse(&free_game_state_handle(handle));
        assert_eq!(freed["result"]["freed"], true);
    }

    #[test]
    fn step_before_load_is_error() {
        let created = parse(&create_empty_session_handle());
        let handle = created["result"]["handle"].as_u64().unwrap();
        let bad = parse(&step_game_state_handle_light(handle, "{}"));
        assert_eq!(bad["ok"], false);
        assert_eq!(bad["error"]["code"], "invalid_runtime_handle");
    }

    #[test]
    fn full_state_roundtrip() {
        let created = parse(&create_empty_session_handle());
        let handle = created["result"]["handle"].as_u64().unwrap();
        parse(&load_chart_into_session_from_text(handle, MAIDATA, 1));

        let batch = r#"{"currentTime":0,"events":[{"buttonClick":{"tp":0,"zone":"K1"}}]}"#;
        let stepped = parse(&step_game_state_handle(handle, batch));
        assert_eq!(stepped["ok"], true);
        assert_eq!(stepped["result"]["events"][0]["grade"], "Perfect");
        assert_eq!(stepped["result"]["state"]["currentTime"], 0);
        assert_eq!(stepped["result"]["state"]["score"]["combo"], 1);
        assert_eq!(stepped["result"]["state"]["tapQueues"].as_array().unwrap().len(), 8);
        assert_eq!(stepped["result"]["state"]["touchQueues"].as_array().unwrap().len(), 33);

        let state = parse(&get_game_state_json_by_handle(handle));
        assert_eq!(state["ok"], true);
        assert_eq!(state["result"]["score"]["counts"]["tapCount"]["Perfect"], 1);
        assert!(state["result"]["slides"].is_array());

        parse(&free_game_state_handle(handle));
    }
}

