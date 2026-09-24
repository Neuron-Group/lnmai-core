//! Host-facing command events emitted by the runtime.
//!
//! Mirrors the `AudioCommand` / `RenderCommand` inductives in
//! `LnmaiCore/Types.lean`.

use crate::areas::{OuterSlot, SensorArea};
use crate::time::{Duration, TimePoint};
use crate::types::{JudgeEventKind, JudgeGrade, RuntimePos};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AudioCommand {
    PlayJudgeSfx {
        kind: JudgeEventKind,
        grade: JudgeGrade,
        is_break: bool,
        at_time: TimePoint,
        note_index: usize,
    },
    PlaySlideCue {
        note_index: usize,
        track_index: usize,
        is_break: bool,
        at_time: TimePoint,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderCommand {
    ShowJudgeResult {
        kind: JudgeEventKind,
        grade: JudgeGrade,
        is_break: bool,
        diff: Duration,
        note_index: usize,
    },
    UpdateSlideProgress {
        note_index: usize,
        remaining: usize,
    },
    UpdateSlideTrackProgress {
        note_index: usize,
        track_index: usize,
        remaining: usize,
    },
    HideAllSlideBars {
        note_index: usize,
    },
    HideSlideBars {
        note_index: usize,
        end_index: usize,
    },
    HideSlideTrackBars {
        note_index: usize,
        track_index: usize,
        end_index: usize,
    },
}

/// Convenience constructors matching the Lean `JudgeEvent` shape.
#[allow(dead_code)]
pub(crate) fn judge_event(
    kind: JudgeEventKind,
    grade: JudgeGrade,
    diff: Duration,
    position: RuntimePos,
    note_index: usize,
    is_break: bool,
) -> crate::types::JudgeEvent {
    crate::types::JudgeEvent {
        kind,
        grade,
        diff,
        position,
        note_index,
        is_break,
        multiple: 1,
    }
}

#[allow(dead_code)]
pub(crate) fn button_pos(slot: OuterSlot) -> RuntimePos {
    RuntimePos::Button(slot.to_button_zone())
}

#[allow(dead_code)]
pub(crate) fn sensor_pos(area: SensorArea) -> RuntimePos {
    RuntimePos::Sensor(area)
}
