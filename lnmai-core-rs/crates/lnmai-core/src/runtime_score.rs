//! Runtime score state with per-note-type judge counts.
//!
//! Extends the verified numeric score fields with `NoteTypeJudgeCounts` (a
//! grade → count table per note type), matching `LnmaiCore/Types.lean`.
//! Counts are stored as fixed arrays indexed by the Lean `JudgeGrade` order.

use crate::score::{base_score, count_fast_late, score_break, score_non_break, update_combo};
use crate::time::Duration;
use crate::types::{JudgeDisplayOption, JudgeEvent, JudgeEventKind, JudgeGrade, NoteType};

/// Index of a grade in the canonical 15-tier order.
pub fn grade_index(g: JudgeGrade) -> usize {
    match g {
        JudgeGrade::Miss => 0,
        JudgeGrade::LateGood => 1,
        JudgeGrade::LateGreat3rd => 2,
        JudgeGrade::LateGreat2nd => 3,
        JudgeGrade::LateGreat => 4,
        JudgeGrade::LatePerfect3rd => 5,
        JudgeGrade::LatePerfect2nd => 6,
        JudgeGrade::Perfect => 7,
        JudgeGrade::FastPerfect2nd => 8,
        JudgeGrade::FastPerfect3rd => 9,
        JudgeGrade::FastGreat => 10,
        JudgeGrade::FastGreat2nd => 11,
        JudgeGrade::FastGreat3rd => 12,
        JudgeGrade::FastGood => 13,
        JudgeGrade::TooFast => 14,
    }
}

const NUM_GRADES: usize = 15;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoteTypeJudgeCounts {
    pub tap: [usize; NUM_GRADES],
    pub hold: [usize; NUM_GRADES],
    pub slide: [usize; NUM_GRADES],
    pub touch: [usize; NUM_GRADES],
    pub break_: [usize; NUM_GRADES],
}

impl Default for NoteTypeJudgeCounts {
    fn default() -> Self {
        NoteTypeJudgeCounts {
            tap: [0; NUM_GRADES],
            hold: [0; NUM_GRADES],
            slide: [0; NUM_GRADES],
            touch: [0; NUM_GRADES],
            break_: [0; NUM_GRADES],
        }
    }
}

impl NoteTypeJudgeCounts {
    pub fn grade_count(&self, grade: JudgeGrade) -> usize {
        let i = grade_index(grade);
        self.tap[i] + self.hold[i] + self.slide[i] + self.touch[i] + self.break_[i]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeScoreState {
    pub combo: usize,
    pub p_combo: usize,
    pub c_p_combo: usize,
    pub total_base: usize,
    pub total_extra: usize,
    pub earned_base: usize,
    pub earned_extra: usize,
    pub earned_classic_extra: usize,
    pub lost_base: usize,
    pub lost_extra: usize,
    pub lost_classic_extra: usize,
    pub dx_score: i64,
    pub max_dx_score: usize,
    pub fast_count: usize,
    pub late_count: usize,
    pub counts: NoteTypeJudgeCounts,
}

impl Default for RuntimeScoreState {
    fn default() -> Self {
        RuntimeScoreState {
            combo: 0,
            p_combo: 0,
            c_p_combo: 0,
            total_base: 0,
            total_extra: 0,
            earned_base: 0,
            earned_extra: 0,
            earned_classic_extra: 0,
            lost_base: 0,
            lost_extra: 0,
            lost_classic_extra: 0,
            dx_score: 0,
            max_dx_score: 0,
            fast_count: 0,
            late_count: 0,
            counts: NoteTypeJudgeCounts::default(),
        }
    }
}

fn event_score_note_type(evt: &JudgeEvent) -> NoteType {
    if evt.is_break || evt.kind == JudgeEventKind::Break {
        NoteType::Break
    } else {
        match evt.kind {
            JudgeEventKind::Tap => NoteType::Tap,
            JudgeEventKind::Hold => NoteType::Hold,
            JudgeEventKind::Slide => NoteType::Slide,
            JudgeEventKind::Touch => NoteType::Touch,
            JudgeEventKind::Break => NoteType::Break,
        }
    }
}

/// Returns `(earnedBase, earnedExtra, earnedClassicExtra, lostBase, lostExtra, lostClassicExtra)`.
fn event_score_deltas(evt: &JudgeEvent, multiple: usize) -> (usize, usize, usize, usize, usize, usize) {
    match event_score_note_type(evt) {
        NoteType::Break => {
            let (eb, ee, ece, lb, le, lce) = score_break(evt.grade, multiple as u32);
            (eb as usize, ee as usize, ece as usize, lb as usize, le as usize, lce as usize)
        }
        kind => {
            let (earned, lost) = score_non_break(base_score(kind), evt.grade, multiple as u32);
            (earned as usize, 0, 0, lost as usize, 0, 0)
        }
    }
}

/// `foldEventIntoScore`.
pub fn fold_event_into_score(
    note_display: JudgeDisplayOption,
    break_display: JudgeDisplayOption,
    s: &RuntimeScoreState,
    evt: &JudgeEvent,
) -> RuntimeScoreState {
    let multiple = evt.multiple.max(1);
    let combo_delta = update_combo(
        s.combo as u32,
        s.p_combo as u32,
        s.c_p_combo as u32,
        s.dx_score,
        evt.grade,
        multiple as u32,
    );
    let (eb, ee, ece, lb, le, lce) = event_score_deltas(evt, multiple);
    let display = if evt.is_break || evt.kind == JudgeEventKind::Break {
        break_display
    } else {
        note_display
    };
    let (is_fast, is_late) = count_fast_late(evt.grade, evt.diff, display);
    let gi = grade_index(evt.grade);
    let mut counts = s.counts.clone();
    if evt.is_break || evt.kind == JudgeEventKind::Break {
        counts.break_[gi] += multiple;
    } else {
        match evt.kind {
            JudgeEventKind::Tap => counts.tap[gi] += multiple,
            JudgeEventKind::Hold => counts.hold[gi] += multiple,
            JudgeEventKind::Slide => counts.slide[gi] += multiple,
            JudgeEventKind::Touch => counts.touch[gi] += multiple,
            JudgeEventKind::Break => counts.break_[gi] += multiple,
        }
    }
    RuntimeScoreState {
        combo: combo_delta.combo as usize,
        p_combo: combo_delta.p_combo as usize,
        c_p_combo: combo_delta.c_p_combo as usize,
        earned_base: s.earned_base + eb,
        earned_extra: s.earned_extra + ee,
        earned_classic_extra: s.earned_classic_extra + ece,
        lost_base: s.lost_base + lb,
        lost_extra: s.lost_extra + le,
        lost_classic_extra: s.lost_classic_extra + lce,
        dx_score: combo_delta.dx_score_lost,
        fast_count: s.fast_count + if is_fast { multiple } else { 0 },
        late_count: s.late_count + if is_late { multiple } else { 0 },
        counts,
        ..s.clone()
    }
}

/// `foldEventsIntoScore`.
pub fn fold_events_into_score(
    note_display: JudgeDisplayOption,
    break_display: JudgeDisplayOption,
    s: &RuntimeScoreState,
    events: &[JudgeEvent],
) -> RuntimeScoreState {
    let mut acc = s.clone();
    for evt in events {
        acc = fold_event_into_score(note_display, break_display, &acc, evt);
    }
    acc
}

#[allow(dead_code)]
fn _unused(_d: Duration) {}
