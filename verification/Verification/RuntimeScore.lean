import Verification.AdapterScore
import Verification.AdapterJudgments
import LnmaiCore.Scheduler

namespace Verification.RuntimeScoreProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open Bridge Adapters

/- A mathematical model of runtime_score.rs after decoding its unbounded
   integers and grade-count arrays. Finite coefficients and fast/late flags
   below are computed by the actual extracted Rust functions. These theorems
   compose those calls with the reference's private event fold; they do not
   assert extraction of runtime_score.rs or bigint/array refinement. -/

open private LnmaiCore.Scheduler.eventScoreNoteType LnmaiCore.Scheduler.eventScoreDeltas
  LnmaiCore.Scheduler.foldEventIntoScore LnmaiCore.Scheduler.foldEventsIntoScore
  from LnmaiCore.Scheduler

def ofLnmDisplay : LnmaiCore.JudgeDisplayOption → types.JudgeDisplayOption
  | .All => .All
  | .BelowCP => .BelowCP
  | .BelowP => .BelowP
  | .BelowGR => .BelowGR
  | .MissOnly => .MissOnly
  | .Disable => .Disable

@[simp] theorem display_roundtrip (d : LnmaiCore.JudgeDisplayOption) :
    toLnmDisplayOption (ofLnmDisplay d) = d := by cases d <;> rfl

def nonBreakAdapter (base multiple : Nat) (grade : LnmaiCore.JudgeGrade) :
    Result (Nat × Nat) := do
  let coefficient ← score.score_non_break 10#u32 (ofLnmJudgeGrade grade) 1#u32
  let total := base * multiple
  let earned := total * coefficient.1.val / 10
  ok (earned, total - earned)

theorem non_break_adapter_equiv (base multiple : Nat) (grade : LnmaiCore.JudgeGrade) :
    nonBreakAdapter base multiple grade =
      ok (LnmaiCore.Score.scoreNonBreak base grade multiple) := by
  obtain ⟨coefficient, hc, hv⟩ :=
    non_break_coefficient_adapter base multiple (ofLnmJudgeGrade grade)
  simpa only [nonBreakAdapter, hc, bind_tc_ok, toLnmJudgeGrade_ofLnmJudgeGrade] using
    congrArg Result.ok hv

def breakAdapter (multiple : Nat) (grade : LnmaiCore.JudgeGrade) :
    Result (Nat × Nat × Nat × Nat × Nat × Nat) := do
  let c ← score.score_break (ofLnmJudgeGrade grade) 1#u32
  ok (multiple * c.1.val, multiple * c.2.1.val, multiple * c.2.2.1.val,
    multiple * c.2.2.2.1.val, multiple * c.2.2.2.2.1.val, multiple * c.2.2.2.2.2.val)

theorem break_adapter_equiv (multiple : Nat) (grade : LnmaiCore.JudgeGrade) :
    breakAdapter multiple grade = ok (LnmaiCore.Score.scoreBreak grade multiple) := by
  obtain ⟨coefficient, hc, hv⟩ :=
    break_coefficient_adapter multiple (ofLnmJudgeGrade grade)
  simpa only [breakAdapter, hc, bind_tc_ok, toLnmJudgeGrade_ofLnmJudgeGrade] using
    congrArg Result.ok hv

def comboAdapter (s : LnmaiCore.ScoreState) (multiple : Nat)
    (grade : LnmaiCore.JudgeGrade) : Result LnmaiCore.Score.ComboDelta := do
  let c ← score.update_combo 0#u32 0#u32 0#u32 0#i64 (ofLnmJudgeGrade grade) 1#u32
  ok (comboFromCoefficients s.combo s.pCombo s.cPCombo s.dxScore multiple c)

theorem combo_adapter_equiv (s : LnmaiCore.ScoreState) (multiple : Nat)
    (grade : LnmaiCore.JudgeGrade) :
    comboAdapter s multiple grade =
      ok (LnmaiCore.Score.updateCombo s.combo s.pCombo s.cPCombo s.dxScore grade multiple) := by
  obtain ⟨coefficient, hc, hv⟩ :=
    combo_coefficient_adapter s.combo s.pCombo s.cPCombo multiple s.dxScore
      (ofLnmJudgeGrade grade)
  simpa only [comboAdapter, hc, bind_tc_ok, toLnmJudgeGrade_ofLnmJudgeGrade] using
    congrArg Result.ok hv

def eventNoteType (evt : LnmaiCore.JudgeEvent) : LnmaiCore.NoteType :=
  if evt.isBreak || evt.kind == .Break then .Break else
    match evt.kind with
    | .Tap => .Tap
    | .Hold => .Hold
    | .Slide => .Slide
    | .Touch => .Touch
    | .Break => .Break

theorem event_note_type_equiv (evt : LnmaiCore.JudgeEvent) :
    eventNoteType evt = LnmaiCore.Scheduler.eventScoreNoteType evt := rfl

def eventDeltasAdapter (evt : LnmaiCore.JudgeEvent) (multiple : Nat) :
    Result (Nat × Nat × Nat × Nat × Nat × Nat) := do
  match eventNoteType evt with
  | .Break => breakAdapter multiple evt.grade
  | kind =>
      let d ← nonBreakAdapter (LnmaiCore.Score.baseScore kind) multiple evt.grade
      ok (d.1, 0, 0, d.2, 0, 0)

theorem event_deltas_adapter_equiv (evt : LnmaiCore.JudgeEvent) (multiple : Nat) :
    eventDeltasAdapter evt multiple = ok (LnmaiCore.Scheduler.eventScoreDeltas evt multiple) := by
  have hbase (kind : LnmaiCore.NoteType) :
      LnmaiCore.Score.baseScore kind = LnmaiCore.NoteType.baseScore kind := by
    cases kind <;> rfl
  simp only [eventDeltasAdapter, event_note_type_equiv, LnmaiCore.Scheduler.eventScoreDeltas]
  cases LnmaiCore.Scheduler.eventScoreNoteType evt <;>
    simp only [non_break_adapter_equiv, break_adapter_equiv, bind_tc_ok, hbase]

def updateCounts (counts : LnmaiCore.NoteTypeJudgeCounts) (evt : LnmaiCore.JudgeEvent)
    (multiple : Nat) : LnmaiCore.NoteTypeJudgeCounts :=
  let bump (xs : LnmaiCore.JudgeCounts) :=
    fun g => if g == evt.grade then xs g + multiple else xs g
  if evt.isBreak || evt.kind == .Break then
    { counts with breakCount := bump counts.breakCount }
  else
    match evt.kind with
    | .Tap => { counts with tapCount := bump counts.tapCount }
    | .Hold => { counts with holdCount := bump counts.holdCount }
    | .Slide => { counts with slideCount := bump counts.slideCount }
    | .Touch => { counts with touchCount := bump counts.touchCount }
    | .Break => { counts with breakCount := bump counts.breakCount }

def DifferenceConversion (encode : Int → time.Duration) : Prop :=
  ∀ value, (encode value).micros.val = bounded value 1000000

def fastLateAdapter (encode : Int → time.Duration) (evt : LnmaiCore.JudgeEvent)
    (display : LnmaiCore.JudgeDisplayOption) : Result (Bool × Bool) :=
  score.count_fast_late (ofLnmJudgeGrade evt.grade) (encode evt.diff.toMicros)
    (ofLnmDisplay display)

theorem fast_late_adapter_equiv (encode : Int → time.Duration)
    (hencode : DifferenceConversion encode) (evt : LnmaiCore.JudgeEvent)
    (display : LnmaiCore.JudgeDisplayOption) :
    fastLateAdapter encode evt display =
      ok (LnmaiCore.Score.countFastLate evt.grade evt.diff display) := by
  have hdiff : LnmaiCore.Duration.fromMicros evt.diff.toMicros = evt.diff := by
    apply LnmaiCore.Duration.toMicros_injective
    rfl
  simpa only [fastLateAdapter, toLnmJudgeGrade_ofLnmJudgeGrade, display_roundtrip, hdiff] using
    count_fast_late_adapter_equiv (ofLnmJudgeGrade evt.grade) evt.diff.toMicros
      (encode evt.diff.toMicros) (ofLnmDisplay display) (hencode evt.diff.toMicros)

def foldEventAdapter (encode : Int → time.Duration)
    (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption)
    (s : LnmaiCore.ScoreState) (evt : LnmaiCore.JudgeEvent) : Result LnmaiCore.ScoreState := do
  if evt.kind == .Hold && evt.phase == .head then ok s else
    let multiple := max evt.multiple 1
    let combo ← comboAdapter s multiple evt.grade
    let deltas ← eventDeltasAdapter evt multiple
    let display := if evt.isBreak || evt.kind == .Break then breakDisplay else noteDisplay
    let flags ← fastLateAdapter encode evt display
    ok { s with
      combo := combo.combo
      pCombo := combo.pCombo
      cPCombo := combo.cPCombo
      earnedBase := s.earnedBase + deltas.1
      earnedExtra := s.earnedExtra + deltas.2.1
      earnedClassicExtra := s.earnedClassicExtra + deltas.2.2.1
      lostBase := s.lostBase + deltas.2.2.2.1
      lostExtra := s.lostExtra + deltas.2.2.2.2.1
      lostClassicExtra := s.lostClassicExtra + deltas.2.2.2.2.2
      dxScore := combo.dXScoreLost
      fastCount := s.fastCount + if flags.1 then multiple else 0
      lateCount := s.lateCount + if flags.2 then multiple else 0
      counts := updateCounts s.counts evt multiple }

theorem fold_event_adapter_equiv (encode : Int → time.Duration)
    (hencode : DifferenceConversion encode)
    (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption)
    (s : LnmaiCore.ScoreState) (evt : LnmaiCore.JudgeEvent) :
    foldEventAdapter encode noteDisplay breakDisplay s evt =
      ok (LnmaiCore.Scheduler.foldEventIntoScore noteDisplay breakDisplay s evt) := by
  simp only [foldEventAdapter, LnmaiCore.Scheduler.foldEventIntoScore, Nat.max_comm evt.multiple 1]
  split
  · rfl
  · simp only [combo_adapter_equiv, event_deltas_adapter_equiv,
      fast_late_adapter_equiv encode hencode, bind_tc_ok, updateCounts]
    rfl

def foldEventsAdapter (encode : Int → time.Duration)
    (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption) :
    LnmaiCore.ScoreState → List LnmaiCore.JudgeEvent → Result LnmaiCore.ScoreState
  | s, [] => ok s
  | s, evt :: rest => do
      let next ← foldEventAdapter encode noteDisplay breakDisplay s evt
      foldEventsAdapter encode noteDisplay breakDisplay next rest

theorem fold_events_adapter_equiv (encode : Int → time.Duration)
    (hencode : DifferenceConversion encode)
    (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption)
    (s : LnmaiCore.ScoreState) (events : List LnmaiCore.JudgeEvent) :
    foldEventsAdapter encode noteDisplay breakDisplay s events =
      ok (LnmaiCore.Scheduler.foldEventsIntoScore noteDisplay breakDisplay s events) := by
  induction events generalizing s with
  | nil => rfl
  | cons evt rest ih =>
      simp only [foldEventsAdapter, fold_event_adapter_equiv encode hencode,
        bind_tc_ok, ih, LnmaiCore.Scheduler.foldEventsIntoScore]

def scoreTraceAdapter (encode : Int → time.Duration)
    (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption) :
    LnmaiCore.ScoreState → List LnmaiCore.JudgeEvent → Result (List LnmaiCore.ScoreState)
  | _, [] => ok []
  | s, evt :: rest => do
      let next ← foldEventAdapter encode noteDisplay breakDisplay s evt
      let tail ← scoreTraceAdapter encode noteDisplay breakDisplay next rest
      ok (next :: tail)

def referenceScoreTrace (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption) :
    LnmaiCore.ScoreState → List LnmaiCore.JudgeEvent → List LnmaiCore.ScoreState
  | _, [] => []
  | s, evt :: rest =>
      let next := LnmaiCore.Scheduler.foldEventIntoScore noteDisplay breakDisplay s evt
      next :: referenceScoreTrace noteDisplay breakDisplay next rest

theorem score_trace_adapter_equiv (encode : Int → time.Duration)
    (hencode : DifferenceConversion encode)
    (noteDisplay breakDisplay : LnmaiCore.JudgeDisplayOption)
    (s : LnmaiCore.ScoreState) (events : List LnmaiCore.JudgeEvent) :
    scoreTraceAdapter encode noteDisplay breakDisplay s events =
      ok (referenceScoreTrace noteDisplay breakDisplay s events) := by
  induction events generalizing s with
  | nil => rfl
  | cons evt rest ih =>
      simp only [scoreTraceAdapter, fold_event_adapter_equiv encode hencode,
        bind_tc_ok, ih, referenceScoreTrace]

end Verification.RuntimeScoreProof
