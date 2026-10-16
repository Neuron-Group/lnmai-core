import Verification.RuntimeScore
import Verification.JudgmentAdapter

/-!
Compose the runtime-used exact fast/late kernel into event and trace scoring.
The whole clamping formula is no longer an adapter assumption. Arithmetic on
score counters, grade-count storage and the unextracted runtime facade remain
interpreted by RuntimeScoreProof; this is not a BigInt or allocation proof.
-/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.RuntimeScoreJudgmentsProof
open Bridge LnmaiCore RuntimeScoreProof
open private foldEventIntoScore foldEventsIntoScore from LnmaiCore.Scheduler

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → Int) (encode : Int → I)
  (h : JudgmentAdapterProof.IntegerAgrees inst ops decode)
  (hencode : ∀ value, decode (encode value) = value)
include h hencode

def fastLate (event : JudgeEvent) (display : JudgeDisplayOption) : Result (Bool × Bool) :=
  judgment_adapter.count_fast_late inst (ofLnmJudgeGrade event.grade)
    (encode event.diff.toMicros) (ofLnmDisplay display) ops

theorem fast_late_equiv (event : JudgeEvent) (display : JudgeDisplayOption) :
    fastLate inst ops encode event display =
      ok (Score.countFastLate event.grade event.diff display) := by
  have hdiff : Duration.fromMicros event.diff.toMicros = event.diff := by
    apply Duration.toMicros_injective
    rfl
  simpa only [fastLate, hencode, toLnmJudgeGrade_ofLnmJudgeGrade,
    display_roundtrip, hdiff] using
    JudgmentAdapterProof.count_fast_late_equiv inst ops decode h
      (ofLnmJudgeGrade event.grade) (encode event.diff.toMicros) (ofLnmDisplay display)

def foldEvent (noteDisplay breakDisplay : JudgeDisplayOption)
    (state : ScoreState) (event : JudgeEvent) : Result ScoreState := do
  if event.kind == .Hold && event.phase == .head then ok state else
    let multiple := max event.multiple 1
    let combo ← comboAdapter state multiple event.grade
    let deltas ← eventDeltasAdapter event multiple
    let display := if event.isBreak || event.kind == .Break then breakDisplay else noteDisplay
    let flags ← fastLate inst ops encode event display
    ok { state with
      combo := combo.combo
      pCombo := combo.pCombo
      cPCombo := combo.cPCombo
      earnedBase := state.earnedBase + deltas.1
      earnedExtra := state.earnedExtra + deltas.2.1
      earnedClassicExtra := state.earnedClassicExtra + deltas.2.2.1
      lostBase := state.lostBase + deltas.2.2.2.1
      lostExtra := state.lostExtra + deltas.2.2.2.2.1
      lostClassicExtra := state.lostClassicExtra + deltas.2.2.2.2.2
      dxScore := combo.dXScoreLost
      fastCount := state.fastCount + if flags.1 then multiple else 0
      lateCount := state.lateCount + if flags.2 then multiple else 0
      counts := updateCounts state.counts event multiple }

theorem fold_event_equiv (noteDisplay breakDisplay : JudgeDisplayOption)
    (state : ScoreState) (event : JudgeEvent) :
    foldEvent inst ops encode noteDisplay breakDisplay state event =
      ok (foldEventIntoScore noteDisplay breakDisplay state event) := by
  simp only [foldEvent, foldEventIntoScore, Nat.max_comm event.multiple 1]
  split
  · rfl
  · simp only [combo_adapter_equiv, event_deltas_adapter_equiv,
      fast_late_equiv inst ops decode encode h hencode, bind_tc_ok, updateCounts]
    rfl

def foldEvents (noteDisplay breakDisplay : JudgeDisplayOption) :
    ScoreState → List JudgeEvent → Result ScoreState
  | state, [] => ok state
  | state, event :: events => do
      let next ← foldEvent inst ops encode noteDisplay breakDisplay state event
      foldEvents noteDisplay breakDisplay next events

theorem fold_events_equiv (noteDisplay breakDisplay : JudgeDisplayOption)
    (state : ScoreState) (events : List JudgeEvent) :
    foldEvents inst ops encode noteDisplay breakDisplay state events =
      ok (foldEventsIntoScore noteDisplay breakDisplay state events) := by
  induction events generalizing state with
  | nil => rfl
  | cons event events ih =>
      simp only [foldEvents, fold_event_equiv inst ops decode encode h hencode,
        bind_tc_ok, ih, foldEventsIntoScore]

def scoreTrace (noteDisplay breakDisplay : JudgeDisplayOption) :
    ScoreState → List JudgeEvent → Result (List ScoreState)
  | _, [] => ok []
  | state, event :: events => do
      let next ← foldEvent inst ops encode noteDisplay breakDisplay state event
      let rest ← scoreTrace noteDisplay breakDisplay next events
      ok (next :: rest)

/-- Preserves every intermediate score, including score-neutral hold heads. -/
theorem score_trace_equiv (noteDisplay breakDisplay : JudgeDisplayOption)
    (state : ScoreState) (events : List JudgeEvent) :
    scoreTrace inst ops encode noteDisplay breakDisplay state events =
      ok (referenceScoreTrace noteDisplay breakDisplay state events) := by
  induction events generalizing state with
  | nil => rfl
  | cons event events ih =>
      simp only [scoreTrace, fold_event_equiv inst ops decode encode h hencode,
        bind_tc_ok, ih, referenceScoreTrace]

end Verification.RuntimeScoreJudgmentsProof
