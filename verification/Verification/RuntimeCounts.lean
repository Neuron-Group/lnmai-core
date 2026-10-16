import Verification.GradeCounts
import Verification.RuntimeScore

namespace Verification.RuntimeCountsProof
open Aeneas Aeneas.Std Result lnmai_core_verify Bridge GradeCountsProof

/- Representation of the five runtime arrays. Updates below call the actual
   extracted get_count/set_count functions through GradeCountsProof.bump.
   Arithmetic decoding and the unextracted caller remain explicit obligations. -/

structure Tables (N : Type) where
  tap : Std.Array N 15#usize
  hold : Std.Array N 15#usize
  slide : Std.Array N 15#usize
  touch : Std.Array N 15#usize
  breakCount : Std.Array N 15#usize

def decodeTables {N : Type} (natural : N → Nat) (tables : Tables N) :
    LnmaiCore.NoteTypeJudgeCounts :=
  { tapCount := decode natural tables.tap
    holdCount := decode natural tables.hold
    slideCount := decode natural tables.slide
    touchCount := decode natural tables.touch
    breakCount := decode natural tables.breakCount }

def select {N : Type} (tables : Tables N) : LnmaiCore.NoteType → Std.Array N 15#usize
  | .Tap => tables.tap
  | .Hold => tables.hold
  | .Slide => tables.slide
  | .Touch => tables.touch
  | .Break => tables.breakCount

def replace {N : Type} (tables : Tables N) (kind : LnmaiCore.NoteType)
    (counts : Std.Array N 15#usize) : Tables N :=
  match kind with
  | .Tap => { tables with tap := counts }
  | .Hold => { tables with hold := counts }
  | .Slide => { tables with slide := counts }
  | .Touch => { tables with touch := counts }
  | .Break => { tables with breakCount := counts }

def selectCounts (tables : LnmaiCore.NoteTypeJudgeCounts) :
    LnmaiCore.NoteType → LnmaiCore.JudgeCounts
  | .Tap => tables.tapCount
  | .Hold => tables.holdCount
  | .Slide => tables.slideCount
  | .Touch => tables.touchCount
  | .Break => tables.breakCount

def replaceCounts (tables : LnmaiCore.NoteTypeJudgeCounts) (kind : LnmaiCore.NoteType)
    (counts : LnmaiCore.JudgeCounts) : LnmaiCore.NoteTypeJudgeCounts :=
  match kind with
  | .Tap => { tables with tapCount := counts }
  | .Hold => { tables with holdCount := counts }
  | .Slide => { tables with slideCount := counts }
  | .Touch => { tables with touchCount := counts }
  | .Break => { tables with breakCount := counts }

theorem decode_select {N : Type} (natural : N → Nat) (tables : Tables N)
    (kind : LnmaiCore.NoteType) :
    decode natural (select tables kind) = selectCounts (decodeTables natural tables) kind := by
  cases kind <;> rfl

theorem decode_replace {N : Type} (natural : N → Nat) (tables : Tables N)
    (kind : LnmaiCore.NoteType) (counts : Std.Array N 15#usize) :
    decodeTables natural (replace tables kind counts) =
      replaceCounts (decodeTables natural tables) kind (decode natural counts) := by
  cases kind <;> rfl

def update {N : Type} (add : N → N → N) (tables : Tables N)
    (evt : LnmaiCore.JudgeEvent) (multiple : N) : Result (Tables N) := do
  let kind := RuntimeScoreProof.eventNoteType evt
  let next ← bump add (select tables kind) (ofLnmJudgeGrade evt.grade) multiple
  ok (replace tables kind next)

theorem route_equiv (counts : LnmaiCore.NoteTypeJudgeCounts)
    (evt : LnmaiCore.JudgeEvent) (multiple : Nat) :
    replaceCounts counts (RuntimeScoreProof.eventNoteType evt)
      (fun g => if g == evt.grade then
        selectCounts counts (RuntimeScoreProof.eventNoteType evt) g + multiple
        else selectCounts counts (RuntimeScoreProof.eventNoteType evt) g) =
      RuntimeScoreProof.updateCounts counts evt multiple := by
  cases hk : evt.kind <;> cases hb : evt.isBreak <;>
    simp [RuntimeScoreProof.eventNoteType, RuntimeScoreProof.updateCounts, hk, hb,
      selectCounts, replaceCounts]

theorem update_tables_equiv {N : Type} (natural : N → Nat) (add : N → N → N)
    (hadd : ∀ a b, natural (add a b) = natural a + natural b)
    (tables : Tables N) (evt : LnmaiCore.JudgeEvent) (multiple : N) :
    (do let next ← update add tables evt multiple; ok (decodeTables natural next)) =
      ok (RuntimeScoreProof.updateCounts (decodeTables natural tables) evt
        (natural multiple)) := by
  let next := (select tables (RuntimeScoreProof.eventNoteType evt)).set
    (gradeIndex evt.grade)
    (add (read (select tables (RuntimeScoreProof.eventNoteType evt)) evt.grade) multiple)
  have hnext : bump add (select tables (RuntimeScoreProof.eventNoteType evt))
      (ofLnmJudgeGrade evt.grade) multiple = ok next := by
    simp only [bump, get_count_equiv, bind_tc_ok, set_count_equiv,
      toLnmJudgeGrade_ofLnmJudgeGrade, next]
  have hdecode := bump_equiv natural add hadd
    (select tables (RuntimeScoreProof.eventNoteType evt)) (ofLnmJudgeGrade evt.grade) multiple
  simp only [hnext, bind_tc_ok, Result.ok.injEq,
    toLnmJudgeGrade_ofLnmJudgeGrade, decode_select] at hdecode
  simp only [update, hnext, bind_tc_ok, decode_replace, hdecode]
  exact congrArg Result.ok (route_equiv (decodeTables natural tables) evt (natural multiple))

end Verification.RuntimeCountsProof
