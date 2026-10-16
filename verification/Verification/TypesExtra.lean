import Verification.Types
import Verification.Judge

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.TypesProof
open Bridge JudgeProof

theorem noteStatus_leq_equiv (a b : types.NoteStatus) :
    types.NoteStatus.leq a b =
      ok (LnmaiCore.NoteStatus.leq (toLnmNoteStatus a) (toLnmNoteStatus b)) := by
  cases a <;> cases b <;> rfl

def toLnmNoteJudgeResult (r : types.NoteJudgeResult) : LnmaiCore.NoteJudgeResult :=
  { grade := toLnmJudgeGrade r.grade, diff := toLnmDuration r.diff
    isBreak := r.is_break, isEX := r.is_ex }

theorem noteJudgeResult_is_fast_equiv (r : types.NoteJudgeResult) :
    types.NoteJudgeResult.is_fast r =
      ok (LnmaiCore.NoteJudgeResult.isFast (toLnmNoteJudgeResult r)) := by
  simp [types.NoteJudgeResult.is_fast, time.Duration.to_micros,
    LnmaiCore.NoteJudgeResult.isFast, toLnmNoteJudgeResult,
    lnmDuration_lt_iff_toMicros, zero_toMicros]

theorem noteJudgeResult_is_miss_or_too_fast_equiv (r : types.NoteJudgeResult) :
    types.NoteJudgeResult.is_miss_or_too_fast r =
      ok (LnmaiCore.NoteJudgeResult.isMissOrTooFast (toLnmNoteJudgeResult r)) := by
  exact judgeGrade_is_miss_or_too_fast_equiv r.grade

/-- The finite kernel stores the score counters, but not runtime grade counts.
    This bridge adds empty counts and is used only for its default value. -/
def toLnmInitialScoreState (s : types.ScoreState) : LnmaiCore.ScoreState :=
  { combo := s.combo.val, pCombo := s.p_combo.val, cPCombo := s.c_p_combo.val
    totalBase := s.total_base.val, totalExtra := s.total_extra.val
    earnedBase := s.earned_base.val, earnedExtra := s.earned_extra.val
    earnedClassicExtra := s.earned_classic_extra.val
    lostBase := s.lost_base.val, lostExtra := s.lost_extra.val
    lostClassicExtra := s.lost_classic_extra.val
    dxScore := s.dx_score.val, maxDxScore := s.max_dx_score.val
    fastCount := s.fast_count.val, lateCount := s.late_count.val }

theorem scoreState_default_equiv :
    ∃ s : types.ScoreState,
      types.ScoreState.Insts.CoreDefaultDefault.default = ok s ∧
      toLnmInitialScoreState s = ({} : LnmaiCore.ScoreState) := by
  exact ⟨_, rfl, rfl⟩

end Verification.TypesProof
