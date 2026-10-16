import Verification.TypesPublic

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.DefaultsProof
open Bridge TypesProof

/-- Defaults embed exactly; no arbitrary-precision arithmetic is needed. -/
theorem duration_default_equiv :
    (do let value ← time.Duration.Insts.CoreDefaultDefault.default
        ok (toLnmDuration value)) = ok (default : LnmaiCore.Duration) := rfl

theorem time_point_default_equiv :
    (do let value ← time.TimePoint.Insts.CoreDefaultDefault.default
        ok (toLnmTimePoint value)) = ok (default : LnmaiCore.TimePoint) := rfl

theorem note_judge_result_default_equiv :
    (do let value ← types.NoteJudgeResult.Insts.CoreDefaultDefault.default
        ok (toLnmNoteJudgeResult value)) = ok (default : LnmaiCore.NoteJudgeResult) := rfl

def toLnmGroupState (value : types.GroupState) : LnmaiCore.GroupState :=
  { groupId := value.group_id.val, count := value.count.val, size := value.size.val
    grade := toLnmJudgeGrade value.grade, diff := toLnmDuration value.diff }

theorem group_state_default_equiv :
    (do let value ← types.GroupState.Insts.CoreDefaultDefault.default
        ok (toLnmGroupState value)) = ok (default : LnmaiCore.GroupState) := rfl

def toLnmJudgeEvent (value : types.JudgeEvent) : LnmaiCore.JudgeEvent :=
  { kind := toLnmJudgeEventKind value.kind, phase := toLnmJudgePhase value.phase
    grade := toLnmJudgeGrade value.grade, diff := toLnmDuration value.diff
    position := toLnmRuntimePos value.position, noteIndex := value.note_index.val
    isBreak := value.is_break, isEX := value.is_ex, multiple := value.multiple.val }

theorem judge_event_default_equiv :
    (do let value ← types.JudgeEvent.Insts.CoreDefaultDefault.default
        ok (toLnmJudgeEvent value)) = ok (default : LnmaiCore.JudgeEvent) := rfl

def toLnmComboDelta (value : score.ComboDelta) : LnmaiCore.Score.ComboDelta :=
  { combo := value.combo.val, pCombo := value.p_combo.val, cPCombo := value.c_p_combo.val
    dXScoreLost := value.dx_score_lost.val }

theorem combo_delta_default_equiv :
    (do let value ← score.ComboDelta.Insts.CoreDefaultDefault.default
        ok (toLnmComboDelta value)) = ok (default : LnmaiCore.Score.ComboDelta) := rfl

end Verification.DefaultsProof

