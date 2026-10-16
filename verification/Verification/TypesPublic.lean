import Verification.TypesExtra
import Verification.AreasPublic

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 4000000

namespace Verification.TypesProof
open Bridge

def toLnmJudgeEventKind : types.JudgeEventKind → LnmaiCore.JudgeEventKind
  | .Tap => .Tap | .Hold => .Hold | .Slide => .Slide | .Touch => .Touch | .Break => .Break

def toLnmJudgePhase : types.JudgePhase → LnmaiCore.JudgePhase
  | .Head => .head | .Tail => .tail

theorem judgeGrade_default_equiv :
    (do let value ← types.JudgeGrade.Insts.CoreDefaultDefault.default
        ok (toLnmJudgeGrade value)) = ok (default : LnmaiCore.JudgeGrade) := rfl

theorem judgeGrade_clone_equiv (value : types.JudgeGrade) :
    types.JudgeGrade.Insts.CoreCloneClone.clone value = ok value := rfl

theorem judgeGrade_eq_equiv (a b : types.JudgeGrade) :
    types.JudgeGrade.Insts.CoreCmpPartialEqJudgeGrade.eq a b =
      ok (decide (toLnmJudgeGrade a = toLnmJudgeGrade b)) := by
  cases a <;> cases b <;>
    simp [types.JudgeGrade.Insts.CoreCmpPartialEqJudgeGrade.eq,
      types.JudgeGrade.read_discriminant, toLnmJudgeGrade] <;> rfl

theorem judgeGrade_cmp_equiv (a b : types.JudgeGrade) :
    types.JudgeGrade.Insts.CoreCmpOrd.cmp a b =
      ok (compare (toLnmJudgeGrade a) (toLnmJudgeGrade b)) := by
  cases a <;> cases b <;>
    simp [types.JudgeGrade.Insts.CoreCmpOrd.cmp,
      types.JudgeGrade.read_discriminant, toLnmJudgeGrade] <;> rfl

theorem judgeGrade_partial_cmp_equiv (a b : types.JudgeGrade) :
    types.JudgeGrade.Insts.CoreCmpPartialOrdJudgeGrade.partial_cmp a b =
      ok (some (compare (toLnmJudgeGrade a) (toLnmJudgeGrade b))) := by
  simp only [types.JudgeGrade.Insts.CoreCmpPartialOrdJudgeGrade.partial_cmp,
    judgeGrade_cmp_equiv, bind_tc_ok]

theorem displayOption_default_equiv :
    (do let value ← types.JudgeDisplayOption.Insts.CoreDefaultDefault.default
        ok (toLnmDisplayOption value)) = ok (default : LnmaiCore.JudgeDisplayOption) := rfl

theorem displayOption_clone_equiv (value : types.JudgeDisplayOption) :
    types.JudgeDisplayOption.Insts.CoreCloneClone.clone value = ok value := rfl

theorem displayOption_eq_equiv (a b : types.JudgeDisplayOption) :
    types.JudgeDisplayOption.Insts.CoreCmpPartialEqJudgeDisplayOption.eq a b =
      ok (decide (toLnmDisplayOption a = toLnmDisplayOption b)) := by
  cases a <;> cases b <;>
    simp [types.JudgeDisplayOption.Insts.CoreCmpPartialEqJudgeDisplayOption.eq,
      types.JudgeDisplayOption.read_discriminant, toLnmDisplayOption] <;> rfl

theorem noteType_default_equiv :
    (do let value ← types.NoteType.Insts.CoreDefaultDefault.default
        ok (toLnmNoteType value)) = ok (default : LnmaiCore.NoteType) := rfl

theorem noteType_clone_equiv (value : types.NoteType) :
    types.NoteType.Insts.CoreCloneClone.clone value = ok value := rfl

theorem noteType_eq_equiv (a b : types.NoteType) :
    types.NoteType.Insts.CoreCmpPartialEqNoteType.eq a b =
      ok (decide (toLnmNoteType a = toLnmNoteType b)) := by
  cases a <;> cases b <;>
    simp [types.NoteType.Insts.CoreCmpPartialEqNoteType.eq,
      types.NoteType.read_discriminant, toLnmNoteType] <;> rfl

theorem slideKind_default_equiv :
    (do let value ← types.SlideKind.Insts.CoreDefaultDefault.default
        ok (toLnmSlideKind value)) = ok (default : LnmaiCore.SlideKind) := rfl

theorem slideKind_clone_equiv (value : types.SlideKind) :
    types.SlideKind.Insts.CoreCloneClone.clone value = ok value := rfl

theorem slideKind_eq_equiv (a b : types.SlideKind) :
    types.SlideKind.Insts.CoreCmpPartialEqSlideKind.eq a b =
      ok (decide (toLnmSlideKind a = toLnmSlideKind b)) := by
  cases a <;> cases b <;>
    simp [types.SlideKind.Insts.CoreCmpPartialEqSlideKind.eq,
      types.SlideKind.read_discriminant, toLnmSlideKind] <;> rfl

theorem areaPolicy_default_equiv :
    (do let value ← types.AreaPolicy.Insts.CoreDefaultDefault.default
        ok (toLnmAreaPolicy value)) = ok (default : LnmaiCore.AreaPolicy) := rfl

theorem areaPolicy_clone_equiv (value : types.AreaPolicy) :
    types.AreaPolicy.Insts.CoreCloneClone.clone value = ok value := rfl

theorem areaPolicy_eq_equiv (a b : types.AreaPolicy) :
    types.AreaPolicy.Insts.CoreCmpPartialEqAreaPolicy.eq a b =
      ok (decide (toLnmAreaPolicy a = toLnmAreaPolicy b)) := by
  cases a <;> cases b <;>
    simp [types.AreaPolicy.Insts.CoreCmpPartialEqAreaPolicy.eq,
      types.AreaPolicy.read_discriminant, toLnmAreaPolicy] <;> rfl

theorem judgeStyle_default_equiv :
    (do let value ← types.JudgeStyle.Insts.CoreDefaultDefault.default
        ok (toLnmJudgeStyle value)) = ok (default : LnmaiCore.JudgeStyle) := rfl

theorem judgeStyle_clone_equiv (value : types.JudgeStyle) :
    types.JudgeStyle.Insts.CoreCloneClone.clone value = ok value := rfl

theorem judgeStyle_eq_equiv (a b : types.JudgeStyle) :
    types.JudgeStyle.Insts.CoreCmpPartialEqJudgeStyle.eq a b =
      ok (decide (toLnmJudgeStyle a = toLnmJudgeStyle b)) := by
  cases a <;> cases b <;>
    simp [types.JudgeStyle.Insts.CoreCmpPartialEqJudgeStyle.eq,
      types.JudgeStyle.read_discriminant, toLnmJudgeStyle] <;> rfl

theorem noteStatus_default_equiv :
    (do let value ← types.NoteStatus.Insts.CoreDefaultDefault.default
        ok (toLnmNoteStatus value)) = ok (default : LnmaiCore.NoteStatus) := rfl

theorem noteStatus_clone_equiv (value : types.NoteStatus) :
    types.NoteStatus.Insts.CoreCloneClone.clone value = ok value := rfl

theorem noteStatus_eq_equiv (a b : types.NoteStatus) :
    types.NoteStatus.Insts.CoreCmpPartialEqNoteStatus.eq a b =
      ok (decide (toLnmNoteStatus a = toLnmNoteStatus b)) := by
  cases a <;> cases b <;>
    simp [types.NoteStatus.Insts.CoreCmpPartialEqNoteStatus.eq,
      types.NoteStatus.read_discriminant, toLnmNoteStatus] <;> rfl

theorem comboState_default_equiv :
    (do let value ← types.ComboState.Insts.CoreDefaultDefault.default
        ok (toLnmComboState value)) = ok (default : LnmaiCore.ComboState) := rfl

theorem comboState_clone_equiv (value : types.ComboState) :
    types.ComboState.Insts.CoreCloneClone.clone value = ok value := rfl

theorem comboState_eq_equiv (a b : types.ComboState) :
    types.ComboState.Insts.CoreCmpPartialEqComboState.eq a b =
      ok (decide (toLnmComboState a = toLnmComboState b)) := by
  cases a <;> cases b <;>
    simp [types.ComboState.Insts.CoreCmpPartialEqComboState.eq,
      types.ComboState.read_discriminant, toLnmComboState] <;> rfl

theorem comboState_cmp_equiv (a b : types.ComboState) :
    types.ComboState.Insts.CoreCmpOrd.cmp a b =
      ok (compare (toLnmComboState a) (toLnmComboState b)) := by
  cases a <;> cases b <;>
    simp [types.ComboState.Insts.CoreCmpOrd.cmp,
      types.ComboState.read_discriminant, toLnmComboState] <;> rfl

theorem comboState_partial_cmp_equiv (a b : types.ComboState) :
    types.ComboState.Insts.CoreCmpPartialOrdComboState.partial_cmp a b =
      ok (some (compare (toLnmComboState a) (toLnmComboState b))) := by
  simp only [types.ComboState.Insts.CoreCmpPartialOrdComboState.partial_cmp,
    comboState_cmp_equiv, bind_tc_ok]

theorem runtimePos_default_equiv :
    (do let value ← types.RuntimePos.Insts.CoreDefaultDefault.default
        ok (toLnmRuntimePos value)) = ok (default : LnmaiCore.RuntimePos) := rfl

theorem runtimePos_clone_equiv (value : types.RuntimePos) :
    types.RuntimePos.Insts.CoreCloneClone.clone value = ok value := rfl

theorem runtimePos_eq_equiv (a b : types.RuntimePos) :
    types.RuntimePos.Insts.CoreCmpPartialEqRuntimePos.eq a b =
      ok (decide (toLnmRuntimePos a = toLnmRuntimePos b)) := by
  cases a <;> cases b <;>
    (rename_i x y; cases x <;> cases y <;>
      simp [types.RuntimePos.Insts.CoreCmpPartialEqRuntimePos.eq,
        types.RuntimePos.read_discriminant, Areas.buttonZone_eq_equiv,
        Areas.sensorArea_eq_equiv, toLnmRuntimePos, Areas.toLnmButtonZone,
        Areas.toLnmSensorArea]) <;> rfl

theorem judgeEventKind_default_equiv :
    (do let value ← types.JudgeEventKind.Insts.CoreDefaultDefault.default
        ok (toLnmJudgeEventKind value)) = ok (default : LnmaiCore.JudgeEventKind) := rfl

theorem judgeEventKind_clone_equiv (value : types.JudgeEventKind) :
    types.JudgeEventKind.Insts.CoreCloneClone.clone value = ok value := rfl

theorem judgeEventKind_eq_equiv (a b : types.JudgeEventKind) :
    types.JudgeEventKind.Insts.CoreCmpPartialEqJudgeEventKind.eq a b =
      ok (decide (toLnmJudgeEventKind a = toLnmJudgeEventKind b)) := by
  cases a <;> cases b <;>
    simp [types.JudgeEventKind.Insts.CoreCmpPartialEqJudgeEventKind.eq,
      types.JudgeEventKind.read_discriminant, toLnmJudgeEventKind] <;> rfl

theorem judgePhase_default_equiv :
    (do let value ← types.JudgePhase.Insts.CoreDefaultDefault.default
        ok (toLnmJudgePhase value)) = ok (default : LnmaiCore.JudgePhase) := rfl

theorem judgePhase_clone_equiv (value : types.JudgePhase) :
    types.JudgePhase.Insts.CoreCloneClone.clone value = ok value := rfl

theorem judgePhase_eq_equiv (a b : types.JudgePhase) :
    types.JudgePhase.Insts.CoreCmpPartialEqJudgePhase.eq a b =
      ok (decide (toLnmJudgePhase a = toLnmJudgePhase b)) := by
  cases a <;> cases b <;>
    simp [types.JudgePhase.Insts.CoreCmpPartialEqJudgePhase.eq,
      types.JudgePhase.read_discriminant, toLnmJudgePhase] <;> rfl

end Verification.TypesProof

