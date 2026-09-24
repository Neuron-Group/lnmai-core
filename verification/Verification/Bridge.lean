-- M1 bridge: isomorphisms and coercion lemmas between the Aeneas model
-- (`lnmai_core_verify`) and the authoritative Lean specification (`LnmaiCore`).
import Verification.Generated
import Verification.Areas
import LnmaiCore.Types
import LnmaiCore.Time
import LnmaiCore.Areas
import LnmaiCore.Convert
import LnmaiCore.Judge
import LnmaiCore.Score

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace Bridge

/-! ## Time -/

/-- Model duration → spec duration. -/
def toLnmDuration (d : time.Duration) : LnmaiCore.Duration :=
  LnmaiCore.Duration.fromMicros d.micros.val

/-- Model time point → spec time point. -/
def toLnmTimePoint (p : time.TimePoint) : LnmaiCore.TimePoint :=
  LnmaiCore.TimePoint.fromMicros p.micros.val

@[simp] theorem toLnmDuration_toMicros (d : time.Duration) :
    (toLnmDuration d).toMicros = d.micros.val := by
  rfl

@[simp] theorem toLnmDuration_zero :
    toLnmDuration (⟨0#i64⟩ : time.Duration) = LnmaiCore.Duration.zero := by
  rfl

/-! ## Areas (re-exported for convenience) -/

abbrev toLnmSensorArea := Areas.toLnmSensorArea
abbrev ofLnmSensorArea := Areas.ofLnmSensorArea
abbrev toLnmButtonZone := Areas.toLnmButtonZone
abbrev ofLnmButtonZone := Areas.ofLnmButtonZone
abbrev toLnmOuterSlot := Areas.toLnmOuterSlot
abbrev ofLnmOuterSlot := Areas.ofLnmOuterSlot

/-! ## Core types -/

def toLnmJudgeGrade : types.JudgeGrade → LnmaiCore.JudgeGrade
  | .Miss => .Miss | .LateGood => .LateGood | .LateGreat3rd => .LateGreat3rd
  | .LateGreat2nd => .LateGreat2nd | .LateGreat => .LateGreat
  | .LatePerfect3rd => .LatePerfect3rd | .LatePerfect2nd => .LatePerfect2nd
  | .Perfect => .Perfect | .FastPerfect2nd => .FastPerfect2nd
  | .FastPerfect3rd => .FastPerfect3rd | .FastGreat => .FastGreat
  | .FastGreat2nd => .FastGreat2nd | .FastGreat3rd => .FastGreat3rd
  | .FastGood => .FastGood | .TooFast => .TooFast

def ofLnmJudgeGrade : LnmaiCore.JudgeGrade → types.JudgeGrade
  | .Miss => .Miss | .LateGood => .LateGood | .LateGreat3rd => .LateGreat3rd
  | .LateGreat2nd => .LateGreat2nd | .LateGreat => .LateGreat
  | .LatePerfect3rd => .LatePerfect3rd | .LatePerfect2nd => .LatePerfect2nd
  | .Perfect => .Perfect | .FastPerfect2nd => .FastPerfect2nd
  | .FastPerfect3rd => .FastPerfect3rd | .FastGreat => .FastGreat
  | .FastGreat2nd => .FastGreat2nd | .FastGreat3rd => .FastGreat3rd
  | .FastGood => .FastGood | .TooFast => .TooFast

def toLnmJudgeStyle : types.JudgeStyle → LnmaiCore.JudgeStyle
  | .Default => .Default | .Maji => .Maji | .Gachi => .Gachi | .Gori => .Gori

def ofLnmJudgeStyle : LnmaiCore.JudgeStyle → types.JudgeStyle
  | .Default => .Default | .Maji => .Maji | .Gachi => .Gachi | .Gori => .Gori

def toLnmNoteType : types.NoteType → LnmaiCore.NoteType
  | .Tap => .Tap | .Hold => .Hold | .Slide => .Slide | .Touch => .Touch | .Break => .Break

def ofLnmNoteType : LnmaiCore.NoteType → types.NoteType
  | .Tap => .Tap | .Hold => .Hold | .Slide => .Slide | .Touch => .Touch | .Break => .Break

def toLnmSlideKind : types.SlideKind → LnmaiCore.SlideKind
  | .Single => .Single | .Wifi => .Wifi | .ConnPart => .ConnPart

def toLnmAreaPolicy : types.AreaPolicy → LnmaiCore.AreaPolicy
  | .Or => .Or | .And => .And

def toLnmDisplayOption : types.JudgeDisplayOption → LnmaiCore.JudgeDisplayOption
  | .All => .All | .BelowCP => .BelowCP | .BelowP => .BelowP
  | .BelowGR => .BelowGR | .MissOnly => .MissOnly | .Disable => .Disable

def toLnmNoteStatus : types.NoteStatus → LnmaiCore.NoteStatus
  | .Start => .Start | .Inited => .Inited | .Scaling => .Scaling
  | .Running => .Running | .Arrived => .Arrived | .End => .End

def toLnmComboState : types.ComboState → LnmaiCore.ComboState
  | .None => .None | .FC => .FC | .FCPlus => .FCPlus | .AP => .AP | .APPlus => .APPlus

def toLnmRuntimePos : types.RuntimePos → LnmaiCore.RuntimePos
  | .Button z => .button (toLnmButtonZone z)
  | .Sensor a => .sensor (toLnmSensorArea a)

@[simp] theorem ofLnmJudgeGrade_toLnmJudgeGrade (g : types.JudgeGrade) :
    ofLnmJudgeGrade (toLnmJudgeGrade g) = g := by cases g <;> rfl

@[simp] theorem toLnmJudgeGrade_ofLnmJudgeGrade (g : LnmaiCore.JudgeGrade) :
    toLnmJudgeGrade (ofLnmJudgeGrade g) = g := by cases g <;> rfl

/-- Distribution of `ofLnmJudgeGrade` over `if`, for symbolic branch proofs. -/
@[simp] theorem ofLnmJudgeGrade_ite (c : Prop) [Decidable c]
    (a b : LnmaiCore.JudgeGrade) :
    ofLnmJudgeGrade (if c then a else b) =
      if c then ofLnmJudgeGrade a else ofLnmJudgeGrade b := by
  split <;> rfl

@[simp] theorem ofLnmJudgeStyle_toLnmJudgeStyle (s : types.JudgeStyle) :
    ofLnmJudgeStyle (toLnmJudgeStyle s) = s := by cases s <;> rfl

@[simp] theorem toLnmJudgeStyle_ofLnmJudgeStyle (s : LnmaiCore.JudgeStyle) :
    toLnmJudgeStyle (ofLnmJudgeStyle s) = s := by cases s <;> rfl

@[simp] theorem ofLnmNoteType_toLnmNoteType (n : types.NoteType) :
    ofLnmNoteType (toLnmNoteType n) = n := by cases n <;> rfl

@[simp] theorem toLnmNoteType_ofLnmNoteType (n : LnmaiCore.NoteType) :
    toLnmNoteType (ofLnmNoteType n) = n := by cases n <;> rfl

end Bridge

end Verification
