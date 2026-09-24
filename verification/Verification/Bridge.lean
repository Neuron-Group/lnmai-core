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

/-! ## Time arithmetic value bridges -/

/-- A `u32 → i64` cast (`UScalar.hcast`) preserves the value. -/
theorem hcast_i64_val (x : Std.U32) :
    (UScalar.hcast IScalarTy.I64 x).val = (x.val : ℤ) := by
  have h := UScalar.hcast_inBounds_spec IScalarTy.I64 x (by scalar_tac)
  simpa [lift, WP.spec_ok] using h

/-- `Duration.scale_nat` value spec: `r.micros = d.micros * factor` when the
product stays in `i64` range. -/
theorem scale_nat_spec (d : time.Duration) (factor : Std.U32)
    (hlo : IScalar.min .I64 ≤ d.micros.val * (factor.val : ℤ))
    (hhi : d.micros.val * (factor.val : ℤ) ≤ IScalar.max .I64) :
    time.Duration.scale_nat d factor ⦃ r =>
      r.micros.val = d.micros.val * (factor.val : ℤ) ⦄ := by
  unfold time.Duration.scale_nat
  step
  · step <;> simp_all [hcast_i64_val]

/-- `x = 0#u32` iff its value is `0`. -/
theorem u32_eq_zero_iff (x : Std.U32) : (x = 0#u32) ↔ x.val = 0 := by
  constructor
  · intro h; rw [h]; rfl
  · intro h; exact UScalar.eq_of_val_eq (by simpa using h)

/-- `Duration.div_nat` value spec. Note the model divides with truncation
toward zero (`Int.tdiv`), matching Rust; the spec's `Duration.divNat` uses
Lean's Euclidean `/`. Use only with a nonnegative dividend. -/
theorem div_nat_spec (d : time.Duration) (divisor : Std.U32)
    (hNoOverflow : ¬ (d.micros.val = IScalar.min .I64 ∧ (divisor.val : ℤ) = -1)) :
    time.Duration.div_nat d divisor ⦃ r =>
      r.micros.val = (if divisor.val = 0 then 0
        else Int.tdiv d.micros.val (divisor.val : ℤ)) ⦄ := by
  unfold time.Duration.div_nat
  split
  · rename_i hdiv
    have hz : divisor.val = 0 := (u32_eq_zero_iff divisor).mp hdiv
    simp [hz]
  · rename_i hdiv
    have hnz2 : divisor.val ≠ 0 := fun h => hdiv ((u32_eq_zero_iff divisor).mpr h)
    have hnz : (UScalar.hcast IScalarTy.I64 divisor).val ≠ (0 : ℤ) := by
      rw [hcast_i64_val]
      intro h
      exact hnz2 (by omega)
    have hno : ¬ (d.micros.val = IScalar.min .I64 ∧
        (UScalar.hcast IScalarTy.I64 divisor).val = -1) := by
      rw [hcast_i64_val]; exact hNoOverflow
    simp only [lift, bind_tc_ok]
    step
    rw [i1_post, hcast_i64_val]
    simp [hnz2]

/-- Equality form of the `div_nat` value spec. -/
theorem div_nat_eq (d : time.Duration) (divisor : Std.U32)
    (hNoOverflow : ¬ (d.micros.val = IScalar.min .I64 ∧ (divisor.val : ℤ) = -1)) :
    ∃ r : time.Duration, time.Duration.div_nat d divisor = ok r ∧
      r.micros.val = (if divisor.val = 0 then 0
        else Int.tdiv d.micros.val (divisor.val : ℤ)) := by
  have h := div_nat_spec d divisor hNoOverflow
  cases hf : time.Duration.div_nat d divisor with
  | ok x => refine ⟨x, rfl, ?_⟩; rw [hf, WP.spec_ok] at h; exact h
  | fail e => simp [hf, WP.spec_fail] at h
  | div => simp [hf, WP.spec_div] at h

/-- Equality form of the `scale_nat` value spec. -/
theorem scale_nat_eq (d : time.Duration) (factor : Std.U32)
    (hlo : IScalar.min .I64 ≤ d.micros.val * (factor.val : ℤ))
    (hhi : d.micros.val * (factor.val : ℤ) ≤ IScalar.max .I64) :
    ∃ r : time.Duration, time.Duration.scale_nat d factor = ok r ∧
      r.micros.val = d.micros.val * (factor.val : ℤ) := by
  have h := scale_nat_spec d factor hlo hhi
  cases hf : time.Duration.scale_nat d factor with
  | ok x => refine ⟨x, rfl, ?_⟩; rw [hf, WP.spec_ok] at h; exact h
  | fail e => simp [hf, WP.spec_fail] at h
  | div => simp [hf, WP.spec_div] at h

/-! ## `Duration` value projections (used by the modern-slide proof) -/

theorem Duration.toMicros_add (a b : LnmaiCore.Duration) :
    (a + b).toMicros = a.toMicros + b.toMicros := rfl

theorem Duration.toMicros_min (a b : LnmaiCore.Duration) :
    (a ⊓ b).toMicros = min a.toMicros b.toMicros := by
  show (if a ≤ b then a else b).toMicros = min a.toMicros b.toMicros
  by_cases h : a ≤ b
  · rw [if_pos h]
    exact (min_eq_left h).symm
  · rw [if_neg h]
    exact (min_eq_right (le_of_lt (not_le.mp h))).symm

theorem Duration.toMicros_scaleNat (a : LnmaiCore.Duration) (k : Nat) :
    (LnmaiCore.Duration.scaleNat a k).toMicros = a.toMicros * (k : ℤ) := by
  simp [LnmaiCore.Duration.scaleNat, LnmaiCore.Duration.toMicros, LnmaiCore.Duration.toInt,
    LnmaiCore.Duration.ofInt, LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt]

theorem Duration.toMicros_divNat (a : LnmaiCore.Duration) (k : Nat) :
    (LnmaiCore.Duration.divNat a k).toMicros =
      (if k = 0 then 0 else a.toMicros / (k : ℤ)) := by
  simp only [LnmaiCore.Duration.divNat]
  split
  · simp [LnmaiCore.Duration.zero, LnmaiCore.Duration.toMicros, LnmaiCore.Duration.toInt,
      LnmaiCore.Duration.ofInt, LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt]
  · simp [LnmaiCore.Duration.toMicros, LnmaiCore.Duration.toInt,
      LnmaiCore.Duration.ofInt, LnmaiCore.Duration.ofTick, LnmaiCore.TimeTick.ofInt]

/-- `i64` addition equality (no-overflow), for the `+` used by the model. -/
theorem i64_add_eq (a b : Std.I64)
    (hlo : IScalar.min .I64 ≤ a.val + b.val) (hhi : a.val + b.val ≤ IScalar.max .I64) :
    ∃ r : Std.I64, a + b = ok r ∧ r.val = a.val + b.val := by
  have h := IScalar.add_spec (ty := .I64) hlo hhi
  cases hf : a + b with
  | ok x => exact ⟨x, rfl, by rw [hf, WP.spec_ok] at h; exact h⟩
  | fail e => simp [hf, WP.spec_fail] at h
  | div => simp [hf, WP.spec_div] at h

end Bridge

end Verification
