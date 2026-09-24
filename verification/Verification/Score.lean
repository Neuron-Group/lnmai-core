-- M1d: Score equivalence.
--
-- The score/combo functions combine enum dispatch with bounded-integer
-- arithmetic. The enum structure is proved directly; the arithmetic
-- obligations carry explicit no-overflow preconditions (game values are well
-- within `u32`/`i64` range).
import Verification.Bridge

open Aeneas Aeneas.Std Result
open lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification

namespace ScoreProof

open Bridge

theorem base_score_equiv (nt : types.NoteType) :
    ∃ v : Std.U32, score.base_score nt = ok v ∧
      v.val = LnmaiCore.Score.baseScore (toLnmNoteType nt) := by
  cases nt <;> exact ⟨_, rfl, rfl⟩

/-! ## `u32` arithmetic equalities (no-overflow) -/

theorem u32_mul_eq (a b : Std.U32) (h : a.val * b.val < 2^32) :
    ∃ r : Std.U32, a * b = ok r ∧ r.val = a.val * b.val := by
  have hs := U32.mul_spec (x := a) (y := b) (by scalar_tac)
  cases hf : a * b with
  | ok x => exact ⟨x, rfl, by rw [hf, WP.spec_ok] at hs; exact hs⟩
  | fail e => simp [hf, WP.spec_fail] at hs
  | div => simp [hf, WP.spec_div] at hs

theorem u32_div_eq (a b : Std.U32) (h : b.val ≠ 0) :
    ∃ r : Std.U32, a / b = ok r ∧ r.val = a.val / b.val := by
  have hs := U32.div_spec (x := a) (y := b) h
  cases hf : a / b with
  | ok x => exact ⟨x, rfl, by rw [hf, WP.spec_ok] at hs; exact hs⟩
  | fail e => simp [hf, WP.spec_fail] at hs
  | div => simp [hf, WP.spec_div] at hs

theorem u32_sub_eq (a b : Std.U32) (h : b.val ≤ a.val) :
    ∃ r : Std.U32, a - b = ok r ∧ r.val = a.val - b.val := by
  have hs := U32.sub_spec (x := a) (y := b) (by scalar_tac)
  cases hf : a - b with
  | ok x => exact ⟨x, rfl, by rw [hf, WP.spec_ok] at hs; exact hs.1⟩
  | fail e => simp [hf, WP.spec_fail] at hs
  | div => simp [hf, WP.spec_div] at hs

/-! ## Function equivalences -/

/-- `score_non_break` matches the spec under no-overflow. -/
theorem score_non_break_equiv (base multiple : Std.U32) (grade : types.JudgeGrade)
    (hb1 : base.val * multiple.val < 2^32)
    (hb4 : base.val * multiple.val * 4 < 2^32) :
    score.score_non_break base grade multiple ⦃ r =>
      (r.1.val, r.2.val) =
        LnmaiCore.Score.scoreNonBreak base.val (toLnmJudgeGrade grade) multiple.val ⦄ := by
  generalize hg : toLnmJudgeGrade grade = g
  cases grade <;>
    simp only [toLnmJudgeGrade] at hg <;>
    rw [← hg] <;>
    simp only [score.score_non_break, LnmaiCore.Score.scoreNonBreak] <;>
    step* <;> simp_all <;> omega

/-- `update_combo` matches the spec under no-overflow. -/
theorem update_combo_equiv (combo p_combo c_p_combo : Std.U32) (dx_score_lost : Std.I64)
    (grade : types.JudgeGrade) (multiple : Std.U32)
    (hcombo : combo.val + multiple.val < 2^32)
    (hp : p_combo.val + multiple.val < 2^32)
    (hcp : c_p_combo.val + multiple.val < 2^32)
    (hlo : IScalar.min .I64 ≤ dx_score_lost.val - 3 * (multiple.val : ℤ))
    (hhi : dx_score_lost.val - 3 * (multiple.val : ℤ) ≤ IScalar.max .I64) :
    score.update_combo combo p_combo c_p_combo dx_score_lost grade multiple ⦃ r =>
      r.combo.val = (LnmaiCore.Score.updateCombo combo.val p_combo.val c_p_combo.val
        dx_score_lost.val (toLnmJudgeGrade grade) multiple.val).combo ∧
      r.p_combo.val = (LnmaiCore.Score.updateCombo combo.val p_combo.val c_p_combo.val
        dx_score_lost.val (toLnmJudgeGrade grade) multiple.val).pCombo ∧
      r.c_p_combo.val = (LnmaiCore.Score.updateCombo combo.val p_combo.val c_p_combo.val
        dx_score_lost.val (toLnmJudgeGrade grade) multiple.val).cPCombo ∧
      r.dx_score_lost.val = (LnmaiCore.Score.updateCombo combo.val p_combo.val c_p_combo.val
        dx_score_lost.val (toLnmJudgeGrade grade) multiple.val).dXScoreLost ⦄ := by
  generalize hg : toLnmJudgeGrade grade = g
  cases grade <;>
    simp only [toLnmJudgeGrade] at hg <;>
    rw [← hg] <;>
    simp only [score.update_combo, LnmaiCore.Score.updateCombo] <;>
    step* <;> simp_all [hcast_i64_val] <;> (first | omega | scalar_tac)

end ScoreProof

end Verification
