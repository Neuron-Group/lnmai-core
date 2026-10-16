import Verification.ScoreExtra

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 1000000
set_option maxErrors 5

namespace Verification.Adapters
open Bridge ScoreProof

/- The runtime obtains finite coefficients from the extracted kernel and
   performs the arithmetic with arbitrary-precision naturals/integers.
   These theorems verify that decomposition for every mathematical input;
   the BigInt/BigUint representation connection is a separate obligation. -/

theorem non_break_coefficient_adapter (base multiple : Nat) (grade : types.JudgeGrade) :
    ∃ coefficient : Std.U32 × Std.U32,
      score.score_non_break 10#u32 grade 1#u32 = ok coefficient ∧
      (let total := base * multiple
       let earned := total * coefficient.1.val / 10
       (earned, total - earned)) =
        LnmaiCore.Score.scoreNonBreak base (toLnmJudgeGrade grade) multiple := by
  obtain ⟨coefficient, hc, hv⟩ := WP.spec_imp_exists
    (score_non_break_equiv 10#u32 1#u32 grade (by decide) (by decide))
  refine ⟨coefficient, hc, ?_⟩
  have he := congrArg Prod.fst hv
  simp only [Prod.fst] at he
  rw [he]
  cases grade <;> simp [toLnmJudgeGrade, LnmaiCore.Score.scoreNonBreak] <;> omega

theorem break_coefficient_adapter (multiple : Nat) (grade : types.JudgeGrade) :
    ∃ coefficient : Std.U32 × Std.U32 × Std.U32 × Std.U32 × Std.U32 × Std.U32,
      score.score_break grade 1#u32 = ok coefficient ∧
      (multiple * coefficient.1.val, multiple * coefficient.2.1.val,
        multiple * coefficient.2.2.1.val, multiple * coefficient.2.2.2.1.val,
        multiple * coefficient.2.2.2.2.1.val, multiple * coefficient.2.2.2.2.2.val) =
      LnmaiCore.Score.scoreBreak (toLnmJudgeGrade grade) multiple := by
  obtain ⟨coefficient, hc, hv⟩ := WP.spec_imp_exists
    (score_break_equiv grade 1#u32 (by decide))
  refine ⟨coefficient, hc, ?_⟩
  cases grade <;>
    simp only [toLnmJudgeGrade, LnmaiCore.Score.scoreBreak,
      UScalar.ofNatCore_val_eq, Nat.mul_one, Prod.mk.injEq] at hv ⊢ <;>
    rcases hv with ⟨h1, h2, h3, h4, h5, h6⟩ <;>
    simp [h1, h2, h3, h4, h5, h6, Nat.mul_comm]

def comboFromCoefficients (combo pCombo cPCombo : Nat) (lost : ℤ)
    (multiple : Nat) (coefficient : score.ComboDelta) : LnmaiCore.Score.ComboDelta :=
  { combo := if coefficient.combo.val = 0 then 0 else combo + multiple
    pCombo := if coefficient.p_combo.val = 0 then 0 else pCombo + multiple
    cPCombo := if coefficient.c_p_combo.val = 0 then 0 else cPCombo + multiple
    dXScoreLost := lost + coefficient.dx_score_lost.val * (multiple : ℤ) }

theorem combo_coefficient_adapter (combo pCombo cPCombo multiple : Nat) (lost : ℤ)
    (grade : types.JudgeGrade) :
    ∃ coefficient : score.ComboDelta,
      score.update_combo 0#u32 0#u32 0#u32 0#i64 grade 1#u32 = ok coefficient ∧
      comboFromCoefficients combo pCombo cPCombo lost multiple coefficient =
        LnmaiCore.Score.updateCombo combo pCombo cPCombo lost (toLnmJudgeGrade grade) multiple := by
  obtain ⟨coefficient, hc, hv⟩ := WP.spec_imp_exists
    (update_combo_equiv 0#u32 0#u32 0#u32 0#i64 grade 1#u32
      (by decide) (by decide) (by decide) (by scalar_tac) (by scalar_tac))
  refine ⟨coefficient, hc, ?_⟩
  cases grade <;>
    simp only [toLnmJudgeGrade, LnmaiCore.Score.updateCombo,
      UScalar.ofNatCore_val_eq, IScalar.ofInt_val_eq] at hv ⊢ <;>
    rcases hv with ⟨h1, h2, h3, h4⟩ <;>
    simp [comboFromCoefficients, h1, h2, h3, h4, Int.natCast_mul, sub_eq_add_neg]

end Verification.Adapters
