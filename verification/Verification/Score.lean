-- M1d: Score equivalence (structural part).
--
-- The score/combo functions combine enum dispatch with bounded-integer
-- arithmetic. The enum structure is proved here; the arithmetic obligations
-- (no `u32` overflow) are added as separate, explicitly-preconditioned lemmas
-- so that they can be checked in batch (each Lean check loads Mathlib and costs
-- minutes on this machine).
import Verification.Bridge

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace ScoreProof

open Bridge

theorem base_score_equiv (nt : types.NoteType) :
    ∃ v : Std.U32, score.base_score nt = ok v ∧
      v.val = LnmaiCore.Score.baseScore (toLnmNoteType nt) := by
  cases nt <;> exact ⟨_, rfl, rfl⟩

end ScoreProof

end Verification
