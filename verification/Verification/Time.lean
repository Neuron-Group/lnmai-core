import Verification.Bridge

open Aeneas Aeneas.Std Result lnmai_core_verify
namespace Verification.TimeProof
open Bridge

theorem duration_from_micros_equiv (value : Std.I64) :
    time.Duration.from_micros value = ok ⟨value⟩ := rfl

theorem duration_zero_equiv :
    time.Duration.zero = ok ⟨0#i64⟩ := rfl

theorem duration_to_micros_equiv (d : time.Duration) :
    time.Duration.to_micros d = ok d.micros := rfl

theorem duration_add_equiv (a b : time.Duration)
    (hlo : IScalar.min .I64 ≤ a.micros.val + b.micros.val)
    (hhi : a.micros.val + b.micros.val ≤ IScalar.max .I64) :
    ∃ r, time.Duration.Insts.CoreOpsArithAddDurationDuration.add a b = ok r ∧
      toLnmDuration r = toLnmDuration a + toLnmDuration b := by
  obtain ⟨v, hv, hval⟩ := i64_add_eq a.micros b.micros hlo hhi
  refine ⟨⟨v⟩, ?_, ?_⟩
  · simp [time.Duration.Insts.CoreOpsArithAddDurationDuration.add, hv]
  · apply LnmaiCore.Duration.toMicros_injective
    change v.val = a.micros.val + b.micros.val
    exact hval

theorem duration_sub_equiv (a b : time.Duration)
    (hlo : IScalar.min .I64 ≤ a.micros.val - b.micros.val)
    (hhi : a.micros.val - b.micros.val ≤ IScalar.max .I64) :
    ∃ r, time.Duration.Insts.CoreOpsArithSubDurationDuration.sub a b = ok r ∧
      toLnmDuration r = toLnmDuration a - toLnmDuration b := by
  obtain ⟨v, hv, hval⟩ := WP.spec_imp_exists (IScalar.sub_spec hlo hhi)
  refine ⟨⟨v⟩, ?_, ?_⟩
  · simp [time.Duration.Insts.CoreOpsArithSubDurationDuration.sub, hv]
  · apply LnmaiCore.Duration.toMicros_injective
    change v.val = a.micros.val - b.micros.val
    exact hval

theorem time_point_from_micros_equiv (value : Std.I64) :
    time.TimePoint.from_micros value = ok ⟨value⟩ := rfl

theorem time_point_zero_equiv : time.TimePoint.zero = ok ⟨0#i64⟩ := rfl

theorem time_point_to_micros_equiv (p : time.TimePoint) :
    time.TimePoint.to_micros p = ok p.micros := rfl

theorem time_point_sub_equiv (a b : time.TimePoint)
    (hlo : IScalar.min .I64 ≤ a.micros.val - b.micros.val)
    (hhi : a.micros.val - b.micros.val ≤ IScalar.max .I64) :
    ∃ r, time.TimePoint.Insts.CoreOpsArithSubTimePointDuration.sub a b = ok r ∧
      toLnmDuration r = toLnmTimePoint a - toLnmTimePoint b := by
  obtain ⟨v, hv, hval⟩ := WP.spec_imp_exists (IScalar.sub_spec hlo hhi)
  refine ⟨⟨v⟩, ?_, ?_⟩
  · simp [time.TimePoint.Insts.CoreOpsArithSubTimePointDuration.sub, hv]
  · apply LnmaiCore.Duration.toMicros_injective
    change v.val = a.micros.val - b.micros.val
    exact hval

end Verification.TimeProof
