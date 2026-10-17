import Verification.GeneratedQueueRemaining
import Verification.SchedulerRemainingFields

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.QueueRemainingExtractedProof

open SchedulerRemainingFieldsProof
open QueueMaintenanceProof

/-! Bridge from the actual extracted queue-budget helpers to the runtime facade
model. The extracted bodies are the same `Vec.len`, `Option.unwrap_or`,
`Usize.saturating_sub` and checked-addition primitives that
`SchedulerRemainingFields` models; BigUint conversion and the queue facade
itself stay explicit obligations. -/

theorem vec_len_eq {T : Type} (notes : alloc.vec.Vec T) :
    queue_remaining.vec_len notes = ok (alloc.vec.Vec.len notes) := rfl

theorem vec_len_spec {T : Type} (notes : alloc.vec.Vec T) :
    queue_remaining.vec_len notes ⦃ len => len.val = notes.val.length ⦄ := by
  apply exists_imp_spec
  refine ⟨alloc.vec.Vec.len notes, rfl, ?_⟩
  simp only [alloc.vec.Vec.len_val]

theorem remaining_scalar_eq (len : Usize) (converted : Option Usize) :
    queue_remaining.remaining_scalar len converted = ok (remainingScalar len converted) := by
  simp only [queue_remaining.remaining_scalar, remainingScalar, lift, bind_tc_ok]

theorem remaining_scalar_val (len : Usize) (converted : Option Usize) :
    queue_remaining.remaining_scalar len converted ⦃ out =>
      out.val = remainingFacadeValue len converted ⦄ := by
  apply exists_imp_spec
  exact ⟨remainingScalar len converted, remaining_scalar_eq len converted,
    remainingScalar_val len converted⟩

theorem head_fuel_scalar_eq (len : Usize) (converted : Option Usize) :
    queue_remaining.head_fuel_scalar len converted = headFuelScalar len converted := by
  simp only [queue_remaining.head_fuel_scalar, headFuelScalar, remaining_scalar_eq,
    bind_tc_ok]

theorem head_fuel_scalar_spec (len : Usize) (converted : Option Usize)
    (headroom : remainingFacadeValue len converted + 1 ≤ Usize.max) :
    queue_remaining.head_fuel_scalar len converted ⦃ out =>
      out.val = remainingFacadeValue len converted + 1 ⦄ := by
  apply exists_imp_spec
  obtain ⟨fuel, hrun, hval⟩ := headFuelScalar_spec len converted headroom
  exact ⟨fuel, by rw [head_fuel_scalar_eq]; exact hrun, hval⟩

theorem head_fuel_scalar_none_spec (len : Usize) :
    queue_remaining.head_fuel_scalar len none ⦃ out => out.val = 1 ⦄ := by
  apply exists_imp_spec
  obtain ⟨fuel, hrun, hval⟩ := headFuelScalar_none_spec len
  exact ⟨fuel, by rw [head_fuel_scalar_eq]; exact hrun, hval⟩

end Verification.QueueRemainingExtractedProof
