import Verification.TapSchedule
import Verification.TouchSchedule

namespace Verification.SchedulerCounterLocalProof

open LnmaiCore

/-!
Arithmetic for the guarded counter writes in the actual Tap/Touch branches.
The runtime fields are Natural(BigUint), not usize. The parameter bound is a
local bound on the supplied input count; no global bound on all cursor values
is assumed. These lemmas do not prove BigUint encoding or allocation success.
-/

/-- An available click can be spent without exceeding the already supplied
count, including when the supplied count equals a chosen representation bound. -/
theorem successor_le_supply (used supplied : Nat)
    (available : decide (used < supplied) = true) : used + 1 ≤ supplied := by
  have hlt : used < supplied := by
    simpa only [decide_eq_true_eq] using available
  omega

/-- Only the actual input count needs to fit the chosen scalar/value bound. -/
theorem successor_le_bound (used supplied bound : Nat)
    (available : decide (used < supplied) = true) (supplied_fits : supplied ≤ bound) :
    used + 1 ≤ bound :=
  Nat.le_trans (successor_le_supply used supplied available) supplied_fits

theorem unavailable_of_supply_le_used (used supplied : Nat)
    (h : supplied ≤ used) : decide (used < supplied) = false := by
  have hnot : ¬ used < supplied := by omega
  simp [hnot]

/-- For a bounded representation the maximum case takes the no-write branch.
An inhabited maximum witness matters: boundedness alone could be vacuous. -/
theorem unavailable_at_bound (used supplied bound : Nat)
    (at_bound : used = bound) (supplied_fits : supplied ≤ bound) :
    decide (used < supplied) = false := by
  apply unavailable_of_supply_le_used used supplied
  omega

/-- Exact obstruction for a hypothetical fixed-width counter facade.
This does not identify Natural(BigUint) with such a bounded representation. -/
theorem no_bounded_total_successor {C : Type} (value : C → Nat) (bound : Nat)
    (all_fit : ∀ c, value c ≤ bound) (maximum : ∃ c, value c = bound) :
    ¬ (∀ c, ∃ next, value next = value c + 1) := by
  intro total
  obtain ⟨c, hc⟩ := maximum
  obtain ⟨next, hn⟩ := total c
  have hfit := all_fit next
  omega

/-- The Tap point-law guard supplies the strict inequality directly. -/
theorem tap_button_successor_le_bound (ctx : TapScheduleModel.Context)
    (cursor : Scheduler.ClickCursor) (bound : Nat)
    (available : TapScheduleProof.buttonAvailable ctx cursor = true)
    (supplied_fits : ctx.input.getButtonClickCount ctx.zone ≤ bound) :
    cursor.buttonUsed.getD ctx.zone 0 + 1 ≤ bound := by
  exact successor_le_bound _ _ bound available supplied_fits

theorem tap_sensor_successor_le_bound (ctx : TapScheduleModel.Context)
    (cursor : Scheduler.ClickCursor) (area : SensorArea) (bound : Nat)
    (available : TapScheduleProof.sensorAvailable ctx cursor area = true)
    (supplied_fits : ctx.input.getSensorClickCount area ≤ bound) :
    cursor.sensorUsed.getD area 0 + 1 ≤ bound := by
  exact successor_le_bound _ _ bound available supplied_fits

theorem touch_button_successor_le_bound (ctx : TouchScheduleModel.Context)
    (cursor : Scheduler.ClickCursor) (zone : ButtonZone) (bound : Nat)
    (available : TouchScheduleProof.buttonAvailable ctx cursor zone = true)
    (supplied_fits : ctx.input.getButtonClickCount zone ≤ bound) :
    cursor.buttonUsed.getD zone 0 + 1 ≤ bound := by
  exact successor_le_bound _ _ bound available supplied_fits

theorem touch_sensor_successor_le_bound (ctx : TouchScheduleModel.Context)
    (cursor : Scheduler.ClickCursor) (area : SensorArea) (bound : Nat)
    (available : TouchScheduleProof.sensorAvailable ctx cursor area = true)
    (supplied_fits : ctx.input.getSensorClickCount area ≤ bound) :
    cursor.sensorUsed.getD area 0 + 1 ≤ bound := by
  exact successor_le_bound _ _ bound available supplied_fits

end Verification.SchedulerCounterLocalProof
