import Verification.Bridge
import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate

namespace Verification.QueueMaintenanceModel
open LnmaiCore InputModel Lifecycle
open private normalizeHoldQueueCursorFuel normalizeHoldQueueCursor
  normalizeTouchQueueCursorFuel normalizeTouchQueueCursor
  holdQueueResolved touchQueueResolved replaceHoldQueueNote replaceTouchQueueNote
  from LnmaiCore.Scheduler

/-!
The two original cursor-normalization recursions have identical structure.
This generic equation is proved equal to each original below; it is not an
assumption about a complete runtime queue operation. Indices remain Nat,
including values beyond the end of the list.
-/

def remaining (queue : ZoneQueue α) : Nat :=
  queue.notes.length - queue.currentIndex

def normalizeFuel (resolved : α → Bool) : Nat → ZoneQueue α → ZoneQueue α
  | fuel, queue =>
      match queue.peek with
      | none => queue
      | some note =>
          match fuel with
          | 0 => queue
          | fuel + 1 =>
              if resolved note then normalizeFuel resolved fuel queue.advance else queue

theorem hold_fuel_eq (fuel : Nat) (queue : ZoneQueue HoldNote) :
    normalizeFuel holdQueueResolved fuel queue = normalizeHoldQueueCursorFuel fuel queue := by
  induction fuel generalizing queue with
  | zero =>
      unfold normalizeFuel normalizeHoldQueueCursorFuel
      cases queue.peek <;> rfl
  | succ fuel ih =>
      conv => lhs; unfold normalizeFuel
      conv => rhs; unfold normalizeHoldQueueCursorFuel
      cases hp : queue.peek with
      | none => rfl
      | some note =>
          dsimp only
          split <;> first | exact ih _ | rfl

theorem touch_fuel_eq (fuel : Nat) (queue : ZoneQueue TouchNote) :
    normalizeFuel touchQueueResolved fuel queue = normalizeTouchQueueCursorFuel fuel queue := by
  induction fuel generalizing queue with
  | zero =>
      unfold normalizeFuel normalizeTouchQueueCursorFuel
      cases queue.peek <;> rfl
  | succ fuel ih =>
      conv => lhs; unfold normalizeFuel
      conv => rhs; unfold normalizeTouchQueueCursorFuel
      cases hp : queue.peek with
      | none => rfl
      | some note =>
          dsimp only
          split <;> first | exact ih _ | rfl

theorem remaining_zero_peek (queue : ZoneQueue α)
    (h : remaining queue = 0) : queue.peek = none := by
  apply List.getElem?_eq_none
  dsimp only [remaining] at h
  omega

theorem remaining_advance (queue : ZoneQueue α) :
    remaining queue.advance = remaining queue - 1 := by
  simp only [remaining, ZoneQueue.advance]
  omega

theorem peek_remaining_positive (queue : ZoneQueue α) (note : α)
    (h : queue.peek = some note) : 0 < remaining queue := by
  by_contra hn
  have hempty := remaining_zero_peek queue (by omega)
  rw [hempty] at h
  contradiction

theorem normalize_empty (resolved : α → Bool) (fuel : Nat) (queue : ZoneQueue α)
    (hempty : queue.peek = none) : normalizeFuel resolved fuel queue = queue := by
  unfold normalizeFuel
  rw [hempty]

theorem normalize_zero (resolved : α → Bool) (queue : ZoneQueue α) :
    normalizeFuel resolved 0 queue = queue := by
  unfold normalizeFuel
  cases queue.peek <;> rfl

/-- Once every pending slot can be visited, additional fuel has no effect.
The machine wrapper therefore does not need a potentially overflowing +1. -/
theorem fuel_irrelevant (resolved : α → Bool) (fuel other : Nat) (queue : ZoneQueue α)
    (hfuel : remaining queue ≤ fuel) (hother : remaining queue ≤ other) :
    normalizeFuel resolved fuel queue = normalizeFuel resolved other queue := by
  induction fuel generalizing other queue with
  | zero =>
      have hempty := remaining_zero_peek queue (by omega)
      rw [normalize_empty resolved _ queue hempty, normalize_empty resolved _ queue hempty]
  | succ fuel ih =>
      cases hp : queue.peek with
      | none => rw [normalize_empty resolved _ queue hp, normalize_empty resolved _ queue hp]
      | some note =>
          have hpositive := peek_remaining_positive queue note hp
          cases other with
          | zero => omega
          | succ other =>
              conv => lhs; unfold normalizeFuel
              conv => rhs; unfold normalizeFuel
              simp only [hp]
              split
              · apply ih <;> rw [remaining_advance] <;> omega
              · rfl

theorem hold_pending_equiv (queue : ZoneQueue HoldNote) :
    normalizeFuel holdQueueResolved (remaining queue) queue = normalizeHoldQueueCursor queue := by
  rw [fuel_irrelevant holdQueueResolved (remaining queue) (remaining queue + 1) queue
    (Nat.le_refl _) (by omega)]
  exact hold_fuel_eq _ queue

theorem touch_pending_equiv (queue : ZoneQueue TouchNote) :
    normalizeFuel touchQueueResolved (remaining queue) queue = normalizeTouchQueueCursor queue := by
  rw [fuel_irrelevant touchQueueResolved (remaining queue) (remaining queue + 1) queue
    (Nat.le_refl _) (by omega)]
  exact touch_fuel_eq _ queue

theorem normalize_notes (resolved : α → Bool) (fuel : Nat) (queue : ZoneQueue α) :
    (normalizeFuel resolved fuel queue).notes = queue.notes := by
  induction fuel generalizing queue with
  | zero => rw [normalize_zero]
  | succ fuel ih =>
      cases hp : queue.peek with
      | none => rw [normalize_empty resolved _ queue hp]
      | some note =>
          unfold normalizeFuel
          simp only [hp]
          split
          · exact ih queue.advance
          · rfl

/-- Every matching identifier is replaced, including duplicate identifiers
in restored input; this is intentionally not a first-match operation. -/
def replace (id : α → Nat) (queue : ZoneQueue α) (note : α) : ZoneQueue α :=
  { queue with notes := queue.notes.map (fun existing =>
      if id existing == id note then note else existing) }

theorem replace_hold_eq (queue : ZoneQueue HoldNote) (note : HoldNote) :
    replace (fun n => n.params.noteIndex) queue note = replaceHoldQueueNote queue note := rfl

theorem replace_touch_eq (queue : ZoneQueue TouchNote) (note : TouchNote) :
    replace (fun n => n.params.noteIndex) queue note = replaceTouchQueueNote queue note := rfl

theorem replace_preserves_index (id : α → Nat) (queue : ZoneQueue α) (note : α) :
    (replace id queue note).currentIndex = queue.currentIndex := rfl

theorem replace_preserves_length (id : α → Nat) (queue : ZoneQueue α) (note : α) :
    (replace id queue note).notes.length = queue.notes.length := by
  simp [replace]

end Verification.QueueMaintenanceModel
