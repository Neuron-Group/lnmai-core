import LnmaiCore.Lifecycle
import Batteries.Tactic.OpenPrivate
import Mathlib.Data.List.Range

namespace Verification.SlideAreaProof
open private slideAreaTargetHistory slideAreaTargetHistory.go sensorHeldAt
  from LnmaiCore.Lifecycle

/-- One pair of latched flags, indexed in the original restored histories. -/
def nextHistory (wasOn wasOff : List Bool) (held : LnmaiCore.SensorVec Bool)
    (entry : LnmaiCore.SensorArea × Nat) : Bool × Bool :=
  let oldOn := (wasOn[entry.2]?).getD false
  let oldOff := (wasOff[entry.2]?).getD false
  let isHeld := held.getD entry.1 false
  (oldOn || isHeld, oldOff || (oldOn && !isHeld))

def historyPairs (targets : List LnmaiCore.SensorArea) (wasOn wasOff : List Bool)
    (held : LnmaiCore.SensorVec Bool) (start : Nat := 0) : List (Bool × Bool) :=
  (targets.zipIdx start).map (nextHistory wasOn wasOff held)

theorem target_history_model_from (targets : List LnmaiCore.SensorArea)
    (wasOn wasOff : List Bool) (held : LnmaiCore.SensorVec Bool) (start : Nat) :
    slideAreaTargetHistory.go held targets (wasOn.drop start) (wasOff.drop start) =
      ((historyPairs targets wasOn wasOff held start).map Prod.fst,
       (historyPairs targets wasOn wasOff held start).map Prod.snd) := by
  induction targets generalizing start with
  | nil => simp [slideAreaTargetHistory.go, historyPairs]
  | cons target rest ih =>
      simp [slideAreaTargetHistory.go, historyPairs, List.drop_drop, ih,
        nextHistory, sensorHeldAt]

theorem target_history_model (targets : List LnmaiCore.SensorArea)
    (wasOn wasOff : List Bool) (held : LnmaiCore.SensorVec Bool) :
    slideAreaTargetHistory targets wasOn wasOff held =
      ((historyPairs targets wasOn wasOff held).map Prod.fst,
       (historyPairs targets wasOn wasOff held).map Prod.snd) := by
  simpa [slideAreaTargetHistory] using
    target_history_model_from targets wasOn wasOff held 0

end Verification.SlideAreaProof

