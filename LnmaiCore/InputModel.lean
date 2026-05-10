/-
  Per-frame input model and per-zone note queues.
  Simplified to use List instead of Array for ease of verification.
-/

import LnmaiCore.Types
import LnmaiCore.Constants
import LnmaiCore.Lifecycle

namespace LnmaiCore.InputModel

open Constants

----------------------------------------------------------------------------
-- Frame Input (read-only snapshot for one frame)
----------------------------------------------------------------------------

structure FrameInput where
  buttonClicked : List Bool := List.replicate BUTTON_ZONE_COUNT false
  buttonHeld    : List Bool := List.replicate BUTTON_ZONE_COUNT false
  sensorClicked : List Bool := List.replicate SENSOR_AREA_COUNT false
  sensorHeld    : List Bool := List.replicate SENSOR_AREA_COUNT false
  buttonClickCount : List Nat := List.replicate BUTTON_ZONE_COUNT 0
  sensorClickCount : List Nat := List.replicate SENSOR_AREA_COUNT 0
  deltaSec      : Float := 0.0
deriving Inhabited

inductive TimedInputKind where
  | ButtonClick | ButtonHold | SensorClick | SensorHold
deriving Inhabited, Repr, DecidableEq

structure TimedInputEvent where
  atSec   : Float
  kind    : TimedInputKind
  index   : Nat
  isDown  : Bool := true
deriving Inhabited, Repr

structure TimedInputBatch where
  currentSec : Float := 0.0
  events     : List TimedInputEvent := []
deriving Inhabited, Repr

/-- Get nth element, default false -/
def FrameInput.getButtonHeld (fi : FrameInput) (i : Nat) : Bool :=
  (fi.buttonHeld[i]?).getD false

def FrameInput.getButtonClicked (fi : FrameInput) (i : Nat) : Bool :=
  (fi.buttonClicked[i]?).getD false

def FrameInput.getSensorHeld (fi : FrameInput) (i : Nat) : Bool :=
  (fi.sensorHeld[i]?).getD false

def FrameInput.getSensorClicked (fi : FrameInput) (i : Nat) : Bool :=
  (fi.sensorClicked[i]?).getD false

def FrameInput.getButtonClickCount (fi : FrameInput) (i : Nat) : Nat :=
  let count := (fi.buttonClickCount[i]?).getD 0
  if count > 0 then count else if fi.getButtonClicked i then 1 else 0

def FrameInput.getSensorClickCount (fi : FrameInput) (i : Nat) : Nat :=
  let count := (fi.sensorClickCount[i]?).getD 0
  if count > 0 then count else if fi.getSensorClicked i then 1 else 0

private def setBoolAt : List Bool → Nat → Bool → List Bool
  | [], _, _ => []
  | _ :: rest, 0, value => value :: rest
  | x :: rest, index+1, value => x :: setBoolAt rest index value

def TimedInputBatch.toFrameInput (batch : TimedInputBatch) (deltaSec : Float)
    (prevButtonHeld : List Bool := List.replicate BUTTON_ZONE_COUNT false)
    (prevSensorHeld : List Bool := List.replicate SENSOR_AREA_COUNT false) : FrameInput :=
  let withinFrame (evt : TimedInputEvent) : Bool :=
    evt.atSec > batch.currentSec - deltaSec && evt.atSec ≤ batch.currentSec
  let buttonClicked :=
    (List.range BUTTON_ZONE_COUNT).map (fun index =>
      batch.events.any (fun evt => withinFrame evt && evt.kind = .ButtonClick && evt.index = index))
  let sensorClicked :=
    (List.range SENSOR_AREA_COUNT).map (fun index =>
      batch.events.any (fun evt => withinFrame evt && evt.kind = .SensorClick && evt.index = index))
  let buttonHeld :=
    batch.events.foldl (fun held evt =>
      if evt.kind = .ButtonHold then setBoolAt held evt.index evt.isDown else held) prevButtonHeld
  let sensorHeld :=
    batch.events.foldl (fun held evt =>
      if evt.kind = .SensorHold then setBoolAt held evt.index evt.isDown else held) prevSensorHeld
  let buttonClickCount :=
    (List.range BUTTON_ZONE_COUNT).map (fun index =>
      batch.events.foldl (fun acc evt => if withinFrame evt && evt.kind = .ButtonClick && evt.index = index then acc + 1 else acc) 0)
  let sensorClickCount :=
    (List.range SENSOR_AREA_COUNT).map (fun index =>
      batch.events.foldl (fun acc evt => if withinFrame evt && evt.kind = .SensorClick && evt.index = index then acc + 1 else acc) 0)
  { buttonClicked := buttonClicked
  , buttonHeld := buttonHeld
  , sensorClicked := sensorClicked
  , sensorHeld := sensorHeld
  , buttonClickCount := buttonClickCount
  , sensorClickCount := sensorClickCount
  , deltaSec := deltaSec }

----------------------------------------------------------------------------
-- Per-Zone Note Queues
----------------------------------------------------------------------------

structure ZoneQueue (α : Type) where
  notes        : List α
  currentIndex : Nat := 0
deriving Inhabited, Repr

def ZoneQueue.isEmpty (q : ZoneQueue α) : Bool :=
  q.currentIndex ≥ q.notes.length

def ZoneQueue.peek (q : ZoneQueue α) : Option α :=
  q.notes[q.currentIndex]?

def ZoneQueue.advance (q : ZoneQueue α) : ZoneQueue α :=
  { q with currentIndex := q.currentIndex + 1 }

----------------------------------------------------------------------------
-- Game State (held across frames)
----------------------------------------------------------------------------

structure GameState where
  currentTime   : Float := 0.0
  prevButton    : List Bool := List.replicate BUTTON_ZONE_COUNT false
  prevSensor    : List Bool := List.replicate SENSOR_AREA_COUNT false
  tapQueues     : List (ZoneQueue Lifecycle.TapNote) := List.replicate BUTTON_ZONE_COUNT { notes := [] }
  holdQueues    : List (ZoneQueue Lifecycle.HoldNote) := List.replicate BUTTON_ZONE_COUNT { notes := [] }
  touchQueues   : List (ZoneQueue Lifecycle.TouchNote) := List.replicate SENSOR_AREA_COUNT { notes := [] }
  slides        : List Lifecycle.SlideNote := []
  activeHolds   : List (Nat × Lifecycle.HoldNote) := []
  activeTouchHolds : List (Nat × Lifecycle.HoldNote) := []
  currentBatch  : TimedInputBatch := {}
  score         : ScoreState := {}
  judgeStyle    : JudgeStyle := JudgeStyle.Default
deriving Inhabited, Repr

end LnmaiCore.InputModel
