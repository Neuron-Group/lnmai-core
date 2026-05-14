/-
  Per-frame input model and per-zone note queues.
  Simplified to use List instead of Array for ease of verification.
-/

import LnmaiCore.Types
import LnmaiCore.Areas
import LnmaiCore.Storage
import LnmaiCore.Constants
import LnmaiCore.Lifecycle
import LnmaiCore.Time

namespace LnmaiCore.InputModel

open Constants

----------------------------------------------------------------------------
-- Frame Input (read-only snapshot for one frame)
----------------------------------------------------------------------------

structure FrameInput where
  buttonClicked : ButtonVec Bool := ButtonVec.replicate BUTTON_ZONE_COUNT false
  buttonHeld    : ButtonVec Bool := ButtonVec.replicate BUTTON_ZONE_COUNT false
  sensorClicked : SensorVec Bool := SensorVec.replicate SENSOR_AREA_COUNT false
  sensorHeld    : SensorVec Bool := SensorVec.replicate SENSOR_AREA_COUNT false
  buttonClickCount : ButtonVec Nat := ButtonVec.replicate BUTTON_ZONE_COUNT 0
  sensorClickCount : SensorVec Nat := SensorVec.replicate SENSOR_AREA_COUNT 0
  delta         : Duration := Duration.zero
deriving Inhabited

inductive TimedInputEvent where
  | buttonClick (tp : TimePoint) (zone : ButtonZone)
  | buttonHold (tp : TimePoint) (zone : ButtonZone) (isDown : Bool := true)
  | sensorClick (tp : TimePoint) (area : SensorArea)
  | sensorHold (tp : TimePoint) (area : SensorArea) (isDown : Bool := true)
deriving Inhabited, Repr

def TimedInputEvent.at : TimedInputEvent → TimePoint
  | .buttonClick tp _ => tp
  | .buttonHold tp _ _ => tp
  | .sensorClick tp _ => tp
  | .sensorHold tp _ _ => tp

structure TimedInputBatch where
  currentTime : TimePoint := TimePoint.zero
  events     : List TimedInputEvent := []
deriving Inhabited, Repr

def FrameInput.getButtonHeld (fi : FrameInput) (zone : ButtonZone) : Bool :=
  fi.buttonHeld.getD zone false

def FrameInput.getButtonHeldList (fi : FrameInput) : List Bool :=
  fi.buttonHeld.toList

def FrameInput.getButtonClicked (fi : FrameInput) (zone : ButtonZone) : Bool :=
  fi.buttonClicked.getD zone false

def FrameInput.getSensorHeld (fi : FrameInput) (area : SensorArea) : Bool :=
  fi.sensorHeld.getD area false

def FrameInput.getSensorHeldList (fi : FrameInput) : List Bool :=
  fi.sensorHeld.toList

def FrameInput.getSensorClicked (fi : FrameInput) (area : SensorArea) : Bool :=
  fi.sensorClicked.getD area false

def FrameInput.getButtonClickCount (fi : FrameInput) (zone : ButtonZone) : Nat :=
  let count := fi.buttonClickCount.getD zone 0
  if count > 0 then count else if fi.getButtonClicked zone then 1 else 0

def FrameInput.getSensorClickCount (fi : FrameInput) (area : SensorArea) : Nat :=
  let count := fi.sensorClickCount.getD area 0
  if count > 0 then count else if fi.getSensorClicked area then 1 else 0

def prevSensorHeldAt (prevSensor : SensorVec Bool) (area : SensorArea) : Bool :=
  prevSensor.getD area false

def TimedInputBatch.toFrameInput (batch : TimedInputBatch) (delta : Duration)
    (prevButtonHeld : ButtonVec Bool := ButtonVec.replicate BUTTON_ZONE_COUNT false)
    (prevSensorHeld : SensorVec Bool := SensorVec.replicate SENSOR_AREA_COUNT false) : FrameInput :=
  let withinFrame (evt : TimedInputEvent) : Bool :=
    evt.at > batch.currentTime - delta && evt.at ≤ batch.currentTime
  let initial : FrameInput :=
    { buttonHeld := prevButtonHeld
    , sensorHeld := prevSensorHeld
    , delta := delta }
  let acc :=
    batch.events.foldl (fun fi evt =>
      match evt with
      | .buttonClick tp zone =>
          if withinFrame (.buttonClick tp zone) then
            { fi with
              buttonClicked := fi.buttonClicked.set zone true
            , buttonClickCount := fi.buttonClickCount.set zone (fi.buttonClickCount.getD zone 0 + 1) }
          else
            fi
      | .sensorClick tp area =>
          if withinFrame (.sensorClick tp area) then
            { fi with
              sensorClicked := fi.sensorClicked.set area true
            , sensorClickCount := fi.sensorClickCount.set area (fi.sensorClickCount.getD area 0 + 1) }
          else
            fi
      | .buttonHold _ zone isDown =>
          { fi with buttonHeld := fi.buttonHeld.set zone isDown }
      | .sensorHold _ area isDown =>
          { fi with sensorHeld := fi.sensorHeld.set area isDown }) initial
  { acc with delta := delta }

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

abbrev ButtonQueueVec (α : Type) := ButtonVec (ZoneQueue α)

abbrev SensorQueueVec (α : Type) := SensorVec (ZoneQueue α)

def ButtonQueueVec.replicate (n : Nat) (queue : ZoneQueue α) : ButtonQueueVec α :=
  ButtonVec.replicate n queue

def SensorQueueVec.replicate (n : Nat) (queue : ZoneQueue α) : SensorQueueVec α :=
  SensorVec.replicate n queue

def buttonQueueAt {α : Type} (queues : ButtonQueueVec α) (zone : ButtonZone) : ZoneQueue α :=
  queues.getD zone { notes := [] }

def sensorQueueAt {α : Type} (queues : SensorQueueVec α) (area : SensorArea) : ZoneQueue α :=
  queues.getD area { notes := [] }

def setButtonQueueAt {α : Type} (queues : ButtonQueueVec α) (zone : ButtonZone) (queue : ZoneQueue α) : ButtonQueueVec α :=
  queues.set zone queue

def setSensorQueueAt {α : Type} (queues : SensorQueueVec α) (area : SensorArea) (queue : ZoneQueue α) : SensorQueueVec α :=
  queues.set area queue

----------------------------------------------------------------------------
-- Game State (held across frames)
----------------------------------------------------------------------------

structure GameState where
  currentTime   : TimePoint := TimePoint.zero
  prevButton    : ButtonVec Bool := ButtonVec.replicate BUTTON_ZONE_COUNT false
  prevSensor    : SensorVec Bool := SensorVec.replicate SENSOR_AREA_COUNT false
  tapQueues     : ButtonQueueVec Lifecycle.TapNote := ButtonQueueVec.replicate BUTTON_ZONE_COUNT { notes := [] }
  holdQueues    : ButtonQueueVec Lifecycle.HoldNote := ButtonQueueVec.replicate BUTTON_ZONE_COUNT { notes := [] }
  touchHoldQueues : SensorQueueVec Lifecycle.HoldNote := SensorQueueVec.replicate SENSOR_AREA_COUNT { notes := [] }
  touchQueues   : SensorQueueVec Lifecycle.TouchNote := SensorQueueVec.replicate SENSOR_AREA_COUNT { notes := [] }
  slides        : List Lifecycle.SlideNote := []
  activeHolds   : List (ButtonZone × Lifecycle.HoldNote) := []
  activeTouchHolds : List (SensorArea × Lifecycle.HoldNote) := []
  touchGroupStates : List GroupState := []
  touchHoldGroupStates : List GroupState := []
  currentBatch  : TimedInputBatch := {}
  score         : ScoreState := {}
  judgeStyle    : JudgeStyle := JudgeStyle.Default
  touchPanelOffset : Duration := Duration.zero
  useButtonRingForTouch : Bool := Constants.USE_BUTTON_RING_FOR_TOUCH
  subdivideSlideJudgeGrade : Bool := Constants.SUBDIVIDE_SLIDE_JUDGE_GRADE
deriving Inhabited, Repr

end LnmaiCore.InputModel
