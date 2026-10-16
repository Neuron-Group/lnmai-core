import Verification.Bridge
import LnmaiCore.Scheduler

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 800000

namespace Verification.SchedulerClickProof
open Bridge LnmaiCore
open private tryUseButtonClickAt tryUseSensorClickAt hasUnusedButtonClickAt
  hasUnusedSensorClickAt hasUnusedTouchInputAt consumeButtonThenFallbackSensor
  tapFamilyQueueHasUnusedClick
  from LnmaiCore.Scheduler

/-- Runtime click consumption for one note. The extracted kernel decides which
    counter is spent; the caller supplies its own comparisons and performs the
    increments with its unbounded counters. -/
def consumeClickRuntime (input : InputModel.FrameInput) (cursor : Scheduler.ClickCursor)
    (allowInput : Bool) (zone : ButtonZone) (sensorArea : SensorArea) :
    Result (Bool × Bool × Scheduler.ClickCursor) := do
  let plan ← scheduler.consume_click allowInput
    (decide (cursor.buttonUsed.getD zone 0 < input.getButtonClickCount zone))
    (decide (cursor.sensorUsed.getD sensorArea 0 < input.getSensorClickCount sensorArea))
  ok (plan.used_button, plan.used_sensor,
    { cursor with
      buttonUsed :=
        if plan.used_button then
          cursor.buttonUsed.set zone (cursor.buttonUsed.getD zone 0 + 1)
        else cursor.buttonUsed
      sensorUsed :=
        if plan.used_sensor then
          cursor.sensorUsed.set sensorArea (cursor.sensorUsed.getD sensorArea 0 + 1)
        else cursor.sensorUsed })

/-- The runtime accepts a touch-group click through the sensor or the button
    ring mapped from that sensor area. -/
def touchInputRuntime (input : InputModel.FrameInput) (cursor : Scheduler.ClickCursor)
    (area : SensorArea) (buttonRingForTouch : Bool) : Result Bool := do
  let ringAvailable :=
    match area.toOuterButtonZone? with
    | some zone => decide (cursor.buttonUsed.getD zone 0 < input.getButtonClickCount zone)
    | none => false
  let available ← scheduler.touch_input_available buttonRingForTouch
    (decide (cursor.sensorUsed.getD area 0 < input.getSensorClickCount area))
    (some ringAvailable)
  ok available

/-- Button-before-sensor ownership, the suppression of the fallback after a
    consumed button, and the counter updates agree with the reference. -/
theorem consume_click_equiv (input : InputModel.FrameInput) (cursor : Scheduler.ClickCursor)
    (allowInput : Bool) (zone : ButtonZone) (sensorArea : SensorArea) :
    consumeClickRuntime input cursor allowInput zone sensorArea =
      ok (consumeButtonThenFallbackSensor input cursor allowInput zone sensorArea) := by
  unfold consumeClickRuntime consumeButtonThenFallbackSensor
  unfold tryUseButtonClickAt tryUseSensorClickAt
  simp [scheduler.consume_click, bind_tc_ok] <;> split_ifs <;> simp_all

/-- The tap family's queue test uses the same button/sensor availability. -/
theorem tap_queue_click_equiv (input : InputModel.FrameInput) (cursor : Scheduler.ClickCursor)
    (zone : ButtonZone) (fallbackArea : SensorArea) :
    scheduler.tap_queue_has_unused_click
        (decide (cursor.buttonUsed.getD zone 0 < input.getButtonClickCount zone))
        (decide (cursor.sensorUsed.getD fallbackArea 0 < input.getSensorClickCount fallbackArea)) =
      ok (tapFamilyQueueHasUnusedClick input cursor zone fallbackArea) := by
  simp [scheduler.tap_queue_has_unused_click, tapFamilyQueueHasUnusedClick,
    hasUnusedButtonClickAt, hasUnusedSensorClickAt] <;> split_ifs <;> simp_all

/-- Touch-group availability agrees, including the absent outer button ring. -/
theorem touch_input_equiv (input : InputModel.FrameInput) (cursor : Scheduler.ClickCursor)
    (area : SensorArea) (buttonRingForTouch : Bool) :
    touchInputRuntime input cursor area buttonRingForTouch =
      ok (hasUnusedTouchInputAt input cursor area buttonRingForTouch) := by
  cases hzone : area.toOuterButtonZone? with
  | none =>
      simp [touchInputRuntime, hasUnusedTouchInputAt, hasUnusedSensorClickAt,
        scheduler.touch_input_available, hzone, bind_tc_ok] <;> split_ifs <;> simp_all
  | some zone =>
      simp [touchInputRuntime, hasUnusedTouchInputAt, hasUnusedSensorClickAt,
        hasUnusedButtonClickAt, scheduler.touch_input_available, hzone, bind_tc_ok] <;>
        split_ifs <;> simp_all

end Verification.SchedulerClickProof
