/-
  Proof scaffolding for the “inner-screen unfair” slide configuration.

  This file is intentionally theorem-oriented: it states small lemmas about
  slide-area completion and a simplified queue-exposure model that matches the
  area-jump intuition used by the reference behavior.
-/

import LnmaiCore.Lifecycle

namespace LnmaiCore.Proofs.InnerScreenUnfair

open LnmaiCore.Constants
open LnmaiCore.Lifecycle

----------------------------------------------------------------------------
-- Local model: queue exposure under a skip-friendly front area
----------------------------------------------------------------------------

def exposeFront (queue : SlideQueue) : SlideQueue :=
  match queue with
  | [] => []
  | first :: second :: rest =>
      if first.isSkippable || first.on then
        second :: rest
      else
        first :: second :: rest
  | [first] => [first]

def InnerScreenUnfairWitness (queue : SlideQueue) : Prop :=
  match queue with
  | first :: second :: _ => first.isSkippable = true ∧ !first.isLast ∧ second.isLast = false
  | _ => False

----------------------------------------------------------------------------
-- Lemmas about slide-area completion
----------------------------------------------------------------------------

theorem slideArea_isFinished_of_last_on (area : SlideArea) (hLast : area.isLast = true) (hOn : area.wasOn = true) :
    area.isFinished = true := by
  simp [SlideArea.isFinished, hLast, hOn]

theorem slideArea_isFinished_of_nonlast_on_and_off (area : SlideArea) (hLast : area.isLast = false)
    (hOn : area.wasOn = true) (hOff : area.wasOff = true) :
    area.isFinished = true := by
  simp [SlideArea.isFinished, hLast, hOn, hOff]

theorem slideArea_notFinished_of_never_on (area : SlideArea) (hOn : area.wasOn = false) :
    area.isFinished = false := by
  simp [SlideArea.isFinished, hOn]

----------------------------------------------------------------------------
-- Lemmas about the exposure step
----------------------------------------------------------------------------

theorem exposeFront_nil : exposeFront [] = [] := by
  rfl

theorem exposeFront_singleton (area : SlideArea) : exposeFront [area] = [area] := by
  simp [exposeFront]

theorem exposeFront_skippable_front
    (first second : SlideArea) (rest : SlideQueue)
    (hSkip : first.isSkippable = true) :
    exposeFront (first :: second :: rest) = second :: rest := by
  simp [exposeFront, hSkip]

theorem exposeFront_on_front
    (first second : SlideArea) (rest : SlideQueue)
    (hOn : first.on = true) :
    exposeFront (first :: second :: rest) = second :: rest := by
  simp [exposeFront, hOn]

theorem exposeFront_preserves_tail_length
    (first second : SlideArea) (rest : SlideQueue)
    (hSkip : first.isSkippable = true) :
    (exposeFront (first :: second :: rest)).length = (second :: rest).length := by
  simp [exposeFront_skippable_front, hSkip]

----------------------------------------------------------------------------
-- A compact proposition for “inner-screen unfair” configurations
----------------------------------------------------------------------------

def hasInnerScreenUnfairShape (queue : SlideQueue) : Prop :=
  match queue with
  | first :: second :: rest =>
      first.isSkippable = true ∧ !first.isLast ∧ second.isLast = false ∧
        second.isFinished = false ∧ rest.length ≥ 0
  | _ => False

theorem innerScreenUnfairShape_implies_exposable
    (queue : SlideQueue) (h : hasInnerScreenUnfairShape queue) :
    match queue with
    | _ :: second :: rest => exposeFront queue = second :: rest
    | _ => exposeFront queue = queue := by
  cases queue with
  | nil =>
      simp [hasInnerScreenUnfairShape] at h ⊢
  | cons first tail =>
      cases tail with
      | nil =>
          simp [hasInnerScreenUnfairShape] at h ⊢
      | cons second rest =>
          simp [hasInnerScreenUnfairShape] at h ⊢
          have hSkip : first.isSkippable = true := h.left
          simp [exposeFront, hSkip]

theorem innerScreenUnfairShape_isWitness
    (queue : SlideQueue) (h : hasInnerScreenUnfairShape queue) :
    InnerScreenUnfairWitness queue := by
  cases queue with
  | nil =>
      simp [hasInnerScreenUnfairShape] at h
  | cons first tail =>
      cases tail with
      | nil =>
          simp [hasInnerScreenUnfairShape] at h
      | cons second rest =>
          simp [hasInnerScreenUnfairShape, InnerScreenUnfairWitness] at h ⊢
          exact ⟨h.left, h.right.left, h.right.right.left⟩

----------------------------------------------------------------------------
-- Concrete witness: “Ghost Tokyo” 1s5 inner-screen unfair fragment
----------------------------------------------------------------------------

def ghostTokyoSensorNone : List Bool :=
  [false, false, false, false, false, false, false, false, false, false, false, false]

def ghostTokyoSensorA1 : List Bool :=
  [false, true, false, false, false, false, false, false, false, false, false, false]

def ghostTokyoSensorA5 : List Bool :=
  [false, false, false, false, false, true, false, false, false, false, false, false]

def ghostTokyo1s5_A1Pending : SlideArea := {
  targetAreas := [1]
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 0
  arrowProgressWhenFinished := 0
  wasOn := false
  wasOff := false
}

def ghostTokyo1s5_A1On : SlideArea := {
  targetAreas := [1]
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 0
  arrowProgressWhenFinished := 0
  wasOn := true
  wasOff := false
}

def ghostTokyo1s5_A1Solved : SlideArea := {
  targetAreas := [1]
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 0
  arrowProgressWhenFinished := 0
  wasOn := true
  wasOff := true
}

def ghostTokyo1s5_B4Pending : SlideArea := {
  targetAreas := [11]
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 1
  arrowProgressWhenFinished := 1
  wasOn := false
  wasOff := false
}

def ghostTokyo1s5_A5Pending : SlideArea := {
  targetAreas := [5]
  isLast := true
  isSkippable := true
  arrowProgressWhenOn := 2
  arrowProgressWhenFinished := 2
  wasOn := false
  wasOff := false
}

def ghostTokyo1s5QueueStart : SlideQueue :=
  [ghostTokyo1s5_A1Pending, ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending]

def ghostTokyo1s5QueueAfterA1Press : SlideQueue :=
  [ghostTokyo1s5_A1On, ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending]

def ghostTokyo1s5Queue : SlideQueue :=
  [ghostTokyo1s5_A1Solved, ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending]

def ghostTokyoAdvanceExposedPair (first second : SlideArea) (sensorHeld : List Bool) : SlideQueue :=
  [first.check sensorHeld, second.check sensorHeld]

theorem ghostTokyo1s5_A1_press_marks_front_on :
    ghostTokyo1s5_A1Pending.check ghostTokyoSensorA1 = ghostTokyo1s5_A1On := by
  simp [ghostTokyo1s5_A1Pending, ghostTokyo1s5_A1On, ghostTokyoSensorA1, SlideArea.check]

theorem ghostTokyo1s5_A1_release_finishes_front :
    ghostTokyo1s5_A1On.check ghostTokyoSensorNone = ghostTokyo1s5_A1Solved := by
  simp [ghostTokyo1s5_A1On, ghostTokyo1s5_A1Solved, ghostTokyoSensorNone, SlideArea.check]

theorem ghostTokyo1s5_phased_front_resolution :
    [ghostTokyo1s5_A1Pending.check ghostTokyoSensorA1,
      ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending] = ghostTokyo1s5QueueAfterA1Press ∧
    [ghostTokyo1s5_A1On.check ghostTokyoSensorNone,
      ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending] = ghostTokyo1s5Queue := by
  constructor
  · simp [ghostTokyo1s5QueueAfterA1Press, ghostTokyo1s5_A1_press_marks_front_on]
  · simp [ghostTokyo1s5Queue, ghostTokyo1s5_A1_release_finishes_front]

theorem ghostTokyo1s5_hasInnerScreenUnfairShape :
    hasInnerScreenUnfairShape ghostTokyo1s5Queue := by
  simp [ghostTokyo1s5Queue, hasInnerScreenUnfairShape,
    ghostTokyo1s5_A1Solved, ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending,
    SlideArea.isFinished]

theorem ghostTokyo1s5_isWitness :
    InnerScreenUnfairWitness ghostTokyo1s5Queue := by
  exact innerScreenUnfairShape_isWitness ghostTokyo1s5Queue ghostTokyo1s5_hasInnerScreenUnfairShape

theorem ghostTokyo1s5_exposes_B4_after_A1 :
    exposeFront ghostTokyo1s5Queue = [ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending] := by
  exact innerScreenUnfairShape_implies_exposable ghostTokyo1s5Queue ghostTokyo1s5_hasInnerScreenUnfairShape

def ghostTokyo1s5_A5On : SlideArea := {
  targetAreas := [5]
  isLast := true
  isSkippable := true
  arrowProgressWhenOn := 2
  arrowProgressWhenFinished := 2
  wasOn := true
  wasOff := false
}

def ghostTokyo1s5FinalTapQueue : SlideQueue :=
  [ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5On]

theorem ghostTokyo1s5_A5_tap_marks_last_on :
    ghostTokyo1s5_A5Pending.check ghostTokyoSensorA5 = ghostTokyo1s5_A5On := by
  simp [ghostTokyo1s5_A5Pending, ghostTokyo1s5_A5On, ghostTokyoSensorA5, SlideArea.check]

theorem ghostTokyo1s5_finalTap_updates_exposed_pair :
    ghostTokyoAdvanceExposedPair ghostTokyo1s5_B4Pending ghostTokyo1s5_A5Pending ghostTokyoSensorA5 =
      ghostTokyo1s5FinalTapQueue := by
  simp [ghostTokyoAdvanceExposedPair, ghostTokyo1s5FinalTapQueue,
    ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending, ghostTokyo1s5_A5On,
    ghostTokyoSensorA5, SlideArea.check]

theorem ghostTokyo1s5_finalTap_exposes_A5 :
    exposeFront (ghostTokyoAdvanceExposedPair ghostTokyo1s5_B4Pending ghostTokyo1s5_A5Pending ghostTokyoSensorA5) =
      [ghostTokyo1s5_A5On] := by
  simp [ghostTokyoAdvanceExposedPair,
    ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending, ghostTokyo1s5_A5On,
    ghostTokyoSensorA5, SlideArea.check, exposeFront]

theorem ghostTokyo1s5_finalTap_finishes_A5 :
    ghostTokyo1s5_A5On.isFinished = true := by
  exact slideArea_isFinished_of_last_on ghostTokyo1s5_A5On rfl rfl

theorem ghostTokyo1s5_three_note_interaction_unfair :
    let queueAfterA1Release := ghostTokyo1s5Queue
    let queueDuringFinalTap :=
      ghostTokyoAdvanceExposedPair ghostTokyo1s5_B4Pending ghostTokyo1s5_A5Pending ghostTokyoSensorA5
    hasInnerScreenUnfairShape queueAfterA1Release ∧
      exposeFront queueAfterA1Release = [ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending] ∧
      exposeFront queueDuringFinalTap = [ghostTokyo1s5_A5On] ∧
      ghostTokyo1s5_A5On.isFinished = true := by
  simp [ghostTokyo1s5_hasInnerScreenUnfairShape, ghostTokyo1s5_exposes_B4_after_A1,
    ghostTokyo1s5_finalTap_exposes_A5, ghostTokyo1s5_finalTap_finishes_A5]

structure GhostTokyoScene where
  slide73 : SlideQueue
  slide15 : SlideQueue
  tap5Pressed : Bool

def ghostTokyo7x3_B7On : SlideArea := {
  targetAreas := [7]
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 0
  arrowProgressWhenFinished := 0
  wasOn := true
  wasOff := false
}

def ghostTokyo7x3_COn : SlideArea := {
  targetAreas := [8]
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 1
  arrowProgressWhenFinished := 1
  wasOn := true
  wasOff := false
}

def ghostTokyo7x3_B3Pending : SlideArea := {
  targetAreas := [3]
  isLast := true
  isSkippable := true
  arrowProgressWhenOn := 2
  arrowProgressWhenFinished := 2
  wasOn := false
  wasOff := false
}

def ghostTokyo7x3QueueCrossing : SlideQueue :=
  [ghostTokyo7x3_B7On, ghostTokyo7x3_COn, ghostTokyo7x3_B3Pending]

def ghostTokyoSceneAfterA1Release : GhostTokyoScene := {
  slide73 := ghostTokyo7x3QueueCrossing
  slide15 := ghostTokyo1s5Queue
  tap5Pressed := false
}

def ghostTokyoSceneDuringFinalTap : GhostTokyoScene := {
  slide73 := ghostTokyo7x3QueueCrossing
  slide15 := ghostTokyoAdvanceExposedPair ghostTokyo1s5_B4Pending ghostTokyo1s5_A5Pending ghostTokyoSensorA5
  tap5Pressed := true
}

theorem ghostTokyo7x3_crossing_not_finished :
    slideQueuesCleared [ghostTokyo7x3QueueCrossing] = false := by
  simp [ghostTokyo7x3QueueCrossing, slideQueuesCleared]

theorem ghostTokyo7x3_front_is_on :
    ghostTokyo7x3_B7On.on = true := by
  simp [ghostTokyo7x3_B7On, SlideArea.on]

theorem ghostTokyo7x3_exposes_center :
    exposeFront ghostTokyo7x3QueueCrossing = [ghostTokyo7x3_COn, ghostTokyo7x3_B3Pending] := by
  simp [ghostTokyo7x3QueueCrossing, ghostTokyo7x3_B7On, ghostTokyo7x3_COn, ghostTokyo7x3_B3Pending,
    exposeFront, SlideArea.on]

theorem ghostTokyoSceneAfterA1Release_properties :
    hasInnerScreenUnfairShape ghostTokyoSceneAfterA1Release.slide15 ∧
      exposeFront ghostTokyoSceneAfterA1Release.slide15 = [ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending] ∧
      ghostTokyoSceneAfterA1Release.tap5Pressed = false ∧
      slideQueuesCleared [ghostTokyoSceneAfterA1Release.slide73] = false := by
  simp [ghostTokyoSceneAfterA1Release, ghostTokyo1s5_hasInnerScreenUnfairShape,
    ghostTokyo1s5_exposes_B4_after_A1, ghostTokyo7x3_crossing_not_finished]

theorem ghostTokyoSceneDuringFinalTap_properties :
    exposeFront ghostTokyoSceneDuringFinalTap.slide15 = [ghostTokyo1s5_A5On] ∧
      ghostTokyo1s5_A5On.isFinished = true ∧
      ghostTokyoSceneDuringFinalTap.tap5Pressed = true ∧
      slideQueuesCleared [ghostTokyoSceneDuringFinalTap.slide73] = false := by
  simp [ghostTokyoSceneDuringFinalTap, ghostTokyo1s5_finalTap_exposes_A5,
    ghostTokyo1s5_finalTap_finishes_A5, ghostTokyo7x3_crossing_not_finished]

theorem ghostTokyo_unfair :
    let beforeTap := ghostTokyoSceneAfterA1Release
    let duringTap := ghostTokyoSceneDuringFinalTap
    hasInnerScreenUnfairShape beforeTap.slide15 ∧
      exposeFront beforeTap.slide15 = [ghostTokyo1s5_B4Pending, ghostTokyo1s5_A5Pending] ∧
      exposeFront beforeTap.slide73 = [ghostTokyo7x3_COn, ghostTokyo7x3_B3Pending] ∧
      slideQueuesCleared [beforeTap.slide73] = false ∧
      exposeFront duringTap.slide15 = [ghostTokyo1s5_A5On] ∧
      ghostTokyo1s5_A5On.isFinished = true ∧
      duringTap.tap5Pressed = true ∧
      slideQueuesCleared [duringTap.slide73] = false := by
  dsimp
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · exact ghostTokyoSceneAfterA1Release_properties.left
  · exact ghostTokyoSceneAfterA1Release_properties.right.left
  · exact ghostTokyo7x3_exposes_center
  · exact ghostTokyoSceneAfterA1Release_properties.right.right.right
  · exact ghostTokyoSceneDuringFinalTap_properties.left
  · exact ghostTokyoSceneDuringFinalTap_properties.right.left
  · exact ghostTokyoSceneDuringFinalTap_properties.right.right.left
  · exact ghostTokyoSceneDuringFinalTap_properties.right.right.right

end LnmaiCore.Proofs.InnerScreenUnfair
