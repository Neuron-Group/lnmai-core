/-
  Proof scaffolding for the “inner-screen unfair” slide configuration.

  This file is intentionally theorem-oriented: it states small lemmas about
  slide-area completion and a simplified queue-exposure model that matches the
  area-jump intuition used by the reference behavior.
-/

import LnmaiCore.Lifecycle
import LnmaiCore.Simai
import LnmaiCore.Proofs.QueueInterop

namespace LnmaiCore.Proofs.InnerScreenUnfair

open LnmaiCore.Constants
open LnmaiCore.Lifecycle
open LnmaiCore.Simai
open LnmaiCore.Proofs.QueueInterop

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

def ghostTokyoSimaiFragment : NormalizedChart :=
  simai_normalized_chart! "&first=0\n&inote_1=\n(124){4}\n7x-3[8:1],1xs5[4:1],5x,\n"

def ghostTokyo1s5Normalized : NormalizedSlide :=
  match ghostTokyoSimaiFragment.slides with
  | _ :: slide15 :: _ => slide15
  | _ => panic! "Ghost Tokyo fragment should contain the 1s5 slide as its second normalized slide"

def ghostTokyo7x3Normalized : NormalizedSlide :=
  match ghostTokyoSimaiFragment.slides with
  | slide73 :: _ :: _ => slide73
  | _ => panic! "Ghost Tokyo fragment should contain the 7x-3 slide as its first normalized slide"

def ghostTokyo7x3FullQueueStart : SlideQueue :=
  flattenSimaiSlideQueues ghostTokyo7x3Normalized.judgeQueues

def ghostTokyo1s5FullQueueStart : SlideQueue :=
  flattenSimaiSlideQueues ghostTokyo1s5Normalized.judgeQueues

theorem ghostTokyo1s5Normalized_matches_reference_shape :
    ghostTokyo1s5Normalized.simaiShape.kind = SlideKind.s ∧
      ghostTokyo1s5Normalized.judgeQueues.length = 7 := by
  native_decide

theorem ghostTokyo1s5FullQueue_matches_parsed_topology :
    List.map (fun area => area.targetAreas) ghostTokyo1s5FullQueueStart =
      [[1], [8], [7], [16], [3], [4], [5]] := by
  native_decide

theorem ghostTokyo7x3FullQueue_matches_parsed_topology :
    List.map (fun area => area.targetAreas) ghostTokyo7x3FullQueueStart =
      [[1], [9], [16], [14], [5]] := by
  native_decide

def ghostTokyoSensorNone : List Bool :=
  List.replicate 17 false

def ghostTokyoSensorA1 : List Bool :=
  (List.range 17).map (fun i => i = 1)

def ghostTokyoSensorB7 : List Bool :=
  (List.range 17).map (fun i => i = 7)

def ghostTokyoSensorC : List Bool :=
  (List.range 17).map (fun i => i = 16)

def ghostTokyoSensorB3 : List Bool :=
  (List.range 17).map (fun i => i = 3)

def ghostTokyoSensorA5 : List Bool :=
  (List.range 17).map (fun i => i = 5)

def ghostTokyo1s5AfterA1Press : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo1s5FullQueueStart ghostTokyoSensorA1

def ghostTokyo1s5AfterA1Release : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo1s5AfterA1Press ghostTokyoSensorNone

def ghostTokyo1s5AfterB7 : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo1s5AfterA1Release ghostTokyoSensorB7

def ghostTokyo1s5AfterC : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo1s5AfterB7 ghostTokyoSensorC

def ghostTokyo1s5AfterB3 : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo1s5AfterC ghostTokyoSensorB3

def ghostTokyo1s5AfterA5 : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo1s5AfterB3 ghostTokyoSensorA5

def ghostTokyo7x3AfterB7 : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo7x3FullQueueStart ghostTokyoSensorB7

def ghostTokyo7x3AfterC : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo7x3AfterB7 ghostTokyoSensorC

def ghostTokyo7x3AfterB3 : SlideQueue :=
  Lifecycle.replaySlideQueue ghostTokyo7x3AfterC ghostTokyoSensorB3

theorem ghostTokyo1s5_exact_progression :
    List.map (fun area => area.targetAreas) ghostTokyo1s5AfterA1Press = [[1], [8], [7], [16], [3], [4], [5]] ∧
    ghostTokyo1s5AfterA1Press[0]!.wasOn = true ∧
    List.map (fun area => area.targetAreas) ghostTokyo1s5AfterA1Release = [[8], [7], [16], [3], [4], [5]] ∧
    List.map (fun area => area.targetAreas) ghostTokyo1s5AfterB7 = [[7], [16], [3], [4], [5]] ∧
    List.map (fun area => area.targetAreas) ghostTokyo1s5AfterC = [[16], [3], [4], [5]] ∧
    List.map (fun area => area.targetAreas) ghostTokyo1s5AfterB3 = [[3], [4], [5]] ∧
    ghostTokyo1s5AfterA5 = [] := by
  native_decide

theorem ghostTokyo7x3_exact_progression :
    List.map (fun area => area.targetAreas) ghostTokyo7x3AfterB7 = [[1], [9], [16], [14], [5]] ∧
    List.map (fun area => area.targetAreas) ghostTokyo7x3AfterC = [[1], [9], [16], [14], [5]] ∧
    List.map (fun area => area.targetAreas) ghostTokyo7x3AfterB3 = [[1], [9], [16], [14], [5]] := by
  native_decide

-- The older reduced-witness Ghost Tokyo proof family is retired.
-- The authoritative proof path now goes through parser-derived full queues
-- and the exact replay theorems above.

end LnmaiCore.Proofs.InnerScreenUnfair
