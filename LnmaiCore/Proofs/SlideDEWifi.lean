/-
  Proofs for the D/E exclusion rule and the Wifi exception.

  This file uses a small proof-local partition of inner-screen zones:
  ordinary slide judgment uses the low-numbered zone family, while Wifi is
  allowed to reach the higher-numbered D/E family.
-/

import LnmaiCore.Proofs.SlideJudgmentSegments

namespace LnmaiCore.Proofs.SlideDEWifi

open LnmaiCore.Lifecycle
open LnmaiCore.Proofs.SlideJudgmentSegments
open LnmaiCore.Proofs.InnerScreenUnfair

def IsOrdinarySlideZone (zone : Nat) : Prop :=
  zone < 12

def IsDEZone (zone : Nat) : Prop :=
  12 ≤ zone

def ExcludesDE (area : SlideArea) : Prop :=
  ∀ zone ∈ area.targetAreas, IsOrdinarySlideZone zone

def ExcludesDEQueue (queue : SlideQueue) : Prop :=
  ∀ area ∈ queue, ExcludesDE area

def UsesDEZone (area : SlideArea) : Prop :=
  ∃ zone ∈ area.targetAreas, IsDEZone zone

def UsesDEZoneQueue (queue : SlideQueue) : Prop :=
  ∃ area ∈ queue, UsesDEZone area

theorem ordinaryZone_isNotDE (zone : Nat)
    (hOrdinary : IsOrdinarySlideZone zone) :
    ¬ IsDEZone zone := by
  intro hDE
  exact Nat.not_le_of_gt hOrdinary hDE

theorem ordinaryArea_excludesDE_of_allOrdinary
    (area : SlideArea)
    (hOrdinary : ∀ zone ∈ area.targetAreas, IsOrdinarySlideZone zone) :
    ExcludesDE area := by
  intro zone hMem
  exact hOrdinary zone hMem

theorem nil_excludesDEQueue :
    ExcludesDEQueue [] := by
  intro area hMem
  cases hMem

theorem cons_excludesDEQueue
    (head : SlideArea) (tail : SlideQueue)
    (hHead : ExcludesDE head)
    (hTail : ExcludesDEQueue tail) :
    ExcludesDEQueue (head :: tail) := by
  intro area hMem
  simp at hMem
  rcases hMem with rfl | hMem
  · exact hHead
  · exact hTail area hMem

theorem ordinarySlideQueue_excludesDE
    (queue : SlideQueue)
    (hQueue : ∀ area, area ∈ queue → ∀ zone, zone ∈ area.targetAreas → IsOrdinarySlideZone zone) :
    ExcludesDEQueue queue := by
  intro area hMem
  exact ordinaryArea_excludesDE_of_allOrdinary area (hQueue area hMem)

theorem ordinarySlideQueue_hasNoDEZones
    (queue : SlideQueue)
    (hQueue : ExcludesDEQueue queue) :
    ¬ UsesDEZoneQueue queue := by
  intro hUses
  rcases hUses with ⟨area, hAreaMem, zone, hZoneMem, hDE⟩
  have hOrdinary := hQueue area hAreaMem zone hZoneMem
  exact ordinaryZone_isNotDE zone hOrdinary hDE

def wifiDEArea : SlideArea := {
  targetAreas := [12, 13]
  policy := .Or
  isLast := false
  isSkippable := true
  arrowProgressWhenOn := 0
  arrowProgressWhenFinished := 0
  wasOn := false
  wasOff := false
}

def wifiDEQueue : SlideQueue :=
  [wifiDEArea]

theorem wifiDEArea_usesDEZone :
    UsesDEZone wifiDEArea := by
  refine ⟨12, ?_⟩
  constructor
  · simp [wifiDEArea]
  · simp [IsDEZone]

theorem wifiDEQueue_usesDEZoneQueue :
    UsesDEZoneQueue wifiDEQueue := by
  refine ⟨wifiDEArea, ?_, wifiDEArea_usesDEZone⟩
  simp [wifiDEQueue]

theorem wifiDEArea_isJudgmentSegment :
    IsJudgmentSegment wifiDEArea := by
  simp [IsJudgmentSegment, wifiDEArea]

theorem wifiSlide_may_use_DE_zones :
    UsesDEZoneQueue wifiDEQueue ∧ ¬ ExcludesDEQueue wifiDEQueue := by
  constructor
  · exact wifiDEQueue_usesDEZoneQueue
  · intro hExcludes
    have h := hExcludes wifiDEArea (by simp [wifiDEQueue]) 12 (by simp [wifiDEArea])
    exact Nat.lt_irrefl 12 h

end LnmaiCore.Proofs.SlideDEWifi
