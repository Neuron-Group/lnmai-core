/-
  Proofs for the proposition that a slide is represented as a queue of
  judgment segments.

  In the current runtime model, each `SlideArea` is one judgment segment, and
  its `targetAreas` list records the 1–2 inner-screen zones that witness that
  segment.
-/

import LnmaiCore.Lifecycle
import LnmaiCore.Proofs.InnerScreenUnfair

namespace LnmaiCore.Proofs.SlideJudgmentSegments

open LnmaiCore.Lifecycle
open LnmaiCore.Proofs.InnerScreenUnfair

def IsJudgmentSegment (area : SlideArea) : Prop :=
  area.targetAreas.length = 1 ∨ area.targetAreas.length = 2

def HasJudgmentSegments (queue : SlideQueue) : Prop :=
  ∀ area, area ∈ queue → IsJudgmentSegment area

def HasJudgmentSegmentsPerTrack (queues : List SlideQueue) : Prop :=
  ∀ queue, queue ∈ queues → HasJudgmentSegments queue

theorem singleZone_isJudgmentSegment (area : SlideArea)
    (hLen : area.targetAreas.length = 1) :
    IsJudgmentSegment area := by
  exact Or.inl hLen

theorem doubleZone_isJudgmentSegment (area : SlideArea)
    (hLen : area.targetAreas.length = 2) :
    IsJudgmentSegment area := by
  exact Or.inr hLen

theorem nil_hasJudgmentSegments :
    HasJudgmentSegments [] := by
  intro area hMem
  cases hMem

theorem cons_hasJudgmentSegments
    (head : SlideArea) (tail : SlideQueue)
    (hHead : IsJudgmentSegment head)
    (hTail : HasJudgmentSegments tail) :
    HasJudgmentSegments (head :: tail) := by
  intro area hMem
  simp at hMem
  rcases hMem with rfl | hMem
  · exact hHead
  · exact hTail area hMem

theorem singleton_hasJudgmentSegments
    (area : SlideArea) (hArea : IsJudgmentSegment area) :
    HasJudgmentSegments [area] := by
  exact cons_hasJudgmentSegments area [] hArea nil_hasJudgmentSegments

theorem ghostTokyo1s5_A1_isJudgmentSegment :
    IsJudgmentSegment ghostTokyo1s5_A1Pending := by
  simp [IsJudgmentSegment, ghostTokyo1s5_A1Pending]

theorem ghostTokyo1s5_B4_isJudgmentSegment :
    IsJudgmentSegment ghostTokyo1s5_B4Pending := by
  simp [IsJudgmentSegment, ghostTokyo1s5_B4Pending]

theorem ghostTokyo1s5_A5_isJudgmentSegment :
    IsJudgmentSegment ghostTokyo1s5_A5Pending := by
  simp [IsJudgmentSegment, ghostTokyo1s5_A5Pending]

theorem ghostTokyo1s5Queue_hasJudgmentSegments :
    HasJudgmentSegments ghostTokyo1s5QueueStart := by
  apply cons_hasJudgmentSegments
  · exact ghostTokyo1s5_A1_isJudgmentSegment
  · apply cons_hasJudgmentSegments
    · exact ghostTokyo1s5_B4_isJudgmentSegment
    · apply cons_hasJudgmentSegments
      · exact ghostTokyo1s5_A5_isJudgmentSegment
      · exact nil_hasJudgmentSegments

theorem judgmentSegment_has_one_or_two_zones
    (queue : SlideQueue) (hSegments : HasJudgmentSegments queue)
    (area : SlideArea) (hMem : area ∈ queue) :
    area.targetAreas.length = 1 ∨ area.targetAreas.length = 2 := by
  exact hSegments area hMem

end LnmaiCore.Proofs.SlideJudgmentSegments
