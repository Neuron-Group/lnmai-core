import LnmaiCore.Lifecycle
import LnmaiCore.Simai.SlideTables

namespace LnmaiCore.Proofs.QueueInterop

open LnmaiCore.Lifecycle

/-- Build a runtime `SlideArea` from a Simai topology spec. -/
def slideAreaOfSpec (spec : LnmaiCore.Simai.SlideAreaSpec) : SlideArea :=
  { targetAreas := spec.targetAreas
  , policy := spec.policy
  , isLast := spec.isLast
  , isSkippable := spec.isSkippable
  , arrowProgressWhenOn := spec.arrowProgressWhenOn
  , arrowProgressWhenFinished := spec.arrowProgressWhenFinished }

/-- Flatten Simai topology queues into a single runtime queue for proof-facing reasoning. -/
def flattenSimaiSlideQueues : List (List LnmaiCore.Simai.SlideAreaSpec) → SlideQueue
  | [] => []
  | q :: qs => q.map slideAreaOfSpec ++ flattenSimaiSlideQueues qs

end LnmaiCore.Proofs.QueueInterop
