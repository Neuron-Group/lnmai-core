import LnmaiCore.Proofs.Runtime
import LnmaiCore.RuntimeSeek

namespace LnmaiCore

open InputModel ChartLoader

structure AutoplayPart where
  runtimeNoteIndex : Nat
  role : Nat := 0
  trackIndex : Option Nat := none
  events : List TimedInputEvent := []
deriving Inhabited, Repr, Lean.ToJson, Lean.FromJson

structure AutoplayGroup where
  id : Nat
  kind : Nat
  runtimeNoteIndices : List Nat := []
  parts : List AutoplayPart := []
  dependsOnGroupIds : List Nat := []
deriving Inhabited, Repr, Lean.ToJson, Lean.FromJson

structure AutoplayPlan where
  version : Nat := 1
  groups : List AutoplayGroup := []
deriving Inhabited, Repr, Lean.ToJson, Lean.FromJson

def AutoplayPlan.events (plan : AutoplayPlan) : List TimedInputEvent :=
  sortTacticEvents (plan.groups.flatMap (fun (group : AutoplayGroup) =>
    group.parts.flatMap (fun (part : AutoplayPart) => part.events)))

/-- Capture ownership while resolving each skeleton, before event flattening. -/
def autoplayPlanFromChart (chart : ChartSpec) : AutoplayPlan := Id.run do
  let mut groups : List AutoplayGroup := []
  for skeleton in chartTimingSkeleton chart do
    let id := skeleton.noteIndex
    let entry : Nat × Nat × List AutoplayPart × List Nat := match skeleton with
      | .slide spec =>
        let body : Option SlideChartNote := chart.slides.find? (fun (note : SlideChartNote) => note.noteIndex == spec.noteIndex)
        let root := RuntimeSeek.slideRoot chart.slides spec.noteIndex
        let heads : List SlideHeadChartNote := chart.slideHeads.filter (fun (head : SlideHeadChartNote) =>
          body.any (fun (note : SlideChartNote) => note.logicalSlideId == head.logicalSlideId))
        let headParts : List AutoplayPart := heads.map (fun (head : SlideHeadChartNote) =>
          { runtimeNoteIndex := head.noteIndex, role := 1
            events := [tapAtTime head.timing head.slot.toButtonZone] : AutoplayPart })
        let bodyPart : AutoplayPart :=
          { runtimeNoteIndex := spec.noteIndex, role := 2
            events := (resolveDefaultTimingSkeleton (.slide { spec with hasHeadInput := false })).events }
        (root, 4, headParts ++ [bodyPart], heads.map (fun (head : SlideHeadChartNote) => head.noteIndex) ++ [spec.noteIndex])
      | .tap .. => (id, 0, [{ runtimeNoteIndex := id, events := (resolveDefaultTimingSkeleton skeleton).events }], [id])
      | .hold .. => (id, 1, [{ runtimeNoteIndex := id, events := (resolveDefaultTimingSkeleton skeleton).events }], [id])
      | .touch .. => (id, 2, [{ runtimeNoteIndex := id, events := (resolveDefaultTimingSkeleton skeleton).events }], [id])
      | .touchHold .. => (id, 3, [{ runtimeNoteIndex := id, events := (resolveDefaultTimingSkeleton skeleton).events }], [id])
    let (groupId, kind, parts, ids) := entry
    match groups.find? (fun (group : AutoplayGroup) => group.id == groupId) with
    | none => groups := groups ++ [{ id := groupId, kind := kind, runtimeNoteIndices := ids, parts := parts }]
    | some _ => groups := groups.map (fun (group : AutoplayGroup) =>
        if group.id == groupId then
          { group with runtimeNoteIndices := group.runtimeNoteIndices ++ ids, parts := group.parts ++ parts }
        else group)
  return { groups := groups }

end LnmaiCore
