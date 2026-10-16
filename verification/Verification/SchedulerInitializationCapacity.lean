import Verification.TouchHoldRebuildCapacity
import Verification.SchedulerParentFields

/-! Derive rebuild capacity from the source dictionary's actual native length
read. These entries remove PrefixFits as a caller-supplied hypothesis. They
retain the concrete facade's point contracts and its representation boundary. -/

open Aeneas Aeneas.Std Result lnmai_core_verify

namespace Verification.SchedulerInitializationCapacityProof

open LnmaiCore InputModel
open SchedulerFrameInitializationProof SchedulerParentFieldsProof
open TouchHoldRebuildCapacityProof TouchHoldRebuildTraversalProof

theorem source_length_native (b : BodyKernel)
    (source : SourceAtoms b.call b.ops b.rep b.sourceView) (holds : b.Source) :
    (b.sourceView holds).length ≤ Usize.max := by
  obtain ⟨len, _, hlen⟩ := source.hold_length holds
  rw [hlen]
  scalar_tac

theorem native_rebuild_fits (b : BodyKernel)
    (source : SourceAtoms b.call b.ops b.rep b.sourceView)
    (hcapacity : b.capacity = uniformCapacity Usize.max) (holds : b.Source) :
    PrefixFits b.capacity (b.sourceView holds) [] := by
  rw [hcapacity]
  exact prefix_fits_native _ (source_length_native b source holds)

/-- Existing buffers are copied unchanged; empty buffers rebuild within the
bound already provided by the source slice length, including every prefix. -/
theorem prepare_body_groups_native_equiv (b : BodyKernel) (points : BodyPoints b)
    (hcapacity : b.capacity = uniformCapacity Usize.max)
    (holds : b.Source) (existing : b.Buffer) :
    ∃ out, prepareBodyGroups b holds existing = ok out ∧
      b.rep.groups out =
        if (b.rep.groups existing).isEmpty then
          TouchHoldRebuildModel.SchedulerReference.reference (b.sourceView holds)
        else b.rep.groups existing :=
  prepare_body_groups_equiv b points holds existing
    (fun _ => native_rebuild_fits b points.source hcapacity holds)

/-- Parent fields are concretely instantiated, and rebuilding no longer needs
a uniform bound over arbitrary frames or a family of prefix assumptions. -/
theorem initialize_with_native_capacity_equiv (b : BodyKernel) (body : BodyPoints b)
    (hcapacity : b.capacity = uniformCapacity Usize.max)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields parentKernel b) (state : GameState) (input : FrameInput)
    (hfields : FieldsAgree parentKernel b fields state)
    (htime : addTime state.currentTime input.delta = ok (state.currentTime + input.delta)) :
    initializeFrame parentKernel b addTime fields state input =
      ok (SchedulerFrameProof.initialFrame state input) := by
  apply initialize_with_fields_equiv b body addTime fields state input hfields htime
  intro _
  rw [← hfields.holds]
  exact native_rebuild_fits b body.source hcapacity fields.holds

end Verification.SchedulerInitializationCapacityProof
