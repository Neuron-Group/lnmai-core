import Verification.TouchHoldBodyRebuild
import Verification.GeneratedTouchHoldBodyAdapter

open Aeneas Aeneas.Std Result WP lnmai_core_verify

namespace Verification.TouchHoldRebuildAdapterProof

open TouchHoldBodyReadsProof TouchHoldGroupsProof
open TouchHoldRebuildTraversalProof TouchHoldBodyRebuildProof

/-! Connect the actual extracted Rust trait implementation to the existing
record/scan/mutation/reversal proof. The dictionary below is an abbreviation
of Generated's Rust implementation, not a separately supplied Lean dictionary.
The only source boundary consists of four immutable HoldSource reads. Primitive
Id Eq/Clone/Default and native resource behavior retain their stated scope. -/

variable {Id S Ops : Type}

abbrev Adapter (Ops : Type) := touch_hold_body_adapter.TouchHoldBodyAdapter Ops

abbrev adapterCall (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (source : touch_hold_body_adapter.HoldSource Ops S Id) :=
  touch_hold_body_adapter.TouchHoldBodyAdapter.Insts.Lnmai_core_verifyTouch_hold_rebuildTouchHoldRebuildSVecTouchHoldBodyGroupStateId
    eq cloneInst defaultInst source

def sourceFields (source : touch_hold_body_adapter.HoldSource Ops S Id) :
    HoldFields (Adapter Ops) S Id :=
  { length := fun wrapper => source.hold_length wrapper.ops
    groupId := fun wrapper => source.hold_group_id wrapper.ops
    noteIndex := fun wrapper => source.hold_note_index wrapper.ops
    triggered := fun wrapper => source.hold_triggered wrapper.ops }

/-- All sixteen fields reduce to the real extracted functions, including their
borrow conversion and the exact source forwarding. No field equation is assumed. -/
theorem dictionary_eq (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (source : touch_hold_body_adapter.HoldSource Ops S Id) :
    adapterCall eq cloneInst defaultInst source =
      rebuildCall (sourceFields source) eq cloneInst defaultInst := rfl

structure SourceAgrees (source : touch_hold_body_adapter.HoldSource Ops S Id)
    (ops : Ops) (decode : Id → Nat) (holdsView : S → TouchHoldRebuildModel.Holds) : Prop where
  length : ∀ holds, ∃ len, source.hold_length ops holds = ok len ∧
    (holdsView holds).length = len.val
  groupId : ∀ holds index pre area note rest,
    At (holdsView holds) index pre (area, note) rest →
    ∃ id, source.hold_group_id ops holds index = ok id ∧
      id.map decode = note.touchHoldGroupId
  noteIndex : ∀ holds index pre area note rest,
    At (holdsView holds) index pre (area, note) rest →
    ∃ id, source.hold_note_index ops holds index = ok id ∧
      decode id = note.params.noteIndex
  triggered : ∀ holds index pre area note rest,
    At (holdsView holds) index pre (area, note) rest →
    source.hold_triggered ops holds index = ok note.touchHoldGroupTriggered

theorem source_atoms (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (source : touch_hold_body_adapter.HoldSource Ops S Id) (ops : Ops)
    (decode : Id → Nat) (holdsView : S → TouchHoldRebuildModel.Holds)
    (reads : SourceAgrees source ops decode holdsView) :
    SourceAtoms (adapterCall eq cloneInst defaultInst source)
      { ops := ops } (representation decode) holdsView := by
  exact ⟨reads.length, reads.groupId, reads.noteIndex, reads.triggered⟩

/-- The actual source length read discharges every native rebuild-prefix bound.
Identifiers remain arbitrary decoded Nat values; they are never narrowed. -/
theorem prefix_fits (source : touch_hold_body_adapter.HoldSource Ops S Id)
    (ops : Ops) (decode : Id → Nat) (holdsView : S → TouchHoldRebuildModel.Holds)
    (reads : SourceAgrees source ops decode holdsView) (holds : S) :
    PrefixFits capacity (holdsView holds) [] := by
  obtain ⟨len, _, hlen⟩ := reads.length holds
  have hbound : (holdsView holds).length ≤ Usize.max := by
    rw [hlen]
    scalar_tac
  exact TouchHoldRebuildCapacityProof.prefix_fits_native (holdsView holds) hbound

/-- Both runtime callers use this extracted dictionary. Their original Lean
Scheduler/ChartLoader rebuilds are obtained without a group-operation premise. -/
theorem rebuild_equiv (eq : core.cmp.PartialEq Id Id)
    (cloneInst : core.clone.Clone Id) (defaultInst : core.default.Default Id)
    (source : touch_hold_body_adapter.HoldSource Ops S Id) (ops : Ops)
    (decode : Id → Nat)
    (heq : ∀ left right, eq.eq left right = ok (decode left == decode right))
    (hclone : ∀ value, cloneInst.clone value = ok value)
    (filler : Id) (hdefault : defaultInst.default = ok filler)
    (holdsView : S → TouchHoldRebuildModel.Holds)
    (reads : SourceAgrees source ops decode holdsView) (holds : S) :
    ∃ out, touch_hold_rebuild.rebuild
        (adapterCall eq cloneInst defaultInst source) holds { ops := ops } = ok out ∧
      viewGroups decode out =
        TouchHoldRebuildModel.SchedulerReference.reference (holdsView holds) ∧
      viewGroups decode out =
        TouchHoldRebuildModel.ChartLoaderReference.reference (holdsView holds) ∧
      BodyFits capacity (viewGroups decode out) := by
  exact TouchHoldBodyRebuildProof.rebuild_equiv
    (sourceFields source) { ops := ops } eq cloneInst defaultInst decode heq hclone
    filler hdefault holdsView
    (source_atoms eq cloneInst defaultInst source ops decode holdsView reads) holds

end Verification.TouchHoldRebuildAdapterProof
