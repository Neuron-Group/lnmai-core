import Verification.Generated
import Verification.Bridge
import LnmaiCore.Simai.Normalize

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace ConnectedSlideMetadata

/-! The Rust extraction represents BigNat opaquely. The adapter below keeps
the proof strict: every fact used about an extracted operation is an explicit
field, rather than an axiom hidden in the proof. -/

structure BigNatAdapter where
  view : big_nat.BigNat → Nat
  zero : big_nat.BigNat
  one : big_nat.BigNat
  zero_spec : big_nat.zero = ok zero
  one_spec : big_nat.one = ok one
  zero_view : view zero = 0
  one_view : view one = 1
  equal_spec : ∀ left right,
    big_nat.equal left right = ok (decide (view left = view right))
  compare_spec : ∀ left right, ∃ order : Std.I8,
    big_nat.compare left right = ok order ∧
      (order > 0#i8 ↔ view right < view left)
  add_one_spec : ∀ value, ∃ result,
    big_nat.add_u64 value 1#u64 = ok result ∧
      view result = view value + 1
  clone_spec : ∀ value,
    big_nat.BigNat.Insts.CoreCloneClone.clone value = ok value

def optionView (view : big_nat.BigNat → Nat) : Option big_nat.BigNat → Option Nat :=
  Option.map view

structure SourceAppliedMetadata where
  slideKind : LnmaiCore.SlideKind
  isConnSlide : Bool
  parentNoteIndex : Option Nat
  isGroupHead : Bool
  isGroupEnd : Bool
  nextGroupId : Option Nat
  nextParentNoteIndex : Option Nat

inductive SourceMetadataStep where
  | clear
  | reset
  | apply (metadata : SourceAppliedMetadata)

def sourceStep
    (sourceGroupId : Option Nat)
    (sourceGroupIndex : Option Nat)
    (sourceGroupSize : Option Nat)
    (originalSlideKind : LnmaiCore.SlideKind)
    (currentGroupId : Option Nat)
    (parentNoteIndex : Option Nat)
    (noteIndex : Nat) : SourceMetadataStep :=
  match sourceGroupId, sourceGroupIndex, sourceGroupSize with
  | some groupId, some index, some size =>
      let isHead := index = 0
      let isEnd := index + 1 = size
      let isConn := size > 1
      let slideKind :=
        if isConn then
          if originalSlideKind = .Wifi then .Wifi else .ConnPart
        else originalSlideKind
      let actualParent := if isHead then none else parentNoteIndex
      let nextGroupId := if isEnd then none else some groupId
      let nextParentNoteIndex := if isEnd then none else some noteIndex
      if currentGroupId ≠ some groupId && index ≠ 0 then
        .reset
      else
        .apply
          { slideKind := slideKind
            isConnSlide := decide isConn
            parentNoteIndex := actualParent
            isGroupHead := decide isHead
            isGroupEnd := decide isEnd
            nextGroupId := nextGroupId
            nextParentNoteIndex := nextParentNoteIndex }
  | _, _, _ => .clear

def toSourceAppliedMetadata
    (view : big_nat.BigNat → Nat)
    (metadata : lnmai_core_verify.connected_slide_metadata.AppliedMetadata) : SourceAppliedMetadata :=
  { slideKind := Bridge.toLnmSlideKind metadata.slide_kind
    isConnSlide := metadata.is_conn_slide
    parentNoteIndex := metadata.parent_note_index.map view
    isGroupHead := metadata.is_group_head
    isGroupEnd := metadata.is_group_end
    nextGroupId := metadata.next_group_id.map view
    nextParentNoteIndex := metadata.next_parent_note_index.map view }

def toSourceStep
    (view : big_nat.BigNat → Nat) :
    lnmai_core_verify.connected_slide_metadata.MetadataStep → SourceMetadataStep
  | .Clear => .clear
  | .Reset => .reset
  | .Apply metadata => .apply (toSourceAppliedMetadata view metadata)

def mapStepResult
    (view : big_nat.BigNat → Nat) :
    Result lnmai_core_verify.connected_slide_metadata.MetadataStep → Result SourceMetadataStep
  | ok step => ok (toSourceStep view step)
  | fail error => fail error
  | div => div

theorem slideKind_eq_spec (left right : types.SlideKind) :
    types.SlideKind.Insts.CoreCmpPartialEqSlideKind.eq left right =
      ok (decide (Bridge.toLnmSlideKind left = Bridge.toLnmSlideKind right)) := by
  cases left <;> cases right <;>
    simp [types.SlideKind.Insts.CoreCmpPartialEqSlideKind.eq,
      types.SlideKind.read_discriminant, Bridge.toLnmSlideKind] <;> rfl

theorem optionEqual_spec (adapter : BigNatAdapter)
    (left right : Option big_nat.BigNat) :
    lnmai_core_verify.connected_slide_metadata.option_equal left right =
      ok (decide (optionView adapter.view left = optionView adapter.view right)) := by
  cases left <;> cases right
  · rfl
  · rfl
  · rfl
  · simp [lnmai_core_verify.connected_slide_metadata.option_equal,
      adapter.clone_spec, adapter.equal_spec, optionView]

theorem apply_semantics (adapter : BigNatAdapter)
    (sourceGroupId : Option big_nat.BigNat)
    (sourceGroupIndex : Option big_nat.BigNat)
    (sourceGroupSize : Option big_nat.BigNat)
    (originalSlideKind : types.SlideKind)
    (currentGroupId : Option big_nat.BigNat)
    (parentNoteIndex : Option big_nat.BigNat)
    (noteIndex : big_nat.BigNat) :
    mapStepResult adapter.view
        (lnmai_core_verify.connected_slide_metadata.apply sourceGroupId sourceGroupIndex sourceGroupSize
          originalSlideKind currentGroupId parentNoteIndex noteIndex) =
      ok (sourceStep
        (optionView adapter.view sourceGroupId)
        (optionView adapter.view sourceGroupIndex)
        (optionView adapter.view sourceGroupSize)
        (Bridge.toLnmSlideKind originalSlideKind)
        (optionView adapter.view currentGroupId)
        (optionView adapter.view parentNoteIndex)
        (adapter.view noteIndex)) := by
  cases sourceGroupId with
  | none => rfl
  | some groupId =>
      cases sourceGroupIndex with
      | none => rfl
      | some index =>
          cases sourceGroupSize with
          | none => rfl
          | some size =>
              rcases adapter.compare_spec size adapter.one with
                ⟨order, hcompare, hcompare_pos⟩
              rcases adapter.add_one_spec index with
                ⟨successor, hadd, hsuccessor⟩
              unfold lnmai_core_verify.connected_slide_metadata.apply
              simp only [adapter.zero_spec, adapter.one_spec, bind_tc_ok,
                adapter.equal_spec, adapter.zero_view, hcompare, hadd,
                hsuccessor, slideKind_eq_spec, adapter.clone_spec,
                optionEqual_spec adapter]
              have hcompare_pos' : order > 0#i8 ↔ 1 < adapter.view size := by
                simpa [adapter.one_view] using hcompare_pos
              cases originalSlideKind <;>
                by_cases hconn : 1 < adapter.view size <;>
                by_cases hhead : adapter.view index = 0 <;>
                  by_cases hend : adapter.view index + 1 = adapter.view size
              all_goals try omega
              all_goals by_cases hsize : adapter.view size = 1
              all_goals try omega
              all_goals
                by_cases hmatch :
                  optionView adapter.view currentGroupId = optionView adapter.view (some groupId)
                · have hmatch_exists :
                      ∃ a, currentGroupId = some a ∧ adapter.view a = adapter.view groupId := by
                    cases currentGroupId with
                    | none => simp [optionView] at hmatch
                    | some current =>
                        refine ⟨current, rfl, ?_⟩
                        simpa [optionView] using hmatch
                  simp [mapStepResult, toSourceStep, toSourceAppliedMetadata,
                    sourceStep, optionView, hconn, hhead, hend, hsize, hmatch,
                    hmatch_exists, hcompare_pos', adapter.one_view,
                    adapter.clone_spec, Bridge.toLnmSlideKind, decide_eq_true_eq,
                    Option.map_some, Option.map_none]
                · have hmatch_forall :
                      ∀ a, currentGroupId = some a → adapter.view a ≠ adapter.view groupId := by
                    intro a ha hEq
                    apply hmatch
                    simp [optionView, ha, hEq]
                  have hmatch_not_exists :
                      ¬(∃ a, currentGroupId = some a ∧ adapter.view a = adapter.view groupId) := by
                    rintro ⟨a, ha, hEq⟩
                    exact hmatch_forall a ha hEq
                  simp [mapStepResult, toSourceStep, toSourceAppliedMetadata,
                    sourceStep, optionView, hconn, hhead, hend, hsize, hmatch,
                    hmatch_forall, hmatch_not_exists, hcompare_pos', adapter.one_view,
                    adapter.clone_spec, Bridge.toLnmSlideKind, decide_eq_true_eq,
                    Option.map_some, Option.map_none]

end ConnectedSlideMetadata

end Verification
