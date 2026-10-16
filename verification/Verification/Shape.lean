import Verification.Symmetry
import LnmaiCore.Simai.Shape
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify
open Verification.Areas Verification.SymmetryProof

set_option maxHeartbeats 1000000
namespace Verification.ShapeProof

def toLnmKind : shape.SlideKind → LnmaiCore.Simai.SlideKind
  | .Line => .line | .Circle => .circle | .V => .v | .Turn => .turn
  | .Pq => .pq | .Ppqq => .ppqq | .S => .s | .Wifi => .wifi

def toLnmCanonical : shape.CanonicalSlideShape → LnmaiCore.Simai.CanonicalSlideShape
  | .Line n => .line n.val | .Circle n => .circle n.val
  | .V n => .v n.val | .Turn n => .turn n.val
  | .Pq n => .pq n.val | .Ppqq n => .ppqq n.val
  | .S => .s | .Wifi => .wifi

def toLnmShape (s : shape.SlideShape) : LnmaiCore.Simai.SlideShape :=
  ⟨toLnmCanonical s.canonical, toLnmSymmetry s.symmetry⟩

def toLnmBodyKind : shape.SlideBodyKind → LnmaiCore.Simai.SlideBodyKind
  | .Line => .line | .CircleRight => .circleRight | .CircleLeft => .circleLeft
  | .CircleUp => .circleUp | .V => .v | .Pp => .pp | .Qq => .qq
  | .P => .p | .Q => .q | .S => .s | .Z => .z | .Turn => .turn | .Wifi => .wifi

/-- The runtime error adapter, including the empty raw text for non-outer areas. -/
def toLnmError (raw : String) : shape.ShapeError → LnmaiCore.Simai.ParseError
  | .EndNotOuter => ⟨.invalidEndPosition, "", "slide end must be on outer A-ring", none⟩
  | .InvalidEnd => ⟨.invalidEndPosition, raw, "invalid end", none⟩
  | .InvalidTurnEnd => ⟨.invalidTurnPosition, raw, "invalid end", none⟩
  | .InvalidTurn => ⟨.invalidTurnPosition, raw, "invalid turn", none⟩

def toLnmResult {α β : Type} (f : α → β) (raw : String) :
    core.result.Result α shape.ShapeError → Except LnmaiCore.Simai.ParseError β
  | .Ok x => .ok (f x)
  | .Err e => .error (toLnmError raw e)

theorem default_equiv :
    (do let s ← shape.SlideShape.Insts.CoreDefaultDefault.default; ok (toLnmShape s)) =
      ok (default : LnmaiCore.Simai.SlideShape) := rfl

theorem kind_default_equiv :
    (do let k ← shape.SlideKind.Insts.CoreDefaultDefault.default; ok (toLnmKind k)) =
      ok (default : LnmaiCore.Simai.SlideKind) := rfl

theorem canonical_default_equiv :
    (do let c ← shape.CanonicalSlideShape.Insts.CoreDefaultDefault.default;
        ok (toLnmCanonical c)) =
      ok (default : LnmaiCore.Simai.CanonicalSlideShape) := rfl

theorem body_kind_default_equiv :
    (do let k ← shape.SlideBodyKind.Insts.CoreDefaultDefault.default; ok (toLnmBodyKind k)) =
      ok (default : LnmaiCore.Simai.SlideBodyKind) := rfl

theorem kind_clone_equiv (k : shape.SlideKind) :
    shape.SlideKind.Insts.CoreCloneClone.clone k = ok k := rfl

theorem canonical_clone_equiv (c : shape.CanonicalSlideShape) :
    shape.CanonicalSlideShape.Insts.CoreCloneClone.clone c = ok c := rfl

theorem shape_clone_equiv (s : shape.SlideShape) :
    shape.SlideShape.Insts.CoreCloneClone.clone s = ok s := rfl

theorem body_kind_clone_equiv (k : shape.SlideBodyKind) :
    shape.SlideBodyKind.Insts.CoreCloneClone.clone k = ok k := rfl

theorem kind_eq_equiv (a b : shape.SlideKind) :
    shape.SlideKind.Insts.CoreCmpPartialEqSlideKind.eq a b =
      ok (decide (toLnmKind a = toLnmKind b)) := by
  cases a <;> cases b <;>
    simp [shape.SlideKind.Insts.CoreCmpPartialEqSlideKind.eq,
      shape.SlideKind.read_discriminant, toLnmKind]

theorem body_kind_eq_equiv (a b : shape.SlideBodyKind) :
    shape.SlideBodyKind.Insts.CoreCmpPartialEqSlideBodyKind.eq a b =
      ok (decide (toLnmBodyKind a = toLnmBodyKind b)) := by
  cases a <;> cases b <;>
    simp [shape.SlideBodyKind.Insts.CoreCmpPartialEqSlideBodyKind.eq,
      shape.SlideBodyKind.read_discriminant, toLnmBodyKind]

theorem canonical_eq_equiv (a b : shape.CanonicalSlideShape) :
    shape.CanonicalSlideShape.Insts.CoreCmpPartialEqCanonicalSlideShape.eq a b =
      ok (decide (toLnmCanonical a = toLnmCanonical b)) := by
  cases a <;> cases b <;>
    simp [shape.CanonicalSlideShape.Insts.CoreCmpPartialEqCanonicalSlideShape.eq,
      shape.CanonicalSlideShape.read_discriminant, toLnmCanonical,
      core.cmp.impls.PartialEqUsize.eq, UScalar.eq_equiv, lift]

theorem shape_eq_equiv (a b : shape.SlideShape) :
    shape.SlideShape.Insts.CoreCmpPartialEqSlideShape.eq a b =
      ok (decide (toLnmShape a = toLnmShape b)) := by
  unfold shape.SlideShape.Insts.CoreCmpPartialEqSlideShape.eq
  rw [canonical_eq_equiv]
  simp [Verification.SymmetryProof.partial_eq_equiv, toLnmShape]
  split_ifs <;> simp_all


theorem shape_kind_equiv (s : shape.SlideShape) :
    (do let k ← shape.shape_kind s; ok (toLnmKind k)) =
      ok (LnmaiCore.Simai.SlideShape.kind (toLnmShape s)) := by
  rcases s with ⟨c, g⟩
  cases c <;> rfl

theorem shape_rel_end_equiv (s : shape.SlideShape) :
    (do let n ← shape.shape_rel_end s; ok (n.map UScalar.val)) =
      ok (LnmaiCore.Simai.SlideShape.relEnd (toLnmShape s)) := by
  rcases s with ⟨c, g⟩
  cases c <;> rfl

theorem shape_mirrored_equiv (s : shape.SlideShape) :
    shape.shape_mirrored s = ok (LnmaiCore.Simai.SlideShape.mirrored (toLnmShape s)) := by
  exact is_mirrored_equiv s.symmetry

theorem mk_slide_shape_equiv (c : shape.CanonicalSlideShape) (g : symmetry.SlideSymmetry) :
    (do let s ← shape.mk_slide_shape c g; ok (toLnmShape s)) =
      ok (LnmaiCore.Simai.mkSlideShape (toLnmCanonical c) (toLnmSymmetry g)) := rfl

theorem mk_canonical_slide_shape_unchecked_equiv (k : shape.SlideKind) (n : Option Usize) :
    (do let c ← shape.mk_canonical_slide_shape_unchecked k n; ok (toLnmCanonical c)) =
      ok (LnmaiCore.Simai.mkCanonicalSlideShapeUnchecked (toLnmKind k)
        (n.map UScalar.val)) := by
  cases k <;> cases n <;> rfl

theorem mk_canonical_slide_shape_opt_equiv (k : shape.SlideKind) (n : Option Usize) :
    (do let c ← shape.mk_canonical_slide_shape_opt k n; ok (c.map toLnmCanonical)) =
      ok (LnmaiCore.Simai.mkCanonicalSlideShape? (toLnmKind k) (n.map UScalar.val)) := by
  cases k <;> cases n <;>
    simp [shape.mk_canonical_slide_shape_opt, LnmaiCore.Simai.mkCanonicalSlideShape?,
      toLnmKind, toLnmCanonical, UScalar.le_equiv, UScalar.eq_equiv]
  all_goals split_ifs <;> simp_all [toLnmCanonical]

theorem digit_to_nat_equiv (c : Char) :
    (do let n ← shape.digit_to_nat c; ok (n.map UScalar.val)) =
      ok (LnmaiCore.Simai.digitToNat? c) := by
  unfold shape.digit_to_nat LnmaiCore.Simai.digitToNat?
  split_ifs <;> simp_all

open private LnmaiCore.Simai.mirrorKey LnmaiCore.Simai.mirrorRelEnd
  LnmaiCore.Simai.outerSlotIsRightHalf LnmaiCore.Simai.outerSlotIsUpperHalf
  LnmaiCore.Simai.relativeEndPos LnmaiCore.Simai.relativeEndFromTyped
  from LnmaiCore.Simai.Shape

theorem mirror_key_equiv (n : Usize) :
    (do let m ← shape.mirror_key n; ok m.val) = ok (LnmaiCore.Simai.mirrorKey n.val) := by
  unfold shape.mirror_key LnmaiCore.Simai.mirrorKey
  split <;> simp_all

theorem canonical_rel_end_equiv (g : symmetry.SlideSymmetry) (n : Usize) :
    (do let m ← shape.canonical_rel_end g n; ok m.val) =
      ok (LnmaiCore.Simai.canonicalRelEnd (toLnmSymmetry g) n.val) := by
  cases g <;> simp [shape.canonical_rel_end, symmetry.is_mirrored,
    LnmaiCore.Simai.canonicalRelEnd, LnmaiCore.Simai.SlideSymmetry.isMirrored,
    toLnmSymmetry, LnmaiCore.Simai.mirrorRelEnd, mirror_key_equiv]

theorem base_rel_end_equiv (g : symmetry.SlideSymmetry) (n : Usize) :
    (do let m ← shape.base_rel_end g n; ok m.val) =
      ok (LnmaiCore.Simai.baseRelEnd (toLnmSymmetry g) n.val) :=
  canonical_rel_end_equiv g n

theorem outer_slot_is_right_half_equiv (s : areas.OuterSlot) :
    shape.outer_slot_is_right_half s =
      ok (LnmaiCore.Simai.outerSlotIsRightHalf (toLnmOuterSlot s)) := by
  cases s <;> rfl

theorem outer_slot_is_upper_half_equiv (s : areas.OuterSlot) :
    shape.outer_slot_is_upper_half s =
      ok (LnmaiCore.Simai.outerSlotIsUpperHalf (toLnmOuterSlot s)) := by
  cases s <;> rfl

@[step] theorem relative_end_pos_spec (s e : Usize)
    (hs : 1 ≤ s.val ∧ s.val ≤ 8) (he : 1 ≤ e.val ∧ e.val ≤ 8) :
    shape.relative_end_pos s e ⦃ r => r.val = LnmaiCore.Simai.relativeEndPos s.val e.val ⦄ := by
  unfold shape.relative_end_pos LnmaiCore.Simai.relativeEndPos
  step*

end Verification.ShapeProof
