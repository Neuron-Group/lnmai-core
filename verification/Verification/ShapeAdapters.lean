import Verification.Shape

namespace Verification.ShapeProof
open Aeneas Aeneas.Std Result lnmai_core_verify
open SymmetryProof

/- The runtime's Natural endpoints need not fit in usize. These adapters
   compose the actual extracted small-endpoint operations with the fallback
   branches in simai/shape.rs. The conversion contract below is explicit:
   this does not extract or assume a proof of the bigint implementation. -/

def EndpointConversion (encode : Nat → Option Usize) : Prop :=
  ∀ n, match encode n with
    | some small => small.val = n
    | none => Usize.max < n

def largeCanonicalOpt (kind : shape.SlideKind) (n : Nat) :
    Option LnmaiCore.Simai.CanonicalSlideShape :=
  match kind with
  | .Circle => some (.circle n)
  | .V => some (.v n)
  | .Pq => some (.pq n)
  | .Ppqq => some (.ppqq n)
  | _ => none

def largeCanonicalUnchecked (kind : shape.SlideKind) (n : Nat) :
    LnmaiCore.Simai.CanonicalSlideShape :=
  match kind with
  | .Line => .line n
  | .Circle => .circle n
  | .V => .v n
  | .Turn => .turn n
  | .Pq => .pq n
  | .Ppqq => .ppqq n
  | .S => .s
  | .Wifi => .wifi

theorem large_canonical_opt_equiv (kind : shape.SlideKind) (n : Nat)
    (hn : 8 < n) :
    largeCanonicalOpt kind n =
      LnmaiCore.Simai.mkCanonicalSlideShape? (toLnmKind kind) (some n) := by
  have hupper : ¬n ≤ 8 := by omega
  have hfive : n ≠ 5 := by omega
  cases kind <;>
    simp [largeCanonicalOpt, LnmaiCore.Simai.mkCanonicalSlideShape?,
      toLnmKind, hupper, hfive]

theorem large_canonical_unchecked_equiv (kind : shape.SlideKind) (n : Nat) :
    largeCanonicalUnchecked kind n =
      LnmaiCore.Simai.mkCanonicalSlideShapeUnchecked (toLnmKind kind) (some n) := by
  cases kind <;> rfl

def canonicalOptAdapter (encode : Nat → Option Usize)
    (kind : shape.SlideKind) (endpoint : Option Nat) :
    Result (Option LnmaiCore.Simai.CanonicalSlideShape) := do
  match endpoint with
  | none =>
      let out ← shape.mk_canonical_slide_shape_opt kind none
      ok (out.map toLnmCanonical)
  | some n =>
      match encode n with
      | some small =>
          let out ← shape.mk_canonical_slide_shape_opt kind (some small)
          ok (out.map toLnmCanonical)
      | none => ok (largeCanonicalOpt kind n)

def canonicalUncheckedAdapter (encode : Nat → Option Usize)
    (kind : shape.SlideKind) (endpoint : Option Nat) :
    Result LnmaiCore.Simai.CanonicalSlideShape := do
  match endpoint with
  | none =>
      let out ← shape.mk_canonical_slide_shape_unchecked kind none
      ok (toLnmCanonical out)
  | some n =>
      match encode n with
      | some small =>
          let out ← shape.mk_canonical_slide_shape_unchecked kind (some small)
          ok (toLnmCanonical out)
      | none => ok (largeCanonicalUnchecked kind n)

theorem canonical_opt_unbounded_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (kind : shape.SlideKind)
    (endpoint : Option Nat) :
    canonicalOptAdapter encode kind endpoint =
      ok (LnmaiCore.Simai.mkCanonicalSlideShape? (toLnmKind kind) endpoint) := by
  cases endpoint with
  | none => exact mk_canonical_slide_shape_opt_equiv kind none
  | some n =>
      have hn := hencode n
      cases hsmall : encode n with
      | some small =>
          simp only [hsmall] at hn
          simpa only [canonicalOptAdapter, hsmall, Option.map_some, hn] using
            mk_canonical_slide_shape_opt_equiv kind (some small)
      | none =>
          simp only [hsmall] at hn
          have hmax : 8 ≤ Usize.max := by scalar_tac
          simp only [canonicalOptAdapter, hsmall,
            large_canonical_opt_equiv kind n (by omega)]

theorem canonical_unchecked_unbounded_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (kind : shape.SlideKind)
    (endpoint : Option Nat) :
    canonicalUncheckedAdapter encode kind endpoint =
      ok (LnmaiCore.Simai.mkCanonicalSlideShapeUnchecked (toLnmKind kind) endpoint) := by
  cases endpoint with
  | none => exact mk_canonical_slide_shape_unchecked_equiv kind none
  | some n =>
      have hn := hencode n
      cases hsmall : encode n with
      | some small =>
          simp only [hsmall] at hn
          simpa only [canonicalUncheckedAdapter, hsmall, Option.map_some, hn] using
            mk_canonical_slide_shape_unchecked_equiv kind (some small)
      | none =>
          simp only [canonicalUncheckedAdapter, hsmall, large_canonical_unchecked_equiv]

open private LnmaiCore.Simai.mirrorKey LnmaiCore.Simai.mirrorRelEnd
  from LnmaiCore.Simai.Shape

theorem mirror_key_large (n : Nat) (hn : 8 < n) :
    LnmaiCore.Simai.mirrorKey n = n := by
  unfold LnmaiCore.Simai.mirrorKey
  split <;> omega

theorem canonical_rel_end_large (sym : LnmaiCore.Simai.SlideSymmetry)
    (n : Nat) (hn : 8 < n) :
    LnmaiCore.Simai.canonicalRelEnd sym n = n := by
  simp [LnmaiCore.Simai.canonicalRelEnd, LnmaiCore.Simai.mirrorRelEnd,
    mirror_key_large n hn]

def canonicalRelEndAdapter (encode : Nat → Option Usize)
    (sym : symmetry.SlideSymmetry) (n : Nat) : Result Nat := do
  match encode n with
  | some small =>
      let out ← shape.canonical_rel_end sym small
      ok out.val
  | none => ok n

theorem canonical_rel_end_unbounded_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (sym : symmetry.SlideSymmetry) (n : Nat) :
    canonicalRelEndAdapter encode sym n =
      ok (LnmaiCore.Simai.canonicalRelEnd (toLnmSymmetry sym) n) := by
  have hn := hencode n
  cases hsmall : encode n with
  | some small =>
      simp only [hsmall] at hn
      simpa only [canonicalRelEndAdapter, hsmall, hn] using
        canonical_rel_end_equiv sym small
  | none =>
      simp only [hsmall] at hn
      have hmax : 8 ≤ Usize.max := by scalar_tac
      simp only [canonicalRelEndAdapter, hsmall,
        canonical_rel_end_large (toLnmSymmetry sym) n (by omega)]

theorem base_rel_end_unbounded_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (sym : symmetry.SlideSymmetry) (n : Nat) :
    canonicalRelEndAdapter encode sym n =
      ok (LnmaiCore.Simai.baseRelEnd (toLnmSymmetry sym) n) :=
  canonical_rel_end_unbounded_equiv encode hencode sym n

end Verification.ShapeProof
