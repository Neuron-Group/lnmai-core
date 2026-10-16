import Verification.SlideChainModel
import Verification.NaturalParser
import Verification.Tokenize

/-!
Actual extraction-3 slide_chain reads, interpreted in SlideChainModel.
Extraction SHA256: d037f357eb395803e657d1ca4b445cc73908e6910c59bb5c37af0de3493ab1bf.
This file is a proof draft awaiting the main worker's single Lean compiler.
No source-model equivalence premise is used. No new primitive laws are needed
for these reads: Slice indexing and bounded usize addition use Aeneas proofs.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.SlideChainExtracted

abbrev Issue := SlideChainReference.Issue

def decodeError : slide_chain.ChainError → Issue
  | .InvalidSyntax => .invalidSyntax
  | .FreshNumericHead => .freshNumericHead
  | .UnterminatedTiming => .unterminatedTiming

def decodeResult (view : α → β) :
    core.result.Result α slide_chain.ChainError → Except Issue β
  | .Ok value => .ok (view value)
  | .Err error => .error (decodeError error)

def decodePair (p : Char × Usize) : Char × Nat := (p.1, p.2.val)

def decodePlan (p : slide_chain.SegmentPlan) : SlideChainModel.Plan :=
  { head := p.head, begin := p.«begin».val, stop := p.«end».val,
    hasTiming := p.has_timing }

def decodePlans (plans : alloc.vec.Vec slide_chain.SegmentPlan) :
    List SlideChainModel.Plan := plans.val.map decodePlan

theorem is_chain_digit_equiv (c : Char) :
    slide_chain.is_chain_digit c = ok (SlideChainModel.isChainDigit c) := by
  simp only [slide_chain.is_chain_digit, SlideChainModel.isChainDigit]
  split_ifs <;> simp_all

/-- All cursors, including past-end cursors. A successful digit read advances
exactly once; zero and nine are accepted, with no lane restriction. -/
theorem read_digit_spec (chars : Slice Char) (cursor : Usize) :
    slide_chain.read_digit chars cursor
      ⦃ out => decodeResult decodePair out =
        SlideChainModel.readDigitAt chars.val cursor.val ⦄ := by
  apply exists_imp_spec
  by_cases hmore : cursor < Slice.len chars
  · have hi : cursor.val < chars.val.length := by simpa using hmore
    obtain ⟨c, hget, hc⟩ := spec_imp_exists
      (Slice.index_usize_spec chars cursor hi)
    have hlookup : chars.val[cursor.val]? = some c := by
      simpa only [List.getElem?_eq_getElem hi, hc]
    cases hdigit : SlideChainModel.isChainDigit c with
    | false =>
        refine ⟨.Err .InvalidSyntax, ?_, ?_⟩
        · simp [slide_chain.read_digit, hmore, hget, is_chain_digit_equiv, hdigit]
        · simp [decodeResult, decodeError, SlideChainModel.readDigitAt, hlookup, hdigit]
    | true =>
        obtain ⟨next, hnext, hn⟩ := spec_imp_exists
          (UScalar.add_spec (x := cursor) (y := 1#usize)
            (NaturalParserProof.scan_index_increment_bound chars cursor hi))
        have hnv : next.val = cursor.val + 1 := by simpa using hn
        refine ⟨.Ok (c, next), ?_, ?_⟩
        · simp [slide_chain.read_digit, hmore, hget, is_chain_digit_equiv, hdigit, hnext]
        · simp [decodeResult, decodePair, SlideChainModel.readDigitAt, hlookup, hdigit, hnv]
  · have ho : chars.val.length ≤ cursor.val := by simpa using hmore
    refine ⟨.Err .InvalidSyntax, ?_, ?_⟩
    · simp [slide_chain.read_digit, hmore]
    · simp [decodeResult, decodeError, SlideChainModel.readDigitAt_oob _ _ ho]

theorem read_digit_equiv (chars : Slice Char) (cursor : Usize) :
    (do let out ← slide_chain.read_digit chars cursor
        ok (decodeResult decodePair out)) =
      ok (SlideChainModel.readDigitAt chars.val cursor.val) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_digit_spec chars cursor)
  simp only [hrun, bind_tc_ok, hpost]

/-- Factoring only the generated p/q selection block. This is an equality
proved from the real extraction, not a replacement extracted declaration. -/
theorem read_shape_non_v (chars : Slice Char) (op : Char) (cursor : Usize)
    (hv : op ≠ 'V') :
    slide_chain.read_shape chars op cursor =
      (do
        let next ←
          if op = 'p' || op = 'q' then
            if cursor < Slice.len chars then do
              let c ← Slice.index_usize chars cursor
              if c = op then cursor + 1#usize else ok cursor
            else ok cursor
          else ok cursor
        slide_chain.read_digit chars next) := by
  by_cases hp : op = 'p'
  · simp [slide_chain.read_shape, hv, hp]
  · by_cases hq : op = 'q' <;> simp [slide_chain.read_shape, hv, hp, hq]

/-- V consumes two required digits; pp/qq consumes the repeated mark only
when the indexed character is equal. Every error is preserved. -/
theorem read_shape_spec (chars : Slice Char) (op : Char) (cursor : Usize) :
    slide_chain.read_shape chars op cursor
      ⦃ out => decodeResult decodePair out =
        SlideChainModel.readShapeAt chars.val op cursor.val ⦄ := by
  apply exists_imp_spec
  by_cases hv : op = 'V'
  · obtain ⟨first, hfirst, hp⟩ := spec_imp_exists (read_digit_spec chars cursor)
    cases first with
    | Err error =>
        refine ⟨.Err error, ?_, ?_⟩
        · simp [slide_chain.read_shape, hv, hfirst]
        · simp [SlideChainModel.readShapeAt, hv, ← hp, decodeResult, Bind.bind, Except.bind]
    | Ok pair =>
        rcases pair with ⟨middle, next⟩
        obtain ⟨last, hlast, hl⟩ := spec_imp_exists (read_digit_spec chars next)
        refine ⟨last, ?_, ?_⟩
        · simp [slide_chain.read_shape, hv, hfirst, hlast]
        · simpa only [SlideChainModel.readShapeAt, if_pos hv, ← hp, decodeResult,
            decodePair, Bind.bind, Except.bind] using hl
  · rw [read_shape_non_v chars op cursor hv]
    by_cases hdouble : (op = 'p' || op = 'q') = true
    · by_cases hmore : cursor < Slice.len chars
      · have hi : cursor.val < chars.val.length := by simpa using hmore
        obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
        have hlookup : chars.val[cursor.val]? = some c := by
          simpa only [List.getElem?_eq_getElem hi, hc]
        by_cases heq : c = op
        · obtain ⟨next, hnext, hn⟩ := spec_imp_exists
            (UScalar.add_spec (x := cursor) (y := 1#usize)
              (NaturalParserProof.scan_index_increment_bound chars cursor hi))
          have hnv : next.val = cursor.val + 1 := by simpa using hn
          obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_digit_spec chars next)
          refine ⟨out, ?_, ?_⟩
          · simp [hdouble, hmore, hget, heq, hnext, hrun]
          · simpa [SlideChainModel.readShapeAt, SlideChainModel.shapeCursor, hv, hdouble, hlookup, heq, hnv] using hpost
        · obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_digit_spec chars cursor)
          refine ⟨out, ?_, ?_⟩
          · simp [hdouble, hmore, hget, heq, hrun]
          · simpa [SlideChainModel.readShapeAt, SlideChainModel.shapeCursor, hv, hdouble, hlookup, heq] using hpost
      · have ho : chars.val.length ≤ cursor.val := by simpa using hmore
        have hlookup : chars.val[cursor.val]? = none := List.getElem?_eq_none ho
        obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_digit_spec chars cursor)
        refine ⟨out, ?_, ?_⟩
        · simp [hdouble, hmore, hrun]
        · simpa [SlideChainModel.readShapeAt, SlideChainModel.shapeCursor, hv, hdouble, hlookup] using hpost
    · obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_digit_spec chars cursor)
      refine ⟨out, ?_, ?_⟩
      · simp [hdouble, hrun]
      · simpa [SlideChainModel.readShapeAt, SlideChainModel.shapeCursor, hv, hdouble] using hpost

theorem read_shape_equiv (chars : Slice Char) (op : Char) (cursor : Usize) :
    (do let out ← slide_chain.read_shape chars op cursor
        ok (decodeResult decodePair out)) =
      ok (SlideChainModel.readShapeAt chars.val op cursor.val) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_shape_spec chars op cursor)
  simp only [hrun, bind_tc_ok, hpost]

/-- Bounds follow from the existing model definition, not an input-validity
assumption. They will be used to justify the actual renderer's indexed reads. -/
theorem readDigitAt_bounds (chars : List Char) (cursor : Nat) (c : Char) (next : Nat)
    (h : SlideChainModel.readDigitAt chars cursor = .ok (c, next)) :
    next = cursor + 1 ∧ next ≤ chars.length := by
  by_cases hi : cursor < chars.length
  · cases hd : SlideChainModel.isChainDigit chars[cursor] with
    | false => simp [SlideChainModel.readDigitAt, List.getElem?_eq_getElem hi, hd] at h
    | true =>
        simp only [SlideChainModel.readDigitAt, List.getElem?_eq_getElem hi, hd,
          if_true, Except.ok.injEq, Prod.mk.injEq] at h
        exact ⟨h.2.symm, by omega⟩
  · have ho : chars.length ≤ cursor := by omega
    rw [SlideChainModel.readDigitAt_oob chars cursor ho] at h
    cases h

theorem readShapeAt_bounds (chars : List Char) (op : Char) (cursor : Nat)
    (c : Char) (next : Nat)
    (h : SlideChainModel.readShapeAt chars op cursor = .ok (c, next)) :
    cursor < next ∧ next ≤ chars.length := by
  have hs := SlideChainModel.readShapeAt_success chars op cursor c next h
  exact ⟨hs.1, hs.2.1⟩

end Verification.SlideChainExtracted
