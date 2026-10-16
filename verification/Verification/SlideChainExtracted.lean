import Verification.SlideChainExtractedEncode

/-!
Actual extraction-3 slide_chain composition with the existing character/index
model and the ACTUAL original private Lean parser. All files in this delivery
are uncompiled proof scripts. No complete grammar/renderer/encoder equivalence
is a hypothesis. Source helper correspondence is reused from Linnaeus.

Explicit open runtime boundaries:
* UTF-8 String decoding into the supplied character Slice;
* the one-call EncodingLaw, including concrete UTF-8 byte capacity/allocation;
* instantiation of runtime ParseError construction with decodeIssue;
* the sanitizer/None-threshold wrapper and downstream frontend composition.
The strict character capacity bound below accounts for the synthetic head.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.SlideChainExtracted

open private readDigitChar parseSlideShapeChars readBracketSuffixFuel
  readBracketSuffix from LnmaiCore.Simai.Tokenize

/-- The successful digit and the complete original ParseError are both
interpreted, for every representable cursor including past-end cursors. -/
theorem read_digit_reference (raw : String) (chars : Slice Char) (cursor : Usize) :
    (do let out ← slide_chain.read_digit chars cursor
        ok (SlideChainReference.resultView raw
          (fun value => (value.1, chars.val.drop value.2)) (decodeResult decodePair out))) =
      ok (readDigitChar raw (chars.val.drop cursor.val)) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_digit_spec chars cursor)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.readDigitAt_reference]

theorem read_shape_reference (raw : String) (chars : Slice Char) (op : Char) (cursor : Usize) :
    (do let out ← slide_chain.read_shape chars op cursor
        ok (SlideChainReference.resultView raw
          (fun value => (String.ofList
            (op :: SlideChainModel.sourceRange chars.val cursor.val value.2),
            chars.val.drop value.2)) (decodeResult decodePair out))) =
      ok (parseSlideShapeChars raw op (chars.val.drop cursor.val)) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (read_shape_spec chars op cursor)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.readShapeAt_reference]

theorem timing_end_with_fuel_reference (raw : String) (chars : Slice Char)
    (cursor fuel : Usize) (acc : List Char) :
    (do let out ← slide_chain.timing_end_with_fuel chars cursor fuel
        ok (SlideChainReference.resultView raw
          (fun stop => (String.ofList
            (acc.reverse ++ SlideChainModel.sourceRange chars.val cursor.val stop),
            chars.val.drop stop)) (decodeResult UScalar.val out))) =
      ok (readBracketSuffixFuel fuel.val raw (chars.val.drop cursor.val) acc) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (timing_end_with_fuel_spec chars cursor fuel)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.timingEndFuel_reference]

theorem timing_end_reference (raw : String) (chars : Slice Char)
    (cursor : Usize) (acc : List Char) :
    (do let out ← slide_chain.timing_end chars cursor
        ok (SlideChainReference.resultView raw
          (fun stop => (String.ofList
            (acc.reverse ++ SlideChainModel.sourceRange chars.val cursor.val stop),
            chars.val.drop stop)) (decodeResult UScalar.val out))) =
      ok (readBracketSuffix raw (chars.val.drop cursor.val) acc) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (timing_end_spec chars cursor)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.timingEnd_reference]

/-- Arbitrary cursor, fuel and accumulator. This is an interpretation of
plans; actual byte/string rendering is proved separately below. -/
theorem parse_plans_loop_reference (raw : String) (chars : Slice Char)
    (head : Char) (fuel : Usize) (plans : alloc.vec.Vec slide_chain.SegmentPlan)
    (cursor : Usize) (hcapacity : plans.val.length + fuel.val ≤ Usize.max) :
    (do let out ← slide_chain.parse_plans_with_fuel_loop chars head fuel plans cursor
        ok (SlideChainReference.resultView raw (List.map (SlideChainModel.renderSegment chars.val))
          (decodeResult decodePlans out))) =
      ok (SlideChainReference.prefixResult
        ((decodePlans plans).map (SlideChainModel.renderSegment chars.val))
        (SlideChainReference.originalFuel fuel.val raw head (chars.val.drop cursor.val))) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (parse_plans_with_fuel_loop_spec chars head fuel plans cursor hcapacity)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.scanFuel_reference,
    SlideChainReference.collectFuel_reference]

theorem parse_plans_with_fuel_reference (raw : String) (chars : Slice Char)
    (head : Char) (fuel : Usize) :
    (do let out ← slide_chain.parse_plans_with_fuel chars head fuel
        ok (SlideChainReference.resultView raw (List.map (SlideChainModel.renderSegment chars.val))
          (decodeResult decodePlans out))) =
      ok (SlideChainReference.originalFuel fuel.val raw head chars.val) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (parse_plans_with_fuel_spec chars head fuel)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.parsePlansFuel_reference]

/-- Parse errors return before encoding, so this theorem needs neither the
encoding point law nor the extra output-character capacity bound. -/
theorem parse_segments_error_spec {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (head : Char) (encode : Encode) (issue : Issue)
    (herror : SlideChainModel.parsePlans chars.val head = .error issue) :
    slide_chain.parse_segments call chars head encode
      ⦃ out => ∃ error : slide_chain.ChainError,
        out = .Err error ∧ decodeError error = issue ⦄ := by
  apply exists_imp_spec
  obtain ⟨parsed, hparse, hpost⟩ := spec_imp_exists (parse_plans_spec chars head)
  cases parsed with
  | Ok plans =>
      have hbad : (Except.ok (decodePlans plans) : Except Issue (List SlideChainModel.Plan)) =
          .error issue := hpost.trans herror
      cases hbad
  | Err error =>
      have heq : decodeError error = issue := by
        have hbad := hpost.trans herror
        simpa only [decodeResult, Except.error.injEq] using hbad
      refine ⟨.Err error, ?_, error, rfl, heq⟩
      simp [slide_chain.parse_segments, hparse]

/-- Complete generated parse_segments, not merely parse_plans: all successful
segment records are rendered/encoded by the real loops and all errors agree. -/
theorem parse_segments_spec {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (head : Char) (encode : Encode) (viewText : Text → String)
    (hsize : chars.val.length < Usize.max)
    (hencode : EncodingLaw call encode viewText (chars.val.length + 1)) :
    slide_chain.parse_segments call chars head encode
      ⦃ out => decodeResult (decodeSegments viewText) out =
        SlideChainModel.parseSegments chars.val head ⦄ := by
  apply exists_imp_spec
  obtain ⟨parsed, hparse, hpost⟩ := spec_imp_exists (parse_plans_spec chars head)
  cases parsed with
  | Err error =>
      have hmodel : SlideChainModel.parsePlans chars.val head =
          .error (decodeError error) := hpost.symm
      refine ⟨.Err error, ?_, ?_⟩
      · simp [slide_chain.parse_segments, hparse]
      · simp only [decodeResult, SlideChainModel.parseSegments, hmodel, Except.map]
  | Ok plans =>
      have hmodel : SlideChainModel.parsePlans chars.val head = .ok (decodePlans plans) :=
        hpost.symm
      have hplans : ∀ plan ∈ (alloc.vec.Vec.deref plans).val, RenderablePlan chars.val plan := by
        simpa only [alloc.vec.Vec.deref] using parse_plans_renderable chars head plans hsize hmodel
      obtain ⟨segments, hencodeRun, hsegments⟩ := spec_imp_exists
        (encode_plans_spec call chars (alloc.vec.Vec.deref plans) encode viewText hplans hencode)
      have hvalues : decodeSegments viewText segments =
          (decodePlans plans).map (SlideChainModel.renderSegment chars.val) := by
        simpa only [alloc.vec.Vec.deref, decodePlans, List.map_map, Function.comp_def] using hsegments
      refine ⟨.Ok segments, ?_, ?_⟩
      · simp [slide_chain.parse_segments, hparse, hencodeRun]
      · simp only [decodeResult, SlideChainModel.parseSegments, hmodel, Except.map, hvalues]

theorem parse_segments_equiv {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (head : Char) (encode : Encode) (viewText : Text → String)
    (hsize : chars.val.length < Usize.max)
    (hencode : EncodingLaw call encode viewText (chars.val.length + 1)) :
    (do let out ← slide_chain.parse_segments call chars head encode
        ok (decodeResult (decodeSegments viewText) out)) =
      ok (SlideChainModel.parseSegments chars.val head) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (parse_segments_spec call chars head encode viewText hsize hencode)
  simp only [hrun, bind_tc_ok, hpost]

/-- Exact original segment records and original ParseError interpretation.
The original parser is a private-definition abbrev supplied by Reference,
not a reimplementation of Rust or a whole-function agreement hypothesis. -/
theorem parse_segments_reference {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (raw : String) (chars : Slice Char) (head : Char) (encode : Encode)
    (viewText : Text → String) (hsize : chars.val.length < Usize.max)
    (hencode : EncodingLaw call encode viewText (chars.val.length + 1)) :
    (do let out ← slide_chain.parse_segments call chars head encode
        ok (SlideChainReference.resultView raw id (decodeResult (decodeSegments viewText) out))) =
      ok (SlideChainReference.original raw head chars.val) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (parse_segments_spec call chars head encode viewText hsize hencode)
  simp only [hrun, bind_tc_ok, hpost, SlideChainModel.parseSegments_reference]

end Verification.SlideChainExtracted
