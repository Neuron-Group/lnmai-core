import Verification.SlideParserText
import Verification.SlideChainExtracted
import Verification.SlideChainWrapper

/-!
Source-level entry composition using the actual extracted filtering and chain
kernels. This is a proof-script draft for the main worker to elaborate.

The prefix operation is the ORIGINAL Lean stripPrefixDirectives, concretely.
No equivalence of the native Rust prefix routine is assumed or proved here.
The runtime tokenize.rs sanitizer still uses a native iterator: installing
this source composition alone does not certify that iterator or wire it to
the shared sanitize_slide_chars kernel.

Only String/UTF-8 collection and a single Vec<char>-to-Text encoding call have
representation premises. They include successful allocation and byte-capacity
obligations when instantiated in Rust. No parser or sanitizer equality is a
premise. All parse errors below are concrete records, not an error callback.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai

namespace Verification.SlideChainFrontend

open SlideChainReference (OriginalSegment)
open SlideChainExtracted (EncodingLaw decodeError decodeResult decodeSegments)
open SlideChainWrapper (selectChain referenceChars originalWrapper)
open SlideParserProof (keepSlideChar)
open private sanitizeSlideText from LnmaiCore.Simai.SlideParser

/-- The slide-text filter is exactly the suffix of the original token sanitizer.
This is a definitional factorization of original Lean, not a runtime premise. -/
theorem sanitizer_prefix_factor (token : String) :
    sanitizeSlideText (stripPrefixDirectives token) = sanitizeSlideToken token := rfl

/-- Unicode scalar order is unchanged by the source filter, including inside
brackets and between two p/q characters. Prefix removal happens first. -/
theorem sanitized_chars (token : String) :
    (sanitizeSlideToken token).toList =
      (stripPrefixDirectives token).toList.filter keepSlideChar := by
  change (String.ofList
    ((stripPrefixDirectives token).toList.filter keepSlideChar)).toList = _
  rw [String.toList_ofList]

theorem sanitized_length_le (token : String) :
    (sanitizeSlideToken token).toList.length ≤
      (stripPrefixDirectives token).toList.length := by
  rw [sanitized_chars]
  exact List.length_filter_le _ _

/-- Restrict an existing encoding POINT law to a smaller character bound. -/
theorem encodingLaw_mono {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (encode : Encode) (viewText : Text → String) (small large : Nat)
    (hbound : small ≤ large) (hlaw : EncodingLaw call encode viewText large) :
    EncodingLaw call encode viewText small := by
  intro text htext
  exact hlaw text (Nat.le_trans htext hbound)

/-- Generated filtering followed by the SAME primitive encoding operation
used by the actual extracted chain renderer. Encoding can fail in Result;
no filtering or parsing is delegated to that operation. -/
def sanitizeEncoded {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (encode : Encode) : Result Text := do
  let kept ← slide_parser_text.sanitize_slide_chars chars
  call.call encode kept

/-- The filter loop theorem supplies both character order and the allocation
bound for this single encoding call. UTF-8 byte capacity is still a point law. -/
theorem sanitizeEncoded_spec {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (encode : Encode) (viewText : Text → String)
    (raw : String) (hchars : chars.val = raw.toList)
    (hencode : EncodingLaw call encode viewText chars.val.length) :
    sanitizeEncoded call chars encode
      ⦃ text => viewText text = sanitizeSlideText raw ⦄ := by
  apply exists_imp_spec
  obtain ⟨kept, hfilter, hlen, hkept⟩ := spec_imp_exists
    (SlideParserTextProof.sanitize_slide_chars_spec chars (Slice.length_ineq chars))
  obtain ⟨text, hcall, htext⟩ := spec_imp_exists (hencode kept hlen)
  refine ⟨text, ?_, ?_⟩
  · simp only [sanitizeEncoded, hfilter, bind_tc_ok, hcall]
  · calc
      viewText text = String.ofList kept.val := htext
      _ = String.ofList (raw.toList.filter keepSlideChar) := by rw [hkept, hchars]
      _ = sanitizeSlideText raw :=
        SlideParserProof.sanitize_filter_equiv raw keepSlideChar
          SlideParserProof.keep_slide_char_equiv

/-- Exact runtime finite-error fields. The native Rust String allocation used
by syntax_error is a representation boundary, not hidden in an Agrees premise. -/
def chainParseError (raw : String) (error : slide_chain.ChainError) : ParseError :=
  { kind := .invalidSyntax
    rawText := raw
    message := match error with
      | .InvalidSyntax => "invalid connected slide syntax"
      | .FreshNumericHead => "connected slide chain cannot contain a fresh numeric head"
      | .UnterminatedTiming => "unterminated slide timing spec"
    span := none }

theorem chainParseError_reference (raw : String) (error : slide_chain.ChainError) :
    chainParseError raw error = SlideChainReference.decodeIssue raw (decodeError error) := by
  cases error <;> rfl

/-- The source wrapper examines the actual Vec length only AFTER the complete
core succeeds. Error outcomes never pass through the <= 1 threshold. -/
def finish {Text : Type} (raw : String) (viewText : Text → String)
    (parsed : core.result.Result
      (alloc.vec.Vec (slide_chain.Segment Text)) slide_chain.ChainError) :
    Except ParseError (Option (List OriginalSegment)) :=
  match parsed with
  | .Err error => .error (chainParseError raw error)
  | .Ok segments =>
      if alloc.vec.Vec.len segments ≤ 1#usize then .ok none
      else .ok (some (decodeSegments viewText segments))

/-- Vec length, exact ordered decoded segment list, and has_timing all survive
this factoring. The only discarded successful lists have length zero or one. -/
theorem finish_reference {Text : Type} (raw : String) (viewText : Text → String)
    (parsed : core.result.Result
      (alloc.vec.Vec (slide_chain.Segment Text)) slide_chain.ChainError) :
    finish raw viewText parsed =
      SlideChainReference.resultView raw id
        ((decodeResult (decodeSegments viewText) parsed).map selectChain) := by
  cases parsed with
  | Err error =>
      change Except.error (chainParseError raw error) =
        Except.error (SlideChainReference.decodeIssue raw (decodeError error))
      rw [chainParseError_reference]
  | Ok segments =>
      have hlen : (alloc.vec.Vec.len segments ≤ 1#usize) ↔
          (decodeSegments viewText segments).length ≤ 1 := by
        simp [UScalar.le_equiv, decodeSegments]
      simp only [finish, decodeResult, Except.map, SlideChainReference.resultView, id_eq]
      rw [hlen]
      by_cases hsmall : (decodeSegments viewText segments).length ≤ 1 <;>
        simp [selectChain, hsmall]

/-- A nonempty representable full input always has a STRICTLY representable
suffix. Its length + 1 is exactly the full input length: no extra +1 bound is
imposed on the original token, including at Usize.max. -/
theorem tail_capacity (chars : Slice Char) (head : Char) (rest : List Char)
    (hchars : chars.val = head :: rest) :
    (chars.drop 1#usize).val = rest ∧
      (chars.drop 1#usize).val.length < Usize.max ∧
      (chars.drop 1#usize).val.length + 1 = chars.val.length := by
  have htail : (chars.drop 1#usize).val = rest := by simp [Slice.drop, hchars]
  have hlen : (chars.drop 1#usize).val.length + 1 = chars.val.length := by
    simp only [htail, hchars, List.length_cons]
  have hsize := Slice.length_ineq chars
  exact ⟨htail, by omega, hlen⟩

/-- Source factorization of split_first and the original caller's guard. The
character predicate and complete chain parse are the ACTUAL generated functions.
The list case and drop are explicit primitive slice observations, not a whole
wrapper callback. The Rust wrapper itself has not been extracted here. -/
def parseChars {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (raw : String) (chars : Slice Char) (encode : Encode) (viewText : Text → String) :
    Result (Except ParseError (Option (List OriginalSegment))) :=
  match chars.val with
  | [] => ok (.ok none)
  | head :: _ => do
      let digit ← slide_chain.is_chain_digit head
      if !digit then ok (.ok none)
      else
        let parsed ← slide_chain.parse_segments call (chars.drop 1#usize) head encode
        ok (finish raw viewText parsed)

/-- Complete generated core + wrapper equation. Empty and non-ASCII-digit
heads do not invoke the chain core. ASCII 0 and 9 use the same complete parser
as 1..8; syntax errors propagate before thresholding. -/
theorem parseChars_model {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (raw : String) (chars : Slice Char) (encode : Encode) (viewText : Text → String)
    (hencode : EncodingLaw call encode viewText chars.val.length) :
    parseChars call raw chars encode viewText =
      ok (SlideChainReference.resultView raw id (SlideChainWrapper.wrapperChars chars.val)) := by
  cases hchars : chars.val with
  | nil =>
      simp only [parseChars, hchars, SlideChainWrapper.wrapperChars,
        SlideChainReference.resultView, id_eq]
  | cons head rest =>
      cases hdigit : SlideChainModel.isChainDigit head with
      | false =>
          simp [parseChars, hchars, SlideChainExtracted.is_chain_digit_equiv, hdigit,
            SlideChainWrapper.wrapperChars, SlideChainReference.resultView]
      | true =>
          obtain ⟨htail, hsize, hlen⟩ := tail_capacity chars head rest hchars
          have htailEncode : EncodingLaw call encode viewText
              ((chars.drop 1#usize).val.length + 1) := by
            rw [hlen]
            simpa only [hchars] using hencode
          obtain ⟨parsed, hparse, hparsed⟩ := spec_imp_exists
            (SlideChainExtracted.parse_segments_spec call (chars.drop 1#usize)
              head encode viewText hsize htailEncode)
          have hfinish : finish raw viewText parsed =
              SlideChainReference.resultView raw id
                ((SlideChainModel.parseSegments rest head).map selectChain) := by
            rw [finish_reference, hparsed, htail]
          simp [parseChars, hchars, SlideChainExtracted.is_chain_digit_equiv, hdigit,
            hparse, hfinish, SlideChainWrapper.wrapperChars]

/-- Reuse the proved source grammar correspondence, rather than accepting a
complete parser-equivalence premise from the caller. -/
theorem parseChars_reference {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (raw : String) (chars : Slice Char) (encode : Encode) (viewText : Text → String)
    (hencode : EncodingLaw call encode viewText chars.val.length) :
    parseChars call raw chars encode viewText = ok (referenceChars raw chars.val) := by
  rw [parseChars_model call raw chars encode viewText hencode,
    SlideChainWrapper.wrapperChars_reference]

/-- The collector is only String.chars().collect(), with no filtering or
parsing responsibility. Failure/allocation stays in the outer Result. -/
def parseSanitized {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char)) (raw : String)
    (encode : Encode) (viewText : Text → String) :
    Result (Except ParseError (Option (List OriginalSegment))) := do
  let chars ← collectChars raw
  parseChars call raw chars encode viewText

theorem parseSanitized_reference {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char)) (raw : String)
    (encode : Encode) (viewText : Text → String)
    (hcollect : collectChars raw ⦃ chars => chars.val = raw.toList ⦄)
    (hencode : EncodingLaw call encode viewText raw.toList.length) :
    parseSanitized call collectChars raw encode viewText =
      ok (referenceChars raw raw.toList) := by
  obtain ⟨chars, hrun, hchars⟩ := spec_imp_exists hcollect
  have hcharsEncode : EncodingLaw call encode viewText chars.val.length := by
    rw [hchars]
    exact hencode
  unfold parseSanitized
  rw [hrun]
  simp only [bind_tc_ok]
  rw [parseChars_reference call raw chars encode viewText hcharsEncode, hchars]

/-- Original source prefix operation; generated sanitizer; explicit String
encode/decode; generated chain core; complete source wrapper.

The source prefix operation is NOT an opaque frontend callback. Using it here
states the proved scope without assuming that native Rust prefix processing,
its trim policy, or the currently native tokenizer filter has been verified. -/
def parseTokenSource {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char))
    (encode : Encode) (viewText : Text → String) (token : String) :
    Result (Except ParseError (Option (List OriginalSegment))) := do
  let chars ← collectChars (stripPrefixDirectives token)
  let sanitized ← sanitizeEncoded call chars encode
  parseSanitized call collectChars (viewText sanitized) encode viewText

/-- Entry equality at the ACTUAL original private parseContinuousSlideSegments?.
All errors and None/Some branches are included. The two collection premises
concern a single UTF-8 decoding operation at each concrete String, and the only
encoding premise concerns a single Vec<char> call. There is no whole sanitizer,
whole wrapper, grammar, or renderer equivalence hypothesis.

The character bound covers BOTH filtering and chain output: filtered length
cannot grow, and chain rendering prepends its head only after that head has
already been removed from the suffix supplied to the generated core. -/
theorem parseTokenSource_reference {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char))
    (encode : Encode) (viewText : Text → String) (token : String)
    (hcollectPrefix : collectChars (stripPrefixDirectives token)
      ⦃ chars => chars.val = (stripPrefixDirectives token).toList ⦄)
    (hcollectSanitized : collectChars (sanitizeSlideToken token)
      ⦃ chars => chars.val = (sanitizeSlideToken token).toList ⦄)
    (hencode : EncodingLaw call encode viewText (stripPrefixDirectives token).toList.length) :
    parseTokenSource call collectChars encode viewText token = ok (originalWrapper token) := by
  obtain ⟨chars, hcollect, hchars⟩ := spec_imp_exists hcollectPrefix
  have hcharsEncode : EncodingLaw call encode viewText chars.val.length := by
    rw [hchars]
    exact hencode
  obtain ⟨sanitized, hsanitize, htext⟩ := spec_imp_exists
    (sanitizeEncoded_spec call chars encode viewText (stripPrefixDirectives token)
      hchars hcharsEncode)
  rw [sanitizer_prefix_factor] at htext
  have hsanitizedEncode : EncodingLaw call encode viewText
      (sanitizeSlideToken token).toList.length :=
    encodingLaw_mono call encode viewText _ _ (sanitized_length_le token) hencode
  unfold parseTokenSource
  rw [hcollect]
  simp only [bind_tc_ok]
  rw [hsanitize]
  simp only [bind_tc_ok]
  rw [htext, parseSanitized_reference call collectChars (sanitizeSlideToken token)
    encode viewText hcollectSanitized hsanitizedEncode]
  rw [← SlideChainWrapper.originalWrapper_factor token]

end Verification.SlideChainFrontend

