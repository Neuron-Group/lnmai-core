import Verification.PrefixDirectivesExtracted
import Verification.PrefixDirectivesReference
import Verification.SlideChainFrontend

/-! Compose the actual prefix and sanitizer kernels used by tokenize.rs with
the actual connected-slide core. Input/output UTF-8 conversion and allocation
remain primitive representation obligations; no complete prefix, sanitizer or
parser equivalence is a premise. This is not extraction of the runtime facade.
Mainline compilation and the independent axiom audit are pending. -/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai

namespace Verification.SlideChainPrefixFrontend

open SlideChainReference (OriginalSegment)
open SlideChainExtracted (EncodingLaw)
open SlideChainWrapper (originalWrapper)
open SlideChainFrontend (encodingLaw_mono parseSanitized parseSanitized_reference)
open SlideParserProof (keepSlideChar)
open PrefixDirectivesModel (stripChars)

/-- One input character collection, then the two real kernels. The runtime
keeps the intermediate Vec, so no String conversion premise is needed there. -/
def sanitizeChars (chars : Slice Char) : Result (alloc.vec.Vec Char) := do
  let stripped ← prefix_directives.strip_prefix_chars chars
  slide_parser_text.sanitize_slide_chars (alloc.vec.Vec.deref stripped)

theorem sanitize_chars_spec (chars : Slice Char) :
    sanitizeChars chars ⦃ out => out.val.length ≤ chars.val.length ∧
      out.val = (stripChars chars.val).filter keepSlideChar ⦄ := by
  obtain ⟨stripped, hprefix, hprefixLength, hprefixChars⟩ := spec_imp_exists
    (PrefixDirectivesExtracted.strip_prefix_chars_spec chars)
  obtain ⟨kept, hfilter, hfilterLength, hfilterChars⟩ := spec_imp_exists
    (SlideParserTextProof.sanitize_slide_chars_spec (alloc.vec.Vec.deref stripped)
      (Slice.length_ineq (alloc.vec.Vec.deref stripped)))
  apply exists_imp_spec
  refine ⟨kept, ?_, ?_, ?_⟩
  · simp only [sanitizeChars, hprefix, bind_tc_ok, hfilter]
  · exact Nat.le_trans hfilterLength hprefixLength
  · simpa only [alloc.vec.Vec.deref, hprefixChars] using hfilterChars

theorem source_chars (token : String) :
    (sanitizeSlideToken token).toList =
      (stripChars token.toList).filter keepSlideChar := by
  simpa only [keepSlideChar] using
    PrefixDirectivesReference.sanitizeSlideToken_toList token

theorem source_length_le (token : String) :
    (sanitizeSlideToken token).toList.length ≤ token.toList.length := by
  rw [source_chars]
  exact Nat.le_trans (List.length_filter_le _ _)
    (PrefixDirectivesModel.stripChars_length_le token.toList)

/-- String decoding and encoding are separate single-operation boundaries. -/
def sanitizeToken {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char)) (encode : Encode)
    (token : String) : Result Text := do
  let chars ← collectChars token
  let kept ← sanitizeChars chars
  call.call encode kept

theorem sanitize_token_reference {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char)) (encode : Encode)
    (viewText : Text → String) (token : String)
    (hcollect : collectChars token ⦃ chars => chars.val = token.toList ⦄)
    (hencode : EncodingLaw call encode viewText token.toList.length) :
    sanitizeToken call collectChars encode token
      ⦃ out => viewText out = sanitizeSlideToken token ⦄ := by
  obtain ⟨chars, hdecode, hchars⟩ := spec_imp_exists hcollect
  obtain ⟨kept, hsanitize, hlength, hkept⟩ :=
    spec_imp_exists (sanitize_chars_spec chars)
  have hroom : kept.val.length ≤ token.toList.length := by
    simpa only [hchars] using hlength
  obtain ⟨out, hcall, hout⟩ := spec_imp_exists (hencode kept hroom)
  apply exists_imp_spec
  refine ⟨out, ?_, ?_⟩
  · simp only [sanitizeToken, hdecode, bind_tc_ok, hsanitize, hcall]
  · apply String.toList_inj.mp
    rw [hout, String.toList_ofList, hkept, hchars, source_chars]

def parseToken {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char))
    (encode : Encode) (viewText : Text → String) (token : String) :
    Result (Except ParseError (Option (List OriginalSegment))) := do
  let sanitized ← sanitizeToken call collectChars encode token
  parseSanitized call collectChars (viewText sanitized) encode viewText

/-- Full source wrapper, preserving errors, their sanitized rawText, and the
successful zero/one/many-segment threshold. All three parser operations here
are actual extracted kernels, with explicit primitive UTF-8 point contracts. -/
theorem parse_token_reference {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (collectChars : String → Result (Slice Char))
    (encode : Encode) (viewText : Text → String) (token : String)
    (hcollect : collectChars token ⦃ chars => chars.val = token.toList ⦄)
    (hcollectSanitized : collectChars (sanitizeSlideToken token)
      ⦃ chars => chars.val = (sanitizeSlideToken token).toList ⦄)
    (hencode : EncodingLaw call encode viewText token.toList.length) :
    parseToken call collectChars encode viewText token = ok (originalWrapper token) := by
  obtain ⟨sanitized, hsanitize, htext⟩ := spec_imp_exists
    (sanitize_token_reference call collectChars encode viewText token hcollect hencode)
  have hsanitizedEncode : EncodingLaw call encode viewText
      (sanitizeSlideToken token).toList.length :=
    encodingLaw_mono call encode viewText _ _ (source_length_le token) hencode
  simp only [parseToken, hsanitize, bind_tc_ok, htext]
  rw [parseSanitized_reference call collectChars (sanitizeSlideToken token)
    encode viewText hcollectSanitized hsanitizedEncode]
  rw [← SlideChainWrapper.originalWrapper_factor token]

end Verification.SlideChainPrefixFrontend
