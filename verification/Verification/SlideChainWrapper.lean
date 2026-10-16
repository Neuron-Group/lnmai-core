import Verification.SlideChainModel

/-!
Source-only wrapper correspondence for parseContinuousSlideSegments?.
No compiler, test or extraction has been run for this draft.

The source model uses the concrete cursor parser and theorem from SlideChainModel;
it takes no parser callback or whole-operation equivalence hypothesis.
The token wrapper uses the ORIGINAL sanitizeSlideToken. The Rust implementation
of sanitization, UTF-8 collection, allocation and the actual extracted loops
are not certified by this module.
-/

namespace Verification.SlideChainWrapper

open LnmaiCore LnmaiCore.Simai SlideChainReference SlideChainModel
open private parseContinuousSlideSegments? from LnmaiCore.Simai.Tokenize

/-- The exact original private wrapper, including the sanitized error context. -/
abbrev originalWrapper (token : String) :
    Except ParseError (Option (List OriginalSegment)) :=
  parseContinuousSlideSegments? token

/-- The runtime threshold is tested only after a successful complete core parse.
The entire ordered list is returned unchanged when its length exceeds one. -/
def selectChain (segments : List α) : Option (List α) :=
  if segments.length ≤ 1 then none else some segments

@[simp] theorem selectChain_nil : selectChain ([] : List α) = none := by
  simp [selectChain]

@[simp] theorem selectChain_singleton (segment : α) : selectChain [segment] = none := by
  simp [selectChain]

theorem selectChain_none_iff (segments : List α) :
    selectChain segments = none ↔ segments.length ≤ 1 := by
  by_cases hsmall : segments.length ≤ 1 <;> simp [selectChain, hsmall]

theorem selectChain_some_iff (input segments : List α) :
    selectChain input = some segments ↔ input = segments ∧ 1 < segments.length := by
  by_cases hsmall : input.length ≤ 1
  · constructor
    · intro h
      simp [selectChain, hsmall] at h
    · rintro ⟨rfl, hlarge⟩
      omega
  · constructor
    · intro h
      have heq : input = segments := by simpa [selectChain, hsmall] using h
      refine ⟨heq, ?_⟩
      rw [← heq]
      omega
    · rintro ⟨rfl, _⟩
      simp [selectChain, hsmall]

/-- Pointwise segment decoding preserves the exact threshold and list order.
No injectivity or whole-parser law is needed for the threshold decision. -/
theorem selectChain_map (decode : α → β) (segments : List α) :
    selectChain (segments.map decode) = (selectChain segments).map (List.map decode) := by
  by_cases hsmall : segments.length ≤ 1 <;> simp [selectChain, hsmall]

/-- A syntax error is propagated before the successful-list threshold can run. -/
theorem map_selectChain_error_iff (result : Except ε (List α)) (error : ε) :
    result.map selectChain = .error error ↔ result = .error error := by
  cases result <;> simp [Except.map]

theorem map_selectChain_none_iff (result : Except ε (List α)) :
    result.map selectChain = .ok none ↔
      ∃ segments, result = .ok segments ∧ segments.length ≤ 1 := by
  cases result <;> simp [Except.map, selectChain_none_iff]

theorem map_selectChain_some_iff (result : Except ε (List α)) (segments : List α) :
    result.map selectChain = .ok (some segments) ↔
      result = .ok segments ∧ 1 < segments.length := by
  cases result <;> simp [Except.map, selectChain_some_iff]

/-- This is only the successful-result threshold factoring of Except.bind. -/
theorem map_selectChain_eq_bind (result : Except ε (List α)) :
    result.map selectChain =
      (do let segments ← result
          if segments.length ≤ 1 then pure none else pure (some segments)) := by
  cases result with
  | error error => rfl
  | ok segments =>
      by_cases hsmall : segments.length ≤ 1 <;>
        simp [Except.map, selectChain, hsmall, Bind.bind, Except.bind, pure, Except.pure]

/-- Encoding a finite error and applying the successful-list threshold commute;
this theorem does not assert correctness of any Rust String/error facade. -/
theorem resultView_selectChain (raw : String)
    (result : Except Issue (List OriginalSegment)) :
    resultView raw id (result.map selectChain) =
      (resultView raw id result).map selectChain := by
  cases result <;> rfl

/-- Source model of the current runtime caller after String -> character list.
Empty/non-ASCII-digit heads bypass the core. A numeric head first parses the
ENTIRE suffix; only a successful result is subjected to the length threshold. -/
def wrapperChars : List Char → Except Issue (Option (List OriginalSegment))
  | [] => .ok none
  | start :: rest =>
      if !isChainDigit start then .ok none
      else (SlideChainModel.parseSegments rest start).map selectChain

/-- Direct factoring of the original wrapper at an already sanitized string.
The core here is the actual original private Lean core via its existing abbrev. -/
def referenceChars (raw : String) : List Char → Except ParseError (Option (List OriginalSegment))
  | [] => .ok none
  | start :: rest =>
      if !start.isDigit then .ok none
      else do
        let segments ← original raw start rest
        if segments.length ≤ 1 then pure none else pure (some segments)

/-- O12 source connection at arbitrary character input, including every error.
The required core equation is the Model theorem declaration, not a premise. -/
theorem wrapperChars_reference (raw : String) (chars : List Char) :
    resultView raw id (wrapperChars chars) = referenceChars raw chars := by
  cases chars with
  | nil => rfl
  | cons start rest =>
      cases hd : start.isDigit with
      | false =>
          simp [wrapperChars, referenceChars, isChainDigit_reference, hd, resultView]
      | true =>
          have hresult :
              resultView raw id ((SlideChainModel.parseSegments rest start).map selectChain) =
                (do let segments ← original raw start rest
                    if segments.length ≤ 1 then pure none else pure (some segments)) := by
            rw [resultView_selectChain, parseSegments_reference]
            exact map_selectChain_eq_bind _
          simpa [wrapperChars, referenceChars, isChainDigit_reference, hd] using hresult

/-- No alternate sanitizer is assumed: this is the original wrapper's own let. -/
theorem originalWrapper_factor (token : String) :
    originalWrapper token =
      referenceChars (sanitizeSlideToken token) (sanitizeSlideToken token).toList := by
  unfold originalWrapper parseContinuousSlideSegments?
  dsimp only
  -- Keep the concrete sanitizer opaque while comparing the two wrapper matches.
  generalize sanitizeSlideToken token = sanitized
  cases hs : sanitized.toList <;> rfl

/-- Source composition at token entry. Runtime preprocessing remains a separate
representation obligation; using the original sanitizer here does not prove it. -/
def parseToken (token : String) : Except ParseError (Option (List OriginalSegment)) :=
  let sanitized := sanitizeSlideToken token
  resultView sanitized id (wrapperChars sanitized.toList)

/-- Complete source-wrapper equation, with no validity/success restriction,
no parser-equivalence hypothesis, and the exact original ParseError payload. -/
theorem parseToken_reference (token : String) : parseToken token = originalWrapper token := by
  unfold parseToken
  rw [wrapperChars_reference]
  exact (originalWrapper_factor token).symm

@[simp] theorem wrapperChars_nil : wrapperChars [] = .ok none := rfl

theorem wrapperChars_nonDigit (start : Char) (rest : List Char)
    (hd : isChainDigit start = false) : wrapperChars (start :: rest) = .ok none := by
  simp [wrapperChars, hd]

/-- A head alone has a zero-segment successful suffix, even when it is 0 or 9.
This also covers a nonnumeric singleton via the early-return branch. -/
theorem wrapperChars_singleton (start : Char) : wrapperChars [start] = .ok none := by
  cases hd : isChainDigit start <;>
    simp [wrapperChars, hd, SlideChainModel.parseSegments, parsePlans, parsePlansFuel, scanFuel,
      Except.map, selectChain]

/-- Exact None alternatives. Numeric inputs require COMPLETE core success. -/
theorem wrapperChars_cons_none_iff (start : Char) (rest : List Char) :
    wrapperChars (start :: rest) = .ok none ↔
      isChainDigit start = false ∨
        (isChainDigit start = true ∧
          ∃ segments, SlideChainModel.parseSegments rest start = .ok segments ∧ segments.length ≤ 1) := by
  cases hd : isChainDigit start <;>
    simp [wrapperChars, hd, map_selectChain_none_iff]

/-- Every successful Some contains the entire core list in its original order. -/
theorem wrapperChars_cons_some_iff (start : Char) (rest : List Char)
    (segments : List OriginalSegment) :
    wrapperChars (start :: rest) = .ok (some segments) ↔
      isChainDigit start = true ∧
        SlideChainModel.parseSegments rest start = .ok segments ∧ 1 < segments.length := by
  cases hd : isChainDigit start <;>
    simp [wrapperChars, hd, map_selectChain_some_iff]

/-- A later syntax/timing error cannot be converted to None after an earlier
valid segment. The error outcome itself is necessary and sufficient. -/
theorem wrapperChars_cons_error_iff (start : Char) (rest : List Char) (issue : Issue) :
    wrapperChars (start :: rest) = .error issue ↔
      isChainDigit start = true ∧ SlideChainModel.parseSegments rest start = .error issue := by
  cases hd : isChainDigit start <;>
    simp [wrapperChars, hd, map_selectChain_error_iff]

/-- Encoding does not lose the successful Option/list payload. -/
theorem resultView_ok_iff (raw : String) (result : Except Issue α) (value : α) :
    resultView raw id result = .ok value ↔ result = .ok value := by
  cases result <;> simp [resultView]

/-- Every encoded error arises from one of the concrete finite source errors.
This does not need an injectivity assumption about the error encoder. -/
theorem resultView_error_iff (raw : String) (result : Except Issue α) (error : ParseError) :
    resultView raw id result = .error error ↔
      ∃ issue, result = .error issue ∧ decodeIssue raw issue = error := by
  cases result <;> simp [resultView]

/-- The token-level None alternatives, for the concrete sanitized first read. -/
theorem originalWrapper_cons_none_iff (token : String) (start : Char) (rest : List Char)
    (hchars : (sanitizeSlideToken token).toList = start :: rest) :
    originalWrapper token = .ok none ↔
      isChainDigit start = false ∨
        (isChainDigit start = true ∧
          ∃ segments, SlideChainModel.parseSegments rest start = .ok segments ∧ segments.length ≤ 1) := by
  rw [← parseToken_reference]
  unfold parseToken
  dsimp only
  rw [hchars, resultView_ok_iff, wrapperChars_cons_none_iff]

/-- Threshold and unchanged segment list at the original private entry point. -/
theorem originalWrapper_cons_some_iff (token : String) (start : Char) (rest : List Char)
    (segments : List OriginalSegment)
    (hchars : (sanitizeSlideToken token).toList = start :: rest) :
    originalWrapper token = .ok (some segments) ↔
      isChainDigit start = true ∧
        SlideChainModel.parseSegments rest start = .ok segments ∧ 1 < segments.length := by
  rw [← parseToken_reference]
  unfold parseToken
  dsimp only
  rw [hchars, resultView_ok_iff, wrapperChars_cons_some_iff]

/-- Complete token-level error characterization. In particular there is no
threshold-based recovery from a core error, even after one valid segment. -/
theorem originalWrapper_cons_error_iff (token : String) (start : Char) (rest : List Char)
    (error : ParseError)
    (hchars : (sanitizeSlideToken token).toList = start :: rest) :
    originalWrapper token = .error error ↔
      isChainDigit start = true ∧
        ∃ issue, SlideChainModel.parseSegments rest start = .error issue ∧
          decodeIssue (sanitizeSlideToken token) issue = error := by
  rw [← parseToken_reference]
  unfold parseToken
  dsimp only
  rw [hchars, resultView_error_iff]
  simp only [wrapperChars_cons_error_iff]
  constructor
  · rintro ⟨issue, ⟨hd, hcore⟩, hencoded⟩
    exact ⟨hd, issue, hcore, hencoded⟩
  · rintro ⟨hd, issue, hcore, hencoded⟩
    exact ⟨issue, ⟨hd, hcore⟩, hencoded⟩

/-- Empty sanitized input does not invoke the core. -/
theorem originalWrapper_empty (token : String)
    (hchars : (sanitizeSlideToken token).toList = []) :
    originalWrapper token = .ok none := by
  rw [← parseToken_reference]
  simp [parseToken, hchars, wrapperChars, resultView]

/-- The runtime caller passes the sanitized string as raw_text, exactly as the
original wrapper does. All three finite errors retain that complete context. -/
theorem originalWrapper_error (token : String) (start : Char) (rest : List Char) (issue : Issue)
    (hchars : (sanitizeSlideToken token).toList = start :: rest)
    (hd : isChainDigit start = true)
    (herror : SlideChainModel.parseSegments rest start = .error issue) :
    originalWrapper token = .error (decodeIssue (sanitizeSlideToken token) issue) := by
  rw [← parseToken_reference]
  simp [parseToken, hchars, wrapperChars, hd, herror, Except.map, resultView]

end Verification.SlideChainWrapper
