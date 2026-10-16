import LnmaiCore.Simai.Tokenize
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

/-!
SOURCE-ONLY DRAFT. No elaboration, compiler, test or extraction was run.
These lemmas factor the actual original private Lean definitions. They do not
assert proofs of Generated.slide_chain; the extraction connection is separate.
There is no assumed whole-chain equation and no admitted/axiomatic theorem.
The main worker must elaborate and repair the tactic scripts before using them.
-/

namespace Verification.SlideChainReference
open LnmaiCore LnmaiCore.Simai

open private ContinuousChainSegment ContinuousChainSegment.mk chainSyntaxError readDigitChar
  readBracketSuffixFuel readBracketSuffix parseSlideShapeChars
  parseContinuousSlideSegmentsCoreFuel parseContinuousSlideSegmentsCore
  parseContinuousSlideSegments? from LnmaiCore.Simai.Tokenize

abbrev OriginalSegment := ContinuousChainSegment

/-- Explicit access to the original private constructor; no replacement datatype. -/
def mkOriginalSegment (rawText : String) (hasTiming : Bool) : OriginalSegment :=
  ContinuousChainSegment.mk rawText hasTiming

abbrev originalFuel (fuel : Nat) (raw : String) (head : Char) (chars : List Char) :=
  parseContinuousSlideSegmentsCoreFuel fuel raw head chars

abbrev original (raw : String) (head : Char) (chars : List Char) :=
  parseContinuousSlideSegmentsCore raw head chars

inductive Issue where
  | invalidSyntax
  | freshNumericHead
  | unterminatedTiming
  deriving DecidableEq, Repr

def issueMessage : Issue → String
  | .invalidSyntax => "invalid connected slide syntax"
  | .freshNumericHead => "connected slide chain cannot contain a fresh numeric head"
  | .unterminatedTiming => "unterminated slide timing spec"

def decodeIssue (raw : String) (issue : Issue) : ParseError :=
  chainSyntaxError raw (issueMessage issue)

def resultView (raw : String) (view : α → β) : Except Issue α → Except ParseError β
  | .error issue => .error (decodeIssue raw issue)
  | .ok value => .ok (view value)

/-- Prefix is discarded on error, including an error after successful segments. -/
def prefixResult (accSegments : List α) : Except ε (List α) → Except ε (List α)
  | .error error => .error error
  | .ok tail => .ok (accSegments ++ tail)

theorem prefixResult_nil (result : Except ε (List α)) :
    prefixResult [] result = result := by
  cases result <;> simp [prefixResult]

theorem prefixResult_append (left right : List α) (result : Except ε (List α)) :
    prefixResult (left ++ right) result = prefixResult left (prefixResult right result) := by
  cases result <;> simp [prefixResult, List.append_assoc]

theorem except_bind_assoc (value : Except ε α)
    (next : α → Except ε β) (last : β → Except ε γ) :
    ((value >>= next) >>= last) = (value >>= fun a => next a >>= last) := by
  cases value <;> rfl

theorem except_pure_bind (value : α) (next : α → Except ε β) :
    ((pure value : Except ε α) >>= next) = next value := rfl

/-- The original optional timing action, including its first-']' behavior. -/
def timingStep (raw : String) (chars : List Char) :
    Except ParseError (String × List Char × Bool) :=
  match chars with
  | '[' :: tail => do
      let (suffix, rest) ← readBracketSuffix raw tail ['[']
      pure (suffix, rest, true)
  | _ => pure ("", chars, false)

structure ReferenceStep where
  segment : OriginalSegment
  nextHead : Char
  rest : List Char

/-- No fuel check here: the original outer recursion checks fuel before this
step. These are the original shape/timing helpers, not Rust facade callbacks. -/
def referenceStep (raw : String) (head c : Char) (rest : List Char) :
    Except ParseError ReferenceStep := do
  if c.isDigit then
    .error (decodeIssue raw .freshNumericHead)
  else if !isSlideMarkChar c then
    .error (decodeIssue raw .invalidSyntax)
  else
    let (shape, rest) ← parseSlideShapeChars raw c rest
    let (suffix, rest, hasTiming) ← timingStep raw rest
    pure { segment := mkOriginalSegment ((String.singleton head ++ shape) ++ suffix) hasTiming,
           nextHead := shape.toList.reverse.head?.getD head,
           rest := rest }

/-- Exact one-step factoring, valid for errors and for every natural fuel. -/
theorem originalFuel_cons (fuel : Nat) (raw : String) (head c : Char) (rest : List Char) :
    originalFuel (fuel + 1) raw head (c :: rest) =
      (do let step ← referenceStep raw head c rest
          let tail ← originalFuel fuel raw step.nextHead step.rest
          pure (step.segment :: tail)) := by
  simp only [originalFuel, parseContinuousSlideSegmentsCoreFuel, referenceStep, mkOriginalSegment]
  split
  · rfl
  · split
    · rfl
    · simp only [except_bind_assoc, except_pure_bind]
      cases hshape : parseSlideShapeChars raw c rest with
      | error error => rfl
      | ok shapeRest =>
          rcases shapeRest with ⟨shape, remaining⟩
          cases remaining with
          | nil => rfl
          | cons next tail =>
              by_cases hopen : next = '['
              · subst next
                cases htiming : readBracketSuffix raw tail ['['] with
                | error error =>
                    simp [timingStep, htiming, Bind.bind, Except.bind,
                      pure, Except.pure]
                | ok suffixRest =>
                    rcases suffixRest with ⟨suffix, remaining⟩
                    simp [timingStep, htiming, Bind.bind, Except.bind,
                      pure, Except.pure]
              · simp [timingStep, hopen, Bind.bind, Except.bind,
                  pure, Except.pure]

/-- A forward-accumulator factoring of the old recursion. Empty precedes fuel. -/
def collectFuel (raw : String) : Nat → Char → List Char → List OriginalSegment →
    Except ParseError (List OriginalSegment)
  | _, _, [], accSegments => .ok accSegments
  | 0, _, _ :: _, _ => .error (decodeIssue raw .invalidSyntax)
  | fuel + 1, head, c :: rest, accSegments =>
      match referenceStep raw head c rest with
      | .error error => .error error
      | .ok step =>
          collectFuel raw fuel step.nextHead step.rest (accSegments ++ [step.segment])

/-- This is a source factoring theorem, not an assumed complete parser law. -/
theorem collectFuel_reference (fuel : Nat) (raw : String) (head : Char)
    (chars : List Char) (accSegments : List OriginalSegment) :
    collectFuel raw fuel head chars accSegments = prefixResult accSegments (originalFuel fuel raw head chars) := by
  induction fuel generalizing head chars accSegments with
  | zero =>
      cases chars <;>
        simp [collectFuel, originalFuel, parseContinuousSlideSegmentsCoreFuel,
          prefixResult, decodeIssue, issueMessage, pure, Except.pure]
  | succ fuel ih =>
      cases chars with
      | nil =>
          simp [collectFuel, originalFuel, parseContinuousSlideSegmentsCoreFuel,
            prefixResult, pure, Except.pure]
      | cons c rest =>
          rw [originalFuel_cons]
          simp only [collectFuel]
          cases hs : referenceStep raw head c rest with
          | error error => simp [hs, prefixResult, Bind.bind, Except.bind]
          | ok step =>
              simp only [hs, Bind.bind, Except.bind]
              rw [ih]
              cases originalFuel fuel raw step.nextHead step.rest <;>
                simp [prefixResult, List.append_assoc, pure, Except.pure,
                  Bind.bind, Except.bind]

/-- Immediate ']' is accepted even when the bracket fuel is zero. -/
theorem bracket_zero_close (raw : String) (rest acc : List Char) :
    readBracketSuffixFuel 0 raw (']' :: rest) acc =
      .ok (String.ofList (']' :: acc).reverse, rest) := by
  simp [readBracketSuffixFuel, pure, Except.pure]

/-- In contrast, an outer nonempty/fuel-zero input never reaches digit checks. -/
theorem outer_zero_nonempty (raw : String) (head c : Char) (rest : List Char) :
    originalFuel 0 raw head (c :: rest) = .error (decodeIssue raw .invalidSyntax) := rfl

/-- Stability of the original bracket reader above the remaining character
count. This includes unterminated inputs, not just successful parses. -/
theorem bracketFuel_stable (raw : String) (chars acc : List Char) (fuel other : Nat)
    (hf : chars.length ≤ fuel) (hg : chars.length ≤ other) :
    readBracketSuffixFuel fuel raw chars acc = readBracketSuffixFuel other raw chars acc := by
  induction chars generalizing fuel other acc with
  | nil => simp [readBracketSuffixFuel]
  | cons c rest ih =>
      cases fuel with
      | zero => simp at hf
      | succ fuel =>
          cases other with
          | zero => simp at hg
          | succ other =>
              by_cases hc : c = ']'
              · simp [readBracketSuffixFuel, hc]
              · simp only [readBracketSuffixFuel, hc, if_false]
                exact ih (acc := c :: acc) (fuel := fuel) (other := other) (by simpa using hf) (by simpa using hg)

theorem bracket_remaining_fuel (raw : String) (chars acc : List Char) :
    readBracketSuffixFuel chars.length raw chars acc = readBracketSuffix raw chars acc := by
  unfold readBracketSuffix
  exact bracketFuel_stable raw chars acc _ _ (by omega) (by omega)

/-- A successful digit read removes exactly one character; ASCII 0 and 9 are
not restricted to the later note-lane range 1..8. -/
theorem digit_remaining_lt (raw : String) (chars : List Char) (digit : Char)
    (rest : List Char) (h : readDigitChar raw chars = .ok (digit, rest)) :
    rest.length < chars.length := by
  cases chars with
  | nil => simp [readDigitChar] at h
  | cons c tail =>
      simp only [readDigitChar] at h
      split at h
      · simp only [pure, Except.pure, Except.ok.injEq, Prod.mk.injEq] at h
        rcases h with ⟨rfl, rfl⟩
        simp
      · cases h

/-- The old p/q prefix selection, before the required endpoint digit. -/
def shapePrefix (op : Char) (chars : List Char) : String × List Char :=
  if op = 'p' || op = 'q' then
    match chars with
    | next :: tail =>
        if next = op then (String.singleton op ++ String.singleton next, tail)
        else (String.singleton op, chars)
    | [] => (String.singleton op, chars)
  else (String.singleton op, chars)

theorem shapePrefix_remaining_le (op : Char) (chars : List Char) :
    (shapePrefix op chars).2.length ≤ chars.length := by
  by_cases hp : (decide (op = 'p') || decide (op = 'q')) = true
  · cases chars with
    | nil => simp [shapePrefix, hp]
    | cons c rest =>
        by_cases hc : c = op <;> simp [shapePrefix, hp, hc]
  · simp [shapePrefix, hp]

/-- Successful V reads two digits; doubled p/q reads exactly one extra mark;
every other accepted mark still requires one digit. -/
theorem shape_remaining_lt (raw : String) (op : Char) (chars : List Char)
    (shape : String) (rest : List Char)
    (h : parseSlideShapeChars raw op chars = .ok (shape, rest)) :
    rest.length < chars.length := by
  unfold parseSlideShapeChars at h
  split at h
  · cases hfirst : readDigitChar raw chars with
    | error error => simp [hfirst, Bind.bind, Except.bind] at h
    | ok first =>
        rcases first with ⟨middle, tail⟩
        cases hlast : readDigitChar raw tail with
        | error error => simp [hfirst, hlast, Bind.bind, Except.bind] at h
        | ok last =>
            rcases last with ⟨finish, remaining⟩
            have hfirstLen := digit_remaining_lt raw chars middle tail hfirst
            have hlastLen := digit_remaining_lt raw tail finish remaining hlast
            simp only [hfirst, hlast, Bind.bind, Except.bind, pure, Except.pure,
              Except.ok.injEq, Prod.mk.injEq] at h
            rcases h with ⟨_, rfl⟩
            omega
  · change (do
        let (finish, rest) ← readDigitChar raw (shapePrefix op chars).2
        pure ((shapePrefix op chars).1 ++ String.singleton finish, rest)) = .ok (shape, rest) at h
    cases hlast : readDigitChar raw (shapePrefix op chars).2 with
    | error error => simp [hlast, Bind.bind, Except.bind] at h
    | ok last =>
        rcases last with ⟨finish, remaining⟩
        have hshort := digit_remaining_lt raw (shapePrefix op chars).2 finish remaining hlast
        have hprefix := shapePrefix_remaining_le op chars
        simp only [hlast, Bind.bind, Except.bind, pure, Except.pure,
          Except.ok.injEq, Prod.mk.injEq] at h
        rcases h with ⟨_, rfl⟩
        omega

theorem bracket_remaining_le (raw : String) (chars acc : List Char)
    (fuel : Nat) (suffix : String) (rest : List Char)
    (h : readBracketSuffixFuel fuel raw chars acc = .ok (suffix, rest)) :
    rest.length ≤ chars.length := by
  induction chars generalizing fuel acc with
  | nil => simp [readBracketSuffixFuel] at h
  | cons c tail ih =>
      by_cases hc : c = ']'
      · subst c
        cases fuel <;>
          simp [readBracketSuffixFuel, pure, Except.pure] at h <;>
          rcases h with ⟨_, rfl⟩ <;> simp
      · cases fuel with
        | zero => simp [readBracketSuffixFuel, hc] at h
        | succ fuel =>
            have hrec : readBracketSuffixFuel fuel raw tail (c :: acc) = .ok (suffix, rest) := by
              simpa only [readBracketSuffixFuel, hc, if_false] using h
            have hlen := ih (acc := c :: acc) (fuel := fuel) hrec
            simp only [List.length_cons]
            omega

theorem timingStep_remaining_le (raw : String) (chars : List Char)
    (suffix : String) (rest : List Char) (timing : Bool)
    (h : timingStep raw chars = .ok (suffix, rest, timing)) :
    rest.length ≤ chars.length := by
  cases chars with
  | nil =>
      simp only [timingStep, pure, Except.pure, Except.ok.injEq, Prod.mk.injEq] at h
      rcases h with ⟨_, rfl, _⟩
      simp
  | cons c tail =>
      by_cases hc : c = '['
      · subst c
        cases hb : readBracketSuffix raw tail ['['] with
        | error error => simp [timingStep, hb, Bind.bind, Except.bind] at h
        | ok output =>
            rcases output with ⟨text, remaining⟩
            have hlen := bracket_remaining_le raw tail ['['] (tail.length + 1) text remaining hb
            simp only [timingStep, hb, Bind.bind, Except.bind, pure, Except.pure,
              Except.ok.injEq, Prod.mk.injEq] at h
            rcases h with ⟨_, rfl, _⟩
            simp only [List.length_cons]
            omega
      · simp [timingStep, hc, pure, Except.pure] at h
        rcases h with ⟨_, rfl, _⟩
        exact Nat.le_refl _

/-- The one-step decrease used to justify remaining-length fuel; it is derived
from the original helpers, not passed in as an extra whole-step hypothesis. -/
theorem referenceStep_shorter (raw : String) (head c : Char) (rest : List Char)
    (step : ReferenceStep) (h : referenceStep raw head c rest = .ok step) :
    step.rest.length < (c :: rest).length := by
  unfold referenceStep at h
  split at h
  · cases h
  · split at h
    · cases h
    · cases hs : parseSlideShapeChars raw c rest with
      | error error => simp [hs, Bind.bind, Except.bind] at h
      | ok shapeOutput =>
          rcases shapeOutput with ⟨shape, tail⟩
          cases ht : timingStep raw tail with
          | error error => simp [hs, ht, Bind.bind, Except.bind] at h
          | ok timingOutput =>
              rcases timingOutput with ⟨suffix, remaining, timing⟩
              have hshape := shape_remaining_lt raw c rest shape tail hs
              have htiming := timingStep_remaining_le raw tail suffix remaining timing ht
              simp only [hs, ht, Bind.bind, Except.bind, pure, Except.pure,
                Except.ok.injEq] at h
              subst step
              dsimp only
              simp only [List.length_cons]
              omega

/-- Stable ABOVE remaining length, for both success and every grammar error. -/
theorem originalFuel_stable (raw : String) (fuel other : Nat)
    (head : Char) (chars : List Char)
    (hf : chars.length ≤ fuel) (hg : chars.length ≤ other) :
    originalFuel fuel raw head chars = originalFuel other raw head chars := by
  induction fuel generalizing other head chars with
  | zero =>
      have hempty : chars = [] := List.length_eq_zero_iff.mp (by omega)
      subst chars
      simp [originalFuel, parseContinuousSlideSegmentsCoreFuel]
  | succ fuel ih =>
      cases chars with
      | nil => simp [originalFuel, parseContinuousSlideSegmentsCoreFuel]
      | cons c rest =>
          cases other with
          | zero => simp at hg
          | succ other =>
              rw [originalFuel_cons, originalFuel_cons]
              cases hs : referenceStep raw head c rest with
              | error error => simp [hs, Bind.bind, Except.bind]
              | ok step =>
                  have hshort := referenceStep_shorter raw head c rest step hs
                  have hrec := ih (other := other) (head := step.nextHead) (chars := step.rest) (by omega) (by omega)
                  simp only [hs, Bind.bind, Except.bind]
                  rw [hrec]

theorem original_remaining_fuel (raw : String) (head : Char) (chars : List Char) :
    originalFuel chars.length raw head chars = original raw head chars := by
  unfold original parseContinuousSlideSegmentsCore
  exact originalFuel_stable raw _ _ head chars (by omega) (by omega)

theorem collect_remaining_fuel (raw : String) (head : Char) (chars : List Char) :
    collectFuel raw chars.length head chars [] = original raw head chars := by
  rw [collectFuel_reference, prefixResult_nil, original_remaining_fuel]

end Verification.SlideChainReference
