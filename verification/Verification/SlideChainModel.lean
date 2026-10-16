import Verification.SlideChainReference
import Init.Data.Char.Lemmas
import Init.Data.List.Nat.TakeDrop

/-!
SOURCE-ONLY cursor model and proof draft for slide_chain.rs. NOT elaborated.
Natural cursors stand for representable usize values. Every getElem? below
models a guarded single read, never an opaque grammar/whole-chain operation.
The concrete Rust loops still require REAL extraction and loop proofs.
See proof-plan.md and OPEN-OBLIGATIONS.md; no Generated theorem is asserted here.
-/

namespace Verification.SlideChainModel
open LnmaiCore LnmaiCore.Simai SlideChainReference
open private readDigitChar from LnmaiCore.Simai.Tokenize

structure Plan where
  head : Char
  begin : Nat
  stop : Nat
  hasTiming : Bool
  deriving DecidableEq, Repr

/-- Same ten equalities as Rust; no Unicode number category or lane restriction. -/
def isChainDigit (c : Char) : Bool := decide (
  c = '0' ∨ c = '1' ∨ c = '2' ∨ c = '3' ∨ c = '4' ∨
  c = '5' ∨ c = '6' ∨ c = '7' ∨ c = '8' ∨ c = '9')

theorem ascii_digit_cases (c : Char) (h : c.isDigit = true) :
    c = '0' ∨ c = '1' ∨ c = '2' ∨ c = '3' ∨ c = '4' ∨
    c = '5' ∨ c = '6' ∨ c = '7' ∨ c = '8' ∨ c = '9' := by
  have hb := Char.isDigit_iff_toNat.mp h
  change 48 ≤ c.toNat ∧ c.toNat ≤ 57 at hb
  simp only [← Char.toNat_inj]
  change c.toNat = 48 ∨ c.toNat = 49 ∨ c.toNat = 50 ∨ c.toNat = 51 ∨
    c.toNat = 52 ∨ c.toNat = 53 ∨ c.toNat = 54 ∨ c.toNat = 55 ∨
    c.toNat = 56 ∨ c.toNat = 57
  omega

theorem isChainDigit_reference (c : Char) : isChainDigit c = c.isDigit := by
  cases hd : c.isDigit with
  | true => simp [isChainDigit, ascii_digit_cases c hd]
  | false =>
      have hn : ¬(c = '0' ∨ c = '1' ∨ c = '2' ∨ c = '3' ∨ c = '4' ∨
          c = '5' ∨ c = '6' ∨ c = '7' ∨ c = '8' ∨ c = '9') := by
        intro hc
        rcases hc with hc | hc | hc | hc | hc | hc | hc | hc | hc | hc <;>
          subst c <;> cases hd
      simp [isChainDigit, hn]

def readDigitAt (chars : List Char) (cursor : Nat) : Except Issue (Char × Nat) :=
  match chars[cursor]? with
  | none => .error .invalidSyntax
  | some c => if isChainDigit c then .ok (c, cursor + 1) else .error .invalidSyntax

theorem readDigitAt_oob (chars : List Char) (cursor : Nat) (h : chars.length ≤ cursor) :
    readDigitAt chars cursor = .error .invalidSyntax := by
  simp [readDigitAt, List.getElem?_eq_none h]

/-- The single guarded read is connected to the ACTUAL original digit helper,
including the error payload and all beyond-end natural cursors. -/
theorem readDigitAt_reference (raw : String) (chars : List Char) (cursor : Nat) :
    resultView raw (fun value => (value.1, chars.drop value.2)) (readDigitAt chars cursor) =
      readDigitChar raw (chars.drop cursor) := by
  by_cases hi : cursor < chars.length
  · rw [List.drop_eq_getElem_cons hi]
    simp only [readDigitAt, List.getElem?_eq_getElem hi, isChainDigit_reference]
    cases hd : (chars[cursor]).isDigit <;>
      simp [hd, readDigitChar, resultView, decodeIssue, issueMessage, pure, Except.pure]
  · have ho : chars.length ≤ cursor := by omega
    simp [readDigitAt, List.getElem?_eq_none ho, List.drop_eq_nil_of_le ho,
      readDigitChar, resultView, decodeIssue, issueMessage]

def readShapeAt (chars : List Char) (op : Char) (cursor : Nat) : Except Issue (Char × Nat) := do
  if op = 'V' then
    let (_, next) ← readDigitAt chars cursor
    readDigitAt chars next
  else
    let next := if op = 'p' || op = 'q' then
        if chars[cursor]? = some op then cursor + 1 else cursor
      else cursor
    readDigitAt chars next

/-- Fuel zero still reads and accepts a closing bracket. At the end of input
there is no read and no fuel arithmetic. A non-closing character consumes fuel. -/
def timingEndFuel : Nat → List Char → Nat → Except Issue Nat
  | 0, chars, cursor =>
      match chars[cursor]? with
      | none => .error .unterminatedTiming
      | some c => if c = ']' then .ok (cursor + 1) else .error .unterminatedTiming
  | fuel + 1, chars, cursor =>
      match chars[cursor]? with
      | none => .error .unterminatedTiming
      | some c =>
          if c = ']' then .ok (cursor + 1)
          else timingEndFuel fuel chars (cursor + 1)

def timingEnd (chars : List Char) (cursor : Nat) : Except Issue Nat :=
  if chars.length < cursor then .error .unterminatedTiming
  else timingEndFuel (chars.length - cursor) chars cursor

theorem timingEndFuel_zero_close (chars : List Char) (cursor : Nat)
    (h : chars[cursor]? = some ']') :
    timingEndFuel 0 chars cursor = .ok (cursor + 1) := by
  simp [timingEndFuel, h]

theorem timingEndFuel_oob (fuel : Nat) (chars : List Char) (cursor : Nat)
    (h : chars.length ≤ cursor) :
    timingEndFuel fuel chars cursor = .error .unterminatedTiming := by
  cases fuel <;> simp [timingEndFuel, List.getElem?_eq_none h]

/-- A successful bracket scan advances within bounds; a later ']' cannot be
chosen if an earlier one was read, because that earlier branch returns. -/
theorem timingEndFuel_bounds (fuel : Nat) (chars : List Char) (cursor stop : Nat)
    (h : timingEndFuel fuel chars cursor = .ok stop) :
    cursor < stop ∧ stop ≤ chars.length := by
  induction fuel generalizing cursor with
  | zero =>
      cases hr : chars[cursor]? with
      | none => simp [timingEndFuel, hr] at h
      | some c =>
          have hi : cursor < chars.length := by
            by_contra hn
            have ho := List.getElem?_eq_none (l := chars) (i := cursor) (by omega)
            rw [ho] at hr
            cases hr
          by_cases hc : c = ']'
          · simp [timingEndFuel, hr, hc] at h
            omega
          · simp [timingEndFuel, hr, hc] at h
  | succ fuel ih =>
      cases hr : chars[cursor]? with
      | none => simp [timingEndFuel, hr] at h
      | some c =>
          have hi : cursor < chars.length := by
            by_contra hn
            have ho := List.getElem?_eq_none (l := chars) (i := cursor) (by omega)
            rw [ho] at hr
            cases hr
          by_cases hc : c = ']'
          · simp [timingEndFuel, hr, hc] at h
            omega
          · have hrec : timingEndFuel fuel chars (cursor + 1) = .ok stop := by
              simpa only [timingEndFuel, hr, hc, if_false] using h
            have hb := ih (cursor := cursor + 1) hrec
            omega

structure StepAt where
  plan : Plan
  nextHead : Char

/-- Exactly one nonempty iteration, after the outer fuel guard/decrement.
All grammar decisions occur here, not in a trait or function supplied by a user. -/
def stepAt (chars : List Char) (head : Char) (cursor : Nat) : Except Issue StepAt := do
  match chars[cursor]? with
  | none => .error .invalidSyntax
  | some op =>
      if isChainDigit op then .error .freshNumericHead
      else if !isSlideMarkChar op then .error .invalidSyntax
      else
        let (endpoint, next) ← readShapeAt chars op (cursor + 1)
        let (stop, timing) ←
          if chars[next]? = some '[' then do
            let stop ← timingEnd chars (next + 1)
            pure (stop, true)
          else pure (next, false)
        pure { plan := { head, begin := cursor, stop, hasTiming := timing }
               nextHead := endpoint }

/-- This is the model of the Rust forward Vec accumulator, not the old suffix
recursion. A later error discards prefix. cursor >= length returns prefix before
any fuel/error checks, just as the loop's entry guard does. -/
def scanFuel (chars : List Char) : Nat → Char → Nat → List Plan → Except Issue (List Plan)
  | 0, _, cursor, prefix =>
      if cursor < chars.length then .error .invalidSyntax else .ok prefix
  | fuel + 1, head, cursor, prefix =>
      if cursor < chars.length then
        match stepAt chars head cursor with
        | .error issue => .error issue
        | .ok step => scanFuel chars fuel step.nextHead step.plan.stop (prefix ++ [step.plan])
      else .ok prefix

def parsePlansFuel (chars : List Char) (head : Char) (fuel : Nat) : Except Issue (List Plan) :=
  scanFuel chars fuel head 0 []

def parsePlans (chars : List Char) (head : Char) : Except Issue (List Plan) :=
  parsePlansFuel chars head chars.length

def renderChars (chars : List Char) (plan : Plan) : List Char :=
  plan.head :: (chars.drop plan.begin).take (plan.stop - plan.begin)

def renderSegment (chars : List Char) (plan : Plan) : OriginalSegment :=
  { rawText := String.ofList (renderChars chars plan), hasTiming := plan.hasTiming }

def parseSegments (chars : List Char) (head : Char) : Except Issue (List OriginalSegment) :=
  (parsePlans chars head).map (List.map (renderSegment chars))

theorem scanFuel_oob (chars : List Char) (fuel : Nat) (head : Char) (cursor : Nat)
    (prefix : List Plan) (h : chars.length ≤ cursor) :
    scanFuel chars fuel head cursor prefix = .ok prefix := by
  cases fuel <;> simp [scanFuel, Nat.not_lt.mpr h]

theorem scanFuel_zero_nonempty (chars : List Char) (head : Char) (cursor : Nat)
    (prefix : List Plan) (h : cursor < chars.length) :
    scanFuel chars 0 head cursor prefix = .error .invalidSyntax := by
  simp [scanFuel, h]

/-- List append is an interpretation of the actual Vec::push operations.
This result does not assume that a complete scan agrees with the original. -/
theorem scanFuel_prefix (chars : List Char) (fuel : Nat) (head : Char) (cursor : Nat)
    (prefix : List Plan) :
    scanFuel chars fuel head cursor prefix =
      prefixResult prefix (scanFuel chars fuel head cursor []) := by
  induction fuel generalizing head cursor prefix with
  | zero =>
      by_cases h : cursor < chars.length <;> simp [scanFuel, h, prefixResult]
  | succ fuel ih =>
      by_cases h : cursor < chars.length
      · simp only [scanFuel, h, if_true]
        cases hs : stepAt chars head cursor with
        | error error => simp [hs, prefixResult]
        | ok step =>
            simp only [hs, List.nil_append]
            rw [ih (head := step.nextHead) (cursor := step.plan.stop) (prefix := prefix ++ [step.plan]),
              ih (head := step.nextHead) (cursor := step.plan.stop) (prefix := [step.plan])]
            exact prefixResult_append prefix [step.plan] _
      · simp [scanFuel, h, prefixResult]

/-- Used by render_plan: exactly the synthetic head followed by the original
range. Bounds are explicit because arbitrary hand-built Rust plans may panic. -/
theorem renderChars_length (chars : List Char) (plan : Plan)
    (hbegin : plan.begin ≤ plan.stop) (hstop : plan.stop ≤ chars.length) :
    (renderChars chars plan).length = 1 + (plan.stop - plan.begin) := by
  simp only [renderChars, List.length_cons, List.length_take, List.length_drop]
  rw [Nat.min_eq_left (by omega)]
  omega

/-- Consecutive ranges partition the consumed source prefix in its original
order. The minimum two characters exclude the synthetic head. This predicate
is a loop invariant to PROVE, not a facade requirement supplied to parsePlans. -/
def spansFrom (chars : List Char) : Nat → List Plan → Nat → Prop
  | cursor, [], final => cursor = final
  | cursor, plan :: rest, final =>
      plan.begin = cursor ∧ cursor + 2 ≤ plan.stop ∧ plan.stop ≤ chars.length ∧
      spansFrom chars plan.stop rest final

/-- The render loop's prospective invariant uses its actual growing buffer. -/
def renderProgress (chars : List Char) (plan : Plan) (cursor : Nat) (text : List Char) : Prop :=
  plan.begin ≤ cursor ∧ cursor ≤ plan.stop ∧ plan.stop ≤ chars.length ∧
  text = plan.head :: (chars.drop plan.begin).take (cursor - plan.begin)

/-- Exact next source-to-reference goal, intentionally NOT assumed/asserted:

For cursor < chars.length, let c := chars[cursor]. Prove
  resultView raw
    (fun step => (renderSegment chars step.plan, step.nextHead, chars.drop step.plan.stop))
    (stepAt chars head cursor)
  = (referenceStep raw head c (chars.drop (cursor + 1))).map
      (fun step => (step.segment, step.nextHead, step.rest)).

The local proof must unfold readShapeAt (including V/pp/qq), prove the shape
spelling and endpoint law, and connect timingEndFuel to readBracketSuffixFuel.
The latter uses the FIRST closing bracket and bracket_remaining_fuel.
Then induction on fuel, using scanFuel_prefix/collectFuel_reference, proves
  resultView raw (List.map (renderSegment chars)) (parsePlansFuel chars head fuel)
    = originalFuel fuel raw head chars
for EVERY natural fuel, including errors. With original_remaining_fuel it gives
  resultView raw id (parseSegments chars head) = original raw head chars.
These source-model correspondence goals and the real extracted loop goals in
proof-plan.md remain OPEN; this file introduces no axiom or equality hypothesis
that would hide them.
-/

end Verification.SlideChainModel
