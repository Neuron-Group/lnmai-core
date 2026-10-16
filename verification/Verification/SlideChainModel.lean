import Verification.SlideChainReference
import Init.Data.Char.Lemmas
import Init.Data.List.Nat.TakeDrop

/-!
SOURCE-ONLY cursor model and proof draft for slide_chain.rs. NOT elaborated.
Natural cursors stand for representable usize values. Every getElem? below
models a guarded single read, never an opaque grammar/whole-chain operation.
The real extraction exists separately; its loop proofs belong to the extracted
proof workstream. See HANDOFF.md; no Generated theorem is asserted here.
-/

namespace Verification.SlideChainModel
open LnmaiCore LnmaiCore.Simai SlideChainReference
open private readDigitChar parseSlideShapeChars readBracketSuffixFuel
  readBracketSuffix from LnmaiCore.Simai.Tokenize

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

/-- The optional second p/q mark. This is definitionally the original local let. -/
def shapeCursor (chars : List Char) (op : Char) (cursor : Nat) : Nat :=
  if op = 'p' || op = 'q' then
    if chars[cursor]? = some op then cursor + 1 else cursor
  else cursor

def readShapeAt (chars : List Char) (op : Char) (cursor : Nat) : Except Issue (Char × Nat) := do
  if op = 'V' then
    let (_, next) ← readDigitAt chars cursor
    readDigitAt chars next
  else
    readDigitAt chars (shapeCursor chars op cursor)

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

/-- The optional timing action, factored without changing its control flow. -/
def timingAt (chars : List Char) (cursor : Nat) : Except Issue (Nat × Bool) := do
  if chars[cursor]? = some '[' then
    let stop ← timingEnd chars (cursor + 1)
    pure (stop, true)
  else pure (cursor, false)

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
        let (stop, timing) ← timingAt chars next
        pure { plan := { head := head, begin := cursor, stop := stop, hasTiming := timing },
               nextHead := endpoint }

/-- This is the model of the Rust forward Vec accumulator, not the old suffix
recursion. A later error discards accSegments. cursor >= length returns accSegments before
any fuel/error checks, just as the loop's entry guard does. -/
def scanFuel (chars : List Char) : Nat → Char → Nat → List Plan → Except Issue (List Plan)
  | 0, _, cursor, accSegments =>
      if cursor < chars.length then .error .invalidSyntax else .ok accSegments
  | fuel + 1, head, cursor, accSegments =>
      if cursor < chars.length then
        match stepAt chars head cursor with
        | .error issue => .error issue
        | .ok step => scanFuel chars fuel step.nextHead step.plan.stop (accSegments ++ [step.plan])
      else .ok accSegments

def parsePlansFuel (chars : List Char) (head : Char) (fuel : Nat) : Except Issue (List Plan) :=
  scanFuel chars fuel head 0 []

def parsePlans (chars : List Char) (head : Char) : Except Issue (List Plan) :=
  parsePlansFuel chars head chars.length

def renderChars (chars : List Char) (plan : Plan) : List Char :=
  plan.head :: (chars.drop plan.begin).take (plan.stop - plan.begin)

def renderSegment (chars : List Char) (plan : Plan) : OriginalSegment :=
  mkOriginalSegment (String.ofList (renderChars chars plan)) plan.hasTiming

def parseSegments (chars : List Char) (head : Char) : Except Issue (List OriginalSegment) :=
  (parsePlans chars head).map (List.map (renderSegment chars))

theorem scanFuel_oob (chars : List Char) (fuel : Nat) (head : Char) (cursor : Nat)
    (accSegments : List Plan) (h : chars.length ≤ cursor) :
    scanFuel chars fuel head cursor accSegments = .ok accSegments := by
  cases fuel <;> simp [scanFuel, Nat.not_lt.mpr h]

theorem scanFuel_zero_nonempty (chars : List Char) (head : Char) (cursor : Nat)
    (accSegments : List Plan) (h : cursor < chars.length) :
    scanFuel chars 0 head cursor accSegments = .error .invalidSyntax := by
  simp [scanFuel, h]

/-- List append is an interpretation of the actual Vec::push operations.
This result does not assume that a complete scan agrees with the original. -/
theorem scanFuel_prefix (chars : List Char) (fuel : Nat) (head : Char) (cursor : Nat)
    (accSegments : List Plan) :
    scanFuel chars fuel head cursor accSegments =
      prefixResult accSegments (scanFuel chars fuel head cursor []) := by
  induction fuel generalizing head cursor accSegments with
  | zero =>
      by_cases h : cursor < chars.length <;> simp [scanFuel, h, prefixResult]
  | succ fuel ih =>
      by_cases h : cursor < chars.length
      · simp only [scanFuel, h, if_true]
        cases hs : stepAt chars head cursor with
        | error error => simp [hs, prefixResult]
        | ok step =>
            simp only [hs, List.nil_append]
            rw [ih (head := step.nextHead) (cursor := step.plan.stop) (accSegments := accSegments ++ [step.plan]),
              ih (head := step.nextHead) (cursor := step.plan.stop) (accSegments := [step.plan])]
            exact prefixResult_append accSegments [step.plan] _
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

/-! Source correspondence. All equalities below are derived from concrete
character reads and the original private definitions; none is an input law. -/

/-- Half-open character range used by the scanner and renderer. -/
def sourceRange (chars : List Char) (first stop : Nat) : List Char :=
  (chars.drop first).take (stop - first)

@[simp] theorem sourceRange_self (chars : List Char) (cursor : Nat) :
    sourceRange chars cursor cursor = [] := by
  simp [sourceRange]

theorem get_some_lt (chars : List Char) (cursor : Nat) (c : Char)
    (h : chars[cursor]? = some c) : cursor < chars.length := by
  by_contra hn
  have ho : chars[cursor]? = none := List.getElem?_eq_none (by omega)
  rw [ho] at h
  cases h

theorem drop_of_some (chars : List Char) (cursor : Nat) (c : Char)
    (h : chars[cursor]? = some c) :
    chars.drop cursor = c :: chars.drop (cursor + 1) := by
  rw [List.drop_eq_getElem?_toList_append, h]
  rfl

theorem drop_of_none (chars : List Char) (cursor : Nat)
    (h : chars[cursor]? = none) : chars.drop cursor = [] := by
  by_cases hi : cursor < chars.length
  · rw [List.getElem?_eq_getElem hi] at h
    cases h
  · exact List.drop_eq_nil_of_le (by omega)

theorem sourceRange_cons (chars : List Char) (cursor stop : Nat) (c : Char)
    (h : chars[cursor]? = some c) (hstop : cursor + 1 ≤ stop) :
    sourceRange chars cursor stop = c :: sourceRange chars (cursor + 1) stop := by
  unfold sourceRange
  rw [drop_of_some chars cursor c h]
  have hn : stop - cursor = (stop - (cursor + 1)) + 1 := by omega
  rw [hn, List.take_succ_cons]

theorem sourceRange_one (chars : List Char) (cursor : Nat) (c : Char)
    (h : chars[cursor]? = some c) : sourceRange chars cursor (cursor + 1) = [c] := by
  rw [sourceRange_cons chars cursor (cursor + 1) c h (by omega), sourceRange_self]

theorem sourceRange_split (chars : List Char) (first middle stop : Nat)
    (hfm : first ≤ middle) (hms : middle ≤ stop) :
    sourceRange chars first stop =
      sourceRange chars first middle ++ sourceRange chars middle stop := by
  unfold sourceRange
  have hn : stop - first = (middle - first) + (stop - middle) := by omega
  have hi : first + (middle - first) = middle := by omega
  rw [hn, List.take_add, List.drop_drop, hi]

theorem ofList_pair (a b : Char) :
    String.ofList [a, b] = String.singleton a ++ String.singleton b := by
  apply String.toList_injective
  simp [String.toList_append]

theorem ofList_triple (a b c : Char) :
    String.ofList [a, b, c] =
      (String.singleton a ++ String.singleton b) ++ String.singleton c := by
  apply String.toList_injective
  simp [String.toList_append]

theorem ofList_append_char (text : List Char) (c : Char) :
    String.ofList (text ++ [c]) = String.ofList text ++ String.singleton c := by
  apply String.toList_injective
  simp [String.toList_append]

/-- The cursor increment and read character follow from the actual guarded read. -/
theorem readDigitAt_success (chars : List Char) (cursor : Nat) (digit : Char) (next : Nat)
    (h : readDigitAt chars cursor = .ok (digit, next)) :
    next = cursor + 1 ∧ chars[cursor]? = some digit ∧
      isChainDigit digit = true ∧ cursor < chars.length := by
  cases hr : chars[cursor]? with
  | none => simp [readDigitAt, hr] at h
  | some c =>
      by_cases hd : isChainDigit c = true
      · have heq : c = digit ∧ cursor + 1 = next := by
          simpa [readDigitAt, hr, hd] using h
        rcases heq with ⟨rfl, rfl⟩
        exact ⟨rfl, hr, hd, get_some_lt chars cursor digit hr⟩
      · simp [readDigitAt, hr, hd] at h

/-- Complete p/q selection: at most one identical mark is consumed. -/
theorem shapeCursor_spec (chars : List Char) (op : Char) (cursor : Nat) :
    cursor ≤ shapeCursor chars op cursor ∧
    (cursor ≤ chars.length → shapeCursor chars op cursor ≤ chars.length) ∧
    shapePrefix op (chars.drop cursor) =
      (String.ofList (op :: sourceRange chars cursor (shapeCursor chars op cursor)),
        chars.drop (shapeCursor chars op cursor)) := by
  by_cases hp : (decide (op = 'p') || decide (op = 'q')) = true
  · by_cases hc : chars[cursor]? = some op
    · have hi := get_some_lt chars cursor op hc
      have hd := drop_of_some chars cursor op hc
      simp only [shapeCursor, hp, hc, if_true]
      refine ⟨by omega, ?_, ?_⟩
      · intro _
        omega
      · rw [sourceRange_one chars cursor op hc]
        simp [shapePrefix, hp, hd, ofList_pair]
    · simp only [shapeCursor, hp, hc, if_true, if_false]
      refine ⟨Nat.le_refl _, fun h => h, ?_⟩
      rw [sourceRange_self]
      cases hr : chars[cursor]? with
      | none =>
          rw [drop_of_none chars cursor hr]
          simp [shapePrefix, hp, String.singleton_eq_ofList]
      | some c =>
          have hn : c ≠ op := by
            intro heq
            subst c
            exact hc hr
          rw [drop_of_some chars cursor c hr]
          simp [shapePrefix, hp, hn, String.singleton_eq_ofList]
  · simp [shapeCursor, shapePrefix, hp, sourceRange, String.singleton_eq_ofList]

/-- Definitional non-V factoring of the actual original shape parser. -/
theorem original_shape_nonV (raw : String) (op : Char) (chars : List Char)
    (hv : op ≠ 'V') :
    parseSlideShapeChars raw op chars =
      (do let (finish, rest) ← readDigitChar raw (shapePrefix op chars).2
          pure ((shapePrefix op chars).1 ++ String.singleton finish, rest)) := by
  unfold parseSlideShapeChars
  simp only [hv, if_false]
  rfl

/-- O3: complete shape branch contract, with spelling, endpoint, residual input,
and range bounds. The error alternative includes the exact original ParseError. -/
theorem readShapeAt_spec (raw : String) (chars : List Char) (op : Char) (cursor : Nat) :
    match readShapeAt chars op cursor with
    | .error issue =>
        parseSlideShapeChars raw op (chars.drop cursor) = .error (decodeIssue raw issue)
    | .ok (endpoint, stop) =>
        cursor < stop ∧ stop ≤ chars.length ∧
        parseSlideShapeChars raw op (chars.drop cursor) =
          .ok (String.ofList (op :: sourceRange chars cursor stop), chars.drop stop) ∧
        ∀ fallback : Char,
          (op :: sourceRange chars cursor stop).reverse.head?.getD fallback = endpoint := by
  by_cases hv : op = 'V'
  · cases hf : readDigitAt chars cursor with
    | error issue =>
        have href := readDigitAt_reference raw chars cursor
        simp only [hf, resultView] at href
        simp [readShapeAt, hv, hf, parseSlideShapeChars,
          ← href, Bind.bind, Except.bind]
    | ok first =>
        rcases first with ⟨middle, next⟩
        obtain ⟨rfl, hmiddle, _, himiddle⟩ :=
          readDigitAt_success chars cursor middle next hf
        have href := readDigitAt_reference raw chars cursor
        simp only [hf, resultView] at href
        cases hl : readDigitAt chars (cursor + 1) with
        | error issue =>
            have hlast := readDigitAt_reference raw chars (cursor + 1)
            simp only [hl, resultView] at hlast
            simp [readShapeAt, hv, hf, hl, parseSlideShapeChars,
              ← href, ← hlast, Bind.bind, Except.bind]
        | ok last =>
            rcases last with ⟨endpoint, stop⟩
            obtain ⟨rfl, hendpoint, _, hiendpoint⟩ :=
              readDigitAt_success chars (cursor + 1) endpoint stop hl
            have hlast := readDigitAt_reference raw chars (cursor + 1)
            simp only [hl, resultView] at hlast
            have htext : sourceRange chars cursor ((cursor + 1) + 1) = [middle, endpoint] := by
              rw [sourceRange_cons chars cursor ((cursor + 1) + 1) middle hmiddle (by omega),
                sourceRange_one chars (cursor + 1) endpoint hendpoint]
            simp only [readShapeAt, hv, if_true, hf, hl, Bind.bind, Except.bind]
            refine ⟨by omega, by omega, ?_, ?_⟩
            · simp [parseSlideShapeChars, hv, ← href, ← hlast, htext, ofList_triple,
                Bind.bind, Except.bind, pure, Except.pure]
            · intro fallback
              simp [htext]
  · obtain ⟨hnext, _, hprefix⟩ := shapeCursor_spec chars op cursor
    have hshape := original_shape_nonV raw op (chars.drop cursor) hv
    rw [hprefix] at hshape
    dsimp only at hshape
    cases hl : readDigitAt chars (shapeCursor chars op cursor) with
    | error issue =>
        have hlast := readDigitAt_reference raw chars (shapeCursor chars op cursor)
        simp only [hl, resultView] at hlast
        simp [readShapeAt, hv, hl, hshape, ← hlast, Bind.bind, Except.bind]
    | ok last =>
        rcases last with ⟨endpoint, stop⟩
        obtain ⟨rfl, hendpoint, _, hiendpoint⟩ :=
          readDigitAt_success chars (shapeCursor chars op cursor) endpoint stop hl
        have hlast := readDigitAt_reference raw chars (shapeCursor chars op cursor)
        simp only [hl, resultView] at hlast
        have htext : sourceRange chars cursor (shapeCursor chars op cursor + 1) =
            sourceRange chars cursor (shapeCursor chars op cursor) ++ [endpoint] := by
          rw [sourceRange_split chars cursor (shapeCursor chars op cursor)
            (shapeCursor chars op cursor + 1) hnext (by omega),
            sourceRange_one chars (shapeCursor chars op cursor) endpoint hendpoint]
        simp only [readShapeAt, hv, if_false, hl]
        refine ⟨by omega, by omega, ?_, ?_⟩
        · rw [hshape, ← hlast]
          simp only [Bind.bind, Except.bind, pure, Except.pure]
          rw [htext, ← List.cons_append, ofList_append_char]
        · intro fallback
          rw [htext, ← List.cons_append]
          simp

/-- O3 result equation, obtained from the proved branch contract. -/
theorem readShapeAt_reference (raw : String) (chars : List Char) (op : Char) (cursor : Nat) :
    resultView raw
      (fun value => (String.ofList (op :: sourceRange chars cursor value.2), chars.drop value.2))
      (readShapeAt chars op cursor) = parseSlideShapeChars raw op (chars.drop cursor) := by
  have hs := readShapeAt_spec raw chars op cursor
  cases hr : readShapeAt chars op cursor with
  | error issue =>
      simp only [hr] at hs
      simpa only [resultView] using hs.symm
  | ok value =>
      rcases value with ⟨endpoint, stop⟩
      simp only [hr] at hs
      simpa only [resultView] using hs.2.2.1.symm

/-- Success always consumes a required digit, even after V or doubled p/q. -/
theorem readShapeAt_success (chars : List Char) (op : Char) (cursor : Nat)
    (endpoint : Char) (stop : Nat) (h : readShapeAt chars op cursor = .ok (endpoint, stop)) :
    cursor < stop ∧ stop ≤ chars.length ∧
      ∀ fallback : Char,
        (op :: sourceRange chars cursor stop).reverse.head?.getD fallback = endpoint := by
  have hs := readShapeAt_spec "" chars op cursor
  simp only [h] at hs
  exact ⟨hs.1, hs.2.1, hs.2.2.2⟩

/-- O4: arbitrary fuel, arbitrary accumulator, and every natural cursor.
The original reversed accumulator is related to the exact consumed range.
The zero-fuel closing-bracket case is handled before the fuel failure branch. -/
theorem timingEndFuel_reference (fuel : Nat) (raw : String) (chars : List Char)
    (cursor : Nat) (acc : List Char) :
    resultView raw
      (fun stop => (String.ofList (acc.reverse ++ sourceRange chars cursor stop), chars.drop stop))
      (timingEndFuel fuel chars cursor) =
        readBracketSuffixFuel fuel raw (chars.drop cursor) acc := by
  induction fuel generalizing cursor acc with
  | zero =>
      cases hr : chars[cursor]? with
      | none =>
          have hd := drop_of_none chars cursor hr
          simp [timingEndFuel, hr, hd, resultView, readBracketSuffixFuel,
            decodeIssue, issueMessage]
      | some c =>
          have hd := drop_of_some chars cursor c hr
          by_cases hc : c = ']'
          · subst c
            have ht := sourceRange_one chars cursor ']' hr
            simp [timingEndFuel, hr, hd, ht, resultView, readBracketSuffixFuel,
              List.reverse_cons, pure, Except.pure]
          · simp [timingEndFuel, hr, hd, hc, resultView, readBracketSuffixFuel,
              decodeIssue, issueMessage]
  | succ fuel ih =>
      cases hr : chars[cursor]? with
      | none =>
          have hd := drop_of_none chars cursor hr
          simp [timingEndFuel, hr, hd, resultView, readBracketSuffixFuel,
            decodeIssue, issueMessage]
      | some c =>
          have hd := drop_of_some chars cursor c hr
          by_cases hc : c = ']'
          · subst c
            have ht := sourceRange_one chars cursor ']' hr
            simp [timingEndFuel, hr, hd, ht, resultView, readBracketSuffixFuel,
              List.reverse_cons, pure, Except.pure]
          · simp only [timingEndFuel, hr, hc, if_false, hd, readBracketSuffixFuel]
            rw [← ih (cursor := cursor + 1) (acc := c :: acc)]
            cases hs : timingEndFuel fuel chars (cursor + 1) with
            | error issue => rfl
            | ok stop =>
                have hb := timingEndFuel_bounds fuel chars (cursor + 1) stop hs
                simp only [resultView]
                rw [sourceRange_cons chars cursor stop c hr (by omega)]
                simp [List.reverse_cons, List.append_assoc]

/-- The explicit beyond-end guard has the same result as the guarded reader. -/
theorem timingEnd_eq_fuel (chars : List Char) (cursor : Nat) :
    timingEnd chars cursor = timingEndFuel (chars.length - cursor) chars cursor := by
  unfold timingEnd
  split
  · exact (timingEndFuel_oob _ chars cursor (by omega)).symm
  · rfl

/-- O4 default-fuel bridge. Remaining length, without +1, suffices for the
ACTUAL original bracket scanner, on success and every error. -/
theorem timingEnd_reference (raw : String) (chars : List Char) (cursor : Nat) (acc : List Char) :
    resultView raw
      (fun stop => (String.ofList (acc.reverse ++ sourceRange chars cursor stop), chars.drop stop))
      (timingEnd chars cursor) = readBracketSuffix raw (chars.drop cursor) acc := by
  rw [timingEnd_eq_fuel, timingEndFuel_reference]
  simpa only [List.length_drop] using bracket_remaining_fuel raw (chars.drop cursor) acc

theorem timingEnd_success (chars : List Char) (cursor stop : Nat)
    (h : timingEnd chars cursor = .ok stop) : cursor < stop ∧ stop ≤ chars.length := by
  rw [timingEnd_eq_fuel] at h
  exact timingEndFuel_bounds _ chars cursor stop h

/-- Optional timing contributes exactly its source range, including '[' and
its first ']', or contributes the empty range when no '[' is next. -/
theorem timingAt_reference (raw : String) (chars : List Char) (cursor : Nat) :
    resultView raw
      (fun value => (String.ofList (sourceRange chars cursor value.1),
        chars.drop value.1, value.2))
      (timingAt chars cursor) = timingStep raw (chars.drop cursor) := by
  by_cases ho : chars[cursor]? = some '['
  · have hd := drop_of_some chars cursor '[' ho
    have href := timingEnd_reference raw chars (cursor + 1) ['[']
    cases he : timingEnd chars (cursor + 1) with
    | error issue =>
        simp only [he, resultView] at href
        simp [timingAt, ho, he, resultView, timingStep, hd, ← href,
          Bind.bind, Except.bind]
    | ok stop =>
        have hb := timingEnd_success chars (cursor + 1) stop he
        have ht := sourceRange_cons chars cursor stop '[' ho (by omega)
        simp only [he, resultView] at href
        simp [timingAt, ho, he, resultView, timingStep, hd, ← href, ht,
          Bind.bind, Except.bind, pure, Except.pure]
  · cases hr : chars[cursor]? with
    | none =>
        have hd := drop_of_none chars cursor hr
        simp [timingAt, ho, resultView, timingStep, hd, sourceRange,
          pure, Except.pure]
    | some c =>
        have hc : c ≠ '[' := by
          intro heq
          subst c
          exact ho hr
        have hd := drop_of_some chars cursor c hr
        simp [timingAt, ho, resultView, timingStep, hd, hc, sourceRange,
          pure, Except.pure]

theorem timingAt_success (chars : List Char) (cursor stop : Nat) (timing : Bool)
    (hc : cursor ≤ chars.length) (h : timingAt chars cursor = .ok (stop, timing)) :
    cursor ≤ stop ∧ stop ≤ chars.length := by
  by_cases ho : chars[cursor]? = some '['
  · cases he : timingEnd chars (cursor + 1) with
    | error issue => simp [timingAt, ho, he, Bind.bind, Except.bind] at h
    | ok finish =>
        have hb := timingEnd_success chars (cursor + 1) finish he
        have heq : finish = stop ∧ true = timing := by
          simpa [timingAt, ho, he, Bind.bind, Except.bind, pure, Except.pure] using h
        rcases heq with ⟨rfl, rfl⟩
        exact ⟨by omega, hb.2⟩
  · have heq : cursor = stop ∧ false = timing := by
      simpa [timingAt, ho, pure, Except.pure] using h
    rcases heq with ⟨rfl, rfl⟩
    exact ⟨Nat.le_refl _, hc⟩

/-- Exact source interpretation of one successful cursor step. -/
def stepView (chars : List Char) (step : StepAt) : ReferenceStep :=
  { segment := renderSegment chars step.plan,
    nextHead := step.nextHead,
    rest := chars.drop step.plan.stop }

/-- O5 one-step bridge. The guarded character is the only premise; shape,
timing, errors, endpoint and record construction are proved here. -/
theorem stepAt_reference (raw : String) (chars : List Char) (head : Char)
    (cursor : Nat) (c : Char) (hget : chars[cursor]? = some c) :
    resultView raw (stepView chars) (stepAt chars head cursor) =
      referenceStep raw head c (chars.drop (cursor + 1)) := by
  cases hd : c.isDigit with
  | true =>
      simp [stepAt, referenceStep, hget, isChainDigit_reference, hd, resultView]
  | false =>
      cases hm : isSlideMarkChar c with
      | false =>
          simp [stepAt, referenceStep, hget, isChainDigit_reference, hd, hm, resultView]
      | true =>
          cases hs : readShapeAt chars c (cursor + 1) with
          | error issue =>
              have hshape := readShapeAt_spec raw chars c (cursor + 1)
              simp only [hs] at hshape
              simp [stepAt, referenceStep, hget, isChainDigit_reference, hd, hm,
                hs, hshape, resultView, Bind.bind, Except.bind]
          | ok value =>
              rcases value with ⟨endpoint, next⟩
              have hspec := readShapeAt_spec raw chars c (cursor + 1)
              simp only [hs] at hspec
              rcases hspec with ⟨hprogress, hbound, hshape, hlast⟩
              have htiming := timingAt_reference raw chars next
              cases ht : timingAt chars next with
              | error issue =>
                  simp only [ht, resultView] at htiming
                  simp [stepAt, referenceStep, hget, isChainDigit_reference, hd, hm,
                    hs, hshape, ht, ← htiming, resultView, Bind.bind, Except.bind]
              | ok value =>
                  rcases value with ⟨stop, timing⟩
                  have hbounds := timingAt_success chars next stop timing hbound ht
                  simp only [ht, resultView] at htiming
                  have hrange : sourceRange chars cursor stop =
                      c :: (sourceRange chars (cursor + 1) next ++ sourceRange chars next stop) := by
                    rw [sourceRange_cons chars cursor stop c hget (by omega),
                      sourceRange_split chars (cursor + 1) next stop (by omega) hbounds.1]
                  have htext : String.ofList (head :: sourceRange chars cursor stop) =
                      (String.singleton head ++ String.ofList (c :: sourceRange chars (cursor + 1) next)) ++
                        String.ofList (sourceRange chars next stop) := by
                    apply String.toList_injective
                    simp only [String.toList_ofList, String.toList_append, String.toList_singleton]
                    rw [hrange]
                    rfl
                  have hsegment :
                      renderSegment chars { head, begin := cursor, stop, hasTiming := timing } =
                        mkOriginalSegment
                          ((String.singleton head ++
                            String.ofList (c :: sourceRange chars (cursor + 1) next)) ++
                              String.ofList (sourceRange chars next stop)) timing := by
                    change mkOriginalSegment (String.ofList (head :: sourceRange chars cursor stop)) timing = _
                    rw [htext]
                  simp [stepAt, referenceStep, hget, isChainDigit_reference, hd, hm,
                    hs, hshape, ht, ← htiming, resultView, stepView, hsegment, hlast,
                    Bind.bind, Except.bind, pure, Except.pure]

/-- O5 progress is derived from the successful required digit and optional
bracket scan. It is not a caller-supplied whole-step condition. -/
theorem stepAt_success (chars : List Char) (head : Char) (cursor : Nat) (step : StepAt)
    (h : stepAt chars head cursor = .ok step) :
    step.plan.head = head ∧ step.plan.begin = cursor ∧
      cursor + 2 ≤ step.plan.stop ∧ step.plan.stop ≤ chars.length := by
  cases hg : chars[cursor]? with
  | none => simp [stepAt, hg] at h
  | some op =>
      cases hd : isChainDigit op with
      | true => simp [stepAt, hg, hd] at h
      | false =>
          cases hm : isSlideMarkChar op with
          | false => simp [stepAt, hg, hd, hm] at h
          | true =>
              cases hs : readShapeAt chars op (cursor + 1) with
              | error issue => simp [stepAt, hg, hd, hm, hs, Bind.bind, Except.bind] at h
              | ok value =>
                  rcases value with ⟨endpoint, next⟩
                  have hshape := readShapeAt_success chars op (cursor + 1) endpoint next hs
                  cases ht : timingAt chars next with
                  | error issue =>
                      simp [stepAt, hg, hd, hm, hs, ht, Bind.bind, Except.bind] at h
                  | ok value =>
                      rcases value with ⟨stop, timing⟩
                      have htiming := timingAt_success chars next stop timing hshape.2.1 ht
                      have heq : ({ plan := { head := head, begin := cursor, stop := stop, hasTiming := timing },
                          nextHead := endpoint } : StepAt) = step := by
                        simpa [stepAt, hg, hd, hm, hs, ht, Bind.bind, Except.bind,
                          pure, Except.pure] using h
                      subst step
                      dsimp only
                      exact ⟨rfl, rfl, by omega, htiming.2⟩

/-- O5 full forward scan correspondence, for every fuel, every natural cursor,
and an arbitrary already accumulated list. Errors discard the entire list. -/
theorem scanFuel_reference (raw : String) (chars : List Char) (fuel : Nat)
    (head : Char) (cursor : Nat) (accSegments : List Plan) :
    resultView raw (List.map (renderSegment chars))
      (scanFuel chars fuel head cursor accSegments) =
        collectFuel raw fuel head (chars.drop cursor) (accSegments.map (renderSegment chars)) := by
  induction fuel generalizing head cursor accSegments with
  | zero =>
      by_cases hi : cursor < chars.length
      · have hg := List.getElem?_eq_getElem hi
        have hd := drop_of_some chars cursor (chars[cursor]) hg
        simp [scanFuel, hi, hd, collectFuel, resultView]
      · have hd : chars.drop cursor = [] := List.drop_eq_nil_of_le (by omega)
        simp [scanFuel, hi, hd, collectFuel, resultView]
  | succ fuel ih =>
      by_cases hi : cursor < chars.length
      · have hg := List.getElem?_eq_getElem hi
        have hd := drop_of_some chars cursor (chars[cursor]) hg
        have href := stepAt_reference raw chars head cursor (chars[cursor]) hg
        cases hs : stepAt chars head cursor with
        | error issue =>
            simp only [hs, resultView] at href
            simp [scanFuel, hi, hs, hd, collectFuel, ← href, resultView]
        | ok step =>
            simp only [hs, resultView] at href
            simp only [scanFuel, hi, if_true, hs]
            rw [ih]
            simp [hd, collectFuel, ← href, stepView, List.map_append]
      · have hd : chars.drop cursor = [] := List.drop_eq_nil_of_le (by omega)
        simp [scanFuel, hi, hd, collectFuel, resultView]

/-- Ordered, adjacent, in-bounds ranges for exactly the newly scanned suffix.
This also proves that successful termination reaches the end of input. -/
theorem scanFuel_spans (chars : List Char) (fuel : Nat) (head : Char) (cursor : Nat)
    (accSegments plans : List Plan) (hc : cursor ≤ chars.length)
    (h : scanFuel chars fuel head cursor accSegments = .ok plans) :
    ∃ tail, plans = accSegments ++ tail ∧ spansFrom chars cursor tail chars.length := by
  induction fuel generalizing head cursor accSegments plans with
  | zero =>
      by_cases hi : cursor < chars.length
      · simp [scanFuel, hi] at h
      · have heq : accSegments = plans := by simpa [scanFuel, hi] using h
        refine ⟨[], ?_, ?_⟩
        · simpa using heq.symm
        · change cursor = chars.length
          omega
  | succ fuel ih =>
      by_cases hi : cursor < chars.length
      · cases hs : stepAt chars head cursor with
        | error issue => simp [scanFuel, hi, hs] at h
        | ok step =>
            have hb := stepAt_success chars head cursor step hs
            have hrec : scanFuel chars fuel step.nextHead step.plan.stop
                (accSegments ++ [step.plan]) = .ok plans := by
              simpa only [scanFuel, hi, if_true, hs] using h
            obtain ⟨tail, heq, hspans⟩ := ih step.nextHead step.plan.stop
              (accSegments ++ [step.plan]) plans hb.2.2.2 hrec
            refine ⟨step.plan :: tail, ?_, ?_⟩
            · simpa only [List.append_assoc, List.singleton_append] using heq
            · exact ⟨hb.2.1, hb.2.2.1, hb.2.2.2, hspans⟩
      · have heq : accSegments = plans := by simpa [scanFuel, hi] using h
        refine ⟨[], ?_, ?_⟩
        · simpa using heq.symm
        · change cursor = chars.length
          omega

/-- O5 all-fuel grammar theorem. No successful-prefix or valid-input restriction. -/
theorem parsePlansFuel_reference (raw : String) (chars : List Char) (head : Char) (fuel : Nat) :
    resultView raw (List.map (renderSegment chars)) (parsePlansFuel chars head fuel) =
      originalFuel fuel raw head chars := by
  rw [parsePlansFuel, scanFuel_reference]
  simp only [List.drop_zero, List.map_nil, collectFuel_reference, prefixResult_nil]

theorem parsePlans_spans (chars : List Char) (head : Char) (plans : List Plan)
    (h : parsePlans chars head = .ok plans) : spansFrom chars 0 plans chars.length := by
  unfold parsePlans parsePlansFuel at h
  obtain ⟨tail, heq, hspans⟩ := scanFuel_spans chars chars.length head 0 [] plans (by omega) h
  simp only [List.nil_append] at heq
  subst plans
  exact hspans

/-- O3/O4/O5 composition: the source cursor grammar, including finite error
encoding and complete ordered segment records, equals the original private
core. Remaining-length fuel is used; the old core's +1 is discharged by the
proved source fuel-stability theorem rather than assumed as an adapter law. -/
theorem parseSegments_reference (raw : String) (chars : List Char) (head : Char) :
    resultView raw id (parseSegments chars head) = original raw head chars := by
  calc
    resultView raw id (parseSegments chars head) =
        resultView raw (List.map (renderSegment chars)) (parsePlansFuel chars head chars.length) := by
          unfold parseSegments parsePlans
          cases parsePlansFuel chars head chars.length <;> rfl
    _ = originalFuel chars.length raw head chars :=
          parsePlansFuel_reference raw chars head chars.length
    _ = original raw head chars := original_remaining_fuel raw head chars

end Verification.SlideChainModel
