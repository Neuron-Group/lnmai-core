import Verification.Generated
import Init.Data.Char.Lemmas
import Init.Data.String.Search
import Init.Data.String.Lemmas.Iterate
import Init.Data.List.Monadic

/-!
Equivalence proofs for the runtime-used natural_parser character scanner,
against the declarations extracted from its shared Rust implementation.

The reference is the installed Lean 4.30.0 source, not a replacement parser:
  Init/Data/String/Slice.lean:949-963   String.Slice.isNat
  Init/Data/String/Slice.lean:987-991   String.Slice.toNat?
  Init/Data/String/Search.lean:375-376 String.toNat?
The standard library's forIn_eq_forIn_toList and foldl_eq_foldl_toList bridge
the original Slice iteration to lists. The fused scan is then connected to
that original syntax check and decimal fold on every character sequence.

BigUint arithmetic is kept unbounded. Only construction of zero and a single
value * 10 + digit operation have primitive contracts. Concrete Rust UTF-8
decoding/Vec allocation and BigUint/Natural representation remain adapters;
there is no assumption equating an entire parser to String.toNat?.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.NaturalParserProof

-- Match the installed String.Slice loop's elaboration and product state.
set_option backward.do.legacy false in
/-- The exact source loop of String.Slice.isNat, with its container changed to
a List and its initial state exposed for induction. The next theorem connects
this loop to the installed definition through the standard ForIn theorem. -/
def isNatList (chars : List Char) (initial : Bool) : Bool := Id.run do
  let mut lastWasDigit := initial
  for c in chars do
    if c = '_' then
      if !lastWasDigit then
        return false
      lastWasDigit := false
    else if c.isDigit then
      lastWasDigit := true
    else
      return false
  return lastWasDigit

theorem slice_isNat_eq_list (s : String.Slice) :
    s.isNat = isNatList s.copy.toList false := by
  simp only [String.Slice.isNat, isNatList, String.Slice.forIn_eq_forIn_toList]

theorem isNatList_nil (last : Bool) : isNatList [] last = last := by
  rfl

theorem isNatList_cons (c : Char) (chars : List Char) (last : Bool) :
    isNatList (c :: chars) last =
      if c = '_' then
        if last then isNatList chars false else false
      else if c.isDigit then isNatList chars true else false := by
  by_cases hsep : c = '_'
  · cases last <;> simp [isNatList, List.forIn_cons, hsep]
  · by_cases hdigit : c.isDigit = true <;>
      simp [isNatList, List.forIn_cons, hsep, hdigit]

/-- Recursive form of the original isNat loop, including its pending-separator
state. A false final state rejects both empty input and a trailing underscore. -/
def validSyntax : List Char → Bool → Bool
  | [], last => last
  | c :: rest, last =>
      if c = '_' then
        if last then validSyntax rest false else false
      else if c.isDigit then validSyntax rest true else false

theorem isNatList_eq_syntax (chars : List Char) (last : Bool) :
    isNatList chars last = validSyntax chars last := by
  induction chars generalizing last with
  | nil => exact isNatList_nil last
  | cons c rest ih => simp only [isNatList_cons, validSyntax, ih]

theorem slice_isNat_eq_syntax (s : String.Slice) :
    s.isNat = validSyntax s.copy.toList false := by
  rw [slice_isNat_eq_list, isNatList_eq_syntax]

theorem string_isNat_eq_syntax (s : String) :
    s.isNat = validSyntax s.toList false := by
  simpa only [String.isNat, String.copy_toSlice] using
    slice_isNat_eq_syntax s.toSlice

/-- Exactly the fold function used by String.Slice.toNat?. -/
def decimalStep (value : Nat) (c : Char) : Nat :=
  if c = '_' then value else value * 10 + (c.toNat - '0'.toNat)

/-- The fused scanner's remaining result, with an unbounded accumulator. -/
def scan : List Char → Nat → Bool → Option Nat
  | [], value, last => if last then some value else none
  | c :: rest, value, last =>
      if c = '_' then
        if last then scan rest value false else none
      else if c.isDigit then
        scan rest (value * 10 + (c.toNat - '0'.toNat)) true
      else none

/-- Fusing validation and accumulation preserves the original two-pass result,
including every invalid prefix and all final separator states. -/
theorem scan_eq_syntax_fold (chars : List Char) (value : Nat) (last : Bool) :
    scan chars value last =
      if validSyntax chars last then some (chars.foldl decimalStep value) else none := by
  induction chars generalizing value last with
  | nil => rfl
  | cons c rest ih =>
      by_cases hsep : c = '_'
      · cases last <;> simp [scan, validSyntax, decimalStep, hsep, ih]
      · by_cases hdigit : c.isDigit = true <;>
          simp [scan, validSyntax, decimalStep, hsep, hdigit, ih]

/-- Direct connection to the installed original String.Slice.toNat?. -/
theorem scan_eq_slice_toNat (s : String.Slice) :
    scan s.copy.toList 0 false = s.toNat? := by
  simp only [scan_eq_syntax_fold, String.Slice.toNat?, slice_isNat_eq_syntax,
    String.Slice.foldl_eq_foldl_toList, decimalStep]
  rfl

/-- No parser-equivalence premise: both original String wrappers are unfolded. -/
theorem scan_eq_string_toNat (s : String) :
    scan s.toList 0 false = s.toNat? := by
  simpa only [String.toNat?, String.copy_toSlice] using scan_eq_slice_toNat s.toSlice

theorem syntax_empty : validSyntax [] false = false := rfl

theorem syntax_leading_underscore (rest : List Char) :
    validSyntax ('_' :: rest) false = false := by
  simp [validSyntax]

theorem syntax_trailing_underscore (chars : List Char) (last : Bool) :
    validSyntax (chars ++ ['_']) last = false := by
  induction chars generalizing last with
  | nil => simp [validSyntax]
  | cons c rest ih => simp [validSyntax, ih]

theorem syntax_repeated_underscore (before after : List Char) (last : Bool) :
    validSyntax (before ++ '_' :: '_' :: after) last = false := by
  induction before generalizing last with
  | nil => simp [validSyntax]
  | cons c rest ih => simp [validSyntax, ih]

/-- Covers signs, whitespace and any other non-digit/non-separator anywhere
in the input, with no condition on the surrounding characters. -/
theorem syntax_rejects_at (before after : List Char) (c : Char) (last : Bool)
    (hsep : c ≠ '_') (hdigit : c.isDigit = false) :
    validSyntax (before ++ c :: after) last = false := by
  induction before generalizing last with
  | nil => simp [validSyntax, hsep, hdigit]
  | cons head rest ih => simp [validSyntax, ih]

theorem non_ascii_not_digit (c : Char) (hhigh : 127 < c.toNat) :
    c.isDigit = false := by
  cases hdigit : c.isDigit with
  | false => rfl
  | true =>
      have hbounds := Char.isDigit_iff_toNat.mp hdigit
      change 48 ≤ c.toNat ∧ c.toNat ≤ 57 at hbounds
      omega

theorem syntax_rejects_non_ascii (before after : List Char) (c : Char) (last : Bool)
    (hhigh : 127 < c.toNat) :
    validSyntax (before ++ c :: after) last = false := by
  apply syntax_rejects_at before after c last
  · intro hsep
    subst c
    change 127 < 95 at hhigh
    omega
  · exact non_ascii_not_digit c hhigh

/-- Exhaustiveness is derived from the installed Char.isDigit definition, not
from a sampled set of input strings. In particular, Unicode digits are absent. -/
theorem ascii_digit_cases (c : Char) (hdigit : c.isDigit = true) :
    c = '0' ∨ c = '1' ∨ c = '2' ∨ c = '3' ∨ c = '4' ∨
      c = '5' ∨ c = '6' ∨ c = '7' ∨ c = '8' ∨ c = '9' := by
  have hbounds := Char.isDigit_iff_toNat.mp hdigit
  change 48 ≤ c.toNat ∧ c.toNat ≤ 57 at hbounds
  simp only [← Char.toNat_inj]
  change c.toNat = 48 ∨ c.toNat = 49 ∨ c.toNat = 50 ∨ c.toNat = 51 ∨
    c.toNat = 52 ∨ c.toNat = 53 ∨ c.toNat = 54 ∨ c.toNat = 55 ∨
    c.toNat = 56 ∨ c.toNat = 57
  omega

theorem digit_value_lt_ten (c : Char) (hdigit : c.isDigit = true) :
    c.toNat - '0'.toNat < 10 := by
  have hbounds := Char.isDigit_iff_toNat.mp hdigit
  change 48 ≤ c.toNat ∧ c.toNat ≤ 57 at hbounds
  change c.toNat - 48 < 10
  omega

/-- A bound is needed only for the digit passed to the arithmetic primitive;
there is no machine-integer bound on the accumulator or parsed result. -/
def DigitPost (c : Char) : Option U8 → Prop
  | none => c.isDigit = false
  | some digit => c.isDigit = true ∧
      digit.val = c.toNat - '0'.toNat ∧ digit.val < 10

def digitValue (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat) else none

/-- Actual ten-way Rust character dispatch, for every Lean/Rust scalar value. -/
theorem decimal_digit_spec (c : Char) :
    natural_parser.decimal_digit c ⦃ DigitPost c ⦄ := by
  apply exists_imp_spec
  by_cases h0 : c = '0'
  · subst c; exact ⟨some 0#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h1 : c = '1'
  · subst c; exact ⟨some 1#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h2 : c = '2'
  · subst c; exact ⟨some 2#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h3 : c = '3'
  · subst c; exact ⟨some 3#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h4 : c = '4'
  · subst c; exact ⟨some 4#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h5 : c = '5'
  · subst c; exact ⟨some 5#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h6 : c = '6'
  · subst c; exact ⟨some 6#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h7 : c = '7'
  · subst c; exact ⟨some 7#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h8 : c = '8'
  · subst c; exact ⟨some 8#u8, rfl, by unfold DigitPost; decide⟩
  by_cases h9 : c = '9'
  · subst c; exact ⟨some 9#u8, rfl, by unfold DigitPost; decide⟩
  have hdigit : c.isDigit = false := by
    cases h : c.isDigit with
    | false => rfl
    | true =>
        rcases ascii_digit_cases c h with h | h | h | h | h | h | h | h | h | h
        all_goals contradiction
  refine ⟨none, ?_, hdigit⟩
  simp only [natural_parser.decimal_digit, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9,
    ↓reduceIte]

theorem decimal_digit_equiv (c : Char) :
    (do let digit ← natural_parser.decimal_digit c
        ok (digit.map UScalar.val)) = ok (digitValue c) := by
  obtain ⟨digit, hdigit, hpost⟩ := spec_imp_exists (decimal_digit_spec c)
  simp only [hdigit, bind_tc_ok]
  cases digit with
  | none => simp [digitValue, DigitPost] at hpost ⊢; simp [hpost]
  | some digit =>
      obtain ⟨hisDigit, hvalue, _⟩ := hpost
      simp [digitValue, hisDigit, hvalue]

/-- Callback contract for precisely one BigUint-style decimal operation. The
kernel supplies a proved ASCII digit; the value remains an arbitrary natural. -/
def DecimalAppendLaw {T F : Type}
    (call : core.ops.function.Fn F (T × U8) T) (append : F)
    (decode : T → Nat) : Prop :=
  ∀ (value : T) (digit : U8), digit.val < 10 →
    call.call append (value, digit)
      ⦃ next => decode next = decode value * 10 + digit.val ⦄

/-- The invariant records the remaining fused scan, so it covers success and
every early rejection with one relation. Bounds are part of the invariant. -/
def ScanInvariant {T : Type} (chars : Slice Char) (decode : T → Nat)
    (target : Option Nat) (state : T × Bool × Usize) : Prop :=
  state.2.2.val ≤ chars.val.length ∧
    scan (chars.val.drop state.2.2.val) (decode state.1) state.2.1 = target

theorem scan_index_increment_bound (chars : Slice Char) (index : Usize)
    (hmore : index.val < chars.val.length) :
    index.val + (1#usize).val ≤ UScalar.max .Usize := by
  have hsize := Slice.length_ineq chars
  scalar_tac

/-- The generated while loop agrees with the remaining reference scan. Its
invariant covers every early rejection as well as every successful prefix. -/
theorem parse_decimal_loop_spec {T F : Type}
    (call : core.ops.function.Fn F (T × U8) T) (chars : Slice Char)
    (append : F) (decode : T → Nat) (initial : T)
    (initialLast : Bool) (initialIndex : Usize)
    (hindex : initialIndex.val ≤ chars.val.length)
    (happend : DecimalAppendLaw call append decode) :
    natural_parser.parse_decimal_loop call chars append initial initialLast initialIndex
      ⦃ out => out.map decode =
        scan (chars.val.drop initialIndex.val) (decode initial) initialLast ⦄ := by
  unfold natural_parser.parse_decimal_loop
  apply loop.spec_decr_nat
    (fun (state : T × Bool × Usize) => chars.val.length - state.2.2.val)
    (ScanInvariant chars decode
      (scan (chars.val.drop initialIndex.val) (decode initial) initialLast))
  · rintro ⟨value, last, index⟩ hinv
    obtain ⟨hbound, hinv⟩ := hinv
    dsimp only at hbound hinv ⊢
    unfold natural_parser.parse_decimal_loop.body
    by_cases hmore : index < Slice.len chars
    · have hlt : index.val < chars.val.length := by simpa using hmore
      obtain ⟨c, hget, hc⟩ := spec_imp_exists
        (Slice.index_usize_spec chars index hlt)
      rw [List.drop_eq_getElem_cons hlt, ← hc] at hinv
      obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
        (UScalar.add_spec (x := index) (y := 1#usize)
          (scan_index_increment_bound chars index hlt))
      have hv : next.val = index.val + 1 := by simpa using hnextVal
      have hnextBound : next.val ≤ chars.val.length := by omega
      have hdecrease : chars.val.length - next.val < chars.val.length - index.val := by
        omega
      by_cases hsep : c = '_'
      · cases last with
        | false =>
            have hout : (none : Option Nat) =
                scan (chars.val.drop initialIndex.val) (decode initial) initialLast := by
              simpa [scan, hsep] using hinv
            simpa [hmore, hget, bind_tc_ok, hsep] using hout
        | true =>
            have hkeep : ScanInvariant chars decode
                (scan (chars.val.drop initialIndex.val) (decode initial) initialLast)
                (value, false, next) := by
              refine ⟨hnextBound, ?_⟩
              simpa [scan, hsep, hv] using hinv
            simpa [hmore, hget, bind_tc_ok, hsep, hnext] using
              And.intro hkeep hdecrease
      · obtain ⟨digit, hdigit, hpost⟩ := spec_imp_exists (decimal_digit_spec c)
        cases digit with
        | none =>
            have hfalse : c.isDigit = false := hpost
            have hout : (none : Option Nat) =
                scan (chars.val.drop initialIndex.val) (decode initial) initialLast := by
              simpa [scan, hsep, hfalse] using hinv
            simpa [hmore, hget, bind_tc_ok, hsep, hdigit] using hout
        | some digit =>
            obtain ⟨hisDigit, hvalue, hsmall⟩ := hpost
            obtain ⟨appended, happendRun, happendVal⟩ := spec_imp_exists
              (happend value digit hsmall)
            have hkeep : ScanInvariant chars decode
                (scan (chars.val.drop initialIndex.val) (decode initial) initialLast)
                (appended, true, next) := by
              refine ⟨hnextBound, ?_⟩
              simpa [scan, hsep, hisDigit, hv, happendVal, hvalue] using hinv
            simpa [hmore, hget, bind_tc_ok, hsep, hdigit, happendRun, hnext] using
              And.intro hkeep hdecrease
    · have hdone : chars.val.length ≤ index.val := by simpa using hmore
      have hempty : chars.val.drop index.val = [] := List.drop_eq_nil_of_le hdone
      cases last <;> simpa [hmore, hempty, scan] using hinv
  · exact ⟨hindex, rfl⟩

/-- Only zero construction and one decimal append are assumed; there is no
whole-parser premise and no upper bound on the decoded natural result. -/
theorem parse_decimal_spec {T Z F : Type}
    (zeroCall : core.ops.function.Fn Z Unit T)
    (appendCall : core.ops.function.Fn F (T × U8) T)
    (chars : Slice Char) (zero : Z) (append : F) (decode : T → Nat)
    (hzero : zeroCall.call zero () ⦃ value => decode value = 0 ⦄)
    (happend : DecimalAppendLaw appendCall append decode) :
    natural_parser.parse_decimal zeroCall appendCall chars zero append
      ⦃ out => out.map decode = scan chars.val 0 false ⦄ := by
  obtain ⟨initial, hrun, hinitial⟩ := spec_imp_exists hzero
  unfold natural_parser.parse_decimal
  simp only [hrun, bind_tc_ok]
  simpa [hinitial] using parse_decimal_loop_spec appendCall chars append decode
    initial false 0#usize (by simp) happend

/-- The shared Rust scanner implements the installed original String parser
under explicit character and arithmetic representation laws. -/
theorem parse_decimal_string_equiv {T Z F : Type}
    (zeroCall : core.ops.function.Fn Z Unit T)
    (appendCall : core.ops.function.Fn F (T × U8) T)
    (chars : Slice Char) (zero : Z) (append : F) (decode : T → Nat)
    (s : String) (hchars : chars.val = s.toList)
    (hzero : zeroCall.call zero () ⦃ value => decode value = 0 ⦄)
    (happend : DecimalAppendLaw appendCall append decode) :
    (do let parsed ← natural_parser.parse_decimal zeroCall appendCall chars zero append
        ok (parsed.map decode)) = ok s.toNat? := by
  obtain ⟨parsed, hrun, hparsed⟩ := spec_imp_exists
    (parse_decimal_spec zeroCall appendCall chars zero append decode hzero happend)
  rw [hchars, scan_eq_string_toNat] at hparsed
  simp only [hrun, bind_tc_ok, hparsed]

end Verification.NaturalParserProof
