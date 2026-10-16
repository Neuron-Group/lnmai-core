import Verification.NaturalParser
import LnmaiCore.Simai.Timing

/-!
Character model for the runtime timing-number kernel. The whole-number grammar
is connected to the installed String.toNat? definition; the fraction preserves
its exact decimal numerator and power-of-ten denominator. The legacy
String.splitOn adapter and the concrete Rat representation are separate open
obligations, not assumptions of the scanner lemmas below.
-/

namespace Verification.TimingNumberModel
open NaturalParserProof

structure Parts where
  negative : Bool
  whole : Nat
  fraction : Option (Nat × Nat)
  deriving DecidableEq

def fractionScan : List Char → Nat → Nat → Option (Nat × Nat)
  | [], value, denominator => some (value, denominator)
  | c :: rest, value, denominator =>
      if c.isDigit then
        fractionScan rest (value * 10 + (c.toNat - '0'.toNat)) (denominator * 10)
      else none

def wholeScan : List Char → Nat → Bool → Option (Nat × Option (Nat × Nat))
  | [], value, last => if last then some (value, none) else none
  | c :: rest, value, last =>
      if c = '.' then
        if last then (fractionScan rest 0 1).map (fun fraction => (value, some fraction))
        else none
      else if c = '_' then
        if last then wholeScan rest value false else none
      else if c.isDigit then
        wholeScan rest (value * 10 + (c.toNat - '0'.toNat)) true
      else none

def parse (chars : List Char) : Option Parts :=
  match chars with
  | [] => none
  | c :: rest =>
      let negative := c == '-'
      let unsigned := if negative then rest else chars
      (wholeScan unsigned 0 false).map fun (whole, fraction) =>
        ⟨negative, whole, fraction⟩

private theorem digit_ne_separator (c : Char) (h : c.isDigit = true) : c ≠ '_' := by
  intro heq
  subst c
  contradiction

theorem fractionScan_eq_fold (chars : List Char) (value denominator : Nat) :
    fractionScan chars value denominator =
      if chars.all Char.isDigit then
        some (chars.foldl decimalStep value, denominator * 10 ^ chars.length)
      else none := by
  induction chars generalizing value denominator with
  | nil => simp [fractionScan]
  | cons c rest ih =>
      by_cases hdigit : c.isDigit = true
      · have hsep := digit_ne_separator c hdigit
        simp [fractionScan, hdigit, ih, decimalStep, hsep, Nat.pow_succ,
          Nat.mul_assoc, Nat.mul_left_comm, Nat.mul_comm]
      · simp [fractionScan, hdigit]

theorem all_digits_syntax (chars : List Char) (last : Bool)
    (hdigits : chars.all Char.isDigit = true) :
    validSyntax chars last = if chars.isEmpty then last else true := by
  induction chars generalizing last with
  | nil => rfl
  | cons c rest ih =>
      have hparts : c.isDigit = true ∧ rest.all Char.isDigit = true := by
        simpa only [List.all_cons, Bool.and_eq_true] using hdigits
      obtain ⟨hc, hr⟩ := hparts
      have hsep := digit_ne_separator c hc
      simp [validSyntax, hsep, hc, ih true hr]

theorem digits_fold_eq_toNat_getD (s : String)
    (hdigits : s.toList.all Char.isDigit = true) :
    s.toList.foldl decimalStep 0 = s.toNat?.getD 0 := by
  rw [← scan_eq_string_toNat, scan_eq_syntax_fold, all_digits_syntax _ _ hdigits]
  cases hchars : s.toList with
  | nil => simp
  | cons c rest => simp

/-- Exact empty-fraction behavior is included: its numerator is zero and its
denominator is one, just as the original parser's getD 0 and 10^length. -/
theorem fractionScan_string (s : String) :
    fractionScan s.toList 0 1 =
      if s.toList.all Char.isDigit then some (s.toNat?.getD 0, 10 ^ s.length)
      else none := by
  rw [fractionScan_eq_fold]
  by_cases hdigits : s.toList.all Char.isDigit = true
  · simp [hdigits, digits_fold_eq_toNat_getD s hdigits]
  · simp [hdigits]

theorem wholeScan_without_dot (chars : List Char) (value : Nat) (last : Bool)
    (hdot : '.' ∉ chars) :
    wholeScan chars value last = (scan chars value last).map (fun n => (n, none)) := by
  induction chars generalizing value last with
  | nil => cases last <;> rfl
  | cons c rest ih =>
      have hc : c ≠ '.' := by intro heq; subst c; exact hdot (by simp)
      have hr : '.' ∉ rest := by intro h; exact hdot (by simp [h])
      by_cases hsep : c = '_'
      · cases last <;> simp [wholeScan, scan, hc, hsep, ih _ _ hr]
      · by_cases hdigit : c.isDigit = true <;>
          simp [wholeScan, scan, hc, hsep, hdigit, ih _ _ hr]

theorem wholeScan_string_without_dot (s : String) (hdot : '.' ∉ s.toList) :
    wholeScan s.toList 0 false = s.toNat?.map (fun n => (n, none)) := by
  rw [wholeScan_without_dot _ _ _ hdot, scan_eq_string_toNat]

theorem wholeScan_at_dot (before after : List Char) (value : Nat) (last : Bool)
    (hdot : '.' ∉ before) :
    wholeScan (before ++ '.' :: after) value last =
      (scan before value last).bind fun whole =>
        (fractionScan after 0 1).map (fun fraction => (whole, some fraction)) := by
  induction before generalizing value last with
  | nil => cases last <;> simp [wholeScan, scan]
  | cons c rest ih =>
      have hc : c ≠ '.' := by intro heq; subst c; exact hdot (by simp)
      have hr : '.' ∉ rest := by intro h; exact hdot (by simp [h])
      by_cases hsep : c = '_'
      · cases last <;> simp [wholeScan, scan, hc, hsep, ih _ _ hr]
      · by_cases hdigit : c.isDigit = true <;>
          simp [wholeScan, scan, hc, hsep, hdigit, ih _ _ hr]

end Verification.TimingNumberModel
