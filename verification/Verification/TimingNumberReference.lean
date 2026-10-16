import Verification.TimingNumberModel
import Verification.StringSplit
import Init.Data.String.Lemmas.TakeDrop
import Init.Data.String.Lemmas.Pattern.TakeDrop.String

namespace Verification.TimingNumberModel
open NaturalParserProof

private theorem first_dot_cases (chars : List Char) :
    '.' ∉ chars ∨ ∃ before after, '.' ∉ before ∧ chars = before ++ '.' :: after := by
  induction chars with
  | nil => exact Or.inl (by simp)
  | cons c rest ih =>
      by_cases hc : c = '.'
      · subst c
        exact Or.inr ⟨[], rest, by simp, rfl⟩
      · rcases ih with hrest | ⟨before, after, hbefore, hrest⟩
        · exact Or.inl (by simp [hc, Ne.symm hc, hrest])
        · exact Or.inr ⟨c :: before, after, by simp [hc, Ne.symm hc, hbefore],
            by simp [hrest]⟩

private theorem split_no_dot (chars : List Char) (h : '.' ∉ chars) :
    chars.splitOnP (fun c => c == '.') = [chars] := by
  apply List.splitOnP_eq_singleton
  intro c hc
  have hne : c ≠ '.' := by intro heq; subst c; exact h hc
  simp [hne]

private theorem split_first_dot (before after : List Char) (h : '.' ∉ before) :
    (before ++ '.' :: after).splitOnP (fun c => c == '.') =
      before :: after.splitOnP (fun c => c == '.') := by
  apply List.splitOnP_append_cons_of_forall_mem
  · intro c hc
    have hne : c ≠ '.' := by intro heq; subst c; exact h hc
    simp [hne]
  · rfl

theorem fractionScan_has_dot (chars : List Char) (value denominator : Nat)
    (hdot : '.' ∈ chars) : fractionScan chars value denominator = none := by
  rw [fractionScan_eq_fold]
  have hfalse : chars.all Char.isDigit = false := by
    cases hall : chars.all Char.isDigit with
    | false => rfl
    | true =>
        have hdigit := List.all_eq_true.mp hall '.' hdot
        contradiction
  simp only [hfalse, Bool.false_eq_true, ↓reduceIte]

/-- The original splitter's one-field and two-field cases, before Rat arithmetic. -/
def fields : List String → Option (Nat × Option (Nat × Nat))
  | [whole] => whole.toNat?.map (fun n => (n, none))
  | [whole, frac] => whole.toNat?.bind fun n =>
      (fractionScan frac.toList 0 1).map (fun fraction => (n, some fraction))
  | _ => none

theorem wholeScan_fields (chars : List Char) :
    wholeScan chars 0 false =
      fields ((chars.splitOnP (fun c => c == '.')).map String.ofList) := by
  rcases first_dot_cases chars with hnone | ⟨before, after, hbefore, rfl⟩
  · rw [split_no_dot chars hnone]
    simpa only [List.map_cons, List.map_nil, fields, String.toList_ofList] using
      wholeScan_string_without_dot (String.ofList chars) (by simpa using hnone)
  · rw [wholeScan_at_dot before after 0 false hbefore, split_first_dot before after hbefore]
    rcases first_dot_cases after with hnone | ⟨middle, rest, hmiddle, rfl⟩
    · rw [split_no_dot after hnone]
      simp only [List.map_cons, List.map_nil, fields, String.toList_ofList]
      have hscan : scan before 0 false = (String.ofList before).toNat? := by
        simpa using scan_eq_string_toNat (String.ofList before)
      rw [hscan]
    · rw [split_first_dot middle rest hmiddle]
      have hrest := List.splitOnP_ne_nil (fun c : Char => c == '.') rest
      cases hs : rest.splitOnP (fun c => c == '.') with
      | nil => exact False.elim (hrest hs)
      | cons first remaining =>
          rw [fractionScan_has_dot _ 0 1 (by simp)]
          cases scan before 0 false <;> rfl

/-- This equation uses the actual legacy String.splitOn called by parseRatString?. -/
theorem wholeScan_original_split (s : String) :
    wholeScan s.toList 0 false = fields (s.splitOn ".") := by
  rw [show ("." : String) = String.ofList ['.'] from rfl,
    StringSplit.splitOn_singleton, wholeScan_fields]

def unsignedValue (parts : Nat × Option (Nat × Nat)) : Rat :=
  match parts.2 with
  | none => Int.ofNat parts.1
  | some (value, denominator) =>
      Int.ofNat parts.1 + Int.ofNat value / (Int.ofNat denominator : Rat)

def Parts.value (parts : Parts) : Rat :=
  let value := unsignedValue (parts.whole, parts.fraction)
  if parts.negative then -value else value

theorem parse_string_shape (s : String) :
    parse s.toList =
      if s = "" then none else
        let negative := s.startsWith "-"
        let unsigned := if negative then (s.drop 1).toString else s
        (wholeScan unsigned.toList 0 false).map fun (whole, fraction) =>
          ⟨negative, whole, fraction⟩ := by
  cases hchars : s.toList with
  | nil =>
      have hs : s = "" := by apply String.toList_inj.mp; simpa using hchars
      simp [hs, parse]
  | cons c rest =>
      have hs : s ≠ "" := by intro heq; subst s; simp at hchars
      have hstarts : s.startsWith "-" = (c == '-') := by
        have hiff : (s.startsWith "-" = true) ↔ ((c == '-') = true) := by
          simp only [String.startsWith_string_iff, show ("-" : String).toList = ['-'] from rfl,
            hchars, List.singleton_prefix_cons_iff, beq_iff_eq]
          exact eq_comm
        exact Bool.eq_iff_iff.mpr hiff
      have hdrop : (s.drop 1).toString.toList = rest := by
        change (s.drop 1).copy.toList = rest
        simp [String.toList_copy_drop, hchars]
      simp only [parse, hchars, hs, ↓reduceIte, hstarts]
      cases hn : c == '-' <;> simp [hn, hdrop, hchars]

/-- Character scanning agrees with the original trimmed signed rational parser.
This is the reference-model connection; extracted Rust and numeric representation
are connected separately. -/
theorem parse_value_eq_original (s : String) :
    (parse (LnmaiCore.Simai.trim s).toList).map Parts.value =
      LnmaiCore.Simai.parseRatString? s := by
  rw [parse_string_shape]
  unfold LnmaiCore.Simai.parseRatString?
  by_cases hempty : LnmaiCore.Simai.trim s = ""
  · simp [hempty]
  · simp only [hempty, ↓reduceIte]
    rw [wholeScan_original_split]
    generalize hnegative : (LnmaiCore.Simai.trim s).startsWith "-" = negative
    generalize hparts :
      (if negative then ((LnmaiCore.Simai.trim s).drop 1).toString
        else LnmaiCore.Simai.trim s).splitOn "." = parts
    cases parts with
    | nil => rfl
    | cons whole rest =>
        cases rest with
        | nil => cases hn : whole.toNat? <;> simp [fields, hn, Parts.value, unsignedValue]
        | cons frac rest =>
            cases rest with
            | cons extra rest => rfl
            | nil =>
                cases hn : whole.toNat? with
                | none => simp [fields, hn]
                | some n =>
                    cases hdigits : frac.toList.all Char.isDigit <;>
                      simp [fields, hn, fractionScan_string, hdigits, Parts.value, unsignedValue]

end Verification.TimingNumberModel
