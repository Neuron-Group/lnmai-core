import Verification.TimingNumberReference

/-! Proofs for the real extracted timing-number loops. Only the three decimal
arithmetic primitives have contracts; the syntax, cursor movement, sign and
fraction traversal are proved from Generated. Concrete Natural/BigUint and
UTF-8 representation are separate obligations. -/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.TimingNumberProof
open NaturalParserProof TimingNumberModel

structure DecimalLaws {T O : Type} (inst : timing_number.DecimalOps O T)
    (ops : O) (decode : T → Nat) : Prop where
  zero : inst.zero ops ⦃ value => decode value = 0 ⦄
  one : inst.one ops ⦃ value => decode value = 1 ⦄
  append : ∀ (value : T) (digit : U8), digit.val < 10 →
    inst.append ops value digit ⦃ next => decode next = decode value * 10 + digit.val ⦄

def decodePair {T : Type} (decode : T → Nat) (pair : T × T) : Nat × Nat :=
  (decode pair.1, decode pair.2)

def decodeParts {T : Type} (decode : T → Nat) (parts : timing_number.Parts T) :
    TimingNumberModel.Parts :=
  ⟨parts.negative, decode parts.whole, parts.fraction.map (decodePair decode)⟩

def withSign (negative : Bool) (parts : Nat × Option (Nat × Nat)) :
    TimingNumberModel.Parts := ⟨negative, parts.1, parts.2⟩

def FractionInvariant {T : Type} (chars : Slice Char) (decode : T → Nat)
    (target : Option (Nat × Nat)) (state : Usize × T × T) : Prop :=
  state.1.val ≤ chars.val.length ∧
    fractionScan (chars.val.drop state.1.val) (decode state.2.1) (decode state.2.2) = target

theorem fraction_loop_spec {T O : Type} (inst : timing_number.DecimalOps O T)
    (chars : Slice Char) (ops : O) (decode : T → Nat) (laws : DecimalLaws inst ops decode)
    (initialIndex : Usize) (initialValue initialDenominator : T)
    (hindex : initialIndex.val ≤ chars.val.length) :
    timing_number.fraction_loop inst chars initialIndex ops initialValue initialDenominator
      ⦃ out => out.map (decodePair decode) =
        fractionScan (chars.val.drop initialIndex.val) (decode initialValue)
          (decode initialDenominator) ⦄ := by
  unfold timing_number.fraction_loop
  apply loop.spec_decr_nat
    (fun (state : Usize × T × T) => chars.val.length - state.1.val)
    (FractionInvariant chars decode
      (fractionScan (chars.val.drop initialIndex.val) (decode initialValue)
        (decode initialDenominator)))
  · rintro ⟨index, value, denominator⟩ hinv
    obtain ⟨hbound, hinv⟩ := hinv
    dsimp only at hbound hinv ⊢
    unfold timing_number.fraction_loop.body
    by_cases hmore : index < Slice.len chars
    · have hlt : index.val < chars.val.length := by simpa using hmore
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars index hlt)
      rw [List.drop_eq_getElem_cons hlt, ← hc] at hinv
      obtain ⟨digit, hdigit, hpost⟩ := spec_imp_exists (decimal_digit_spec c)
      cases digit with
      | none =>
          have hfalse : c.isDigit = false := hpost
          have hout : (none : Option (Nat × Nat)) =
              fractionScan (chars.val.drop initialIndex.val) (decode initialValue)
                (decode initialDenominator) := by
            simpa [fractionScan, hfalse] using hinv
          simpa [hmore, hget, bind_tc_ok, hdigit] using hout
      | some digit =>
          obtain ⟨hisDigit, hvalue, hsmall⟩ := hpost
          obtain ⟨nextValue, hvalueRun, hvalueDecode⟩ := spec_imp_exists
            (laws.append value digit hsmall)
          obtain ⟨nextDenominator, hdenomRun, hdenomDecode⟩ := spec_imp_exists
            (laws.append denominator 0#u8 (by decide))
          obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
            (UScalar.add_spec (x := index) (y := 1#usize)
              (scan_index_increment_bound chars index hlt))
          have hv : next.val = index.val + 1 := by simpa using hnextVal
          have hkeep : FractionInvariant chars decode
              (fractionScan (chars.val.drop initialIndex.val) (decode initialValue)
                (decode initialDenominator)) (next, nextValue, nextDenominator) := by
            refine ⟨?_, ?_⟩
            · change next.val ≤ chars.val.length
              omega
            simpa [fractionScan, hisDigit, hv, hvalueDecode, hdenomDecode, hvalue] using hinv
          have hdecrease : chars.val.length - next.val < chars.val.length - index.val := by
            omega
          simpa [hmore, hget, bind_tc_ok, hdigit, hvalueRun, hdenomRun, hnext] using
            And.intro hkeep hdecrease
    · have hdone : chars.val.length ≤ index.val := by simpa using hmore
      have hempty : chars.val.drop index.val = [] := List.drop_eq_nil_of_le hdone
      simpa [hmore, hempty, fractionScan, decodePair] using hinv
  · exact ⟨hindex, rfl⟩

theorem fraction_spec {T O : Type} (inst : timing_number.DecimalOps O T)
    (chars : Slice Char) (index : Usize) (ops : O) (decode : T → Nat)
    (laws : DecimalLaws inst ops decode) (hindex : index.val ≤ chars.val.length) :
    timing_number.fraction inst chars index ops
      ⦃ out => out.map (decodePair decode) = fractionScan (chars.val.drop index.val) 0 1 ⦄ := by
  have hguard : ¬index > Slice.len chars := by simpa using Nat.not_lt.mpr hindex
  obtain ⟨zero, hzero, hzeroValue⟩ := spec_imp_exists laws.zero
  obtain ⟨one, hone, honeValue⟩ := spec_imp_exists laws.one
  unfold timing_number.fraction
  simp only [hguard, ↓reduceIte, hzero, hone, bind_tc_ok]
  simpa [hzeroValue, honeValue] using
    fraction_loop_spec inst chars ops decode laws index zero one hindex

theorem fraction_outside {T O : Type} (inst : timing_number.DecimalOps O T)
    (chars : Slice Char) (index : Usize) (ops : O)
    (hindex : chars.val.length < index.val) :
    timing_number.fraction inst chars index ops = ok none := by
  have hguard : index > Slice.len chars := by simpa using hindex
  simp [timing_number.fraction, hguard]

def WholeInvariant {T : Type} (chars : Slice Char) (decode : T → Nat)
    (negative : Bool) (target : Option TimingNumberModel.Parts)
    (state : Usize × T × Bool) : Prop :=
  state.1.val ≤ chars.val.length ∧
    (wholeScan (chars.val.drop state.1.val) (decode state.2.1) state.2.2).map
      (withSign negative) = target

theorem parse_loop_spec {T O : Type} (inst : timing_number.DecimalOps O T)
    (chars : Slice Char) (ops : O) (decode : T → Nat) (laws : DecimalLaws inst ops decode)
    (negative : Bool) (initialIndex : Usize) (initialValue : T) (initialLast : Bool)
    (hindex : initialIndex.val ≤ chars.val.length) :
    timing_number.parse_loop inst chars ops negative initialIndex initialValue initialLast
      ⦃ out => out.map (decodeParts decode) =
        (wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
          (withSign negative) ⦄ := by
  unfold timing_number.parse_loop
  apply loop.spec_decr_nat
    (fun (state : Usize × T × Bool) => chars.val.length - state.1.val)
    (WholeInvariant chars decode negative
      ((wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
        (withSign negative)))
  · rintro ⟨index, value, last⟩ hinv
    obtain ⟨hbound, hinv⟩ := hinv
    dsimp only at hbound hinv ⊢
    unfold timing_number.parse_loop.body
    by_cases hmore : index < Slice.len chars
    · have hlt : index.val < chars.val.length := by simpa using hmore
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars index hlt)
      rw [List.drop_eq_getElem_cons hlt, ← hc] at hinv
      obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
        (UScalar.add_spec (x := index) (y := 1#usize)
          (scan_index_increment_bound chars index hlt))
      have hv : next.val = index.val + 1 := by simpa using hnextVal
      have hnextBound : next.val ≤ chars.val.length := by omega
      have hdecrease : chars.val.length - next.val < chars.val.length - index.val := by omega
      by_cases hdot : c = '.'
      · cases last with
        | false =>
            have hout : (none : Option TimingNumberModel.Parts) =
                (wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
                  (withSign negative) := by simpa [wholeScan, hdot] using hinv
            simpa [hmore, hget, bind_tc_ok, hdot] using hout
        | true =>
            obtain ⟨fraction, hfracRun, hfrac⟩ := spec_imp_exists
              (fraction_spec inst chars next ops decode laws hnextBound)
            have htail : fractionScan (chars.val.drop (index.val + 1)) 0 1 =
                fraction.map (decodePair decode) := by simpa [hv] using hfrac.symm
            cases fraction <;>
              simpa [hmore, hget, bind_tc_ok, hdot, hnext, hfracRun,
                wholeScan, htail, decodeParts, withSign] using hinv
      · by_cases hsep : c = '_'
        · cases last with
          | false =>
              have hout : (none : Option TimingNumberModel.Parts) =
                  (wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
                    (withSign negative) := by simpa [wholeScan, hdot, hsep] using hinv
              simpa [hmore, hget, bind_tc_ok, hdot, hsep] using hout
          | true =>
              have hkeep : WholeInvariant chars decode negative
                  ((wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
                    (withSign negative)) (next, value, false) := by
                refine ⟨hnextBound, ?_⟩
                simpa [wholeScan, hdot, hsep, hv] using hinv
              simpa [hmore, hget, bind_tc_ok, hdot, hsep, hnext] using
                And.intro hkeep hdecrease
        · obtain ⟨digit, hdigit, hpost⟩ := spec_imp_exists (decimal_digit_spec c)
          cases digit with
          | none =>
              have hfalse : c.isDigit = false := hpost
              have hout : (none : Option TimingNumberModel.Parts) =
                  (wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
                    (withSign negative) := by
                simpa [wholeScan, hdot, hsep, hfalse] using hinv
              simpa [hmore, hget, bind_tc_ok, hdot, hsep, hdigit] using hout
          | some digit =>
              obtain ⟨hisDigit, hvalue, hsmall⟩ := hpost
              obtain ⟨appended, happendRun, happendVal⟩ := spec_imp_exists
                (laws.append value digit hsmall)
              have hkeep : WholeInvariant chars decode negative
                  ((wholeScan (chars.val.drop initialIndex.val) (decode initialValue) initialLast).map
                    (withSign negative)) (next, appended, true) := by
                refine ⟨hnextBound, ?_⟩
                simpa [wholeScan, hdot, hsep, hisDigit, hv, happendVal, hvalue] using hinv
              simpa [hmore, hget, bind_tc_ok, hdot, hsep, hdigit, happendRun, hnext] using
                And.intro hkeep hdecrease
    · have hdone : chars.val.length ≤ index.val := by simpa using hmore
      have hempty : chars.val.drop index.val = [] := List.drop_eq_nil_of_le hdone
      cases last <;> simpa [hmore, hempty, wholeScan, decodeParts, withSign] using hinv
  · exact ⟨hindex, rfl⟩

theorem parse_spec {T O : Type} (inst : timing_number.DecimalOps O T)
    (chars : Slice Char) (ops : O) (decode : T → Nat) (laws : DecimalLaws inst ops decode) :
    timing_number.parse inst chars ops
      ⦃ out => out.map (decodeParts decode) = TimingNumberModel.parse chars.val ⦄ := by
  unfold timing_number.parse
  simp only [core.slice.Slice.is_empty, bind_tc_ok]
  cases hlist : chars.val with
  | nil => simp [Slice.length, hlist, TimingNumberModel.parse]
  | cons c rest =>
      have hlt : (0#usize).val < chars.val.length := by simp [hlist]
      obtain ⟨first, hget, hfirst⟩ := spec_imp_exists (Slice.index_usize_spec chars 0#usize hlt)
      have hfc : first = c := by simpa [hlist] using hfirst
      subst first
      obtain ⟨zero, hzero, hzeroVal⟩ := spec_imp_exists laws.zero
      by_cases hminus : c = '-'
      · have hscan := parse_loop_spec inst chars ops decode laws true 1#usize zero false
          (by simp [hlist])
        simpa [Slice.length, hlist, hget, bind_tc_ok, hminus, hzero, hzeroVal,
          TimingNumberModel.parse, withSign] using hscan
      · have hscan := parse_loop_spec inst chars ops decode laws false 0#usize zero false
          (by simp)
        have hminusBool : (c == '-') = false := by simp [hminus]
        simpa [Slice.length, hlist, hget, bind_tc_ok, hminus, hzero, hzeroVal,
          TimingNumberModel.parse, withSign, hminusBool] using hscan

/-- Exact signed-rational parser result, under explicit character decoding and
decimal primitive laws. No parser or scan equality is assumed. -/
theorem parse_value_eq_original {T O : Type} (inst : timing_number.DecimalOps O T)
    (chars : Slice Char) (ops : O) (decode : T → Nat) (laws : DecimalLaws inst ops decode)
    (s : String) (hchars : chars.val = (LnmaiCore.Simai.trim s).toList) :
    (do let out ← timing_number.parse inst chars ops
        ok (out.map fun parts => (decodeParts decode parts).value)) =
      ok (LnmaiCore.Simai.parseRatString? s) := by
  obtain ⟨out, hrun, hparts⟩ := spec_imp_exists (parse_spec inst chars ops decode laws)
  have hvalue := congrArg (Option.map TimingNumberModel.Parts.value) hparts
  rw [hchars, TimingNumberModel.parse_value_eq_original] at hvalue
  simpa [hrun, bind_tc_ok, Option.map_map, Function.comp_def] using hvalue

end Verification.TimingNumberProof
