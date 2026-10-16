import Verification.SlideChainExtractedReads

/-! Actual extraction-3 timing loop. Proof draft; no compiler was run. -/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.SlideChainExtracted

/-- The invariant is the remaining existing-model computation. Every cont
decreases actual machine fuel; a closing bracket is accepted before checking
fuel zero. The theorem includes every usize cursor and every usize fuel. -/
theorem timing_end_with_fuel_loop_spec (chars : Slice Char)
    (initialCursor initialFuel : Usize) :
    slide_chain.timing_end_with_fuel_loop chars initialCursor initialFuel
      ⦃ out => decodeResult UScalar.val out =
        SlideChainModel.timingEndFuel initialFuel.val chars.val initialCursor.val ⦄ := by
  unfold slide_chain.timing_end_with_fuel_loop
  apply loop.spec_decr_nat (fun (state : Usize × Usize) => state.2.val)
    (fun state => SlideChainModel.timingEndFuel state.2.val chars.val state.1.val =
      SlideChainModel.timingEndFuel initialFuel.val chars.val initialCursor.val)
  · rintro ⟨cursor, fuel⟩ hinv
    dsimp only at hinv ⊢
    unfold slide_chain.timing_end_with_fuel_loop.body
    by_cases hmore : cursor < Slice.len chars
    · have hi : cursor.val < chars.val.length := by simpa using hmore
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
      have hlookup : chars.val[cursor.val]? = some c := by
        simpa only [List.getElem?_eq_getElem hi, hc]
      obtain ⟨next, hnext, hn⟩ := spec_imp_exists
        (UScalar.add_spec (x := cursor) (y := 1#usize)
          (NaturalParserProof.scan_index_increment_bound chars cursor hi))
      have hnv : next.val = cursor.val + 1 := by simpa using hn
      by_cases hclose : c = ']'
      · have hout : (Except.ok next.val : Except Issue Nat) =
            SlideChainModel.timingEndFuel initialFuel.val chars.val initialCursor.val := by
          cases hf : fuel.val <;>
            simpa [SlideChainModel.timingEndFuel, hf, hlookup, hclose, hnv] using hinv
        simpa [hmore, hget, hnext, hclose, decodeResult] using hout
      · by_cases hzero : fuel = 0#usize
        · have hf : fuel.val = 0 := by simp [hzero]
          have hout : (Except.error Issue.unterminatedTiming : Except Issue Nat) =
              SlideChainModel.timingEndFuel initialFuel.val chars.val initialCursor.val := by
            simpa [SlideChainModel.timingEndFuel, hf, hlookup, hclose] using hinv
          simpa [hmore, hget, hnext, hclose, hzero, decodeResult, decodeError] using hout
        · have hfuel : (1#usize).val ≤ fuel.val := by scalar_tac
          obtain ⟨remaining, hsub, hr, _⟩ := spec_imp_exists
            (UScalar.sub_spec (x := fuel) (y := 1#usize) hfuel)
          have hrv : remaining.val = fuel.val - 1 := by simpa using hr
          have hsucc : fuel.val = remaining.val + 1 := by scalar_tac
          have hkeep : SlideChainModel.timingEndFuel remaining.val chars.val next.val =
              SlideChainModel.timingEndFuel initialFuel.val chars.val initialCursor.val := by
            simpa only [hsucc, SlideChainModel.timingEndFuel, hlookup, if_neg hclose, hnv] using hinv
          have hdec : remaining.val < fuel.val := by omega
          simpa [hmore, hget, hnext, hclose, hzero, hsub] using And.intro hkeep hdec
    · have ho : chars.val.length ≤ cursor.val := by simpa using hmore
      have hout : (Except.error Issue.unterminatedTiming : Except Issue Nat) =
          SlideChainModel.timingEndFuel initialFuel.val chars.val initialCursor.val := by
        simpa only [SlideChainModel.timingEndFuel_oob fuel.val chars.val cursor.val ho] using hinv
      simpa [hmore, decodeResult, decodeError] using hout
  · rfl

theorem timing_end_with_fuel_spec (chars : Slice Char) (cursor fuel : Usize) :
    slide_chain.timing_end_with_fuel chars cursor fuel
      ⦃ out => decodeResult UScalar.val out =
        SlideChainModel.timingEndFuel fuel.val chars.val cursor.val ⦄ := by
  simpa only [slide_chain.timing_end_with_fuel] using
    timing_end_with_fuel_loop_spec chars cursor fuel

theorem timing_end_spec (chars : Slice Char) (cursor : Usize) :
    slide_chain.timing_end chars cursor
      ⦃ out => decodeResult UScalar.val out =
        SlideChainModel.timingEnd chars.val cursor.val ⦄ := by
  apply exists_imp_spec
  by_cases hafter : cursor > Slice.len chars
  · have ho : chars.val.length < cursor.val := by simpa using hafter
    refine ⟨.Err .UnterminatedTiming, ?_, ?_⟩
    · simp [slide_chain.timing_end, hafter]
    · simp [decodeResult, decodeError, SlideChainModel.timingEnd, ho]
  · have hi : cursor.val ≤ chars.val.length := by simpa using hafter
    have hle : cursor.val ≤ (Slice.len chars).val := by simpa using hi
    obtain ⟨fuel, hsub, hf, _⟩ := spec_imp_exists
      (UScalar.sub_spec (x := Slice.len chars) (y := cursor) hle)
    have hfv : fuel.val = chars.val.length - cursor.val := by simpa using hf
    obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (timing_end_with_fuel_spec chars cursor fuel)
    refine ⟨out, ?_, ?_⟩
    · simp [slide_chain.timing_end, hafter, hsub, hrun]
    · simpa [SlideChainModel.timingEnd, Nat.not_lt.mpr hi, hfv] using hpost

theorem timing_end_equiv (chars : Slice Char) (cursor : Usize) :
    (do let out ← slide_chain.timing_end chars cursor
        ok (decodeResult UScalar.val out)) =
      ok (SlideChainModel.timingEnd chars.val cursor.val) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (timing_end_spec chars cursor)
  simp only [hrun, bind_tc_ok, hpost]

/-- Successful timing ranges remain within the original Slice, including
the closing bracket. This is derived from the current model theorem. -/
theorem timingEnd_bounds (chars : List Char) (cursor stop : Nat)
    (h : SlideChainModel.timingEnd chars cursor = .ok stop) :
    cursor < stop ∧ stop ≤ chars.length := by
  exact SlideChainModel.timingEnd_success chars cursor stop h

end Verification.SlideChainExtracted
