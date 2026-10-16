import Verification.Generated
import LnmaiCore.Simai.Tokenize

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.TokenizeProof

theorem is_touch_area_char_equiv (c : Char) :
    tokenize.is_touch_area_char c = ok (LnmaiCore.Simai.isTouchAreaChar c) := by
  simp [tokenize.is_touch_area_char, LnmaiCore.Simai.isTouchAreaChar]
  split_ifs <;> simp_all

theorem is_slide_mark_char_equiv (c : Char) :
    tokenize.is_slide_mark_char c = ok (LnmaiCore.Simai.isSlideMarkChar c) := by
  simp [tokenize.is_slide_mark_char, LnmaiCore.Simai.isSlideMarkChar]
  split_ifs <;> simp_all

/-- Interpret the forward Rust buffers as the reference's reversed accumulators. -/
def resume (sep : Char) (chars : List Char) (depth : Nat)
    (current : List Char) (pieces : List (List Char)) : List String :=
  LnmaiCore.Simai.splitTopLevel.loop sep chars depth current.reverse
    (pieces.map String.ofList).reverse

theorem resume_nil (sep : Char) (depth : Nat)
    (current : List Char) (pieces : List (List Char)) :
    resume sep [] depth current pieces = pieces.map String.ofList ++ [String.ofList current] := by
  simp [resume, LnmaiCore.Simai.splitTopLevel.loop]

theorem resume_cons (sep c : Char) (rest : List Char) (depth : Nat)
    (current : List Char) (pieces : List (List Char)) :
    resume sep (c :: rest) depth current pieces =
      if c = '[' then resume sep rest (depth + 1) (current ++ [c]) pieces
      else if c = ']' then resume sep rest (depth - 1) (current ++ [c]) pieces
      else if c = sep ∧ depth = 0 then resume sep rest depth [] (pieces ++ [current])
      else resume sep rest depth (current ++ [c]) pieces := by
  by_cases hopen : c = '['
  · subst c
    simp [resume, LnmaiCore.Simai.splitTopLevel.loop]
  · by_cases hclose : c = ']'
    · subst c
      simp [resume, LnmaiCore.Simai.splitTopLevel.loop]
    · simp [resume, LnmaiCore.Simai.splitTopLevel.loop, hopen, hclose]

theorem saturating_sub_one_val (depth : Usize) :
    (core.num.Usize.saturating_sub depth 1#usize).val = depth.val - 1 := by
  change (BitVec.ofNat _ (max 0 (depth.val - (1#usize).val))).toNat = depth.val - 1
  simp only [UScalar.ofNatCore_val_eq, Nat.zero_max, BitVec.toNat_ofNat]
  exact Nat.mod_eq_of_lt (lt_of_le_of_lt (Nat.sub_le _ _) depth.bv.isLt)

/-- The scanning loop preserves the reference result and all vector/index bounds. -/
theorem split_top_level_loop_equiv (sep : Char) (chars : Slice Char)
    (hsize : chars.val.length < Usize.max) :
    tokenize.split_top_level_loop sep chars (alloc.vec.Vec.new _) 0#usize
      (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.1.val.length ≤ chars.val.length ∧
        (out.1.val.map (fun piece => String.ofList piece.val) ++
          [String.ofList out.2.val]) = resume sep chars.val 0 [] [] ⦄ := by
  unfold tokenize.split_top_level_loop
  apply loop.spec_decr_nat (fun p => chars.val.length - p.2.2.2.val)
    (fun p => p.2.2.2.val ≤ chars.val.length ∧
      p.2.1.val ≤ p.2.2.2.val ∧
      p.2.2.1.val.length ≤ p.2.2.2.val ∧
      p.1.val.length ≤ p.2.2.2.val ∧
      resume sep (chars.val.drop p.2.2.2.val) p.2.1.val p.2.2.1.val
        (p.1.val.map (·.val)) = resume sep chars.val 0 [] [])
  · rintro ⟨pieces, depth, current, index⟩ ⟨hi, hd, hc, hp, hinv⟩
    dsimp only at *
    unfold tokenize.split_top_level_loop.body
    dsimp only
    split
    · rename_i hmore
      have hindex : index.val < chars.val.length := by simpa using hmore
      step as ⟨c, hchar⟩
      rw [List.drop_eq_getElem_cons hindex, ← hchar, resume_cons] at hinv
      split
      · rename_i hopen
        simp [hopen] at hinv
        step as ⟨depth', hdepth'⟩
        step as ⟨current', hcurrent'⟩
        step as ⟨index', hindex'⟩
        simp_all
        scalar_tac
      · rename_i hopen
        split
        · rename_i hclose
          simp [hopen, hclose] at hinv
          simp only [lift, bind_tc_ok]
          step as ⟨current', hcurrent'⟩
          step as ⟨index', hindex'⟩
          simp_all [saturating_sub_one_val]
          scalar_tac
        · rename_i hclose
          simp [hopen, hclose] at hinv
          split
          · rename_i hsep
            split
            · rename_i hzero
              step as ⟨pieces', hpieces'⟩
              step as ⟨index', hindex'⟩
              simp_all
              scalar_tac
            · rename_i hzero
              step as ⟨current', hcurrent'⟩
              step as ⟨index', hindex'⟩
              simp_all
              scalar_tac
          · rename_i hsep
            step as ⟨current', hcurrent'⟩
            step as ⟨index', hindex'⟩
            simp_all
            scalar_tac
    · rename_i hdone
      have hempty : chars.val.drop index.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hdone)
      simp_all [resume_nil, List.map_map, Function.comp_def]
      scalar_tac
  · simp

/-- The extracted runtime splitter agrees with Lean for every character sequence
    whose final output fits in a Rust vector. UTF-8 string conversion is external. -/
theorem split_top_level_equiv (sep : Char) (chars : Slice Char)
    (hsize : chars.val.length < Usize.max) :
    tokenize.split_top_level sep chars
      ⦃ out => out.val.map (fun piece => String.ofList piece.val) =
        LnmaiCore.Simai.splitTopLevel sep (String.ofList chars.val) ⦄ := by
  unfold tokenize.split_top_level
  step with split_top_level_loop_equiv sep chars hsize as ⟨pieces, current, hlen, hout⟩
  step as ⟨result, hresult⟩
  simpa [hresult, resume, List.map_map, Function.comp_def,
    LnmaiCore.Simai.splitTopLevel] using hout

end Verification.TokenizeProof
