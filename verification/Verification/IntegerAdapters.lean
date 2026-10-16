import Verification.Bridge

namespace Verification.IntegerProof

/- Mathematical model of Integer::div_euclid. Unlike the bounded kernel's
   div_nat, its divisor may be negative or zero. Rust BigInt's decoded
   division/remainder must be connected to Int.tdiv/Int.tmod separately. -/

def divEuclid (a b : Int) : Int :=
  if b = 0 then 0 else
    let q := a.tdiv b
    let r := a.tmod b
    if r < 0 then
      if b < 0 then q + 1 else q - 1
    else q

theorem div_euclid_equiv (a b : Int) : divEuclid a b = a / b := by
  unfold divEuclid
  by_cases hb : b = 0
  · simp [hb]
  rw [if_neg hb]
  have hnonneg := Int.emod_nonneg a hb
  have hlt := Int.emod_lt a hb
  by_cases h : 0 ≤ a ∨ b ∣ a
  · simp only [Int.tdiv_eq_ediv, Int.tmod_eq_emod, if_pos h,
      Int.cast_ofNat_Int, sub_zero, add_zero, if_neg (by omega : ¬a % b < 0)]
  · simp only [Int.tdiv_eq_ediv, Int.tmod_eq_emod, if_neg h, Int.cast_ofNat_Int]
    rw [if_pos (by omega : a % b - (b.natAbs : Int) < 0)]
    by_cases hn : b < 0
    · simp only [if_pos hn, Int.sign_eq_neg_one_of_neg hn]
      omega
    · have hp : 0 < b := by omega
      simp only [if_neg hn, Int.sign_eq_one_of_pos hp]
      omega

end Verification.IntegerProof
