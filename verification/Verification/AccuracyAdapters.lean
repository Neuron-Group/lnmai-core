import LnmaiCore.Score

namespace Verification.AccuracyProof

/- Representation-parametric model of accuracy.rs. Correctness of the actual
   Natural/BigInt/Rat implementations is deliberately an explicit obligation.
   In particular, subtraction below is rational subtraction, not saturating
   natural subtraction; losses may exceed totals. -/

structure Operations (N R : Type) where
  natural : N → Nat
  rational : R → Rat
  maxOne : N → N
  isZero : N → Bool
  fromNat : N → R
  zero : R
  hundred : R
  add : R → R → R
  sub : R → R → R
  mul : R → R → R
  div : R → R → R

structure Correct {N R : Type} (ops : Operations N R) : Prop where
  maxOne : ∀ n, ops.natural (ops.maxOne n) = max (ops.natural n) 1
  isZero : ∀ n, ops.isZero n = (ops.natural n == 0)
  fromNat : ∀ n, ops.rational (ops.fromNat n) = (Int.ofNat (ops.natural n) : Rat)
  zero : ops.rational ops.zero = 0
  hundred : ops.rational ops.hundred = 100
  add : ∀ a b, ops.rational (ops.add a b) = ops.rational a + ops.rational b
  sub : ∀ a b, ops.rational (ops.sub a b) = ops.rational a - ops.rational b
  mul : ∀ a b, ops.rational (ops.mul a b) = ops.rational a * ops.rational b
  div : ∀ a b, ops.rational (ops.div a b) = ops.rational a / ops.rational b

structure Rates (R : Type) where
  classicPlus : R
  classicMinus : R
  dxMinus101 : R
  dxMinus100 : R
  dxPlus : R

def decode {N R : Type} (ops : Operations N R) (rates : Rates R) : LnmaiCore.Score.AccRates :=
  { classicAccPlus := ops.rational rates.classicPlus
    classicAccMinus := ops.rational rates.classicMinus
    dxAccMinus101 := ops.rational rates.dxMinus101
    dxAccMinus100 := ops.rational rates.dxMinus100
    dxAccPlus := ops.rational rates.dxPlus }

def compute {N R : Type} (ops : Operations N R)
    (totalBase currentBase lostBase totalExtra currentExtra lostExtra currentExtraClassic : N) :
    Rates R :=
  let tb := ops.fromNat totalBase
  let cb := ops.fromNat currentBase
  let earnedBase := ops.sub tb (ops.fromNat lostBase)
  let te := ops.fromNat (ops.maxOne totalExtra)
  let ce := ops.fromNat currentExtra
  let earnedExtra := ops.sub (ops.fromNat totalExtra) (ops.fromNat lostExtra)
  let cc := ops.fromNat currentExtraClassic
  let hundred := ops.hundred
  if ops.isZero totalBase then
    ⟨ops.zero, ops.zero, ops.zero, ops.zero, ops.zero⟩
  else
    { classicPlus := ops.mul (ops.div (ops.add cb cc) tb) hundred
      classicMinus := ops.mul (ops.div (ops.add earnedBase cc) tb) hundred
      dxMinus101 := ops.mul
        (ops.add (ops.div earnedBase tb) (ops.div earnedExtra (ops.mul te hundred))) hundred
      dxMinus100 := ops.mul
        (ops.add (ops.div earnedBase tb) (ops.div ce (ops.mul te hundred))) hundred
      dxPlus := ops.mul
        (ops.add (ops.div cb tb) (ops.div ce (ops.mul te hundred))) hundred }

theorem compute_acc_rates_equiv {N R : Type} (ops : Operations N R) (h : Correct ops)
    (totalBase currentBase lostBase totalExtra currentExtra lostExtra currentExtraClassic : N) :
    decode ops (compute ops totalBase currentBase lostBase totalExtra
      currentExtra lostExtra currentExtraClassic) =
    LnmaiCore.Score.computeAccRates (ops.natural totalBase) (ops.natural currentBase)
      (ops.natural lostBase) (ops.natural totalExtra) (ops.natural currentExtra)
      (ops.natural lostExtra) (ops.natural currentExtraClassic) := by
  simp only [compute, h.isZero, LnmaiCore.Score.computeAccRates]
  split <;> simp only [decode, h.zero, h.hundred, h.add, h.sub, h.mul, h.div,
    h.fromNat, h.maxOne, Nat.max_comm]

end Verification.AccuracyProof
