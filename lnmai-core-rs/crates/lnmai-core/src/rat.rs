//! Arbitrary-precision rational arithmetic matching Lean's normalized Rat.
//! A zero denominator denotes zero, as in Lean's total division operation.
use core::cmp::Ordering;
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::borrow::Borrow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rat {
    num: BigInt,
    den: BigInt,
}

fn gcd(mut a: BigInt, mut b: BigInt) -> BigInt {
    a = a.abs();
    b = b.abs();
    while !b.is_zero() {
        let t = a % &b;
        a = b;
        b = t;
    }
    if a.is_zero() {
        BigInt::one()
    } else {
        a
    }
}

impl Rat {
    pub fn new(num: impl Into<BigInt>, den: impl Into<BigInt>) -> Self {
        let num = num.into();
        let den = den.into();
        if den.is_zero() {
            return Self::zero();
        }
        let (num, den) = if den.is_negative() {
            (-num, -den)
        } else {
            (num, den)
        };
        let g = gcd(num.clone(), den.clone());
        Self {
            num: num / &g,
            den: den / g,
        }
    }
    pub fn from_int(n: impl Into<BigInt>) -> Self {
        Self {
            num: n.into(),
            den: BigInt::one(),
        }
    }
    pub fn zero() -> Self {
        Self::from_int(0)
    }
    pub fn one() -> Self {
        Self::from_int(1)
    }
    pub fn num(&self) -> &BigInt {
        &self.num
    }
    pub fn den(&self) -> &BigInt {
        &self.den
    }
    pub fn is_zero(&self) -> bool {
        self.num.is_zero()
    }
    pub fn add(&self, other: impl Borrow<Rat>) -> Self {
        let other = other.borrow();
        Self::new(
            &self.num * &other.den + &other.num * &self.den,
            &self.den * &other.den,
        )
    }
    pub fn sub(&self, other: impl Borrow<Rat>) -> Self {
        let other = other.borrow();
        Self::new(
            &self.num * &other.den - &other.num * &self.den,
            &self.den * &other.den,
        )
    }
    pub fn mul(&self, other: impl Borrow<Rat>) -> Self {
        let other = other.borrow();
        Self::new(&self.num * &other.num, &self.den * &other.den)
    }
    pub fn div(&self, other: impl Borrow<Rat>) -> Self {
        let other = other.borrow();
        Self::new(&self.num * &other.den, &self.den * &other.num)
    }
    /// Lean's decimal renderer uses at most twelve fractional digits.
    pub fn to_decimal_string(&self) -> String {
        let num_abs = self.num.abs();
        let whole = &num_abs / &self.den;
        let mut rem = num_abs % &self.den;
        let mut frac = Vec::new();
        for _ in 0..12 {
            if rem.is_zero() {
                break;
            }
            let scaled = rem * 10u8;
            let digit = (&scaled / &self.den).to_u8().unwrap();
            rem = scaled % &self.den;
            frac.push(b'0' + digit);
        }
        while frac.last() == Some(&b'0') {
            frac.pop();
        }
        let sign = if self.num.is_negative() { "-" } else { "" };
        if frac.is_empty() {
            format!("{sign}{whole}")
        } else {
            format!("{sign}{whole}.{}", String::from_utf8(frac).unwrap())
        }
    }
}
impl Ord for Rat {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.num * &other.den).cmp(&(&other.num * &self.den))
    }
}
impl PartialOrd for Rat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn normalizes() {
        assert_eq!(Rat::new(2, 4), Rat::new(1, 2));
        assert_eq!(Rat::new(-2, 4), Rat::new(-1, 2));
        assert_eq!(Rat::new(2, -4), Rat::new(-1, 2));
        assert_eq!(Rat::new(5, 0), Rat::zero());
        assert_eq!(Rat::new(i128::MIN, i128::MIN), Rat::one());
    }
    #[test]
    fn arithmetic() {
        let a = Rat::new(1, 3);
        let b = Rat::new(1, 6);
        assert_eq!(a.add(&b), Rat::new(1, 2));
        assert_eq!(a.sub(&b), Rat::new(1, 6));
        assert_eq!(a.mul(&b), Rat::new(1, 18));
        assert_eq!(a.div(&b), Rat::from_int(2));
        let huge = BigInt::one() << 1024;
        let a = Rat::new(&huge + 1, &huge + 2);
        let b = Rat::new(&huge + 2, &huge + 1);
        assert_eq!(a.mul(&b), Rat::one());
        assert!(a < b);
    }
    #[test]
    fn decimal_string() {
        assert_eq!(Rat::new(1, 2).to_decimal_string(), "0.5");
        assert_eq!(Rat::from_int(3).to_decimal_string(), "3");
        assert_eq!(Rat::new(-1, 4).to_decimal_string(), "-0.25");
        assert_eq!(
            Rat::new(i128::MIN, 1).to_decimal_string(),
            i128::MIN.to_string()
        );
    }
}
