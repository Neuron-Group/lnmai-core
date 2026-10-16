//! Exact signed integers with inline storage for ordinary timeline ticks.
//! Large values are normalized back to inline storage whenever they fit.

use num_bigint::{BigInt, Sign};
use num_traits::{Signed, ToPrimitive, Zero};
use std::{
    cmp::Ordering,
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

#[derive(Clone, PartialEq, Eq, Hash)]
enum Repr {
    Small(i64),
    Large(BigInt),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Integer(Repr);

impl Integer {
    pub const fn small(value: i64) -> Self {
        Self(Repr::Small(value))
    }
    pub fn to_bigint(&self) -> BigInt {
        match &self.0 {
            Repr::Small(n) => BigInt::from(*n),
            Repr::Large(n) => n.clone(),
        }
    }
    pub fn to_i64(&self) -> Option<i64> {
        match &self.0 {
            Repr::Small(n) => Some(*n),
            Repr::Large(_) => None,
        }
    }
    pub fn abs(&self) -> Self {
        Self::from(self.to_bigint().abs())
    }
    /// Lean Int division is Euclidean and total, including division by zero.
    pub fn div_euclid(&self, divisor: &Self) -> Self {
        let (a, b) = (self.to_bigint(), divisor.to_bigint());
        if b.is_zero() {
            return Self::small(0);
        }
        let q = &a / &b;
        let r = &a % &b;
        Self::from(if r.is_negative() {
            if b.is_negative() {
                q + 1
            } else {
                q - 1
            }
        } else {
            q
        })
    }
    /// Native ABI conversion agrees with Lean UInt64.ofInt (modulo 2^64).
    pub fn to_word(&self) -> u64 {
        match &self.0 {
            Repr::Small(n) => *n as u64,
            Repr::Large(n) => {
                let magnitude = n.iter_u64_digits().next().unwrap_or(0);
                if n.sign() == Sign::Minus {
                    magnitude.wrapping_neg()
                } else {
                    magnitude
                }
            }
        }
    }
}

impl Default for Integer {
    fn default() -> Self {
        Self::small(0)
    }
}
impl From<BigInt> for Integer {
    fn from(n: BigInt) -> Self {
        match n.to_i64() {
            Some(s) => Self::small(s),
            None => Self(Repr::Large(n)),
        }
    }
}
impl From<&Integer> for Integer {
    fn from(n: &Integer) -> Self {
        n.clone()
    }
}
impl From<Integer> for BigInt {
    fn from(n: Integer) -> Self {
        n.to_bigint()
    }
}
impl From<&Integer> for BigInt {
    fn from(n: &Integer) -> Self {
        n.to_bigint()
    }
}
impl Ord for Integer {
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.0, &other.0) {
            (Repr::Small(a), Repr::Small(b)) => a.cmp(b),
            _ => self.to_bigint().cmp(&other.to_bigint()),
        }
    }
}
impl PartialOrd for Integer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Repr::Small(n) => n.fmt(f),
            Repr::Large(n) => n.fmt(f),
        }
    }
}
impl fmt::Debug for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl serde::Serialize for Integer {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let n: serde_json::Number = self.to_string().parse().expect("integer JSON number");
        n.serialize(serializer)
    }
}

macro_rules! primitive {
    ($($ty:ty),*) => { $(
        impl From<$ty> for Integer { fn from(n: $ty) -> Self { Self::from(BigInt::from(n)) } }
        impl PartialEq<$ty> for Integer { fn eq(&self,n: &$ty) -> bool { self == &Self::from(*n) } }
        impl PartialOrd<$ty> for Integer { fn partial_cmp(&self,n: &$ty) -> Option<Ordering> { Some(self.cmp(&Self::from(*n))) } }
        impl PartialEq<Integer> for $ty { fn eq(&self,n: &Integer) -> bool { n == self } }
        impl PartialOrd<Integer> for $ty { fn partial_cmp(&self,n: &Integer) -> Option<Ordering> { Some(Integer::from(*self).cmp(n)) } }
    )* };
}
primitive!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);

macro_rules! arithmetic {
    ($trait:ident,$method:ident,$checked:ident,$op:tt) => {
        impl<T: Into<Integer>> $trait<T> for Integer {
            type Output = Integer;
            fn $method(self,rhs: T) -> Integer { (&self).$method(rhs) }
        }
        impl<T: Into<Integer>> $trait<T> for &Integer {
            type Output = Integer;
            fn $method(self,rhs: T) -> Integer {
                let rhs = rhs.into();
                if let (Repr::Small(a),Repr::Small(b)) = (&self.0,&rhs.0) {
                    if let Some(n) = a.$checked(*b) { return Integer::small(n); }
                }
                Integer::from(self.to_bigint() $op rhs.to_bigint())
            }
        }
    };
}
arithmetic!(Add,add,checked_add,+);
arithmetic!(Sub,sub,checked_sub,-);
arithmetic!(Mul,mul,checked_mul,*);
impl<T: Into<Integer>> Div<T> for Integer {
    type Output = Integer;
    fn div(self, rhs: T) -> Integer {
        self.div_euclid(&rhs.into())
    }
}
impl Neg for Integer {
    type Output = Integer;
    fn neg(self) -> Integer {
        Integer::from(-self.to_bigint())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn promotes_and_normalizes_without_changing_order_or_value() {
        let large = Integer::from(i64::MAX) + 1;
        assert!(large > i64::MAX);
        assert_eq!(&large - 1, Integer::from(i64::MAX));
        assert_eq!((&large * &large).to_bigint(), BigInt::from(1u8) << 126);
        assert_eq!(Integer::from(i64::MIN).abs(), large);
        assert_eq!(large.to_word(), 1u64 << 63);
        assert_eq!((-large).to_word(), 1u64 << 63);
    }
    #[test]
    fn euclidean_division_is_total() {
        for (a, b, q) in [(-5, 3, -2), (-5, -3, 2), (5, -3, -1), (5, 0, 0)] {
            assert_eq!(Integer::from(a).div_euclid(&Integer::from(b)), q);
        }
    }
}
