//! Exact Lean natural numbers. Subtraction and division are total.
use num_bigint::{BigInt, BigUint};
use num_traits::{ToPrimitive, Zero};
use std::{
    fmt,
    ops::{Add, AddAssign, Div, Mul, Rem, Sub, SubAssign},
    str::FromStr,
};

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Natural(BigUint);

impl Natural {
    /// Lean 4.30 `String.toNat?`: decimal digits with single underscores
    /// between digits. Signs, surrounding whitespace and empty strings fail.
    pub fn from_lean_str(s: &str) -> Option<Self> {
        let mut value = BigUint::zero();
        let mut last_was_digit = false;
        for byte in s.bytes() {
            if byte == b'_' {
                if !last_was_digit {
                    return None;
                }
                last_was_digit = false;
            } else if byte.is_ascii_digit() {
                value *= 10u8;
                value += byte - b'0';
                last_was_digit = true;
            } else {
                return None;
            }
        }
        last_was_digit.then_some(Self(value))
    }

    pub fn to_biguint(&self) -> BigUint {
        self.0.clone()
    }
    pub fn to_usize(&self) -> Option<usize> {
        self.0.to_usize()
    }
    /// Lean's native UInt64 conversion retains the low 64 bits.
    pub fn to_word(&self) -> u64 {
        self.0.iter_u64_digits().next().unwrap_or(0)
    }
    pub fn saturating_sub(&self, rhs: impl Into<Self>) -> Self {
        self - rhs
    }
    pub fn range(&self) -> impl Iterator<Item = Self> + '_ {
        std::iter::successors(Some(Self::default()), |n| Some(n + 1)).take_while(|n| n < self)
    }
}
impl From<BigUint> for Natural {
    fn from(n: BigUint) -> Self {
        Self(n)
    }
}
impl From<Natural> for BigUint {
    fn from(n: Natural) -> Self {
        n.0
    }
}
impl From<&Natural> for BigUint {
    fn from(n: &Natural) -> Self {
        n.0.clone()
    }
}
impl From<Natural> for BigInt {
    fn from(n: Natural) -> Self {
        n.0.into()
    }
}
impl From<&Natural> for BigInt {
    fn from(n: &Natural) -> Self {
        n.0.clone().into()
    }
}
impl From<&Natural> for Natural {
    fn from(n: &Natural) -> Self {
        n.clone()
    }
}
impl FromStr for Natural {
    type Err = num_bigint::ParseBigIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(Self)
    }
}
impl fmt::Display for Natural {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl fmt::Debug for Natural {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl serde::Serialize for Natural {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        self.to_string()
            .parse::<serde_json::Number>()
            .expect("natural JSON number")
            .serialize(s)
    }
}
macro_rules! primitive {
    ($($ty:ty),*) => { $(
        impl From<$ty> for Natural { fn from(n: $ty) -> Self { Self(BigUint::from(n)) } }
        impl PartialEq<$ty> for Natural { fn eq(&self, n: &$ty) -> bool { self == &Self::from(*n) } }
        impl PartialOrd<$ty> for Natural { fn partial_cmp(&self, n: &$ty) -> Option<std::cmp::Ordering> { Some(self.cmp(&Self::from(*n))) } }
        impl PartialEq<Natural> for $ty { fn eq(&self, n: &Natural) -> bool { n == self } }
        impl PartialOrd<Natural> for $ty { fn partial_cmp(&self, n: &Natural) -> Option<std::cmp::Ordering> { Some(Natural::from(*self).cmp(n)) } }
    )* };
}
primitive!(u8, u16, u32, u64, u128, usize);
// Unsuffixed nonnegative Rust literals are i32. Fallible external conversion
// uses the JSON decoder / FromStr instead.
impl From<i32> for Natural {
    fn from(n: i32) -> Self {
        Self(BigUint::from(
            u32::try_from(n).expect("nonnegative natural literal"),
        ))
    }
}
impl PartialEq<i32> for Natural {
    fn eq(&self, n: &i32) -> bool {
        *n >= 0 && self == &Self::from(*n)
    }
}
impl PartialOrd<i32> for Natural {
    fn partial_cmp(&self, n: &i32) -> Option<std::cmp::Ordering> {
        Some(if *n < 0 {
            std::cmp::Ordering::Greater
        } else {
            self.cmp(&Self::from(*n))
        })
    }
}
macro_rules! operation {
    ($trait:ident,$method:ident,$body:expr) => {
        impl<T: Into<Natural>> $trait<T> for &Natural {
            type Output = Natural;
            fn $method(self, rhs: T) -> Natural {
                let rhs = rhs.into();
                Natural(($body)(&self.0, &rhs.0))
            }
        }
        impl<T: Into<Natural>> $trait<T> for Natural {
            type Output = Natural;
            fn $method(self, rhs: T) -> Natural {
                (&self).$method(rhs)
            }
        }
    };
}
operation!(Add, add, |a: &BigUint, b: &BigUint| a + b);
operation!(Mul, mul, |a: &BigUint, b: &BigUint| a * b);
operation!(Sub, sub, |a: &BigUint, b: &BigUint| if a < b {
    BigUint::zero()
} else {
    a - b
});
operation!(Div, div, |a: &BigUint, b: &BigUint| if b.is_zero() {
    BigUint::zero()
} else {
    a / b
});
operation!(Rem, rem, |a: &BigUint, b: &BigUint| if b.is_zero() {
    a.clone()
} else {
    a % b
});
impl<T: Into<Natural>> AddAssign<T> for Natural {
    fn add_assign(&mut self, rhs: T) {
        *self = &*self + rhs;
    }
}
impl<T: Into<Natural>> SubAssign<T> for Natural {
    fn sub_assign(&mut self, rhs: T) {
        *self = &*self - rhs;
    }
}
impl std::iter::Sum for Natural {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| a + b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exact_total_arithmetic_and_native_conversion() {
        let n = Natural::from(BigUint::from(1u8) << 1024);
        assert_eq!((&n + 17).to_word(), 17);
        assert_eq!((&n + 17) - &n, 17);
        assert_eq!((&n - (&n + 1)), 0);
        assert_eq!((&n / 0), 0);
        assert_eq!((&n % 0), n);
        assert_eq!((&n * &n) / &n, n);
        assert_eq!(n.to_usize(), None);
    }
}
