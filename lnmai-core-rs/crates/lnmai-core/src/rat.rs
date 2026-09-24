//! Exact rational numbers.
//!
//! Mirrors Lean's `Rat` (a normalized `num / den` with `den > 0`). The Simai
//! timing pipeline needs exact arithmetic, so `f64` is not acceptable here.
//! Values are `i128`-backed; game inputs stay far inside that range.

use core::cmp::Ordering;

/// A normalized rational number with a positive denominator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rat {
    pub num: i128,
    pub den: i128,
}

fn gcd(mut a: i128, mut b: i128) -> i128 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = a % b;
        a = b;
        b = t;
    }
    if a == 0 { 1 } else { a }
}

impl Rat {
    /// Build `num / den`, normalized the way Lean's `Rat` normalizes.
    pub fn new(num: i128, den: i128) -> Rat {
        if den == 0 {
            return Rat { num: 0, den: 1 };
        }
        let (num, den) = if den < 0 { (-num, -den) } else { (num, den) };
        let g = gcd(num, den);
        Rat { num: num / g, den: den / g }
    }

    pub const fn from_int(n: i128) -> Rat {
        Rat { num: n, den: 1 }
    }

    pub const fn zero() -> Rat {
        Rat { num: 0, den: 1 }
    }

    pub const fn one() -> Rat {
        Rat { num: 1, den: 1 }
    }

    pub const fn num(self) -> i128 {
        self.num
    }

    pub const fn den(self) -> i128 {
        self.den
    }

    pub fn is_zero(self) -> bool {
        self.num == 0
    }

    pub fn add(self, other: Rat) -> Rat {
        Rat::new(self.num * other.den + other.num * self.den, self.den * other.den)
    }

    pub fn sub(self, other: Rat) -> Rat {
        Rat::new(self.num * other.den - other.num * self.den, self.den * other.den)
    }

    pub fn mul(self, other: Rat) -> Rat {
        Rat::new(self.num * other.num, self.den * other.den)
    }

    pub fn div(self, other: Rat) -> Rat {
        Rat::new(self.num * other.den, self.den * other.num)
    }

    pub fn cmp(self, other: Rat) -> Ordering {
        (self.num * other.den).cmp(&(other.num * self.den))
    }

    pub fn lt(self, other: Rat) -> bool {
        self.cmp(other) == Ordering::Less
    }

    /// Lean's `ratToDecimalString`: whole part plus up to 12 fractional digits,
    /// with trailing zeros trimmed.
    pub fn to_decimal_string(self) -> String {
        let negative = self.num < 0;
        let num_abs = self.num.unsigned_abs() as i128;
        let den = self.den;
        let whole = num_abs / den;
        let mut rem = num_abs % den;
        let mut frac: Vec<u8> = Vec::new();
        // 12 significant digit steps, matching the Lean fuel.
        for _ in 0..12 {
            if rem == 0 {
                break;
            }
            let scaled = rem * 10;
            let digit = scaled / den;
            rem = scaled % den;
            frac.push(b'0' + digit as u8);
        }
        while frac.last() == Some(&b'0') {
            frac.pop();
        }
        let sign = if negative { "-" } else { "" };
        if frac.is_empty() {
            format!("{}{}", sign, whole)
        } else {
            format!("{}{}.{}", sign, whole, String::from_utf8(frac).unwrap())
        }
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
    }

    #[test]
    fn arithmetic() {
        let a = Rat::new(1, 3);
        let b = Rat::new(1, 6);
        assert_eq!(a.add(b), Rat::new(1, 2));
        assert_eq!(a.sub(b), Rat::new(1, 6));
        assert_eq!(a.mul(b), Rat::new(1, 18));
        assert_eq!(a.div(b), Rat::new(2, 1));
    }

    #[test]
    fn decimal_string() {
        assert_eq!(Rat::new(1, 2).to_decimal_string(), "0.5");
        assert_eq!(Rat::new(3, 1).to_decimal_string(), "3");
        assert_eq!(Rat::new(-1, 4).to_decimal_string(), "-0.25");
    }
}
