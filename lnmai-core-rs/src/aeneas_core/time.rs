//! Aeneas-friendly time module
//!
//! Design: Simple i64-based time types, no complex operations

/// Duration in microseconds
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration {
    pub micros: i64,
}

impl Duration {
    pub fn from_micros(micros: i64) -> Self {
        Self { micros }
    }

    pub fn to_micros(&self) -> i64 {
        self.micros
    }

    pub fn zero() -> Self {
        Self { micros: 0 }
    }

    pub fn abs(&self) -> Self {
        Self { micros: self.micros.abs() }
    }

    pub fn scale_nat(&self, factor: u32) -> Self {
        Self { micros: self.micros * factor as i64 }
    }

    pub fn div_nat(&self, divisor: u32) -> Self {
        if divisor == 0 {
            Self::zero()
        } else {
            Self { micros: self.micros / divisor as i64 }
        }
    }
}

impl std::ops::Add for Duration {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self { micros: self.micros + other.micros }
    }
}

impl std::ops::Sub for Duration {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self { micros: self.micros - other.micros }
    }
}

impl std::ops::Neg for Duration {
    type Output = Self;
    fn neg(self) -> Self {
        Self { micros: -self.micros }
    }
}

/// Time point in microseconds
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TimePoint {
    pub micros: i64,
}

impl TimePoint {
    pub fn from_micros(micros: i64) -> Self {
        Self { micros }
    }

    pub fn to_micros(&self) -> i64 {
        self.micros
    }

    pub fn zero() -> Self {
        Self { micros: 0 }
    }
}

impl std::ops::Add<Duration> for TimePoint {
    type Output = Self;
    fn add(self, duration: Duration) -> Self {
        Self { micros: self.micros + duration.micros }
    }
}

impl std::ops::Sub<Duration> for TimePoint {
    type Output = Self;
    fn sub(self, duration: Duration) -> Self {
        Self { micros: self.micros - duration.micros }
    }
}

impl std::ops::Sub for TimePoint {
    type Output = Duration;
    fn sub(self, other: Self) -> Duration {
        Duration { micros: self.micros - other.micros }
    }
}

/// Parse seconds string to microseconds
pub fn quantize_seconds_string(text: &str) -> Option<i64> {
    let t = text.trim();
    if t.is_empty() {
        return None;
    }

    let negative = t.starts_with('-');
    let unsigned = if negative { &t[1..] } else { t };

    let parts: Vec<&str> = unsigned.split('.').collect();
    match parts.as_slice() {
        [whole] => {
            whole.parse::<i64>().ok().map(|n| {
                let micros = n * 1_000_000;
                if negative { -micros } else { micros }
            })
        }
        [whole, frac] => {
            whole.parse::<i64>().ok().and_then(|whole_n| {
                if frac.chars().all(|c| c.is_ascii_digit()) {
                    let frac_digits = frac.len();
                    let frac_n = frac.parse::<i64>().unwrap_or(0);
                    let frac_numerator = frac_n * 1_000_000;
                    let frac_denominator = 10_i64.pow(frac_digits as u32);
                    let frac_micros = if frac_denominator == 0 {
                        0
                    } else {
                        let half_den = frac_denominator / 2;
                        (frac_numerator + half_den) / frac_denominator
                    };
                    let micros = whole_n * 1_000_000 + frac_micros;
                    Some(if negative { -micros } else { micros })
                } else {
                    None
                }
            })
        }
        _ => None,
    }
}

/// Parse seconds string to Duration
pub fn parse_seconds_string(text: &str) -> Option<Duration> {
    quantize_seconds_string(text).map(Duration::from_micros)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_duration_arithmetic() {
        let a = Duration::from_micros(100);
        let b = Duration::from_micros(200);
        assert_eq!((a + b).to_micros(), 300);
        assert_eq!((b - a).to_micros(), 100);
    }

    #[test]
    fn test_timepoint_arithmetic() {
        let p = TimePoint::from_micros(1000);
        let d = Duration::from_micros(200);
        assert_eq!((p + d).to_micros(), 1200);
        assert_eq!((p - d).to_micros(), 800);
    }

    #[test]
    fn test_quantize_seconds_string() {
        assert_eq!(quantize_seconds_string("1"), Some(1_000_000));
        assert_eq!(quantize_seconds_string("1.5"), Some(1_500_000));
        assert_eq!(quantize_seconds_string("-1.5"), Some(-1_500_000));
    }
}
