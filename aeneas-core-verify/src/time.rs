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
}
