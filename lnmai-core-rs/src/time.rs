//! Time primitives for the maimai game.
//!
//! Provides TimePoint, Duration, and TimeTick types with microsecond precision.

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, Neg, Sub};

/// Exact machine-facing tick count in microseconds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TimeTick {
    pub val: i64,
}

impl TimeTick {
    pub fn new(val: i64) -> Self {
        Self { val }
    }

    pub fn zero() -> Self {
        Self { val: 0 }
    }

    pub fn to_int(&self) -> i64 {
        self.val
    }
}

impl PartialOrd for TimeTick {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimeTick {
    fn cmp(&self, other: &Self) -> Ordering {
        self.val.cmp(&other.val)
    }
}

impl Add for TimeTick {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self { val: self.val + other.val }
    }
}

impl Sub for TimeTick {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self { val: self.val - other.val }
    }
}

impl Neg for TimeTick {
    type Output = Self;
    fn neg(self) -> Self {
        Self { val: -self.val }
    }
}

impl fmt::Display for TimeTick {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

/// Duration on the local song timeline, measured in microsecond ticks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Duration {
    pub ticks: TimeTick,
}

impl Duration {
    pub fn from_tick(tick: TimeTick) -> Self {
        Self { ticks: tick }
    }

    pub fn from_int(value: i64) -> Self {
        Self { ticks: TimeTick::new(value) }
    }

    pub fn from_micros(micros: i64) -> Self {
        Self::from_int(micros)
    }

    pub fn zero() -> Self {
        Self::from_int(0)
    }

    pub fn to_tick(&self) -> TimeTick {
        self.ticks
    }

    pub fn to_int(&self) -> i64 {
        self.ticks.val
    }

    pub fn to_micros(&self) -> i64 {
        self.to_int()
    }

    pub fn scale_nat(&self, factor: u32) -> Self {
        Self::from_int(self.to_int() * factor as i64)
    }

    pub fn div_nat(&self, divisor: u32) -> Self {
        if divisor == 0 {
            Self::zero()
        } else {
            Self::from_int(self.to_int() / divisor as i64)
        }
    }

    pub fn abs(&self) -> Self {
        if self.to_int() < 0 {
            Self::from_int(-self.to_int())
        } else {
            *self
        }
    }
}

impl PartialOrd for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Duration {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ticks.cmp(&other.ticks)
    }
}

impl Add for Duration {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self::from_int(self.to_int() + other.to_int())
    }
}

impl Sub for Duration {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self::from_int(self.to_int() - other.to_int())
    }
}

impl Neg for Duration {
    type Output = Self;
    fn neg(self) -> Self {
        Self::from_int(-self.to_int())
    }
}

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}μs", self.to_micros())
    }
}

/// Point on the local song timeline, measured in microsecond ticks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TimePoint {
    pub ticks: TimeTick,
}

impl TimePoint {
    pub fn from_tick(tick: TimeTick) -> Self {
        Self { ticks: tick }
    }

    pub fn from_int(value: i64) -> Self {
        Self { ticks: TimeTick::new(value) }
    }

    pub fn from_micros(micros: i64) -> Self {
        Self::from_int(micros)
    }

    pub fn zero() -> Self {
        Self::from_int(0)
    }

    pub fn to_tick(&self) -> TimeTick {
        self.ticks
    }

    pub fn to_int(&self) -> i64 {
        self.ticks.val
    }

    pub fn to_micros(&self) -> i64 {
        self.to_int()
    }
}

impl PartialOrd for TimePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ticks.cmp(&other.ticks)
    }
}

impl Add<Duration> for TimePoint {
    type Output = Self;
    fn add(self, duration: Duration) -> Self {
        Self::from_int(self.to_int() + duration.to_int())
    }
}

impl Sub<Duration> for TimePoint {
    type Output = Self;
    fn sub(self, duration: Duration) -> Self {
        Self::from_int(self.to_int() - duration.to_int())
    }
}

impl Sub<TimePoint> for TimePoint {
    type Output = Duration;
    fn sub(self, other: TimePoint) -> Duration {
        Duration::from_int(self.to_int() - other.to_int())
    }
}

impl fmt::Display for TimePoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}μs", self.to_micros())
    }
}

/// Time constants
pub const MICROS_PER_MILLI: i64 = 1000;
pub const MICROS_PER_SECOND: i64 = 1_000_000;
pub const MICROS_PER_MINUTE: i64 = 60 * MICROS_PER_SECOND;

/// Convert milliseconds to microseconds
pub fn millis_to_micros(millis: i64) -> i64 {
    millis * MICROS_PER_MILLI
}

/// Round division away from zero
fn round_div_away_from_zero(num: i64, den: i64) -> i64 {
    if den == 0 {
        0
    } else {
        let den_abs = den.abs();
        let num_adj = if num < 0 {
            num - den_abs / 2
        } else {
            num + den_abs / 2
        };
        num_adj / den_abs
    }
}

/// Quantize a floating-point value to microseconds
pub fn quantize_rat_micros(value: f64) -> i64 {
    value.round() as i64
}

/// Create a Duration from a floating-point microsecond value
pub fn duration_from_rat_micros(value: f64) -> Duration {
    Duration::from_micros(quantize_rat_micros(value))
}

/// Create a TimePoint from a floating-point microsecond value
pub fn point_from_rat_micros(value: f64) -> TimePoint {
    TimePoint::from_micros(quantize_rat_micros(value))
}

/// Calculate BPM beat duration in microseconds
pub fn bpm_beat_micros_rat(bpm: f64) -> f64 {
    if bpm == 0.0 {
        1.0
    } else {
        MICROS_PER_MINUTE as f64 / bpm
    }
}

/// Calculate BPM measure duration in microseconds
pub fn bpm_measure_micros_rat(bpm: f64) -> f64 {
    bpm_beat_micros_rat(bpm) * 4.0
}

/// Create a Duration from seconds (floating-point)
pub fn duration_from_seconds_rat(seconds: f64) -> Duration {
    duration_from_rat_micros(seconds * MICROS_PER_SECOND as f64)
}

/// Create a TimePoint from seconds (floating-point)
pub fn point_from_seconds_rat(seconds: f64) -> TimePoint {
    point_from_rat_micros(seconds * MICROS_PER_SECOND as f64)
}

/// Create a Duration from milliseconds
pub fn from_millis(millis: i64) -> Duration {
    Duration::from_micros(millis_to_micros(millis))
}

/// Create a TimePoint from milliseconds
pub fn point_from_millis(millis: i64) -> TimePoint {
    TimePoint::from_micros(millis_to_micros(millis))
}

/// Quantize a decimal string in seconds into whole microseconds
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
                let micros = n * MICROS_PER_SECOND;
                if negative { -micros } else { micros }
            })
        }
        [whole, frac] => {
            whole.parse::<i64>().ok().and_then(|whole_n| {
                if frac.chars().all(|c| c.is_ascii_digit()) {
                    let frac_digits = frac.len();
                    let frac_n = frac.parse::<i64>().unwrap_or(0);
                    let frac_numerator = frac_n * MICROS_PER_SECOND;
                    let frac_denominator = 10_i64.pow(frac_digits as u32);
                    let frac_micros = if frac_denominator == 0 {
                        0
                    } else {
                        let half_den = frac_denominator / 2;
                        (frac_numerator + half_den) / frac_denominator
                    };
                    let micros = whole_n * MICROS_PER_SECOND + frac_micros;
                    Some(if negative { -micros } else { micros })
                } else {
                    None
                }
            })
        }
        _ => None,
    }
}

/// Parse a seconds string into a Duration
pub fn parse_seconds_string(text: &str) -> Option<Duration> {
    quantize_seconds_string(text).map(Duration::from_micros)
}

/// Parse a seconds string into a TimePoint
pub fn parse_seconds_point_string(text: &str) -> Option<TimePoint> {
    quantize_seconds_string(text).map(TimePoint::from_micros)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_duration_injective() {
        let a = Duration::from_micros(100);
        let b = Duration::from_micros(100);
        assert_eq!(a, b);

        let c = Duration::from_micros(200);
        assert_ne!(a, c);
    }

    #[test]
    fn test_duration_order_preserving() {
        let a = Duration::from_micros(100);
        let b = Duration::from_micros(200);
        assert!(a < b);
        assert!(a.to_micros() < b.to_micros());
    }

    #[test]
    fn test_timepoint_injective() {
        let a = TimePoint::from_micros(100);
        let b = TimePoint::from_micros(100);
        assert_eq!(a, b);

        let c = TimePoint::from_micros(200);
        assert_ne!(a, c);
    }

    #[test]
    fn test_timepoint_order_preserving() {
        let a = TimePoint::from_micros(100);
        let b = TimePoint::from_micros(200);
        assert!(a < b);
        assert!(a.to_micros() < b.to_micros());
    }

    #[test]
    fn test_duration_arithmetic() {
        let a = Duration::from_micros(100);
        let b = Duration::from_micros(200);
        assert_eq!((a + b).to_micros(), 300);
        assert_eq!((b - a).to_micros(), 100);
        assert_eq!((-a).to_micros(), -100);
    }

    #[test]
    fn test_timepoint_arithmetic() {
        let p = TimePoint::from_micros(1000);
        let d = Duration::from_micros(200);
        assert_eq!((p + d).to_micros(), 1200);
        assert_eq!((p - d).to_micros(), 800);
        assert_eq!((p - TimePoint::from_micros(900)).to_micros(), 100);
    }

    #[test]
    fn test_quantize_seconds_string() {
        assert_eq!(quantize_seconds_string("1"), Some(1_000_000));
        assert_eq!(quantize_seconds_string("1.5"), Some(1_500_000));
        assert_eq!(quantize_seconds_string("-1.5"), Some(-1_500_000));
        assert_eq!(quantize_seconds_string("0.001"), Some(1000));
        assert_eq!(quantize_seconds_string(""), None);
        assert_eq!(quantize_seconds_string("abc"), None);
    }
}
