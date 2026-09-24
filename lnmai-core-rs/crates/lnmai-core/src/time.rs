//! Rational-time helpers and re-exported timeline types.
//!
//! Re-exports the verified `Duration`/`TimePoint` from `lnmai_core_verify` and
//! adds the rational-valued timing functions from `LnmaiCore/Time.lean`
//! (`quantizeRatMicros`, `bpmBeatMicrosRat`, `parseSecondsString?`, ...).
//!
//! Lean models the tick count as unbounded `ℤ`/`Rat`; here the tick count is an
//! `i64` and rationals are exact `i128`-backed [`Rat`]s. Game inputs stay well
//! inside these bounds.

pub use lnmai_core_verify::time::{Duration, TimePoint};

/// `TimePoint + Duration` (the trait impl lives in the other crate, so this is
/// a free function to respect the orphan rule).
pub fn time_point_add_duration(point: TimePoint, duration: Duration) -> TimePoint {
    TimePoint::from_micros(point.to_micros().wrapping_add(duration.to_micros()))
}

/// `TimePoint - Duration`.
pub fn time_point_sub_duration(point: TimePoint, duration: Duration) -> TimePoint {
    TimePoint::from_micros(point.to_micros().wrapping_sub(duration.to_micros()))
}

use crate::rat::Rat;

pub const MICROS_PER_MILLI: i128 = 1000;
pub const MICROS_PER_SECOND: i128 = 1_000_000;
pub const MICROS_PER_MINUTE: i128 = 60 * MICROS_PER_SECOND;

pub fn millis_to_micros(millis: i128) -> i128 {
    millis * MICROS_PER_MILLI
}

/// `roundDivAwayFromZero`: round `num / den` half away from zero. `den == 0`
/// yields `0`, and a positive denominator makes truncation match Lean's `Int.div`.
fn round_div_away_from_zero(num: i128, den: i128) -> i128 {
    if den == 0 {
        0
    } else {
        let den_pos = den.abs();
        let num_adj = if num < 0 { num - den_pos / 2 } else { num + den_pos / 2 };
        num_adj / den_pos
    }
}

/// `Time.quantizeRatMicros`.
pub fn quantize_rat_micros(value: Rat) -> i128 {
    round_div_away_from_zero(value.num(), value.den())
}

pub fn duration_from_rat_micros(value: Rat) -> Duration {
    Duration::from_micros(quantize_rat_micros(value) as i64)
}

pub fn point_from_rat_micros(value: Rat) -> TimePoint {
    TimePoint::from_micros(quantize_rat_micros(value) as i64)
}

/// `Time.bpmBeatMicrosRat`.
pub fn bpm_beat_micros_rat(bpm: Rat) -> Rat {
    if bpm.is_zero() {
        Rat::one()
    } else {
        Rat::from_int(MICROS_PER_MINUTE).div(bpm)
    }
}

/// `Time.bpmMeasureMicrosRat`.
pub fn bpm_measure_micros_rat(bpm: Rat) -> Rat {
    bpm_beat_micros_rat(bpm).mul(Rat::from_int(4))
}

/// `Time.durationFromSecondsRat`.
pub fn duration_from_seconds_rat(seconds: Rat) -> Duration {
    duration_from_rat_micros(seconds.mul(Rat::from_int(MICROS_PER_SECOND)))
}

/// `Time.pointFromSecondsRat`.
pub fn point_from_seconds_rat(seconds: Rat) -> TimePoint {
    point_from_rat_micros(seconds.mul(Rat::from_int(MICROS_PER_SECOND)))
}

/// `Time.fromMillis`.
pub fn from_millis(millis: i128) -> Duration {
    Duration::from_micros(millis_to_micros(millis) as i64)
}

fn pow10(n: usize) -> Option<i128> {
    let mut acc: i128 = 1;
    for _ in 0..n {
        acc = acc.checked_mul(10)?;
    }
    Some(acc)
}

/// `Time.quantizeSecondsString`: parse a decimal-seconds string to whole
/// microseconds, rounding fractional microseconds half away from zero.
pub fn quantize_seconds_string(text: &str) -> Option<i128> {
    let t = text.trim();
    if t.is_empty() {
        return None;
    }
    let negative = t.starts_with('-');
    let unsigned = if negative { t.strip_prefix('-').unwrap_or(t) } else { t };
    let parts: Vec<&str> = unsigned.split('.').collect();
    match parts.as_slice() {
        [whole] => {
            let n: i128 = whole.parse().ok()?;
            let micros = n.checked_mul(MICROS_PER_SECOND)?;
            Some(if negative { -micros } else { micros })
        }
        [whole, frac] => {
            let whole_nat: i128 = whole.parse().ok()?;
            if !frac.chars().all(|c| c.is_ascii_digit()) {
                return None;
            }
            let frac_nat: i128 = if frac.is_empty() { 0 } else { frac.parse().ok()? };
            let denom = pow10(frac.len())?;
            let frac_numerator = frac_nat.checked_mul(MICROS_PER_SECOND)?;
            let frac_micros = if denom == 0 {
                0
            } else {
                (frac_numerator + denom / 2) / denom
            };
            let micros = whole_nat.checked_mul(MICROS_PER_SECOND)?.checked_add(frac_micros)?;
            Some(if negative { -micros } else { micros })
        }
        _ => None,
    }
}

/// `Time.parseSecondsString?`.
pub fn parse_seconds_string(text: &str) -> Option<Duration> {
    quantize_seconds_string(text).map(|m| Duration::from_micros(m as i64))
}

/// `Time.parseSecondsPointString?`.
pub fn parse_seconds_point_string(text: &str) -> Option<TimePoint> {
    quantize_seconds_string(text).map(|m| TimePoint::from_micros(m as i64))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn beat_measure() {
        // 120 BPM → one beat = 500000µs, one measure = 2000000µs.
        assert_eq!(quantize_rat_micros(bpm_beat_micros_rat(Rat::from_int(120))), 500000);
        assert_eq!(quantize_rat_micros(bpm_measure_micros_rat(Rat::from_int(120))), 2000000);
    }

    #[test]
    fn bpm_zero() {
        assert_eq!(bpm_beat_micros_rat(Rat::zero()), Rat::one());
    }

    #[test]
    fn seconds_string() {
        assert_eq!(quantize_seconds_string("1.5"), Some(1_500_000));
        assert_eq!(quantize_seconds_string("-0.25"), Some(-250_000));
        assert_eq!(quantize_seconds_string("0.0000005"), Some(1)); // half away from zero
        assert_eq!(quantize_seconds_string("abc"), None);
        assert_eq!(quantize_seconds_string(""), None);
    }
}
