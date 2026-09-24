//! Simai timing-string parsing.
//!
//! Mirrors `LnmaiCore/Simai/Timing.lean`.

use crate::rat::Rat;
use crate::time::{self, Duration};

/// `Timing.trim`.
pub fn trim(s: &str) -> String {
    s.trim().to_string()
}

/// `Timing.parseNatString?`.
pub fn parse_nat_string(s: &str) -> Option<usize> {
    let t = trim(s);
    if t.is_empty() { None } else { t.parse::<usize>().ok() }
}

/// `Timing.parseRatString?`.
pub fn parse_rat_string(s: &str) -> Option<Rat> {
    let t = trim(s);
    if t.is_empty() {
        return None;
    }
    let negative = t.starts_with('-');
    let unsigned = if negative { t.strip_prefix('-').unwrap_or(&t) } else { t.as_str() };
    let parts: Vec<&str> = unsigned.split('.').collect();
    match parts.as_slice() {
        [whole] => {
            let n: i128 = whole.parse().ok()?;
            let value = Rat::from_int(n);
            Some(if negative { Rat::zero().sub(value) } else { value })
        }
        [whole, frac] => {
            let whole_nat: i128 = whole.parse().ok()?;
            if !frac.chars().all(|c| c.is_ascii_digit()) {
                return None;
            }
            let frac_nat: i128 = if frac.is_empty() { 0 } else { frac.parse().ok()? };
            let denom = pow10(frac.len())?;
            let value = Rat::from_int(whole_nat).add(Rat::from_int(frac_nat).div(Rat::from_int(denom)));
            Some(if negative { Rat::zero().sub(value) } else { value })
        }
        _ => None,
    }
}

fn pow10(n: usize) -> Option<i128> {
    let mut acc: i128 = 1;
    for _ in 0..n {
        acc = acc.checked_mul(10)?;
    }
    Some(acc)
}

/// `Timing.parseRatDef`.
pub fn parse_rat_def(s: &str, fallback: Rat) -> Rat {
    parse_rat_string(s).unwrap_or(fallback)
}

/// `Timing.parseNatDef`.
pub fn parse_nat_def(s: &str, fallback: usize) -> usize {
    parse_nat_string(s).unwrap_or(fallback)
}

/// `Timing.parseDurationString?`.
pub fn parse_duration_string(s: &str) -> Option<Duration> {
    time::parse_seconds_string(s)
}

/// `Timing.parseSecondsRatString?`.
pub fn parse_seconds_rat_string(s: &str) -> Option<Rat> {
    parse_rat_string(s)
}

/// `Timing.measureDurSec`.
pub fn measure_dur_sec(bpm: Rat) -> Duration {
    time::duration_from_rat_micros(time::bpm_measure_micros_rat(bpm))
}

/// `Timing.beatSec`.
pub fn beat_sec(bpm: Rat) -> Duration {
    time::duration_from_rat_micros(time::bpm_beat_micros_rat(bpm))
}

/// `Timing.extractBracketContents`.
pub fn extract_bracket_contents(token: &str) -> Vec<String> {
    let mut acc: Vec<String> = Vec::new();
    let mut inside = false;
    let mut current = String::new();
    for c in token.chars() {
        match c {
            '[' => {
                if inside {
                    current.push('[');
                } else {
                    inside = true;
                    current.clear();
                }
            }
            ']' => {
                if inside {
                    inside = false;
                    acc.push(std::mem::take(&mut current));
                }
            }
            _ => {
                if inside {
                    current.push(c);
                }
            }
        }
    }
    acc
}

/// `Timing.splitHash2`.
pub fn split_hash2(s: &str) -> Option<(String, String, String)> {
    let parts: Vec<&str> = s.split('#').collect();
    match parts.as_slice() {
        [a, b, c] => Some((a.to_string(), b.to_string(), c.to_string())),
        _ => None,
    }
}

/// `Timing.splitHash1`.
pub fn split_hash1(s: &str) -> Option<(String, String)> {
    let parts: Vec<&str> = s.split('#').collect();
    match parts.as_slice() {
        [a, b] => Some((a.to_string(), b.to_string())),
        _ => None,
    }
}

/// `Timing.parseNdDuration`.
pub fn parse_nd_duration(bpm: Rat, timing: &str) -> Option<Duration> {
    let parts: Vec<&str> = timing.split(':').collect();
    match parts.as_slice() {
        [num_str, den_str] => {
            let beat_division = parse_nat_string(num_str);
            let num_beats = parse_nat_string(den_str);
            match (beat_division, num_beats) {
                (Some(beat_division), Some(num_beats)) => {
                    if beat_division == 0 {
                        None
                    } else {
                        let note_micros = time::bpm_measure_micros_rat(bpm)
                            .mul(Rat::from_int(num_beats as i128))
                            .div(Rat::from_int(beat_division as i128));
                        Some(time::duration_from_rat_micros(note_micros))
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// `Timing.parseNdDurationExact`.
pub fn parse_nd_duration_exact(bpm: Rat, timing: &str) -> Option<Duration> {
    parse_nd_duration(bpm, timing)
}

/// `Timing.parseDurationInner`.
pub fn parse_duration_inner(current_bpm: Rat, inner: &str) -> Option<Duration> {
    let hash_count = inner.matches('#').count();
    if inner.starts_with('#') && hash_count == 1 && !inner.contains(':') {
        parse_duration_string(&inner[1..])
    } else if hash_count == 2 {
        match split_hash2(inner) {
            Some((_, _, duration_part)) => parse_duration_string(&duration_part),
            None => None,
        }
    } else if hash_count == 1 {
        match split_hash1(inner) {
            Some((custom_bpm_str, timing_str)) => {
                let seg_bpm = match parse_rat_string(&custom_bpm_str) {
                    Some(v) if Rat::zero().lt(v) => v,
                    _ => current_bpm,
                };
                match parse_nd_duration_exact(seg_bpm, &timing_str) {
                    Some(duration) => Some(duration),
                    None => parse_duration_string(&timing_str),
                }
            }
            None => None,
        }
    } else {
        match parse_nd_duration_exact(current_bpm, inner) {
            Some(duration) => Some(duration),
            None => {
                if !inner.starts_with('#') {
                    parse_duration_string(inner)
                } else {
                    None
                }
            }
        }
    }
}

/// `Timing.parseDurationSpec`.
pub fn parse_duration_spec(bpm: Rat, token: &str) -> Option<Duration> {
    let contents = extract_bracket_contents(token);
    contents.iter().fold(None, |acc, inner| {
        match (acc, parse_duration_inner(bpm, inner)) {
            (Some(sum), Some(duration)) => Some(sum + duration),
            (Some(sum), None) => Some(sum),
            (None, Some(duration)) => Some(duration),
            (None, None) => None,
        }
    })
}

/// `Timing.parseStarWaitSpec`.
pub fn parse_star_wait_spec(bpm: Rat, token: &str) -> Option<Duration> {
    match extract_bracket_contents(token).first() {
        None => Some(beat_sec(bpm)),
        Some(inner) => {
            let hash_count = inner.matches('#').count();
            if hash_count == 2 {
                match split_hash2(inner) {
                    Some((wait_part, _, _)) => {
                        if wait_part.is_empty() {
                            Some(beat_sec(bpm))
                        } else {
                            match parse_seconds_rat_string(&wait_part) {
                                Some(wait_seconds) => Some(time::duration_from_seconds_rat(wait_seconds)),
                                None => parse_duration_string(&wait_part),
                            }
                        }
                    }
                    None => Some(beat_sec(bpm)),
                }
            } else if hash_count == 1 {
                match split_hash1(inner) {
                    Some((wait_bpm_str, _)) => match parse_rat_string(&wait_bpm_str) {
                        Some(wait_bpm) if Rat::zero().lt(wait_bpm) => Some(beat_sec(wait_bpm)),
                        _ => Some(beat_sec(bpm)),
                    },
                    None => Some(beat_sec(bpm)),
                }
            } else {
                Some(beat_sec(bpm))
            }
        }
    }
}

/// `Timing.noteTimingIncrement`.
pub fn note_timing_increment(bpm: Rat, divisor: usize) -> Duration {
    if Rat::zero().lt(bpm) && divisor > 0 {
        time::duration_from_rat_micros(
            time::bpm_measure_micros_rat(bpm).div(Rat::from_int(divisor as i128)),
        )
    } else {
        Duration::zero()
    }
}

/// `Timing.pseudoIncrement`.
pub fn pseudo_increment(bpm: Rat) -> Duration {
    if Rat::zero().lt(bpm) {
        time::duration_from_rat_micros(time::bpm_beat_micros_rat(bpm).div(Rat::from_int(32)))
    } else {
        Duration::from_micros(1000)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rat_string() {
        assert_eq!(parse_rat_string("1.5"), Some(Rat::new(3, 2)));
        assert_eq!(parse_rat_string("-0.25"), Some(Rat::new(-1, 4)));
        assert_eq!(parse_rat_string("120"), Some(Rat::from_int(120)));
        assert_eq!(parse_rat_string("1.2.3"), None);
    }

    #[test]
    fn nd_duration() {
        // 120 BPM, "4:1" = one quarter note = 0.5s.
        let d = parse_nd_duration(Rat::from_int(120), "4:1").unwrap();
        assert_eq!(d.to_micros(), 500_000);
        assert_eq!(parse_nd_duration(Rat::from_int(120), "0:1"), None);
    }

    #[test]
    fn bracket_contents() {
        assert_eq!(extract_bracket_contents("1-3[4:1][8:1]"), vec!["4:1", "8:1"]);
        assert_eq!(extract_bracket_contents("[#0.5]"), vec!["#0.5"]);
    }

    #[test]
    fn duration_spec_sum() {
        // Two quarter-note segments sum to 1s at 120 BPM.
        let d = parse_duration_spec(Rat::from_int(120), "[4:1][4:1]").unwrap();
        assert_eq!(d.to_micros(), 1_000_000);
    }

    #[test]
    fn star_wait_default_is_beat() {
        assert_eq!(
            parse_star_wait_spec(Rat::from_int(120), "1").unwrap().to_micros(),
            500_000
        );
    }

    #[test]
    fn note_increment_bpm_zero() {
        assert_eq!(note_timing_increment(Rat::zero(), 4).to_micros(), 0);
        assert_eq!(pseudo_increment(Rat::zero()).to_micros(), 1000);
    }
}
