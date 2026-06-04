//! Grade conversion: maps the raw 15-tier judgment to a reduced grade
//! depending on the selected difficulty mode (JudgeStyle).
//!
//! Fully structural — no LT/LE comparisons needed.

use super::types::{JudgeGrade, JudgeStyle};

/// Convert grade using Maji mode
///
/// Great* → Good
/// Perfect3rd → Great
/// Perfect2nd → Miss/TooFast
/// Perfect, Miss, TooFast → fixed points
pub fn convert_maji(grade: JudgeGrade) -> JudgeGrade {
    match grade {
        // Great* → Good
        JudgeGrade::LateGreat => JudgeGrade::LateGood,
        JudgeGrade::LateGreat2nd => JudgeGrade::LateGood,
        JudgeGrade::LateGreat3rd => JudgeGrade::LateGood,
        JudgeGrade::FastGreat => JudgeGrade::FastGood,
        JudgeGrade::FastGreat2nd => JudgeGrade::FastGood,
        JudgeGrade::FastGreat3rd => JudgeGrade::FastGood,
        // Perfect3rd → Great
        JudgeGrade::LatePerfect3rd => JudgeGrade::LateGreat,
        JudgeGrade::FastPerfect3rd => JudgeGrade::FastGreat,
        // Perfect2nd → Miss/TooFast
        JudgeGrade::LatePerfect2nd => JudgeGrade::Miss,
        JudgeGrade::FastPerfect2nd => JudgeGrade::TooFast,
        // Good → Miss/TooFast
        JudgeGrade::LateGood => JudgeGrade::Miss,
        JudgeGrade::FastGood => JudgeGrade::TooFast,
        // Fixed points
        JudgeGrade::Perfect => JudgeGrade::Perfect,
        JudgeGrade::Miss => JudgeGrade::Miss,
        JudgeGrade::TooFast => JudgeGrade::TooFast,
    }
}

/// Convert grade using Gachi mode
///
/// Perfect3rd → Good
/// Perfect2nd → Great
/// Everything else → Miss/TooFast
pub fn convert_gachi(grade: JudgeGrade) -> JudgeGrade {
    match grade {
        // Perfect3rd → Good
        JudgeGrade::LatePerfect3rd => JudgeGrade::LateGood,
        JudgeGrade::FastPerfect3rd => JudgeGrade::FastGood,
        // Perfect2nd → Great
        JudgeGrade::LatePerfect2nd => JudgeGrade::LateGreat,
        JudgeGrade::FastPerfect2nd => JudgeGrade::FastGreat,
        // Everything else → Miss/TooFast
        JudgeGrade::LateGreat => JudgeGrade::Miss,
        JudgeGrade::LateGreat2nd => JudgeGrade::Miss,
        JudgeGrade::LateGreat3rd => JudgeGrade::Miss,
        JudgeGrade::LateGood => JudgeGrade::Miss,
        JudgeGrade::FastGreat => JudgeGrade::TooFast,
        JudgeGrade::FastGreat2nd => JudgeGrade::TooFast,
        JudgeGrade::FastGreat3rd => JudgeGrade::TooFast,
        JudgeGrade::FastGood => JudgeGrade::TooFast,
        // Fixed points
        JudgeGrade::Perfect => JudgeGrade::Perfect,
        JudgeGrade::Miss => JudgeGrade::Miss,
        JudgeGrade::TooFast => JudgeGrade::TooFast,
    }
}

/// Convert grade using Gori mode
///
/// Perfect → Perfect
/// Miss → Miss
/// All late grades → Miss
/// All fast grades → TooFast
pub fn convert_gori(grade: JudgeGrade) -> JudgeGrade {
    match grade {
        // Fixed points
        JudgeGrade::Perfect => JudgeGrade::Perfect,
        JudgeGrade::Miss => JudgeGrade::Miss,
        // All late grades → Miss
        JudgeGrade::LateGood => JudgeGrade::Miss,
        JudgeGrade::LateGreat3rd => JudgeGrade::Miss,
        JudgeGrade::LateGreat2nd => JudgeGrade::Miss,
        JudgeGrade::LateGreat => JudgeGrade::Miss,
        JudgeGrade::LatePerfect3rd => JudgeGrade::Miss,
        JudgeGrade::LatePerfect2nd => JudgeGrade::Miss,
        // All fast grades → TooFast
        JudgeGrade::FastPerfect2nd => JudgeGrade::TooFast,
        JudgeGrade::FastPerfect3rd => JudgeGrade::TooFast,
        JudgeGrade::FastGreat => JudgeGrade::TooFast,
        JudgeGrade::FastGreat2nd => JudgeGrade::TooFast,
        JudgeGrade::FastGreat3rd => JudgeGrade::TooFast,
        JudgeGrade::FastGood => JudgeGrade::TooFast,
        JudgeGrade::TooFast => JudgeGrade::TooFast,
    }
}

/// Convert grade based on judge style
pub fn convert_grade(style: JudgeStyle, grade: JudgeGrade) -> JudgeGrade {
    match style {
        JudgeStyle::Default => grade,
        JudgeStyle::Maji => convert_maji(grade),
        JudgeStyle::Gachi => convert_gachi(grade),
        JudgeStyle::Gori => convert_gori(grade),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_perfect_fixed() {
        // Perfect is a fixed point under all conversion styles
        for style in &[JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori] {
            assert_eq!(convert_grade(*style, JudgeGrade::Perfect), JudgeGrade::Perfect);
        }
    }

    #[test]
    fn test_miss_fixed() {
        // Miss is a fixed point under all conversion styles
        for style in &[JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori] {
            assert_eq!(convert_grade(*style, JudgeGrade::Miss), JudgeGrade::Miss);
        }
    }

    #[test]
    fn test_too_fast_fixed_maji_gachi() {
        // TooFast is a fixed point under Maji and Gachi
        assert_eq!(convert_maji(JudgeGrade::TooFast), JudgeGrade::TooFast);
        assert_eq!(convert_gachi(JudgeGrade::TooFast), JudgeGrade::TooFast);
    }

    #[test]
    fn test_perfect_is_upper_bound() {
        // Only Perfect can convert to Perfect
        for style in &[JudgeStyle::Default, JudgeStyle::Maji, JudgeStyle::Gachi, JudgeStyle::Gori] {
            for grade in &[
                JudgeGrade::Miss,
                JudgeGrade::LateGood,
                JudgeGrade::LateGreat3rd,
                JudgeGrade::LateGreat2nd,
                JudgeGrade::LateGreat,
                JudgeGrade::LatePerfect3rd,
                JudgeGrade::LatePerfect2nd,
                JudgeGrade::FastPerfect2nd,
                JudgeGrade::FastPerfect3rd,
                JudgeGrade::FastGreat,
                JudgeGrade::FastGreat2nd,
                JudgeGrade::FastGreat3rd,
                JudgeGrade::FastGood,
                JudgeGrade::TooFast,
            ] {
                let converted = convert_grade(*style, *grade);
                assert_ne!(converted, JudgeGrade::Perfect, "Non-Perfect grade {:?} should not convert to Perfect with style {:?}", grade, style);
            }
        }
    }

    #[test]
    fn test_default_identity() {
        // Default style is identity
        let grades = [
            JudgeGrade::Miss,
            JudgeGrade::LateGood,
            JudgeGrade::LateGreat3rd,
            JudgeGrade::LateGreat2nd,
            JudgeGrade::LateGreat,
            JudgeGrade::LatePerfect3rd,
            JudgeGrade::LatePerfect2nd,
            JudgeGrade::Perfect,
            JudgeGrade::FastPerfect2nd,
            JudgeGrade::FastPerfect3rd,
            JudgeGrade::FastGreat,
            JudgeGrade::FastGreat2nd,
            JudgeGrade::FastGreat3rd,
            JudgeGrade::FastGood,
            JudgeGrade::TooFast,
        ];
        for grade in grades {
            assert_eq!(convert_grade(JudgeStyle::Default, grade), grade);
        }
    }
}
