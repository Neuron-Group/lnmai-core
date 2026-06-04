//! Property tests for the convert module.

use lnmai_core::convert::*;
use lnmai_core::types::*;
use proptest::prelude::*;

fn arb_judge_grade() -> impl Strategy<Value = JudgeGrade> {
    prop_oneof![
        Just(JudgeGrade::Miss),
        Just(JudgeGrade::LateGood),
        Just(JudgeGrade::LateGreat3rd),
        Just(JudgeGrade::LateGreat2nd),
        Just(JudgeGrade::LateGreat),
        Just(JudgeGrade::LatePerfect3rd),
        Just(JudgeGrade::LatePerfect2nd),
        Just(JudgeGrade::Perfect),
        Just(JudgeGrade::FastPerfect2nd),
        Just(JudgeGrade::FastPerfect3rd),
        Just(JudgeGrade::FastGreat),
        Just(JudgeGrade::FastGreat2nd),
        Just(JudgeGrade::FastGreat3rd),
        Just(JudgeGrade::FastGood),
        Just(JudgeGrade::TooFast),
    ]
}

fn arb_judge_style() -> impl Strategy<Value = JudgeStyle> {
    prop_oneof![
        Just(JudgeStyle::Default),
        Just(JudgeStyle::Maji),
        Just(JudgeStyle::Gachi),
        Just(JudgeStyle::Gori),
    ]
}

proptest! {
    #[test]
    fn perfect_fixed(style in arb_judge_style()) {
        prop_assert_eq!(convert_grade(style, JudgeGrade::Perfect), JudgeGrade::Perfect);
    }

    #[test]
    fn miss_fixed(style in arb_judge_style()) {
        prop_assert_eq!(convert_grade(style, JudgeGrade::Miss), JudgeGrade::Miss);
    }

    #[test]
    fn perfect_is_upper_bound(style in arb_judge_style(), grade in arb_judge_grade()) {
        let converted = convert_grade(style, grade);
        if converted == JudgeGrade::Perfect {
            prop_assert_eq!(grade, JudgeGrade::Perfect);
        }
    }

    #[test]
    fn default_is_identity(grade in arb_judge_grade()) {
        prop_assert_eq!(convert_grade(JudgeStyle::Default, grade), grade);
    }
}
