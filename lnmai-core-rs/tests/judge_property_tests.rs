use lnmai_core::judge::*;
use lnmai_core::time::Duration;
use lnmai_core::types::*;
use proptest::prelude::*;

proptest! {
    #[test]
    fn judge_tap_ex_always_perfect(diff in -1000000i64..1000000) {
        let grade = judge_tap(Duration::from_micros(diff), true);
        prop_assert_eq!(grade, JudgeGrade::Perfect);
    }

    #[test]
    fn judge_tap_zero_is_perfect(diff in -16667i64..16667) {
        let grade = judge_tap(Duration::from_micros(diff), false);
        prop_assert_eq!(grade, JudgeGrade::Perfect);
    }

    #[test]
    fn judge_slide_too_late_one_remaining(_x in 0u32..1) {
        prop_assert_eq!(judge_slide_too_late(1), JudgeGrade::LateGood);
    }

    #[test]
    fn judge_slide_too_late_multi_remaining(remaining in 2u32..100) {
        prop_assert_eq!(judge_slide_too_late(remaining), JudgeGrade::Miss);
    }

    #[test]
    fn correct_slide_grade_perfect_variants(grade in prop_oneof![
        Just(JudgeGrade::Perfect),
        Just(JudgeGrade::LatePerfect2nd),
        Just(JudgeGrade::LatePerfect3rd),
        Just(JudgeGrade::FastPerfect2nd),
        Just(JudgeGrade::FastPerfect3rd),
    ]) {
        prop_assert_eq!(correct_slide_grade(grade), JudgeGrade::Perfect);
    }
}
