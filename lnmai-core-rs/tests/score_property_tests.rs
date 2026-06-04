//! Property tests for the score module.

use lnmai_core::score::*;
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

proptest! {
    #[test]
    fn base_score_positive(nt in prop_oneof![
        Just(NoteType::Tap),
        Just(NoteType::Hold),
        Just(NoteType::Slide),
        Just(NoteType::Touch),
        Just(NoteType::Break),
    ]) {
        prop_assert!(base_score(nt) > 0);
    }

    #[test]
    fn score_non_break_perfect_earns_all(base in 1u32..10000, multiple in 1u32..10) {
        let (earned, _lost) = score_non_break(base, JudgeGrade::Perfect, multiple);
        prop_assert_eq!(earned, base * multiple);
    }

    #[test]
    fn score_non_break_miss_loses_all(base in 1u32..10000, multiple in 1u32..10) {
        let (earned, lost) = score_non_break(base, JudgeGrade::Miss, multiple);
        prop_assert_eq!(earned, 0);
        prop_assert_eq!(lost, base * multiple);
    }

    #[test]
    fn score_non_break_conservation(base in 1u32..10000, grade in arb_judge_grade(), multiple in 1u32..10) {
        let (earned, lost) = score_non_break(base, grade, multiple);
        prop_assert_eq!(earned + lost, base * multiple);
    }

    #[test]
    fn score_break_base_conservation(grade in arb_judge_grade(), multiple in 1u32..10) {
        let (base, _, _, base_lost, _, _) = score_break(grade, multiple);
        prop_assert_eq!(base + base_lost, 2500 * multiple);
    }

    #[test]
    fn dx_score_rank_bounded(achieved in 0u32..1000, max in 1u32..1000) {
        let rank = dx_score_rank(achieved, max);
        prop_assert!(rank <= 5);
    }

    #[test]
    fn update_combo_perfect_increments(combo in 0u32..100, p_combo in 0u32..100, c_p_combo in 0u32..100) {
        let delta = update_combo(combo, p_combo, c_p_combo, 0, JudgeGrade::Perfect, 1);
        prop_assert_eq!(delta.combo, combo + 1);
        prop_assert_eq!(delta.p_combo, p_combo + 1);
        prop_assert_eq!(delta.c_p_combo, c_p_combo + 1);
    }

    #[test]
    fn update_combo_miss_resets(combo in 1u32..100, p_combo in 1u32..100, c_p_combo in 1u32..100) {
        let delta = update_combo(combo, p_combo, c_p_combo, 0, JudgeGrade::Miss, 1);
        prop_assert_eq!(delta.combo, 0);
        prop_assert_eq!(delta.p_combo, 0);
        prop_assert_eq!(delta.c_p_combo, 0);
    }
}
