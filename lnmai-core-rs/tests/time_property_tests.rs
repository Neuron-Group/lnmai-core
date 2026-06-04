//! Property tests for the time module.

use lnmai_core::time::*;
use proptest::prelude::*;

proptest! {
    #[test]
    fn duration_to_micros_injective(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);
        if da.to_micros() == db.to_micros() {
            prop_assert_eq!(da, db);
        }
    }

    #[test]
    fn duration_to_micros_le_iff(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);
        prop_assert_eq!(da <= db, da.to_micros() <= db.to_micros());
    }

    #[test]
    fn duration_to_micros_lt_iff(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);
        prop_assert_eq!(da < db, da.to_micros() < db.to_micros());
    }

    #[test]
    fn duration_to_micros_eq_iff(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);
        prop_assert_eq!(da == db, da.to_micros() == db.to_micros());
    }

    #[test]
    fn duration_to_int_of_int(value in -1000000i64..1000000) {
        let d = Duration::from_int(value);
        prop_assert_eq!(d.to_int(), value);
    }

    #[test]
    fn duration_compare_to_micros(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);
        prop_assert_eq!(da.cmp(&db), da.to_micros().cmp(&db.to_micros()));
    }

    #[test]
    fn timepoint_to_micros_injective(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let pa = TimePoint::from_micros(a);
        let pb = TimePoint::from_micros(b);
        if pa.to_micros() == pb.to_micros() {
            prop_assert_eq!(pa, pb);
        }
    }

    #[test]
    fn timepoint_to_micros_le_iff(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let pa = TimePoint::from_micros(a);
        let pb = TimePoint::from_micros(b);
        prop_assert_eq!(pa <= pb, pa.to_micros() <= pb.to_micros());
    }

    #[test]
    fn timepoint_to_micros_lt_iff(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let pa = TimePoint::from_micros(a);
        let pb = TimePoint::from_micros(b);
        prop_assert_eq!(pa < pb, pa.to_micros() < pb.to_micros());
    }

    #[test]
    fn timepoint_to_micros_eq_iff(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let pa = TimePoint::from_micros(a);
        let pb = TimePoint::from_micros(b);
        prop_assert_eq!(pa == pb, pa.to_micros() == pb.to_micros());
    }

    #[test]
    fn timepoint_to_int_of_int(value in -1000000i64..1000000) {
        let p = TimePoint::from_int(value);
        prop_assert_eq!(p.to_int(), value);
    }

    #[test]
    fn timepoint_compare_to_micros(a in -1000000i64..1000000, b in -1000000i64..1000000) {
        let pa = TimePoint::from_micros(a);
        let pb = TimePoint::from_micros(b);
        prop_assert_eq!(pa.cmp(&pb), pa.to_micros().cmp(&pb.to_micros()));
    }

    #[test]
    fn duration_add_commutative(a in -500000i64..500000, b in -500000i64..500000) {
        let da = Duration::from_micros(a);
        let db = Duration::from_micros(b);
        prop_assert_eq!((da + db).to_micros(), (db + da).to_micros());
    }

    #[test]
    fn duration_sub_self_is_zero(a in -1000000i64..1000000) {
        let da = Duration::from_micros(a);
        prop_assert_eq!((da - da).to_micros(), 0);
    }

    #[test]
    fn timepoint_add_duration_sub(p in -500000i64..500000, d in -500000i64..500000) {
        let point = TimePoint::from_micros(p);
        let dur = Duration::from_micros(d);
        prop_assert_eq!((point + dur - dur).to_micros(), point.to_micros());
    }
}
