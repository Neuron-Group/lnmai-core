//! Property tests for the areas module.
//!
//! These tests verify the theorems from Lean's Areas.lean:
//! - sensorArea_ofIndex_toIndex
//! - sensorArea_toIndex_ofIndex
//! - buttonZone_ofIndex_toIndex
//! - buttonZone_toIndex_ofIndex
//! - outerSlot_ofIndex_toIndex
//! - outerSlot_toIndex_ofIndex

use lnmai_core::areas::*;
use proptest::prelude::*;

fn arb_sensor_area() -> impl Strategy<Value = SensorArea> {
    (0..33usize).prop_map(|i| SensorArea::from_index(i).unwrap())
}

fn arb_button_zone() -> impl Strategy<Value = ButtonZone> {
    (0..8usize).prop_map(|i| ButtonZone::from_index(i).unwrap())
}

fn arb_outer_slot() -> impl Strategy<Value = OuterSlot> {
    (0..8usize).prop_map(|i| OuterSlot::from_index(i).unwrap())
}

proptest! {
    // =========================================================================
    // SensorArea Properties
    // =========================================================================

    #[test]
    fn sensor_area_of_index_to_index(area in arb_sensor_area()) {
        // ∀ a, SensorArea.ofIndex? (SensorArea.toIndex a) = some a
        let index = area.to_index();
        let recovered = SensorArea::from_index(index);
        assert_eq!(recovered, Some(area));
    }

    #[test]
    fn sensor_area_to_index_of_index(index in 0..33usize) {
        // ∀ i h, SensorArea.toIndex (SensorArea.ofIndex? i h) = i
        let area = SensorArea::from_index(index).unwrap();
        assert_eq!(area.to_index(), index);
    }

    #[test]
    fn sensor_area_index_bijection(area in arb_sensor_area()) {
        // toIndex and fromIndex form a bijection
        let index = area.to_index();
        let recovered = SensorArea::from_index(index).unwrap();
        assert_eq!(recovered, area);
        assert_eq!(recovered.to_index(), index);
    }

    // =========================================================================
    // ButtonZone Properties
    // =========================================================================

    #[test]
    fn button_zone_of_index_to_index(zone in arb_button_zone()) {
        // ∀ a, ButtonZone.ofIndex? (ButtonZone.toIndex a) = some a
        let index = zone.to_index();
        let recovered = ButtonZone::from_index(index);
        assert_eq!(recovered, Some(zone));
    }

    #[test]
    fn button_zone_to_index_of_index(index in 0..8usize) {
        // ∀ i h, ButtonZone.toIndex (ButtonZone.ofIndex? i h) = i
        let zone = ButtonZone::from_index(index).unwrap();
        assert_eq!(zone.to_index(), index);
    }

    #[test]
    fn button_zone_index_bijection(zone in arb_button_zone()) {
        let index = zone.to_index();
        let recovered = ButtonZone::from_index(index).unwrap();
        assert_eq!(recovered, zone);
        assert_eq!(recovered.to_index(), index);
    }

    // =========================================================================
    // OuterSlot Properties
    // =========================================================================

    #[test]
    fn outer_slot_of_index_to_index(slot in arb_outer_slot()) {
        // ∀ a, OuterSlot.ofIndex? (OuterSlot.toIndex a) = some a
        let index = slot.to_index();
        let recovered = OuterSlot::from_index(index);
        assert_eq!(recovered, Some(slot));
    }

    #[test]
    fn outer_slot_to_index_of_index(index in 0..8usize) {
        // ∀ i h, OuterSlot.toIndex (OuterSlot.ofIndex? i h) = i
        let slot = OuterSlot::from_index(index).unwrap();
        assert_eq!(slot.to_index(), index);
    }

    #[test]
    fn outer_slot_index_bijection(slot in arb_outer_slot()) {
        let index = slot.to_index();
        let recovered = OuterSlot::from_index(index).unwrap();
        assert_eq!(recovered, slot);
        assert_eq!(recovered.to_index(), index);
    }

    // =========================================================================
    // Cross-type Properties
    // =========================================================================

    #[test]
    fn outer_slot_to_button_zone_preserves_index(slot in arb_outer_slot()) {
        // OuterSlot.toButtonZone preserves index
        let zone = slot.to_button_zone();
        assert_eq!(slot.to_index(), zone.to_index());
    }

    #[test]
    fn button_zone_to_outer_slot_preserves_index(zone in arb_button_zone()) {
        // ButtonZone.toOuterSlot preserves index
        let slot = zone.to_outer_slot();
        assert_eq!(zone.to_index(), slot.to_index());
    }

    #[test]
    fn outer_slot_button_zone_roundtrip(slot in arb_outer_slot()) {
        // slot.toButtonZone.toOuterSlot = slot
        let zone = slot.to_button_zone();
        let recovered = zone.to_outer_slot();
        assert_eq!(recovered, slot);
    }

    #[test]
    fn button_zone_outer_slot_roundtrip(zone in arb_button_zone()) {
        // zone.toOuterSlot.toButtonZone = zone
        let slot = zone.to_outer_slot();
        let recovered = slot.to_button_zone();
        assert_eq!(recovered, zone);
    }

    #[test]
    fn rotate_identity(area in arb_sensor_area()) {
        // Rotating by 0 (or 8) gives the same area
        let rotated = area.rotate(8);
        assert_eq!(rotated, area);
    }

    #[test]
    fn rotate_full_cycle(area in arb_sensor_area()) {
        // Rotating by 8 gives the same area
        let rotated = area.rotate(8);
        assert_eq!(rotated, area);
    }
}
