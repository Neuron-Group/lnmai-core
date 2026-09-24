//! Slide symmetry group (`DihedralGroup 8` in Lean).
//!
//! Mirrors `LnmaiCore/Simai/Symmetry.lean`. The Lean code only ever needs a
//! group element as "mirrored or not" plus a rotation count, and applies it to
//! sensor areas, so a two-constructor representation is sufficient.

use crate::areas::SensorArea;

/// An element of the dihedral group on 8 positions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideSymmetry {
    /// Rotation by `k` (Lean `DihedralGroup.r k`).
    R(u32),
    /// Reflection followed by rotation by `k` (Lean `DihedralGroup.sr k`).
    Sr(u32),
}

/// `SlideSymmetry.direct`.
pub const fn direct() -> SlideSymmetry {
    SlideSymmetry::R(0)
}

/// `SlideSymmetry.mirror`.
pub const fn mirror() -> SlideSymmetry {
    SlideSymmetry::Sr(0)
}

/// `SlideSymmetry.isMirrored`.
pub fn is_mirrored(g: SlideSymmetry) -> bool {
    match g {
        SlideSymmetry::R(_) => false,
        SlideSymmetry::Sr(_) => true,
    }
}

/// `SlideSymmetry.rotationSteps`.
pub fn rotation_steps(g: SlideSymmetry) -> u32 {
    match g {
        SlideSymmetry::R(k) => k,
        SlideSymmetry::Sr(k) => k,
    }
}

/// Mirror a sensor area across the vertical axis (Lean's private `reflect`).
fn reflect(area: SensorArea) -> SensorArea {
    match area {
        SensorArea::C => SensorArea::C,
        SensorArea::A1 => SensorArea::A1,
        SensorArea::A2 => SensorArea::A8,
        SensorArea::A3 => SensorArea::A7,
        SensorArea::A4 => SensorArea::A6,
        SensorArea::A5 => SensorArea::A5,
        SensorArea::A6 => SensorArea::A4,
        SensorArea::A7 => SensorArea::A3,
        SensorArea::A8 => SensorArea::A2,
        SensorArea::B1 => SensorArea::B1,
        SensorArea::B2 => SensorArea::B8,
        SensorArea::B3 => SensorArea::B7,
        SensorArea::B4 => SensorArea::B6,
        SensorArea::B5 => SensorArea::B5,
        SensorArea::B6 => SensorArea::B4,
        SensorArea::B7 => SensorArea::B3,
        SensorArea::B8 => SensorArea::B2,
        SensorArea::D1 => SensorArea::D1,
        SensorArea::D2 => SensorArea::D8,
        SensorArea::D3 => SensorArea::D7,
        SensorArea::D4 => SensorArea::D6,
        SensorArea::D5 => SensorArea::D5,
        SensorArea::D6 => SensorArea::D4,
        SensorArea::D7 => SensorArea::D3,
        SensorArea::D8 => SensorArea::D2,
        SensorArea::E1 => SensorArea::E1,
        SensorArea::E2 => SensorArea::E8,
        SensorArea::E3 => SensorArea::E7,
        SensorArea::E4 => SensorArea::E6,
        SensorArea::E5 => SensorArea::E5,
        SensorArea::E6 => SensorArea::E4,
        SensorArea::E7 => SensorArea::E3,
        SensorArea::E8 => SensorArea::E2,
    }
}

/// `SlideSymmetry.actOnSensorArea`.
pub fn act_on_sensor_area(g: SlideSymmetry, area: SensorArea) -> SensorArea {
    match g {
        SlideSymmetry::R(k) => area.rotate(k as usize),
        SlideSymmetry::Sr(k) => reflect(area).rotate(k as usize),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn direct_is_identity() {
        for area in crate::storage::sensor_area_storage_order() {
            assert_eq!(act_on_sensor_area(direct(), area), area);
        }
    }

    #[test]
    fn mirror_swaps_2_and_8() {
        assert_eq!(act_on_sensor_area(mirror(), SensorArea::A2), SensorArea::A8);
        assert_eq!(act_on_sensor_area(mirror(), SensorArea::A8), SensorArea::A2);
    }

    #[test]
    fn rotation_advances_ring() {
        let g = SlideSymmetry::R(1);
        assert_eq!(act_on_sensor_area(g, SensorArea::A1), SensorArea::A2);
        assert_eq!(act_on_sensor_area(g, SensorArea::A8), SensorArea::A1);
    }
}
