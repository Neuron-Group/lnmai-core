//! Physical area types for the maimai game.
//!
//! Defines sensor areas (A1-A8, B1-B8, C, D1-D8, E1-E8),
//! button zones (K1-K8), and outer slots (S1-S8).

use serde::{Deserialize, Serialize};
use std::fmt;

/// 33 sensor areas on the maimai cabinet
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum SensorArea {
    A1, A2, A3, A4, A5, A6, A7, A8,
    B1, B2, B3, B4, B5, B6, B7, B8,
    C,
    D1, D2, D3, D4, D5, D6, D7, D8,
    E1, E2, E3, E4, E5, E6, E7, E8,
}

impl SensorArea {
    pub const ALL: &'static [SensorArea] = &[
        SensorArea::A1, SensorArea::A2, SensorArea::A3, SensorArea::A4,
        SensorArea::A5, SensorArea::A6, SensorArea::A7, SensorArea::A8,
        SensorArea::B1, SensorArea::B2, SensorArea::B3, SensorArea::B4,
        SensorArea::B5, SensorArea::B6, SensorArea::B7, SensorArea::B8,
        SensorArea::C,
        SensorArea::D1, SensorArea::D2, SensorArea::D3, SensorArea::D4,
        SensorArea::D5, SensorArea::D6, SensorArea::D7, SensorArea::D8,
        SensorArea::E1, SensorArea::E2, SensorArea::E3, SensorArea::E4,
        SensorArea::E5, SensorArea::E6, SensorArea::E7, SensorArea::E8,
    ];

    pub fn to_index(&self) -> usize {
        match self {
            SensorArea::A1 => 0, SensorArea::A2 => 1, SensorArea::A3 => 2, SensorArea::A4 => 3,
            SensorArea::A5 => 4, SensorArea::A6 => 5, SensorArea::A7 => 6, SensorArea::A8 => 7,
            SensorArea::B1 => 8, SensorArea::B2 => 9, SensorArea::B3 => 10, SensorArea::B4 => 11,
            SensorArea::B5 => 12, SensorArea::B6 => 13, SensorArea::B7 => 14, SensorArea::B8 => 15,
            SensorArea::C => 16,
            SensorArea::D1 => 17, SensorArea::D2 => 18, SensorArea::D3 => 19, SensorArea::D4 => 20,
            SensorArea::D5 => 21, SensorArea::D6 => 22, SensorArea::D7 => 23, SensorArea::D8 => 24,
            SensorArea::E1 => 25, SensorArea::E2 => 26, SensorArea::E3 => 27, SensorArea::E4 => 28,
            SensorArea::E5 => 29, SensorArea::E6 => 30, SensorArea::E7 => 31, SensorArea::E8 => 32,
        }
    }

    pub fn from_index(index: usize) -> Option<SensorArea> {
        match index {
            0 => Some(SensorArea::A1), 1 => Some(SensorArea::A2), 2 => Some(SensorArea::A3), 3 => Some(SensorArea::A4),
            4 => Some(SensorArea::A5), 5 => Some(SensorArea::A6), 6 => Some(SensorArea::A7), 7 => Some(SensorArea::A8),
            8 => Some(SensorArea::B1), 9 => Some(SensorArea::B2), 10 => Some(SensorArea::B3), 11 => Some(SensorArea::B4),
            12 => Some(SensorArea::B5), 13 => Some(SensorArea::B6), 14 => Some(SensorArea::B7), 15 => Some(SensorArea::B8),
            16 => Some(SensorArea::C),
            17 => Some(SensorArea::D1), 18 => Some(SensorArea::D2), 19 => Some(SensorArea::D3), 20 => Some(SensorArea::D4),
            21 => Some(SensorArea::D5), 22 => Some(SensorArea::D6), 23 => Some(SensorArea::D7), 24 => Some(SensorArea::D8),
            25 => Some(SensorArea::E1), 26 => Some(SensorArea::E2), 27 => Some(SensorArea::E3), 28 => Some(SensorArea::E4),
            29 => Some(SensorArea::E5), 30 => Some(SensorArea::E6), 31 => Some(SensorArea::E7), 32 => Some(SensorArea::E8),
            _ => None,
        }
    }

    pub fn label(&self) -> &'static str {
        match self {
            SensorArea::A1 => "Sensor A1", SensorArea::A2 => "Sensor A2", SensorArea::A3 => "Sensor A3", SensorArea::A4 => "Sensor A4",
            SensorArea::A5 => "Sensor A5", SensorArea::A6 => "Sensor A6", SensorArea::A7 => "Sensor A7", SensorArea::A8 => "Sensor A8",
            SensorArea::B1 => "Sensor B1", SensorArea::B2 => "Sensor B2", SensorArea::B3 => "Sensor B3", SensorArea::B4 => "Sensor B4",
            SensorArea::B5 => "Sensor B5", SensorArea::B6 => "Sensor B6", SensorArea::B7 => "Sensor B7", SensorArea::B8 => "Sensor B8",
            SensorArea::C => "Sensor C",
            SensorArea::D1 => "Sensor D1", SensorArea::D2 => "Sensor D2", SensorArea::D3 => "Sensor D3", SensorArea::D4 => "Sensor D4",
            SensorArea::D5 => "Sensor D5", SensorArea::D6 => "Sensor D6", SensorArea::D7 => "Sensor D7", SensorArea::D8 => "Sensor D8",
            SensorArea::E1 => "Sensor E1", SensorArea::E2 => "Sensor E2", SensorArea::E3 => "Sensor E3", SensorArea::E4 => "Sensor E4",
            SensorArea::E5 => "Sensor E5", SensorArea::E6 => "Sensor E6", SensorArea::E7 => "Sensor E7", SensorArea::E8 => "Sensor E8",
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            SensorArea::A1 => "A1", SensorArea::A2 => "A2", SensorArea::A3 => "A3", SensorArea::A4 => "A4",
            SensorArea::A5 => "A5", SensorArea::A6 => "A6", SensorArea::A7 => "A7", SensorArea::A8 => "A8",
            SensorArea::B1 => "B1", SensorArea::B2 => "B2", SensorArea::B3 => "B3", SensorArea::B4 => "B4",
            SensorArea::B5 => "B5", SensorArea::B6 => "B6", SensorArea::B7 => "B7", SensorArea::B8 => "B8",
            SensorArea::C => "C",
            SensorArea::D1 => "D1", SensorArea::D2 => "D2", SensorArea::D3 => "D3", SensorArea::D4 => "D4",
            SensorArea::D5 => "D5", SensorArea::D6 => "D6", SensorArea::D7 => "D7", SensorArea::D8 => "D8",
            SensorArea::E1 => "E1", SensorArea::E2 => "E2", SensorArea::E3 => "E3", SensorArea::E4 => "E4",
            SensorArea::E5 => "E5", SensorArea::E6 => "E6", SensorArea::E7 => "E7", SensorArea::E8 => "E8",
        }
    }

    pub fn rotate(&self, steps: usize) -> SensorArea {
        match self {
            SensorArea::C => SensorArea::C,
            _ => {
                let index = self.to_index();
                let ring_start = if index < 8 { 0 } else if index < 16 { 8 } else if index == 16 { 16 } else if index < 25 { 17 } else { 25 };
                let ring_size = if index == 16 { 1 } else { 8 };
                let pos_in_ring = index - ring_start;
                let new_pos = (pos_in_ring + steps) % ring_size;
                SensorArea::from_index(ring_start + new_pos).unwrap()
            }
        }
    }

    pub fn to_outer_slot(&self) -> Option<OuterSlot> {
        match self {
            SensorArea::A1 => Some(OuterSlot::S1), SensorArea::A2 => Some(OuterSlot::S2),
            SensorArea::A3 => Some(OuterSlot::S3), SensorArea::A4 => Some(OuterSlot::S4),
            SensorArea::A5 => Some(OuterSlot::S5), SensorArea::A6 => Some(OuterSlot::S6),
            SensorArea::A7 => Some(OuterSlot::S7), SensorArea::A8 => Some(OuterSlot::S8),
            _ => None,
        }
    }

    pub fn to_outer_button_zone(&self) -> Option<ButtonZone> {
        self.to_outer_slot().map(|s| s.to_button_zone())
    }
}

impl fmt::Display for SensorArea {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

/// 8 button zones on the maimai cabinet
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ButtonZone {
    K1, K2, K3, K4, K5, K6, K7, K8,
}

impl ButtonZone {
    pub const ALL: &'static [ButtonZone] = &[
        ButtonZone::K1, ButtonZone::K2, ButtonZone::K3, ButtonZone::K4,
        ButtonZone::K5, ButtonZone::K6, ButtonZone::K7, ButtonZone::K8,
    ];

    pub fn to_index(&self) -> usize {
        match self {
            ButtonZone::K1 => 0, ButtonZone::K2 => 1, ButtonZone::K3 => 2, ButtonZone::K4 => 3,
            ButtonZone::K5 => 4, ButtonZone::K6 => 5, ButtonZone::K7 => 6, ButtonZone::K8 => 7,
        }
    }

    pub fn from_index(index: usize) -> Option<ButtonZone> {
        match index {
            0 => Some(ButtonZone::K1), 1 => Some(ButtonZone::K2), 2 => Some(ButtonZone::K3), 3 => Some(ButtonZone::K4),
            4 => Some(ButtonZone::K5), 5 => Some(ButtonZone::K6), 6 => Some(ButtonZone::K7), 7 => Some(ButtonZone::K8),
            _ => None,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            ButtonZone::K1 => "K1", ButtonZone::K2 => "K2", ButtonZone::K3 => "K3", ButtonZone::K4 => "K4",
            ButtonZone::K5 => "K5", ButtonZone::K6 => "K6", ButtonZone::K7 => "K7", ButtonZone::K8 => "K8",
        }
    }

    pub fn rotate(&self, steps: usize) -> ButtonZone {
        let index = self.to_index();
        let new_index = (index + steps) % 8;
        ButtonZone::from_index(new_index).unwrap()
    }

    pub fn to_outer_slot(&self) -> OuterSlot {
        match self {
            ButtonZone::K1 => OuterSlot::S1, ButtonZone::K2 => OuterSlot::S2,
            ButtonZone::K3 => OuterSlot::S3, ButtonZone::K4 => OuterSlot::S4,
            ButtonZone::K5 => OuterSlot::S5, ButtonZone::K6 => OuterSlot::S6,
            ButtonZone::K7 => OuterSlot::S7, ButtonZone::K8 => OuterSlot::S8,
        }
    }

    pub fn to_outer_sensor_area(&self) -> SensorArea {
        self.to_outer_slot().to_outer_sensor_area()
    }
}

impl fmt::Display for ButtonZone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

/// 8 outer slots on the maimai cabinet
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum OuterSlot {
    S1, S2, S3, S4, S5, S6, S7, S8,
}

impl OuterSlot {
    pub const ALL: &'static [OuterSlot] = &[
        OuterSlot::S1, OuterSlot::S2, OuterSlot::S3, OuterSlot::S4,
        OuterSlot::S5, OuterSlot::S6, OuterSlot::S7, OuterSlot::S8,
    ];

    pub fn to_index(&self) -> usize {
        match self {
            OuterSlot::S1 => 0, OuterSlot::S2 => 1, OuterSlot::S3 => 2, OuterSlot::S4 => 3,
            OuterSlot::S5 => 4, OuterSlot::S6 => 5, OuterSlot::S7 => 6, OuterSlot::S8 => 7,
        }
    }

    pub fn from_index(index: usize) -> Option<OuterSlot> {
        match index {
            0 => Some(OuterSlot::S1), 1 => Some(OuterSlot::S2), 2 => Some(OuterSlot::S3), 3 => Some(OuterSlot::S4),
            4 => Some(OuterSlot::S5), 5 => Some(OuterSlot::S6), 6 => Some(OuterSlot::S7), 7 => Some(OuterSlot::S8),
            _ => None,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            OuterSlot::S1 => "S1", OuterSlot::S2 => "S2", OuterSlot::S3 => "S3", OuterSlot::S4 => "S4",
            OuterSlot::S5 => "S5", OuterSlot::S6 => "S6", OuterSlot::S7 => "S7", OuterSlot::S8 => "S8",
        }
    }

    pub fn rotate(&self, steps: usize) -> OuterSlot {
        let index = self.to_index();
        let new_index = (index + steps) % 8;
        OuterSlot::from_index(new_index).unwrap()
    }

    pub fn to_button_zone(&self) -> ButtonZone {
        match self {
            OuterSlot::S1 => ButtonZone::K1, OuterSlot::S2 => ButtonZone::K2,
            OuterSlot::S3 => ButtonZone::K3, OuterSlot::S4 => ButtonZone::K4,
            OuterSlot::S5 => ButtonZone::K5, OuterSlot::S6 => ButtonZone::K6,
            OuterSlot::S7 => ButtonZone::K7, OuterSlot::S8 => ButtonZone::K8,
        }
    }

    pub fn to_outer_sensor_area(&self) -> SensorArea {
        match self {
            OuterSlot::S1 => SensorArea::A1, OuterSlot::S2 => SensorArea::A2,
            OuterSlot::S3 => SensorArea::A3, OuterSlot::S4 => SensorArea::A4,
            OuterSlot::S5 => SensorArea::A5, OuterSlot::S6 => SensorArea::A6,
            OuterSlot::S7 => SensorArea::A7, OuterSlot::S8 => SensorArea::A8,
        }
    }
}

impl fmt::Display for OuterSlot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sensor_area_roundtrip() {
        for area in SensorArea::ALL {
            let index = area.to_index();
            let recovered = SensorArea::from_index(index).unwrap();
            assert_eq!(*area, recovered);
        }
    }

    #[test]
    fn test_button_zone_roundtrip() {
        for zone in ButtonZone::ALL {
            let index = zone.to_index();
            let recovered = ButtonZone::from_index(index).unwrap();
            assert_eq!(*zone, recovered);
        }
    }

    #[test]
    fn test_outer_slot_roundtrip() {
        for slot in OuterSlot::ALL {
            let index = slot.to_index();
            let recovered = OuterSlot::from_index(index).unwrap();
            assert_eq!(*slot, recovered);
        }
    }

    #[test]
    fn test_outer_slot_to_button_zone() {
        for slot in OuterSlot::ALL {
            let zone = slot.to_button_zone();
            assert_eq!(slot.to_index(), zone.to_index());
        }
    }

    #[test]
    fn test_button_zone_to_outer_slot() {
        for zone in ButtonZone::ALL {
            let slot = zone.to_outer_slot();
            assert_eq!(zone.to_index(), slot.to_index());
        }
    }
}
