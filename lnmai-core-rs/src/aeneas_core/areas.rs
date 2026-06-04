//! Aeneas-friendly areas module
//!
//! Sensor areas, button zones, and outer slot types.
//! Matches LnmaiCore/Areas.lean

/// Sensor areas (33 areas)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SensorArea {
    A1, A2, A3, A4, A5, A6, A7, A8,
    D1, D2, D3, D4, D5, D6, D7, D8,
    C,
    E1, E2, E3, E4, E5, E6, E7, E8,
    B1, B2, B3, B4, B5, B6, B7, B8,
}

impl SensorArea {
    pub fn to_index(&self) -> usize {
        match self {
            SensorArea::A1 => 0, SensorArea::A2 => 1, SensorArea::A3 => 2, SensorArea::A4 => 3,
            SensorArea::A5 => 4, SensorArea::A6 => 5, SensorArea::A7 => 6, SensorArea::A8 => 7,
            SensorArea::D1 => 8, SensorArea::D2 => 9, SensorArea::D3 => 10, SensorArea::D4 => 11,
            SensorArea::D5 => 12, SensorArea::D6 => 13, SensorArea::D7 => 14, SensorArea::D8 => 15,
            SensorArea::C => 16,
            SensorArea::E1 => 17, SensorArea::E2 => 18, SensorArea::E3 => 19, SensorArea::E4 => 20,
            SensorArea::E5 => 21, SensorArea::E6 => 22, SensorArea::E7 => 23, SensorArea::E8 => 24,
            SensorArea::B1 => 25, SensorArea::B2 => 26, SensorArea::B3 => 27, SensorArea::B4 => 28,
            SensorArea::B5 => 29, SensorArea::B6 => 30, SensorArea::B7 => 31, SensorArea::B8 => 32,
        }
    }

    pub fn from_index(index: usize) -> Option<SensorArea> {
        match index {
            0 => Some(SensorArea::A1), 1 => Some(SensorArea::A2), 2 => Some(SensorArea::A3), 3 => Some(SensorArea::A4),
            4 => Some(SensorArea::A5), 5 => Some(SensorArea::A6), 6 => Some(SensorArea::A7), 7 => Some(SensorArea::A8),
            8 => Some(SensorArea::D1), 9 => Some(SensorArea::D2), 10 => Some(SensorArea::D3), 11 => Some(SensorArea::D4),
            12 => Some(SensorArea::D5), 13 => Some(SensorArea::D6), 14 => Some(SensorArea::D7), 15 => Some(SensorArea::D8),
            16 => Some(SensorArea::C),
            17 => Some(SensorArea::E1), 18 => Some(SensorArea::E2), 19 => Some(SensorArea::E3), 20 => Some(SensorArea::E4),
            21 => Some(SensorArea::E5), 22 => Some(SensorArea::E6), 23 => Some(SensorArea::E7), 24 => Some(SensorArea::E8),
            25 => Some(SensorArea::B1), 26 => Some(SensorArea::B2), 27 => Some(SensorArea::B3), 28 => Some(SensorArea::B4),
            29 => Some(SensorArea::B5), 30 => Some(SensorArea::B6), 31 => Some(SensorArea::B7), 32 => Some(SensorArea::B8),
            _ => None,
        }
    }
}

/// Button zones (8 zones)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ButtonZone {
    K1, K2, K3, K4, K5, K6, K7, K8,
}

impl ButtonZone {
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
}

/// Outer slots (8 slots)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OuterSlot {
    S1, S2, S3, S4, S5, S6, S7, S8,
}

impl OuterSlot {
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

    pub fn to_button_zone(&self) -> ButtonZone {
        match self {
            OuterSlot::S1 => ButtonZone::K1, OuterSlot::S2 => ButtonZone::K2,
            OuterSlot::S3 => ButtonZone::K3, OuterSlot::S4 => ButtonZone::K4,
            OuterSlot::S5 => ButtonZone::K5, OuterSlot::S6 => ButtonZone::K6,
            OuterSlot::S7 => ButtonZone::K7, OuterSlot::S8 => ButtonZone::K8,
        }
    }
}
