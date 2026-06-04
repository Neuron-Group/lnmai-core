//! Storage layer: ButtonVec and SensorVec backed by List/Vec.
//!
//! Mirrors LnmaiCore/Storage.lean.
//! SensorVec uses the same storage order as Lean:
//!   A1-A8, D1-D8, C, E1-E8, B1-B8
//! (33 elements, order matching sensorAreaToStorageIndex)

use serde::{Deserialize, Serialize};

use super::areas::{ButtonZone, SensorArea};

/// Flat storage index for SensorArea matching Lean sensorAreaToStorageIndex
pub fn sensor_area_to_storage_index(area: SensorArea) -> usize {
    match area {
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

pub fn sensor_area_of_storage_index(index: usize) -> Option<SensorArea> {
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

/// Ordered list of sensor areas in storage order (Lean: sensorAreaStorageOrder)
pub const SENSOR_AREA_STORAGE_ORDER: &[SensorArea] = &[
    SensorArea::A1, SensorArea::A2, SensorArea::A3, SensorArea::A4,
    SensorArea::A5, SensorArea::A6, SensorArea::A7, SensorArea::A8,
    SensorArea::D1, SensorArea::D2, SensorArea::D3, SensorArea::D4,
    SensorArea::D5, SensorArea::D6, SensorArea::D7, SensorArea::D8,
    SensorArea::C,
    SensorArea::E1, SensorArea::E2, SensorArea::E3, SensorArea::E4,
    SensorArea::E5, SensorArea::E6, SensorArea::E7, SensorArea::E8,
    SensorArea::B1, SensorArea::B2, SensorArea::B3, SensorArea::B4,
    SensorArea::B5, SensorArea::B6, SensorArea::B7, SensorArea::B8,
];

/// Ordered list of button zones (Lean: ButtonZone.storageOrder)
pub const BUTTON_ZONE_STORAGE_ORDER: &[ButtonZone] = &[
    ButtonZone::K1, ButtonZone::K2, ButtonZone::K3, ButtonZone::K4,
    ButtonZone::K5, ButtonZone::K6, ButtonZone::K7, ButtonZone::K8,
];

/// Button vector (8 elements, backed by array)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ButtonVec<T> {
    pub data: Vec<T>,
}

impl<T: Clone> ButtonVec<T> {
    pub fn replicate(value: T) -> Self {
        Self { data: vec![value; 8] }
    }

    pub fn get_d(&self, zone: ButtonZone, default: T) -> T {
        self.data.get(zone.to_index()).cloned().unwrap_or(default)
    }

    pub fn set(&self, zone: ButtonZone, value: T) -> Self {
        let mut data = self.data.clone();
        data[zone.to_index()] = value;
        Self { data }
    }

    pub fn to_list(&self) -> Vec<T> {
        self.data.clone()
    }

    pub fn entries(&self) -> Vec<(ButtonZone, T)> {
        BUTTON_ZONE_STORAGE_ORDER.iter()
            .zip(self.data.iter())
            .map(|(z, v)| (*z, v.clone()))
            .collect()
    }

    pub fn of_fn<F: Fn(ButtonZone) -> T>(f: F) -> Self {
        Self {
            data: BUTTON_ZONE_STORAGE_ORDER.iter().map(|z| f(*z)).collect(),
        }
    }
}

impl<T: Default + Clone> Default for ButtonVec<T> {
    fn default() -> Self {
        Self::replicate(T::default())
    }
}

/// Sensor vector (33 elements, backed by Vec, Lean storage order)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SensorVec<T> {
    pub data: Vec<T>,
}

impl<T: Clone> SensorVec<T> {
    pub fn replicate(value: T) -> Self {
        Self { data: vec![value; 33] }
    }

    pub fn get_d(&self, area: SensorArea, default: T) -> T {
        let idx = sensor_area_to_storage_index(area);
        self.data.get(idx).cloned().unwrap_or(default)
    }

    pub fn set(&self, area: SensorArea, value: T) -> Self {
        let mut data = self.data.clone();
        data[sensor_area_to_storage_index(area)] = value;
        Self { data }
    }

    pub fn to_list(&self) -> Vec<T> {
        self.data.clone()
    }

    pub fn entries(&self) -> Vec<(SensorArea, T)> {
        SENSOR_AREA_STORAGE_ORDER.iter()
            .zip(self.data.iter())
            .map(|(a, v)| (*a, v.clone()))
            .collect()
    }

    pub fn of_fn<F: Fn(SensorArea) -> T>(f: F) -> Self {
        Self {
            data: SENSOR_AREA_STORAGE_ORDER.iter().map(|a| f(*a)).collect(),
        }
    }
}

impl<T: Default + Clone> Default for SensorVec<T> {
    fn default() -> Self {
        Self::replicate(T::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sensor_storage_index_roundtrip() {
        for area in SensorArea::ALL {
            let idx = sensor_area_to_storage_index(*area);
            let recovered = sensor_area_of_storage_index(idx).unwrap();
            assert_eq!(*area, recovered);
        }
    }

    #[test]
    fn test_button_vec_get_set() {
        let mut vec = ButtonVec::replicate(false);
        assert_eq!(vec.get_d(ButtonZone::K1, true), false);
        vec = vec.set(ButtonZone::K1, true);
        assert_eq!(vec.get_d(ButtonZone::K1, false), true);
        assert_eq!(vec.get_d(ButtonZone::K2, false), false);
    }

    #[test]
    fn test_sensor_vec_get_set() {
        let mut vec = SensorVec::replicate(false);
        assert_eq!(vec.get_d(SensorArea::A1, true), false);
        vec = vec.set(SensorArea::A1, true);
        assert_eq!(vec.get_d(SensorArea::A1, false), true);
        assert_eq!(vec.get_d(SensorArea::B1, false), false);
    }

    #[test]
    fn test_sensor_storage_order_len() {
        assert_eq!(SENSOR_AREA_STORAGE_ORDER.len(), 33);
        assert_eq!(BUTTON_ZONE_STORAGE_ORDER.len(), 8);
    }

    #[test]
    fn test_button_vec_of_fn() {
        let vec = ButtonVec::of_fn(|z| z.to_index());
        for zone in ButtonZone::ALL {
            assert_eq!(vec.get_d(*zone, 999), zone.to_index());
        }
    }

    #[test]
    fn test_sensor_vec_of_fn() {
        let vec = SensorVec::of_fn(|a| sensor_area_to_storage_index(a));
        for area in SensorArea::ALL {
            assert_eq!(vec.get_d(*area, 999), sensor_area_to_storage_index(*area));
        }
    }
}
