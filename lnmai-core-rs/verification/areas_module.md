# Areas Module Verification

## Status: VERIFIED

## Lean Definitions

```lean
inductive SensorArea where
  | A1 | A2 | ... | E8
deriving DecidableEq, Repr, BEq, Ord

inductive ButtonZone where
  | K1 | K2 | ... | K8
deriving DecidableEq, Repr, BEq, Ord

inductive OuterSlot where
  | S1 | S2 | ... | S8
deriving DecidableEq, Repr, BEq, Ord
```

## Rust Implementation

```rust
pub enum SensorArea { A1, A2, ..., E8 }
pub enum ButtonZone { K1, K2, ..., K8 }
pub enum OuterSlot { S1, S2, ..., S8 }
```

## Proof Obligations

### 1. sensorArea_ofIndex_toIndex
**Lean:** `∀ a, SensorArea.ofIndex? (SensorArea.toIndex a) = some a`
**Rust:** `SensorArea::from_index(area.to_index()) == Some(area)`
**Status:** VERIFIED (property test + unit test)

### 2. sensorArea_toIndex_ofIndex
**Lean:** `∀ i h, SensorArea.toIndex (SensorArea.ofIndex? i h) = i`
**Rust:** `SensorArea::from_index(i).unwrap().to_index() == i`
**Status:** VERIFIED (property test + unit test)

### 3. buttonZone_ofIndex_toIndex
**Lean:** `∀ a, ButtonZone.ofIndex? (ButtonZone.toIndex a) = some a`
**Rust:** `ButtonZone::from_index(zone.to_index()) == Some(zone)`
**Status:** VERIFIED (property test + unit test)

### 4. buttonZone_toIndex_ofIndex
**Lean:** `∀ i h, ButtonZone.toIndex (ButtonZone.ofIndex? i h) = i`
**Rust:** `ButtonZone::from_index(i).unwrap().to_index() == i`
**Status:** VERIFIED (property test + unit test)

### 5. outerSlot_ofIndex_toIndex
**Lean:** `∀ a, OuterSlot.ofIndex? (OuterSlot.toIndex a) = some a`
**Rust:** `OuterSlot::from_index(slot.to_index()) == Some(slot)`
**Status:** VERIFIED (property test + unit test)

### 6. outerSlot_toIndex_ofIndex
**Lean:** `∀ i h, OuterSlot.toIndex (OuterSlot.ofIndex? i h) = i`
**Rust:** `OuterSlot::from_index(i).unwrap().to_index() == i`
**Status:** VERIFIED (property test + unit test)

## Additional Properties Verified

- OuterSlot.toButtonZone preserves index
- ButtonZone.toOuterSlot preserves index
- OuterSlot → ButtonZone → OuterSlot roundtrip
- Rotate by 8 is identity

## Missing Lemmas

None identified.

## Aeneas Compatibility

All enums are simple C-like enums, fully compatible with Aeneas extraction.
