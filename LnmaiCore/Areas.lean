import Mathlib
import Lean.Data.Json
import LnmaiCore.Constants

open Lean

namespace LnmaiCore

inductive SensorArea where
  | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8
  | B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8
  | C
  | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8
  | E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8
deriving Inhabited, Repr, BEq, DecidableEq, Ord

inductive ButtonZone where
  | K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8
deriving Inhabited, Repr, BEq, DecidableEq, Ord

inductive OuterSlot where
  | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8
deriving Inhabited, Repr, BEq, DecidableEq, Ord

def SensorArea.all : List SensorArea :=
  [ .A1, .A2, .A3, .A4, .A5, .A6, .A7, .A8
  , .B1, .B2, .B3, .B4, .B5, .B6, .B7, .B8
  , .C
  , .D1, .D2, .D3, .D4, .D5, .D6, .D7, .D8
  , .E1, .E2, .E3, .E4, .E5, .E6, .E7, .E8 ]

def ButtonZone.all : List ButtonZone :=
  [ .K1, .K2, .K3, .K4, .K5, .K6, .K7, .K8 ]

def OuterSlot.all : List OuterSlot :=
  [ .S1, .S2, .S3, .S4, .S5, .S6, .S7, .S8 ]

def SensorArea.toIndex : SensorArea → Nat
  | .A1 => 0 | .A2 => 1 | .A3 => 2 | .A4 => 3 | .A5 => 4 | .A6 => 5 | .A7 => 6 | .A8 => 7
  | .D1 => 8 | .D2 => 9 | .D3 => 10 | .D4 => 11 | .D5 => 12 | .D6 => 13 | .D7 => 14 | .D8 => 15
  | .C => 16
  | .E1 => 17 | .E2 => 18 | .E3 => 19 | .E4 => 20 | .E5 => 21 | .E6 => 22 | .E7 => 23 | .E8 => 24
  | .B1 => 25 | .B2 => 26 | .B3 => 27 | .B4 => 28 | .B5 => 29 | .B6 => 30 | .B7 => 31 | .B8 => 32

def SensorArea.ofIndex? : Nat → Option SensorArea
  | 0 => some .A1 | 1 => some .A2 | 2 => some .A3 | 3 => some .A4 | 4 => some .A5 | 5 => some .A6 | 6 => some .A7 | 7 => some .A8
  | 8 => some .D1 | 9 => some .D2 | 10 => some .D3 | 11 => some .D4 | 12 => some .D5 | 13 => some .D6 | 14 => some .D7 | 15 => some .D8
  | 16 => some .C
  | 17 => some .E1 | 18 => some .E2 | 19 => some .E3 | 20 => some .E4 | 21 => some .E5 | 22 => some .E6 | 23 => some .E7 | 24 => some .E8
  | 25 => some .B1 | 26 => some .B2 | 27 => some .B3 | 28 => some .B4 | 29 => some .B5 | 30 => some .B6 | 31 => some .B7 | 32 => some .B8
  | _ => none

def ButtonZone.toIndex : ButtonZone → Nat
  | .K1 => 0 | .K2 => 1 | .K3 => 2 | .K4 => 3 | .K5 => 4 | .K6 => 5 | .K7 => 6 | .K8 => 7

def ButtonZone.ofIndex? : Nat → Option ButtonZone
  | 0 => some .K1 | 1 => some .K2 | 2 => some .K3 | 3 => some .K4
  | 4 => some .K5 | 5 => some .K6 | 6 => some .K7 | 7 => some .K8
  | _ => none

def OuterSlot.toIndex : OuterSlot → Nat
  | .S1 => 0 | .S2 => 1 | .S3 => 2 | .S4 => 3 | .S5 => 4 | .S6 => 5 | .S7 => 6 | .S8 => 7

def OuterSlot.ofIndex? : Nat → Option OuterSlot
  | 0 => some .S1 | 1 => some .S2 | 2 => some .S3 | 3 => some .S4
  | 4 => some .S5 | 5 => some .S6 | 6 => some .S7 | 7 => some .S8
  | _ => none

theorem sensorArea_ofIndex_toIndex (area : SensorArea) : SensorArea.ofIndex? area.toIndex = some area := by
  cases area <;> rfl

theorem sensorArea_toIndex_ofIndex (index : Nat) (h : index < Constants.SENSOR_AREA_COUNT) :
    match SensorArea.ofIndex? index with
    | some area => area.toIndex = index
    | none => False := by
  have h' : index < 33 := by simpa [Constants.SENSOR_AREA_COUNT] using h
  interval_cases index <;> rfl

theorem buttonZone_ofIndex_toIndex (zone : ButtonZone) : ButtonZone.ofIndex? zone.toIndex = some zone := by
  cases zone <;> rfl

theorem outerSlot_ofIndex_toIndex (slot : OuterSlot) : OuterSlot.ofIndex? slot.toIndex = some slot := by
  cases slot <;> rfl

theorem buttonZone_toIndex_ofIndex (index : Nat) (h : index < Constants.BUTTON_ZONE_COUNT) :
    match ButtonZone.ofIndex? index with
    | some zone => zone.toIndex = index
    | none => False := by
  have h' : index < 8 := by simpa [Constants.BUTTON_ZONE_COUNT] using h
  interval_cases index <;> rfl

theorem outerSlot_toIndex_ofIndex (index : Nat) (h : index < Constants.BUTTON_ZONE_COUNT) :
    match OuterSlot.ofIndex? index with
    | some slot => slot.toIndex = index
    | none => False := by
  have h' : index < 8 := by simpa [Constants.BUTTON_ZONE_COUNT] using h
  interval_cases index <;> rfl

def SensorArea.label : SensorArea → String
  | .A1 => "Sensor A1" | .A2 => "Sensor A2" | .A3 => "Sensor A3" | .A4 => "Sensor A4"
  | .A5 => "Sensor A5" | .A6 => "Sensor A6" | .A7 => "Sensor A7" | .A8 => "Sensor A8"
  | .B1 => "Sensor B1" | .B2 => "Sensor B2" | .B3 => "Sensor B3" | .B4 => "Sensor B4"
  | .B5 => "Sensor B5" | .B6 => "Sensor B6" | .B7 => "Sensor B7" | .B8 => "Sensor B8"
  | .C => "Sensor C"
  | .D1 => "Sensor D1" | .D2 => "Sensor D2" | .D3 => "Sensor D3" | .D4 => "Sensor D4"
  | .D5 => "Sensor D5" | .D6 => "Sensor D6" | .D7 => "Sensor D7" | .D8 => "Sensor D8"
  | .E1 => "Sensor E1" | .E2 => "Sensor E2" | .E3 => "Sensor E3" | .E4 => "Sensor E4"
  | .E5 => "Sensor E5" | .E6 => "Sensor E6" | .E7 => "Sensor E7" | .E8 => "Sensor E8"

def SensorArea.code : SensorArea → String
  | .A1 => "A1" | .A2 => "A2" | .A3 => "A3" | .A4 => "A4" | .A5 => "A5" | .A6 => "A6" | .A7 => "A7" | .A8 => "A8"
  | .B1 => "B1" | .B2 => "B2" | .B3 => "B3" | .B4 => "B4" | .B5 => "B5" | .B6 => "B6" | .B7 => "B7" | .B8 => "B8"
  | .C => "C"
  | .D1 => "D1" | .D2 => "D2" | .D3 => "D3" | .D4 => "D4" | .D5 => "D5" | .D6 => "D6" | .D7 => "D7" | .D8 => "D8"
  | .E1 => "E1" | .E2 => "E2" | .E3 => "E3" | .E4 => "E4" | .E5 => "E5" | .E6 => "E6" | .E7 => "E7" | .E8 => "E8"

def ButtonZone.code : ButtonZone → String
  | .K1 => "K1" | .K2 => "K2" | .K3 => "K3" | .K4 => "K4"
  | .K5 => "K5" | .K6 => "K6" | .K7 => "K7" | .K8 => "K8"

def OuterSlot.code : OuterSlot → String
  | .S1 => "S1" | .S2 => "S2" | .S3 => "S3" | .S4 => "S4"
  | .S5 => "S5" | .S6 => "S6" | .S7 => "S7" | .S8 => "S8"

instance : ToString SensorArea where
  toString := SensorArea.code

instance : ToString ButtonZone where
  toString := ButtonZone.code

instance : ToString OuterSlot where
  toString := OuterSlot.code

instance : ToJson SensorArea where
  toJson area := Json.str area.code

instance : FromJson SensorArea where
  fromJson?
    | Json.str "A1" => .ok .A1 | Json.str "A2" => .ok .A2 | Json.str "A3" => .ok .A3 | Json.str "A4" => .ok .A4
    | Json.str "A5" => .ok .A5 | Json.str "A6" => .ok .A6 | Json.str "A7" => .ok .A7 | Json.str "A8" => .ok .A8
    | Json.str "B1" => .ok .B1 | Json.str "B2" => .ok .B2 | Json.str "B3" => .ok .B3 | Json.str "B4" => .ok .B4
    | Json.str "B5" => .ok .B5 | Json.str "B6" => .ok .B6 | Json.str "B7" => .ok .B7 | Json.str "B8" => .ok .B8
    | Json.str "C" => .ok .C
    | Json.str "D1" => .ok .D1 | Json.str "D2" => .ok .D2 | Json.str "D3" => .ok .D3 | Json.str "D4" => .ok .D4
    | Json.str "D5" => .ok .D5 | Json.str "D6" => .ok .D6 | Json.str "D7" => .ok .D7 | Json.str "D8" => .ok .D8
    | Json.str "E1" => .ok .E1 | Json.str "E2" => .ok .E2 | Json.str "E3" => .ok .E3 | Json.str "E4" => .ok .E4
    | Json.str "E5" => .ok .E5 | Json.str "E6" => .ok .E6 | Json.str "E7" => .ok .E7 | Json.str "E8" => .ok .E8
    | _ => .error "invalid SensorArea"

instance : ToJson ButtonZone where
  toJson zone := Json.str zone.code

instance : FromJson ButtonZone where
  fromJson?
    | Json.str "K1" => .ok .K1 | Json.str "K2" => .ok .K2 | Json.str "K3" => .ok .K3 | Json.str "K4" => .ok .K4
    | Json.str "K5" => .ok .K5 | Json.str "K6" => .ok .K6 | Json.str "K7" => .ok .K7 | Json.str "K8" => .ok .K8
    | _ => .error "invalid ButtonZone"

instance : ToJson OuterSlot where
  toJson slot := Json.str slot.code

instance : FromJson OuterSlot where
  fromJson?
    | Json.str "S1" => .ok .S1 | Json.str "S2" => .ok .S2 | Json.str "S3" => .ok .S3 | Json.str "S4" => .ok .S4
    | Json.str "S5" => .ok .S5 | Json.str "S6" => .ok .S6 | Json.str "S7" => .ok .S7 | Json.str "S8" => .ok .S8
    | _ => .error "invalid OuterSlot"

private def rotateRingIndex (steps : Nat) (n : Nat) : Nat :=
  (((n - 1) + steps) % 8) + 1

private def rotateRingSensor (ctor : Nat → SensorArea) (steps n : Nat) : SensorArea :=
  ctor (rotateRingIndex steps n)

private def rotateRingButton (steps n : Nat) : ButtonZone :=
  match rotateRingIndex steps n with
  | 1 => .K1 | 2 => .K2 | 3 => .K3 | 4 => .K4 | 5 => .K5 | 6 => .K6 | 7 => .K7 | _ => .K8

def SensorArea.rotate (steps : Nat) : SensorArea → SensorArea
  | .C => .C
  | .A1 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 1
  | .A2 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 2
  | .A3 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 3
  | .A4 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 4
  | .A5 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 5
  | .A6 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 6
  | .A7 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 7
  | .A8 => rotateRingSensor (fun | 1 => .A1 | 2 => .A2 | 3 => .A3 | 4 => .A4 | 5 => .A5 | 6 => .A6 | 7 => .A7 | _ => .A8) steps 8
  | .B1 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 1
  | .B2 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 2
  | .B3 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 3
  | .B4 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 4
  | .B5 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 5
  | .B6 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 6
  | .B7 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 7
  | .B8 => rotateRingSensor (fun | 1 => .B1 | 2 => .B2 | 3 => .B3 | 4 => .B4 | 5 => .B5 | 6 => .B6 | 7 => .B7 | _ => .B8) steps 8
  | .D1 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 1
  | .D2 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 2
  | .D3 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 3
  | .D4 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 4
  | .D5 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 5
  | .D6 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 6
  | .D7 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 7
  | .D8 => rotateRingSensor (fun | 1 => .D1 | 2 => .D2 | 3 => .D3 | 4 => .D4 | 5 => .D5 | 6 => .D6 | 7 => .D7 | _ => .D8) steps 8
  | .E1 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 1
  | .E2 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 2
  | .E3 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 3
  | .E4 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 4
  | .E5 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 5
  | .E6 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 6
  | .E7 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 7
  | .E8 => rotateRingSensor (fun | 1 => .E1 | 2 => .E2 | 3 => .E3 | 4 => .E4 | 5 => .E5 | 6 => .E6 | 7 => .E7 | _ => .E8) steps 8

def ButtonZone.rotate (steps : Nat) : ButtonZone → ButtonZone
  | .K1 => rotateRingButton steps 1 | .K2 => rotateRingButton steps 2
  | .K3 => rotateRingButton steps 3 | .K4 => rotateRingButton steps 4
  | .K5 => rotateRingButton steps 5 | .K6 => rotateRingButton steps 6
  | .K7 => rotateRingButton steps 7 | .K8 => rotateRingButton steps 8

def OuterSlot.rotate (steps : Nat) : OuterSlot → OuterSlot
  | .S1 => match (1 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S2 => match (2 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S3 => match (3 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S4 => match (4 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S5 => match (5 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S6 => match (6 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S7 => match (7 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8
  | .S8 => match (8 - 1 + steps) % 8 + 1 with | 1 => .S1 | 2 => .S2 | 3 => .S3 | 4 => .S4 | 5 => .S5 | 6 => .S6 | 7 => .S7 | _ => .S8

def OuterSlot.toButtonZone : OuterSlot → ButtonZone
  | .S1 => .K1 | .S2 => .K2 | .S3 => .K3 | .S4 => .K4
  | .S5 => .K5 | .S6 => .K6 | .S7 => .K7 | .S8 => .K8

def ButtonZone.toOuterSlot : ButtonZone → OuterSlot
  | .K1 => .S1 | .K2 => .S2 | .K3 => .S3 | .K4 => .S4
  | .K5 => .S5 | .K6 => .S6 | .K7 => .S7 | .K8 => .S8

def OuterSlot.toOuterSensorArea : OuterSlot → SensorArea
  | .S1 => .A1 | .S2 => .A2 | .S3 => .A3 | .S4 => .A4
  | .S5 => .A5 | .S6 => .A6 | .S7 => .A7 | .S8 => .A8

def SensorArea.toOuterSlot? : SensorArea → Option OuterSlot
  | .A1 => some .S1 | .A2 => some .S2 | .A3 => some .S3 | .A4 => some .S4
  | .A5 => some .S5 | .A6 => some .S6 | .A7 => some .S7 | .A8 => some .S8
  | _ => none

def ButtonZone.toOuterSensorArea : ButtonZone → SensorArea
  | zone => zone.toOuterSlot.toOuterSensorArea

def SensorArea.toOuterButtonZone? : SensorArea → Option ButtonZone
  | area => area.toOuterSlot?.map OuterSlot.toButtonZone

end LnmaiCore
