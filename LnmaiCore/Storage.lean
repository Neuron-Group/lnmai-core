import LnmaiCore.Areas
import Lean.Data.Json

open Lean

namespace LnmaiCore

private def sensorAreaToStorageIndex : SensorArea → Nat
  | .A1 => 0 | .A2 => 1 | .A3 => 2 | .A4 => 3 | .A5 => 4 | .A6 => 5 | .A7 => 6 | .A8 => 7
  | .D1 => 8 | .D2 => 9 | .D3 => 10 | .D4 => 11 | .D5 => 12 | .D6 => 13 | .D7 => 14 | .D8 => 15
  | .C => 16
  | .E1 => 17 | .E2 => 18 | .E3 => 19 | .E4 => 20 | .E5 => 21 | .E6 => 22 | .E7 => 23 | .E8 => 24
  | .B1 => 25 | .B2 => 26 | .B3 => 27 | .B4 => 28 | .B5 => 29 | .B6 => 30 | .B7 => 31 | .B8 => 32

private def sensorAreaOfStorageIndex? : Nat → Option SensorArea
  | 0 => some .A1 | 1 => some .A2 | 2 => some .A3 | 3 => some .A4 | 4 => some .A5 | 5 => some .A6 | 6 => some .A7 | 7 => some .A8
  | 8 => some .D1 | 9 => some .D2 | 10 => some .D3 | 11 => some .D4 | 12 => some .D5 | 13 => some .D6 | 14 => some .D7 | 15 => some .D8
  | 16 => some .C
  | 17 => some .E1 | 18 => some .E2 | 19 => some .E3 | 20 => some .E4 | 21 => some .E5 | 22 => some .E6 | 23 => some .E7 | 24 => some .E8
  | 25 => some .B1 | 26 => some .B2 | 27 => some .B3 | 28 => some .B4 | 29 => some .B5 | 30 => some .B6 | 31 => some .B7 | 32 => some .B8
  | _ => none

private def collectSomeRev : List (Option α) → List α → List α
  | [], acc => acc.reverse
  | some value :: rest, acc => collectSomeRev rest (value :: acc)
  | none :: rest, acc => collectSomeRev rest acc

private def sensorAreaStorageOrder : List SensorArea :=
  collectSomeRev ((List.range Constants.SENSOR_AREA_COUNT).map sensorAreaOfStorageIndex?) []

structure ButtonVec (α : Type) where
  data : List α
deriving Inhabited, Repr

structure SensorVec (α : Type) where
  data : List α
deriving Inhabited, Repr

instance {α : Type} [ToJson α] : ToJson (ButtonVec α) where
  toJson vec := toJson vec.data

instance {α : Type} [FromJson α] : FromJson (ButtonVec α) where
  fromJson? json := ButtonVec.mk <$> fromJson? json

instance {α : Type} [ToJson α] : ToJson (SensorVec α) where
  toJson vec := toJson vec.data

instance {α : Type} [FromJson α] : FromJson (SensorVec α) where
  fromJson? json := SensorVec.mk <$> fromJson? json

def ButtonVec.replicate (n : Nat) (value : α) : ButtonVec α :=
  { data := List.replicate n value }

def SensorVec.replicate (n : Nat) (value : α) : SensorVec α :=
  { data := List.replicate n value }

/-- Storage-only helper: read the backing list by numeric offset. -/
private def ButtonVec.getDIndex (vec : ButtonVec α) (index : Nat) (default : α) : α :=
  (vec.data[index]?).getD default

/-- Storage-only helper: read the backing list by numeric offset. -/
private def SensorVec.getDIndex (vec : SensorVec α) (index : Nat) (default : α) : α :=
  (vec.data[index]?).getD default

def ButtonVec.getD (vec : ButtonVec α) (zone : ButtonZone) (default : α) : α :=
  vec.getDIndex zone.toIndex default

def SensorVec.getD (vec : SensorVec α) (area : SensorArea) (default : α) : α :=
  vec.getDIndex (sensorAreaToStorageIndex area) default

private def listSetAt : List α → Nat → α → List α
  | [], _, _ => []
  | _ :: rest, 0, value => value :: rest
  | x :: rest, index + 1, value => x :: listSetAt rest index value

def ButtonVec.set (vec : ButtonVec α) (zone : ButtonZone) (value : α) : ButtonVec α :=
  { data := listSetAt vec.data zone.toIndex value }

def SensorVec.set (vec : SensorVec α) (area : SensorArea) (value : α) : SensorVec α :=
  { data := listSetAt vec.data (sensorAreaToStorageIndex area) value }

def ButtonVec.toList (vec : ButtonVec α) : List α :=
  vec.data

def SensorVec.toList (vec : SensorVec α) : List α :=
  vec.data

def ButtonVec.entries (vec : ButtonVec α) : List (ButtonZone × α) :=
  ButtonZone.storageOrder.zip vec.data

def SensorVec.entries (vec : SensorVec α) : List (SensorArea × α) :=
  sensorAreaStorageOrder.zip vec.data

private def ButtonVec.ofList (xs : List α) : ButtonVec α :=
  { data := xs }

private def SensorVec.ofList (xs : List α) : SensorVec α :=
  { data := xs }

def ButtonVec.ofFn (f : ButtonZone → α) : ButtonVec α :=
  { data := ButtonZone.storageOrder.map f }

def SensorVec.ofFn (f : SensorArea → α) : SensorVec α :=
  { data := sensorAreaStorageOrder.map f }

private def buttonMapAccumList (keys : List ButtonZone) (values : List α) (state : σ)
    (f : ButtonZone → α → σ → β × σ) : List β × σ :=
  match keys, values with
  | key :: keyRest, value :: valueRest =>
      let (mapped, state') := f key value state
      let (mappedRest, state'') := buttonMapAccumList keyRest valueRest state' f
      (mapped :: mappedRest, state'')
  | _, _ => ([], state)

private def sensorMapAccumList (keys : List SensorArea) (values : List α) (state : σ)
    (f : SensorArea → α → σ → β × σ) : List β × σ :=
  match keys, values with
  | key :: keyRest, value :: valueRest =>
      let (mapped, state') := f key value state
      let (mappedRest, state'') := sensorMapAccumList keyRest valueRest state' f
      (mapped :: mappedRest, state'')
  | _, _ => ([], state)

def ButtonVec.mapAccum (vec : ButtonVec α) (state : σ)
    (f : ButtonZone → α → σ → β × σ) : ButtonVec β × σ :=
  let (mapped, state') := buttonMapAccumList ButtonZone.storageOrder vec.data state f
  (ButtonVec.ofList mapped, state')

def SensorVec.mapAccum (vec : SensorVec α) (state : σ)
    (f : SensorArea → α → σ → β × σ) : SensorVec β × σ :=
  let (mapped, state') := sensorMapAccumList sensorAreaStorageOrder vec.data state f
  (SensorVec.ofList mapped, state')

end LnmaiCore
