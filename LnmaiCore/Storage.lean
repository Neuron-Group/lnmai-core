import LnmaiCore.Areas

namespace LnmaiCore

structure ButtonVec (α : Type) where
  data : List α
deriving Inhabited, Repr

structure SensorVec (α : Type) where
  data : List α
deriving Inhabited, Repr

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
  vec.getDIndex area.toIndex default

private def listSetAt : List α → Nat → α → List α
  | [], _, _ => []
  | _ :: rest, 0, value => value :: rest
  | x :: rest, index + 1, value => x :: listSetAt rest index value

def ButtonVec.set (vec : ButtonVec α) (zone : ButtonZone) (value : α) : ButtonVec α :=
  { data := listSetAt vec.data zone.toIndex value }

def SensorVec.set (vec : SensorVec α) (area : SensorArea) (value : α) : SensorVec α :=
  { data := listSetAt vec.data area.toIndex value }

def ButtonVec.toList (vec : ButtonVec α) : List α :=
  vec.data

def SensorVec.toList (vec : SensorVec α) : List α :=
  vec.data

def ButtonVec.entries (vec : ButtonVec α) : List (ButtonZone × α) :=
  ButtonZone.all.zip vec.data

def SensorVec.entries (vec : SensorVec α) : List (SensorArea × α) :=
  SensorArea.all.zip vec.data

private def ButtonVec.ofList (xs : List α) : ButtonVec α :=
  { data := xs }

private def SensorVec.ofList (xs : List α) : SensorVec α :=
  { data := xs }

def ButtonVec.ofFn (f : ButtonZone → α) : ButtonVec α :=
  { data := ButtonZone.all.map f }

def SensorVec.ofFn (f : SensorArea → α) : SensorVec α :=
  { data := SensorArea.all.map f }

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
  let (mapped, state') := buttonMapAccumList ButtonZone.all vec.data state f
  (ButtonVec.ofList mapped, state')

def SensorVec.mapAccum (vec : SensorVec α) (state : σ)
    (f : SensorArea → α → σ → β × σ) : SensorVec β × σ :=
  let (mapped, state') := sensorMapAccumList SensorArea.all vec.data state f
  (SensorVec.ofList mapped, state')

end LnmaiCore
