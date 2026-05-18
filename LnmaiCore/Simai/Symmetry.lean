import Mathlib.GroupTheory.SpecificGroups.Dihedral
import Lean.Data.Json
import LnmaiCore.Areas

open Lean

namespace LnmaiCore.Simai

abbrev SlideSymmetry := DihedralGroup 8

deriving instance DecidableEq for SlideSymmetry

instance : Inhabited SlideSymmetry where
  default := DihedralGroup.r 0

instance : BEq SlideSymmetry where
  beq a b := decide (a = b)

instance : Repr SlideSymmetry where
  reprPrec g _ :=
    match g with
    | DihedralGroup.r k => Std.Format.text s!"r{k.val}"
    | DihedralGroup.sr k => Std.Format.text s!"sr{k.val}"

namespace SlideSymmetry

def direct : SlideSymmetry := 1

def mirror : SlideSymmetry := DihedralGroup.sr 0

def isMirrored : SlideSymmetry → Bool
  | DihedralGroup.r _ => false
  | DihedralGroup.sr _ => true

def rotationSteps : SlideSymmetry → Nat
  | DihedralGroup.r k => k.val
  | DihedralGroup.sr k => k.val

end SlideSymmetry

instance : ToJson SlideSymmetry where
  toJson g :=
    Json.mkObj
      [ ("rotationSteps", toJson <| SlideSymmetry.rotationSteps g)
      , ("mirrored", toJson <| SlideSymmetry.isMirrored g) ]

instance : FromJson SlideSymmetry where
  fromJson? json := do
    let rotationSteps ← json.getObjValAs? Nat "rotationSteps"
    let mirrored ← json.getObjValAs? Bool "mirrored"
    let k : Fin 8 := ⟨rotationSteps % 8, by omega⟩
    pure <| if mirrored then DihedralGroup.sr k else DihedralGroup.r k

def actOnSensorArea (g : SlideSymmetry) : SensorArea → SensorArea :=
  let reflect : SensorArea → SensorArea
    | .C => .C
    | .A1 => .A1 | .A2 => .A8 | .A3 => .A7 | .A4 => .A6
    | .A5 => .A5 | .A6 => .A4 | .A7 => .A3 | .A8 => .A2
    | .B1 => .B1 | .B2 => .B8 | .B3 => .B7 | .B4 => .B6
    | .B5 => .B5 | .B6 => .B4 | .B7 => .B3 | .B8 => .B2
    | .D1 => .D1 | .D2 => .D8 | .D3 => .D7 | .D4 => .D6
    | .D5 => .D5 | .D6 => .D4 | .D7 => .D3 | .D8 => .D2
    | .E1 => .E1 | .E2 => .E8 | .E3 => .E7 | .E4 => .E6
    | .E5 => .E5 | .E6 => .E4 | .E7 => .E3 | .E8 => .E2
  let rotate := SensorArea.rotate
  match g with
  | DihedralGroup.r k => rotate k.val
  | DihedralGroup.sr k => fun area => rotate k.val (reflect area)

end LnmaiCore.Simai
