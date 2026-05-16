import Mathlib
import LnmaiCore.Areas
import LnmaiCore.Types
import LnmaiCore.Simai.Syntax
import Lean.Data.Json

open Lean

namespace LnmaiCore.Simai

abbrev ExactArea := LnmaiCore.SensorArea

def ExactArea.label : ExactArea → String := SensorArea.label

def rotateAreaSteps (steps : Nat) : ExactArea → ExactArea := SensorArea.rotate steps

structure SlideAreaSpec where
  targetAreas : List ExactArea
  policy : AreaPolicy := AreaPolicy.Or
  isLast : Bool := false
  isSkippable : Bool := true
  arrowProgressWhenOn : Nat := 0
  arrowProgressWhenFinished : Nat := 0
deriving Inhabited, BEq, Repr, ToJson, FromJson

private def mkAreaSpec (areas : List ExactArea) (on finished : Nat) (isSkippable : Bool := true) (isLast : Bool := false) : SlideAreaSpec :=
  { targetAreas := areas
  , arrowProgressWhenOn := on
  , arrowProgressWhenFinished := finished
  , isSkippable := isSkippable
  , isLast := isLast }

private def one (areas : List ExactArea) (on finished : Nat) (isSkippable : Bool := true) (isLast : Bool := false) : SlideAreaSpec :=
  mkAreaSpec areas on finished isSkippable isLast

private def one' (area : ExactArea) (on : Nat) (finish : Nat) (isSkippable : Bool := true) (isLast : Bool := false) : SlideAreaSpec :=
  one [area] on finish isSkippable isLast

private def track (steps : List SlideAreaSpec) : List (List SlideAreaSpec) :=
  [steps]

structure WifiTableSpec where
  left : List SlideAreaSpec
  center : List SlideAreaSpec
  right : List SlideAreaSpec
deriving Inhabited

def rotateAreaSpec (steps : Nat) (spec : SlideAreaSpec) : SlideAreaSpec :=
  let rotated := spec.targetAreas.map (rotateAreaSteps steps)
  { spec with targetAreas := rotated }

private def mirrorArea : ExactArea → ExactArea
  | .C => .C
  | .A1 => .A1 | .A2 => .A8 | .A3 => .A7 | .A4 => .A6
  | .A5 => .A5 | .A6 => .A4 | .A7 => .A3 | .A8 => .A2
  | .B1 => .B1 | .B2 => .B8 | .B3 => .B7 | .B4 => .B6
  | .B5 => .B5 | .B6 => .B4 | .B7 => .B3 | .B8 => .B2
  | .D1 => .D1 | .D2 => .D8 | .D3 => .D7 | .D4 => .D6
  | .D5 => .D5 | .D6 => .D4 | .D7 => .D3 | .D8 => .D2
  | .E1 => .E1 | .E2 => .E8 | .E3 => .E7 | .E4 => .E6
  | .E5 => .E5 | .E6 => .E4 | .E7 => .E3 | .E8 => .E2

def mirrorAreaSpec (spec : SlideAreaSpec) : SlideAreaSpec :=
  let mirrored := spec.targetAreas.map mirrorArea
  { spec with targetAreas := mirrored }

def mirrorJudgeQueues (queues : List (List SlideAreaSpec)) : List (List SlideAreaSpec) :=
  queues.map (fun queue => queue.map mirrorAreaSpec)

def rotateJudgeQueues (steps : Nat) (queues : List (List SlideAreaSpec)) : List (List SlideAreaSpec) :=
  queues.map (fun queue => queue.map (rotateAreaSpec steps))

def stripMirrorPrefix : String → String
  | "" => ""
  | s => if s.front = '-' then s.drop 1 |>.toString else s

def judgeQueuesForShapeKey (shapeKey : String) (isClassic : Bool := false) : Option (List (List SlideAreaSpec)) :=
  let isMirrored := shapeKey.startsWith "-"
  let key := stripMirrorPrefix shapeKey
  let wifi : WifiTableSpec :=
    { left := [one' .A1 0 0, one' .B8 2 2, one' .B7 4 4, one [.A6, .D6] 7 7 true true]
    , center := if isClassic then [one' .A1 0 0, one' .B1 2 2, one' .C 7 7 true false] else [one' .A1 0 0, one' .B1 2 2, one' .C 4 4, one [.A5, .B5] 7 7 true true]
    , right := [one' .A1 0 0, one' .B2 2 2, one' .B3 4 4, one [.A4, .D5] 7 7 true true] }
  let ordinary : List (String × List (List SlideAreaSpec)) :=
    [ ("circle2", track [one' .A1 0 3 false false, one' .A2 5 7 true true])
    , ("circle3", track [one' .A1 0 3, one' .A2 7 11 false false, one' .A3 13 15 true true])
    , ("circle4", track [one' .A1 0 3, one' .A2 7 11, one' .A3 14 19, one' .A4 21 23 true true])
    , ("circle5", track [one' .A1 0 3, one' .A2 7 11, one' .A3 14 19, one' .A4 23 27, one' .A5 29 31 true true])
    , ("circle6", track [one' .A1 0 3, one' .A2 7 11, one' .A3 14 19, one' .A4 23 27, one' .A5 31 35, one' .A6 37 39 true true])
    , ("circle7", track [one' .A1 0 3, one' .A2 7 11, one' .A3 14 19, one' .A4 23 27, one' .A5 31 35, one' .A6 39 43, one' .A7 45 47 true true])
    , ("circle8", track [one' .A1 0 3, one' .A2 7 11, one' .A3 14 19, one' .A4 23 27, one' .A5 31 35, one' .A6 39 43, one' .A7 46 51, one' .A8 53 55 true true])
    , ("circle1", track [one' .A1 0 3, one' .A2 7 11, one' .A3 14 19, one' .A4 23 27, one' .A5 31 35, one' .A6 39 43, one' .A7 46 51, one' .A8 54 59, one' .A1 61 63 true true])
    , ("line3", [ [one' .A1 0 3], [one [.A2, .B2] 6 9 false false], [one' .A3 10 13 true true] ])
    , ("line4", [ [one' .A1 0 4], [one' .B2 6 9], [one' .B3 11 14], [one' .A4 15 18 true true] ])
    , ("line5", [ [one' .A1 0 4], [one' .B1 5 7], [one' .C 10 12], [one' .B5 13 16], [one' .A5 17 19 true true] ])
    , ("line6", [ [one' .A1 0 4], [one' .B8 6 9], [one' .B7 11 14], [one' .A6 15 18 true true] ])
    , ("line7", [ [one' .A1 0 3], [one [.A8, .B8] 6 9 false false], [one' .A7 10 13 true true] ])
    , ("v1", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B1 14 16], [one' .A1 17 19 true true] ])
    , ("v2", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B2 14 16], [one' .A2 17 19 true true] ])
    , ("v3", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B3 14 16], [one' .A3 17 19 true true] ])
    , ("v4", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B4 14 16], [one' .A4 17 19 true true] ])
    , ("v6", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B6 14 16], [one' .A6 17 19 true true] ])
    , ("v7", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B7 14 16], [one' .A7 17 19 true true] ])
    , ("v8", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 8 13], [one' .B8 14 16], [one' .A8 17 19 true true] ])
    , ("ppqq1", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 10 13], [one' .B4 15 17], [one' .A3 21 26], [one' .A2 29 32], [one' .A1 33 35 true true] ])
    , ("ppqq2", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 20 25], [one' .A2 26 28 true true] ])
    , ("ppqq3", [ [one' .A1 0 3], [one' .B1 4 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 19 22 true true] ])
    , ("ppqq4", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 20 25], [one' .A2 28 33], [one' .B1 34 37], [one' .C 39 43], [one' .B4 44 46], [one' .A4 47 49 true true] ])
    , ("ppqq5", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 20 25], [one' .A2 28 33], [one' .B1 34 37], [one' .C 39 43], [one' .B5 44 46], [one' .A5 47 49 true true] ])
    , ("ppqq6", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 20 25], [one' .A2 28 33], [one' .B1 34 37], [one [.C, .B8] 38 40], [one [.B7, .B6] 42 44], [one' .A6 46 48 true true] ])
    , ("ppqq7", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 20 25], [one' .A2 28 33], [one' .B1 34 37], [one' .B8 38 42], [one' .A7 43 46 true true] ])
    , ("ppqq8", [ [one' .A1 0 3], [one' .B1 5 7], [one' .C 9 13], [one' .B4 14 17], [one' .A3 20 25], [one' .A2 28 33], [one [.B1, .A1] 35 37], [one' .A8 38 41 true true] ])
    , ("L2", [ [one' .A1 0 3], [one [.A8, .A1] 6 10 false false], [one' .A7 12 19], [one' .A8 21 24], [one' .B1 25 28], [one' .A2 29 32 true true] ])
    , ("L3", [ [one' .A1 0 3], [one [.A8, .A1] 6 10 false false], [one' .A7 12 18], [one' .A7 20 22], [one' .C 25 27], [one' .B3 28 31], [one' .A3 32 34 true true] ])
    , ("L4", [ [one' .A1 0 3], [one [.A8, .A1] 6 10 false false], [one' .A7 12 19], [one' .A6 21 24], [one' .A5 25 28], [one' .A4 29 32 true true] ])
    , ("L5", [ [one' .A1 0 3], [one [.A8, .A1] 6 10 false false], [one' .A7 12 18], [one [.A6, .E7] 21 24 false false], [one' .A5 27 28 true true] ])
    , ("s", [ [one' .A1 0 4], [one' .B8 7 9], [one' .B7 10 12], [one' .C 14 17], [one' .B3 19 21], [one' .B4 22 25], [one' .A5 27 30 true true] ])
    , ("pq1", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 11], [one' .B6 12 14], [one' .B5 15 17], [one' .B4 19 21], [one' .B3 22 24], [one' .B2 25 29], [one' .A1 30 33 true true] ])
    , ("pq2", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 11], [one' .B6 12 14], [one' .B5 16 18], [one' .B4 19 21], [one' .B3 22 26], [one' .A2 27 30 true true] ])
    , ("pq3", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 11], [one' .B6 12 14], [one' .B5 16 18], [one' .B4 20 23], [one' .A3 25 27 true true] ])
    , ("pq4", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 11], [one' .B6 12 14], [one' .B5 16 20], [one' .A4 22 24 true true] ])
    , ("pq5", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 12], [one' .B6 14 17], [one' .A5 19 21 true true] ])
    , ("pq6", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 11], [one' .B6 13 15], [one' .B5 16 18], [one' .B4 19 21], [one' .B3 22 24], [one' .B2 25 27], [one' .B1 28 30], [one' .B8 31 33], [one' .B7 35 38], [one' .A6 40 42 true true] ])
    , ("pq7", [ [one' .A1 0 4], [one' .B8 7 9], [one' .B7 10 12], [one' .B6 13 15], [one' .B5 16 18], [one' .B4 20 22], [one' .B3 23 25], [one' .B2 26 28], [one' .B1 30 32], [one' .B8 33 36], [one' .A7 37 40 true true] ])
    , ("pq8", [ [one' .A1 0 4], [one' .B8 5 8], [one' .B7 9 11], [one' .B6 12 14], [one' .B5 15 17], [one' .B4 19 21], [one' .B3 22 24], [one' .B2 25 27], [one' .B1 28 32], [one' .A8 33 36 true true] ])
    ]
  let base :=
    match ordinary.find? (fun pair => pair.1 == key) with
    | some (_, queues) => some [queues.foldl (fun acc queue => acc ++ queue) []]
    | none =>
        if key == "wifi" then
          some [wifi.left, wifi.center, wifi.right]
        else
          none
  base.map (fun queues => if isMirrored then mirrorJudgeQueues queues else queues)

end LnmaiCore.Simai
