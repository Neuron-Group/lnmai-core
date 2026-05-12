import LnmaiCore.Types
import LnmaiCore.Simai.Syntax
import Lean.Data.Json

open Lean

namespace LnmaiCore.Simai

structure SlideAreaSpec where
  targetAreas : List Nat
  policy : AreaPolicy := AreaPolicy.Or
  isLast : Bool := false
  isSkippable : Bool := true
  arrowProgressWhenOn : Nat := 0
  arrowProgressWhenFinished : Nat := 0
deriving Inhabited, Repr, BEq, ToJson, FromJson

private def one (areas : List Nat) (on finished : Nat) (isSkippable : Bool := true) (isLast : Bool := false) : SlideAreaSpec :=
  { targetAreas := areas, arrowProgressWhenOn := on, arrowProgressWhenFinished := finished, isSkippable := isSkippable, isLast := isLast }

private def one' (area : Nat) (on : Nat) (finish : Nat) (isSkippable : Bool := true) (isLast : Bool := false) : SlideAreaSpec :=
  one [area] on finish isSkippable isLast

structure WifiTableSpec where
  left : List SlideAreaSpec
  center : List SlideAreaSpec
  right : List SlideAreaSpec
deriving Inhabited, Repr

def stripMirrorPrefix : String → String
  | "" => ""
  | s => if s.front = '-' then s.drop 1 |>.toString else s

def judgeQueuesForShapeKey (shapeKey : String) (isClassic : Bool := false) : Option (List (List SlideAreaSpec)) :=
  let key := stripMirrorPrefix shapeKey
  let wifi : WifiTableSpec :=
    { left := [one' 1 0 0, one' 8 2 2, one' 7 4 4, one [6, 22] 7 7 true true]
    , center := if isClassic then [one' 1 0 0, one' 2 2 2, one' 16 7 7 true false] else [one' 1 0 0, one' 2 2 2, one' 16 4 4 true true]
    , right := [one' 1 0 0, one' 3 2 2, one' 4 4 4, one [5, 21] 7 7 true true] }
  let ordinary : List (String × List (List SlideAreaSpec)) :=
    [ ("circle2", [ [one' 1 0 3 false false], [one' 2 5 7 true true] ])
    , ("circle3", [ [one' 1 0 3], [one' 2 7 11 false false], [one' 3 13 15 true true] ])
    , ("circle4", [ [one' 1 0 3], [one' 2 7 11], [one' 3 14 19], [one' 4 21 23 true true] ])
    , ("circle5", [ [one' 1 0 3], [one' 2 7 11], [one' 3 14 19], [one' 4 23 27], [one' 5 29 31 true true] ])
    , ("circle6", [ [one' 1 0 3], [one' 2 7 11], [one' 3 14 19], [one' 4 23 27], [one' 5 31 35], [one' 6 37 39 true true] ])
    , ("circle7", [ [one' 1 0 3], [one' 2 7 11], [one' 3 14 19], [one' 4 23 27], [one' 5 31 35], [one' 6 39 43], [one' 7 45 47 true true] ])
    , ("circle8", [ [one' 1 0 3], [one' 2 7 11], [one' 3 14 19], [one' 4 23 27], [one' 5 31 35], [one' 6 39 43], [one' 7 46 51], [one' 8 53 55 true true] ])
    , ("circle1", [ [one' 1 0 3], [one' 2 7 11], [one' 3 14 19], [one' 4 23 27], [one' 5 31 35], [one' 6 39 43], [one' 7 46 51], [one' 8 54 59], [one' 1 61 63 true true] ])
    , ("line3", [ [one' 1 0 3], [one [2, 10] 6 9 false false], [one' 3 10 13 true true] ])
    , ("line4", [ [one' 1 0 4], [one' 11 6 9], [one' 12 11 14], [one' 4 15 18 true true] ])
    , ("line5", [ [one' 1 0 4], [one' 9 5 7], [one' 16 10 12], [one' 14 13 16], [one' 5 17 19 true true] ])
    , ("line6", [ [one' 1 0 4], [one' 8 6 9], [one' 7 11 14], [one' 6 15 18 true true] ])
    , ("line7", [ [one' 1 0 3], [one [8, 10] 6 9 false false], [one' 7 10 13 true true] ])
    , ("v1", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 9 14 16], [one' 1 17 19 true true] ])
    , ("v2", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 2 14 16], [one' 2 17 19 true true] ])
    , ("v3", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 3 14 16], [one' 3 17 19 true true] ])
    , ("v4", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 4 14 16], [one' 4 17 19 true true] ])
    , ("v6", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 6 14 16], [one' 6 17 19 true true] ])
    , ("v7", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 7 14 16], [one' 7 17 19 true true] ])
    , ("v8", [ [one' 1 0 3], [one' 9 4 7], [one' 16 8 13], [one' 8 14 16], [one' 8 17 19 true true] ])
    , ("ppqq1", [ [one' 1 0 3], [one' 9 5 7], [one' 16 10 13], [one' 12 15 17], [one' 3 21 26], [one' 2 29 32], [one' 1 33 35 true true] ])
    , ("ppqq2", [ [one' 1 0 3], [one' 9 5 7], [one' 16 9 13], [one' 12 14 17], [one' 3 20 25], [one' 2 26 28 true true] ])
    , ("ppqq3", [ [one' 1 0 3], [one' 9 4 7], [one' 16 9 13], [one' 12 14 17], [one' 3 19 22 true true] ])
    , ("ppqq4", [ [one' 1 0 3], [one' 9 5 7], [one' 16 9 13], [one' 12 14 17], [one' 3 20 25], [one' 2 28 33], [one' 9 34 37], [one' 16 39 43], [one' 12 44 46], [one' 4 47 49 true true] ])
    , ("ppqq5", [ [one' 1 0 3], [one' 9 5 7], [one' 16 9 13], [one' 12 14 17], [one' 3 20 25], [one' 2 28 33], [one' 9 34 37], [one' 16 39 43], [one' 5 44 46], [one' 5 47 49 true true] ])
    , ("ppqq6", [ [one' 1 0 3], [one' 9 5 7], [one' 16 9 13], [one' 12 14 17], [one' 3 20 25], [one' 2 28 33], [one' 9 34 37], [one [16, 8] 38 40], [one [7, 6] 42 44], [one' 6 46 48 true true] ])
    , ("ppqq7", [ [one' 1 0 3], [one' 9 5 7], [one' 16 9 13], [one' 12 14 17], [one' 3 20 25], [one' 2 28 33], [one' 9 34 37], [one' 8 38 42], [one' 7 43 46 true true] ])
    , ("ppqq8", [ [one' 1 0 3], [one' 9 5 7], [one' 16 9 13], [one' 12 14 17], [one' 3 20 25], [one' 2 28 33], [one [9, 1] 35 37], [one' 8 38 41 true true] ])
    , ("L2", [ [one' 1 0 3], [one [8, 1] 6 10 false false], [one' 7 12 19], [one' 8 21 24], [one' 9 25 28], [one' 2 29 32 true true] ])
    , ("L3", [ [one' 1 0 3], [one [8, 1] 6 10 false false], [one' 7 12 18], [one' 7 20 22], [one' 16 25 27], [one' 11 28 31], [one' 3 32 34 true true] ])
    , ("L4", [ [one' 1 0 3], [one [8, 1] 6 10 false false], [one' 7 12 19], [one' 6 21 24], [one' 5 25 28], [one' 4 29 32 true true] ])
    , ("L5", [ [one' 1 0 3], [one [8, 1] 6 10 false false], [one' 7 12 18], [one [6, 15] 21 24 false false], [one' 5 27 28 true true] ])
    , ("s", [ [one' 1 0 4], [one' 8 7 9], [one' 7 10 12], [one' 16 14 17], [one' 3 19 21], [one' 4 22 25], [one' 5 27 30 true true] ])
    , ("pq1", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 11], [one' 6 12 14], [one' 5 15 17], [one' 4 19 21], [one' 3 22 24], [one' 2 25 29], [one' 1 30 33 true true] ])
    , ("pq2", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 11], [one' 6 12 14], [one' 5 16 18], [one' 4 19 21], [one' 3 22 26], [one' 2 27 30 true true] ])
    , ("pq3", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 11], [one' 6 12 14], [one' 5 16 18], [one' 4 20 23], [one' 3 25 27 true true] ])
    , ("pq4", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 11], [one' 6 12 14], [one' 5 16 20], [one' 4 22 24 true true] ])
    , ("pq5", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 12], [one' 6 14 17], [one' 5 19 21 true true] ])
    , ("pq6", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 11], [one' 6 13 15], [one' 5 16 18], [one' 4 19 21], [one' 3 22 24], [one' 2 25 27], [one' 1 28 30], [one' 8 31 33], [one' 7 35 38], [one' 6 40 42 true true] ])
    , ("pq7", [ [one' 1 0 4], [one' 8 7 9], [one' 7 10 12], [one' 6 13 15], [one' 5 16 18], [one' 4 20 22], [one' 3 23 25], [one' 2 26 28], [one' 1 30 32], [one' 8 33 36], [one' 7 37 40 true true] ])
    , ("pq8", [ [one' 1 0 4], [one' 8 5 8], [one' 7 9 11], [one' 6 12 14], [one' 5 15 17], [one' 4 19 21], [one' 3 22 24], [one' 2 25 27], [one' 1 28 32], [one' 8 33 36 true true] ]) ]
  match ordinary.find? (fun pair => pair.1 == key) with
  | some (_, queues) => some queues
  | none =>
      if key == "wifi" then
        some (if isClassic then [wifi.left, wifi.center, wifi.right] else [wifi.left, wifi.center, wifi.right])
      else
        none

end LnmaiCore.Simai
