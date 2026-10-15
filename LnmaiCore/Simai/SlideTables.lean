import Mathlib
import LnmaiCore.Areas
import LnmaiCore.Types
import LnmaiCore.Simai.Syntax
import LnmaiCore.Simai.Symmetry
import LnmaiCore.Simai.Shape
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

def transformAreaSpec (g : SlideSymmetry) (spec : SlideAreaSpec) : SlideAreaSpec :=
  let transformed := spec.targetAreas.map (actOnSensorArea g)
  { spec with targetAreas := transformed }

def mirrorAreaSpec (spec : SlideAreaSpec) : SlideAreaSpec :=
  transformAreaSpec SlideSymmetry.mirror spec

def mirrorJudgeQueues (queues : List (List SlideAreaSpec)) : List (List SlideAreaSpec) :=
  queues.map (fun queue => queue.map mirrorAreaSpec)

def transformJudgeQueues (g : SlideSymmetry) (queues : List (List SlideAreaSpec)) : List (List SlideAreaSpec) :=
  queues.map (fun queue => queue.map (transformAreaSpec g))

def rotateJudgeQueues (steps : Nat) (queues : List (List SlideAreaSpec)) : List (List SlideAreaSpec) :=
  queues.map (fun queue => queue.map (rotateAreaSpec steps))

def stripMirrorPrefix : String → String
  | "" => ""
  | s => if s.front = '-' then s.drop 1 |>.toString else s

def parseShapeKeySymmetry (shapeKey : String) : SlideSymmetry :=
  if shapeKey.startsWith "-" then SlideSymmetry.mirror else SlideSymmetry.direct

def judgeQueuesForShapeKey (shapeKey : String) (isClassic : Bool := false) : Option (List (List SlideAreaSpec)) :=
  let key := stripMirrorPrefix shapeKey
  let sym := parseShapeKeySymmetry shapeKey
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
    , ("line2", [ [one' .A1 0 3], [one' .A2 6 9 true true] ])
    , ("line3", [ [one' .A1 0 3], [one [.A2, .B2] 6 9 false false], [one' .A3 10 13 true true] ])
    , ("line4", [ [one' .A1 0 4], [one' .B2 6 9], [one' .B3 11 14], [one' .A4 15 18 true true] ])
    , ("line5", [ [one' .A1 0 4], [one' .B1 5 7], [one' .C 10 12], [one' .B5 13 16], [one' .A5 17 19 true true] ])
    , ("line6", [ [one' .A1 0 4], [one' .B8 6 9], [one' .B7 11 14], [one' .A6 15 18 true true] ])
    , ("line7", [ [one' .A1 0 3], [one [.A8, .B8] 6 9 false false], [one' .A7 10 13 true true] ])
    , ("line8", [ [one' .A1 0 3], [one' .A1 6 9 true true] ])
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
    , ("L2", [ [one' .A1 0 3], [one [.B8, .A8] 6 10 false false], [one' .A7 12 19], [one' .B8 21 24], [one' .B1 25 28], [one' .A2 29 32 true true] ])
    , ("L3", [ [one' .A1 0 3], [one [.B8, .A8] 6 10 false false], [one' .A7 12 18], [one' .B7 20 22], [one' .C 25 27], [one' .B3 28 31], [one' .A3 32 34 true true] ])
    , ("L4", [ [one' .A1 0 3], [one [.B8, .A8] 6 10 false false], [one' .A7 12 19], [one' .B6 21 24], [one' .B5 25 28], [one' .A4 29 32 true true] ])
    , ("L5", [ [one' .A1 0 3], [one [.B8, .A8] 6 10 false false], [one' .A7 12 18], [one [.B6, .A6] 21 24 false false], [one' .A5 27 28 true true] ])
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
  base.map (transformJudgeQueues sym)

def judgeQueuesForShape (shape : SlideShape) (isClassic : Bool := false) : Option (List (List SlideAreaSpec)) :=
  let base :=
    match judgeQueuesForShapeKey (canonicalShapeKey shape) isClassic with
    | some queues => some queues
    | none => none
  base.map (transformJudgeQueues shape.symmetry)

/- MajDataPlay's connected-slide timing uses the number of visual path
   children in each slide prefab, rather than the number of judge-queue
   entries. Keep this separate from judge queues: queues are runtime input,
   while these weights are only for proportional whole-chain timing. -/
def slideBarCountForShapeKey (shapeKey : String) : Option Nat :=
  match stripMirrorPrefix shapeKey with
  | "circle1" => some 64
  | "circle2" => some 8
  | "circle3" => some 16
  | "circle4" => some 24
  | "circle5" => some 32
  | "circle6" => some 40
  | "circle7" => some 48
  | "circle8" => some 56
  | "line3" => some 14
  | "line4" => some 19
  | "line5" => some 20
  | "line6" => some 19
  | "line7" => some 14
  | "v1" => some 21
  | "v2" | "v3" | "v4" | "v6" | "v7" | "v8" => some 20
  | "ppqq1" => some 36
  | "ppqq2" => some 29
  | "ppqq3" => some 23
  | "ppqq4" | "ppqq5" => some 50
  | "ppqq6" => some 49
  | "ppqq7" => some 47
  | "ppqq8" => some 42
  | "pq1" => some 34
  | "pq2" => some 31
  | "pq3" => some 28
  | "pq4" => some 25
  | "pq5" => some 22
  | "pq6" => some 43
  | "pq7" => some 41
  | "pq8" => some 37
  | "L2" => some 33
  | "L3" => some 35
  | "L4" => some 33
  | "L5" => some 29
  | "s" => some 31
  | _ => none

def slideBarCountForShape (shape : SlideShape) : Option Nat :=
  slideBarCountForShapeKey (canonicalShapeKey shape)

-- SlideDrop.InitializeSlideGroup and WifiDrop.Init use these fractions of
-- Length for LastWaitTimeSec, and judge at StartTiming + Length * (1 - fraction).
-- Store the reference decimal constants as integer millionths.
def slideLastWaitRatioForShapeKey (shapeKey : String) (isClassic : Bool := false) :
    Option Rat :=
  let parts : Option (Nat × Nat) :=
    match stripMirrorPrefix shapeKey with
    | "circle1" => some (58000, 65500)
    | "circle2" => some (465000, 505000)
    | "circle3" => some (233000, 263000)
    | "circle4" => some (155000, 175000)
    | "circle5" => some (116000, 131000)
    | "circle6" => some (93000, 108000)
    | "circle7" => some (78000, 85500)
    | "circle8" => some (66000, 76000)
    | "line3" | "line7" => some (182000, 277000)
    | "line4" | "line6" => some (190000, 230000)
    | "line5" => some (152000, 167000)
    | "v1" => some (185000, 205000)
    | "v2" => some (150000, 170000)
    | "v3" | "v4" | "v6" | "v7" => some (158000, 178000)
    | "v8" => some (154000, 174000)
    | "ppqq1" => some (65000, 95000)
    | "ppqq2" => some (86000, 131000)
    | "ppqq3" => some (157000, 197000)
    | "ppqq4" => some (65000, 72500)
    | "ppqq5" => some (65000, 75000)
    | "ppqq6" => some (67000, 77000)
    | "ppqq7" => some (79000, 94000)
    | "ppqq8" => some (62600, 80100)
    | "L2" => some (100000, 120000)
    | "L3" => some (104000, 114000)
    | "L4" => some (98000, 123000)
    | "L5" => some (105000, 150000)
    | "s" => some (130000, 155000)
    | "pq1" => some (95000, 115000)
    | "pq2" => some (112000, 137000)
    | "pq3" => some (125000, 150000)
    | "pq4" => some (139000, 169000)
    | "pq5" => some (160000, 192500)
    | "pq6" => some (80000, 97500)
    | "pq7" => some (84000, 104000)
    | "pq8" => some (89500, 109500)
    | "wifi" => some (162870, 162870)
    | _ => none
  parts.map (fun pair => ((if isClassic then pair.2 else pair.1 : Nat) : Rat) / 1000000)

def slideLastWaitRatioForShape (shape : SlideShape) (isClassic : Bool := false) :
    Option Rat :=
  slideLastWaitRatioForShapeKey (canonicalShapeKey shape) isClassic

end LnmaiCore.Simai
