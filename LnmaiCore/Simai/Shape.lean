import LnmaiCore.Simai.Syntax
import LnmaiCore.Areas

namespace LnmaiCore.Simai

def getAt? (xs : List Char) : Nat → Option Char
  | 0 => xs.head?
  | n + 1 => xs.tail?.bind (fun tail => getAt? tail n)

def digitToNat? : Char → Option Nat
  | '1' => some 1 | '2' => some 2 | '3' => some 3 | '4' => some 4
  | '5' => some 5 | '6' => some 6 | '7' => some 7 | '8' => some 8
  | _ => none

private def keyPosToOuterSlot? (pos : Nat) : Option OuterSlot :=
  OuterSlot.ofIndex? (pos - 1)

private def keyPosToOuterSensorArea? (pos : Nat) : Option SensorArea :=
  (OuterSlot.ofIndex? (pos - 1)).map OuterSlot.toOuterSensorArea

private def mirrorKey : Nat → Nat
  | 1 => 8 | 2 => 7 | 3 => 6 | 4 => 5
  | 5 => 4 | 6 => 3 | 7 => 2 | 8 => 1
  | n => n

private def mirrorRelEnd : Nat → Nat := mirrorKey

private def relativeEndPos (startPos endPos : Nat) : Nat :=
  (((endPos - 1) + 8 - (startPos - 1)) % 8) + 1

private def outerSlotIsRightHalf (slot : OuterSlot) : Bool :=
  slot.toIndex < 4

private def outerSlotIsUpperHalf (slot : OuterSlot) : Bool :=
  match slot with
  | .S7 | .S8 | .S1 | .S2 => true
  | _ => false

private def relativeEndFromTyped (startLane : OuterSlot) (endArea : SensorArea) : Except ParseError Nat :=
  match endArea.toOuterSlot? with
  | some endZone => pure <| relativeEndPos (startLane.toIndex + 1) (endZone.toIndex + 1)
  | none => Except.error { kind := .invalidEndPosition, rawText := "", message := "slide end must be on outer A-ring" }

private def readDigitAt (content : List Char) (index : Nat) : Except ParseError Nat :=
  match getAt? content index with
  | some c =>
      match digitToNat? c with
      | some n => Except.ok n
      | none => Except.error { kind := .invalidSyntax, rawText := String.ofList content, message := s!"expected digit at {index}" }
  | none => Except.error { kind := .invalidSyntax, rawText := String.ofList content, message := s!"missing digit at {index}" }

private def readStartLaneAt (content : List Char) (index : Nat) : Except ParseError OuterSlot := do
  let pos ← readDigitAt content index
  match keyPosToOuterSlot? pos with
  | some zone => pure zone
  | none => Except.error { kind := .invalidSyntax, rawText := String.ofList content, message := s!"invalid start lane at {index}" }

private def readEndAreaAt (content : List Char) (index : Nat) : Except ParseError SensorArea := do
  let pos ← readDigitAt content index
  match keyPosToOuterSensorArea? pos with
  | some area => pure area
  | none => Except.error { kind := .invalidSyntax, rawText := String.ofList content, message := s!"invalid end area at {index}" }

def detectShapeFromText (content : String) : Except ParseError SlideShape := do
  let cs := content.toList
  if content.contains '-' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if relEnd < 3 || relEnd > 7 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "- slide end must be 3..7" }
    else
      pure { kind := .line, relEnd := some relEnd, mirrored := false }
  else if content.contains '>' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if outerSlotIsUpperHalf startLane then
      pure { kind := .circle, relEnd := some relEnd, mirrored := false }
    else
      pure { kind := .circle, relEnd := some (mirrorRelEnd relEnd), mirrored := true }
  else if content.contains '<' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if !outerSlotIsUpperHalf startLane then
      pure { kind := .circle, relEnd := some relEnd, mirrored := false }
    else
      pure { kind := .circle, relEnd := some (mirrorRelEnd relEnd), mirrored := true }
  else if content.contains '^' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if relEnd == 1 || relEnd == 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "^ slide end is invalid" }
    else if relEnd < 5 then
      pure { kind := .circle, relEnd := some relEnd, mirrored := false }
    else
      pure { kind := .circle, relEnd := some (mirrorRelEnd relEnd), mirrored := true }
  else if content.contains 'v' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if relEnd == 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "v slide end is invalid" }
    else
      pure { kind := .v, relEnd := some relEnd, mirrored := false }
  else if content.contains "pp" then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 3
    let relEnd ← relativeEndFromTyped startLane endArea
    pure { kind := .ppqq, relEnd := some relEnd, mirrored := false }
  else if content.contains "qq" then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 3
    let relEnd ← relativeEndFromTyped startLane endArea
    pure { kind := .ppqq, relEnd := some (mirrorRelEnd relEnd), mirrored := true }
  else if content.contains 'p' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    pure { kind := .pq, relEnd := some relEnd, mirrored := false }
  else if content.contains 'q' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    pure { kind := .pq, relEnd := some (mirrorRelEnd relEnd), mirrored := true }
  else if content.contains 's' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if relEnd != 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "s slide end must be 5" }
    else
      pure { kind := .s, relEnd := none, mirrored := false }
  else if content.contains 'z' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if relEnd != 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "z slide end must be 5" }
    else
      pure { kind := .s, relEnd := none, mirrored := true }
  else if content.contains 'V' then
    let startLane ← readStartLaneAt cs 0
    let turnArea ← readEndAreaAt cs 2
    let endArea ← readEndAreaAt cs 3
    let turnRel ← relativeEndFromTyped startLane turnArea
    let endRel ← relativeEndFromTyped startLane endArea
    if turnRel == 7 then
      if endRel < 2 || endRel > 5 then
        Except.error { kind := .invalidTurnPosition, rawText := content, message := "V slide end invalid" }
      else
        pure { kind := .turn, relEnd := some endRel, mirrored := false }
    else if turnRel == 3 then
      if endRel < 5 then
        Except.error { kind := .invalidTurnPosition, rawText := content, message := "V slide end invalid" }
      else
        pure { kind := .turn, relEnd := some (mirrorRelEnd endRel), mirrored := true }
    else
      Except.error { kind := .invalidTurnPosition, rawText := content, message := "V turn must be one key apart" }
  else if content.contains 'w' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    if relEnd != 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "wifi end must be 5" }
    else
      pure { kind := .wifi, relEnd := none, mirrored := false }
  else
    Except.error { kind := .invalidShape, rawText := content, message := "unrecognized Simai slide shape" }

def detectJustType (content : String) : Except ParseError Bool := do
  let cs := content.toList
  if content.contains '>' then
    let startLane ← readStartLaneAt cs 0
    let _endArea ← readEndAreaAt cs 2
    Except.ok (outerSlotIsUpperHalf startLane)
  else if content.contains '<' then
    let startLane ← readStartLaneAt cs 0
    let _endArea ← readEndAreaAt cs 2
    Except.ok (!outerSlotIsUpperHalf startLane)
  else if content.contains '^' then
    let startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let relEnd ← relativeEndFromTyped startLane endArea
    Except.ok (relEnd < 4)
  else if content.contains 'V' then
    let _startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 3
    let some endZone := endArea.toOuterSlot?
      | Except.error { kind := .invalidEndPosition, rawText := content, message := "slide end must be on outer A-ring" }
    Except.ok (outerSlotIsRightHalf endZone)
  else if content.contains 'w' then
    let _startLane ← readStartLaneAt cs 0
    let endArea ← readEndAreaAt cs 2
    let some endZone := endArea.toOuterSlot?
      | Except.error { kind := .invalidEndPosition, rawText := content, message := "slide end must be on outer A-ring" }
    pure (outerSlotIsUpperHalf endZone)
  else
    let endArea ← if content.contains "qq" || content.contains "pp" then
      readEndAreaAt cs 3
    else
      readEndAreaAt cs 2
    let some endZone := endArea.toOuterSlot?
      | Except.error { kind := .invalidEndPosition, rawText := content, message := "slide end must be on outer A-ring" }
    pure (outerSlotIsRightHalf endZone)

def parseStartLaneAt (content : List Char) (index : Nat) : Except ParseError OuterSlot :=
  readStartLaneAt content index

def parseEndAreaAt (content : List Char) (index : Nat) : Except ParseError SensorArea :=
  readEndAreaAt content index

def shapeKey : SlideShape → String
  | { kind := .line, relEnd := some n, mirrored := false } => s!"line{n}"
  | { kind := .circle, relEnd := some n, mirrored := false } => s!"circle{n}"
  | { kind := .circle, relEnd := some n, mirrored := true } => s!"-circle{n}"
  | { kind := .v, relEnd := some n, mirrored := false } => s!"v{n}"
  | { kind := .turn, relEnd := some n, mirrored := false } => s!"L{n}"
  | { kind := .turn, relEnd := some n, mirrored := true } => s!"-L{n}"
  | { kind := .pq, relEnd := some n, mirrored := false } => s!"pq{n}"
  | { kind := .pq, relEnd := some n, mirrored := true } => s!"-pq{n}"
  | { kind := .ppqq, relEnd := some n, mirrored := false } => s!"ppqq{n}"
  | { kind := .ppqq, relEnd := some n, mirrored := true } => s!"-ppqq{n}"
  | { kind := .s, mirrored := false, .. } => "s"
  | { kind := .s, mirrored := true, .. } => "-s"
  | { kind := .wifi, .. } => "wifi"
  | _ => ""

end LnmaiCore.Simai
