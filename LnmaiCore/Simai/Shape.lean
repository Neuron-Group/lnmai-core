import LnmaiCore.Simai.Syntax
import LnmaiCore.Areas

namespace LnmaiCore.Simai

def SlideShape.kind (shape : SlideShape) : SlideKind :=
  match shape.canonical with
  | .line _ => .line
  | .circle _ => .circle
  | .v _ => .v
  | .turn _ => .turn
  | .pq _ => .pq
  | .ppqq _ => .ppqq
  | .s => .s
  | .wifi => .wifi

def SlideShape.relEnd (shape : SlideShape) : Option Nat :=
  match shape.canonical with
  | .line n | .circle n | .v n | .turn n | .pq n | .ppqq n => some n
  | .s | .wifi => none

def SlideShape.mirrored (shape : SlideShape) : Bool :=
  SlideSymmetry.isMirrored shape.symmetry

private def directShapeSymmetry : SlideSymmetry := SlideSymmetry.direct

private def mirroredShapeSymmetry : SlideSymmetry := SlideSymmetry.mirror

def mkCanonicalSlideShape? (kind : SlideKind) (relEnd : Option Nat) : Option CanonicalSlideShape :=
  match kind, relEnd with
  | .line, some n => if 2 ≤ n && n ≤ 8 then some (.line n) else none
  | .circle, some n => some (.circle n)
  | .v, some n => if n ≠ 5 then some (.v n) else none
  | .turn, some n => if 2 ≤ n && n ≤ 8 then some (.turn n) else none
  | .pq, some n => some (.pq n)
  | .ppqq, some n => some (.ppqq n)
  | .s, none => some .s
  | .wifi, none => some .wifi
  | _, _ => none

def mkCanonicalSlideShape (kind : SlideKind) (relEnd : Option Nat) (rawText : String) (message : String) : Except ParseError CanonicalSlideShape :=
  match mkCanonicalSlideShape? kind relEnd with
  | some shape => pure shape
  | none => Except.error { kind := .invalidEndPosition, rawText := rawText, message := message }

def mkCanonicalSlideShapeUnchecked (kind : SlideKind) (relEnd : Option Nat) : CanonicalSlideShape :=
  match kind, relEnd with
  | .line, some n => .line n
  | .circle, some n => .circle n
  | .v, some n => .v n
  | .turn, some n => .turn n
  | .pq, some n => .pq n
  | .ppqq, some n => .ppqq n
  | .s, _ => .s
  | .wifi, _ => .wifi
  | .line, none => .line 3
  | .circle, none => .circle 2
  | .v, none => .v 1
  | .turn, none => .turn 2
  | .pq, none => .pq 1
  | .ppqq, none => .ppqq 1

def mkSlideShape (canonical : CanonicalSlideShape) (symmetry : SlideSymmetry := SlideSymmetry.direct) : SlideShape :=
  { canonical := canonical, symmetry := symmetry }

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
  | 1 => 1 | 2 => 8 | 3 => 7 | 4 => 6
  | 5 => 5 | 6 => 4 | 7 => 3 | 8 => 2
  | n => n

private def mirrorRelEnd : Nat → Nat := mirrorKey

def canonicalRelEnd (sym : SlideSymmetry) (relEnd : Nat) : Nat :=
  if SlideSymmetry.isMirrored sym then mirrorRelEnd relEnd else relEnd

def baseRelEnd (sym : SlideSymmetry) (relEnd : Nat) : Nat :=
  canonicalRelEnd sym relEnd

def canonicalShapeKey : SlideShape → String
  | { canonical := .line n, .. } => s!"line{n}"
  | { canonical := .circle n, symmetry := sym } => s!"circle{baseRelEnd sym n}"
  | { canonical := .v n, .. } => s!"v{n}"
  | { canonical := .turn n, symmetry := sym } => s!"L{baseRelEnd sym n}"
  | { canonical := .pq n, symmetry := sym } => s!"pq{baseRelEnd sym n}"
  | { canonical := .ppqq n, symmetry := sym } => s!"ppqq{baseRelEnd sym n}"
  | { canonical := .s, .. } => "s"
  | { canonical := .wifi, .. } => "wifi"

def displayShapeKey (shape : SlideShape) : String :=
  let base := canonicalShapeKey shape
  if base = "wifi" || base = "" || !SlideSymmetry.isMirrored shape.symmetry then base else s!"-{base}"

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

def parseSlideBodyFromText (content : String) : Except ParseError ParsedSlideBody := do
  let cs := content.toList
  let startLane ← readStartLaneAt cs 0
  if content.contains '-' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .line, endArea := some endArea }
  else if content.contains '>' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .circleRight, endArea := some endArea }
  else if content.contains '<' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .circleLeft, endArea := some endArea }
  else if content.contains '^' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .circleUp, endArea := some endArea }
  else if content.contains 'v' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .v, endArea := some endArea }
  else if content.contains "pp" then
    let endArea ← readEndAreaAt cs 3
    pure { rawText := content, startLane := startLane, kind := .pp, endArea := some endArea }
  else if content.contains "qq" then
    let endArea ← readEndAreaAt cs 3
    pure { rawText := content, startLane := startLane, kind := .qq, endArea := some endArea }
  else if content.contains 'p' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .p, endArea := some endArea }
  else if content.contains 'q' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .q, endArea := some endArea }
  else if content.contains 's' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .s, endArea := some endArea }
  else if content.contains 'z' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .z, endArea := some endArea }
  else if content.contains 'V' then
    let turnArea ← readEndAreaAt cs 2
    let endArea ← readEndAreaAt cs 3
    pure { rawText := content, startLane := startLane, kind := .turn, turnArea := some turnArea, endArea := some endArea }
  else if content.contains 'w' then
    let endArea ← readEndAreaAt cs 2
    pure { rawText := content, startLane := startLane, kind := .wifi, endArea := some endArea }
  else
    Except.error { kind := .invalidShape, rawText := content, message := "unrecognized Simai slide shape" }

def solveSlideShape (body : ParsedSlideBody) : Except ParseError SlideShape := do
  match body.kind with
  | .line =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .line (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical directShapeSymmetry
  | .circleRight =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .circle (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical (if outerSlotIsUpperHalf body.startLane then directShapeSymmetry else mirroredShapeSymmetry)
  | .circleLeft =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .circle (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical (if !outerSlotIsUpperHalf body.startLane then directShapeSymmetry else mirroredShapeSymmetry)
  | .circleUp =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .circle (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical (if relEnd < 5 then directShapeSymmetry else mirroredShapeSymmetry)
  | .v =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .v (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical directShapeSymmetry
  | .pp =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .ppqq (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical directShapeSymmetry
  | .qq =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .ppqq (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical mirroredShapeSymmetry
  | .p =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .pq (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical directShapeSymmetry
  | .q =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      let canonical ← mkCanonicalSlideShape .pq (some relEnd) body.rawText "invalid end"
      pure <| mkSlideShape canonical mirroredShapeSymmetry
  | .s =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      if relEnd != 5 then
        Except.error { kind := .invalidEndPosition, rawText := body.rawText, message := "invalid end" }
      else
        let canonical ← mkCanonicalSlideShape .s none body.rawText "invalid end"
        pure <| mkSlideShape canonical directShapeSymmetry
  | .z =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      if relEnd != 5 then
        Except.error { kind := .invalidEndPosition, rawText := body.rawText, message := "invalid end" }
      else
        let canonical ← mkCanonicalSlideShape .s none body.rawText "invalid end"
        pure <| mkSlideShape canonical mirroredShapeSymmetry
  | .turn =>
      let turnArea := body.turnArea.getD .A1
      let endArea := body.endArea.getD .A1
      let turnRel ← relativeEndFromTyped body.startLane turnArea
      let endRel ← relativeEndFromTyped body.startLane endArea
      if turnRel == 7 then
        if endRel < 2 || endRel > 5 then
          Except.error { kind := .invalidTurnPosition, rawText := body.rawText, message := "invalid end" }
        else
          let canonical ← mkCanonicalSlideShape .turn (some endRel) body.rawText "invalid end"
          pure <| mkSlideShape canonical directShapeSymmetry
      else if turnRel == 3 then
        if endRel < 5 then
          Except.error { kind := .invalidTurnPosition, rawText := body.rawText, message := "invalid end" }
        else
          let canonical ← mkCanonicalSlideShape .turn (some endRel) body.rawText "invalid end"
          pure <| mkSlideShape canonical mirroredShapeSymmetry
      else
        Except.error { kind := .invalidTurnPosition, rawText := body.rawText, message := "invalid turn" }
  | .wifi =>
      let endArea := body.endArea.getD .A1
      let relEnd ← relativeEndFromTyped body.startLane endArea
      if relEnd != 5 then
        Except.error { kind := .invalidEndPosition, rawText := body.rawText, message := "invalid end" }
      else
        let canonical ← mkCanonicalSlideShape .wifi none body.rawText "invalid end"
        pure <| mkSlideShape canonical directShapeSymmetry

def detectShapeFromText (content : String) : Except ParseError SlideShape := do
  let body ← parseSlideBodyFromText content
  solveSlideShape body

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

def shapeKey (shape : SlideShape) : String :=
  displayShapeKey shape

end LnmaiCore.Simai
