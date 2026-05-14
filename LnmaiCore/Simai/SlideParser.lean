import LnmaiCore.Simai.Shape
import LnmaiCore.Time

namespace LnmaiCore.Simai

private def relativeEndPosCompat (startSlot endZone : OuterSlot) : Nat :=
  (((endZone.toIndex + 1 - 1) + 8 - startSlot.toIndex) % 8) + 1

private def isRightHalfCompat (zone : OuterSlot) : Bool :=
  zone.toIndex < 4

private def isUpperHalfCompat : OuterSlot → Bool
  | .S7 | .S8 | .S1 | .S2 => true
  | _ => false

private def sanitizeSlideText (rawText : String) : String :=
  String.ofList <| rawText.toList.filter (fun c =>
    c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$')

private def outerSlotForEndArea (endArea : SensorArea) : Except ParseError OuterSlot :=
  match endArea.toOuterSlot? with
  | some zone => pure zone
  | none => Except.error { kind := .invalidSyntax, rawText := "", message := "invalid slide end position" }

private def fallbackShapeFromText (rawText : String) (startSlot : OuterSlot) (endArea : SensorArea) : Except ParseError SlideShape := do
  let endZone ← outerSlotForEndArea endArea
  let relEnd := relativeEndPosCompat startSlot endZone
  if rawText.contains '-' then
    pure { kind := .line, relEnd := some relEnd, mirrored := false }
  else if rawText.contains '>' then
    pure { kind := .circle, relEnd := some relEnd, mirrored := false }
  else if rawText.contains '<' then
    pure { kind := .circle, relEnd := some relEnd, mirrored := true }
  else if rawText.contains '^' then
    pure { kind := .circle, relEnd := some relEnd, mirrored := relEnd ≥ 5 }
  else if rawText.contains 'v' then
    pure { kind := .v, relEnd := some relEnd, mirrored := false }
  else if rawText.contains "pp" then
    pure { kind := .ppqq, relEnd := some relEnd, mirrored := false }
  else if rawText.contains "qq" then
    pure { kind := .ppqq, relEnd := some relEnd, mirrored := true }
  else if rawText.contains 'p' then
    pure { kind := .pq, relEnd := some relEnd, mirrored := false }
  else if rawText.contains 'q' then
    pure { kind := .pq, relEnd := some relEnd, mirrored := true }
  else if rawText.contains 's' then
    pure { kind := .s, relEnd := none, mirrored := false }
  else if rawText.contains 'z' then
    pure { kind := .s, relEnd := none, mirrored := true }
  else if rawText.contains 'V' then
    pure { kind := .turn, relEnd := some relEnd, mirrored := false }
  else if rawText.contains 'w' then
    pure { kind := .wifi, relEnd := none, mirrored := false }
  else
    Except.error { kind := .invalidShape, rawText := rawText, message := "unrecognized Simai slide shape" }

private def fallbackJustType (rawText : String) (startSlot : OuterSlot) (endArea : SensorArea) : Except ParseError Bool := do
  let endZone ← outerSlotForEndArea endArea
  if rawText.contains '>' then
    pure <| isUpperHalfCompat startSlot
  else if rawText.contains '<' then
    pure <| !isUpperHalfCompat startSlot
  else if rawText.contains '^' then
    pure <| relativeEndPosCompat startSlot endZone < 4
  else if rawText.contains 'V' then
    pure <| isRightHalfCompat endZone
  else if rawText.contains 'w' then
    pure <| isUpperHalfCompat endZone
  else
    pure <| isRightHalfCompat endZone

def parseSlideNote (rawText : String) (startSlot : OuterSlot) (endArea : SensorArea) : Except ParseError SlideNoteSemantics := do
  let sanitized := sanitizeSlideText rawText
  let shape ←
    match detectShapeFromText sanitized with
    | .ok shape => pure shape
    | .error _ => fallbackShapeFromText sanitized startSlot endArea
  let isJustRight :=
    match detectJustType sanitized with
    | .ok value => pure value
    | .error _ => fallbackJustType sanitized startSlot endArea
  let isJustRight ← isJustRight
  pure {
    rawText := sanitized,
    startSlot := startSlot,
    endArea := endArea,
    shape := shape,
    isJustRight := isJustRight
  }

def parseTerminalEndArea (rawText : String) : Except ParseError SensorArea := do
  let cs := rawText.toList
  if rawText.contains 'V' then
    parseEndAreaAt cs 3
  else if rawText.contains "pp" || rawText.contains "qq" then
    parseEndAreaAt cs 3
  else
    parseEndAreaAt cs 2

def parseSlideShapeText (rawText : String) : Except ParseError SlideShape :=
  let sanitized := sanitizeSlideText rawText
  match detectShapeFromText sanitized with
  | .ok shape => pure shape
  | .error _ => do
      let cs := sanitized.toList
      let startSlot ← parseStartLaneAt cs 0
      let endArea ← parseTerminalEndArea sanitized
      fallbackShapeFromText sanitized startSlot endArea

def parseSlideJustText (rawText : String) : Except ParseError Bool :=
  let sanitized := sanitizeSlideText rawText
  match detectJustType sanitized with
  | .ok value => pure value
  | .error _ => do
      let cs := sanitized.toList
      let startSlot ← parseStartLaneAt cs 0
      let endArea ← parseTerminalEndArea sanitized
      fallbackJustType sanitized startSlot endArea

def parseSlideTimingPoint (timing : TimePoint) (bpm hSpeed : Rat) (rawNotes : List String) : Except ParseError TimingPointSemantics := do
  let notes ← rawNotes.mapM (fun raw => do
    let shape ← detectShapeFromText raw
    let just ← detectJustType raw
    let cs := raw.toList
    let startSlot ← parseStartLaneAt cs 0
    let endArea ← parseTerminalEndArea raw
    pure {
      rawText := raw,
      startSlot := startSlot,
      endArea := endArea,
      shape := shape,
      isJustRight := just
    })
  pure { timing := timing, bpm := bpm, hSpeed := hSpeed, notes := notes }

end LnmaiCore.Simai
