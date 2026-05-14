import LnmaiCore.Simai.Shape

namespace LnmaiCore.Simai

private def relativeEndPosCompat (startLane endZone : ButtonZone) : Nat :=
  (((endZone.toIndex + 1 - 1) + 8 - startLane.toIndex) % 8) + 1

private def isRightHalfCompat (zone : ButtonZone) : Bool :=
  zone.toIndex < 4

private def isUpperHalfCompat : ButtonZone → Bool
  | .K7 | .K8 | .K1 | .K2 => true
  | _ => false

private def sanitizeSlideText (rawText : String) : String :=
  String.ofList <| rawText.toList.filter (fun c =>
    c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$')

private def outerButtonZoneForEndArea (endArea : SensorArea) : Except ParseError ButtonZone :=
  match endArea.toOuterButtonZone? with
  | some zone => pure zone
  | none => Except.error { kind := .invalidSyntax, rawText := "", message := "invalid slide end position" }

private def fallbackShapeFromText (rawText : String) (startLane : ButtonZone) (endArea : SensorArea) : Except ParseError SlideShape := do
  let endZone ← outerButtonZoneForEndArea endArea
  let relEnd := relativeEndPosCompat startLane endZone
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

private def fallbackJustType (rawText : String) (startLane : ButtonZone) (endArea : SensorArea) : Except ParseError Bool := do
  let endZone ← outerButtonZoneForEndArea endArea
  if rawText.contains '>' then
    pure <| isUpperHalfCompat startLane
  else if rawText.contains '<' then
    pure <| !isUpperHalfCompat startLane
  else if rawText.contains '^' then
    pure <| relativeEndPosCompat startLane endZone < 4
  else if rawText.contains 'V' then
    pure <| isRightHalfCompat endZone
  else if rawText.contains 'w' then
    pure <| isUpperHalfCompat endZone
  else
    pure <| isRightHalfCompat endZone

def parseSlideNote (rawText : String) (startLane : ButtonZone) (endArea : SensorArea) : Except ParseError SlideNoteSemantics := do
  let sanitized := sanitizeSlideText rawText
  let shape ←
    match detectShapeFromText sanitized with
    | .ok shape => pure shape
    | .error _ => fallbackShapeFromText sanitized startLane endArea
  let isJustRight :=
    match detectJustType sanitized with
    | .ok value => pure value
    | .error _ => fallbackJustType sanitized startLane endArea
  let isJustRight ← isJustRight
  pure {
    rawText := sanitized,
    startLane := startLane,
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
      let startLane ← parseStartLaneAt cs 0
      let endArea ← parseTerminalEndArea sanitized
      fallbackShapeFromText sanitized startLane endArea

def parseSlideJustText (rawText : String) : Except ParseError Bool :=
  let sanitized := sanitizeSlideText rawText
  match detectJustType sanitized with
  | .ok value => pure value
  | .error _ => do
      let cs := sanitized.toList
      let startLane ← parseStartLaneAt cs 0
      let endArea ← parseTerminalEndArea sanitized
      fallbackJustType sanitized startLane endArea

def parseSlideTimingPoint (timingSec bpm hSpeed : Float) (rawNotes : List String) : Except ParseError TimingPointSemantics := do
  let notes ← rawNotes.mapM (fun raw => do
    let shape ← detectShapeFromText raw
    let just ← detectJustType raw
    let cs := raw.toList
    let startLane ← parseStartLaneAt cs 0
    let endArea ← parseTerminalEndArea raw
    pure {
      rawText := raw,
      startLane := startLane,
      endArea := endArea,
      shape := shape,
      isJustRight := just
    })
  pure { timingSec := timingSec, bpm := bpm, hSpeed := hSpeed, notes := notes }

end LnmaiCore.Simai
