import LnmaiCore.Simai.Shape
import LnmaiCore.Time

namespace LnmaiCore.Simai

private def sanitizeSlideText (rawText : String) : String :=
  String.ofList <| rawText.toList.filter (fun c =>
    c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$')

def parseSlideNote (rawText : String) (startSlot : OuterSlot) (endArea : SensorArea) : Except ParseError SlideNoteSemantics := do
  let sanitized := sanitizeSlideText rawText
  let shape ← parseSlideBodyFromText sanitized >>= solveSlideShape
  let isJustRight ← detectJustType sanitized
  pure {
    rawText := sanitized,
    startSlot := startSlot,
    endArea := endArea,
    shape := shape,
    isJustRight := isJustRight
  }

def parseSlideNoteFromBody (rawText : String) (body : ParsedSlideBody) (endArea : SensorArea) : Except ParseError SlideNoteSemantics := do
  let shape ← solveSlideShape body
  let isJustRight ← detectJustType rawText
  pure {
    rawText := rawText,
    startSlot := body.startLane,
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
  parseSlideBodyFromText sanitized >>= solveSlideShape

def parseSlideJustText (rawText : String) : Except ParseError Bool :=
  let sanitized := sanitizeSlideText rawText
  detectJustType sanitized

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
