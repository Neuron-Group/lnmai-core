import LnmaiCore.Simai.Shape

namespace LnmaiCore.Simai

private def sanitizeSlideText (rawText : String) : String :=
  String.ofList <| rawText.toList.filter (fun c =>
    c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$')

private def fallbackShapeFromText (rawText : String) (startPos endPos : Nat) : Except ParseError SlideShape := do
  let relEnd := relativeEndPos startPos endPos
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

private def fallbackJustType (rawText : String) (startPos endPos : Nat) : Bool :=
  if rawText.contains '>' then
    isUpperHalf startPos
  else if rawText.contains '<' then
    !isUpperHalf startPos
  else if rawText.contains '^' then
    relativeEndPos startPos endPos < 4
  else if rawText.contains 'V' then
    isRightHalf endPos
  else if rawText.contains 'w' then
    isUpperHalf endPos
  else
    isRightHalf endPos

def parseSlideNote (rawText : String) (startPos endPos : Nat) : Except ParseError SlideNoteSemantics := do
  let sanitized := sanitizeSlideText rawText
  let shape ←
    match detectShapeFromText sanitized with
    | .ok shape => pure shape
    | .error _ => fallbackShapeFromText sanitized startPos endPos
  let isJustRight :=
    match detectJustType sanitized with
    | .ok value => value
    | .error _ => fallbackJustType sanitized startPos endPos
  pure {
    rawText := sanitized,
    startPos := startPos,
    endPos := endPos,
    shape := shape,
    isJustRight := isJustRight
  }

def parseSlideShapeText (rawText : String) : Except ParseError SlideShape :=
  let sanitized := sanitizeSlideText rawText
  match detectShapeFromText sanitized with
  | .ok shape => pure shape
  | .error _ => fallbackShapeFromText sanitized 1 1

def parseSlideJustText (rawText : String) : Except ParseError Bool :=
  let sanitized := sanitizeSlideText rawText
  match detectJustType sanitized with
  | .ok value => pure value
  | .error _ => pure <| fallbackJustType sanitized 1 1

def parseSlideTimingPoint (timingSec bpm hSpeed : Float) (rawNotes : List String) : Except ParseError TimingPointSemantics := do
  let notes ← rawNotes.mapM (fun raw => do
    let shape ← detectShapeFromText raw
    let just ← detectJustType raw
    let cs := raw.toList
    let startPos ←
      match cs.head? with
      | some c =>
          match digitToNat? c with
          | some n => pure n
          | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "missing start digit" }
      | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "empty slide text" }
    let endPos ←
      if raw.contains 'V' then
        match getAt? cs 3 with
        | some c =>
            match digitToNat? c with
            | some n => pure n
            | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "invalid end digit" }
        | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "missing end digit" }
      else if raw.contains "pp" || raw.contains "qq" then
        match getAt? cs 3 with
        | some c =>
            match digitToNat? c with
            | some n => pure n
            | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "invalid end digit" }
        | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "missing end digit" }
      else
        match getAt? cs 2 with
        | some c =>
            match digitToNat? c with
            | some n => pure n
            | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "invalid end digit" }
        | none => Except.error { kind := .invalidSyntax, rawText := raw, message := "missing end digit" }
    pure {
      rawText := raw,
      startPos := startPos,
      endPos := endPos,
      shape := shape,
      isJustRight := just
    })
  pure { timingSec := timingSec, bpm := bpm, hSpeed := hSpeed, notes := notes }

end LnmaiCore.Simai
