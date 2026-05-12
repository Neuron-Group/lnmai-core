import LnmaiCore.Simai.Shape

namespace LnmaiCore.Simai

def parseSlideNote (rawText : String) (startPos endPos : Nat) : Except ParseError SlideNoteSemantics := do
  let shape ← detectShapeFromText rawText
  let isJustRight ← detectJustType rawText
  pure {
    rawText := rawText,
    startPos := startPos,
    endPos := endPos,
    shape := shape,
    isJustRight := isJustRight
  }

def parseSlideShapeText (rawText : String) : Except ParseError SlideShape :=
  detectShapeFromText rawText

def parseSlideJustText (rawText : String) : Except ParseError Bool :=
  detectJustType rawText

def parseTimingPointSlides (timingSec bpm hSpeed : Float) (rawNotes : List String) : Except ParseError TimingPointSemantics := do
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
