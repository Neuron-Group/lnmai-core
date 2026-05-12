import LnmaiCore.Simai.Syntax

namespace LnmaiCore.Simai

def getAt? (xs : List Char) : Nat → Option Char
  | 0 => xs.head?
  | n + 1 => xs.tail?.bind (fun tail => getAt? tail n)

def digitToNat? : Char → Option Nat
  | '1' => some 1 | '2' => some 2 | '3' => some 3 | '4' => some 4
  | '5' => some 5 | '6' => some 6 | '7' => some 7 | '8' => some 8
  | _ => none

def mirrorKey : Nat → Nat
  | 1 => 8 | 2 => 7 | 3 => 6 | 4 => 5
  | 5 => 4 | 6 => 3 | 7 => 2 | 8 => 1
  | n => n

def isRightHalf : Nat → Bool
  | 1 | 2 | 3 | 4 => true
  | _ => false

def isUpperHalf : Nat → Bool
  | 7 | 8 | 1 | 2 => true
  | _ => false

def relativeEndPos (startPos endPos : Nat) : Nat :=
  (((endPos - 1) + 8 - (startPos - 1)) % 8) + 1

private def readDigitAt (content : List Char) (index : Nat) : Except ParseError Nat :=
  match getAt? content index with
  | some c =>
      match digitToNat? c with
      | some n => Except.ok n
      | none => Except.error { kind := .invalidSyntax, rawText := String.ofList content, message := s!"expected digit at {index}" }
  | none => Except.error { kind := .invalidSyntax, rawText := String.ofList content, message := s!"missing digit at {index}" }

def detectShapeFromText (content : String) : Except ParseError SlideShape := do
  let cs := content.toList
  if content.contains '-' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if relEnd < 3 || relEnd > 7 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "- slide end must be 3..7" }
    else
      pure { kind := .line, relEnd := some relEnd, mirrored := false }
  else if content.contains '>' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if isUpperHalf startPos then
      pure { kind := .circle, relEnd := some relEnd, mirrored := false }
    else
      pure { kind := .circle, relEnd := some (mirrorKey relEnd), mirrored := true }
  else if content.contains '<' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if !isUpperHalf startPos then
      pure { kind := .circle, relEnd := some relEnd, mirrored := false }
    else
      pure { kind := .circle, relEnd := some (mirrorKey relEnd), mirrored := true }
  else if content.contains '^' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if relEnd == 1 || relEnd == 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "^ slide end is invalid" }
    else if relEnd < 5 then
      pure { kind := .circle, relEnd := some relEnd, mirrored := false }
    else
      pure { kind := .circle, relEnd := some (mirrorKey relEnd), mirrored := true }
  else if content.contains 'v' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if relEnd == 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "v slide end is invalid" }
    else
      pure { kind := .v, relEnd := some relEnd, mirrored := false }
  else if content.contains "pp" then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 3
    let relEnd := relativeEndPos startPos endPos
    pure { kind := .ppqq, relEnd := some relEnd, mirrored := false }
  else if content.contains "qq" then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 3
    let relEnd := relativeEndPos startPos endPos
    pure { kind := .ppqq, relEnd := some (mirrorKey relEnd), mirrored := true }
  else if content.contains 'p' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    pure { kind := .pq, relEnd := some relEnd, mirrored := false }
  else if content.contains 'q' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    pure { kind := .pq, relEnd := some (mirrorKey relEnd), mirrored := true }
  else if content.contains 's' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if relEnd != 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "s slide end must be 5" }
    else
      pure { kind := .s, relEnd := none, mirrored := false }
  else if content.contains 'z' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if relEnd != 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "z slide end must be 5" }
    else
      pure { kind := .s, relEnd := none, mirrored := true }
  else if content.contains 'V' then
    let startPos ← readDigitAt cs 0
    let turnPos ← readDigitAt cs 2
    let endPos ← readDigitAt cs 3
    let turnRel := relativeEndPos startPos turnPos
    let endRel := relativeEndPos startPos endPos
    if turnRel == 7 then
      if endRel < 2 || endRel > 5 then
        Except.error { kind := .invalidTurnPosition, rawText := content, message := "V slide end invalid" }
      else
        pure { kind := .turn, relEnd := some endRel, mirrored := false }
    else if turnRel == 3 then
      if endRel < 5 then
        Except.error { kind := .invalidTurnPosition, rawText := content, message := "V slide end invalid" }
      else
        pure { kind := .turn, relEnd := some (mirrorKey endRel), mirrored := true }
    else
      Except.error { kind := .invalidTurnPosition, rawText := content, message := "V turn must be one key apart" }
  else if content.contains 'w' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    if relEnd != 5 then
      Except.error { kind := .invalidEndPosition, rawText := content, message := "wifi end must be 5" }
    else
      pure { kind := .wifi, relEnd := none, mirrored := false }
  else
    Except.error { kind := .invalidShape, rawText := content, message := "unrecognized Simai slide shape" }

def detectJustType (content : String) : Except ParseError Bool := do
  let cs := content.toList
  if content.contains '>' then
    let startPos ← readDigitAt cs 0
    let _endPos ← readDigitAt cs 2
    Except.ok (isUpperHalf startPos)
  else if content.contains '<' then
    let startPos ← readDigitAt cs 0
    let _endPos ← readDigitAt cs 2
    Except.ok (!isUpperHalf startPos)
  else if content.contains '^' then
    let startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    let relEnd := relativeEndPos startPos endPos
    Except.ok (relEnd < 4)
  else if content.contains 'V' then
    let _startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 3
    Except.ok (isRightHalf endPos)
  else if content.contains 'w' then
    let _startPos ← readDigitAt cs 0
    let endPos ← readDigitAt cs 2
    pure (isUpperHalf endPos)
  else
    let endPos ← if content.contains "qq" || content.contains "pp" then
      readDigitAt cs 3
    else
      readDigitAt cs 2
    pure (isRightHalf endPos)

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
