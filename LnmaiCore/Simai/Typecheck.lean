import LnmaiCore.Simai.Syntax
import LnmaiCore.Simai.SlideParser

namespace LnmaiCore.Simai

structure TypedSlidePart where
  token : RawNoteToken
  semantics : SlideNoteSemantics
deriving Inhabited, Repr

inductive TypedSlideExpr where
  | single (part : TypedSlidePart)
  | conn (parts : List TypedSlidePart)
deriving Inhabited, Repr

private def typedSlidePart? (token : RawNoteToken) : Except ParseError TypedSlidePart := do
  let slot ←
    match token.slot with
    | some slot => pure slot
    | none => Except.error { kind := .invalidSyntax, rawText := token.rawText, message := "slide token is missing a start slot" }
  let endArea ← parseTerminalEndArea token.rawText
  let semantics ← parseSlideNote token.rawText slot endArea
  pure { token := token, semantics := semantics }

private def wifiConnError (parts : List TypedSlidePart) : ParseError :=
  let raw :=
    match parts with
    | head :: _ => head.token.rawText
    | [] => ""
  { kind := .invalidSyntax
  , rawText := raw
  , message := "wifi slide cannot be part of a connection slide group" }

private def invalidConnGroupError (token : RawNoteToken) : ParseError :=
  { kind := .invalidSyntax
  , rawText := token.rawText
  , message := "invalid connection slide group metadata" }

private partial def collectConnGroup
    (gid : Nat) (expectedSize : Nat) (acc : List TypedSlidePart) (remaining : List RawNoteToken) :
    Except ParseError (List TypedSlidePart × List RawNoteToken) := do
  if acc.length = expectedSize then
    pure (acc.reverse, remaining)
  else
    match remaining with
    | [] => Except.error <| invalidConnGroupError (acc.head?.map TypedSlidePart.token |>.getD default)
    | token :: rest =>
        match token.kind, token.sourceGroupId with
        | .slide, some tokenGid =>
            if tokenGid = gid then
              let part ← typedSlidePart? token
              collectConnGroup gid expectedSize (part :: acc) rest
            else
              Except.error <| invalidConnGroupError token
        | _, _ => Except.error <| invalidConnGroupError token

private def validateConnGroup (parts : List TypedSlidePart) : Except ParseError TypedSlideExpr := do
  if parts.any (fun part => part.semantics.shape.kind = .wifi) then
    Except.error (wifiConnError parts)
  else
    pure (.conn parts)

private partial def buildTypedSlides : List RawNoteToken → Except ParseError (List TypedSlideExpr)
  | [] => pure []
  | token :: rest =>
      match token.kind with
      | .slide =>
          match token.sourceGroupId, token.sourceGroupIndex, token.sourceGroupSize with
          | some gid, some 0, some size => do
              let part ← typedSlidePart? token
              if size ≤ 1 then
                let tail ← buildTypedSlides rest
                pure (.single part :: tail)
              else
                let (parts, remaining) ← collectConnGroup gid size [part] rest
                let expr ← validateConnGroup parts
                let tail ← buildTypedSlides remaining
                pure (expr :: tail)
          | some _, some idx, some _ =>
              if idx = 0 then
                Except.error <| invalidConnGroupError token
              else
                buildTypedSlides rest
          | _, _, _ => do
              let part ← typedSlidePart? token
              let tail ← buildTypedSlides rest
              pure (.single part :: tail)
      | _ => buildTypedSlides rest

def typecheckSlides (tokens : List RawNoteToken) : Except ParseError (List TypedSlideExpr) :=
  buildTypedSlides tokens

end LnmaiCore.Simai
