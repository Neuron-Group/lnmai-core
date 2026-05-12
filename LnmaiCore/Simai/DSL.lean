import Lean
import LnmaiCore.Simai.Frontend

open Lean Elab Term

namespace LnmaiCore.Simai

syntax "simai_chart!" str : term
syntax "simai_note!" str : term
syntax "simai_slide!" str : term
syntax "simai_normalized_slide!" str : term

private def getStringLiteral? (stx : Syntax) : Option String :=
  stx.isStrLit?

private def throwDslError (kind : String) (input : String) (err : ParseError) : TermElabM α :=
  throwError m!"{kind} parse failed for {repr input}: {err.message}"

elab_rules : term
  | `(simai_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendChartResult content 1 with
      | .ok _ =>
          let stx ← `((frontendChartLiteral $s : FrontendChartResult))
          elabTerm stx none
      | .error err => throwDslError "simai_chart!" content err

elab_rules : term
  | `(simai_note! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendSingleToken content with
      | .ok _ =>
          let stx ← `((frontendNoteLiteral $s : RawNoteToken))
          elabTerm stx none
      | .error err => throwDslError "simai_note!" content err

elab_rules : term
  | `(simai_slide! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendSingleSlideNote content with
      | .ok _ =>
          let stx ← `((frontendSlideNoteLiteral $s : SlideNoteSemantics))
          elabTerm stx none
      | .error err => throwDslError "simai_slide!" content err

elab_rules : term
  | `(simai_normalized_slide! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendSingleNormalizedSlide content with
      | .ok _ =>
          let stx ← `((frontendNormalizedSlideLiteral $s : NormalizedSlide))
          elabTerm stx none
      | .error err => throwDslError "simai_normalized_slide!" content err

end LnmaiCore.Simai

def x := simai_normalized_slide! "1<5[4:1]"
#eval x

