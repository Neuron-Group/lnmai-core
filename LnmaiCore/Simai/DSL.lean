import Lean
import LnmaiCore.Simai.Frontend

open Lean Elab Term

namespace LnmaiCore.Simai

syntax "simai_chart!" str : term
syntax "simai_chart_at!" num str : term
syntax "simai_chart_file!" str : term
syntax "simai_chart_file_at!" num str : term
syntax "simai_semantic_chart!" str : term
syntax "simai_semantic_chart_at!" num str : term
syntax "simai_semantic_chart_file!" str : term
syntax "simai_semantic_chart_file_at!" num str : term
syntax "simai_inspection_chart!" str : term
syntax "simai_inspection_chart_at!" num str : term
syntax "simai_inspection_chart_file!" str : term
syntax "simai_inspection_chart_file_at!" num str : term
syntax "simai_normalized_chart!" str : term
syntax "simai_normalized_chart_at!" num str : term
syntax "simai_normalized_chart_file!" str : term
syntax "simai_normalized_chart_file_at!" num str : term
syntax "simai_lowered_chart!" str : term
syntax "simai_lowered_chart_at!" num str : term
syntax "simai_lowered_chart_file!" str : term
syntax "simai_lowered_chart_file_at!" num str : term
syntax "simai_note!" str : term
syntax "simai_slide!" str : term
syntax "simai_normalized_slide!" str : term

private def getStringLiteral? (stx : Syntax) : Option String :=
  stx.isStrLit?

private def throwDslError (kind : String) (input : String) (err : ParseError) : TermElabM α :=
  throwError m!"{kind} parse failed for {repr input}: {err.message}"

private def getNatLiteral? (stx : Syntax) : Option Nat :=
  stx.isNatLit?

private def validateChartLiteral (kind : String) (levelIndex : Nat) (content : String) : TermElabM Unit := do
  match parseFrontendChartResult content levelIndex with
  | .ok _ => pure ()
  | .error err => throwDslError kind content err

private def readChartFile (path : String) : TermElabM String := do
  let content ← (IO.FS.readFile path : IO String)
  pure content

private def validateChartFileLiteral (kind : String) (levelIndex : Nat) (path : String) : TermElabM String := do
  let content ← readChartFile path
  validateChartLiteral kind levelIndex content
  pure content

elab_rules : term
  | `(simai_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_chart!" 1 content
      let stx ← `((frontendChartLiteral $s : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_chart_at!" levelIndex content
      let stx ← `((frontendChartLiteral $s $n : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_chart_file!" 1 path
      let stx ← `((frontendChartLiteral $(quote content) : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_chart_file_at!" levelIndex path
      let stx ← `((frontendChartLiteral $(quote content) $n : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_semantic_chart!" 1 content
      let stx ← `((frontendSemanticChartLiteral $s : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_semantic_chart_at!" levelIndex content
      let stx ← `((frontendSemanticChartLiteral $s $n : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_semantic_chart_file!" 1 path
      let stx ← `((frontendSemanticChartLiteral $(quote content) : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_semantic_chart_file_at!" levelIndex path
      let stx ← `((frontendSemanticChartLiteral $(quote content) $n : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_inspection_chart!" 1 content
      let stx ← `((frontendInspectionChartLiteral $s : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_inspection_chart_at!" levelIndex content
      let stx ← `((frontendInspectionChartLiteral $s $n : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_inspection_chart_file!" 1 path
      let stx ← `((frontendInspectionChartLiteral $(quote content) : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_inspection_chart_file_at!" levelIndex path
      let stx ← `((frontendInspectionChartLiteral $(quote content) $n : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_normalized_chart!" 1 content
      let stx ← `((frontendNormalizedChartLiteral $s : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_normalized_chart_at!" levelIndex content
      let stx ← `((frontendNormalizedChartLiteral $s $n : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_normalized_chart_file!" 1 path
      let stx ← `((frontendNormalizedChartLiteral $(quote content) : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_normalized_chart_file_at!" levelIndex path
      let stx ← `((frontendNormalizedChartLiteral $(quote content) $n : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_lowered_chart!" 1 content
      let stx ← `((frontendLoweredChartLiteral $s : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_lowered_chart_at!" levelIndex content
      let stx ← `((frontendLoweredChartLiteral $s $n : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_lowered_chart_file!" 1 path
      let stx ← `((frontendLoweredChartLiteral $(quote content) : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_lowered_chart_file_at!" levelIndex path
      let stx ← `((frontendLoweredChartLiteral $(quote content) $n : ChartLoader.ChartSpec))
      elabTerm stx none

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
