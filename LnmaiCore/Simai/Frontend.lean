import LnmaiCore.Simai.Source.Maidata

namespace LnmaiCore.Simai

private def singleNoteMaidata (noteText : String) : String :=
  s!"&first=0\n&inote_1=\n(120)\n{noteText},\n"

private def exactlyOneError (kind : String) (rawText : String) (count : Nat) : ParseError :=
  { kind := .invalidSyntax
  , rawText := rawText
  , message := s!"expected exactly one {kind}, found {count}" }

def parseFrontendMaidata (content : String) : Except ParseError MaidataFile :=
  parseSourceMaidata content

def frontendChartResultOfBlock (file : MaidataFile) (block : MaidataChartBlock) : Except ParseError FrontendChartResult :=
  lowerSourceChartBlock file block

def frontendChartResultOfLevel (file : MaidataFile) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  lowerSourceChartByLevel file levelIndex

def parseFrontendChartResult (content : String) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  parseAndLowerSourceMaidata content levelIndex

def parseFrontendSemanticChart (content : String) (levelIndex : Nat) : Except ParseError FrontendSemanticChart := do
  let result ← parseFrontendChartResult content levelIndex
  pure result.semantic

def parseFrontendInspectionChart (content : String) (levelIndex : Nat) : Except ParseError FrontendChartInspection := do
  let result ← parseFrontendChartResult content levelIndex
  pure result.inspection

def frontendNormalizedChart (content : String) (levelIndex : Nat) : Except ParseError NormalizedChart := do
  let semantic ← parseFrontendSemanticChart content levelIndex
  pure semantic.normalized

def frontendLoweredChart (content : String) (levelIndex : Nat) : Except ParseError ChartLoader.ChartSpec := do
  let semantic ← parseFrontendSemanticChart content levelIndex
  pure semantic.lowered

def parseFrontendSingleToken (noteText : String) : Except ParseError RawNoteToken := do
  let result ← parseFrontendChartResult (singleNoteMaidata noteText) 1
  match result.inspection.tokens with
  | [token] => pure token
  | tokens => Except.error <| exactlyOneError "note token" noteText tokens.length

def parseFrontendSingleSlideNote (noteText : String) : Except ParseError SlideNoteSemantics := do
  let result ← parseFrontendChartResult (singleNoteMaidata noteText) 1
  match result.inspection.slideNotes with
  | [note] => pure note
  | notes => Except.error <| exactlyOneError "slide note" noteText notes.length

def parseFrontendSingleNormalizedSlide (noteText : String) : Except ParseError NormalizedSlide := do
  let result ← parseFrontendChartResult (singleNoteMaidata noteText) 1
  match result.semantic.normalized.slides with
  | [slide] => pure slide
  | slides => Except.error <| exactlyOneError "normalized slide" noteText slides.length

def frontendChartLiteral (content : String) (levelIndex : Nat := 1) : FrontendChartResult :=
  match parseFrontendChartResult content levelIndex with
  | .ok result => result
  | .error err => panic! s!"invalid simai chart literal: {err.message}"

def frontendNoteLiteral (noteText : String) : RawNoteToken :=
  match parseFrontendSingleToken noteText with
  | .ok token => token
  | .error err => panic! s!"invalid simai note literal: {err.message}"

def frontendSlideNoteLiteral (noteText : String) : SlideNoteSemantics :=
  match parseFrontendSingleSlideNote noteText with
  | .ok note => note
  | .error err => panic! s!"invalid simai slide literal: {err.message}"

def frontendNormalizedSlideLiteral (noteText : String) : NormalizedSlide :=
  match parseFrontendSingleNormalizedSlide noteText with
  | .ok slide => slide
  | .error err => panic! s!"invalid simai normalized slide literal: {err.message}"

def lowerFrontendChartBlock (file : MaidataFile) (block : MaidataChartBlock) : Except ParseError FrontendChartResult :=
  frontendChartResultOfBlock file block

def lowerFrontendChartByLevel (file : MaidataFile) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  frontendChartResultOfLevel file levelIndex

def parseFrontendChartByLevel (content : String) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  parseFrontendChartResult content levelIndex

end LnmaiCore.Simai
