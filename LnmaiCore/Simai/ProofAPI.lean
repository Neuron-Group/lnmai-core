import LnmaiCore.Simai.Frontend

namespace LnmaiCore.Simai

def compileChart (content : String) (levelIndex : Nat := 1) : Except ParseError FrontendChartResult :=
  parseFrontendChartResult content levelIndex

def compileSemantic (content : String) (levelIndex : Nat := 1) : Except ParseError FrontendSemanticChart :=
  parseFrontendSemanticChart content levelIndex

def compileInspection (content : String) (levelIndex : Nat := 1) : Except ParseError FrontendChartInspection :=
  parseFrontendInspectionChart content levelIndex

def compileNormalized (content : String) (levelIndex : Nat := 1) : Except ParseError NormalizedChart :=
  frontendNormalizedChart content levelIndex

def compileLowered (content : String) (levelIndex : Nat := 1) : Except ParseError ChartLoader.ChartSpec :=
  frontendLoweredChart content levelIndex

def compileSingleToken (noteText : String) : Except ParseError RawNoteToken :=
  parseFrontendSingleToken noteText

def compileSingleSlide (noteText : String) : Except ParseError SlideNoteSemantics :=
  parseFrontendSingleSlideNote noteText

def compileSingleNormalizedSlide (noteText : String) : Except ParseError NormalizedSlide :=
  parseFrontendSingleNormalizedSlide noteText

def compileNormalized? (content : String) (levelIndex : Nat := 1) : Option NormalizedChart :=
  (compileNormalized content levelIndex).toOption

def compileInspection? (content : String) (levelIndex : Nat := 1) : Option FrontendChartInspection :=
  (compileInspection content levelIndex).toOption

def compileSingleSlide? (noteText : String) : Option SlideNoteSemantics :=
  (compileSingleSlide noteText).toOption

def compileSingleNormalizedSlide? (noteText : String) : Option NormalizedSlide :=
  (compileSingleNormalizedSlide noteText).toOption

def normalizedSlidesOf (content : String) (levelIndex : Nat := 1) : Except ParseError (List NormalizedSlide) := do
  return (← compileNormalized content levelIndex).slides

def normalizedTapsOf (content : String) (levelIndex : Nat := 1) : Except ParseError (List NormalizedTap) := do
  return (← compileNormalized content levelIndex).taps

def tokenStreamOf (content : String) (levelIndex : Nat := 1) : Except ParseError (List RawNoteToken) := do
  return (← compileInspection content levelIndex).tokens

def slideNotesOf (content : String) (levelIndex : Nat := 1) : Except ParseError (List SlideNoteSemantics) := do
  return (← compileInspection content levelIndex).slideNotes

theorem compileNormalized_eq_frontendNormalizedChart
    (content : String) (levelIndex : Nat := 1) :
    compileNormalized content levelIndex = frontendNormalizedChart content levelIndex := rfl

theorem compileInspection_eq_parseFrontendInspectionChart
    (content : String) (levelIndex : Nat := 1) :
    compileInspection content levelIndex = parseFrontendInspectionChart content levelIndex := rfl

theorem compileSingleNormalizedSlide_eq_parseFrontendSingleNormalizedSlide
    (noteText : String) :
    compileSingleNormalizedSlide noteText = parseFrontendSingleNormalizedSlide noteText := rfl

end LnmaiCore.Simai
