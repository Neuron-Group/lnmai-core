import LnmaiCore.Simai.Source.Maidata

namespace LnmaiCore.Simai

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

def lowerFrontendChartBlock (file : MaidataFile) (block : MaidataChartBlock) : Except ParseError FrontendChartResult :=
  frontendChartResultOfBlock file block

def lowerFrontendChartByLevel (file : MaidataFile) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  frontendChartResultOfLevel file levelIndex

def parseFrontendChartByLevel (content : String) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  parseFrontendChartResult content levelIndex

end LnmaiCore.Simai
