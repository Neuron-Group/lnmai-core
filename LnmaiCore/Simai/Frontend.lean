import LnmaiCore.Simai.Source.Maidata

namespace LnmaiCore.Simai

abbrev FrontendChartResult := ParsedMaidataChart

def parseFrontendMaidata (content : String) : Except ParseError MaidataFile :=
  parseSourceMaidata content

def lowerFrontendChartBlock (file : MaidataFile) (block : MaidataChartBlock) : Except ParseError FrontendChartResult :=
  lowerSourceChartBlock file block

def lowerFrontendChartByLevel (file : MaidataFile) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  lowerSourceChartByLevel file levelIndex

def parseFrontendChartByLevel (content : String) (levelIndex : Nat) : Except ParseError FrontendChartResult :=
  parseAndLowerSourceMaidata content levelIndex

def frontendNormalizedChart (content : String) (levelIndex : Nat) : Except ParseError NormalizedChart := do
  let parsed ← parseFrontendChartByLevel content levelIndex
  pure parsed.normalized

def frontendLoweredChart (content : String) (levelIndex : Nat) : Except ParseError ChartLoader.ChartSpec := do
  let parsed ← parseFrontendChartByLevel content levelIndex
  pure parsed.lowered

end LnmaiCore.Simai
