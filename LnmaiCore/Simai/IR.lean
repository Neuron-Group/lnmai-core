import LnmaiCore.ChartLoader
import LnmaiCore.Simai.Syntax

namespace LnmaiCore.Simai

structure NormalizedSlideDebug where
  noteIndex : Nat
  rawText : String
deriving Inhabited, Repr

structure NormalizedTap where
  timingSec : Float
  lane : Nat
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  isForceStar : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedHold where
  timingSec : Float
  lane : Nat
  lengthSec : Float
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  isTouch : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedTouch where
  timingSec : Float
  sensorPos : Nat
  isBreak : Bool := false
  isHanabi : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedSlide where
  timingSec : Float
  lane : Nat
  lengthSec : Float
  startTimingSec : Float
  hSpeed : Float := 1.0
  slideKind : LnmaiCore.SlideKind := .Single
  trackCount : Nat := 1
  judgeAtSec : Option Float := none
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  isSlideNoHead : Bool := false
  isForceStar : Bool := false
  isFakeRotate : Bool := false
  isSlideBreak : Bool := false
  isConnSlide : Bool := false
  parentNoteIndex : Option Nat := none
  isGroupHead : Bool := false
  isGroupEnd : Bool := false
  totalJudgeQueueLen : Nat := 0
  judgeQueues : List (List SlideAreaSpec) := []
  sourceGroupId : Option Nat := none
  sourceGroupIndex : Option Nat := none
  sourceGroupSize : Option Nat := none
  noteIndex : Nat
  simaiShape : SlideShape
deriving Inhabited, Repr

structure NormalizedChart where
  taps : List NormalizedTap := []
  holds : List NormalizedHold := []
  touches : List NormalizedTouch := []
  touchHolds : List NormalizedHold := []
  slides : List NormalizedSlide := []
  slideDebug : List NormalizedSlideDebug := []
  slideSkipping : Bool := true
deriving Inhabited, Repr

structure FrontendChartInspection where
  metadata : MaidataMetadata
  chart : MaidataChartBlock
  source : SourceChart := {}
  tokens : List RawNoteToken
  slideNotes : List SlideNoteSemantics
deriving Inhabited, Repr

structure FrontendSemanticChart where
  normalized : NormalizedChart
  lowered : ChartLoader.ChartSpec
deriving Inhabited, Repr

structure FrontendChartResult where
  semantic : FrontendSemanticChart
  inspection : FrontendChartInspection
deriving Inhabited, Repr

end LnmaiCore.Simai
