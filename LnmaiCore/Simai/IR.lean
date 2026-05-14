import LnmaiCore.ChartLoader
import LnmaiCore.Simai.Syntax
import LnmaiCore.Areas
import LnmaiCore.Time

namespace LnmaiCore.Simai

structure NormalizedSlideDebug where
  noteIndex : Nat
  rawText : String
deriving Inhabited, Repr

structure NormalizedTap where
  timing : TimePoint
  slot : OuterSlot
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  isForceStar : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedHold where
  timing : TimePoint
  slot : OuterSlot
  length : Duration
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedTouchHold where
  timing : TimePoint
  sensorPos : SensorArea
  length : Duration
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedTouch where
  timing : TimePoint
  sensorPos : SensorArea
  isBreak : Bool := false
  isHanabi : Bool := false
  noteIndex : Nat
deriving Inhabited, Repr

structure NormalizedSlide where
  timing : TimePoint
  slot : OuterSlot
  length : Duration
  startTiming : TimePoint
  hSpeed : Float := 1.0
  slideKind : LnmaiCore.SlideKind := .Single
  trackCount : Nat := 1
  judgeAt : Option TimePoint := none
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
  touchHolds : List NormalizedTouchHold := []
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
