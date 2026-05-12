import LnmaiCore.ChartLoader
import LnmaiCore.Simai.Syntax

namespace LnmaiCore.Simai

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
  noteIndex : Nat
  simaiRawText : String
  simaiShapeKey : String
  simaiIsJustRight : Bool
deriving Inhabited, Repr

structure NormalizedChart where
  taps : List NormalizedTap := []
  holds : List NormalizedHold := []
  touches : List NormalizedTouch := []
  touchHolds : List NormalizedHold := []
  slides : List NormalizedSlide := []
  slideSkipping : Bool := true
deriving Inhabited, Repr

structure ParsedMaidataChart where
  metadata : MaidataMetadata
  chart : MaidataChartBlock
  tokens : List RawNoteToken
  slideNotes : List SlideNoteSemantics
  normalized : NormalizedChart
  lowered : ChartLoader.ChartSpec
deriving Inhabited, Repr

end LnmaiCore.Simai
