import Mathlib
import Lean.Data.Json
import LnmaiCore.Areas

namespace LnmaiCore.Simai

structure SourcePos where
  line : Nat
  column : Nat
deriving DecidableEq, Repr, Inhabited, BEq

structure SourceSpan where
  start : SourcePos
  stop : SourcePos
deriving DecidableEq, Repr, Inhabited, BEq

inductive ParseErrorKind where
  | invalidSyntax
  | invalidShape
  | invalidEndPosition
  | invalidTurnPosition
  | invalidChainTiming
deriving DecidableEq, Repr, Inhabited, BEq

structure ParseError where
  kind : ParseErrorKind
  rawText : String
  message : String
  span : Option SourceSpan := none
deriving DecidableEq, Repr, Inhabited, BEq

inductive SlideKind where
  | line
  | circle
  | v
  | turn
  | pq
  | ppqq
  | s
  | wifi
deriving DecidableEq, Repr, Inhabited, BEq

structure SlideShape where
  kind : SlideKind
  relEnd : Option Nat := none
  mirrored : Bool := false
deriving DecidableEq, Repr, Inhabited, BEq

structure SlideNoteSemantics where
  rawText : String
  startLane : ButtonZone
  endArea : SensorArea
  shape : SlideShape
  isJustRight : Bool := false
deriving DecidableEq, Repr, Inhabited, BEq

structure TimingPointSemantics where
  timingSec : Float
  bpm : Float
  hSpeed : Float
  notes : List SlideNoteSemantics := []
deriving Inhabited, Repr

structure SimaiChartSemantics where
  timingPoints : List TimingPointSemantics := []
  commaTimings : List Float := []
  title : String := ""
  designer : String := ""
  level : String := ""
deriving Inhabited, Repr

inductive RawNoteKind where
  | tap
  | hold
  | slide
  | touch
  | touchHold
  | rest
  | unknown
deriving DecidableEq, Repr, Inhabited, BEq

structure MaidataMetadata where
  fields : List (String × String) := []
deriving Inhabited, Repr

structure MaidataChartBlock where
  levelIndex : Nat
  rawBody : String
deriving Inhabited, Repr

structure MaidataFile where
  metadata : MaidataMetadata := {}
  charts : List MaidataChartBlock := []
deriving Inhabited, Repr

structure RawNoteToken where
  rawText : String
  kind : RawNoteKind
  timingSec : Float
  bpm : Float
  hSpeed : Float := 1.0
  divisor : Nat
  lane : Option ButtonZone := none
  sensorPos : Option SensorArea := none
  lengthSec : Option Float := none
  starWaitSec : Option Float := none
  isBreak : Bool := false
  isEX : Bool := false
  isHanabi : Bool := false
  isSlideNoHead : Bool := false
  isForceStar : Bool := false
  isFakeRotate : Bool := false
  isSlideBreak : Bool := false
  sourceGroupId : Option Nat := none
  sourceGroupIndex : Option Nat := none
  sourceGroupSize : Option Nat := none
  sourcePos : Option SourceSpan := none
deriving Inhabited, Repr

structure SourceNote where
  token : RawNoteToken
  sourcePos : Option SourceSpan := none
deriving Inhabited, Repr

structure SourceEvent where
  timingSec : Float
  bpm : Float
  hSpeed : Float := 1.0
  divisor : Nat := 4
  notes : List SourceNote := []
  sourcePos : Option SourceSpan := none
deriving Inhabited, Repr

structure SourceChart where
  events : List SourceEvent := []
deriving Inhabited, Repr

end LnmaiCore.Simai
