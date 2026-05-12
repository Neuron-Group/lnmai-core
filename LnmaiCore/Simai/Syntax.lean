import Mathlib
import Lean.Data.Json

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
  startPos : Nat
  endPos : Nat
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

end LnmaiCore.Simai
