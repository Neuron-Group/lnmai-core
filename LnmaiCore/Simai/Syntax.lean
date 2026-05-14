import Mathlib
import Lean.Data.Json
import LnmaiCore.Areas
import LnmaiCore.Time

open Lean

namespace LnmaiCore.Simai

private def ratToDecimalStringAux (numerator denominator : Nat) : Nat → Nat → List Char
  | 0, _ => []
  | fuel + 1, rem =>
      if rem = 0 then
        []
      else
        let scaled := rem * 10
        let digit := scaled / denominator
        let nextRem := scaled % denominator
        Char.ofNat (digit + '0'.toNat) :: ratToDecimalStringAux numerator denominator fuel nextRem

def ratToDecimalString (value : Rat) : String :=
  let num := value.num
  let denNat := value.den
  let negative := num < 0
  let numAbs := num.natAbs
  let whole := numAbs / denNat
  let rem := numAbs % denNat
  let fracChars := ratToDecimalStringAux numAbs denNat 12 rem
  let fracTrimmed := fracChars.reverse.dropWhile (· = '0') |>.reverse
  let sign := if negative then "-" else ""
  if fracTrimmed.isEmpty then
    s!"{sign}{whole}"
  else
    s!"{sign}{whole}.{String.ofList fracTrimmed}"

def ratToJson (value : Rat) : Json :=
  Json.mkObj
    [ ("num", toJson value.num)
    , ("den", toJson value.den)
    , ("decimal", Json.str (ratToDecimalString value)) ]

def ratFromJson? (json : Json) : Except String Rat := do
  let numJson ← json.getObjVal? "num"
  let denJson ← json.getObjVal? "den"
  let num : Int <- fromJson? numJson
  let denNat : Nat <- fromJson? denJson
  if denNat = 0 then
    Except.error "rational denominator must be nonzero"
  else
    pure (num / denNat)

instance : ToJson Rat where
  toJson := ratToJson

instance : FromJson Rat where
  fromJson? := ratFromJson?

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
  startSlot : OuterSlot
  endArea : SensorArea
  shape : SlideShape
  isJustRight : Bool := false
deriving DecidableEq, Repr, Inhabited, BEq

structure TimingPointSemantics where
  timing : TimePoint
  bpm : Rat
  hSpeed : Rat
  notes : List SlideNoteSemantics := []
deriving Inhabited, Repr

structure SimaiChartSemantics where
  timingPoints : List TimingPointSemantics := []
  commaTimings : List TimePoint := []
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
  timing : TimePoint
  bpm : Rat
  hSpeed : Rat := 1
  divisor : Nat
  slot : Option OuterSlot := none
  sensorPos : Option SensorArea := none
  length : Option Duration := none
  starWait : Option Duration := none
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
  timing : TimePoint
  bpm : Rat
  hSpeed : Rat := 1
  divisor : Nat := 4
  notes : List SourceNote := []
  sourcePos : Option SourceSpan := none
deriving Inhabited, Repr

structure SourceChart where
  events : List SourceEvent := []
deriving Inhabited, Repr

end LnmaiCore.Simai
