import Mathlib
import Lean.Data.Json

open Lean

namespace LnmaiCore

/-- Exact machine-facing tick count in microseconds. -/
structure TimeTick where
  val : Int
deriving DecidableEq, Repr, Inhabited, BEq

/-- Duration on the local song timeline, measured in microsecond ticks. -/
structure Duration where
  ticks : TimeTick
deriving DecidableEq, Repr, Inhabited, BEq

/-- Point on the local song timeline, measured in microsecond ticks. -/
structure TimePoint where
  ticks : TimeTick
deriving DecidableEq, Repr, Inhabited, BEq

namespace TimeTick

def ofInt (value : Int) : TimeTick :=
  { val := value }

def toInt (tick : TimeTick) : Int :=
  tick.val

def zero : TimeTick :=
  ofInt 0

instance : LT TimeTick where
  lt a b := a.val < b.val

instance : LE TimeTick where
  le a b := a.val ≤ b.val

instance : Ord TimeTick where
  compare a b := compare a.val b.val

instance : Min TimeTick where
  min a b := if a ≤ b then a else b

instance : Max TimeTick where
  max a b := if a ≤ b then b else a

instance : ToJson TimeTick where
  toJson tick := toJson tick.val

instance : FromJson TimeTick where
  fromJson?
    | json => TimeTick.ofInt <$> fromJson? json

end TimeTick

namespace Duration

def ofTick (tick : TimeTick) : Duration :=
  { ticks := tick }

def ofInt (value : Int) : Duration :=
  ofTick <| TimeTick.ofInt value

def zero : Duration :=
  ofInt 0

def toTick (duration : Duration) : TimeTick :=
  duration.ticks

def toInt (duration : Duration) : Int :=
  duration.ticks.val

def fromMicros (micros : Int) : Duration :=
  ofInt micros

def toMicros (duration : Duration) : Int :=
  duration.toInt

def scaleNat (duration : Duration) (factor : Nat) : Duration :=
  ofInt (duration.toInt * Int.ofNat factor)

def divNat (duration : Duration) (divisor : Nat) : Duration :=
  if divisor = 0 then zero else ofInt (duration.toInt / Int.ofNat divisor)

def abs (duration : Duration) : Duration :=
  if duration.toInt < 0 then ofInt (-duration.toInt) else duration

instance : LT Duration where
  lt a b := a.toInt < b.toInt

instance : LE Duration where
  le a b := a.toInt ≤ b.toInt

instance : Ord Duration where
  compare a b := compare a.toInt b.toInt

instance : Min Duration where
  min a b := if a ≤ b then a else b

instance : Max Duration where
  max a b := if a ≤ b then b else a

instance : Add Duration where
  add a b := ofInt (a.toInt + b.toInt)

instance : Sub Duration where
  sub a b := ofInt (a.toInt - b.toInt)

instance : Neg Duration where
  neg a := ofInt (-a.toInt)

instance : OfNat Duration n where
  ofNat := ofInt n

instance : ToJson Duration where
  toJson duration := toJson duration.toMicros

instance : FromJson Duration where
  fromJson?
    | json => Duration.fromMicros <$> fromJson? json

theorem toMicros_injective : Function.Injective toMicros := by
  intro a b h
  cases a with
  | mk ta =>
      cases b with
      | mk tb =>
          have ht : ta = tb := by
            cases ta with
            | mk av =>
                cases tb with
                | mk bv =>
                    simp [toMicros, toInt] at h
                    cases h
                    rfl
          cases ht
          rfl

theorem toMicros_le_toMicros (a b : Duration) : a.toMicros ≤ b.toMicros ↔ a ≤ b := by
  rfl

theorem toMicros_lt_toMicros (a b : Duration) : a.toMicros < b.toMicros ↔ a < b := by
  rfl

theorem toMicros_eq_toMicros (a b : Duration) : a.toMicros = b.toMicros ↔ a = b := by
  constructor
  · intro h
    exact toMicros_injective h
  · intro h
    simp [h]

end Duration

namespace TimePoint

def ofTick (tick : TimeTick) : TimePoint :=
  { ticks := tick }

def ofInt (value : Int) : TimePoint :=
  ofTick <| TimeTick.ofInt value

def zero : TimePoint :=
  ofInt 0

def toTick (point : TimePoint) : TimeTick :=
  point.ticks

def toInt (point : TimePoint) : Int :=
  point.ticks.val

def fromMicros (micros : Int) : TimePoint :=
  ofInt micros

def toMicros (point : TimePoint) : Int :=
  point.toInt

instance : LT TimePoint where
  lt a b := a.toInt < b.toInt

instance : LE TimePoint where
  le a b := a.toInt ≤ b.toInt

instance : Ord TimePoint where
  compare a b := compare a.toInt b.toInt

instance : Min TimePoint where
  min a b := if a ≤ b then a else b

instance : Max TimePoint where
  max a b := if a ≤ b then b else a

instance : HAdd TimePoint Duration TimePoint where
  hAdd point duration := ofInt (point.toInt + duration.toInt)

instance : HSub TimePoint Duration TimePoint where
  hSub point duration := ofInt (point.toInt - duration.toInt)

instance : HSub TimePoint TimePoint Duration where
  hSub a b := Duration.ofInt (a.toInt - b.toInt)

instance : ToJson TimePoint where
  toJson point := toJson point.toMicros

instance : FromJson TimePoint where
  fromJson?
    | json => TimePoint.fromMicros <$> fromJson? json

theorem toMicros_injective : Function.Injective toMicros := by
  intro a b h
  cases a with
  | mk ta =>
      cases b with
      | mk tb =>
          have ht : ta = tb := by
            cases ta with
            | mk av =>
                cases tb with
                | mk bv =>
                    simp [toMicros, toInt] at h
                    cases h
                    rfl
          cases ht
          rfl

theorem toMicros_le_toMicros (a b : TimePoint) : a.toMicros ≤ b.toMicros ↔ a ≤ b := by
  rfl

theorem toMicros_lt_toMicros (a b : TimePoint) : a.toMicros < b.toMicros ↔ a < b := by
  rfl

theorem toMicros_eq_toMicros (a b : TimePoint) : a.toMicros = b.toMicros ↔ a = b := by
  constructor
  · intro h
    exact toMicros_injective h
  · intro h
    simp [h]

end TimePoint

namespace Time

def microsPerMilli : Int := 1000

def microsPerSecond : Int := 1000000

def microsPerMinute : Int := 60 * microsPerSecond

def millisToMicros (millis : Int) : Int :=
  millis * microsPerMilli

private def roundDivAwayFromZero (num den : Int) : Int :=
  if den = 0 then
    0
  else
    let denAbs := Int.natAbs den
    let denPos : Int := Int.ofNat denAbs
    let numAdj :=
      if num < 0 then
        num - denPos / 2
      else
        num + denPos / 2
    numAdj / denPos

def quantizeRatMicros (value : Rat) : Int :=
  roundDivAwayFromZero value.num value.den

def durationFromRatMicros (value : Rat) : Duration :=
  Duration.fromMicros (quantizeRatMicros value)

def pointFromRatMicros (value : Rat) : TimePoint :=
  TimePoint.fromMicros (quantizeRatMicros value)

def bpmBeatMicrosRat (bpm : Rat) : Rat :=
  if bpm = 0 then
    1
  else
    (microsPerMinute : Rat) / bpm

def bpmMeasureMicrosRat (bpm : Rat) : Rat :=
  bpmBeatMicrosRat bpm * 4

def durationFromSecondsRat (seconds : Rat) : Duration :=
  durationFromRatMicros (seconds * microsPerSecond)

def pointFromSecondsRat (seconds : Rat) : TimePoint :=
  pointFromRatMicros (seconds * microsPerSecond)

def fromMillis (millis : Int) : Duration :=
  Duration.fromMicros (millisToMicros millis)

def pointFromMillis (millis : Int) : TimePoint :=
  TimePoint.fromMicros (millisToMicros millis)

theorem timePoint_toMicros_order_preserving (a b : TimePoint) :
    a ≤ b ↔ a.toMicros ≤ b.toMicros := by
  exact Iff.symm (TimePoint.toMicros_le_toMicros a b)

theorem duration_toMicros_order_preserving (a b : Duration) :
    a ≤ b ↔ a.toMicros ≤ b.toMicros := by
  exact Iff.symm (Duration.toMicros_le_toMicros a b)

theorem timePoint_toMicros_strict_order_preserving (a b : TimePoint) :
    a < b ↔ a.toMicros < b.toMicros := by
  exact Iff.symm (TimePoint.toMicros_lt_toMicros a b)

theorem duration_toMicros_strict_order_preserving (a b : Duration) :
    a < b ↔ a.toMicros < b.toMicros := by
  exact Iff.symm (Duration.toMicros_lt_toMicros a b)

theorem timePoint_compare_toMicros (a b : TimePoint) :
    compare a b = compare a.toMicros b.toMicros := by
  rfl

theorem duration_compare_toMicros (a b : Duration) :
    compare a b = compare a.toMicros b.toMicros := by
  rfl

theorem timePoint_pairwise_le_toMicros_iff (xs : List TimePoint) :
    xs.Pairwise (fun a b => a ≤ b) ↔ xs.Pairwise (fun a b => a.toMicros ≤ b.toMicros) := by
  induction xs with
  | nil => simp
  | cons head tail ih =>
      simp [List.pairwise_cons, TimePoint.toMicros_le_toMicros, ih]

theorem duration_pairwise_le_toMicros_iff (xs : List Duration) :
    xs.Pairwise (fun a b => a ≤ b) ↔ xs.Pairwise (fun a b => a.toMicros ≤ b.toMicros) := by
  induction xs with
  | nil => simp
  | cons head tail ih =>
      simp [List.pairwise_cons, Duration.toMicros_le_toMicros, ih]

/-- Quantize a decimal string in seconds into whole microseconds. Fractional
microseconds are rounded half away from zero. -/
def quantizeSecondsString (text : String) : Option Int :=
  let t := text.trimAscii.toString
  if t = "" then
    none
  else
    let negative := t.startsWith "-"
    let unsigned := if negative then (t.drop 1).toString else t
    match unsigned.splitOn "." with
    | [whole] =>
        match whole.toNat? with
        | some n =>
            let micros := Int.ofNat n * 1000000
            some <| if negative then -micros else micros
        | none => none
    | [whole, frac] =>
        match whole.toNat? with
        | none => none
        | some wholeNat =>
            if frac.toList.all Char.isDigit then
              let fracDigits := frac.length
              let fracNat := frac.toNat?.getD 0
              let fracNumerator := Int.ofNat fracNat * 1000000
              let fracDenominator : Int := Int.ofNat ((10 : Nat) ^ fracDigits)
              let fracMicros :=
                if fracDenominator = 0 then
                  0
                else
                  let halfDen := fracDenominator / 2
                  (fracNumerator + halfDen) / fracDenominator
              let micros := Int.ofNat wholeNat * 1000000 + fracMicros
              some <| if negative then -micros else micros
            else
              none
    | _ => none

def parseSecondsString? (text : String) : Option Duration :=
  Duration.fromMicros <$> quantizeSecondsString text

def parseSecondsPointString? (text : String) : Option TimePoint :=
  TimePoint.fromMicros <$> quantizeSecondsString text

end Time

theorem duration_toInt_ofInt (value : Int) :
    (Duration.ofInt value).toInt = value := rfl

theorem timePoint_toInt_ofInt (value : Int) :
    (TimePoint.ofInt value).toInt = value := rfl

end LnmaiCore
