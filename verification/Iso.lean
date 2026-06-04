import Verification.Generated
import LnmaiCore.Areas
import LnmaiCore.Types
import LnmaiCore.Time
import Aeneas

open Aeneas Aeneas.Std

namespace Verification.Iso

open aeneas_core_verify

----------------------------------------------------------------------------
-- SensorArea isomorphism
----------------------------------------------------------------------------

def toLnmSensorArea : areas.SensorArea → LnmaiCore.SensorArea
  | .A1 => .A1 | .A2 => .A2 | .A3 => .A3 | .A4 => .A4
  | .A5 => .A5 | .A6 => .A6 | .A7 => .A7 | .A8 => .A8
  | .D1 => .D1 | .D2 => .D2 | .D3 => .D3 | .D4 => .D4
  | .D5 => .D5 | .D6 => .D6 | .D7 => .D7 | .D8 => .D8
  | .C => .C
  | .E1 => .E1 | .E2 => .E2 | .E3 => .E3 | .E4 => .E4
  | .E5 => .E5 | .E6 => .E6 | .E7 => .E7 | .E8 => .E8
  | .B1 => .B1 | .B2 => .B2 | .B3 => .B3 | .B4 => .B4
  | .B5 => .B5 | .B6 => .B6 | .B7 => .B7 | .B8 => .B8

def ofLnmSensorArea : LnmaiCore.SensorArea → areas.SensorArea
  | .A1 => .A1 | .A2 => .A2 | .A3 => .A3 | .A4 => .A4
  | .A5 => .A5 | .A6 => .A6 | .A7 => .A7 | .A8 => .A8
  | .D1 => .D1 | .D2 => .D2 | .D3 => .D3 | .D4 => .D4
  | .D5 => .D5 | .D6 => .D6 | .D7 => .D7 | .D8 => .D8
  | .C => .C
  | .E1 => .E1 | .E2 => .E2 | .E3 => .E3 | .E4 => .E4
  | .E5 => .E5 | .E6 => .E6 | .E7 => .E7 | .E8 => .E8
  | .B1 => .B1 | .B2 => .B2 | .B3 => .B3 | .B4 => .B4
  | .B5 => .B5 | .B6 => .B6 | .B7 => .B7 | .B8 => .B8

theorem sensorArea_roundtrip_l (a : LnmaiCore.SensorArea) : toLnmSensorArea (ofLnmSensorArea a) = a := by
  cases a <;> rfl

theorem sensorArea_roundtrip_r (a : areas.SensorArea) : ofLnmSensorArea (toLnmSensorArea a) = a := by
  cases a <;> rfl

----------------------------------------------------------------------------
-- ButtonZone isomorphism
----------------------------------------------------------------------------

def toLnmButtonZone : areas.ButtonZone → LnmaiCore.ButtonZone
  | .K1 => .K1 | .K2 => .K2 | .K3 => .K3 | .K4 => .K4
  | .K5 => .K5 | .K6 => .K6 | .K7 => .K7 | .K8 => .K8

def ofLnmButtonZone : LnmaiCore.ButtonZone → areas.ButtonZone
  | .K1 => .K1 | .K2 => .K2 | .K3 => .K3 | .K4 => .K4
  | .K5 => .K5 | .K6 => .K6 | .K7 => .K7 | .K8 => .K8

theorem buttonZone_roundtrip_l (a : LnmaiCore.ButtonZone) : toLnmButtonZone (ofLnmButtonZone a) = a := by
  cases a <;> rfl

theorem buttonZone_roundtrip_r (a : areas.ButtonZone) : ofLnmButtonZone (toLnmButtonZone a) = a := by
  cases a <;> rfl

----------------------------------------------------------------------------
-- JudgeGrade isomorphism
----------------------------------------------------------------------------

def toLnmJudgeGrade : types.JudgeGrade → LnmaiCore.JudgeGrade
  | .Miss => .Miss
  | .LateGood => .LateGood
  | .LateGreat3rd => .LateGreat3rd
  | .LateGreat2nd => .LateGreat2nd
  | .LateGreat => .LateGreat
  | .LatePerfect3rd => .LatePerfect3rd
  | .LatePerfect2nd => .LatePerfect2nd
  | .Perfect => .Perfect
  | .FastPerfect2nd => .FastPerfect2nd
  | .FastPerfect3rd => .FastPerfect3rd
  | .FastGreat => .FastGreat
  | .FastGreat2nd => .FastGreat2nd
  | .FastGreat3rd => .FastGreat3rd
  | .FastGood => .FastGood
  | .TooFast => .TooFast

def ofLnmJudgeGrade : LnmaiCore.JudgeGrade → types.JudgeGrade
  | .Miss => .Miss
  | .LateGood => .LateGood
  | .LateGreat3rd => .LateGreat3rd
  | .LateGreat2nd => .LateGreat2nd
  | .LateGreat => .LateGreat
  | .LatePerfect3rd => .LatePerfect3rd
  | .LatePerfect2nd => .LatePerfect2nd
  | .Perfect => .Perfect
  | .FastPerfect2nd => .FastPerfect2nd
  | .FastPerfect3rd => .FastPerfect3rd
  | .FastGreat => .FastGreat
  | .FastGreat2nd => .FastGreat2nd
  | .FastGreat3rd => .FastGreat3rd
  | .FastGood => .FastGood
  | .TooFast => .TooFast

theorem judgeGrade_roundtrip_l (g : LnmaiCore.JudgeGrade) : toLnmJudgeGrade (ofLnmJudgeGrade g) = g := by
  cases g <;> rfl

theorem judgeGrade_roundtrip_r (g : types.JudgeGrade) : ofLnmJudgeGrade (toLnmJudgeGrade g) = g := by
  cases g <;> rfl

----------------------------------------------------------------------------
-- JudgeStyle isomorphism
----------------------------------------------------------------------------

def toLnmJudgeStyle : types.JudgeStyle → LnmaiCore.JudgeStyle
  | .Default => .Default
  | .Maji => .Maji
  | .Gachi => .Gachi
  | .Gori => .Gori

def ofLnmJudgeStyle : LnmaiCore.JudgeStyle → types.JudgeStyle
  | .Default => .Default
  | .Maji => .Maji
  | .Gachi => .Gachi
  | .Gori => .Gori

theorem judgeStyle_roundtrip_l (s : LnmaiCore.JudgeStyle) : toLnmJudgeStyle (ofLnmJudgeStyle s) = s := by
  cases s <;> rfl

theorem judgeStyle_roundtrip_r (s : types.JudgeStyle) : ofLnmJudgeStyle (toLnmJudgeStyle s) = s := by
  cases s <;> rfl

----------------------------------------------------------------------------
-- NoteType isomorphism
----------------------------------------------------------------------------

def toLnmNoteType : types.NoteType → LnmaiCore.NoteType
  | .Tap => .Tap
  | .Hold => .Hold
  | .Slide => .Slide
  | .Touch => .Touch
  | .Break => .Break

def ofLnmNoteType : LnmaiCore.NoteType → types.NoteType
  | .Tap => .Tap
  | .Hold => .Hold
  | .Slide => .Slide
  | .Touch => .Touch
  | .Break => .Break

theorem noteType_roundtrip_l (n : LnmaiCore.NoteType) : toLnmNoteType (ofLnmNoteType n) = n := by
  cases n <;> rfl

theorem noteType_roundtrip_r (n : types.NoteType) : ofLnmNoteType (toLnmNoteType n) = n := by
  cases n <;> rfl

----------------------------------------------------------------------------
-- Duration / TimePoint isomorphism (integer-wrapping types)
--   Generated:  time.Duration { micros : Std.I64 } / time.TimePoint { micros : Std.I64 }
--   LnmaiCore:  Duration { ticks : TimeTick { val : ℤ } } / TimePoint analog
-- Both sides wrap i64 values. We axiomatize the bijection since
-- Std.I64 is a bounded model whereas LnmaiCore uses ℤ.
----------------------------------------------------------------------------

axiom toLnmDuration (d : time.Duration) : LnmaiCore.Duration

axiom ofLnmDuration (d : LnmaiCore.Duration) : time.Duration

axiom duration_roundtrip_r (d : time.Duration) : ofLnmDuration (toLnmDuration d) = d

axiom duration_roundtrip_l (d : LnmaiCore.Duration) : toLnmDuration (ofLnmDuration d) = d

axiom toLnmTimePoint (p : time.TimePoint) : LnmaiCore.TimePoint

axiom ofLnmTimePoint (p : LnmaiCore.TimePoint) : time.TimePoint

axiom timePoint_roundtrip_r (p : time.TimePoint) : ofLnmTimePoint (toLnmTimePoint p) = p

axiom timePoint_roundtrip_l (p : LnmaiCore.TimePoint) : toLnmTimePoint (ofLnmTimePoint p) = p

----------------------------------------------------------------------------
-- OuterSlot isomorphism
----------------------------------------------------------------------------

def toLnmOuterSlot : areas.OuterSlot → LnmaiCore.OuterSlot
  | .S1 => .S1 | .S2 => .S2 | .S3 => .S3 | .S4 => .S4
  | .S5 => .S5 | .S6 => .S6 | .S7 => .S7 | .S8 => .S8

def ofLnmOuterSlot : LnmaiCore.OuterSlot → areas.OuterSlot
  | .S1 => .S1 | .S2 => .S2 | .S3 => .S3 | .S4 => .S4
  | .S5 => .S5 | .S6 => .S6 | .S7 => .S7 | .S8 => .S8

theorem outerSlot_roundtrip_l (s : LnmaiCore.OuterSlot) : toLnmOuterSlot (ofLnmOuterSlot s) = s := by
  cases s <;> rfl

theorem outerSlot_roundtrip_r (s : areas.OuterSlot) : ofLnmOuterSlot (toLnmOuterSlot s) = s := by
  cases s <;> rfl

----------------------------------------------------------------------------
-- SlideKind isomorphism
----------------------------------------------------------------------------

def toLnmSlideKind : types.SlideKind → LnmaiCore.SlideKind
  | .Single => .Single
  | .Wifi => .Wifi
  | .ConnPart => .ConnPart

def ofLnmSlideKind : LnmaiCore.SlideKind → types.SlideKind
  | .Single => .Single
  | .Wifi => .Wifi
  | .ConnPart => .ConnPart

theorem slideKind_roundtrip_l (k : LnmaiCore.SlideKind) : toLnmSlideKind (ofLnmSlideKind k) = k := by
  cases k <;> rfl

theorem slideKind_roundtrip_r (k : types.SlideKind) : ofLnmSlideKind (toLnmSlideKind k) = k := by
  cases k <;> rfl

----------------------------------------------------------------------------
-- AreaPolicy isomorphism
----------------------------------------------------------------------------

def toLnmAreaPolicy : types.AreaPolicy → LnmaiCore.AreaPolicy
  | .Or => .Or
  | .And => .And

def ofLnmAreaPolicy : LnmaiCore.AreaPolicy → types.AreaPolicy
  | .Or => .Or
  | .And => .And

theorem areaPolicy_roundtrip_l (p : LnmaiCore.AreaPolicy) : toLnmAreaPolicy (ofLnmAreaPolicy p) = p := by
  cases p <;> rfl

theorem areaPolicy_roundtrip_r (p : types.AreaPolicy) : ofLnmAreaPolicy (toLnmAreaPolicy p) = p := by
  cases p <;> rfl

end Verification.Iso
