import LnmaiCore.Lifecycle

namespace LnmaiCore.Lifecycle

open LnmaiCore Constants

def tapInputCanBeConsumed (note : TapFamilyNote) (currentTime : TimePoint) : Prop :=
  (match note.state with | .Waiting | .Judgeable => True | _ => False) ∧
  currentTime ≥ note.params.effectiveTiming - JUDGABLE_RANGE_SEC ∧
  currentTime ≤ note.params.effectiveTiming + tapGoodMs

theorem tap_consumed_faces_judge
    (note : TapFamilyNote) (currentTime : TimePoint) (judgeDiff : Duration) (style : JudgeStyle)
    (h : tapInputCanBeConsumed note currentTime) :
    (tapFamilyStep note currentTime judgeDiff true style).2.isSome = true := by
  cases note with
  | tap note =>
      cases note.state with
      | Waiting =>
          simp [tapInputCanBeConsumed, tapFamilyStep, tapStep] at h ⊢
          omega
      | Judgeable =>
          simp [tapInputCanBeConsumed, tapFamilyStep, tapStep] at h ⊢
          omega
      | Judged grade => simp [tapInputCanBeConsumed] at h
      | Ended => simp [tapInputCanBeConsumed] at h
  | slideHead note =>
      cases note.state with
      | Waiting =>
          simp [tapInputCanBeConsumed, tapFamilyStep, slideHeadStep] at h ⊢
          omega
      | Judgeable =>
          simp [tapInputCanBeConsumed, tapFamilyStep, slideHeadStep] at h ⊢
          omega
      | Judged grade => simp [tapInputCanBeConsumed] at h
      | Ended => simp [tapInputCanBeConsumed] at h

end LnmaiCore.Lifecycle
