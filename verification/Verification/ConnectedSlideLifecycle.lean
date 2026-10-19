import Verification.Generated
import Verification.Bridge
import LnmaiCore.Simai.Normalize

open Aeneas Aeneas.Std Result
open lnmai_core_verify

namespace Verification

namespace ConnectedSlideLifecycle

/-! The lifecycle kernel carries arbitrary precision signed times.  This
adapter records exactly the value facts needed at this boundary; the
arithmetic implementation remains the extracted Rust code. -/

structure BigIntAdapter where
  view : big_int.BigIntKernel → ℤ
  clone_spec : ∀ value,
    big_int.BigIntKernel.Insts.CoreCloneClone.clone value = ok value
  add_spec : ∀ left right, ∃ result,
    big_int.add left right = ok result ∧
      view result = view left + view right
  sub_spec : ∀ left right, ∃ result,
    big_int.sub left right = ok result ∧
      view result = view left - view right

def optionTime (view : big_int.BigIntKernel → ℤ) :
    Option big_int.BigIntKernel → Option LnmaiCore.TimePoint :=
  Option.map (fun value => LnmaiCore.TimePoint.fromMicros (view value))

structure SourceTimingStep where
  startTiming : LnmaiCore.TimePoint
  storedGroupStartTiming : Option LnmaiCore.TimePoint
  judgeAt : Option LnmaiCore.TimePoint
  nextParentEndTiming : Option LnmaiCore.TimePoint
  nextGroupStartTiming : Option LnmaiCore.TimePoint

def sourceTimingStep
    (isConnSlide : Bool)
    (isGroupHead : Bool)
    (isGroupEnd : Bool)
    (originalStartTiming : LnmaiCore.TimePoint)
    (originalJudgeAt : Option LnmaiCore.TimePoint)
    (length : LnmaiCore.Duration)
    (parentEndTiming : Option LnmaiCore.TimePoint)
    (groupStartTiming : Option LnmaiCore.TimePoint) : SourceTimingStep :=
  let startTiming :=
    if isGroupHead then originalStartTiming else parentEndTiming.getD originalStartTiming
  let groupStart :=
    if isGroupHead then originalStartTiming else groupStartTiming.getD originalStartTiming
  let storedGroupStartTiming := if isConnSlide then some groupStart else none
  let startShift := startTiming - originalStartTiming
  let judgeAt := originalJudgeAt.map
    (fun (value : LnmaiCore.TimePoint) => (value + startShift : LnmaiCore.TimePoint))
  let nextParentEnd := startTiming + length
  { startTiming := startTiming
    storedGroupStartTiming := storedGroupStartTiming
    judgeAt := judgeAt
    nextParentEndTiming := if isGroupEnd then none else some nextParentEnd
    nextGroupStartTiming := if isGroupEnd then none else some groupStart }

def toSourceTimingStep
    (view : big_int.BigIntKernel → ℤ)
    (step : lnmai_core_verify.connected_slide_lifecycle.TimingStep) :
    SourceTimingStep :=
  { startTiming := LnmaiCore.TimePoint.fromMicros (view step.start_timing)
    storedGroupStartTiming := optionTime view step.stored_group_start_timing
    judgeAt := optionTime view step.judge_at
    nextParentEndTiming := optionTime view step.next_parent_end_timing
    nextGroupStartTiming := optionTime view step.next_group_start_timing }

def mapTimingResult
    (view : big_int.BigIntKernel → ℤ) :
    Result lnmai_core_verify.connected_slide_lifecycle.TimingStep →
      Result SourceTimingStep
  | ok step => ok (toSourceTimingStep view step)
  | fail error => fail error
  | div => div

theorem apply_semantics (adapter : BigIntAdapter)
    (isConnSlide : Bool)
    (isGroupHead : Bool)
    (isGroupEnd : Bool)
    (originalStartTiming : big_int.BigIntKernel)
    (originalJudgeAt : Option big_int.BigIntKernel)
    (length : big_int.BigIntKernel)
    (parentEndTiming : Option big_int.BigIntKernel)
    (groupStartTiming : Option big_int.BigIntKernel) :
    mapTimingResult adapter.view
        (lnmai_core_verify.connected_slide_lifecycle.apply
          isConnSlide isGroupHead isGroupEnd originalStartTiming originalJudgeAt
          length parentEndTiming groupStartTiming) =
      ok (sourceTimingStep
        isConnSlide isGroupHead isGroupEnd
        (LnmaiCore.TimePoint.fromMicros (adapter.view originalStartTiming))
        (optionTime adapter.view originalJudgeAt)
        (LnmaiCore.Duration.fromMicros (adapter.view length))
        (optionTime adapter.view parentEndTiming)
        (optionTime adapter.view groupStartTiming)) := by
  let startTiming : big_int.BigIntKernel :=
    if isGroupHead then originalStartTiming
    else parentEndTiming.getD originalStartTiming
  let groupStart : big_int.BigIntKernel :=
    if isGroupHead then originalStartTiming
    else groupStartTiming.getD originalStartTiming
  have hstartBind {α : Type} (f : big_int.BigIntKernel → Result α) :
      (do
        let start_timing ←
          if isGroupHead then
            big_int.BigIntKernel.Insts.CoreCloneClone.clone originalStartTiming
          else
            match parentEndTiming with
            | none =>
              big_int.BigIntKernel.Insts.CoreCloneClone.clone originalStartTiming
            | some value => ok value
        f start_timing) = f startTiming := by
    by_cases h : isGroupHead = true <;> cases parentEndTiming <;>
      simp [h, startTiming, adapter.clone_spec]
  have hgroupBind {α : Type} (f : big_int.BigIntKernel → Result α) :
      (do
        let group_start ←
          if isGroupHead then
            big_int.BigIntKernel.Insts.CoreCloneClone.clone originalStartTiming
          else
            match groupStartTiming with
            | none =>
              big_int.BigIntKernel.Insts.CoreCloneClone.clone originalStartTiming
            | some value => ok value
        f group_start) = f groupStart := by
    by_cases h : isGroupHead = true <;> cases groupStartTiming <;>
      simp [h, groupStart, adapter.clone_spec]
  obtain ⟨startShift, hstartShift, hstartShiftValue⟩ :=
    adapter.sub_spec startTiming originalStartTiming
  obtain ⟨nextParentEnd, hnextParentEnd, hnextParentEndValue⟩ :=
    adapter.add_spec startTiming length
  have hsubBind {α : Type} (f : big_int.BigIntKernel → Result α) :
      (do
        let start_shift ← big_int.sub startTiming originalStartTiming
        f start_shift) = f startShift := by
    simp [hstartShift]
  have hnextBind {α : Type} (f : big_int.BigIntKernel → Result α) :
      (do
        let next_parent_end ← big_int.add startTiming length
        f next_parent_end) = f nextParentEnd := by
    simp [hnextParentEnd]
  have hTimePointAdd (a b : ℤ) :
      LnmaiCore.TimePoint.fromMicros (a + b) =
        LnmaiCore.TimePoint.fromMicros a + LnmaiCore.Duration.fromMicros b := by
    change LnmaiCore.TimePoint.ofInt (a + b) =
      LnmaiCore.TimePoint.ofInt
        (LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt a) +
          LnmaiCore.Duration.toInt (LnmaiCore.Duration.ofInt b))
    rfl
  have hTimePointSubSelf (a s : ℤ) :
      LnmaiCore.TimePoint.fromMicros a =
        LnmaiCore.TimePoint.fromMicros a +
          (LnmaiCore.TimePoint.fromMicros s - LnmaiCore.TimePoint.fromMicros s) := by
    change LnmaiCore.TimePoint.ofInt a =
      LnmaiCore.TimePoint.ofInt
        (LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt a) +
          (LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt s) -
            LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt s)))
    simp [LnmaiCore.TimePoint.toInt, LnmaiCore.TimePoint.ofInt,
      LnmaiCore.TimePoint.ofTick, LnmaiCore.TimeTick.ofInt]
  have hTimePointAddSub (a b c : ℤ) :
      LnmaiCore.TimePoint.fromMicros (a + (b - c)) =
        LnmaiCore.TimePoint.fromMicros a +
          (LnmaiCore.TimePoint.fromMicros b - LnmaiCore.TimePoint.fromMicros c) := by
    change LnmaiCore.TimePoint.ofInt (a + (b - c)) =
      LnmaiCore.TimePoint.ofInt
        (LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt a) +
          (LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt b) -
            LnmaiCore.TimePoint.toInt (LnmaiCore.TimePoint.ofInt c)))
    simp [LnmaiCore.TimePoint.toInt, LnmaiCore.TimePoint.ofInt,
      LnmaiCore.TimePoint.ofTick, LnmaiCore.TimeTick.ofInt]
  cases originalJudgeAt with
  | none =>
      cases isConnSlide <;> cases isGroupHead <;> cases isGroupEnd <;>
        cases parentEndTiming <;> cases groupStartTiming <;>
        all_goals
          simp [startTiming] at hstartShift hstartShiftValue hnextParentEnd hnextParentEndValue
          simp [connected_slide_lifecycle.apply, mapTimingResult,
            toSourceTimingStep, optionTime, sourceTimingStep, Option.getD,
            hstartShift, hnextParentEnd, hstartShiftValue,
            hnextParentEndValue, adapter.clone_spec, bind_tc_ok,
            hTimePointAddSub, hTimePointAdd] <;>
          try { exact hTimePointSubSelf _ _ }
  | some originalJudgeAt =>
      obtain ⟨judgeAt, hjudgeAt, hjudgeAtValue⟩ :=
        adapter.add_spec originalJudgeAt startShift
      have hjudgeBind {α : Type} (f : big_int.BigIntKernel → Result α) :
          (do
            let judge_value ← big_int.add originalJudgeAt startShift
            f judge_value) = f judgeAt := by
        simp [hjudgeAt]
      cases isConnSlide <;> cases isGroupHead <;> cases isGroupEnd <;>
        cases parentEndTiming <;> cases groupStartTiming <;>
        all_goals
          simp [startTiming] at hstartShift hstartShiftValue hnextParentEnd hnextParentEndValue
          simp [connected_slide_lifecycle.apply, mapTimingResult,
            toSourceTimingStep, optionTime, sourceTimingStep, Option.getD,
            hstartShift, hjudgeAt, hnextParentEnd, hjudgeAtValue,
            hstartShiftValue, hnextParentEndValue,
            adapter.clone_spec, bind_tc_ok, hTimePointAddSub,
            hTimePointAdd] <;>
          try { exact hTimePointSubSelf _ _ }

end ConnectedSlideLifecycle

end Verification
