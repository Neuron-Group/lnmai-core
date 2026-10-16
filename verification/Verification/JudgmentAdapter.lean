import Verification.AdapterJudgments

/-!
Exact-integer judgment adapters extracted from the Rust kernel that the runtime
calls. The only representation assumptions below concern individual integer
operations. Clamping, cancellation, hold arithmetic and percentage selection
are proved from the extracted control flow, not supplied as callbacks.

The concrete Integer/BigInt representation and the runtime trait instance are
still separate refinement obligations. These theorems impose no machine-width
bound on decoded judgment inputs.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.JudgmentAdapterProof

open Bridge JudgeProof

/-- Atomic representation laws. Conversion is needed only in the representable
range established by the extracted clamp; no truncation law is assumed. -/
structure IntegerAgrees {I Ops : Type}
    (inst : judgment_adapter.IntegerOps Ops I) (ops : Ops)
    (decode : I → ℤ) : Prop where
  from_i64 : ∀ value, inst.from_i64 ops value ⦃ out => decode out = value.val ⦄
  less : ∀ left right, inst.less ops left right =
    ok (decide (decode left < decode right))
  equal : ∀ left right, inst.equal ops left right =
    ok (decide (decode left = decode right))
  add : ∀ left right, inst.add ops left right
    ⦃ out => decode out = decode left + decode right ⦄
  sub : ∀ left right, inst.sub ops left right
    ⦃ out => decode out = decode left - decode right ⦄
  mul_i64 : ∀ value factor, inst.mul_i64 ops value factor
    ⦃ out => decode out = decode value * factor.val ⦄
  min : ∀ left right, inst.min ops left right
    ⦃ out => decode out = min (decode left) (decode right) ⦄
  max : ∀ left right, inst.max ops left right
    ⦃ out => decode out = max (decode left) (decode right) ⦄
  to_i64 : ∀ value, IScalar.min .I64 ≤ decode value →
    decode value ≤ IScalar.max .I64 →
    ∃ out, inst.to_i64 ops value = ok (some out) ∧ out.val = decode value

def intervalClamp (value lower upper : ℤ) : ℤ :=
  if value < lower then lower else if upper < value then upper else value

theorem interval_clamp_symmetric (value limit : ℤ) :
    intervalClamp value (-limit) limit = Adapters.bounded value limit := rfl

variable {I Ops : Type} (inst : judgment_adapter.IntegerOps Ops I)
  (ops : Ops) (decode : I → ℤ) (h : IntegerAgrees inst ops decode)
include h

/-- In particular the panic branch is unreachable under the atomic conversion
law. The interval endpoints themselves are checked machine integers. -/
theorem bounded_spec (value : I) (lower upper : Std.I64)
    (hordered : lower.val ≤ upper.val) :
    judgment_adapter.bounded inst value lower upper ops
      ⦃ out => out.val = intervalClamp (decode value) lower.val upper.val ⦄ := by
  apply exists_imp_spec
  obtain ⟨lo, hlo, hloVal⟩ := spec_imp_exists (h.from_i64 lower)
  by_cases hlow : decode value < lower.val
  · refine ⟨lower, ?_, ?_⟩
    · simp [judgment_adapter.bounded, hlo, h.less, hloVal, hlow]
    · simp [intervalClamp, hlow]
  · obtain ⟨hi, hhi, hhiVal⟩ := spec_imp_exists (h.from_i64 upper)
    by_cases hhigh : upper.val < decode value
    · refine ⟨upper, ?_, ?_⟩
      · simp [judgment_adapter.bounded, hlo, hhi, h.less, hloVal, hhiVal, hlow, hhigh]
      · simp [intervalClamp, hlow, hhigh]
    · have hmin : IScalar.min .I64 ≤ decode value := by
        have hlower : IScalar.min .I64 ≤ lower.val := by scalar_tac
        omega
      have hmax : decode value ≤ IScalar.max .I64 := by
        have hupper : upper.val ≤ IScalar.max .I64 := by scalar_tac
        omega
      obtain ⟨out, hout, houtVal⟩ := h.to_i64 value hmin hmax
      refine ⟨out, ?_, ?_⟩
      · simp [judgment_adapter.bounded, hlo, hhi, h.less, hloVal, hhiVal,
          hlow, hhigh, hout]
      · simpa [intervalClamp, hlow, hhigh] using houtVal

theorem diff_rep_spec (value : I) :
    judgment_adapter.diff_rep inst value ops
      ⦃ out => out.micros.val = Adapters.bounded (decode value) 1000000 ⦄ := by
  obtain ⟨out, hout, houtVal⟩ := spec_imp_exists
    (bounded_spec inst ops decode h value (-1000000)#i64 1000000#i64 (by decide))
  apply exists_imp_spec
  refine ⟨⟨out⟩, ?_, ?_⟩
  · simp [judgment_adapter.diff_rep, hout, time.Duration.from_micros]
  · simpa only [IScalar.ofInt_val_eq, interval_clamp_symmetric] using houtVal

theorem tap_equiv (value : I) (isEX : Bool) :
    judgment_adapter.judge_tap inst value isEX ops =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeTap
        (LnmaiCore.Duration.fromMicros (decode value)) isEX)) := by
  obtain ⟨diff, hdiff, hval⟩ := spec_imp_exists (diff_rep_spec inst ops decode h value)
  simpa only [judgment_adapter.judge_tap, hdiff, bind_tc_ok] using
    Adapters.tap_adapter_equiv (decode value) diff isEX hval

theorem touch_equiv (value : I) (isEX : Bool) :
    judgment_adapter.judge_touch inst value isEX ops =
      ok ((LnmaiCore.Judge.judgeTouch (LnmaiCore.Duration.fromMicros (decode value))
        isEX).map ofLnmJudgeGrade) := by
  obtain ⟨diff, hdiff, hval⟩ := spec_imp_exists (diff_rep_spec inst ops decode h value)
  simpa only [judgment_adapter.judge_touch, hdiff, bind_tc_ok] using
    Adapters.touch_adapter_equiv (decode value) diff isEX hval

theorem slide_classic_equiv (value : I) :
    judgment_adapter.judge_slide_classic inst value ops =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeSlideClassic
        (LnmaiCore.Duration.fromMicros (decode value)))) := by
  obtain ⟨diff, hdiff, hval⟩ := spec_imp_exists (diff_rep_spec inst ops decode h value)
  simpa only [judgment_adapter.judge_slide_classic, hdiff, bind_tc_ok] using
    Adapters.slide_classic_adapter_equiv (decode value) diff hval

theorem slide_modern_equiv (value stay : I) (isEX : Bool) :
    judgment_adapter.judge_slide_modern inst value stay isEX ops =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeSlideModern
        (LnmaiCore.Duration.fromMicros (decode value))
        (LnmaiCore.Duration.fromMicros (decode stay)) isEX)) := by
  obtain ⟨diff, hdiff, hdiffVal⟩ := spec_imp_exists (diff_rep_spec inst ops decode h value)
  obtain ⟨stayRep, hstay, hstayVal⟩ := spec_imp_exists
    (bounded_spec inst ops decode h stay (-4000000)#i64 4000000#i64 (by decide))
  have hs : stayRep.val = Adapters.bounded (decode stay) 4000000 := by
    simpa only [IScalar.ofInt_val_eq, interval_clamp_symmetric] using hstayVal
  simpa only [judgment_adapter.judge_slide_modern, hdiff, hstay, bind_tc_ok,
    time.Duration.from_micros] using
    Adapters.slide_modern_adapter_equiv (decode value) (decode stay)
      diff ⟨stayRep⟩ isEX hdiffVal hs

/-- Remaining queue length is related to a Nat only at this boundary. Large
queue counts are never converted to a bounded machine integer. -/
theorem slide_too_late_equiv (remaining : I) (count : Nat)
    (hcount : decode remaining = (count : ℤ)) :
    judgment_adapter.judge_slide_too_late inst remaining ops =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeSlideTooLate count)) := by
  obtain ⟨one, hone, honeVal⟩ := spec_imp_exists (h.from_i64 1#i64)
  by_cases heq : count = 1
  · simp [judgment_adapter.judge_slide_too_late, hone, h.equal, honeVal,
      hcount, heq, judge.judge_slide_too_late, LnmaiCore.Judge.judgeSlideTooLate,
      ofLnmJudgeGrade]
  · have hne : (count : ℤ) ≠ 1 := by omega
    simp [judgment_adapter.judge_slide_too_late, hone, h.equal, honeVal,
      hcount, heq, hne, judge.judge_slide_too_late, LnmaiCore.Judge.judgeSlideTooLate,
      ofLnmJudgeGrade]

theorem count_fast_late_equiv (grade : types.JudgeGrade) (value : I)
    (display : types.JudgeDisplayOption) :
    judgment_adapter.count_fast_late inst grade value display ops =
      ok (LnmaiCore.Score.countFastLate (toLnmJudgeGrade grade)
        (LnmaiCore.Duration.fromMicros (decode value)) (toLnmDisplayOption display)) := by
  obtain ⟨diff, hdiff, hval⟩ := spec_imp_exists (diff_rep_spec inst ops decode h value)
  simpa only [judgment_adapter.count_fast_late, hdiff, bind_tc_ok] using
    Adapters.count_fast_late_adapter_equiv grade (decode value) diff display hval

/-- All integer products remain exact, and equality stays in the earlier band. -/
theorem press_band_equiv (held reality : I) :
    judgment_adapter.press_band_micros inst held reality ops =
      ok (Adapters.pressBandRepresentative (decode held) (decode reality)) := by
  obtain ⟨held100, hh, hhVal⟩ := spec_imp_exists (h.mul_i64 held 100#i64)
  obtain ⟨real67, h67, h67Val⟩ := spec_imp_exists (h.mul_i64 reality 67#i64)
  obtain ⟨real33, h33, h33Val⟩ := spec_imp_exists (h.mul_i64 reality 33#i64)
  obtain ⟨real5, h5, h5Val⟩ := spec_imp_exists (h.mul_i64 reality 5#i64)
  simp only [judgment_adapter.press_band_micros, h.less, hh, h67, h33, h5,
    hhVal, h67Val, h33Val, h5Val, IScalar.ofInt_val_eq, bind_tc_ok, decide_eq_true_eq,
    Adapters.pressBandRepresentative]
  split_ifs <;> first | rfl | omega

/-- The actual Rust min/max and subtraction sequence refines the already
proved unbounded hold formula. No entire hold formula is an assumption. -/
theorem modern_hold_refines (head : types.JudgeGrade)
    (diff length ignore release : I) :
    judgment_adapter.judge_hold_end inst head diff length ignore release ops =
      Adapters.modernHoldAdapter head (decode diff) (decode length)
        (decode ignore) (decode release) := by
  obtain ⟨zero, hz, hzVal⟩ := spec_imp_exists (h.from_i64 0#i64)
  obtain ⟨grace, hg, hgVal⟩ := spec_imp_exists (h.from_i64 300000#i64)
  obtain ⟨maximumRaw, hmr, hmrVal⟩ := spec_imp_exists (h.sub length grace)
  obtain ⟨maximum, hm, hmVal⟩ := spec_imp_exists (h.max maximumRaw zero)
  obtain ⟨realityRaw, hrr, hrrVal⟩ := spec_imp_exists (h.sub length ignore)
  obtain ⟨nonnegative, hn, hnVal⟩ := spec_imp_exists (h.max realityRaw zero)
  obtain ⟨reality, hr, hrVal⟩ := spec_imp_exists (h.min nonnegative maximum)
  obtain ⟨heldRaw, hhr, hhrVal⟩ := spec_imp_exists (h.sub reality release)
  obtain ⟨held, hh, hhVal⟩ := spec_imp_exists (h.max heldRaw zero)
  simp only [judgment_adapter.judge_hold_end, hz, hg, hmr, hm, hrr, hn, hr,
    hhr, hh, h.less, press_band_equiv inst ops decode h,
    hzVal, hgVal, hmrVal, hmVal, hrrVal, hnVal, hrVal, hhrVal, hhVal,
    IScalar.ofInt_val_eq, bind_tc_ok, decide_eq_true_eq, Adapters.modernHoldAdapter]
  split_ifs <;> simp_all only [← decide_not, not_lt] <;> omega

theorem modern_hold_equiv (head : types.JudgeGrade)
    (diff length ignore release : I) :
    judgment_adapter.judge_hold_end inst head diff length ignore release ops =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeHoldEnd (toLnmJudgeGrade head)
        (LnmaiCore.Duration.fromMicros (decode diff))
        (LnmaiCore.Duration.fromMicros (decode length))
        (LnmaiCore.Duration.fromMicros (decode ignore))
        (LnmaiCore.Duration.fromMicros (decode release)))) := by
  rw [modern_hold_refines inst ops decode h]
  exact Adapters.modern_hold_adapter_equiv head (decode diff) (decode length)
    (decode ignore) (decode release)

theorem classic_hold_equiv (head : types.JudgeGrade) (timing length release : I) :
    judgment_adapter.judge_hold_classic_end inst head timing length release ops =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeHoldClassicEnd (toLnmJudgeGrade head)
        (LnmaiCore.TimePoint.fromMicros (decode timing))
        (LnmaiCore.Duration.fromMicros (decode length))
        (LnmaiCore.TimePoint.fromMicros (decode release)))) := by
  obtain ⟨last, hl, hlVal⟩ := spec_imp_exists (h.add timing length)
  obtain ⟨diff, hd, hdVal⟩ := spec_imp_exists (h.sub last release)
  obtain ⟨representative, hr, hrVal⟩ := spec_imp_exists
    (bounded_spec inst ops decode h diff (-1000000)#i64 1000000#i64 (by decide))
  have hrep : representative.val =
      Adapters.bounded (decode timing + decode length - decode release) 1000000 := by
    simpa only [IScalar.ofInt_val_eq, interval_clamp_symmetric, hdVal, hlVal] using hrVal
  simpa only [judgment_adapter.judge_hold_classic_end, hl, hd, hr, bind_tc_ok,
    time.Duration.zero] using Adapters.classic_hold_adapter_equiv
      head (decode timing) (decode length) (decode release) representative hrep

omit h in
/-- Exact cancellation happens before limiting the difference. This identity
also covers offsets and differences outside all machine-integer ranges. -/
theorem too_late_adjusted_equiv (diff offset : ℤ) :
    LnmaiCore.Judge.isTooLateSlide
      (LnmaiCore.Duration.fromMicros
        (Adapters.bounded (diff - min offset 0) 1000000))
      LnmaiCore.Duration.zero =
    LnmaiCore.Judge.isTooLateSlide (LnmaiCore.Duration.fromMicros diff)
      (LnmaiCore.Duration.fromMicros offset) := by
  simp only [LnmaiCore.Judge.isTooLateSlide,
    lnmDuration_lt_iff_toMicros, Duration.toMicros_add,
    Duration.toMicros_min, Adapters.from_micros_value, zero_toMicros,
    slideGood_spec, min_self, add_zero]
  have hcmp : (600012 < Adapters.bounded (diff - min offset 0) 1000000) ↔
      600012 + min offset 0 < diff := by
    unfold Adapters.bounded
    split_ifs <;> omega
  simp only [hcmp]

theorem is_too_late_equiv (diff offset : I) :
    judgment_adapter.is_too_late_slide inst diff offset ops =
      ok (LnmaiCore.Judge.isTooLateSlide (LnmaiCore.Duration.fromMicros (decode diff))
        (LnmaiCore.Duration.fromMicros (decode offset))) := by
  obtain ⟨zero, hz, hzVal⟩ := spec_imp_exists (h.from_i64 0#i64)
  obtain ⟨minimum, hm, hmVal⟩ := spec_imp_exists (h.min offset zero)
  obtain ⟨adjusted, ha, haVal⟩ := spec_imp_exists (h.sub diff minimum)
  obtain ⟨rep, hr, hrVal⟩ := spec_imp_exists (diff_rep_spec inst ops decode h adjusted)
  have hk := JudgeProof.is_too_late_slide_equiv rep ⟨0#i64⟩ (by scalar_tac) (by scalar_tac)
  have hval : rep.micros.val =
      Adapters.bounded (decode diff - min (decode offset) 0) 1000000 := by
    simpa only [haVal, hmVal, hzVal, IScalar.ofInt_val_eq] using hrVal
  simpa only [judgment_adapter.is_too_late_slide, hz, hm, ha, hr, bind_tc_ok,
    time.Duration.zero, toLnmDuration, hval, IScalar.ofInt_val_eq,
    show LnmaiCore.Duration.fromMicros 0 = LnmaiCore.Duration.zero from rfl,
    too_late_adjusted_equiv] using hk

end Verification.JudgmentAdapterProof
