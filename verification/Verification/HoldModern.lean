import Verification.Hold

open Aeneas Aeneas.Std Result lnmai_core_verify
set_option maxHeartbeats 1000000
set_option maxErrors 8

namespace Verification.JudgeProof
open Bridge LnmaiCore.JudgeGrade

def pressBandCore (held reality : ℤ) : Nat :=
  if held ≥ reality then 0
  else if held * 100 ≥ reality * 67 then 1
  else if held * 100 ≥ reality * 33 then 2
  else if held * 100 ≥ reality * 5 then 3 else 4

/-- The finite grade table independently expressed over the Lean grade type. -/
def holdBandCore (headGrade : LnmaiCore.JudgeGrade) (isLate : Prop) [Decidable isLate]
    (band : Nat) : LnmaiCore.JudgeGrade :=
  match band with
  | 0 =>  -- ≥ 100%: release never or very late
    match headGrade with
    | LatePerfect3rd | LatePerfect2nd | LnmaiCore.JudgeGrade.Perfect | FastPerfect2nd | FastPerfect3rd =>
      headGrade
    | LateGood | LateGreat3rd | LateGreat2nd | LateGreat =>
      LateGreat
    | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
      FastGreat
    | Miss   => LateGood
    | TooFast => FastGood
  | 1 =>  -- [67%, 100%): release slightly early
    match headGrade with
    | LnmaiCore.JudgeGrade.Perfect =>
      if isLate then LatePerfect2nd else FastPerfect2nd
    | LatePerfect3rd | LatePerfect2nd | FastPerfect2nd | FastPerfect3rd =>
      headGrade
    | LateGood | LateGreat3rd | LateGreat2nd | LateGreat =>
      LateGreat
    | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
      FastGreat
    | Miss   => LateGood
    | TooFast => FastGood
  | 2 =>  -- [33%, 67%): release moderately early
    match headGrade with
    | LnmaiCore.JudgeGrade.Perfect =>
      if isLate then LateGreat2nd else FastGreat2nd
    | LateGood | LateGreat3rd | LateGreat2nd | LateGreat | LatePerfect3rd | LatePerfect2nd =>
      LateGreat
    | FastPerfect2nd | FastPerfect3rd | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
      FastGreat
    | Miss   => LateGood
    | TooFast => FastGood
  | 3 =>  -- [5%, 33%): release very early
    match headGrade with
    | LnmaiCore.JudgeGrade.Perfect =>
      if isLate then LateGood else FastGood
    | Miss | LateGood | LateGreat3rd | LateGreat2nd | LateGreat | LatePerfect3rd | LatePerfect2nd =>
      LateGood
    | FastPerfect2nd | FastPerfect3rd | FastGreat | FastGreat2nd | FastGreat3rd | FastGood | TooFast =>
      FastGood
  | _ =>  -- [0%, 5%): release almost immediately
    match headGrade with
    | LnmaiCore.JudgeGrade.Perfect =>
      if isLate then LateGood else FastGood
    | LateGood | LateGreat3rd | LateGreat2nd | LateGreat | LatePerfect3rd | LatePerfect2nd =>
      LateGood
    | FastPerfect2nd | FastPerfect3rd | FastGreat | FastGreat2nd | FastGreat3rd | FastGood =>
      FastGood
    | Miss | TooFast =>
      headGrade

theorem hold_band_grade_equiv (head : types.JudgeGrade) (isLate : Bool)
    (band : Std.U32) :
    judge.hold_band_grade head isLate band =
      ok (ofLnmJudgeGrade (holdBandCore (toLnmJudgeGrade head) (isLate = true) band.val)) := by
  by_cases h : band.val < 4
  · interval_cases hv : band.val
    · have hb : band = 0#u32 := UScalar.eq_of_val_eq (by simpa using hv)
      subst band
      cases head <;> cases isLate <;> rfl
    · have hb : band = 1#u32 := UScalar.eq_of_val_eq (by simpa using hv)
      subst band
      cases head <;> cases isLate <;> rfl
    · have hb : band = 2#u32 := UScalar.eq_of_val_eq (by simpa using hv)
      subst band
      cases head <;> cases isLate <;> rfl
    · have hb : band = 3#u32 := UScalar.eq_of_val_eq (by simpa using hv)
      subst band
      cases head <;> cases isLate <;> rfl
  · have h0 : band ≠ 0#u32 := by intro he; subst band; simp at h
    have h1 : band ≠ 1#u32 := by intro he; subst band; simp at h
    have h2 : band ≠ 2#u32 := by intro he; subst band; simp at h
    have h3 : band ≠ 3#u32 := by intro he; subst band; simp at h
    have hv : band.val = (band.val - 4) + 4 := by omega
    unfold judge.hold_band_grade
    split
    all_goals first
      | exact False.elim (h0 rfl)
      | exact False.elim (h1 rfl)
      | exact False.elim (h2 rfl)
      | exact False.elim (h3 rfl)
      | (rw [hv]; cases head <;> cases isLate <;> rfl)

def modernHoldCore (head : LnmaiCore.JudgeGrade) (diff length ignore release : ℤ) :
    LnmaiCore.JudgeGrade :=
  let reality := max 0 (min (length - ignore) (length - 300000))
  if reality ≤ 0 then head else
    holdBandCore head (diff ≥ 0) (pressBandCore (max 0 (reality - release)) reality)

theorem duration_max_micros (a b : LnmaiCore.Duration) :
    (max a b).toMicros = max a.toMicros b.toMicros := by
  change (if a ≤ b then b else a).toMicros = _
  by_cases h : a ≤ b
  · rw [if_pos h, max_eq_right h]
  · rw [if_neg h, max_eq_left (le_of_lt (not_le.mp h))]

theorem modern_hold_expand (head : LnmaiCore.JudgeGrade) (diff length ignore release : ℤ) :
    LnmaiCore.Judge.judgeHoldEnd head (LnmaiCore.Duration.fromMicros diff)
      (LnmaiCore.Duration.fromMicros length) (LnmaiCore.Duration.fromMicros ignore)
      (LnmaiCore.Duration.fromMicros release) = modernHoldCore head diff length ignore release := by
  let reality := max LnmaiCore.Duration.zero
    (min (LnmaiCore.Duration.fromMicros (length - ignore))
      (LnmaiCore.Duration.fromMicros (length - 300000)))
  change (if reality.toMicros ≤ 0 then head else
    holdBandCore head (diff ≥ 0)
      (pressBandCore (max LnmaiCore.Duration.zero
        (reality - LnmaiCore.Duration.fromMicros release)).toMicros reality.toMicros)) = _
  have hsub (a b : LnmaiCore.Duration) : (a - b).toMicros = a.toMicros - b.toMicros := rfl
  simp only [duration_max_micros, hsub, reality, Duration.toMicros_min]
  rfl

@[step] theorem hold_band_grade_spec (head : types.JudgeGrade) (isLate : Bool)
    (band : Std.U32) :
    judge.hold_band_grade head isLate band ⦃ r =>
      r = ofLnmJudgeGrade (holdBandCore (toLnmJudgeGrade head) (isLate = true) band.val) ⦄ := by
  rw [hold_band_grade_equiv]
  simp

/-- Modern hold arithmetic and the entire grade table agree with Lean. The
    input bounds ensure every checked subtraction/product is representable. -/
theorem judge_hold_end_equiv (head : types.JudgeGrade)
    (diff length ignore release : time.Duration)
    (hl : -2^38 ≤ length.micros.val ∧ length.micros.val ≤ 2^38)
    (hi : -2^38 ≤ ignore.micros.val ∧ ignore.micros.val ≤ 2^38)
    (hr : -2^38 ≤ release.micros.val ∧ release.micros.val ≤ 2^38) :
    judge.judge_hold_end head diff length ignore release =
      ok (ofLnmJudgeGrade (LnmaiCore.Judge.judgeHoldEnd (toLnmJudgeGrade head)
        (toLnmDuration diff) (toLnmDuration length)
        (toLnmDuration ignore) (toLnmDuration release))) := by
  simp only [toLnmDuration, modern_hold_expand]
  obtain ⟨raw, hraw, hrawval⟩ := WP.spec_imp_exists
    (IScalar.sub_spec (by scalar_tac) (by scalar_tac) :
      length.micros - ignore.micros ⦃ r => r.val = length.micros.val - ignore.micros.val ⦄)
  obtain ⟨maximum, hmaximum, hmaximumval⟩ := WP.spec_imp_exists
    (IScalar.sub_spec (by scalar_tac) (by scalar_tac) :
      length.micros - (300000#i64 : Std.I64) ⦃ r => r.val = length.micros.val - 300000 ⦄)
  let cap : Std.I64 := if length.micros > 300000#i64 then maximum else 0#i64
  let reality : Std.I64 := if raw < 0#i64 then 0#i64 else if raw > cap then cap else raw
  have hreality : reality.val = max 0 (min (length.micros.val - ignore.micros.val)
      (length.micros.val - 300000)) := by
    dsimp only [reality, cap]
    split_ifs <;> simp_all only [i64_lt_iff, IScalar.ofInt_val_eq, max_def, min_def] <;>
      split_ifs <;> omega
  have hrange : 0 ≤ reality.val ∧ reality.val ≤ 2^39 := by
    rw [hreality]
    simp only [max_def, min_def]
    split_ifs <;> omega
  have hnormalize : judge.judge_hold_end head diff length ignore release =
      (if reality ≤ 0#i64 then ok head else do
        let remaining ← reality - release.micros
        let held ← if remaining < 0#i64 then ok 0#i64 else ok remaining
        let band ← judge.press_band_micros held reality
        judge.hold_band_grade head (diff.micros >= 0#i64) band) := by
    simp only [judge.judge_hold_end, hraw, time.Duration.from_micros,
      time.Duration.zero, time.Duration.to_micros, bind_tc_ok, hmaximum]
    dsimp only [reality, cap]
    split_ifs <;> simp_all only [bind_tc_ok, ite_true, ite_false, le_refl, not_true_eq_false]
  rw [hnormalize]
  by_cases hz : reality ≤ 0#i64
  · have hzval : max 0 (min (length.micros.val - ignore.micros.val)
        (length.micros.val - 300000)) ≤ 0 := by
      simpa only [i64_le_iff, IScalar.ofInt_val_eq, hreality] using hz
    simp only [if_pos hz, modernHoldCore, if_pos hzval]
    cases head <;> rfl
  · obtain ⟨remaining, hremaining, hremainingval⟩ := WP.spec_imp_exists
      (IScalar.sub_spec (by scalar_tac) (by scalar_tac) :
        reality - release.micros ⦃ r => r.val = reality.val - release.micros.val ⦄)
    let held : Std.I64 := if remaining < 0#i64 then 0#i64 else remaining
    have hheld : held.val = max 0 (reality.val - release.micros.val) := by
      dsimp only [held]
      split_ifs <;> simp_all only [i64_lt_iff, IScalar.ofInt_val_eq, max_def] <;>
        split_ifs <;> omega
    have hheldrange : -2^40 ≤ held.val ∧ held.val ≤ 2^40 := by
      rw [hheld]
      simp only [max_def]
      split_ifs <;> omega
    obtain ⟨band, hband, hbandval⟩ := WP.spec_imp_exists
      (press_band_spec held reality hheldrange (by omega))
    have hzval : ¬max 0 (min (length.micros.val - ignore.micros.val)
        (length.micros.val - 300000)) ≤ 0 := by
      simpa only [i64_le_iff, IScalar.ofInt_val_eq, hreality] using hz
    have hheldresult : (if remaining < 0#i64 then ok 0#i64 else ok remaining) = ok held := by
      dsimp only [held]
      split <;> rfl
    simp only [if_neg hz, hremaining, bind_tc_ok, hheldresult]
    change (do let b ← judge.press_band_micros held reality
               judge.hold_band_grade head (diff.micros >= 0#i64) b) = _
    rw [hband]
    simp only [bind_tc_ok, hold_band_grade_equiv, hbandval, modernHoldCore,
      if_neg hzval, pressBandCore, hheld, hreality, decide_eq_true_eq,
      i64_le_iff, IScalar.ofInt_val_eq]

end Verification.JudgeProof
