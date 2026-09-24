import Verification.Generated
import Verification.Bridge
import Aeneas

open Aeneas Aeneas.Std Result

namespace Verification.GeneratedExt

open aeneas_core_verify

----------------------------------------------------------------------------
-- judge_slide_modern: Aeneas-style translation of the Rust function
----------------------------------------------------------------------------

def judge.judge_slide_modern
  (diff : time.Duration) (stay_time : time.Duration) (_is_ex : Bool) :
  Result types.JudgeGrade := do
  let diff_msec ← time.Duration.abs diff
  -- Dynamic extension: ext = min(stay_time / 4, 22-frame max)
  let stay_div4 ← time.Duration.div_nat stay_time 4#u32
  let ext := if stay_div4.micros < constants.SLIDE_MAX_EXT.micros
             then stay_div4 else constants.SLIDE_MAX_EXT
  let seg_3rd ← time.Duration.Insts.CoreOpsArithAddDurationDuration.add
    constants.SLIDE_PERFECT_3RD ext
  let seg_1st ← time.Duration.div_nat seg_3rd 3#u32
  let seg_2nd_scaled ← time.Duration.scale_nat seg_3rd 2#u32
  let seg_2nd ← time.Duration.div_nat seg_2nd_scaled 3#u32
  if diff_msec.micros <= seg_1st.micros
  then ok types.JudgeGrade.Perfect
  else if diff_msec.micros <= seg_2nd.micros
  then
    if diff.micros < 0#i64
    then ok types.JudgeGrade.FastPerfect2nd
    else ok types.JudgeGrade.LatePerfect2nd
  else if diff_msec.micros <= seg_3rd.micros
  then
    if diff.micros < 0#i64
    then ok types.JudgeGrade.FastPerfect3rd
    else ok types.JudgeGrade.LatePerfect3rd
  else if diff_msec.micros <= constants.SLIDE_GREAT_1ST.micros
  then
    if diff.micros < 0#i64
    then ok types.JudgeGrade.FastGreat
    else ok types.JudgeGrade.LateGreat
  else if diff_msec.micros <= constants.SLIDE_GREAT_2ND.micros
  then
    if diff.micros < 0#i64
    then ok types.JudgeGrade.FastGreat2nd
    else ok types.JudgeGrade.LateGreat2nd
  else if diff_msec.micros <= constants.SLIDE_GREAT_3RD.micros
  then
    if diff.micros < 0#i64
    then ok types.JudgeGrade.FastGreat3rd
    else ok types.JudgeGrade.LateGreat3rd
  else
    if diff.micros < 0#i64
    then ok types.JudgeGrade.FastGood
    else ok types.JudgeGrade.LateGood

----------------------------------------------------------------------------
-- press_band_micros
----------------------------------------------------------------------------

def judge.press_band_micros (held_micros : I64) (reality_micros : I64) : Result U32 := do
  if held_micros >= reality_micros
  then ok 0#u32
  else
    let h100 ← held_micros * 100#i64
    let r67 ← reality_micros * 67#i64
    if h100 >= r67
    then ok 1#u32
    else
      let r33 ← reality_micros * 33#i64
      if h100 >= r33
      then ok 2#u32
      else
        let r5 ← reality_micros * 5#i64
        if h100 >= r5
        then ok 3#u32
        else ok 4#u32

----------------------------------------------------------------------------
-- judge_hold_end
----------------------------------------------------------------------------

def judge.judge_hold_end
  (head_grade : types.JudgeGrade) (judge_diff : time.Duration)
  (length : time.Duration) (ignore_time : time.Duration)
  (player_release_time : time.Duration) : Result types.JudgeGrade := do
  let b ← types.JudgeGrade.Insts.CoreCmpPartialEqJudgeGrade.eq head_grade head_grade
  let is_fast ← types.JudgeGrade.is_fast head_grade
  let offset := if is_fast then time.Duration.mk 0#i64 else judge_diff
  let reality_ht_raw ←
    let t1 ← time.Duration.Insts.CoreOpsArithSubDurationDuration.sub length ignore_time
    time.Duration.Insts.CoreOpsArithSubDurationDuration.sub t1 offset
  let hold_max ← time.Duration.from_micros 300000#i64
  let reality_ht_max ←
    time.Duration.Insts.CoreOpsArithSubDurationDuration.sub length hold_max
  let reality_ht ←
    if reality_ht_raw.micros <= 0#i64
    then ok reality_ht_raw
    else
      if reality_ht_raw.micros <= reality_ht_max.micros
      then ok reality_ht_raw
      else ok reality_ht_max
  if reality_ht.micros <= 0#i64
  then ok head_grade
  else do
    let held ←
      let h ← time.Duration.Insts.CoreOpsArithSubDurationDuration.sub
        reality_ht player_release_time
      if h.micros < 0#i64
      then time.Duration.from_micros 0#i64
      else ok h
    let held_m ← time.Duration.to_micros held
    let real_m ← time.Duration.to_micros reality_ht
    let band ← judge.press_band_micros held_m real_m
    -- The match on band (0-4) and then on head_grade
    -- is pure structural; we write it as a nested match
    match band with
    | 0#u32 =>
      match head_grade with
      | types.JudgeGrade.LatePerfect3rd => ok head_grade
      | types.JudgeGrade.LatePerfect2nd => ok head_grade
      | types.JudgeGrade.Perfect => ok head_grade
      | types.JudgeGrade.FastPerfect2nd => ok head_grade
      | types.JudgeGrade.FastPerfect3rd => ok head_grade
      | types.JudgeGrade.LateGood => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat3rd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat2nd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.FastGreat => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat2nd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat3rd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGood => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.Miss => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.TooFast => ok types.JudgeGrade.FastGood
    | 1#u32 =>
      match head_grade with
      | types.JudgeGrade.Perfect =>
        if judge_diff.micros >= 0#i64
        then ok types.JudgeGrade.LatePerfect2nd
        else ok types.JudgeGrade.FastPerfect2nd
      | types.JudgeGrade.LatePerfect3rd => ok head_grade
      | types.JudgeGrade.LatePerfect2nd => ok head_grade
      | types.JudgeGrade.FastPerfect2nd => ok head_grade
      | types.JudgeGrade.FastPerfect3rd => ok head_grade
      | types.JudgeGrade.LateGood => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat3rd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat2nd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.FastGreat => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat2nd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat3rd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGood => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.Miss => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.TooFast => ok types.JudgeGrade.FastGood
    | 2#u32 =>
      match head_grade with
      | types.JudgeGrade.Perfect =>
        if judge_diff.micros >= 0#i64
        then ok types.JudgeGrade.LateGreat2nd
        else ok types.JudgeGrade.FastGreat2nd
      | types.JudgeGrade.LateGood => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat3rd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat2nd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LateGreat => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LatePerfect3rd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.LatePerfect2nd => ok types.JudgeGrade.LateGreat
      | types.JudgeGrade.FastPerfect2nd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastPerfect3rd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat2nd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGreat3rd => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.FastGood => ok types.JudgeGrade.FastGreat
      | types.JudgeGrade.Miss => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.TooFast => ok types.JudgeGrade.FastGood
    | 3#u32 =>
      match head_grade with
      | types.JudgeGrade.Perfect =>
        if judge_diff.micros >= 0#i64
        then ok types.JudgeGrade.LateGood
        else ok types.JudgeGrade.FastGood
      | types.JudgeGrade.Miss => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGood => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGreat3rd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGreat2nd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGreat => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LatePerfect3rd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LatePerfect2nd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.FastPerfect2nd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastPerfect3rd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGreat => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGreat2nd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGreat3rd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGood => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.TooFast => ok types.JudgeGrade.FastGood
    | _ =>
      match head_grade with
      | types.JudgeGrade.Perfect =>
        if judge_diff.micros >= 0#i64
        then ok types.JudgeGrade.LateGood
        else ok types.JudgeGrade.FastGood
      | types.JudgeGrade.LateGood => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGreat3rd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGreat2nd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LateGreat => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LatePerfect3rd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.LatePerfect2nd => ok types.JudgeGrade.LateGood
      | types.JudgeGrade.FastPerfect2nd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastPerfect3rd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGreat => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGreat2nd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGreat3rd => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.FastGood => ok types.JudgeGrade.FastGood
      | types.JudgeGrade.Miss => ok head_grade
      | types.JudgeGrade.TooFast => ok head_grade

----------------------------------------------------------------------------
-- judge_hold_classic_end
----------------------------------------------------------------------------

def judge.judge_hold_classic_end
  (head_grade : types.JudgeGrade) (timing : I64)
  (length : time.Duration) (release_timing : I64) :
  Result types.JudgeGrade := do
  let b ← types.JudgeGrade.is_miss_or_too_fast head_grade
  if b
  then ok head_grade
  else do
    let len_m ← time.Duration.to_micros length
    let diff_micros ←
      let s ← timing + len_m
      s - release_timing
    let diff := time.Duration.mk diff_micros
    let abs_diff ← time.Duration.abs diff
    let is_fast := diff.micros > 0#i64
    let end_grade ←
      if is_fast
      then
        if abs_diff.micros < constants.HOLD_HEAD_IGNORE.micros
        then ok types.JudgeGrade.Perfect
        else ok types.JudgeGrade.FastGood
      else
        if abs_diff.micros < constants.HOLD_TAIL_IGNORE.micros
        then ok types.JudgeGrade.Perfect
        else ok types.JudgeGrade.LateGood
    let head_dist ← types.JudgeGrade.dist_from_perfect head_grade
    let end_dist ← types.JudgeGrade.dist_from_perfect end_grade
    if end_dist > head_dist
    then ok end_grade
    else ok head_grade

----------------------------------------------------------------------------
-- is_too_late_slide
----------------------------------------------------------------------------

def judge.is_too_late_slide
  (diff : time.Duration) (user_offset : time.Duration) : Result Bool := do
  let min_offset :=
    if user_offset.micros < 0#i64 then user_offset
    else time.Duration.mk 0#i64
  let thresh ←
    time.Duration.Insts.CoreOpsArithAddDurationDuration.add
      constants.SLIDE_GOOD min_offset
  diff.micros > thresh.micros

----------------------------------------------------------------------------
-- score_break
----------------------------------------------------------------------------

def score.score_break
  (grade : types.JudgeGrade) (multiple : U32) :
  Result (U32 × U32 × U32 × U32 × U32 × U32) := do
  let m := multiple
  match grade with
  | types.JudgeGrade.Miss =>
    let b2500 ← 2500#u32 * m
    let b100 ← 100#u32 * m
    ok (0#u32, 0#u32, 0#u32, b2500, b100, b100)
  | types.JudgeGrade.TooFast =>
    let b2500 ← 2500#u32 * m
    let b100 ← 100#u32 * m
    ok (0#u32, 0#u32, 0#u32, b2500, b100, b100)
  | types.JudgeGrade.LateGood =>
    let b1000 ← 1000#u32 * m
    let b30 ← 30#u32 * m
    let b1500 ← 1500#u32 * m
    let b70 ← 70#u32 * m
    let b100 ← 100#u32 * m
    ok (b1000, b30, 0#u32, b1500, b70, b100)
  | types.JudgeGrade.FastGood =>
    let b1000 ← 1000#u32 * m
    let b30 ← 30#u32 * m
    let b1500 ← 1500#u32 * m
    let b70 ← 70#u32 * m
    let b100 ← 100#u32 * m
    ok (b1000, b30, 0#u32, b1500, b70, b100)
  | types.JudgeGrade.LateGreat3rd =>
    let b1250 ← 1250#u32 * m
    let b40 ← 40#u32 * m
    let b1250l ← 1250#u32 * m
    let b60 ← 60#u32 * m
    let b100 ← 100#u32 * m
    ok (b1250, b40, 0#u32, b1250l, b60, b100)
  | types.JudgeGrade.FastGreat3rd =>
    let b1250 ← 1250#u32 * m
    let b40 ← 40#u32 * m
    let b1250l ← 1250#u32 * m
    let b60 ← 60#u32 * m
    let b100 ← 100#u32 * m
    ok (b1250, b40, 0#u32, b1250l, b60, b100)
  | types.JudgeGrade.LateGreat2nd =>
    let b1500 ← 1500#u32 * m
    let b40 ← 40#u32 * m
    let b1000 ← 1000#u32 * m
    let b60 ← 60#u32 * m
    let b100 ← 100#u32 * m
    ok (b1500, b40, 0#u32, b1000, b60, b100)
  | types.JudgeGrade.FastGreat2nd =>
    let b1500 ← 1500#u32 * m
    let b40 ← 40#u32 * m
    let b1000 ← 1000#u32 * m
    let b60 ← 60#u32 * m
    let b100 ← 100#u32 * m
    ok (b1500, b40, 0#u32, b1000, b60, b100)
  | types.JudgeGrade.LateGreat =>
    let b2000 ← 2000#u32 * m
    let b40 ← 40#u32 * m
    let b500 ← 500#u32 * m
    let b60 ← 60#u32 * m
    let b100 ← 100#u32 * m
    ok (b2000, b40, 0#u32, b500, b60, b100)
  | types.JudgeGrade.FastGreat =>
    let b2000 ← 2000#u32 * m
    let b40 ← 40#u32 * m
    let b500 ← 500#u32 * m
    let b60 ← 60#u32 * m
    let b100 ← 100#u32 * m
    ok (b2000, b40, 0#u32, b500, b60, b100)
  | types.JudgeGrade.LatePerfect3rd =>
    let b2500 ← 2500#u32 * m
    let b50 ← 50#u32 * m
    let b100 ← 100#u32 * m
    ok (b2500, b50, 0#u32, 0#u32, b50, b100)
  | types.JudgeGrade.FastPerfect3rd =>
    let b2500 ← 2500#u32 * m
    let b50 ← 50#u32 * m
    let b100 ← 100#u32 * m
    ok (b2500, b50, 0#u32, 0#u32, b50, b100)
  | types.JudgeGrade.LatePerfect2nd =>
    let b2500 ← 2500#u32 * m
    let b75 ← 75#u32 * m
    let b50 ← 50#u32 * m
    let b25 ← 25#u32 * m
    let b50l ← 50#u32 * m
    ok (b2500, b75, b50, 0#u32, b25, b50l)
  | types.JudgeGrade.FastPerfect2nd =>
    let b2500 ← 2500#u32 * m
    let b75 ← 75#u32 * m
    let b50 ← 50#u32 * m
    let b25 ← 25#u32 * m
    let b50l ← 50#u32 * m
    ok (b2500, b75, b50, 0#u32, b25, b50l)
  | types.JudgeGrade.Perfect =>
    let b2500 ← 2500#u32 * m
    let b100 ← 100#u32 * m
    let b100c ← 100#u32 * m
    ok (b2500, b100, b100c, 0#u32, 0#u32, 0#u32)

end Verification.GeneratedExt
