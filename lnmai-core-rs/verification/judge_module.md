# Judge Module Verification

## Status: PARTIALLY VERIFIED

## Lean Definitions

```lean
def judgeTap (diff : Duration) (isEX : Bool := false) : JudgeGrade := ...
def judgeTouch (diff : Duration) (isEX : Bool := false) : Option JudgeGrade := ...
def judgeSlideModern (diff : Duration) (stayTime : Duration) (isEX : Bool := false) : JudgeGrade := ...
def judgeSlideClassic (diff : Duration) : JudgeGrade := ...
def correctSlideGrade : JudgeGrade → JudgeGrade := ...
def judgeHoldEnd ... : JudgeGrade := ...
def judgeSlideTooLate (queueRemaining : Nat) : JudgeGrade := ...
def isTooLateSlide (diff : Duration) (userOffset : Duration := Duration.zero) : Bool := ...
```

## Rust Implementation

```rust
pub fn judge_tap(diff: Duration, is_ex: bool) -> JudgeGrade { ... }
pub fn judge_touch(diff: Duration, is_ex: bool) -> Option<JudgeGrade> { ... }
pub fn judge_slide_modern(diff: Duration, stay_time: Duration, is_ex: bool) -> JudgeGrade { ... }
pub fn judge_slide_classic(diff: Duration) -> JudgeGrade { ... }
pub fn correct_slide_grade(grade: JudgeGrade) -> JudgeGrade { ... }
pub fn judge_hold_end(...) -> JudgeGrade { ... }
pub fn judge_slide_too_late(queue_remaining: u32) -> JudgeGrade { ... }
pub fn is_too_late_slide(diff: Duration, user_offset: Duration) -> bool { ... }
```

## Proof Obligations

### 1. judgeTap EX always Perfect
**Lean:** `∀ diff, judgeTap diff true = Perfect`
**Rust:** `judge_tap(diff, true) == Perfect`
**Status:** VERIFIED (property test)

### 2. judgeTap zero is Perfect
**Lean:** `∀ diff, |diff| ≤ 16667 → judgeTap diff false = Perfect`
**Rust:** `judge_tap(diff, false) == Perfect` for `|diff| ≤ 16667`
**Status:** VERIFIED (property test)

### 3. judgeSlideTooLate single remaining
**Lean:** `judgeSlideTooLate 1 = LateGood`
**Rust:** `judge_slide_too_late(1) == LateGood`
**Status:** VERIFIED (unit test + property test)

### 4. judgeSlideTooLate multi remaining
**Lean:** `∀ n, n ≥ 2 → judgeSlideTooLate n = Miss`
**Rust:** `judge_slide_too_late(n) == Miss` for `n ≥ 2`
**Status:** VERIFIED (property test)

### 5. correctSlideGrade perfect variants
**Lean:** `∀ g ∈ {Perfect, LatePerfect2nd, LatePerfect3rd, FastPerfect2nd, FastPerfect3rd}, correctSlideGrade g = Perfect`
**Rust:** `correct_slide_grade(g) == Perfect` for perfect variants
**Status:** VERIFIED (property test)

### 6. judgeHoldEnd (partial)
**Lean:** Complex 5-band press table
**Rust:** Faithful transcription
**Status:** PARTIALLY VERIFIED (unit tests needed)

### 7. judgeSlideModern (partial)
**Lean:** Dynamic extension based on stayTime
**Rust:** Faithful transcription
**Status:** PARTIALLY VERIFIED (property tests needed)

### 8. judgeSlideClassic (partial)
**Lean:** Fixed windows, separate fast/late thresholds
**Rust:** Faithful transcription
**Status:** PARTIALLY VERIFIED (property tests needed)

## Missing Lemmas

- judgeHoldEnd full behavioral specification
- judgeSlideModern extension boundary conditions
- judgeSlideClassic threshold matching

## Aeneas Compatibility

The judge functions are pure functions with pattern matching, compatible with Aeneas. The main concern is the `Duration` arithmetic which uses `i64`.
