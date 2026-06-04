# Lifecycle Module Verification

## Status: PARTIALLY VERIFIED

## Lean Definitions

```lean
inductive TapState where
  | Waiting | Judgeable | Judged (grade : JudgeGrade) | Ended

inductive HoldSubState where
  | HeadWaiting | HeadJudgeable | HeadJudged | BodyHeld | BodyReleased | Ended

inductive TouchState where
  | Waiting | Judgeable | Judged (grade : JudgeGrade) | Ended

inductive SlideState where
  | Waiting | Active (waitTime : Duration) | Judged (grade : JudgeGrade) (waitTime : Duration) (judgeDiff : Duration) | Ended
```

## Rust Implementation

```rust
pub enum TapState { Waiting, Judgeable, Judged(JudgeGrade), Ended }
pub enum HoldSubState { HeadWaiting, HeadJudgeable, HeadJudged, BodyHeld, BodyReleased, Ended }
pub enum TouchState { Waiting, Judgeable, Judged(JudgeGrade), Ended }
pub enum SlideState { Waiting, Active { wait_time: i64 }, Judged { grade: JudgeGrade, wait_time: i64, judge_diff: i64 }, Ended }
```

## Proof Obligations

### 1. TapState transitions
**Lean:** Waiting → Judgeable → Judged → Ended
**Rust:** Verified through tap_step function
**Status:** PARTIALLY VERIFIED

### 2. HoldSubState transitions
**Lean:** HeadWaiting → HeadJudgeable → HeadJudged → BodyHeld/BodyReleased → Ended
**Rust:** Verified through hold_step function
**Status:** PARTIALLY VERIFIED

### 3. TouchState transitions
**Lean:** Waiting → Judgeable → Judged → Ended
**Rust:** Verified through touch_step function
**Status:** PARTIALLY VERIFIED

### 4. SlideState transitions
**Lean:** Waiting → Active → Judged → Ended
**Rust:** Verified through slide_step function
**Status:** PARTIALLY VERIFIED

## Missing Lemmas

- Full state machine transition proofs
- No backward transitions invariant
- Timing boundary conditions

## Aeneas Compatibility

The lifecycle types use enums and structs, compatible with Aeneas. The main concern is the `Vec` usage in `SlideNote.judge_queues`.
