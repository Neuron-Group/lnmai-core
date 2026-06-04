# LnmaiCore Lean Project Architecture

## 1. Project Overview

**Purpose:** A verified game judgment runtime for the maimai (mai2) rhythm game. The project is a formal Lean specification of the game's note judgment, scoring, and lifecycle engine, intended to serve as the "source of truth" for a Rust rewrite.

**Root module file:** `LnmaiCore/Basic.lean`

---

## 2. Module Tree

```
LnmaiCore/
├── Basic.lean (umbrella re-export)
├── Types.lean (core types and enums)
├── Areas.lean (sensor/button/slot enums)
├── Time.lean (time primitives)
├── Constants.lean (game constants)
├── Convert.lean (grade conversion)
├── Judge.lean (judgment logic)
├── Score.lean (scoring computation)
├── Storage.lean (indexed vectors)
├── Lifecycle.lean (note state machines)
├── InputModel.lean (input processing)
├── ChartLoader.lean (chart parsing/loading)
├── Scheduler.lean (frame processing)
├── FFI.lean (foreign function interface)
├── Domain.lean (empty module)
├── Simai.lean (umbrella for Simai subsystem)
├── Simai/
│   ├── Syntax.lean (AST definitions)
│   ├── Symmetry.lean (symmetry transformations)
│   ├── Timing.lean (timing computation)
│   ├── Tokenize.lean (tokenizer/lexer)
│   ├── Shape.lean (slide shape definitions)
│   ├── SlideTables.lean (slide path lookup tables)
│   ├── SlideParser.lean (slide path parser)
│   ├── IR.lean (intermediate representation)
│   ├── Source/Maidata.lean (maidata.txt parser)
│   ├── Maidata.lean (maidata higher-level parsing)
│   ├── Frontend.lean (public API)
│   ├── DSL.lean (DSL helpers)
│   ├── Normalize.lean (chart normalization)
│   ├── Typecheck.lean (type checking)
│   └── Tests.lean (parser tests)
└── RuntimeTests.lean (runtime test cases)

Proofs/
├── Runtime.lean (runtime simulation framework)
├── Simai.lean (Simai parser proof support)
├── RealChartVerification100524.lean
├── RealChartVerification11264.lean
├── RealChartVerification11358.lean
├── RealChartVerification7thSense.lean
└── RealChartVerificationPandora.lean
```

---

## 3. Dependency Graph

### 3.1 Core Module Dependencies

```
Basic (umbrella)
├── Types ← Areas, Time, Mathlib
├── Areas ← Constants, Mathlib
├── Time ← Mathlib
├── Constants ← Time
├── Convert ← Types
├── Judge ← Types, Constants, Time
├── Score ← Types, Mathlib
├── Storage ← Areas
├── Lifecycle ← Types, Areas, Storage, Constants, Judge, Convert, Time
├── InputModel ← Types, Areas, Storage, Constants, Lifecycle, Time
├── ChartLoader ← Types, Areas, Storage, Constants, Lifecycle, InputModel, Time,
│                  Simai.Syntax, Simai.Shape, Simai.SlideTables, Simai.SlideParser
├── Scheduler ← Types, Areas, Constants, Judge, Convert, Score, Lifecycle, Storage, InputModel, Time
└── FFI ← Simai.Frontend, ChartLoader, Scheduler, InputModel, Std
```

### 3.2 Simai Subsystem Dependencies

```
Simai (umbrella) ← Syntax, Symmetry, Timing, Tokenize, Shape, SlideTables,
                    SlideParser, IR, Source.Maidata, Maidata, Frontend, DSL, Proofs.Simai
```

### 3.3 Proof Dependencies

```
Proofs/Runtime ← Basic, Storage, Simai.DSL
Proofs/RealChart* ← LnmaiCore (full)
```

---

## 4. Inductive Types

### 4.1 Core Enums

| Module | Type | Values/Constructors |
|--------|------|---------------------|
| Areas | `SensorArea` | A1-A8, B1-B8, C, D1-D8, E1-E8 (33 values) |
| Areas | `ButtonZone` | K1-K8 (8 values) |
| Areas | `OuterSlot` | S1-S8 (8 values) |
| Types | `RuntimePos` | `button (zone: ButtonZone)` \| `sensor (area: SensorArea)` |
| Types | `JudgeGrade` | 15-tier lattice: Miss, LateGood, LateGreat3rd, LateGreat2nd, LateGreat, LatePerfect3rd, LatePerfect2nd, Perfect, FastPerfect2nd, FastPerfect3rd, FastGreat, FastGreat2nd, FastGreat3rd, FastGood, TooFast |
| Types | `NoteType` | Tap \| Hold \| Slide \| Touch \| Break |
| Types | `SlideKind` | Single \| Wifi \| ConnPart |
| Types | `AreaPolicy` | Or \| And |
| Types | `JudgeStyle` | Default \| Maji \| Gachi \| Gori |
| Types | `NoteStatus` | Start \| Inited \| Scaling \| Running \| Arrived \| End |
| Types | `ComboState` | None \| FC \| FCPlus \| AP \| APPlus |
| Types | `JudgeEventKind` | Tap \| Hold \| Slide \| Touch \| Break |
| Types | `AudioCommand` | PlayJudgeSfx \| PlaySlideCue |
| Types | `RenderCommand` | ShowJudgeResult \| UpdateSlideProgress \| UpdateSlideTrackProgress \| HideAllSlideBars \| HideSlideBars \| HideSlideTrackBars |

### 4.2 Lifecycle State Machines

| Module | Type | Values/Constructors |
|--------|------|---------------------|
| Lifecycle | `HoldStart` | `button (ButtonZone)` \| `sensor (SensorArea)` |
| Lifecycle | `TapState` | Waiting \| Judgeable \| Judged \| Ended |
| Lifecycle | `HoldSubState` | HeadWaiting \| HeadJudgeable \| HeadJudged \| BodyHeld \| BodyReleased \| Ended |
| Lifecycle | `TouchState` | Waiting \| Judgeable \| Judged \| Ended |
| Lifecycle | `SlideState` | Waiting \| Active (waitTime) \| Judged (grade, waitTime, judgeDiff) \| Ended |

### 4.3 Input Model Types

| Module | Type | Values/Constructors |
|--------|------|---------------------|
| InputModel | `TimedInputEvent` | buttonClick \| buttonHold \| sensorClick \| sensorHold (each with timestamp) |

### 4.4 Scheduler Types

| Module | Type | Values/Constructors |
|--------|------|---------------------|
| Scheduler | `ActiveNote` | tapNote \| holdNote \| touchNote \| slideNote |

### 4.5 Score Types

| Module | Type | Values/Constructors |
|--------|------|---------------------|
| Score | `FastLateDisplay` | All \| BelowCP \| BelowP |

### 4.6 FFI Types

| Module | Type | Values/Constructors |
|--------|------|---------------------|
| FFI | `RuntimeSession` | empty \| loaded (chartSpec, state) |

---

## 5. Structures

### 5.1 Time Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| Time | `TimeTick` | `tick: Int` |
| Time | `Duration` | `toTick: TimeTick` |
| Time | `TimePoint` | `toTick: TimeTick` |

### 5.2 Type Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| Types | `NoteJudgeResult` | `grade: JudgeGrade`, `diff: Duration`, `isBreak: Bool`, `isEX: Bool` |
| Types | `GroupState` | `groupId: Nat`, `count: Nat`, `size: Nat`, `grade: JudgeGrade`, `diff: Duration` |
| Types | `NoteTypeJudgeCounts` | `tapCount: Nat`, `holdCount: Nat`, `slideCount: Nat`, `touchCount: Nat`, `breakCount: Nat` |
| Types | `ScoreState` | `combo: Nat`, `pCombo: Nat`, `cPCombo: Nat`, `totalBase: Nat`, `totalExtra: Nat`, `earnedBase: Nat`, `earnedExtra: Nat`, `lostBase: Nat`, `lostExtra: Nat`, `dxScore: Nat`, `maxDxScore: Nat`, `fastCount: Nat`, `lateCount: Nat`, `counts: NoteTypeJudgeCounts` |
| Types | `JudgeEvent` | `kind: JudgeEventKind`, `grade: JudgeGrade`, `diff: Duration`, `position: RuntimePos`, `noteIndex: Nat` |

### 5.3 Score Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| Score | `ComboDelta` | `combo: Nat`, `pCombo: Nat`, `cPCombo: Nat`, `dXScoreLost: Nat` |
| Score | `AccRates` | `classicAccPlus: Float`, `classicAccMinus: Float`, `dxAccMinus101: Float`, `dxAccMinus100: Float`, `dxAccPlus: Float` |

### 5.4 Storage Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| Storage | `ButtonVec (alpha)` | `toList: List alpha` (length 8) |
| Storage | `SensorVec (alpha)` | `toList: List alpha` (length 33) |

### 5.5 Lifecycle Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| Lifecycle | `CommonNoteParams` | `judgeTiming: TimePoint`, `judgeOffset: Duration`, `isBreak: Bool`, `isEX: Bool`, `noteIndex: Nat` |
| Lifecycle | `TapNote` | `params: CommonNoteParams`, `lane: OuterSlot`, `state: TapState`, `buttonQueueIndex: Nat` |
| Lifecycle | `HoldNote` | `params: CommonNoteParams`, `start: HoldStart`, `state: HoldSubState`, `length: Duration`, `headDiff: Option Duration`, `headGrade: Option JudgeGrade`, `playerReleaseTime: Option TimePoint`, `isClassic: Bool`, `isTouchHold: Bool`, `touchGroupId: Option Nat`, `touchGroupSize: Option Nat`, `touchGroupCount: Option Nat` |
| Lifecycle | `TouchNote` | `params: CommonNoteParams`, `state: TouchState`, `sensorPos: SensorArea`, `touchGroupId: Option Nat`, `touchGroupSize: Option Nat`, `touchGroupCount: Option Nat` |
| Lifecycle | `SlideNote` | `params: CommonNoteParams`, `lane: OuterSlot`, `state: SlideState`, `length: Duration`, `timing: TimePoint`, `startTiming: TimePoint`, `slideKind: SlideKind`, `isClassic: Bool`, `isConnSlide: Bool`, `parentIndex: Option Nat`, `groupIndices: List Nat`, `judgeQueues: List (List SlideArea)` |
| Lifecycle | `SlideArea` | `targetAreas: List SensorArea`, `policy: AreaPolicy`, `isLast: Bool`, `isSkippable: Bool`, `arrowProgressWhenOn: Option TimePoint`, `arrowProgressWhenFinished: Option TimePoint`, `wasOn: Bool`, `wasOff: Bool` |
| Lifecycle | `SlideStepContext` | `currentTime: TimePoint`, `touchPanelOffset: Duration`, `delta: Duration`, `style: JudgeStyle`, `subdivideSlideJudgeGrade: Bool`, `sensorHeld: SensorVec Bool` |
| Lifecycle | `SlideStepSemantic` | internal semantic result structure |

### 5.6 Input Model Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| InputModel | `FrameInput` | `buttonClicked: ButtonVec Bool`, `buttonHeld: ButtonVec Bool`, `sensorClicked: SensorVec Bool`, `sensorHeld: SensorVec Bool`, `buttonClickCount: ButtonVec Nat`, `sensorClickCount: SensorVec Nat`, `delta: Duration` |
| InputModel | `TimedInputBatch` | `currentTime: TimePoint`, `events: List TimedInputEvent` |
| InputModel | `FrameWindow` | `prevTime: TimePoint`, `currentTime: TimePoint` |
| InputModel | `ZoneQueue (alpha)` | `notes: List alpha`, `currentIndex: Nat` |
| InputModel | `GameState` | **Central state structure** with fields: `currentTime: TimePoint`, `prevButton: ButtonVec Bool`, `prevSensor: SensorVec Bool`, `buttonQueueFrontiers: ButtonVec Nat`, `touchQueueFrontiers: SensorVec Nat`, `tapQueues: ButtonVec (ZoneQueue TapNote)`, `holdQueues: ButtonVec (ZoneQueue HoldNote)`, `touchHoldQueues: SensorVec (ZoneQueue HoldNote)`, `touchQueues: SensorVec (ZoneQueue TouchNote)`, `slides: List SlideNote`, `activeHolds: List Nat`, `activeTouchHolds: List Nat`, `touchGroupStates: List GroupState`, `touchHoldGroupStates: List GroupState`, `currentBatch: Option TimedInputBatch`, `score: ScoreState`, `judgeStyle: JudgeStyle`, `touchPanelOffset: Duration`, `useButtonRingForTouch: Bool`, `subdivideSlideJudgeGrade: Bool` |

### 5.7 Chart Loader Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| ChartLoader | `TapChartNote` | `timing: TimePoint`, `slot: OuterSlot`, `isBreak: Bool`, `isEX: Bool`, `buttonQueueIndex: Nat`, `noteIndex: Nat` |
| ChartLoader | `HoldChartNote` | `timing: TimePoint`, `slot: OuterSlot`, `length: Duration`, `isBreak: Bool`, `isEX: Bool`, `isTouch: Bool`, `isClassic: Bool`, `buttonQueueIndex: Nat`, `touchHoldGroupId: Option Nat`, `touchHoldGroupSize: Option Nat`, `noteIndex: Nat` |
| ChartLoader | `TouchHoldChartNote` | `timing: TimePoint`, `sensorPos: SensorArea`, `length: Duration`, `isBreak: Bool`, `isEX: Bool`, `touchQueueIndex: Nat`, `touchGroupId: Option Nat`, `touchGroupSize: Option Nat`, `noteIndex: Nat` |
| ChartLoader | `TouchChartNote` | `timing: TimePoint`, `sensorPos: SensorArea`, `isBreak: Bool`, `touchQueueIndex: Nat`, `touchGroupId: Option Nat`, `touchGroupSize: Option Nat`, `noteIndex: Nat` |
| ChartLoader | `SlideChartNote` | `timing: TimePoint`, `slot: OuterSlot`, `length: Duration`, `startTiming: TimePoint`, `slideKind: SlideKind`, `isClassic: Bool`, `isConnSlide: Bool`, `parentIndex: Option Nat`, `groupIndices: List Nat`, `trackCount: Nat`, `judgeAt: List TimePoint`, `isBreak: Bool`, `isEX: Bool`, `noteIndex: Nat`, `judgeQueues: List (List SlideArea)`, `debugSimai: Option String` |
| ChartLoader | `ChartSpec` | `taps: List TapChartNote`, `holds: List HoldChartNote`, `touches: List TouchChartNote`, `touchHolds: List TouchHoldChartNote`, `slides: List SlideChartNote`, `slideSkipping: Bool` |

### 5.8 Scheduler Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| Scheduler | `ClickCursor` | `buttonClicks: ButtonVec Nat`, `sensorClicks: SensorVec Nat` |

### 5.9 FFI Structures

| Module | Structure | Fields |
|--------|-----------|--------|
| FFI | `RuntimeStepResult` | `state: GameState`, `events: List JudgeEvent`, `audioCommands: List AudioCommand`, `renderCommands: List RenderCommand` |
| FFI | `RuntimeStepLightResult` | `events: List JudgeEvent`, `audioCommands: List AudioCommand`, `renderCommands: List RenderCommand`, `score: ScoreState`, `currentTime: TimePoint` |
| FFI | `LoadedChartSummary` | `tapCount: Nat`, `holdCount: Nat`, `touchCount: Nat`, `touchHoldCount: Nat`, `slideCount: Nat` |
| FFI | `RuntimeRegistry` | `nextHandle: UInt64`, `sessions: Std.HashMap UInt64 RuntimeSession` |

---

## 6. Theorems and Lemmas

### 6.1 Areas Module

| Theorem | Statement |
|---------|-----------|
| `sensorArea_ofIndex_toIndex` | `∀ a, SensorArea.ofIndex? (SensorArea.toIndex a) = some a` |
| `sensorArea_toIndex_ofIndex` | `∀ i h, SensorArea.toIndex (SensorArea.ofIndex? i h) = i` |
| `buttonZone_ofIndex_toIndex` | `∀ a, ButtonZone.ofIndex? (ButtonZone.toIndex a) = some a` |
| `outerSlot_ofIndex_toIndex` | `∀ a, OuterSlot.ofIndex? (OuterSlot.toIndex a) = some a` |
| `buttonZone_toIndex_ofIndex` | `∀ i h, ButtonZone.toIndex (ButtonZone.ofIndex? i h) = i` |
| `outerSlot_toIndex_ofIndex` | `∀ i h, OuterSlot.toIndex (OuterSlot.ofIndex? i h) = i` |

### 6.2 Time Module

| Theorem | Statement |
|---------|-----------|
| `Duration.toMicros_injective` | `∀ a b, a.toMicros = b.toMicros → a = b` |
| `Duration.toMicros_le_toMicros` | `∀ a b, a ≤ b ↔ a.toMicros ≤ b.toMicros` |
| `Duration.toMicros_lt_toMicros` | `∀ a b, a < b ↔ a.toMicros < b.toMicros` |
| `Duration.toMicros_eq_toMicros` | `∀ a b, a = b ↔ a.toMicros = b.toMicros` |
| `TimePoint.toMicros_injective` | `∀ a b, a.toMicros = b.toMicros → a = b` |
| `TimePoint.toMicros_le_toMicros` | `∀ a b, a ≤ b ↔ a.toMicros ≤ b.toMicros` |
| `TimePoint.toMicros_lt_toMicros` | `∀ a b, a < b ↔ a.toMicros < b.toMicros` |
| `TimePoint.toMicros_eq_toMicros` | `∀ a b, a = b ↔ a.toMicros = b.toMicros` |
| `Time.timePoint_toMicros_order_preserving` | `∀ a b, a ≤ b ↔ a.toMicros ≤ b.toMicros` |
| `Time.duration_toMicros_order_preserving` | `∀ a b, a ≤ b ↔ a.toMicros ≤ b.toMicros` |
| `Time.timePoint_toMicros_strict_order_preserving` | `∀ a b, a < b ↔ a.toMicros < b.toMicros` |
| `Time.duration_toMicros_strict_order_preserving` | `∀ a b, a < b ↔ a.toMicros < b.toMicros` |
| `Time.timePoint_compare_toMicros` | `∀ a b, compare a b = compare a.toMicros b.toMicros` |
| `Time.duration_compare_toMicros` | `∀ a b, compare a b = compare a.toMicros b.toMicros` |
| `Time.timePoint_pairwise_le_toMicros_iff` | `∀ l, l.Pairwise (· ≤ ·) ↔ l.map (·.toMicros).Pairwise (· ≤ ·)` |
| `Time.duration_pairwise_le_toMicros_iff` | `∀ l, l.Pairwise (· ≤ ·) ↔ l.map (·.toMicros).Pairwise (· ≤ ·)` |
| `duration_toInt_ofInt` | `∀ i, (Duration.ofInt i).toInt = i` |
| `timePoint_toInt_ofInt` | `∀ i, (TimePoint.ofInt i).toInt = i` |

### 6.3 Convert Module

| Theorem | Statement |
|---------|-----------|
| `perfect_fixed` | `∀ style, convertGrade style Perfect = Perfect` |
| `miss_fixed` | `∀ style, convertGrade style Miss = Miss` |
| `tooFast_fixed_maji_gachi` | `∀ style, style = Maji ∨ style = Gachi → convertGrade style TooFast = TooFast` |
| `perfect_is_upper_bound` | `∀ style g, convertGrade style g = Perfect → g = Perfect` |

### 6.4 Scheduler Module

| Theorem | Statement |
|---------|-----------|
| `updateSlideParentFlags_length` | `∀ l, (updateSlideParentFlags l).length = l.length` |

### 6.5 ChartLoader Module

| Theorem | Statement |
|---------|-----------|
| `shortConnSlide_applySingleTrackConnRules` | Structural property of conn slide rule application |

### 6.6 RuntimeTests Module (native_decide verified)

| Theorem | Statement |
|---------|-----------|
| `conn_child_becomes_checkable_at_parent_pending_finish` | Connected slide child becomes checkable when parent is pending finish |
| `conn_child_becomes_checkable_at_parent_finished` | Connected slide child becomes checkable when parent is finished |
| `conn_parent_not_force_finished_without_child_progress` | Connected parent not force-finished without child progress |
| `conn_child_progress_only_force_finishes_direct_parent` | Connected child progress only force-finishes direct parent |
| `slide_too_late_last_segment_remaining_becomes_lategood_in_reduced_wifi_case` | Slide too-late: last segment remaining becomes LateGood in reduced wifi case |
| `slide_too_late_two_or_more_segments_remaining_stays_miss_in_reduced_wifi_case` | Slide too-late: 2+ segments remaining stays Miss in reduced wifi case |
| `slide_too_late_last_segment_remaining_becomes_lategood` | Slide too-late: last segment remaining becomes LateGood |
| `slide_too_late_two_or_more_segments_remaining_stays_miss` | Slide too-late: 2+ segments remaining stays Miss |
| `wifi_center_cleared_uses_special_progress_marker` | Wifi center cleared uses special progress marker |
| `wifi_center_cleared_without_both_tails_uses_max_remaining_progress` | Wifi center cleared without both tails uses max remaining progress |
| `wifi_max_remaining_one_implies_lategood` | Wifi max remaining 1 implies LateGood |
| `wifi_head_checkability_boundary_excludes_before_minus_50ms` | Wifi head checkability boundary excludes before -50ms |
| `wifi_head_checkability_boundary_includes_exact_minus_50ms` | Wifi head checkability boundary includes exact -50ms |
| `wifi_exact_too_late_boundary_preserved` | Wifi exact too-late boundary preserved |
| `slide_exact_too_late_boundary_preserved` | Slide exact too-late boundary preserved |
| `slide_frame_zero_becomes_checkable_and_progresses_same_frame` | Slide frame zero becomes checkable and progresses same frame |

---

## 7. Key Functions

### 7.1 Judgment Functions (Judge.lean)

| Function | Signature | Description |
|----------|-----------|-------------|
| `judgeTap` | `(diff: Duration) → (isEX: Bool) → JudgeGrade` | Tap/Hold head judgment with 6-tier window |
| `judgeTouch` | `(diff: Duration) → (isEX: Bool) → Option JudgeGrade` | Touch judgment (late-only) |
| `judgeSlideModern` | `(diff: Duration) → (stayTime: Duration) → (isEX: Bool) → JudgeGrade` | Modern slide with dynamic extension |
| `judgeSlideClassic` | `(diff: Duration) → JudgeGrade` | Classic slide with fixed asymmetric windows |
| `correctSlideGrade` | `(grade: JudgeGrade) → JudgeGrade` | Collapse subdivided Perfect grades |
| `judgeHoldEnd` | `(headGrade: JudgeGrade) → (judgeDiff: Duration) → (length: Duration) → (ignoreTime: Duration) → (playerReleaseTime: Option TimePoint) → JudgeGrade` | Deluxe hold end with 5-band press table |
| `judgeHoldClassicEnd` | `(headGrade: JudgeGrade) → (timing: TimePoint) → (length: Duration) → (releaseTiming: Option TimePoint) → JudgeGrade` | Classic hold: worst of head vs end |
| `judgeSlideTooLate` | `(queueRemaining: Nat) → JudgeGrade` | Too-late: 1 remaining → LateGood, else Miss |
| `isTooLateSlide` | `(diff: Duration) → (userOffset: Duration) → Bool` | Too-late check threshold |

### 7.2 Lifecycle Functions (Lifecycle.lean)

| Function | Signature | Description |
|----------|-----------|-------------|
| `tapStep` | `(note: TapNote) → (currentTime: TimePoint) → (judgeDiff: Duration) → (inputClicked: Bool) → (style: JudgeStyle) → TapNote × Option JudgeEvent` | One frame advance for TapNote |
| `holdStep` | `(note: HoldNote) → (currentTime: TimePoint) → (judgeDiff: Duration) → ... → HoldNote × Option JudgeEvent` | One frame advance for HoldNote |
| `touchStep` | `(note: TouchNote) → (currentTime: TimePoint) → (judgeDiff: Duration) → (inputClicked: Bool) → (sharedResult: Option JudgeGrade) → (style: JudgeStyle) → TouchNote × Option JudgeEvent` | One frame advance for TouchNote |
| `slideStep` | `(note: SlideNote) → (currentTime: TimePoint) → (sensorHeld: SensorVec Bool) → (touchPanelOffset: Duration) → (delta: Duration) → (style: JudgeStyle) → (subdivideSlideJudgeGrade: Bool) → SlideNote × List JudgeEvent × List AudioCommand × List RenderCommand` | One frame advance for SlideNote |

### 7.3 Scheduler Functions (Scheduler.lean)

| Function | Signature | Description |
|----------|-----------|-------------|
| `stepFrame` | `(st: GameState) → (input: FrameInput) → GameState × List JudgeEvent × List AudioCommand × List RenderCommand` | Main entry point: advance all notes one frame |
| `stepFrameTimed` | `(st: GameState) → (batch: TimedInputBatch) → GameState × List JudgeEvent × List AudioCommand × List RenderCommand` | Convenience wrapper with timed input |

### 7.4 Chart Loader Functions (ChartLoader.lean)

| Function | Signature | Description |
|----------|-----------|-------------|
| `buildGameState` | `(chart: ChartSpec) → GameState` | Main loader entry: convert ChartSpec to GameState |
| `parseChartJson` | `(json: Json) → Except String ChartSpec` | Parse JSON to ChartSpec |
| `parseChartJsonString` | `(s: String) → Except String ChartSpec` | Parse JSON string to ChartSpec |
| `loadChartFile` | `(path: System.FilePath) → IO (Except String ChartSpec)` | Load chart from file |

---

## 8. Key Semantic Patterns

### 8.1 Integer Semantics

- Lean `Int` maps to Rust `i64`
- Lean `Nat` maps to Rust `u64` (or `usize`)
- `Duration` and `TimePoint` wrap `Int` (microseconds)
- Division uses integer division with `roundDivAwayFromZero` for quantization

### 8.2 Pure Functional State Machines

All lifecycle transitions are pure functions returning `(NewState, Option JudgeEvent)`. No mutation. The Rust rewrite must maintain this pattern.

### 8.3 Frame Processing Order

The Scheduler processes notes in a fixed order: tap → hold → touch → touch-hold → slide. This is semantically meaningful and must be preserved exactly.

### 8.4 Shared Queue Indexing

Taps and holds sharing the same button zone use a shared `buttonQueueFrontiers` mechanism. Touches and touch-holds sharing sensor areas use `touchQueueFrontiers`. This prevents double-consumption of clicks.

### 8.5 Slide Queue Traversal

Slides use multi-track queues (`List (List SlideArea)`) with skip logic, parent-child conn-slide relationships, and force-finish semantics. The `slideQueueCore` function is the critical traversal algorithm.

### 8.6 Touch Group Sharing

Touch notes and touch-holds sharing the same sensor area form groups. A strict majority (>50%) is required for group result sharing. The `GroupState` tracks count/size/grade/diff.

### 8.7 Hold End Judgment

Modern holds use a 5-band press table based on held percentage. Classic holds use independent timing comparison with worst-of semantics. The release-ignore grace period (2 frames) is skipped for missed heads.

### 8.8 Score Computation

15-tier grades with non-linear score mapping. Break notes have dual DX/Classic extra scoring tracks. Combo tracking includes Perfect-Combo and Critical-Perfect-Combo chains.

---

## 9. Summary Statistics

| Metric | Count |
|--------|-------|
| Core modules (LnmaiCore/) | 17 |
| Simai sub-modules | 15 |
| Proof files | 7 (2 infrastructure + 5 chart verifications) |
| Inductive types | ~25 |
| Structures | ~30 |
| Theorems (formal) | ~50+ |
| Test cases | ~50+ |
| FFI exports | 18 |
| Verified chart AP proofs | 5 charts |

---

## 10. Verification Assets

### 10.1 Verified Charts

1. **100524_[協]Hand in Hand** (level 7) - Custom sensor-tap strategy
2. **11264_幽霊東京** (level 5) - Default + local hold-through-start
3. **11358_インドア系ならトラックメイカー** (level 5) - Default tactic
4. **462_7thSense** (level 5) - Default + skip-chain analysis
5. **834_PANDORA PARADOXXX** (level 6) - Default tactic

### 10.2 Verification Properties

All verified charts prove:
- `checkpoint_has_no_missing_notes` - All notes are judged
- `checkpoint_achieves_ap` - All notes achieve Perfect or better
- `checkpoint_has_no_non_perfect_notes` - No notes below Perfect grade

---

*Generated for Lean → Rust verified rewrite project.*
