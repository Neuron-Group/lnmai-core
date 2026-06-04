# Specification Extraction

## 1. Time Module Specifications

### 1.1 Duration.toMicros_injective

**Source:** `LnmaiCore/Time.lean:124`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `a.toMicros = b.toMicros → a = b`

**Invariant:**
- The `toMicros` function is injective
- Different Duration values have different microsecond representations

### 1.2 Duration.toMicros_le_toMicros

**Source:** `LnmaiCore/Time.lean:141`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `a.toMicros ≤ b.toMicros ↔ a ≤ b`

**Invariant:**
- Order-preserving bijection between Duration and its microsecond representation
- Comparison on microsecond values is equivalent to comparison on Duration values

### 1.3 Duration.toMicros_lt_toMicros

**Source:** `LnmaiCore/Time.lean:144`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `a.toMicros < b.toMicros ↔ a < b`

**Invariant:**
- Strict order-preserving bijection between Duration and its microsecond representation

### 1.4 Duration.toMicros_eq_toMicros

**Source:** `LnmaiCore/Time.lean:147`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `a.toMicros = b.toMicros ↔ a = b`

**Invariant:**
- Equality-preserving bijection between Duration and its microsecond representation

### 1.5 TimePoint.toMicros_injective

**Source:** `LnmaiCore/Time.lean:210`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `a.toMicros = b.toMicros → a = b`

**Invariant:**
- The `toMicros` function is injective for TimePoint
- Different TimePoint values have different microsecond representations

### 1.6 TimePoint.toMicros_le_toMicros

**Source:** `LnmaiCore/Time.lean:227`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `a.toMicros ≤ b.toMicros ↔ a ≤ b`

**Invariant:**
- Order-preserving bijection between TimePoint and its microsecond representation

### 1.7 TimePoint.toMicros_lt_toMicros

**Source:** `LnmaiCore/Time.lean:230`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `a.toMicros < b.toMicros ↔ a < b`

**Invariant:**
- Strict order-preserving bijection between TimePoint and its microsecond representation

### 1.8 TimePoint.toMicros_eq_toMicros

**Source:** `LnmaiCore/Time.lean:233`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `a.toMicros = b.toMicros ↔ a = b`

**Invariant:**
- Equality-preserving bijection between TimePoint and its microsecond representation

### 1.9 Time.timePoint_toMicros_order_preserving

**Source:** `LnmaiCore/Time.lean:296`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `a ≤ b ↔ a.toMicros ≤ b.toMicros`

**Invariant:**
- Order-preserving property for TimePoint comparison

### 1.10 Time.duration_toMicros_order_preserving

**Source:** `LnmaiCore/Time.lean:300`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `a ≤ b ↔ a.toMicros ≤ b.toMicros`

**Invariant:**
- Order-preserving property for Duration comparison

### 1.11 Time.timePoint_toMicros_strict_order_preserving

**Source:** `LnmaiCore/Time.lean:304`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `a < b ↔ a.toMicros < b.toMicros`

**Invariant:**
- Strict order-preserving property for TimePoint comparison

### 1.12 Time.duration_toMicros_strict_order_preserving

**Source:** `LnmaiCore/Time.lean:308`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `a < b ↔ a.toMicros < b.toMicros`

**Invariant:**
- Strict order-preserving property for Duration comparison

### 1.13 Time.timePoint_compare_toMicros

**Source:** `LnmaiCore/Time.lean:312`

**Precondition:**
- `a, b : TimePoint`

**Postcondition:**
- `compare a b = compare a.toMicros b.toMicros`

**Invariant:**
- Comparison result preservation for TimePoint

### 1.14 Time.duration_compare_toMicros

**Source:** `LnmaiCore/Time.lean:316`

**Precondition:**
- `a, b : Duration`

**Postcondition:**
- `compare a b = compare a.toMicros b.toMicros`

**Invariant:**
- Comparison result preservation for Duration

### 1.15 Time.timePoint_pairwise_le_toMicros_iff

**Source:** `LnmaiCore/Time.lean:320`

**Precondition:**
- `xs : List TimePoint`

**Postcondition:**
- `xs.Pairwise (fun a b => a ≤ b) ↔ xs.Pairwise (fun a b => a.toMicros ≤ b.toMicros)`

**Invariant:**
- Pairwise ordering preservation for lists of TimePoint

### 1.16 Time.duration_pairwise_le_toMicros_iff

**Source:** `LnmaiCore/Time.lean:327`

**Precondition:**
- `xs : List Duration`

**Postcondition:**
- `xs.Pairwise (fun a b => a ≤ b) ↔ xs.Pairwise (fun a b => a.toMicros ≤ b.toMicros)`

**Invariant:**
- Pairwise ordering preservation for lists of Duration

### 1.17 duration_toInt_ofInt

**Source:** `LnmaiCore/Time.lean:379`

**Precondition:**
- `value : Int`

**Postcondition:**
- `(Duration.ofInt value).toInt = value`

**Invariant:**
- Roundtrip property for Duration integer conversion

### 1.18 timePoint_toInt_ofInt

**Source:** `LnmaiCore/Time.lean:382`

**Precondition:**
- `value : Int`

**Postcondition:**
- `(TimePoint.ofInt value).toInt = value`

**Invariant:**
- Roundtrip property for TimePoint integer conversion

---

## 2. Areas Module Specifications

### 2.1 sensorArea_ofIndex_toIndex

**Source:** `LnmaiCore/Areas.lean:69`

**Precondition:**
- `area : SensorArea`

**Postcondition:**
- `SensorArea.ofIndex? area.toIndex = some area`

**Invariant:**
- Roundtrip property for SensorArea index conversion
- Every SensorArea can be converted to index and back

### 2.2 sensorArea_toIndex_ofIndex

**Source:** `LnmaiCore/Areas.lean:72`

**Precondition:**
- `index : Nat`
- `h : index < Constants.SENSOR_AREA_COUNT` (index < 33)

**Postcondition:**
- `match SensorArea.ofIndex? index with | some area => area.toIndex = index | none => False`

**Invariant:**
- Inverse roundtrip property for SensorArea
- Valid indices (0-32) always map back to the same index

### 2.3 buttonZone_ofIndex_toIndex

**Source:** `LnmaiCore/Areas.lean:79`

**Precondition:**
- `zone : ButtonZone`

**Postcondition:**
- `ButtonZone.ofIndex? zone.toIndex = some zone`

**Invariant:**
- Roundtrip property for ButtonZone index conversion
- Every ButtonZone can be converted to index and back

### 2.4 outerSlot_ofIndex_toIndex

**Source:** `LnmaiCore/Areas.lean:82`

**Precondition:**
- `slot : OuterSlot`

**Postcondition:**
- `OuterSlot.ofIndex? slot.toIndex = some slot`

**Invariant:**
- Roundtrip property for OuterSlot index conversion
- Every OuterSlot can be converted to index and back

### 2.5 buttonZone_toIndex_ofIndex

**Source:** `LnmaiCore/Areas.lean:85`

**Precondition:**
- `index : Nat`
- `h : index < Constants.BUTTON_ZONE_COUNT` (index < 8)

**Postcondition:**
- `match ButtonZone.ofIndex? index with | some zone => zone.toIndex = index | none => False`

**Invariant:**
- Inverse roundtrip property for ButtonZone
- Valid indices (0-7) always map back to the same index

### 2.6 outerSlot_toIndex_ofIndex

**Source:** `LnmaiCore/Areas.lean:92`

**Precondition:**
- `index : Nat`
- `h : index < Constants.BUTTON_ZONE_COUNT` (index < 8)

**Postcondition:**
- `match OuterSlot.ofIndex? index with | some slot => slot.toIndex = index | none => False`

**Invariant:**
- Inverse roundtrip property for OuterSlot
- Valid indices (0-7) always map back to the same index

---

## 3. Convert Module Specifications

### 3.1 perfect_fixed

**Source:** `LnmaiCore/Convert.lean:94`

**Precondition:**
- `style : JudgeStyle`

**Postcondition:**
- `convertGrade style JudgeGrade.Perfect = JudgeGrade.Perfect`

**Invariant:**
- Perfect grade is a fixed point under all conversion styles
- Converting Perfect always results in Perfect regardless of style

### 3.2 miss_fixed

**Source:** `LnmaiCore/Convert.lean:97`

**Precondition:**
- `style : JudgeStyle`

**Postcondition:**
- `convertGrade style Miss = Miss`

**Invariant:**
- Miss grade is a fixed point under all conversion styles
- Converting Miss always results in Miss regardless of style

### 3.3 tooFast_fixed_maji_gachi

**Source:** `LnmaiCore/Convert.lean:100`

**Precondition:**
- None

**Postcondition:**
- `convertMaji TooFast = TooFast ∧ convertGachi TooFast = TooFast`

**Invariant:**
- TooFast grade is a fixed point under Maji and Gachi conversion styles
- Converting TooFast with Maji or Gachi always results in TooFast

### 3.4 perfect_is_upper_bound

**Source:** `LnmaiCore/Convert.lean:103`

**Precondition:**
- `style : JudgeStyle`
- `g : JudgeGrade`

**Postcondition:**
- `convertGrade style g = JudgeGrade.Perfect → g = JudgeGrade.Perfect`

**Invariant:**
- Perfect is the upper bound of conversion
- Only Perfect can convert to Perfect; no other grade can become Perfect through conversion

---

## 4. Scheduler Module Specifications

### 4.1 updateSlideParentFlags_length

**Source:** `LnmaiCore/Scheduler.lean`

**Precondition:**
- `l : List SlideNote`

**Postcondition:**
- `(updateSlideParentFlags l).length = l.length`

**Invariant:**
- The `updateSlideParentFlags` function preserves list length
- Updating slide parent flags does not change the number of slides

---

## 5. ChartLoader Module Specifications

### 5.1 shortConnSlide_applySingleTrackConnRules

**Source:** `LnmaiCore/ChartLoader.lean`

**Precondition:**
- Connected slide structure

**Postcondition:**
- Structural property of conn slide rule application

**Invariant:**
- Connected slide rules maintain structural consistency
- The transformation preserves slide connectivity properties

---

## 6. RuntimeTests Module Specifications

### 6.1 conn_child_becomes_checkable_at_parent_pending_finish

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Connected slide parent is in pending finish state

**Postcondition:**
- Connected slide child becomes checkable

**Invariant:**
- Connected slide child activation depends on parent state
- Child slides become checkable when parent reaches pending finish

### 6.2 conn_child_becomes_checkable_at_parent_finished

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Connected slide parent is finished

**Postcondition:**
- Connected slide child becomes checkable

**Invariant:**
- Connected slide child activation depends on parent state
- Child slides become checkable when parent is finished

### 6.3 conn_parent_not_force_finished_without_child_progress

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Connected slide child has not progressed

**Postcondition:**
- Connected parent is not force-finished

**Invariant:**
- Parent slides cannot be force-finished without child progress
- Child progress is required for parent completion

### 6.4 conn_child_progress_only_force_finishes_direct_parent

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Connected slide child progresses

**Postcondition:**
- Only direct parent is force-finished

**Invariant:**
- Child progress only affects direct parent
- Indirect parent slides are not affected by child progress

### 6.5 slide_too_late_last_segment_remaining_becomes_lategood_in_reduced_wifi_case

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Slide is too late
- Last segment is remaining
- Reduced wifi case

**Postcondition:**
- Grade becomes LateGood

**Invariant:**
- LateGood is assigned for last segment remaining in reduced wifi case
- Special handling for wifi slides with single remaining segment

### 6.6 slide_too_late_two_or_more_segments_remaining_stays_miss_in_reduced_wifi_case

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Slide is too late
- Two or more segments remaining
- Reduced wifi case

**Postcondition:**
- Grade stays Miss

**Invariant:**
- Miss is maintained for multiple remaining segments in reduced wifi case
- No grade improvement for multiple remaining segments

### 6.7 slide_too_late_last_segment_remaining_becomes_lategood

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Slide is too late
- Last segment is remaining

**Postcondition:**
- Grade becomes LateGood

**Invariant:**
- LateGood is assigned for last segment remaining in general case
- Single remaining segment gets LateGood grade

### 6.8 slide_too_late_two_or_more_segments_remaining_stays_miss

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Slide is too late
- Two or more segments remaining

**Postcondition:**
- Grade stays Miss

**Invariant:**
- Miss is maintained for multiple remaining segments
- No grade improvement for multiple remaining segments

### 6.9 wifi_center_cleared_uses_special_progress_marker

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Wifi slide center is cleared

**Postcondition:**
- Special progress marker is used

**Invariant:**
- Wifi center clearance uses special progress tracking
- Center cleared state is marked distinctly

### 6.10 wifi_center_cleared_without_both_tails_uses_max_remaining_progress

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Wifi slide center is cleared
- Both tails are not cleared

**Postcondition:**
- Max remaining progress is used

**Invariant:**
- Wifi progress calculation depends on tail clearance
- Incomplete tail clearance uses max remaining progress

### 6.11 wifi_max_remaining_one_implies_lategood

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Wifi max remaining is 1

**Postcondition:**
- Grade is LateGood

**Invariant:**
- Single remaining wifi segment gets LateGood grade
- Wifi slides with one remaining segment are LateGood

### 6.12 wifi_head_checkability_boundary_excludes_before_minus_50ms

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Time is before -50ms boundary

**Postcondition:**
- Wifi head is not checkable

**Invariant:**
- Wifi head checkability has temporal boundary
- Early times are excluded from checkability

### 6.13 wifi_head_checkability_boundary_includes_exact_minus_50ms

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Time is exactly at -50ms boundary

**Postcondition:**
- Wifi head is checkable

**Invariant:**
- Wifi head checkability boundary is inclusive at -50ms
- Exact boundary time allows checkability

### 6.14 wifi_exact_too_late_boundary_preserved

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Wifi slide at exact too-late boundary

**Postcondition:**
- Too-late boundary is preserved

**Invariant:**
- Wifi slides preserve exact too-late timing
- Boundary conditions are maintained

### 6.15 slide_exact_too_late_boundary_preserved

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Slide at exact too-late boundary

**Postcondition:**
- Too-late boundary is preserved

**Invariant:**
- Slides preserve exact too-late timing
- Boundary conditions are maintained

### 6.16 slide_frame_zero_becomes_checkable_and_progresses_same_frame

**Source:** `LnmaiCore/RuntimeTests.lean`

**Precondition:**
- Slide at frame zero

**Postcondition:**
- Slide becomes checkable and progresses same frame

**Invariant:**
- Frame zero slides can be immediately checkable
- Same-frame progression is possible for slides

---

## 7. Key Behavioral Specifications

### 7.1 Frame Processing Order

**Specification:**
- The Scheduler processes notes in a fixed order: tap → hold → touch → touch-hold → slide
- This order is semantically meaningful and must be preserved

**Invariant:**
- Note processing order is deterministic
- Order affects judgment results due to shared queue indexing

### 7.2 Shared Queue Indexing

**Specification:**
- Taps and holds sharing the same button zone use shared `buttonQueueFrontiers`
- Touches and touch-holds sharing sensor areas use `touchQueueFrontiers`

**Invariant:**
- Prevents double-consumption of clicks
- Queue indices are consistent across note types

### 7.3 Touch Group Sharing

**Specification:**
- Touch notes and touch-holds sharing the same sensor area form groups
- Strict majority (>50%) required for group result sharing

**Invariant:**
- Group result sharing is deterministic
- Majority threshold is strictly enforced

### 7.4 Hold End Judgment

**Specification:**
- Modern holds use 5-band press table based on held percentage
- Classic holds use independent timing comparison with worst-of semantics

**Invariant:**
- Hold judgment is deterministic based on input
- Release-ignore grace period (2 frames) is skipped for missed heads

### 7.5 Score Computation

**Specification:**
- 15-tier grades with non-linear score mapping
- Break notes have dual DX/Classic extra scoring tracks

**Invariant:**
- Score computation is deterministic
- Grade mapping is consistent across all note types

---

## 8. Integer Semantics Specifications

### 8.1 Lean Int → Rust i64

**Specification:**
- All Lean `Int` values map to Rust `i64`
- Overflow behavior must be documented and consistent

**Invariant:**
- Integer arithmetic produces identical results
- Division uses integer division with `roundDivAwayFromZero` for quantization

### 8.2 Lean Nat → Rust usize

**Specification:**
- All Lean `Nat` values map to Rust `usize` (or `u64` for FFI handles)
- Natural numbers are non-negative

**Invariant:**
- Natural number operations are safe
- No negative results from natural number operations

---

## 9. State Machine Specifications

### 9.1 TapState Transitions

**States:** Waiting → Judgeable → Judged → Ended

**Transitions:**
- Waiting → Judgeable: when timing is within judgable range
- Judgeable → Judged: when input is detected
- Judged → Ended: after judgment is processed

**Invariant:**
- Transitions are deterministic
- No backward transitions

### 9.2 HoldSubState Transitions

**States:** HeadWaiting → HeadJudgeable → HeadJudged → BodyHeld → BodyReleased → Ended

**Transitions:**
- HeadWaiting → HeadJudgeable: when timing is within judgable range
- HeadJudgeable → HeadJudged: when head input is detected
- HeadJudged → BodyHeld: when body is held
- HeadJudged → BodyReleased: when body is released
- BodyHeld → BodyReleased: when body is released
- BodyReleased → Ended: when hold ends

**Invariant:**
- Transitions are deterministic
- Head judgment affects body state
- Release-ignore grace period applies

### 9.3 TouchState Transitions

**States:** Waiting → Judgeable → Judged → Ended

**Transitions:**
- Waiting → Judgeable: when timing is within judgable range
- Judgeable → Judged: when input is detected
- Judged → Ended: after judgment is processed

**Invariant:**
- Transitions are deterministic
- No backward transitions

### 9.4 SlideState Transitions

**States:** Waiting → Active → Judged → Ended

**Transitions:**
- Waiting → Active: when timing is within judgable range
- Active → Judged: when slide is completed or too late
- Judged → Ended: after judgment is processed

**Invariant:**
- Transitions are deterministic
- Active state tracks wait time
- Judgment depends on slide completion status

---

## 10. Proof Obligations for Rust Implementation

### 10.1 Time Module

1. **Duration.toMicros_injective**: Rust `Duration::to_micros` must be injective
2. **Duration.toMicros_le_toMicros**: Rust comparison must preserve order
3. **TimePoint.toMicros_injective**: Rust `TimePoint::to_micros` must be injective
4. **TimePoint.toMicros_le_toMicros**: Rust comparison must preserve order

### 10.2 Areas Module

1. **sensorArea_ofIndex_toIndex**: Rust roundtrip must be exact
2. **buttonZone_ofIndex_toIndex**: Rust roundtrip must be exact
3. **outerSlot_ofIndex_toIndex**: Rust roundtrip must be exact

### 10.3 Convert Module

1. **perfect_fixed**: Rust conversion must preserve Perfect
2. **miss_fixed**: Rust conversion must preserve Miss
3. **perfect_is_upper_bound**: Rust conversion must not create Perfect from non-Perfect

### 10.4 Scheduler Module

1. **updateSlideParentFlags_length**: Rust must preserve list length

### 10.5 RuntimeTests Module

1. All 16 runtime test theorems must be verified in Rust
2. Property tests must cover all boundary conditions
3. Differential tests must match Lean reference implementation

---

*Generated for Lean → Rust verified rewrite project.*
