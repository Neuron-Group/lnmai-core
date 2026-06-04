# Lean → Rust Mapping

## 1. Type Mappings

### 1.1 Core Enums

| Lean Type | Rust Type | Rust Enum |
|-----------|-----------|-----------|
| `SensorArea` | `SensorArea` | `enum SensorArea { A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, C, D1, D2, D3, D4, D5, D6, D7, D8, E1, E2, E3, E4, E5, E6, E7, E8 }` |
| `ButtonZone` | `ButtonZone` | `enum ButtonZone { K1, K2, K3, K4, K5, K6, K7, K8 }` |
| `OuterSlot` | `OuterSlot` | `enum OuterSlot { S1, S2, S3, S4, S5, S6, S7, S8 }` |
| `RuntimePos` | `RuntimePos` | `enum RuntimePos { Button(ButtonZone), Sensor(SensorArea) }` |
| `JudgeGrade` | `JudgeGrade` | `enum JudgeGrade { Miss, LateGood, LateGreat3rd, LateGreat2nd, LateGreat, LatePerfect3rd, LatePerfect2nd, Perfect, FastPerfect2nd, FastPerfect3rd, FastGreat, FastGreat2nd, FastGreat3rd, FastGood, TooFast }` |
| `NoteType` | `NoteType` | `enum NoteType { Tap, Hold, Slide, Touch, Break }` |
| `SlideKind` | `SlideKind` | `enum SlideKind { Single, Wifi, ConnPart }` |
| `AreaPolicy` | `AreaPolicy` | `enum AreaPolicy { Or, And }` |
| `JudgeStyle` | `JudgeStyle` | `enum JudgeStyle { Default, Maji, Gachi, Gori }` |
| `NoteStatus` | `NoteStatus` | `enum NoteStatus { Start, Inited, Scaling, Running, Arrived, End }` |
| `ComboState` | `ComboState` | `enum ComboState { None, FC, FCPlus, AP, APPlus }` |
| `JudgeEventKind` | `JudgeEventKind` | `enum JudgeEventKind { Tap, Hold, Slide, Touch, Break }` |
| `AudioCommand` | `AudioCommand` | `enum AudioCommand { PlayJudgeSfx, PlaySlideCue }` |
| `RenderCommand` | `RenderCommand` | `enum RenderCommand { ShowJudgeResult, UpdateSlideProgress, UpdateSlideTrackProgress, HideAllSlideBars, HideSlideBars, HideSlideTrackBars }` |

### 1.2 Lifecycle State Machines

| Lean Type | Rust Type | Rust Enum |
|-----------|-----------|-----------|
| `HoldStart` | `HoldStart` | `enum HoldStart { Button(ButtonZone), Sensor(SensorArea) }` |
| `TapState` | `TapState` | `enum TapState { Waiting, Judgeable, Judged, Ended }` |
| `HoldSubState` | `HoldSubState` | `enum HoldSubState { HeadWaiting, HeadJudgeable, HeadJudged, BodyHeld, BodyReleased, Ended }` |
| `TouchState` | `TouchState` | `enum TouchState { Waiting, Judgeable, Judged, Ended }` |
| `SlideState` | `SlideState` | `enum SlideState { Waiting, Active { wait_time: Duration }, Judged { grade: JudgeGrade, wait_time: Duration, judge_diff: Duration }, Ended }` |

### 1.3 Input Model Types

| Lean Type | Rust Type | Rust Enum |
|-----------|-----------|-----------|
| `TimedInputEvent` | `TimedInputEvent` | `enum TimedInputEvent { ButtonClick { timestamp: TimePoint }, ButtonHold { timestamp: TimePoint }, SensorClick { timestamp: TimePoint }, SensorHold { timestamp: TimePoint } }` |

### 1.4 Scheduler Types

| Lean Type | Rust Type | Rust Enum |
|-----------|-----------|-----------|
| `ActiveNote` | `ActiveNote` | `enum ActiveNote { TapNote(TapNote), HoldNote(HoldNote), TouchNote(TouchNote), SlideNote(SlideNote) }` |

### 1.5 Score Types

| Lean Type | Rust Type | Rust Enum |
|-----------|-----------|-----------|
| `FastLateDisplay` | `FastLateDisplay` | `enum FastLateDisplay { All, BelowCP, BelowP }` |

### 1.6 FFI Types

| Lean Type | Rust Type | Rust Enum |
|-----------|-----------|-----------|
| `RuntimeSession` | `RuntimeSession` | `enum RuntimeSession { Empty, Loaded { chart_spec: ChartSpec, state: GameState } }` |

---

## 2. Structure Mappings

### 2.1 Time Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `TimeTick` | `TimeTick` | `tick: i64` |
| `Duration` | `Duration` | `to_tick: TimeTick` |
| `TimePoint` | `TimePoint` | `to_tick: TimeTick` |

### 2.2 Type Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `NoteJudgeResult` | `NoteJudgeResult` | `grade: JudgeGrade, diff: Duration, is_break: bool, is_ex: bool` |
| `GroupState` | `GroupState` | `group_id: usize, count: usize, size: usize, grade: JudgeGrade, diff: Duration` |
| `NoteTypeJudgeCounts` | `NoteTypeJudgeCounts` | `tap_count: usize, hold_count: usize, slide_count: usize, touch_count: usize, break_count: usize` |
| `ScoreState` | `ScoreState` | `combo: usize, p_combo: usize, c_p_combo: usize, total_base: usize, total_extra: usize, earned_base: usize, earned_extra: usize, lost_base: usize, lost_extra: usize, dx_score: usize, max_dx_score: usize, fast_count: usize, late_count: usize, counts: NoteTypeJudgeCounts` |
| `JudgeEvent` | `JudgeEvent` | `kind: JudgeEventKind, grade: JudgeGrade, diff: Duration, position: RuntimePos, note_index: usize` |

### 2.3 Score Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `ComboDelta` | `ComboDelta` | `combo: usize, p_combo: usize, c_p_combo: usize, d_x_score_lost: usize` |
| `AccRates` | `AccRates` | `classic_acc_plus: f64, classic_acc_minus: f64, dx_acc_minus_101: f64, dx_acc_minus_100: f64, dx_acc_plus: f64` |

### 2.4 Storage Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `ButtonVec (alpha)` | `ButtonVec<T>` | `data: [T; 8]` |
| `SensorVec (alpha)` | `SensorVec<T>` | `data: [T; 33]` |

### 2.5 Lifecycle Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `CommonNoteParams` | `CommonNoteParams` | `judge_timing: TimePoint, judge_offset: Duration, is_break: bool, is_ex: bool, note_index: usize` |
| `TapNote` | `TapNote` | `params: CommonNoteParams, lane: OuterSlot, state: TapState, button_queue_index: usize` |
| `HoldNote` | `HoldNote` | `params: CommonNoteParams, start: HoldStart, state: HoldSubState, length: Duration, head_diff: Option<Duration>, head_grade: Option<JudgeGrade>, player_release_time: Option<TimePoint>, is_classic: bool, is_touch_hold: bool, touch_group_id: Option<usize>, touch_group_size: Option<usize>, touch_group_count: Option<usize>` |
| `TouchNote` | `TouchNote` | `params: CommonNoteParams, state: TouchState, sensor_pos: SensorArea, touch_group_id: Option<usize>, touch_group_size: Option<usize>, touch_group_count: Option<usize>` |
| `SlideNote` | `SlideNote` | `params: CommonNoteParams, lane: OuterSlot, state: SlideState, length: Duration, timing: TimePoint, start_timing: TimePoint, slide_kind: SlideKind, is_classic: bool, is_conn_slide: bool, parent_index: Option<usize>, group_indices: Vec<usize>, judge_queues: Vec<Vec<SlideArea>>` |
| `SlideArea` | `SlideArea` | `target_areas: Vec<SensorArea>, policy: AreaPolicy, is_last: bool, is_skippable: bool, arrow_progress_when_on: Option<TimePoint>, arrow_progress_when_finished: Option<TimePoint>, was_on: bool, was_off: bool` |
| `SlideStepContext` | `SlideStepContext` | `current_time: TimePoint, touch_panel_offset: Duration, delta: Duration, style: JudgeStyle, subdivide_slide_judge_grade: bool, sensor_held: SensorVec<bool>` |
| `SlideStepSemantic` | `SlideStepSemantic` | internal semantic result structure |

### 2.6 Input Model Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `FrameInput` | `FrameInput` | `button_clicked: ButtonVec<bool>, button_held: ButtonVec<bool>, sensor_clicked: SensorVec<bool>, sensor_held: SensorVec<bool>, button_click_count: ButtonVec<usize>, sensor_click_count: SensorVec<usize>, delta: Duration` |
| `TimedInputBatch` | `TimedInputBatch` | `current_time: TimePoint, events: Vec<TimedInputEvent>` |
| `FrameWindow` | `FrameWindow` | `prev_time: TimePoint, current_time: TimePoint` |
| `ZoneQueue (alpha)` | `ZoneQueue<T>` | `notes: Vec<T>, current_index: usize` |
| `GameState` | `GameState` | `current_time: TimePoint, prev_button: ButtonVec<bool>, prev_sensor: SensorVec<bool>, button_queue_frontiers: ButtonVec<usize>, touch_queue_frontiers: SensorVec<usize>, tap_queues: ButtonVec<ZoneQueue<TapNote>>, hold_queues: ButtonVec<ZoneQueue<HoldNote>>, touch_hold_queues: SensorVec<ZoneQueue<HoldNote>>, touch_queues: SensorVec<ZoneQueue<TouchNote>>, slides: Vec<SlideNote>, active_holds: Vec<usize>, active_touch_holds: Vec<usize>, touch_group_states: Vec<GroupState>, touch_hold_group_states: Vec<GroupState>, current_batch: Option<TimedInputBatch>, score: ScoreState, judge_style: JudgeStyle, touch_panel_offset: Duration, use_button_ring_for_touch: bool, subdivide_slide_judge_grade: bool` |

### 2.7 Chart Loader Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `TapChartNote` | `TapChartNote` | `timing: TimePoint, slot: OuterSlot, is_break: bool, is_ex: bool, button_queue_index: usize, note_index: usize` |
| `HoldChartNote` | `HoldChartNote` | `timing: TimePoint, slot: OuterSlot, length: Duration, is_break: bool, is_ex: bool, is_touch: bool, is_classic: bool, button_queue_index: usize, touch_hold_group_id: Option<usize>, touch_hold_group_size: Option<usize>, note_index: usize` |
| `TouchHoldChartNote` | `TouchHoldChartNote` | `timing: TimePoint, sensor_pos: SensorArea, length: Duration, is_break: bool, is_ex: bool, touch_queue_index: usize, touch_group_id: Option<usize>, touch_group_size: Option<usize>, note_index: usize` |
| `TouchChartNote` | `TouchChartNote` | `timing: TimePoint, sensor_pos: SensorArea, is_break: bool, touch_queue_index: usize, touch_group_id: Option<usize>, touch_group_size: Option<usize>, note_index: usize` |
| `SlideChartNote` | `SlideChartNote` | `timing: TimePoint, slot: OuterSlot, length: Duration, start_timing: TimePoint, slide_kind: SlideKind, is_classic: bool, is_conn_slide: bool, parent_index: Option<usize>, group_indices: Vec<usize>, track_count: usize, judge_at: Vec<TimePoint>, is_break: bool, is_ex: bool, note_index: usize, judge_queues: Vec<Vec<SlideArea>>, debug_simai: Option<String>` |
| `ChartSpec` | `ChartSpec` | `taps: Vec<TapChartNote>, holds: Vec<HoldChartNote>, touches: Vec<TouchChartNote>, touch_holds: Vec<TouchHoldChartNote>, slides: Vec<SlideChartNote>, slide_skipping: bool` |

### 2.8 Scheduler Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `ClickCursor` | `ClickCursor` | `button_clicks: ButtonVec<usize>, sensor_clicks: SensorVec<usize>` |

### 2.9 FFI Structures

| Lean Structure | Rust Structure | Rust Fields |
|----------------|----------------|-------------|
| `RuntimeStepResult` | `RuntimeStepResult` | `state: GameState, events: Vec<JudgeEvent>, audio_commands: Vec<AudioCommand>, render_commands: Vec<RenderCommand>` |
| `RuntimeStepLightResult` | `RuntimeStepLightResult` | `events: Vec<JudgeEvent>, audio_commands: Vec<AudioCommand>, render_commands: Vec<RenderCommand>, score: ScoreState, current_time: TimePoint` |
| `LoadedChartSummary` | `LoadedChartSummary` | `tap_count: usize, hold_count: usize, touch_count: usize, touch_hold_count: usize, slide_count: usize` |
| `RuntimeRegistry` | `RuntimeRegistry` | `next_handle: u64, sessions: HashMap<u64, RuntimeSession>` |

---

## 3. Function Mappings

### 3.1 Judgment Functions (Judge.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `judgeTap` | `judge_tap` | `fn judge_tap(diff: Duration, is_ex: bool) -> JudgeGrade` |
| `judgeTouch` | `judge_touch` | `fn judge_touch(diff: Duration, is_ex: bool) -> Option<JudgeGrade>` |
| `judgeSlideModern` | `judge_slide_modern` | `fn judge_slide_modern(diff: Duration, stay_time: Duration, is_ex: bool) -> JudgeGrade` |
| `judgeSlideClassic` | `judge_slide_classic` | `fn judge_slide_classic(diff: Duration) -> JudgeGrade` |
| `correctSlideGrade` | `correct_slide_grade` | `fn correct_slide_grade(grade: JudgeGrade) -> JudgeGrade` |
| `judgeHoldEnd` | `judge_hold_end` | `fn judge_hold_end(head_grade: JudgeGrade, judge_diff: Duration, length: Duration, ignore_time: Duration, player_release_time: Option<TimePoint>) -> JudgeGrade` |
| `judgeHoldClassicEnd` | `judge_hold_classic_end` | `fn judge_hold_classic_end(head_grade: JudgeGrade, timing: TimePoint, length: Duration, release_timing: Option<TimePoint>) -> JudgeGrade` |
| `judgeSlideTooLate` | `judge_slide_too_late` | `fn judge_slide_too_late(queue_remaining: usize) -> JudgeGrade` |
| `isTooLateSlide` | `is_too_late_slide` | `fn is_too_late_slide(diff: Duration, user_offset: Duration) -> bool` |

### 3.2 Lifecycle Functions (Lifecycle.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `tapStep` | `tap_step` | `fn tap_step(note: &TapNote, current_time: TimePoint, judge_diff: Duration, input_clicked: bool, style: JudgeStyle) -> (TapNote, Option<JudgeEvent>)` |
| `holdStep` | `hold_step` | `fn hold_step(note: &HoldNote, current_time: TimePoint, judge_diff: Duration, ...) -> (HoldNote, Option<JudgeEvent>)` |
| `touchStep` | `touch_step` | `fn touch_step(note: &TouchNote, current_time: TimePoint, judge_diff: Duration, input_clicked: bool, shared_result: Option<JudgeGrade>, style: JudgeStyle) -> (TouchNote, Option<JudgeEvent>)` |
| `slideStep` | `slide_step` | `fn slide_step(note: &SlideNote, current_time: TimePoint, sensor_held: &SensorVec<bool>, touch_panel_offset: Duration, delta: Duration, style: JudgeStyle, subdivide_slide_judge_grade: bool) -> (SlideNote, Vec<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>)` |

### 3.3 Scheduler Functions (Scheduler.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `stepFrame` | `step_frame` | `fn step_frame(state: &GameState, input: &FrameInput) -> (GameState, Vec<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>)` |
| `stepFrameTimed` | `step_frame_timed` | `fn step_frame_timed(state: &GameState, batch: &TimedInputBatch) -> (GameState, Vec<JudgeEvent>, Vec<AudioCommand>, Vec<RenderCommand>)` |

### 3.4 Chart Loader Functions (ChartLoader.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `buildGameState` | `build_game_state` | `fn build_game_state(chart: &ChartSpec) -> GameState` |
| `parseChartJson` | `parse_chart_json` | `fn parse_chart_json(json: &JsonValue) -> Result<ChartSpec, String>` |
| `parseChartJsonString` | `parse_chart_json_string` | `fn parse_chart_json_string(s: &str) -> Result<ChartSpec, String>` |
| `loadChartFile` | `load_chart_file` | `fn load_chart_file(path: &Path) -> Result<ChartSpec, String>` |

### 3.5 Score Functions (Score.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `baseScore` | `base_score` | `fn base_score(note_type: NoteType) -> usize` |
| `extraScore` | `extra_score` | `fn extra_score(note_type: NoteType) -> usize` |
| `scoreNonBreak` | `score_non_break` | `fn score_non_break(base_score: usize, grade: JudgeGrade, multiple: usize) -> (usize, usize)` |
| `scoreBreak` | `score_break` | `fn score_break(grade: JudgeGrade, multiple: usize) -> (usize, usize, usize, usize, usize, usize)` |
| `updateCombo` | `update_combo` | `fn update_combo(combo: usize, p_combo: usize, c_p_combo: usize, d_x_score_lost: usize, grade: JudgeGrade, multiple: usize) -> ComboDelta` |
| `countFastLate` | `count_fast_late` | `fn count_fast_late(grade: JudgeGrade, diff: Duration, display: FastLateDisplay) -> (bool, bool)` |
| `dxScoreRank` | `dx_score_rank` | `fn dx_score_rank(achieved: usize, max: usize) -> usize` |
| `computeAccRates` | `compute_acc_rates` | `fn compute_acc_rates(score: &ScoreState) -> AccRates` |

### 3.6 Convert Functions (Convert.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `convertMaji` | `convert_maji` | `fn convert_maji(grade: JudgeGrade) -> JudgeGrade` |
| `convertGachi` | `convert_gachi` | `fn convert_gachi(grade: JudgeGrade) -> JudgeGrade` |
| `convertGrade` | `convert_grade` | `fn convert_grade(style: JudgeStyle, grade: JudgeGrade) -> JudgeGrade` |

### 3.7 Storage Functions (Storage.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `ButtonVec.replicate` | `ButtonVec::replicate` | `fn replicate(value: T) -> ButtonVec<T>` |
| `SensorVec.replicate` | `SensorVec::replicate` | `fn replicate(value: T) -> SensorVec<T>` |
| `ButtonVec.getD` | `ButtonVec::get` | `fn get(&self, zone: ButtonZone) -> &T` |
| `SensorVec.getD` | `SensorVec::get` | `fn get(&self, area: SensorArea) -> &T` |
| `ButtonVec.set` | `ButtonVec::set` | `fn set(&mut self, zone: ButtonZone, value: T)` |
| `SensorVec.set` | `SensorVec::set` | `fn set(&mut self, area: SensorArea, value: T)` |
| `ButtonVec.toList` | `ButtonVec::to_vec` | `fn to_vec(&self) -> Vec<T>` |
| `SensorVec.toList` | `SensorVec::to_vec` | `fn to_vec(&self) -> Vec<T>` |
| `ButtonVec.entries` | `ButtonVec::entries` | `fn entries(&self) -> Vec<(ButtonZone, &T)>` |
| `SensorVec.entries` | `SensorVec::entries` | `fn entries(&self) -> Vec<(SensorArea, &T)>` |
| `ButtonVec.ofFn` | `ButtonVec::from_fn` | `fn from_fn(f: impl Fn(ButtonZone) -> T) -> ButtonVec<T>` |
| `SensorVec.ofFn` | `SensorVec::from_fn` | `fn from_fn(f: impl Fn(SensorArea) -> T) -> SensorVec<T>` |
| `ButtonVec.mapAccum` | `ButtonVec::map_accum` | `fn map_accum<S>(&self, init: S, f: impl Fn(S, ButtonZone, T) -> (S, T)) -> (S, ButtonVec<T>)` |
| `SensorVec.mapAccum` | `SensorVec::map_accum` | `fn map_accum<S>(&self, init: S, f: impl Fn(S, SensorArea, T) -> (S, T)) -> (S, SensorVec<T>)` |

### 3.8 Area Functions (Areas.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `SensorArea.all` | `SensorArea::all` | `fn all() -> &'static [SensorArea]` |
| `ButtonZone.all` | `ButtonZone::all` | `fn all() -> &'static [ButtonZone]` |
| `OuterSlot.all` | `OuterSlot::all` | `fn all() -> &'static [OuterSlot]` |
| `SensorArea.toIndex` | `SensorArea::to_index` | `fn to_index(&self) -> usize` |
| `SensorArea.ofIndex?` | `SensorArea::from_index` | `fn from_index(index: usize) -> Option<SensorArea>` |
| `ButtonZone.toIndex` | `ButtonZone::to_index` | `fn to_index(&self) -> usize` |
| `ButtonZone.ofIndex?` | `ButtonZone::from_index` | `fn from_index(index: usize) -> Option<ButtonZone>` |
| `OuterSlot.toIndex` | `OuterSlot::to_index` | `fn to_index(&self) -> usize` |
| `OuterSlot.ofIndex?` | `OuterSlot::from_index` | `fn from_index(index: usize) -> Option<OuterSlot>` |
| `SensorArea.label` | `SensorArea::label` | `fn label(&self) -> &'static str` |
| `SensorArea.code` | `SensorArea::code` | `fn code(&self) -> &'static str` |
| `ButtonZone.code` | `ButtonZone::code` | `fn code(&self) -> &'static str` |
| `OuterSlot.code` | `OuterSlot::code` | `fn code(&self) -> &'static str` |
| `SensorArea.rotate` | `SensorArea::rotate` | `fn rotate(&self, n: usize) -> SensorArea` |
| `ButtonZone.rotate` | `ButtonZone::rotate` | `fn rotate(&self, n: usize) -> ButtonZone` |
| `OuterSlot.rotate` | `OuterSlot::rotate` | `fn rotate(&self, n: usize) -> OuterSlot` |
| `OuterSlot.toButtonZone` | `OuterSlot::to_button_zone` | `fn to_button_zone(&self) -> ButtonZone` |
| `ButtonZone.toOuterSlot` | `ButtonZone::to_outer_slot` | `fn to_outer_slot(&self) -> OuterSlot` |
| `OuterSlot.toOuterSensorArea` | `OuterSlot::to_outer_sensor_area` | `fn to_outer_sensor_area(&self) -> SensorArea` |
| `SensorArea.toOuterSlot?` | `SensorArea::to_outer_slot` | `fn to_outer_slot(&self) -> Option<OuterSlot>` |
| `ButtonZone.toOuterSensorArea` | `ButtonZone::to_outer_sensor_area` | `fn to_outer_sensor_area(&self) -> SensorArea` |
| `SensorArea.toOuterButtonZone?` | `SensorArea::to_outer_button_zone` | `fn to_outer_button_zone(&self) -> Option<ButtonZone>` |

### 3.9 Time Functions (Time.lean)

| Lean Function | Rust Function | Signature |
|---------------|---------------|-----------|
| `TimeTick.ofInt` | `TimeTick::from_int` | `fn from_int(tick: i64) -> TimeTick` |
| `TimeTick.toInt` | `TimeTick::to_int` | `fn to_int(&self) -> i64` |
| `TimeTick.zero` | `TimeTick::zero` | `fn zero() -> TimeTick` |
| `Duration.ofTick` | `Duration::from_tick` | `fn from_tick(tick: TimeTick) -> Duration` |
| `Duration.ofInt` | `Duration::from_int` | `fn from_int(micros: i64) -> Duration` |
| `Duration.zero` | `Duration::zero` | `fn zero() -> Duration` |
| `Duration.toTick` | `Duration::to_tick` | `fn to_tick(&self) -> TimeTick` |
| `Duration.toInt` | `Duration::to_int` | `fn to_int(&self) -> i64` |
| `Duration.fromMicros` | `Duration::from_micros` | `fn from_micros(micros: i64) -> Duration` |
| `Duration.toMicros` | `Duration::to_micros` | `fn to_micros(&self) -> i64` |
| `Duration.scaleNat` | `Duration::scale_nat` | `fn scale_nat(&self, n: usize) -> Duration` |
| `Duration.divNat` | `Duration::div_nat` | `fn div_nat(&self, n: usize) -> Duration` |
| `Duration.abs` | `Duration::abs` | `fn abs(&self) -> Duration` |
| `TimePoint.ofTick` | `TimePoint::from_tick` | `fn from_tick(tick: TimeTick) -> TimePoint` |
| `TimePoint.ofInt` | `TimePoint::from_int` | `fn from_int(micros: i64) -> TimePoint` |
| `TimePoint.zero` | `TimePoint::zero` | `fn zero() -> TimePoint` |
| `TimePoint.toTick` | `TimePoint::to_tick` | `fn to_tick(&self) -> TimeTick` |
| `TimePoint.toInt` | `TimePoint::to_int` | `fn to_int(&self) -> i64` |
| `TimePoint.fromMicros` | `TimePoint::from_micros` | `fn from_micros(micros: i64) -> TimePoint` |
| `TimePoint.toMicros` | `TimePoint::to_micros` | `fn to_micros(&self) -> i64` |
| `Time.microsPerMilli` | `MICROS_PER_MILLI` | `const MICROS_PER_MILLI: i64 = 1000` |
| `Time.microsPerSecond` | `MICROS_PER_SECOND` | `const MICROS_PER_SECOND: i64 = 1_000_000` |
| `Time.microsPerMinute` | `MICROS_PER_MINUTE` | `const MICROS_PER_MINUTE: i64 = 60_000_000` |
| `Time.millisToMicros` | `millis_to_micros` | `fn millis_to_micros(millis: i64) -> i64` |
| `Time.quantizeRatMicros` | `quantize_rat_micros` | `fn quantize_rat_micros(rat: f64) -> i64` |
| `Time.durationFromRatMicros` | `duration_from_rat_micros` | `fn duration_from_rat_micros(rat: f64) -> Duration` |
| `Time.pointFromRatMicros` | `point_from_rat_micros` | `fn point_from_rat_micros(rat: f64) -> TimePoint` |
| `Time.bpmBeatMicrosRat` | `bpm_beat_micros_rat` | `fn bpm_beat_micros_rat(bpm: f64) -> f64` |
| `Time.bpmMeasureMicrosRat` | `bpm_measure_micros_rat` | `fn bpm_measure_micros_rat(bpm: f64) -> f64` |
| `Time.durationFromSecondsRat` | `duration_from_seconds_rat` | `fn duration_from_seconds_rat(seconds: f64) -> Duration` |
| `Time.pointFromSecondsRat` | `point_from_seconds_rat` | `fn point_from_seconds_rat(seconds: f64) -> TimePoint` |
| `Time.fromMillis` | `from_millis` | `fn from_millis(millis: i64) -> Duration` |
| `Time.pointFromMillis` | `point_from_millis` | `fn point_from_millis(millis: i64) -> TimePoint` |
| `Time.quantizeSecondsString` | `quantize_seconds_string` | `fn quantize_seconds_string(s: &str) -> Option<i64>` |
| `Time.parseSecondsString?` | `parse_seconds_string` | `fn parse_seconds_string(s: &str) -> Option<Duration>` |
| `Time.parseSecondsPointString?` | `parse_seconds_point_string` | `fn parse_seconds_point_string(s: &str) -> Option<TimePoint>` |

---

## 4. Constant Mappings

### 4.1 Frame Constants

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `FRAME_LENGTH` | `FRAME_LENGTH` | `Duration::from_micros(16667)` |
| `FRAME_LENGTH_MSEC` | `FRAME_LENGTH_MSEC` | `Duration::from_micros(16667)` |

### 4.2 Tap Judgment Windows

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `TAP_JUDGE_SEG_1ST_PERFECT_MSEC` | `TAP_JUDGE_SEG_1ST_PERFECT_MSEC` | `Duration::from_micros(16667)` |
| `TAP_JUDGE_SEG_2ND_PERFECT_MSEC` | `TAP_JUDGE_SEG_2ND_PERFECT_MSEC` | `Duration::from_micros(33334)` |
| `TAP_JUDGE_SEG_3RD_PERFECT_MSEC` | `TAP_JUDGE_SEG_3RD_PERFECT_MSEC` | `Duration::from_micros(50001)` |
| `TAP_JUDGE_SEG_1ST_GREAT_MSEC` | `TAP_JUDGE_SEG_1ST_GREAT_MSEC` | `Duration::from_micros(66668)` |
| `TAP_JUDGE_SEG_2ND_GREAT_MSEC` | `TAP_JUDGE_SEG_2ND_GREAT_MSEC` | `Duration::from_micros(83335)` |
| `TAP_JUDGE_SEG_3RD_GREAT_MSEC` | `TAP_JUDGE_SEG_3RD_GREAT_MSEC` | `Duration::from_micros(100002)` |
| `TAP_JUDGE_SEG_1ST_GOOD_MSEC` | `TAP_JUDGE_SEG_1ST_GOOD_MSEC` | `Duration::from_micros(116669)` |
| `TAP_JUDGE_SEG_2ND_GOOD_MSEC` | `TAP_JUDGE_SEG_2ND_GOOD_MSEC` | `Duration::from_micros(133336)` |
| `TAP_JUDGE_GOOD_AREA_MSEC` | `TAP_JUDGE_GOOD_AREA_MSEC` | `Duration::from_micros(150003)` |

### 4.3 Touch Judgment Windows

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC` | `TOUCH_JUDGE_SEG_1ST_PERFECT_MSEC` | `Duration::from_micros(150003)` |
| `TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC` | `TOUCH_JUDGE_SEG_2ND_PERFECT_MSEC` | `Duration::from_micros(166670)` |
| `TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC` | `TOUCH_JUDGE_SEG_3RD_PERFECT_MSEC` | `Duration::from_micros(183337)` |
| `TOUCH_JUDGE_SEG_1ST_GREAT_MSEC` | `TOUCH_JUDGE_SEG_1ST_GREAT_MSEC` | `Duration::from_micros(200004)` |
| `TOUCH_JUDGE_SEG_2ND_GREAT_MSEC` | `TOUCH_JUDGE_SEG_2ND_GREAT_MSEC` | `Duration::from_micros(216671)` |
| `TOUCH_JUDGE_SEG_3RD_GREAT_MSEC` | `TOUCH_JUDGE_SEG_3RD_GREAT_MSEC` | `Duration::from_micros(233338)` |
| `TOUCH_JUDGE_SEG_1ST_GOOD_MSEC` | `TOUCH_JUDGE_SEG_1ST_GOOD_MSEC` | `Duration::from_micros(250005)` |
| `TOUCH_JUDGE_SEG_2ND_GOOD_MSEC` | `TOUCH_JUDGE_SEG_2ND_GOOD_MSEC` | `Duration::from_micros(266672)` |
| `TOUCH_JUDGE_GOOD_AREA_MSEC` | `TOUCH_JUDGE_GOOD_AREA_MSEC` | `Duration::from_micros(283339)` |

### 4.4 Hold Constants

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `HOLD_HEAD_IGNORE_LENGTH_SEC` | `HOLD_HEAD_IGNORE_LENGTH_SEC` | `Duration::from_micros(100002)` |
| `HOLD_TAIL_IGNORE_LENGTH_SEC` | `HOLD_TAIL_IGNORE_LENGTH_SEC` | `Duration::from_micros(200004)` |
| `TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC` | `TOUCH_HOLD_HEAD_IGNORE_LENGTH_SEC` | `Duration::from_micros(250005)` |
| `TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC` | `TOUCH_HOLD_TAIL_IGNORE_LENGTH_SEC` | `Duration::from_micros(200004)` |
| `DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC` | `DELUXE_HOLD_RELEASE_IGNORE_TIME_SEC` | `Duration::from_micros(33334)` |
| `CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC` | `CLASSIC_HOLD_ALLOW_OVER_LENGTH_SEC` | `Duration::from_micros(333340)` |
| `HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC` | `HOLD_CLASSIC_END_JUDGE_PERFECT_FAST_MSEC` | `Duration::from_micros(150003)` |
| `HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC` | `HOLD_CLASSIC_END_JUDGE_PERFECT_LATE_MSEC` | `Duration::from_micros(200004)` |

### 4.5 Slide Constants

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC` | `SLIDE_JUDGE_MAXIMUM_ALLOWED_EXT_LENGTH_MSEC` | `Duration::from_micros(366674)` |
| `SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC` | `SLIDE_JUDGE_SEG_BASE_3RD_PERFECT_MSEC` | `Duration::from_micros(233338)` |
| `SLIDE_JUDGE_SEG_1ST_GREAT_MSEC` | `SLIDE_JUDGE_SEG_1ST_GREAT_MSEC` | `Duration::from_micros(350007)` |
| `SLIDE_JUDGE_SEG_2ND_GREAT_MSEC` | `SLIDE_JUDGE_SEG_2ND_GREAT_MSEC` | `Duration::from_micros(416675)` |
| `SLIDE_JUDGE_SEG_3RD_GREAT_MSEC` | `SLIDE_JUDGE_SEG_3RD_GREAT_MSEC` | `Duration::from_micros(483343)` |
| `SLIDE_JUDGE_GOOD_AREA_MSEC` | `SLIDE_JUDGE_GOOD_AREA_MSEC` | `Duration::from_micros(600012)` |

### 4.6 Count Constants

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `BUTTON_ZONE_COUNT` | `BUTTON_ZONE_COUNT` | `8` |
| `SENSOR_AREA_COUNT` | `SENSOR_AREA_COUNT` | `33` |

### 4.7 Range Constants

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `JUDGABLE_RANGE_SEC` | `JUDGABLE_RANGE_SEC` | `Duration::from_micros(150000)` |
| `TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC` | `TOUCH_JUDGABLE_RANGE_LATE_EXTRA_SEC` | `Duration::from_micros(166670)` |

### 4.8 Feature Flags

| Lean Constant | Rust Constant | Value |
|---------------|---------------|-------|
| `JUDGE_OFFSET` | `JUDGE_OFFSET` | `Duration::from_micros(0)` |
| `TOUCH_PANEL_OFFSET` | `TOUCH_PANEL_OFFSET` | `Duration::from_micros(0)` |
| `USE_BUTTON_RING_FOR_TOUCH` | `USE_BUTTON_RING_FOR_TOUCH` | `false` |
| `SUBDIVIDE_SLIDE_JUDGE_GRADE` | `SUBDIVIDE_SLIDE_JUDGE_GRADE` | `false` |

---

## 5. Instance Mappings

### 5.1 Time Instances

| Lean Instance | Rust Implementation |
|---------------|---------------------|
| `LT TimeTick` | `impl PartialOrd for TimeTick` |
| `LE TimeTick` | `impl PartialOrd for TimeTick` |
| `Ord TimeTick` | `impl Ord for TimeTick` |
| `Min TimeTick` | `impl Ord for TimeTick` |
| `Max TimeTick` | `impl Ord for TimeTick` |
| `Add TimeTick` | `impl Add for TimeTick` |
| `Sub TimeTick` | `impl Sub for TimeTick` |
| `Neg TimeTick` | `impl Neg for TimeTick` |
| `OfNat TimeTick` | `impl From<usize> for TimeTick` |
| `HAdd TimeTick TimeTick TimeTick` | `impl Add for TimeTick` |
| `HSub TimeTick TimeTick TimeTick` | `impl Sub for TimeTick` |
| `ToJson TimeTick` | `impl Serialize for TimeTick` |
| `FromJson TimeTick` | `impl Deserialize for TimeTick` |
| `LT Duration` | `impl PartialOrd for Duration` |
| `LE Duration` | `impl PartialOrd for Duration` |
| `Ord Duration` | `impl Ord for Duration` |
| `Min Duration` | `impl Ord for Duration` |
| `Max Duration` | `impl Ord for Duration` |
| `Add Duration` | `impl Add for Duration` |
| `Sub Duration` | `impl Sub for Duration` |
| `Neg Duration` | `impl Neg for Duration` |
| `OfNat Duration` | `impl From<usize> for Duration` |
| `HAdd Duration Duration Duration` | `impl Add for Duration` |
| `HSub Duration Duration Duration` | `impl Sub for Duration` |
| `ToJson Duration` | `impl Serialize for Duration` |
| `FromJson Duration` | `impl Deserialize for Duration` |
| `LT TimePoint` | `impl PartialOrd for TimePoint` |
| `LE TimePoint` | `impl PartialOrd for TimePoint` |
| `Ord TimePoint` | `impl Ord for TimePoint` |
| `Min TimePoint` | `impl Ord for TimePoint` |
| `Max TimePoint` | `impl Ord for TimePoint` |
| `Add TimePoint Duration TimePoint` | `impl Add<Duration> for TimePoint` |
| `Sub TimePoint Duration TimePoint` | `impl Sub<Duration> for TimePoint` |
| `HSub TimePoint TimePoint Duration` | `impl Sub<TimePoint> for TimePoint` |
| `ToJson TimePoint` | `impl Serialize for TimePoint` |
| `FromJson TimePoint` | `impl Deserialize for TimePoint` |

### 5.2 Area Instances

| Lean Instance | Rust Implementation |
|---------------|---------------------|
| `ToString SensorArea` | `impl Display for SensorArea` |
| `ToJson SensorArea` | `impl Serialize for SensorArea` |
| `FromJson SensorArea` | `impl Deserialize for SensorArea` |
| `Inhabited SensorArea` | `impl Default for SensorArea` |
| `Repr SensorArea` | `impl Debug for SensorArea` |
| `BEq SensorArea` | `impl PartialEq for SensorArea` |
| `DecidableEq SensorArea` | `impl Eq for SensorArea` |
| `Ord SensorArea` | `impl Ord for SensorArea` |
| `ToString ButtonZone` | `impl Display for ButtonZone` |
| `ToJson ButtonZone` | `impl Serialize for ButtonZone` |
| `FromJson ButtonZone` | `impl Deserialize for ButtonZone` |
| `Inhabited ButtonZone` | `impl Default for ButtonZone` |
| `Repr ButtonZone` | `impl Debug for ButtonZone` |
| `BEq ButtonZone` | `impl PartialEq for ButtonZone` |
| `DecidableEq ButtonZone` | `impl Eq for ButtonZone` |
| `Ord ButtonZone` | `impl Ord for ButtonZone` |
| `ToString OuterSlot` | `impl Display for OuterSlot` |
| `ToJson OuterSlot` | `impl Serialize for OuterSlot` |
| `FromJson OuterSlot` | `impl Deserialize for OuterSlot` |
| `Inhabited OuterSlot` | `impl Default for OuterSlot` |
| `Repr OuterSlot` | `impl Debug for OuterSlot` |
| `BEq OuterSlot` | `impl PartialEq for OuterSlot` |
| `DecidableEq OuterSlot` | `impl Eq for OuterSlot` |
| `Ord OuterSlot` | `impl Ord for OuterSlot` |

### 5.3 Storage Instances

| Lean Instance | Rust Implementation |
|---------------|---------------------|
| `Inhabited (ButtonVec alpha)` | `impl<T: Default> Default for ButtonVec<T>` |
| `Repr (ButtonVec alpha)` | `impl<T: Debug> Debug for ButtonVec<T>` |
| `ToJson (ButtonVec alpha)` | `impl<T: Serialize> Serialize for ButtonVec<T>` |
| `FromJson (ButtonVec alpha)` | `impl<T: Deserialize> Deserialize for ButtonVec<T>` |
| `Inhabited (SensorVec alpha)` | `impl<T: Default> Default for SensorVec<T>` |
| `Repr (SensorVec alpha)` | `impl<T: Debug> Debug for SensorVec<T>` |
| `ToJson (SensorVec alpha)` | `impl<T: Serialize> Serialize for SensorVec<T>` |
| `FromJson (SensorVec alpha)` | `impl<T: Deserialize> Deserialize for SensorVec<T>` |

---

## 6. Semantic Notes

### 6.1 Integer Semantics

- Lean `Int` → Rust `i64`
- Lean `Nat` → Rust `usize` (or `u64` for FFI handles)
- Division uses integer division with `roundDivAwayFromZero` for quantization
- Overflow behavior must be documented and consistent

### 6.2 Pure Functional State Machines

All lifecycle transitions are pure functions returning `(NewState, Option JudgeEvent)`. No mutation. The Rust rewrite must maintain this pattern.

### 6.3 Frame Processing Order

The Scheduler processes notes in a fixed order: tap → hold → touch → touch-hold → slide. This is semantically meaningful and must be preserved exactly.

### 6.4 Shared Queue Indexing

Taps and holds sharing the same button zone use a shared `buttonQueueFrontiers` mechanism. Touches and touch-holds sharing sensor areas use `touchQueueFrontiers`. This prevents double-consumption of clicks.

### 6.5 Slide Queue Traversal

Slides use multi-track queues (`List (List SlideArea)`) with skip logic, parent-child conn-slide relationships, and force-finish semantics. The `slideQueueCore` function is the critical traversal algorithm.

### 6.6 Touch Group Sharing

Touch notes and touch-holds sharing the same sensor area form groups. A strict majority (>50%) is required for group result sharing. The `GroupState` tracks count/size/grade/diff.

### 6.7 Hold End Judgment

Modern holds use a 5-band press table based on held percentage. Classic holds use independent timing comparison with worst-of semantics. The release-ignore grace period (2 frames) is skipped for missed heads.

### 6.8 Score Computation

15-tier grades with non-linear score mapping. Break notes have dual DX/Classic extra scoring tracks. Combo tracking includes Perfect-Combo and Critical-Perfect-Combo chains.

---

*Generated for Lean → Rust verified rewrite project.*
