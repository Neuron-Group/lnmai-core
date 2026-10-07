import LnmaiCore.Simai
import LnmaiCore.Simai.DSL
import LnmaiCore.Simai.SlideTables
import LnmaiCore.Proofs.Simai
import Lean.Data.Json

namespace LnmaiCore.Simai.Tests

/-- Lean mirror of `../reference/PySimaiParser/tests/test_core.py`. -/

structure ParityCase where
  name : String
  supported : Bool
  passed : Bool
  note : String
deriving Repr

private def parseLevel1 (content : String) : Except ParseError FrontendChartResult :=
  compileChart content 1

example (content : String) : compileChart content 1 = parseFrontendChartResult content 1 := rfl
example (content : String) : compileNormalized content 1 = frontendNormalizedChart content 1 := rfl
example (content : String) : compileInspection content 1 = parseFrontendInspectionChart content 1 := rfl
example (noteText : String) : compileSingleNormalizedSlide noteText = parseFrontendSingleNormalizedSlide noteText := rfl

private def supportedCase (name : String) (passed : Bool) (note : String := "") : ParityCase :=
  { name := name, supported := true, passed := passed, note := note }

private def gapCase (name : String) (note : String) : ParityCase :=
  { name := name, supported := false, passed := false, note := note }

private def areaCodes (queue : List SlideAreaSpec) : List String :=
  queue.map (fun spec =>
    match spec.targetAreas with
    | area :: _ => area.code
    | [] => "")

private def areaGroups (queue : List SlideAreaSpec) : List (List String) :=
  queue.map (fun spec => spec.targetAreas.map ExactArea.label)

private def queueSkippableFlags (queue : List SlideAreaSpec) : List Bool :=
  queue.map (fun spec => spec.isSkippable)

def test_simai_chart_dsl_smoke : ParityCase :=
  let chart : FrontendChartResult := simai_chart! "&first=0\n&inote_1=\n(120)\n1,\n"
  supportedCase "simai_chart_dsl_smoke"
    (chart.semantic.normalized.taps.length = 1)
    "chart DSL elaborates through frontend parser"

def test_simai_slide_dsl_smoke : ParityCase :=
  let slide : SlideNoteSemantics := simai_slide! "1V35"
  let normalized : NormalizedSlide := simai_normalized_slide! "1w5[4:1]"
  supportedCase "simai_slide_dsl_smoke"
    (slide.startSlot = .S1 && slide.endArea = .A5 &&
     slide.shape.kind = SlideKind.turn &&
     normalized.trackCount = 3 && normalized.slideKind = LnmaiCore.SlideKind.Wifi)
    "slide DSL elaborates through shared parser/normalizer"

def test_simai_chart_level_dsl_smoke : ParityCase :=
  let chart : FrontendChartResult :=
    simai_chart_at! 2 "&first=0\n&inote_1=\n(120)\n1,\n&inote_2=\n(120)\n2,\n"
  supportedCase "simai_chart_level_dsl_smoke"
    (match chart.semantic.normalized.taps with
     | tap :: _ => tap.slot = .S2
     | _ => false)
    "level-aware chart DSL selects the requested inote block"

def test_simai_normalized_chart_dsl_smoke : ParityCase :=
  let chart : NormalizedChart := simai_normalized_chart! "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n"
  supportedCase "simai_normalized_chart_dsl_smoke"
    (match chart.slides with
     | slide :: _ => !slide.judgeQueues.isEmpty && slide.totalJudgeQueueLen > 0
     | _ => false)
    "normalized chart DSL exposes normalization-owned topology"

def test_simai_lowered_slide_split_ir_dsl : ParityCase :=
  let ordinary : LoweredSlideSplitIr := simai_lowered_slide_split_ir! "1-3[4:1]"
  let connected : LoweredSlideSplitIr := simai_lowered_slide_split_ir! "1-3[4:1]*>5[4:1]"
  supportedCase "simai_lowered_slide_split_ir_dsl"
    (match ordinary.slideHeads, ordinary.slideBodies with
     | [head], [body] =>
         match connected.slideHeads, connected.slideBodies with
         | [connHead], [firstBody, secondBody] =>
             head.logicalNoteIndex = head.logicalSlideId &&
             body.logicalNoteIndex = body.logicalSlideId &&
             head.runtimeNoteIndex = head.noteIndex &&
             body.runtimeNoteIndex = body.noteIndex &&
             head.logicalSlideId = body.logicalSlideId &&
             head.noteIndex != body.noteIndex &&
             head.timingMicros = body.headTimingMicros &&
             connHead.logicalNoteIndex = firstBody.logicalNoteIndex &&
             connHead.runtimeNoteIndex = connHead.noteIndex &&
             firstBody.runtimeNoteIndex = firstBody.noteIndex &&
             connHead.logicalSlideId = firstBody.logicalSlideId &&
             connHead.noteIndex != firstBody.noteIndex &&
             !firstBody.isSlideNoHead &&
             secondBody.isSlideNoHead
         | _, _ => false
     | _, _ => false)
    "lowered slide DSL now shows explicit head/body split instead of only lowered slide bodies"

def test_simai_normalized_slide_ir_identity_dsl : ParityCase :=
  let slides : List NormalizedSlideIr := simai_normalized_slide_ir! "1-3[4:1]*>5[4:1]"
  supportedCase "simai_normalized_slide_ir_identity_dsl"
    (match slides with
     | [first, second] =>
         let firstIds := first.runtimeIds
         let secondIds := second.runtimeIds
         firstIds.logicalNoteIndex = first.noteIndex &&
         firstIds.hasSeparateHeadAndBodyRuntimeIds &&
         firstIds.primaryRuntimeNoteIndex = firstIds.bodyRuntimeNoteIndex.getD 0 &&
         secondIds.logicalNoteIndex = second.noteIndex &&
         secondIds.isBodyOnlyRuntimeIdShape &&
         secondIds.primaryRuntimeNoteIndex = secondIds.bodyRuntimeNoteIndex.getD 0 &&
         second.parentNoteIndex = some first.noteIndex
     | _ => false)
    "normalized slide DSL exposes separate head/body runtime ids even when it still presents one logical slide node"

def test_metadata_parsing : ParityCase :=
  match parseFrontendMaidata "&title=My Awesome Song\n&artist=The Best Artist\n&des=Chart Master\n&first=1.5\n&lv_1=1\n&lv_4=10+\n&lv_5=12\n&uot_other=some_value\n" with
  | .ok file =>
      supportedCase "metadata_parsing"
        (file.metadata.fields.any (fun p => p.1 = "&title" && p.2 = "My Awesome Song") &&
         file.metadata.fields.any (fun p => p.1 = "&artist" && p.2 = "The Best Artist") &&
         file.metadata.fields.any (fun p => p.1 = "&des" && p.2 = "Chart Master") &&
         file.metadata.fields.any (fun p => p.1 = "&first" && p.2 = "1.5"))
        "raw metadata fields parse"
  | .error err => supportedCase "metadata_parsing" false s!"unexpected parse error: {err.message}"

def test_empty_fumen : ParityCase :=
  match parseLevel1 "&inote_1=\n" with
  | .ok chart =>
      supportedCase "empty_fumen"
        (chart.inspection.tokens.isEmpty && chart.semantic.normalized.slides.isEmpty && chart.semantic.normalized.taps.isEmpty)
        "empty inote lowers to empty token stream"
  | .error err => supportedCase "empty_fumen" false s!"unexpected parse error: {err.message}"

def test_simple_tap_and_bpm : ParityCase :=
  match parseLevel1 "&first=0.5\n&inote_1=\n(120)\n1,\n2,\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps with
      | first :: second :: _ =>
        supportedCase "simple_tap_and_bpm"
            (first.slot = .S1 && second.slot = .S2 &&
             first.timing = TimePoint.fromMicros 500000 && second.timing = TimePoint.fromMicros 1000000)
            "&first offset and BPM step apply"
      | _ => supportedCase "simple_tap_and_bpm" false "expected two taps"
  | .error err => supportedCase "simple_tap_and_bpm" false s!"unexpected parse error: {err.message}"

def test_hold_note_basic_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1h[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.holds with
      | hold :: _ => supportedCase "hold_note_basic_duration" (hold.slot = .S1 && hold.length = Duration.fromMicros 1000000) "generic BPM parsing works"
      | _ => supportedCase "hold_note_basic_duration" false "expected one hold"
  | .error err => supportedCase "hold_note_basic_duration" false s!"unexpected parse error: {err.message}"

def test_hold_note_custom_bpm_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1h[120#4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.holds with
      | hold :: _ => supportedCase "hold_note_custom_bpm_duration" (hold.length = Duration.fromMicros 500000) "custom-BPM duration works"
      | _ => supportedCase "hold_note_custom_bpm_duration" false "expected one hold"
  | .error err => supportedCase "hold_note_custom_bpm_duration" false s!"unexpected parse error: {err.message}"

def test_hold_note_absolute_time_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1h[#2.5],\n" with
  | .ok chart =>
      match chart.semantic.normalized.holds with
      | hold :: _ => supportedCase "hold_note_absolute_time_duration" (hold.length = Duration.fromMicros 2500000) "absolute duration works"
      | _ => supportedCase "hold_note_absolute_time_duration" false "expected one hold"
  | .error err => supportedCase "hold_note_absolute_time_duration" false s!"unexpected parse error: {err.message}"

def test_slide_note_duration_and_star_wait : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-4[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_duration_and_star_wait"
            (slide.length = Duration.fromMicros 500000 &&
             slide.startTiming = TimePoint.fromMicros 500000 &&
             slide.judgeAt = some (TimePoint.fromMicros 905000))
            "slide duration and star-wait both lower"
      | _ => supportedCase "slide_note_duration_and_star_wait" false "expected one slide"
  | .error err => supportedCase "slide_note_duration_and_star_wait" false s!"unexpected parse error: {err.message}"

def test_slide_note_custom_bpm_star_and_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1V[120#8:1],\n" with
  | .ok _ => supportedCase "slide_note_custom_bpm_star_and_duration" false "bare `V` slide should be rejected"
  | .error err =>
      supportedCase "slide_note_custom_bpm_star_and_duration"
        (err.message = "missing digit at 2" || err.message = "expected digit at 2" ||
         err.message = "V slide requires explicit turn and end positions" ||
         err.message = "invalid connected slide syntax")
        "bare `V` slide is rejected because turn slides require explicit turn and end positions"

def test_slide_note_absolute_star_wait_no_hash_and_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1<5[0.2##0.75],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_absolute_star_wait_no_hash_and_duration"
            (slide.startTiming = TimePoint.fromMicros 200000 &&
             slide.length = Duration.fromMicros 750000 &&
             slide.judgeAt = some (TimePoint.fromMicros 863000))
            "absolute star-wait and duration work"
      | _ => supportedCase "slide_note_absolute_star_wait_no_hash_and_duration" false "expected one slide"
  | .error err => supportedCase "slide_note_absolute_star_wait_no_hash_and_duration" false s!"unexpected parse error: {err.message}"

def test_classic_slide_mode_lowers_reference_timing_and_queue : ParityCase :=
  let content := "&first=0\n&inote_1=\n(120)\n1-4[4:1],\n"
  match parseFrontendChartResultWithMode content 1 true with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | [slide], [lowered] =>
          let runtime := ChartLoader.buildGameState chart.semantic.lowered
          supportedCase "classic_slide_mode_lowers_reference_timing_and_queue"
            (slide.isClassic && lowered.isClassic &&
             slide.judgeAt = some (TimePoint.fromMicros 885000) &&
             lowered.judgeAt = slide.judgeAt && slide.judgeQueues.length = 1 &&
             runtime.slides.any (fun note => note.isClassic &&
               note.params.judgeTiming = TimePoint.fromMicros 885000 &&
               note.startTiming + note.length - note.params.judgeTiming =
                 Duration.fromMicros 115000))
            "classic line4 reaches runtime with ClassicConst=0.23 and a 115ms end wait"
      | _, _ => supportedCase "classic_slide_mode_lowers_reference_timing_and_queue" false
          "expected one normalized and lowered slide"
  | .error err => supportedCase "classic_slide_mode_lowers_reference_timing_and_queue" false err.message

def test_classic_slide_mode_selects_wifi_and_connected_timing : ParityCase :=
  let content := "&first=0\n&inote_1=\n(120)\n1w5[4:1],1-3-5[4:1],\n"
  match parseFrontendChartResultWithMode content 1 true, parseFrontendChartResult content 1 with
  | .ok classic, .ok modern =>
      match classic.semantic.lowered.slides, modern.semantic.lowered.slides with
      | [wifi, parent, child], modernWifi :: _ =>
          supportedCase "classic_slide_mode_selects_wifi_and_connected_timing"
            (wifi.isClassic && parent.isClassic && child.isClassic &&
             wifi.judgeQueues.map List.length = [4, 3, 4] &&
             modernWifi.judgeQueues.map List.length = [4, 4, 4] &&
             !(wifi.judgeQueues[1]!.getLast!).isLast &&
             wifi.judgeAt = some (TimePoint.fromMicros 918565) &&
             child.startTiming = parent.startTiming + parent.length &&
             child.judgeAt = some (TimePoint.fromMicros 1430750) &&
             child.isGroupEnd && child.parentNoteIndex = some parent.noteIndex)
            "classic wifi uses its three-area center; connected tail uses its own ClassicConst"
      | _, _ => supportedCase "classic_slide_mode_selects_wifi_and_connected_timing" false
          "expected wifi followed by two connected segments"
  | _, _ => supportedCase "classic_slide_mode_selects_wifi_and_connected_timing" false
      "expected both playback modes to parse"

theorem slide_body_start_is_later_than_head_for_44pace_reference_case :
    test_slide_note_duration_and_star_wait.passed = true := by native_decide

theorem slide_body_start_supports_explicit_absolute_star_wait :
    test_slide_note_absolute_star_wait_no_hash_and_duration.passed = true := by native_decide

def test_touch_note : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\nA1/C,E4h[4:1],\n" with
  | .ok chart =>
      supportedCase "touch_note"
        (chart.semantic.normalized.touches.length = 2 && chart.semantic.normalized.touchHolds.length = 1)
        "touch and touch-hold tokenize and lower"
  | .error err => supportedCase "touch_note" false s!"unexpected parse error: {err.message}"

def test_slash_each_touch_allocates_touch_group : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\nA1/D1,\n" with
  | .ok chart =>
      let state := ChartLoader.buildGameState chart.semantic.lowered
      match chart.semantic.lowered.touches,
          (InputModel.sensorQueueAt state.touchQueues SensorArea.A1).notes,
          (InputModel.sensorQueueAt state.touchQueues SensorArea.D1).notes with
      | [left, right], [runtimeLeft], [runtimeRight] =>
          supportedCase "slash_each_touch_allocates_touch_group"
            (left.sourceGroupId.isSome &&
             left.sourceGroupId = right.sourceGroupId &&
             runtimeLeft.touchGroupId.isSome &&
             runtimeLeft.touchGroupId = runtimeRight.touchGroupId &&
             runtimeLeft.touchGroupSize = 2 &&
             runtimeRight.touchGroupSize = 2)
            "slash-simultaneous connected touches should receive MajdataPlay-style touch groups"
      | _, _, _ =>
          supportedCase "slash_each_touch_allocates_touch_group" false
            "expected two lowered touches and one runtime touch per sensor"
  | .error err =>
      supportedCase "slash_each_touch_allocates_touch_group" false
        s!"unexpected parse error: {err.message}"

def test_slash_each_touchhold_allocates_head_and_body_groups : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\nA1h[4:1]/D1h[4:1],\n" with
  | .ok chart =>
      let state := ChartLoader.buildGameState chart.semantic.lowered
      match chart.semantic.lowered.touchHolds,
          (InputModel.sensorQueueAt state.touchHoldQueues SensorArea.A1).notes,
          (InputModel.sensorQueueAt state.touchHoldQueues SensorArea.D1).notes,
          state.touchHoldGroupStates with
      | [left, right], [runtimeLeft], [runtimeRight], [bodyGroup] =>
          supportedCase "slash_each_touchhold_allocates_head_and_body_groups"
            (left.sourceGroupId.isSome &&
             left.sourceGroupId = right.sourceGroupId &&
             runtimeLeft.touchGroupId.isSome &&
             runtimeLeft.touchGroupId = runtimeRight.touchGroupId &&
             runtimeLeft.touchGroupSize = 2 &&
             runtimeRight.touchGroupSize = 2 &&
             runtimeLeft.touchHoldGroupId.isSome &&
             runtimeLeft.touchHoldGroupId = runtimeRight.touchHoldGroupId &&
             runtimeLeft.touchHoldGroupSize = 2 &&
             runtimeRight.touchHoldGroupSize = 2 &&
             bodyGroup.memberNoteIndices.length = 2)
            "slash-simultaneous touch-holds should receive both head touch groups and body groups"
      | _, _, _, _ =>
          supportedCase "slash_each_touchhold_allocates_head_and_body_groups" false
            "expected two lowered touch-holds, runtime queue entries, and one body group"
  | .error err =>
      supportedCase "slash_each_touchhold_allocates_head_and_body_groups" false
        s!"unexpected parse error: {err.message}"

def test_touch_nohead_slide_each_suppression_matches_reference : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\nA1/1?-3[4:1],\n" with
  | .ok chart =>
      let state := ChartLoader.buildGameState chart.semantic.lowered
      match chart.semantic.lowered.touches,
          (InputModel.sensorQueueAt state.touchQueues SensorArea.A1).notes with
      | [touch], [runtimeTouch] =>
          supportedCase "touch_nohead_slide_each_suppression_matches_reference"
            (touch.sourceGroupId.isNone && runtimeTouch.touchGroupId.isNone)
            "MajdataPlay suppresses touch Each when only no-head slides accompany one touch"
      | _, _ =>
          supportedCase "touch_nohead_slide_each_suppression_matches_reference" false
            "expected one lowered and runtime touch"
  | .error err =>
      supportedCase "touch_nohead_slide_each_suppression_matches_reference" false
        s!"unexpected parse error: {err.message}"

def test_touchhold_nohead_slide_still_allocates_each_group : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\nA1h[4:1]/1?-3[4:1],\n" with
  | .ok chart =>
      let state := ChartLoader.buildGameState chart.semantic.lowered
      match chart.semantic.lowered.touchHolds,
          (InputModel.sensorQueueAt state.touchHoldQueues SensorArea.A1).notes,
          state.touchHoldGroupStates with
      | [touchHold], [runtimeHold], [bodyGroup] =>
          supportedCase "touchhold_nohead_slide_still_allocates_each_group"
            (touchHold.sourceGroupId.isSome &&
             runtimeHold.touchGroupId.isSome &&
             runtimeHold.touchGroupSize = 1 &&
             runtimeHold.touchHoldGroupId.isSome &&
             runtimeHold.touchHoldGroupSize = 1 &&
             bodyGroup.memberNoteIndices.length = 1)
            "MajdataPlay keeps touch-hold Each grouping when a no-head slide is the only companion"
      | _, _, _ =>
          supportedCase "touchhold_nohead_slide_still_allocates_each_group" false
            "expected one lowered touch-hold, one runtime touch-hold, and one body group"
  | .error err =>
      supportedCase "touchhold_nohead_slide_still_allocates_each_group" false
        s!"unexpected parse error: {err.message}"

def test_modifiers : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1bfx$,\n2h[4:1]b!,\n" with
  | .ok chart =>
      match chart.inspection.tokens with
      | tapTok :: holdTok :: _ =>
          supportedCase "modifiers"
            (tapTok.isBreak && tapTok.isEX && tapTok.isHanabi && tapTok.isForceStar &&
             holdTok.isBreak && holdTok.isSlideNoHead)
            "basic note modifiers parse"
      | _ => supportedCase "modifiers" false "expected two tokens"
  | .error err => supportedCase "modifiers" false s!"unexpected parse error: {err.message}"

def test_slide_modifiers : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1b-2[4:1]x!$$,\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides with
      | tok :: _, slide :: _ =>
          supportedCase "slide_modifiers"
            (tok.isBreak && !tok.isSlideBreak && tok.isEX && tok.isSlideNoHead && tok.isForceStar && tok.isFakeRotate &&
             slide.isBreak && slide.isEX && slide.isSlideNoHead && slide.isForceStar && slide.isFakeRotate)
            "slide head modifiers parse"
      | _, _ => supportedCase "slide_modifiers" false "expected one slide token"
  | .error err => supportedCase "slide_modifiers" false s!"unexpected parse error: {err.message}"

private def slideBreakFlagsMatch (noteText : String) (headBreak bodyBreak : Bool) : Bool :=
  match parseLevel1 s!"&first=0\n&inote_1=\n(120)\n{noteText},\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides,
          chart.semantic.lowered.slideHeads, chart.semantic.lowered.slides with
      | tok :: _, slide :: _, head :: _, body :: _ =>
          (tok.isBreak == headBreak) &&
          (tok.isSlideBreak == bodyBreak) &&
          (slide.isBreak == headBreak) &&
          (slide.isSlideBreak == bodyBreak) &&
          (head.isBreak == headBreak) &&
          (body.isBreak == bodyBreak)
      | _, _, _, _ => false
  | .error _ => false

def test_slide_break_on_segment : ParityCase :=
  supportedCase "slide_break_on_segment"
    (slideBreakFlagsMatch "1-3b[4:1]" false true &&
     slideBreakFlagsMatch "1>3b[4:1]" false true &&
     slideBreakFlagsMatch "1w5b[4:1]" false true &&
     slideBreakFlagsMatch "1-3[4:1]b" false true &&
     slideBreakFlagsMatch "1b-3[4:1]b" true true)
    "MajSimai recognizes slide breaks before timing brackets and after the complete timing spec"

def test_lowered_slide_break_split_uses_segment_break_for_body : ParityCase :=
  let segmentBody :=
    slideBreakFlagsMatch "1-3b[4:1]" false true &&
    slideBreakFlagsMatch "1>3b[4:1]" false true &&
    slideBreakFlagsMatch "1w5b[4:1]" false true
  let headOnlyBreak :=
    slideBreakFlagsMatch "1b-3[4:1]" true false &&
    slideBreakFlagsMatch "1b>3[4:1]" true false &&
    slideBreakFlagsMatch "1bw5[4:1]" true false
  let headAndBodyBreak :=
    slideBreakFlagsMatch "1b-3b[4:1]" true true &&
    slideBreakFlagsMatch "1b>3b[4:1]" true true &&
    slideBreakFlagsMatch "1bw5b[4:1]" true true
  supportedCase "lowered_slide_break_split_uses_segment_break_for_body"
    (segmentBody && headOnlyBreak && headAndBodyBreak)
    "lowered slide heads use head break while lowered slide bodies use segment-local slide break"

def test_simultaneous_notes_slash : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1/8/Ch[4:1],\n" with
  | .ok chart =>
      supportedCase "simultaneous_notes_slash"
        (chart.inspection.tokens.length = 3 && chart.semantic.normalized.taps.length = 2 && chart.semantic.normalized.touchHolds.length = 1)
        "simple slash splitting works"
  | .error err => supportedCase "simultaneous_notes_slash" false s!"unexpected parse error: {err.message}"

def test_pseudo_simultaneous_backtick : ParityCase :=
  match parseLevel1 "&first=1.0\n&inote_1=\n(60)\n1`2h[4:1]`A3,\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps, chart.semantic.normalized.holds, chart.semantic.normalized.touches with
      | tap :: _, hold :: _, touch :: _ =>
          supportedCase "pseudo_simultaneous_backtick"
            (tap.timing = TimePoint.fromMicros 1000000 &&
             hold.timing = TimePoint.fromMicros 1031250 &&
             touch.timing = TimePoint.fromMicros 1062500)
            "backtick pseudo-simultaneous timing works"
      | _, _, _ => supportedCase "pseudo_simultaneous_backtick" false "expected tap/hold/touch sequence"
  | .error err => supportedCase "pseudo_simultaneous_backtick" false s!"unexpected parse error: {err.message}"

def test_comment_handling : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120) || BPM set\n1, || Note 1\n|| Standalone comment\n2, || Note 2\n" with
  | .ok chart =>
      supportedCase "comment_handling"
        (chart.semantic.normalized.taps.length = 2 &&
         match chart.semantic.normalized.taps with
         | first :: second :: _ => first.timing = TimePoint.zero && second.timing = TimePoint.fromMicros 500000
         | _ => false)
        "line comments are stripped"
  | .error err => supportedCase "comment_handling" false s!"unexpected parse error: {err.message}"

def test_beat_signature_change : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1,\n{8}\n2,\n{2}\n3,\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps with
      | first :: second :: third :: _ =>
          supportedCase "beat_signature_change"
            (first.timing = TimePoint.zero &&
             second.timing = TimePoint.fromMicros 1000000 &&
             third.timing = TimePoint.fromMicros 1500000)
            "beat/divisor changes work"
      | _ => supportedCase "beat_signature_change" false "expected three taps"
  | .error err => supportedCase "beat_signature_change" false s!"unexpected parse error: {err.message}"

def test_hspeed_change : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n<H2.5>\n1,\n<HS*0.5>\n2,\n" with
  | .ok chart =>
      match chart.inspection.tokens with
      | first :: second :: _ =>
          supportedCase "hspeed_change"
            (first.hSpeed = (5 : Rat) / 2 && second.hSpeed = (1 : Rat) / 2)
            "hspeed directives parse"
      | _ => supportedCase "hspeed_change" false "expected two tap tokens"
  | .error err => supportedCase "hspeed_change" false s!"unexpected parse error: {err.message}"

def test_unfit_bpm_quantizes_consistently : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(180)\n1,\n2,\n3,\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps with
      | first :: second :: third :: _ =>
          supportedCase "unfit_bpm_quantizes_consistently"
            (first.timing = TimePoint.zero &&
             second.timing = TimePoint.fromMicros 333333 &&
             third.timing = TimePoint.fromMicros 666667)
            "non-integral beat durations quantize once per absolute event with stable nearest-microsecond results"
      | _ => supportedCase "unfit_bpm_quantizes_consistently" false "expected three taps"
  | .error err => supportedCase "unfit_bpm_quantizes_consistently" false s!"unexpected parse error: {err.message}"

def test_rational_inspection_json_is_stable : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(180)\n<H2.5>\n1,\n" with
  | .ok chart =>
      match chart.inspection.tokens with
      | token :: _ =>
          let bpmJson := Lean.toJson token.bpm
          let hSpeedJson := Lean.toJson token.hSpeed
          let expectedBpmJson := Lean.Json.mkObj [("num", Lean.toJson (180 : ℤ)), ("den", Lean.toJson (1 : Nat)), ("decimal", Lean.Json.str "180")]
          let expectedHSpeedJson := Lean.Json.mkObj [("num", Lean.toJson (5 : ℤ)), ("den", Lean.toJson (2 : Nat)), ("decimal", Lean.Json.str "2.5")]
          supportedCase "rational_inspection_json_is_stable"
            (bpmJson == expectedBpmJson && hSpeedJson == expectedHSpeedJson)
            "inspection rationals serialize as stable num/den/decimal objects"
      | _ => supportedCase "rational_inspection_json_is_stable" false "expected one token"
  | .error err => supportedCase "rational_inspection_json_is_stable" false s!"unexpected parse error: {err.message}"

def test_same_head_slide_group_lowering : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1],\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides,
          chart.semantic.lowered.slides with
      | [tok1, tok2], [first, second], [body1, body2] =>
          supportedCase "same_head_slide_group_lowering"
            (chart.inspection.source.events.length = 1 &&
             tok1.sourceGroupId.isNone && tok2.sourceGroupId.isNone &&
             !tok1.isSlideNoHead && tok2.isSlideNoHead &&
             !first.isConnSlide && !second.isConnSlide &&
             first.slot = .S1 && second.slot = .S1 &&
             first.startTiming = TimePoint.fromMicros 500000 &&
             second.startTiming = first.startTiming &&
             first.parentNoteIndex.isNone && second.parentNoteIndex.isNone &&
             !body1.isConnSlide && !body2.isConnSlide &&
             chart.semantic.lowered.slideHeads.length = 1)
            "MajSimai * branches share a head and run simultaneously without parent links"
      | _, _, _ => supportedCase "same_head_slide_group_lowering" false "expected two branches"
  | .error err => supportedCase "same_head_slide_group_lowering" false err.message

def test_lowered_ordinary_slide_splits_head_and_body : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slideHeads, chart.semantic.lowered.slides with
      | normalized :: _, head :: _, body :: _ =>
         supportedCase "lowered_ordinary_slide_splits_head_and_body"
            (normalized.hasHeadNote && normalized.hasBody && !normalized.isSlideNoHead &&
             chart.semantic.lowered.slideHeads.length = 1 &&
             chart.semantic.lowered.slides.length = 1 &&
             head.logicalSlideId = body.logicalSlideId &&
             head.noteIndex != body.noteIndex &&
             head.timing = body.headTiming &&
             head.slot = body.slot)
            "ordinary slides now lower into one explicit slide-head note plus one slide-body note"
      | _, _, _ => supportedCase "lowered_ordinary_slide_splits_head_and_body" false "expected one normalized slide plus one lowered slide head and one lowered slide body"
  | .error err => supportedCase "lowered_ordinary_slide_splits_head_and_body" false s!"unexpected parse error: {err.message}"

def test_identical_simultaneous_slides_fold_body_multiplicity : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]/1-3[4:1],\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides, chart.semantic.lowered.slideHeads, chart.semantic.lowered.slides with
      | _ :: _ :: _, [normalized], [head1, head2], [body] =>
          supportedCase "identical_simultaneous_slides_fold_body_multiplicity"
            (normalized.multiple = 2 &&
             body.multiple = 2 &&
             head1.logicalSlideId = body.logicalSlideId &&
             head2.logicalSlideId = body.logicalSlideId &&
             head1.noteIndex != head2.noteIndex &&
             head1.noteIndex != body.noteIndex &&
             head2.noteIndex != body.noteIndex)
            "identical simultaneous slides lower to multiple heads plus one body carrying the folded Multiple count"
      | _, _, _, _ =>
          supportedCase "identical_simultaneous_slides_fold_body_multiplicity" false
            "expected two source tokens, one folded normalized slide body, and two lowered heads"
  | .error err =>
      supportedCase "identical_simultaneous_slides_fold_body_multiplicity" false
        s!"unexpected parse error: {err.message}"

def test_identical_simultaneous_connected_slides_fold_group_multiplicity : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]>5[4:1]/1-3[4:1]>5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slideHeads, chart.semantic.lowered.slides with
      | [firstNormalized, secondNormalized], [head1, head2], [firstBody, secondBody] =>
          supportedCase "identical_simultaneous_connected_slides_fold_group_multiplicity"
            (chart.inspection.tokens.length = 4 &&
             firstNormalized.multiple = 2 &&
             secondNormalized.multiple = 2 &&
             firstNormalized.isConnSlide &&
             secondNormalized.isConnSlide &&
             firstNormalized.isGroupHead &&
             !firstNormalized.isGroupEnd &&
             !secondNormalized.isGroupHead &&
             secondNormalized.isGroupEnd &&
             secondNormalized.parentNoteIndex = some firstNormalized.noteIndex &&
             firstBody.multiple = 2 &&
             secondBody.multiple = 2 &&
             firstBody.isConnSlide &&
             secondBody.isConnSlide &&
             secondBody.parentNoteIndex = some firstBody.noteIndex &&
             head1.logicalSlideId = firstBody.logicalSlideId &&
             head2.logicalSlideId = firstBody.logicalSlideId &&
             head1.noteIndex != head2.noteIndex)
            "identical simultaneous connected slide chains fold as one MajdataPlay Multiple group"
      | _, _, _ =>
          supportedCase "identical_simultaneous_connected_slides_fold_group_multiplicity" false
            "expected four source tokens, two folded connected bodies, and two lowered heads"
  | .error err =>
      supportedCase "identical_simultaneous_connected_slides_fold_group_multiplicity" false
        s!"unexpected parse error: {err.message}"

def test_connected_slide_multiplicity_requires_whole_group_match : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]>5[4:1]/1-3[4:1]>6[4:1],\n" with
  | .ok chart =>
      supportedCase "connected_slide_multiplicity_requires_whole_group_match"
        (chart.semantic.normalized.slides.length = 4 &&
         chart.semantic.lowered.slides.length = 4 &&
         chart.semantic.normalized.slides.all (fun slide => slide.multiple = 1) &&
         chart.semantic.lowered.slides.all (fun slide => slide.multiple = 1))
        "connected slide folding compares the whole group and does not fold a shared prefix segment alone"
  | .error err =>
      supportedCase "connected_slide_multiplicity_requires_whole_group_match" false
        s!"unexpected parse error: {err.message}"

def test_lowered_headless_slide_has_body_only : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1?-3[4:1],\n" with
  | .ok chart =>
      supportedCase "lowered_headless_slide_has_body_only"
        (chart.semantic.normalized.slides.all (fun note => !note.hasHeadNote && note.hasBody && note.isSlideNoHead) &&
         chart.semantic.lowered.slideHeads.isEmpty &&
         chart.semantic.lowered.slides.length = 1 &&
         chart.semantic.lowered.slides.all (fun note => note.isSlideNoHead))
        "no-head slides lower to slide-body notes without a separate slide-head note"
  | .error err => supportedCase "lowered_headless_slide_has_body_only" false s!"unexpected parse error: {err.message}"

def test_lowered_conn_group_has_one_head_for_first_body : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slideHeads, chart.semantic.lowered.slides with
      | firstNormalized :: secondNormalized :: _, head :: _, firstBody :: secondBody :: _ =>
          supportedCase "lowered_conn_group_has_one_head_for_first_body"
            (firstNormalized.hasHeadNote && firstNormalized.hasBody &&
             !secondNormalized.hasHeadNote && secondNormalized.hasBody &&
             chart.semantic.lowered.slideHeads.length = 1 &&
             chart.semantic.lowered.slides.length = 2 &&
             head.logicalSlideId = firstBody.logicalSlideId &&
             head.noteIndex != firstBody.noteIndex &&
             !firstBody.isSlideNoHead &&
             secondBody.isSlideNoHead)
            "same-head connected groups lower to one slide head for the first body and headless child bodies thereafter"
      | _, _, _ => supportedCase "lowered_conn_group_has_one_head_for_first_body" false "expected two normalized slides plus one lowered slide head and two lowered slide bodies"
  | .error err => supportedCase "lowered_conn_group_has_one_head_for_first_body" false s!"unexpected parse error: {err.message}"

def test_same_head_wifi_group_accepted : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1]*-3[4:1],\n" with
  | .ok chart =>
      supportedCase "same_head_wifi_group_accepted"
        (chart.semantic.normalized.slides.length = 2 &&
         chart.semantic.normalized.slides.all (fun s => !s.isConnSlide) &&
         chart.semantic.lowered.slideHeads.length = 1)
        "wifi is legal in simultaneous branches"
  | .error err => supportedCase "same_head_wifi_group_accepted" false err.message

def test_same_head_conn_child_start_inherits_parent_end : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1<5[4:1]*1>5[4:1],\n" with
  | .ok _ => supportedCase "same_head_conn_child_start_inherits_parent_end" false "expected malformed same-head group to be rejected"
  | .error err =>
      supportedCase "same_head_conn_child_start_inherits_parent_end"
        (err.kind = .invalidSyntax)
        "typed validation preserves rejection of malformed same-head connection syntax before lowering"

def test_normalized_slide_topology_attached : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | slide :: _, lowered :: _ =>
          supportedCase "normalized_slide_topology_attached"
            (!slide.judgeQueues.isEmpty && slide.totalJudgeQueueLen > 0 &&
             !lowered.judgeQueues.isEmpty && lowered.totalJudgeQueueLen = slide.totalJudgeQueueLen)
            "normalization attaches authoritative slide queues before runtime build"
      | _, _ => supportedCase "normalized_slide_topology_attached" false "expected one lowered slide"
  | .error err => supportedCase "normalized_slide_topology_attached" false s!"unexpected parse error: {err.message}"

def test_normalized_line3_has_protected_middle_segment : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | slide :: _, lowered :: _ =>
          let normalizedQueue := slide.judgeQueues.headD []
          let loweredQueue := lowered.judgeQueues.headD []
          let normalizedProtected :=
            areaGroups normalizedQueue = [["Sensor A1"], ["Sensor A2", "Sensor B2"], ["Sensor A3"]] &&
            queueSkippableFlags normalizedQueue = [true, false, true]
          let loweredProtected :=
            areaGroups loweredQueue = [["Sensor A1"], ["Sensor A2", "Sensor B2"], ["Sensor A3"]] &&
            queueSkippableFlags loweredQueue = [true, false, true]
          supportedCase "normalized_line3_has_protected_middle_segment"
            (slide.totalJudgeQueueLen = 3 && lowered.totalJudgeQueueLen = 3 &&
             normalizedProtected && loweredProtected)
            "normalized and lowered line3 retain the protected middle A2/B2 segment, matching the short-slide jump-protection rule"
      | _, _ => supportedCase "normalized_line3_has_protected_middle_segment" false "expected one normalized and one lowered slide"
  | .error err => supportedCase "normalized_line3_has_protected_middle_segment" false s!"unexpected parse error: {err.message}"

theorem test_normalized_line3_has_protected_middle_segment_proof :
    test_normalized_line3_has_protected_middle_segment.passed = true := by native_decide

def test_normalized_short_conn_skip_rule : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1^2[4:1]^3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | first :: second :: _ =>
          supportedCase "normalized_short_conn_skip_rule"
            (first.isConnSlide && second.isConnSlide &&
             first.totalJudgeQueueLen = 3 && second.totalJudgeQueueLen = 3 &&
             queueSkippableFlags (first.judgeQueues.headD []) = [true, false] &&
             queueSkippableFlags (second.judgeQueues.headD []) = [false, true])
            "short connected groups fold shared endpoints once and protect the interior entry"
      | _ => supportedCase "normalized_short_conn_skip_rule" false "expected grouped slides"
  | .error err => supportedCase "normalized_short_conn_skip_rule" false s!"unexpected parse error: {err.message}"

def test_continuous_conn_three_part_parent_chain : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]>5[4:1]<7[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | first :: second :: third :: _, loweredFirst :: loweredSecond :: loweredThird :: _ =>
          let normalizedImmediateParentChain :=
            second.parentNoteIndex = some first.noteIndex &&
            third.parentNoteIndex = some second.noteIndex
          let loweredImmediateParentChain :=
            loweredSecond.parentNoteIndex = some loweredFirst.noteIndex &&
            loweredThird.parentNoteIndex = some loweredSecond.noteIndex
          let inheritedTimingChain :=
            second.startTiming = first.startTiming + first.length &&
            third.startTiming = second.startTiming + second.length &&
            loweredSecond.startTiming = loweredFirst.startTiming + loweredFirst.length &&
            loweredThird.startTiming = loweredSecond.startTiming + loweredSecond.length
          supportedCase "same_head_conn_three_part_parent_chain"
            (first.isGroupHead && !first.isGroupEnd && first.parentNoteIndex = none &&
             !second.isGroupHead && !second.isGroupEnd &&
             !third.isGroupHead && third.isGroupEnd &&
             normalizedImmediateParentChain &&
             loweredImmediateParentChain &&
             inheritedTimingChain)
            "3-part connected-slide groups link each child to the immediate previous part, matching MajdataPlay"
      | _, _ => supportedCase "same_head_conn_three_part_parent_chain" false "expected three grouped slides"
  | .error err => supportedCase "same_head_conn_three_part_parent_chain" false s!"unexpected parse error: {err.message}"

def test_same_head_with_tap_head_matches_python_flattening : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1*>2[4:1]*-3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps, chart.semantic.normalized.slides with
      | tap :: _, first :: second :: _ =>
          supportedCase "same_head_with_tap_head_matches_python_flattening"
            (tap.slot = .S1 &&
             !first.isConnSlide && !second.isConnSlide &&
             first.parentNoteIndex.isNone && second.parentNoteIndex.isNone &&
             first.startTiming = second.startTiming &&
             first.isSlideNoHead && second.isSlideNoHead &&
             first.noteIndex < second.noteIndex)
            "head-only first * part stays a tap with simultaneous headless slide branches"
      | _, _ => supportedCase "same_head_with_tap_head_matches_python_flattening" false "expected tap plus two grouped slides"
  | .error err => supportedCase "same_head_with_tap_head_matches_python_flattening" false s!"unexpected parse error: {err.message}"

def test_same_head_subsequent_parts_are_headless : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>2[4:1],\n" with
  | .ok chart =>
      match chart.inspection.tokens with
      | first :: second :: _ =>
          supportedCase "same_head_subsequent_parts_are_headless"
            (!first.isSlideNoHead && second.isSlideNoHead)
            "subsequent `*` parts inherit the head and become headless, matching Python"
      | _ => supportedCase "same_head_subsequent_parts_are_headless" false "expected grouped slide tokens"
  | .error err => supportedCase "same_head_subsequent_parts_are_headless" false s!"unexpected parse error: {err.message}"

def test_continuous_conn_qq_chain_matches_majdataplay : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(180){64}\n3qq7qq5[192#30:109],\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | firstTok :: secondTok :: _, first :: second :: _, loweredFirst :: loweredSecond :: _ =>
          supportedCase "continuous_conn_qq_chain_matches_majdataplay"
            (chart.inspection.tokens.length = 2 &&
             firstTok.rawText = "3qq7" && secondTok.rawText = "7qq5[192#30:109]" &&
             first.slot = .S3 && second.slot = .S7 &&
             first.isConnSlide && second.isConnSlide &&
             first.isGroupHead && !first.isGroupEnd &&
             !second.isGroupHead && second.isGroupEnd &&
             second.parentNoteIndex = some first.noteIndex &&
             second.startTiming = first.startTiming + first.length &&
             first.length = Duration.fromMicros 3110731 &&
             second.length = Duration.fromMicros 1430936 &&
             first.headTiming = TimePoint.zero && second.headTiming = TimePoint.zero &&
             first.startTiming = TimePoint.fromMicros 312500 &&
             first.length + second.length = Duration.fromMicros 4541667 &&
             loweredSecond.startTiming = loweredFirst.startTiming + loweredFirst.length)
            "MajdataPlay-style continuous qq chains split into connected slide parts and preserve proportional whole-chain timing"
      | _, _, _ =>
          supportedCase "continuous_conn_qq_chain_matches_majdataplay" false "expected two connected qq slide parts"
  | .error err =>
      supportedCase "continuous_conn_qq_chain_matches_majdataplay" false s!"unexpected parse error: {err.message}"

def test_continuous_chain_timing_uses_prefab_bar_counts : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]>5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | first :: second :: _ =>
          supportedCase "continuous_chain_timing_uses_prefab_bar_counts"
            (slideBarCountForShapeKey "line3" = some 14 &&
             slideBarCountForShapeKey "circle7" = some 48 &&
             (judgeQueuesForShapeKey "line3" |>.getD []).head!.length = 3 &&
             first.length = Duration.fromMicros 225806 &&
             second.length = Duration.fromMicros 774194 &&
             first.totalJudgeQueueLen = 9 && second.totalJudgeQueueLen = 9 &&
             second.startTiming = first.startTiming + first.length)
            "whole-chain timing weights match MajDataPlay prefab children, not judge queue entries"
      | _ =>
          supportedCase "continuous_chain_timing_uses_prefab_bar_counts" false "expected two connected slide parts"
  | .error err =>
      supportedCase "continuous_chain_timing_uses_prefab_bar_counts" false s!"unexpected parse error: {err.message}"

def test_normalized_topology_comes_from_typed_shape : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1<5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          let strictAccepted :=
            match parseSlideShapeText "1<5[4:1]" with
            | .ok _ => true
            | .error _ => false
          supportedCase "normalized_topology_comes_from_typed_shape"
            (strictAccepted &&
             shapeKey slide.simaiShape = "-circle5" && slide.simaiShape.mirrored &&
             !slide.judgeQueues.isEmpty)
            "strict typed parsing accepts `1<5`, and normalization derives topology from parser-produced shape semantics"
      | _ => supportedCase "normalized_topology_comes_from_typed_shape" false "expected one slide"
  | .error err => supportedCase "normalized_topology_comes_from_typed_shape" false s!"unexpected parse error: {err.message}"

def test_current_slide_strings_parse_strictly : ParityCase :=
  let strictNow :=
    [ "1-2[4:1]"
    , "1b-2[4:1]x!$$"
    , "1<5[4:1]"
    , "1>3[4:1]"
    , "1<3[4:1]"
    , "1-3[4:1]"
    , "1-3b[4:1]"
    , "1v3[4:1]"
    , "1pp3[4:1]"
    , "1V35[4:1]"
    , "1s5[4:1]"
    , "1q3[4:1]"
    , "1p5[4:1]"
    , "1qq3[4:1]"
    , "1V73[4:1]"
    , "1w5[4:1]"
    ]
  supportedCase "current_slide_strings_parse_strictly"
    (strictNow.all (fun raw => (parseSlideShapeText raw).isOk))
    "current real slide strings parse strictly without fallback"

def test_strict_line2_acceptance : ParityCase :=
  let strictAccepted :=
    match parseSlideShapeText "1-2[4:1]" with
    | .ok shape => shapeKey shape = "line2"
    | .error _ => false
  supportedCase "strict_line2_acceptance"
    strictAccepted
    "strict typed parsing accepts adjacent line slides as canonical `line2`, matching MajdataPlay"

def test_strict_circle1_acceptance : ParityCase :=
  let strictAccepted :=
    match parseSlideShapeText "4<4[4:1]" with
    | .ok shape => shapeKey shape = "circle1"
    | .error _ => false
  supportedCase "strict_circle1_acceptance"
    strictAccepted
    "strict typed parsing accepts same-start circle slides as canonical `circle1`, matching MajdataPlay"

def test_reference_circle_mirror_semantics : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1>3[4:1],1<3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | right :: left :: _ =>
          supportedCase "reference_circle_mirror_semantics"
            (shapeKey right.simaiShape = "circle3" &&
             !right.simaiShape.mirrored &&
             shapeKey left.simaiShape = "-circle7" &&
             left.simaiShape.mirrored)
            "`1>3` stays `circle3` while `1<3` mirrors to `-circle7`, matching MajdataPlay"
      | _ => supportedCase "reference_circle_mirror_semantics" false "expected two slides"
  | .error err => supportedCase "reference_circle_mirror_semantics" false s!"unexpected parse error: {err.message}"

def test_reference_circle_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1>3[4:1],1<3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | right :: left :: _ =>
          let rightPath := right.judgeQueues.headD [] |> areaCodes
          let leftPath := left.judgeQueues.headD [] |> areaCodes
          supportedCase "reference_circle_realpaths"
            (rightPath = ["A1", "A2", "A3"] &&
             leftPath = ["A1", "A8", "A7", "A6", "A5", "A4", "A3"])
            "resolved judge queues match MajdataPlay table semantics for `circle3` and mirrored `circle7`"
      | _ => supportedCase "reference_circle_realpaths" false "expected two slides"
  | .error err => supportedCase "reference_circle_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_other_shape_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1],1v3[4:1],1pp3[4:1],1V35[4:1],1s5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | line :: vshape :: pp :: turn :: sshape :: _ =>
          let linePath := line.judgeQueues.headD [] |> areaCodes
          let vPath := vshape.judgeQueues.headD [] |> areaCodes
          let ppPath := pp.judgeQueues.headD [] |> areaCodes
          let turnPath := turn.judgeQueues.headD [] |> areaCodes
          let sPath := sshape.judgeQueues.headD [] |> areaCodes
          supportedCase "reference_other_shape_realpaths"
            (linePath = ["A1", "A2", "A3"] &&
             vPath = ["A1", "B1", "C", "B3", "A3"] &&
             ppPath = ["A1", "B1", "C", "B4", "A3"] &&
             turnPath = ["A1", "B2", "A3", "B4", "A5"] &&
             sPath = ["A1", "B8", "B7", "C", "B3", "B4", "A5"])
            "non-circle slide families resolve to MajdataPlay single-track judge paths"
      | _ => supportedCase "reference_other_shape_realpaths" false "expected five slides"
  | .error err => supportedCase "reference_other_shape_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_pq_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1q3[4:1],1p5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | qshape :: pshape :: _ =>
          let qPath := qshape.judgeQueues.headD [] |> areaCodes
          let pPath := pshape.judgeQueues.headD [] |> areaCodes
          supportedCase "reference_pq_realpaths"
            (qPath = ["A1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B1", "B2", "A3"] &&
             pPath = ["A1", "B8", "B7", "B6", "A5"])
            "pq-family slides resolve to the MajdataPlay judge paths that fixed the real-chart AP regression"
      | _ => supportedCase "reference_pq_realpaths" false "expected two pq-family slides"
  | .error err => supportedCase "reference_pq_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_ppqq_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1pp3[4:1],1pp7[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | shortpp :: longpp :: _ =>
          let shortPath := shortpp.judgeQueues.headD [] |> areaCodes
          let longPath := longpp.judgeQueues.headD [] |> areaCodes
          supportedCase "reference_ppqq_realpaths"
            (shortPath = ["A1", "B1", "C", "B4", "A3"] &&
             longPath = ["A1", "B1", "C", "B4", "A3", "A2", "B1", "B8", "A7"])
            "ppqq-family slides resolve to the MajdataPlay single-track judge paths for both short and long variants"
      | _ => supportedCase "reference_ppqq_realpaths" false "expected two ppqq-family slides"
  | .error err => supportedCase "reference_ppqq_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_mirrored_pq_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1q2[4:1],1p4[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | qshape :: pshape :: _ =>
          let qPath := qshape.judgeQueues.headD [] |> areaCodes
          let pPath := pshape.judgeQueues.headD [] |> areaCodes
          supportedCase "reference_mirrored_pq_realpaths"
            (qshape.simaiShape.mirrored && !pshape.simaiShape.mirrored &&
             qPath = ["A1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B1", "A2"] &&
             pPath = ["A1", "B8", "B7", "B6", "B5", "A4"])
            "`q` mirrors while `p` stays direct, and both resolve to the expected MajdataPlay judge paths"
      | _ => supportedCase "reference_mirrored_pq_realpaths" false "expected one mirrored q slide and one direct p slide"
  | .error err => supportedCase "reference_mirrored_pq_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_mirrored_ppqq_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1qq3[4:1],1qq7[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | shortqq :: longqq :: _ =>
          let shortPath := shortqq.judgeQueues.headD [] |> areaCodes
          let longPath := longqq.judgeQueues.headD [] |> areaCodes
          supportedCase "reference_mirrored_ppqq_realpaths"
            (shortqq.simaiShape.mirrored && longqq.simaiShape.mirrored &&
             shortPath = ["A1", "B1", "C", "B6", "A7", "A8", "B1", "B2", "A3"] &&
             longPath = ["A1", "B1", "C", "B6", "A7"])
            "mirrored ppqq-family slides reflect the MajdataPlay path tables across the cabinet axis"
      | _ => supportedCase "reference_mirrored_ppqq_realpaths" false "expected two mirrored ppqq-family slides"
  | .error err => supportedCase "reference_mirrored_ppqq_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_mirrored_turn_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1V73[4:1],1V75[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | shortTurn :: longTurn :: _ =>
          let shortPath := shortTurn.judgeQueues.headD [] |> areaCodes
          let longPath := longTurn.judgeQueues.headD [] |> areaCodes
          let shortGroups := shortTurn.judgeQueues.headD [] |> areaGroups
          let longGroups := longTurn.judgeQueues.headD [] |> areaGroups
          supportedCase "reference_mirrored_turn_realpaths"
            (!shortTurn.simaiShape.mirrored && !longTurn.simaiShape.mirrored &&
             shortPath = ["A1", "B8", "A7", "B7", "C", "B3", "A3"] &&
             longPath = ["A1", "B8", "A7", "B6", "A5"] &&
             shortGroups = [["Sensor A1"], ["Sensor B8", "Sensor A8"], ["Sensor A7"], ["Sensor B7"], ["Sensor C"], ["Sensor B3"], ["Sensor A3"]] &&
             longGroups = [["Sensor A1"], ["Sensor B8", "Sensor A8"], ["Sensor A7"], ["Sensor B6", "Sensor A6"], ["Sensor A5"]])
            "turn-family slides currently resolve as direct `V` turns with the expected parser/runtime judge paths"
      | _ => supportedCase "reference_mirrored_turn_realpaths" false "expected two direct turn slides"
  | .error err => supportedCase "reference_mirrored_turn_realpaths" false s!"unexpected parse error: {err.message}"

def test_shape_key_is_annotation_not_authority : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | normalized :: _, lowered :: _ =>
          supportedCase "shape_key_is_annotation_not_authority"
            (normalized.simaiShape.kind = SlideKind.wifi &&
             normalized.trackCount = 3 &&
             normalized.slideKind = LnmaiCore.SlideKind.Wifi &&
             lowered.debugSimai = some ("1w5[4:1]", "wifi", false))
            "the string shape key is retained for annotation, while typed shape semantics drive normalization"
      | _, _ => supportedCase "shape_key_is_annotation_not_authority" false "expected one wifi slide"
  | .error err => supportedCase "shape_key_is_annotation_not_authority" false s!"unexpected parse error: {err.message}"

def test_reference_wifi_realpaths : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | wifi :: _ =>
          let leftPath := wifi.judgeQueues.getD 0 [] |> areaCodes
          let centerPath := wifi.judgeQueues.getD 1 [] |> areaCodes
          let rightPath := wifi.judgeQueues.getD 2 [] |> areaCodes
          supportedCase "reference_wifi_realpaths"
            (wifi.trackCount = 3 &&
             leftPath = ["A1", "B8", "B7", "A6"] &&
             centerPath = ["A1", "B1", "C", "A5"] &&
             rightPath = ["A1", "B2", "B3", "A4"])
            "wifi realpaths preserve the MajdataPlay per-segment primary-area paths across all three tracks"
      | _ => supportedCase "reference_wifi_realpaths" false "expected one wifi slide"
  | .error err => supportedCase "reference_wifi_realpaths" false s!"unexpected parse error: {err.message}"

def test_reference_wifi_classic_center_path : ParityCase :=
  let queues := judgeQueuesForShapeKey "wifi" true |>.getD []
  let centerPath := queues.getD 1 [] |> areaCodes
  supportedCase "reference_wifi_classic_center_path"
    (centerPath = ["A1", "B1", "C"])
    "classic wifi center queue stays three segments, matching MajdataPlay"

def test_reference_wifi_multi_area_tails : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | wifi :: _ =>
          let leftGroups := wifi.judgeQueues.getD 0 [] |> areaGroups
          let centerGroups := wifi.judgeQueues.getD 1 [] |> areaGroups
          let rightGroups := wifi.judgeQueues.getD 2 [] |> areaGroups
          supportedCase "reference_wifi_multi_area_tails"
            (leftGroups = [["Sensor A1"], ["Sensor B8"], ["Sensor B7"], ["Sensor A6", "Sensor D6"]] &&
             centerGroups = [["Sensor A1"], ["Sensor B1"], ["Sensor C"], ["Sensor A5", "Sensor B5"]] &&
             rightGroups = [["Sensor A1"], ["Sensor B2"], ["Sensor B3"], ["Sensor A4", "Sensor D5"]])
            "wifi tail segments preserve the full MajdataPlay multi-area target sets on all three tracks"
      | _ => supportedCase "reference_wifi_multi_area_tails" false "expected one wifi slide"
  | .error err => supportedCase "reference_wifi_multi_area_tails" false s!"unexpected parse error: {err.message}"

def test_just_right_is_debug_not_normalized_authority : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | normalized :: _, lowered :: _ =>
          let hasDebugRaw :=
            match chart.semantic.normalized.slideDebug.find? (fun dbg => dbg.noteIndex = normalized.noteIndex) with
            | some dbg => dbg.rawText == "1w5[4:1]"
            | none => false
          supportedCase "just_right_is_debug_not_normalized_authority"
            (hasDebugRaw &&
             lowered.debugSimai = some ("1w5[4:1]", "wifi", false))
            "just-right is no longer stored as normalized authority and is only exported as derived debug data"
      | _, _ => supportedCase "just_right_is_debug_not_normalized_authority" false "expected one wifi slide"
  | .error err => supportedCase "just_right_is_debug_not_normalized_authority" false s!"unexpected parse error: {err.message}"

def test_long_connected_queue_skip_rule : ParityCase :=
  match parseLevel1 "&inote_1=(120)1-3[4:1]-5[4:1]," with
  | .ok chart =>
      supportedCase "long_connected_queue_skip_rule"
        (chart.semantic.normalized.slides.length = 2 &&
         chart.semantic.normalized.slides.all (fun s =>
         s.totalJudgeQueueLen = 5 && s.judgeQueues.all (fun queue => queue.all (fun area => area.isSkippable))) &&
         chart.semantic.lowered.slides.all (fun s =>
         s.totalJudgeQueueLen = 5 && s.judgeQueues.all (fun queue => queue.all (fun area => area.isSkippable))))
  | .error err => supportedCase "long_connected_queue_skip_rule" false err.message

def test_break_truth_table : ParityCase :=
  let cases : List (String × Bool × Bool) :=
    [("1-3[4:1]", false, false), ("1b-3[4:1]", true, false),
     ("1-3b[4:1]", false, true), ("1b-3b[4:1]", true, true),
     ("1b>3[4:1]", true, false), ("1>3b[4:1]", false, true),
     ("1bw5[4:1]", true, false), ("1w5b[4:1]", false, true),
     ("1-3bx[4:1]", false, false), ("1-3xb[4:1]", false, true),
     ("1b-3[4:1]b", true, true), ("1b-3[4:1]>5b[4:1]", true, true),
     ("1b-3[4:1]>5[4:1]", true, false)]
  supportedCase "break_truth_table" (cases.all (fun (text, headBreak, bodyBreak) =>
    match parseLevel1 s!"&inote_1=(120){text}," with
    | .ok chart =>
        !chart.semantic.normalized.slides.isEmpty &&
        chart.semantic.normalized.slides.all (fun s =>
          s.isBreak == headBreak && s.isSlideBreak == bodyBreak) &&
        chart.semantic.lowered.slideHeads.all (fun s => s.isBreak == headBreak) &&
        chart.semantic.lowered.slides.all (fun s => s.isBreak == bodyBreak)
    | .error _ => false))

def test_simultaneous_branch_breaks_are_independent : ParityCase :=
  match parseLevel1 "&inote_1=(120)1b-3[4:1]*>5b[4:1]," with
  | .ok chart =>
      supportedCase "simultaneous_branch_breaks_are_independent"
        (match chart.semantic.normalized.slides with
         | [a, b] =>
             a.isBreak && !a.isSlideBreak && !b.isBreak && b.isSlideBreak &&
             a.hasHeadNote && !b.hasHeadNote && !a.isConnSlide && !b.isConnSlide &&
             a.startTiming = b.startTiming && b.parentNoteIndex.isNone
         | _ => false)
  | .error err => supportedCase "simultaneous_branch_breaks_are_independent" false err.message

def test_nested_branch_chains_have_distinct_groups : ParityCase :=
  match parseLevel1 "&inote_1=(120)1-3[4:1]>5[4:1]*-5[4:1]>7[4:1]/2-4-6[2:1]," with
  | .ok chart =>
      supportedCase "nested_branch_chains_have_distinct_groups"
        (match chart.semantic.normalized.slides with
         | [a, b, c, d, e, f] =>
             a.sourceGroupId != c.sourceGroupId && c.sourceGroupId != e.sourceGroupId &&
             a.sourceGroupId != e.sourceGroupId &&
             b.parentNoteIndex = some a.noteIndex && d.parentNoteIndex = some c.noteIndex &&
             f.parentNoteIndex = some e.noteIndex &&
             a.parentNoteIndex.isNone && c.parentNoteIndex.isNone && e.parentNoteIndex.isNone &&
             a.hasHeadNote && !c.hasHeadNote && e.hasHeadNote &&
             b.startTiming = a.startTiming + a.length &&
             d.startTiming = c.startTiming + c.length &&
             chart.semantic.lowered.slideHeads.length = 2
         | _ => false)
  | .error err => supportedCase "nested_branch_chains_have_distinct_groups" false err.message

def test_repeated_event_chains_keep_separate_queue_totals : ParityCase :=
  match parseLevel1 "&inote_1=(120)1-3-5[2:1],1-3>5[2:1]," with
  | .ok chart =>
      supportedCase "repeated_event_chains_keep_separate_queue_totals"
        (match chart.semantic.normalized.slides with
         | [a, b, c, d] =>
             a.totalJudgeQueueLen = 5 && b.totalJudgeQueueLen = 5 &&
             c.totalJudgeQueueLen = 9 && d.totalJudgeQueueLen = 9 &&
             c.parentNoteIndex.isNone && d.parentNoteIndex = some c.noteIndex &&
             a.headTiming != c.headTiming
         | _ => false)
  | .error err => supportedCase "repeated_event_chains_keep_separate_queue_totals" false err.message

def test_whitespace_and_backtick_shorthand : ParityCase :=
  match parseLevel1 "  &title=Whitespace\n  &first=0\n  &inote_1=(120)1 2`3\t4,\n" with
  | .ok chart =>
      supportedCase "whitespace_and_backtick_shorthand"
        (chart.inspection.metadata.fields.contains ("&title", "Whitespace") &&
         chart.semantic.normalized.taps.map (·.slot) = [.S1, .S2, .S3, .S4] &&
         chart.semantic.normalized.taps.map (·.timing.toMicros) = [0, 0, 15625, 15625])
  | .error err => supportedCase "whitespace_and_backtick_shorthand" false err.message

def test_unsupported_modifiers_are_explicit_errors : ParityCase :=
  supportedCase "unsupported_modifiers_are_explicit_errors"
    (["1m", "1c", "1@-3[4:1]", "1K3[4:1]"].all (fun text =>
      match parseLevel1 s!"&inote_1=(120){text}," with
      | .error err => err.kind = .invalidSyntax && err.rawText = text &&
          err.message.startsWith "unsupported"
      | .ok _ => false))

def test_malformed_connected_groups_rejected : ParityCase :=
  supportedCase "malformed_connected_groups_rejected"
    (["1-3[4:1]>5", "1-3[4:1]>5<7[4:1]", "1-3>5", "1-3[4:1]>5[4:1",
      "1w5>7[4:1]", "1-3w7[4:1]", "1-3[4:1]*1>5[4:1]"].all (fun text =>
      match parseLevel1 s!"&inote_1=(120){text}," with
      | .error err => err.kind = .invalidSyntax
      | .ok _ => false))

def test_custom_chain_wait_and_duration_forms : ParityCase :=
  supportedCase "custom_chain_wait_and_duration_forms"
    ([("1-3[0.25##4:1]", 250000, 500000),
      ("1-3[0.25##240#4:1]", 250000, 250000),
      ("1-3[4:1]>5[240#4:1]", 250000, 750000)].all
      (fun (text, wait, duration) =>
        match parseLevel1 s!"&inote_1=(120){text}," with
        | .ok chart =>
            (chart.inspection.tokens.head?.bind (·.starWait)).map Duration.toMicros = some wait &&
            (chart.semantic.normalized.slides.map (·.length.toMicros)).sum = duration
        | .error _ => false))

def test_chain_duration_rounds_total_once : ParityCase :=
  match parseLevel1 "&inote_1=(180)1-3[4:1]>5[4:1]," with
  | .ok chart => supportedCase "chain_duration_rounds_total_once"
      ((chart.semantic.normalized.slides.map (·.length.toMicros)).sum = 666667)
  | .error err => supportedCase "chain_duration_rounds_total_once" false err.message

def test_long_timeline_and_backticks_round_absolute_times : ParityCase :=
  let body := String.intercalate "," (List.replicate 300 "")
  match parseLevel1 s!"&inote_1=(180),{body},1``2`3," with
  | .ok chart => supportedCase "long_timeline_and_backticks_round_absolute_times"
      (chart.semantic.normalized.taps.map (·.timing.toMicros) =
        [100333333, 100343750, 100354167])
  | .error err => supportedCase "long_timeline_and_backticks_round_absolute_times" false err.message

def test_mid_segment_directive_resets_pending_note : ParityCase :=
  match parseLevel1 "&inote_1=(120)c,3\n{8}5x<1p4[8:12]/3x," with
  | .ok chart => supportedCase "mid_segment_directive_resets_pending_note"
      (chart.semantic.normalized.slides.length = 2 &&
       chart.semantic.normalized.slides.head?.map (·.slot) = some .S5 &&
       chart.semantic.normalized.taps.length = 1)
  | .error err => supportedCase "mid_segment_directive_resets_pending_note" false err.message

def majDataPlayRegressionCases : List ParityCase :=
  [test_long_connected_queue_skip_rule, test_break_truth_table,
   test_simultaneous_branch_breaks_are_independent, test_nested_branch_chains_have_distinct_groups,
   test_repeated_event_chains_keep_separate_queue_totals, test_whitespace_and_backtick_shorthand,
   test_unsupported_modifiers_are_explicit_errors, test_malformed_connected_groups_rejected,
   test_custom_chain_wait_and_duration_forms, test_chain_duration_rounds_total_once,
   test_long_timeline_and_backticks_round_absolute_times,
   test_mid_segment_directive_resets_pending_note]

theorem majDataPlayRegressionCases_pass : majDataPlayRegressionCases.all (·.passed) = true := by
  native_decide

def all : List ParityCase :=
  [ test_simai_chart_dsl_smoke
  , test_simai_slide_dsl_smoke
  , test_simai_chart_level_dsl_smoke
  , test_simai_normalized_chart_dsl_smoke
  , test_simai_lowered_slide_split_ir_dsl
  , test_metadata_parsing
  , test_empty_fumen
  , test_simple_tap_and_bpm
  , test_hold_note_basic_duration
  , test_hold_note_custom_bpm_duration
  , test_hold_note_absolute_time_duration
  , test_slide_note_duration_and_star_wait
  , test_slide_note_custom_bpm_star_and_duration
  , test_slide_note_absolute_star_wait_no_hash_and_duration
  , test_classic_slide_mode_lowers_reference_timing_and_queue
  , test_classic_slide_mode_selects_wifi_and_connected_timing
  , test_touch_note
  , test_slash_each_touch_allocates_touch_group
  , test_slash_each_touchhold_allocates_head_and_body_groups
  , test_touch_nohead_slide_each_suppression_matches_reference
  , test_touchhold_nohead_slide_still_allocates_each_group
  , test_modifiers
  , test_slide_modifiers
  , test_slide_break_on_segment
  , test_lowered_slide_break_split_uses_segment_break_for_body
  , test_simultaneous_notes_slash
  , test_pseudo_simultaneous_backtick
  , test_comment_handling
  , test_beat_signature_change
  , test_hspeed_change
  , test_unfit_bpm_quantizes_consistently
  , test_rational_inspection_json_is_stable
  , test_same_head_slide_group_lowering
  , test_lowered_ordinary_slide_splits_head_and_body
  , test_identical_simultaneous_slides_fold_body_multiplicity
  , test_identical_simultaneous_connected_slides_fold_group_multiplicity
  , test_connected_slide_multiplicity_requires_whole_group_match
  , test_lowered_headless_slide_has_body_only
  , test_lowered_conn_group_has_one_head_for_first_body
  , test_same_head_wifi_group_accepted
  , test_same_head_conn_child_start_inherits_parent_end
  , test_normalized_slide_topology_attached
  , test_normalized_short_conn_skip_rule
  , test_continuous_conn_three_part_parent_chain
  , test_same_head_with_tap_head_matches_python_flattening
  , test_same_head_subsequent_parts_are_headless
  , test_continuous_conn_qq_chain_matches_majdataplay
  , test_continuous_chain_timing_uses_prefab_bar_counts
  , test_normalized_topology_comes_from_typed_shape
  , test_current_slide_strings_parse_strictly
  , test_strict_line2_acceptance
  , test_strict_circle1_acceptance
  , test_reference_circle_mirror_semantics
  , test_reference_circle_realpaths
  , test_reference_other_shape_realpaths
  , test_reference_pq_realpaths
  , test_reference_ppqq_realpaths
  , test_reference_mirrored_pq_realpaths
  , test_reference_mirrored_ppqq_realpaths
  , test_reference_mirrored_turn_realpaths
  , test_shape_key_is_annotation_not_authority
  , test_reference_wifi_realpaths
  , test_reference_wifi_classic_center_path
  , test_reference_wifi_multi_area_tails
  , test_just_right_is_debug_not_normalized_authority ] ++ majDataPlayRegressionCases

def leanMirroredCaseNames : List String :=
  all.map (fun c => s!"test_{c.name}")

def supportedCount : Nat :=
  all.foldl (fun acc item => if item.supported then acc + 1 else acc) 0

def passedCount : Nat :=
  all.foldl (fun acc item => if item.supported && item.passed then acc + 1 else acc) 0

#eval! all
#eval (supportedCount, passedCount, all.length)

theorem classic_slide_mode_runtime_parity :
    test_classic_slide_mode_lowers_reference_timing_and_queue.passed = true ∧
    test_classic_slide_mode_selects_wifi_and_connected_timing.passed = true := by
  native_decide

theorem test_simai_chart_dsl_smoke_proof : test_simai_chart_dsl_smoke.passed = true := by native_decide
theorem test_simai_slide_dsl_smoke_proof : test_simai_slide_dsl_smoke.passed = true := by native_decide
theorem test_simai_chart_level_dsl_smoke_proof : test_simai_chart_level_dsl_smoke.passed = true := by native_decide
theorem test_simai_normalized_chart_dsl_smoke_proof : test_simai_normalized_chart_dsl_smoke.passed = true := by native_decide
theorem test_metadata_parsing_proof : test_metadata_parsing.passed = true := by native_decide
theorem test_empty_fumen_proof : test_empty_fumen.passed = true := by native_decide
theorem test_simple_tap_and_bpm_proof : test_simple_tap_and_bpm.passed = true := by native_decide
theorem test_hold_note_basic_duration_proof : test_hold_note_basic_duration.passed = true := by native_decide
theorem test_hold_note_custom_bpm_duration_proof : test_hold_note_custom_bpm_duration.passed = true := by native_decide
theorem test_hold_note_absolute_time_duration_proof : test_hold_note_absolute_time_duration.passed = true := by native_decide
theorem test_slide_note_duration_and_star_wait_proof : test_slide_note_duration_and_star_wait.passed = true := by native_decide
theorem test_slide_note_custom_bpm_star_and_duration_proof : test_slide_note_custom_bpm_star_and_duration.passed = true := by native_decide
theorem test_slide_note_absolute_star_wait_no_hash_and_duration_proof : test_slide_note_absolute_star_wait_no_hash_and_duration.passed = true := by native_decide
theorem test_touch_note_proof : test_touch_note.passed = true := by native_decide
theorem test_slash_each_touch_allocates_touch_group_proof :
    test_slash_each_touch_allocates_touch_group.passed = true := by native_decide
theorem test_slash_each_touchhold_allocates_head_and_body_groups_proof :
    test_slash_each_touchhold_allocates_head_and_body_groups.passed = true := by native_decide
theorem test_touch_nohead_slide_each_suppression_matches_reference_proof :
    test_touch_nohead_slide_each_suppression_matches_reference.passed = true := by native_decide
theorem test_touchhold_nohead_slide_still_allocates_each_group_proof :
    test_touchhold_nohead_slide_still_allocates_each_group.passed = true := by native_decide
theorem test_modifiers_proof : test_modifiers.passed = true := by native_decide
theorem test_slide_modifiers_proof : test_slide_modifiers.passed = true := by native_decide
theorem test_slide_break_on_segment_proof : test_slide_break_on_segment.passed = true := by native_decide
theorem test_lowered_slide_break_split_uses_segment_break_for_body_proof :
    test_lowered_slide_break_split_uses_segment_break_for_body.passed = true := by native_decide
theorem test_simultaneous_notes_slash_proof : test_simultaneous_notes_slash.passed = true := by native_decide
theorem test_pseudo_simultaneous_backtick_proof : test_pseudo_simultaneous_backtick.passed = true := by native_decide
theorem test_comment_handling_proof : test_comment_handling.passed = true := by native_decide
theorem test_beat_signature_change_proof : test_beat_signature_change.passed = true := by native_decide
theorem test_hspeed_change_proof : test_hspeed_change.passed = true := by native_decide
theorem test_unfit_bpm_quantizes_consistently_proof : test_unfit_bpm_quantizes_consistently.passed = true := by native_decide
theorem test_rational_inspection_json_is_stable_proof : test_rational_inspection_json_is_stable.passed = true := by native_decide
theorem test_same_head_slide_group_lowering_proof : test_same_head_slide_group_lowering.passed = true := by native_decide
theorem test_lowered_ordinary_slide_splits_head_and_body_proof : test_lowered_ordinary_slide_splits_head_and_body.passed = true := by native_decide
theorem test_identical_simultaneous_slides_fold_body_multiplicity_proof :
    test_identical_simultaneous_slides_fold_body_multiplicity.passed = true := by native_decide
theorem test_identical_simultaneous_connected_slides_fold_group_multiplicity_proof :
    test_identical_simultaneous_connected_slides_fold_group_multiplicity.passed = true := by
  native_decide
theorem test_connected_slide_multiplicity_requires_whole_group_match_proof :
    test_connected_slide_multiplicity_requires_whole_group_match.passed = true := by native_decide
theorem test_lowered_headless_slide_has_body_only_proof : test_lowered_headless_slide_has_body_only.passed = true := by native_decide
theorem test_lowered_conn_group_has_one_head_for_first_body_proof : test_lowered_conn_group_has_one_head_for_first_body.passed = true := by native_decide
theorem test_same_head_wifi_group_accepted_proof : test_same_head_wifi_group_accepted.passed = true := by native_decide
theorem test_same_head_conn_child_start_inherits_parent_end_proof : test_same_head_conn_child_start_inherits_parent_end.passed = true := by native_decide
theorem test_normalized_slide_topology_attached_proof : test_normalized_slide_topology_attached.passed = true := by native_decide
theorem test_normalized_short_conn_skip_rule_proof : test_normalized_short_conn_skip_rule.passed = true := by native_decide
theorem test_continuous_conn_three_part_parent_chain_proof : test_continuous_conn_three_part_parent_chain.passed = true := by native_decide
theorem test_same_head_with_tap_head_matches_python_flattening_proof : test_same_head_with_tap_head_matches_python_flattening.passed = true := by native_decide
theorem test_same_head_subsequent_parts_are_headless_proof : test_same_head_subsequent_parts_are_headless.passed = true := by native_decide
theorem test_continuous_conn_qq_chain_matches_majdataplay_proof : test_continuous_conn_qq_chain_matches_majdataplay.passed = true := by native_decide
theorem test_continuous_chain_timing_uses_prefab_bar_counts_proof :
    test_continuous_chain_timing_uses_prefab_bar_counts.passed = true := by native_decide
theorem test_normalized_topology_comes_from_typed_shape_proof : test_normalized_topology_comes_from_typed_shape.passed = true := by native_decide
theorem test_current_slide_strings_parse_strictly_proof : test_current_slide_strings_parse_strictly.passed = true := by native_decide
theorem test_strict_line2_acceptance_proof : test_strict_line2_acceptance.passed = true := by native_decide
theorem test_strict_circle1_acceptance_proof : test_strict_circle1_acceptance.passed = true := by native_decide
theorem test_reference_circle_mirror_semantics_proof : test_reference_circle_mirror_semantics.passed = true := by native_decide
theorem test_reference_circle_realpaths_proof : test_reference_circle_realpaths.passed = true := by native_decide
theorem test_reference_other_shape_realpaths_proof : test_reference_other_shape_realpaths.passed = true := by native_decide
theorem test_reference_pq_realpaths_proof : test_reference_pq_realpaths.passed = true := by native_decide
theorem test_reference_ppqq_realpaths_proof : test_reference_ppqq_realpaths.passed = true := by native_decide
theorem test_reference_mirrored_pq_realpaths_proof : test_reference_mirrored_pq_realpaths.passed = true := by native_decide
theorem test_reference_mirrored_ppqq_realpaths_proof : test_reference_mirrored_ppqq_realpaths.passed = true := by native_decide
theorem test_reference_mirrored_turn_realpaths_proof : test_reference_mirrored_turn_realpaths.passed = true := by native_decide
theorem test_shape_key_is_annotation_not_authority_proof : test_shape_key_is_annotation_not_authority.passed = true := by native_decide
theorem test_just_right_is_debug_not_normalized_authority_proof : test_just_right_is_debug_not_normalized_authority.passed = true := by native_decide

end LnmaiCore.Simai.Tests
