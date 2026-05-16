import LnmaiCore.Simai
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
             slide.judgeAt = some (TimePoint.fromMicros 1000000))
            "slide duration and star-wait both lower"
      | _ => supportedCase "slide_note_duration_and_star_wait" false "expected one slide"
  | .error err => supportedCase "slide_note_duration_and_star_wait" false s!"unexpected parse error: {err.message}"

def test_slide_note_custom_bpm_star_and_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1V[120#8:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_custom_bpm_star_and_duration"
            (slide.startTiming = TimePoint.fromMicros 500000 &&
             slide.length = Duration.fromMicros 250000 &&
             slide.judgeAt = some (TimePoint.fromMicros 750000))
            "custom-BPM slide star/duration timing works"
      | _ => supportedCase "slide_note_custom_bpm_star_and_duration" false "expected one slide"
  | .error err => supportedCase "slide_note_custom_bpm_star_and_duration" false s!"unexpected parse error: {err.message}"

def test_slide_note_absolute_star_wait_no_hash_and_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1<5[0.2##0.75],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_absolute_star_wait_no_hash_and_duration"
            (slide.startTiming = TimePoint.fromMicros 200000 &&
             slide.length = Duration.fromMicros 750000 &&
             slide.judgeAt = some (TimePoint.fromMicros 950000))
            "absolute star-wait and duration work"
      | _ => supportedCase "slide_note_absolute_star_wait_no_hash_and_duration" false "expected one slide"
  | .error err => supportedCase "slide_note_absolute_star_wait_no_hash_and_duration" false s!"unexpected parse error: {err.message}"

def test_touch_note : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\nA1/C,E4h[4:1],\n" with
  | .ok chart =>
      supportedCase "touch_note"
        (chart.semantic.normalized.touches.length = 2 && chart.semantic.normalized.touchHolds.length = 1)
        "touch and touch-hold tokenize and lower"
  | .error err => supportedCase "touch_note" false s!"unexpected parse error: {err.message}"

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

def test_slide_break_on_segment : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3b[4:1],\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides with
      | tok :: _, slide :: _ =>
          supportedCase "slide_break_on_segment"
            (!tok.isBreak && tok.isSlideBreak && !slide.isBreak && slide.isSlideBreak)
            "segment-local slide break is separate from head break"
      | _, _ => supportedCase "slide_break_on_segment" false "expected one slide token"
  | .error err => supportedCase "slide_break_on_segment" false s!"unexpected parse error: {err.message}"

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
             third.timing = TimePoint.fromMicros 666666)
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
          let expectedBpmJson := Lean.Json.mkObj [("num", Lean.toJson (180 : Int)), ("den", Lean.toJson (1 : Nat)), ("decimal", Lean.Json.str "180")]
          let expectedHSpeedJson := Lean.Json.mkObj [("num", Lean.toJson (5 : Int)), ("den", Lean.toJson (2 : Nat)), ("decimal", Lean.Json.str "2.5")]
          supportedCase "rational_inspection_json_is_stable"
            (bpmJson == expectedBpmJson && hSpeedJson == expectedHSpeedJson)
            "inspection rationals serialize as stable num/den/decimal objects"
      | _ => supportedCase "rational_inspection_json_is_stable" false "expected one token"
  | .error err => supportedCase "rational_inspection_json_is_stable" false s!"unexpected parse error: {err.message}"

def test_same_head_slide_group_lowering : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1],\n" with
  | .ok chart =>
      match chart.inspection.tokens, chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | tok1 :: tok2 :: _, slide1 :: slide2 :: _, lowered1 :: lowered2 :: _ =>
          supportedCase "same_head_slide_group_lowering"
            (chart.inspection.source.events.length = 1 &&
             tok1.sourceGroupSize = some 2 && tok1.sourceGroupIndex = some 0 &&
             tok2.sourceGroupSize = some 2 && tok2.sourceGroupIndex = some 1 && tok2.isSlideNoHead &&
             slide1.isConnSlide && slide2.isConnSlide &&
             slide1.isGroupHead && !slide1.isGroupEnd &&
             !slide2.isGroupHead && slide2.isGroupEnd &&
             slide1.slideKind = LnmaiCore.SlideKind.ConnPart && slide2.slideKind = LnmaiCore.SlideKind.ConnPart &&
             slide2.parentNoteIndex = some slide1.noteIndex &&
             lowered1.isConnSlide && lowered2.isConnSlide &&
             lowered2.parentNoteIndex = some lowered1.noteIndex)
            "same-head `*` groups lower to connected slides"
      | _, _, _ => supportedCase "same_head_slide_group_lowering" false "expected grouped slide tokens"
  | .error err => supportedCase "same_head_slide_group_lowering" false s!"unexpected parse error: {err.message}"

def test_same_head_wifi_group_rejected : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1]*-3[4:1],\n" with
  | .ok _ => supportedCase "same_head_wifi_group_rejected" false "expected wifi connection group to be rejected"
  | .error err =>
      supportedCase "same_head_wifi_group_rejected"
        (err.message.contains "wifi slide cannot be part of a connection slide group")
        "wifi connection groups are rejected during typed Simai validation, matching MajdataPlay"

def test_same_head_conn_child_start_inherits_parent_end : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1<5[4:1]*1>5[4:1],\n" with
  | .ok _ => supportedCase "same_head_conn_child_start_inherits_parent_end" false "expected malformed same-head group to be rejected"
  | .error err =>
      supportedCase "same_head_conn_child_start_inherits_parent_end"
        (err.message.contains "expected digit at 2")
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

def test_normalized_short_conn_skip_rule : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>2[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | first :: second :: _ =>
          supportedCase "normalized_short_conn_skip_rule"
            (first.totalJudgeQueueLen < 4 && second.totalJudgeQueueLen < 4 &&
             !first.judgeQueues.isEmpty && !second.judgeQueues.isEmpty)
            "short connected-slide topology is attached during normalization"
      | _ => supportedCase "normalized_short_conn_skip_rule" false "expected grouped slides"
  | .error err => supportedCase "normalized_short_conn_skip_rule" false s!"unexpected parse error: {err.message}"

def test_same_head_conn_three_part_parent_chain : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1]*<7[4:1],\n" with
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
             first.isConnSlide && second.isConnSlide &&
             first.isGroupHead && !first.isGroupEnd && first.parentNoteIndex = none &&
             !second.isGroupHead && second.isGroupEnd && second.parentNoteIndex = some first.noteIndex &&
             first.noteIndex < second.noteIndex)
            "head-only first `*` part stays a tap; grouping starts at the first actual slide"
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

def test_normalized_topology_comes_from_typed_shape : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1<5[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "normalized_topology_comes_from_typed_shape"
            (shapeKey slide.simaiShape = "-circle5" && slide.simaiShape.mirrored &&
             !slide.judgeQueues.isEmpty)
            "normalization derives topology from parser-produced shape semantics, not an unconstrained string"
      | _ => supportedCase "normalized_topology_comes_from_typed_shape" false "expected one slide"
  | .error err => supportedCase "normalized_topology_comes_from_typed_shape" false s!"unexpected parse error: {err.message}"

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
             turnPath = ["A1", "A2", "A3", "A4", "A5"] &&
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
          supportedCase "reference_mirrored_turn_realpaths"
            (!shortTurn.simaiShape.mirrored && !longTurn.simaiShape.mirrored &&
             shortPath = ["A1", "A8", "A7", "A7", "C", "B3", "A3"] &&
             longPath = ["A1", "A8", "A7", "A6", "A5"])
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

def all : List ParityCase :=
  [ test_metadata_parsing
  , test_empty_fumen
  , test_simple_tap_and_bpm
  , test_hold_note_basic_duration
  , test_hold_note_custom_bpm_duration
  , test_hold_note_absolute_time_duration
  , test_slide_note_duration_and_star_wait
  , test_slide_note_custom_bpm_star_and_duration
  , test_slide_note_absolute_star_wait_no_hash_and_duration
  , test_touch_note
  , test_modifiers
  , test_slide_modifiers
  , test_slide_break_on_segment
  , test_simultaneous_notes_slash
  , test_pseudo_simultaneous_backtick
  , test_comment_handling
  , test_beat_signature_change
  , test_hspeed_change
  , test_unfit_bpm_quantizes_consistently
  , test_rational_inspection_json_is_stable
  , test_same_head_slide_group_lowering
  , test_same_head_wifi_group_rejected
  , test_same_head_conn_child_start_inherits_parent_end
  , test_normalized_slide_topology_attached
  , test_normalized_short_conn_skip_rule
  , test_same_head_conn_three_part_parent_chain
  , test_same_head_with_tap_head_matches_python_flattening
  , test_same_head_subsequent_parts_are_headless
  , test_normalized_topology_comes_from_typed_shape
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
  , test_just_right_is_debug_not_normalized_authority ]

def leanMirroredCaseNames : List String :=
  all.map (fun c => s!"test_{c.name}")

def supportedCount : Nat :=
  all.foldl (fun acc item => if item.supported then acc + 1 else acc) 0

def passedCount : Nat :=
  all.foldl (fun acc item => if item.supported && item.passed then acc + 1 else acc) 0

#eval! all
#eval (supportedCount, passedCount, all.length)

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
theorem test_modifiers_proof : test_modifiers.passed = true := by native_decide
theorem test_slide_modifiers_proof : test_slide_modifiers.passed = true := by native_decide
theorem test_slide_break_on_segment_proof : test_slide_break_on_segment.passed = true := by native_decide
theorem test_simultaneous_notes_slash_proof : test_simultaneous_notes_slash.passed = true := by native_decide
theorem test_pseudo_simultaneous_backtick_proof : test_pseudo_simultaneous_backtick.passed = true := by native_decide
theorem test_comment_handling_proof : test_comment_handling.passed = true := by native_decide
theorem test_beat_signature_change_proof : test_beat_signature_change.passed = true := by native_decide
theorem test_hspeed_change_proof : test_hspeed_change.passed = true := by native_decide
theorem test_unfit_bpm_quantizes_consistently_proof : test_unfit_bpm_quantizes_consistently.passed = true := by native_decide
theorem test_rational_inspection_json_is_stable_proof : test_rational_inspection_json_is_stable.passed = true := by native_decide
theorem test_same_head_slide_group_lowering_proof : test_same_head_slide_group_lowering.passed = true := by native_decide
theorem test_same_head_wifi_group_rejected_proof : test_same_head_wifi_group_rejected.passed = true := by native_decide
theorem test_same_head_conn_child_start_inherits_parent_end_proof : test_same_head_conn_child_start_inherits_parent_end.passed = true := by native_decide
theorem test_normalized_slide_topology_attached_proof : test_normalized_slide_topology_attached.passed = true := by native_decide
theorem test_normalized_short_conn_skip_rule_proof : test_normalized_short_conn_skip_rule.passed = true := by native_decide
theorem test_same_head_conn_three_part_parent_chain_proof : test_same_head_conn_three_part_parent_chain.passed = true := by native_decide
theorem test_same_head_with_tap_head_matches_python_flattening_proof : test_same_head_with_tap_head_matches_python_flattening.passed = true := by native_decide
theorem test_same_head_subsequent_parts_are_headless_proof : test_same_head_subsequent_parts_are_headless.passed = true := by native_decide
theorem test_normalized_topology_comes_from_typed_shape_proof : test_normalized_topology_comes_from_typed_shape.passed = true := by native_decide
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
