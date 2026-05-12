import LnmaiCore.Simai

namespace LnmaiCore.Simai.Tests

/-- Lean mirror of `../reference/PySimaiParser/tests/test_core.py`. -/

structure ParityCase where
  name : String
  supported : Bool
  passed : Bool
  note : String
deriving Repr

private def floatEq (a b : Float) : Bool :=
  Float.abs (a - b) < 0.0001

private def parseLevel1 (content : String) : Except ParseError FrontendChartResult :=
  parseFrontendChartResult content 1

private def supportedCase (name : String) (passed : Bool) (note : String := "") : ParityCase :=
  { name := name, supported := true, passed := passed, note := note }

private def gapCase (name : String) (note : String) : ParityCase :=
  { name := name, supported := false, passed := false, note := note }

def test_simai_chart_dsl_smoke : ParityCase :=
  let chart : FrontendChartResult := simai_chart! "&first=0\n&inote_1=\n(120)\n1,\n"
  supportedCase "simai_chart_dsl_smoke"
    (chart.semantic.normalized.taps.length = 1)
    "chart DSL elaborates through frontend parser"

def test_simai_slide_dsl_smoke : ParityCase :=
  let slide : SlideNoteSemantics := simai_slide! "1V35"
  let normalized : NormalizedSlide := simai_normalized_slide! "1w5[4:1]"
  supportedCase "simai_slide_dsl_smoke"
    (slide.startPos = 1 && slide.endPos = 5 &&
     slide.shape.kind = SlideKind.turn &&
     normalized.trackCount = 3 && normalized.slideKind = LnmaiCore.SlideKind.Wifi)
    "slide DSL elaborates through shared parser/normalizer"

def test_simai_chart_level_dsl_smoke : ParityCase :=
  let chart : FrontendChartResult :=
    simai_chart_at! 2 "&first=0\n&inote_1=\n(120)\n1,\n&inote_2=\n(120)\n2,\n"
  supportedCase "simai_chart_level_dsl_smoke"
    (match chart.semantic.normalized.taps with
     | tap :: _ => tap.lane = 1
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
            (first.lane = 0 && second.lane = 1 &&
             floatEq first.timingSec 0.5 && floatEq second.timingSec 1.0)
            "&first offset and BPM step apply"
      | _ => supportedCase "simple_tap_and_bpm" false "expected two taps"
  | .error err => supportedCase "simple_tap_and_bpm" false s!"unexpected parse error: {err.message}"

def test_hold_note_basic_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1h[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.holds with
      | hold :: _ => supportedCase "hold_note_basic_duration" (hold.lane = 0 && floatEq hold.lengthSec 1.0) "generic BPM parsing works"
      | _ => supportedCase "hold_note_basic_duration" false "expected one hold"
  | .error err => supportedCase "hold_note_basic_duration" false s!"unexpected parse error: {err.message}"

def test_hold_note_custom_bpm_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1h[120#4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.holds with
      | hold :: _ => supportedCase "hold_note_custom_bpm_duration" (floatEq hold.lengthSec 0.5) "custom-BPM duration works"
      | _ => supportedCase "hold_note_custom_bpm_duration" false "expected one hold"
  | .error err => supportedCase "hold_note_custom_bpm_duration" false s!"unexpected parse error: {err.message}"

def test_hold_note_absolute_time_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1h[#2.5],\n" with
  | .ok chart =>
      match chart.semantic.normalized.holds with
      | hold :: _ => supportedCase "hold_note_absolute_time_duration" (floatEq hold.lengthSec 2.5) "absolute duration works"
      | _ => supportedCase "hold_note_absolute_time_duration" false "expected one hold"
  | .error err => supportedCase "hold_note_absolute_time_duration" false s!"unexpected parse error: {err.message}"

def test_slide_note_duration_and_star_wait : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-4[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_duration_and_star_wait"
            (floatEq slide.lengthSec 0.5 && floatEq slide.startTimingSec 0.5 && slide.judgeAtSec == some 1.0)
            "slide duration and star-wait both lower"
      | _ => supportedCase "slide_note_duration_and_star_wait" false "expected one slide"
  | .error err => supportedCase "slide_note_duration_and_star_wait" false s!"unexpected parse error: {err.message}"

def test_slide_note_custom_bpm_star_and_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1V[120#8:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_custom_bpm_star_and_duration"
            (floatEq slide.startTimingSec 0.5 && floatEq slide.lengthSec 0.25 && slide.judgeAtSec == some 0.75)
            "custom-BPM slide star/duration timing works"
      | _ => supportedCase "slide_note_custom_bpm_star_and_duration" false "expected one slide"
  | .error err => supportedCase "slide_note_custom_bpm_star_and_duration" false s!"unexpected parse error: {err.message}"

def test_slide_note_absolute_star_wait_no_hash_and_duration : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(100)\n1<[0.2##0.75],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | slide :: _ =>
          supportedCase "slide_note_absolute_star_wait_no_hash_and_duration"
            (floatEq slide.startTimingSec 0.2 && floatEq slide.lengthSec 0.75 && slide.judgeAtSec == some 0.95)
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
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1-b[4:1],\n" with
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
            (floatEq tap.timingSec 1.0 && floatEq hold.timingSec 1.03125 && floatEq touch.timingSec 1.0625)
            "backtick pseudo-simultaneous timing works"
      | _, _, _ => supportedCase "pseudo_simultaneous_backtick" false "expected tap/hold/touch sequence"
  | .error err => supportedCase "pseudo_simultaneous_backtick" false s!"unexpected parse error: {err.message}"

def test_comment_handling : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120) || BPM set\n1, || Note 1\n|| Standalone comment\n2, || Note 2\n" with
  | .ok chart =>
      supportedCase "comment_handling"
        (chart.semantic.normalized.taps.length = 2 &&
         match chart.semantic.normalized.taps with
         | first :: second :: _ => floatEq first.timingSec 0.0 && floatEq second.timingSec 0.5
         | _ => false)
        "line comments are stripped"
  | .error err => supportedCase "comment_handling" false s!"unexpected parse error: {err.message}"

def test_beat_signature_change : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n1,\n{8}\n2,\n{2}\n3,\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps with
      | first :: second :: third :: _ =>
          supportedCase "beat_signature_change"
            (floatEq first.timingSec 0.0 && floatEq second.timingSec 1.0 && floatEq third.timingSec 1.5)
            "beat/divisor changes work"
      | _ => supportedCase "beat_signature_change" false "expected three taps"
  | .error err => supportedCase "beat_signature_change" false s!"unexpected parse error: {err.message}"

def test_hspeed_change : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(60)\n<H2.5>\n1,\n<HS*0.5>\n2,\n" with
  | .ok chart =>
      match chart.inspection.tokens with
      | first :: second :: _ =>
          supportedCase "hspeed_change"
            (floatEq first.hSpeed 2.5 && floatEq second.hSpeed 0.5)
            "hspeed directives parse"
      | _ => supportedCase "hspeed_change" false "expected two tap tokens"
  | .error err => supportedCase "hspeed_change" false s!"unexpected parse error: {err.message}"

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

def test_same_head_wifi_group_lowering : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1w5[4:1]*-3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.slides with
      | first :: second :: _ =>
          supportedCase "same_head_wifi_group_lowering"
            (first.trackCount = 3 && first.isConnSlide && first.isGroupHead &&
             first.slideKind = LnmaiCore.SlideKind.Wifi &&
             second.slideKind = LnmaiCore.SlideKind.ConnPart &&
             second.parentNoteIndex = some first.noteIndex)
            "wifi heads keep wifi kind while grouped children become conn parts"
      | _ => supportedCase "same_head_wifi_group_lowering" false "expected two lowered slides"
  | .error err => supportedCase "same_head_wifi_group_lowering" false s!"unexpected parse error: {err.message}"

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

def test_same_head_with_tap_head_matches_python_flattening : ParityCase :=
  match parseLevel1 "&first=0\n&inote_1=\n(120)\n1*>2[4:1]*-3[4:1],\n" with
  | .ok chart =>
      match chart.semantic.normalized.taps, chart.semantic.normalized.slides with
      | tap :: _, first :: second :: _ =>
          supportedCase "same_head_with_tap_head_matches_python_flattening"
            (tap.lane = 0 &&
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
            (shapeKey slide.simaiShape = "-circle4" && slide.simaiShape.mirrored &&
             !slide.judgeQueues.isEmpty)
            "normalization derives topology from parser-produced shape semantics, not an unconstrained string"
      | _ => supportedCase "normalized_topology_comes_from_typed_shape" false "expected one slide"
  | .error err => supportedCase "normalized_topology_comes_from_typed_shape" false s!"unexpected parse error: {err.message}"

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
  , test_same_head_slide_group_lowering
  , test_same_head_wifi_group_lowering
  , test_normalized_slide_topology_attached
  , test_normalized_short_conn_skip_rule
  , test_same_head_with_tap_head_matches_python_flattening
  , test_same_head_subsequent_parts_are_headless
  , test_normalized_topology_comes_from_typed_shape
  , test_shape_key_is_annotation_not_authority
  , test_just_right_is_debug_not_normalized_authority ]

def leanMirroredCaseNames : List String :=
  all.map (fun c => s!"test_{c.name}")

def supportedCount : Nat :=
  all.foldl (fun acc item => if item.supported then acc + 1 else acc) 0

def passedCount : Nat :=
  all.foldl (fun acc item => if item.supported && item.passed then acc + 1 else acc) 0

#eval all
#eval (supportedCount, passedCount, all.length)

end LnmaiCore.Simai.Tests
