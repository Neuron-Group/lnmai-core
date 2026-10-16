import Verification.SlideParserFrontend
import Verification.SlideParserComposition

/-!
Proofs for the runtime-used slide_parser_text kernels: all thirteen command
observations and the sanitizer's actual Vec/index loop. The loop proof includes
termination, safe indexing/pushes, output length and character order.

UTF-8 decoding, String encoding/allocation, runtime record interpretation, and
the read/just diagnostic adapters remain explicit boundaries. There is no
whole-command-facts or whole-sanitizer equivalence premise. In particular,
filtering happens before scans of sanitized text: it can create pp/qq pairs.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.SlideParserProof
open Verification.SlideParserFrontendProof (doubleScan)

set_option maxHeartbeats 1000000

namespace Verification.SlideParserTextProof

open private sanitizeSlideText from LnmaiCore.Simai.SlideParser

/-- Character interpretation of all thirteen observations, with no restriction
on malformed or conflicting commands. Dispatch priority is proved separately. -/
def commandFactsForChars (chars : List Char) : slide_parser.CommandFacts :=
  { line := decide ('-' ∈ chars)
    circle_right := decide ('>' ∈ chars)
    circle_left := decide ('<' ∈ chars)
    circle_up := decide ('^' ∈ chars)
    v := decide ('v' ∈ chars)
    pp := doubleScan 'p' false chars
    qq := doubleScan 'q' false chars
    p := decide ('p' ∈ chars)
    q := decide ('q' ∈ chars)
    s := decide ('s' ∈ chars)
    z := decide ('z' ∈ chars)
    turn := decide ('V' ∈ chars)
    wifi := decide ('w' ∈ chars) }

theorem command_facts_chars_equiv (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_text.command_facts chars = ok (commandFactsForChars chars.val) := by
  simp [slide_parser_text.command_facts,
    SlideParserFrontendProof.contains_char_equiv chars _ hsize,
    SlideParserFrontendProof.contains_double_equiv chars _ hsize,
    commandFactsForChars]

theorem command_facts_reference (text : String) :
    commandFactsForChars text.toList = SlideParserProof.factsForText text := by
  simp only [commandFactsForChars, SlideParserProof.factsForText,
    String.contains_char_eq]
  rw [SlideParserFrontendProof.double_scan_string_equiv text 'p',
    SlideParserFrontendProof.double_scan_string_equiv text 'q']

/-- The actual extracted observations, not a premise about a native contains. -/
theorem command_facts_equiv (chars : Slice Char) (text : String)
    (hchars : chars.val = text.toList) (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_text.command_facts chars = ok (SlideParserProof.factsForText text) := by
  rw [command_facts_chars_equiv chars hsize, hchars, command_facts_reference]

/-- The output buffer followed by the filter of the unprocessed suffix. -/
def filterResume (remaining kept : List Char) : List Char :=
  kept ++ remaining.filter keepSlideChar

theorem filter_resume_nil (kept : List Char) : filterResume [] kept = kept := by
  simp [filterResume]

theorem filter_resume_cons (c : Char) (remaining kept : List Char) :
    filterResume (c :: remaining) kept =
      if keepSlideChar c then filterResume remaining (kept ++ [c])
      else filterResume remaining kept := by
  cases hk : keepSlideChar c <;> simp [filterResume, hk, List.append_assoc]

/-- Every loop iteration preserves the filtered result and keeps the output
length below the advancing input index. No extra final push is needed, so the
input bound may be non-strict, unlike the tokenizer's final-piece buffer. -/
theorem sanitize_slide_chars_loop_spec (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_text.sanitize_slide_chars_loop chars (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.val.length ≤ chars.val.length ∧
        out.val = chars.val.filter keepSlideChar ⦄ := by
  unfold slide_parser_text.sanitize_slide_chars_loop
  apply loop.spec_decr_nat (fun p => chars.val.length - p.2.val)
    (fun p => p.2.val ≤ chars.val.length ∧
      p.1.val.length ≤ p.2.val ∧
      filterResume (chars.val.drop p.2.val) p.1.val = chars.val.filter keepSlideChar)
  · rintro ⟨kept, index⟩ ⟨hi, hkept, hinv⟩
    dsimp only at *
    unfold slide_parser_text.sanitize_slide_chars_loop.body
    dsimp only
    split
    · rename_i hmore
      have hindex : index.val < chars.val.length := by simpa using hmore
      step as ⟨c, hc⟩
      rw [List.drop_eq_getElem_cons hindex, ← hc, filter_resume_cons] at hinv
      rw [keep_slide_char_equiv]
      simp only [bind_tc_ok]
      split
      · rename_i hkeep
        simp [hkeep] at hinv
        step as ⟨kept', hkept'⟩
        step as ⟨next, hnext⟩
        simp_all <;> scalar_tac
      · rename_i hskip
        simp [hskip] at hinv
        simp only [bind_tc_ok]
        step as ⟨next, hnext⟩
        simp_all <;> scalar_tac
    · rename_i hdone
      have hempty : chars.val.drop index.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hdone)
      simp_all [filter_resume_nil] <;> scalar_tac
  · simp [filterResume]

theorem sanitize_slide_chars_spec (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_text.sanitize_slide_chars chars
      ⦃ out => out.val.length ≤ chars.val.length ∧
        out.val = chars.val.filter keepSlideChar ⦄ := by
  simpa only [slide_parser_text.sanitize_slide_chars] using
    sanitize_slide_chars_loop_spec chars hsize

theorem sanitize_slide_chars_equiv (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) :
    (do let out ← slide_parser_text.sanitize_slide_chars chars
        ok out.val) = ok (chars.val.filter keepSlideChar) := by
  obtain ⟨out, hout, _, hval⟩ := spec_imp_exists (sanitize_slide_chars_spec chars hsize)
  simp only [hout, bind_tc_ok, hval]

/-- encode interprets only Vec<char>.into_iter().collect::<String>(). It does
not perform filtering. The preceding Rust chars().collect() supplies chars. -/
def sanitizeRuntime (encode : List Char → String) (chars : Slice Char) : Result String := do
  let out ← slide_parser_text.sanitize_slide_chars chars
  ok (encode out.val)

/-- The bound on the primitive encoding law is inherited from the actual loop.
No equality between an opaque sanitizer and sanitizeSlideText is assumed. -/
theorem sanitize_runtime_equiv (encode : List Char → String)
    (chars : Slice Char) (raw : String)
    (hchars : chars.val = raw.toList) (hsize : chars.val.length ≤ Usize.max)
    (hencode : ∀ kept : List Char, kept.length ≤ chars.val.length →
      encode kept = String.ofList kept) :
    sanitizeRuntime encode chars = ok (sanitizeSlideText raw) := by
  obtain ⟨out, hout, hlen, hval⟩ := spec_imp_exists (sanitize_slide_chars_spec chars hsize)
  unfold sanitizeRuntime
  rw [hout]
  simp only [bind_tc_ok]
  rw [hencode out.val hlen, hval, hchars,
    sanitize_filter_equiv raw keepSlideChar keep_slide_char_equiv]

/-- Install the character interpretation of the command scans and the existing
reader/just kernels. Decoding and the base solver field remain unchanged; the
composition below invokes the extracted solveBody instead of the base solver. -/
def installText (base : Frontend) : Frontend :=
  SlideParserFrontendProof.installReadsAndJust
    { base with facts := fun text => commandFactsForChars (base.chars text) }

theorem installed_facts_runtime (base : Frontend) (chars : Slice Char) (text : String)
    (hchars : chars.val = base.chars text) (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_text.command_facts chars = ok ((installText base).facts text) := by
  simpa only [installText, SlideParserFrontendProof.installReadsAndJust, hchars] using
    command_facts_chars_equiv chars hsize

theorem installed_facts_reference (base : Frontend) (text : String)
    (hchars : base.chars text = text.toList) :
    (installText base).facts text = SlideParserProof.factsForText text := by
  change commandFactsForChars (base.chars text) = SlideParserProof.factsForText text
  rw [hchars, command_facts_reference]

/-- Only the per-input character representation remains. The actual runtime
facts equation is installed_facts_runtime; the read/just runtime and diagnostic
equations are supplied by SlideParserFrontend for this same updated base. -/
theorem installed_frontend_laws (base : Frontend) (text : String)
    (hchars : base.chars text = text.toList) :
    FrontendLaws (installText base) text := by
  unfold installText
  apply SlideParserFrontendProof.installed_frontend_laws
  · exact hchars
  · change commandFactsForChars (base.chars text) = SlideParserProof.factsForText text
    rw [hchars, command_facts_reference]

theorem installed_just_equiv (base : Frontend) (text : String)
    (hchars : base.chars text = text.toList) :
    (installText base).just text = detectJustType text := by
  unfold installText
  exact SlideParserFrontendProof.installed_just_equiv _ text hchars

theorem parse_body_equiv (base : Frontend) (text : String)
    (hchars : base.chars text = text.toList) :
    parseBodyRuntime (installText base) text = ok (parseSlideBodyFromText text) :=
  SlideParserProof.parse_body_equiv _ text (installed_frontend_laws base text hchars)

/-- The runtime terminal entry now uses the same scanned CommandFacts. It still
selects its endpoint independently of the body parser's command priority. -/
theorem parse_terminal_end_area_equiv (base : Frontend) (text : String)
    (hchars : base.chars text = text.toList) :
    parseTerminalRuntime (installText base) text = ok (parseTerminalEndArea text) :=
  SlideParserProof.parse_terminal_end_area_equiv _ text
    (installed_frontend_laws base text hchars)

def parseNoteFromChars (base : Frontend) (encode : List Char → String)
    (chars : Slice Char) (start : OuterSlot) (endArea : SensorArea) :
    Result (Except ParseError SlideNoteSemantics) := do
  let sanitized ← sanitizeRuntime encode chars
  SlideParserComposition.parseNote (installText base) sanitized start endArea

/-- Decode raw once for filtering, encode the filtered buffer, and decode the
produced String for the parser. hdecode is this final primitive decoding law;
it is not a whole-sanitizer, command-facts, reader, just, or solver law. -/
theorem parse_note_from_chars_equiv (base : Frontend) (encode : List Char → String)
    (chars : Slice Char) (raw : String) (start : OuterSlot) (endArea : SensorArea)
    (hchars : chars.val = raw.toList) (hsize : chars.val.length ≤ Usize.max)
    (hencode : ∀ kept : List Char, kept.length ≤ chars.val.length →
      encode kept = String.ofList kept)
    (hdecode : base.chars (sanitizeSlideText raw) = (sanitizeSlideText raw).toList) :
    parseNoteFromChars base encode chars start endArea =
      ok (parseSlideNote raw start endArea) := by
  unfold parseNoteFromChars
  rw [sanitize_runtime_equiv encode chars raw hchars hsize hencode]
  simp only [bind_tc_ok]
  exact SlideParserComposition.parse_note_equiv (installText base) raw
    (sanitizeSlideText raw) start endArea rfl
    (installed_frontend_laws base _ hdecode) (installed_just_equiv base _ hdecode)

def parseShapeFromChars (base : Frontend) (encode : List Char → String)
    (chars : Slice Char) : Result (Except ParseError SlideShape) := do
  let sanitized ← sanitizeRuntime encode chars
  SlideParserComposition.shapeFromText (installText base) sanitized

theorem parse_shape_from_chars_equiv (base : Frontend) (encode : List Char → String)
    (chars : Slice Char) (raw : String)
    (hchars : chars.val = raw.toList) (hsize : chars.val.length ≤ Usize.max)
    (hencode : ∀ kept : List Char, kept.length ≤ chars.val.length →
      encode kept = String.ofList kept)
    (hdecode : base.chars (sanitizeSlideText raw) = (sanitizeSlideText raw).toList) :
    parseShapeFromChars base encode chars = ok (parseSlideShapeText raw) := by
  unfold parseShapeFromChars
  rw [sanitize_runtime_equiv encode chars raw hchars hsize hencode]
  simp only [bind_tc_ok]
  exact SlideParserComposition.parse_shape_text_equiv (installText base) raw
    (sanitizeSlideText raw) rfl (installed_frontend_laws base _ hdecode)

def parseJustFromChars (base : Frontend) (encode : List Char → String)
    (chars : Slice Char) : Result (Except ParseError Bool) := do
  let sanitized ← sanitizeRuntime encode chars
  ok ((installText base).just sanitized)

theorem parse_just_from_chars_equiv (base : Frontend) (encode : List Char → String)
    (chars : Slice Char) (raw : String)
    (hchars : chars.val = raw.toList) (hsize : chars.val.length ≤ Usize.max)
    (hencode : ∀ kept : List Char, kept.length ≤ chars.val.length →
      encode kept = String.ofList kept)
    (hdecode : base.chars (sanitizeSlideText raw) = (sanitizeSlideText raw).toList) :
    parseJustFromChars base encode chars = ok (parseSlideJustText raw) := by
  unfold parseJustFromChars
  rw [sanitize_runtime_equiv encode chars raw hchars hsize hencode]
  simp only [bind_tc_ok]
  rw [installed_just_equiv base _ hdecode] <;> rfl

end Verification.SlideParserTextProof
