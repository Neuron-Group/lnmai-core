import Verification.Shape
import LnmaiCore.Simai.SlideParser
import LnmaiCore.Simai.SlideTables
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 2000000

/-!
Equivalence proofs for the extracted slide command and scalar-table kernels.

The command theorems cover arbitrary strings, including conflicting marks and
syntax errors. The scalar theorems cover every classified key, unknown keys,
and both timing modes. They do not assume that input is valid Simai.

The runtime's UTF-8/String operations, bounded collections, digit/error
adapters, shape solver, and just-type parser are not extracted by this slice.
Their required equalities appear as explicit hypotheses below. In particular,
the shape-solver hypothesis is not a proof of the existing solve_shape kernel.
The timing-point Vec loop and judge-queue payload/transform tables also remain
frontend obligations. `SlideParserComposition` discharges the solver premise
using the extracted solver; separate frontend proofs address character reads
and just-type parsing. See SOURCE_AUDIT.md for checked versions and boundaries.
-/

namespace Verification.SlideParserProof
open LnmaiCore LnmaiCore.Simai Verification.ShapeProof
open private sanitizeSlideText from LnmaiCore.Simai.SlideParser
open private readStartLaneAt readEndAreaAt from LnmaiCore.Simai.Shape

def keepSlideChar (c : Char) : Bool :=
  c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$'

theorem keep_slide_char_equiv (c : Char) :
    slide_parser.keep_slide_char c = ok (keepSlideChar c) := by
  simp [slide_parser.keep_slide_char, keepSlideChar] <;> split_ifs <;> simp_all
  by_cases h : c = '$' <;> simp_all

/-- Once the frontend's chars/filter/collect is represented by List.filter and
String.ofList, the extracted predicate gives the original sanitizer. This does
not assert that the unextracted Rust iterator or allocation has been verified. -/
theorem sanitize_filter_equiv (raw : String) (keep : Char → Bool)
    (hkeep : ∀ c, slide_parser.keep_slide_char c = ok (keep c)) :
    String.ofList (raw.toList.filter keep) = sanitizeSlideText raw := by
  have hp : keep = keepSlideChar := by
    funext c
    have h : (ok (keep c) : Result Bool) = ok (keepSlideChar c) :=
      (hkeep c).symm.trans (keep_slide_char_equiv c)
    simpa using h
  subst keep
  rfl

def factsForText (text : String) : slide_parser.CommandFacts :=
  { line := text.contains '-'
    circle_right := text.contains '>'
    circle_left := text.contains '<'
    circle_up := text.contains '^'
    v := text.contains 'v'
    pp := text.contains "pp"
    qq := text.contains "qq"
    p := text.contains 'p'
    q := text.contains 'q'
    s := text.contains 's'
    z := text.contains 'z'
    turn := text.contains 'V'
    wifi := text.contains 'w' }

/-- This records dispatch priority only; it imposes no consistency constraint
on the booleans (e.g. pp need not imply p for this kernel theorem). -/
def commandForFacts (f : slide_parser.CommandFacts) : Option SlideBodyKind :=
  if f.line then some .line
  else if f.circle_right then some .circleRight
  else if f.circle_left then some .circleLeft
  else if f.circle_up then some .circleUp
  else if f.v then some .v
  else if f.pp then some .pp
  else if f.qq then some .qq
  else if f.p then some .p
  else if f.q then some .q
  else if f.s then some .s
  else if f.z then some .z
  else if f.turn then some .turn
  else if f.wifi then some .wifi
  else none

theorem select_command_equiv (facts : slide_parser.CommandFacts) :
    (do let kind ← slide_parser.select_command facts
        ok (kind.map toLnmBodyKind)) = ok (commandForFacts facts) := by
  unfold slide_parser.select_command commandForFacts
  by_cases h : facts.line = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.circle_right = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.circle_left = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.circle_up = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.v = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.pp = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.qq = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.p = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.q = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.s = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.z = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.turn = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  by_cases h : facts.wifi = true
  · rw [if_pos h, if_pos h] <;> rfl
  rw [if_neg h, if_neg h]
  all_goals rfl

def endIndex : SlideBodyKind → Nat
  | .pp | .qq | .turn => 3
  | _ => 2

def turnIndex : SlideBodyKind → Option Nat
  | .turn => some 2
  | _ => none

theorem command_end_index_equiv (kind : shape.SlideBodyKind) :
    (do let index ← slide_parser.command_end_index kind; ok index.val) =
      ok (endIndex (toLnmBodyKind kind)) := by
  cases kind <;> rfl

theorem command_turn_index_equiv (kind : shape.SlideBodyKind) :
    (do let index ← slide_parser.command_turn_index kind
        ok (index.map (fun n => n.val))) =
      ok (turnIndex (toLnmBodyKind kind)) := by
  cases kind <;> rfl

theorem terminal_end_index_equiv (hasTurn hasPP hasQQ : Bool) :
    (do let index ← slide_parser.terminal_end_index hasTurn hasPP hasQQ
        ok index.val) =
      ok (if hasTurn then 3 else if hasPP || hasQQ then 3 else 2 : Nat) := by
  cases hasTurn <;> cases hasPP <;> cases hasQQ <;> rfl

/-- Interpret the runtime adapters in Lean types. Their implementations and
representation relations are outside this extracted control-flow slice. -/
structure Frontend where
  chars : String → List Char
  facts : String → slide_parser.CommandFacts
  readStart : List Char → Nat → Except ParseError OuterSlot
  readEnd : List Char → Nat → Except ParseError SensorArea
  solve : ParsedSlideBody → Except ParseError SlideShape
  just : String → Except ParseError Bool

/-- Obligations for this input only, including error kind, text, message, and
span. The bounded Rust frontend must establish its String/Vec representation
and allocation bounds before discharging these equations. No equality between
an actual bounded frontend and an unbounded function on every input is assumed.
Shape solving and just-type parsing have separate, call-specific hypotheses. -/
structure FrontendLaws (f : Frontend) (text : String) : Prop where
  chars_eq : f.chars text = text.toList
  facts_eq : f.facts text = factsForText text
  readStart_eq : f.readStart text.toList 0 = parseStartLaneAt text.toList 0
  readEnd2_eq : f.readEnd text.toList 2 = parseEndAreaAt text.toList 2
  readEnd3_eq : f.readEnd text.toList 3 = parseEndAreaAt text.toList 3

/-- The runtime adapter, retaining the order of fallible operations: start
lane, command selection, optional turn, then endpoint. -/
def parseBodyRuntime (f : Frontend) (text : String) :
    Result (Except ParseError ParsedSlideBody) := do
  match f.readStart (f.chars text) 0 with
  | .error err => ok (.error err)
  | .ok start =>
      let command ← slide_parser.select_command (f.facts text)
      match command with
      | none => ok (.error
          { kind := .invalidShape, rawText := text,
            message := "unrecognized Simai slide shape" })
      | some kind =>
          let turnAt ← slide_parser.command_turn_index kind
          let turn : Except ParseError (Option SensorArea) := do
            match turnAt with
            | none => pure none
            | some index =>
                let area ← f.readEnd (f.chars text) index.val
                pure (some area)
          match turn with
          | .error err => ok (.error err)
          | .ok turnArea =>
              let endAt ← slide_parser.command_end_index kind
              ok (do
                let endArea ← f.readEnd (f.chars text) endAt.val
                pure
                  { rawText := text
                    startLane := start
                    kind := toLnmBodyKind kind
                    endArea := some endArea
                    turnArea := turnArea })

/-- Full-domain refinement to the original parser, not just a restatement of
the finite dispatch table. It also compares all failing parses. -/
theorem parse_body_equiv (f : Frontend) (text : String) (laws : FrontendLaws f text) :
    parseBodyRuntime f text = ok (parseSlideBodyFromText text) := by
  cases hs : readStartLaneAt text.toList 0 with
  | error err =>
      simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
        parseStartLaneAt, parseSlideBodyFromText, hs, Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq]
  | ok start =>
      by_cases h0 : text.contains '-' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, ↓reduceIte]
      by_cases h1 : text.contains '>' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, ↓reduceIte]
      by_cases h2 : text.contains '<' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, ↓reduceIte]
      by_cases h3 : text.contains '^' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, ↓reduceIte]
      by_cases h4 : text.contains 'v' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, ↓reduceIte]
      by_cases h5 : text.contains "pp" = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, ↓reduceIte]
      by_cases h6 : text.contains "qq" = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, ↓reduceIte]
      by_cases h7 : text.contains 'p' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, h7, ↓reduceIte]
      by_cases h8 : text.contains 'q' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, h7, h8, ↓reduceIte]
      by_cases h9 : text.contains 's' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, ↓reduceIte]
      by_cases h10 : text.contains 'z' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, ↓reduceIte]
      by_cases h11 : text.contains 'V' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, ↓reduceIte]
      by_cases h12 : text.contains 'w' = true
      · cases hend2 : readEndAreaAt text.toList 2 <;>
          cases hend3 : readEndAreaAt text.toList 3 <;>
          simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
            laws.readEnd2_eq, laws.readEnd3_eq, parseStartLaneAt, parseEndAreaAt,
            parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
            slide_parser.select_command, slide_parser.command_turn_index,
            slide_parser.command_end_index, bind_tc_ok, toLnmBodyKind,
            Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq, pure, Except.pure, hend2, hend3,
            h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, ↓reduceIte]
      simp only [parseBodyRuntime, laws.chars_eq, laws.readStart_eq,
        parseStartLaneAt, parseSlideBodyFromText, hs, laws.facts_eq, factsForText,
        slide_parser.select_command, bind_tc_ok, Bind.bind, Except.bind, bind_ok, Bool.false_eq_true, UScalar.ofNatCore_val_eq,
        h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, ↓reduceIte]

def parseTerminalRuntime (f : Frontend) (text : String) :
    Result (Except ParseError SensorArea) := do
  let facts := f.facts text
  let index ← slide_parser.terminal_end_index facts.turn facts.pp facts.qq
  ok (f.readEnd (f.chars text) index.val)

/-- Terminal parsing is independent of command priority, even on mixed text. -/
theorem parse_terminal_end_area_equiv (f : Frontend) (text : String)
    (laws : FrontendLaws f text) :
    parseTerminalRuntime f text = ok (parseTerminalEndArea text) := by
  cases ht : text.contains 'V' <;>
    cases hp : text.contains "pp" <;> cases hq : text.contains "qq" <;>
    simp [parseTerminalRuntime, laws.chars_eq, laws.facts_eq,
      laws.readEnd2_eq, laws.readEnd3_eq, factsForText,
      slide_parser.terminal_end_index, parseTerminalEndArea, ht, hp, hq]

def shapeFromTextRuntime (f : Frontend) (text : String) :
    Result (Except ParseError SlideShape) := do
  let body ← parseBodyRuntime f text
  ok (do let parsed ← body; f.solve parsed)

theorem shape_from_text_equiv (f : Frontend) (text : String)
    (laws : FrontendLaws f text)
    (hsolve : ∀ body, parseSlideBodyFromText text = .ok body →
      f.solve body = solveSlideShape body) :
    shapeFromTextRuntime f text = ok (detectShapeFromText text) := by
  cases hb : parseSlideBodyFromText text with
  | error err =>
      simp [shapeFromTextRuntime, parse_body_equiv f text laws,
        detectShapeFromText, hb, Bind.bind, Except.bind]
  | ok body =>
      simp [shapeFromTextRuntime, parse_body_equiv f text laws,
        detectShapeFromText, hb, hsolve body hb, Bind.bind, Except.bind]

def parseNoteRuntime (f : Frontend) (sanitized : String)
    (start : OuterSlot) (endArea : SensorArea) :
    Result (Except ParseError SlideNoteSemantics) := do
  let parsed ← shapeFromTextRuntime f sanitized
  ok (do
    let shape ← parsed
    let just ← f.just sanitized
    pure
      { rawText := sanitized
        startSlot := start
        endArea := endArea
        shape := shape
        isJustRight := just })

/-- The explicit caller-supplied start/end are retained; no equality with the
parsed endpoints is assumed. Sanitization is the String adapter obligation. -/
theorem parse_note_equiv (f : Frontend)
    (raw sanitized : String) (start : OuterSlot) (endArea : SensorArea)
    (hsanitized : sanitized = sanitizeSlideText raw)
    (laws : FrontendLaws f sanitized)
    (hsolve : ∀ body, parseSlideBodyFromText sanitized = .ok body →
      f.solve body = solveSlideShape body)
    (hjust : f.just sanitized = detectJustType sanitized) :
    parseNoteRuntime f sanitized start endArea =
      ok (parseSlideNote raw start endArea) := by
  simp only [parseNoteRuntime, shape_from_text_equiv f sanitized laws hsolve, bind_tc_ok]
  rw [hjust, hsanitized] <;> rfl

theorem parse_shape_text_equiv (f : Frontend)
    (raw sanitized : String) (hsanitized : sanitized = sanitizeSlideText raw)
    (laws : FrontendLaws f sanitized)
    (hsolve : ∀ body, parseSlideBodyFromText sanitized = .ok body →
      f.solve body = solveSlideShape body) :
    shapeFromTextRuntime f sanitized = ok (parseSlideShapeText raw) := by
  rw [shape_from_text_equiv f sanitized laws hsolve, hsanitized] <;> rfl

theorem parse_just_text_equiv (f : Frontend)
    (raw sanitized : String) (hsanitized : sanitized = sanitizeSlideText raw)
    (hjust : f.just sanitized = detectJustType sanitized) :
    f.just sanitized = parseSlideJustText raw := by
  rw [hjust, hsanitized] <;> rfl

def parseNoteFromBodyRuntime (f : Frontend) (raw : String)
    (body : ParsedSlideBody) (endArea : SensorArea) :
    Except ParseError SlideNoteSemantics := do
  let shape ← f.solve body
  let just ← f.just raw
  pure
    { rawText := raw
      startSlot := body.startLane
      endArea := endArea
      shape := shape
      isJustRight := just }

theorem parse_note_from_body_equiv (f : Frontend)
    (raw : String) (body : ParsedSlideBody) (endArea : SensorArea)
    (hsolve : f.solve body = solveSlideShape body)
    (hjust : f.just raw = detectJustType raw) :
    parseNoteFromBodyRuntime f raw body endArea =
      parseSlideNoteFromBody raw body endArea := by
  simp only [parseNoteFromBodyRuntime, hsolve, hjust, parseSlideNoteFromBody]

end Verification.SlideParserProof

namespace Verification.SlideTablesProof
open LnmaiCore.Simai

/-- Specification of the runtime's exact string classifier AFTER removing one
mirror prefix. This function is an adapter model, not extracted string code.
Unknown covers all other strings, including further mirror prefixes, Unicode,
v5, and out-of-range suffixes; there is no valid-key precondition. -/
def classifyTableKey (key : String) : slide_tables.TableKey :=
  if key = "circle1" then .Circle1
  else if key = "circle2" then .Circle2
  else if key = "circle3" then .Circle3
  else if key = "circle4" then .Circle4
  else if key = "circle5" then .Circle5
  else if key = "circle6" then .Circle6
  else if key = "circle7" then .Circle7
  else if key = "circle8" then .Circle8
  else if key = "line2" then .Line2
  else if key = "line3" then .Line3
  else if key = "line4" then .Line4
  else if key = "line5" then .Line5
  else if key = "line6" then .Line6
  else if key = "line7" then .Line7
  else if key = "line8" then .Line8
  else if key = "v1" then .V1
  else if key = "v2" then .V2
  else if key = "v3" then .V3
  else if key = "v4" then .V4
  else if key = "v6" then .V6
  else if key = "v7" then .V7
  else if key = "v8" then .V8
  else if key = "ppqq1" then .Ppqq1
  else if key = "ppqq2" then .Ppqq2
  else if key = "ppqq3" then .Ppqq3
  else if key = "ppqq4" then .Ppqq4
  else if key = "ppqq5" then .Ppqq5
  else if key = "ppqq6" then .Ppqq6
  else if key = "ppqq7" then .Ppqq7
  else if key = "ppqq8" then .Ppqq8
  else if key = "pq1" then .Pq1
  else if key = "pq2" then .Pq2
  else if key = "pq3" then .Pq3
  else if key = "pq4" then .Pq4
  else if key = "pq5" then .Pq5
  else if key = "pq6" then .Pq6
  else if key = "pq7" then .Pq7
  else if key = "pq8" then .Pq8
  else if key = "L2" then .L2
  else if key = "L3" then .L3
  else if key = "L4" then .L4
  else if key = "L5" then .L5
  else if key = "s" then .S
  else if key = "wifi" then .Wifi
  else .Unknown

/-- All scalar bar counts agree with the original Lean table under the explicit
string-classification relation. Line2/Line8 and Wifi intentionally return none;
a judge queue's existence does not imply a visual path weight exists. -/
theorem slide_bar_count_equiv (raw : String) (key : slide_tables.TableKey)
    (hkey : key = classifyTableKey (stripMirrorPrefix raw)) :
    (do let count ← slide_tables.slide_bar_count key
        ok (count.map (fun n => n.val))) = ok (slideBarCountForShapeKey raw) := by
  subst key
  unfold classifyTableKey slideBarCountForShapeKey
  by_cases h0 : stripMirrorPrefix raw = "circle1"
  · rw [h0]
    rfl
  rw [if_neg h0]
  by_cases h1 : stripMirrorPrefix raw = "circle2"
  · rw [h1]
    rfl
  rw [if_neg h1]
  by_cases h2 : stripMirrorPrefix raw = "circle3"
  · rw [h2]
    rfl
  rw [if_neg h2]
  by_cases h3 : stripMirrorPrefix raw = "circle4"
  · rw [h3]
    rfl
  rw [if_neg h3]
  by_cases h4 : stripMirrorPrefix raw = "circle5"
  · rw [h4]
    rfl
  rw [if_neg h4]
  by_cases h5 : stripMirrorPrefix raw = "circle6"
  · rw [h5]
    rfl
  rw [if_neg h5]
  by_cases h6 : stripMirrorPrefix raw = "circle7"
  · rw [h6]
    rfl
  rw [if_neg h6]
  by_cases h7 : stripMirrorPrefix raw = "circle8"
  · rw [h7]
    rfl
  rw [if_neg h7]
  by_cases h8 : stripMirrorPrefix raw = "line2"
  · rw [h8]
    rfl
  rw [if_neg h8]
  by_cases h9 : stripMirrorPrefix raw = "line3"
  · rw [h9]
    rfl
  rw [if_neg h9]
  by_cases h10 : stripMirrorPrefix raw = "line4"
  · rw [h10]
    rfl
  rw [if_neg h10]
  by_cases h11 : stripMirrorPrefix raw = "line5"
  · rw [h11]
    rfl
  rw [if_neg h11]
  by_cases h12 : stripMirrorPrefix raw = "line6"
  · rw [h12]
    rfl
  rw [if_neg h12]
  by_cases h13 : stripMirrorPrefix raw = "line7"
  · rw [h13]
    rfl
  rw [if_neg h13]
  by_cases h14 : stripMirrorPrefix raw = "line8"
  · rw [h14]
    rfl
  rw [if_neg h14]
  by_cases h15 : stripMirrorPrefix raw = "v1"
  · rw [h15]
    rfl
  rw [if_neg h15]
  by_cases h16 : stripMirrorPrefix raw = "v2"
  · rw [h16]
    rfl
  rw [if_neg h16]
  by_cases h17 : stripMirrorPrefix raw = "v3"
  · rw [h17]
    rfl
  rw [if_neg h17]
  by_cases h18 : stripMirrorPrefix raw = "v4"
  · rw [h18]
    rfl
  rw [if_neg h18]
  by_cases h19 : stripMirrorPrefix raw = "v6"
  · rw [h19]
    rfl
  rw [if_neg h19]
  by_cases h20 : stripMirrorPrefix raw = "v7"
  · rw [h20]
    rfl
  rw [if_neg h20]
  by_cases h21 : stripMirrorPrefix raw = "v8"
  · rw [h21]
    rfl
  rw [if_neg h21]
  by_cases h22 : stripMirrorPrefix raw = "ppqq1"
  · rw [h22]
    rfl
  rw [if_neg h22]
  by_cases h23 : stripMirrorPrefix raw = "ppqq2"
  · rw [h23]
    rfl
  rw [if_neg h23]
  by_cases h24 : stripMirrorPrefix raw = "ppqq3"
  · rw [h24]
    rfl
  rw [if_neg h24]
  by_cases h25 : stripMirrorPrefix raw = "ppqq4"
  · rw [h25]
    rfl
  rw [if_neg h25]
  by_cases h26 : stripMirrorPrefix raw = "ppqq5"
  · rw [h26]
    rfl
  rw [if_neg h26]
  by_cases h27 : stripMirrorPrefix raw = "ppqq6"
  · rw [h27]
    rfl
  rw [if_neg h27]
  by_cases h28 : stripMirrorPrefix raw = "ppqq7"
  · rw [h28]
    rfl
  rw [if_neg h28]
  by_cases h29 : stripMirrorPrefix raw = "ppqq8"
  · rw [h29]
    rfl
  rw [if_neg h29]
  by_cases h30 : stripMirrorPrefix raw = "pq1"
  · rw [h30]
    rfl
  rw [if_neg h30]
  by_cases h31 : stripMirrorPrefix raw = "pq2"
  · rw [h31]
    rfl
  rw [if_neg h31]
  by_cases h32 : stripMirrorPrefix raw = "pq3"
  · rw [h32]
    rfl
  rw [if_neg h32]
  by_cases h33 : stripMirrorPrefix raw = "pq4"
  · rw [h33]
    rfl
  rw [if_neg h33]
  by_cases h34 : stripMirrorPrefix raw = "pq5"
  · rw [h34]
    rfl
  rw [if_neg h34]
  by_cases h35 : stripMirrorPrefix raw = "pq6"
  · rw [h35]
    rfl
  rw [if_neg h35]
  by_cases h36 : stripMirrorPrefix raw = "pq7"
  · rw [h36]
    rfl
  rw [if_neg h36]
  by_cases h37 : stripMirrorPrefix raw = "pq8"
  · rw [h37]
    rfl
  rw [if_neg h37]
  by_cases h38 : stripMirrorPrefix raw = "L2"
  · rw [h38]
    rfl
  rw [if_neg h38]
  by_cases h39 : stripMirrorPrefix raw = "L3"
  · rw [h39]
    rfl
  rw [if_neg h39]
  by_cases h40 : stripMirrorPrefix raw = "L4"
  · rw [h40]
    rfl
  rw [if_neg h40]
  by_cases h41 : stripMirrorPrefix raw = "L5"
  · rw [h41]
    rfl
  rw [if_neg h41]
  by_cases h42 : stripMirrorPrefix raw = "s"
  · rw [h42]
    rfl
  rw [if_neg h42]
  by_cases h43 : stripMirrorPrefix raw = "wifi"
  · rw [h43]
    rfl
  rw [if_neg h43]
  simp only [slide_tables.slide_bar_count, bind_tc_ok, Option.map_none,
    h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24, h25, h26, h27, h28, h29, h30, h31, h32, h33, h34, h35, h36, h37, h38, h39, h40, h41, h42, h43]

/-- The actual extracted millionths are consumed by the runtime's Rat::new
adapter. Its exact rational interpretation is an explicit arithmetic premise;
no bounded-rational model or implicit floating-point approximation is used. -/
def lastWaitRuntime (key : slide_tables.TableKey) (classic : Bool)
    (makeRatio : U32 → Rat) : Result (Option Rat) := do
  let numerator ← slide_tables.slide_last_wait_millionths key classic
  ok (numerator.map makeRatio)

/-- Both timing modes and every string key, including rejection cases. The only
premises are the string classifier and exact integer-to-rational adapter. -/
theorem slide_last_wait_ratio_equiv (raw : String) (key : slide_tables.TableKey)
    (classic : Bool) (makeRatio : U32 → Rat)
    (hkey : key = classifyTableKey (stripMirrorPrefix raw))
    (hratio : ∀ n, makeRatio n = (n.val : Rat) / 1000000) :
    lastWaitRuntime key classic makeRatio =
      ok (slideLastWaitRatioForShapeKey raw classic) := by
  have hmake : makeRatio = fun n : U32 => (n.val : Rat) / 1000000 := funext hratio
  subst makeRatio
  subst key
  unfold classifyTableKey slideLastWaitRatioForShapeKey
  by_cases h0 : stripMirrorPrefix raw = "circle1"
  · rw [h0]
    cases classic <;> rfl
  rw [if_neg h0]
  by_cases h1 : stripMirrorPrefix raw = "circle2"
  · rw [h1]
    cases classic <;> rfl
  rw [if_neg h1]
  by_cases h2 : stripMirrorPrefix raw = "circle3"
  · rw [h2]
    cases classic <;> rfl
  rw [if_neg h2]
  by_cases h3 : stripMirrorPrefix raw = "circle4"
  · rw [h3]
    cases classic <;> rfl
  rw [if_neg h3]
  by_cases h4 : stripMirrorPrefix raw = "circle5"
  · rw [h4]
    cases classic <;> rfl
  rw [if_neg h4]
  by_cases h5 : stripMirrorPrefix raw = "circle6"
  · rw [h5]
    cases classic <;> rfl
  rw [if_neg h5]
  by_cases h6 : stripMirrorPrefix raw = "circle7"
  · rw [h6]
    cases classic <;> rfl
  rw [if_neg h6]
  by_cases h7 : stripMirrorPrefix raw = "circle8"
  · rw [h7]
    cases classic <;> rfl
  rw [if_neg h7]
  by_cases h8 : stripMirrorPrefix raw = "line2"
  · rw [h8]
    cases classic <;> rfl
  rw [if_neg h8]
  by_cases h9 : stripMirrorPrefix raw = "line3"
  · rw [h9]
    cases classic <;> rfl
  rw [if_neg h9]
  by_cases h10 : stripMirrorPrefix raw = "line4"
  · rw [h10]
    cases classic <;> rfl
  rw [if_neg h10]
  by_cases h11 : stripMirrorPrefix raw = "line5"
  · rw [h11]
    cases classic <;> rfl
  rw [if_neg h11]
  by_cases h12 : stripMirrorPrefix raw = "line6"
  · rw [h12]
    cases classic <;> rfl
  rw [if_neg h12]
  by_cases h13 : stripMirrorPrefix raw = "line7"
  · rw [h13]
    cases classic <;> rfl
  rw [if_neg h13]
  by_cases h14 : stripMirrorPrefix raw = "line8"
  · rw [h14]
    cases classic <;> rfl
  rw [if_neg h14]
  by_cases h15 : stripMirrorPrefix raw = "v1"
  · rw [h15]
    cases classic <;> rfl
  rw [if_neg h15]
  by_cases h16 : stripMirrorPrefix raw = "v2"
  · rw [h16]
    cases classic <;> rfl
  rw [if_neg h16]
  by_cases h17 : stripMirrorPrefix raw = "v3"
  · rw [h17]
    cases classic <;> rfl
  rw [if_neg h17]
  by_cases h18 : stripMirrorPrefix raw = "v4"
  · rw [h18]
    cases classic <;> rfl
  rw [if_neg h18]
  by_cases h19 : stripMirrorPrefix raw = "v6"
  · rw [h19]
    cases classic <;> rfl
  rw [if_neg h19]
  by_cases h20 : stripMirrorPrefix raw = "v7"
  · rw [h20]
    cases classic <;> rfl
  rw [if_neg h20]
  by_cases h21 : stripMirrorPrefix raw = "v8"
  · rw [h21]
    cases classic <;> rfl
  rw [if_neg h21]
  by_cases h22 : stripMirrorPrefix raw = "ppqq1"
  · rw [h22]
    cases classic <;> rfl
  rw [if_neg h22]
  by_cases h23 : stripMirrorPrefix raw = "ppqq2"
  · rw [h23]
    cases classic <;> rfl
  rw [if_neg h23]
  by_cases h24 : stripMirrorPrefix raw = "ppqq3"
  · rw [h24]
    cases classic <;> rfl
  rw [if_neg h24]
  by_cases h25 : stripMirrorPrefix raw = "ppqq4"
  · rw [h25]
    cases classic <;> rfl
  rw [if_neg h25]
  by_cases h26 : stripMirrorPrefix raw = "ppqq5"
  · rw [h26]
    cases classic <;> rfl
  rw [if_neg h26]
  by_cases h27 : stripMirrorPrefix raw = "ppqq6"
  · rw [h27]
    cases classic <;> rfl
  rw [if_neg h27]
  by_cases h28 : stripMirrorPrefix raw = "ppqq7"
  · rw [h28]
    cases classic <;> rfl
  rw [if_neg h28]
  by_cases h29 : stripMirrorPrefix raw = "ppqq8"
  · rw [h29]
    cases classic <;> rfl
  rw [if_neg h29]
  by_cases h30 : stripMirrorPrefix raw = "pq1"
  · rw [h30]
    cases classic <;> rfl
  rw [if_neg h30]
  by_cases h31 : stripMirrorPrefix raw = "pq2"
  · rw [h31]
    cases classic <;> rfl
  rw [if_neg h31]
  by_cases h32 : stripMirrorPrefix raw = "pq3"
  · rw [h32]
    cases classic <;> rfl
  rw [if_neg h32]
  by_cases h33 : stripMirrorPrefix raw = "pq4"
  · rw [h33]
    cases classic <;> rfl
  rw [if_neg h33]
  by_cases h34 : stripMirrorPrefix raw = "pq5"
  · rw [h34]
    cases classic <;> rfl
  rw [if_neg h34]
  by_cases h35 : stripMirrorPrefix raw = "pq6"
  · rw [h35]
    cases classic <;> rfl
  rw [if_neg h35]
  by_cases h36 : stripMirrorPrefix raw = "pq7"
  · rw [h36]
    cases classic <;> rfl
  rw [if_neg h36]
  by_cases h37 : stripMirrorPrefix raw = "pq8"
  · rw [h37]
    cases classic <;> rfl
  rw [if_neg h37]
  by_cases h38 : stripMirrorPrefix raw = "L2"
  · rw [h38]
    cases classic <;> rfl
  rw [if_neg h38]
  by_cases h39 : stripMirrorPrefix raw = "L3"
  · rw [h39]
    cases classic <;> rfl
  rw [if_neg h39]
  by_cases h40 : stripMirrorPrefix raw = "L4"
  · rw [h40]
    cases classic <;> rfl
  rw [if_neg h40]
  by_cases h41 : stripMirrorPrefix raw = "L5"
  · rw [h41]
    cases classic <;> rfl
  rw [if_neg h41]
  by_cases h42 : stripMirrorPrefix raw = "s"
  · rw [h42]
    cases classic <;> rfl
  rw [if_neg h42]
  by_cases h43 : stripMirrorPrefix raw = "wifi"
  · rw [h43]
    cases classic <;> rfl
  rw [if_neg h43]
  simp only [lastWaitRuntime, slide_tables.slide_last_wait_millionths, bind_tc_ok, Option.map_none,
    h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, h24, h25, h26, h27, h28, h29, h30, h31, h32, h33, h34, h35, h36, h37, h38, h39, h40, h41, h42, h43]

/-- Shape-to-key formatting/canonical symmetry handling is a separate adapter. -/
theorem slide_bar_count_for_shape_equiv (shape : LnmaiCore.Simai.SlideShape)
    (key : slide_tables.TableKey)
    (hkey : key = classifyTableKey (stripMirrorPrefix (canonicalShapeKey shape))) :
    (do let count ← slide_tables.slide_bar_count key
        ok (count.map (fun n => n.val))) = ok (slideBarCountForShape shape) := by
  exact slide_bar_count_equiv (canonicalShapeKey shape) key hkey

theorem slide_last_wait_ratio_for_shape_equiv (shape : LnmaiCore.Simai.SlideShape)
    (key : slide_tables.TableKey) (classic : Bool) (makeRatio : U32 → Rat)
    (hkey : key = classifyTableKey (stripMirrorPrefix (canonicalShapeKey shape)))
    (hratio : ∀ n, makeRatio n = (n.val : Rat) / 1000000) :
    lastWaitRuntime key classic makeRatio =
      ok (slideLastWaitRatioForShape shape classic) := by
  exact slide_last_wait_ratio_equiv (canonicalShapeKey shape) key classic makeRatio hkey hratio

end Verification.SlideTablesProof
