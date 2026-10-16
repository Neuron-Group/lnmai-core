import Verification.Normalization
import Verification.SlideParserText

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai

set_option maxHeartbeats 1000000

namespace Verification.NormalizationFrontendProof

open Verification.NormalizationModel Verification.NormalizationProof
open Verification.ShapeProof (toLnmShape toLnmResult exists_of_project_eq)
open private sanitizeSlideText from LnmaiCore.Simai.SlideParser

/-!
UNELABORATED independent composition draft. No compiler, tests or extraction
were run to produce it. It imports the frozen Normalization API, without
changing that API or the main worker's model/proof repairs.

The shape solver result is retained in its ACTUAL Generated type through
parsing and into normalization.slide_layout. The sanitizer is also the actual
extracted loop, via SlideParserTextProof.sanitizeRuntime. No complete-parser,
complete-sanitizer, shape-read, or lowerSlideToken equation is a premise.

The surrounding Option/Except glue, record copies, exact-time expressions,
and append lists are SOURCE INTERPRETATIONS of the runtime call sites. They
are not new Generated definitions. Runtime UTF-8/String/Vec representation,
the reader/diagnostic bridges, Natural/shape conversion, time/table leaves,
and allocation/append success still have to be installed at those boundaries.
In particular retainedShapeKind below is an explicitly defined extracted
shape instance, NOT a proof about arbitrary Rust ShapeKind implementations.
-/

def exceptView {E A B : Type} (view : A → B) : Except E A → Except E B
  | .error error => .error error
  | .ok value => .ok (view value)

/-- Preserve the generated solver payload instead of discarding it and
assuming an encoder from an arbitrary Lean shape back into a bounded shape. -/
structure RetainedNote where
  rawText : String
  startSlot : OuterSlot
  endArea : SensorArea
  parsedShape : shape.SlideShape
  isJustRight : Bool

def noteView (note : RetainedNote) : SlideNoteSemantics :=
  { rawText := note.rawText
    startSlot := note.startSlot
    endArea := note.endArea
    shape := toLnmShape note.parsedShape
    isJustRight := note.isJustRight }

def completeNote (text : String) (start : OuterSlot) (endArea : SensorArea)
    (parsed : Except ParseError shape.SlideShape) (just : Unit → Except ParseError Bool) :
    Except ParseError RetainedNote := do
  let parsedShape ← parsed
  let isJustRight ← just ()
  pure { rawText := text, startSlot := start, endArea, parsedShape, isJustRight }

theorem completeNote_view (text : String) (start : OuterSlot) (endArea : SensorArea)
    (parsed : Except ParseError shape.SlideShape) (just : Unit → Except ParseError Bool) :
    exceptView noteView (completeNote text start endArea parsed just) =
      (do let parsedShape ← exceptView toLnmShape parsed
          let isJustRight ← just ()
          pure ({ rawText := text, startSlot := start, endArea, shape := parsedShape, isJustRight } : SlideNoteSemantics)) := by
  cases parsed <;> cases hjust : just () <;>
    simp [completeNote, exceptView, noteView, hjust, Bind.bind, Except.bind, pure, Except.pure]

def solveRetained (body : ParsedSlideBody) :
    Result (Except ParseError shape.SlideShape) := do
  let result ← shape.solve_shape (SlideParserComposition.ofBodyKind body.kind)
    (Verification.Areas.ofLnmOuterSlot body.startLane)
    (body.endArea.map Verification.Areas.ofLnmSensorArea)
    (body.turnArea.map Verification.Areas.ofLnmSensorArea)
  ok (toLnmResult id body.rawText result)

/-- This invokes the existing real solver proof, rather than asking a caller
to assume that a solver callback agrees with solveSlideShape. -/
theorem solveRetained_spec (body : ParsedSlideBody) :
    ∃ out, solveRetained body = ok out ∧
      exceptView toLnmShape out = solveSlideShape body := by
  have hsolver := SlideParserComposition.solve_body_equiv body
  unfold SlideParserComposition.solveBody at hsolver
  obtain ⟨result, hcall, hview⟩ := exists_of_project_eq _ _ _ hsolver
  refine ⟨toLnmResult id body.rawText result, ?_, ?_⟩
  · simp only [solveRetained, hcall, bind_tc_ok]
  · have hmap :
        exceptView toLnmShape (toLnmResult id body.rawText result) =
          toLnmResult toLnmShape body.rawText result := by
      cases result <;> rfl
    exact hmap.trans hview

def shapeFromTextRetained (base : SlideParserProof.Frontend) (text : String) :
    Result (Except ParseError shape.SlideShape) := do
  let body ← SlideParserProof.parseBodyRuntime (SlideParserTextProof.installText base) text
  match body with
  | .error error => ok (.error error)
  | .ok body => solveRetained body

theorem shapeFromTextRetained_spec (base : SlideParserProof.Frontend) (text : String)
    (hdecode : base.chars text = text.toList) :
    ∃ out, shapeFromTextRetained base text = ok out ∧
      exceptView toLnmShape out = detectShapeFromText text := by
  have hbody := SlideParserTextProof.parse_body_equiv base text hdecode
  cases href : parseSlideBodyFromText text with
  | error error =>
      refine ⟨.error error, ?_, ?_⟩
      · simp only [shapeFromTextRetained, hbody, href, bind_tc_ok]
      · simp only [exceptView, detectShapeFromText, href, Bind.bind, Except.bind]
  | ok body =>
      obtain ⟨out, hcall, hview⟩ := solveRetained_spec body
      refine ⟨out, ?_, ?_⟩
      · simp only [shapeFromTextRetained, hbody, href, bind_tc_ok, hcall]
      · simpa only [detectShapeFromText, href, Bind.bind, Except.bind] using hview

def parseTextRetained (base : SlideParserProof.Frontend) (sanitized : String)
    (start : OuterSlot) (endArea : SensorArea) :
    Result (Except ParseError RetainedNote) := do
  let parsed ← shapeFromTextRetained base sanitized
  ok (completeNote sanitized start endArea parsed
    (fun _ => (SlideParserTextProof.installText base).just sanitized))

theorem parseTextRetained_spec (base : SlideParserProof.Frontend)
    (raw sanitized : String) (start : OuterSlot) (endArea : SensorArea)
    (hsanitized : sanitized = sanitizeSlideText raw)
    (hdecode : base.chars sanitized = sanitized.toList) :
    ∃ out, parseTextRetained base sanitized start endArea = ok out ∧
      exceptView noteView out = parseSlideNote raw start endArea := by
  obtain ⟨parsed, hcall, hview⟩ := shapeFromTextRetained_spec base sanitized hdecode
  refine ⟨completeNote sanitized start endArea parsed
    (fun _ => (SlideParserTextProof.installText base).just sanitized), ?_, ?_⟩
  · simp only [parseTextRetained, hcall, bind_tc_ok]
  · rw [completeNote_view, hview,
      SlideParserTextProof.installed_just_equiv base sanitized hdecode, hsanitized] <;> rfl

def parseBodyRetained (base : SlideParserProof.Frontend) (raw : String)
    (body : ParsedSlideBody) (endArea : SensorArea) :
    Result (Except ParseError RetainedNote) := do
  let parsed ← solveRetained body
  ok (completeNote raw body.startLane endArea parsed
    (fun _ => (SlideParserTextProof.installText base).just raw))

theorem parseBodyRetained_spec (base : SlideParserProof.Frontend)
    (raw : String) (body : ParsedSlideBody) (endArea : SensorArea)
    (hdecode : base.chars raw = raw.toList) :
    ∃ out, parseBodyRetained base raw body endArea = ok out ∧
      exceptView noteView out = parseSlideNoteFromBody raw body endArea := by
  obtain ⟨parsed, hcall, hview⟩ := solveRetained_spec body
  refine ⟨completeNote raw body.startLane endArea parsed
    (fun _ => (SlideParserTextProof.installText base).just raw), ?_, ?_⟩
  · simp only [parseBodyRetained, hcall, bind_tc_ok]
  · rw [completeNote_view, hview,
      SlideParserTextProof.installed_just_equiv base raw hdecode] <;> rfl

def parseCharsRetained (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (start : OuterSlot) (endArea : SensorArea) :
    Result (Except ParseError RetainedNote) := do
  let sanitized ← SlideParserTextProof.sanitizeRuntime encode chars
  parseTextRetained base sanitized start endArea

/-- Only character decoding/encoding and the physical character bound.
This bundle contains no parser, sanitizer, layout, or whole-update law.
The body-present and early-error lemmas below require fewer fields. -/
structure TextPoints (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char) (raw : String) : Prop where
  raw_slice : chars.val = raw.toList
  raw_size : chars.val.length ≤ Usize.max
  raw_decode : base.chars raw = raw.toList
  encode_kept : ∀ kept : List Char, kept.length ≤ chars.val.length →
    encode kept = String.ofList kept
  sanitized_decode :
    base.chars (sanitizeSlideText raw) = (sanitizeSlideText raw).toList

theorem parseCharsRetained_spec (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char) (raw : String)
    (start : OuterSlot) (endArea : SensorArea)
    (points : TextPoints base encode chars raw) :
    ∃ out, parseCharsRetained base encode chars start endArea = ok out ∧
      exceptView noteView out = parseSlideNote raw start endArea := by
  obtain ⟨out, hcall, hview⟩ := parseTextRetained_spec base raw
    (sanitizeSlideText raw) start endArea rfl points.sanitized_decode
  refine ⟨out, ?_, hview⟩
  simp only [parseCharsRetained, SlideParserTextProof.sanitize_runtime_equiv
    encode chars raw points.raw_slice points.raw_size points.encode_kept, bind_tc_ok, hcall]

def selectedReference (token : RawNoteToken) (slot : OuterSlot)
    (endArea : SensorArea) : Except ParseError SlideNoteSemantics :=
  match token.slideBody with
  | some body => parseSlideNoteFromBody token.rawText body endArea
  | none => parseSlideNote token.rawText slot endArea

def selectedRetained (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (token : RawNoteToken) (slot : OuterSlot) (endArea : SensorArea) :
    Result (Except ParseError RetainedNote) :=
  match token.slideBody with
  | some body => parseBodyRetained base token.rawText body endArea
  | none => parseCharsRetained base encode chars slot endArea

/-- A present body excludes the text/sanitizer branch even if solving that
body fails, its startLane differs from slot, or its rawText differs from raw. -/
theorem selectedRetained_body (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (token : RawNoteToken) (slot : OuterSlot) (endArea : SensorArea)
    (body : ParsedSlideBody) (hbody : token.slideBody = some body) :
    selectedRetained base encode chars token slot endArea =
      parseBodyRetained base token.rawText body endArea := by
  simp only [selectedRetained, hbody]

theorem selectedRetained_spec (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (token : RawNoteToken) (slot : OuterSlot) (endArea : SensorArea)
    (points : TextPoints base encode chars token.rawText) :
    ∃ out, selectedRetained base encode chars token slot endArea = ok out ∧
      exceptView noteView out = selectedReference token slot endArea := by
  cases hbody : token.slideBody with
  | none =>
      simpa only [selectedRetained, selectedReference, hbody] using
        parseCharsRetained_spec base encode chars token.rawText slot endArea points
  | some body =>
      simpa only [selectedRetained, selectedReference, hbody] using
        parseBodyRetained_spec base token.rawText body endArea points.raw_decode

/-- Actual extracted shape-kind read, on the actual extracted solver payload.
The runtime conversion to its Natural-payload shape is a separate bridge. -/
def retainedShapeKind : normalization.ShapeKind Unit shape.SlideShape :=
  { kind := fun _ input => shape.shape_kind input }

/-- Source interpretation of simai/shape.rs::shape_kind after Into::into.
All Natural payloads are retained; this point operation does not convert any
endpoint to usize. The physical Rust conversion is still a runtime boundary. -/
def naturalShapeKind : normalization.ShapeKind Unit SlideShape :=
  { kind := fun _ input =>
      match input.canonical with
      | .line _ => ok .Line
      | .circle _ => ok .Circle
      | .v _ => ok .V
      | .turn _ => ok .Turn
      | .pq _ => ok .Pq
      | .ppqq _ => ok .Ppqq
      | .s => ok .S
      | .wifi => ok .Wifi }

def naturalShapeRep : NormalizationPoints.ShapeRepresentation SlideShape :=
  { view := id, fits := fun _ => True }

theorem naturalShapeAtoms : ShapeAtomicAgrees naturalShapeKind () naturalShapeRep := by
  constructor
  intro input _hfit
  rcases input with ⟨canonical, symmetry⟩
  cases canonical <;> exact ⟨_, rfl, rfl⟩

/-- The interpreted runtime discriminant plus the real generic layout kernel
also covers unchecked shapes with endpoints larger than Usize.max. -/
theorem natural_layout_equiv (input : SlideShape)
    (flags : normalization.RawFlags) (isClassic : Bool) :
    (do let out ← normalization.slide_layout naturalShapeKind input flags isClassic ()
        ok (toModelSlideLayout out)) =
      ok (slideLayout input.kind (toModelRawFlags flags) isClassic) :=
  slide_layout_equiv naturalShapeKind () naturalShapeRep naturalShapeAtoms
    input flags isClassic True.intro

theorem natural_kind_on_retained (input : shape.SlideShape) :
    naturalShapeKind.kind () (toLnmShape input) = shape.shape_kind input := by
  rcases input with ⟨canonical, symmetry⟩
  cases canonical <;> rfl

/-- Exact kernel-call correspondence across that one-way shape conversion.
This proves the point adapter's source semantics, not Rust Into allocation. -/
theorem natural_layout_on_retained (input : shape.SlideShape)
    (flags : normalization.RawFlags) (isClassic : Bool) :
    normalization.slide_layout naturalShapeKind (toLnmShape input) flags isClassic () =
      normalization.slide_layout retainedShapeKind input flags isClassic () := by
  simp only [normalization.slide_layout, natural_kind_on_retained, retainedShapeKind]

def retainedShapeRep : NormalizationPoints.ShapeRepresentation shape.SlideShape :=
  { view := toLnmShape, fits := fun _ => True }

theorem retainedShapeAtoms : ShapeAtomicAgrees retainedShapeKind () retainedShapeRep := by
  constructor
  intro input _hfit
  obtain ⟨kind, hcall, hview⟩ := exists_of_project_eq _ _ _
    (Verification.ShapeProof.shape_kind_equiv input)
  refine ⟨kind, hcall, ?_⟩
  change toModelShapeKind kind = (toLnmShape input).kind
  have hkind : toModelShapeKind kind = Verification.ShapeProof.toLnmKind kind := by
    cases kind <;> rfl
  exact hkind.trans hview

/-- Explicit field-copy interpretation, using the layout actually returned
by the kernel. Time/table operations are exact Lean leaves here. Their Rust
implementations are not certified by this definition or its rfl bridge. -/
def assembleSlide (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (parsed : SlideNoteSemantics) (layout : SlideLayout) :
    NormalizedSlide :=
  let length := token.length.getD (noteTimingIncrement token.bpm (max token.divisor 1))
  let startTiming := token.timing + token.starWait.getD Duration.zero
  let lastWaitRatio := (slideLastWaitRatioForShape parsed.shape isClassic).getD 0
  let judgeDuration :=
    Time.durationFromRatMicros ((length.toMicros : Rat) * (1 - lastWaitRatio))
  { headTiming := token.timing
    slot := slot
    length := length
    startTiming := startTiming
    groupStartTiming := none
    hSpeed := token.hSpeed
    slideKind := layout.slideKind
    isClassic := layout.isClassic
    trackCount := layout.trackCount
    judgeAt := some (startTiming + judgeDuration)
    isBreak := layout.flags.isBreak
    isEX := layout.flags.isEX
    isHanabi := layout.flags.isHanabi
    hasHeadNote := layout.hasHeadNote
    hasBody := layout.hasBody
    isSlideNoHead := layout.flags.isSlideNoHead
    isForceStar := layout.flags.isForceStar
    isFakeRotate := layout.flags.isFakeRotate
    isSlideBreak := layout.flags.isSlideBreak
    isConnSlide := layout.isConnSlide
    parentNoteIndex := none
    isGroupHead := layout.isGroupHead
    isGroupEnd := layout.isGroupEnd
    totalJudgeQueueLen := layout.totalJudgeQueueLen
    judgeQueues := []
    sourceGroupId := token.sourceGroupId
    sourceGroupIndex := token.sourceGroupIndex
    sourceGroupSize := token.sourceGroupSize
    multiple := layout.multiple
    noteIndex := noteIndex
    simaiShape := parsed.shape }

theorem assembleSlide_model (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : RawFlags) (parsed : SlideNoteSemantics) :
    assembleSlide isClassic noteIndex token slot parsed
        (slideLayout parsed.shape.kind flags isClassic) =
      buildSlide isClassic noteIndex token slot flags parsed := rfl

def buildRetained (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : normalization.RawFlags) (parsed : RetainedNote) :
    Result (NormalizedSlide × SlideNoteSemantics) := do
  let layout ← normalization.slide_layout retainedShapeKind parsed.parsedShape flags isClassic ()
  ok (assembleSlide isClassic noteIndex token slot (noteView parsed)
    (toModelSlideLayout layout), noteView parsed)

/-- All eight shape branches and all seven flags are supplied by the actual
normalization theorem. No additional ShapeAtomicAgrees premise is requested. -/
theorem buildRetained_equiv (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : normalization.RawFlags) (parsed : RetainedNote) :
    buildRetained isClassic noteIndex token slot flags parsed =
      ok (buildSlide isClassic noteIndex token slot (toModelRawFlags flags)
        (noteView parsed), noteView parsed) := by
  obtain ⟨layout, hcall, hview⟩ := spec_imp_exists
    (slide_layout_spec retainedShapeKind () retainedShapeRep retainedShapeAtoms
      parsed.parsedShape flags isClassic True.intro)
  have hview' : toModelSlideLayout layout =
      slideLayout (noteView parsed).shape.kind (toModelRawFlags flags) isClassic := hview
  simp only [buildRetained, hcall, bind_tc_ok, hview', assembleSlide_model]

def lowerAtSlot (base : SlideParserProof.Frontend) (encode : List Char → String)
    (chars : Slice Char) (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : normalization.RawFlags) :
    Result (Option (NormalizedSlide × SlideNoteSemantics)) := do
  let terminal ← SlideParserProof.parseTerminalRuntime
    (SlideParserTextProof.installText base) token.rawText
  match terminal with
  | .error _ => ok none
  | .ok endArea =>
      let parsed ← selectedRetained base encode chars token slot endArea
      match parsed with
      | .error _ => ok none
      | .ok parsed =>
          let built ← buildRetained isClassic noteIndex token slot flags parsed
          ok (some built)

/-- Keep the selected parser behind a named definition when rewriting. This
avoids relying on elaborated nested-match expressions having identical forms. -/
theorem lowerSlideAtSlot_selected (isClassic : Bool) (noteIndex : Nat)
    (token : RawNoteToken) (slot : OuterSlot) (flags : RawFlags) :
    lowerSlideAtSlot isClassic noteIndex token slot flags =
      (match parseTerminalEndArea token.rawText with
       | .error _ => none
       | .ok endArea =>
           match selectedReference token slot endArea with
           | .error _ => none
           | .ok parsed =>
               some (buildSlide isClassic noteIndex token slot flags parsed, parsed)) := rfl

/-- A failed terminal parse is returned before body selection, sanitizing,
shape solving, layout, or record/time construction. No sanitizer/size law is
needed on this path. This is a parse error, not an Aeneas failure. -/
theorem lowerAtSlot_terminal_error (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : normalization.RawFlags) (error : ParseError)
    (hdecode : base.chars token.rawText = token.rawText.toList)
    (herror : parseTerminalEndArea token.rawText = .error error) :
    lowerAtSlot base encode chars isClassic noteIndex token slot flags = ok none := by
  simp only [lowerAtSlot, SlideParserTextProof.parse_terminal_end_area_equiv
    base token.rawText hdecode, herror, bind_tc_ok]

theorem lowerAtSlot_equiv (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : normalization.RawFlags)
    (points : TextPoints base encode chars token.rawText) :
    lowerAtSlot base encode chars isClassic noteIndex token slot flags =
      ok (lowerSlideAtSlot isClassic noteIndex token slot (toModelRawFlags flags)) := by
  have hterminal := SlideParserTextProof.parse_terminal_end_area_equiv
    base token.rawText points.raw_decode
  rw [lowerSlideAtSlot_selected]
  cases hend : parseTerminalEndArea token.rawText with
  | error error =>
      simp only [lowerAtSlot, hterminal, hend, bind_tc_ok]
  | ok endArea =>
      obtain ⟨out, hcall, hview⟩ :=
        selectedRetained_spec base encode chars token slot endArea points
      have href : selectedReference token slot endArea = exceptView noteView out := hview.symm
      cases out with
      | error error =>
          simp only [lowerAtSlot, hterminal, hend, bind_tc_ok, hcall,
            href, exceptView]
      | ok parsed =>
          simp only [lowerAtSlot, hterminal, hend, bind_tc_ok, hcall,
            href, exceptView, buildRetained_equiv]

def kernelFlags (flags : RawFlags) : normalization.RawFlags :=
  ⟨flags.isBreak, flags.isEX, flags.isHanabi, flags.isSlideNoHead,
    flags.isForceStar, flags.isFakeRotate, flags.isSlideBreak⟩

@[simp] theorem kernelFlags_view (flags : RawFlags) :
    toModelRawFlags (kernelFlags flags) = flags := by
  cases flags; rfl

/-- Direct public wrapper deliberately does not inspect token.kind. -/
def lowerToken (base : SlideParserProof.Frontend) (encode : List Char → String)
    (chars : Slice Char) (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken) :
    Result (Option (NormalizedSlide × SlideNoteSemantics)) :=
  match token.slot with
  | none => ok none
  | some slot => lowerAtSlot base encode chars isClassic noteIndex token slot
      (kernelFlags (flagsFromToken token))

theorem lowerToken_missing_slot (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (hslot : token.slot = none) :
    lowerToken base encode chars isClassic noteIndex token = ok none := by
  simp only [lowerToken, hslot]

/-- Connects real parser/normalization kernels, through the stated source
glue, to the original public lowerSlideToken. noteIndex is arbitrary Nat;
there is no usize conversion, canonical-metadata, or positive-time premise. -/
theorem lowerToken_equiv (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (points : TextPoints base encode chars token.rawText) :
    lowerToken base encode chars isClassic noteIndex token =
      ok (lowerSlideToken noteIndex token isClassic) := by
  cases hslot : token.slot with
  | none => simp only [lowerToken, hslot, lowerSlideToken]
  | some slot =>
      simp only [lowerToken, hslot, lowerAtSlot_equiv base encode chars
        isClassic noteIndex token slot (kernelFlags (flagsFromToken token)) points,
        kernelFlags_view, lowerSlideAtSlot_reference isClassic noteIndex token slot hslot]

/-- Runtime lists grow by append; the original Lean fold grows by cons.
This interpretation is explicit instead of silently treating the two states
as equal. Concrete Vec allocation/push and Natural increment remain leaves. -/
structure ForwardState where
  nextIndex : Nat
  taps : List NormalizedTap
  holds : List NormalizedHold
  touches : List NormalizedTouch
  touchHolds : List NormalizedTouchHold
  slides : List NormalizedSlide
  debug : List NormalizedSlideDebug
  semantics : List SlideNoteSemantics

def stateView (state : ForwardState) : LowerState :=
  (state.nextIndex, state.taps.reverse, state.holds.reverse, state.touches.reverse,
    state.touchHolds.reverse, state.slides.reverse, state.debug.reverse, state.semantics.reverse)

def appendSlide (state : ForwardState) (token : RawNoteToken) :
    Option (NormalizedSlide × SlideNoteSemantics) → ForwardState
  | none => state
  | some (slide, parsed) =>
      { state with
        nextIndex := state.nextIndex + 1
        slides := state.slides ++ [slide]
        debug := state.debug ++ [{ noteIndex := state.nextIndex, rawText := token.rawText }]
        semantics := state.semantics ++ [parsed] }

def acceptSlide (state : LowerState) (token : RawNoteToken)
    (lowered : Option (NormalizedSlide × SlideNoteSemantics)) : LowerState :=
  let (noteIndex, taps, holds, touches, touchHolds, slides, debug, semantics) := state
  match lowered with
  | some (slide, parsed) =>
      (noteIndex + 1, taps, holds, touches, touchHolds, slide :: slides,
        { noteIndex := noteIndex, rawText := token.rawText } :: debug, parsed :: semantics)
  | none => state

/-- Exact lists, including order and duplicates. No uniqueness/disjointness
assumption is made about prior noteIndex values or accumulated records. -/
theorem appendSlide_view (state : ForwardState) (token : RawNoteToken)
    (lowered : Option (NormalizedSlide × SlideNoteSemantics)) :
    stateView (appendSlide state token lowered) =
      acceptSlide (stateView state) token lowered := by
  cases lowered with
  | none => rfl
  | some pair =>
      rcases pair with ⟨slide, parsed⟩
      simp [stateView, appendSlide, acceptSlide, List.reverse_append]

theorem referenceStep_slide (isClassic : Bool) (measureDurSec : Rat → Duration)
    (state : LowerState) (token : RawNoteToken) (hkind : token.kind = .slide) :
    referenceStep isClassic measureDurSec state token =
      acceptSlide state token (lowerSlideToken state.1 token isClassic) := by
  rcases state with ⟨noteIndex, taps, holds, touches, touchHolds, slides, debug, semantics⟩
  simp only [referenceStep, hkind, acceptSlide]
  rfl

def lowerSlideStep (base : SlideParserProof.Frontend) (encode : List Char → String)
    (chars : Slice Char) (isClassic : Bool) (state : ForwardState)
    (token : RawNoteToken) : Result ForwardState := do
  let lowered ← lowerToken base encode chars isClassic state.nextIndex token
  ok (appendSlide state token lowered)

/-- The accepted-slide branch of the outer fold, including parse rejection.
This does not claim the Rust for-loop, NotePlan dispatch, or later metadata/
queue/table passes have themselves been extracted by this file. -/
theorem lowerSlideStep_equiv (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (measureDurSec : Rat → Duration)
    (state : ForwardState) (token : RawNoteToken)
    (points : TextPoints base encode chars token.rawText) (hkind : token.kind = .slide) :
    (do let out ← lowerSlideStep base encode chars isClassic state token
        ok (stateView out)) =
      ok (referenceStep isClassic measureDurSec (stateView state) token) := by
  rw [referenceStep_slide isClassic measureDurSec (stateView state) token hkind]
  unfold lowerSlideStep
  rw [lowerToken_equiv base encode chars isClassic state.nextIndex token points]
  simp only [bind_tc_ok, appendSlide_view]
  rfl

theorem lowerSlideStep_rejected (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (state : ForwardState) (token : RawNoteToken)
    (points : TextPoints base encode chars token.rawText)
    (hrejected : lowerSlideToken state.nextIndex token isClassic = none) :
    lowerSlideStep base encode chars isClassic state token = ok state := by
  simp only [lowerSlideStep, lowerToken_equiv base encode chars
    isClassic state.nextIndex token points, hrejected, bind_tc_ok, appendSlide]

/-- Success appends once to each of the three slide outputs and advances the
unbounded note index once. All non-slide lists are preserved exactly. -/
theorem lowerSlideStep_accepted (base : SlideParserProof.Frontend)
    (encode : List Char → String) (chars : Slice Char)
    (isClassic : Bool) (state : ForwardState) (token : RawNoteToken)
    (slide : NormalizedSlide) (parsed : SlideNoteSemantics)
    (points : TextPoints base encode chars token.rawText)
    (haccepted : lowerSlideToken state.nextIndex token isClassic = some (slide, parsed)) :
    lowerSlideStep base encode chars isClassic state token =
      ok { state with
        nextIndex := state.nextIndex + 1
        slides := state.slides ++ [slide]
        debug := state.debug ++ [{ noteIndex := state.nextIndex, rawText := token.rawText }]
        semantics := state.semantics ++ [parsed] } := by
  simp only [lowerSlideStep, lowerToken_equiv base encode chars
    isClassic state.nextIndex token points, haccepted, bind_tc_ok, appendSlide]

end Verification.NormalizationFrontendProof
