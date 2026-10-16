import Verification.SlideParserText

/-!
Proofs for the runtime-used indexed timing-point collection loop. These
declarations target the unified Generated.lean and have passed Lean
elaboration and transitive axiom checks.

Observed extraction of `try_map_indexed<T, E, F>`:
  try_map_indexed call len parse
  try_map_indexed_loop call len parse result index
  try_map_indexed_loop.body call len parse result index
The loop state is `(Vec T, Usize)` and its result is
`core.result.Result (Vec T) E`. No whole-batch parsing equation is a premise.

The generic proof needs the callback equation only after a successful prefix.
Its invariant derives output order, index validity, push capacity, and the
strictly decreasing remaining length. An error immediately returns the same
error, with no condition on callbacks after it.

The timing-point interpretation below invokes the actual extracted collector
and the existing Text/Composition interpretations for one note. Concrete Rust
String/record/error representations, allocation, and closure capture remain
adapter boundaries. In particular, this file does not identify an unextracted
runtime closure with its Lean interpretation by assertion.
The kernel grows an initially empty Vec; the old runtime reserved input length
up front. Capacity and out-of-memory behavior are outside this functional model.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.SlideParserProof

set_option maxHeartbeats 1000000

namespace Verification.SlideParserSequenceProof

def toRustResult {T E : Type} : Except E T → core.result.Result T E
  | .ok value => .Ok value
  | .error error => .Err error

def collectionResult {T E : Type} :
    core.result.Result (alloc.vec.Vec T) E → Except E (List T)
  | .Ok values => .ok values.val
  | .Err error => .error error

/-- This is a per-call contract, restricted to the current position after a
successful prefix. It says nothing about an unreachable suffix after an error. -/
def CallbackAgrees {A T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (parse : F) (inputs : List A) (model : A → Except E T) : Prop :=
  ∀ (index : Usize) (pref : List A) (head : A) (suffix : List A)
    (values : List T),
    inputs = pref ++ head :: suffix → pref.length = index.val →
    pref.mapM model = .ok values →
    call.call parse index = ok (toRustResult (model head))

/-- The equality here is an invariant established from the empty prefix and
one successful callback at a time; it is not assumed for the whole input. -/
def PrefixInvariant {A T E : Type} (inputs : List A) (model : A → Except E T)
    (state : alloc.vec.Vec T × Usize) : Prop :=
  ∃ pref rest : List A,
    inputs = pref ++ rest ∧ pref.length = state.2.val ∧
    pref.mapM model = .ok state.1.val ∧ state.1.val.length = state.2.val

theorem prefix_invariant_bounds {A T E : Type} (inputs : List A)
    (model : A → Except E T) (len : Usize) (buffer : alloc.vec.Vec T)
    (index : Usize) (hlen : len.val = inputs.length)
    (hinv : PrefixInvariant inputs model (buffer, index)) :
    index.val ≤ len.val ∧ buffer.val.length = index.val ∧
      (index < len → buffer.val.length < Usize.max ∧
        index.val + 1 ≤ Usize.max) := by
  obtain ⟨pref, rest, hinputs, hindex, _, hbuffer⟩ := hinv
  dsimp only at hindex hbuffer
  have hlength : len.val = pref.length + rest.length := by
    rw [hlen, hinputs, List.length_append]
  have hmax : len.val ≤ Usize.max := by scalar_tac
  refine ⟨by omega, hbuffer, ?_⟩
  intro hmore
  have hlt : index.val < len.val := by simpa using hmore
  constructor <;> omega

/-- Success retains the entire ordered map. Failure identifies the first
failing input and a successful prefix, including the complete error value. -/
def CollectionPost {A T E : Type} (inputs : List A) (model : A → Except E T) :
    core.result.Result (alloc.vec.Vec T) E → Prop
  | .Ok values =>
      inputs.mapM model = .ok values.val ∧ values.val.length = inputs.length
  | .Err error =>
      ∃ (pref : List A) (head : A) (suffix : List A) (values : List T),
        inputs = pref ++ head :: suffix ∧ pref.mapM model = .ok values ∧
        model head = .error error ∧ values.length = pref.length

theorem collection_post_equiv {A T E : Type} (inputs : List A)
    (model : A → Except E T) (result : core.result.Result (alloc.vec.Vec T) E)
    (hpost : CollectionPost inputs model result) :
    collectionResult result = inputs.mapM model := by
  cases result with
  | Ok values => exact hpost.1.symm
  | Err error =>
      obtain ⟨pref, head, suffix, values, hinputs, hprefix, herror, _⟩ := hpost
      simp [collectionResult, hinputs, List.mapM_append, List.mapM_cons,
        hprefix, herror, Bind.bind, Except.bind, pure, Except.pure]

/-- The extracted body stops immediately on an error. Neither a push, an
increment, nor any later callback is needed for this equation. -/
theorem try_map_indexed_body_error {T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (len : Usize) (parse : F) (buffer : alloc.vec.Vec T) (index : Usize)
    (error : E) (hmore : index < len)
    (hcall : call.call parse index = ok (.Err error)) :
    slide_parser_sequence.try_map_indexed_loop.body call len parse buffer index =
      ok (.done (.Err error)) := by
  simp [slide_parser_sequence.try_map_indexed_loop.body, hmore, hcall]

/-- Each successful step appends once and advances by exactly one. Combined
with the initial index zero and body_error, this exposes callback order. -/
theorem try_map_indexed_body_success {T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (len : Usize) (parse : F) (buffer bufferNext : alloc.vec.Vec T)
    (index next : Usize) (value : T) (hmore : index < len)
    (hcall : call.call parse index = ok (.Ok value))
    (hpush : alloc.vec.Vec.push buffer value = ok bufferNext)
    (hnext : index + 1#usize = ok next) :
    slide_parser_sequence.try_map_indexed_loop.body call len parse buffer index =
      ok (.cont (bufferNext, next)) := by
  simp [slide_parser_sequence.try_map_indexed_loop.body, hmore, hcall, hpush, hnext]

/-- Full-domain bounded collection: no assumptions about command validity or
callback results, only their pointwise interpretation at reachable indices. -/
theorem try_map_indexed_spec {A T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (len : Usize) (parse : F) (inputs : List A) (model : A → Except E T)
    (hlen : len.val = inputs.length)
    (hcall : CallbackAgrees call parse inputs model) :
    slide_parser_sequence.try_map_indexed call len parse
      ⦃ CollectionPost inputs model ⦄ := by
  unfold slide_parser_sequence.try_map_indexed
    slide_parser_sequence.try_map_indexed_loop
  apply loop.spec_decr_nat (fun state => len.val - state.2.val)
    (PrefixInvariant inputs model)
  · rintro ⟨buffer, index⟩ hinv
    have hbounds := prefix_invariant_bounds inputs model len buffer index hlen hinv
    obtain ⟨pref, rest, hinputs, hindex, hprefix, hbuffer⟩ := hinv
    have hlength : len.val = pref.length + rest.length := by
      rw [hlen, hinputs, List.length_append]
    dsimp only at *
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil =>
          simp only [List.length_nil, Nat.add_zero] at hlength
          omega
      | cons head suffix =>
          have hcallback := hcall index pref head suffix buffer.val
            hinputs hindex hprefix
          cases hmodel : model head with
          | error error =>
              have herr : call.call parse index = ok (.Err error) := by
                simpa only [hmodel, toRustResult] using hcallback
              rw [try_map_indexed_body_error call len parse buffer index error hmore herr]
              have hpost : CollectionPost inputs model (.Err error) :=
                ⟨pref, head, suffix, buffer.val, hinputs, hprefix, hmodel, by omega⟩
              simpa using hpost
          | ok value =>
              have hok : call.call parse index = ok (.Ok value) := by
                simpa only [hmodel, toRustResult] using hcallback
              obtain ⟨bufferNext, hpush, hpushVal⟩ := spec_imp_exists
                (alloc.vec.Vec.push_spec buffer value (hbounds.2.2 hmore).1)
              have hadd : index.val + (1#usize).val ≤ UScalar.max .Usize := by
                scalar_tac
              obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
                (UScalar.add_spec (x := index) (y := 1#usize) hadd)
              have hsucc : next.val = index.val + 1 := by simpa using hnextVal
              rw [try_map_indexed_body_success call len parse buffer bufferNext
                index next value hmore hok hpush hnext]
              have hcontinue : PrefixInvariant inputs model (bufferNext, next) ∧
                  len.val - next.val < len.val - index.val := by
                constructor
                · refine ⟨pref ++ [head], suffix, ?_, ?_, ?_, ?_⟩
                  · simpa only [List.append_assoc, List.singleton_append] using hinputs
                  · simp [List.length_append, hindex, hsucc]
                  · simp [List.mapM_append, List.mapM_cons, hprefix, hmodel,
                      hpushVal, Bind.bind, Except.bind, pure, Except.pure]
                  · simp [hpushVal, List.length_append, hbuffer, hsucc]
                · omega
              simpa using hcontinue
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head suffix => simp only [List.length_cons] at hlength; omega
      have hpref : inputs = pref := by simpa [hempty] using hinputs
      have hpost : CollectionPost inputs model (.Ok buffer) := by
        refine ⟨?_, ?_⟩
        · simpa only [hpref] using hprefix
        · rw [hpref, hbuffer, hindex]
      simpa [slide_parser_sequence.try_map_indexed_loop.body, hmore] using hpost
  · exact ⟨[], inputs, by simp, by simp, rfl, by simp⟩

theorem try_map_indexed_equiv {A T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (len : Usize) (parse : F) (inputs : List A) (model : A → Except E T)
    (hlen : len.val = inputs.length)
    (hcall : CallbackAgrees call parse inputs model) :
    (do let result ← slide_parser_sequence.try_map_indexed call len parse
        ok (collectionResult result)) = ok (inputs.mapM model) := by
  obtain ⟨result, hresult, hpost⟩ := spec_imp_exists
    (try_map_indexed_spec call len parse inputs model hlen hcall)
  simp only [hresult, bind_tc_ok]
  exact congrArg Result.ok (collection_post_equiv inputs model result hpost)

/-- On failure, every earlier input succeeds and the returned error is the
current input's complete error. Later entries need not satisfy a callback law. -/
theorem try_map_indexed_first_error {A T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (len : Usize) (parse : F) (inputs : List A) (model : A → Except E T)
    (hlen : len.val = inputs.length)
    (hcall : CallbackAgrees call parse inputs model) (error : E)
    (hrun : slide_parser_sequence.try_map_indexed call len parse = ok (.Err error)) :
    ∃ (pref : List A) (head : A) (suffix : List A) (values : List T),
      inputs = pref ++ head :: suffix ∧ pref.mapM model = .ok values ∧
      model head = .error error ∧ values.length = pref.length := by
  obtain ⟨result, hresult, hpost⟩ := spec_imp_exists
    (try_map_indexed_spec call len parse inputs model hlen hcall)
  rw [hrun] at hresult
  cases hresult
  exact hpost

theorem try_map_indexed_success_order {A T E F : Type}
    (call : core.ops.function.Fn F Usize (core.result.Result T E))
    (len : Usize) (parse : F) (inputs : List A) (model : A → Except E T)
    (hlen : len.val = inputs.length)
    (hcall : CallbackAgrees call parse inputs model) (values : alloc.vec.Vec T)
    (hrun : slide_parser_sequence.try_map_indexed call len parse = ok (.Ok values)) :
    inputs.mapM model = .ok values.val ∧ values.val.length = inputs.length := by
  obtain ⟨result, hresult, hpost⟩ := spec_imp_exists
    (try_map_indexed_spec call len parse inputs model hlen hcall)
  rw [hrun] at hresult
  cases hresult
  exact hpost

/-- The original timing-point lambda, factored only for naming. This entry
uses the original text, not parseSlideNote's sanitized text or supplied ends. -/
def timingNote (raw : String) : Except ParseError SlideNoteSemantics := do
  let shape ← detectShapeFromText raw
  let just ← detectJustType raw
  let startSlot ← parseStartLaneAt raw.toList 0
  let endArea ← parseTerminalEndArea raw
  pure {
    rawText := raw, startSlot := startSlot, endArea := endArea,
    shape := shape, isJustRight := just }

/-- The runtime closure's four fallible stages in their actual order. The
terminal call is reached only after shape, just, and start all succeed. -/
def timingNoteRuntime (base : Frontend) (raw : String) :
    Result (Except ParseError SlideNoteSemantics) := do
  let f := SlideParserTextProof.installText base
  let parsed ← SlideParserComposition.shapeFromText f raw
  match parsed with
  | .error error => ok (.error error)
  | .ok shape =>
      match f.just raw with
      | .error error => ok (.error error)
      | .ok just =>
          match f.readStart (f.chars raw) 0 with
          | .error error => ok (.error error)
          | .ok startSlot =>
              let terminal ← parseTerminalRuntime f raw
              ok (do
                let endArea ← terminal
                pure {
                  rawText := raw, startSlot := startSlot, endArea := endArea,
                  shape := shape, isJustRight := just })

/-- Shape, just, start, and terminal laws are composed from the existing
kernel theorems, rather than assumed as a complete note-parser equation. -/
theorem timing_note_equiv (base : Frontend) (raw : String)
    (hchars : base.chars raw = raw.toList) :
    timingNoteRuntime base raw = ok (timingNote raw) := by
  have laws := SlideParserTextProof.installed_frontend_laws base raw hchars
  have hshape := SlideParserComposition.shape_from_text_equiv
    (SlideParserTextProof.installText base) raw laws
  have hjust := SlideParserTextProof.installed_just_equiv base raw hchars
  have hend := SlideParserTextProof.parse_terminal_end_area_equiv base raw hchars
  cases hs : detectShapeFromText raw <;>
    cases hj : detectJustType raw <;>
    cases hstart : parseStartLaneAt raw.toList 0 <;>
    simp [timingNoteRuntime, timingNote, hshape, hjust, laws.chars_eq,
      laws.readStart_eq, hend, hs, hj, hstart, Bind.bind, Except.bind, pure, Except.pure]

/-- Interprets the closure's primitive indexed read and the proved note stages.
The Slice uses interpreted strings; concrete decoding remains a boundary. -/
def timingCallback (base : Frontend) (rawNotes : Slice String) (index : Usize) :
    Result (core.result.Result SlideNoteSemantics ParseError) := do
  let raw ← Slice.index_usize rawNotes index
  let parsed ← timingNoteRuntime base raw
  ok (toRustResult parsed)

theorem timing_callback_equiv_at (base : Frontend) (rawNotes : Slice String)
    (index : Usize) (pref : List String) (raw : String) (suffix : List String)
    (hinputs : rawNotes.val = pref ++ raw :: suffix)
    (hindex : pref.length = index.val)
    (hchars : base.chars raw = raw.toList) :
    timingCallback base rawNotes index = ok (toRustResult (timingNote raw)) := by
  have hbound : index.val < rawNotes.val.length := by
    rw [hinputs, List.length_append, List.length_cons]
    omega
  obtain ⟨item, hlookup, hitem⟩ := spec_imp_exists
    (Slice.index_usize_spec rawNotes index hbound)
  have hraw : item = raw := hitem.trans (List.getElem_of_append hinputs hindex)
  rw [hraw] at hlookup
  simp only [timingCallback, hlookup, bind_tc_ok, timing_note_equiv base raw hchars]

theorem timing_callback_agrees (base : Frontend) (rawNotes : Slice String)
    (hchars : ∀ raw ∈ rawNotes.val, base.chars raw = raw.toList) :
    CallbackAgrees
      (BuiltinFn Usize (core.result.Result SlideNoteSemantics ParseError))
      (timingCallback base rawNotes) rawNotes.val timingNote := by
  intro index pref raw suffix values hinputs hindex _
  change timingCallback base rawNotes index = ok (toRustResult (timingNote raw))
  exact timing_callback_equiv_at base rawNotes index pref raw suffix hinputs hindex
    (hchars raw (by rw [hinputs]; simp))

/-- Final record construction following the extracted collection call. Using
Lean record types here is an interpretation, not a Rust representation proof. -/
def timingPointWithCallback {F : Type}
    (call : core.ops.function.Fn F Usize
      (core.result.Result SlideNoteSemantics ParseError))
    (len : Usize) (parse : F) (timing : TimePoint) (bpm hSpeed : Rat) :
    Result (Except ParseError TimingPointSemantics) := do
  let result ← slide_parser_sequence.try_map_indexed call len parse
  ok (do
    let notes ← collectionResult result
    pure { timing := timing, bpm := bpm, hSpeed := hSpeed, notes := notes })

theorem timing_point_with_callback_equiv {F : Type}
    (call : core.ops.function.Fn F Usize
      (core.result.Result SlideNoteSemantics ParseError))
    (len : Usize) (parse : F) (rawNotes : List String)
    (timing : TimePoint) (bpm hSpeed : Rat)
    (hlen : len.val = rawNotes.length)
    (hcall : CallbackAgrees call parse rawNotes timingNote) :
    timingPointWithCallback call len parse timing bpm hSpeed =
      ok (parseSlideTimingPoint timing bpm hSpeed rawNotes) := by
  obtain ⟨result, hresult, hpost⟩ := spec_imp_exists
    (try_map_indexed_spec call len parse rawNotes timingNote hlen hcall)
  simp only [timingPointWithCallback, hresult, bind_tc_ok,
    collection_post_equiv rawNotes timingNote result hpost]
  rfl

def timingPointRuntime (base : Frontend) (rawNotes : Slice String)
    (timing : TimePoint) (bpm hSpeed : Rat) :
    Result (Except ParseError TimingPointSemantics) :=
  timingPointWithCallback
    (BuiltinFn Usize (core.result.Result SlideNoteSemantics ParseError))
    (Slice.len rawNotes) (timingCallback base rawNotes) timing bpm hSpeed

/-- The concrete indexed model discharges the pointwise callback obligation.
There is no hypothesis equating a whole batch, mapM, or note parser. The
String/enum/diagnostic/allocation adapters described above remain outstanding. -/
theorem parse_timing_point_equiv (base : Frontend) (rawNotes : Slice String)
    (timing : TimePoint) (bpm hSpeed : Rat)
    (hchars : ∀ raw ∈ rawNotes.val, base.chars raw = raw.toList) :
    timingPointRuntime base rawNotes timing bpm hSpeed =
      ok (parseSlideTimingPoint timing bpm hSpeed rawNotes.val) := by
  unfold timingPointRuntime
  exact timing_point_with_callback_equiv _ _ _ _ timing bpm hSpeed
    (by simp) (timing_callback_agrees base rawNotes hchars)

/-- A connection point for the separately interpreted actual closure. Its
premise is the closure body's primitive implementation equation at a valid
index; single-note and whole-sequence correctness are then derived above.
This equation is still an adapter obligation, not an accepted runtime proof. -/
theorem closure_callback_agrees {F : Type}
    (call : core.ops.function.Fn F Usize
      (core.result.Result SlideNoteSemantics ParseError))
    (parse : F) (base : Frontend) (rawNotes : Slice String)
    (hchars : ∀ raw ∈ rawNotes.val, base.chars raw = raw.toList)
    (hclosure : ∀ index : Usize, index.val < rawNotes.val.length →
      call.call parse index = timingCallback base rawNotes index) :
    CallbackAgrees call parse rawNotes.val timingNote := by
  intro index pref raw suffix values hinputs hindex _
  have hbound : index.val < rawNotes.val.length := by
    rw [hinputs, List.length_append, List.length_cons]
    omega
  rw [hclosure index hbound]
  exact timing_callback_equiv_at base rawNotes index pref raw suffix hinputs hindex
    (hchars raw (by rw [hinputs]; simp))

/-- Conditional connection for an interpreted runtime closure. hclosure is
still explicitly required, while callback correctness and ordered collection
correctness are supplied by the preceding proofs. -/
theorem parse_timing_point_with_closure_equiv {F : Type}
    (call : core.ops.function.Fn F Usize
      (core.result.Result SlideNoteSemantics ParseError))
    (parse : F) (base : Frontend) (rawNotes : Slice String)
    (timing : TimePoint) (bpm hSpeed : Rat)
    (hchars : ∀ raw ∈ rawNotes.val, base.chars raw = raw.toList)
    (hclosure : ∀ index : Usize, index.val < rawNotes.val.length →
      call.call parse index = timingCallback base rawNotes index) :
    timingPointWithCallback call (Slice.len rawNotes) parse timing bpm hSpeed =
      ok (parseSlideTimingPoint timing bpm hSpeed rawNotes.val) := by
  exact timing_point_with_callback_equiv call (Slice.len rawNotes) parse
    rawNotes.val timing bpm hSpeed (by simp)
    (closure_callback_agrees call parse base rawNotes hchars hclosure)

end Verification.SlideParserSequenceProof
