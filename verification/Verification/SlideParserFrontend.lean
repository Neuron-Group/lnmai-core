import Verification.SlideParser
import Verification.SlideParserComposition
import Verification.ShapeAdapters
import Verification.ShapeSolver
import Verification.Tokenize
import Init.Data.String.Lemmas.Pattern.Find.Char
import Init.Data.String.Lemmas.Pattern.Find.String

/-!
Character reads and just-type parsing, including the complete diagnostic value.

These theorems target the runtime-used slide_parser_frontend kernel and its
Aeneas extraction. They compose character reads, command scans, just-type
selection, and the shape solver with the original Lean parser.

The remaining representation boundaries are explicit: Rust UTF-8 decoding into
a bounded character slice, Natural-to-usize conversion, and allocation/decimal
formatting of diagnostics. Digit recognition, index bounds, scan loops, command
priority, and fallible-operation order are not assumed as whole-function laws.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.Areas Verification.ShapeProof

set_option maxHeartbeats 1000000

namespace Verification.SlideParserFrontendProof

open private sanitizeSlideText from LnmaiCore.Simai.SlideParser

open private readDigitAt readStartLaneAt readEndAreaAt
  keyPosToOuterSlot? keyPosToOuterSensorArea?
  outerSlotIsRightHalf outerSlotIsUpperHalf relativeEndFromTyped
  from LnmaiCore.Simai.Shape

abbrev KernelReadError := slide_parser_frontend.ReadError
abbrev KernelJustError := slide_parser_frontend.JustError

/-- Only primitive string representation/formatting lives in this adapter. -/
structure Diagnostics where
  text : List Char → String
  index : Nat → String

def canonicalDiagnostics : Diagnostics := ⟨String.ofList, toString⟩

def readErrorWith (d : Diagnostics) (chars : List Char) (index : Nat)
    (issue : KernelReadError) : ParseError :=
  { kind := .invalidSyntax
    rawText := d.text chars
    message := match issue with
      | .MissingDigit => "missing digit at " ++ d.index index
      | .ExpectedDigit => "expected digit at " ++ d.index index
      | .InvalidStartLane => "invalid start lane at " ++ d.index index
      | .InvalidEndArea => "invalid end area at " ++ d.index index
    span := none }

def readError := readErrorWith canonicalDiagnostics

/-- Includes kind, entire raw character sequence, exact decimal index, and span. -/
theorem read_error_adapter_equiv (d : Diagnostics) (chars : List Char) (index : Nat)
    (htext : d.text chars = String.ofList chars)
    (hindex : d.index index = toString index) (issue : KernelReadError) :
    readErrorWith d chars index issue = readError chars index issue := by
  cases issue <;> simp [readErrorWith, readError, canonicalDiagnostics, htext, hindex]

def readResult {α β : Type} (project : α → β) (chars : List Char) (index : Nat) :
    core.result.Result α KernelReadError → Except ParseError β
  | .Ok value => .ok (project value)
  | .Err issue => .error (readError chars index issue)

def readResultWith {α β : Type} (d : Diagnostics) (project : α → β)
    (chars : List Char) (index : Nat) :
    core.result.Result α KernelReadError → Except ParseError β
  | .Ok value => .ok (project value)
  | .Err issue => .error (readErrorWith d chars index issue)

theorem read_result_adapter_equiv {α β : Type} (d : Diagnostics) (project : α → β)
    (chars : List Char) (index : Nat)
    (htext : d.text chars = String.ofList chars)
    (hindex : d.index index = toString index) (out : core.result.Result α KernelReadError) :
    readResultWith d project chars index out = readResult project chars index out := by
  cases out with
  | Ok value => rfl
  | Err issue =>
      exact congrArg Except.error (read_error_adapter_equiv d chars index htext hindex issue)

def digitRead (chars : List Char) (index : Nat) (found : Option Char) :
    Except ParseError Nat :=
  match found with
  | none => .error (readError chars index .MissingDigit)
  | some c => match digitToNat? c with
    | none => .error (readError chars index .ExpectedDigit)
    | some n => .ok n

def startRead (chars : List Char) (index : Nat) (found : Option Char) :
    Except ParseError OuterSlot := do
  let pos ← digitRead chars index found
  match OuterSlot.ofIndex? (pos - 1) with
  | some slot => pure slot
  | none => .error (readError chars index .InvalidStartLane)

def endRead (chars : List Char) (index : Nat) (found : Option Char) :
    Except ParseError SensorArea := do
  let pos ← digitRead chars index found
  match OuterSlot.ofIndex? (pos - 1) with
  | some slot => pure slot.toOuterSensorArea
  | none => .error (readError chars index .InvalidEndArea)

theorem digit_read_reference (chars : List Char) (index : Nat) :
    digitRead chars index (getAt? chars index) = readDigitAt chars index := by
  unfold digitRead readDigitAt
  cases getAt? chars index with
  | none => rfl
  | some c =>
      dsimp only
      cases digitToNat? c <;> rfl

theorem start_read_reference (chars : List Char) (index : Nat) :
    startRead chars index (getAt? chars index) = parseStartLaneAt chars index := by
  simp only [startRead, parseStartLaneAt, readStartLaneAt, digit_read_reference,
    keyPosToOuterSlot?]
  cases readDigitAt chars index <;> rfl

theorem end_read_reference (chars : List Char) (index : Nat) :
    endRead chars index (getAt? chars index) = parseEndAreaAt chars index := by
  simp only [endRead, parseEndAreaAt, readEndAreaAt, digit_read_reference,
    keyPosToOuterSensorArea?]
  cases readDigitAt chars index with
  | error err => rfl
  | ok pos =>
      simp only [Bind.bind, Except.bind]
      cases OuterSlot.ofIndex? (pos - 1) <;> rfl

theorem read_digit_equiv (chars : List Char) (index : Nat) (found : Option Char) :
    (do let out ← slide_parser_frontend.read_digit found
        ok (readResult UScalar.val chars index out)) =
      ok (digitRead chars index found) := by
  cases found with
  | none => rfl
  | some c =>
      obtain ⟨n, hn, hval⟩ := exists_of_project_eq _ _ _ (digit_to_nat_equiv c)
      simp only [slide_parser_frontend.read_digit, hn, bind_tc_ok, digitRead, ← hval]
      cases n <;> rfl

theorem read_start_lane_equiv (chars : List Char) (index : Nat) (found : Option Char) :
    (do let out ← slide_parser_frontend.read_start_lane found
        ok (readResult toLnmOuterSlot chars index out)) =
      ok (startRead chars index found) := by
  obtain ⟨r, hr, hval⟩ := exists_of_project_eq _ _ _ (read_digit_equiv chars index found)
  unfold slide_parser_frontend.read_start_lane startRead
  rw [hr, ← hval]
  cases r with
  | Err issue => rfl
  | Ok pos =>
      simp only [bind_tc_ok, bind_ok, lift, readResult, pure, Except.pure, Bind.bind, Except.bind]
      rw [outerSlot_of_index_equiv, Verification.TokenizeProof.saturating_sub_one_val]
      cases OuterSlot.ofIndex? (pos.val - 1) <;>
        simp [readResult, toLnmOuterSlot_ofLnmOuterSlot]

theorem read_end_area_equiv (chars : List Char) (index : Nat) (found : Option Char) :
    (do let out ← slide_parser_frontend.read_end_area found
        ok (readResult toLnmSensorArea chars index out)) =
      ok (endRead chars index found) := by
  obtain ⟨r, hr, hval⟩ := exists_of_project_eq _ _ _ (read_digit_equiv chars index found)
  unfold slide_parser_frontend.read_end_area endRead
  rw [hr, ← hval]
  cases r with
  | Err issue => rfl
  | Ok pos =>
      simp only [bind_tc_ok, bind_ok, lift, readResult, pure, Except.pure, Bind.bind, Except.bind]
      rw [outerSlot_of_index_equiv, Verification.TokenizeProof.saturating_sub_one_val]
      cases OuterSlot.ofIndex? (pos.val - 1) <;>
        simp [readResult, outerSlot_to_outer_sensor_area_equiv]

theorem get_at_reference (chars : List Char) (index : Nat) :
    getAt? chars index = chars[index]? := by
  induction index generalizing chars with
  | zero => cases chars <;> rfl
  | succ index ih =>
      cases chars with
      | nil => rfl
      | cons c rest => simpa [getAt?] using ih rest

theorem get_at_equiv (chars : Slice Char) (index : Usize) :
    slide_parser_frontend.get_at chars index = ok (getAt? chars.val index.val) := by
  unfold slide_parser_frontend.get_at
  by_cases hi : index.val < chars.val.length
  · obtain ⟨c, hc, hval⟩ := spec_imp_exists (Slice.index_usize_spec chars index hi)
    simp [hi, hc, hval, get_at_reference]
  · simp [hi, get_at_reference]

/-- The actual Natural overflow branch is a missing character, not a truncation. -/
def getAtRuntime (encode : Nat → Option Usize) (chars : Slice Char) (index : Nat) :
    Result (Option Char) :=
  match encode index with
  | some small => slide_parser_frontend.get_at chars small
  | none => ok none

theorem get_at_unbounded_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) (index : Nat) :
    getAtRuntime encode chars index = ok (getAt? chars.val index) := by
  have hn := hencode index
  cases he : encode index with
  | some small =>
      simp only [he] at hn
      simpa only [getAtRuntime, he, hn] using get_at_equiv chars small
  | none =>
      simp only [he] at hn
      have hout : ¬ index < chars.val.length := by omega
      simp [getAtRuntime, he, get_at_reference, hout]

def readStartRuntime (encode : Nat → Option Usize) (chars : Slice Char) (index : Nat) :
    Result (Except ParseError OuterSlot) := do
  let found ← getAtRuntime encode chars index
  let out ← slide_parser_frontend.read_start_lane found
  ok (readResult toLnmOuterSlot chars.val index out)

def readEndRuntime (encode : Nat → Option Usize) (chars : Slice Char) (index : Nat) :
    Result (Except ParseError SensorArea) := do
  let found ← getAtRuntime encode chars index
  let out ← slide_parser_frontend.read_end_area found
  ok (readResult toLnmSensorArea chars.val index out)

theorem read_start_lane_at_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) (index : Nat) :
    readStartRuntime encode chars index = ok (parseStartLaneAt chars.val index) := by
  unfold readStartRuntime
  rw [get_at_unbounded_equiv encode hencode chars hsize]
  simpa only [bind_tc_ok, start_read_reference] using
    read_start_lane_equiv chars.val index (getAt? chars.val index)

theorem read_end_area_at_equiv (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) (index : Nat) :
    readEndRuntime encode chars index = ok (parseEndAreaAt chars.val index) := by
  unfold readEndRuntime
  rw [get_at_unbounded_equiv encode hencode chars hsize]
  simpa only [bind_tc_ok, end_read_reference] using
    read_end_area_equiv chars.val index (getAt? chars.val index)

/-- These are the complete runtime adapters, including the primitive rendering
of the raw character list and the arbitrary-size decimal index. -/
def readStartRuntimeWith (d : Diagnostics) (encode : Nat → Option Usize)
    (chars : Slice Char) (index : Nat) : Result (Except ParseError OuterSlot) := do
  let found ← getAtRuntime encode chars index
  let out ← slide_parser_frontend.read_start_lane found
  ok (readResultWith d toLnmOuterSlot chars.val index out)

def readEndRuntimeWith (d : Diagnostics) (encode : Nat → Option Usize)
    (chars : Slice Char) (index : Nat) : Result (Except ParseError SensorArea) := do
  let found ← getAtRuntime encode chars index
  let out ← slide_parser_frontend.read_end_area found
  ok (readResultWith d toLnmSensorArea chars.val index out)

theorem read_start_lane_at_with_equiv (d : Diagnostics) (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) (index : Nat)
    (htext : d.text chars.val = String.ofList chars.val)
    (hindex : d.index index = toString index) :
    readStartRuntimeWith d encode chars index = ok (parseStartLaneAt chars.val index) := by
  simpa only [readStartRuntimeWith, readStartRuntime,
    read_result_adapter_equiv d toLnmOuterSlot chars.val index htext hindex] using
      read_start_lane_at_equiv encode hencode chars hsize index

theorem read_end_area_at_with_equiv (d : Diagnostics) (encode : Nat → Option Usize)
    (hencode : EndpointConversion encode) (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) (index : Nat)
    (htext : d.text chars.val = String.ofList chars.val)
    (hindex : d.index index = toString index) :
    readEndRuntimeWith d encode chars index = ok (parseEndAreaAt chars.val index) := by
  simpa only [readEndRuntimeWith, readEndRuntime,
    read_result_adapter_equiv d toLnmSensorArea chars.val index htext hindex] using
      read_end_area_at_equiv encode hencode chars hsize index

/-- The suffix interpretation for a scan which remembers whether the preceding
character matched. This handles overlapping runs such as ppp as well. -/
def doubleScan (target : Char) : Bool → List Char → Bool
  | _, [] => false
  | previous, c :: rest =>
      if previous && (c == target) then true else doubleScan target (c == target) rest

def factsForChars (chars : List Char) : slide_parser_frontend.JustFacts :=
  { circle_right := decide ('>' ∈ chars)
    circle_left := decide ('<' ∈ chars)
    circle_up := decide ('^' ∈ chars)
    turn := decide ('V' ∈ chars)
    wifi := decide ('w' ∈ chars)
    qq := doubleScan 'q' false chars
    pp := doubleScan 'p' false chars }

def factsForText (text : String) : slide_parser_frontend.JustFacts :=
  { circle_right := text.contains '>'
    circle_left := text.contains '<'
    circle_up := text.contains '^'
    turn := text.contains 'V'
    wifi := text.contains 'w'
    qq := text.contains "qq"
    pp := text.contains "pp" }

theorem double_scan_iff (target : Char) (chars : List Char) (previous : Bool) :
    doubleScan target previous chars = true ↔
      (previous = true ∧ chars.head? = some target) ∨ [target, target] <:+: chars := by
  induction chars generalizing previous with
  | nil => simp [doubleScan]
  | cons c rest ih =>
      by_cases hc : c = target
      · subst c
        cases previous <;>
          simp [doubleScan, ih, List.infix_cons_iff,
            List.singleton_prefix_iff_head?_eq_some]
      · simp [doubleScan, hc, Ne.symm hc, ih, List.infix_cons_iff,
          List.singleton_prefix_iff_head?_eq_some]

theorem double_scan_string_equiv (text : String) (target : Char) :
    doubleScan target false text.toList = text.contains (String.ofList [target, target]) := by
  apply Bool.eq_iff_iff.mpr
  rw [double_scan_iff, String.contains_string_iff]
  simp

theorem facts_for_chars_equiv (text : String) :
    factsForChars text.toList = factsForText text := by
  simp only [factsForChars, factsForText, String.contains_char_eq]
  rw [double_scan_string_equiv text 'q', double_scan_string_equiv text 'p']

/-- An early-return scan is proved for every slice, not only parsed slide text.
The explicit size premise also discharges the advancing usize index. -/
theorem contains_char_spec (chars : Slice Char) (target : Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_frontend.contains_char chars target
      ⦃ out => out = decide (target ∈ chars.val) ⦄ := by
  unfold slide_parser_frontend.contains_char slide_parser_frontend.contains_char_loop
  apply loop.spec_decr_nat (fun index => chars.val.length - index.val)
    (fun index => index.val ≤ chars.val.length ∧
      decide (target ∈ chars.val.drop index.val) = decide (target ∈ chars.val))
  · intro index hinv
    obtain ⟨hi, hresume⟩ := hinv
    unfold slide_parser_frontend.contains_char_loop.body
    dsimp only
    split
    · rename_i hmore
      have hindex : index.val < chars.val.length := by simpa using hmore
      step as ⟨c, hc⟩
      rw [List.drop_eq_getElem_cons hindex, ← hc] at hresume
      split
      · rename_i hmatch
        have heq : c = target := by simpa using hmatch
        simpa [heq] using hresume
      · rename_i hmiss
        have hcne : c ≠ target := by simpa using hmiss
        have hresume' : decide (target ∈ chars.val.drop (index.val + 1)) =
            decide (target ∈ chars.val) := by
          simpa only [List.mem_cons, Ne.symm hcne, false_or] using hresume
        step as ⟨next, hnext⟩
        refine ⟨by scalar_tac, ?_, by scalar_tac⟩
        simpa only [hnext] using hresume'
    · rename_i hdone
      have hempty : chars.val.drop index.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hdone)
      simpa [hempty] using hresume
  · simp

theorem contains_char_equiv (chars : Slice Char) (target : Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_frontend.contains_char chars target = ok (decide (target ∈ chars.val)) := by
  obtain ⟨out, hout, hval⟩ := spec_imp_exists (contains_char_spec chars target hsize)
  simpa only [hval] using hout

theorem contains_double_spec (chars : Slice Char) (target : Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_frontend.contains_double chars target
      ⦃ out => out = doubleScan target false chars.val ⦄ := by
  unfold slide_parser_frontend.contains_double slide_parser_frontend.contains_double_loop
  apply loop.spec_decr_nat (fun p => chars.val.length - p.2.val)
    (fun p => p.2.val ≤ chars.val.length ∧
      doubleScan target p.1 (chars.val.drop p.2.val) = doubleScan target false chars.val)
  · rintro ⟨previous, index⟩ ⟨hi, hresume⟩
    dsimp only at *
    unfold slide_parser_frontend.contains_double_loop.body
    dsimp only
    split
    · rename_i hmore
      have hindex : index.val < chars.val.length := by simpa using hmore
      step as ⟨c, hc⟩
      rw [List.drop_eq_getElem_cons hindex, ← hc] at hresume
      cases previous <;> by_cases hmatch : c = target
      all_goals simp only [Bool.beq_eq_decide_eq, hmatch, beq_iff_eq, decide_true, decide_false,
        Bool.false_eq_true, Bool.false_and, Bool.true_and,
        ↓reduceIte, bind_tc_ok]
      all_goals first
        | simpa [doubleScan, hmatch] using hresume
        | (step as ⟨next, hnext⟩
           simp_all [doubleScan, Bool.beq_eq_decide_eq]
           scalar_tac)
    · rename_i hdone
      have hempty : chars.val.drop index.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hdone)
      simpa [hempty, doubleScan] using hresume
  · simp

theorem contains_double_equiv (chars : Slice Char) (target : Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_frontend.contains_double chars target =
      ok (doubleScan target false chars.val) := by
  obtain ⟨out, hout, hval⟩ := spec_imp_exists (contains_double_spec chars target hsize)
  simpa only [hval] using hout

theorem just_facts_equiv (chars : Slice Char)
    (hsize : chars.val.length ≤ Usize.max) :
    slide_parser_frontend.just_facts chars = ok (factsForChars chars.val) := by
  simp [slide_parser_frontend.just_facts, contains_char_equiv chars _ hsize,
    contains_double_equiv chars _ hsize, factsForChars]

def justError (chars : List Char) (raw : String) : KernelJustError → ParseError
  | .At issue index => readError chars index.val issue
  | .Shape issue => ShapeProof.toLnmError "" issue
  | .EndNotOuter =>
      { kind := .invalidEndPosition, rawText := raw,
        message := "slide end must be on outer A-ring", span := none }

def projectResult {α β ε : Type} (project : α → β) (error : ε → ParseError) :
    core.result.Result α ε → Except ParseError β
  | .Ok value => .ok (project value)
  | .Err issue => .error (error issue)

def justResult {α β : Type} (project : α → β) (chars : List Char) (raw : String) :=
  projectResult project (justError chars raw)

def justErrorWith (d : Diagnostics) (chars : List Char) (raw : String) :
    KernelJustError → ParseError
  | .At issue index => readErrorWith d chars index.val issue
  | .Shape issue => ShapeProof.toLnmError "" issue
  | .EndNotOuter =>
      { kind := .invalidEndPosition, rawText := raw,
        message := "slide end must be on outer A-ring", span := none }

theorem just_error_adapter_equiv (d : Diagnostics) (chars : List Char) (raw : String)
    (htext : d.text chars = String.ofList chars)
    (hindex : ∀ index, d.index index = toString index) (issue : KernelJustError) :
    justErrorWith d chars raw issue = justError chars raw issue := by
  cases issue with
  | At issue index => exact read_error_adapter_equiv d chars index.val htext (hindex _) issue
  | Shape issue => rfl
  | EndNotOuter => rfl

theorem just_result_adapter_equiv {α β : Type} (d : Diagnostics) (project : α → β)
    (chars : List Char) (raw : String)
    (htext : d.text chars = String.ofList chars)
    (hindex : ∀ index, d.index index = toString index) (out : core.result.Result α KernelJustError) :
    projectResult project (justErrorWith d chars raw) out = justResult project chars raw out := by
  cases out with
  | Ok value => rfl
  | Err issue =>
      exact congrArg Except.error (just_error_adapter_equiv d chars raw htext hindex issue)

def outerRead (raw : String) (area : SensorArea) : Except ParseError OuterSlot :=
  match area.toOuterSlot? with
  | some slot => pure slot
  | none => .error
      { kind := .invalidEndPosition, rawText := raw,
        message := "slide end must be on outer A-ring", span := none }

theorem outer_read_bind {α : Type} (raw : String) (area : SensorArea)
    (next : OuterSlot → Except ParseError α) :
    (do let slot ← outerRead raw area; next slot) =
      match area.toOuterSlot? with
      | some slot => next slot
      | none => .error
          { kind := .invalidEndPosition, rawText := raw,
            message := "slide end must be on outer A-ring", span := none } := by
  cases harea : area.toOuterSlot? <;>
    simp [outerRead, harea, Bind.bind, Except.bind, pure, Except.pure]

theorem just_start_equiv (chars : List Char) (raw : String) (found : Option Char) :
    (do let out ← slide_parser_frontend.just_start found
        ok (justResult toLnmOuterSlot chars raw out)) = ok (startRead chars 0 found) := by
  obtain ⟨out, hout, hval⟩ := exists_of_project_eq _ _ _
    (read_start_lane_equiv chars 0 found)
  unfold slide_parser_frontend.just_start
  rw [hout, ← hval]
  cases out <;> rfl

theorem just_end_equiv (chars : List Char) (raw : String)
    (found : Option Char) (index : Usize) :
    (do let out ← slide_parser_frontend.just_end found index
        ok (justResult toLnmSensorArea chars raw out)) = ok (endRead chars index.val found) := by
  obtain ⟨out, hout, hval⟩ := exists_of_project_eq _ _ _
    (read_end_area_equiv chars index.val found)
  unfold slide_parser_frontend.just_end
  rw [hout, ← hval]
  cases out <;> rfl

theorem just_outer_equiv (chars : List Char) (raw : String) (area : areas.SensorArea) :
    (do let out ← slide_parser_frontend.just_outer area
        ok (justResult toLnmOuterSlot chars raw out)) = ok (outerRead raw (toLnmSensorArea area)) := by
  unfold slide_parser_frontend.just_outer
  rw [sensorArea_to_outer_slot_equiv]
  cases harea : (toLnmSensorArea area).toOuterSlot? <;>
    simp [justResult, projectResult, justError, outerRead, harea, pure, Except.pure]

/-- Explicit propagation, matching the extracted read_try macro. -/
def rustChain {α β ε : Type} (first : Result (core.result.Result α ε))
    (next : α → Result (core.result.Result β ε)) : Result (core.result.Result β ε) := do
  let out ← first
  match out with
  | .Ok value => next value
  | .Err issue => ok (.Err issue)

theorem project_chain {α β γ δ ε : Type}
    (first : Result (core.result.Result α ε))
    (next : α → Result (core.result.Result β ε))
    (project : α → γ) (project' : β → δ) (error : ε → ParseError)
    (expected : Except ParseError γ) (cont : γ → Except ParseError δ)
    (hfirst : (do let out ← first; ok (projectResult project error out)) = ok expected)
    (hnext : ∀ a, (do let out ← next a; ok (projectResult project' error out)) =
      ok (cont (project a))) :
    (do let out ← (do
          let value ← first
          match value with
          | .Ok value => next value
          | .Err issue => ok (.Err issue))
        ok (projectResult project' error out)) =
      ok (expected >>= cont) := by
  obtain ⟨out, hout, hval⟩ := exists_of_project_eq _ _ _ hfirst
  rw [← hval]
  cases out with
  | Err issue => simp [rustChain, hout, projectResult, Bind.bind, Except.bind]
  | Ok value =>
      simpa [rustChain, hout, projectResult, Bind.bind, Except.bind] using hnext value

theorem just_end_right_equiv (chars : List Char) (raw : String)
    (found : Option Char) (index : Usize) :
    (do
      let out ← rustChain (slide_parser_frontend.just_end found index) (fun area =>
        rustChain (slide_parser_frontend.just_outer area) (fun slot => do
          let result ← shape.outer_slot_is_right_half slot
          ok (.Ok result)))
      ok (justResult id chars raw out)) =
      ok (do let area ← endRead chars index.val found
             let slot ← outerRead raw area
             pure (outerSlotIsRightHalf slot)) := by
  apply project_chain (project := toLnmSensorArea)
  · exact just_end_equiv chars raw found index
  · intro area
    apply project_chain (project := toLnmOuterSlot)
    · exact just_outer_equiv chars raw area
    · intro slot
      rw [outer_slot_is_right_half_equiv]
      rfl

theorem just_end_upper_equiv (chars : List Char) (raw : String)
    (found : Option Char) (index : Usize) :
    (do
      let out ← rustChain (slide_parser_frontend.just_end found index) (fun area =>
        rustChain (slide_parser_frontend.just_outer area) (fun slot => do
          let result ← shape.outer_slot_is_upper_half slot
          ok (.Ok result)))
      ok (justResult id chars raw out)) =
      ok (do let area ← endRead chars index.val found
             let slot ← outerRead raw area
             pure (outerSlotIsUpperHalf slot)) := by
  apply project_chain (project := toLnmSensorArea)
  · exact just_end_equiv chars raw found index
  · intro area
    apply project_chain (project := toLnmOuterSlot)
    · exact just_outer_equiv chars raw area
    · intro slot
      rw [outer_slot_is_upper_half_equiv]
      rfl

theorem just_relative_equiv (chars : List Char) (raw : String)
    (start : areas.OuterSlot) (area : areas.SensorArea) :
    (do
      let relative ← shape.relative_end_from_typed start area
      let out : core.result.Result Bool KernelJustError ← match relative with
        | .Ok pos => ok (.Ok (decide (pos < 4#usize)))
        | .Err issue => ok (.Err (slide_parser_frontend.JustError.Shape issue))
      ok (justResult id chars raw out)) =
      ok (do let pos ← relativeEndFromTyped (toLnmOuterSlot start) (toLnmSensorArea area)
             pure (decide (pos < 4))) := by
  obtain ⟨out, hout, hval⟩ := relative_end_from_typed_spec start area ""
  rw [hout, ← hval]
  cases out <;>
    simp [justResult, projectResult, justError, ShapeProof.toLnmResult,
      UScalar.lt_equiv, pure, Except.pure, Bind.bind, Except.bind]

def observedJust (facts : slide_parser_frontend.JustFacts) (chars : List Char)
    (raw : String) (start end2 end3 : Option Char) : Except ParseError Bool := do
  if facts.circle_right then
    let slot ← startRead chars 0 start
    let _ ← endRead chars 2 end2
    pure (outerSlotIsUpperHalf slot)
  else if facts.circle_left then
    let slot ← startRead chars 0 start
    let _ ← endRead chars 2 end2
    pure (!outerSlotIsUpperHalf slot)
  else if facts.circle_up then
    let slot ← startRead chars 0 start
    let area ← endRead chars 2 end2
    let pos ← relativeEndFromTyped slot area
    pure (decide (pos < 4))
  else if facts.turn then
    let _ ← startRead chars 0 start
    let area ← endRead chars 3 end3
    let slot ← outerRead raw area
    pure (outerSlotIsRightHalf slot)
  else if facts.wifi then
    let _ ← startRead chars 0 start
    let area ← endRead chars 2 end2
    let slot ← outerRead raw area
    pure (outerSlotIsUpperHalf slot)
  else
    let area ← if facts.qq || facts.pp then endRead chars 3 end3 else endRead chars 2 end2
    let slot ← outerRead raw area
    pure (outerSlotIsRightHalf slot)

/-- Arbitrary and conflicting facts are allowed. In particular the default
branch has no start-lane read, and the turn branch does not validate index 2. -/
theorem detect_just_type_observed_equiv (facts : slide_parser_frontend.JustFacts)
    (chars : List Char) (raw : String) (start end2 end3 : Option Char) :
    (do let out ← slide_parser_frontend.detect_just_type_observed facts start end2 end3
        ok (justResult id chars raw out)) = ok (observedJust facts chars raw start end2 end3) := by
  obtain ⟨startOut, hstart, hstartVal⟩ := exists_of_project_eq _ _ _
    (just_start_equiv chars raw start)
  have end2Proof := just_end_equiv chars raw end2 2#usize
  have end3Proof := just_end_equiv chars raw end3 3#usize
  simp only [UScalar.ofNatCore_val_eq] at end2Proof end3Proof
  obtain ⟨end2Out, hend2, hend2Val⟩ := exists_of_project_eq _ _ _ end2Proof
  obtain ⟨end3Out, hend3, hend3Val⟩ := exists_of_project_eq _ _ _ end3Proof
  unfold slide_parser_frontend.detect_just_type_observed observedJust
  by_cases hr : facts.circle_right = true
  · simp only [hr, Bool.false_eq_true, ↓reduceIte]
    rw [hstart, hend2, ← hstartVal, ← hend2Val]
    cases startOut <;> cases end2Out <;>
      simp [justResult, projectResult, outer_slot_is_upper_half_equiv,
        Bind.bind, Std.bind, Except.bind, pure, Except.pure]
  simp only [hr, Bool.false_eq_true, ↓reduceIte]
  by_cases hl : facts.circle_left = true
  · simp only [hl, Bool.false_eq_true, ↓reduceIte]
    rw [hstart, hend2, ← hstartVal, ← hend2Val]
    cases startOut <;> cases end2Out <;>
      simp [justResult, projectResult, outer_slot_is_upper_half_equiv,
        Bind.bind, Std.bind, Except.bind, pure, Except.pure]
  simp only [hl, Bool.false_eq_true, ↓reduceIte]
  by_cases hu : facts.circle_up = true
  · simp only [hu, Bool.false_eq_true, ↓reduceIte]
    rw [hstart, hend2, ← hstartVal, ← hend2Val]
    cases startOut with
    | Err issue => rfl
    | Ok slot =>
      cases end2Out with
      | Err issue => rfl
      | Ok area =>
        obtain ⟨relative, hrelative, hrelativeVal⟩ := relative_end_from_typed_spec slot area ""
        simp only [bind_tc_ok, justResult, projectResult, Bind.bind, Except.bind,
          Std.bind_ok]
        rw [hrelative, ← hrelativeVal]
        cases relative <;>
          simp [justResult, projectResult, justError, ShapeProof.toLnmResult,
            UScalar.lt_equiv, Bind.bind, Std.bind, Except.bind, pure, Except.pure]
  simp only [hu, Bool.false_eq_true, ↓reduceIte]
  by_cases ht : facts.turn = true
  · simp only [ht, Bool.false_eq_true, ↓reduceIte]
    rw [hstart, ← hstartVal]
    cases startOut with
    | Err issue => rfl
    | Ok slot =>
      rw [hend3, ← hend3Val]
      cases end3Out with
      | Err issue => rfl
      | Ok area =>
        obtain ⟨outer, houter, houterVal⟩ := exists_of_project_eq _ _ _
          (just_outer_equiv chars raw area)
        simp only [bind_tc_ok, justResult, projectResult, Bind.bind, Except.bind,
          Std.bind_ok]
        rw [houter, ← houterVal]
        cases outer <;>
          simp [justResult, projectResult, outer_slot_is_right_half_equiv,
            Bind.bind, Std.bind, Except.bind, pure, Except.pure]
  simp only [ht, Bool.false_eq_true, ↓reduceIte]
  by_cases hw : facts.wifi = true
  · simp only [hw, Bool.false_eq_true, ↓reduceIte]
    rw [hstart, ← hstartVal]
    cases startOut with
    | Err issue => rfl
    | Ok slot =>
      rw [hend2, ← hend2Val]
      cases end2Out with
      | Err issue => rfl
      | Ok area =>
        obtain ⟨outer, houter, houterVal⟩ := exists_of_project_eq _ _ _
          (just_outer_equiv chars raw area)
        simp only [bind_tc_ok, justResult, projectResult, Bind.bind, Except.bind,
          Std.bind_ok]
        rw [houter, ← houterVal]
        cases outer <;>
          simp [justResult, projectResult, outer_slot_is_upper_half_equiv,
            Bind.bind, Std.bind, Except.bind, pure, Except.pure]
  simp only [hw, Bool.false_eq_true, ↓reduceIte]
  cases hq : facts.qq <;> cases hp : facts.pp <;>
    simp only [hq, hp, Bool.false_or, Bool.true_or, Bool.false_eq_true, ↓reduceIte]
  all_goals
    first
    | rw [hend2, ← hend2Val]; cases end2Out
    | rw [hend3, ← hend3Val]; cases end3Out
  all_goals try rfl
  all_goals
    rename_i area
    obtain ⟨outer, houter, houterVal⟩ := exists_of_project_eq _ _ _
      (just_outer_equiv chars raw area)
    simp only [bind_tc_ok, justResult, projectResult, Bind.bind, Except.bind,
      Std.bind_ok]
    rw [houter, ← houterVal]
    cases outer <;>
      simp [justResult, projectResult, outer_slot_is_right_half_equiv,
        Bind.bind, Std.bind, Except.bind, pure, Except.pure]

theorem observed_just_reference (text : String) :
    observedJust (factsForText text) text.toList text
      (getAt? text.toList 0) (getAt? text.toList 2) (getAt? text.toList 3) =
      detectJustType text := by
  simp only [observedJust, factsForText, start_read_reference, end_read_reference,
    parseStartLaneAt, parseEndAreaAt, outer_read_bind, detectJustType, pure, Except.pure]
  simp only [Bind.bind, Except.bind]
  repeat' first | rfl | split
  all_goals simp_all

/-- Before relating a Rust string to a Lean string, the whole extracted kernel
already agrees with its character-list interpretation. The raw error text is
an independent adapter input, including for malformed commands. -/
theorem detect_just_type_chars_equiv (chars : Slice Char) (raw : String)
    (hsize : chars.val.length ≤ Usize.max) :
    (do let out ← slide_parser_frontend.detect_just_type chars
        ok (justResult id chars.val raw out)) =
      ok (observedJust (factsForChars chars.val) chars.val raw
        (getAt? chars.val 0) (getAt? chars.val 2) (getAt? chars.val 3)) := by
  unfold slide_parser_frontend.detect_just_type
  rw [just_facts_equiv chars hsize]
  simp only [bind_tc_ok, get_at_equiv]
  exact detect_just_type_observed_equiv (factsForChars chars.val) chars.val raw
    (getAt? chars.val 0) (getAt? chars.val 2) (getAt? chars.val 3)

/-- All lists of Unicode scalar values are covered. The sole text premise is
the UTF-8-to-character-slice relation; scanning/reading/just-type are proved. -/
theorem detect_just_type_equiv (chars : Slice Char) (text : String)
    (hchars : chars.val = text.toList) (hsize : chars.val.length ≤ Usize.max) :
    (do let out ← slide_parser_frontend.detect_just_type chars
        ok (justResult id chars.val text out)) = ok (detectJustType text) := by
  rw [detect_just_type_chars_equiv chars text hsize]
  simpa only [hchars, facts_for_chars_equiv] using congrArg ok (observed_just_reference text)

theorem detect_just_type_with_equiv (d : Diagnostics) (chars : Slice Char) (text : String)
    (hchars : chars.val = text.toList) (hsize : chars.val.length ≤ Usize.max)
    (htext : d.text chars.val = String.ofList chars.val)
    (hindex : ∀ index, d.index index = toString index) :
    (do let out ← slide_parser_frontend.detect_just_type chars
        ok (projectResult id (justErrorWith d chars.val text) out)) =
      ok (detectJustType text) := by
  simpa only [just_result_adapter_equiv d id chars.val text htext hindex] using
    detect_just_type_equiv chars text hchars hsize

/-! Connection to the parser's existing Frontend interface.

The installed fields are character-list interpretations of the kernels above.
The runtime equations below connect them to actual extracted calls, including
diagnostic rendering. They are not chosen to be the original Lean readers.
The base frontend supplies only decoding, command facts, and the solver; this
file neither assumes nor establishes a solver law.
-/

def installReadsAndJust (base : SlideParserProof.Frontend) : SlideParserProof.Frontend :=
  { base with
    readStart := fun chars index => startRead chars index chars[index]?
    readEnd := fun chars index => endRead chars index chars[index]?
    just := fun text =>
      let chars := base.chars text
      observedJust (factsForChars chars) chars text chars[0]? chars[2]? chars[3]? }

theorem installed_read_start_reference (base : SlideParserProof.Frontend)
    (chars : List Char) (index : Nat) :
    (installReadsAndJust base).readStart chars index = parseStartLaneAt chars index := by
  change startRead chars index chars[index]? = _
  rw [← get_at_reference, start_read_reference]

theorem installed_read_end_reference (base : SlideParserProof.Frontend)
    (chars : List Char) (index : Nat) :
    (installReadsAndJust base).readEnd chars index = parseEndAreaAt chars index := by
  change endRead chars index chars[index]? = _
  rw [← get_at_reference, end_read_reference]

theorem installed_read_start_runtime (base : SlideParserProof.Frontend)
    (d : Diagnostics) (encode : Nat → Option Usize) (hencode : EndpointConversion encode)
    (chars : Slice Char) (hsize : chars.val.length ≤ Usize.max) (index : Nat)
    (htext : d.text chars.val = String.ofList chars.val)
    (hindex : d.index index = toString index) :
    readStartRuntimeWith d encode chars index =
      ok ((installReadsAndJust base).readStart chars.val index) := by
  rw [read_start_lane_at_with_equiv d encode hencode chars hsize index htext hindex,
    installed_read_start_reference]

theorem installed_read_end_runtime (base : SlideParserProof.Frontend)
    (d : Diagnostics) (encode : Nat → Option Usize) (hencode : EndpointConversion encode)
    (chars : Slice Char) (hsize : chars.val.length ≤ Usize.max) (index : Nat)
    (htext : d.text chars.val = String.ofList chars.val)
    (hindex : d.index index = toString index) :
    readEndRuntimeWith d encode chars index =
      ok ((installReadsAndJust base).readEnd chars.val index) := by
  rw [read_end_area_at_with_equiv d encode hencode chars hsize index htext hindex,
    installed_read_end_reference]

/-- No whole-reader equation is an assumption of FrontendLaws anymore.
Command facts still belong to the separate command-parsing frontend adapter. -/
theorem installed_frontend_laws (base : SlideParserProof.Frontend) (text : String)
    (hchars : base.chars text = text.toList)
    (hfacts : base.facts text = SlideParserProof.factsForText text) :
    SlideParserProof.FrontendLaws (installReadsAndJust base) text := by
  exact
    { chars_eq := hchars
      facts_eq := hfacts
      readStart_eq := installed_read_start_reference base text.toList 0
      readEnd2_eq := installed_read_end_reference base text.toList 2
      readEnd3_eq := installed_read_end_reference base text.toList 3 }

theorem installed_just_runtime (base : SlideParserProof.Frontend) (d : Diagnostics)
    (chars : Slice Char) (text : String) (hchars : chars.val = base.chars text)
    (hsize : chars.val.length ≤ Usize.max)
    (htext : d.text chars.val = String.ofList chars.val)
    (hindex : ∀ index, d.index index = toString index) :
    (do let out ← slide_parser_frontend.detect_just_type chars
        ok (projectResult id (justErrorWith d chars.val text) out)) =
      ok ((installReadsAndJust base).just text) := by
  simpa only [installReadsAndJust, ← hchars, ← get_at_reference,
    just_result_adapter_equiv d id chars.val text htext hindex] using
      detect_just_type_chars_equiv chars text hsize

theorem installed_just_equiv (base : SlideParserProof.Frontend) (text : String)
    (hchars : base.chars text = text.toList) :
    (installReadsAndJust base).just text = detectJustType text := by
  simpa only [installReadsAndJust, hchars, ← get_at_reference, facts_for_chars_equiv] using
    observed_just_reference text

theorem installed_parse_body_equiv (base : SlideParserProof.Frontend) (text : String)
    (hchars : base.chars text = text.toList)
    (hfacts : base.facts text = SlideParserProof.factsForText text) :
    SlideParserProof.parseBodyRuntime (installReadsAndJust base) text =
      ok (parseSlideBodyFromText text) :=
  SlideParserProof.parse_body_equiv _ text (installed_frontend_laws base text hchars hfacts)

theorem installed_parse_terminal_end_area_equiv (base : SlideParserProof.Frontend)
    (text : String) (hchars : base.chars text = text.toList)
    (hfacts : base.facts text = SlideParserProof.factsForText text) :
    SlideParserProof.parseTerminalRuntime (installReadsAndJust base) text =
      ok (parseTerminalEndArea text) :=
  SlideParserProof.parse_terminal_end_area_equiv _ text
    (installed_frontend_laws base text hchars hfacts)

/-- Normalization is retained as a separate, explicit caller obligation. -/
theorem installed_parse_just_text_equiv (base : SlideParserProof.Frontend)
    (raw sanitized : String) (hsanitized : sanitized = sanitizeSlideText raw)
    (hchars : base.chars sanitized = sanitized.toList) :
    (installReadsAndJust base).just sanitized = parseSlideJustText raw :=
  SlideParserProof.parse_just_text_equiv _ raw sanitized hsanitized
    (installed_just_equiv base sanitized hchars)

/-- Compose the proved readers/just-type frontend with the extracted shape
solver. No whole-reader, just-type or whole-solver equation is a premise. -/
theorem installed_shape_from_text_equiv (base : SlideParserProof.Frontend) (text : String)
    (hchars : base.chars text = text.toList)
    (hfacts : base.facts text = SlideParserProof.factsForText text) :
    SlideParserComposition.shapeFromText (installReadsAndJust base) text =
      ok (detectShapeFromText text) :=
  SlideParserComposition.shape_from_text_equiv _ text
    (installed_frontend_laws base text hchars hfacts)

theorem installed_parse_note_equiv (base : SlideParserProof.Frontend)
    (raw sanitized : String) (start : OuterSlot) (endArea : SensorArea)
    (hsanitized : sanitized = sanitizeSlideText raw)
    (hchars : base.chars sanitized = sanitized.toList)
    (hfacts : base.facts sanitized = SlideParserProof.factsForText sanitized) :
    SlideParserComposition.parseNote (installReadsAndJust base) sanitized start endArea =
      ok (parseSlideNote raw start endArea) :=
  SlideParserComposition.parse_note_equiv _ raw sanitized start endArea hsanitized
    (installed_frontend_laws base sanitized hchars hfacts)
    (installed_just_equiv base sanitized hchars)

theorem installed_parse_shape_text_equiv (base : SlideParserProof.Frontend)
    (raw sanitized : String) (hsanitized : sanitized = sanitizeSlideText raw)
    (hchars : base.chars sanitized = sanitized.toList)
    (hfacts : base.facts sanitized = SlideParserProof.factsForText sanitized) :
    SlideParserComposition.shapeFromText (installReadsAndJust base) sanitized =
      ok (parseSlideShapeText raw) :=
  SlideParserComposition.parse_shape_text_equiv _ raw sanitized hsanitized
    (installed_frontend_laws base sanitized hchars hfacts)

theorem installed_parse_note_from_body_equiv (base : SlideParserProof.Frontend)
    (raw : String) (body : ParsedSlideBody) (endArea : SensorArea)
    (hchars : base.chars raw = raw.toList) :
    SlideParserComposition.parseNoteFromBody (installReadsAndJust base) raw body endArea =
      ok (parseSlideNoteFromBody raw body endArea) :=
  SlideParserComposition.parse_note_from_body_equiv _ raw body endArea
    (installed_just_equiv base raw hchars)

/- Remaining integration obligations, intentionally outside these proofs:
* Rust str::chars / Vec<char> / Slice representation and allocation bounds;
* Natural::to_usize satisfying EndpointConversion, without truncation;
* Rust character collection and Natural Display satisfying Diagnostics;
* command-fact string operations and sanitizeSlideText normalization.
The parent's solver composition supplies hsolve independently. No table payload
or normalization implementation is extracted or assumed correct in this file.
-/

end Verification.SlideParserFrontendProof
