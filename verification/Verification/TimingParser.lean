import Verification.Generated
import LnmaiCore.Simai.Timing
import Batteries.Tactic.OpenPrivate

/-!
Proofs for the runtime-used timing_parser kernels. The declarations below
target the unified Generated.lean and have passed Lean elaboration and
transitive axiom checks:

* prepend_open_loop current result index -> Result (Vec Char)
* extract_brackets_loop chars result inside current index -> Result (Vec (Vec Char))
* duration_route fields -> Result DurationRoute

The copy loop uses Vec.index with SliceIndexUsizeSlice, reduced by the existing
Vec.index_slice_index theorem. The scanner sets inside to true on '[' and false
on ']'; the branch proofs account for those normalized Boolean states. The
route's two-field branch reads field 0 and tests Vec.len against zero.

The reference is the original Timing.lean, including its nested-'[' behavior.
There are no whole-scanner or whole-parser equivalence premises. The routing
interpreter deliberately calls the original Lean numeric functions: its theorem
proves dispatch only, not Rust BigInt/Rat arithmetic or numeric parsing.

Still open: Rust str::chars/String collection, split('#') representation,
allocation bounds, arbitrary-precision numeric parsing, normalized Rat
representation, aggregation, and quantization. The empty-field check uses the
modeled Vec::len primitive rather than an opaque Vec::is_empty accessor.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai

set_option maxHeartbeats 1000000

namespace Verification.TimingParserProof

open private parseNdMicros parseSecondsMicros parseRatioOrSecondsMicros
  durationBpm parseDurationInnerMicros from LnmaiCore.Simai.Timing

/-- Rust buffers are forward lists; the reference accumulates both in reverse. -/
def bracketResume (remaining : List Char) (inside : Bool)
    (current : List Char) (pieces : List (List Char)) : List String :=
  extractBracketContents.loop remaining inside current.reverse
    (pieces.map String.ofList).reverse

theorem bracket_resume_nil (inside : Bool)
    (current : List Char) (pieces : List (List Char)) :
    bracketResume [] inside current pieces = pieces.map String.ofList := by
  simp [bracketResume, extractBracketContents.loop]

/-- In particular '[' inside a bracket is prepended, not appended. -/
theorem bracket_resume_cons (c : Char) (remaining : List Char) (inside : Bool)
    (current : List Char) (pieces : List (List Char)) :
    bracketResume (c :: remaining) inside current pieces =
      if c = '[' then
        if inside then bracketResume remaining inside ('[' :: current) pieces
        else bracketResume remaining true [] pieces
      else if c = ']' then
        if inside then bracketResume remaining false [] (pieces ++ [current])
        else bracketResume remaining inside current pieces
      else if inside then bracketResume remaining inside (current ++ [c]) pieces
      else bracketResume remaining inside current pieces := by
  by_cases hopen : c = '['
  · subst c
    cases inside <;> simp [bracketResume, extractBracketContents.loop]
  · by_cases hclose : c = ']'
    · subst c
      cases inside <;> simp [bracketResume, extractBracketContents.loop]
    · cases inside <;> simp [bracketResume, extractBracketContents.loop, hopen, hclose]

/-- Copying preserves the exact prefix and leaves room for the leading '['. -/
theorem prepend_open_loop_spec (current initial : alloc.vec.Vec Char)
    (hsize : current.val.length < Usize.max) (hinitial : initial.val = ['[']) :
    timing_parser.prepend_open_loop current initial 0#usize
      ⦃ out => out.val = '[' :: current.val ⦄ := by
  unfold timing_parser.prepend_open_loop
  apply loop.spec_decr_nat (fun p => current.val.length - p.2.val)
    (fun p => p.2.val ≤ current.val.length ∧
      p.1.val = '[' :: current.val.take p.2.val)
  · rintro ⟨copied, index⟩ ⟨hi, hcopy⟩
    dsimp only at *
    unfold timing_parser.prepend_open_loop.body
    dsimp only
    split
    · rename_i hmore
      have hindex : index.val < current.val.length := by simpa using hmore
      have hcopiedLength : copied.val.length = index.val + 1 := by
        simp [hcopy, Nat.min_eq_left hi]
      simp only [alloc.vec.Vec.index_slice_index]
      step with alloc.vec.Vec.index_usize_spec current index hindex as ⟨c, hc⟩
      step as ⟨copied', hcopied'⟩
      step as ⟨next, hnext⟩
      refine ⟨by scalar_tac, ?_, by scalar_tac⟩
      simp only [hnext, hcopied', hcopy, hc,
        List.take_succ_eq_append_getElem hindex, List.cons_append]
    · rename_i hdone
      have hfull : current.val.length ≤ index.val := by simpa using hdone
      simpa [List.take_of_length_le hfull] using hcopy
  · simp [hinitial]

/-- Uses only push/index and an explicit copy loop; no Vec.insert model. -/
theorem prepend_open_equiv (current : alloc.vec.Vec Char)
    (hsize : current.val.length < Usize.max) :
    timing_parser.prepend_open current ⦃ out => out.val = '[' :: current.val ⦄ := by
  unfold timing_parser.prepend_open
  step as ⟨initial, hinitial⟩
  exact prepend_open_loop_spec current initial hsize (by simpa using hinitial)

/-- Each iteration keeps all vector lengths within the processed prefix.
The strict size premise is a representation bound, not a grammar restriction. -/
theorem extract_brackets_loop_spec (chars : Slice Char)
    (hsize : chars.val.length < Usize.max) :
    timing_parser.extract_brackets_loop chars (alloc.vec.Vec.new _) false
      (alloc.vec.Vec.new _) 0#usize
      ⦃ out => out.val.length ≤ chars.val.length ∧
        out.val.map (fun piece => String.ofList piece.val) =
          bracketResume chars.val false [] [] ⦄ := by
  unfold timing_parser.extract_brackets_loop
  apply loop.spec_decr_nat (fun p => chars.val.length - p.2.2.2.val)
    (fun p => p.2.2.2.val ≤ chars.val.length ∧
      p.2.2.1.val.length ≤ p.2.2.2.val ∧
      p.1.val.length ≤ p.2.2.2.val ∧
      bracketResume (chars.val.drop p.2.2.2.val) p.2.1 p.2.2.1.val
        (p.1.val.map (·.val)) = bracketResume chars.val false [] [])
  · rintro ⟨pieces, inside, current, index⟩ ⟨hi, hc, hp, hinv⟩
    dsimp only at *
    unfold timing_parser.extract_brackets_loop.body
    dsimp only
    split
    · rename_i hmore
      have hindex : index.val < chars.val.length := by simpa using hmore
      step as ⟨c, hchar⟩
      rw [List.drop_eq_getElem_cons hindex, ← hchar, bracket_resume_cons] at hinv
      split
      · rename_i hopen
        simp [hopen] at hinv
        split
        · rename_i hinside
          step with prepend_open_equiv current (by scalar_tac) as ⟨current', hcurrent'⟩
          step as ⟨next, hnext⟩
          simp_all
          scalar_tac
        · rename_i houtside
          simp only [bind_tc_ok]
          step as ⟨next, hnext⟩
          simp_all
          scalar_tac
      · rename_i hopen
        split
        · rename_i hclose
          simp [hopen, hclose] at hinv
          split
          · rename_i hinside
            step as ⟨pieces', hpieces'⟩
            step as ⟨next, hnext⟩
            simp_all
            scalar_tac
          · rename_i houtside
            simp only [bind_tc_ok]
            step as ⟨next, hnext⟩
            simp_all
            scalar_tac
        · rename_i hclose
          simp [hopen, hclose] at hinv
          split
          · rename_i hinside
            step as ⟨current', hcurrent'⟩
            step as ⟨next, hnext⟩
            simp_all
            scalar_tac
          · rename_i houtside
            simp only [bind_tc_ok]
            step as ⟨next, hnext⟩
            simp_all
            scalar_tac
    · rename_i hdone
      have hempty : chars.val.drop index.val = [] :=
        List.drop_eq_nil_of_le (by simpa using hdone)
      simp_all [bracket_resume_nil, List.map_map, Function.comp_def]
      scalar_tac
  · simp

/-- Main scanner target is the original Lean definition on the represented text. -/
theorem extract_brackets_equiv (chars : Slice Char)
    (hsize : chars.val.length < Usize.max) :
    timing_parser.extract_brackets chars
      ⦃ out => out.val.map (fun piece => String.ofList piece.val) =
        extractBracketContents (String.ofList chars.val) ⦄ := by
  unfold timing_parser.extract_brackets
  step with extract_brackets_loop_spec chars hsize as ⟨out, hlen, hout⟩
  simpa [bracketResume, extractBracketContents] using hout

/-- This boundary states character representation only, not scanner equivalence. -/
theorem extract_brackets_text_equiv (chars : Slice Char) (token : String)
    (hchars : chars.val = token.toList) (hsize : chars.val.length < Usize.max) :
    (do let out ← timing_parser.extract_brackets chars
        ok (out.val.map (fun piece => String.ofList piece.val))) =
      ok (extractBracketContents token) := by
  obtain ⟨out, hout, hval⟩ := spec_imp_exists (extract_brackets_equiv chars hsize)
  simp only [hout, bind_tc_ok, hval]
  simp [hchars]

abbrev DurationRoute := timing_parser.DurationRoute

/-- Strict empty-string dispatch occurs before any trimming or numeric parsing. -/
def routeForFields (fields : List String) : DurationRoute :=
  match fields with
  | [_] => .CurrentRatioOrSeconds 0#usize
  | [first, _] =>
      if first = "" then .SecondsOnly 1#usize
      else .CustomRatioOrSeconds 0#usize 1#usize
  | [_, _, _] => .CurrentRatioOrSeconds 2#usize
  | [_, _, _, _] => .CustomRatioOnly 2#usize 3#usize
  | _ => .Reject

/-- Every field read performed by the runtime interpreter has a valid index. -/
def RouteInBounds (length : Nat) : DurationRoute → Prop
  | .Reject => True
  | .CurrentRatioOrSeconds timing | .SecondsOnly timing => timing.val < length
  | .CustomRatioOrSeconds bpm timing | .CustomRatioOnly bpm timing =>
      bpm.val < length ∧ timing.val < length

theorem route_for_fields_bounds (fields : List String) :
    RouteInBounds fields.length (routeForFields fields) := by
  rcases fields with _ | ⟨a, fields⟩
  · simp [routeForFields, RouteInBounds]
  rcases fields with _ | ⟨b, fields⟩
  · simp [routeForFields, RouteInBounds]
  rcases fields with _ | ⟨c, fields⟩
  · by_cases ha : a = "" <;> simp [routeForFields, RouteInBounds, ha]
  rcases fields with _ | ⟨d, fields⟩
  · simp [routeForFields, RouteInBounds]
  rcases fields with _ | ⟨e, fields⟩ <;> simp [routeForFields, RouteInBounds]

/-- Proves the real kernel's field-count/emptiness tests, including its index-0
read. The only nontrivial read occurs with exactly two fields. No grammar
premise or whole-function adapter law is used. Vec::len = 0 is the precise
empty-field check; the Rust code deliberately avoids Vec::is_empty. -/
theorem duration_route_equiv (fields : Slice (alloc.vec.Vec Char)) :
    timing_parser.duration_route fields =
      ok (routeForFields (fields.val.map (fun field => String.ofList field.val))) := by
  rcases fields with ⟨fields, hbound⟩
  rcases fields with _ | ⟨a, fields⟩
  · simp [timing_parser.duration_route, routeForFields, UScalar.eq_equiv]
  rcases fields with _ | ⟨b, fields⟩
  · simp [timing_parser.duration_route, routeForFields, UScalar.eq_equiv]
  rcases fields with _ | ⟨c, fields⟩
  · by_cases ha : a.val = []
    all_goals simp [timing_parser.duration_route, routeForFields, UScalar.eq_equiv,
      Slice.index_usize, List.length_eq_zero_iff, String.ofList_eq_empty_iff, ha]
  rcases fields with _ | ⟨d, fields⟩
  · simp [timing_parser.duration_route, routeForFields, UScalar.eq_equiv]
  rcases fields with _ | ⟨e, fields⟩
  · simp [timing_parser.duration_route, routeForFields, UScalar.eq_equiv]
  · have h1 : (a :: b :: c :: d :: e :: fields).length ≠ 1 := by
      simp only [List.length_cons] <;> omega
    have h2 : (a :: b :: c :: d :: e :: fields).length ≠ 2 := by
      simp only [List.length_cons] <;> omega
    have h3 : (a :: b :: c :: d :: e :: fields).length ≠ 3 := by
      simp only [List.length_cons] <;> omega
    have h4 : (a :: b :: c :: d :: e :: fields).length ≠ 4 := by
      simp only [List.length_cons] <;> omega
    simp [timing_parser.duration_route, routeForFields, UScalar.eq_equiv, h1, h2, h3, h4]

theorem duration_route_indices_safe (fields : Slice (alloc.vec.Vec Char)) :
    timing_parser.duration_route fields
      ⦃ route => RouteInBounds fields.val.length route ⦄ := by
  rw [duration_route_equiv]
  simpa only [spec_ok, List.length_map] using
    route_for_fields_bounds (fields.val.map (fun field => String.ofList field.val))

/-- This is a Lean interpretation of the returned route, not an abstraction
assumed equal to the Rust numeric parser. Option indexing is total, and the
preceding theorem separately establishes the runtime's indexing bounds. -/
def evalDurationRoute (currentBpm : Rat) (fields : List String) :
    DurationRoute → Option Rat
  | .Reject => none
  | .CurrentRatioOrSeconds timing =>
      fields[timing.val]?.bind (parseRatioOrSecondsMicros currentBpm)
  | .SecondsOnly timing => fields[timing.val]?.bind parseSecondsMicros
  | .CustomRatioOrSeconds bpm timing => do
      let custom ← fields[bpm.val]?
      let text ← fields[timing.val]?
      parseRatioOrSecondsMicros (durationBpm currentBpm custom) text
  | .CustomRatioOnly bpm timing => do
      let custom ← fields[bpm.val]?
      let text ← fields[timing.val]?
      parseNdMicros (durationBpm currentBpm custom) text

/-- Factor the original definition; the four-field branch has no seconds fallback. -/
theorem eval_duration_route_reference (currentBpm : Rat) (inner : String) :
    evalDurationRoute currentBpm (inner.splitOn "#")
      (routeForFields (inner.splitOn "#")) =
      parseDurationInnerMicros currentBpm inner := by
  unfold parseDurationInnerMicros
  generalize inner.splitOn "#" = fields
  rcases fields with _ | ⟨a, fields⟩
  · simp [routeForFields, evalDurationRoute]
  rcases fields with _ | ⟨b, fields⟩
  · simp [routeForFields, evalDurationRoute]
  rcases fields with _ | ⟨c, fields⟩
  · by_cases ha : a = "" <;> simp [routeForFields, evalDurationRoute, ha]
  rcases fields with _ | ⟨d, fields⟩
  · simp [routeForFields, evalDurationRoute]
  rcases fields with _ | ⟨e, fields⟩ <;> simp [routeForFields, evalDurationRoute]

/-- The runtime routing kernel interpreted with the reference numeric functions. -/
def routedReference (currentBpm : Rat) (fields : Slice (alloc.vec.Vec Char)) :
    Result (Option Rat) := do
  let route ← timing_parser.duration_route fields
  ok (evalDurationRoute currentBpm
    (fields.val.map (fun field => String.ofList field.val)) route)

/-- hfields is only the split/character representation boundary. It does not
assert numeric parsing, routing, aggregation, or quantization equivalence. -/
theorem duration_route_reference_equiv (currentBpm : Rat) (inner : String)
    (fields : Slice (alloc.vec.Vec Char))
    (hfields : fields.val.map (fun field => String.ofList field.val) = inner.splitOn "#") :
    routedReference currentBpm fields = ok (parseDurationInnerMicros currentBpm inner) := by
  unfold routedReference
  rw [duration_route_equiv]
  simp only [bind_tc_ok, hfields]
  rw [eval_duration_route_reference]

/-- Public Lean target, still using reference quantization rather than claiming
the BigInt/Rat boundary has been discharged. -/
theorem duration_route_duration_reference_equiv (currentBpm : Rat) (inner : String)
    (fields : Slice (alloc.vec.Vec Char))
    (hfields : fields.val.map (fun field => String.ofList field.val) = inner.splitOn "#") :
    (do let total ← routedReference currentBpm fields
        ok (total.map Time.durationFromRatMicros)) =
      ok (parseDurationInner currentBpm inner) := by
  rw [duration_route_reference_equiv currentBpm inner fields hfields]
  rfl

end Verification.TimingParserProof
