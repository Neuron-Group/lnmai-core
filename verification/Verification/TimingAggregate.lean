import Verification.TimingParser

/-!
Runtime-used timing aggregation and first-success scanning, related to the
original Timing.lean duration and star-wait definitions.

The sum loop state is `(Option T, Usize)`; the find loop state is `Usize`.
Both loops return `Option T`. The find body's successful `some _` branch
returns `done o` directly. The extracted kernels are imported from Generated.

The sum invariant is constructed from the empty prefix and one callback at a
time. Failed parses preserve the accumulator; the first success initializes it;
later successes use one exact addition. Quantization occurs after that loop.
The find invariant records only failed earlier entries, and a successful body
returns without an increment or another callback. No whole-fold/find equation
is an assumption.

Explicit adapter boundaries: callback/closure capture, chars/String collection,
one-inner parsing (including split and numeric interpretation), Rust BigInt/Rat
representation and addition, one final quantization, and the default beat.
The final interpretations call the actual extracted bracket scanner and
aggregation kernels, with the original Timing.lean definitions as targets.
The concrete runtime closure and String representations still need the
explicit adapter contracts; these interpretations do not discharge them.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai

set_option maxHeartbeats 1000000

namespace Verification.TimingAggregateProof

open private parseDurationInnerMicros from LnmaiCore.Simai.Timing

/-- The reference's four cases, without a synthetic additive identity. -/
def sumStep {I V : Type} (parse : I → Option V) (add : V → V → V)
    (total : Option V) (input : I) : Option V :=
  match total, parse input with
  | some sum, some value => some (add sum value)
  | some sum, none => some sum
  | none, some value => some value
  | none, none => none

/-- A single callback at a valid position, not an equation for a whole fold. -/
def ParseAgrees {I T V P : Type}
    (call : core.ops.function.Fn P Usize (Option T)) (parse : P)
    (inputs : List I) (model : I → Option V) (decode : T → V) : Prop :=
  ∀ (index : Usize) (pref : List I) (head : I) (suffix : List I),
    inputs = pref ++ head :: suffix → pref.length = index.val →
    call.call parse index ⦃ out => out.map decode = model head ⦄

def SumPrefix {I T V : Type} (inputs : List I) (model : I → Option V)
    (add : V → V → V) (decode : T → V) (state : Option T × Usize) : Prop :=
  ∃ pref rest : List I,
    inputs = pref ++ rest ∧ pref.length = state.2.val ∧
    pref.foldl (sumStep model add) none = state.1.map decode

theorem sum_prefix_advance {I T V : Type}
    (inputs : List I) (model : I → Option V) (add : V → V → V)
    (decode : T → V) (pref : List I) (head : I) (suffix : List I)
    (total totalNext : Option T) (index next : Usize)
    (hinputs : inputs = pref ++ head :: suffix) (hindex : pref.length = index.val)
    (hprefix : pref.foldl (sumStep model add) none = total.map decode)
    (hnext : next.val = index.val + 1)
    (hstep : totalNext.map decode = sumStep model add (total.map decode) head) :
    SumPrefix inputs model add decode (totalNext, next) := by
  refine ⟨pref ++ [head], suffix, ?_, ?_, ?_⟩
  · simpa only [List.append_assoc, List.singleton_append] using hinputs
  · simp [hindex, hnext]
  · simpa only [List.foldl_append, List.foldl_cons, List.foldl_nil, hprefix]
      using hstep.symm

/-- None preserves the exact accumulator, then advances once. -/
theorem sum_body_none {T P A : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (len : Usize) (parse : P) (add : A) (total : Option T) (index next : Usize)
    (hmore : index < len) (hparse : parseCall.call parse index = ok none)
    (hnext : index + 1#usize = ok next) :
    timing_aggregate.sum_optional_indexed_loop.body parseCall addCall
      len parse add total index = ok (.cont (total, next)) := by
  simp [timing_aggregate.sum_optional_indexed_loop.body, hmore, hparse, hnext]

/-- The first exact value is kept directly; addition is not called. -/
theorem sum_body_first {T P A : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (len : Usize) (parse : P) (add : A) (index next : Usize) (value : T)
    (hmore : index < len) (hparse : parseCall.call parse index = ok (some value))
    (hnext : index + 1#usize = ok next) :
    timing_aggregate.sum_optional_indexed_loop.body parseCall addCall
      len parse add none index = ok (.cont (some value, next)) := by
  simp [timing_aggregate.sum_optional_indexed_loop.body, hmore, hparse, hnext]

theorem sum_body_add {T P A : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (len : Usize) (parse : P) (add : A) (sum value combined : T)
    (index next : Usize) (hmore : index < len)
    (hparse : parseCall.call parse index = ok (some value))
    (hadd : addCall.call add (sum, value) = ok combined)
    (hnext : index + 1#usize = ok next) :
    timing_aggregate.sum_optional_indexed_loop.body parseCall addCall
      len parse add (some sum) index = ok (.cont (some combined, next)) := by
  simp [timing_aggregate.sum_optional_indexed_loop.body, hmore, hparse, hadd, hnext]

/-- The numeric relation is pointwise and can decode an arbitrary Rust type.
No associativity or commutativity premise hides a different evaluation order. -/
theorem sum_optional_indexed_spec {I T V P A : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (len : Usize) (parse : P) (add : A)
    (inputs : List I) (model : I → Option V) (combine : V → V → V)
    (decode : T → V) (hlen : len.val = inputs.length)
    (hparse : ParseAgrees parseCall parse inputs model decode)
    (hadd : ∀ left right, addCall.call add (left, right)
      ⦃ out => decode out = combine (decode left) (decode right) ⦄) :
    timing_aggregate.sum_optional_indexed parseCall addCall len parse add
      ⦃ out => out.map decode = inputs.foldl (sumStep model combine) none ⦄ := by
  unfold timing_aggregate.sum_optional_indexed timing_aggregate.sum_optional_indexed_loop
  apply loop.spec_decr_nat (fun state => len.val - state.2.val)
    (SumPrefix inputs model combine decode)
  · rintro ⟨total, index⟩ ⟨pref, rest, hinputs, hindex, hprefix⟩
    dsimp only at *
    have hlength : len.val = pref.length + rest.length := by
      rw [hlen, hinputs, List.length_append]
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons head suffix =>
          have hsafe : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
          obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
            (UScalar.add_spec (x := index) (y := 1#usize) hsafe)
          have hsucc : next.val = index.val + 1 := by simpa using hnextVal
          have hdecr : len.val - next.val < len.val - index.val := by omega
          have advance := sum_prefix_advance inputs model combine decode pref head
            suffix total
          obtain ⟨parsed, hcallback, hparsed⟩ := spec_imp_exists
            (hparse index pref head suffix hinputs hindex)
          cases parsed with
          | none =>
              have hmodel : model head = none := by simpa using hparsed.symm
              rw [sum_body_none parseCall addCall len parse add total index next
                hmore hcallback hnext]
              have hcontinue : SumPrefix inputs model combine decode (total, next) ∧
                  len.val - next.val < len.val - index.val := by
                refine ⟨advance total index next hinputs hindex hprefix hsucc ?_, hdecr⟩
                cases total <;> simp [sumStep, hmodel]
              simpa using hcontinue
          | some value =>
              have hmodel : model head = some (decode value) := by
                simpa using hparsed.symm
              cases total with
              | none =>
                  rw [sum_body_first parseCall addCall len parse add index next value
                    hmore hcallback hnext]
                  have hcontinue : SumPrefix inputs model combine decode (some value, next) ∧
                      len.val - next.val < len.val - index.val := by
                    refine ⟨advance (some value) index next hinputs hindex hprefix hsucc ?_, hdecr⟩
                    simp [sumStep, hmodel]
                  simpa using hcontinue
              | some sum =>
                  obtain ⟨combined, hcombine, hcombined⟩ := spec_imp_exists (hadd sum value)
                  rw [sum_body_add parseCall addCall len parse add sum value combined
                    index next hmore hcallback hcombine hnext]
                  have hcontinue : SumPrefix inputs model combine decode (some combined, next) ∧
                      len.val - next.val < len.val - index.val := by
                    refine ⟨advance (some combined) index next hinputs hindex hprefix hsucc ?_, hdecr⟩
                    simp [sumStep, hmodel, hcombined]
                  simpa using hcontinue
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head suffix => simp only [List.length_cons] at hlength; omega
      have hpref : inputs = pref := by simpa [hempty] using hinputs
      have hpost : total.map decode = inputs.foldl (sumStep model combine) none := by
        simpa only [hpref] using hprefix.symm
      simpa [timing_aggregate.sum_optional_indexed_loop.body, hmore] using hpost
  · exact ⟨[], inputs, by simp, by simp, rfl⟩

/-- The only quantizer call is after the complete exact sum, in its Some branch. -/
theorem sum_then_map_indexed_spec {I T U V W P A Q : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (quantizeCall : core.ops.function.Fn Q T U)
    (len : Usize) (parse : P) (add : A) (quantize : Q)
    (inputs : List I) (model : I → Option V) (combine : V → V → V)
    (decode : T → V) (decodeOut : U → W) (finish : V → W)
    (hlen : len.val = inputs.length)
    (hparse : ParseAgrees parseCall parse inputs model decode)
    (hadd : ∀ left right, addCall.call add (left, right)
      ⦃ out => decode out = combine (decode left) (decode right) ⦄)
    (hfinish : ∀ value, quantizeCall.call quantize value
      ⦃ out => decodeOut out = finish (decode value) ⦄) :
    timing_aggregate.sum_then_map_indexed parseCall addCall quantizeCall
      len parse add quantize
      ⦃ out => out.map decodeOut =
        (inputs.foldl (sumStep model combine) none).map finish ⦄ := by
  obtain ⟨total, htotal, htotalVal⟩ := spec_imp_exists
    (sum_optional_indexed_spec parseCall addCall len parse add inputs model
      combine decode hlen hparse hadd)
  unfold timing_aggregate.sum_then_map_indexed
  simp only [htotal, bind_tc_ok]
  cases total with
  | none => simp [← htotalVal]
  | some value =>
      obtain ⟨out, hout, houtVal⟩ := spec_imp_exists (hfinish value)
      simp [hout, ← htotalVal, houtVal]

/-- A find callback is constrained only after a completely failing prefix. -/
def FirstParseAgrees {I T V P : Type}
    (call : core.ops.function.Fn P Usize (Option T)) (parse : P)
    (inputs : List I) (model : I → Option V) (decode : T → V) : Prop :=
  ∀ (index : Usize) (pref : List I) (head : I) (suffix : List I),
    inputs = pref ++ head :: suffix → pref.length = index.val →
    pref.findSome? model = none →
    call.call parse index ⦃ out => out.map decode = model head ⦄

def FindPrefix {I V : Type} (inputs : List I) (model : I → Option V)
    (index : Usize) : Prop :=
  ∃ pref rest : List I, inputs = pref ++ rest ∧
    pref.length = index.val ∧ pref.findSome? model = none

def FirstPost {I T V : Type} (inputs : List I) (model : I → Option V)
    (decode : T → V) : Option T → Prop
  | none => inputs.findSome? model = none
  | some value => ∃ (pref : List I) (head : I) (suffix : List I),
      inputs = pref ++ head :: suffix ∧ pref.findSome? model = none ∧
      model head = some (decode value)

theorem first_post_equiv {I T V : Type} (inputs : List I) (model : I → Option V)
    (decode : T → V) (out : Option T) (hpost : FirstPost inputs model decode out) :
    out.map decode = inputs.findSome? model := by
  cases out with
  | none => exact hpost.symm
  | some value =>
      obtain ⟨pref, head, suffix, hinputs, hprefix, hhead⟩ := hpost
      simp [hinputs, List.findSome?_append, List.findSome?_cons, hprefix, hhead]

/-- No increment and no later parse occurs on this branch. -/
theorem first_body_success {T P : Type}
    (call : core.ops.function.Fn P Usize (Option T))
    (len : Usize) (parse : P) (index : Usize) (value : T) (hmore : index < len)
    (hparse : call.call parse index = ok (some value)) :
    timing_aggregate.first_some_indexed_loop.body call len parse index =
      ok (.done (some value)) := by
  simp [timing_aggregate.first_some_indexed_loop.body, hmore, hparse]

theorem first_body_none {T P : Type}
    (call : core.ops.function.Fn P Usize (Option T))
    (len : Usize) (parse : P) (index next : Usize) (hmore : index < len)
    (hparse : call.call parse index = ok none) (hnext : index + 1#usize = ok next) :
    timing_aggregate.first_some_indexed_loop.body call len parse index =
      ok (.cont next) := by
  simp [timing_aggregate.first_some_indexed_loop.body, hmore, hparse, hnext]

theorem first_some_indexed_spec {I T V P : Type}
    (call : core.ops.function.Fn P Usize (Option T))
    (len : Usize) (parse : P) (inputs : List I) (model : I → Option V)
    (decode : T → V) (hlen : len.val = inputs.length)
    (hparse : FirstParseAgrees call parse inputs model decode) :
    timing_aggregate.first_some_indexed call len parse ⦃ FirstPost inputs model decode ⦄ := by
  unfold timing_aggregate.first_some_indexed timing_aggregate.first_some_indexed_loop
  apply loop.spec_decr_nat (fun index => len.val - index.val) (FindPrefix inputs model)
  · intro index hinv
    obtain ⟨pref, rest, hinputs, hindex, hprefix⟩ := hinv
    have hlength : len.val = pref.length + rest.length := by
      rw [hlen, hinputs, List.length_append]
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons head suffix =>
          obtain ⟨parsed, hcallback, hparsed⟩ := spec_imp_exists
            (hparse index pref head suffix hinputs hindex hprefix)
          cases parsed with
          | some value =>
              have hmodel : model head = some (decode value) := by
                simpa using hparsed.symm
              rw [first_body_success call len parse index value hmore hcallback]
              have hpost : FirstPost inputs model decode (some value) :=
                ⟨pref, head, suffix, hinputs, hprefix, hmodel⟩
              simpa using hpost
          | none =>
              have hmodel : model head = none := by simpa using hparsed.symm
              have hsafe : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
              obtain ⟨next, hnext, hnextVal⟩ := spec_imp_exists
                (UScalar.add_spec (x := index) (y := 1#usize) hsafe)
              have hsucc : next.val = index.val + 1 := by simpa using hnextVal
              rw [first_body_none call len parse index next hmore hcallback hnext]
              have hcontinue : FindPrefix inputs model next ∧
                  len.val - next.val < len.val - index.val := by
                constructor
                · refine ⟨pref ++ [head], suffix, ?_, ?_, ?_⟩
                  · simpa only [List.append_assoc, List.singleton_append] using hinputs
                  · simp [hindex, hsucc]
                  · simp [List.findSome?_append, List.findSome?_cons, hprefix, hmodel]
                · omega
              simpa using hcontinue
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head suffix => simp only [List.length_cons] at hlength; omega
      have hpref : inputs = pref := by simpa [hempty] using hinputs
      have hpost : FirstPost inputs model decode none := by simpa [FirstPost, hpref] using hprefix
      simpa [timing_aggregate.first_some_indexed_loop.body, hmore] using hpost
  · exact ⟨[], inputs, by simp, by simp, rfl⟩

theorem first_some_indexed_equiv {I T V P : Type}
    (call : core.ops.function.Fn P Usize (Option T))
    (len : Usize) (parse : P) (inputs : List I) (model : I → Option V)
    (decode : T → V) (hlen : len.val = inputs.length)
    (hparse : FirstParseAgrees call parse inputs model decode) :
    timing_aggregate.first_some_indexed call len parse
      ⦃ out => out.map decode = inputs.findSome? model ⦄ := by
  obtain ⟨out, hout, hpost⟩ := spec_imp_exists
    (first_some_indexed_spec call len parse inputs model decode hlen hparse)
  simpa [hout] using first_post_equiv inputs model decode out hpost

/-- Naming the original lambda does not change its parser or numeric branches. -/
def customWait (bpm : Rat) (inner : String) : Option LnmaiCore.Duration :=
  match inner.splitOn "#" with
  | [customBpm, _] =>
      (parseRatString? customBpm).map fun value =>
        if value > 0 then beatSec value else beatSec bpm
  | [wait, _, _] | [wait, _, _, _] =>
      (parseSecondsRatString? wait).map Time.durationFromSecondsRat
  | _ => none

theorem duration_fold_reference (bpm : Rat) (token : String) :
    ((extractBracketContents token).foldl
      (sumStep (parseDurationInnerMicros bpm) (· + ·)) none).map
        Time.durationFromRatMicros = parseDurationSpec bpm token := by
  unfold parseDurationSpec
  apply congrArg (fun total : Option Rat => total.map Time.durationFromRatMicros)
  apply congrArg (fun step : Option Rat → String → Option Rat =>
    (extractBracketContents token).foldl step none)
  funext total inner
  cases total <;> cases hparsed : parseDurationInnerMicros bpm inner <;>
    simp [sumStep, hparsed]

theorem star_find_reference (bpm : Rat) (token : String) :
    some (((extractBracketContents token).findSome? (customWait bpm)).getD
      (beatSec bpm)) = parseStarWaitSpec bpm token := rfl

/-- Scanner output is consumed by the actual shared sum-and-quantize kernel.
parseFor represents the runtime closure after collecting each buffer as a String. -/
def durationSpecRuntime {T U P A Q : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (quantizeCall : core.ops.function.Fn Q T U)
    (chars : Slice Char) (parseFor : alloc.vec.Vec (alloc.vec.Vec Char) → P)
    (add : A) (quantize : Q) : Result (Option U) := do
  let pieces ← timing_parser.extract_brackets chars
  timing_aggregate.sum_then_map_indexed parseCall addCall quantizeCall
    pieces.len (parseFor pieces) add quantize

/-- Conditional only on character representation and atomic parse/numeric laws.
The scanner, full fold, evaluation order, and final map are proved compositionally. -/
theorem parse_duration_spec_equiv {T U P A Q : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (addCall : core.ops.function.Fn A (T × T) T)
    (quantizeCall : core.ops.function.Fn Q T U)
    (chars : Slice Char) (parseFor : alloc.vec.Vec (alloc.vec.Vec Char) → P)
    (add : A) (quantize : Q) (decode : T → Rat) (decodeOut : U → LnmaiCore.Duration)
    (bpm : Rat) (token : String)
    (hchars : chars.val = token.toList) (hsize : chars.val.length < Usize.max)
    (hparse : ∀ pieces, ParseAgrees parseCall (parseFor pieces)
      (pieces.val.map (fun piece => String.ofList piece.val))
      (parseDurationInnerMicros bpm) decode)
    (hadd : ∀ left right, addCall.call add (left, right)
      ⦃ out => decode out = decode left + decode right ⦄)
    (hfinish : ∀ value, quantizeCall.call quantize value
      ⦃ out => decodeOut out = Time.durationFromRatMicros (decode value) ⦄) :
    durationSpecRuntime parseCall addCall quantizeCall chars parseFor add quantize
      ⦃ out => out.map decodeOut = parseDurationSpec bpm token ⦄ := by
  obtain ⟨pieces, hscanner, hpieces⟩ := spec_imp_exists
    (TimingParserProof.extract_brackets_equiv chars hsize)
  have hcontents : pieces.val.map (fun piece => String.ofList piece.val) =
      extractBracketContents token := by simpa [hchars] using hpieces
  unfold durationSpecRuntime
  simp only [hscanner, bind_tc_ok]
  have hsum := sum_then_map_indexed_spec parseCall addCall quantizeCall pieces.len
    (parseFor pieces) add quantize
    (pieces.val.map (fun piece => String.ofList piece.val))
    (parseDurationInnerMicros bpm) (· + ·) decode decodeOut Time.durationFromRatMicros
    (by simp) (hparse pieces) hadd hfinish
  simpa only [hcontents, duration_fold_reference] using hsum

/-- The fallback callback is evaluated only when every custom parse failed. -/
def starWaitSpecRuntime {T P F : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (fallbackCall : core.ops.function.Fn F Unit T)
    (chars : Slice Char) (parseFor : alloc.vec.Vec (alloc.vec.Vec Char) → P)
    (fallback : F) : Result (Option T) := do
  let pieces ← timing_parser.extract_brackets chars
  let custom ← timing_aggregate.first_some_indexed parseCall pieces.len (parseFor pieces)
  match custom with
  | some value => ok (some value)
  | none =>
      let value ← fallbackCall.call fallback ()
      ok (some value)

theorem parse_star_wait_spec_equiv {T P F : Type}
    (parseCall : core.ops.function.Fn P Usize (Option T))
    (fallbackCall : core.ops.function.Fn F Unit T)
    (chars : Slice Char) (parseFor : alloc.vec.Vec (alloc.vec.Vec Char) → P)
    (fallback : F) (decode : T → LnmaiCore.Duration) (bpm : Rat) (token : String)
    (hchars : chars.val = token.toList) (hsize : chars.val.length < Usize.max)
    (hparse : ∀ pieces, FirstParseAgrees parseCall (parseFor pieces)
      (pieces.val.map (fun piece => String.ofList piece.val)) (customWait bpm) decode)
    (hfallback : fallbackCall.call fallback () ⦃ out => decode out = beatSec bpm ⦄) :
    starWaitSpecRuntime parseCall fallbackCall chars parseFor fallback
      ⦃ out => out.map decode = parseStarWaitSpec bpm token ⦄ := by
  obtain ⟨pieces, hscanner, hpieces⟩ := spec_imp_exists
    (TimingParserProof.extract_brackets_equiv chars hsize)
  have hcontents : pieces.val.map (fun piece => String.ofList piece.val) =
      extractBracketContents token := by simpa [hchars] using hpieces
  obtain ⟨custom, hcustom, hcustomVal⟩ := spec_imp_exists
    (first_some_indexed_equiv parseCall pieces.len (parseFor pieces)
      (pieces.val.map (fun piece => String.ofList piece.val)) (customWait bpm)
      decode (by simp) (hparse pieces))
  have href : parseStarWaitSpec bpm token = some ((custom.map decode).getD (beatSec bpm)) := by
    rw [← star_find_reference, ← hcontents, ← hcustomVal]
  unfold starWaitSpecRuntime
  simp only [hscanner, bind_tc_ok, hcustom]
  cases custom with
  | some value => simp [href]
  | none =>
      obtain ⟨value, hvalue, hvalueVal⟩ := spec_imp_exists hfallback
      simp [hvalue, href, hvalueVal]

end Verification.TimingAggregateProof
