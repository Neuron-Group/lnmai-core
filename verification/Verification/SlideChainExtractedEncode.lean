import Verification.SlideChainExtractedRender

/-!
Actual extraction-3 encode_plans loop. The single primitive String encoding
call is an explicit point law. Rendering, indexed traversal and each Vec push
are proved from the generated functions. This draft has not been compiled.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1200000

namespace Verification.SlideChainExtracted

def decodeSegment {Text : Type} (viewText : Text → String)
    (segment : slide_chain.Segment Text) : SlideChainReference.OriginalSegment :=
  SlideChainReference.mkOriginalSegment (viewText segment.raw_text) segment.has_timing

def decodeSegments {Text : Type} (viewText : Text → String)
    (segments : alloc.vec.Vec (slide_chain.Segment Text)) :
    List SlideChainReference.OriginalSegment :=
  segments.val.map (decodeSegment viewText)

/-- OPEN RUNTIME BOUNDARY: only one primitive Vec<char>-to-Text call. This does
not assume a parser, renderer, segment list, or complete encode_plans result.
The bound is in characters. Actual UTF-8 byte capacity and allocation success
must be discharged when instantiating the Rust encoding closure. -/
def EncodingLaw {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (encode : Encode) (viewText : Text → String) (maxChars : Nat) : Prop :=
  ∀ text : alloc.vec.Vec Char, text.val.length ≤ maxChars →
    call.call encode text ⦃ value => viewText value = String.ofList text.val ⦄

def EncodeInvariant {Text : Type} (chars : List Char)
    (plans : List slide_chain.SegmentPlan) (viewText : Text → String)
    (target : List SlideChainReference.OriginalSegment)
    (state : alloc.vec.Vec (slide_chain.Segment Text) × Usize) : Prop :=
  state.2.val ≤ plans.length ∧
  state.1.val.length + (plans.length - state.2.val) ≤ Usize.max ∧
  decodeSegments viewText state.1 ++
    (plans.drop state.2.val).map (fun plan => SlideChainModel.renderSegment chars (decodePlan plan))
      = target

/-- Generalized loop with arbitrary initial output and suffix index. The
remaining mapped list records segment order, including the has_timing field. -/
theorem encode_plans_loop_spec {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (plans : Slice slide_chain.SegmentPlan) (encode : Encode)
    (viewText : Text → String)
    (initialSegments : alloc.vec.Vec (slide_chain.Segment Text)) (initialIndex : Usize)
    (hplans : ∀ plan ∈ plans.val, RenderablePlan chars.val plan)
    (hencode : EncodingLaw call encode viewText (chars.val.length + 1))
    (hindex : initialIndex.val ≤ plans.val.length)
    (hcapacity : initialSegments.val.length + (plans.val.length - initialIndex.val) ≤ Usize.max) :
    slide_chain.encode_plans_loop call chars plans encode initialSegments initialIndex
      ⦃ out => decodeSegments viewText out = decodeSegments viewText initialSegments ++
        (plans.val.drop initialIndex.val).map
          (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan)) ⦄ := by
  unfold slide_chain.encode_plans_loop
  apply loop.spec_decr_nat
    (fun (state : alloc.vec.Vec (slide_chain.Segment Text) × Usize) =>
      plans.val.length - state.2.val)
    (EncodeInvariant chars.val plans.val viewText
      (decodeSegments viewText initialSegments ++
        (plans.val.drop initialIndex.val).map
          (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan))))
  · rintro ⟨segments, index⟩ ⟨hi, hcap, hinv⟩
    dsimp only at hi hcap hinv ⊢
    by_cases hmore : index < Slice.len plans
    · have hlt : index.val < plans.val.length := by simpa using hmore
      obtain ⟨plan, hget, hp⟩ := spec_imp_exists (Slice.index_usize_spec plans index hlt)
      have hmem : plan ∈ plans.val := List.mem_of_getElem hp.symm
      obtain ⟨hbegin, hstop, hrenderCap⟩ := hplans plan hmem
      obtain ⟨text, hrender, htext⟩ := spec_imp_exists
        (render_plan_spec chars plan hbegin hstop hrenderCap)
      have htextLength : text.val.length ≤ chars.val.length + 1 := by
        rw [htext, SlideChainModel.renderChars_length chars.val (decodePlan plan) hbegin hstop]
        change 1 + (plan.«end».val - plan.«begin».val) ≤ chars.val.length + 1
        omega
      obtain ⟨value, hcall, hvalue⟩ := spec_imp_exists (hencode text htextLength)
      let segment : slide_chain.Segment Text :=
        { raw_text := value, has_timing := plan.has_timing }
      have hsegment : decodeSegment viewText segment =
          SlideChainModel.renderSegment chars.val (decodePlan plan) := by
        simp only [decodeSegment, segment, SlideChainModel.renderSegment, hvalue, htext, decodePlan]
      obtain ⟨appended, hpush, happend⟩ := spec_imp_exists
        (alloc.vec.Vec.push_spec segments segment (by omega))
      have hstepCap : index.val + (1#usize).val ≤ UScalar.max .Usize := by
        have hsize := Slice.length_ineq plans
        scalar_tac
      obtain ⟨next, hnext, hn⟩ := spec_imp_exists
        (UScalar.add_spec (x := index) (y := 1#usize) hstepCap)
      have hnv : next.val = index.val + 1 := by simpa using hn
      have hvalues : decodeSegments viewText appended = decodeSegments viewText segments ++
          [SlideChainModel.renderSegment chars.val (decodePlan plan)] := by
        simp only [decodeSegments, happend, List.map_append, List.map_cons, List.map_nil, hsegment]
      have hrest : plans.val.drop index.val = plan :: plans.val.drop next.val := by
        rw [List.drop_eq_getElem_cons hlt, ← hp, hnv]
      have hkeep : decodeSegments viewText appended ++
          (plans.val.drop next.val).map
            (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan)) =
          decodeSegments viewText initialSegments ++
            (plans.val.drop initialIndex.val).map
              (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan)) := by
        rw [hrest] at hinv
        simpa only [hvalues, List.map_cons, List.append_assoc, List.singleton_append] using hinv
      have hcontinue : EncodeInvariant chars.val plans.val viewText
          (decodeSegments viewText initialSegments ++
            (plans.val.drop initialIndex.val).map
              (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan)))
          (appended, next) ∧ plans.val.length - next.val < plans.val.length - index.val := by
        refine ⟨⟨by omega, ?_, hkeep⟩, by omega⟩
        simp only [happend, List.length_append, List.length_singleton]
        omega
      simpa [slide_chain.encode_plans_loop.body, hmore, hget, hrender,
        hcall, hpush, hnext, segment] using hcontinue
    · have hge : plans.val.length ≤ index.val := by simpa using hmore
      have hout : decodeSegments viewText segments =
          decodeSegments viewText initialSegments ++
            (plans.val.drop initialIndex.val).map
              (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan)) := by
        simpa only [List.drop_eq_nil_of_le hge, List.map_nil, List.append_nil] using hinv
      simpa [slide_chain.encode_plans_loop.body, hmore] using hout
  · exact ⟨hindex, hcapacity, rfl⟩

theorem encode_plans_spec {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (plans : Slice slide_chain.SegmentPlan) (encode : Encode)
    (viewText : Text → String)
    (hplans : ∀ plan ∈ plans.val, RenderablePlan chars.val plan)
    (hencode : EncodingLaw call encode viewText (chars.val.length + 1)) :
    slide_chain.encode_plans call chars plans encode
      ⦃ out => decodeSegments viewText out =
        plans.val.map (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan)) ⦄ := by
  have hcap : (alloc.vec.Vec.new (slide_chain.Segment Text)).val.length +
      (plans.val.length - (0#usize).val) ≤ Usize.max := by
    simpa using Slice.length_ineq plans
  simpa [slide_chain.encode_plans, decodeSegments] using
    encode_plans_loop_spec call chars plans encode viewText (alloc.vec.Vec.new _) 0#usize
      hplans hencode (by simp) hcap

theorem encode_plans_equiv {Text Encode : Type}
    (call : core.ops.function.Fn Encode (alloc.vec.Vec Char) Text)
    (chars : Slice Char) (plans : Slice slide_chain.SegmentPlan) (encode : Encode)
    (viewText : Text → String)
    (hplans : ∀ plan ∈ plans.val, RenderablePlan chars.val plan)
    (hencode : EncodingLaw call encode viewText (chars.val.length + 1)) :
    (do let out ← slide_chain.encode_plans call chars plans encode
        ok (decodeSegments viewText out)) =
      ok (plans.val.map (fun plan => SlideChainModel.renderSegment chars.val (decodePlan plan))) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (encode_plans_spec call chars plans encode viewText hplans hencode)
  simp only [hrun, bind_tc_ok, hpost]

end Verification.SlideChainExtracted
