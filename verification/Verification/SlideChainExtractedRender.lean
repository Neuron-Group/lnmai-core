import Verification.SlideChainExtractedLoops

/-!
Actual extraction-3 render_plan loop: reads, pushes, order, termination and
the synthetic head. This is an uncompiled proof script. The only capacity
condition below is a character-Vec bound; it is NOT a UTF-8 byte/allocation law.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.SlideChainExtracted

def RenderInvariant (chars : List Char) (stop : Nat) (target : List Char)
    (state : alloc.vec.Vec Char × Usize) : Prop :=
  state.2.val ≤ stop ∧
  state.1.val.length + (stop - state.2.val) ≤ Usize.max ∧
  state.1.val ++ SlideChainModel.sourceRange chars state.2.val stop = target

/-- Generalized accumulator specification for the REAL generated render loop.
Arbitrary manually constructed plans need bounds; parsed plans obtain those
bounds from the source model's proved spans invariant below. -/
theorem render_plan_loop_spec (chars : Slice Char) (stop : Usize)
    (initialText : alloc.vec.Vec Char) (initialCursor : Usize)
    (hbegin : initialCursor.val ≤ stop.val)
    (hstop : stop.val ≤ chars.val.length)
    (hcapacity : initialText.val.length + (stop.val - initialCursor.val) ≤ Usize.max) :
    slide_chain.render_plan_loop chars stop initialText initialCursor
      ⦃ out => out.val = initialText.val ++
        SlideChainModel.sourceRange chars.val initialCursor.val stop.val ⦄ := by
  unfold slide_chain.render_plan_loop
  apply loop.spec_decr_nat
    (fun (state : alloc.vec.Vec Char × Usize) => stop.val - state.2.val)
    (RenderInvariant chars.val stop.val
      (initialText.val ++ SlideChainModel.sourceRange chars.val initialCursor.val stop.val))
  · rintro ⟨text, cursor⟩ ⟨hcursor, hcap, hinv⟩
    dsimp only at hcursor hcap hinv ⊢
    by_cases hmore : cursor < stop
    · have hlt : cursor.val < stop.val := by simpa using hmore
      have hi : cursor.val < chars.val.length := by omega
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
      have hlookup : chars.val[cursor.val]? = some c := by
        simpa only [List.getElem?_eq_getElem hi, hc]
      obtain ⟨appended, hpush, hp⟩ := spec_imp_exists
        (alloc.vec.Vec.push_spec text c (by omega))
      obtain ⟨next, hnext, hn⟩ := spec_imp_exists
        (UScalar.add_spec (x := cursor) (y := 1#usize)
          (NaturalParserProof.scan_index_increment_bound chars cursor hi))
      have hnv : next.val = cursor.val + 1 := by simpa using hn
      have hmove :
          text.val ++ SlideChainModel.sourceRange chars.val cursor.val stop.val =
          appended.val ++ SlideChainModel.sourceRange chars.val next.val stop.val := by
        rw [SlideChainModel.sourceRange_cons chars.val cursor.val stop.val c hlookup (by omega)]
        simp only [hp, hnv, List.append_assoc, List.singleton_append]
      have hcontinue : RenderInvariant chars.val stop.val
          (initialText.val ++ SlideChainModel.sourceRange chars.val initialCursor.val stop.val)
          (appended, next) ∧ stop.val - next.val < stop.val - cursor.val := by
        refine ⟨⟨by omega, ?_, hmove.symm.trans hinv⟩, by omega⟩
        simp only [hp, List.length_append, List.length_singleton]
        omega
      simpa [slide_chain.render_plan_loop.body, hmore, hget, hpush, hnext] using hcontinue
    · have hge : stop.val ≤ cursor.val := by simpa using hmore
      have heq : cursor.val = stop.val := by omega
      have hout : text.val = initialText.val ++
          SlideChainModel.sourceRange chars.val initialCursor.val stop.val := by
        simpa only [heq, SlideChainModel.sourceRange_self, List.append_nil] using hinv
      simpa [slide_chain.render_plan_loop.body, hmore] using hout
  · exact ⟨hbegin, hcapacity, rfl⟩

/-- Bounds and one extra character of capacity suffice for the actual renderer.
No equation about a complete renderer is assumed. -/
theorem render_plan_spec (chars : Slice Char) (plan : slide_chain.SegmentPlan)
    (hbegin : plan.«begin».val ≤ plan.«end».val)
    (hstop : plan.«end».val ≤ chars.val.length)
    (hcapacity : 1 + (plan.«end».val - plan.«begin».val) ≤ Usize.max) :
    slide_chain.render_plan chars plan
      ⦃ out => out.val = SlideChainModel.renderChars chars.val (decodePlan plan) ⦄ := by
  apply exists_imp_spec
  obtain ⟨text, hpush, ht⟩ := spec_imp_exists
    (alloc.vec.Vec.push_spec (alloc.vec.Vec.new Char) plan.head (by simp <;> scalar_tac))
  have htext : text.val = [plan.head] := by simpa using ht
  have hcap : text.val.length + (plan.«end».val - plan.«begin».val) ≤ Usize.max := by
    simpa only [htext, List.length_singleton] using hcapacity
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (render_plan_loop_spec chars plan.«end» text plan.«begin» hbegin hstop hcap)
  refine ⟨out, ?_, ?_⟩
  · simp [slide_chain.render_plan, hpush, hrun]
  · simpa only [htext, List.singleton_append, SlideChainModel.sourceRange,
      SlideChainModel.renderChars, decodePlan] using hpost

theorem render_plan_equiv (chars : Slice Char) (plan : slide_chain.SegmentPlan)
    (hbegin : plan.«begin».val ≤ plan.«end».val)
    (hstop : plan.«end».val ≤ chars.val.length)
    (hcapacity : 1 + (plan.«end».val - plan.«begin».val) ≤ Usize.max) :
    (do let out ← slide_chain.render_plan chars plan
        ok out.val) = ok (SlideChainModel.renderChars chars.val (decodePlan plan)) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
    (render_plan_spec chars plan hbegin hstop hcapacity)
  simp only [hrun, bind_tc_ok, hpost]

/-- Only projections of Linnaeus's spans theorem are needed here. This does
not repeat its parser/source proof. -/
theorem spansFrom_member_bounds (chars : List Char) (plans : List SlideChainModel.Plan)
    (cursor final : Nat) (hspans : SlideChainModel.spansFrom chars cursor plans final)
    (plan : SlideChainModel.Plan) (hmem : plan ∈ plans) :
    plan.begin ≤ plan.stop ∧ plan.stop ≤ chars.length := by
  induction plans generalizing cursor with
  | nil => simp at hmem
  | cons first rest ih =>
      rcases hspans with ⟨hbegin, hprogress, hstop, htail⟩
      rcases List.mem_cons.mp hmem with heq | hrest
      · subst plan
        exact ⟨by omega, hstop⟩
      · exact ih first.stop htail hrest

/-- Well-formedness needed by the actual indexed renderer. Output capacity is
kept as a separate explicit arithmetic boundary. -/
def RenderablePlan (chars : List Char) (plan : slide_chain.SegmentPlan) : Prop :=
  plan.«begin».val ≤ plan.«end».val ∧
  plan.«end».val ≤ chars.length ∧
  1 + (plan.«end».val - plan.«begin».val) ≤ Usize.max

theorem parse_plans_renderable (chars : Slice Char) (head : Char)
    (plans : alloc.vec.Vec slide_chain.SegmentPlan)
    (hsize : chars.val.length < Usize.max)
    (hparsed : SlideChainModel.parsePlans chars.val head = .ok (decodePlans plans)) :
    ∀ plan ∈ plans.val, RenderablePlan chars.val plan := by
  have hspans := SlideChainModel.parsePlans_spans chars.val head (decodePlans plans) hparsed
  intro plan hmem
  have hdecoded : decodePlan plan ∈ decodePlans plans := by
    exact List.mem_map.mpr ⟨plan, hmem, rfl⟩
  obtain ⟨hbegin, hstop⟩ := spansFrom_member_bounds chars.val (decodePlans plans)
    0 chars.val.length hspans (decodePlan plan) hdecoded
  change plan.«begin».val ≤ plan.«end».val at hbegin
  change plan.«end».val ≤ chars.val.length at hstop
  exact ⟨hbegin, hstop, by omega⟩

end Verification.SlideChainExtracted
