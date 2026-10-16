import Verification.SlideChainExtractedTiming

/-!
Actual extraction-3 scan body and loop, targeting the existing stepAt/scanFuel.
No complete step or scan equivalence is assumed. The body proves every grammar
branch using the extracted read/shape/timing theorems. Proof draft, uncompiled.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1500000

namespace Verification.SlideChainExtracted

abbrev ScanState := Char × Usize × alloc.vec.Vec slide_chain.SegmentPlan × Usize
abbrev ScanResult := core.result.Result (alloc.vec.Vec slide_chain.SegmentPlan)
  slide_chain.ChainError

/-- For a nonempty/fuel-positive iteration, success means one actual push and
the model's actual step. Early errors retain their exact kind. -/
def StepPost (chars : Slice Char) (head : Char) (fuel : Usize)
    (plans : alloc.vec.Vec slide_chain.SegmentPlan) (cursor : Usize) :
    ControlFlow ScanState ScanResult → Prop
  | .done (.Err error) =>
      SlideChainModel.stepAt chars.val head cursor.val = .error (decodeError error)
  | .done (.Ok _) => False
  | .cont (nextHead, remaining, appended, next) =>
      ∃ plan : slide_chain.SegmentPlan,
        SlideChainModel.stepAt chars.val head cursor.val =
          .ok { plan := decodePlan plan, nextHead } ∧
        plan.«end» = next ∧
        appended.val = plans.val ++ [plan] ∧
        remaining.val + 1 = fuel.val

theorem parse_plans_body_step_spec (chars : Slice Char) (head : Char)
    (fuel : Usize) (plans : alloc.vec.Vec slide_chain.SegmentPlan) (cursor : Usize)
    (hmore : cursor < Slice.len chars) (hzero : fuel ≠ 0#usize)
    (hcapacity : plans.val.length < Usize.max) :
    slide_chain.parse_plans_with_fuel_loop.body chars head fuel plans cursor
      ⦃ StepPost chars head fuel plans cursor ⦄ := by
  apply exists_imp_spec
  have hi : cursor.val < chars.val.length := by simpa using hmore
  have hfuel : (1#usize).val ≤ fuel.val := by scalar_tac
  obtain ⟨remaining, hsub, hr, _⟩ := spec_imp_exists
    (UScalar.sub_spec (x := fuel) (y := 1#usize) hfuel)
  have hremaining : remaining.val + 1 = fuel.val := by scalar_tac
  obtain ⟨op, hget, hop⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
  have hlookup : chars.val[cursor.val]? = some op := by
    simpa only [List.getElem?_eq_getElem hi, hop]
  obtain ⟨afterOp, hadd, ha⟩ := spec_imp_exists
    (UScalar.add_spec (x := cursor) (y := 1#usize)
      (NaturalParserProof.scan_index_increment_bound chars cursor hi))
  have hav : afterOp.val = cursor.val + 1 := by simpa using ha
  cases hdigit : SlideChainModel.isChainDigit op with
  | true =>
      refine ⟨.done (.Err .FreshNumericHead), ?_, ?_⟩
      · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero, hsub,
          hget, hadd, is_chain_digit_equiv, hdigit]
      · simp [StepPost, SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, decodeError]
  | false =>
      cases hmark : LnmaiCore.Simai.isSlideMarkChar op with
      | false =>
          refine ⟨.done (.Err .InvalidSyntax), ?_, ?_⟩
          · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero, hsub,
              hget, hadd, is_chain_digit_equiv, hdigit,
              TokenizeProof.is_slide_mark_char_equiv, hmark]
          · simp [StepPost, SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, hmark, decodeError]
      | true =>
          obtain ⟨shape, hshape, hs⟩ := spec_imp_exists (read_shape_spec chars op afterOp)
          have hshapeAt : SlideChainModel.readShapeAt chars.val op (cursor.val + 1) =
              decodeResult decodePair shape := by simpa only [hav] using hs.symm
          cases shape with
          | Err error =>
              refine ⟨.done (.Err error), ?_, ?_⟩
              · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero, hsub,
                  hget, hadd, is_chain_digit_equiv, hdigit,
                  TokenizeProof.is_slide_mark_char_equiv, hmark, hshape]
              · simp [StepPost, SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, hmark,
                  hshapeAt, decodeResult, Bind.bind, Except.bind]
          | Ok pair =>
              rcases pair with ⟨endpoint, next⟩
              by_cases hnextMore : next < Slice.len chars
              · have hnextIn : next.val < chars.val.length := by simpa using hnextMore
                obtain ⟨c, hnextGet, hc⟩ := spec_imp_exists
                  (Slice.index_usize_spec chars next hnextIn)
                have hnextLookup : chars.val[next.val]? = some c := by
                  simpa only [List.getElem?_eq_getElem hnextIn, hc]
                by_cases hopen : c = '['
                · obtain ⟨afterOpen, hnextAdd, hao⟩ := spec_imp_exists
                    (UScalar.add_spec (x := next) (y := 1#usize)
                      (NaturalParserProof.scan_index_increment_bound chars next hnextIn))
                  have haov : afterOpen.val = next.val + 1 := by simpa using hao
                  obtain ⟨timing, htiming, ht⟩ := spec_imp_exists (timing_end_spec chars afterOpen)
                  have htimingAt : SlideChainModel.timingEnd chars.val (next.val + 1) =
                      decodeResult UScalar.val timing := by simpa only [haov] using ht.symm
                  cases timing with
                  | Err error =>
                      refine ⟨.done (.Err error), ?_, ?_⟩
                      · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero,
                          hsub, hget, hadd, is_chain_digit_equiv, hdigit,
                          TokenizeProof.is_slide_mark_char_equiv, hmark, hshape,
                          hnextMore, hnextGet, hopen, hnextAdd, htiming]
                      · simp [StepPost, SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, hmark,
                          hshapeAt, decodeResult, decodePair, hnextLookup, hopen,
                          htimingAt, Bind.bind, Except.bind]
                  | Ok stop =>
                      let plan : slide_chain.SegmentPlan :=
                        { head, «begin» := cursor, «end» := stop, has_timing := true }
                      obtain ⟨appended, hpush, hp⟩ := spec_imp_exists
                        (alloc.vec.Vec.push_spec plans plan hcapacity)
                      refine ⟨.cont (endpoint, remaining, appended, stop), ?_, ?_⟩
                      · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero,
                          hsub, hget, hadd, is_chain_digit_equiv, hdigit,
                          TokenizeProof.is_slide_mark_char_equiv, hmark, hshape,
                          hnextMore, hnextGet, hopen, hnextAdd, htiming, hpush, plan]
                      · refine ⟨plan, ?_, rfl, hp, hremaining⟩
                        simp [SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, hmark, hshapeAt,
                          decodeResult, decodePair, hnextLookup, hopen, htimingAt,
                          decodePlan, plan, Bind.bind, Except.bind, pure, Except.pure]
                · let plan : slide_chain.SegmentPlan :=
                    { head, «begin» := cursor, «end» := next, has_timing := false }
                  obtain ⟨appended, hpush, hp⟩ := spec_imp_exists
                    (alloc.vec.Vec.push_spec plans plan hcapacity)
                  refine ⟨.cont (endpoint, remaining, appended, next), ?_, ?_⟩
                  · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero,
                      hsub, hget, hadd, is_chain_digit_equiv, hdigit,
                      TokenizeProof.is_slide_mark_char_equiv, hmark, hshape,
                      hnextMore, hnextGet, hopen, hpush, plan]
                  · refine ⟨plan, ?_, rfl, hp, hremaining⟩
                    simp [SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, hmark, hshapeAt,
                      decodeResult, decodePair, hnextLookup, hopen, decodePlan, plan,
                      Bind.bind, Except.bind, pure, Except.pure]
              · have hnextOut : chars.val.length ≤ next.val := by simpa using hnextMore
                have hnextLookup : chars.val[next.val]? = none := List.getElem?_eq_none hnextOut
                let plan : slide_chain.SegmentPlan :=
                  { head, «begin» := cursor, «end» := next, has_timing := false }
                obtain ⟨appended, hpush, hp⟩ := spec_imp_exists
                  (alloc.vec.Vec.push_spec plans plan hcapacity)
                refine ⟨.cont (endpoint, remaining, appended, next), ?_, ?_⟩
                · simp [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero,
                    hsub, hget, hadd, is_chain_digit_equiv, hdigit,
                    TokenizeProof.is_slide_mark_char_equiv, hmark, hshape,
                    hnextMore, hpush, plan]
                · refine ⟨plan, ?_, rfl, hp, hremaining⟩
                  simp [SlideChainModel.stepAt, SlideChainModel.timingAt, hlookup, hdigit, hmark, hshapeAt,
                    decodeResult, decodePair, hnextLookup, decodePlan, plan,
                    Bind.bind, Except.bind, pure, Except.pure]

def ScanInvariant (chars : Slice Char)
    (target : Except Issue (List SlideChainModel.Plan)) (state : ScanState) : Prop :=
  state.2.2.1.val.length + state.2.1.val ≤ Usize.max ∧
  SlideChainModel.scanFuel chars.val state.2.1.val state.1 state.2.2.2.val
    (decodePlans state.2.2.1) = target

/-- Fuel decreases at every actual continuation. Its sum with the Vec length
is preserved, proving every push's capacity precondition. Out-of-bounds is
checked before zero fuel exactly as in the existing model. -/
theorem parse_plans_with_fuel_loop_spec (chars : Slice Char)
    (initialHead : Char) (initialFuel : Usize)
    (initialPlans : alloc.vec.Vec slide_chain.SegmentPlan) (initialCursor : Usize)
    (hcapacity : initialPlans.val.length + initialFuel.val ≤ Usize.max) :
    slide_chain.parse_plans_with_fuel_loop chars initialHead initialFuel initialPlans initialCursor
      ⦃ out => decodeResult decodePlans out =
        SlideChainModel.scanFuel chars.val initialFuel.val initialHead initialCursor.val
          (decodePlans initialPlans) ⦄ := by
  unfold slide_chain.parse_plans_with_fuel_loop
  apply loop.spec_decr_nat (fun (state : ScanState) => state.2.1.val)
    (ScanInvariant chars (SlideChainModel.scanFuel chars.val initialFuel.val initialHead
      initialCursor.val (decodePlans initialPlans)))
  · rintro ⟨head, fuel, plans, cursor⟩ ⟨hcap, hinv⟩
    dsimp only at hcap hinv ⊢
    by_cases hmore : cursor < Slice.len chars
    · have hi : cursor.val < chars.val.length := by simpa using hmore
      by_cases hzero : fuel = 0#usize
      · have hz : fuel.val = 0 := by simp [hzero]
        have hout : (Except.error Issue.invalidSyntax : Except Issue (List SlideChainModel.Plan)) =
            SlideChainModel.scanFuel chars.val initialFuel.val initialHead initialCursor.val
              (decodePlans initialPlans) := by
          simpa only [hz, SlideChainModel.scanFuel_zero_nonempty chars.val head cursor.val
            (decodePlans plans) hi] using hinv
        simpa [slide_chain.parse_plans_with_fuel_loop.body, hmore, hzero,
          decodeResult, decodeError] using hout
      · have hfpos : 0 < fuel.val := by scalar_tac
        have hpushCap : plans.val.length < Usize.max := by omega
        obtain ⟨out, hrun, hpost⟩ := spec_imp_exists
          (parse_plans_body_step_spec chars head fuel plans cursor hmore hzero hpushCap)
        cases out with
        | done result =>
            cases result with
            | Ok result => exact False.elim hpost
            | Err error =>
                have hf : fuel.val = (fuel.val - 1) + 1 := by omega
                have hscan : SlideChainModel.scanFuel chars.val fuel.val head cursor.val
                    (decodePlans plans) = .error (decodeError error) := by
                  rw [hf]
                  simp only [SlideChainModel.scanFuel, if_pos hi, hpost]
                have hout : decodeResult decodePlans (.Err error) =
                    SlideChainModel.scanFuel chars.val initialFuel.val initialHead
                      initialCursor.val (decodePlans initialPlans) := hscan.symm.trans hinv
                simpa [hrun] using hout
        | cont state =>
            rcases state with ⟨nextHead, remaining, appended, next⟩
            obtain ⟨plan, hstep, hend, hp, hf⟩ := hpost
            have hvalues : decodePlans appended = decodePlans plans ++ [decodePlan plan] := by
              simp [decodePlans, hp]
            have hcapNext : appended.val.length + remaining.val ≤ Usize.max := by
              simp only [hp, List.length_append, List.length_singleton]
              omega
            have hkeep : SlideChainModel.scanFuel chars.val remaining.val nextHead next.val
                (decodePlans appended) =
                SlideChainModel.scanFuel chars.val initialFuel.val initialHead initialCursor.val
                  (decodePlans initialPlans) := by
              rw [← hf] at hinv
              simpa only [SlideChainModel.scanFuel, if_pos hi, hstep, decodePlan,
                hend, hvalues] using hinv
            have hdec : remaining.val < fuel.val := by omega
            have hcontinue : ScanInvariant chars
                (SlideChainModel.scanFuel chars.val initialFuel.val initialHead initialCursor.val
                  (decodePlans initialPlans)) (nextHead, remaining, appended, next) ∧
                remaining.val < fuel.val := ⟨⟨hcapNext, hkeep⟩, hdec⟩
            simpa [hrun] using hcontinue
    · have ho : chars.val.length ≤ cursor.val := by simpa using hmore
      have hout : decodeResult decodePlans (.Ok plans) =
          SlideChainModel.scanFuel chars.val initialFuel.val initialHead initialCursor.val
            (decodePlans initialPlans) := by
        simpa only [SlideChainModel.scanFuel_oob chars.val fuel.val head cursor.val
          (decodePlans plans) ho] using hinv
      simpa [slide_chain.parse_plans_with_fuel_loop.body, hmore] using hout
  · exact ⟨hcapacity, rfl⟩

theorem parse_plans_with_fuel_spec (chars : Slice Char) (head : Char) (fuel : Usize) :
    slide_chain.parse_plans_with_fuel chars head fuel
      ⦃ out => decodeResult decodePlans out =
        SlideChainModel.parsePlansFuel chars.val head fuel.val ⦄ := by
  simpa [slide_chain.parse_plans_with_fuel, SlideChainModel.parsePlansFuel, decodePlans] using
    parse_plans_with_fuel_loop_spec chars head fuel (alloc.vec.Vec.new _) 0#usize
      (by simp <;> scalar_tac)

theorem parse_plans_spec (chars : Slice Char) (head : Char) :
    slide_chain.parse_plans chars head
      ⦃ out => decodeResult decodePlans out = SlideChainModel.parsePlans chars.val head ⦄ := by
  simpa [slide_chain.parse_plans, SlideChainModel.parsePlans] using
    parse_plans_with_fuel_spec chars head (Slice.len chars)

theorem parse_plans_equiv (chars : Slice Char) (head : Char) :
    (do let out ← slide_chain.parse_plans chars head
        ok (decodeResult decodePlans out)) = ok (SlideChainModel.parsePlans chars.val head) := by
  obtain ⟨out, hrun, hpost⟩ := spec_imp_exists (parse_plans_spec chars head)
  simp only [hrun, bind_tc_ok, hpost]

end Verification.SlideChainExtracted
