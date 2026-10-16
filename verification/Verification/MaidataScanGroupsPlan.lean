import Verification.Generated
import Verification.MaidataScanGroupsModel

/-!
UNELABORATED actual extraction-3 proofs for same_event_end and plan_touch_group.
Frozen extraction SHA256:
d037f357eb395803e657d1ca4b445cc73908e6910c59bb5c37af0de3493ab1bf.
Only single-token reads and one event-key comparison are supplied contracts.
The endpoint and both count loops are proved here, never callback assumptions.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.MaidataScanModel Verification.MaidataScanFuel
open Verification.MaidataScanGroupsModel

set_option maxHeartbeats 1000000

namespace Verification.MaidataScanGroupsProof

def touchKind (token : RawNoteToken) : maidata_scan.TouchKind :=
  match token.kind with
  | .slide => .Slide
  | .touch => .Touch
  | .touchHold => .TouchHold
  | _ => .Other

structure ReadPoints {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) : Prop where
  sameEvent : ∀ (head index : Usize) (hh : head.val < tokens.length)
      (hi : index.val < tokens.length),
    Read.same_event ops head index = ok (sameAs tokens[head.val] tokens[index.val])
  kind : ∀ (index : Usize) (hi : index.val < tokens.length),
    Read.kind ops index = ok (touchKind tokens[index.val])
  child : ∀ (index : Usize) (hi : index.val < tokens.length),
    Read.source_group_index_nonzero ops index = ok (childIndex tokens[index.val])
  noHead : ∀ (index : Usize) (hi : index.val < tokens.length),
    Read.is_slide_no_head ops index = ok tokens[index.val].isSlideNoHead

/-- A strict loop guard supplies the checked-add bound, including len=MAX. -/
theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

theorem same_event_end_body_spec {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) (R : ReadPoints Read ops tokens)
    (len head cursor : Usize) (hlen : len.val = tokens.length)
    (hh : head.val < tokens.length) (hc : cursor.val ≤ len.val) :
    maidata_scan.same_event_end_loop.body Read len head ops cursor ⦃ out =>
      match out with
      | .done stop => cursor.val ≤ stop.val ∧ stop.val ≤ len.val ∧
          stop.val = scanEnd (sameAs tokens[head.val]) tokens cursor.val
      | .cont next => next.val = cursor.val + 1 ∧ next.val ≤ len.val ∧
          scanEnd (sameAs tokens[head.val]) tokens next.val =
            scanEnd (sameAs tokens[head.val]) tokens cursor.val ⦄ := by
  by_cases hmore : cursor < len
  · have hlt : cursor.val < len.val := by simpa using hmore
    have hi : cursor.val < tokens.length := by omega
    have hs := scanEnd_step (sameAs tokens[head.val]) tokens cursor.val hi
    cases hb : sameAs tokens[head.val] tokens[cursor.val] with
    | false =>
        apply exists_imp_spec
        refine ⟨.done cursor, ?_, Nat.le_refl _, hc, ?_⟩
        · simp [maidata_scan.same_event_end_loop.body, hmore, R.sameEvent head cursor hh hi, hb]
        · simpa only [hb, Bool.false_eq_true, if_false] using hs.symm
    | true =>
        obtain ⟨next, hadd, hnext⟩ := increment_index cursor len hlt
        apply exists_imp_spec
        refine ⟨.cont next, ?_, hnext, ?_, ?_⟩
        · simp [maidata_scan.same_event_end_loop.body, hmore, R.sameEvent head cursor hh hi, hb, hadd]
        · omega
        · simpa only [hnext, hb, if_true] using hs.symm
  · have hge : len.val ≤ cursor.val := by simpa using hmore
    apply exists_imp_spec
    refine ⟨.done cursor, ?_, Nat.le_refl _, hc, ?_⟩
    · simp [maidata_scan.same_event_end_loop.body, hmore]
    · exact (scanEnd_finished (sameAs tokens[head.val]) tokens cursor.val (by omega)).symm

theorem same_event_end_loop_spec {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) (R : ReadPoints Read ops tokens)
    (len head start : Usize) (hlen : len.val = tokens.length)
    (hh : head.val < tokens.length) (hs : start.val ≤ len.val) :
    maidata_scan.same_event_end_loop Read len head ops start ⦃ stop =>
      start.val ≤ stop.val ∧ stop.val ≤ len.val ∧
        stop.val = scanEnd (sameAs tokens[head.val]) tokens start.val ⦄ := by
  unfold maidata_scan.same_event_end_loop
  apply loop.spec_decr_nat (fun cursor : Usize => len.val - cursor.val)
    (fun cursor => start.val ≤ cursor.val ∧ cursor.val ≤ len.val ∧
      scanEnd (sameAs tokens[head.val]) tokens cursor.val =
        scanEnd (sameAs tokens[head.val]) tokens start.val)
  · intro cursor hinv
    rcases hinv with ⟨hlower, hupper, heq⟩
    apply spec_mono (same_event_end_body_spec Read ops tokens R len head cursor hlen hh hupper)
    intro out hout
    cases out with
    | done stop =>
        rcases hout with ⟨hcs, hstop, hval⟩
        exact ⟨Nat.le_trans hlower hcs, hstop, hval.trans heq⟩
    | cont next =>
        rcases hout with ⟨hnext, hn, hsame⟩
        exact ⟨⟨by omega, hn, hsame.trans heq⟩, by omega⟩
  · exact ⟨Nat.le_refl _, hs, rfl⟩

theorem same_event_end_spec {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) (R : ReadPoints Read ops tokens)
    (len start : Usize) (hlen : len.val = tokens.length) (hi : start.val < tokens.length) :
    maidata_scan.same_event_end Read len start ops ⦃ stop =>
      start.val < stop.val ∧ stop.val ≤ len.val ∧ stop.val = eventEnd tokens start.val hi ⦄ := by
  obtain ⟨next, hadd, hnext⟩ := increment_index start len (by omega)
  obtain ⟨stop, hloop, hlower, hupper, hval⟩ := spec_imp_exists
    (same_event_end_loop_spec Read ops tokens R len start next hlen hi (by omega))
  apply exists_imp_spec
  refine ⟨stop, ?_, ?_, hupper, ?_⟩
  · simp only [maidata_scan.same_event_end, hadd, bind_tc_ok, hloop]
  · omega
  · simpa only [eventEnd, hnext] using hval

private theorem headless_indicator_le (token : RawNoteToken) :
    (if noHeadPoint token then 1 else 0) ≤ (if timingPoint token then 1 else 0) := by
  cases ht : timingPoint token with
  | false => simp [noHeadPoint, ht]
  | true => cases hn : noHeadPoint token <;> simp [ht, hn]

/-- The actual count body, including the expanded-slide-child branch that reads
no headless flag and increments neither counter. Absolute cursor bounds suffice
for both checked count additions; no count-over-the-range premise is used. -/
theorem count_body_spec {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) (R : ReadPoints Read ops tokens)
    (stop cursor timing headless : Usize) (hstop : stop.val ≤ tokens.length)
    (hc : cursor.val ≤ stop.val) (hnh : headless.val ≤ timing.val)
    (ht : timing.val ≤ cursor.val) :
    maidata_scan.plan_touch_group_loop.body Read ops stop cursor timing headless ⦃ out =>
      match out with
      | .done counts => counts.2.val ≤ counts.1.val ∧ counts.1.val ≤ stop.val ∧
          counts.1.val = timing.val + timingCount (rangeTokens tokens stop.val cursor.val) ∧
          counts.2.val = headless.val + noHeadCount (rangeTokens tokens stop.val cursor.val)
      | .cont (next, timing', headless') =>
          next.val = cursor.val + 1 ∧ next.val ≤ stop.val ∧
          headless'.val ≤ timing'.val ∧ timing'.val ≤ next.val ∧
          timing'.val + timingCount (rangeTokens tokens stop.val next.val) =
            timing.val + timingCount (rangeTokens tokens stop.val cursor.val) ∧
          headless'.val + noHeadCount (rangeTokens tokens stop.val next.val) =
            headless.val + noHeadCount (rangeTokens tokens stop.val cursor.val) ⦄ := by
  by_cases hmore : cursor < stop
  · have hlt : cursor.val < stop.val := by simpa using hmore
    have hi : cursor.val < tokens.length := by omega
    obtain ⟨next, hadd, hnext⟩ := increment_index cursor stop hlt
    obtain ⟨timingNext, haddT, htimingNext⟩ := increment_index timing stop (by omega)
    obtain ⟨headlessNext, haddH, hheadlessNext⟩ := increment_index headless stop (by omega)
    let timing' := if timingPoint tokens[cursor.val] then timingNext else timing
    let headless' := if noHeadPoint tokens[cursor.val] then headlessNext else headless
    have htval : timing'.val = timing.val + (if timingPoint tokens[cursor.val] then 1 else 0) := by
      cases hp : timingPoint tokens[cursor.val] <;> simp [timing', hp, htimingNext]
    have hnval : headless'.val = headless.val + (if noHeadPoint tokens[cursor.val] then 1 else 0) := by
      cases hp : noHeadPoint tokens[cursor.val] <;> simp [headless', hp, hheadlessNext]
    have hdelta := headless_indicator_le tokens[cursor.val]
    have htbound : timing'.val ≤ next.val := by
      rw [htval, hnext]
      split <;> omega
    have hnbound : headless'.val ≤ timing'.val := by omega
    have hstep := range_counts_step tokens stop.val cursor.val hlt hstop
    apply exists_imp_spec
    refine ⟨.cont (next, timing', headless'), ?_, hnext, ?_, hnbound, htbound, ?_, ?_⟩
    · cases hk : tokens[cursor.val].kind <;>
        cases hchild : childIndex tokens[cursor.val] <;>
        cases hnohead : tokens[cursor.val].isSlideNoHead <;>
        simp [maidata_scan.plan_touch_group_loop.body, hmore, R.kind cursor hi,
          R.child cursor hi, R.noHead cursor hi, touchKind, timing', headless',
          timingPoint, noHeadPoint, rawKind_beq_slide, hk, hchild, hnohead, hadd, haddT, haddH]
    · omega
    · rw [hnext, htval]
      omega
    · rw [hnext, hnval]
      omega
  · have hge : stop.val ≤ cursor.val := by simpa using hmore
    have hempty := rangeTokens_done tokens stop.val cursor.val hge
    apply exists_imp_spec
    refine ⟨.done (timing, headless), ?_, hnh, Nat.le_trans ht hc, ?_, ?_⟩
    · simp [maidata_scan.plan_touch_group_loop.body, hmore]
    · simp only [hempty, timingCount, List.countP_nil, Nat.add_zero]
    · simp only [hempty, noHeadCount, List.countP_nil, Nat.add_zero]

theorem count_loop_spec {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) (R : ReadPoints Read ops tokens)
    (stop start timing headless : Usize) (hstop : stop.val ≤ tokens.length)
    (hs : start.val ≤ stop.val) (hnh : headless.val ≤ timing.val) (ht : timing.val ≤ start.val) :
    maidata_scan.plan_touch_group_loop Read ops stop start timing headless ⦃ counts =>
      counts.2.val ≤ counts.1.val ∧ counts.1.val ≤ stop.val ∧
      counts.1.val = timing.val + timingCount (rangeTokens tokens stop.val start.val) ∧
      counts.2.val = headless.val + noHeadCount (rangeTokens tokens stop.val start.val) ⦄ := by
  unfold maidata_scan.plan_touch_group_loop
  apply loop.spec_decr_nat (fun p : Usize × Usize × Usize => stop.val - p.1.val)
    (fun p => p.1.val ≤ stop.val ∧ p.2.2.val ≤ p.2.1.val ∧ p.2.1.val ≤ p.1.val ∧
      p.2.1.val + timingCount (rangeTokens tokens stop.val p.1.val) =
        timing.val + timingCount (rangeTokens tokens stop.val start.val) ∧
      p.2.2.val + noHeadCount (rangeTokens tokens stop.val p.1.val) =
        headless.val + noHeadCount (rangeTokens tokens stop.val start.val))
  · rintro ⟨cursor, timing', headless'⟩ ⟨hc, hn, ht', hiT, hiH⟩
    apply spec_mono (count_body_spec Read ops tokens R stop cursor timing' headless' hstop hc hn ht')
    intro out hout
    cases out with
    | done counts =>
        rcases hout with ⟨hn', ht'', hvT, hvH⟩
        exact ⟨hn', ht'', hvT.trans hiT, hvH.trans hiH⟩
    | cont state =>
        rcases state with ⟨next, timing'', headless''⟩
        rcases hout with ⟨hnext, hbound, hn', ht'', hvT, hvH⟩
        exact ⟨⟨hbound, hn', ht'', hvT.trans hiT, hvH.trans hiH⟩, by omega⟩
  · exact ⟨hs, hnh, ht, rfl, rfl⟩

/-- This predicate is a derived postcondition, never an input contract. -/
structure PlanAt (tokens : List RawNoteToken) (start : Nat) (hi : start < tokens.length)
    (plan : maidata_scan.TouchGroupPlan) : Prop where
  start_eq : plan.range.start.val = start
  end_eq : plan.range.«end».val = eventEnd tokens start hi
  timing_eq : plan.timing_note_count.val =
    timingCount (rangeTokens tokens (eventEnd tokens start hi) start)
  headless_eq : plan.no_head_slide_count.val =
    noHeadCount (rangeTokens tokens (eventEnd tokens start hi) start)

theorem plan_touch_group_spec {Ops : Type} (Read : maidata_scan.TouchRead Ops)
    (ops : Ops) (tokens : List RawNoteToken) (R : ReadPoints Read ops tokens)
    (len start : Usize) (hlen : len.val = tokens.length) (hi : start.val < tokens.length) :
    maidata_scan.plan_touch_group Read len start ops ⦃ plan => PlanAt tokens start.val hi plan ⦄ := by
  obtain ⟨stop, hfind, hprogress, hbound, hstop⟩ := spec_imp_exists
    (same_event_end_spec Read ops tokens R len start hlen hi)
  obtain ⟨counts, hcount, _, _, ht, hn⟩ := spec_imp_exists
    (count_loop_spec Read ops tokens R stop start 0#usize 0#usize (by omega)
      (by omega) (by simp) (by simp))
  apply exists_imp_spec
  refine ⟨{ range := { start, «end» := stop }, timing_note_count := counts.1,
    no_head_slide_count := counts.2 }, ?_, ?_⟩
  · simp only [maidata_scan.plan_touch_group, hfind, bind_tc_ok, hcount]
  · refine ⟨rfl, hstop, ?_, ?_⟩
    · simpa only [hstop, UScalar.ofNatCore_val_eq, Nat.zero_add] using ht
    · simpa only [hstop, UScalar.ofNatCore_val_eq, Nat.zero_add] using hn

end Verification.MaidataScanGroupsProof
