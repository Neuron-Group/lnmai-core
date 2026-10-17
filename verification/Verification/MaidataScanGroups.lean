import Verification.MaidataScanGroupsPlan

/-!
UNELABORATED. Actual extraction-3 emission and outer annotation loops.
Generated SHA256 d037f357eb395803e657d1ca4b445cc73908e6910c59bb5c37af0de3493ab1bf.

The supplied contracts describe three individual writes and four reads. All
actual bodies, loops, planning and final source correspondence are derived.
ready and idReady are resource/representation predicates. They must be
instantiated for the native facade; they do not assert a completed scan result.
The group-ID resource has a remaining-input budget, including the next_group
call made AFTER the final group. Group IDs decode to Nat, never to Usize.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.MaidataScanModel Verification.MaidataScanFuel
open Verification.MaidataScanGroupsModel

set_option maxHeartbeats 1000000

namespace Verification.MaidataScanGroupsProof

open private annotateTouchEachGroupsFrom annotateTouchEachGroups
  from LnmaiCore.Simai.Source.Maidata

structure Encoding (GroupId Output : Type) where
  groupId : GroupId → Nat
  output : Output → List RawNoteToken

/-- Single-token output laws, one Natural successor, and resource weakening.
The cursor in nextGroup is the outer group's ORIGINAL cursor. Thus a final
increment is covered even when emission has reached tokens.length. -/
structure WritePoints {Ops GroupId Output : Type}
    (Write : maidata_scan.TouchWrite Ops GroupId Output) (ops : Ops)
    (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop) : Prop where
  idAdvance : ∀ (cursor next : Nat) (groupId : GroupId),
    cursor ≤ next → next ≤ tokens.length → idReady cursor groupId → idReady next groupId
  appendUnchanged : ∀ (output : Output) (cursor : Usize)
      (hi : cursor.val < tokens.length), ready cursor.val output →
    Write.append_unchanged ops output cursor ⦃ out =>
      enc.output out = enc.output output ++ [tokens[cursor.val]] ∧
      ready (cursor.val + 1) out ⦄
  appendTagged : ∀ (output : Output) (cursor : Usize) (groupId : GroupId)
      (index size : Usize) (hi : cursor.val < tokens.length),
    ready cursor.val output → idReady cursor.val groupId →
    index.val ≤ cursor.val → size.val ≤ tokens.length →
    Write.append_tagged ops output cursor groupId index size ⦃ out =>
      enc.output out = enc.output output ++
        [tagged (enc.groupId groupId) index.val size.val tokens[cursor.val]] ∧
      ready (cursor.val + 1) out ⦄
  nextGroup : ∀ (cursor : Nat) (groupId : GroupId),
    cursor < tokens.length → idReady cursor groupId →
    Write.next_group ops groupId ⦃ next =>
      enc.groupId next = enc.groupId groupId + 1 ∧ idReady (cursor + 1) next ⦄

private theorem tag_decision (token : RawNoteToken) (size : Nat) (touchEach : Bool) :
    (match touchKind token with
      | .Slide => (ok false : Result Bool)
      | .Touch => ok touchEach
      | .TouchHold => ok (decide (size > 1))
      | .Other => ok false) = ok (shouldTag size touchEach token) := by
  cases hk : token.kind <;> simp [touchKind, shouldTag, hk]

/-- A token is emitted once. The tag index advances only in the tagged branch;
it is bounded by the absolute cursor, sufficient even when stop = Usize.max. -/
theorem emit_body_spec {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (stop size : Usize) (groupId : GroupId) (touchEach : Bool)
    (output : Output) (cursor index : Usize)
    (hstop : stop.val ≤ tokens.length) (hc : cursor.val ≤ stop.val)
    (hindex : index.val ≤ cursor.val) (hsize : size.val ≤ tokens.length)
    (hr : ready cursor.val output) (hg : idReady cursor.val groupId) :
    maidata_scan.emit_touch_group_loop.body Read Write stop size groupId ops touchEach
      (decide (size.val > 1)) output cursor index ⦃ out =>
      match out with
      | .done final => ready stop.val final ∧
          enc.output final = emitResume tokens stop.val cursor.val (enc.groupId groupId)
            size.val touchEach index.val (enc.output output)
      | .cont (output', next, index') =>
          next.val = cursor.val + 1 ∧ next.val ≤ stop.val ∧ index'.val ≤ next.val ∧
          ready next.val output' ∧ idReady next.val groupId ∧
          emitResume tokens stop.val next.val (enc.groupId groupId) size.val touchEach
            index'.val (enc.output output') =
          emitResume tokens stop.val cursor.val (enc.groupId groupId) size.val touchEach
            index.val (enc.output output) ⦄ := by
  by_cases hmore : cursor < stop
  · have hlt : cursor.val < stop.val := by simpa using hmore
    have hi : cursor.val < tokens.length := by omega
    obtain ⟨next, hadd, hnext⟩ := increment_index cursor stop hlt
    have hreadyId : idReady next.val groupId :=
      W.idAdvance cursor.val next.val groupId (by omega) (by omega) hg
    have hstep := emitResume_step tokens stop.val cursor.val (enc.groupId groupId)
      size.val touchEach index.val (enc.output output) hlt hstop
    cases htag : shouldTag size.val touchEach tokens[cursor.val] with
    | false =>
        obtain ⟨output', hwrite, hout, hr'⟩ := spec_imp_exists
          (W.appendUnchanged output cursor hi hr)
        apply exists_imp_spec
        refine ⟨.cont (output', next, index), ?_, hnext, ?_, ?_, ?_, hreadyId, ?_⟩
        · simp [maidata_scan.emit_touch_group_loop.body, hmore, R.kind cursor hi,
            tag_decision, htag, hwrite, hadd]
        · omega
        · omega
        · simpa only [hnext] using hr'
        · simpa only [htag, Bool.false_eq_true, if_false, hnext, hout] using hstep.symm
    | true =>
        obtain ⟨output', hwrite, hout, hr'⟩ := spec_imp_exists
          (W.appendTagged output cursor groupId index size hi hr hg hindex hsize)
        obtain ⟨index', haddIndex, hindex'⟩ := increment_index index stop (by omega)
        apply exists_imp_spec
        refine ⟨.cont (output', next, index'), ?_, hnext, ?_, ?_, ?_, hreadyId, ?_⟩
        · simp [maidata_scan.emit_touch_group_loop.body, hmore, R.kind cursor hi,
            tag_decision, htag, hwrite, haddIndex, hadd]
        · omega
        · omega
        · simpa only [hnext] using hr'
        · simpa only [htag, if_true, hnext, hindex', hout] using hstep.symm
  · have hge : stop.val ≤ cursor.val := by simpa using hmore
    have heq : cursor.val = stop.val := by omega
    apply exists_imp_spec
    refine ⟨.done output, ?_, ?_, ?_⟩
    · simp [maidata_scan.emit_touch_group_loop.body, hmore]
    · simpa only [heq] using hr
    · exact (emitResume_done tokens stop.val cursor.val (enc.groupId groupId)
        size.val touchEach index.val (enc.output output) hge).symm

theorem emit_loop_spec {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (stop size : Usize) (groupId : GroupId) (touchEach : Bool)
    (output : Output) (start index : Usize)
    (hstop : stop.val ≤ tokens.length) (hs : start.val ≤ stop.val)
    (hindex : index.val ≤ start.val) (hsize : size.val ≤ tokens.length)
    (hr : ready start.val output) (hg : idReady start.val groupId) :
    maidata_scan.emit_touch_group_loop Read Write stop size groupId output ops touchEach
      (decide (size.val > 1)) start index ⦃ out => ready stop.val out ∧
      enc.output out = emitResume tokens stop.val start.val (enc.groupId groupId)
        size.val touchEach index.val (enc.output output) ⦄ := by
  unfold maidata_scan.emit_touch_group_loop
  apply loop.spec_decr_nat (fun p : Output × Usize × Usize => stop.val - p.2.1.val)
    (fun p => p.2.1.val ≤ stop.val ∧ p.2.2.val ≤ p.2.1.val ∧
      ready p.2.1.val p.1 ∧ idReady p.2.1.val groupId ∧
      emitResume tokens stop.val p.2.1.val (enc.groupId groupId) size.val touchEach
        p.2.2.val (enc.output p.1) =
      emitResume tokens stop.val start.val (enc.groupId groupId) size.val touchEach
        index.val (enc.output output))
  · rintro ⟨output', cursor, index'⟩ ⟨hc, hi, hr', hg', hinv⟩
    dsimp only at hc hi hr' hg' hinv ⊢
    apply spec_mono (emit_body_spec Read Write ops tokens enc ready idReady R W
      stop size groupId touchEach output' cursor index' hstop hc hi hsize hr' hg')
    intro out hout
    cases out with
    | done final =>
        exact ⟨hout.1, hout.2.trans hinv⟩
    | cont state =>
        rcases state with ⟨output'', next, index''⟩
        rcases hout with ⟨hnext, hn, hi', hr'', hg'', heq⟩
        refine ⟨⟨hn, hi', hr'', hg'', heq.trans hinv⟩, ?_⟩
        change stop.val - next.val < stop.val - cursor.val
        omega
  · exact ⟨hs, hindex, hr, hg, rfl⟩

/-- Saturation is derived from the real scalar definition, not a point law. -/
theorem saturating_sub_val (left right : Usize) :
    (core.num.Usize.saturating_sub left right).val = left.val - right.val := by
  change (BitVec.ofNat _ (max 0 (left.val - right.val))).toNat = left.val - right.val
  simp only [Nat.zero_max, BitVec.toNat_ofNat]
  exact Nat.mod_eq_of_lt (lt_of_le_of_lt (Nat.sub_le _ _) left.bv.isLt)

private theorem bne_one_val (value : Usize) :
    (value != 1#usize) = (value.val != 1) := by
  apply Bool.eq_iff_iff.mpr
  simp only [bne_iff_ne, ne_eq, UScalar.eq_equiv, UScalar.ofNatCore_val_eq]

/-- The plan premise here is the postcondition established by plan_touch_group_spec.
It is discharged internally by annotate_body_spec, not assumed by the public scan. -/
theorem emit_touch_group_spec {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (start : Nat) (hi : start < tokens.length) (plan : maidata_scan.TouchGroupPlan)
    (P : PlanAt tokens start hi plan) (groupId : GroupId) (output : Output)
    (hr : ready start output) (hg : idReady start groupId) :
    maidata_scan.emit_touch_group Read Write plan groupId output ops ⦃ out =>
      ready plan.range.«end».val out ∧ enc.output out = enc.output output ++
        tagGroup (enc.groupId groupId)
          (rangeTokens tokens plan.range.«end».val plan.range.start.val) ⦄ := by
  have hb := eventEnd_bounds tokens start hi
  have hstop : plan.range.«end».val ≤ tokens.length := by rw [P.end_eq]; exact hb.2
  have hs : plan.range.start.val ≤ plan.range.«end».val := by
    rw [P.start_eq, P.end_eq]
    omega
  have ht : plan.timing_note_count.val =
      timingCount (rangeTokens tokens plan.range.«end».val plan.range.start.val) := by
    simpa only [P.start_eq, P.end_eq] using P.timing_eq
  have hn : plan.no_head_slide_count.val =
      noHeadCount (rangeTokens tokens plan.range.«end».val plan.range.start.val) := by
    simpa only [P.start_eq, P.end_eq] using P.headless_eq
  have hsize : plan.timing_note_count.val ≤ tokens.length := by
    have hcounts := counts_bounds
      (rangeTokens tokens plan.range.«end».val plan.range.start.val)
    have hlength := rangeTokens_length_le tokens plan.range.«end».val plan.range.start.val
    omega
  let touchEach : Bool := decide (plan.timing_note_count.val > 1) &&
    (plan.timing_note_count.val - plan.no_head_slide_count.val != 1)
  have hcall : maidata_scan.emit_touch_group Read Write plan groupId output ops =
      maidata_scan.emit_touch_group_loop Read Write plan.range.«end» plan.timing_note_count
        groupId output ops touchEach (decide (plan.timing_note_count.val > 1))
        plan.range.start 0#usize := by
    by_cases hlarge : plan.timing_note_count.val > 1 <;>
      simp [maidata_scan.emit_touch_group, lift, bne_one_val,
        saturating_sub_val, touchEach, hlarge]
  rw [hcall]
  apply spec_mono (emit_loop_spec Read Write ops tokens enc ready idReady R W
    plan.range.«end» plan.timing_note_count groupId touchEach output plan.range.start 0#usize
    hstop hs (by simp) hsize (by simpa only [P.start_eq] using hr)
    (by simpa only [P.start_eq] using hg))
  intro out hout
  refine ⟨hout.1, ?_⟩
  simpa only [emitResume, UScalar.ofNatCore_val_eq, touchEach, ht, hn, emitTokens_group]
    using hout.2

theorem annotate_body_spec {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (len : Usize) (groupId : GroupId) (output : Output) (cursor : Usize)
    (hlen : len.val = tokens.length) (hc : cursor.val ≤ len.val)
    (hr : ready cursor.val output) (hg : idReady cursor.val groupId) :
    maidata_scan.annotate_touch_each_loop.body Read Write len ops groupId output cursor ⦃ out =>
      match out with
      | .done final => ready len.val final ∧ enc.output final =
          groupResume tokens cursor.val (enc.groupId groupId) (enc.output output)
      | .cont (groupId', output', next) =>
          cursor.val < next.val ∧ next.val ≤ len.val ∧ ready next.val output' ∧
          idReady next.val groupId' ∧
          groupResume tokens next.val (enc.groupId groupId') (enc.output output') =
            groupResume tokens cursor.val (enc.groupId groupId) (enc.output output) ⦄ := by
  by_cases hmore : cursor < len
  · have hi : cursor.val < tokens.length := by simpa [hlen] using hmore
    obtain ⟨plan, hplan, P⟩ := spec_imp_exists
      (plan_touch_group_spec Read ops tokens R len cursor hlen hi)
    obtain ⟨output', hemit, hr', hout⟩ := spec_imp_exists
      (emit_touch_group_spec Read Write ops tokens enc ready idReady R W
        cursor.val hi plan P groupId output hr hg)
    obtain ⟨groupId', hnext, hgroupId, hg'⟩ := spec_imp_exists
      (W.nextGroup cursor.val groupId hi hg)
    have hb := eventEnd_bounds tokens cursor.val hi
    have hprogress : cursor.val < plan.range.«end».val := by rw [P.end_eq]; exact hb.1
    have hbound : plan.range.«end».val ≤ len.val := by rw [P.end_eq, hlen]; exact hb.2
    have hreadyId : idReady plan.range.«end».val groupId' :=
      W.idAdvance (cursor.val + 1) plan.range.«end».val groupId'
        (by omega) (by omega) hg'
    have hstep := groupResume_step tokens cursor.val (enc.groupId groupId) (enc.output output) hi
    apply exists_imp_spec
    refine ⟨.cont (groupId', output', plan.range.«end»), ?_,
      hprogress, hbound, hr', hreadyId, ?_⟩
    · simp [maidata_scan.annotate_touch_each_loop.body, hmore, hplan, hemit, hnext]
    · simpa only [P.start_eq, P.end_eq, hgroupId, hout] using hstep.symm
  · have hge : tokens.length ≤ cursor.val := by simpa [hlen] using hmore
    have heq : cursor.val = len.val := by omega
    apply exists_imp_spec
    refine ⟨.done output, ?_, ?_, ?_⟩
    · simp [maidata_scan.annotate_touch_each_loop.body, hmore]
    · simpa only [heq] using hr
    · exact (groupResume_done tokens cursor.val (enc.groupId groupId) (enc.output output) hge).symm

theorem annotate_loop_spec {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (len : Usize) (groupId : GroupId) (output : Output) (start : Usize)
    (hlen : len.val = tokens.length) (hs : start.val ≤ len.val)
    (hr : ready start.val output) (hg : idReady start.val groupId) :
    maidata_scan.annotate_touch_each_loop Read Write len groupId output ops start ⦃ out =>
      ready len.val out ∧ enc.output out =
        groupResume tokens start.val (enc.groupId groupId) (enc.output output) ⦄ := by
  unfold maidata_scan.annotate_touch_each_loop
  apply loop.spec_decr_nat (fun p : GroupId × Output × Usize => len.val - p.2.2.val)
    (fun p => p.2.2.val ≤ len.val ∧ ready p.2.2.val p.2.1 ∧ idReady p.2.2.val p.1 ∧
      groupResume tokens p.2.2.val (enc.groupId p.1) (enc.output p.2.1) =
        groupResume tokens start.val (enc.groupId groupId) (enc.output output))
  · rintro ⟨groupId', output', cursor⟩ ⟨hc, hr', hg', hinv⟩
    dsimp only at hc hr' hg' hinv ⊢
    apply spec_mono (annotate_body_spec Read Write ops tokens enc ready idReady R W
      len groupId' output' cursor hlen hc hr' hg')
    intro out hout
    cases out with
    | done final => exact ⟨hout.1, hout.2.trans hinv⟩
    | cont state =>
        rcases state with ⟨groupId'', output'', next⟩
        rcases hout with ⟨hprogress, hbound, hr'', hg'', heq⟩
        refine ⟨⟨hbound, hr'', hg'', heq.trans hinv⟩, ?_⟩
        change len.val - next.val < len.val - cursor.val
        omega
  · exact ⟨hs, hr, hg, rfl⟩

/-- Actual Generated wrapper, arbitrary initial Natural ID and output prefix. -/
theorem annotate_touch_each_source_equiv {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (len : Usize) (groupId : GroupId) (output : Output)
    (hlen : len.val = tokens.length) (hr : ready 0 output) (hg : idReady 0 groupId) :
    maidata_scan.annotate_touch_each Read Write len groupId output ops ⦃ out =>
      ready len.val out ∧ enc.output out = enc.output output ++
        annotateTouchEachGroupsFrom (enc.groupId groupId) tokens ⦄ := by
  unfold maidata_scan.annotate_touch_each
  apply spec_mono (annotate_loop_spec Read Write ops tokens enc ready idReady R W
    len groupId output 0#usize hlen (by simp) (by simpa using hr) (by simpa using hg))
  intro out hout
  exact ⟨hout.1, by simpa only [UScalar.ofNatCore_val_eq, groupResume_start] using hout.2⟩

/-- The zero-ID/empty-output runtime entry point reaches the original source
annotation, conditional only on the listed individual reads/writes and resources. -/
theorem annotate_touch_each_equiv {Ops GroupId Output : Type}
    (Read : maidata_scan.TouchRead Ops) (Write : maidata_scan.TouchWrite Ops GroupId Output)
    (ops : Ops) (tokens : List RawNoteToken) (enc : Encoding GroupId Output)
    (ready : Nat → Output → Prop) (idReady : Nat → GroupId → Prop)
    (R : ReadPoints Read ops tokens) (W : WritePoints Write ops tokens enc ready idReady)
    (len : Usize) (groupId : GroupId) (output : Output)
    (hlen : len.val = tokens.length) (hr : ready 0 output) (hg : idReady 0 groupId)
    (hgroupId : enc.groupId groupId = 0) (houtput : enc.output output = []) :
    maidata_scan.annotate_touch_each Read Write len groupId output ops ⦃ out =>
      ready len.val out ∧ enc.output out = annotateTouchEachGroups tokens ⦄ := by
  apply spec_mono (annotate_touch_each_source_equiv Read Write ops tokens enc ready idReady
    R W len groupId output hlen hr hg)
  intro out hout
  refine ⟨hout.1, ?_⟩
  simpa only [hgroupId, houtput, List.nil_append, annotateTouchEachGroups] using hout.2

end Verification.MaidataScanGroupsProof
