import Verification.Generated
import Verification.NormalizationModel
import Verification.NormalizationPoints

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai

set_option maxHeartbeats 500000

namespace Verification.NormalizationProof

open NormalizationModel
open private applySingleTrackConnRulesNormalized from LnmaiCore.Simai.Normalize

/-!
UNELABORATED proof draft for the ACTUAL extracted normalization module.
Read-only extraction used:
  /tmp/lnmai-touch-parser-batch-extraction-2/LnmaiCoreVerify.lean
  SHA256 6c46c2439ea3f3cdd6c5827efd567becc678762b3aa72ebe48384c4280868b77
The second extraction renames the slide_layout binder to input_shape.

Install beside NormalizationModel from the model-fix-1 snapshot and the
unchanged NormalizationPoints. Only the main worker may compile this draft.
No compiler, tests or extraction were run to write it.

plan_note has no primitive premises. slide_layout assumes only the actual
single discriminant read. normalize_track assumes only successful in-bounds
point writes, exact physical len, capacity, and preservation of representation.
The actual normalize_track_loop is proved with loop.spec_decr_nat; the Except
source recursion in NormalizationPoints is NOT substituted for that loop.

The runtime Vec clone/len/write instance, shape_kind instance, Natural < 4
comparison, flag/position field copies, parser/time leaves and outer token
iteration remain concrete adapter/composition obligations. No theorem here
certifies an arbitrary facade, allocator or entire normalization pipeline.
-/

def toModelNoteKind : normalization.NoteKind → RawNoteKind
  | .Tap => .tap
  | .Hold => .hold
  | .Slide => .slide
  | .Touch => .touch
  | .TouchHold => .touchHold
  | .Rest => .rest
  | .Unknown => .unknown

def toModelRawFlags (flags : normalization.RawFlags) : RawFlags :=
  ⟨flags.is_break, flags.is_ex, flags.is_hanabi, flags.is_slide_no_head,
    flags.is_force_star, flags.is_fake_rotate, flags.is_slide_break⟩

def toModelTapFlags (flags : normalization.TapFlags) : TapFlags :=
  ⟨flags.is_break, flags.is_ex, flags.is_hanabi, flags.is_force_star⟩

def toModelHoldFlags (flags : normalization.HoldFlags) : HoldFlags :=
  ⟨flags.is_break, flags.is_ex, flags.is_hanabi⟩

def toModelTouchFlags (flags : normalization.TouchFlags) : TouchFlags :=
  ⟨flags.is_break, flags.is_hanabi⟩

def toModelPlan {Slot Sensor Slot' Sensor' : Type}
    (slotView : Slot → Slot') (sensorView : Sensor → Sensor') :
    normalization.NotePlan Slot Sensor → NotePlan Slot' Sensor'
  | .Skip => .skip
  | .Tap slot flags => .tap (slotView slot) (toModelTapFlags flags)
  | .Hold slot flags => .hold (slotView slot) (toModelHoldFlags flags)
  | .Touch sensor flags => .touch (sensorView sensor) (toModelTouchFlags flags)
  | .TouchHold sensor flags => .touchHold (sensorView sensor) (toModelHoldFlags flags)
  | .Slide slot flags => .slide (slotView slot) (toModelRawFlags flags)

/-- Every actual kind/Option branch returns successfully. Position mappings
need not be injective, finite, canonical, or bounded by Usize. -/
theorem plan_note_spec {Slot Sensor Slot' Sensor' : Type}
    (slotView : Slot → Slot') (sensorView : Sensor → Sensor')
    (kind : normalization.NoteKind) (slot : Option Slot) (sensor : Option Sensor)
    (flags : normalization.RawFlags) :
    normalization.plan_note kind slot sensor flags ⦃ out =>
      toModelPlan slotView sensorView out =
        planNote (toModelNoteKind kind) (slot.map slotView) (sensor.map sensorView)
          (toModelRawFlags flags) ⦄ := by
  apply exists_imp_spec
  cases kind <;> cases slot <;> cases sensor <;> exact ⟨_, rfl, rfl⟩

theorem plan_note_equiv {Slot Sensor Slot' Sensor' : Type}
    (slotView : Slot → Slot') (sensorView : Sensor → Sensor')
    (kind : normalization.NoteKind) (slot : Option Slot) (sensor : Option Sensor)
    (flags : normalization.RawFlags) :
    (do let out ← normalization.plan_note kind slot sensor flags
        ok (toModelPlan slotView sensorView out)) =
      ok (planNote (toModelNoteKind kind) (slot.map slotView) (sensor.map sensorView)
        (toModelRawFlags flags)) := by
  obtain ⟨out, hcall, hview⟩ := spec_imp_exists
    (plan_note_spec slotView sensorView kind slot sensor flags)
  simp only [hcall, bind_tc_ok, hview]

/-- Explicit field representation premises connect the generic result to the
plan used by planStep_reference/lowerWithPlan_reference in the frozen model.
This does not assume that a complete Rust token iteration is correct. -/
theorem plan_note_token_spec {Slot Sensor : Type}
    (slotView : Slot → OuterSlot) (sensorView : Sensor → SensorArea)
    (kind : normalization.NoteKind) (slot : Option Slot) (sensor : Option Sensor)
    (flags : normalization.RawFlags) (token : RawNoteToken)
    (hkind : toModelNoteKind kind = token.kind)
    (hslot : slot.map slotView = token.slot)
    (hsensor : sensor.map sensorView = token.sensorPos)
    (hflags : toModelRawFlags flags = flagsFromToken token) :
    normalization.plan_note kind slot sensor flags ⦃ out =>
      toModelPlan slotView sensorView out =
        planNote token.kind token.slot token.sensorPos (flagsFromToken token) ⦄ := by
  simpa only [hkind, hslot, hsensor, hflags] using
    plan_note_spec slotView sensorView kind slot sensor flags

def toModelShapeKind : shape.SlideKind → LnmaiCore.Simai.SlideKind
  | .Line => .line
  | .Circle => .circle
  | .V => .v
  | .Turn => .turn
  | .Pq => .pq
  | .Ppqq => .ppqq
  | .S => .s
  | .Wifi => .wifi

def toModelSlideKind : types.SlideKind → LnmaiCore.SlideKind
  | .Single => .Single
  | .Wifi => .Wifi
  | .ConnPart => .ConnPart

def toModelSlideLayout (layout : normalization.SlideLayout) : SlideLayout :=
  { flags := toModelRawFlags layout.flags
    slideKind := toModelSlideKind layout.slide_kind
    trackCount := layout.track_count.val
    isClassic := layout.is_classic
    hasHeadNote := layout.has_head_note
    hasBody := layout.has_body
    isConnSlide := layout.is_conn_slide
    isGroupHead := layout.is_group_head
    isGroupEnd := layout.is_group_end
    totalJudgeQueueLen := layout.total_judge_queue_len.val
    multiple := layout.multiple.val }

/-- The only shape premise is one successful discriminant read. The view
retains arbitrary Natural endpoints; no endpoint is converted to Usize. -/
structure ShapeAtomicAgrees {Shape Ops : Type}
    (call : normalization.ShapeKind Ops Shape) (ops : Ops)
    (rep : NormalizationPoints.ShapeRepresentation Shape) : Prop where
  kind_ok : ∀ inputShape, rep.fits inputShape →
    ∃ kind, call.kind ops inputShape = ok kind ∧
      toModelShapeKind kind = (rep.view inputShape).kind

/-- Actual generated shape match and complete record initialization. -/
theorem slide_layout_kind_spec {Shape Ops : Type}
    (call : normalization.ShapeKind Ops Shape) (ops : Ops) (inputShape : Shape)
    (flags : normalization.RawFlags) (isClassic : Bool) (kind : shape.SlideKind)
    (hread : call.kind ops inputShape = ok kind) :
    normalization.slide_layout call inputShape flags isClassic ops ⦃ out =>
      toModelSlideLayout out =
        slideLayout (toModelShapeKind kind) (toModelRawFlags flags) isClassic ⦄ := by
  apply exists_imp_spec
  cases kind <;>
    simp only [normalization.slide_layout, hread, bind_tc_ok] <;>
    refine ⟨_, rfl, ?_⟩ <;>
    simp [toModelSlideLayout, toModelShapeKind, toModelSlideKind,
      toModelRawFlags, slideLayout]

theorem slide_layout_spec {Shape Ops : Type}
    (call : normalization.ShapeKind Ops Shape) (ops : Ops)
    (rep : NormalizationPoints.ShapeRepresentation Shape)
    (atoms : ShapeAtomicAgrees call ops rep) (inputShape : Shape)
    (flags : normalization.RawFlags) (isClassic : Bool) (hfit : rep.fits inputShape) :
    normalization.slide_layout call inputShape flags isClassic ops ⦃ out =>
      toModelSlideLayout out =
        slideLayout (rep.view inputShape).kind (toModelRawFlags flags) isClassic ⦄ := by
  obtain ⟨kind, hread, hkind⟩ := atoms.kind_ok inputShape hfit
  simpa only [hkind] using
    slide_layout_kind_spec call ops inputShape flags isClassic kind hread

theorem slide_layout_equiv {Shape Ops : Type}
    (call : normalization.ShapeKind Ops Shape) (ops : Ops)
    (rep : NormalizationPoints.ShapeRepresentation Shape)
    (atoms : ShapeAtomicAgrees call ops rep) (inputShape : Shape)
    (flags : normalization.RawFlags) (isClassic : Bool) (hfit : rep.fits inputShape) :
    (do let out ← normalization.slide_layout call inputShape flags isClassic ops
        ok (toModelSlideLayout out)) =
      ok (slideLayout (rep.view inputShape).kind (toModelRawFlags flags) isClassic) := by
  obtain ⟨out, hcall, hview⟩ := spec_imp_exists
    (slide_layout_spec call ops rep atoms inputShape flags isClassic hfit)
  simp only [hcall, bind_tc_ok, hview]

/-- Failure/divergence of the single read is retained, not asserted impossible
for arbitrary ShapeKind instances. -/
theorem slide_layout_read_failure {Shape Ops : Type}
    (call : normalization.ShapeKind Ops Shape) (ops : Ops) (inputShape : Shape)
    (flags : normalization.RawFlags) (isClassic : Bool) (error : Error)
    (hread : call.kind ops inputShape = fail error) :
    normalization.slide_layout call inputShape flags isClassic ops = fail error := by
  simp only [normalization.slide_layout, hread, bind_tc_fail]

theorem slide_layout_read_divergence {Shape Ops : Type}
    (call : normalization.ShapeKind Ops Shape) (ops : Ops) (inputShape : Shape)
    (flags : normalization.RawFlags) (isClassic : Bool)
    (hread : call.kind ops inputShape = div) :
    normalization.slide_layout call inputShape flags isClassic ops = div := by
  simp only [normalization.slide_layout, hread, bind_tc_div]

/-- Actual Result/Usize version of the frozen point contract. This field
updates ONE position and retains the exact prefix, suffix and all payload.
capacity bounds represented slots, while rep.fits may additionally describe
the concrete Vec/element representation. Neither is allocation success; the
success of each reached write is an explicit part of write_ok. -/
structure TrackAtomicAgrees {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (rep : NormalizationPoints.TrackRepresentation Q) (capacity : Nat) : Prop where
  write_ok : ∀ (queue : Q) (index : Usize) (pre : List SlideAreaSpec)
      (old : SlideAreaSpec) (rest : List SlideAreaSpec) (value : Bool),
    rep.fits queue → rep.view queue = pre ++ old :: rest →
    pre.length = index.val → (pre ++ old :: rest).length ≤ capacity →
    ∃ out, call.set_skippable ops queue index value = ok out ∧ rep.fits out ∧
      rep.view out = pre ++ setSkip value old :: rest

/-- The actual Usize bound comes from len : Usize. capacity can be set to
UScalar.max .Usize (or a smaller adapter capacity); no capacity+1 is formed. -/
private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hvalue⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hvalue⟩

/-- The actual guard precedes every write and increment, even for an index
strictly beyond len. No queue length or adapter success premise is needed. -/
theorem normalize_track_body_done {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (len : Usize) (queue : Q) (index : Usize) (hstop : len.val ≤ index.val) :
    normalization.normalize_track_loop.body call len ops queue index =
      ok (.done queue) := by
  have hguard : ¬ index < len := by simpa using (Nat.not_lt.mpr hstop)
  simp only [normalization.normalize_track_loop.body, hguard, ↓reduceIte]

theorem normalize_track_body_write_failure {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (len : Usize) (queue : Q) (index : Usize) (error : Error)
    (hmore : index.val < len.val)
    (hwrite : call.set_skippable ops queue index true = fail error) :
    normalization.normalize_track_loop.body call len ops queue index = fail error := by
  have hguard : index < len := by simpa using hmore
  simp only [normalization.normalize_track_loop.body, hguard, ↓reduceIte,
    hwrite, bind_tc_fail]

theorem normalize_track_body_write_divergence {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (len : Usize) (queue : Q) (index : Usize)
    (hmore : index.val < len.val)
    (hwrite : call.set_skippable ops queue index true = div) :
    normalization.normalize_track_loop.body call len ops queue index = div := by
  have hguard : index < len := by simpa using hmore
  simp only [normalization.normalize_track_loop.body, hguard, ↓reduceIte,
    hwrite, bind_tc_div]

/-- Composition of one actual point call and the checked machine increment. -/
theorem normalize_track_body_spec {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (rep : NormalizationPoints.TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees call ops rep capacity)
    (len : Usize) (queue : Q) (index : Usize)
    (pre : List SlideAreaSpec) (old : SlideAreaSpec) (rest : List SlideAreaSpec)
    (hfit : rep.fits queue) (hview : rep.view queue = pre ++ old :: rest)
    (hindex : pre.length = index.val)
    (hcap : (pre ++ old :: rest).length ≤ capacity) (hlt : index.val < len.val) :
    ∃ nextQueue nextIndex,
      normalization.normalize_track_loop.body call len ops queue index =
        ok (.cont (nextQueue, nextIndex)) ∧
      rep.fits nextQueue ∧
      rep.view nextQueue = pre ++ setSkip true old :: rest ∧
      nextIndex.val = index.val + 1 := by
  obtain ⟨nextQueue, hwrite, hnextFit, hnextView⟩ :=
    atoms.write_ok queue index pre old rest true hfit hview hindex hcap
  obtain ⟨nextIndex, hincrement, hnextIndex⟩ := increment_index index len hlt
  have hguard : index < len := by simpa using hlt
  refine ⟨nextQueue, nextIndex, ?_, hnextFit, hnextView, hnextIndex⟩
  simp only [normalization.normalize_track_loop.body, hguard, ↓reduceIte,
    hwrite, bind_tc_ok, hincrement]

/-- An exact mapped prefix and untouched suffix, retaining order and duplicates.
This is a predicate on the ACTUAL extracted (queue,index) state. -/
def TrackInv {Q : Type} (rep : NormalizationPoints.TrackRepresentation Q)
    (original : List SlideAreaSpec) (queue : Q) (index : Usize) : Prop :=
  rep.fits queue ∧
    ∃ pre rest : List SlideAreaSpec, original = pre ++ rest ∧
      rep.view queue = pre.map (setSkip true) ++ rest ∧ pre.length = index.val

/-- The real extracted while loop, not the handwritten Except recursion.
Termination is len-index; no fuel wrapper or len+1 capacity premise is used. -/
theorem normalize_track_loop_spec {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (rep : NormalizationPoints.TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees call ops rep capacity)
    (len : Usize) (original : List SlideAreaSpec) (initial : Q) (start : Usize)
    (hlen : len.val = original.length) (hcap : len.val ≤ capacity)
    (hinit : TrackInv rep original initial start) :
    normalization.normalize_track_loop call len initial ops start ⦃ out =>
      rep.fits out ∧ rep.view out = original.map (setSkip true) ⦄ := by
  unfold normalization.normalize_track_loop
  apply loop.spec_decr_nat (fun (state : Q × Usize) => len.val - state.2.val)
    (fun state => TrackInv rep original state.1 state.2)
  · rintro ⟨queue, index⟩ ⟨hfit, pre, rest, horiginal, hview, hindex⟩
    dsimp only at hfit horiginal hview hindex ⊢
    have hlength : len.val = pre.length + rest.length := by
      rw [hlen, horiginal, List.length_append]
    by_cases hmore : index < len
    · have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil =>
          simp only [List.length_nil, Nat.add_zero] at hlength
          omega
      | cons old rest =>
          have hslotCap : ((pre.map (setSkip true)) ++ old :: rest).length ≤ capacity := by
            simp only [List.length_append, List.length_map, List.length_cons]
            simp only [List.length_cons] at hlength
            omega
          obtain ⟨nextQueue, nextIndex, hbody, hnextFit, hnextView, hnextIndex⟩ :=
            normalize_track_body_spec call ops rep capacity atoms len queue index
              (pre.map (setSkip true)) old rest hfit hview
              (by simpa only [List.length_map] using hindex) hslotCap hlt
          apply exists_imp_spec
          refine ⟨.cont (nextQueue, nextIndex), hbody, ?_, ?_⟩
          · refine ⟨hnextFit, pre ++ [old], rest, ?_, ?_, ?_⟩
            · simpa only [List.append_assoc, List.singleton_append] using horiginal
            · simpa only [List.map_append, List.map_cons, List.map_nil,
                List.append_assoc, List.singleton_append] using hnextView
            · simp only [List.length_append, List.length_cons, List.length_nil,
                Nat.zero_add, hindex, hnextIndex]
          · dsimp only
            omega
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons old tail =>
            simp only [List.length_cons] at hlength
            omega
      have hdone : rep.view queue = original.map (setSkip true) := by
        simpa only [horiginal, hempty, List.append_nil] using hview
      apply exists_imp_spec
      exact ⟨.done queue, normalize_track_body_done call ops len queue index hge,
        hfit, hdone⟩
  · exact hinit

/-- Even an inconsistent/out-of-bounds loop cursor returns unchanged at once.
The normal wrapper begins at zero; this stronger fact needs no representation. -/
theorem normalize_track_loop_done {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (len : Usize) (queue : Q) (index : Usize) (hstop : len.val ≤ index.val) :
    normalization.normalize_track_loop call len queue ops index = ok queue := by
  have hspec : normalization.normalize_track_loop call len queue ops index
      ⦃ out => out = queue ⦄ := by
    unfold normalization.normalize_track_loop
    apply loop.spec_decr_nat (fun (_ : Q × Usize) => 0)
      (fun state => state = (queue, index))
    · rintro ⟨queue', index'⟩ hstate
      have hq := congrArg Prod.fst hstate
      have hi := congrArg Prod.snd hstate
      dsimp only at hq hi ⊢
      subst queue' index'
      apply exists_imp_spec
      exact ⟨.done queue, normalize_track_body_done call ops len queue index hstop, rfl⟩
    · rfl
  obtain ⟨out, hcall, hout⟩ := spec_imp_exists hspec
  simpa only [hout] using hcall

/-- No adapter calls occur when disconnected, regardless of len. -/
theorem normalize_track_disconnected {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (short isHead isEnd : Bool) (len : Usize) (queue : Q) :
    normalization.normalize_track call false short isHead isEnd len queue ops =
      ok queue := by
  simp only [normalization.normalize_track, Bool.false_eq_true, ↓reduceIte]

theorem normalize_track_short_empty {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (queue : Q) (isHead isEnd : Bool) :
    normalization.normalize_track call true true isHead isEnd 0#usize queue ops =
      ok queue := by
  simp only [normalization.normalize_track, ↓reduceIte]

/-- Captures the actual generated short-circuit Bool branch, followed by just
one write. This equation preserves any failure/divergence of that write. -/
theorem normalize_track_short_single {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (queue : Q) (isHead isEnd : Bool) :
    normalization.normalize_track call true true isHead isEnd 1#usize queue ops =
      call.set_skippable ops queue 0#usize (isHead || isEnd) := by
  cases isHead <;> simp [normalization.normalize_track]

/-- Exact two-write order. There is no premise that reported total or physical
length is two: every suffix after physical slot one is retained by the atoms. -/
theorem normalize_track_short_many {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (len : Usize) (queue : Q) (isHead isEnd : Bool)
    (hzero : len ≠ 0#usize) (hone : len ≠ 1#usize) :
    normalization.normalize_track call true true isHead isEnd len queue ops =
      (do let next ← call.set_skippable ops queue 0#usize isHead
          call.set_skippable ops next 1#usize isEnd) := by
  simp only [normalization.normalize_track, hzero, hone, ↓reduceIte]

/-- Complete actual normalize_track, with every guard and both short writes.
len is the physical queue length, independent of the Natural total used to
compute short. Short branches can clear previously true flags. -/
theorem normalize_track_spec {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (rep : NormalizationPoints.TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees call ops rep capacity)
    (isConn short isHead isEnd : Bool) (len : Usize) (queue : Q)
    (hfit : rep.fits queue) (hlen : len.val = (rep.view queue).length)
    (hcap : len.val ≤ capacity) :
    normalization.normalize_track call isConn short isHead isEnd len queue ops
      ⦃ out => rep.fits out ∧
        rep.view out = track isConn short isHead isEnd (rep.view queue) ⦄ := by
  apply exists_imp_spec
  cases isConn with
  | false =>
      exact ⟨queue, normalize_track_disconnected call ops short isHead isEnd len queue,
        hfit, by simp [track]⟩
  | true =>
      cases short with
      | false =>
          have hinit : TrackInv rep (rep.view queue) queue 0#usize := by
            refine ⟨hfit, [], rep.view queue, ?_, ?_, ?_⟩ <;> simp
          obtain ⟨out, hloop, houtFit, houtView⟩ := spec_imp_exists
            (normalize_track_loop_spec call ops rep capacity atoms len (rep.view queue)
              queue 0#usize hlen hcap hinit)
          refine ⟨out, ?_, houtFit, ?_⟩
          · simpa [normalization.normalize_track] using hloop
          · simpa [track] using houtView
      | true =>
          cases hview : rep.view queue with
          | nil =>
              have hzeroVal : len.val = 0 := by simpa [hview] using hlen
              have hzero : len = 0#usize :=
                UScalar.eq_of_val_eq (by simpa using hzeroVal)
              refine ⟨queue, ?_, hfit, ?_⟩
              · simpa only [hzero] using normalize_track_short_empty call ops queue isHead isEnd
              · simp [track, hview]
          | cons first rest =>
              cases rest with
              | nil =>
                  have honeVal : len.val = 1 := by simpa [hview] using hlen
                  have hone : len = 1#usize :=
                    UScalar.eq_of_val_eq (by simpa using honeVal)
                  obtain ⟨out, hwrite, houtFit, houtView⟩ :=
                    atoms.write_ok queue 0#usize [] first [] (isHead || isEnd) hfit
                      (by simpa using hview) (by simp)
                      (by simp only [List.nil_append, List.length_cons, List.length_nil]; omega)
                  refine ⟨out, ?_, houtFit, ?_⟩
                  · rw [hone, normalize_track_short_single]
                    exact hwrite
                  · simpa [track, hview] using houtView
              | cons second tail =>
                  have hlength : len.val = tail.length + 2 := by
                    simpa [hview, Nat.add_assoc] using hlen
                  have hzero : len ≠ 0#usize := by
                    intro hz
                    have hzval : len.val = 0 := by
                      simpa using congrArg (fun n : Usize => n.val) hz
                    omega
                  have hone : len ≠ 1#usize := by
                    intro ho
                    have hoval : len.val = 1 := by
                      simpa using congrArg (fun n : Usize => n.val) ho
                    omega
                  obtain ⟨middle, hfirst, hmiddleFit, hmiddleView⟩ :=
                    atoms.write_ok queue 0#usize [] first (second :: tail) isHead hfit
                      (by simpa using hview) (by simp)
                      (by simp only [List.nil_append, List.length_cons]; omega)
                  obtain ⟨out, hsecond, houtFit, houtView⟩ :=
                    atoms.write_ok middle 1#usize [setSkip isHead first] second tail
                      isEnd hmiddleFit (by simpa using hmiddleView) (by simp)
                      (by simp only [List.length_append, List.length_cons, List.length_nil]; omega)
                  refine ⟨out, ?_, houtFit, ?_⟩
                  · rw [normalize_track_short_many call ops len queue isHead isEnd hzero hone,
                      hfirst]
                    simpa only [bind_tc_ok] using hsecond
                  · simpa [track, hview] using houtView

/-- Successful Result equality for the actual wrapper. All facade premises
are the same single-write/length/representation premises as in the loop proof. -/
theorem normalize_track_equiv {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (rep : NormalizationPoints.TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees call ops rep capacity)
    (isConn short isHead isEnd : Bool) (len : Usize) (queue : Q)
    (hfit : rep.fits queue) (hlen : len.val = (rep.view queue).length)
    (hcap : len.val ≤ capacity) :
    (do let out ← normalization.normalize_track call isConn short isHead isEnd
          len queue ops
        ok (rep.view out)) =
      ok (track isConn short isHead isEnd (rep.view queue)) := by
  obtain ⟨out, hcall, _houtFit, houtView⟩ := spec_imp_exists
    (normalize_track_spec call ops rep capacity atoms isConn short isHead isEnd
      len queue hfit hlen hcap)
  simp only [hcall, bind_tc_ok, houtView]

/-- Original private Lean function, not a separately assumed whole-pass law.
The representation equality also gives exact length and payload/order
preservation, with duplicate elements and arbitrary Natural metadata allowed.
The total < 4 expression is exact: total=4 always selects the long branch. -/
theorem normalize_track_original {Q Ops : Type}
    (call : normalization.TrackWrite Ops Q) (ops : Ops)
    (rep : NormalizationPoints.TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees call ops rep capacity)
    (slide : NormalizedSlide) (len : Usize) (queue : Q)
    (hfit : rep.fits queue) (hlen : len.val = (rep.view queue).length)
    (hcap : len.val ≤ capacity) :
    normalization.normalize_track call slide.isConnSlide
      (decide (slide.totalJudgeQueueLen < 4)) slide.isGroupHead slide.isGroupEnd
      len queue ops ⦃ out =>
        rep.fits out ∧
        rep.view out = applySingleTrackConnRulesNormalized slide (rep.view queue) ∧
        (rep.view out).length = (rep.view queue).length ∧
        (rep.view out).map withoutSkip = (rep.view queue).map withoutSkip ⦄ := by
  obtain ⟨out, hcall, houtFit, houtView⟩ := spec_imp_exists
    (normalize_track_spec call ops rep capacity atoms slide.isConnSlide
      (decide (slide.totalJudgeQueueLen < 4)) slide.isGroupHead slide.isGroupEnd
      len queue hfit hlen hcap)
  apply exists_imp_spec
  refine ⟨out, hcall, houtFit, houtView.trans (track_reference slide (rep.view queue)), ?_, ?_⟩
  · rw [houtView]
    exact track_length _ _ _ _ _
  · rw [houtView]
    exact track_payload_order _ _ _ _ _

end Verification.NormalizationProof
