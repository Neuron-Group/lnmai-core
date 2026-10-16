import Verification.NormalizationModel

/-!
Unelaborated proof draft for POINT contracts and complete source control flow.
Except is a hand-written semantic model of possible primitive failure. It is
NOT a renamed Generated module and this file proves no extraction theorem.
After actual extraction the main worker must replace the source-call wrappers
by Aeneas Result calls and prove their connection (loop.spec_decr_nat for the
actual while loop). No whole-normalizer or whole-parser equation is a premise.
-/

namespace Verification.NormalizationPoints
open LnmaiCore LnmaiCore.Simai
open Verification.NormalizationModel

open private applySingleTrackConnRulesNormalized from LnmaiCore.Simai.Normalize

def mapPlan {Slot Sensor Slot' Sensor' : Type} (fs : Slot → Slot') (ft : Sensor → Sensor') :
    NotePlan Slot Sensor → NotePlan Slot' Sensor'
  | .skip => .skip
  | .tap slot flags => .tap (fs slot) flags
  | .hold slot flags => .hold (fs slot) flags
  | .touch sensor flags => .touch (ft sensor) flags
  | .touchHold sensor flags => .touchHold (ft sensor) flags
  | .slide slot flags => .slide (fs slot) flags

/-- No finite-range assumption on positions is introduced by generic dispatch. -/
theorem planNote_map {Slot Sensor Slot' Sensor' : Type}
    (fs : Slot → Slot') (ft : Sensor → Sensor') (kind : RawNoteKind)
    (slot : Option Slot) (sensor : Option Sensor) (flags : RawFlags) :
    mapPlan fs ft (planNote kind slot sensor flags) =
      planNote kind (slot.map fs) (sensor.map ft) flags := by
  cases kind <;> cases slot <;> cases sensor <;> rfl

structure ShapeRepresentation (Shape : Type) where
  view : Shape → SlideShape
  fits : Shape → Prop

/-- Read exactly one discriminant. This is NOT the parser's success contract. -/
structure ShapeAtomicAgrees {Shape Error : Type}
    (readKind : Shape → Except Error LnmaiCore.Simai.SlideKind)
    (rep : ShapeRepresentation Shape) : Prop where
  kind_ok : ∀ shape, rep.fits shape →
    readKind shape = .ok (rep.view shape).kind

def slideLayoutCall {Shape Error : Type}
    (readKind : Shape → Except Error LnmaiCore.Simai.SlideKind)
    (shape : Shape) (flags : RawFlags) (isClassic : Bool) : Except Error SlideLayout := do
  let kind ← readKind shape
  pure (slideLayout kind flags isClassic)

theorem slideLayoutCall_success {Shape Error : Type}
    (readKind : Shape → Except Error LnmaiCore.Simai.SlideKind)
    (rep : ShapeRepresentation Shape) (atoms : ShapeAtomicAgrees readKind rep)
    (shape : Shape) (flags : RawFlags) (isClassic : Bool) (hfit : rep.fits shape) :
    slideLayoutCall readKind shape flags isClassic =
      .ok (slideLayout (rep.view shape).kind flags isClassic) := by
  simp [slideLayoutCall, atoms.kind_ok shape hfit]

structure TrackRepresentation (Q : Type) where
  view : Q → List SlideAreaSpec
  fits : Q → Prop

/-- Only a single in-bounds write. It must succeed and preserve the entire
prefix, suffix and payload, not merely length or a bag of elements.
For an actual Aeneas adapter, call success is part of this contract. -/
structure TrackAtomicAgrees {Q Error : Type}
    (write : Q → Nat → Bool → Except Error Q)
    (rep : TrackRepresentation Q) (capacity : Nat) : Prop where
  write_ok : ∀ (queue : Q) (pre : List SlideAreaSpec) (old : SlideAreaSpec)
      (rest : List SlideAreaSpec) (value : Bool),
    rep.fits queue → rep.view queue = pre ++ old :: rest →
    (pre ++ old :: rest).length ≤ capacity →
    ∃ out, write queue pre.length value = .ok out ∧ rep.fits out ∧
      rep.view out = pre ++ setSkip value old :: rest

/-- Exact source while-body recursion. The guard precedes both the point write
and index increment. Termination uses len-index, without a fuel+1 wrapper. -/
def trackLoop {Q Error : Type} (write : Q → Nat → Bool → Except Error Q)
    (len : Nat) (queue : Q) (index : Nat) : Except Error Q :=
  if hmore : index < len then
    match write queue index true with
    | .error error => .error error
    | .ok next => trackLoop write len next (index + 1)
  else .ok queue
termination_by len - index
decreasing_by omega

/-- This is the arithmetic side condition to replay for actual Usize.add.
Only the queue's physical length is bounded, not noteIndex or any endpoint. -/
theorem increment_within_capacity (index len capacity : Nat)
    (hmore : index < len) (hcap : len ≤ capacity) : index + 1 ≤ capacity := by
  omega

/-- The invariant identifies the exact processed prefix and untouched suffix.
There is no callback returning the complete map, and no uniqueness premise. -/
theorem trackLoop_success {Q Error : Type}
    (write : Q → Nat → Bool → Except Error Q)
    (rep : TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees write rep capacity) (len : Nat)
    (queue : Q) (pre rest : List SlideAreaSpec)
    (hfit : rep.fits queue) (hview : rep.view queue = pre ++ rest)
    (hlen : len = pre.length + rest.length) (hcap : len ≤ capacity) :
    ∃ out, trackLoop write len queue pre.length = .ok out ∧ rep.fits out ∧
      rep.view out = pre ++ rest.map (setSkip true) := by
  induction rest generalizing queue pre with
  | nil =>
      refine ⟨queue, ?_, hfit, ?_⟩
      · have hstop : ¬pre.length < len := by simp only [List.length_nil] at hlen; omega
        rw [trackLoop]
        split
        · omega
        · rfl
      · simpa using hview
  | cons old rest ih =>
      have hmore : pre.length < len := by simp only [List.length_cons] at hlen; omega
      have hstepCapacity := increment_within_capacity pre.length len capacity hmore hcap
      have hslotCap : (pre ++ old :: rest).length ≤ capacity := by
        simp only [List.length_append, List.length_cons] at hlen ⊢
        omega
      obtain ⟨next, hwrite, hnextFit, hnextView⟩ :=
        atoms.write_ok queue pre old rest true hfit hview hslotCap
      have hnextView' : rep.view next = (pre ++ [setSkip true old]) ++ rest := by
        simpa only [List.append_assoc, List.singleton_append] using hnextView
      have hnextLen : len = (pre ++ [setSkip true old]).length + rest.length := by
        simp only [List.length_append, List.length_cons, List.length_nil] at hlen ⊢
        omega
      obtain ⟨out, hloop, houtFit, houtView⟩ :=
        ih next (pre ++ [setSkip true old]) hnextFit hnextView' hnextLen
      refine ⟨out, ?_, houtFit, ?_⟩
      · rw [trackLoop]
        split
        · rw [hwrite]
          simpa only [List.length_append, List.length_cons, List.length_nil, Nat.add_zero]
            using hloop
        · omega
      · simpa only [List.map_cons, List.append_assoc, List.singleton_append] using houtView

/-- Full source control flow of normalization::normalize_track, including both
short-queue writes and the zero/singleton distinction. Errors propagate at the
point of the failed write; the success theorem requires every reached atom. -/
def normalizeTrack {Q Error : Type} (write : Q → Nat → Bool → Except Error Q)
    (isConn short isHead isEnd : Bool) (len : Nat) (queue : Q) : Except Error Q :=
  if !isConn then .ok queue
  else if short then
    if len = 0 then .ok queue
    else if len = 1 then write queue 0 (isHead || isEnd)
    else do
      let next ← write queue 0 isHead
      write next 1 isEnd
  else trackLoop write len queue 0

theorem normalizeTrack_success {Q Error : Type}
    (write : Q → Nat → Bool → Except Error Q)
    (rep : TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees write rep capacity)
    (isConn short isHead isEnd : Bool) (len : Nat) (queue : Q)
    (hfit : rep.fits queue) (hlen : len = (rep.view queue).length)
    (hcap : len ≤ capacity) :
    ∃ out, normalizeTrack write isConn short isHead isEnd len queue = .ok out ∧
      rep.fits out ∧ rep.view out = track isConn short isHead isEnd (rep.view queue) := by
  cases isConn with
  | false =>
      exact ⟨queue, by simp [normalizeTrack], hfit, by simp [track]⟩
  | true =>
      cases short with
      | false =>
          obtain ⟨out, hloop, houtFit, houtView⟩ :=
            trackLoop_success write rep capacity atoms len queue [] (rep.view queue)
              hfit (by simp) (by simpa using hlen) hcap
          refine ⟨out, ?_, houtFit, ?_⟩
          · simpa [normalizeTrack] using hloop
          · simpa [track] using houtView
      | true =>
          cases hview : rep.view queue with
          | nil =>
              have hzero : len = 0 := by simpa [hview] using hlen
              exact ⟨queue, by simp [normalizeTrack, hzero], hfit, by simp [track, hview]⟩
          | cons first rest =>
              cases rest with
              | nil =>
                  have hone : len = 1 := by simpa [hview] using hlen
                  obtain ⟨out, hwrite, houtFit, houtView⟩ :=
                    atoms.write_ok queue [] first [] (isHead || isEnd) hfit
                      (by simpa using hview) (by
                        simp only [List.nil_append, List.length_cons, List.length_nil]
                        omega)
                  exact ⟨out, by simpa [normalizeTrack, hone] using hwrite, houtFit,
                    by simpa [track, hview] using houtView⟩
              | cons second tail =>
                  have hlen' : len = tail.length + 2 := by
                    simpa [hview, Nat.add_assoc] using hlen
                  have hzero : len ≠ 0 := by omega
                  have hone : len ≠ 1 := by omega
                  obtain ⟨middle, hfirst, hmiddleFit, hmiddleView⟩ :=
                    atoms.write_ok queue [] first (second :: tail) isHead hfit
                      (by simpa using hview) (by
                        simp only [List.nil_append, List.length_cons]
                        omega)
                  obtain ⟨out, hsecond, houtFit, houtView⟩ :=
                    atoms.write_ok middle [setSkip isHead first] second tail isEnd hmiddleFit
                      (by simpa using hmiddleView) (by
                        simp only [List.length_append, List.length_cons, List.length_nil]
                        omega)
                  refine ⟨out, ?_, houtFit, ?_⟩
                  · simp only [List.length_nil] at hfirst
                    simp only [List.length_cons, List.length_nil] at hsecond
                    simp [normalizeTrack, hzero, hone, hfirst]
                    exact hsecond
                  · simpa [track, hview] using houtView

/-- Actual original private function as the target, with success and all
point/length/capacity premises exposed. Still a source-model theorem until the
new Generated calls have been connected by the sole main worker. -/
theorem normalizeTrack_original {Q Error : Type}
    (write : Q → Nat → Bool → Except Error Q)
    (rep : TrackRepresentation Q) (capacity : Nat)
    (atoms : TrackAtomicAgrees write rep capacity)
    (slide : NormalizedSlide) (len : Nat) (queue : Q)
    (hfit : rep.fits queue) (hlen : len = (rep.view queue).length)
    (hcap : len ≤ capacity) :
    ∃ out, normalizeTrack write slide.isConnSlide (decide (slide.totalJudgeQueueLen < 4))
        slide.isGroupHead slide.isGroupEnd len queue = .ok out ∧
      rep.fits out ∧ rep.view out = applySingleTrackConnRulesNormalized slide (rep.view queue) := by
  obtain ⟨out, hcall, houtFit, houtView⟩ :=
    normalizeTrack_success write rep capacity atoms slide.isConnSlide
      (decide (slide.totalJudgeQueueLen < 4)) slide.isGroupHead slide.isGroupEnd
      len queue hfit hlen hcap
  exact ⟨out, hcall, houtFit, houtView.trans (track_reference slide (rep.view queue))⟩

end Verification.NormalizationPoints
