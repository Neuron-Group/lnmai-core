import Verification.Bridge
import LnmaiCore.Scheduler
import Batteries.Tactic.OpenPrivate

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 300000

namespace Verification.SchedulerParentsProof
open LnmaiCore Lifecycle
open private slideRemaining emptySlideQueues updateSlideParentFlags hideSlideRenderCmds
  forceFinishDirectParent endSlideAncestorsFuel endSlideAncestors from LnmaiCore.Scheduler

/-!
Proofs for the shared `slide_parents` Rust kernel. The runtime calls this
kernel from all four parent-propagation entry points. The reference operations
below are the original private Scheduler definitions, not a replacement model.

Adapter boundaries are point reads, point writes, ID equality, and one-command
append. IDs have an arbitrary interpretation into Nat; neither injectivity nor
machine-integer bounds on IDs are required. Lengths/indices/fuel are Usize.
The runtime adapters (Natural arithmetic, cloning, slice/list representation,
queue clearing, and vector allocation) remain explicit proof obligations.
No callback is assumed to implement a whole search, update pass, or ancestor
step. The scan and ancestor-loop theorems have passed Lean elaboration and
transitive axiom checks; the concrete adapter obligations below remain open.
-/

def isEnded : SlideState → Bool
  | .Ended => true
  | _ => false

def matchesId (target : Nat) (slide : SlideNote) : Bool :=
  slide.params.noteIndex == target

def editSlide (edit : slide_parents.ParentEdit) (slide : SlideNote) : SlideNote :=
  match edit with
  | .Flags finished pending =>
      { slide with parentFinished := finished, parentPendingFinish := pending }
  | .ClearQueues => emptySlideQueues slide
  | .End => { slide with state := .Ended }

def editMatching (target : Nat) (edit : slide_parents.ParentEdit)
    (slide : SlideNote) : SlideNote :=
  if matchesId target slide then editSlide edit slide else slide

@[simp] theorem editSlide_noteIndex (edit : slide_parents.ParentEdit) (slide : SlideNote) :
    (editSlide edit slide).params.noteIndex = slide.params.noteIndex := by
  cases edit <;> rfl

@[simp] theorem editMatching_noteIndex (target : Nat) (edit : slide_parents.ParentEdit)
    (slide : SlideNote) :
    (editMatching target edit slide).params.noteIndex = slide.params.noteIndex := by
  simp only [editMatching]
  split <;> simp

/-- All slide kinds produce exactly the same singleton hide command. -/
theorem hide_commands (slide : SlideNote) :
    hideSlideRenderCmds slide = [RenderCommand.HideAllSlideBars slide.params.noteIndex] := by
  unfold hideSlideRenderCmds
  cases slide.slideKind <;> rfl

/-- Metadata describes the *whole* reference slot, including the first
    parent's parent ID. Clone fidelity is part of the read adapter. -/
structure InfoAgrees {I : Type} (id : I → Nat)
    (info : slide_parents.ParentInfo I) (slide : SlideNote) : Prop where
  note_index : id info.note_index = slide.params.noteIndex
  parent_index : info.parent_index.map id = slide.parentNoteIndex
  zero_remaining : info.zero_remaining = (slideRemaining slide == 0)
  one_remaining : info.one_remaining = (slideRemaining slide == 1)
  connected : info.connected = slide.isConnSlide
  final_part : info.final_part = slide.isGroupPartEnd
  ended : info.ended = isEnded slide.state

/-- The extracted named reader trait takes curried arguments. Shared Rust
    borrows contribute no updated state to this point-read result. -/
def ReadAgrees {S I R : Type}
    (call : slide_parents.ParentRead R S I)
    (read : R) (repr : S → List SlideNote) (id : I → Nat) : Prop :=
  ∀ (state : S) (index : Usize) (pre : List SlideNote) (slide : SlideNote)
    (suffix : List SlideNote),
    repr state = pre ++ slide :: suffix → pre.length = index.val →
    ∃ info, call.read read state index = ok info ∧ InfoAgrees id info slide

def EqualAgrees {I E : Type} (call : slide_parents.ParentEqual E I)
    (equal : E) (id : I → Nat) : Prop :=
  ∀ a b, call.equal equal a b = ok (id a == id b)

/-- One indexed update preserves every other slot and every unrelated field. -/
def WriteAgrees {S W : Type}
    (call : core.ops.function.Fn W (S × Usize × slide_parents.ParentEdit) S)
    (write : W) (repr : S → List SlideNote) : Prop :=
  ∀ (state : S) (index : Usize) (pre : List SlideNote) (slide : SlideNote)
    (suffix : List SlideNote) (edit : slide_parents.ParentEdit),
    repr state = pre ++ slide :: suffix → pre.length = index.val →
    ∃ next, call.call write (state, index, edit) = ok next ∧
      repr next = pre ++ editSlide edit slide :: suffix

def EmitAgrees {C I E : Type} (call : core.ops.function.Fn E (C × I) C)
    (emit : E) (repr : C → List RenderCommand) (id : I → Nat) (capacity : Nat) : Prop :=
  ∀ commands noteIndex, (repr commands).length < capacity →
    ∃ next, call.call emit (commands, noteIndex) = ok next ∧
    repr next = repr commands ++ [RenderCommand.HideAllSlideBars (id noteIndex)]

def LookupAgrees {I : Type} (id : I → Nat) (expected : Option SlideNote) :
    Option (slide_parents.ParentInfo I) → Prop
  | none => expected = none
  | some info => ∃ slide, expected = some slide ∧ InfoAgrees id info slide

/-- Reading the reference status list is exactly first-parent lookup. -/
theorem find_status (slides : List SlideNote) (target : Nat) :
    (slides.map (fun slide => (slide.params.noteIndex, slideRemaining slide))).findSome?
        (fun entry => if entry.1 = target then some entry.2 else none) =
      (slides.find? (matchesId target)).map slideRemaining := by
  induction slides with
  | nil => rfl
  | cons slide rest ih =>
      by_cases h : slide.params.noteIndex = target
      · simp [List.findSome?, List.find?, matchesId, h]
      · simp [List.findSome?, List.find?, matchesId, h, ih]
        rw [beq_eq_false_iff_ne.mpr h]

def flagsEdit (slides : List SlideNote) (child : SlideNote) : slide_parents.ParentEdit :=
  match child.parentNoteIndex with
  | none => .Flags false false
  | some parentIndex =>
      match slides.find? (matchesId parentIndex) with
      | none => .Flags false false
      | some parent => .Flags (slideRemaining parent == 0) (slideRemaining parent == 1)

/-- The immutable snapshot used by Rust is the original status-list semantics. -/
theorem refresh_reference (slides : List SlideNote) :
    slides.map (fun child => editSlide (flagsEdit slides child) child) =
      updateSlideParentFlags slides := by
  unfold updateSlideParentFlags
  apply List.map_congr_left
  intro child _
  cases hp : child.parentNoteIndex with
  | none => simp [flagsEdit, hp, editSlide]
  | some target =>
      simp only [flagsEdit, hp]
      rw [find_status]
      cases hf : slides.find? (matchesId target) <;> simp [hf, editSlide, hp]

/-- A first matching parent remains the first after an all-matches edit.
    Later duplicate IDs are edited too; they cannot change the selected link. -/
theorem find_after_update (slides : List SlideNote) (target : Nat)
    (edit : slide_parents.ParentEdit) :
    (slides.map (editMatching target edit)).find? (matchesId target) =
      (slides.find? (matchesId target)).map (editSlide edit) := by
  induction slides with
  | nil => rfl
  | cons slide rest ih =>
      by_cases h : slide.params.noteIndex = target
      · simp [List.find?, editMatching, matchesId, h]
      · simp [List.find?, editMatching, matchesId, h, ih]
        rw [beq_eq_false_iff_ne.mpr h]

theorem ancestors_zero (slides : List SlideNote) (parent : Option Nat) :
    endSlideAncestorsFuel 0 slides parent = (slides, []) := by
  rfl

theorem ancestors_none (fuel : Nat) (slides : List SlideNote) :
    endSlideAncestorsFuel fuel slides none = (slides, []) := by
  cases fuel <;> rfl

theorem ancestors_missing (fuel : Nat) (slides : List SlideNote) (target : Nat)
    (hfind : slides.find? (matchesId target) = none) :
    endSlideAncestorsFuel fuel slides (some target) = (slides, []) := by
  have hf : slides.find? (fun slide => slide.params.noteIndex == target) = none := hfind
  cases fuel with
  | zero => rfl
  | succ fuel =>
      conv =>
        lhs
        unfold endSlideAncestorsFuel
      rw [hf]

theorem ancestors_ended (fuel : Nat) (slides : List SlideNote) (target : Nat)
    (parent : SlideNote) (hfind : slides.find? (matchesId target) = some parent)
    (hended : isEnded parent.state = true) :
    endSlideAncestorsFuel fuel slides (some target) = (slides, []) := by
  have hs : parent.state = .Ended := by
    cases hs : parent.state <;> simp [hs, isEnded] at hended ⊢
  have hf : slides.find? (fun slide => slide.params.noteIndex == target) = some parent := hfind
  cases fuel with
  | zero => rfl
  | succ fuel =>
      conv =>
        lhs
        unfold endSlideAncestorsFuel
      rw [hf]
      dsimp only
      rw [hs]


/-- This is an unfolding of the original fuel recursion, with the actual
    all-duplicates update and the direct-ancestor-first command prefix. -/
theorem ancestors_step (fuel : Nat) (slides : List SlideNote) (target : Nat)
    (parent : SlideNote) (hfind : slides.find? (matchesId target) = some parent)
    (hended : isEnded parent.state = false) :
    endSlideAncestorsFuel (fuel + 1) slides (some target) =
      let next := endSlideAncestorsFuel fuel (slides.map (editMatching target .End))
        parent.parentNoteIndex
      (next.1, [RenderCommand.HideAllSlideBars parent.params.noteIndex] ++ next.2) := by
  have hfind' : slides.find? (fun slide => slide.params.noteIndex == target) = some parent := hfind
  -- Unfold only the outer LHS call. In particular, never give the recursive
  -- definition to simp: both sides retain the same arbitrary-fuel continuation.
  conv =>
    lhs
    unfold endSlideAncestorsFuel
  rw [hfind']
  cases hs : parent.state <;>
    simp [hs, isEnded] at hended <;>
    simp only [hs, editMatching, editSlide, matchesId, hide_commands] <;> rfl

/-- Revisit of an ID already ended by a previous iteration stops for every
    remaining fuel value, even with duplicate IDs. -/
theorem updated_parent_stops (fuel : Nat) (slides : List SlideNote) (target : Nat)
    (parent : SlideNote) (hfind : slides.find? (matchesId target) = some parent) :
    endSlideAncestorsFuel fuel (slides.map (editMatching target .End)) (some target) =
      (slides.map (editMatching target .End), []) := by
  apply ancestors_ended fuel _ target (editSlide .End parent)
  · rw [find_after_update, hfind]
    rfl
  · rfl

/-- A self-cycle emits one hide, then stops. No acyclicity assumption is used. -/
theorem self_cycle_once (fuel : Nat) (slides : List SlideNote) (target : Nat)
    (parent : SlideNote) (hfind : slides.find? (matchesId target) = some parent)
    (hended : isEnded parent.state = false) (hself : parent.parentNoteIndex = some target) :
    endSlideAncestorsFuel (fuel + 1) slides (some target) =
      (slides.map (editMatching target .End),
        [RenderCommand.HideAllSlideBars parent.params.noteIndex]) := by
  rw [ancestors_step fuel slides target parent hfind hended, hself,
    updated_parent_stops fuel slides target parent hfind]
  simp

/-- A continuation of the original reference, with the runtime command prefix. -/
def resume (fuel : Nat) (slides : List SlideNote) (parent : Option Nat)
    (commands : List RenderCommand) : List SlideNote × List RenderCommand :=
  let out := endSlideAncestorsFuel fuel slides parent
  (out.1, commands ++ out.2)

theorem resume_step (fuel : Nat) (slides : List SlideNote) (target : Nat)
    (parent : SlideNote) (commands : List RenderCommand)
    (hfind : slides.find? (matchesId target) = some parent)
    (hended : isEnded parent.state = false) :
    resume fuel (slides.map (editMatching target .End)) parent.parentNoteIndex
        (commands ++ [RenderCommand.HideAllSlideBars parent.params.noteIndex]) =
      resume (fuel + 1) slides (some target) commands := by
  simp only [resume, ancestors_step fuel slides target parent hfind hended]
  simp only [List.append_assoc]

/-- Index increments cannot overflow, including at the Usize upper bound. -/
theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩


/-- Actual extracted forward scan. The invariant states that the processed
    prefix cannot change the original reference's first match. -/
theorem find_first_spec {S I R E : Type}
    (readCall : slide_parents.ParentRead R S I)
    (equalCall : slide_parents.ParentEqual E I)
    (read : R) (equal : E) (repr : S → List SlideNote) (id : I → Nat)
    (len : Usize) (state : S) (target : I)
    (hlen : len.val = (repr state).length)
    (hread : ReadAgrees readCall read repr id)
    (hequal : EqualAgrees equalCall equal id) :
    slide_parents.find_first readCall equalCall len state target read equal
      ⦃ LookupAgrees id ((repr state).find? (matchesId (id target))) ⦄ := by
  unfold slide_parents.find_first slide_parents.find_first_loop
  apply loop.spec_decr_nat (fun (index : Usize) => len.val - index.val)
    (fun index => ∃ pre rest : List SlideNote,
      repr state = pre ++ rest ∧ pre.length = index.val ∧
      (repr state).find? (matchesId (id target)) = rest.find? (matchesId (id target)))
  · rintro index ⟨pre, rest, hstate, hindex, hfound⟩
    have hlength : len.val = pre.length + rest.length := by
      rw [hlen, hstate, List.length_append]
    unfold slide_parents.find_first_loop.body
    by_cases hmore : index < len
    · simp only [hmore, ↓reduceIte]
      have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons slide rest =>
          obtain ⟨info, hget, hi⟩ := hread state index pre slide rest hstate hindex
          rw [hget]
          simp only [bind_tc_ok]
          have heq : equalCall.equal equal info.note_index target =
              ok (matchesId (id target) slide) := by
            simpa [matchesId, hi.note_index] using hequal info.note_index target
          rw [heq]
          simp only [bind_tc_ok]
          by_cases hm : slide.params.noteIndex = id target
          · have hfirst : (repr state).find? (matchesId (id target)) = some slide := by
              simpa [List.find?, matchesId, hm] using hfound
            have hdone : LookupAgrees id ((repr state).find? (matchesId (id target)))
                (some info) := ⟨slide, hfirst, hi⟩
            simpa [matchesId, hm] using hdone
          · obtain ⟨next, hnext, hv⟩ := increment_index index len hlt
            have hcontinue :
                (∃ before after : List SlideNote, repr state = before ++ after ∧
                  before.length = next.val ∧
                  (repr state).find? (matchesId (id target)) =
                    after.find? (matchesId (id target))) ∧
                len.val - next.val < len.val - index.val := by
              constructor
              · refine ⟨pre ++ [slide], rest, ?_, ?_, ?_⟩
                · simpa only [List.append_assoc, List.singleton_append] using hstate
                · simp [List.length_append, hindex, hv]
                · have htail : (slide :: rest).find? (matchesId (id target)) =
                      rest.find? (matchesId (id target)) := by
                    simp [List.find?, matchesId, hm]
                    rw [beq_eq_false_iff_ne.mpr hm]
                  exact hfound.trans htail
              · omega
            simpa [matchesId, hm, hnext, bind_tc_ok] using hcontinue
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head tail => simp only [List.length_cons] at hlength; omega
      have hnone : (repr state).find? (matchesId (id target)) = none := by
        simpa [hempty] using hfound
      simpa [hmore, LookupAgrees] using hnone
  · exact ⟨[], repr state, by simp, by simp, rfl⟩

def UpdateInv {S : Type} (repr : S → List SlideNote) (original : List SlideNote)
    (target : Nat) (edit : slide_parents.ParentEdit) (state : S) (index : Usize) : Prop :=
  ∃ pre rest : List SlideNote, original = pre ++ rest ∧
    repr state = pre.map (editMatching target edit) ++ rest ∧
    pre.length = index.val

/-- Actual all-matches loop, including duplicate IDs. The processed prefix
    is mapped; the unprocessed suffix is still the original payload. -/
theorem update_all_spec {S I R E W : Type}
    (readCall : slide_parents.ParentRead R S I)
    (equalCall : slide_parents.ParentEqual E I)
    (writeCall : core.ops.function.Fn W (S × Usize × slide_parents.ParentEdit) S)
    (read : R) (equal : E) (write : W) (repr : S → List SlideNote) (id : I → Nat)
    (len : Usize) (initial : S) (target : I) (edit : slide_parents.ParentEdit)
    (hlen : len.val = (repr initial).length)
    (hread : ReadAgrees readCall read repr id)
    (hequal : EqualAgrees equalCall equal id) (hwrite : WriteAgrees writeCall write repr) :
    slide_parents.update_all readCall equalCall writeCall len initial target edit read equal write
      ⦃ out => repr out = (repr initial).map (editMatching (id target) edit) ⦄ := by
  unfold slide_parents.update_all slide_parents.update_all_loop
  apply loop.spec_decr_nat (fun (p : S × Usize) => len.val - p.2.val)
    (fun p => UpdateInv repr (repr initial) (id target) edit p.1 p.2)
  · rintro ⟨state, index⟩ ⟨pre, rest, horig, hstate, hindex⟩
    dsimp only at hindex
    have hlength : len.val = pre.length + rest.length := by
      rw [hlen, horig, List.length_append]
    unfold slide_parents.update_all_loop.body
    by_cases hmore : index < len
    · simp only [hmore, ↓reduceIte]
      have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons slide rest =>
          obtain ⟨info, hget, hi⟩ := hread state index
            (pre.map (editMatching (id target) edit)) slide rest hstate (by simpa using hindex)
          have heq : equalCall.equal equal info.note_index target =
              ok (matchesId (id target) slide) := by
            simpa [matchesId, hi.note_index] using hequal info.note_index target
          obtain ⟨next, hnext, hv⟩ := increment_index index len hlt
          have advance (out : S)
              (hout : repr out = pre.map (editMatching (id target) edit) ++
                editMatching (id target) edit slide :: rest) :
              UpdateInv repr (repr initial) (id target) edit out next ∧
                len.val - next.val < len.val - index.val := by
            constructor
            · refine ⟨pre ++ [slide], rest, ?_, ?_, ?_⟩
              · simpa only [List.append_assoc, List.singleton_append] using horig
              · simpa only [List.map_append, List.map_cons, List.map_nil,
                  List.append_assoc, List.singleton_append] using hout
              · simp [List.length_append, hindex, hv]
            · omega
          rw [hget]
          simp only [bind_tc_ok]
          rw [heq]
          simp only [bind_tc_ok]
          by_cases hm : slide.params.noteIndex = id target
          · obtain ⟨updated, hput, hu⟩ := hwrite state index
              (pre.map (editMatching (id target) edit)) slide rest edit hstate
              (by simpa using hindex)
            have hc := advance updated (by simpa [editMatching, matchesId, hm] using hu)
            simpa [matchesId, hm, hput, hnext, bind_tc_ok] using hc
          · have hc := advance state (by simpa [editMatching, matchesId, hm] using hstate)
            simpa [matchesId, hm, hnext, bind_tc_ok] using hc
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head tail => simp only [List.length_cons] at hlength; omega
      have hout : repr state = (repr initial).map (editMatching (id target) edit) := by
        simpa only [horig, hempty, List.append_nil] using hstate
      simpa [hmore] using hout
  · exact ⟨[], repr initial, by simp, by simp, by simp⟩


/-- The full extracted ancestor loop refines the original fuel recursion.
    The only effects assumed are single-slot edits and one-command appends.
    The continuation invariant accounts for both early stops and command order. -/
theorem end_ancestors_fuel_spec {S C I R E W A : Type}
    (readCall : slide_parents.ParentRead R S I)
    (equalCall : slide_parents.ParentEqual E I)
    (writeCall : core.ops.function.Fn W (S × Usize × slide_parents.ParentEdit) S)
    (emitCall : core.ops.function.Fn A (C × I) C)
    (read : R) (equal : E) (write : W) (emit : A)
    (repr : S → List SlideNote) (commandsRepr : C → List RenderCommand) (id : I → Nat)
    (capacity : Nat)
    (len fuel : Usize) (initial : S) (commands : C) (parentIndex : Option I)
    (hlen : len.val = (repr initial).length)
    (hcapacity : (commandsRepr commands).length + fuel.val ≤ capacity)
    (hread : ReadAgrees readCall read repr id)
    (hequal : EqualAgrees equalCall equal id)
    (hwrite : WriteAgrees writeCall write repr)
    (hemit : EmitAgrees emitCall emit commandsRepr id capacity) :
    slide_parents.end_ancestors_fuel readCall equalCall writeCall emitCall
        len fuel initial commands parentIndex read equal write emit
      ⦃ out => (repr out.1, commandsRepr out.2) =
        resume fuel.val (repr initial) (parentIndex.map id) (commandsRepr commands) ⦄ := by
  unfold slide_parents.end_ancestors_fuel slide_parents.end_ancestors_fuel_loop
  apply loop.spec_decr_nat (fun (p : Usize × S × C × Option I) => p.1.val)
    (fun p => len.val = (repr p.2.1).length ∧
      (commandsRepr p.2.2.1).length + p.1.val ≤ capacity ∧
      resume p.1.val (repr p.2.1) (p.2.2.2.map id) (commandsRepr p.2.2.1) =
        resume fuel.val (repr initial) (parentIndex.map id) (commandsRepr commands))
  · rintro ⟨remaining, state, emitted, parent⟩ ⟨hsize, hspace, hinv⟩
    dsimp only at hsize hspace hinv
    have finish
        (hstop : endSlideAncestorsFuel remaining.val (repr state) (parent.map id) =
          (repr state, [])) :
        (repr state, commandsRepr emitted) =
          resume fuel.val (repr initial) (parentIndex.map id) (commandsRepr commands) := by
      simpa only [resume, hstop, List.append_nil] using hinv
    unfold slide_parents.end_ancestors_fuel_loop.body
    by_cases hmore : 0#usize < remaining
    · simp only [hmore, ↓reduceIte]
      have hpositive : 0 < remaining.val := by simpa using hmore
      cases parent with
      | none =>
          have hd := finish (ancestors_none remaining.val (repr state))
          simpa using hd
      | some target =>
          dsimp only
          obtain ⟨found, hfind, hf⟩ := spec_imp_exists
            (find_first_spec readCall equalCall read equal repr id len state target
              hsize hread hequal)
          rw [hfind]
          simp only [bind_tc_ok]
          cases found with
          | none =>
              have hd := finish (ancestors_missing remaining.val (repr state) (id target) hf)
              simpa using hd
          | some info =>
              obtain ⟨originalParent, href, hi⟩ := hf
              cases he : info.ended with
              | true =>
                  have hs : isEnded originalParent.state = true := hi.ended.symm.trans he
                  have hd := finish
                    (ancestors_ended remaining.val (repr state) (id target) originalParent href hs)
                  simpa [he] using hd
              | false =>
                  have hs : isEnded originalParent.state = false := hi.ended.symm.trans he
                  obtain ⟨updated, hupdate, hu⟩ := spec_imp_exists
                    (update_all_spec readCall equalCall writeCall read equal write repr id
                      len state target .End hsize hread hequal hwrite)
                  obtain ⟨emittedNext, hemitNext, hc⟩ :=
                    hemit emitted info.note_index (by omega)
                  obtain ⟨remainingNext, hsub, hv, _⟩ := spec_imp_exists
                    (UScalar.sub_spec (x := remaining) (y := 1#usize) (by scalar_tac))
                  have hsucc : remainingNext.val + 1 = remaining.val := by scalar_tac
                  have hlt : remainingNext.val < remaining.val := by scalar_tac
                  have hsizeNext : len.val = (repr updated).length := by
                    simpa only [hu, List.length_map] using hsize
                  have hspaceNext :
                      (commandsRepr emittedNext).length + remainingNext.val ≤ capacity := by
                    simp only [hc, List.length_append, List.length_cons, List.length_nil]
                    omega
                  have hinvNext :
                      resume remainingNext.val (repr updated) (info.parent_index.map id)
                          (commandsRepr emittedNext) =
                        resume fuel.val (repr initial) (parentIndex.map id)
                          (commandsRepr commands) := by
                    calc
                      _ = resume remainingNext.val
                          ((repr state).map (editMatching (id target) .End))
                          originalParent.parentNoteIndex
                          (commandsRepr emitted ++
                            [RenderCommand.HideAllSlideBars originalParent.params.noteIndex]) := by
                            rw [hu, hi.parent_index, hc, hi.note_index]
                      _ = resume (remainingNext.val + 1) (repr state) (some (id target))
                          (commandsRepr emitted) :=
                            resume_step remainingNext.val (repr state) (id target)
                              originalParent (commandsRepr emitted) href hs
                      _ = _ := by simpa only [hsucc, Option.map_some] using hinv
                  have hcontinue :=
                    And.intro (And.intro hsizeNext (And.intro hspaceNext hinvNext)) hlt
                  simpa [he, hupdate, hemitNext, hsub, bind_tc_ok] using hcontinue
    · have hz : remaining.val = 0 := by scalar_tac
      have hd := finish (by rw [hz]; exact ancestors_zero _ _)
      simpa [hmore] using hd
  · exact ⟨hlen, hcapacity, rfl⟩

/-- The runtime wrapper uses the original new-Ended gate and slides.length
    initial fuel. Before/after lifecycle correctness is deliberately external. -/
theorem end_ancestors_spec {S C I R E W A : Type}
    (readCall : slide_parents.ParentRead R S I)
    (equalCall : slide_parents.ParentEqual E I)
    (writeCall : core.ops.function.Fn W (S × Usize × slide_parents.ParentEdit) S)
    (emitCall : core.ops.function.Fn A (C × I) C)
    (read : R) (equal : E) (write : W) (emit : A)
    (repr : S → List SlideNote) (commandsRepr : C → List RenderCommand) (id : I → Nat)
    (capacity : Nat)
    (len : Usize) (initial : S) (commands : C) (parentIndex : Option I)
    (before after : SlideNote)
    (hparent : parentIndex.map id = after.parentNoteIndex)
    (hlen : len.val = (repr initial).length)
    (hcapacity : (commandsRepr commands).length + len.val ≤ capacity)
    (hread : ReadAgrees readCall read repr id)
    (hequal : EqualAgrees equalCall equal id)
    (hwrite : WriteAgrees writeCall write repr)
    (hemit : EmitAgrees emitCall emit commandsRepr id capacity) :
    slide_parents.end_ancestors readCall equalCall writeCall emitCall len initial commands
        (isEnded before.state) (isEnded after.state) parentIndex read equal write emit
      ⦃ out => (repr out.1, commandsRepr out.2) =
        let expected := endSlideAncestors before after (repr initial)
        (expected.1, commandsRepr commands ++ expected.2) ⦄ := by
  obtain ⟨out, hout, ho⟩ := spec_imp_exists
    (end_ancestors_fuel_spec readCall equalCall writeCall emitCall read equal write emit
      repr commandsRepr id capacity len len initial commands parentIndex
      hlen hcapacity hread hequal hwrite hemit)
  cases hb : before.state <;> cases ha : after.state <;>
    simp [slide_parents.end_ancestors, isEnded, hb, ha, endSlideAncestors, hout,
      bind_tc_ok, resume, hlen, hparent] at ho ⊢ <;> exact ho

/-- Fuel bounds the number of commands in the *original* recursion, so the
    refinement above also bounds successful kernel iterations. -/
theorem ancestors_command_bound (fuel : Nat) (slides : List SlideNote) (parent : Option Nat) :
    (endSlideAncestorsFuel fuel slides parent).2.length ≤ fuel := by
  induction fuel generalizing slides parent with
  | zero => simp [ancestors_zero]
  | succ fuel ih =>
      cases parent with
      | none => simp [ancestors_none]
      | some target =>
          cases hf : slides.find? (matchesId target) with
          | none => simp [ancestors_missing (fuel + 1) slides target hf]
          | some p =>
              cases he : isEnded p.state with
              | true => simp [ancestors_ended (fuel + 1) slides target p hf he]
              | false =>
                  rw [ancestors_step fuel slides target p hf he]
                  have hb := ih (slides.map (editMatching target .End)) p.parentNoteIndex
                  simp only [List.length_append, List.length_cons, List.length_nil] at *
                  omega

def sensorPhase : SlideState → slide_parents.SensorPhase
  | .Waiting => .Waiting
  | .Active _ => .Active
  | _ => .Other

def headOn (queue : SlideQueue) : Bool := queue.head?.any (fun area => area.on)

def HeadAgrees {H : Type} (call : core.ops.function.Fn H Usize Bool)
    (head : H) (queues : List SlideQueue) : Prop :=
  ∀ (index : Usize) (pre : List SlideQueue) (queue : SlideQueue) (suffix : List SlideQueue),
    queues = pre ++ queue :: suffix → pre.length = index.val →
    call.call head index = ok (headOn queue)


/-- The actual extracted head scan proves existential On evidence over heads
    only; checkpoints below a head cannot trigger this path. -/
theorem sensor_heads_spec {H : Type} (call : core.ops.function.Fn H Usize Bool)
    (head : H) (len : Usize) (queues : List SlideQueue)
    (hlen : len.val = queues.length) (hhead : HeadAgrees call head queues) :
    slide_parents.sensor_heads call len head
      ⦃ out => out = queues.any headOn ⦄ := by
  unfold slide_parents.sensor_heads slide_parents.sensor_heads_loop
  apply loop.spec_decr_nat (fun (index : Usize) => len.val - index.val)
    (fun index => ∃ pre rest : List SlideQueue, queues = pre ++ rest ∧
      pre.length = index.val ∧ queues.any headOn = rest.any headOn)
  · rintro index ⟨pre, rest, hqueues, hindex, hany⟩
    have hlength : len.val = pre.length + rest.length := by
      rw [hlen, hqueues, List.length_append]
    unfold slide_parents.sensor_heads_loop.body
    by_cases hmore : index < len
    · simp only [hmore, ↓reduceIte]
      have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons queue rest =>
          rw [hhead index pre queue rest hqueues hindex]
          simp only [bind_tc_ok]
          cases hon : headOn queue with
          | true =>
              have hd : true = queues.any headOn := by simpa [hon] using hany.symm
              simpa [hon] using hd
          | false =>
              obtain ⟨next, hnext, hv⟩ := increment_index index len hlt
              have hc :
                  (∃ before after : List SlideQueue, queues = before ++ after ∧
                    before.length = next.val ∧ queues.any headOn = after.any headOn) ∧
                  len.val - next.val < len.val - index.val := by
                constructor
                · refine ⟨pre ++ [queue], rest, ?_, ?_, ?_⟩
                  · simpa only [List.append_assoc, List.singleton_append] using hqueues
                  · simp [List.length_append, hindex, hv]
                  · simpa [hon] using hany
                · omega
              simpa [hon, hnext, bind_tc_ok] using hc
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons first tail => simp only [List.length_cons] at hlength; omega
      have hd : false = queues.any headOn := by simpa [hempty] using hany.symm
      simpa [hmore] using hd
  · exact ⟨[], queues, by simp, by simp, rfl⟩

/-- The gate and head traversal target the public original reference function.
    The unbounded sum comparison is the explicit runtime arithmetic boundary. -/
theorem sensor_triggered_spec {H : Type} (call : core.ops.function.Fn H Usize Bool)
    (head : H) (len : Usize) (consumed : Bool) (before after : SlideNote)
    (hlen : len.val = after.judgeQueues.length)
    (hhead : HeadAgrees call head after.judgeQueues)
    (hconsumed : consumed = decide
      ((after.judgeQueues.map List.length).sum < (before.judgeQueues.map List.length).sum)) :
    slide_parents.sensor_triggered call (sensorPhase before.state) (sensorPhase after.state)
        after.isCheckable consumed len head = ok (Scheduler.slideSensorTriggered before after) := by
  obtain ⟨out, hout, ho⟩ := spec_imp_exists (sensor_heads_spec call head len after.judgeQueues hlen hhead)
  rw [ho] at hout
  cases hb : before.state <;> cases ha : after.state <;>
    cases hc : after.isCheckable <;> cases hd : consumed <;>
    simp [slide_parents.sensor_triggered, sensorPhase, hb, ha, hc, hd, hout,
      Scheduler.slideSensorTriggered, ← hconsumed, headOn] <;> rfl

def flagsFromInfo {I : Type} : Option (slide_parents.ParentInfo I) → slide_parents.ParentEdit
  | none => .Flags false false
  | some info => .Flags info.zero_remaining info.one_remaining

/-- The lookup expression appearing in the extracted refresh loop. -/
def lookupParent {T I R E : Type}
    (readCall : slide_parents.ParentRead R T I)
    (equalCall : slide_parents.ParentEqual E I)
    (len : Usize) (snapshot : T) (read : R) (equal : E) (parent : Option I) :
    Result (Option (slide_parents.ParentInfo I)) :=
  match parent with
  | none => ok none
  | some target => slide_parents.find_first readCall equalCall len snapshot target read equal

theorem lookup_flags_spec {T I R E : Type}
    (readCall : slide_parents.ParentRead R T I)
    (equalCall : slide_parents.ParentEqual E I)
    (read : R) (equal : E) (repr : T → List SlideNote) (id : I → Nat)
    (len : Usize) (snapshot : T) (info : slide_parents.ParentInfo I) (child : SlideNote)
    (hlen : len.val = (repr snapshot).length) (hi : InfoAgrees id info child)
    (hread : ReadAgrees readCall read repr id) (hequal : EqualAgrees equalCall equal id) :
    lookupParent readCall equalCall len snapshot read equal info.parent_index
      ⦃ found => flagsFromInfo found = flagsEdit (repr snapshot) child ⦄ := by
  cases hp : info.parent_index with
  | none =>
      have hchild : child.parentNoteIndex = none := by simpa [hp] using hi.parent_index.symm
      simp [lookupParent, hp, flagsFromInfo, flagsEdit, hchild]
  | some target =>
      have hchild : child.parentNoteIndex = some (id target) := by
        simpa [hp] using hi.parent_index.symm
      obtain ⟨found, hfind, hf⟩ := spec_imp_exists
        (find_first_spec readCall equalCall read equal repr id len snapshot target hlen hread hequal)
      cases found with
      | none =>
          change (repr snapshot).find? (matchesId (id target)) = none at hf
          simp [lookupParent, hp, hfind, flagsFromInfo, flagsEdit, hchild, hf]
      | some parentInfo =>
          obtain ⟨parent, href, hparent⟩ := hf
          simp [lookupParent, hp, hfind, flagsFromInfo, flagsEdit, hchild, href,
            hparent.zero_remaining, hparent.one_remaining]

def RefreshInv {S : Type} (repr : S → List SlideNote) (original : List SlideNote)
    (state : S) (index : Usize) : Prop :=
  ∃ pre rest : List SlideNote, original = pre ++ rest ∧
    repr state = pre.map (fun child => editSlide (flagsEdit original child) child) ++ rest ∧
    pre.length = index.val

/-- The extracted flag pass reads every child and first parent from the same
    immutable snapshot. The per-slot writer is the only mutation assumption. -/
theorem refresh_flags_spec {T S I R E W : Type}
    (readCall : slide_parents.ParentRead R T I)
    (equalCall : slide_parents.ParentEqual E I)
    (writeCall : core.ops.function.Fn W (S × Usize × slide_parents.ParentEdit) S)
    (read : R) (equal : E) (write : W) (snapshotRepr : T → List SlideNote)
    (repr : S → List SlideNote) (id : I → Nat)
    (len : Usize) (snapshot : T) (initial : S)
    (hlen : len.val = (snapshotRepr snapshot).length)
    (hcopy : repr initial = snapshotRepr snapshot)
    (hread : ReadAgrees readCall read snapshotRepr id)
    (hequal : EqualAgrees equalCall equal id) (hwrite : WriteAgrees writeCall write repr) :
    slide_parents.refresh_flags readCall equalCall writeCall len snapshot initial read equal write
      ⦃ out => repr out = updateSlideParentFlags (snapshotRepr snapshot) ⦄ := by
  unfold slide_parents.refresh_flags slide_parents.refresh_flags_loop
  apply loop.spec_decr_nat (fun (p : S × Usize) => len.val - p.2.val)
    (fun p => RefreshInv repr (snapshotRepr snapshot) p.1 p.2)
  · rintro ⟨state, index⟩ ⟨pre, rest, horig, hstate, hindex⟩
    dsimp only at hindex
    have hlength : len.val = pre.length + rest.length := by
      rw [hlen, horig, List.length_append]
    unfold slide_parents.refresh_flags_loop.body
    by_cases hmore : index < len
    · simp only [hmore, ↓reduceIte]
      have hlt : index.val < len.val := by simpa using hmore
      cases rest with
      | nil => simp only [List.length_nil, Nat.add_zero] at hlength; omega
      | cons child rest =>
          obtain ⟨info, hget, hi⟩ := hread snapshot index pre child rest horig hindex
          obtain ⟨found, hlookup, hflags⟩ := spec_imp_exists
            (lookup_flags_spec readCall equalCall read equal snapshotRepr id
              len snapshot info child hlen hi hread hequal)
          obtain ⟨updated, hput, hu⟩ := hwrite state index
            (pre.map (fun child => editSlide (flagsEdit (snapshotRepr snapshot) child) child))
            child rest (flagsEdit (snapshotRepr snapshot) child) hstate (by simpa using hindex)
          obtain ⟨next, hnext, hv⟩ := increment_index index len hlt
          have hc : RefreshInv repr (snapshotRepr snapshot) updated next ∧
              len.val - next.val < len.val - index.val := by
            constructor
            · refine ⟨pre ++ [child], rest, ?_, ?_, ?_⟩
              · simpa only [List.append_assoc, List.singleton_append] using horig
              · simpa only [List.map_append, List.map_cons, List.map_nil,
                  List.append_assoc, List.singleton_append] using hu
              · simp [List.length_append, hindex, hv]
            · omega
          rw [← hflags] at hput
          unfold lookupParent at hlookup
          rw [hget]
          simp only [bind_tc_ok]
          -- Generated binds a (finished, pending) pair before constructing Flags.
          -- Reduce the point-write equation after the same Option case split.
          cases hparent : info.parent_index <;>
            simp only [hparent] at hlookup ⊢ <;>
            rw [hlookup] <;>
            simp only [bind_tc_ok] <;>
            cases found <;>
            simp only [flagsFromInfo] at hput <;>
            simpa [hput, hnext, bind_tc_ok] using hc
    · have hge : len.val ≤ index.val := by simpa using hmore
      have hempty : rest = [] := by
        cases rest with
        | nil => rfl
        | cons head tail => simp only [List.length_cons] at hlength; omega
      have hout : repr state = updateSlideParentFlags (snapshotRepr snapshot) := by
        rw [← refresh_reference]
        simpa only [horig, hempty, List.append_nil] using hstate
      simpa [hmore] using hout
  · exact ⟨[], snapshotRepr snapshot, by simp, by simpa using hcopy, by simp⟩

theorem positive_remaining {I : Type} {id : I → Nat}
    {info : slide_parents.ParentInfo I} {slide : SlideNote} (hi : InfoAgrees id info slide) :
    decide (0 < slideRemaining slide) = !info.zero_remaining := by
  rw [hi.zero_remaining]
  by_cases hz : slideRemaining slide = 0
  · simp [hz]
  · have hp : 0 < slideRemaining slide := Nat.pos_of_ne_zero hz
    simp [hz, hp]


/-- Force-finish reads the first parent, clears every duplicate, and emits
    exactly one command. Only the metadata/slot/append adapters are assumed. -/
theorem force_finish_spec {S C I R E W A : Type}
    (readCall : slide_parents.ParentRead R S I)
    (equalCall : slide_parents.ParentEqual E I)
    (writeCall : core.ops.function.Fn W (S × Usize × slide_parents.ParentEdit) S)
    (emitCall : core.ops.function.Fn A (C × I) C)
    (read : R) (equal : E) (write : W) (emit : A)
    (repr : S → List SlideNote) (commandsRepr : C → List RenderCommand) (id : I → Nat)
    (capacity : Nat) (len : Usize) (initial : S) (commands : C)
    (triggered : Bool) (parentIndex : Option I) (before child : SlideNote)
    (htrigger : triggered = Scheduler.slideSensorTriggered before child)
    (hparent : parentIndex.map id = child.parentNoteIndex)
    (hlen : len.val = (repr initial).length)
    (hcapacity : (commandsRepr commands).length < capacity)
    (hread : ReadAgrees readCall read repr id)
    (hequal : EqualAgrees equalCall equal id)
    (hwrite : WriteAgrees writeCall write repr)
    (hemit : EmitAgrees emitCall emit commandsRepr id capacity) :
    slide_parents.force_finish readCall equalCall writeCall emitCall len initial commands
        triggered parentIndex read equal write emit
      ⦃ out => (repr out.1, commandsRepr out.2) =
        let expected := forceFinishDirectParent (repr initial) before child
        (expected.1, commandsRepr commands ++ expected.2) ⦄ := by
  cases ht : triggered with
  | false =>
      have htRef : Scheduler.slideSensorTriggered before child = false := htrigger.symm.trans ht
      simp [slide_parents.force_finish, ht, forceFinishDirectParent, htRef]
  | true =>
      have htRef : Scheduler.slideSensorTriggered before child = true := htrigger.symm.trans ht
      cases hp : parentIndex with
      | none =>
          have hpRef : child.parentNoteIndex = none := by simpa [hp] using hparent.symm
          simp [slide_parents.force_finish, ht, hp, forceFinishDirectParent, htRef, hpRef]
      | some target =>
          have hpRef : child.parentNoteIndex = some (id target) := by
            simpa [hp] using hparent.symm
          obtain ⟨found, hfind, hf⟩ := spec_imp_exists
            (find_first_spec readCall equalCall read equal repr id len initial target hlen hread hequal)
          cases found with
          | none =>
              have hfRef : (repr initial).find?
                  (fun slide => slide.params.noteIndex == id target) = none := hf
              simp [slide_parents.force_finish, ht, hp, hfind, bind_tc_ok,
                forceFinishDirectParent, htRef, hpRef, hfRef]
          | some info =>
              obtain ⟨parent, href, hi⟩ := hf
              have hfRef : (repr initial).find?
                  (fun slide => slide.params.noteIndex == id target) = some parent := href
              obtain ⟨updated, hupdate, hu⟩ := spec_imp_exists
                (update_all_spec readCall equalCall writeCall read equal write repr id
                  len initial target .ClearQueues hlen hread hequal hwrite)
              obtain ⟨emitted, he, hc⟩ := hemit commands info.note_index hcapacity
              have huRef : repr updated = (repr initial).map (fun slide =>
                  if slide.params.noteIndex == id target then emptySlideQueues slide else slide) := by
                simpa only [editMatching, matchesId, editSlide] using hu
              have hcRef : commandsRepr emitted = commandsRepr commands ++ hideSlideRenderCmds parent := by
                simpa only [hi.note_index, hide_commands] using hc
              have hpositive := positive_remaining hi
              cases hconn : info.connected <;> cases hfinal : info.final_part <;>
                cases hzero : info.zero_remaining <;>
                simp [slide_parents.force_finish, ht, hp, hfind, hconn, hfinal, hzero,
                  hupdate, he, bind_tc_ok, forceFinishDirectParent, htRef, hpRef, hfRef,
                  ← hi.connected, ← hi.final_part, hpositive, huRef, hcRef]

/-!
Remaining concrete obligations, intentionally not axiomatized:

* `ReadAgrees`: slice/list order and lengths; Natural/Option cloning and ID
  interpretation; `slide_queue_remaining` equals the reference maximum; the
  metadata booleans and the first parent's original link/state agree.
* `EqualAgrees`: Natural equality agrees with equality of its unbounded Nat
  interpretation, including IDs larger than the Usize upper bound.
* `WriteAgrees`: indexed mutation preserves all other slots and fields; queue
  clearing preserves the number of queues; state updates preserve queue data;
  flag writes preserve IDs/links/states. Snapshot Vec cloning satisfies `hcopy`.
* `HeadAgrees` and `hconsumed`: queue-head/area.on representation, and runtime
  Natural sums/comparison equal the original unbounded sum comparison.
* `EmitAgrees` and capacity: one successful vector push preserves the existing
  prefix and appends HideAllSlideBars with the original ID. Initial ancestor
  capacity covers existing commands plus fuel; force-finish needs one slot.
* The runtime state discriminants and initial len/fuel/parent arguments satisfy
  the wrapper hypotheses; runtime caller composition still needs the slide
  lifecycle adapter, full-list splice/reconstruction, and ordering of lifecycle,
  force-finish, ancestor commands followed by parent-flag refresh.

The scan/ancestor-loop bodies themselves are not hypotheses. No uniqueness,
acyclicity, bounded-ID, or whole-step equivalence assumptions are used.
-/

end Verification.SchedulerParentsProof
