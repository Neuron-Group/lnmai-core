import Verification.SchedulerFrameInitialization
import Verification.SlideQueueMetrics

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerParentFieldsProof

open SchedulerParentsProof SchedulerFrameInitializationProof
open SlideQueueMetricsProof
open private slideRemaining emptySlideQueues updateSlideParentFlags
  from LnmaiCore.Scheduler

/-!
Concrete interpreted parent point operations for scheduler.rs. Snapshot/indexed
storage and nested judge queues use the Aeneas Slice/Vec representations. The
metadata reader calls the actual extracted slide_queue_metrics.remaining;
refreshParents calls the actual extracted slide_parents.refresh_flags.

This is NOT an extraction of Rust SlideNote or its runtime trait impls. Scalar
fields, Natural IDs and checkpoint payloads are already decoded logical values.
The logical field copy and queue-map below interpret derived Clone and
iter().map(...).collect(); their correspondence to the Rust facade, borrowing,
allocation and arbitrary-precision storage is still outside this theorem.
There is no assumed whole read, refresh, clone, edit or initialization equality.
The concrete dictionaries below discharge ParentPoints for this interpretation.
The author has not run Lean or any other compiler on this draft.
-/

/-- All nonqueue fields are decoded once. The old fields.judgeQueues is ignored;
queues is the authoritative representation of that field. No uniqueness of IDs,
nonempty tracks, fixed track count or equality of track lengths is required. -/
structure SlideFields where
  fields : SlideNote
  queues : alloc.vec.Vec (alloc.vec.Vec SlideArea)

abbrev Snapshot := Slice SlideFields
abbrev Buffer := alloc.vec.Vec SlideFields

def decode (slide : SlideFields) : SlideNote :=
  { slide.fields with judgeQueues := interpretQueues id slide.queues.val }

def snapshotView (slides : Snapshot) : List SlideNote := slides.val.map decode

def bufferView (slides : Buffer) : List SlideNote := slides.val.map decode

/-- Decoded payload copying. This is an interpreted field operation, not a
claim that the unextracted Rust Natural/SlideArea/SlideNote Clone was verified. -/
def valueClone (T : Type) : core.clone.Clone T :=
  { clone := fun value => ok value }

theorem value_clone_eq (T : Type) (value : T) :
    (valueClone T).clone value = ok value := rfl

/-- The standard slice-to-vector clone theorem, specialized to successful
single-element clones; no whole vector-copy premise is introduced. -/
theorem to_vec_eq {T : Type} (clone : core.clone.Clone T) (source : Slice T)
    (h : ∀ value ∈ source.val, clone.clone value = ok value) :
    alloc.slice.Slice.to_vec clone source =
      ok (⟨source.val, source.property⟩ : alloc.vec.Vec T) := by
  obtain ⟨out, hrun, heq⟩ := spec_imp_exists
    (alloc.slice.Slice.to_vec_spec clone source h)
  simpa only [← heq] using hrun

/-- Both nested vector copies use the actual standard Slice.to_vec operation. -/
def queueClone : core.clone.Clone (alloc.vec.Vec SlideArea) :=
  { clone := fun queue =>
      alloc.slice.Slice.to_vec (valueClone SlideArea) (alloc.vec.Vec.deref queue) }

theorem queue_clone_eq (queue : alloc.vec.Vec SlideArea) :
    queueClone.clone queue = ok queue := by
  exact to_vec_eq (valueClone SlideArea) (alloc.vec.Vec.deref queue)
    (by intro value _; rfl)

def fieldsClone : core.clone.Clone SlideFields :=
  { clone := fun slide => do
      let queues ← alloc.slice.Slice.to_vec queueClone (alloc.vec.Vec.deref slide.queues)
      ok { slide with queues := queues } }

theorem fields_clone_eq (slide : SlideFields) :
    fieldsClone.clone slide = ok slide := by
  have hcopy : alloc.slice.Slice.to_vec queueClone (alloc.vec.Vec.deref slide.queues) =
      ok slide.queues :=
    to_vec_eq queueClone (alloc.vec.Vec.deref slide.queues)
      (by intro queue _; exact queue_clone_eq queue)
  simp only [fieldsClone, hcopy, bind_tc_ok]

/-- The usize tests are real bounded-scalar comparisons, not Nat replacements
silently substituted for them in the reader. -/
theorem usize_beq_val (left right : Usize) :
    (left == right) = (left.val == right.val) := by
  apply Bool.eq_iff_iff.mpr
  simp only [beq_iff_eq]
  constructor
  · intro h
    exact congrArg (fun value : Usize => value.val) h
  · exact UScalar.eq_of_val_eq

/-- Field-for-field interpretation of slide_parent_info, with its real remaining
kernel. ID clones are logical value copies, as in valueClone above. -/
def readInfo (slide : SlideFields) : Result (slide_parents.ParentInfo Nat) := do
  let remaining ← slide_queue_metrics.remaining (alloc.vec.Vec.deref slide.queues)
  ok {
    note_index := slide.fields.params.noteIndex
    parent_index := slide.fields.parentNoteIndex
    zero_remaining := remaining == 0#usize
    one_remaining := remaining == 1#usize
    connected := slide.fields.isConnSlide
    final_part := slide.fields.isGroupPartEnd
    ended := isEnded slide.fields.state }

theorem read_info_agrees (slide : SlideFields) :
    ∃ info, readInfo slide = ok info ∧ InfoAgrees id info (decode slide) := by
  obtain ⟨remaining, hrun, hvalue⟩ := spec_imp_exists
    (remaining_equiv (id : SlideArea → SlideArea) (alloc.vec.Vec.deref slide.queues))
  have hremaining : remaining.val =
      slideQueueRemaining (interpretQueues id slide.queues.val) := hvalue
  let info : slide_parents.ParentInfo Nat := {
    note_index := slide.fields.params.noteIndex
    parent_index := slide.fields.parentNoteIndex
    zero_remaining := remaining == 0#usize
    one_remaining := remaining == 1#usize
    connected := slide.fields.isConnSlide
    final_part := slide.fields.isGroupPartEnd
    ended := isEnded slide.fields.state }
  refine ⟨info, ?_, ?_⟩
  · simp only [readInfo, hrun, bind_tc_ok, info]
  · refine {
      note_index := rfl
      parent_index := ?_
      zero_remaining := ?_
      one_remaining := ?_
      connected := rfl
      final_part := rfl
      ended := rfl }
    · simp [info, decode]
    · change (remaining == 0#usize) =
        (slideQueueRemaining (interpretQueues id slide.queues.val) == 0)
      rw [usize_beq_val]
      simpa using congrArg (fun n : Nat => n == 0) hremaining
    · change (remaining == 1#usize) =
        (slideQueueRemaining (interpretQueues id slide.queues.val) == 1)
      rw [usize_beq_val]
      simpa using congrArg (fun n : Nat => n == 1) hremaining

/-- One outer queue slot survives for each original track, even an empty one. -/
def clearQueues (queues : alloc.vec.Vec (alloc.vec.Vec SlideArea)) :
    alloc.vec.Vec (alloc.vec.Vec SlideArea) :=
  ⟨queues.val.map (fun _ => alloc.vec.Vec.new SlideArea),
    by simpa only [List.length_map] using queues.property⟩

theorem clear_queues_view (queues : alloc.vec.Vec (alloc.vec.Vec SlideArea)) :
    interpretQueues id (clearQueues queues).val =
      (interpretQueues id queues.val).map (fun _ => []) := by
  simp [clearQueues, interpretQueues, List.map_map, Function.comp_def]

theorem clear_queues_length (queues : alloc.vec.Vec (alloc.vec.Vec SlideArea)) :
    (clearQueues queues).val.length = queues.val.length := by
  simp only [clearQueues, List.length_map]

/-- Only these fields change. The pure queue map is the explicit interpretation
of empty_slide_queues's unextracted iterator/collect, not an assumed equation. -/
def editFields (edit : slide_parents.ParentEdit) (slide : SlideFields) : SlideFields :=
  match edit with
  | .Flags finished pending =>
      { slide with fields := { slide.fields with
          parentFinished := finished, parentPendingFinish := pending } }
  | .ClearQueues => { slide with queues := clearQueues slide.queues }
  | .End => { slide with fields := { slide.fields with state := .Ended } }

theorem edit_fields_agrees (edit : slide_parents.ParentEdit) (slide : SlideFields) :
    decode (editFields edit slide) = editSlide edit (decode slide) := by
  cases edit <;>
    simp only [editFields, decode, editSlide, emptySlideQueues, clear_queues_view]

/-- The ClearQueues branch also accounts for the interpreted record clone in
Rust's struct-update expression. Flags and End need no such clone. -/
def applyEdit (edit : slide_parents.ParentEdit) (slide : SlideFields) : Result SlideFields :=
  match edit with
  | .ClearQueues => do
      let copied ← fieldsClone.clone slide
      ok { copied with queues := clearQueues slide.queues }
  | .Flags finished pending => ok (editFields (.Flags finished pending) slide)
  | .End => ok (editFields .End slide)

theorem apply_edit_eq (edit : slide_parents.ParentEdit) (slide : SlideFields) :
    applyEdit edit slide = ok (editFields edit slide) := by
  cases edit <;> simp only [applyEdit, fields_clone_eq, bind_tc_ok, editFields]

def readSlot (slides : Snapshot) (index : Usize) :
    Result (slide_parents.ParentInfo Nat) := do
  let slide ← Slice.index_usize slides index
  readInfo slide

def readCall : slide_parents.ParentRead Unit Snapshot Nat :=
  { read := fun _ slides index => readSlot slides index }

def equalCall : slide_parents.ParentEqual Unit Nat :=
  { equal := fun _ left right => ok (left == right) }

/-- Read exactly one vector slot, edit it, and write exactly that slot back. -/
def writeSlot (slides : Buffer) (index : Usize) (edit : slide_parents.ParentEdit) :
    Result Buffer := do
  let slide ← alloc.vec.Vec.index_usize slides index
  let updated ← applyEdit edit slide
  alloc.vec.Vec.update slides index updated

def writeCall : core.ops.function.Fn Unit (Buffer × Usize × slide_parents.ParentEdit) Buffer :=
  { FnMutInst :=
      { FnOnceInst := { call_once := fun _ args => writeSlot args.1 args.2.1 args.2.2 }
        call_mut := fun f args => do
          let out ← writeSlot args.1 args.2.1 args.2.2
          ok (out, f) }
    call := fun _ args => writeSlot args.1 args.2.1 args.2.2 }

private theorem index_bound {T U : Type} (view : T → U) (values : List T)
    (pre : List U) (value : U) (suffix : List U) (index : Nat)
    (hview : values.map view = pre ++ value :: suffix) (hindex : pre.length = index) :
    index < values.length := by
  have hlength := congrArg List.length hview
  simp only [List.length_map, List.length_append, List.length_cons] at hlength
  omega

private theorem decoded_at {T U : Type} (view : T → U) (values : List T)
    (pre : List U) (value : U) (suffix : List U) (index : Nat)
    (hview : values.map view = pre ++ value :: suffix) (hindex : pre.length = index)
    (hbound : index < values.length) : view values[index] = value := by
  have hget : (values.map view)[index]? = some (view values[index]) := by
    simp only [List.getElem?_map, List.getElem?_eq_getElem hbound, Option.map_some]
  have hexpected : (values.map view)[index]? = some value := by
    rw [hview, ← hindex, List.getElem?_append_right (Nat.le_refl _)]
    simp
  exact Option.some.inj (hget.symm.trans hexpected)

theorem read_agrees : ReadAgrees readCall () snapshotView id := by
  intro slides index pre slide suffix hview hindex
  change slides.val.map decode = pre ++ slide :: suffix at hview
  have hbound := index_bound decode slides.val pre slide suffix index.val hview hindex
  obtain ⟨raw, hget, hraw⟩ := spec_imp_exists (Slice.index_usize_spec slides index hbound)
  have hdecode : decode raw = slide := by
    rw [hraw]
    exact decoded_at decode slides.val pre slide suffix index.val hview hindex hbound
  obtain ⟨info, hinfo, hagrees⟩ := read_info_agrees raw
  refine ⟨info, ?_, ?_⟩
  · simpa only [readCall, readSlot, hget, bind_tc_ok] using hinfo
  · simpa only [hdecode] using hagrees

theorem equal_agrees : EqualAgrees equalCall () id := by
  intro left right
  rfl

theorem write_agrees : WriteAgrees writeCall () bufferView := by
  intro slides index pre slide suffix edit hview hindex
  change slides.val.map decode = pre ++ slide :: suffix at hview
  have hbound := index_bound decode slides.val pre slide suffix index.val hview hindex
  obtain ⟨raw, hget, hraw⟩ := spec_imp_exists (alloc.vec.Vec.index_usize_spec slides index hbound)
  have hdecode : decode raw = slide := by
    rw [hraw]
    exact decoded_at decode slides.val pre slide suffix index.val hview hindex hbound
  obtain ⟨next, hput, hnext⟩ := spec_imp_exists
    (alloc.vec.Vec.update_spec slides index (editFields edit raw) hbound)
  refine ⟨next, ?_, ?_⟩
  · simp only [writeCall, writeSlot, hget, bind_tc_ok, apply_edit_eq, hput]
  · rw [hnext]
    change (slides.val.set index.val (editFields edit raw)).map decode =
      pre ++ editSlide edit slide :: suffix
    rw [List.map_set, hview, edit_fields_agrees, hdecode, ← hindex]
    simp [List.set_append]

def copySnapshot (slides : Snapshot) : Result Buffer :=
  alloc.slice.Slice.to_vec fieldsClone slides

theorem length_agrees (slides : Snapshot) :
    (Slice.len slides).val = (snapshotView slides).length := by
  simp [snapshotView]

theorem copy_agrees (slides : Snapshot) :
    ∃ out, copySnapshot slides = ok out ∧ bufferView out = snapshotView slides := by
  refine ⟨⟨slides.val, slides.property⟩, ?_, rfl⟩
  exact to_vec_eq fieldsClone slides (by intro slide _; exact fields_clone_eq slide)

def parentKernel : ParentKernel :=
  { Snapshot := Snapshot
    Buffer := Buffer
    Id := Nat
    Reader := Unit
    Equal := Unit
    Writer := Unit
    readCall := readCall
    equalCall := equalCall
    writeCall := writeCall
    reader := ()
    equalOp := ()
    writer := ()
    snapshotView := snapshotView
    bufferView := bufferView
    idView := id
    length := fun slides => ok (Slice.len slides)
    copy := copySnapshot }

/-- Closed concrete instance: it assumes no parent/read/write/copy/refresh
contract. Container bounds follow from the actual Slice/Vec carrier types. -/
theorem parent_points : ParentPoints parentKernel :=
  { length := fun slides => ⟨Slice.len slides, rfl, length_agrees slides⟩
    copy := copy_agrees
    read := read_agrees
    equal := equal_agrees
    write := write_agrees }

/-- Full actual refresh kernel, now without a ParentPoints premise. Snapshot
reads are immutable; duplicate IDs and arbitrary parents remain permitted. -/
theorem refresh_equiv (slides : Snapshot) :
    ∃ out, refreshParents parentKernel slides = ok out ∧
      bufferView out = updateSlideParentFlags (snapshotView slides) :=
  refresh_parents_equiv parentKernel parent_points slides

/-- The initializer can now use this parent instance directly. Body point
contracts, the three input field equations, one time-add equation and rebuild
prefix capacities remain; no whole initialize or refresh equation is assumed. -/
theorem initialize_with_fields_equiv (b : BodyKernel) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields parentKernel b) (state : GameState) (input : FrameInput)
    (hfields : FieldsAgree parentKernel b fields state)
    (htime : addTime state.currentTime input.delta = ok (state.currentTime + input.delta))
    (hfit : state.touchHoldGroupStates.isEmpty = true →
      TouchHoldRebuildTraversalProof.PrefixFits b.capacity state.activeTouchHolds []) :
    initializeFrame parentKernel b addTime fields state input =
      ok (SchedulerFrameProof.initialFrame state input) :=
  SchedulerFrameInitializationProof.initialize_equiv parentKernel b parent_points body
    addTime fields state input hfields htime hfit

end Verification.SchedulerParentFieldsProof
