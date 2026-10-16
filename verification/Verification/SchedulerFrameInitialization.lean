import Verification.SchedulerFrame
import Verification.SchedulerParents
import Verification.TouchHoldRebuildTraversal

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameInitializationProof

open SchedulerFrameProof SchedulerParentsProof
open TouchHoldGroupsProof TouchHoldRebuildModel TouchHoldRebuildUpsertProof
open TouchHoldRebuildReverseExtractedProof TouchHoldRebuildTraversalProof
open private updateSlideParentFlags touchHoldBodyGroupStatesFromHolds
  from LnmaiCore.Scheduler

/-!
Bounded interpretation of scheduler.rs::step_frame initialization, in runtime
order: time addition, snapshot-based parent refresh, conditional body-group
rebuild, then construction of the initial FrameWork. The two whole reference
helper calls are replaced by real Generated kernels, whose existing point-law
proofs derive initialFrame equality. There is no whole initializer, parent-pass
or rebuild equivalence premise.

The surrounding Rust facade has NOT been extracted. The records below package
actual generated trait dictionaries and point adapters, not new runtime code.
Frame construction/defaults are interpreted as Lean records/lists; correspondence
of Rust GameState cloning, field projections, ClickCursor defaults, and empty
Vec construction is still a concrete representation obligation. Time addition
is one explicit point law, not an extracted arithmetic proof. Rebuild prefix
capacities, native-length representation and point allocation success remain explicit.
No activation, phase traversal or finalization adapter is proved in this file.
This draft has not been run through Lean by its author.
-/

/-- Immutable parent-read snapshot and a distinct writable clone. The concrete
slice length and clone are point adapters surrounding the real refresh kernel. -/
structure ParentKernel where
  Snapshot : Type
  Buffer : Type
  Id : Type
  Reader : Type
  Equal : Type
  Writer : Type
  readCall : slide_parents.ParentRead Reader Snapshot Id
  equalCall : slide_parents.ParentEqual Equal Id
  writeCall : core.ops.function.Fn Writer (Buffer × Usize × slide_parents.ParentEdit) Buffer
  reader : Reader
  equalOp : Equal
  writer : Writer
  snapshotView : Snapshot → List SlideNote
  bufferView : Buffer → List SlideNote
  idView : Id → Nat
  length : Snapshot → Result Usize
  copy : Snapshot → Result Buffer

/-- Length/clone fidelity, one metadata read, one ID comparison, one indexed
write. Successful length reads expose Usize representability; successful copies
include their allocator/clone boundary. No parent search or refresh is assumed. -/
structure ParentPoints (p : ParentKernel) : Prop where
  length : ∀ snapshot, ∃ len, p.length snapshot = ok len ∧
    len.val = (p.snapshotView snapshot).length
  copy : ∀ snapshot, ∃ initial, p.copy snapshot = ok initial ∧
    p.bufferView initial = p.snapshotView snapshot
  read : ReadAgrees p.readCall p.reader p.snapshotView p.idView
  equal : EqualAgrees p.equalCall p.equalOp p.idView
  write : WriteAgrees p.writeCall p.writer p.bufferView

/-- Exact length/copy/refresh wiring of update_slide_parent_flags. All reads
inside refresh_flags use the original snapshot, including repeated parent IDs. -/
def refreshParents (p : ParentKernel) (snapshot : p.Snapshot) : Result p.Buffer := do
  let len ← p.length snapshot
  let initial ← p.copy snapshot
  slide_parents.refresh_flags p.readCall p.equalCall p.writeCall
    len snapshot initial p.reader p.equalOp p.writer

theorem refresh_parents_equiv (p : ParentKernel) (points : ParentPoints p)
    (snapshot : p.Snapshot) :
    ∃ out, refreshParents p snapshot = ok out ∧
      p.bufferView out = updateSlideParentFlags (p.snapshotView snapshot) := by
  obtain ⟨len, hlength, hlen⟩ := points.length snapshot
  obtain ⟨initial, hcopy, hcopied⟩ := points.copy snapshot
  obtain ⟨out, hrun, hout⟩ := spec_imp_exists
    (refresh_flags_spec p.readCall p.equalCall p.writeCall
      p.reader p.equalOp p.writer p.snapshotView p.bufferView p.idView
      len snapshot initial hlen hcopied points.read points.equal points.write)
  refine ⟨out, ?_, hout⟩
  simp only [refreshParents, hlength, hcopy, hrun, bind_tc_ok]

/-- Actual rebuild trait plus the two point operations on the preexisting
body-group buffer. Existing groups are copied only on the nonempty branch. -/
structure BodyKernel where
  Source : Type
  Buffer : Type
  Id : Type
  Ops : Type
  call : touch_hold_rebuild.TouchHoldRebuild Ops Source Buffer Id
  ops : Ops
  rep : BodyRepresentation Buffer Id
  sourceView : Source → Holds
  capacity : BodyCapacity
  isEmpty : Buffer → Result Bool
  copy : Buffer → Result Buffer

structure BodyPoints (b : BodyKernel) : Prop where
  source : SourceAtoms b.call b.ops b.rep b.sourceView
  update : UpdateAtoms b.call b.ops b.rep b.capacity
  reverse : ReverseAtoms b.call b.ops b.rep.groups b.capacity.groups
  empty : ∃ initial, b.call.empty b.ops = ok initial ∧ b.rep.groups initial = []
  isEmpty : ∀ groups, b.isEmpty groups = ok (b.rep.groups groups).isEmpty
  copy : ∀ groups, ∃ out, b.copy groups = ok out ∧
    b.rep.groups out = b.rep.groups groups

def prepareBodyGroups (b : BodyKernel) (holds : b.Source)
    (existing : b.Buffer) : Result b.Buffer := do
  let empty ← b.isEmpty existing
  if empty then touch_hold_rebuild.rebuild b.call holds b.ops
  else b.copy existing

/-- Only the empty branch needs prefix capacity for rebuilding. In particular
an already present group list is not rebuilt, normalized, deduplicated or reversed. -/
theorem prepare_body_groups_equiv (b : BodyKernel) (points : BodyPoints b)
    (holds : b.Source) (existing : b.Buffer)
    (hfit : (b.rep.groups existing).isEmpty = true →
      PrefixFits b.capacity (b.sourceView holds) []) :
    ∃ out, prepareBodyGroups b holds existing = ok out ∧
      b.rep.groups out =
        if (b.rep.groups existing).isEmpty then
          SchedulerReference.reference (b.sourceView holds)
        else b.rep.groups existing := by
  by_cases hempty : (b.rep.groups existing).isEmpty = true
  · obtain ⟨out, hrun, hout, _, _⟩ := rebuild_equiv
      b.call b.ops b.rep b.sourceView b.capacity
      points.source points.update points.reverse points.empty holds (hfit hempty)
    refine ⟨out, ?_, ?_⟩
    · simp only [prepareBodyGroups, points.isEmpty existing, bind_tc_ok,
        hempty, ↓reduceIte, hrun]
    · simpa only [hempty, ↓reduceIte] using hout
  · obtain ⟨out, hcopy, hout⟩ := points.copy existing
    refine ⟨out, ?_, ?_⟩
    · simp only [prepareBodyGroups, points.isEmpty existing, bind_tc_ok]
      rw [if_neg hempty, hcopy]
    · simpa only [hempty, ↓reduceIte] using hout

/-- The three projected input fields used by the shared kernels. In particular
activeTouchHolds is the original state's list, before any frame phase executes. -/
structure FrameFields (p : ParentKernel) (b : BodyKernel) where
  slides : p.Snapshot
  holds : b.Source
  groups : b.Buffer

/-- These are three field equations, not equality of a computed initializer. -/
structure FieldsAgree (p : ParentKernel) (b : BodyKernel)
    (fields : FrameFields p b) (state : GameState) : Prop where
  slides : p.snapshotView fields.slides = state.slides
  holds : b.sourceView fields.holds = state.activeTouchHolds
  groups : b.rep.groups fields.groups = state.touchHoldGroupStates

/-- Interpreted record assembly: all other state fields are preserved and all
per-frame outputs/cursors start at their reference defaults. The Rust record,
clone, vector and default-cursor representation bridge remains outside this file. -/
def packFrame (state : GameState) (time : TimePoint) (slides : List SlideNote)
    (groups : List TouchHoldBodyGroupState) : FrameWork :=
  { state := { state with
      currentTime := time
      slides := slides
      touchHoldGroupStates := groups }
    cursor := {}
    events := { tap := [], hold := [], slide := [], touch := [], touch_hold := [] }
    slideAudio := []
    slideRender := [] }

/-- Thin interpretation of the facade's actual call order. It invokes the
extracted parent refresh and the conditional extracted rebuild; it never calls
the two original Scheduler helper functions to compute an initialized frame. -/
def initializeFrame (p : ParentKernel) (b : BodyKernel)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b) (state : GameState) (input : FrameInput) :
    Result FrameWork := do
  let time ← addTime state.currentTime input.delta
  let slides ← refreshParents p fields.slides
  let groups ← prepareBodyGroups b fields.holds fields.groups
  ok (packFrame state time (p.bufferView slides) (b.rep.groups groups))

/-- Derive initialFrame equality from real kernels and point contracts. The
single arithmetic equation concerns only this frame's time addition. The only
capacity premise is prefix lengths for rebuilding when existing groups are empty. -/
theorem initialize_equiv (p : ParentKernel) (b : BodyKernel)
    (parents : ParentPoints p) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b) (state : GameState) (input : FrameInput)
    (hfields : FieldsAgree p b fields state)
    (htime : addTime state.currentTime input.delta = ok (state.currentTime + input.delta))
    (hfit : state.touchHoldGroupStates.isEmpty = true →
      PrefixFits b.capacity state.activeTouchHolds []) :
    initializeFrame p b addTime fields state input = ok (initialFrame state input) := by
  obtain ⟨slides, hslides, hslideView⟩ := refresh_parents_equiv p parents fields.slides
  have hfit' : (b.rep.groups fields.groups).isEmpty = true →
      PrefixFits b.capacity (b.sourceView fields.holds) [] := by
    intro hempty
    rw [hfields.holds]
    exact hfit (by simpa only [hfields.groups] using hempty)
  obtain ⟨groups, hgroups, hgroupView⟩ :=
    prepare_body_groups_equiv b body fields.holds fields.groups hfit'
  have hgroupReference : b.rep.groups groups =
      if state.touchHoldGroupStates.isEmpty then
        touchHoldBodyGroupStatesFromHolds state.activeTouchHolds
      else state.touchHoldGroupStates := by
    simpa only [hfields.groups, hfields.holds, SchedulerReference.reference] using hgroupView
  simp only [initializeFrame, htime, hslides, hgroups, bind_tc_ok,
    hslideView, hfields.slides, hgroupReference, packFrame, initialFrame]

/-- Attach the real extracted frame driver to the interpreted initialization.
Phase correctness remains separate and can be supplied by the existing five
traversal compositions; no activation or finalizer code is added here. -/
def processInitialized {F : Type} (p : ParentKernel) (b : BodyKernel)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b) (state : GameState) (input : FrameInput)
    (call : core.ops.function.Fn F (scheduler.FramePhase × FrameWork) FrameWork)
    (f : F) : Result FrameWork := do
  let initial ← initializeFrame p b addTime fields state input
  scheduler.process_frame call initial f

/-- Driver composition without a hinitial premise. It intentionally retains
per-phase contracts. finishFrame here is the existing reference interpretation;
this corollary makes no new claim about the concrete finalization facade. -/
theorem process_initialized_equiv {F : Type} (p : ParentKernel) (b : BodyKernel)
    (parents : ParentPoints p) (body : BodyPoints b)
    (addTime : TimePoint → Duration → Result TimePoint)
    (fields : FrameFields p b) (state : GameState) (input : FrameInput)
    (hfields : FieldsAgree p b fields state)
    (htime : addTime state.currentTime input.delta = ok (state.currentTime + input.delta))
    (hfit : state.touchHoldGroupStates.isEmpty = true →
      PrefixFits b.capacity state.activeTouchHolds [])
    (call : core.ops.function.Fn F (scheduler.FramePhase × FrameWork) FrameWork)
    (f : F)
    (hstep : ∀ phase frame, ∃ next, call.call f (phase, frame) = ok next ∧
      next = referencePass input phase frame) :
    ∃ out, processInitialized p b addTime fields state input call f = ok out ∧
      out = processInOrder (referencePass input) (initialFrame state input) ∧
      finishFrame input out = Scheduler.stepFrame state input := by
  have hinit := initialize_equiv p b parents body addTime fields state input
    hfields htime hfit
  obtain ⟨out, hrun, hout⟩ := process_frame_refines call f
    (fun frame : FrameWork => frame) (referencePass input) (initialFrame state input) hstep
  refine ⟨out, ?_, hout, ?_⟩
  · simp only [processInitialized, hinit, bind_tc_ok, hrun]
  · rw [hout]
    exact reference_frame_equiv state input

end Verification.SchedulerFrameInitializationProof
