import Verification.TapSchedulePassJudgments
import Verification.HoldScheduleQueueJudgments
import Verification.SchedulerFrameJudgments

open Aeneas Aeneas.Std Result lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameTraversalsProof
open SchedulerFrameProof

/-!
Compose the actual extracted Tap, Hold and Slide traversals in one invocation
of the five-phase driver. Each phase installs its judgment/lifecycle step at
the CURRENT accumulator. The remaining complete-phase contracts are Touch
and TouchHold only. The records below package code and primitive adapters;
they contain no assumed whole-phase equalities.

Concrete numeric/container/field/encoding laws, capacity and initialization
remain explicit. Hold queue replacement/normalization are derived from the
extracted queue kernels using slot and facade point laws. This theorem does
not identify the constructed trait with the unextracted runtime facade.
-/

structure TapImplementation (W : Type) where
  Q : Type
  N : Type
  F : Type
  C : Type
  E : Type
  Event : Type
  D : Type
  Area : Type
  Ops : Type
  clone : core.clone.Clone Q
  schedule : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area
  adapters : SchedulerFrameTapProof.FrameAdapters W Q F C E Ops
  rep : TapScheduleProof.Representation Q N F C E Event D Area
  pack : TapScheduleJudgmentsProof.OutputEncoding N Event
  capacity : Nat

structure HoldImplementation (W : Type) where
  S : Type
  Z : Type
  N : Type
  F : Type
  Q : Type
  C : Type
  H : Type
  E : Type
  Event : Type
  D : Type
  Area : Type
  Ops : Type
  schedule : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area
  adapters : SchedulerFrameHoldProof.FrameAdapters W S F Q C H E Ops
  rep : HoldScheduleProof.Representation S Z N F Q C H E Event D Area
  pack : HoldScheduleJudgmentsProof.OutputEncoding N Event
  capacity : HoldScheduleProof.Capacity
  ZoneQueue : Type
  queueSlot : QueueMaintenanceComposition.QueueSlot Ops Q Z ZoneQueue
  queueFacade : QueueMaintenanceComposition.Facade Ops ZoneQueue
  replaceCall : queue_maintenance.QueueReplace Ops ZoneQueue N
  cursorCall : queue_maintenance.QueueCursor Ops ZoneQueue N
  queueRep : ZoneQueue → InputModel.ZoneQueue HoldNote
  queueCapacity : Nat

def HoldImplementation.queueSchedule {W : Type} (hold : HoldImplementation W) :
    hold_schedule.HoldSchedule hold.Ops hold.S hold.Z hold.N hold.F hold.Q
      hold.C hold.H hold.E hold.Event hold.D hold.Area :=
  QueueMaintenanceComposition.withQueueMaintenance hold.schedule hold.queueSlot
    hold.queueFacade hold.replaceCall hold.cursorCall

structure SlideImplementation (W : Type) where
  S : Type
  N : Type
  E : Type
  A : Type
  C : Type
  Ops : Type
  schedule : slide_schedule.SlideSchedule Ops S N E A C
  adapters : SchedulerFrameSlideProof.FrameAdapters W S E A C Ops
  rep : SlideScheduleProof.Representation S N E A C
  pack : SlideScheduleJudgmentsProof.OutputEncoding N E A C
  capacity : SlideScheduleProof.Capacity
  parents : SlideScheduleParentsProof.Adapters S N C
  snapshotRep : parents.Snapshot → List SlideNote
  id : parents.Id → Nat
  parentCapacity : Nat

variable {I J W Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (tap : TapImplementation W) (hold : HoldImplementation W)
  (slide : SlideImplementation W) (repr : W → FrameWork)

structure TapContracts (input : FrameInput) : Prop where
  fields : SchedulerFrameTapProof.FrameFieldsAgree tap.adapters tap.rep repr
  clone : ∀ queue, tap.clone.clone queue = ok queue
  remaining_count : ∀ queue, ∃ fuel, tap.adapters.remaining queue = ok fuel ∧
    fuel.val = TapScheduleModel.remaining (tap.rep.queue queue)
  reverse : ∀ events, ∃ reversed, tap.adapters.reverse events = ok reversed ∧
    tap.rep.events reversed = (tap.rep.events events).reverse
  encoding : ∀ frame, TapScheduleJudgmentsProof.EncodingReady
    inst integerOps decode encode tap.rep tap.pack
    (TapSchedulePassProof.zoneContext
      (SchedulerFrameTapProof.frameContext input (repr frame)) .K1)
  remaining : ∀ frame zone, TapScheduleJudgmentsProof.RemainingAtoms
    (tap.schedule frame) (tap.adapters.ops frame zone) tap.rep
    (TapSchedulePassProof.zoneContext
      (SchedulerFrameTapProof.frameContext input (repr frame))
      (Areas.toLnmButtonZone zone)) tap.capacity
  fits : ∀ frame zone queue (acc : tap.F × tap.C × tap.E),
    (TapScheduleModel.resume
      (TapSchedulePassProof.zoneContext
        (SchedulerFrameTapProof.frameContext input (repr frame))
        (Areas.toLnmButtonZone zone)) (TapScheduleModel.remaining (tap.rep.queue queue))
      ⟨tap.rep.queue queue, tap.rep.frontiers acc.1,
        tap.rep.cursor acc.2.1, tap.rep.events acc.2.2⟩).2.2.2.length ≤ tap.capacity

structure HoldContracts (input : FrameInput) : Prop where
  fields : SchedulerFrameHoldProof.FrameFieldsAgree hold.adapters hold.rep repr
  remaining : ∀ frame,
    HoldScheduleQueueJudgmentsProof.OtherAtoms hold.schedule (hold.adapters.ops input frame)
      hold.rep (SchedulerFrameHoldProof.frameContext input (repr frame)) hold.capacity
  encoding : ∀ frame,
    HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode hold.rep hold.pack
      (SchedulerFrameHoldProof.frameContext input (repr frame))
  queues : ∀ frame,
    HoldScheduleQueueJudgmentsProof.QueuePoints hold.queueSlot hold.queueFacade
      hold.replaceCall hold.cursorCall (hold.adapters.ops input frame) hold.rep
      hold.queueRep hold.queueCapacity
  fits : ∀ frame, HoldScheduleProof.Fits hold.capacity
    (HoldScheduleModel.resume (SchedulerFrameHoldProof.frameContext input (repr frame))
      (repr frame).state.activeHolds (SchedulerFrameHoldProof.initialReference (repr frame)))

structure Contracts (input : FrameInput) : Prop where
  tap : TapContracts inst integerOps decode encode tap repr input
  hold : HoldContracts inst integerOps encode hold repr input
  slide : SchedulerFrameJudgmentsProof.SlideContracts inst integerOps encode
    slide.schedule slide.adapters slide.rep slide.pack repr slide.capacity slide.parents
    slide.snapshotRep slide.id slide.parentCapacity input

def tapPhase (input : FrameInput) (frame : W) : Result W :=
  SchedulerFrameTapProof.tapPhase tap.clone
    (TapSchedulePassJudgmentsProof.installFrameStep inst integerOps decode encode
      tap.schedule tap.rep tap.pack repr input) tap.adapters frame

def holdPhase (input : FrameInput) (frame : W) : Result W :=
  SchedulerFrameHoldProof.holdPhase inst integerOps encode
    hold.queueSchedule hold.adapters hold.rep hold.pack repr input frame

def slidePhase (input : FrameInput) (frame : W) : Result W :=
  SchedulerFrameJudgmentsProof.slidePhase inst integerOps encode
    slide.schedule slide.adapters slide.rep slide.pack repr input frame

def frameDispatch (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput × Callback) (args : scheduler.FramePhase × W) : Result W :=
  match args.1 with
  | .Tap => tapPhase inst integerOps decode encode tap repr callback.1 args.2
  | .Hold => holdPhase inst integerOps encode hold repr callback.1 args.2
  | .Slide => slidePhase inst integerOps encode slide repr callback.1 args.2
  | phase => base.call callback.2 (phase, args.2)

def frameCall (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W) :
    core.ops.function.Fn (FrameInput × Callback) (scheduler.FramePhase × W) W :=
  { FnMutInst :=
      { FnOnceInst :=
          { call_once := frameDispatch inst integerOps decode encode tap hold slide repr base }
        call_mut := fun callback args => do
          let out ← frameDispatch inst integerOps decode encode tap hold slide repr base
            callback args
          ok (out, callback) }
    call := frameDispatch inst integerOps decode encode tap hold slide repr base }

def processAndFinish (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput) :
    Result SchedulerFrameStateProof.FrameOutput :=
  SchedulerFrameStateProof.processAndFinishFromInitial inst integerOps encode
    (frameCall inst integerOps decode encode tap hold slide repr base)
    (input, f) repr initial state input

def trace (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput → Callback) (initializeFrame : GameState → FrameInput → W)
    (state : GameState) (inputs : List FrameInput) :
    Result (GameState × List SchedulerFrameStateProof.FrameOutput) :=
  SchedulerFrameStateProof.traceFromInitial inst integerOps encode
    (frameCall inst integerOps decode encode tap hold slide repr base)
    (fun input => (input, callback input)) repr initializeFrame state inputs

include integerLaw decode_encode

theorem tap_phase_refines (input : FrameInput)
    (laws : TapContracts inst integerOps decode encode tap repr input) (frame : W) :
    ∃ next, tapPhase inst integerOps decode encode tap repr input frame = ok next ∧
      repr next = referencePass input .Tap (repr frame) := by
  exact TapSchedulePassJudgmentsProof.tap_phase_refines
    inst integerOps decode integerLaw encode decode_encode tap.clone tap.schedule
    tap.adapters tap.rep tap.pack repr input tap.capacity laws.fields laws.clone
    laws.remaining_count laws.reverse frame (laws.encoding frame)
    (laws.remaining frame) (laws.fits frame)

theorem hold_phase_refines (input : FrameInput)
    (laws : HoldContracts inst integerOps encode hold repr input) (frame : W) :
    ∃ next, holdPhase inst integerOps encode hold repr input frame = ok next ∧
      repr next = referencePass input .Hold (repr frame) := by
  exact HoldScheduleQueueJudgmentsProof.hold_phase_refines
    inst integerOps decode integerLaw encode decode_encode hold.schedule hold.adapters
    hold.rep hold.pack repr input hold.capacity hold.queueSlot hold.queueFacade
    hold.replaceCall hold.cursorCall hold.queueRep hold.queueCapacity laws.fields
    laws.remaining laws.encoding laws.queues laws.fits frame

theorem slide_phase_refines (input : FrameInput)
    (laws : SchedulerFrameJudgmentsProof.SlideContracts inst integerOps encode
      slide.schedule slide.adapters slide.rep slide.pack repr slide.capacity slide.parents
      slide.snapshotRep slide.id slide.parentCapacity input) (frame : W) :
    ∃ next, slidePhase inst integerOps encode slide repr input frame = ok next ∧
      repr next = referencePass input .Slide (repr frame) := by
  exact SchedulerFrameJudgmentsProof.slide_phase_refines
    inst integerOps decode integerLaw encode decode_encode slide.schedule slide.adapters
    slide.rep slide.pack repr input laws frame

/-- Tap/Hold/Slide use derived traversals at each accumulator. Exactly two
complete pass equations remain: Touch and TouchHold. -/
theorem frame_call_refines
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (input : FrameInput)
    (laws : Contracts inst integerOps decode encode tap hold slide repr input)
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    ∀ phase frame, ∃ next,
      (frameCall inst integerOps decode encode tap hold slide repr base).call
        (input, f) (phase, frame) = ok next ∧
      repr next = referencePass input phase (repr frame) := by
  intro phase frame
  cases phase with
  | Tap =>
      exact tap_phase_refines inst integerOps decode integerLaw encode decode_encode
        tap repr input laws.tap frame
  | Hold =>
      exact hold_phase_refines inst integerOps decode integerLaw encode decode_encode
        hold repr input laws.hold frame
  | Slide =>
      exact slide_phase_refines inst integerOps decode integerLaw encode decode_encode
        slide repr input laws.slide frame
  | Touch => exact hTouch frame
  | TouchHold => exact hTouchHold frame

theorem process_and_finish_equiv
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (f : Callback) (initial : W) (state : GameState) (input : FrameInput)
    (laws : Contracts inst integerOps decode encode tap hold slide repr input)
    (hinitial : repr initial = initialFrame state input)
    (hTouch : ∀ frame, ∃ next, base.call f (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ frame, ∃ next, base.call f (.TouchHold, frame) = ok next ∧
      repr next = referencePass input .TouchHold (repr frame)) :
    processAndFinish inst integerOps decode encode tap hold slide repr base f initial
      state input = ok (Scheduler.stepFrame state input) := by
  exact SchedulerFrameStateProof.process_and_finish_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps decode encode tap hold slide repr base)
    (input, f) repr initial state input hinitial
    (frame_call_refines inst integerOps decode integerLaw encode decode_encode
      tap hold slide repr base f input laws hTouch hTouchHold)

/-- Every intermediate state and ordered output is retained. Input and the
current frame are supplied anew at each iteration; no first-frame capture. -/
theorem trace_equiv
    (base : core.ops.function.Fn Callback (scheduler.FramePhase × W) W)
    (callback : FrameInput → Callback) (initializeFrame : GameState → FrameInput → W)
    (laws : ∀ input, Contracts inst integerOps decode encode tap hold slide repr input)
    (hinitial : ∀ state input, repr (initializeFrame state input) = initialFrame state input)
    (hTouch : ∀ input frame, ∃ next, base.call (callback input) (.Touch, frame) = ok next ∧
      repr next = referencePass input .Touch (repr frame))
    (hTouchHold : ∀ input frame,
      ∃ next, base.call (callback input) (.TouchHold, frame) = ok next ∧
        repr next = referencePass input .TouchHold (repr frame))
    (state : GameState) (inputs : List FrameInput) :
    trace inst integerOps decode encode tap hold slide repr base callback initializeFrame
      state inputs = ok (SchedulerFrameStateProof.referenceTrace state inputs) := by
  exact SchedulerFrameStateProof.trace_from_initial_equiv
    inst integerOps decode encode integerLaw decode_encode
    (frameCall inst integerOps decode encode tap hold slide repr base)
    (fun input => (input, callback input)) repr initializeFrame hinitial
    (fun input => frame_call_refines inst integerOps decode integerLaw encode decode_encode
      tap hold slide repr base (callback input) input (laws input)
      (hTouch input) (hTouchHold input)) state inputs

end Verification.SchedulerFrameTraversalsProof
