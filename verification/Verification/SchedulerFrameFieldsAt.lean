import Verification.SchedulerFrameAllTraversals
import Verification.TapSchedulePassLocal
import Verification.TouchSchedulePassLocal

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

/-!
Current-frame field companions. The executable adapters, phase definitions and
extracted traversals are unchanged. Reads and copies are required only at the
supplied frame; write laws still describe arbitrary supplied native fields at
that frame. These are field laws, not assumed pass/phase results.

Reads are required at this frame only. Tap and Touch size conditions follow
the actual storage-order prefixes through the local pass theorems. Source only;
no compiler, test or extraction has been run for this delivery.
-/

namespace Verification.SchedulerFrameTapProof

structure FrameFieldsAt {W Q N F C E Event D Area Ops : Type}
    (a : FrameAdapters W Q F C E Ops)
    (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (repr : W → SchedulerFrameProof.FrameWork) (frame : W) : Prop where
  frontiers : rep.frontiers (a.frontiers frame) = (repr frame).state.buttonQueueFrontiers
  queues : TapSchedulePassProof.decodeQueues rep (a.queues frame) =
    (repr frame).state.tapQueues
  cursor : rep.cursor (a.cursor frame) = (repr frame).cursor
  empty_events : rep.events a.emptyEvents = []
  write : ∀ frontiers queues events cursor,
    repr (a.write frame frontiers queues events cursor) = writeReference (repr frame)
      (rep.frontiers frontiers, TapSchedulePassProof.decodeQueues rep queues,
        rep.events events, rep.cursor cursor)

theorem tap_phase_refines_at {W Q N F C E Event D Area Ops : Type}
    (clone : core.clone.Clone Q)
    (call : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : FrameAdapters W Q F C E Ops)
    (rep : TapScheduleProof.Representation Q N F C E Event D Area)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput) (capacity : Nat)
    (frame : W) (fields : FrameFieldsAt a rep repr frame)
    (hclone : ∀ queue, clone.clone queue = ok queue)
    (hremaining : ∀ queue, ∃ fuel, a.remaining queue = ok fuel ∧
      fuel.val = TapScheduleModel.remaining (rep.queue queue))
    (hreverse : ∀ events, ∃ reversed, a.reverse events = ok reversed ∧
      rep.events reversed = (rep.events events).reverse)
    (atoms : ∀ zone, TapScheduleProof.AtomicAgrees (call frame) (a.ops frame zone) rep
      (TapSchedulePassProof.zoneContext (frameContext input (repr frame))
        (Areas.toLnmButtonZone zone)) capacity)
    (hready : TapSchedulePassLocalProof.Ready rep
      (frameContext input (repr frame)) capacity
      (a.frontiers frame) (a.queues frame) (a.cursor frame)) :
    ∃ next, tapPhase clone call a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Tap (repr frame) := by
  obtain ⟨out, hrun, hout⟩ := TapSchedulePassLocalProof.process_equiv clone (call frame)
    (a.ops frame) a.remaining a.reverse a.emptyEvents rep
    (frameContext input (repr frame)) capacity hclone atoms hremaining hreverse
    fields.empty_events (a.frontiers frame) (a.queues frame) (a.cursor frame) hready
  refine ⟨a.write frame out.1 out.2.1 out.2.2.1 out.2.2.2, ?_, ?_⟩
  · simp only [tapPhase, hrun, bind_tc_ok]
  · rw [fields.write, hout, fields.frontiers, fields.queues, fields.cursor]
    exact reference_tap input (repr frame)

end Verification.SchedulerFrameTapProof

namespace Verification.SchedulerFrameHoldProof
open SchedulerFrameProof HoldScheduleModel HoldScheduleProof

structure FrameFieldsAt {W S Z N F Q C H E Event D Area Ops : Type}
    (a : FrameAdapters W S F Q C H E Ops)
    (rep : Representation S Z N F Q C H E Event D Area) (repr : W → FrameWork) (frame : W) : Prop where
  holds : rep.holds (a.holds frame) = (repr frame).state.activeHolds
  frontiers : rep.frontiers (a.frontiers frame) = (repr frame).state.buttonQueueFrontiers
  queues : rep.queues (a.queues frame) = (repr frame).state.holdQueues
  cursor : rep.cursor (a.cursor frame) = (repr frame).cursor
  copy_frontiers : ∃ copied, a.copyFrontiers (a.frontiers frame) = ok copied ∧
    rep.frontiers copied = rep.frontiers (a.frontiers frame)
  copy_queues : ∃ copied, a.copyQueues (a.queues frame) = ok copied ∧
    rep.queues copied = rep.queues (a.queues frame)
  copy_cursor : ∃ copied, a.copyCursor (a.cursor frame) = ok copied ∧
    rep.cursor copied = rep.cursor (a.cursor frame)
  empty_active : rep.active a.emptyActive = []
  empty_events : rep.events a.emptyEvents = []
  write : ∀ frontiers queues holds events cursor,
    repr (a.write frame frontiers queues holds events cursor) =
      writeReference (repr frame)
        (rep.frontiers frontiers, rep.queues queues, rep.active holds,
          rep.events events, rep.cursor cursor)

theorem initial_work_spec_at {W S Z N F Q C H E Event D Area Ops : Type}
    (a : FrameAdapters W S F Q C H E Ops)
    (rep : Representation S Z N F Q C H E Event D Area) (repr : W → FrameWork)
    (frame : W) (fields : FrameFieldsAt a rep repr frame) :
    ∃ initial, initialWork a frame = ok initial ∧
      interpret rep initial = initialReference (repr frame) := by
  obtain ⟨frontiers, hf, hfrontiers⟩ := fields.copy_frontiers
  obtain ⟨queues, hq, hqueues⟩ := fields.copy_queues
  obtain ⟨cursor, hc, hcursor⟩ := fields.copy_cursor
  refine ⟨⟨frontiers, queues, cursor, a.emptyActive, a.emptyEvents⟩, ?_, ?_⟩
  · simp only [initialWork, hf, hq, hc, bind_tc_ok]
  · simp only [interpret, initialReference, hfrontiers, hqueues, hcursor,
      fields.frontiers, fields.queues, fields.cursor,
      fields.empty_active, fields.empty_events]

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
variable {W S Z N F Q C H E Event D Area Ops Callback : Type}
  (schedule : hold_schedule.HoldSchedule Ops S Z N F Q C H E Event D Area)
  (a : FrameAdapters W S F Q C H E Ops)
  (rep : Representation S Z N F Q C H E Event D Area)
  (pack : HoldScheduleJudgmentsProof.OutputEncoding N Event) (repr : W → FrameWork)

/-- Current context is installed at the call, never at frame initialization. -/
structure HoldContractsAt (cap : Capacity) (input : FrameInput) (frame : W) : Prop where
  fields : FrameFieldsAt a rep repr frame
  remaining :
    HoldScheduleJudgmentsProof.RemainingAtoms schedule (a.ops input frame) rep
      (frameContext input (repr frame)) cap
  encoding :
    HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode rep pack
      (frameContext input (repr frame))
  fits : Fits cap
    (resume (frameContext input (repr frame)) (repr frame).state.activeHolds
      (initialReference (repr frame)))

include integerLaw decode_encode in
theorem hold_phase_refines_at (input : FrameInput) (cap : Capacity) (frame : W)
    (laws : HoldContractsAt inst integerOps encode schedule a rep pack repr cap input frame) :
    ∃ next, holdPhase inst integerOps encode schedule a rep pack repr input frame = ok next ∧
      repr next = referencePass input .Hold (repr frame) := by
  obtain ⟨initial, hinitial, hinit⟩ := initial_work_spec_at a rep repr frame laws.fields
  obtain ⟨out, hrun, houtput⟩ := spec_imp_exists
    (HoldScheduleJudgmentsProof.process_equiv inst integerOps decode integerLaw encode decode_encode
      schedule (a.ops input frame) rep pack (frameContext input (repr frame)) cap
      laws.encoding laws.remaining (a.holds frame) initial
      (repr frame).state.buttonQueueFrontiers (repr frame).state.holdQueues (repr frame).cursor
      hinit (by simpa only [laws.fields.holds, initialReference] using laws.fits))
  refine ⟨a.write frame out.frontiers out.queues out.active out.events out.cursor, ?_, ?_⟩
  · simp only [holdPhase, hinitial, bind_tc_ok, hrun]
  · calc
      repr (a.write frame out.frontiers out.queues out.active out.events out.cursor) =
          writeReference (repr frame) (interpret rep out).output :=
        laws.fields.write out.frontiers out.queues out.active out.events out.cursor
      _ = writeReference (repr frame) (referenceOutput input (repr frame)) :=
        congrArg (writeReference (repr frame)) (by
          simpa only [referenceOutput, laws.fields.holds] using houtput)
      _ = referencePass input .Hold (repr frame) := reference_hold input (repr frame)

end Verification.SchedulerFrameHoldProof

namespace Verification.SchedulerFrameSlideProof

structure FrameFieldsAt {W S N E A C Ops : Type}
    (a : FrameAdapters W S E A C Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (repr : W → SchedulerFrameProof.FrameWork) (frame : W) : Prop where
  slides : rep.slides (a.slides frame) = (repr frame).state.slides
  length : ∀ copied, a.copySlides (a.slides frame) = ok copied →
    (a.length copied).val = (rep.slides copied).length
  empty_events : rep.events a.emptyEvents = []
  empty_audio : rep.audio a.emptyAudio = []
  empty_render : rep.render a.emptyRender = []
  write : ∀ slides events audio render,
    repr (a.write frame slides events audio render) =
      writeReference (repr frame)
        (rep.slides slides, rep.events events, rep.audio audio, rep.render render)

theorem slide_phase_refines_at {W S N E A C Ops : Type}
    (call : slide_schedule.SlideSchedule Ops S N E A C)
    (a : FrameAdapters W S E A C Ops)
    (rep : SlideScheduleProof.Representation S N E A C)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput)
    (cap : SlideScheduleProof.Capacity)
    (parents : SlideScheduleParentsProof.Adapters S N C)
    (snapshotRepr : parents.Snapshot → List SlideNote) (id : parents.Id → Nat)
    (parentCapacity : Nat) (frame : W) (fields : FrameFieldsAt a rep repr frame)
    (points : SlideScheduleParentsProof.PointsAgree parents rep.slides rep.render
      snapshotRepr id parentCapacity)
    (metadata : SlideScheduleParentsProof.MetadataAgrees parents rep.note id)
    (containers : SlideScheduleParentsProof.ContainersAgree parents rep.slides rep.render
      snapshotRepr)
    (hforceCapacity : 0 < parentCapacity)
    (hancestorCapacity : ∀ slides, (parents.length slides).val ≤ parentCapacity)
    (hparentCopy : ∀ slides, SlideScheduleParentsProof.CopyAgrees parents rep.slides slides)
    (hcopy : ∃ copied, a.copySlides (a.slides frame) = ok copied ∧
      rep.slides copied = rep.slides (a.slides frame))
    (facade : SlideScheduleParentsProof.FacadeCalls call (a.ops frame) parents)
    (other : SlideScheduleParentsProof.OtherAtoms call (a.ops frame) rep
      (frameContext input (repr frame)) cap)
    (hfit : SlideScheduleProof.Fits cap (referenceOutput input (repr frame))) :
    ∃ next, slidePhase call a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Slide (repr frame) := by
  obtain ⟨copied, hcopied, hcopiedRepr⟩ := hcopy
  have atoms := SlideScheduleParentsProof.atomic_agrees_of_kernels
    call (a.ops frame) rep (frameContext input (repr frame)) cap parents
    snapshotRepr id parentCapacity points metadata containers facade
    hforceCapacity hancestorCapacity hparentCopy other
  have hinit : SlideScheduleProof.interpret rep (initialWork a copied) =
      SlideScheduleProof.initialReference [] (repr frame).state.slides [] [] [] := by
    simp [SlideScheduleProof.interpret, initialWork, SlideScheduleProof.initialReference,
      fields.empty_events, fields.empty_audio, fields.empty_render, hcopiedRepr,
      fields.slides]
  have hlen : (rep.slides (initialWork a copied).slides).length = (a.length copied).val :=
    (fields.length copied hcopied).symm
  obtain ⟨out, hrun, houtput, _, _⟩ := spec_imp_exists
    (SlideScheduleProof.process_equiv call (a.ops frame) rep
      (frameContext input (repr frame)) cap atoms (a.length copied) (initialWork a copied)
      [] (repr frame).state.slides [] [] [] hinit hlen hfit)
  refine ⟨a.write frame out.slides out.events out.audio out.render, ?_, ?_⟩
  · simp only [slidePhase, hcopied, bind_tc_ok, hrun]
  · calc
      repr (a.write frame out.slides out.events out.audio out.render) =
          writeReference (repr frame) (SlideScheduleProof.interpret rep out).output :=
        fields.write out.slides out.events out.audio out.render
      _ = writeReference (repr frame) (referenceOutput input (repr frame)) :=
        congrArg (writeReference (repr frame)) houtput
      _ = SchedulerFrameProof.referencePass input .Slide (repr frame) :=
        reference_slide input (repr frame)

end Verification.SchedulerFrameSlideProof

namespace Verification.SchedulerFrameTouchProof

structure FrameFieldsAt {W : Type} (impl : W → TouchSchedulePassProof.Implementation)
    (a : FrameAdapters W impl) (repr : W → SchedulerFrameProof.FrameWork) (frame : W) : Prop where
  frontiers : (impl frame).rep.frontiers (a.frontiers frame) =
    (repr frame).state.touchQueueFrontiers
  queues : TouchSchedulePassProof.decodeQueues (impl frame) (a.queues frame) =
    (repr frame).state.touchQueues
  cursor : (impl frame).rep.cursor (a.cursor frame) = (repr frame).cursor
  groups : (impl frame).rep.groups (a.groups frame) =
    (repr frame).state.touchGroupStates
  write : ∀ frontiers queues events cursor groups,
    repr (a.write frame frontiers queues events cursor groups) = writeReference (repr frame)
      ((impl frame).rep.frontiers frontiers, TouchSchedulePassProof.decodeQueues (impl frame) queues,
        (impl frame).rep.events events, (impl frame).rep.cursor cursor, (impl frame).rep.groups groups)

theorem touch_phase_refines_at {W : Type} (impl : W → TouchSchedulePassProof.Implementation)
    (a : FrameAdapters W impl) (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput)
    (frame : W) (fields : FrameFieldsAt impl a repr frame)
    (groupCapacity : Nat)
    (laws : TouchSchedulePassLocalProof.AgreesWithin (impl frame)
      (frameContext input (repr frame)) groupCapacity)
    (ready : TouchSchedulePassLocalProof.Ready (impl frame)
      (frameContext input (repr frame)) groupCapacity
      (a.frontiers frame) (a.queues frame) (a.cursor frame) (a.groups frame)) :
    ∃ next, touchPhase impl a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Touch (repr frame) := by
  obtain ⟨out, hrun, hout⟩ := TouchSchedulePassLocalProof.process_equiv (impl frame)
    (frameContext input (repr frame)) groupCapacity laws (a.frontiers frame) (a.queues frame)
    (a.cursor frame) (a.groups frame) ready
  refine ⟨a.write frame out.1 out.2.1 out.2.2.1 out.2.2.2.1 out.2.2.2.2, ?_, ?_⟩
  · simp only [touchPhase, hrun, bind_tc_ok]
  · rw [fields.write, hout, fields.frontiers, fields.queues, fields.cursor, fields.groups]
    exact reference_touch input (repr frame)

end Verification.SchedulerFrameTouchProof

namespace Verification.SchedulerFrameTouchHoldProof
open SchedulerFrameProof TouchHoldScheduleModel TouchHoldScheduleProof

structure FrameFieldsAt {W S A N F Stores C H E Event D Z Id Grade G B Ops : Type}
    (a : FrameAdapters W S F Stores C H E G B Ops)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (repr : W → FrameWork) (frame : W) : Prop where
  holds : rep.holds (a.holds frame) = (repr frame).state.activeTouchHolds
  frontiers : rep.frontiers (a.frontiers frame) =
    (repr frame).state.touchQueueFrontiers
  queues : rep.queues (a.queues frame) = (repr frame).state.touchHoldQueues
  cursor : rep.cursor (a.cursor frame) = (repr frame).cursor
  groups : rep.groups (a.groups frame) = (repr frame).state.touchGroupStates
  body_groups : rep.bodyGroups (a.bodyGroups frame) =
    (repr frame).state.touchHoldGroupStates
  copy_frontiers : ∃ copied, a.copyFrontiers (a.frontiers frame) = ok copied ∧
    rep.frontiers copied = rep.frontiers (a.frontiers frame)
  copy_queues : ∃ copied, a.copyQueues (a.queues frame) = ok copied ∧
    rep.queues copied = rep.queues (a.queues frame)
  copy_cursor : ∃ copied, a.copyCursor (a.cursor frame) = ok copied ∧
    rep.cursor copied = rep.cursor (a.cursor frame)
  copy_groups : ∃ copied, a.copyGroups (a.groups frame) = ok copied ∧
    rep.groups copied = rep.groups (a.groups frame)
  copy_body_groups : ∃ copied, a.copyBodyGroups (a.bodyGroups frame) = ok copied ∧
    rep.bodyGroups copied = rep.bodyGroups (a.bodyGroups frame)
  empty_active : rep.active a.emptyActive = []
  empty_events : rep.events a.emptyEvents = []
  write : ∀ frontiers queues holds events cursor groups bodyGroups,
    repr (a.write frame frontiers queues holds events cursor groups bodyGroups) =
      writeReference (repr frame)
        (rep.frontiers frontiers, rep.queues queues, rep.active holds, rep.events events,
          rep.cursor cursor, rep.groups groups, rep.bodyGroups bodyGroups)

theorem initial_work_spec_at {W S A N F Stores C H E Event D Z Id Grade G B Ops : Type}
    (a : FrameAdapters W S F Stores C H E G B Ops)
    (rep : Representation S A N F Stores C H E Event D Z Id Grade G B)
    (repr : W → FrameWork) (frame : W) (fields : FrameFieldsAt a rep repr frame) :
    ∃ initial, initialWork a frame = ok initial ∧
      interpret rep initial = initialReference (repr frame) := by
  obtain ⟨frontiers, hf, hfrontiers⟩ := fields.copy_frontiers
  obtain ⟨queues, hq, hqueues⟩ := fields.copy_queues
  obtain ⟨cursor, hc, hcursor⟩ := fields.copy_cursor
  obtain ⟨groups, hg, hgroups⟩ := fields.copy_groups
  obtain ⟨bodyGroups, hb, hbodyGroups⟩ := fields.copy_body_groups
  refine ⟨⟨frontiers, queues, cursor, a.emptyActive, a.emptyEvents, groups, bodyGroups⟩, ?_, ?_⟩
  · simp only [initialWork, hf, hq, hc, hg, hb, bind_tc_ok]
  · simp only [interpret, initialReference, hfrontiers, hqueues, hcursor, hgroups, hbodyGroups,
      fields.frontiers, fields.queues, fields.cursor, fields.groups,
      fields.body_groups, fields.empty_active, fields.empty_events]

variable {I J W Callback : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (touchHold : TouchHoldImplementation W) (repr : W → FrameWork)

structure ContractsAt (input : FrameInput) (frame : W) : Prop where
  fields : FrameFieldsAt touchHold.adapters touchHold.rep repr frame
  remaining : TouchHoldScheduleQueueJudgmentsProof.OtherAtoms
    touchHold.schedule (touchHold.adapters.ops input frame) touchHold.rep
    (frameContext input (repr frame)) touchHold.capacity
  encoding : TouchHoldScheduleJudgmentsProof.EncodingReady
    inst integerOps encode touchHold.rep touchHold.pack (frameContext input (repr frame))
  queues : TouchHoldScheduleQueueJudgmentsProof.QueuePoints
    touchHold.queueSlot touchHold.queueFacade touchHold.replaceCall touchHold.cursorCall
    (touchHold.adapters.ops input frame) touchHold.rep touchHold.queueRep touchHold.queueCapacity
  headPoints : TouchHoldGroupsProof.HeadAtoms touchHold.heads
    (touchHold.adapters.ops input frame) (headRep touchHold.rep) touchHold.capacity.heads
  bodyPoints : TouchHoldGroupsProof.BodyAtoms touchHold.bodies
    (touchHold.adapters.ops input frame) (bodyRep touchHold.rep) touchHold.capacity.bodies
  safe : Safe (frameContext input (repr frame)) touchHold.capacity
    (repr frame).state.activeTouchHolds (initialReference (repr frame))

include integerLaw decode_encode in
/-- Derive the original TouchHold pass from judgments, group and queue kernels. -/
theorem touch_hold_phase_refines_at (input : FrameInput) (frame : W)
    (laws : ContractsAt inst integerOps encode touchHold repr input frame) :
    ∃ next, touchHoldPhase inst integerOps encode touchHold repr input frame = ok next ∧
      repr next = referencePass input .TouchHold (repr frame) := by
  obtain ⟨initial, hinitial, hinit⟩ :=
    initial_work_spec_at touchHold.adapters touchHold.rep repr frame laws.fields
  obtain ⟨out, hrun, houtput⟩ := spec_imp_exists
    (TouchHoldScheduleQueueJudgmentsProof.process_equiv
      inst integerOps decode integerLaw encode decode_encode
      touchHold.schedule touchHold.heads touchHold.bodies (touchHold.adapters.ops input frame)
      touchHold.rep touchHold.pack (frameContext input (repr frame)) touchHold.capacity
      touchHold.queueSlot touchHold.queueFacade touchHold.replaceCall touchHold.cursorCall
      touchHold.queueRep touchHold.queueCapacity laws.encoding laws.remaining
      laws.queues laws.headPoints laws.bodyPoints
      (touchHold.adapters.holds frame) initial (repr frame).state.touchQueueFrontiers
      (repr frame).state.touchHoldQueues (repr frame).cursor (repr frame).state.touchGroupStates
      (repr frame).state.touchHoldGroupStates hinit
      (by simpa only [laws.fields.holds, initialReference] using laws.safe))
  refine ⟨touchHold.adapters.write frame out.frontiers out.queues out.active out.events
    out.cursor out.groups out.body_groups, ?_, ?_⟩
  · simp only [touchHoldPhase, hinitial, bind_tc_ok, hrun]
  · calc
      repr (touchHold.adapters.write frame out.frontiers out.queues out.active out.events
          out.cursor out.groups out.body_groups) =
          writeReference (repr frame) (interpret touchHold.rep out).output :=
        laws.fields.write out.frontiers out.queues out.active out.events
          out.cursor out.groups out.body_groups
      _ = writeReference (repr frame) (referenceOutput input (repr frame)) :=
        congrArg (writeReference (repr frame)) (by
          simpa only [referenceOutput, laws.fields.holds] using houtput)
      _ = referencePass input .TouchHold (repr frame) := reference_touch_hold input (repr frame)

end Verification.SchedulerFrameTouchHoldProof
