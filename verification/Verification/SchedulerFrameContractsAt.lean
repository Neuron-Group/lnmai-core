import Verification.SchedulerFrameFieldsAt

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.SchedulerFrameContractsAtProof
open SchedulerFrameProof SchedulerFrameTraversalsProof

/-!
Five phase contracts at a supplied current frame. No field is a complete
phase/body/step equality; installed judgments, group and queue kernels still
supply those results. The existing executable phase wrappers are reused.

Tap and Touch size obligations refer to the actual vector and its source
accumulators after each visited slot. Touch fuel has an explicit scalar bound;
group registration needs room only for the actual registration output. The
former uniform Tap/Touch output, +1-fuel and missing-id-room premises are absent.
Inherited same-size native field and point-operation laws remain explicit.
-/

variable {I J W : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → Int)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : Int → I) (decode_encode : ∀ value, decode (encode value) = value)
  (tap : TapImplementation W) (hold : HoldImplementation W)
  (slide : SlideImplementation W)
  (touch : SchedulerFrameTouchJudgmentsProof.TouchImplementation W)
  (repr : W → FrameWork)

structure TapAt (input : FrameInput) (frame : W) : Prop where
  fields : SchedulerFrameTapProof.FrameFieldsAt tap.adapters tap.rep repr frame
  clone : ∀ queue, tap.clone.clone queue = ok queue
  remaining_count : ∀ queue, ∃ fuel, tap.adapters.remaining queue = ok fuel ∧
    fuel.val = TapScheduleModel.remaining (tap.rep.queue queue)
  reverse : ∀ events, ∃ reversed, tap.adapters.reverse events = ok reversed ∧
    tap.rep.events reversed = (tap.rep.events events).reverse
  encoding : TapScheduleJudgmentsProof.EncodingReady
    inst integerOps decode encode tap.rep tap.pack
    (TapSchedulePassProof.zoneContext
      (SchedulerFrameTapProof.frameContext input (repr frame)) .K1)
  remaining : ∀ zone, TapScheduleJudgmentsProof.RemainingAtoms
    (tap.schedule frame) (tap.adapters.ops frame zone) tap.rep
    (TapSchedulePassProof.zoneContext
      (SchedulerFrameTapProof.frameContext input (repr frame))
      (Areas.toLnmButtonZone zone)) tap.capacity
  prefix : TapSchedulePassLocalProof.Ready tap.rep
    (SchedulerFrameTapProof.frameContext input (repr frame)) tap.capacity
    (tap.adapters.frontiers frame) (tap.adapters.queues frame) (tap.adapters.cursor frame)

structure HoldAt (input : FrameInput) (frame : W) : Prop where
  fields : SchedulerFrameHoldProof.FrameFieldsAt hold.adapters hold.rep repr frame
  remaining :
    HoldScheduleQueueJudgmentsProof.OtherAtoms hold.schedule (hold.adapters.ops input frame)
      hold.rep (SchedulerFrameHoldProof.frameContext input (repr frame)) hold.capacity
  encoding :
    HoldScheduleJudgmentsProof.EncodingReady inst integerOps encode hold.rep hold.pack
      (SchedulerFrameHoldProof.frameContext input (repr frame))
  queues :
    HoldScheduleQueueJudgmentsProof.QueuePoints hold.queueSlot hold.queueFacade
      hold.replaceCall hold.cursorCall (hold.adapters.ops input frame) hold.rep
      hold.queueRep hold.queueCapacity
  fits : HoldScheduleProof.Fits hold.capacity
    (HoldScheduleModel.resume (SchedulerFrameHoldProof.frameContext input (repr frame))
      (repr frame).state.activeHolds (SchedulerFrameHoldProof.initialReference (repr frame)))

structure SlideAt (input : FrameInput) (frame : W) : Prop where
  fields : SchedulerFrameSlideProof.FrameFieldsAt slide.adapters slide.rep repr frame
  points : SlideScheduleParentsProof.PointsAgree slide.parents slide.rep.slides slide.rep.render
    slide.snapshotRep slide.id slide.parentCapacity
  metadata : SlideScheduleParentsProof.MetadataAgrees slide.parents slide.rep.note slide.id
  containers : SlideScheduleParentsProof.ContainersAgree slide.parents slide.rep.slides
    slide.rep.render slide.snapshotRep
  force_capacity : 0 < slide.parentCapacity
  ancestor_capacity : ∀ slides, (slide.parents.length slides).val ≤ slide.parentCapacity
  parent_copy : ∀ slides, SlideScheduleParentsProof.CopyAgrees slide.parents slide.rep.slides slides
  frame_copy : ∃ copied, slide.adapters.copySlides (slide.adapters.slides frame) = ok copied ∧
    slide.rep.slides copied = slide.rep.slides (slide.adapters.slides frame)
  parent_facade : SlideScheduleParentsProof.FacadeCalls slide.schedule
    (slide.adapters.ops frame) slide.parents
  remaining : SlideScheduleJudgmentsProof.RemainingAtoms slide.schedule
    (slide.adapters.ops frame) slide.rep slide.capacity
  encoding : SlideScheduleJudgmentsProof.EncodingReady inst integerOps encode slide.rep
    slide.pack (SchedulerFrameSlideProof.frameContext input (repr frame))
  fits : SlideScheduleProof.Fits slide.capacity
    (SchedulerFrameSlideProof.referenceOutput input (repr frame))

structure TouchAt (input : FrameInput) (frame : W) : Prop where
  fields : SchedulerFrameTouchProof.FrameFieldsAt touch.base touch.adapters repr frame
  points : TouchSchedulePassLocalProof.RemainingAtomsWithin (touch.base frame)
    (SchedulerFrameTouchProof.frameContext input (repr frame))
  encoding : TouchSchedulePassJudgmentsProof.EncodingReady inst integerOps decode encode
    (touch.base frame) (SchedulerFrameTouchProof.frameContext input (repr frame))
    (touch.pack frame)
  groupPoints : ∀ area, TouchScheduleJudgmentsProof.GroupAtoms
    (touch.headCall frame area) ((touch.base frame).headOps area)
    (touch.base frame).rep touch.groupCapacity
  prefix : TouchSchedulePassLocalProof.Ready (touch.base frame)
    (SchedulerFrameTouchProof.frameContext input (repr frame)) touch.groupCapacity
    (touch.adapters.frontiers frame) (touch.adapters.queues frame)
    (touch.adapters.cursor frame) (touch.adapters.groups frame)

include integerLaw decode_encode

theorem tap_phase_refines_at (input : FrameInput) (frame : W)
    (laws : TapAt inst integerOps decode encode tap repr input frame) :
    ∃ next, SchedulerFrameTraversalsProof.tapPhase inst integerOps decode encode
        tap repr input frame = ok next ∧
      repr next = referencePass input .Tap (repr frame) := by
  have atoms := TapSchedulePassJudgmentsProof.atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    (tap.schedule frame) (tap.adapters.ops frame) tap.rep tap.pack
    (SchedulerFrameTapProof.frameContext input (repr frame)) tap.capacity
    laws.encoding laws.remaining
  exact SchedulerFrameTapProof.tap_phase_refines_at tap.clone
    (TapSchedulePassJudgmentsProof.installFrameStep inst integerOps decode encode
      tap.schedule tap.rep tap.pack repr input)
    tap.adapters tap.rep repr input tap.capacity frame laws.fields
    laws.clone laws.remaining_count laws.reverse atoms laws.prefix

theorem hold_phase_refines_at (input : FrameInput) (frame : W)
    (laws : HoldAt inst integerOps encode hold repr input frame) :
    ∃ next, SchedulerFrameTraversalsProof.holdPhase inst integerOps encode
        hold repr input frame = ok next ∧
      repr next = referencePass input .Hold (repr frame) := by
  have remaining := HoldScheduleQueueJudgmentsProof.remaining_atoms_of_points
    hold.schedule (hold.adapters.ops input frame) hold.rep
    (SchedulerFrameHoldProof.frameContext input (repr frame)) hold.capacity
    hold.queueSlot hold.queueFacade hold.replaceCall hold.cursorCall
    hold.queueRep hold.queueCapacity laws.remaining laws.queues
  exact SchedulerFrameHoldProof.hold_phase_refines_at
    inst integerOps decode integerLaw encode decode_encode
    hold.queueSchedule hold.adapters hold.rep hold.pack repr input hold.capacity frame
    { fields := laws.fields, remaining := remaining, encoding := laws.encoding, fits := laws.fits }

theorem slide_phase_refines_at (input : FrameInput) (frame : W)
    (laws : SlideAt inst integerOps encode slide repr input frame) :
    ∃ next, SchedulerFrameTraversalsProof.slidePhase inst integerOps encode
        slide repr input frame = ok next ∧
      repr next = referencePass input .Slide (repr frame) := by
  let ctx := SchedulerFrameSlideProof.frameContext input (repr frame)
  let installed := SlideScheduleJudgmentsProof.installStep inst integerOps encode
    slide.schedule slide.rep slide.pack ctx
  have other := SlideScheduleJudgmentsProof.other_atoms_of_judgments
    inst integerOps decode integerLaw encode decode_encode slide.schedule
    (slide.adapters.ops frame) slide.rep slide.pack ctx slide.capacity
    laws.encoding laws.remaining
  have facade : SlideScheduleParentsProof.FacadeCalls installed
      (slide.adapters.ops frame) slide.parents :=
    ⟨laws.parent_facade.force_direct, laws.parent_facade.end_ancestors,
      laws.parent_facade.refresh⟩
  exact SchedulerFrameSlideProof.slide_phase_refines_at installed slide.adapters slide.rep
    repr input slide.capacity slide.parents slide.snapshotRep slide.id slide.parentCapacity
    frame laws.fields laws.points laws.metadata laws.containers laws.force_capacity
    laws.ancestor_capacity laws.parent_copy laws.frame_copy facade other laws.fits

theorem touch_phase_refines_at (input : FrameInput) (frame : W)
    (laws : TouchAt inst integerOps decode encode touch repr input frame) :
    ∃ next, SchedulerFrameTouchJudgmentsProof.touchPhase inst integerOps decode encode
        touch repr input frame = ok next ∧
      repr next = referencePass input .Touch (repr frame) := by
  have fields : SchedulerFrameTouchProof.FrameFieldsAt
      (SchedulerFrameTouchJudgmentsProof.install inst integerOps decode encode touch repr input)
      (SchedulerFrameTouchJudgmentsProof.installedAdapters
        inst integerOps decode encode touch repr input) repr frame :=
    { frontiers := laws.fields.frontiers
      queues := laws.fields.queues
      cursor := laws.fields.cursor
      groups := laws.fields.groups
      write := laws.fields.write }
  exact SchedulerFrameTouchProof.touch_phase_refines_at
    (SchedulerFrameTouchJudgmentsProof.install inst integerOps decode encode touch repr input)
    (SchedulerFrameTouchJudgmentsProof.installedAdapters
      inst integerOps decode encode touch repr input) repr input frame fields touch.groupCapacity
    (TouchSchedulePassLocalProof.agrees_of_judgments_groups
      inst integerOps decode integerLaw encode decode_encode (touch.base frame)
      (SchedulerFrameTouchProof.frameContext input (repr frame))
      (touch.pack frame) (touch.headCall frame) laws.encoding laws.points
      touch.groupCapacity laws.groupPoints)
    (TouchSchedulePassLocalProof.ready_installed inst integerOps decode encode
      (touch.base frame) (SchedulerFrameTouchProof.frameContext input (repr frame))
      (touch.pack frame) (touch.headCall frame) touch.groupCapacity
      (touch.adapters.frontiers frame) (touch.adapters.queues frame)
      (touch.adapters.cursor frame) (touch.adapters.groups frame) laws.prefix)

end Verification.SchedulerFrameContractsAtProof
