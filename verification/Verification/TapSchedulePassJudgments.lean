import Verification.TapScheduleJudgments
import Verification.TapSchedulePass
import Verification.SchedulerFrameTap

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TapSchedulePassJudgmentsProof

open TapScheduleProof TapSchedulePassProof TapScheduleJudgmentsProof

/-!
Install the proved Tap/SlideHead judgment/lifecycle composition in the existing
Tap pass and frame phase. The outer map_accum uses ONE trait value for every
zone, with a separate ops value for each zone. installStep reads only the
context's currentTime and style: its note and diff are already supplied by the
queue body. Consequently its installation, and its output encoding condition,
are definitionally independent of the zone in zoneContext.

We choose the actual ButtonZone constructor K1 as an installation anchor, then
prove equality with installation at every zone. This does not run other zones
as K1: the fifteen remaining point laws retain each actual zone's context and
ops. In particular input selection, button frontiers and click counters remain
zone-specific. AtomicAgrees.step is derived from TapScheduleJudgments rather
than required from the caller. No queue, complete pass or frame-phase equation
is a premise. Traversal and frame writes reuse the existing proofs unchanged.

Remaining boundaries: IntegerAgrees and exact integer encoding, concrete
Integer/BigInt arithmetic, note/event/time interpretations, successful field
encodings and native allocation; all fifteen non-step atoms at each zone;
identity queue cloning, remaining-count conversion to Usize, empty events,
event reversal, and the existing uniform capacity bound. Frame composition
also retains FrameFieldsAgree, including the write's other-field preservation.
The universal capacity condition is retained as an obligation, not inferred
from a single final buffer or asserted for arbitrary native allocations.

The conclusions execute the existing composition of Generated ButtonVec
map_accum and tap_schedule.process, and its installed Tap frame phase. They
do not identify these constructed traits/adapters with the unextracted Rust
TapScheduleFrame, process_tap_notes or FrameWork facades, establish OOM traces,
or discharge the other phases of a full GameState/frame trace.
-/

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- Only currentTime/style from the context occur in the installed step.
This is equality of the complete trait values, with the same remaining fields. -/
theorem install_step_zone_eq {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (left right : ButtonZone) :
    TapScheduleJudgmentsProof.installStep inst integerOps decode encode base rep pack
        (zoneContext ctx left) =
      TapScheduleJudgmentsProof.installStep inst integerOps decode encode base rep pack
        (zoneContext ctx right) := rfl

/-- The field encoding condition can be supplied once per pass, without
assuming any lifecycle equation or encoding success for unrelated outputs. -/
theorem encoding_ready_zone_iff {Q N F C E Event D Area : Type}
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (left right : ButtonZone) :
    EncodingReady inst integerOps decode encode rep pack (zoneContext ctx left) ↔
      EncodingReady inst integerOps decode encode rep pack (zoneContext ctx right) := Iff.rfl

/-- One installed trait value shared by all slots of the existing map_accum.
The anchor affects no installed operation; each slot still receives ops zone. -/
def installPassStep {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) : tap_schedule.TapSchedule Ops Q N F C E Event D Area :=
  TapScheduleJudgmentsProof.installStep inst integerOps decode encode base rep pack
    (zoneContext ctx .K1)

theorem install_pass_step_zone {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (zone : ButtonZone) :
    installPassStep inst integerOps decode encode base rep pack ctx =
      TapScheduleJudgmentsProof.installStep inst integerOps decode encode base rep pack
        (zoneContext ctx zone) :=
  install_step_zone_eq inst integerOps decode encode base rep pack ctx .K1 zone

/-- Each frame supplies its own time/style; all zones within that frame share
the resulting installed trait. Frame field interpretation stays explicit. -/
def installFrameStep {W Q N F C E Event D Area Ops : Type}
    (base : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput) :
    W → tap_schedule.TapSchedule Ops Q N F C E Event D Area :=
  fun frame => installPassStep inst integerOps decode encode (base frame) rep pack
    (SchedulerFrameTapProof.frameContext input (repr frame))

include integerLaw decode_encode

/-- Discharge the per-note step law for every zone of a single shared call.
Only the non-step point operations retain zone-dependent hypotheses. -/
theorem atomic_agrees_of_judgments {Q N F C E Event D Area Ops : Type}
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (capacity : Nat)
    (ready : EncodingReady inst integerOps decode encode rep pack (zoneContext ctx .K1))
    (remainingAtoms : ∀ zone, RemainingAtoms base (ops zone) rep
      (zoneContext ctx (Areas.toLnmButtonZone zone)) capacity) :
    ∀ zone, AtomicAgrees (installPassStep inst integerOps decode encode base rep pack ctx)
      (ops zone) rep (zoneContext ctx (Areas.toLnmButtonZone zone)) capacity := by
  intro zone
  have zoneReady : EncodingReady inst integerOps decode encode rep pack
      (zoneContext ctx (Areas.toLnmButtonZone zone)) :=
    (encoding_ready_zone_iff inst integerOps decode encode rep pack ctx
      .K1 (Areas.toLnmButtonZone zone)).mp ready
  rw [install_pass_step_zone inst integerOps decode encode base rep pack ctx
    (Areas.toLnmButtonZone zone)]
  exact TapScheduleJudgmentsProof.atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    base (ops zone) rep pack (zoneContext ctx (Areas.toLnmButtonZone zone))
    capacity zoneReady (remainingAtoms zone)

/-- The existing generated map_accum/queue composition, with the proved
judgment/lifecycle step installed and no per-note step or whole-pass premise.
Storage order, shared accumulator threading and the final reversal are inherited. -/
theorem process_equiv {Q N F C E Event D Area Ops : Type}
    (clone : core.clone.Clone Q)
    (base : tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (ops : areas.ButtonZone → Ops) (remaining : Q → Result Usize)
    (reverse : E → Result E) (emptyEvents : E)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (ctx : Context) (capacity : Nat)
    (ready : EncodingReady inst integerOps decode encode rep pack (zoneContext ctx .K1))
    (remainingAtoms : ∀ zone, RemainingAtoms base (ops zone) rep
      (zoneContext ctx (Areas.toLnmButtonZone zone)) capacity)
    (hclone : ∀ queue, clone.clone queue = ok queue)
    (hremaining : ∀ queue, ∃ fuel, remaining queue = ok fuel ∧
      fuel.val = TapScheduleModel.remaining (rep.queue queue))
    (hreverse : ∀ events, ∃ reversed, reverse events = ok reversed ∧
      rep.events reversed = (rep.events events).reverse)
    (hempty : rep.events emptyEvents = [])
    (hfit : ∀ zone queue (acc : F × C × E),
      (TapScheduleModel.resume (zoneContext ctx (Areas.toLnmButtonZone zone))
        (TapScheduleModel.remaining (rep.queue queue))
        ⟨rep.queue queue, rep.frontiers acc.1, rep.cursor acc.2.1, rep.events acc.2.2⟩).2.2.2.length
          ≤ capacity)
    (frontiers : F) (queues : storage.ButtonVec Q) (cursor : C) :
    ∃ out, TapSchedulePassProof.process clone
      (installPassStep inst integerOps decode encode base rep pack ctx)
      ops remaining reverse emptyEvents frontiers queues cursor = ok out ∧
      (rep.frontiers out.1, decodeQueues rep out.2.1, rep.events out.2.2.1, rep.cursor out.2.2.2) =
        reference ctx (rep.frontiers frontiers) (decodeQueues rep queues) (rep.cursor cursor) := by
  have atoms := atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    base ops rep pack ctx capacity ready remainingAtoms
  exact TapSchedulePassProof.process_equiv clone
    (installPassStep inst integerOps decode encode base rep pack ctx)
    ops remaining reverse emptyEvents rep ctx capacity hclone atoms
    hremaining hreverse hempty hfit frontiers queues cursor

/-- The installed Tap phase refines the original frame's Tap referencePass.
Frame fields/write laws remain atomic; no pass, phase or step equation is assumed. -/
theorem tap_phase_refines {W Q N F C E Event D Area Ops : Type}
    (clone : core.clone.Clone Q)
    (base : W → tap_schedule.TapSchedule Ops Q N F C E Event D Area)
    (a : SchedulerFrameTapProof.FrameAdapters W Q F C E Ops)
    (rep : Representation Q N F C E Event D Area) (pack : OutputEncoding N Event)
    (repr : W → SchedulerFrameProof.FrameWork) (input : FrameInput) (capacity : Nat)
    (fields : SchedulerFrameTapProof.FrameFieldsAgree a rep repr)
    (hclone : ∀ queue, clone.clone queue = ok queue)
    (hremaining : ∀ queue, ∃ fuel, a.remaining queue = ok fuel ∧
      fuel.val = TapScheduleModel.remaining (rep.queue queue))
    (hreverse : ∀ events, ∃ reversed, a.reverse events = ok reversed ∧
      rep.events reversed = (rep.events events).reverse)
    (frame : W)
    (ready : EncodingReady inst integerOps decode encode rep pack
      (zoneContext (SchedulerFrameTapProof.frameContext input (repr frame)) .K1))
    (remainingAtoms : ∀ zone, RemainingAtoms (base frame) (a.ops frame zone) rep
      (zoneContext (SchedulerFrameTapProof.frameContext input (repr frame))
        (Areas.toLnmButtonZone zone)) capacity)
    (hfit : ∀ zone queue (acc : F × C × E),
      (TapScheduleModel.resume
        (zoneContext (SchedulerFrameTapProof.frameContext input (repr frame))
          (Areas.toLnmButtonZone zone)) (TapScheduleModel.remaining (rep.queue queue))
        ⟨rep.queue queue, rep.frontiers acc.1, rep.cursor acc.2.1, rep.events acc.2.2⟩).2.2.2.length
          ≤ capacity) :
    ∃ next, SchedulerFrameTapProof.tapPhase clone
      (installFrameStep inst integerOps decode encode base rep pack repr input) a frame = ok next ∧
      repr next = SchedulerFrameProof.referencePass input .Tap (repr frame) := by
  have atoms := atomic_agrees_of_judgments
    inst integerOps decode integerLaw encode decode_encode
    (base frame) (a.ops frame) rep pack
    (SchedulerFrameTapProof.frameContext input (repr frame)) capacity ready remainingAtoms
  exact SchedulerFrameTapProof.tap_phase_refines clone
    (installFrameStep inst integerOps decode encode base rep pack repr input)
    a rep repr input capacity fields hclone hremaining hreverse frame atoms hfit

end Verification.TapSchedulePassJudgmentsProof
