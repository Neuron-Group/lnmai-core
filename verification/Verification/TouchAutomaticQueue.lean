import Verification.TouchAutomaticJudgments
import Verification.QueueMaintenanceComposition

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

namespace Verification.TouchAutomaticQueueProof
open TouchAutomaticModel TouchAutomaticProof
open private touchQueueResolved normalizeTouchQueueCursor from LnmaiCore.Scheduler

/-! Compose automatic traversal with the actual extracted queue normalizer.
The facade only copies the queue and replaces its notes field; normalization
is proved from peek/resolved/advance and the existing conversion/length laws.
The group snapshot stays in Context and is preserved by the output theorem.
The concrete Rust facade, arithmetic, field encodings and allocation are still
explicit boundaries; this file does not assert their extraction. -/

def queueRep {S N Notes F E Event D Shared : Type}
    (rep : Representation S N Notes F E Event D Shared) :
    QueueMaintenanceProof.Representation S N TouchNote :=
  ⟨rep.source, rep.note⟩

def process {S N Notes F E Event D Shared Ops QOps : Type}
    (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (setNotes : S → Notes → Result S)
    (facade : QueueMaintenanceComposition.Facade QOps S)
    (cursor : queue_maintenance.QueueCursor QOps S N) (queueOps : QOps)
    (source : S) (initial : touch_automatic.TouchAutomaticWork Notes F E) :
    Result (S × F × E) := do
  let out ← touch_automatic.process call source initial ops
  let installed ← setNotes source out.notes
  let queue ← QueueMaintenanceComposition.normalizeViaFacade facade cursor queueOps installed
  ok (queue, out.frontiers, out.events_rev)

theorem process_equiv {S N Notes F E Event D Shared Ops QOps : Type}
    (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (rep : Representation S N Notes F E Event D Shared) (ctx : Context) (cap : Capacity)
    (atoms : AtomicAgrees call ops rep ctx cap)
    (setNotes : S → Notes → Result S)
    (setNotesLaw : ∀ source notes, (rep.notes notes).length ≤ cap.notes →
      ∃ installed, setNotes source notes = ok installed ∧
        rep.source installed = { rep.source source with notes := rep.notes notes })
    (facade : QueueMaintenanceComposition.Facade QOps S)
    (cursor : queue_maintenance.QueueCursor QOps S N) (queueOps : QOps)
    (facadePoints : QueueMaintenanceComposition.FacadeAtomicAgrees facade queueOps (queueRep rep) cap.notes)
    (cursorPoints : QueueMaintenanceProof.CursorAtomicAgrees cursor queueOps
      (queueRep rep) touchQueueResolved cap.notes)
    (source : S) (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (frontiers : SensorVec Nat) (events : List JudgeEvent)
    (hinit : interpret rep initial = ⟨[], frontiers, events⟩)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes ⟨[], frontiers, events⟩)) :
    process call ops setNotes facade cursor queueOps source initial ⦃ out =>
      (rep.source out.1, rep.frontiers out.2.1, ctx.groups, rep.events out.2.2) =
        reference ctx (rep.source source) frontiers events ⦄ := by
  obtain ⟨out, hrun, hout, _⟩ := spec_imp_exists
    (TouchAutomaticProof.process_spec call ops rep ctx cap atoms source initial
      (by simpa only [hinit] using hfit))
  have houtFit : Fits cap (interpret rep out) := by
    rw [hout, hinit]
    exact hfit
  obtain ⟨installed, hset, hinstalled⟩ := setNotesLaw source out.notes houtFit.1
  have hqueueFit : QueueMaintenanceProof.Fits cap.notes ((queueRep rep).queue installed) := by
    change (rep.source installed).notes.length ≤ cap.notes
    rw [hinstalled]
    exact houtFit.1
  obtain ⟨queue, hnormalize, hqueue, _, _⟩ := spec_imp_exists
    (QueueMaintenanceComposition.normalize_via_facade_spec facade cursor queueOps (queueRep rep)
      touchQueueResolved cap.notes facadePoints cursorPoints installed hqueueFit)
  have hqueueRef : rep.source queue =
      normalizeTouchQueueCursor { rep.source source with notes := rep.notes out.notes } := by
    simpa only [QueueMaintenanceModel.touch_pending_equiv, queueRep, hinstalled] using hqueue
  have hfinish : finish ctx (rep.source source) (interpret rep out) =
      reference ctx (rep.source source) frontiers events := by
    rw [hout, hinit, reference_eq_resume]
  apply exists_imp_spec
  refine ⟨(queue, out.frontiers, out.events_rev), ?_, ?_⟩
  · simp only [process, hrun, hset, hnormalize, bind_tc_ok]
  · simpa only [hqueueRef, finish, interpret] using hfinish

/-- The same complete automatic queue, with the extracted judgment/lifecycle
composition installed. No whole-step or whole-normalization equation is a
premise. Group lookup and the concrete point operations remain obligations. -/
theorem process_judgments_equiv {S N Notes F E Event D Shared Ops QOps I J : Type}
    (inst : judgment_adapter.IntegerOps J I) (integerOps : J) (decode : I → ℤ)
    (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
    (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (rep : Representation S N Notes F E Event D Shared)
    (pack : TouchAutomaticJudgmentsProof.OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (ready : TouchAutomaticJudgmentsProof.EncodingReady inst integerOps decode encode rep pack ctx)
    (remaining : TouchAutomaticJudgmentsProof.RemainingAtoms base ops rep ctx cap)
    (setNotes : S → Notes → Result S)
    (setNotesLaw : ∀ source notes, (rep.notes notes).length ≤ cap.notes →
      ∃ installed, setNotes source notes = ok installed ∧
        rep.source installed = { rep.source source with notes := rep.notes notes })
    (facade : QueueMaintenanceComposition.Facade QOps S)
    (cursor : queue_maintenance.QueueCursor QOps S N) (queueOps : QOps)
    (facadePoints : QueueMaintenanceComposition.FacadeAtomicAgrees facade queueOps (queueRep rep) cap.notes)
    (cursorPoints : QueueMaintenanceProof.CursorAtomicAgrees cursor queueOps
      (queueRep rep) touchQueueResolved cap.notes)
    (source : S) (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (frontiers : SensorVec Nat) (events : List JudgeEvent)
    (hinit : interpret rep initial = ⟨[], frontiers, events⟩)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes ⟨[], frontiers, events⟩)) :
    process (TouchAutomaticJudgmentsProof.installStep inst integerOps decode encode base rep pack ctx)
      ops setNotes facade cursor queueOps source initial ⦃ out =>
      (rep.source out.1, rep.frontiers out.2.1, ctx.groups, rep.events out.2.2) =
        reference ctx (rep.source source) frontiers events ⦄ := by
  exact process_equiv
    (TouchAutomaticJudgmentsProof.installStep inst integerOps decode encode base rep pack ctx)
    ops rep ctx cap
    (TouchAutomaticJudgmentsProof.atomic_agrees_of_judgments
      inst integerOps decode integerLaw encode decode_encode base ops rep pack ctx cap ready remaining)
    setNotes setNotesLaw facade cursor queueOps facadePoints cursorPoints
    source initial frontiers events hinit hfit

end Verification.TouchAutomaticQueueProof
