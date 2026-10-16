import Verification.TouchAutomatic
import Verification.LifecycleJudgments

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle Constants

namespace Verification.TouchAutomaticJudgmentsProof
open TouchAutomaticModel TouchAutomaticProof
open private touchQueueResolved advanceSharedTouchQueue from LnmaiCore.Scheduler

/-! Compose the extracted Touch judgment and lifecycle kernels with the actual
automatic traversal. clicked=false is fixed by the runtime. Group lookup and
scalar/container adapters remain explicit; no whole lifecycle equation is
required from the caller. This constructed trait is not an extraction of the
concrete Rust facade, whose field operations remain to be linked. -/

structure OutputEncoding (N Event : Type) where
  note : TouchNote → Result N
  event : JudgeEvent → Result Event

structure EncodingFields {S N Notes F E Event D Shared : Type}
    (pack : OutputEncoding N Event) (rep : Representation S N Notes F E Event D Shared)
    (output : TouchNote × Option JudgeEvent) : Prop where
  note : ∃ after, pack.note output.1 = ok after ∧ rep.note after = output.1
  event : ∀ event, output.2 = some event →
    ∃ encoded, pack.event event = ok encoded ∧ rep.event encoded = event

def encodeOutput {N Event : Type} (pack : OutputEncoding N Event)
    (output : TouchNote × Option JudgeEvent) : Result (N × Option Event) := do
  let after ← pack.note output.1
  let event ← match output.2 with
    | none => ok none
    | some event => do
        let encoded ← pack.event event
        ok (some encoded)
  ok (after, event)

theorem encode_output_spec {S N Notes F E Event D Shared : Type}
    (pack : OutputEncoding N Event) (rep : Representation S N Notes F E Event D Shared)
    (output : TouchNote × Option JudgeEvent) (fields : EncodingFields pack rep output) :
    ∃ after event, encodeOutput pack output = ok (after, event) ∧
      (rep.note after, event.map rep.event) = output := by
  obtain ⟨after, hafter, hnote⟩ := fields.note
  cases hevent : output.2 with
  | none =>
      refine ⟨after, none, ?_, ?_⟩
      · simp only [encodeOutput, hafter, hevent, bind_tc_ok]
      · exact Prod.ext hnote (by simp only [Option.map_none, hevent])
  | some event =>
      obtain ⟨encoded, hencoded, heventRep⟩ := fields.event event hevent
      refine ⟨after, some encoded, ?_, ?_⟩
      · simp only [encodeOutput, hafter, hevent, hencoded, bind_tc_ok]
      · exact Prod.ext hnote (by simp only [Option.map_some, hevent, heventRep])

structure RemainingAtoms {S N Notes F E Event D Shared Ops : Type}
    (call : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (rep : Representation S N Notes F E Event D Shared)
    (ctx : Context) (cap : Capacity) : Prop where
  length : ∀ source, ∃ len, call.length ops source = ok len ∧
    (rep.source source).notes.length = len.val
  get : ∀ source (index : Usize) (h : index.val < (rep.source source).notes.length),
    ∃ note, call.get ops source index = ok note ∧
      rep.note note = (rep.source source).notes[index.val]'h
  before_cursor : ∀ source index, call.before_cursor ops source index =
    ok (decide (index.val < (rep.source source).currentIndex))
  resolved : ∀ note, call.resolved ops note = ok (touchQueueResolved (rep.note note))
  sensor_diff : ∀ note, ∃ diff, call.sensor_diff ops note = ok diff ∧
    rep.duration diff = difference ctx (rep.note note)
  shared : ∀ note, ∃ result, call.shared ops note = ok result ∧
    result.map rep.shared = TouchAutomaticModel.shared ctx (rep.note note)
  too_late : ∀ note, call.too_late ops note = ok (tooLate ctx (rep.note note))
  advance_frontier : ∀ frontiers, ∃ next, call.advance_frontier ops frontiers = ok next ∧
    rep.frontiers next = advanceSharedTouchQueue (rep.frontiers frontiers) ctx.area
  prepend_event : ∀ events event, (rep.events events).length + 1 ≤ cap.events →
    ∃ next, call.prepend_event ops events event = ok next ∧
      rep.events next = rep.event event :: rep.events events
  append_note : ∀ notes note, (rep.notes notes).length + 1 ≤ cap.notes →
    ∃ next, call.append_note ops notes note = ok next ∧
      rep.notes next = rep.notes notes ++ [rep.note note]

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

def step (ctx : Context) (note : TouchNote) (diff : Duration)
    (shared : Option (JudgeGrade × Duration)) : Result (TouchNote × Option JudgeEvent) :=
  LifecycleJudgmentsProof.touchStep inst integerOps decode note ctx.currentTime
    (encode diff.toMicros) false shared ctx.style

def encodedStep {S N Notes F E Event D Shared : Type}
    (rep : Representation S N Notes F E Event D Shared) (pack : OutputEncoding N Event)
    (ctx : Context) (note : N) (diff : D) (shared : Option Shared) : Result (N × Option Event) := do
  let output ← step inst integerOps decode encode ctx (rep.note note)
    (rep.duration diff) (shared.map rep.shared)
  encodeOutput pack output

def EncodingReady {S N Notes F E Event D Shared : Type}
    (rep : Representation S N Notes F E Event D Shared) (pack : OutputEncoding N Event)
    (ctx : Context) : Prop :=
  ∀ (note : N) (diff : D) (shared : Option Shared) (output : TouchNote × Option JudgeEvent),
    step inst integerOps decode encode ctx (rep.note note) (rep.duration diff)
      (shared.map rep.shared) = ok output → EncodingFields pack rep output

def installStep {S N Notes F E Event D Shared Ops : Type}
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared)
    (rep : Representation S N Notes F E Event D Shared) (pack : OutputEncoding N Event)
    (ctx : Context) : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared :=
  { base with step := fun _ note diff shared =>
      encodedStep inst integerOps decode encode rep pack ctx note diff shared }

include integerLaw decode_encode

theorem step_equiv (ctx : Context) (note : TouchNote) (diff : Duration)
    (shared : Option (JudgeGrade × Duration)) :
    step inst integerOps decode encode ctx note diff shared =
      ok (Lifecycle.touchStep note ctx.currentTime diff false shared ctx.style) := by
  unfold step
  have hdiff : Duration.fromMicros diff.toMicros = diff := rfl
  simpa only [decode_encode, hdiff] using
    LifecycleJudgmentsProof.touch_step_equiv inst integerOps decode integerLaw note
      ctx.currentTime (encode diff.toMicros) false shared ctx.style

theorem encoded_step_spec {S N Notes F E Event D Shared : Type}
    (rep : Representation S N Notes F E Event D Shared) (pack : OutputEncoding N Event)
    (ctx : Context) (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (note : N) (diff : D) (shared : Option Shared) :
    ∃ after event, encodedStep inst integerOps decode encode rep pack ctx note diff shared =
        ok (after, event) ∧
      (rep.note after, event.map rep.event) =
        Lifecycle.touchStep (rep.note note) ctx.currentTime (rep.duration diff) false
          (shared.map rep.shared) ctx.style := by
  have hstep := step_equiv inst integerOps decode integerLaw encode decode_encode
    ctx (rep.note note) (rep.duration diff) (shared.map rep.shared)
  obtain ⟨after, event, hpack, hfields⟩ := encode_output_spec pack rep _
    (ready note diff shared _ hstep)
  refine ⟨after, event, ?_, hfields⟩
  simpa only [encodedStep, hstep, bind_tc_ok] using hpack

theorem atomic_agrees_of_judgments {S N Notes F E Event D Shared Ops : Type}
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (rep : Representation S N Notes F E Event D Shared) (pack : OutputEncoding N Event)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx cap) :
    AtomicAgrees (installStep inst integerOps decode encode base rep pack ctx) ops rep ctx cap := by
  refine { length := remaining.length, get := remaining.get, before_cursor := remaining.before_cursor
           resolved := remaining.resolved, sensor_diff := remaining.sensor_diff, shared := remaining.shared
           too_late := remaining.too_late, advance_frontier := remaining.advance_frontier
           prepend_event := remaining.prepend_event, append_note := remaining.append_note, step := ?_ }
  intro note diff shared
  exact encoded_step_spec inst integerOps decode integerLaw encode decode_encode
    rep pack ctx ready note diff shared

theorem process_equiv {S N Notes F E Event D Shared Ops : Type}
    (base : touch_automatic.TouchAutomatic Ops S N Notes F E Event D Shared) (ops : Ops)
    (rep : Representation S N Notes F E Event D Shared) (pack : OutputEncoding N Event)
    (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (remaining : RemainingAtoms base ops rep ctx cap)
    (source : S) (initial : touch_automatic.TouchAutomaticWork Notes F E)
    (frontiers : SensorVec Nat) (events : List JudgeEvent)
    (hinit : interpret rep initial = ⟨[], frontiers, events⟩)
    (hfit : Fits cap (resume ctx (rep.source source) 0 (rep.source source).notes ⟨[], frontiers, events⟩)) :
    touch_automatic.process (installStep inst integerOps decode encode base rep pack ctx)
      source initial ops ⦃ out =>
      finish ctx (rep.source source) (interpret rep out) =
        reference ctx (rep.source source) frontiers events ⦄ := by
  exact TouchAutomaticProof.process_equiv
    (installStep inst integerOps decode encode base rep pack ctx) ops rep ctx cap
    (atomic_agrees_of_judgments inst integerOps decode integerLaw encode decode_encode
      base ops rep pack ctx cap ready remaining) source initial frontiers events hinit hfit

end Verification.TouchAutomaticJudgmentsProof
