import Verification.TouchSchedule
import Verification.LifecycleJudgments
import Verification.TouchHoldGroups

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore InputModel Lifecycle

namespace Verification.TouchScheduleJudgmentsProof

open TouchScheduleModel TouchScheduleProof
open private hasStrictMajority groupShareResult registerTouchGroupResult
  registerTouchGroupResult.loop processTouchQueueHead from LnmaiCore.Scheduler

/-!
UNVALIDATED SOURCE: mainline owns the only Lean worker.

Compose the real Generated touch_schedule.process with the extracted Touch
judgment/lifecycle composition and the separately proved Generated head-group
search/registration kernels. Only the head-group portion of TouchHoldGroups is
used; no TouchHold lifecycle/body scheduling or automatic Touch pass is added.

The original base.step/shared_result/register_group callbacks are replaced,
not required to satisfy composite callback equivalences. The final theorem
requires only scheduler point laws, head-group point laws, IntegerAgrees, an
exact integer round trip, and note/event output-field encoding. Queue loops
remain composed by the imported TouchSchedule and QueueMaintenance proofs.

IMPORTANT CAPACITY/ALLOCATION BOUNDARY: the frozen GroupAgrees contract is
universal in its represented inputs. GroupAtoms keeps the original bounded
append law. GroupRoom separately requires every represented group list to fit
and, when the requested id is absent, to have one free slot. The reference
registration output bound is DERIVED from first-match/absence and these length
facts, never assumed as a composite equivalence. These universal representation
conditions still need a concrete instance; a facade that can represent a full
list with an absent id cannot satisfy GroupRoom. Such a facade needs a
reachable-state/capacity-sensitive contract in mainline. Successful bounded
point calls still include cloning, arithmetic, writes and allocation; a length
bound alone does not prove allocator success. No unbounded append law is used.

Input frozen delivery: delivery-20261017-tlh06ovz/
  TouchScheduleModel.lean:
    ad628ade27aea49cfad83e0119fd9e4145de12c4f820bd3cd2c0d34bfea884e8
  TouchSchedule.lean:
    b2a12ead57bfbb2ce5d06aeefa9c428345dd21d6282a4022d6698771d6b454ea
Group API read from /tmp/lnmai-touchhold-schedule-work/TouchHoldGroups.lean.
Mainline must install that module before this one.
-/

variable {Q N F C G E Event T D Area Zone Id Grade M Ops : Type}

abbrev StepOutput := TouchNote × Option JudgeEvent

/-- Encoders receive computed fields, with no note input or context on which
to rerun judgment or lifecycle. An absent event has no encoder call. -/
structure OutputEncoding (N Event : Type) where
  note : TouchNote → Result N
  event : JudgeEvent → Result Event

structure EncodingFields
    (pack : OutputEncoding N Event)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (output : StepOutput) : Prop where
  note : ∃ after, pack.note output.1 = ok after ∧ rep.note after = output.1
  event : ∀ event, output.2 = some event →
    ∃ encoded, pack.event event = ok encoded ∧ rep.event encoded = event

def encodeOutput (pack : OutputEncoding N Event) (output : StepOutput) :
    Result (N × Option Event) := do
  let after ← pack.note output.1
  let event ← match output.2 with
    | none => ok none
    | some event => do
        let encoded ← pack.event event
        ok (some encoded)
  ok (after, event)

theorem encode_output_spec (pack : OutputEncoding N Event)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (output : StepOutput) (fields : EncodingFields pack rep output) :
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

/-- Only the representation fields needed by the shared head-group kernels. -/
def headRep (rep : Representation Q N F C G E Event T D Area Zone Id Grade) :
    TouchHoldGroupsProof.HeadRepresentation G Id Grade D :=
  ⟨rep.groups, rep.id, rep.grade, rep.duration⟩

/-- Eight point laws. Each indexed write preserves the entire surrounding
list. Search, first-match selection, majority-dependent registration order,
and whole group-list operations are not fields of this structure. -/
structure GroupAtoms (call : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (ops : Ops) (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat) : Prop where
  length : ∀ gs, ∃ len, call.head_length ops gs = ok len ∧
    (rep.groups gs).length = len.val
  matches : ∀ gs index pre group rest id,
    TouchHoldGroupsProof.At (rep.groups gs) index pre group rest →
    call.head_matches ops gs index id = ok (group.groupId == rep.id id)
  majority : ∀ gs index pre group rest,
    TouchHoldGroupsProof.At (rep.groups gs) index pre group rest →
    call.head_majority ops gs index = ok (hasStrictMajority group.count group.size)
  result : ∀ gs index pre group rest,
    TouchHoldGroupsProof.At (rep.groups gs) index pre group rest →
    ∃ grade diff, call.head_result ops gs index = ok (grade, diff) ∧
      rep.grade grade = group.grade ∧ rep.duration diff = group.diff
  increment : ∀ gs index pre group rest,
    TouchHoldGroupsProof.At (rep.groups gs) index pre group rest →
    ∃ next, call.head_increment ops gs index = ok next ∧
      rep.groups next = pre ++ { group with count := group.count + 1 } :: rest
  set_size : ∀ gs index pre group rest size,
    TouchHoldGroupsProof.At (rep.groups gs) index pre group rest →
    ∃ next, call.head_set_size ops gs index size = ok next ∧
      rep.groups next = pre ++ { group with size := rep.id size } :: rest
  set_result : ∀ gs index pre group rest grade diff,
    TouchHoldGroupsProof.At (rep.groups gs) index pre group rest →
    ∃ next, call.head_set_result ops gs index grade diff = ok next ∧
      rep.groups next = pre ++
        { group with grade := rep.grade grade, diff := rep.duration diff } :: rest
  append : ∀ gs id size grade diff,
    (rep.groups gs).length + 1 ≤ capacity →
    ∃ next, call.head_append ops gs id size grade diff = ok next ∧
      rep.groups next = rep.groups gs ++
        [{ groupId := rep.id id, count := 1, size := rep.id size,
           grade := rep.grade grade, diff := rep.duration diff }]

/-- Length/space obligations on represented inputs, with no operational
equivalence field. Their universal quantifiers come from frozen GroupAgrees. -/
structure GroupRoom (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat) : Prop where
  fits : ∀ gs, (rep.groups gs).length ≤ capacity
  append_room : ∀ gs id,
    TouchHoldGroupsProof.NoKey GroupState.groupId (rep.groups gs) (rep.id id) →
    (rep.groups gs).length + 1 ≤ capacity

/-- Reuse the existing bounded group API with exactly the same point laws. -/
theorem head_atoms (call : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (ops : Ops) (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat) (atoms : GroupAtoms call ops rep capacity) :
    TouchHoldGroupsProof.HeadAtoms call ops (headRep rep) capacity := by
  exact { length := atoms.length, matches := atoms.matches, majority := atoms.majority
           result := atoms.result, increment := atoms.increment, set_size := atoms.set_size
           set_result := atoms.set_result, append := atoms.append }

/-- A found id preserves length; an absent id appends exactly one group.
This derives the bounded registration theorem's precondition from point-space
facts. The imported scan supplies the first-match/absence evidence. -/
theorem register_capacity (call : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (ops : Ops) (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat) (atoms : GroupAtoms call ops rep capacity)
    (room : GroupRoom rep capacity) (gs : G) (id size : Id) (grade : Grade) (diff : D) :
    (registerTouchGroupResult (rep.groups gs) (rep.id id) (rep.id size)
      (rep.grade grade) (rep.duration diff)).length ≤ capacity := by
  obtain ⟨found, _, hfirst⟩ := spec_imp_exists
    (TouchHoldGroupsProof.find_head_spec call ops (headRep rep) capacity
      (head_atoms call ops rep capacity atoms) gs id)
  cases found with
  | none =>
      have href : registerTouchGroupResult (rep.groups gs) (rep.id id) (rep.id size)
          (rep.grade grade) (rep.duration diff) = rep.groups gs ++
          [{ groupId := rep.id id, count := 1, size := rep.id size,
             grade := rep.grade grade, diff := rep.duration diff }] := by
        simpa [registerTouchGroupResult, registerTouchGroupResult.loop] using
          TouchHoldGroupsProof.register_head_skip (rep.groups gs) [] (rep.id id)
            (rep.id size) (rep.grade grade) (rep.duration diff) hfirst
      simpa only [href, List.length_append, List.length_singleton] using
        room.append_room gs id hfirst
  | some index =>
      obtain ⟨pre, group, rest, hat, hprefix, hmatch⟩ := hfirst
      have href : registerTouchGroupResult (rep.groups gs) (rep.id id) (rep.id size)
          (rep.grade grade) (rep.duration diff) = pre ++
          TouchHoldGroupsProof.headAfter group (rep.id size) (rep.grade grade)
            (rep.duration diff) :: rest := by
        rw [hat.1]
        exact TouchHoldGroupsProof.register_head_at pre group rest (rep.id id)
          (rep.id size) (rep.grade grade) (rep.duration diff) hprefix hmatch
      simpa only [href, hat.1, List.length_append, List.length_cons] using room.fits gs

/-- Actual extracted group traversal functions replace both composite fields. -/
def installGroups
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D) :
    touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M :=
  { base with
    shared_result := fun ops gs id => touch_hold_schedule.shared_result headCall gs id ops
    register_group := fun ops gs id size grade diff =>
      touch_hold_schedule.register_head headCall gs id size grade diff ops }

/-- Eliminate both composite group premises using the proved first-match scan
and registration. In particular later duplicate ids remain unchanged. -/
theorem group_agrees_of_atoms
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (capacity : Nat) (atoms : GroupAtoms headCall ops rep capacity)
    (room : GroupRoom rep capacity) :
    GroupAgrees (installGroups base headCall) ops rep := by
  constructor
  · intro gs id
    obtain ⟨shared, hrun, hrep⟩ := spec_imp_exists
      (TouchHoldGroupsProof.shared_result_equiv headCall ops (headRep rep) capacity
        (head_atoms headCall ops rep capacity atoms) gs id)
    exact ⟨shared, hrun, hrep⟩
  · intro gs id size grade diff
    have hcapacity := register_capacity headCall ops rep capacity atoms room gs id size grade diff
    obtain ⟨out, hrun, hrep⟩ := spec_imp_exists
      (TouchHoldGroupsProof.register_head_equiv headCall ops (headRep rep) capacity
        (head_atoms headCall ops rep capacity atoms) gs id size grade diff hcapacity)
    exact ⟨out, hrun, hrep⟩

variable {I J : Type} (inst : judgment_adapter.IntegerOps J I)
  (integerOps : J) (decode : I → ℤ)
  (integerLaw : JudgmentAdapterProof.IntegerAgrees inst integerOps decode)
  (encode : ℤ → I) (decode_encode : ∀ value, decode (encode value) = value)

/-- Shared-result grade/duration stays exact; it is not recomputed from the
local click diff. clicked remains the generated button-or-sensor result. -/
def step (ctx : Context) (note : TouchNote) (diff : Duration) (clicked : Bool)
    (shared : Option (JudgeGrade × Duration)) : Result StepOutput :=
  LifecycleJudgmentsProof.touchStep inst integerOps decode note ctx.currentTime
    (encode diff.toMicros) clicked shared ctx.style

def encodedStep (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) (note : N) (diff : D)
    (clicked : Bool) (shared : Option (Grade × D)) : Result (N × Option Event) := do
  let output ← step inst integerOps decode encode ctx (rep.note note)
    (rep.duration diff) clicked (sharedRep rep shared)
  encodeOutput pack output

/-- Only output-field representability and successful encoding on successful
outputs of the actual composition. No lifecycle equivalence is a premise. -/
def EncodingReady (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) : Prop :=
  ∀ (note : N) (diff : D) (clicked : Bool) (shared : Option (Grade × D))
      (output : StepOutput),
    step inst integerOps decode encode ctx (rep.note note) (rep.duration diff)
      clicked (sharedRep rep shared) = ok output → EncodingFields pack rep output

def installStep
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) :
    touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M :=
  { base with step := fun _ note diff clicked shared =>
      encodedStep inst integerOps decode encode rep pack ctx note diff clicked shared }

/-- One trait instance contains both proved compositions. All remaining
scheduler methods, including queue_ops, are definitionally unchanged. -/
def install
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) :
    touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M :=
  installStep inst integerOps decode encode (installGroups base headCall) rep pack ctx

include integerLaw decode_encode in
theorem step_equiv (ctx : Context) (note : TouchNote) (diff : Duration) (clicked : Bool)
    (shared : Option (JudgeGrade × Duration)) :
    step inst integerOps decode encode ctx note diff clicked shared =
      ok (Lifecycle.touchStep note ctx.currentTime diff clicked shared ctx.style) := by
  have hdiff : Duration.fromMicros diff.toMicros = diff := rfl
  simpa only [step, decode_encode, hdiff] using
    LifecycleJudgmentsProof.touch_step_equiv inst integerOps decode integerLaw
      note ctx.currentTime (encode diff.toMicros) clicked shared ctx.style

include integerLaw decode_encode in
theorem encoded_step_spec
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (note : N) (diff : D) (clicked : Bool) (shared : Option (Grade × D)) :
    ∃ after event,
      encodedStep inst integerOps decode encode rep pack ctx note diff clicked shared =
        ok (after, event) ∧
      (rep.note after, event.map rep.event) = Lifecycle.touchStep (rep.note note)
        ctx.currentTime (rep.duration diff) clicked (sharedRep rep shared) ctx.style := by
  have hstep := step_equiv inst integerOps decode integerLaw encode decode_encode
    ctx (rep.note note) (rep.duration diff) clicked (sharedRep rep shared)
  obtain ⟨after, event, hpack, hfields⟩ := encode_output_spec pack rep _
    (ready note diff clicked shared _ hstep)
  refine ⟨after, event, ?_, hfields⟩
  simpa only [encodedStep, hstep, bind_tc_ok] using hpack

include integerLaw decode_encode in
theorem lifecycle_agrees_of_judgments
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (ops : Ops) (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx) :
    LifecycleAgrees (installStep inst integerOps decode encode base rep pack ctx)
      ops rep ctx := by
  constructor
  intro note diff clicked shared
  exact encoded_step_spec inst integerOps decode integerLaw encode decode_encode
    rep pack ctx ready note diff clicked shared

/-- PointAgrees already excludes all three installed composite callbacks, so
it is precisely the remaining scheduler-point boundary for this composition. -/
theorem point_agrees_installed
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (cursorCall : queue_maintenance.QueueCursor M Q N)
    (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (points : PointAgrees base cursorCall replaceCall ops rep ctx cap) :
    PointAgrees (install inst integerOps decode encode base headCall rep pack ctx)
      cursorCall replaceCall ops rep ctx cap := by
  exact {
    queue_ops := points.queue_ops
    queue_remaining := points.queue_remaining
    queue_len := points.queue_len
    timing := points.timing
    sensor_diff := points.sensor_diff
    button_diff := points.button_diff
    group_id := points.group_id
    group_size := points.group_size
    eligible := points.eligible
    unlocked := points.unlocked
    button_ring_for_touch := points.button_ring_for_touch
    sensor_area := points.sensor_area
    outer_button := points.outer_button
    button_available := points.button_available
    sensor_available := points.sensor_available
    use_button := points.use_button
    use_sensor := points.use_sensor
    ended := points.ended
    event_grade := points.event_grade
    is_miss_or_too_fast := points.is_miss_or_too_fast
    prepend_event := points.prepend_event
    advance_frontier := points.advance_frontier }

theorem group_agrees_installed
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context)
    (groupCapacity : Nat) (atoms : GroupAtoms headCall ops rep groupCapacity)
    (room : GroupRoom rep groupCapacity) :
    GroupAgrees (install inst integerOps decode encode base headCall rep pack ctx)
      ops rep := by
  have h := group_agrees_of_atoms base headCall ops rep groupCapacity atoms room
  exact { shared_result := h.shared_result, register_group := h.register_group }

/-- The single-note and group-list equivalence fields are conclusions of this
theorem, not assumptions. Point laws survive both trait installations. -/
include integerLaw decode_encode in
theorem agrees_of_judgments_groups
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (cursorCall : queue_maintenance.QueueCursor M Q N)
    (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (points : PointAgrees base cursorCall replaceCall ops rep ctx cap)
    (groupCapacity : Nat) (groupPoints : GroupAtoms headCall ops rep groupCapacity)
    (groupRoom : GroupRoom rep groupCapacity) :
    PointAgrees (install inst integerOps decode encode base headCall rep pack ctx)
      cursorCall replaceCall ops rep ctx cap ∧
    GroupAgrees (install inst integerOps decode encode base headCall rep pack ctx) ops rep ∧
    LifecycleAgrees (install inst integerOps decode encode base headCall rep pack ctx)
      ops rep ctx := by
  refine ⟨point_agrees_installed inst integerOps decode encode base headCall
    cursorCall replaceCall ops rep pack ctx cap points,
    group_agrees_installed inst integerOps decode encode base headCall
      ops rep pack ctx groupCapacity groupPoints groupRoom, ?_⟩
  exact lifecycle_agrees_of_judgments inst integerOps decode integerLaw encode decode_encode
    (installGroups base headCall) ops rep pack ctx ready

/-- Real Generated process, composed with exact judgment, lifecycle, group
search/registration, and queue maintenance. No body/step/group/pass equation
is required of the caller. The result uses the ORIGINAL head-fuel recursion. -/
include integerLaw decode_encode in
theorem process_equiv
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (cursorCall : queue_maintenance.QueueCursor M Q N)
    (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (points : PointAgrees base cursorCall replaceCall ops rep ctx cap)
    (groupCapacity : Nat) (groupPoints : GroupAtoms headCall ops rep groupCapacity)
    (groupRoom : GroupRoom rep groupCapacity)
    (fuel : Usize) (initial : touch_schedule.TouchWork Q F C G E)
    (hqfit : QueueMaintenanceProof.Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events) :
    touch_schedule.process (install inst integerOps decode encode base headCall rep pack ctx)
      cursorCall replaceCall fuel initial ops ⦃ out =>
      QueueMaintenanceProof.Fits cap.queue (rep.queue out.queue) ∧
      (interpret rep out).output = resume ctx fuel.val (interpret rep initial) ⦄ := by
  obtain ⟨hp, hg, hl⟩ := agrees_of_judgments_groups
    inst integerOps decode integerLaw encode decode_encode
    base headCall cursorCall replaceCall ops rep pack ctx cap ready points
    groupCapacity groupPoints groupRoom
  exact TouchScheduleProof.process_equiv
    (install inst integerOps decode encode base headCall rep pack ctx)
    cursorCall replaceCall ops rep ctx cap hp hg hl fuel initial hqfit hefit

/-- The unchanged runtime wrapper supplies remaining + 1. Its conversion and
addition are still explicit facade obligations, not consequences of queue Fits. -/
include integerLaw decode_encode in
theorem head_process_equiv
    (base : touch_schedule.TouchSchedule Ops Q N F C G E Event T D Area Zone Id Grade M)
    (headCall : touch_hold_schedule.HeadGroups Ops G Id Grade D)
    (cursorCall : queue_maintenance.QueueCursor M Q N)
    (replaceCall : queue_maintenance.QueueReplace M Q N) (ops : Ops)
    (rep : Representation Q N F C G E Event T D Area Zone Id Grade)
    (pack : OutputEncoding N Event) (ctx : Context) (cap : Capacity)
    (ready : EncodingReady inst integerOps decode encode rep pack ctx)
    (points : PointAgrees base cursorCall replaceCall ops rep ctx cap)
    (groupCapacity : Nat) (groupPoints : GroupAtoms headCall ops rep groupCapacity)
    (groupRoom : GroupRoom rep groupCapacity)
    (fuel : Usize) (initial : touch_schedule.TouchWork Q F C G E)
    (hfuel : fuel.val = TouchScheduleModel.remaining (rep.queue initial.queue) + 1)
    (hqfit : QueueMaintenanceProof.Fits cap.queue (rep.queue initial.queue))
    (hefit : (resume ctx fuel.val (interpret rep initial)).2.2.2.2.length ≤ cap.events) :
    touch_schedule.process (install inst integerOps decode encode base headCall rep pack ctx)
      cursorCall replaceCall fuel initial ops ⦃ out =>
      QueueMaintenanceProof.Fits cap.queue (rep.queue out.queue) ∧
      (interpret rep out).output = processTouchQueueHead ctx.area
        (rep.queue initial.queue) (rep.frontiers initial.frontiers) ctx.input ctx.currentTime
        ctx.style (rep.cursor initial.cursor) ctx.touchPanelOffset (rep.groups initial.groups)
        ctx.buttonRingForTouch (rep.events initial.events_rev) ⦄ := by
  obtain ⟨hp, hg, hl⟩ := agrees_of_judgments_groups
    inst integerOps decode integerLaw encode decode_encode
    base headCall cursorCall replaceCall ops rep pack ctx cap ready points
    groupCapacity groupPoints groupRoom
  exact TouchScheduleProof.head_process_equiv
    (install inst integerOps decode encode base headCall rep pack ctx)
    cursorCall replaceCall ops rep ctx cap hp hg hl fuel initial hfuel hqfit hefit

/-!
HANDOFF / REMAINING OBLIGATIONS:

* PointAgrees: 22 scheduler point laws, including a SINGLE queue_ops result
  with CursorAtomicAgrees and ReplaceAtomicAgrees. These retain actual field
  reads, resolved/id interpretation, counter increments, time differences,
  ring/sensor lookup, predicates, event prepend and frontier advancement.
* GroupAtoms: length, indexed id comparison/majority/result reads, indexed
  count/size/result writes, and bounded append-one. There is no composite group law
  left in process_equiv. Majority uses the previous count and size; first-id
  semantics, duplicate preservation, and iteration termination come from
  TouchHoldGroupsProof. GroupRoom requires represented lists to fit and an
  absent id to have one free slot. These universal representation/space
  conditions remain explicit, as does successful allocation on bounded calls.
* IntegerAgrees and decode_encode still require the concrete exact BigInt
  implementation; EncodingReady requires field reification and allocation on
  successful computed notes/events. There is no machine-width diff restriction
  or a claim that a concrete bounded representation satisfies all point laws.
* install constructs a trait with three proved callbacks. Its identification
  with native Rust adapters, initial clones/context wiring, group adapters,
  Usize conversions, and successful allocation remain outside these theorems.
  In particular this is not a proof of the unextracted concrete facade.
* Head behavior inherits buttonRingForTouch/click/fuel/timeout/shared handling,
  ALL-id note replacement, normalization and reverse event accumulator order
  from TouchSchedule. Final event reverse, automatic/normalize outer phases,
  outer sensor traversal and frame composition are mainline responsibilities.
* All source awaits the mainline Lean worker, including imported frozen
  TouchSchedule and the separately delivered TouchHoldGroups dependency.
-/

end Verification.TouchScheduleJudgmentsProof
