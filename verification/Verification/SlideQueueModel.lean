import Verification.SlideHistory

namespace Verification.SlideQueueProof
open LnmaiCore LnmaiCore.Lifecycle
open private slideQueueCoreFuel slideHideBarCmd from LnmaiCore.Lifecycle

/-- Queue traversal with render routing factored out. Every returned natural
    is a hide-progress index, in exactly the order in which it is emitted. -/
def advanceModel (fuel : Nat) (emit : Bool) (queue : SlideQueue)
    (held : SensorVec Bool) : SlideQueue × List Nat :=
  match queue, fuel with
  | [], _ => ([], [])
  | _, 0 => (queue, [])
  | first :: rest, fuel + 1 =>
      let first := first.check held
      match rest with
      | [] =>
          if first.isFinished then
            ([], if emit then [first.arrowProgressWhenFinished] else [])
          else if first.on then
            ([first], if emit then [first.arrowProgressWhenOn] else [])
          else ([first], [])
      | second :: rest =>
          if first.isSkippable || first.on then
            let second := second.check held
            if second.isFinished then
              let (remaining, commands) := advanceModel fuel emit rest held
              (remaining, if emit then second.arrowProgressWhenFinished :: commands else [])
            else if second.on then
              let (remaining, commands) := advanceModel fuel emit (second :: rest) held
              (remaining, if emit then second.arrowProgressWhenOn :: commands else [])
            else if first.isFinished then
              let (remaining, commands) := advanceModel fuel emit (second :: rest) held
              (remaining, if emit then first.arrowProgressWhenFinished :: commands else [])
            else (first :: second :: rest, [])
          else if first.isFinished then
            let (remaining, commands) := advanceModel fuel emit (second :: rest) held
            (remaining, if emit then first.arrowProgressWhenFinished :: commands else [])
          else (first :: second :: rest, [])

/-- Attaching note/track identifiers recovers the original reference's
    commands, without permuting, deduplicating or dropping any emission. -/
theorem advance_model_equiv (fuel : Nat) (emit : Bool) (queue : SlideQueue)
    (held : SensorVec Bool) (note : Nat) (track : Option Nat) :
    let out := advanceModel fuel emit queue held
    (out.1, out.2.map (slideHideBarCmd note track)) =
      slideQueueCoreFuel fuel note track emit queue held := by
  induction fuel generalizing queue with
  | zero => cases queue <;> rfl
  | succ fuel ih =>
      cases queue with
      | nil => rfl
      | cons first rest =>
          cases rest with
          | nil =>
              simp only [advanceModel, slideQueueCoreFuel, updateSlideArea]
              split <;> (try split) <;> cases emit <;> rfl
          | cons second rest =>
              simp only [advanceModel, slideQueueCoreFuel, updateSlideArea]
              split
              · split
                · simpa only [apply_ite, List.map_cons, List.map_nil] using
                    congrArg (fun out : SlideQueue × List RenderCommand =>
                      (out.1, if emit then
                        slideHideBarCmd note track (second.check held).arrowProgressWhenFinished :: out.2
                        else [])) (ih rest)
                · split
                  · simpa only [apply_ite, List.map_cons, List.map_nil] using
                      congrArg (fun out : SlideQueue × List RenderCommand =>
                        (out.1, if emit then
                          slideHideBarCmd note track (second.check held).arrowProgressWhenOn :: out.2
                          else [])) (ih (second.check held :: rest))
                  · split
                    · simpa only [apply_ite, List.map_cons, List.map_nil] using
                        congrArg (fun out : SlideQueue × List RenderCommand =>
                          (out.1, if emit then
                            slideHideBarCmd note track (first.check held).arrowProgressWhenFinished :: out.2
                            else [])) (ih (second.check held :: rest))
                    · rfl
              · split
                · simpa only [apply_ite, List.map_cons, List.map_nil] using
                    congrArg (fun out : SlideQueue × List RenderCommand =>
                      (out.1, if emit then
                        slideHideBarCmd note track (first.check held).arrowProgressWhenFinished :: out.2
                        else [])) (ih (second :: rest))
                · rfl

/-- At most one command is emitted per unit of fuel, and traversal never
    increases the number of checkpoints. These bounds justify Vec writes. -/
theorem advance_model_bounds (fuel : Nat) (emit : Bool) (queue : SlideQueue)
    (held : SensorVec Bool) :
    (advanceModel fuel emit queue held).1.length ≤ queue.length ∧
      (advanceModel fuel emit queue held).2.length ≤ fuel := by
  induction fuel generalizing queue with
  | zero => cases queue <;> simp [advanceModel]
  | succ fuel ih =>
      cases queue with
      | nil => simp [advanceModel]
      | cons first rest =>
          cases rest with
          | nil =>
              simp only [advanceModel]
              split <;> (try split) <;> cases emit <;> simp
          | cons second rest =>
              have hrest := ih rest
              have hsecond := ih (second :: rest)
              have hchecked := ih (second.check held :: rest)
              clear ih
              simp only [List.length_cons] at hsecond hchecked ⊢
              simp only [advanceModel]
              split
              · split
                · cases emit <;> simp_all <;> omega
                · split
                  · cases emit <;> simp_all <;> omega
                  · split <;> cases emit <;> simp_all <;> omega
              · split <;> cases emit <;> simp_all <;> omega

theorem advance_model_no_commands (fuel : Nat) (queue : SlideQueue)
    (held : SensorVec Bool) :
    (advanceModel fuel false queue held).2 = [] := by
  cases fuel <;> cases queue <;> simp only [advanceModel, Bool.false_eq_true, ↓reduceIte]
  rename_i fuel first rest
  cases rest <;> simp only [Bool.false_eq_true, ↓reduceIte]
  all_goals repeat' first | split | rfl

end Verification.SlideQueueProof
