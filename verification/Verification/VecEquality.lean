import Verification.Generated

namespace Verification.VecEquality
open Aeneas Aeneas.Std Result

/-- The Vec comparison model short-circuits over equally long lists. Its
    element comparison must agree with equality of represented values. -/
theorem allM_zip_equiv {T U : Type} [DecidableEq U]
    (eqInst : core.cmp.PartialEq T T) (repr : T → U)
    (heq : ∀ a b, eqInst.eq a b = ok (decide (repr a = repr b)))
    (xs ys : List T) (hlen : xs.length = ys.length) :
    List.allM (fun (a, b) => eqInst.eq a b) (List.zip xs ys) =
      ok (decide (xs.map repr = ys.map repr)) := by
  induction xs generalizing ys with
  | nil =>
      cases ys with
      | nil => rfl
      | cons => simp at hlen
  | cons x xs ih =>
      cases ys with
      | nil => simp_all
      | cons y ys =>
          have htail : xs.length = ys.length := by simpa using hlen
          simp only [List.zip_cons_cons, List.allM_cons, heq x y, bind_tc_ok,
            ih ys htail, List.map_cons, List.cons.injEq]
          by_cases hhead : repr x = repr y <;>
            simp [hhead] <;> rfl

theorem vec_eq_equiv {T U : Type} [DecidableEq U]
    (eqInst : core.cmp.PartialEq T T) (repr : T → U)
    (heq : ∀ a b, eqInst.eq a b = ok (decide (repr a = repr b)))
    (a b : alloc.vec.Vec T) :
    alloc.vec.partial_eq.PartialEqVec.eq eqInst a b =
      ok (decide (a.val.map repr = b.val.map repr)) := by
  unfold alloc.vec.partial_eq.PartialEqVec.eq
  simp only [alloc.vec.Vec.length]
  by_cases hlen : a.val.length = b.val.length
  · rw [if_pos hlen]
    exact allM_zip_equiv eqInst repr heq a.val b.val hlen
  · have hne : a.val.map repr ≠ b.val.map repr := by
      intro h
      apply hlen
      simpa only [List.length_map] using congrArg List.length h
    simp [hlen, hne]

end Verification.VecEquality
