import Init.Data.List.Lemmas
import Init.Data.List.SplitOn.Lemmas
import Init.Data.List.Sublist

/-! Concrete character semantics for prefix_directives.rs.
No Generated declarations or runtime-facade agreement premises occur here.
This is an uncompiled source draft for the main Lean worker. -/

namespace Verification.PrefixDirectivesModel

/-- The four-character predicate used by the original Lean trimAscii. -/
def isPrefixWhitespace (c : Char) : Bool :=
  c = ' ' || c = '\t' || c = '\r' || c = '\n'

theorem isPrefixWhitespace_eq (c : Char) :
    isPrefixWhitespace c = Char.isWhitespace c := rfl

def trimChars (cs : List Char) : List Char :=
  ((cs.dropWhile isPrefixWhitespace).reverse.dropWhile isPrefixWhitespace).reverse

def directiveCloser (c : Char) : Option Char :=
  if c == '{' then some '}'
  else if c == '(' then some ')'
  else if c == '<' then some '>'
  else none

/-- Discard through the first closer. An absent closer discards everything. -/
def afterFirst (closer : Char) : List Char → List Char
  | [] => []
  | c :: cs => if c == closer then cs else afterFirst closer cs

/-- Only one prefix is processed. The suffix is trimmed but never reparsed. -/
def stripTrimmed (cs : List Char) : List Char :=
  match cs with
  | [] => []
  | c :: _ =>
      match directiveCloser c with
      | none => cs
      | some closer => trimChars (afterFirst closer cs)

def stripChars (cs : List Char) : List Char :=
  stripTrimmed (trimChars cs)

@[simp] theorem trimChars_nil : trimChars [] = [] := rfl

@[simp] theorem afterFirst_nil (closer : Char) : afterFirst closer [] = [] := rfl

theorem afterFirst_absent (closer : Char) (cs : List Char)
    (h : closer ∉ cs) : afterFirst closer cs = [] := by
  induction cs with
  | nil => rfl
  | cons c cs ih =>
      have hc : c ≠ closer := by
        intro heq
        exact h (by simp [heq])
      have hr : closer ∉ cs := fun hm => h (List.mem_cons_of_mem _ hm)
      simp only [afterFirst, beq_iff_eq, hc, ↓reduceIte]
      exact ih hr

/-- Any later closer, opener or Unicode scalar in rest is retained unchanged. -/
theorem afterFirst_first (closer : Char) (before rest : List Char)
    (h : closer ∉ before) :
    afterFirst closer (before ++ closer :: rest) = rest := by
  induction before with
  | nil => simp [afterFirst]
  | cons c before ih =>
      have hc : c ≠ closer := by
        intro heq
        exact h (by simp [heq])
      have hr : closer ∉ before := fun hm => h (List.mem_cons_of_mem _ hm)
      simp only [List.cons_append, afterFirst, beq_iff_eq, hc, ↓reduceIte]
      exact ih hr

/-- splitOn followed by intercalating its tail means first-closer scanning,
including the unclosed-input case and all empty fields. -/
theorem afterFirst_splitTail (closer : Char) (cs : List Char) :
    afterFirst closer cs = [closer].intercalate (cs.splitOn closer).tail := by
  induction cs with
  | nil => simp [afterFirst]
  | cons c cs ih =>
      by_cases hc : c = closer
      · subst c
        simp [afterFirst, List.splitOn_cons_eq_if_modifyHead]
      · simp [afterFirst, List.splitOn_cons_eq_if_modifyHead, hc, ih]

theorem afterFirst_sublist (closer : Char) (cs : List Char) :
    (afterFirst closer cs).Sublist cs := by
  induction cs with
  | nil => exact List.Sublist.refl _
  | cons c cs ih =>
      simp only [afterFirst]
      split
      · exact List.Sublist.cons _ (List.Sublist.refl _)
      · exact List.Sublist.cons _ ih

theorem trimChars_sublist (cs : List Char) : (trimChars cs).Sublist cs := by
  have hf := List.dropWhile_sublist (l := cs) isPrefixWhitespace
  have hb := List.dropWhile_sublist
    (l := (cs.dropWhile isPrefixWhitespace).reverse) isPrefixWhitespace
  have hb' : (trimChars cs).Sublist (cs.dropWhile isPrefixWhitespace) := by
    simpa only [trimChars, List.reverse_reverse] using hb.reverse
  exact hb'.trans hf

theorem stripTrimmed_sublist (cs : List Char) : (stripTrimmed cs).Sublist cs := by
  cases cs with
  | nil => exact List.Sublist.refl _
  | cons c rest =>
      simp only [stripTrimmed]
      cases h : directiveCloser c with
      | none => exact List.Sublist.refl _
      | some closer =>
          exact (trimChars_sublist _).trans (afterFirst_sublist closer _)

/-- No insertion, reordering, or duplication occurs, including Unicode input. -/
theorem stripChars_sublist (cs : List Char) : (stripChars cs).Sublist cs :=
  (stripTrimmed_sublist _).trans (trimChars_sublist cs)

theorem stripChars_length_le (cs : List Char) :
    (stripChars cs).length ≤ cs.length :=
  (stripChars_sublist cs).length_le

theorem stripTrimmed_unclosed (c closer : Char) (rest : List Char)
    (hc : directiveCloser c = some closer) (hn : closer ∉ c :: rest) :
    stripTrimmed (c :: rest) = [] := by
  simp only [stripTrimmed, hc, afterFirst_absent closer _ hn, trimChars_nil]

/-- Exposes the second trim without assuming that the result is a note token. -/
theorem stripTrimmed_closed (c closer : Char) (before rest : List Char)
    (hc : directiveCloser c = some closer)
    (hn : closer ∉ c :: before) :
    stripTrimmed (c :: (before ++ closer :: rest)) = trimChars rest := by
  simp only [stripTrimmed, hc]
  change trimChars (afterFirst closer ((c :: before) ++ closer :: rest)) = trimChars rest
  rw [afterFirst_first closer (c :: before) rest hn]

end Verification.PrefixDirectivesModel
