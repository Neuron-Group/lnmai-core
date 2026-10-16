import Verification.MaidataScanModel

/-!
Source fuel sufficiency for the original Maidata parser. UNELABORATED: the
mainline's sole Lean worker must check this file after MaidataScanModel.
The fuel is a mathematical Nat. In particular length + 1 is never encoded as
a machine integer. No scan, loop-body or pass equivalence is a hypothesis.
-/

namespace Verification.MaidataScanFuel
open LnmaiCore LnmaiCore.Simai Verification.MaidataScanModel

open private startsWithAmp parseKeyValueLine collectChartBody
  parseMaidataLinesFuel parseMaidataLines from LnmaiCore.Simai.Source.Maidata

theorem body_remaining_le (rest : List String) :
    (splitPrefix keepBody rest).2.length ≤ rest.length := by
  have h := splitPrefix_lengths keepBody rest
  omega

/-- The outer iteration consumes its header even if the body is empty. -/
theorem body_remaining_lt (line : String) (rest : List String) :
    (splitPrefix keepBody rest).2.length < (line :: rest).length := by
  have h := body_remaining_le rest
  simp only [List.length_cons]
  omega

theorem source_body_remaining_le (rest acc : List String) :
    (collectChartBody rest acc).2.length ≤ rest.length := by
  simpa only [collectChartBody_factor] using body_remaining_le rest

theorem linesFuel_nil (fuel : Nat) (fields : List (String × String))
    (charts : List MaidataChartBlock) :
    linesFuel fuel [] fields charts = { metadata := { fields }, charts } := by
  cases fuel <;> rfl

/-- Both fuel arguments can be arbitrary above remaining length. The proof
splits the real blank/amp/header/chart decisions and uses strict consumption. -/
theorem linesFuel_stable (fuel other : Nat) (lines : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock)
    (hf : lines.length ≤ fuel) (ho : lines.length ≤ other) :
    linesFuel fuel lines fields charts = linesFuel other lines fields charts := by
  induction fuel generalizing other lines fields charts with
  | zero =>
      have hempty : lines = [] := List.length_eq_zero_iff.mp (by omega)
      subst lines
      simp only [linesFuel_nil]
  | succ fuel ih =>
      cases lines with
      | nil => simp only [linesFuel_nil]
      | cons line rest =>
          cases other with
          | zero => simp only [List.length_cons] at ho; omega
          | succ other =>
              have hrest : rest.length ≤ fuel := by
                simp only [List.length_cons] at hf
                omega
              have hrest' : rest.length ≤ other := by
                simp only [List.length_cons] at ho
                omega
              have hbody := body_remaining_le rest
              simp only [linesFuel]
              by_cases hblank : trim line = ""
              · simp only [hblank, if_true]
                exact ih other rest fields charts hrest hrest'
              · simp only [hblank, if_false]
                cases hamp : startsWithAmp line with
                | false =>
                    simp only [hamp, Bool.false_eq_true, if_false]
                    exact ih other rest fields charts hrest hrest'
                | true =>
                    simp only [hamp, if_true]
                    cases hkv : parseKeyValueLine line with
                    | none =>
                        simp only [hkv]
                        exact ih other rest fields charts hrest hrest'
                    | some pair =>
                        rcases pair with ⟨key, value⟩
                        simp only [hkv]
                        cases hchart : key.startsWith "&inote_" with
                        | false =>
                            simp only [hchart, Bool.false_eq_true, if_false]
                            exact ih other rest (fields ++ [(key, value)]) charts
                              hrest hrest'
                        | true =>
                            simp only [hchart, if_true]
                            exact ih other (splitPrefix keepBody rest).2 fields
                              (charts ++ [{
                                levelIndex := ((key.drop 7).toString.toNat?).getD 0
                                rawBody := appendBody value (splitPrefix keepBody rest).1 }])
                              (by omega) (by omega)

/-- Exact correspondence for arbitrary original REVERSED accumulators. -/
theorem sourceFuel_eq_linesFuel (fuel : Nat) (lines : List String)
    (reverseFields : List (String × String)) (reverseCharts : List MaidataChartBlock) :
    parseMaidataLinesFuel fuel lines reverseFields reverseCharts =
      linesFuel fuel lines reverseFields.reverse reverseCharts.reverse := by
  simpa only [List.reverse_reverse] using
    (linesFuel_eq_source fuel lines reverseFields.reverse reverseCharts.reverse).symm

theorem sourceFuel_stable (fuel other : Nat) (lines : List String)
    (reverseFields : List (String × String)) (reverseCharts : List MaidataChartBlock)
    (hf : lines.length ≤ fuel) (ho : lines.length ≤ other) :
    parseMaidataLinesFuel fuel lines reverseFields reverseCharts =
      parseMaidataLinesFuel other lines reverseFields reverseCharts := by
  rw [sourceFuel_eq_linesFuel, sourceFuel_eq_linesFuel]
  exact linesFuel_stable fuel other lines reverseFields.reverse reverseCharts.reverse hf ho

theorem sourceFuel_sufficient (fuel : Nat) (lines : List String)
    (reverseFields : List (String × String)) (reverseCharts : List MaidataChartBlock)
    (hf : lines.length ≤ fuel) :
    parseMaidataLinesFuel fuel lines reverseFields reverseCharts =
      parseMaidataLines lines reverseFields reverseCharts := by
  unfold parseMaidataLines
  exact sourceFuel_stable fuel (lines.length + 1) lines reverseFields reverseCharts
    hf (by omega)

theorem linesFuel_sufficient (fuel : Nat) (lines : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock)
    (hf : lines.length ≤ fuel) :
    linesFuel fuel lines fields charts =
      parseMaidataLines lines fields.reverse charts.reverse := by
  rw [linesFuel_eq_source]
  exact sourceFuel_sufficient fuel lines fields.reverse charts.reverse hf

/-- Complete source result used as a loop invariant. This is a definition from
the original finite semantics, not an additional executable parser assumption. -/
def complete (lines : List String) (fields : List (String × String))
    (charts : List MaidataChartBlock) : MaidataFile :=
  linesFuel lines.length lines fields charts

theorem linesFuel_eq_complete (fuel : Nat) (lines : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock)
    (hf : lines.length ≤ fuel) :
    linesFuel fuel lines fields charts = complete lines fields charts :=
  linesFuel_stable fuel lines.length lines fields charts hf (Nat.le_refl _)

theorem complete_eq_source (lines : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    complete lines fields charts =
      parseMaidataLines lines fields.reverse charts.reverse :=
  linesFuel_sufficient lines.length lines fields charts (Nat.le_refl _)

theorem complete_nil (fields : List (String × String))
    (charts : List MaidataChartBlock) :
    complete [] fields charts = { metadata := { fields }, charts } := rfl

/-- The one-step equation keeps every original branch, including None, and
normalizes the chart branch's smaller remaining fuel using the theorem above. -/
theorem complete_cons (line : String) (rest : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    complete (line :: rest) fields charts =
      if trim line = "" then complete rest fields charts
      else if startsWithAmp line then
        match parseKeyValueLine line with
        | none => complete rest fields charts
        | some (key, value) =>
            if key.startsWith "&inote_" then
              complete (splitPrefix keepBody rest).2 fields
                (charts ++ [{
                  levelIndex := ((key.drop 7).toString.toNat?).getD 0
                  rawBody := appendBody value (splitPrefix keepBody rest).1 }])
            else complete rest (fields ++ [(key, value)]) charts
      else complete rest fields charts := by
  change linesFuel (rest.length + 1) (line :: rest) fields charts = _
  simp only [linesFuel]
  by_cases hblank : trim line = ""
  · simp only [hblank, if_true]; rfl
  · simp only [hblank, if_false]
    cases hamp : startsWithAmp line with
    | false => simp only [hamp, Bool.false_eq_true, if_false]; rfl
    | true =>
        simp only [hamp, if_true]
        cases hkv : parseKeyValueLine line with
        | none => simp only [hkv]; rfl
        | some pair =>
            rcases pair with ⟨key, value⟩
            simp only [hkv]
            cases hchart : key.startsWith "&inote_" with
            | false => simp only [hchart, Bool.false_eq_true, if_false]; rfl
            | true =>
                simp only [hchart, if_true]
                exact linesFuel_eq_complete rest.length (splitPrefix keepBody rest).2
                  fields _ (body_remaining_le rest)

theorem parse_public_factor_sufficient (content : String) (fuel : Nat)
    (hf : (content.splitOn "\n").length ≤ fuel) :
    Except.ok (linesFuel fuel (content.splitOn "\n") [] []) =
      parseSourceMaidata content := by
  calc
    Except.ok (linesFuel fuel (content.splitOn "\n") [] []) =
        Except.ok (linesFuel ((content.splitOn "\n").length + 1)
          (content.splitOn "\n") [] []) := by
      congr 1
      exact linesFuel_stable _ _ _ [] [] hf (by omega)
    _ = parseSourceMaidata content := parse_public_factor content

theorem parse_public_complete (content : String) :
    Except.ok (complete (content.splitOn "\n") [] []) = parseSourceMaidata content :=
  parse_public_factor_sufficient content _ (Nat.le_refl _)

/-- The suffix after an absolute endpoint is exactly the unconsumed suffix. -/
theorem drop_scanEnd {α : Type} (keep : α → Bool) (xs : List α) (start : Nat) :
    xs.drop (scanEnd keep xs start) = (splitPrefix keep (xs.drop start)).2 := by
  simpa only [scanEnd, List.drop_drop] using splitPrefix_drop keep (xs.drop start)

theorem scanEnd_finished {α : Type} (keep : α → Bool) (xs : List α)
    (cursor : Nat) (hcursor : xs.length ≤ cursor) :
    scanEnd keep xs cursor = cursor := by
  simp [scanEnd, List.drop_eq_nil_of_le hcursor, splitPrefix]

/-- Exact endpoint recurrence at a valid index; the first failing item stays. -/
theorem scanEnd_step {α : Type} (keep : α → Bool) (xs : List α)
    (cursor : Nat) (hcursor : cursor < xs.length) :
    scanEnd keep xs cursor =
      if keep xs[cursor] then scanEnd keep xs (cursor + 1) else cursor := by
  unfold scanEnd
  rw [List.drop_eq_getElem_cons hcursor]
  cases hk : keep xs[cursor] <;> simp [splitPrefix, hk, Nat.add_assoc, Nat.add_comm,
    Nat.add_left_comm]

theorem cursor_increment_bounds (len cursor maximum : Nat)
    (hcursor : cursor < len) (hlen : len ≤ maximum) :
    cursor + 1 ≤ len ∧ cursor + 1 ≤ maximum ∧
      len - (cursor + 1) + 1 = len - cursor ∧
      len - (cursor + 1) < len - cursor := by omega

theorem chart_cursor_bounds (lines : List String) (cursor maximum : Nat)
    (hcursor : cursor < lines.length) (hlen : lines.length ≤ maximum) :
    cursor + 1 ≤ scanEnd keepBody lines (cursor + 1) ∧
      scanEnd keepBody lines (cursor + 1) ≤ lines.length ∧
      scanEnd keepBody lines (cursor + 1) ≤ maximum ∧
      lines.length - scanEnd keepBody lines (cursor + 1) < lines.length - cursor := by
  have h := scanEnd_bounds keepBody lines (cursor + 1) (by omega)
  omega

/-- The next outer state's canonical fuel is no greater than the predecessor
fuel minus one, also when a chart consumes an arbitrarily long body. -/
theorem cursor_fuel_sufficient (len cursor next fuel : Nat)
    (hcursor : cursor < next) (hnext : next ≤ len) (hfuel : len - cursor ≤ fuel + 1) :
    len - next ≤ fuel := by omega

/-- An indexed form for the actual borrowed-input loop; cursors past the end
also denote the empty suffix, as in the source model. -/
def resume (lines : List String) (cursor : Nat) (fields : List (String × String))
    (charts : List MaidataChartBlock) : MaidataFile :=
  complete (lines.drop cursor) fields charts

theorem resume_eq_source (lines : List String) (cursor : Nat)
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    resume lines cursor fields charts =
      parseMaidataLines (lines.drop cursor) fields.reverse charts.reverse :=
  complete_eq_source _ fields charts

theorem resume_done (lines : List String) (cursor : Nat)
    (fields : List (String × String)) (charts : List MaidataChartBlock)
    (hcursor : lines.length ≤ cursor) :
    resume lines cursor fields charts = { metadata := { fields }, charts } := by
  simp only [resume, List.drop_eq_nil_of_le hcursor, complete_nil]

theorem resume_start (lines : List String) (fields : List (String × String))
    (charts : List MaidataChartBlock) :
    resume lines 0 fields charts = complete lines fields charts := by
  simp only [resume, List.drop_zero]

/-- Single-line transition for the forthcoming Generated proof. Every branch
is derived from complete_cons, not supplied as a whole-body assumption. -/
theorem resume_step (lines : List String) (cursor : Nat)
    (fields : List (String × String)) (charts : List MaidataChartBlock)
    (hcursor : cursor < lines.length) :
    resume lines cursor fields charts =
      if trim lines[cursor] = "" then resume lines (cursor + 1) fields charts
      else if startsWithAmp lines[cursor] then
        match parseKeyValueLine lines[cursor] with
        | none => resume lines (cursor + 1) fields charts
        | some (key, value) =>
            if key.startsWith "&inote_" then
              resume lines (scanEnd keepBody lines (cursor + 1)) fields
                (charts ++ [{
                  levelIndex := ((key.drop 7).toString.toNat?).getD 0
                  rawBody := appendBody value
                    (splitPrefix keepBody (lines.drop (cursor + 1))).1 }])
            else resume lines (cursor + 1) (fields ++ [(key, value)]) charts
      else resume lines (cursor + 1) fields charts := by
  unfold resume
  rw [List.drop_eq_getElem_cons hcursor, complete_cons, drop_scanEnd]

end Verification.MaidataScanFuel
