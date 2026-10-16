import LnmaiCore.Simai.Source.Maidata
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega
import Init.Data.List.Count
import Init.Data.List.Nat.TakeDrop
import Init.Data.String.Lemmas.Intercalate

/-!
UNELABORATED source factoring draft for the new maidata_scan.rs.
No compiler, tests or extraction were run for this delivery. These are source
lemmas, not a claim about an as-yet-unavailable Generated.maidata_scan.
The list suffixes below describe borrowed cursor ranges; the Rust kernel never
copies them. No full scan, group, Maidata file or pass equivalence is a premise.
-/

namespace Verification.MaidataScanModel
open LnmaiCore LnmaiCore.Simai

open private sameEventKey isExpandedSlideGroupChild majdataTimingNoteCount
  majdataNoHeadSlideCount tagTouchEachGroup tagTouchEachGroup.loop takeSameEventRest
  annotateTouchEachGroupsFromFuel annotateTouchEachGroupsFrom
  startsWithAmp parseKeyValueLine collectChartBody parseMaidataLinesFuel
  parseMaidataLines from LnmaiCore.Simai.Source.Maidata

/-- Maximal consecutive prefix; the first failing element stays in the suffix. -/
def splitPrefix {α : Type} (keep : α → Bool) : List α → List α × List α
  | [] => ([], [])
  | x :: xs =>
      if keep x then
        (x :: (splitPrefix keep xs).1, (splitPrefix keep xs).2)
      else ([], x :: xs)

theorem splitPrefix_append {α : Type} (keep : α → Bool) (xs : List α) :
    (splitPrefix keep xs).1 ++ (splitPrefix keep xs).2 = xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      cases h : keep x <;> simp [splitPrefix, h, ih]

theorem splitPrefix_lengths {α : Type} (keep : α → Bool) (xs : List α) :
    (splitPrefix keep xs).1.length + (splitPrefix keep xs).2.length = xs.length := by
  simpa only [List.length_append] using
    congrArg List.length (splitPrefix_append keep xs)

theorem splitPrefix_take {α : Type} (keep : α → Bool) (xs : List α) :
    xs.take (splitPrefix keep xs).1.length = (splitPrefix keep xs).1 := by
  calc
    xs.take (splitPrefix keep xs).1.length =
        ((splitPrefix keep xs).1 ++ (splitPrefix keep xs).2).take
          (splitPrefix keep xs).1.length :=
      congrArg (fun ys => ys.take (splitPrefix keep xs).1.length)
        (splitPrefix_append keep xs).symm
    _ = _ := List.take_append_length

theorem splitPrefix_drop {α : Type} (keep : α → Bool) (xs : List α) :
    xs.drop (splitPrefix keep xs).1.length = (splitPrefix keep xs).2 := by
  calc
    xs.drop (splitPrefix keep xs).1.length =
        ((splitPrefix keep xs).1 ++ (splitPrefix keep xs).2).drop
          (splitPrefix keep xs).1.length :=
      congrArg (fun ys => ys.drop (splitPrefix keep xs).1.length)
        (splitPrefix_append keep xs).symm
    _ = _ := List.drop_append_length

theorem splitPrefix_boundary {α : Type} (keep : α → Bool) (xs : List α) :
    match (splitPrefix keep xs).2 with
    | [] => True
    | x :: _ => keep x = false := by
  induction xs with
  | nil => trivial
  | cons x xs ih =>
      cases h : keep x <;> simp [splitPrefix, h, ih]

theorem splitPrefix_all {α : Type} (keep : α → Bool) (xs : List α) :
    ∀ x ∈ (splitPrefix keep xs).1, keep x = true := by
  induction xs with
  | nil => simp [splitPrefix]
  | cons x xs ih =>
      cases h : keep x with
      | false => simp [splitPrefix, h]
      | true =>
          intro y hy
          have hmem : y ∈ x :: (splitPrefix keep xs).1 := by
            simpa only [splitPrefix, h, if_true] using hy
          rcases List.mem_cons.mp hmem with rfl | htail
          · exact h
          · exact ih y htail

/-- Absolute cursor endpoint. It needs only start ≤ length, not length < MAX. -/
def scanEnd {α : Type} (keep : α → Bool) (xs : List α) (start : Nat) : Nat :=
  start + (splitPrefix keep (xs.drop start)).1.length

theorem scanEnd_bounds {α : Type} (keep : α → Bool) (xs : List α)
    (start : Nat) (h : start ≤ xs.length) :
    start ≤ scanEnd keep xs start ∧ scanEnd keep xs start ≤ xs.length := by
  have hs := splitPrefix_lengths keep (xs.drop start)
  simp only [List.length_drop] at hs
  unfold scanEnd
  omega

def keepBody (line : String) : Bool := !startsWithAmp line

theorem collectChartBody_factor (lines acc : List String) :
    collectChartBody lines acc =
      (acc.reverse ++ (splitPrefix keepBody lines).1,
        (splitPrefix keepBody lines).2) := by
  induction lines generalizing acc with
  | nil => simp [collectChartBody, splitPrefix]
  | cons line rest ih =>
      cases h : startsWithAmp line <;>
        simp [collectChartBody, splitPrefix, keepBody, h, ih, List.append_assoc]

/-- Delimiter is not consumed, including an indented or bare ampersand line. -/
theorem collectChartBody_stop (line : String) (rest acc : List String)
    (h : startsWithAmp line = true) :
    collectChartBody (line :: rest) acc = (acc.reverse, line :: rest) := by
  simp [collectChartBody, h]

/-- Each step is exactly the two String writes in append_body_line. -/
def appendBody (value : String) : List String → String
  | [] => value
  | line :: rest => appendBody (value ++ "\n" ++ line) rest

theorem appendBody_eq_intercalate (value : String) (lines : List String) :
    appendBody value lines = String.intercalate "\n" (value :: lines) := by
  induction lines generalizing value with
  | nil => simp [appendBody]
  | cons line rest ih =>
      simp [appendBody, ih, String.append_assoc]

def sameAs (head token : RawNoteToken) : Bool :=
  sameEventKey token head.timing head.bpm head.hSpeed head.divisor

theorem takeSameEventRest_factor (head : RawNoteToken) (xs : List RawNoteToken) :
    takeSameEventRest head.timing head.bpm head.hSpeed head.divisor xs =
      splitPrefix (sameAs head) xs := by
  induction xs with
  | nil => rfl
  | cons token rest ih =>
      rw [takeSameEventRest, splitPrefix, ih]
      cases splitPrefix (sameAs head) rest
      rfl

/-- A group always consumes its head, even when the timing count is zero. -/
theorem group_remaining_lt (head : RawNoteToken) (rest : List RawNoteToken) :
    (splitPrefix (sameAs head) rest).2.length < (head :: rest).length := by
  have h := splitPrefix_lengths (sameAs head) rest
  simp only [List.length_cons]
  omega

def childIndex (token : RawNoteToken) : Bool :=
  match token.sourceGroupIndex with
  | some index => index != 0
  | none => false

/-- The Slide branch in the kernel tests childIndex; other kinds always count. -/
def timingPoint (token : RawNoteToken) : Bool :=
  match token.kind with
  | .slide => !childIndex token
  | _ => true

def noHeadPoint (token : RawNoteToken) : Bool :=
  timingPoint token && token.kind == .slide && token.isSlideNoHead

/-- Reduce the derived BEq by constructor computation, independently of simp
attributes on the generated RawNoteKind comparison. -/
theorem rawKind_beq_slide (kind : RawNoteKind) :
    (kind == .slide) = (match kind with | .slide => true | _ => false) := by
  cases kind <;> rfl

theorem timingPoint_eq (token : RawNoteToken) :
    timingPoint token = !isExpandedSlideGroupChild token := by
  cases hk : token.kind <;>
    cases hindex : token.sourceGroupIndex <;>
      simp only [timingPoint, childIndex, isExpandedSlideGroupChild,
        rawKind_beq_slide, hk, hindex] <;> rfl

def timingCount (tokens : List RawNoteToken) : Nat := tokens.countP timingPoint
def noHeadCount (tokens : List RawNoteToken) : Nat := tokens.countP noHeadPoint

theorem timingCount_eq (tokens : List RawNoteToken) :
    timingCount tokens = majdataTimingNoteCount tokens := by
  have hp : timingPoint = (fun token => !isExpandedSlideGroupChild token) :=
    funext timingPoint_eq
  simp only [timingCount, List.countP_eq_length_filter, hp, majdataTimingNoteCount]

theorem noHeadCount_eq (tokens : List RawNoteToken) :
    noHeadCount tokens = majdataNoHeadSlideCount tokens := by
  have hp : noHeadPoint = (fun token =>
      !isExpandedSlideGroupChild token && token.kind == .slide && token.isSlideNoHead) := by
    funext token
    simp only [noHeadPoint, timingPoint_eq]
  simp only [noHeadCount, List.countP_eq_length_filter, hp, majdataNoHeadSlideCount]

theorem counts_bounds (tokens : List RawNoteToken) :
    noHeadCount tokens ≤ timingCount tokens ∧ timingCount tokens ≤ tokens.length := by
  constructor
  · apply List.countP_mono_left
    intro token _ h
    simp only [noHeadPoint, Bool.and_eq_true] at h
    exact h.1.1
  · exact List.countP_le_length

/-- Point-count update needed for the generated counting loop invariant. -/
theorem counts_cons (token : RawNoteToken) (rest : List RawNoteToken) :
    timingCount (token :: rest) = timingCount rest + (if timingPoint token then 1 else 0) ∧
    noHeadCount (token :: rest) = noHeadCount rest + (if noHeadPoint token then 1 else 0) := by
  constructor <;> exact List.countP_cons

/-- Explicit per-token emission. Only tagged tokens advance the tag index.
The local recursion deliberately has the same shape as the source recursion;
the two count functions above are independently related to its filter counts. -/
def tagGroup (groupId : Nat) (tokens : List RawNoteToken) : List RawNoteToken :=
  let timingNoteCount := timingCount tokens
  let nonNoHeadCount := timingNoteCount - noHeadCount tokens
  let touchIsEach := timingNoteCount > 1 && nonNoHeadCount != 1
  let touchHoldIsEach := timingNoteCount > 1
  let rec loop (index : Nat) : List RawNoteToken → List RawNoteToken
    | [] => []
    | token :: rest =>
        let shouldTag :=
          match token.kind with
          | .touch => touchIsEach
          | .touchHold => touchHoldIsEach
          | _ => false
        if shouldTag then
          { token with
            sourceGroupId := some groupId
            , sourceGroupIndex := some index
            , sourceGroupSize := some timingNoteCount } :: loop (index + 1) rest
        else token :: loop index rest
  loop 0 tokens

/-- The two local recursive declarations have different names. Relate their
per-token branches explicitly, with arbitrary counters and initial tag index. -/
theorem tagGroup_loop_eq_source (groupId timingNoteCount : Nat)
    (touchIsEach : Bool) (index : Nat) (tokens : List RawNoteToken) :
    tagGroup.loop groupId timingNoteCount touchIsEach index tokens =
      tagTouchEachGroup.loop groupId timingNoteCount touchIsEach index tokens := by
  induction tokens generalizing index with
  | nil => rfl
  | cons token rest ih =>
      cases hk : token.kind <;>
        simp only [tagGroup.loop, tagTouchEachGroup.loop, hk, ih] <;> rfl

theorem tagGroup_eq_source (groupId : Nat) (tokens : List RawNoteToken) :
    tagGroup groupId tokens = tagTouchEachGroup groupId tokens := by
  unfold tagGroup tagTouchEachGroup
  simp only [timingCount_eq, noHeadCount_eq, tagGroup_loop_eq_source]

/-- Same source fuel semantics, with concrete split/count/write definitions. -/
def groupFuel (fuel groupId : Nat) (tokens : List RawNoteToken) : List RawNoteToken :=
  match tokens with
  | [] => []
  | head :: rest =>
      match fuel with
      | 0 => head :: rest
      | fuel + 1 =>
          let parts := splitPrefix (sameAs head) rest
          tagGroup groupId (head :: parts.1) ++ groupFuel fuel (groupId + 1) parts.2

theorem groupFuel_eq_source (fuel groupId : Nat) (tokens : List RawNoteToken) :
    groupFuel fuel groupId tokens = annotateTouchEachGroupsFromFuel fuel groupId tokens := by
  induction fuel generalizing groupId tokens with
  | zero => cases tokens <;> rfl
  | succ fuel ih =>
      cases tokens with
      | nil => rfl
      | cons head rest =>
          simp only [groupFuel, annotateTouchEachGroupsFromFuel,
            takeSameEventRest_factor, tagGroup_eq_source]
          cases splitPrefix (sameAs head) rest with
          | mk same remaining => simp only [ih]

theorem group_public_factor (groupId : Nat) (tokens : List RawNoteToken) :
    groupFuel tokens.length groupId tokens = annotateTouchEachGroupsFrom groupId tokens := by
  exact groupFuel_eq_source tokens.length groupId tokens

/-- Fuel independence is proved from strict group progress, not assumed. -/
theorem groupFuel_stable (fuel other groupId : Nat) (tokens : List RawNoteToken)
    (h : tokens.length ≤ fuel) (hother : tokens.length ≤ other) :
    groupFuel fuel groupId tokens = groupFuel other groupId tokens := by
  induction fuel generalizing other groupId tokens with
  | zero =>
      have ht : tokens = [] := List.length_eq_zero_iff.mp (by omega)
      subst tokens
      cases other <;> rfl
  | succ fuel ih =>
      cases tokens with
      | nil => cases other <;> rfl
      | cons head rest =>
          cases other with
          | zero => simp only [List.length_cons] at hother; omega
          | succ other =>
              have hremaining := group_remaining_lt head rest
              simp only [List.length_cons] at h hother hremaining
              change
                tagGroup groupId (head :: (splitPrefix (sameAs head) rest).1) ++
                  groupFuel fuel (groupId + 1) (splitPrefix (sameAs head) rest).2 =
                tagGroup groupId (head :: (splitPrefix (sameAs head) rest).1) ++
                  groupFuel other (groupId + 1) (splitPrefix (sameAs head) rest).2
              congr 1
              exact ih other (groupId + 1) (splitPrefix (sameAs head) rest).2
                (by omega) (by omega)

/-- Forward prefixes, as used by the runtime. A header value is always the
first body component, even if empty; raw body lines retain their whitespace. -/
def linesFuel (fuel : Nat) (lines : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock) : MaidataFile :=
  match lines with
  | [] => { metadata := { fields }, charts }
  | line :: rest =>
      match fuel with
      | 0 => { metadata := { fields }, charts }
      | fuel + 1 =>
          if trim line = "" then linesFuel fuel rest fields charts
          else if startsWithAmp line then
            match parseKeyValueLine line with
            | none => linesFuel fuel rest fields charts
            | some (key, value) =>
                if key.startsWith "&inote_" then
                  let parts := splitPrefix keepBody rest
                  let block : MaidataChartBlock :=
                    { levelIndex := ((key.drop 7).toString.toNat?).getD 0
                      rawBody := appendBody value parts.1 }
                  linesFuel fuel parts.2 fields (charts ++ [block])
                else linesFuel fuel rest (fields ++ [(key, value)]) charts
          else linesFuel fuel rest fields charts

/-- Private-source accumulators are reverse prefixes, not equal forward lists. -/
theorem linesFuel_eq_source (fuel : Nat) (lines : List String)
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    linesFuel fuel lines fields charts =
      parseMaidataLinesFuel fuel lines fields.reverse charts.reverse := by
  induction fuel generalizing lines fields charts with
  | zero => cases lines <;> simp [linesFuel, parseMaidataLinesFuel]
  | succ fuel ih =>
      cases lines with
      | nil => simp [linesFuel, parseMaidataLinesFuel]
      | cons line rest =>
          rw [linesFuel, parseMaidataLinesFuel]
          by_cases hblank : trim line = ""
          · simp only [hblank, if_true, ih]
          · simp only [hblank, if_false]
            cases hamp : startsWithAmp line <;> simp only [hamp, Bool.false_eq_true, if_false, if_true]
            · exact ih rest fields charts
            · cases hkv : parseKeyValueLine line with
              | none => simpa only [hkv] using ih rest fields charts
              | some pair =>
                  rcases pair with ⟨key, value⟩
                  simp only [hkv]
                  cases hchart : key.startsWith "&inote_" <;>
                    simp only [hchart, Bool.false_eq_true, if_false, if_true]
                  · simpa using ih rest (fields ++ [(key, value)]) charts
                  · rw [collectChartBody_factor]
                    simp only [List.reverse_cons, List.reverse_nil, List.nil_append,
                      List.singleton_append]
                    let block : MaidataChartBlock :=
                      { levelIndex := ((key.drop 7).toString.toNat?).getD 0
                        rawBody := appendBody value (splitPrefix keepBody rest).1 }
                    let sourceBlock : MaidataChartBlock :=
                      { levelIndex := ((key.drop 7).toString.toNat?).getD 0
                        rawBody := String.intercalate "\n"
                          (value :: (splitPrefix keepBody rest).1) }
                    change linesFuel fuel (splitPrefix keepBody rest).2 fields
                        (charts ++ [block]) =
                      parseMaidataLinesFuel fuel (splitPrefix keepBody rest).2
                        fields.reverse (sourceBlock :: charts.reverse)
                    have hblock : block = sourceBlock := by
                      simp only [block, sourceBlock, appendBody_eq_intercalate]
                    have hacc : (charts ++ [block]).reverse =
                        sourceBlock :: charts.reverse := by
                      simp only [List.reverse_append, List.reverse_cons,
                        List.reverse_nil, List.nil_append, List.singleton_append, hblock]
                    exact (ih (splitPrefix keepBody rest).2 fields (charts ++ [block])).trans
                      (congrArg (fun acc => parseMaidataLinesFuel fuel
                        (splitPrefix keepBody rest).2 fields.reverse acc) hacc)

theorem parse_public_factor (content : String) :
    Except.ok (linesFuel ((content.splitOn "\n").length + 1)
      (content.splitOn "\n") [] []) = parseSourceMaidata content := by
  unfold parseSourceMaidata parseMaidataLines
  rw [linesFuel_eq_source]
  rfl

/- Remaining integration obligations (not hypotheses smuggled into a theorem):
1. Extract and prove each real maidata_scan loop against the cursor/list model.
2. linesFuel fuel sufficiency from splitPrefix_lengths; the public factoring
   above deliberately uses the original length+1 fuel and is not this lemma.
3. Generated count/emit cursor invariants and tag-index bounds; GroupId is Nat,
   never usize, including next_group after the final group.
4. Runtime lexical/field reads, Natural suffix parsing/default/conversions,
   exact event equality, append/record preservation, allocation and capacity.
5. UTF-8 String encoding and actual String/Vec operations. appendBody's source
   equation does not prove Rust's String implementation.
6. sourceChartFromTokens, tokenization, typecheck, normalization and ChartSpec
   are outside this two-kernel refactor, as are first-match facade lookup proofs.
-/

end Verification.MaidataScanModel
