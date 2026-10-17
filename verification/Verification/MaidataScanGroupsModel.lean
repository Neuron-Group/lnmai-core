import Verification.MaidataScanFuel

/-!
Source range/resume lemmas for the extracted maidata_scan touch-group loops.
UNELABORATED: mainline owns Lean execution. No whole scan or group result is
a premise. `emitTokens` is the original per-token source recursion exposed by
MaidataScanModel, not an independent implementation assumed to agree with it.
-/

namespace Verification.MaidataScanGroupsModel

open LnmaiCore LnmaiCore.Simai
open Verification.MaidataScanModel Verification.MaidataScanFuel

open private annotateTouchEachGroupsFrom annotateTouchEachGroups
  from LnmaiCore.Simai.Source.Maidata

def rangeTokens (tokens : List RawNoteToken) (stop cursor : Nat) : List RawNoteToken :=
  (tokens.drop cursor).take (stop - cursor)

theorem rangeTokens_done (tokens : List RawNoteToken) (stop cursor : Nat)
    (h : stop ≤ cursor) : rangeTokens tokens stop cursor = [] := by
  simp only [rangeTokens, Nat.sub_eq_zero_of_le h, List.take_zero]

theorem rangeTokens_step (tokens : List RawNoteToken) (stop cursor : Nat)
    (hmore : cursor < stop) (hstop : stop ≤ tokens.length) :
    rangeTokens tokens stop cursor =
      tokens[cursor]'(by omega) :: rangeTokens tokens stop (cursor + 1) := by
  have hi : cursor < tokens.length := by omega
  have hs : stop - cursor = (stop - (cursor + 1)) + 1 := by omega
  simp only [rangeTokens, List.drop_eq_getElem_cons hi, hs, List.take_succ_cons]

theorem rangeTokens_length_le (tokens : List RawNoteToken) (stop cursor : Nat) :
    (rangeTokens tokens stop cursor).length ≤ stop - cursor := by
  simp only [rangeTokens, List.length_take]
  exact Nat.min_le_left _ _

theorem range_counts_step (tokens : List RawNoteToken) (stop cursor : Nat)
    (hmore : cursor < stop) (hstop : stop ≤ tokens.length) :
    timingCount (rangeTokens tokens stop cursor) =
        timingCount (rangeTokens tokens stop (cursor + 1)) +
          (if timingPoint (tokens[cursor]'(by omega)) then 1 else 0) ∧
    noHeadCount (rangeTokens tokens stop cursor) =
        noHeadCount (rangeTokens tokens stop (cursor + 1)) +
          (if noHeadPoint (tokens[cursor]'(by omega)) then 1 else 0) := by
  rw [rangeTokens_step tokens stop cursor hmore hstop]
  exact counts_cons _ _

def eventEnd (tokens : List RawNoteToken) (cursor : Nat) (hi : cursor < tokens.length) : Nat :=
  scanEnd (sameAs tokens[cursor]) tokens (cursor + 1)

theorem eventEnd_bounds (tokens : List RawNoteToken) (cursor : Nat)
    (hi : cursor < tokens.length) :
    cursor < eventEnd tokens cursor hi ∧ eventEnd tokens cursor hi ≤ tokens.length := by
  have hb := scanEnd_bounds (sameAs tokens[cursor]) tokens (cursor + 1) (by omega)
  unfold eventEnd
  omega

/-- The head is consumed unconditionally; only its consecutive same-event tail
is included. A later token with the same key cannot cross a failed boundary. -/
theorem eventRange_eq (tokens : List RawNoteToken) (cursor : Nat)
    (hi : cursor < tokens.length) :
    rangeTokens tokens (eventEnd tokens cursor hi) cursor =
      tokens[cursor] :: (splitPrefix (sameAs tokens[cursor])
        (tokens.drop (cursor + 1))).1 := by
  have hb := eventEnd_bounds tokens cursor hi
  rw [rangeTokens_step tokens _ cursor hb.1 hb.2]
  simp only [rangeTokens, eventEnd, scanEnd, Nat.add_sub_cancel_left, splitPrefix_take]

theorem eventEnd_remainder (tokens : List RawNoteToken) (cursor : Nat)
    (hi : cursor < tokens.length) :
    tokens.drop (eventEnd tokens cursor hi) =
      (splitPrefix (sameAs tokens[cursor]) (tokens.drop (cursor + 1))).2 :=
  drop_scanEnd (sameAs tokens[cursor]) tokens (cursor + 1)

theorem eventEnd_boundary (tokens : List RawNoteToken) (cursor : Nat)
    (hi : cursor < tokens.length) :
    match tokens.drop (eventEnd tokens cursor hi) with
    | [] => True
    | token :: _ => sameAs tokens[cursor] token = false := by
  rw [eventEnd_remainder]
  exact splitPrefix_boundary (sameAs tokens[cursor]) (tokens.drop (cursor + 1))

def groupComplete (groupId : Nat) (tokens : List RawNoteToken) : List RawNoteToken :=
  groupFuel tokens.length groupId tokens

theorem groupComplete_nil (groupId : Nat) : groupComplete groupId [] = [] := rfl

theorem groupComplete_cons (groupId : Nat) (head : RawNoteToken)
    (rest : List RawNoteToken) :
    groupComplete groupId (head :: rest) =
      tagGroup groupId (head :: (splitPrefix (sameAs head) rest).1) ++
        groupComplete (groupId + 1) (splitPrefix (sameAs head) rest).2 := by
  have hb := splitPrefix_lengths (sameAs head) rest
  change tagGroup groupId (head :: (splitPrefix (sameAs head) rest).1) ++
      groupFuel rest.length (groupId + 1) (splitPrefix (sameAs head) rest).2 = _
  rw [groupFuel_stable rest.length (splitPrefix (sameAs head) rest).2.length
    (groupId + 1) (splitPrefix (sameAs head) rest).2 (by omega) (Nat.le_refl _)]
  rfl

theorem groupComplete_source (groupId : Nat) (tokens : List RawNoteToken) :
    groupComplete groupId tokens = annotateTouchEachGroupsFrom groupId tokens :=
  group_public_factor groupId tokens

def groupResume (tokens : List RawNoteToken) (cursor groupId : Nat)
    (output : List RawNoteToken) : List RawNoteToken :=
  output ++ groupComplete groupId (tokens.drop cursor)

theorem groupResume_done (tokens : List RawNoteToken) (cursor groupId : Nat)
    (output : List RawNoteToken) (h : tokens.length ≤ cursor) :
    groupResume tokens cursor groupId output = output := by
  simp only [groupResume, List.drop_eq_nil_of_le h, groupComplete_nil, List.append_nil]

theorem groupResume_step (tokens : List RawNoteToken) (cursor groupId : Nat)
    (output : List RawNoteToken) (hi : cursor < tokens.length) :
    groupResume tokens cursor groupId output =
      groupResume tokens (eventEnd tokens cursor hi) (groupId + 1)
        (output ++ tagGroup groupId (rangeTokens tokens (eventEnd tokens cursor hi) cursor)) := by
  unfold groupResume
  rw [List.drop_eq_getElem_cons hi, groupComplete_cons, eventRange_eq, eventEnd_remainder]
  exact (List.append_assoc _ _ _).symm

theorem groupResume_start (tokens : List RawNoteToken) (groupId : Nat)
    (output : List RawNoteToken) :
    groupResume tokens 0 groupId output =
      output ++ annotateTouchEachGroupsFrom groupId tokens := by
  simp only [groupResume, List.drop_zero, groupComplete_source]

def shouldTag (size : Nat) (touchEach : Bool) (token : RawNoteToken) : Bool :=
  match token.kind with
  | .touch => touchEach
  | .touchHold => decide (size > 1)
  | _ => false

def tagged (groupId index size : Nat) (token : RawNoteToken) : RawNoteToken :=
  { token with
    sourceGroupId := some groupId
    sourceGroupIndex := some index
    sourceGroupSize := some size }

/-- Precisely the source local recursion; its three-field write and index step
are unfolded below and in the actual extracted emission-body proof. -/
def emitTokens (groupId size : Nat) (touchEach : Bool) (index : Nat)
    (tokens : List RawNoteToken) : List RawNoteToken :=
  tagGroup.loop groupId size touchEach index tokens

theorem emitTokens_nil (groupId size : Nat) (touchEach : Bool) (index : Nat) :
    emitTokens groupId size touchEach index [] = [] := rfl

theorem emitTokens_cons (groupId size : Nat) (touchEach : Bool) (index : Nat)
    (token : RawNoteToken) (rest : List RawNoteToken) :
    emitTokens groupId size touchEach index (token :: rest) =
      if shouldTag size touchEach token then
        tagged groupId index size token :: emitTokens groupId size touchEach (index + 1) rest
      else token :: emitTokens groupId size touchEach index rest := rfl

theorem emitTokens_group (groupId : Nat) (tokens : List RawNoteToken) :
    emitTokens groupId (timingCount tokens)
      (decide (timingCount tokens > 1) && timingCount tokens - noHeadCount tokens != 1)
      0 tokens = tagGroup groupId tokens := rfl

def emitResume (tokens : List RawNoteToken) (stop cursor groupId size : Nat)
    (touchEach : Bool) (index : Nat) (output : List RawNoteToken) : List RawNoteToken :=
  output ++ emitTokens groupId size touchEach index (rangeTokens tokens stop cursor)

theorem emitResume_done (tokens : List RawNoteToken) (stop cursor groupId size : Nat)
    (touchEach : Bool) (index : Nat) (output : List RawNoteToken) (h : stop ≤ cursor) :
    emitResume tokens stop cursor groupId size touchEach index output = output := by
  simp only [emitResume, rangeTokens_done tokens stop cursor h, emitTokens_nil, List.append_nil]

theorem emitResume_step (tokens : List RawNoteToken) (stop cursor groupId size : Nat)
    (touchEach : Bool) (index : Nat) (output : List RawNoteToken)
    (hmore : cursor < stop) (hstop : stop ≤ tokens.length) :
    emitResume tokens stop cursor groupId size touchEach index output =
      if shouldTag size touchEach (tokens[cursor]'(by omega)) then
        emitResume tokens stop (cursor + 1) groupId size touchEach (index + 1)
          (output ++ [tagged groupId index size (tokens[cursor]'(by omega))])
      else emitResume tokens stop (cursor + 1) groupId size touchEach index
          (output ++ [tokens[cursor]'(by omega)]) := by
  rw [emitResume, rangeTokens_step tokens stop cursor hmore hstop, emitTokens_cons]
  split <;> simp only [emitResume, List.append_assoc, List.singleton_append]

end Verification.MaidataScanGroupsModel
