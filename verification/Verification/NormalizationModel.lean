import LnmaiCore.Simai.Normalize
import Batteries.Tactic.OpenPrivate
import Lean.Elab.Tactic.Omega

/-!
Unelaborated source-model draft for normalization.rs.
No Generated.normalization exists in the evidence used for this delivery.
Theorems here relate a source model/factoring to the original Lean definition;
they are NOT the missing Charon/Aeneas correspondence theorem.
Parser and exact-time functions below are the original Lean functions, not
assumed laws about an arbitrary Rust facade.
-/

namespace Verification.NormalizationModel
open LnmaiCore LnmaiCore.Simai

open private applySingleTrackConnRulesNormalized foldSlideMultiplicity
  applyConnectedSlideMetadata attachJudgeQueues splitSlideMultiplicityUnits
  applyConnectedQueueRules from LnmaiCore.Simai.Normalize

structure RawFlags where
  isBreak : Bool
  isEX : Bool
  isHanabi : Bool
  isSlideNoHead : Bool
  isForceStar : Bool
  isFakeRotate : Bool
  isSlideBreak : Bool

structure TapFlags where
  isBreak : Bool
  isEX : Bool
  isHanabi : Bool
  isForceStar : Bool

structure HoldFlags where
  isBreak : Bool
  isEX : Bool
  isHanabi : Bool

structure TouchFlags where
  isBreak : Bool
  isHanabi : Bool

def flagsFromToken (token : RawNoteToken) : RawFlags :=
  ⟨token.isBreak, token.isEX, token.isHanabi, token.isSlideNoHead,
    token.isForceStar, token.isFakeRotate, token.isSlideBreak⟩

inductive NotePlan (Slot Sensor : Type) where
  | skip
  | tap (slot : Slot) (flags : TapFlags)
  | hold (slot : Slot) (flags : HoldFlags)
  | touch (sensor : Sensor) (flags : TouchFlags)
  | touchHold (sensor : Sensor) (flags : HoldFlags)
  | slide (slot : Slot) (flags : RawFlags)

def planNote {Slot Sensor : Type} (kind : RawNoteKind)
    (slot : Option Slot) (sensor : Option Sensor) (flags : RawFlags) :
    NotePlan Slot Sensor :=
  match kind with
  | .tap => match slot with
      | none => .skip
      | some slot => .tap slot
          ⟨flags.isBreak, flags.isEX, flags.isHanabi, flags.isForceStar⟩
  | .hold => match slot with
      | none => .skip
      | some slot => .hold slot ⟨flags.isBreak, flags.isEX, flags.isHanabi⟩
  | .touch => match sensor with
      | none => .skip
      | some sensor => .touch sensor ⟨flags.isBreak, flags.isHanabi⟩
  | .touchHold => match sensor with
      | none => .skip
      | some sensor => .touchHold sensor ⟨flags.isBreak, flags.isEX, flags.isHanabi⟩
  | .slide => match slot with
      | none => .skip
      | some slot => .slide slot flags
  | .rest | .unknown => .skip

structure SlideLayout where
  flags : RawFlags
  slideKind : LnmaiCore.SlideKind
  trackCount : Nat
  isClassic : Bool
  hasHeadNote : Bool
  hasBody : Bool
  isConnSlide : Bool
  isGroupHead : Bool
  isGroupEnd : Bool
  totalJudgeQueueLen : Nat
  multiple : Nat

def slideLayout (kind : LnmaiCore.Simai.SlideKind) (flags : RawFlags)
    (isClassic : Bool) : SlideLayout :=
  let (slideKind, trackCount) := match kind with
    | .wifi => (LnmaiCore.SlideKind.Wifi, 3)
    | _ => (LnmaiCore.SlideKind.Single, 1)
  { flags, slideKind, trackCount, isClassic
    hasHeadNote := !flags.isSlideNoHead
    hasBody := true
    isConnSlide := false
    isGroupHead := false
    isGroupEnd := false
    totalJudgeQueueLen := 0
    multiple := 1 }

/-- Exact successful record in lowerSlideToken, for arbitrary parsed values. -/
def referenceSlide (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (parsed : SlideNoteSemantics) : NormalizedSlide :=
  let isWifi := parsed.shape.kind = LnmaiCore.Simai.SlideKind.wifi
  let length := token.length.getD (noteTimingIncrement token.bpm (max token.divisor 1))
  let startTiming := token.timing + token.starWait.getD Duration.zero
  let lastWaitRatio := (slideLastWaitRatioForShape parsed.shape isClassic).getD 0
  let judgeDuration :=
    Time.durationFromRatMicros ((length.toMicros : Rat) * (1 - lastWaitRatio))
  { headTiming := token.timing
    slot := slot
    length := length
    startTiming := startTiming
    hSpeed := token.hSpeed
    slideKind := if isWifi then LnmaiCore.SlideKind.Wifi else LnmaiCore.SlideKind.Single
    isClassic := isClassic
    trackCount := if isWifi then 3 else 1
    judgeAt := some (startTiming + judgeDuration)
    isBreak := token.isBreak
    isEX := token.isEX
    isHanabi := token.isHanabi
    hasHeadNote := !token.isSlideNoHead
    hasBody := true
    isSlideNoHead := token.isSlideNoHead
    isForceStar := token.isForceStar
    isFakeRotate := token.isFakeRotate
    isSlideBreak := token.isSlideBreak
    totalJudgeQueueLen := 0
    judgeQueues := []
    sourceGroupId := token.sourceGroupId
    sourceGroupIndex := token.sourceGroupIndex
    sourceGroupSize := token.sourceGroupSize
    multiple := 1
    noteIndex := noteIndex
    simaiShape := parsed.shape }

/-- Field-copy runtime constructor with the selected kernel layout substituted.
The timing expressions are the source model's exact expressions; their Rust
representation laws remain outside this theorem. -/
def buildSlide (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : RawFlags) (parsed : SlideNoteSemantics) : NormalizedSlide :=
  let layout := slideLayout parsed.shape.kind flags isClassic
  let length := token.length.getD (noteTimingIncrement token.bpm (max token.divisor 1))
  let startTiming := token.timing + token.starWait.getD Duration.zero
  let lastWaitRatio := (slideLastWaitRatioForShape parsed.shape isClassic).getD 0
  let judgeDuration :=
    Time.durationFromRatMicros ((length.toMicros : Rat) * (1 - lastWaitRatio))
  { headTiming := token.timing
    slot := slot
    length := length
    startTiming := startTiming
    groupStartTiming := none
    hSpeed := token.hSpeed
    slideKind := layout.slideKind
    isClassic := layout.isClassic
    trackCount := layout.trackCount
    judgeAt := some (startTiming + judgeDuration)
    isBreak := layout.flags.isBreak
    isEX := layout.flags.isEX
    isHanabi := layout.flags.isHanabi
    hasHeadNote := layout.hasHeadNote
    hasBody := layout.hasBody
    isSlideNoHead := layout.flags.isSlideNoHead
    isForceStar := layout.flags.isForceStar
    isFakeRotate := layout.flags.isFakeRotate
    isSlideBreak := layout.flags.isSlideBreak
    isConnSlide := layout.isConnSlide
    parentNoteIndex := none
    isGroupHead := layout.isGroupHead
    isGroupEnd := layout.isGroupEnd
    totalJudgeQueueLen := layout.totalJudgeQueueLen
    judgeQueues := []
    sourceGroupId := token.sourceGroupId
    sourceGroupIndex := token.sourceGroupIndex
    sourceGroupSize := token.sourceGroupSize
    multiple := layout.multiple
    noteIndex := noteIndex
    simaiShape := parsed.shape }

theorem buildSlide_reference (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (parsed : SlideNoteSemantics) :
    buildSlide isClassic noteIndex token slot (flagsFromToken token) parsed =
      referenceSlide isClassic noteIndex token slot parsed := by
  cases hk : parsed.shape.kind <;>
    simp [buildSlide, referenceSlide, slideLayout, flagsFromToken, hk]

/-- Uses only original parser functions. This is a factoring of the Lean
source, not an assumed successful normalization result from a Rust callback. -/
def lowerSlideAtSlot (isClassic : Bool) (noteIndex : Nat) (token : RawNoteToken)
    (slot : OuterSlot) (flags : RawFlags) : Option (NormalizedSlide × SlideNoteSemantics) :=
  match parseTerminalEndArea token.rawText with
  | .error _ => none
  | .ok endArea =>
      match (match token.slideBody with
             | some body => parseSlideNoteFromBody token.rawText body endArea
             | none => parseSlideNote token.rawText slot endArea) with
      | .error _ => none
      | .ok parsed => some (buildSlide isClassic noteIndex token slot flags parsed, parsed)

theorem lowerSlideAtSlot_reference (isClassic : Bool) (noteIndex : Nat)
    (token : RawNoteToken) (slot : OuterSlot) (hslot : token.slot = some slot) :
    lowerSlideAtSlot isClassic noteIndex token slot (flagsFromToken token) =
      lowerSlideToken noteIndex token isClassic := by
  cases hend : parseTerminalEndArea token.rawText with
  | error error =>
      simp only [lowerSlideAtSlot, lowerSlideToken, hslot, hend]
  | ok endArea =>
      cases hbody : token.slideBody with
      | none =>
          cases hparsed : parseSlideNote token.rawText slot endArea <;>
            simp only [lowerSlideAtSlot, lowerSlideToken, hslot, hend, hbody, hparsed,
              buildSlide_reference, referenceSlide]
      | some body =>
          cases hparsed : parseSlideNoteFromBody token.rawText body endArea <;>
            simp only [lowerSlideAtSlot, lowerSlideToken, hslot, hend, hbody, hparsed,
              buildSlide_reference, referenceSlide]

theorem lowerSlideToken_missing_slot (isClassic : Bool) (noteIndex : Nat)
    (token : RawNoteToken) (hslot : token.slot = none) :
    lowerSlideToken noteIndex token isClassic = none := by
  simp [lowerSlideToken, hslot]

/-- This theorem quantifies over all parsed shapes and all seven flags.
No parser validity, endpoint fit, group consistency or positive duration is
needed to select shape kind, count or initial connection flags. -/
theorem slide_initial_flags (kind : LnmaiCore.Simai.SlideKind)
    (flags : RawFlags) (isClassic : Bool) :
    let layout := slideLayout kind flags isClassic
    layout.flags = flags ∧ layout.hasHeadNote = !flags.isSlideNoHead ∧
    layout.hasBody = true ∧ layout.isConnSlide = false ∧
    layout.isGroupHead = false ∧ layout.isGroupEnd = false ∧
    layout.multiple = 1 ∧ layout.totalJudgeQueueLen = 0 := by
  cases kind <;> simp [slideLayout]

def setSkip (value : Bool) (area : SlideAreaSpec) : SlideAreaSpec :=
  { area with isSkippable := value }

def track (isConn short isHead isEnd : Bool) (queue : List SlideAreaSpec) :
    List SlideAreaSpec :=
  if !isConn then queue
  else if short then
    match queue with
    | [] => []
    | [single] => [setSkip (isHead || isEnd) single]
    | first :: second :: rest => setSkip isHead first :: setSkip isEnd second :: rest
  else queue.map (setSkip true)

theorem track_reference (slide : NormalizedSlide) (queue : List SlideAreaSpec) :
    track slide.isConnSlide (decide (slide.totalJudgeQueueLen < 4))
      slide.isGroupHead slide.isGroupEnd queue =
      applySingleTrackConnRulesNormalized slide queue := by
  cases hconn : slide.isConnSlide <;>
    by_cases hshort : slide.totalJudgeQueueLen < 4 <;>
    cases queue with
    | nil =>
        simp [track, applySingleTrackConnRulesNormalized, hconn, hshort, setSkip]
    | cons first rest =>
        cases rest <;>
          simp [track, applySingleTrackConnRulesNormalized, hconn, hshort, setSkip]

theorem track_length (isConn short isHead isEnd : Bool) (queue : List SlideAreaSpec) :
    (track isConn short isHead isEnd queue).length = queue.length := by
  cases isConn <;> cases short <;>
    cases queue with
    | nil => simp [track]
    | cons head tail => cases tail <;> simp [track]

/-- Erasing only the modified field makes preservation of every other field,
the ordering, and duplicate elements explicit. No uniqueness premise. -/
def withoutSkip (area : SlideAreaSpec) : SlideAreaSpec := setSkip false area

theorem track_payload_order (isConn short isHead isEnd : Bool)
    (queue : List SlideAreaSpec) :
    (track isConn short isHead isEnd queue).map withoutSkip =
      queue.map withoutSkip := by
  cases isConn <;> cases short <;>
    cases queue with
    | nil => simp [track]
    | cons head tail =>
        cases tail <;> simp [track, withoutSkip, setSkip, List.map_map, Function.comp_def]

theorem short_track_tail_unchanged (isHead isEnd : Bool)
    (first second : SlideAreaSpec) (rest : List SlideAreaSpec) :
    track true true isHead isEnd (first :: second :: rest) =
      setSkip isHead first :: setSkip isEnd second :: rest := rfl

-- The original fold step and its factoring through NotePlan are appended
-- below. They expose accepted-note indexing and the unchanged postprocessing.

abbrev LowerState := Nat × List NormalizedTap × List NormalizedHold ×
  List NormalizedTouch × List NormalizedTouchHold × List NormalizedSlide ×
  List NormalizedSlideDebug × List SlideNoteSemantics

/-- Verbatim anonymous fold body from the original lowerRawTokensWithMode. -/
def referenceStep (isClassic : Bool) (measureDurSec : Rat → Duration)
    (state : LowerState) (token : RawNoteToken) : LowerState :=
  let (noteIndex, taps, holds, touches, touchHolds, slides, slideDebug, slideSemantics) := state
  match token.kind with
  | .tap =>
      match token.slot with
      | some slot =>
          (noteIndex + 1,
           { timing := token.timing, slot := slot, isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, isForceStar := token.isForceStar, noteIndex := noteIndex } :: taps,
           holds, touches, touchHolds, slides, slideDebug, slideSemantics)
      | none => state
  | .hold =>
      match token.slot with
      | some slot =>
          (noteIndex + 1,
           taps,
           { timing := token.timing, slot := slot, length := token.length.getD (measureDurSec token.bpm), isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, noteIndex := noteIndex } :: holds,
           touches, touchHolds, slides, slideDebug, slideSemantics)
      | none => state
  | .touch =>
      match token.sensorPos with
      | some sensorPos =>
          (noteIndex + 1,
           taps, holds,
           { timing := token.timing
           , sensorPos := sensorPos
           , isBreak := token.isBreak
           , isHanabi := token.isHanabi
           , sourceGroupId := token.sourceGroupId
           , sourceGroupIndex := token.sourceGroupIndex
           , sourceGroupSize := token.sourceGroupSize
           , noteIndex := noteIndex } :: touches,
           touchHolds, slides, slideDebug, slideSemantics)
      | none => state
  | .touchHold =>
      match token.sensorPos with
      | some sensorPos =>
          (noteIndex + 1,
           taps, holds, touches,
           { timing := token.timing
           , sensorPos := sensorPos
           , length := token.length.getD (measureDurSec token.bpm)
           , isBreak := token.isBreak
           , isEX := token.isEX
           , isHanabi := token.isHanabi
           , sourceGroupId := token.sourceGroupId
           , sourceGroupIndex := token.sourceGroupIndex
           , sourceGroupSize := token.sourceGroupSize
           , noteIndex := noteIndex } :: touchHolds,
           slides, slideDebug, slideSemantics)
      | none => state
  | .slide =>
      match lowerSlideToken noteIndex token isClassic with
      | some (slide, parsed) =>
          (noteIndex + 1, taps, holds, touches, touchHolds, slide :: slides, { noteIndex := noteIndex, rawText := token.rawText } :: slideDebug, parsed :: slideSemantics)
      | none => state
  | _ => state

/-- Field-copy interpretation of the plan. The Slide case is still fallible;
no accepted-note counter/debug/semantics entry is emitted on failure. -/
def planStep (isClassic : Bool) (measureDurSec : Rat → Duration)
    (state : LowerState) (token : RawNoteToken) : LowerState :=
  let (noteIndex, taps, holds, touches, touchHolds, slides, slideDebug, slideSemantics) := state
  match planNote token.kind token.slot token.sensorPos (flagsFromToken token) with
  | .skip => state
  | .tap slot flags =>
      (noteIndex + 1,
       { timing := token.timing, slot := slot, isBreak := flags.isBreak,
         isEX := flags.isEX, isHanabi := flags.isHanabi,
         isForceStar := flags.isForceStar, noteIndex := noteIndex } :: taps,
       holds, touches, touchHolds, slides, slideDebug, slideSemantics)
  | .hold slot flags =>
      (noteIndex + 1, taps,
       { timing := token.timing, slot := slot,
         length := token.length.getD (measureDurSec token.bpm),
         isBreak := flags.isBreak, isEX := flags.isEX,
         isHanabi := flags.isHanabi, noteIndex := noteIndex } :: holds,
       touches, touchHolds, slides, slideDebug, slideSemantics)
  | .touch sensorPos flags =>
      (noteIndex + 1, taps, holds,
       { timing := token.timing, sensorPos := sensorPos,
         isBreak := flags.isBreak, isHanabi := flags.isHanabi,
         sourceGroupId := token.sourceGroupId,
         sourceGroupIndex := token.sourceGroupIndex,
         sourceGroupSize := token.sourceGroupSize,
         noteIndex := noteIndex } :: touches,
       touchHolds, slides, slideDebug, slideSemantics)
  | .touchHold sensorPos flags =>
      (noteIndex + 1, taps, holds, touches,
       { timing := token.timing, sensorPos := sensorPos,
         length := token.length.getD (measureDurSec token.bpm),
         isBreak := flags.isBreak, isEX := flags.isEX, isHanabi := flags.isHanabi,
         sourceGroupId := token.sourceGroupId,
         sourceGroupIndex := token.sourceGroupIndex,
         sourceGroupSize := token.sourceGroupSize,
         noteIndex := noteIndex } :: touchHolds,
       slides, slideDebug, slideSemantics)
  | .slide slot flags =>
      match lowerSlideAtSlot isClassic noteIndex token slot flags with
      | none => state
      | some (slide, parsed) =>
          (noteIndex + 1, taps, holds, touches, touchHolds, slide :: slides,
           { noteIndex := noteIndex, rawText := token.rawText } :: slideDebug,
           parsed :: slideSemantics)

/-- Source factoring, with every kind/position combination, including arbitrary
flags and metadata. This does not assume Rust lowerSlideToken equivalence. -/
theorem planStep_reference (isClassic : Bool) (measureDurSec : Rat → Duration)
    (state : LowerState) (token : RawNoteToken) :
    planStep isClassic measureDurSec state token =
      referenceStep isClassic measureDurSec state token := by
  rcases state with ⟨noteIndex, taps, holds, touches, touchHolds, slides, debug, semantics⟩
  cases hk : token.kind
  case slide =>
    cases hs : token.slot with
    | none =>
        have hmissing := lowerSlideToken_missing_slot isClassic noteIndex token hs
        simp only [planStep, referenceStep, planNote, hk, hs, hmissing]
    | some slot =>
        -- Keep flagsFromToken intact until the explicit bridge is applied.
        simp only [planStep, referenceStep, planNote, hk, hs]
        rw [lowerSlideAtSlot_reference isClassic noteIndex token slot hs]
        -- The two source matches list their Option alternatives in reverse order.
        cases lowerSlideToken noteIndex token isClassic with
        | none => rfl
        | some value => cases value; rfl
  all_goals
    cases hs : token.slot <;> cases hp : token.sensorPos <;>
      simp [planStep, referenceStep, planNote, hk, hs, hp, flagsFromToken]

theorem planFold_reference (isClassic : Bool) (measureDurSec : Rat → Duration)
    (tokens : List RawNoteToken) (state : LowerState) :
    tokens.foldl (planStep isClassic measureDurSec) state =
      tokens.foldl (referenceStep isClassic measureDurSec) state := by
  induction tokens generalizing state with
  | nil => rfl
  | cons token rest ih =>
      simp only [List.foldl_cons, planStep_reference, ih]

/-- The original postprocessing. None of these phases is newly verified by
this file. Retaining them exposes the precise factoring boundary. -/
def finish (state : LowerState) : NormalizedChart × List SlideNoteSemantics :=
  let (_, taps, holds, touches, touchHolds, slides, slideDebug, slideSemantics) := state
  let attached := (applyConnectedSlideMetadata (foldSlideMultiplicity slides.reverse)).map attachJudgeQueues
  let loweredSlides := (splitSlideMultiplicityUnits attached).flatMap applyConnectedQueueRules
  ({ taps := taps.reverse, holds := holds.reverse, touches := touches.reverse, touchHolds := touchHolds.reverse, slides := loweredSlides, slideDebug := slideDebug.reverse, slideSkipping := true }, slideSemantics.reverse)

/-- Links the copied referenceStep to the actual original anonymous fold. -/
theorem referenceFold_original (isClassic : Bool) (measureDurSec : Rat → Duration)
    (tokens : List RawNoteToken) :
    finish (tokens.foldl (referenceStep isClassic measureDurSec)
      (1, [], [], [], [], [], [], [])) =
      lowerRawTokensWithMode isClassic measureDurSec tokens := rfl

def lowerWithPlan (isClassic : Bool) (measureDurSec : Rat → Duration)
    (tokens : List RawNoteToken) : NormalizedChart × List SlideNoteSemantics :=
  finish (tokens.foldl (planStep isClassic measureDurSec) (1, [], [], [], [], [], [], []))

/-- Entire original Lean pipeline factored through our source plan. No theorem
here states that Rust's entire parser, token loop or postprocessing is proved. -/
theorem lowerWithPlan_reference (isClassic : Bool) (measureDurSec : Rat → Duration)
    (tokens : List RawNoteToken) :
    lowerWithPlan isClassic measureDurSec tokens =
      lowerRawTokensWithMode isClassic measureDurSec tokens := by
  unfold lowerWithPlan
  rw [planFold_reference]
  exact referenceFold_original isClassic measureDurSec tokens

end Verification.NormalizationModel
