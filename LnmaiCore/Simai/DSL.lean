import Lean
import LnmaiCore.Simai.Frontend

open Lean Elab Term

namespace LnmaiCore.Simai

syntax "simai_chart!" str : term
syntax "simai_chart_at!" num str : term
syntax "simai_chart_file!" str : term
syntax "simai_chart_file_at!" num str : term
syntax "simai_semantic_chart!" str : term
syntax "simai_semantic_chart_at!" num str : term
syntax "simai_semantic_chart_file!" str : term
syntax "simai_semantic_chart_file_at!" num str : term
syntax "simai_inspection_chart!" str : term
syntax "simai_inspection_chart_at!" num str : term
syntax "simai_inspection_chart_file!" str : term
syntax "simai_inspection_chart_file_at!" num str : term
syntax "simai_normalized_chart!" str : term
syntax "simai_normalized_chart_at!" num str : term
syntax "simai_normalized_chart_file!" str : term
syntax "simai_normalized_chart_file_at!" num str : term
syntax "simai_note_ir!" str : term
syntax "simai_slide_ir!" str : term
syntax "simai_normalized_slide_ir!" str : term
syntax "simai_lowered_slide_ir!" str : term
syntax "simai_lowered_slide_split_ir!" str : term
syntax "simai_lowered_chart!" str : term
syntax "simai_lowered_chart_at!" num str : term
syntax "simai_lowered_chart_file!" str : term
syntax "simai_lowered_chart_file_at!" num str : term
syntax "simai_note!" str : term
syntax "simai_slide!" str : term
syntax "simai_normalized_slide!" str : term

private def getStringLiteral? (stx : Syntax) : Option String :=
  stx.isStrLit?

private def throwDslError (kind : String) (input : String) (err : ParseError) : TermElabM α :=
  throwError m!"{kind} parse failed for {repr input}: {err.message}"

private def getNatLiteral? (stx : Syntax) : Option Nat :=
  stx.isNatLit?

private def validateChartLiteral (kind : String) (levelIndex : Nat) (content : String) : TermElabM Unit := do
  match parseFrontendChartResult content levelIndex with
  | .ok _ => pure ()
  | .error err => throwDslError kind content err

private def readChartFile (path : String) : TermElabM String := do
  let content ← (IO.FS.readFile path : IO String)
  pure content

private def singleNoteMaidata (noteText : String) : String :=
  s!"&first=0\n&inote_1=\n(120)\n{noteText},\n"

structure RawNoteTokenIr where
  rawText : String
  kind : RawNoteKind
  timingMicros : ℤ
  slot : Option OuterSlot
  sensorPos : Option SensorArea
  lengthMicros : Option ℤ
  starWaitMicros : Option ℤ
  sourceGroupId : Option Nat
  sourceGroupIndex : Option Nat
  sourceGroupSize : Option Nat
  isSlideNoHead : Bool
deriving Repr

structure SlideNoteIr where
  rawText : String
  startSlot : OuterSlot
  endArea : SensorArea
  kind : SlideKind
  canonical : String
  mirrored : Bool
deriving Repr

structure SlideAreaIr where
  targetAreas : List SensorArea
  policy : AreaPolicy
  isLast : Bool
  isSkippable : Bool
  arrowProgressWhenOn : Nat
  arrowProgressWhenFinished : Nat
deriving Repr

structure NormalizedSlideIr where
  logicalNoteIndex : Nat
  headRuntimeNoteIndex : Option Nat
  bodyRuntimeNoteIndex : Option Nat
  primaryRuntimeNoteIndex : Nat
  noteIndex : Nat
  headTimingMicros : ℤ
  slot : OuterSlot
  startTimingMicros : ℤ
  lengthMicros : ℤ
  slideKind : LnmaiCore.SlideKind
  trackCount : Nat
  judgeAtMicros : Option ℤ
  hasHeadNote : Bool
  hasBody : Bool
  isSlideNoHead : Bool
  isConnSlide : Bool
  isGroupHead : Bool
  isGroupEnd : Bool
  totalJudgeQueueLen : Nat
  judgeQueues : List (List SlideAreaIr)
  parentNoteIndex : Option Nat
deriving Repr

structure NormalizedSlideRuntimeIds where
  logicalNoteIndex : Nat
  headRuntimeNoteIndex : Option Nat
  bodyRuntimeNoteIndex : Option Nat
  primaryRuntimeNoteIndex : Nat
deriving Inhabited, Repr

def NormalizedSlideIr.runtimeIds (slide : NormalizedSlideIr) : NormalizedSlideRuntimeIds :=
  { logicalNoteIndex := slide.logicalNoteIndex
  , headRuntimeNoteIndex := slide.headRuntimeNoteIndex
  , bodyRuntimeNoteIndex := slide.bodyRuntimeNoteIndex
  , primaryRuntimeNoteIndex := slide.primaryRuntimeNoteIndex }

def NormalizedSlideRuntimeIds.hasSeparateHeadAndBodyRuntimeIds
    (ids : NormalizedSlideRuntimeIds) : Bool :=
  ids.headRuntimeNoteIndex.isSome && ids.bodyRuntimeNoteIndex.isSome

def NormalizedSlideRuntimeIds.isBodyOnlyRuntimeIdShape
    (ids : NormalizedSlideRuntimeIds) : Bool :=
  ids.headRuntimeNoteIndex.isNone && ids.bodyRuntimeNoteIndex.isSome

structure LoweredSlideIr where
  logicalNoteIndex : Nat
  runtimeNoteIndex : Nat
  logicalSlideId : Nat
  noteIndex : Nat
  headTimingMicros : ℤ
  slot : OuterSlot
  startTimingMicros : ℤ
  lengthMicros : ℤ
  slideKind : LnmaiCore.SlideKind
  judgeAtMicros : Option ℤ
  isSlideNoHead : Bool
  isConnSlide : Bool
  isGroupHead : Bool
  isGroupEnd : Bool
  totalJudgeQueueLen : Nat
  judgeQueues : List (List SlideAreaIr)
  parentNoteIndex : Option Nat
deriving Repr

structure LoweredSlideHeadIr where
  logicalNoteIndex : Nat
  runtimeNoteIndex : Nat
  logicalSlideId : Nat
  noteIndex : Nat
  timingMicros : ℤ
  slot : OuterSlot
  isBreak : Bool
  isEX : Bool
deriving Inhabited, Repr

structure LoweredSlideSplitIr where
  slideHeads : List LoweredSlideHeadIr
  slideBodies : List LoweredSlideIr
deriving Inhabited, Repr

private def slideAreaIrOf (area : SlideAreaSpec) : SlideAreaIr :=
  { targetAreas := area.targetAreas
  , policy := area.policy
  , isLast := area.isLast
  , isSkippable := area.isSkippable
  , arrowProgressWhenOn := area.arrowProgressWhenOn
  , arrowProgressWhenFinished := area.arrowProgressWhenFinished }

private def findLoweredSlideHeadRuntimeNoteIndex
    (slideHeads : List ChartLoader.SlideHeadChartNote) (logicalSlideId : Nat) : Option Nat :=
  slideHeads.find? (fun head => head.logicalSlideId = logicalSlideId) |>.map (fun head => head.noteIndex)

private def findLoweredSlideBodyRuntimeNoteIndex
    (slides : List ChartLoader.SlideChartNote) (logicalSlideId : Nat) : Option Nat :=
  slides.find? (fun slide => slide.logicalSlideId = logicalSlideId) |>.map (fun slide => slide.noteIndex)

def noteIrLiteral (noteText : String) : List RawNoteTokenIr :=
  match parseFrontendChartResult (singleNoteMaidata noteText) 1 with
  | .ok chart =>
      chart.inspection.tokens.map (fun tok =>
        { rawText := tok.rawText
        , kind := tok.kind
        , timingMicros := tok.timing.toMicros
        , slot := tok.slot
        , sensorPos := tok.sensorPos
        , lengthMicros := tok.length.map Duration.toMicros
        , starWaitMicros := tok.starWait.map Duration.toMicros
        , sourceGroupId := tok.sourceGroupId
        , sourceGroupIndex := tok.sourceGroupIndex
        , sourceGroupSize := tok.sourceGroupSize
        , isSlideNoHead := tok.isSlideNoHead })
  | .error err => panic! s!"invalid simai note IR literal: {err.message}"

def slideIrLiteral (noteText : String) : List SlideNoteIr :=
  match parseFrontendChartResult (singleNoteMaidata noteText) 1 with
  | .ok chart =>
      chart.inspection.slideNotes.map (fun slide =>
        { rawText := slide.rawText
        , startSlot := slide.startSlot
        , endArea := slide.endArea
        , kind := slide.shape.kind
        , canonical := canonicalShapeKey slide.shape
        , mirrored := slide.shape.mirrored })
  | .error err => panic! s!"invalid simai slide IR literal: {err.message}"

def normalizedSlideIrLiteral (noteText : String) : List NormalizedSlideIr :=
  match parseFrontendChartResult (singleNoteMaidata noteText) 1 with
  | .ok chart =>
      chart.semantic.normalized.slides.map (fun slide =>
        let headRuntimeNoteIndex :=
          findLoweredSlideHeadRuntimeNoteIndex chart.semantic.lowered.slideHeads slide.noteIndex
        let bodyRuntimeNoteIndex :=
          findLoweredSlideBodyRuntimeNoteIndex chart.semantic.lowered.slides slide.noteIndex
        { logicalNoteIndex := slide.noteIndex
        , headRuntimeNoteIndex := headRuntimeNoteIndex
        , bodyRuntimeNoteIndex := bodyRuntimeNoteIndex
        , primaryRuntimeNoteIndex := bodyRuntimeNoteIndex.getD (headRuntimeNoteIndex.getD slide.noteIndex)
        , noteIndex := slide.noteIndex
        , headTimingMicros := slide.headTiming.toMicros
        , slot := slide.slot
        , startTimingMicros := slide.startTiming.toMicros
        , lengthMicros := slide.length.toMicros
        , slideKind := slide.slideKind
        , trackCount := slide.trackCount
        , judgeAtMicros := slide.judgeAt.map TimePoint.toMicros
        , hasHeadNote := slide.hasHeadNote
        , hasBody := slide.hasBody
        , isSlideNoHead := slide.isSlideNoHead
        , isConnSlide := slide.isConnSlide
        , isGroupHead := slide.isGroupHead
        , isGroupEnd := slide.isGroupEnd
        , totalJudgeQueueLen := slide.totalJudgeQueueLen
        , judgeQueues := slide.judgeQueues.map (fun queue => queue.map slideAreaIrOf)
        , parentNoteIndex := slide.parentNoteIndex })
  | .error err => panic! s!"invalid simai normalized slide IR literal: {err.message}"

def loweredSlideIrLiteral (noteText : String) : List LoweredSlideIr :=
  match parseFrontendChartResult (singleNoteMaidata noteText) 1 with
  | .ok chart =>
      chart.semantic.lowered.slides.map (fun slide =>
        { logicalNoteIndex := slide.logicalSlideId
        , runtimeNoteIndex := slide.noteIndex
        , logicalSlideId := slide.logicalSlideId
        , noteIndex := slide.noteIndex
        , headTimingMicros := slide.headTiming.toMicros
        , slot := slide.slot
        , startTimingMicros := slide.startTiming.toMicros
        , lengthMicros := slide.length.toMicros
        , slideKind := slide.slideKind
        , judgeAtMicros := slide.judgeAt.map TimePoint.toMicros
        , isSlideNoHead := slide.isSlideNoHead
        , isConnSlide := slide.isConnSlide
        , isGroupHead := slide.isGroupHead
        , isGroupEnd := slide.isGroupEnd
        , totalJudgeQueueLen := slide.totalJudgeQueueLen
        , judgeQueues := slide.judgeQueues.map (fun queue => queue.map slideAreaIrOf)
        , parentNoteIndex := slide.parentNoteIndex })
  | .error err => panic! s!"invalid simai lowered slide IR literal: {err.message}"

def loweredSlideSplitIrLiteral (noteText : String) : LoweredSlideSplitIr :=
  match parseFrontendChartResult (singleNoteMaidata noteText) 1 with
  | .ok chart =>
      { slideHeads := chart.semantic.lowered.slideHeads.map (fun head =>
          { logicalNoteIndex := head.logicalSlideId
          , runtimeNoteIndex := head.noteIndex
          , logicalSlideId := head.logicalSlideId
          , noteIndex := head.noteIndex
          , timingMicros := head.timing.toMicros
          , slot := head.slot
          , isBreak := head.isBreak
          , isEX := head.isEX })
      , slideBodies := chart.semantic.lowered.slides.map (fun slide =>
          { logicalNoteIndex := slide.logicalSlideId
          , runtimeNoteIndex := slide.noteIndex
          , logicalSlideId := slide.logicalSlideId
          , noteIndex := slide.noteIndex
          , headTimingMicros := slide.headTiming.toMicros
          , slot := slide.slot
          , startTimingMicros := slide.startTiming.toMicros
          , lengthMicros := slide.length.toMicros
          , slideKind := slide.slideKind
          , judgeAtMicros := slide.judgeAt.map TimePoint.toMicros
          , isSlideNoHead := slide.isSlideNoHead
          , isConnSlide := slide.isConnSlide
          , isGroupHead := slide.isGroupHead
          , isGroupEnd := slide.isGroupEnd
          , totalJudgeQueueLen := slide.totalJudgeQueueLen
          , judgeQueues := slide.judgeQueues.map (fun queue => queue.map slideAreaIrOf)
          , parentNoteIndex := slide.parentNoteIndex }) }
  | .error err => panic! s!"invalid simai lowered slide split IR literal: {err.message}"

private def validateChartFileLiteral (kind : String) (levelIndex : Nat) (path : String) : TermElabM String := do
  let content ← readChartFile path
  validateChartLiteral kind levelIndex content
  pure content

elab_rules : term
  | `(simai_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_chart!" 1 content
      let stx ← `((frontendChartLiteral $s : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_chart_at!" levelIndex content
      let stx ← `((frontendChartLiteral $s $n : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_chart_file!" 1 path
      let stx ← `((frontendChartLiteral $(quote content) : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_chart_file_at!" levelIndex path
      let stx ← `((frontendChartLiteral $(quote content) $n : FrontendChartResult))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_semantic_chart!" 1 content
      let stx ← `((frontendSemanticChartLiteral $s : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_semantic_chart_at!" levelIndex content
      let stx ← `((frontendSemanticChartLiteral $s $n : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_semantic_chart_file!" 1 path
      let stx ← `((frontendSemanticChartLiteral $(quote content) : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_semantic_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_semantic_chart_file_at!" levelIndex path
      let stx ← `((frontendSemanticChartLiteral $(quote content) $n : FrontendSemanticChart))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_inspection_chart!" 1 content
      let stx ← `((frontendInspectionChartLiteral $s : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_inspection_chart_at!" levelIndex content
      let stx ← `((frontendInspectionChartLiteral $s $n : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_inspection_chart_file!" 1 path
      let stx ← `((frontendInspectionChartLiteral $(quote content) : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_inspection_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_inspection_chart_file_at!" levelIndex path
      let stx ← `((frontendInspectionChartLiteral $(quote content) $n : FrontendChartInspection))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_normalized_chart!" 1 content
      let stx ← `((frontendNormalizedChartLiteral $s : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_normalized_chart_at!" levelIndex content
      let stx ← `((frontendNormalizedChartLiteral $s $n : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_normalized_chart_file!" 1 path
      let stx ← `((frontendNormalizedChartLiteral $(quote content) : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_normalized_chart_file_at!" levelIndex path
      let stx ← `((frontendNormalizedChartLiteral $(quote content) $n : NormalizedChart))
      elabTerm stx none

elab_rules : term
  | `(simai_note_ir! $s:str) => do
      let some _ := getStringLiteral? s
        | throwUnsupportedSyntax
      let stx ← `((noteIrLiteral $s : List RawNoteTokenIr))
      elabTerm stx none

elab_rules : term
  | `(simai_slide_ir! $s:str) => do
      let some _ := getStringLiteral? s
        | throwUnsupportedSyntax
      let stx ← `((slideIrLiteral $s : List SlideNoteIr))
      elabTerm stx none

elab_rules : term
  | `(simai_normalized_slide_ir! $s:str) => do
      let some _ := getStringLiteral? s
        | throwUnsupportedSyntax
      let stx ← `((normalizedSlideIrLiteral $s : List NormalizedSlideIr))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_slide_ir! $s:str) => do
      let some _ := getStringLiteral? s
        | throwUnsupportedSyntax
      let stx ← `((loweredSlideIrLiteral $s : List LoweredSlideIr))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_slide_split_ir! $s:str) => do
      let some _ := getStringLiteral? s
        | throwUnsupportedSyntax
      let stx ← `((loweredSlideSplitIrLiteral $s : LoweredSlideSplitIr))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_lowered_chart!" 1 content
      let stx ← `((frontendLoweredChartLiteral $s : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      validateChartLiteral "simai_lowered_chart_at!" levelIndex content
      let stx ← `((frontendLoweredChartLiteral $s $n : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart_file! $s:str) => do
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_lowered_chart_file!" 1 path
      let stx ← `((frontendLoweredChartLiteral $(quote content) : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_lowered_chart_file_at! $n:num $s:str) => do
      let some levelIndex := getNatLiteral? n
        | throwUnsupportedSyntax
      let some path := getStringLiteral? s
        | throwUnsupportedSyntax
      let content ← validateChartFileLiteral "simai_lowered_chart_file_at!" levelIndex path
      let stx ← `((frontendLoweredChartLiteral $(quote content) $n : ChartLoader.ChartSpec))
      elabTerm stx none

elab_rules : term
  | `(simai_note! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendSingleToken content with
      | .ok _ =>
          let stx ← `((frontendNoteLiteral $s : RawNoteToken))
          elabTerm stx none
      | .error err => throwDslError "simai_note!" content err

elab_rules : term
  | `(simai_slide! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendSingleSlideNote content with
      | .ok _ =>
          let stx ← `((frontendSlideNoteLiteral $s : SlideNoteSemantics))
          elabTerm stx none
      | .error err => throwDslError "simai_slide!" content err

elab_rules : term
  | `(simai_normalized_slide! $s:str) => do
      let some content := getStringLiteral? s
        | throwUnsupportedSyntax
      match parseFrontendSingleNormalizedSlide content with
      | .ok _ =>
          let stx ← `((frontendNormalizedSlideLiteral $s : NormalizedSlide))
          elabTerm stx none
      | .error err => throwDslError "simai_normalized_slide!" content err

end LnmaiCore.Simai

-- #eval simai_note_ir! "3qq7qq5[192#30:109]"
-- #eval simai_slide_ir! "3qq7qq5[192#30:109]"
-- #eval simai_normalized_slide_ir! "3qq7qq5[192#30:109]"
-- #eval simai_lowered_slide_ir! "3qq7qq5[192#30:109]"
-- #eval simai_lowered_slide_split_ir! "3qq7qq5[192#30:109]"

-- #eval simai_slide_ir! "4<7>6[205.7143#8:13]"
-- #eval simai_normalized_slide_ir! "4<7>6[205.7143#8:13]"
-- #eval simai_normalized_slide_ir! "4<7[205.7143#8:13]"
