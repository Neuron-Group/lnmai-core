import LnmaiCore.ChartLoader
import LnmaiCore.Simai.IR
import LnmaiCore.Simai.SlideParser
import LnmaiCore.Simai.Timing
import LnmaiCore.Areas
import LnmaiCore.Time

namespace LnmaiCore.Simai

private def firstDigit? (s : String) : Option Nat :=
  s.toList.findSome? digitToNat?

private def totalJudgeQueueLen (queues : List (List SlideAreaSpec)) : Nat :=
  queues.foldl (fun acc queue => Nat.max acc queue.length) 0

private def applySingleTrackConnRulesNormalized (slide : NormalizedSlide) (queue : List SlideAreaSpec) : List SlideAreaSpec :=
  if !slide.isConnSlide then
    queue
  else if slide.totalJudgeQueueLen < 4 then
    match queue with
    | [] => []
    | [single] => [{ single with isSkippable := slide.isGroupHead || slide.isGroupEnd }]
    | first :: second :: rest =>
        { first with isSkippable := slide.isGroupHead } ::
        { second with isSkippable := slide.isGroupEnd } ::
        rest
  else
    queue

private def attachJudgeQueues (slide : NormalizedSlide) : NormalizedSlide :=
  let rawQueues := judgeQueuesForShapeKey (shapeKey slide.simaiShape) slide.isClassic |>.getD []
  let placedQueues := rotateJudgeQueues slide.slot.toIndex rawQueues
  let withLen := { slide with totalJudgeQueueLen := totalJudgeQueueLen placedQueues }
  let queues :=
    match placedQueues with
    | [queue] => [applySingleTrackConnRulesNormalized withLen queue]
    | _ => placedQueues
  { withLen with judgeQueues := queues, totalJudgeQueueLen := totalJudgeQueueLen queues }

private def slideDebugFor (chart : NormalizedChart) (noteIndex : Nat) : Option NormalizedSlideDebug :=
  chart.slideDebug.find? (fun dbg => dbg.noteIndex = noteIndex)

def lowerSlideToken (noteIndex : Nat) (token : RawNoteToken) : Option (NormalizedSlide × SlideNoteSemantics) :=
  match token.slot with
  | some slot =>
      match parseTerminalEndArea token.rawText with
      | .ok endArea =>
          match parseSlideNote token.rawText slot endArea with
          | .ok parsed =>
              let isWifi := parsed.shape.kind = SlideKind.wifi
              let length := token.length.getD (noteTimingIncrement token.bpm (max token.divisor 1))
              let slide : NormalizedSlide :=
                { timing := token.timing
                , slot := slot
                , length := length
                , startTiming := token.timing + token.starWait.getD Duration.zero
                , hSpeed := token.hSpeed
                , slideKind := if isWifi then LnmaiCore.SlideKind.Wifi else LnmaiCore.SlideKind.Single
                , isClassic := false
                , trackCount := if isWifi then 3 else 1
                , judgeAt := some (token.timing + token.starWait.getD Duration.zero + length)
                , isBreak := token.isBreak
                , isEX := token.isEX
                , isHanabi := token.isHanabi
                , isSlideNoHead := token.isSlideNoHead
                , isForceStar := token.isForceStar
                , isFakeRotate := token.isFakeRotate
                , isSlideBreak := token.isSlideBreak
                , totalJudgeQueueLen := 0
                , judgeQueues := []
                , sourceGroupId := token.sourceGroupId
                , sourceGroupIndex := token.sourceGroupIndex
                , sourceGroupSize := token.sourceGroupSize
                , noteIndex := noteIndex
                , simaiShape := parsed.shape }
              some (slide, parsed)
          | .error _ => none
      | .error _ => none
  | none => none

private def applyConnectedSlideMetadata (slides : List NormalizedSlide) : List NormalizedSlide :=
  let rec loop
      (remaining : List NormalizedSlide)
      (currentGroupId : Option Nat)
      (parentNoteIndex : Option Nat)
      (parentEndTiming : Option TimePoint)
      (acc : List NormalizedSlide) :=
    match remaining with
    | [] => acc.reverse
    | slide :: rest =>
        match slide.sourceGroupId, slide.sourceGroupIndex, slide.sourceGroupSize with
        | some gid, some idx, some size =>
            let actualParent := if idx = 0 then none else parentNoteIndex
            let isConn := size > 1
            let slideKind :=
              if !isConn then slide.slideKind
              else if slide.slideKind = LnmaiCore.SlideKind.Wifi then LnmaiCore.SlideKind.Wifi else LnmaiCore.SlideKind.ConnPart
            let startTiming :=
              if idx = 0 then slide.startTiming else parentEndTiming.getD slide.startTiming
            let startShift := startTiming - slide.startTiming
            let judgeAt := slide.judgeAt.map (fun (tp : TimePoint) => tp + startShift)
            let updated :=
              { slide with
                startTiming := startTiming
                judgeAt := judgeAt
                slideKind := slideKind
                isConnSlide := isConn
                parentNoteIndex := actualParent
                isGroupHead := idx = 0
                isGroupEnd := idx + 1 = size }
            let nextParentEndTiming := some (updated.startTiming + updated.length)
            let nextGroupId := if idx + 1 = size then none else some gid
            let nextParent := if idx + 1 = size then none else some updated.noteIndex
            let nextEnd := if idx + 1 = size then none else nextParentEndTiming
            let resetChain := currentGroupId != some gid && idx != 0
            if resetChain then
              loop rest none none none ({ slide with isConnSlide := false, isGroupHead := false, isGroupEnd := false, parentNoteIndex := none } :: acc)
            else
              loop rest nextGroupId nextParent nextEnd (updated :: acc)
        | _, _, _ =>
            loop rest none none none ({ slide with isConnSlide := false, isGroupHead := false, isGroupEnd := false, parentNoteIndex := none } :: acc)
  loop slides none none none []

def lowerRawTokens (measureDurSec : Rat → Duration) (tokens : List RawNoteToken) : NormalizedChart × List SlideNoteSemantics :=
  let (_, taps, holds, touches, touchHolds, slides, slideDebug, slideSemantics) :=
    tokens.foldl
      (fun (state : Nat × List NormalizedTap × List NormalizedHold × List NormalizedTouch × List NormalizedTouchHold × List NormalizedSlide × List NormalizedSlideDebug × List SlideNoteSemantics) token =>
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
                 { timing := token.timing, sensorPos := sensorPos, isBreak := token.isBreak, isHanabi := token.isHanabi, noteIndex := noteIndex } :: touches,
                 touchHolds, slides, slideDebug, slideSemantics)
            | none => state
        | .touchHold =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds, touches,
                 { timing := token.timing, sensorPos := sensorPos, length := token.length.getD (measureDurSec token.bpm), isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, noteIndex := noteIndex } :: touchHolds,
                 slides, slideDebug, slideSemantics)
            | none => state
        | .slide =>
            match lowerSlideToken noteIndex token with
            | some (slide, parsed) =>
                (noteIndex + 1, taps, holds, touches, touchHolds, slide :: slides, { noteIndex := noteIndex, rawText := token.rawText } :: slideDebug, parsed :: slideSemantics)
            | none => state
        | _ => state)
      (1, [], [], [], [], [], [], [])
  let loweredSlides := (applyConnectedSlideMetadata slides.reverse).map attachJudgeQueues
  ({ taps := taps.reverse, holds := holds.reverse, touches := touches.reverse, touchHolds := touchHolds.reverse, slides := loweredSlides, slideDebug := slideDebug.reverse, slideSkipping := true }, slideSemantics.reverse)

def toChartSpec (chart : NormalizedChart) : ChartLoader.ChartSpec :=
  { taps := chart.taps.map (fun note =>
      { timing := note.timing, slot := note.slot, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex })
  , holds := chart.holds.map (fun note =>
      { timing := note.timing, slot := note.slot, length := note.length, isBreak := note.isBreak, isEX := note.isEX, isTouch := false, noteIndex := note.noteIndex })
  , touches := chart.touches.map (fun note =>
      { timing := note.timing, sensorPos := note.sensorPos, isBreak := note.isBreak, noteIndex := note.noteIndex })
  , touchHolds := chart.touchHolds.map (fun note =>
      { timing := note.timing, sensorPos := note.sensorPos, length := note.length, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex })
  , slides := chart.slides.map (fun note =>
      { timing := note.timing
      , slot := note.slot
      , length := note.length
      , startTiming := note.startTiming
      , slideKind := note.slideKind
      , isClassic := note.isClassic
      , isConnSlide := note.isConnSlide
      , parentNoteIndex := note.parentNoteIndex
      , isGroupHead := note.isGroupHead
      , isGroupEnd := note.isGroupEnd
      , totalJudgeQueueLen := note.totalJudgeQueueLen
      , trackCount := note.trackCount
      , judgeAt := note.judgeAt
      , isBreak := note.isBreak
      , isEX := note.isEX
      , noteIndex := note.noteIndex
      , judgeQueues := note.judgeQueues
      , debugSimai := slideDebugFor chart note.noteIndex |>.map (fun dbg =>
          (dbg.rawText, shapeKey note.simaiShape, parseSlideJustText dbg.rawText |>.toOption.getD false)) })
  , slideSkipping := some chart.slideSkipping }

end LnmaiCore.Simai
