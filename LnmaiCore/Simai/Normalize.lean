import LnmaiCore.ChartLoader
import LnmaiCore.Simai.IR
import LnmaiCore.Simai.SlideParser

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
  let rawQueues := judgeQueuesForShapeKey (shapeKey slide.simaiShape) false |>.getD []
  let withLen := { slide with totalJudgeQueueLen := totalJudgeQueueLen rawQueues }
  let queues :=
    match rawQueues with
    | [queue] => [applySingleTrackConnRulesNormalized withLen queue]
    | _ => rawQueues
  { withLen with judgeQueues := queues, totalJudgeQueueLen := totalJudgeQueueLen queues }

private def slideDebugFor (chart : NormalizedChart) (noteIndex : Nat) : Option NormalizedSlideDebug :=
  chart.slideDebug.find? (fun dbg => dbg.noteIndex = noteIndex)

def lowerSlideToken (noteIndex : Nat) (token : RawNoteToken) : Option (NormalizedSlide × SlideNoteSemantics) :=
  match token.lane with
  | some lane =>
      let endPos := firstDigit? ((token.rawText.drop 1).toString) |>.getD (lane + 1)
      match parseSlideNote token.rawText (lane + 1) endPos with
      | .ok parsed =>
          let isWifi := parsed.shape.kind = SlideKind.wifi
          let lengthSec := token.lengthSec.getD ((60.0 / token.bpm) * 4.0 / Float.ofNat (max token.divisor 1))
          let slide : NormalizedSlide :=
            { timingSec := token.timingSec
            , lane := lane
            , lengthSec := lengthSec
            , startTimingSec := token.timingSec + token.starWaitSec.getD 0.0
            , hSpeed := token.hSpeed
            , slideKind := if isWifi then LnmaiCore.SlideKind.Wifi else LnmaiCore.SlideKind.Single
            , trackCount := if isWifi then 3 else 1
            , judgeAtSec := some (token.timingSec + token.starWaitSec.getD 0.0 + lengthSec)
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
  | none => none

private def applyConnectedSlideMetadata (slides : List NormalizedSlide) : List NormalizedSlide :=
  let rec loop (remaining : List NormalizedSlide) (currentGroup : Option (Nat × Nat)) (parentNoteIndex : Option Nat) (acc : List NormalizedSlide) :=
    match remaining with
    | [] => acc.reverse
    | slide :: rest =>
        match slide.sourceGroupId, slide.sourceGroupIndex, slide.sourceGroupSize with
        | some gid, some idx, some size =>
            let parent :=
              match currentGroup with
              | some (cgid, pidx) => if cgid = gid then some pidx else some slide.noteIndex
              | none => some slide.noteIndex
            let actualParent := if idx = 0 then none else parentNoteIndex
            let isConn := size > 1
            let slideKind :=
              if !isConn then slide.slideKind
              else if slide.slideKind = LnmaiCore.SlideKind.Wifi then LnmaiCore.SlideKind.Wifi else LnmaiCore.SlideKind.ConnPart
            let updated :=
              { slide with
                slideKind := slideKind
                isConnSlide := isConn
                parentNoteIndex := actualParent
                isGroupHead := idx = 0
                isGroupEnd := idx + 1 = size }
            let nextGroup := if idx + 1 = size then none else some (gid, parent.getD slide.noteIndex)
            let nextParent := if idx + 1 = size then none else parent
            loop rest nextGroup nextParent (updated :: acc)
        | _, _, _ =>
            loop rest none none ({ slide with isConnSlide := false, isGroupHead := false, isGroupEnd := false, parentNoteIndex := none } :: acc)
  loop slides none none []

def lowerRawTokens (measureDurSec : Float → Float) (tokens : List RawNoteToken) : NormalizedChart × List SlideNoteSemantics :=
  let (_, taps, holds, touches, touchHolds, slides, slideDebug, slideSemantics) :=
    tokens.foldl
      (fun (state : Nat × List NormalizedTap × List NormalizedHold × List NormalizedTouch × List NormalizedHold × List NormalizedSlide × List NormalizedSlideDebug × List SlideNoteSemantics) token =>
        let (noteIndex, taps, holds, touches, touchHolds, slides, slideDebug, slideSemantics) := state
        match token.kind with
        | .tap =>
            match token.lane with
            | some lane =>
                (noteIndex + 1,
                 { timingSec := token.timingSec, lane := lane, isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, isForceStar := token.isForceStar, noteIndex := noteIndex } :: taps,
                 holds, touches, touchHolds, slides, slideDebug, slideSemantics)
            | none => state
        | .hold =>
            match token.lane with
            | some lane =>
                (noteIndex + 1,
                 taps,
                 { timingSec := token.timingSec, lane := lane, lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1)), isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, noteIndex := noteIndex } :: holds,
                 touches, touchHolds, slides, slideDebug, slideSemantics)
            | none => state
        | .touch =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds,
                 { timingSec := token.timingSec, sensorPos := sensorPos, isBreak := token.isBreak, isHanabi := token.isHanabi, noteIndex := noteIndex } :: touches,
                 touchHolds, slides, slideDebug, slideSemantics)
            | none => state
        | .touchHold =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds, touches,
                 { timingSec := token.timingSec, lane := sensorPos, lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1)), isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, isTouch := true, noteIndex := noteIndex } :: touchHolds,
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
      { timingSec := note.timingSec, lane := note.lane, isBreak := note.isBreak, isEX := note.isEX, noteIndex := note.noteIndex })
  , holds := chart.holds.map (fun note =>
      { timingSec := note.timingSec, lane := note.lane, lengthSec := note.lengthSec, isBreak := note.isBreak, isEX := note.isEX, isTouch := false, noteIndex := note.noteIndex })
  , touches := chart.touches.map (fun note =>
      { timingSec := note.timingSec, sensorPos := note.sensorPos, isBreak := note.isBreak, noteIndex := note.noteIndex })
  , touchHolds := chart.touchHolds.map (fun note =>
      { timingSec := note.timingSec, lane := note.lane, lengthSec := note.lengthSec, isBreak := note.isBreak, isEX := note.isEX, isTouch := true, noteIndex := note.noteIndex })
  , slides := chart.slides.map (fun note =>
      { timingSec := note.timingSec
      , lane := note.lane
      , lengthSec := note.lengthSec
      , startTimingSec := note.startTimingSec
      , slideKind := note.slideKind
      , isClassic := false
      , isConnSlide := note.isConnSlide
      , parentNoteIndex := note.parentNoteIndex
      , isGroupHead := note.isGroupHead
      , isGroupEnd := note.isGroupEnd
      , totalJudgeQueueLen := note.totalJudgeQueueLen
      , trackCount := note.trackCount
      , judgeAtSec := note.judgeAtSec
      , isBreak := note.isBreak
      , isEX := note.isEX
      , noteIndex := note.noteIndex
      , judgeQueues := note.judgeQueues
      , debugSimai := slideDebugFor chart note.noteIndex |>.map (fun dbg =>
          (dbg.rawText, shapeKey note.simaiShape, parseSlideJustText dbg.rawText |>.toOption.getD false)) })
  , slideSkipping := some chart.slideSkipping }

end LnmaiCore.Simai
