import LnmaiCore.ChartLoader
import LnmaiCore.Simai.IR
import LnmaiCore.Simai.SlideParser

namespace LnmaiCore.Simai

private def firstDigit? (s : String) : Option Nat :=
  s.toList.findSome? digitToNat?

def lowerSlideToken (noteIndex : Nat) (token : RawNoteToken) : Option (NormalizedSlide × SlideNoteSemantics) :=
  match token.lane with
  | some lane =>
      let endPos := firstDigit? ((token.rawText.drop 1).toString) |>.getD (lane + 1)
      match parseSlideNote token.rawText (lane + 1) endPos with
      | .ok parsed =>
          let shapeKey := shapeKey parsed.shape
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
            , noteIndex := noteIndex
            , simaiRawText := token.rawText
            , simaiShapeKey := shapeKey
            , simaiIsJustRight := parsed.isJustRight }
          some (slide, parsed)
      | .error _ => none
  | none => none

def lowerRawTokens (measureDurSec : Float → Float) (tokens : List RawNoteToken) : NormalizedChart × List SlideNoteSemantics :=
  let (_, taps, holds, touches, touchHolds, slides, slideSemantics) :=
    tokens.foldl
      (fun (state : Nat × List NormalizedTap × List NormalizedHold × List NormalizedTouch × List NormalizedHold × List NormalizedSlide × List SlideNoteSemantics) token =>
        let (noteIndex, taps, holds, touches, touchHolds, slides, slideSemantics) := state
        match token.kind with
        | .tap =>
            match token.lane with
            | some lane =>
                (noteIndex + 1,
                 { timingSec := token.timingSec, lane := lane, isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, isForceStar := token.isForceStar, noteIndex := noteIndex } :: taps,
                 holds, touches, touchHolds, slides, slideSemantics)
            | none => state
        | .hold =>
            match token.lane with
            | some lane =>
                (noteIndex + 1,
                 taps,
                 { timingSec := token.timingSec, lane := lane, lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1)), isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, noteIndex := noteIndex } :: holds,
                 touches, touchHolds, slides, slideSemantics)
            | none => state
        | .touch =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds,
                 { timingSec := token.timingSec, sensorPos := sensorPos, isBreak := token.isBreak, isHanabi := token.isHanabi, noteIndex := noteIndex } :: touches,
                 touchHolds, slides, slideSemantics)
            | none => state
        | .touchHold =>
            match token.sensorPos with
            | some sensorPos =>
                (noteIndex + 1,
                 taps, holds, touches,
                 { timingSec := token.timingSec, lane := sensorPos, lengthSec := token.lengthSec.getD (measureDurSec token.bpm / Float.ofNat (max token.divisor 1)), isBreak := token.isBreak, isEX := token.isEX, isHanabi := token.isHanabi, isTouch := true, noteIndex := noteIndex } :: touchHolds,
                 slides, slideSemantics)
            | none => state
        | .slide =>
            match lowerSlideToken noteIndex token with
            | some (slide, parsed) =>
                (noteIndex + 1, taps, holds, touches, touchHolds, slide :: slides, parsed :: slideSemantics)
            | none => state
        | _ => state)
      (1, [], [], [], [], [], [])
  ({ taps := taps.reverse, holds := holds.reverse, touches := touches.reverse, touchHolds := touchHolds.reverse, slides := slides.reverse, slideSkipping := true }, slideSemantics.reverse)

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
      , isConnSlide := false
      , trackCount := note.trackCount
      , judgeAtSec := note.judgeAtSec
      , isBreak := note.isBreak
      , isEX := note.isEX
      , noteIndex := note.noteIndex
      , judgeQueues := []
      , simaiRawText := some note.simaiRawText
      , simaiShapeKey := some note.simaiShapeKey
      , simaiIsJustRight := some note.simaiIsJustRight })
  , slideSkipping := some chart.slideSkipping }

end LnmaiCore.Simai
