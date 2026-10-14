import LnmaiCore.ChartLoader

namespace LnmaiCore

namespace RuntimeSeek

open ChartLoader InputModel

def slideRoot (slides : List SlideChartNote) (noteIndex : Nat) : Nat :=
  let rec walk (fuel : Nat) (id : Nat) : Nat :=
    match fuel with
    | 0 => id
    | fuel + 1 =>
      match slides.find? (fun note => note.noteIndex == id) with
      | some note => match note.parentNoteIndex with
        | some parent => walk fuel parent
        | none => id
      | none => id
  walk slides.length noteIndex

def noteIndices (chart : ChartSpec) : List Nat :=
  chart.taps.map (·.noteIndex) ++ chart.holds.map (·.noteIndex) ++
  chart.touches.map (·.noteIndex) ++ chart.touchHolds.map (·.noteIndex) ++
  chart.slideHeads.map (·.noteIndex) ++ chart.slides.map (·.noteIndex)

/-- Practice seeks exclude entire started notes/chains, never manufacture misses. -/
def chartFrom (chart : ChartSpec) (target : TimePoint) : ChartSpec :=
  let startedRoots := (chart.slides.filter (fun note => note.headTiming < target)).map
    (fun note => slideRoot chart.slides note.noteIndex)
  let slides := chart.slides.filter (fun note =>
    !(startedRoots.contains (slideRoot chart.slides note.noteIndex)))
  let slideHeads := chart.slideHeads.filter (fun head =>
    head.timing >= target &&
    !(chart.slides.any (fun note => note.logicalSlideId == head.logicalSlideId &&
      startedRoots.contains (slideRoot chart.slides note.noteIndex))))
  { chart with
    taps := chart.taps.filter (fun note => note.timing >= target)
    holds := chart.holds.filter (fun note => note.timing >= target)
    touches := chart.touches.filter (fun note => note.timing >= target)
    touchHolds := chart.touchHolds.filter (fun note => note.timing >= target)
    slideHeads := slideHeads
    slides := slides }

def rebuild (chart : ChartSpec) (previous : GameState) (target : TimePoint) : GameState :=
  { ChartLoader.buildGameState (chartFrom chart target) with
    currentTime := target
    currentBatch := { currentTime := target }
    judgeStyle := previous.judgeStyle
    touchPanelOffset := previous.touchPanelOffset
    subdivideSlideJudgeGrade := previous.subdivideSlideJudgeGrade
    noteFastLateDisplay := previous.noteFastLateDisplay
    breakFastLateDisplay := previous.breakFastLateDisplay
    displayHoldHeadJudgeResult := previous.displayHoldHeadJudgeResult
    buttonRingForTouch := previous.buttonRingForTouch }

end RuntimeSeek
end LnmaiCore
