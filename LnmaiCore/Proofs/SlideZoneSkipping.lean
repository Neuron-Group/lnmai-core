/-
  Proofs for the slide zone-skipping narrative.

  This file keeps the statement small: a proposition for the short connected
  slide rule, plus local queue propositions for the actual zone-skipping
  behavior when the front segment is off, skippable, or already on.
-/

import LnmaiCore.ChartLoader

namespace LnmaiCore.Proofs.SlideZoneSkipping

open LnmaiCore.Lifecycle

def zoneSkipFront (first second : SlideArea) (rest : SlideQueue) (sensorHeld : List Bool) : SlideQueue :=
  let first' := first.check sensorHeld
  if first'.isSkippable || first'.on then
    let second' := second.check sensorHeld
    if second'.on then
      second' :: rest
    else
      first' :: second' :: rest
  else
    first' :: second :: rest

theorem zoneSkipFront_front_off_keeps_second_unchecked
    (first second : SlideArea) (rest : SlideQueue) (sensorHeld : List Bool)
    (hFirstOn : (first.check sensorHeld).on = false)
    (hFirstSkippable : (first.check sensorHeld).isSkippable = false) :
    zoneSkipFront first second rest sensorHeld = (first.check sensorHeld) :: second :: rest := by
  simp [zoneSkipFront, hFirstOn, hFirstSkippable]

theorem zoneSkipFront_skippable_front_checks_second
    (first second : SlideArea) (rest : SlideQueue) (sensorHeld : List Bool)
    (hFirstSkippable : (first.check sensorHeld).isSkippable = true)
    (hSecondOn : (second.check sensorHeld).on = false) :
    zoneSkipFront first second rest sensorHeld =
      (first.check sensorHeld) :: (second.check sensorHeld) :: rest := by
  simp [zoneSkipFront, hFirstSkippable, hSecondOn]

theorem zoneSkipFront_skippable_front_can_skip_immediately
    (first second : SlideArea) (rest : SlideQueue) (sensorHeld : List Bool)
    (hFirstSkippable : (first.check sensorHeld).isSkippable = true)
    (hSecondOn : (second.check sensorHeld).on = true) :
    zoneSkipFront first second rest sensorHeld = (second.check sensorHeld) :: rest := by
  simp [zoneSkipFront, hFirstSkippable, hSecondOn]

theorem zoneSkipFront_front_on_can_skip_to_second
    (first second : SlideArea) (rest : SlideQueue) (sensorHeld : List Bool)
    (hFirstOn : (first.check sensorHeld).on = true)
    (hSecondOn : (second.check sensorHeld).on = true) :
    zoneSkipFront first second rest sensorHeld = (second.check sensorHeld) :: rest := by
  simp [zoneSkipFront, hFirstOn, hSecondOn]

theorem zoneSkipFront_front_on_checks_second_even_if_not_skippable
    (first second : SlideArea) (rest : SlideQueue) (sensorHeld : List Bool)
    (hFirstOn : (first.check sensorHeld).on = true)
    (hSecondOn : (second.check sensorHeld).on = false) :
    zoneSkipFront first second rest sensorHeld =
      (first.check sensorHeld) :: (second.check sensorHeld) :: rest := by
  simp [zoneSkipFront, hFirstOn, hSecondOn]

theorem shortConnSlide_nonhead_disables_front_skip
    (note : ChartLoader.SlideChartNote) (first second : SlideArea) (rest : List SlideArea)
    (hConn : note.isConnSlide = true) (hShort : note.totalJudgeQueueLen < 4)
    (hHead : note.isGroupHead = false) :
    ({ first with isSkippable := note.isGroupHead } ::
      { second with isSkippable := note.isGroupEnd } :: rest).head?.map SlideArea.isSkippable = some false := by
  have _shape := ChartLoader.shortConnSlide_applySingleTrackConnRules note first second rest hConn hShort
  simp [hHead]

theorem shortConnSlide_nonend_disables_second_skip
    (note : ChartLoader.SlideChartNote) (first second : SlideArea) (rest : List SlideArea)
    (hConn : note.isConnSlide = true) (hShort : note.totalJudgeQueueLen < 4)
    (hEnd : note.isGroupEnd = false) :
    (({ first with isSkippable := note.isGroupHead } ::
      { second with isSkippable := note.isGroupEnd } :: rest).drop 1).head?.map SlideArea.isSkippable = some false := by
  have _shape := ChartLoader.shortConnSlide_applySingleTrackConnRules note first second rest hConn hShort
  simp [hEnd]

end LnmaiCore.Proofs.SlideZoneSkipping
