import Proofs.RealChartVerificationKoishiDISCO

open LnmaiCore

namespace Proofs.«RealChartVerification小石DISCO»

-- Both module names refer to the same fixture and regression obligations.
-- Reuse the checked results so the full simulation is evaluated only once.

abbrev checkpointName := RealChartVerificationKoishiDISCO.checkpointName

abbrev checkpointAssetPath := RealChartVerificationKoishiDISCO.checkpointAssetPath

abbrev checkpointLevel := RealChartVerificationKoishiDISCO.checkpointLevel

abbrev checkpointChart := RealChartVerificationKoishiDISCO.checkpointChart

abbrev checkpointResult := RealChartVerificationKoishiDISCO.checkpointResult

abbrev checkpointHoldReleaseResult := RealChartVerificationKoishiDISCO.checkpointHoldReleaseResult

abbrev checkpointOverlapSplitResult := RealChartVerificationKoishiDISCO.checkpointOverlapSplitResult

abbrev checkpointOverlapSplitNonPerfects := RealChartVerificationKoishiDISCO.checkpointOverlapSplitNonPerfects

theorem checkpoint_has_no_missing_notes :
    missingJudgedNoteIndices checkpointResult = [] :=
  RealChartVerificationKoishiDISCO.checkpoint_has_no_missing_notes

theorem checkpoint_default_tactic_does_not_achieve_ap :
    achievesAP checkpointResult = false :=
  RealChartVerificationKoishiDISCO.checkpoint_default_tactic_does_not_achieve_ap

theorem checkpoint_has_non_perfect_notes :
    (checkpointResult.events.filterMap fun evt =>
      if evt.grade = JudgeGrade.Perfect then none else some evt.noteIndex) ≠ [] :=
  RealChartVerificationKoishiDISCO.checkpoint_has_non_perfect_notes

theorem checkpoint_hold_release_pass_has_no_missing_notes :
    missingJudgedNoteIndices checkpointHoldReleaseResult = [] :=
  RealChartVerificationKoishiDISCO.checkpoint_hold_release_pass_has_no_missing_notes

theorem checkpoint_overlap_split_has_no_missing_notes :
    missingJudgedNoteIndices checkpointOverlapSplitResult = [] :=
  RealChartVerificationKoishiDISCO.checkpoint_overlap_split_has_no_missing_notes

theorem checkpoint_overlap_split_achieves_ap :
    achievesAP checkpointOverlapSplitResult = true :=
  RealChartVerificationKoishiDISCO.checkpoint_overlap_split_achieves_ap

theorem checkpoint_overlap_split_has_no_non_perfect_notes :
    checkpointOverlapSplitNonPerfects = [] :=
  RealChartVerificationKoishiDISCO.checkpoint_overlap_split_has_no_non_perfect_notes

end Proofs.«RealChartVerification小石DISCO»
