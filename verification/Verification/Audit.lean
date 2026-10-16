import Verification.Areas
import Verification.AreasPublic
import Verification.Rotations
import Verification.Time
import Verification.Constants
import Verification.Types
import Verification.TypesExtra
import Verification.TypesPublic
import Verification.Defaults
import Verification.Convert
import Verification.Judge
import Verification.Hold
import Verification.HoldModern
import Verification.AdapterJudgments
import Verification.AdapterScore
import Verification.IntegerAdapters
import Verification.AccuracyAdapters
import Verification.RuntimeScore
import Verification.GradeCounts
import Verification.RuntimeCounts
import Verification.Score
import Verification.ScoreExtra
import Verification.StoragePublic
import Verification.StorageEquality
import Verification.Symmetry
import Verification.ShapeSolver
import Verification.ShapeAdapters
import Verification.Tokenize
import Verification.SlideAreaTrace
import Verification.SlideAreaEquality
import Verification.SlideQueueTrace
import Verification.SlideQueueReferenceTrace
import Lean.Util.CollectAxioms

/- Check transitive theorem dependencies, not merely imports. Aeneas contains
   opaque declarations outside our proofs; a theorem depending on any of them
   (or sorryAx/native_decide) must fail this audit. -/
open Lean Elab Command in
run_cmd do
  let env ← getEnv
  let allowed : Array Name := #[``propext, ``Classical.choice, ``Quot.sound]
  let required : Array Name := #[
    ``Verification.Areas.sensorArea_default_equiv,
    ``Verification.Areas.sensorArea_all_equiv,
    ``Verification.Areas.sensorArea_clone_equiv,
    ``Verification.Areas.sensorArea_eq_equiv,
    ``Verification.Areas.sensorArea_cmp_equiv,
    ``Verification.Areas.sensorArea_partial_cmp_equiv,
    ``Verification.Areas.buttonZone_default_equiv,
    ``Verification.Areas.buttonZone_all_equiv,
    ``Verification.Areas.buttonZone_clone_equiv,
    ``Verification.Areas.buttonZone_eq_equiv,
    ``Verification.Areas.buttonZone_cmp_equiv,
    ``Verification.Areas.buttonZone_partial_cmp_equiv,
    ``Verification.Areas.buttonZone_storage_order_equiv,
    ``Verification.Areas.outerSlot_default_equiv,
    ``Verification.Areas.outerSlot_all_equiv,
    ``Verification.Areas.outerSlot_clone_equiv,
    ``Verification.Areas.outerSlot_eq_equiv,
    ``Verification.Areas.outerSlot_cmp_equiv,
    ``Verification.Areas.outerSlot_partial_cmp_equiv,
    ``Verification.Areas.outerSlot_storage_order_equiv,
    ``Verification.Areas.sensorArea_rotate_unbounded_equiv,
    ``Verification.Areas.buttonZone_rotate_unbounded_equiv,
    ``Verification.Areas.outerSlot_rotate_unbounded_equiv,
    ``Verification.Areas.buttonZone_of_unbounded_index_equiv,
    ``Verification.Areas.outerSlot_of_unbounded_index_equiv,
    ``Verification.TypesProof.judgeGrade_default_equiv,
    ``Verification.TypesProof.judgeGrade_clone_equiv,
    ``Verification.TypesProof.judgeGrade_eq_equiv,
    ``Verification.TypesProof.judgeGrade_cmp_equiv,
    ``Verification.TypesProof.judgeGrade_partial_cmp_equiv,
    ``Verification.TypesProof.displayOption_default_equiv,
    ``Verification.TypesProof.displayOption_clone_equiv,
    ``Verification.TypesProof.displayOption_eq_equiv,
    ``Verification.TypesProof.noteType_default_equiv,
    ``Verification.TypesProof.noteType_clone_equiv,
    ``Verification.TypesProof.noteType_eq_equiv,
    ``Verification.TypesProof.slideKind_default_equiv,
    ``Verification.TypesProof.slideKind_clone_equiv,
    ``Verification.TypesProof.slideKind_eq_equiv,
    ``Verification.TypesProof.areaPolicy_default_equiv,
    ``Verification.TypesProof.areaPolicy_clone_equiv,
    ``Verification.TypesProof.areaPolicy_eq_equiv,
    ``Verification.TypesProof.judgeStyle_default_equiv,
    ``Verification.TypesProof.judgeStyle_clone_equiv,
    ``Verification.TypesProof.judgeStyle_eq_equiv,
    ``Verification.TypesProof.noteStatus_default_equiv,
    ``Verification.TypesProof.noteStatus_clone_equiv,
    ``Verification.TypesProof.noteStatus_eq_equiv,
    ``Verification.TypesProof.comboState_default_equiv,
    ``Verification.TypesProof.comboState_clone_equiv,
    ``Verification.TypesProof.comboState_eq_equiv,
    ``Verification.TypesProof.comboState_cmp_equiv,
    ``Verification.TypesProof.comboState_partial_cmp_equiv,
    ``Verification.TypesProof.runtimePos_default_equiv,
    ``Verification.TypesProof.runtimePos_clone_equiv,
    ``Verification.TypesProof.runtimePos_eq_equiv,
    ``Verification.TypesProof.judgeEventKind_default_equiv,
    ``Verification.TypesProof.judgeEventKind_clone_equiv,
    ``Verification.TypesProof.judgeEventKind_eq_equiv,
    ``Verification.TypesProof.judgePhase_default_equiv,
    ``Verification.TypesProof.judgePhase_clone_equiv,
    ``Verification.TypesProof.judgePhase_eq_equiv,
    ``Verification.DefaultsProof.duration_default_equiv,
    ``Verification.DefaultsProof.time_point_default_equiv,
    ``Verification.DefaultsProof.note_judge_result_default_equiv,
    ``Verification.DefaultsProof.group_state_default_equiv,
    ``Verification.DefaultsProof.judge_event_default_equiv,
    ``Verification.DefaultsProof.combo_delta_default_equiv,
    ``Verification.SlideAreaProof.default_equiv,
    ``Verification.SlideAreaProof.clone_equiv,
    ``Verification.SlideAreaProof.eq_equiv,
    ``Verification.SlideAreaProof.on_equiv,
    ``Verification.SlideAreaProof.is_finished_equiv,
    ``Verification.SlideAreaProof.check_equiv,
    ``Verification.SlideAreaProof.check_frames_equiv,
    ``Verification.SlideAreaProof.check_frames_observations_equiv,
    ``Verification.SlideQueueProof.advance_model_equiv,
    ``Verification.SlideQueueProof.advance_model_bounds,
    ``Verification.SlideQueueProof.advance_model_no_commands,
    ``Verification.SlideQueueProof.advance_fuel_equiv,
    ``Verification.SlideQueueProof.advance_equiv,
    ``Verification.SlideQueueProof.advance_render_equiv,
    ``Verification.SlideQueueProof.advance_replay_equiv,
    ``Verification.SlideQueueProof.advance_frames_equiv,
    ``Verification.SlideQueueProof.advance_output_bound,
    ``Verification.SlideQueueProof.trace_frames_equiv,
    ``Verification.SlideQueueProof.trace_frames_replay_equiv,
    ``Verification.ShapeProof.default_equiv,
    ``Verification.ShapeProof.kind_default_equiv,
    ``Verification.ShapeProof.canonical_default_equiv,
    ``Verification.ShapeProof.body_kind_default_equiv,
    ``Verification.ShapeProof.shape_eq_equiv,
    ``Verification.ShapeProof.mk_slide_shape_equiv,
    ``Verification.ShapeProof.mk_canonical_slide_shape_unchecked_equiv,
    ``Verification.ShapeProof.mk_canonical_slide_shape_opt_equiv,
    ``Verification.ShapeProof.digit_to_nat_equiv,
    ``Verification.ShapeProof.mirror_key_equiv,
    ``Verification.ShapeProof.canonical_rel_end_equiv,
    ``Verification.ShapeProof.base_rel_end_equiv,
    ``Verification.ShapeProof.canonical_opt_unbounded_equiv,
    ``Verification.ShapeProof.canonical_unchecked_unbounded_equiv,
    ``Verification.ShapeProof.canonical_rel_end_unbounded_equiv,
    ``Verification.ShapeProof.base_rel_end_unbounded_equiv,
    ``Verification.ShapeProof.relative_end_from_typed_equiv,
    ``Verification.ShapeProof.solve_shape_equiv,
    ``Verification.TokenizeProof.is_touch_area_char_equiv,
    ``Verification.TokenizeProof.is_slide_mark_char_equiv,
    ``Verification.TokenizeProof.split_top_level_loop_equiv,
    ``Verification.TokenizeProof.split_top_level_equiv,
    ``Verification.SymmetryProof.direct_equiv,
    ``Verification.SymmetryProof.mirror_equiv,
    ``Verification.SymmetryProof.clone_equiv,
    ``Verification.SymmetryProof.is_mirrored_equiv,
    ``Verification.SymmetryProof.rotation_steps_equiv,
    ``Verification.SymmetryProof.partial_eq_equiv,
    ``Verification.SymmetryProof.act_on_sensor_area_equiv,
    ``Verification.Areas.sensorArea_rotate_equiv,
    ``Verification.Areas.buttonZone_rotate_equiv,
    ``Verification.Areas.outerSlot_rotate_equiv,
    ``Verification.TimeProof.duration_add_equiv,
    ``Verification.TimeProof.duration_sub_equiv,
    ``Verification.TimeProof.time_point_sub_equiv,
    ``Verification.ConvertProof.convert_grade_equiv,
    ``Verification.TypesProof.noteStatus_leq_equiv,
    ``Verification.TypesProof.noteJudgeResult_is_fast_equiv,
    ``Verification.TypesProof.noteJudgeResult_is_miss_or_too_fast_equiv,
    ``Verification.TypesProof.scoreState_default_equiv,
    ``Verification.JudgeProof.judge_tap_equiv,
    ``Verification.JudgeProof.judge_touch_equiv,
    ``Verification.JudgeProof.judge_slide_classic_equiv,
    ``Verification.JudgeProof.judge_slide_modern_equiv,
    ``Verification.JudgeProof.judge_hold_classic_end_equiv,
    ``Verification.JudgeProof.judge_hold_end_equiv,
    ``Verification.Adapters.tap_adapter_equiv,
    ``Verification.Adapters.touch_adapter_equiv,
    ``Verification.Adapters.slide_classic_adapter_equiv,
    ``Verification.Adapters.slide_modern_adapter_equiv,
    ``Verification.Adapters.classic_hold_bounded,
    ``Verification.Adapters.classic_hold_adapter_equiv,
    ``Verification.Adapters.modern_hold_adapter_equiv,
    ``Verification.Adapters.count_fast_late_adapter_equiv,
    ``Verification.Adapters.non_break_coefficient_adapter,
    ``Verification.Adapters.break_coefficient_adapter,
    ``Verification.Adapters.combo_coefficient_adapter,
    ``Verification.IntegerProof.div_euclid_equiv,
    ``Verification.AccuracyProof.compute_acc_rates_equiv,
    ``Verification.RuntimeScoreProof.non_break_adapter_equiv,
    ``Verification.RuntimeScoreProof.break_adapter_equiv,
    ``Verification.RuntimeScoreProof.combo_adapter_equiv,
    ``Verification.RuntimeScoreProof.event_note_type_equiv,
    ``Verification.RuntimeScoreProof.event_deltas_adapter_equiv,
    ``Verification.RuntimeScoreProof.fast_late_adapter_equiv,
    ``Verification.RuntimeScoreProof.fold_event_adapter_equiv,
    ``Verification.RuntimeScoreProof.fold_events_adapter_equiv,
    ``Verification.RuntimeScoreProof.score_trace_adapter_equiv,
    ``Verification.GradeCountsProof.grade_index_equiv,
    ``Verification.GradeCountsProof.grades_equiv,
    ``Verification.GradeCountsProof.get_count_equiv,
    ``Verification.GradeCountsProof.set_count_equiv,
    ``Verification.GradeCountsProof.decode_set,
    ``Verification.GradeCountsProof.bump_equiv,
    ``Verification.RuntimeCountsProof.decode_select,
    ``Verification.RuntimeCountsProof.decode_replace,
    ``Verification.RuntimeCountsProof.route_equiv,
    ``Verification.RuntimeCountsProof.update_tables_equiv,
    ``Verification.ScoreProof.score_non_break_equiv,
    ``Verification.ScoreProof.score_break_equiv,
    ``Verification.ScoreProof.count_fast_late_equiv,
    ``Verification.ScoreProof.update_combo_equiv,
    ``Verification.ScoreProof.dx_score_rank_equiv,
    ``Verification.StorageProof.sensor_area_to_storage_index_equiv,
    ``Verification.StorageProof.sensor_area_of_storage_index_equiv,
    ``Verification.StorageProof.button_storage_order_equiv,
    ``Verification.StorageProof.sensor_storage_order_equiv,
    ``Verification.StorageProof.button_default_equiv,
    ``Verification.StorageProof.sensor_default_equiv,
    ``Verification.StorageProof.button_replicate_equiv,
    ``Verification.StorageProof.sensor_replicate_equiv,
    ``Verification.StorageProof.button_get_equiv,
    ``Verification.StorageProof.sensor_get_equiv,
    ``Verification.StorageProof.button_set_equiv,
    ``Verification.StorageProof.sensor_set_equiv,
    ``Verification.StorageProof.button_to_list_equiv,
    ``Verification.StorageProof.sensor_to_list_equiv,
    ``Verification.StorageProof.button_entries_equiv,
    ``Verification.StorageProof.sensor_entries_equiv,
    ``Verification.StorageProof.button_of_fn_equiv,
    ``Verification.StorageProof.sensor_of_fn_equiv,
    ``Verification.StorageProof.button_map_accum_equiv,
    ``Verification.StorageProof.sensor_map_accum_equiv,
    ``Verification.StorageProof.button_clone_equiv,
    ``Verification.StorageProof.sensor_clone_equiv,
    ``Verification.StorageProof.button_eq_equiv,
    ``Verification.StorageProof.sensor_eq_equiv,
    ``Verification.StorageProof.button_default_bridge,
    ``Verification.StorageProof.sensor_default_bridge]
  for declName in required do
    unless (env.find? declName).any (·.isTheorem) do
      throwError "Required equivalence theorem is missing: {declName}"
  let count ← env.constants.toList.foldlM (fun (count : Nat) entry => do
    let declName := entry.1
    let declInfo := entry.2
    let belongs := (`Verification).isPrefixOf declName ||
      (declName.toString.startsWith "_private.Verification.")
    if belongs && declInfo.isTheorem then
      let axioms ← Lean.collectAxioms declName
      let unexpected := axioms.filter fun axiomName => !allowed.contains axiomName
      unless unexpected.isEmpty do
        throwError "{declName} depends on unapproved axioms: {unexpected}"
      pure (count + 1)
    else pure count) 0
  if count == 0 then
    throwError "No equivalence theorems were audited"
  logInfo m!"Audited {count} theorems: only propext, Classical.choice and Quot.sound"
