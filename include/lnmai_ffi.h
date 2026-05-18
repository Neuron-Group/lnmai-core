#ifndef LNMAI_FFI_H
#define LNMAI_FFI_H

#include <stdint.h>
#include <lean/lean.h>

#ifdef __cplusplus
extern "C" {
#endif

lean_object * initialize_lnmai_x2dcore_LnmaiCore(uint8_t builtin);
lean_object * initialize_lnmai_x2dcore_LnmaiCore_FFI(uint8_t builtin);

lean_object * lnmai_parse_frontend_chart_json(lean_object * content, uint32_t level_index);
lean_object * lnmai_parse_frontend_semantic_chart_json(lean_object * content, uint32_t level_index);
lean_object * lnmai_parse_frontend_inspection_chart_json(lean_object * content, uint32_t level_index);
lean_object * lnmai_parse_normalized_chart_json(lean_object * content, uint32_t level_index);
lean_object * lnmai_parse_lowered_chart_json(lean_object * content, uint32_t level_index);

lean_object * lnmai_build_game_state_json(lean_object * chart_spec_json);
lean_object * lnmai_step_game_state_json(lean_object * state_json, lean_object * batch_json);

lean_object * lnmai_create_empty_session_handle(void);
lean_object * lnmai_load_chart_into_session_from_text(uint64_t handle, lean_object * content, uint32_t level_index);
lean_object * lnmai_load_chart_into_session_from_json(uint64_t handle, lean_object * chart_spec_json);
lean_object * lnmai_unload_chart_from_session(uint64_t handle);
lean_object * lnmai_get_lowered_chart_json_by_handle(uint64_t handle);

lean_object * lnmai_create_game_state_handle(lean_object * chart_spec_json);
lean_object * lnmai_free_game_state_handle(uint64_t handle);
lean_object * lnmai_get_game_state_json_by_handle(uint64_t handle);
lean_object * lnmai_step_game_state_handle(uint64_t handle, lean_object * batch_json);
lean_object * lnmai_step_game_state_handle_light(uint64_t handle, lean_object * batch_json);

static inline uint8_t lnmai_initialize_runtime(void) {
  lean_initialize();
  lean_object * result = initialize_lnmai_x2dcore_LnmaiCore(1);
  if (lean_io_result_is_error(result)) {
    lean_io_result_show_error(result);
    lean_dec_ref(result);
    return 0;
  }
  lean_dec_ref(result);
  return 1;
}

static inline lean_object * lnmai_mk_string(char const * s) {
  return lean_mk_string(s);
}

static inline char const * lnmai_string_cstr(lean_object * s) {
  return lean_string_cstr(s);
}

static inline void lnmai_dec_result(lean_object * o) {
  lean_dec_ref(o);
}

#ifdef __cplusplus
}
#endif

#endif
