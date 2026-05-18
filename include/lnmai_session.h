#ifndef LNMAI_SESSION_H
#define LNMAI_SESSION_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "lnmai_ffi.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  uint64_t raw;
} lnmai_empty_handle;

typedef struct {
  uint64_t raw;
} lnmai_loaded_handle;

static inline void lnmai_session_string_free(char *s) {
  free(s);
}

static inline char * lnmai_session_string_dup(char const *s) {
  size_t len = strlen(s);
  char *copy = (char *)malloc(len + 1);
  if (!copy) {
    return NULL;
  }
  memcpy(copy, s, len + 1);
  return copy;
}

static inline char * lnmai_session_take_json(lean_object *result) {
  char const *raw = lnmai_string_cstr(result);
  char *copy = lnmai_session_string_dup(raw);
  lnmai_dec_result(result);
  return copy;
}

static inline int lnmai_session_json_ok(char const *json) {
  return strstr(json, "\"ok\":true") != NULL;
}

static inline int lnmai_session_json_extract_u64(char const *json, char const *field, uint64_t *out_value) {
  char needle[64];
  if (strlen(field) + 4 >= sizeof(needle)) {
    return 0;
  }
  strcpy(needle, "\"");
  strcat(needle, field);
  strcat(needle, "\":");
  char const *start = strstr(json, needle);
  if (!start) {
    return 0;
  }
  start += strlen(needle);
  char *end = NULL;
  unsigned long long parsed = strtoull(start, &end, 10);
  if (end == start) {
    return 0;
  }
  *out_value = (uint64_t)parsed;
  return 1;
}

static inline int lnmai_session_init(lnmai_empty_handle *out_handle, char **out_json) {
  lean_object *result = lnmai_create_empty_session_handle();
  char *json = lnmai_session_take_json(result);
  if (out_json) {
    *out_json = json;
  }
  if (!json || !lnmai_session_json_ok(json)) {
    return 0;
  }
  return lnmai_session_json_extract_u64(json, "handle", &out_handle->raw);
}

static inline int lnmai_session_load_chart_from_text(
    lnmai_empty_handle empty_handle,
    char const *content,
    uint32_t level_index,
    lnmai_loaded_handle *out_handle,
    char **out_json) {
  lean_object *content_obj = lnmai_mk_string(content);
  lean_object *result = lnmai_load_chart_into_session_from_text(empty_handle.raw, content_obj, level_index);
  char *json = lnmai_session_take_json(result);
  if (out_json) {
    *out_json = json;
  }
  if (!json || !lnmai_session_json_ok(json)) {
    return 0;
  }
  out_handle->raw = empty_handle.raw;
  return 1;
}

static inline int lnmai_session_load_chart_from_json(
    lnmai_empty_handle empty_handle,
    char const *chart_spec_json,
    lnmai_loaded_handle *out_handle,
    char **out_json) {
  lean_object *chart_obj = lnmai_mk_string(chart_spec_json);
  lean_object *result = lnmai_load_chart_into_session_from_json(empty_handle.raw, chart_obj);
  char *json = lnmai_session_take_json(result);
  if (out_json) {
    *out_json = json;
  }
  if (!json || !lnmai_session_json_ok(json)) {
    return 0;
  }
  out_handle->raw = empty_handle.raw;
  return 1;
}

static inline int lnmai_session_unload_chart(
    lnmai_loaded_handle loaded_handle,
    lnmai_empty_handle *out_handle,
    char **out_json) {
  lean_object *result = lnmai_unload_chart_from_session(loaded_handle.raw);
  char *json = lnmai_session_take_json(result);
  if (out_json) {
    *out_json = json;
  }
  if (!json || !lnmai_session_json_ok(json)) {
    return 0;
  }
  out_handle->raw = loaded_handle.raw;
  return 1;
}

static inline char * lnmai_session_get_lowered_chart_json(lnmai_loaded_handle loaded_handle) {
  lean_object *result = lnmai_get_lowered_chart_json_by_handle(loaded_handle.raw);
  return lnmai_session_take_json(result);
}

static inline char * lnmai_session_advance_frame_light(lnmai_loaded_handle loaded_handle, char const *batch_json) {
  lean_object *batch_obj = lnmai_mk_string(batch_json);
  lean_object *result = lnmai_step_game_state_handle_light(loaded_handle.raw, batch_obj);
  return lnmai_session_take_json(result);
}

static inline char * lnmai_session_advance_frame_full(lnmai_loaded_handle loaded_handle, char const *batch_json) {
  lean_object *batch_obj = lnmai_mk_string(batch_json);
  lean_object *result = lnmai_step_game_state_handle(loaded_handle.raw, batch_obj);
  return lnmai_session_take_json(result);
}

static inline char * lnmai_session_get_state_json(lnmai_loaded_handle loaded_handle) {
  lean_object *result = lnmai_get_game_state_json_by_handle(loaded_handle.raw);
  return lnmai_session_take_json(result);
}

static inline char * lnmai_session_free_empty(lnmai_empty_handle empty_handle) {
  lean_object *result = lnmai_free_game_state_handle(empty_handle.raw);
  return lnmai_session_take_json(result);
}

static inline char * lnmai_session_free_loaded(lnmai_loaded_handle loaded_handle) {
  lean_object *result = lnmai_free_game_state_handle(loaded_handle.raw);
  return lnmai_session_take_json(result);
}

#ifdef __cplusplus
}
#endif

#endif
