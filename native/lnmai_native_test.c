/* Test-only adapter: exercise the actual C ABI from the JSON oracle CLI. */
#include "lnmai_native.h"
#include <lean/lean.h>
#include <stdlib.h>

lean_object *lnmai_test_native_call(uint64_t handle, uint32_t operation,
    uint64_t time, uint32_t level, b_lean_obj_arg words, b_lean_obj_arg text) {
    size_t count = lean_array_size(words) / 4;
    LnmaiInputEvent *events = count ? calloc(count, sizeof(*events)) : NULL;
    if (count && !events) return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("allocation failed")));
    for (size_t i = 0; i < count; ++i) {
        events[i].time_us = (int64_t)lean_unbox_uint64(lean_array_get_core(words, 4*i));
        events[i].kind = (uint32_t)lean_unbox_uint64(lean_array_get_core(words, 4*i+1));
        events[i].position = (uint32_t)lean_unbox_uint64(lean_array_get_core(words, 4*i+2));
        events[i].is_down = (uint32_t)lean_unbox_uint64(lean_array_get_core(words, 4*i+3));
    }
    LnmaiRequest request = {3, sizeof(LnmaiRequest), (int64_t)time, level, 0,
        events, count, lean_string_cstr(text), lean_string_size(text)-1};
    LnmaiResult *out = NULL;
    uint32_t status = lnmai_v3_call(handle, operation, &request, &out);
    free(events);
    uint64_t n = 0;
    const LnmaiRecord *records = out ? lnmai_v3_records(out, &n) : NULL;
    lean_object *rows = lean_mk_empty_array();
    for (uint64_t i = 0; i < n; ++i) {
        lean_object *row = lean_mk_empty_array();
        row = lean_array_push(row, lean_box_uint64(records[i].tag));
        for (size_t j = 0; j < 8; ++j) row = lean_array_push(row, lean_box_uint64(records[i].words[j]));
        rows = lean_array_push(rows, row);
    }
    lean_object *pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, rows);
    lean_ctor_set(pair, 1, lean_mk_string(status ? lnmai_v3_error(out) : ""));
    lean_object *triple = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(triple, 0, lean_box_uint32(status));
    lean_ctor_set(triple, 1, pair);
    lnmai_v3_free_result(out);
    return lean_io_result_mk_ok(triple);
}
