use std::os::raw::c_char;

#[repr(C)]
pub struct lean_object {
    _private: [u8; 0],
}

unsafe extern "C" {
    pub fn lean_initialize();

    pub fn lean_io_result_is_error(result: *mut lean_object) -> bool;
    pub fn lean_io_result_show_error(result: *mut lean_object);
    pub fn lean_dec_ref(obj: *mut lean_object);
    pub fn lean_mk_string(s: *const c_char) -> *mut lean_object;
    pub fn lean_string_cstr(s: *mut lean_object) -> *const c_char;

    pub fn initialize_lnmai_x2dcore_LnmaiCore(builtin: u8) -> *mut lean_object;
    pub fn initialize_lnmai_x2dcore_LnmaiCore_FFI(builtin: u8) -> *mut lean_object;

    pub fn lnmai_parse_frontend_chart_json(content: *mut lean_object, level_index: u32) -> *mut lean_object;
    pub fn lnmai_parse_frontend_semantic_chart_json(content: *mut lean_object, level_index: u32) -> *mut lean_object;
    pub fn lnmai_parse_frontend_inspection_chart_json(content: *mut lean_object, level_index: u32) -> *mut lean_object;
    pub fn lnmai_parse_normalized_chart_json(content: *mut lean_object, level_index: u32) -> *mut lean_object;
    pub fn lnmai_parse_lowered_chart_json(content: *mut lean_object, level_index: u32) -> *mut lean_object;

    pub fn lnmai_build_game_state_json(chart_spec_json: *mut lean_object) -> *mut lean_object;
    pub fn lnmai_step_game_state_json(state_json: *mut lean_object, batch_json: *mut lean_object) -> *mut lean_object;

    pub fn lnmai_create_empty_session_handle() -> *mut lean_object;
    pub fn lnmai_load_chart_into_session_from_text(handle: u64, content: *mut lean_object, level_index: u32) -> *mut lean_object;
    pub fn lnmai_load_chart_into_session_from_json(handle: u64, chart_spec_json: *mut lean_object) -> *mut lean_object;
    pub fn lnmai_unload_chart_from_session(handle: u64) -> *mut lean_object;
    pub fn lnmai_get_lowered_chart_json_by_handle(handle: u64) -> *mut lean_object;

    pub fn lnmai_create_game_state_handle(chart_spec_json: *mut lean_object) -> *mut lean_object;
    pub fn lnmai_free_game_state_handle(handle: u64) -> *mut lean_object;
    pub fn lnmai_get_game_state_json_by_handle(handle: u64) -> *mut lean_object;
    pub fn lnmai_step_game_state_handle(handle: u64, batch_json: *mut lean_object) -> *mut lean_object;
    pub fn lnmai_step_game_state_handle_light(handle: u64, batch_json: *mut lean_object) -> *mut lean_object;
}

pub unsafe fn initialize_lnmai_runtime() -> Result<(), ()> {
    lean_initialize();
    let result = initialize_lnmai_x2dcore_LnmaiCore(1);
    if lean_io_result_is_error(result) {
        lean_io_result_show_error(result);
        lean_dec_ref(result);
        Err(())
    } else {
        lean_dec_ref(result);
        Ok(())
    }
}
