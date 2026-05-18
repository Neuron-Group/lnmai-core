use std::ffi::{CStr, CString};
use std::marker::PhantomData;

use crate::raw;

pub struct Empty;
pub struct Loaded;

#[derive(Debug, Clone)]
pub struct FfiEnvelope {
    pub json: String,
}

#[derive(Debug, Clone)]
pub struct LnmaiError {
    pub json: String,
}

pub type Result<T> = std::result::Result<T, LnmaiError>;

pub struct Session<State> {
    handle: u64,
    _state: PhantomData<State>,
}

impl<State> Session<State> {
    pub fn handle(&self) -> u64 {
        self.handle
    }
}

fn into_string(result: *mut raw::lean_object) -> String {
    unsafe {
        let ptr = raw::lean_string_cstr(result);
        let value = CStr::from_ptr(ptr).to_string_lossy().into_owned();
        raw::lean_dec_ref(result);
        value
    }
}

fn mk_lean_string(content: &str) -> *mut raw::lean_object {
    let c = CString::new(content).expect("content contains interior NUL");
    unsafe { raw::lean_mk_string(c.as_ptr()) }
}

fn is_ok(json: &str) -> bool {
    json.contains("\"ok\":true")
}

fn extract_handle(json: &str) -> Option<u64> {
    let needle = "\"handle\":";
    let start = json.find(needle)? + needle.len();
    let rest = &json[start..];
    let digits: String = rest.chars().take_while(|ch| ch.is_ascii_digit()).collect();
    digits.parse().ok()
}

fn ok_or_error(json: String) -> Result<FfiEnvelope> {
    if is_ok(&json) {
        Ok(FfiEnvelope { json })
    } else {
        Err(LnmaiError { json })
    }
}

pub unsafe fn initialize_runtime() -> std::result::Result<(), ()> {
    raw::initialize_lnmai_runtime()
}

impl Session<Empty> {
    pub fn create() -> Result<Self> {
        let json = unsafe { into_string(raw::lnmai_create_empty_session_handle()) };
        if !is_ok(&json) {
            return Err(LnmaiError { json });
        }
        let handle = extract_handle(&json).ok_or_else(|| LnmaiError { json: json.clone() })?;
        Ok(Self { handle, _state: PhantomData })
    }

    pub fn load_chart_text(self, content: &str, level_index: u32) -> Result<(Session<Loaded>, FfiEnvelope)> {
        let content_obj = mk_lean_string(content);
        let json = unsafe { into_string(raw::lnmai_load_chart_into_session_from_text(self.handle, content_obj, level_index)) };
        let envelope = ok_or_error(json)?;
        Ok((Session { handle: self.handle, _state: PhantomData }, envelope))
    }

    pub fn load_chart_json(self, chart_spec_json: &str) -> Result<(Session<Loaded>, FfiEnvelope)> {
        let chart_obj = mk_lean_string(chart_spec_json);
        let json = unsafe { into_string(raw::lnmai_load_chart_into_session_from_json(self.handle, chart_obj)) };
        let envelope = ok_or_error(json)?;
        Ok((Session { handle: self.handle, _state: PhantomData }, envelope))
    }

    pub fn free(self) -> Result<FfiEnvelope> {
        let json = unsafe { into_string(raw::lnmai_free_game_state_handle(self.handle)) };
        ok_or_error(json)
    }
}

impl Session<Loaded> {
    pub fn get_lowered_chart_json(&self) -> Result<FfiEnvelope> {
        let json = unsafe { into_string(raw::lnmai_get_lowered_chart_json_by_handle(self.handle)) };
        ok_or_error(json)
    }

    pub fn advance_frame_light(&mut self, batch_json: &str) -> Result<FfiEnvelope> {
        let batch_obj = mk_lean_string(batch_json);
        let json = unsafe { into_string(raw::lnmai_step_game_state_handle_light(self.handle, batch_obj)) };
        ok_or_error(json)
    }

    pub fn advance_frame_full(&mut self, batch_json: &str) -> Result<FfiEnvelope> {
        let batch_obj = mk_lean_string(batch_json);
        let json = unsafe { into_string(raw::lnmai_step_game_state_handle(self.handle, batch_obj)) };
        ok_or_error(json)
    }

    pub fn get_state_json(&self) -> Result<FfiEnvelope> {
        let json = unsafe { into_string(raw::lnmai_get_game_state_json_by_handle(self.handle)) };
        ok_or_error(json)
    }

    pub fn unload_chart(self) -> Result<(Session<Empty>, FfiEnvelope)> {
        let json = unsafe { into_string(raw::lnmai_unload_chart_from_session(self.handle)) };
        let envelope = ok_or_error(json)?;
        Ok((Session { handle: self.handle, _state: PhantomData }, envelope))
    }

    pub fn free(self) -> Result<FfiEnvelope> {
        let json = unsafe { into_string(raw::lnmai_free_game_state_handle(self.handle)) };
        ok_or_error(json)
    }
}
