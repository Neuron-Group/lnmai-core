# Rust FFI Bindings

This directory contains low-level `extern "C"` bindings for the currently
implemented `lnmai-core` Lean FFI.

Files:

- `bindings/rust/mod.rs` — raw symbol declarations and a minimal init helper

Notes:

- this is not a published Rust crate yet
- string results are returned as `lean_object *` and must be read with
  `lean_string_cstr`
- returned Lean objects must be released with `lean_dec_ref`
- initialize the Lean runtime before calling exported functions

Recommended next step for real host integration:

- wrap these raw bindings in a safe Rust API that converts:
  - `&str` -> Lean string
  - Lean string result -> `String`
  - JSON envelope -> typed Rust result
