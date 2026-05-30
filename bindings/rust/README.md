# Rust FFI Bindings

This directory contains low-level `extern "C"` bindings for the currently
implemented `lnmai-core` Lean FFI.

Files:

- `bindings/rust/mod.rs` — raw symbol declarations and a minimal init helper
- `bindings/rust/types.rs` — typed JSON-visible Lean payload mirrors
- `bindings/rust/session.rs` — safe-ish typestate session wrapper over the raw FFI

Implementation note:

- `bindings/rust/types.rs` is sourced from the shared schema file at `shared/rust_ffi_types.rs`
- `bindings/rust/mod.rs` is sourced from the shared raw FFI file at `shared/rust_ffi_raw.rs`
- `bindings/rust/session.rs` is sourced from the shared session wrapper file at `shared/rust_ffi_session.rs`
- the root crate `src/types.rs` uses that same shared file to keep both Rust surfaces aligned
- the root crate `src/raw.rs` and `src/session.rs` use those same shared files too

Notes:

- this is not a published Rust crate yet
- string results are returned as `lean_object *` and must be read with
  `lean_string_cstr`
- returned Lean objects must be released with `lean_dec_ref`
- initialize the Lean runtime before calling exported functions

Recommended next step for real host integration:

- wire these files into a small Rust crate layout, for example:
  - `pub mod raw;` using `bindings/rust/mod.rs`
  - `pub mod types;` using `bindings/rust/types.rs`
  - `pub mod session;` using `bindings/rust/session.rs`
- optionally replace the current lightweight JSON-envelope inspection with full
  `serde_json` decoding into typed host structs

Session wrapper usage sketch:

```rust
pub mod raw;
pub mod session;

use session::Session;

unsafe { session::initialize_runtime().unwrap() };
let empty = Session::<session::Empty>::create().unwrap();
let (mut loaded, _load_info) = empty.load_chart_text(chart_text, 0).unwrap();
let step = loaded.advance_frame_light(batch_json).unwrap();
let (_empty, _unload_info) = loaded.unload_chart().unwrap();
```
