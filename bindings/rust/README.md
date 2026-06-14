# Rust FFI Bindings

This directory contains low-level `extern "C"` bindings for the currently
implemented `lnmai-core` Lean FFI.

Files:

- `bindings/rust/mod.rs` — raw symbol declarations and a minimal init helper
- `bindings/rust/api.rs` — typed helpers for parse/build/step string-based FFI APIs
- `bindings/rust/types.rs` — typed JSON-visible Lean payload mirrors
- `bindings/rust/session.rs` — safe-ish typestate session wrapper over the raw FFI

Implementation note:

- `bindings/rust/types.rs` is sourced from the shared schema file at `shared/rust_ffi_types.rs`
- `bindings/rust/api.rs` is sourced from the shared typed API file at `shared/rust_ffi_api.rs`
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

Current slide head/body schema notes:

- lowered charts now expose explicit `slideHeads` plus slide-body `slides`
- ordinary slides lower to head + body; no-head singleton slides and connected child parts lower to body only
- lowered head/body pairs now carry explicit `logicalSlideId`
- lowered slide heads and slide bodies now use distinct `noteIndex` values while sharing `logicalSlideId`
- lowered slide bodies use `headTiming` as the body-side head anchor field in the current schema
- lowered slide bodies use segment-local slide break semantics for their `isBreak` field; slide heads keep head break semantics
- folded identical simultaneous slide bodies carry `multiple`; lowered slide heads remain explicit separate head notes
- Rust typed mirrors require `headTiming` for slide-body payloads
- normalized slide mirrors expose explicit `hasHeadNote` and `hasBody`; `isSlideNoHead` remains compatibility metadata rather than the long-term semantic authority

Runtime state note:

- `GameState.tapQueues` now serialize tagged `TapFamilyNote` entries with `kind = "tap"` or `"slideHead"`
- slide heads remain in the shared tap-family queue; the new tag only exposes runtime object kind to hosts
- `GameState` no longer exposes a core touch-mode flag; hosts should synthesize sensor input before calling the core if they want desktop-style outer-button touch mapping
- `GameState.noteFastLateDisplay` and `GameState.breakFastLateDisplay` mirror the core fast/late counter policy
- `ScoreState.dxScore` is the core DX-loss delta; use `ScoreState::dx_score_remaining()` for achieved DX score
- `ScoreState::combo_state()` derives AP+/AP/FC+/FC from judge-count categories

Recommended next step for real host integration:

- wire these files into a small Rust crate layout, for example:
  - `pub mod raw;` using `bindings/rust/mod.rs`
  - `pub mod api;` using `bindings/rust/api.rs`
  - `pub mod types;` using `bindings/rust/types.rs`
  - `pub mod session;` using `bindings/rust/session.rs`
- use `api.rs` for typed parser/build/step helpers, or `session.rs` for handle-based runtime flows

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
