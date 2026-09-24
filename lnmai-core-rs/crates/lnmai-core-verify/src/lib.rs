//! Verification-facing subset of lnmai-core.
//!
//! This crate is the single source of truth for the pure gameplay semantics.
//! It is written in the Aeneas-compatible subset of Rust (no `String`, no
//! collections, no concurrency) so that Charon can extract it and Aeneas can
//! generate a Lean model from it. The main `lnmai-core` crate depends on this
//! crate, so what is proved here is what ships.

pub mod areas;
pub mod constants;
pub mod convert;
pub mod judge;
pub mod score;
pub mod time;
pub mod types;
