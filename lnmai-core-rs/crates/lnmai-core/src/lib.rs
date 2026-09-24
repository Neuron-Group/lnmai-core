//! `lnmai-core` — Rust port of the Lean `LnmaiCore` gameplay core.
//!
//! Porting is organised by milestone:
//!
//! - **M1 (verify crate)**: the pure semantic core (`areas`, `time`, `types`,
//!   `constants`, `convert`, `judge`, `score`) lives in `lnmai_core_verify`,
//!   which is written in the Aeneas-compatible subset. Its equivalence to the
//!   authoritative Lean modules is proved under `verification/`.
//! - **M2+ (this crate)**: runtime state and the Simai pipeline are ported here
//!   as ordinary Rust, reusing the verified core wherever possible.
//!
//! The verified modules are re-exported so downstream code has a single import
//! surface.

pub use lnmai_core_verify::{areas, constants, convert, judge, score, types};

pub mod cabi;
pub mod chart_loader;
pub mod events;
pub mod ffi;
pub mod input_model;
pub mod lifecycle;
pub mod rat;
pub mod runtime_score;
pub mod scheduler;
pub mod simai;
pub mod storage;
pub mod symmetry;
pub mod time;
