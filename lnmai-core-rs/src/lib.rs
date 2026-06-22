//! LnmaiCore - Verified game judgment runtime for maimai rhythm game
//!
//! This crate is a Rust implementation of the Lean specification in `LnmaiCore`.
//! It provides a pure functional game judgment engine for the maimai rhythm game.
//!
//! # Modules
//!
//! - `types`: Core domain types (grades, notes, styles, events)
//! - `areas`: Physical area types (sensors, buttons, slots)
//! - `time`: Time primitives (TimePoint, Duration)
//! - `constants`: Timing windows and frame constants
//! - `convert`: Grade conversion (MAJI, GACHI, GORI)
//! - `judge`: Pure judgment functions (tap, touch, slide, hold end)
//! - `score`: Score/combo computation
//! - `lifecycle`: Note state machines (tap, hold, touch, slide)
//! - `input_model`: Frame input, queues, game state
//! - `chart_loader`: Declarative chart-to-runtime loader
//! - `scheduler`: stepFrame: one function per frame

#![feature(register_tool)]
#![register_tool(aeneas)]
#![register_tool(charon)]

pub mod types;
pub mod areas;
pub mod storage;
pub mod time;
pub mod constants;
pub mod convert;
pub mod judge;
pub mod score;
pub mod lifecycle;
pub mod input_model;
pub mod chart_loader;
pub mod scheduler;
pub mod simai;
pub mod aeneas_test;

// Re-export main types for convenience
pub use types::*;
pub use areas::*;
pub use time::*;
pub use constants::*;
pub use convert::*;
pub use judge::*;
pub use score::*;
pub use lifecycle::*;
pub use input_model::*;
pub use chart_loader::*;
pub use scheduler::*;
