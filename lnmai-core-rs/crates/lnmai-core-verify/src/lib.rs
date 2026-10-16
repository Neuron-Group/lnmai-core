//! Verification-facing subset of lnmai-core.
//!
//! These finite tables and bounded arithmetic operations are extracted by
//! Charon/Aeneas and related to Lean under explicit preconditions. The runtime
//! shares area/conversion code and calls judgment tables through exact-number
//! adapters; unbounded time and scoring arithmetic live in `lnmai_core`.

pub mod areas;
pub mod constants;
pub mod convert;
pub mod grade_counts;
pub mod judge;
pub mod lifecycle;
pub mod score;
pub mod shape;
pub mod slide_queue;
pub mod storage;
pub mod symmetry;
pub mod time;
pub mod tokenize;
pub mod types;
