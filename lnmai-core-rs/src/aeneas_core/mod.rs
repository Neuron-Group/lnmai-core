//! LnmaiCore Aeneas-Friendly Core
//!
//! This module contains the pure functional core that Aeneas can translate.
//! No serde, no String, no complex trait objects.
//!
//! Design decisions (from rust-lean-aeneas skill):
//! - Vec<u8> instead of String
//! - Explicit while loops instead of iterators
//! - Enum-dispatch instead of dyn Trait
//! - u32 IDs instead of strings

pub mod areas;
pub mod time;
pub mod constants;
pub mod types;
pub mod convert;
pub mod judge;
pub mod score;

// Re-export main types
pub use areas::*;
pub use time::*;
pub use constants::*;
pub use types::*;
pub use convert::*;
pub use judge::*;
pub use score::*;
