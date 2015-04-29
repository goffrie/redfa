#![crate_name="regex_dfa"]
#![crate_type="lib"]
#![crate_type="dylib"]
pub use regex::Regex;
pub use dfa::{State, Dfa};
pub mod regex;
pub mod derivatives;
pub mod dfa;
#[cfg(test)] mod tests;
