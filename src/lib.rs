#![crate_name="regex_dfa"]
#![crate_type="lib"]
#![crate_type="dylib"]

extern crate vec_map;
extern crate bit_set;

pub use regex::Regex;
pub use dfa::{State, Dfa};
pub mod regex;
pub mod derivatives;
pub mod dfa;
#[cfg(test)] mod tests;
