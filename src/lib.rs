#![crate_name="regex_dfa"]
#![crate_type="lib"]
#![crate_type="dylib"]
#![feature(box_syntax, box_patterns, core, collections)]
pub use regex::Regex;
pub use dfa::{State, Dfa};
pub mod regex;
pub mod derivatives;
pub mod dfa;
