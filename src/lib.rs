#![crate_name="regex_dfa"]
#![crate_type="lib"]
#![crate_type="dylib"]
#![feature(box_syntax)]
#![allow(unstable)]
pub use regex::Regex;
pub use dfa::{Transition, Dfa};
pub mod regex;
pub mod dfa;
