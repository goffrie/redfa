extern crate bit_set;
extern crate vec_map;

pub use dfa::{Dfa, State};
pub use regex::Regex;
pub mod derivatives;
pub mod dfa;
pub mod regex;
#[cfg(test)]
mod tests;
