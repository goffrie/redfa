use regex::Regex;
use std::collections::{BTreeMap, RingBuf};
use std::collections::btree_map::{Entry};

#[derive(Show, Clone)]
pub struct Transition {
    pub by_char: BTreeMap<char, u32>,
    pub default: u32,
    pub accepting: bool,
}

#[derive(Show, Clone)]
pub struct Dfa {
    pub transitions: Vec<Transition>,
}

impl Dfa {
    pub fn from_regex(start: Regex) -> Dfa {
        fn index(worklist: &mut (BTreeMap<Regex, u32>, RingBuf<Regex>), re: Regex) -> u32 {
            let next_index = worklist.0.len() as u32;
            match worklist.0.entry(re.clone()) { // FIXME: unnecessary allocation
                Entry::Vacant(view) => {
                    view.insert(next_index);
                    worklist.1.push_back(re);
                    next_index
                }
                Entry::Occupied(view) => {
                    return *view.get();
                }
            }
        }

        let start = start.simplify();
        let mut result = Dfa { transitions: Vec::new() };
        let mut worklist = (BTreeMap::new(), RingBuf::new());

        index(&mut worklist, start);
        index(&mut worklist, Regex::Null);

        while let Some(re) = worklist.1.pop_front() {
            let d = re.derivative();
            let accepting = re.nullable();
            let mut by_char = BTreeMap::new();
            for (chars, dre) in d.d.into_iter() {
                let ix = index(&mut worklist, dre.simplify());
                for ch in chars.into_iter() {
                    by_char.insert(ch, ix);
                }
            }
            let default = index(&mut worklist, d.rest.simplify());
            result.transitions.push(Transition {
                by_char: by_char,
                default: default,
                accepting: accepting,
            });
        }

        result
    }
}
