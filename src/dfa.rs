use derivatives::Differentiable;
use std::collections::{BTreeMap, VecDeque};
use std::collections::btree_map::{Entry};

#[derive(Debug, Clone)]
pub struct Transition {
    pub by_char: BTreeMap<char, u32>,
    pub default: u32,
}

#[derive(Debug, Clone)]
pub struct Dfa {
    pub transitions: Vec<Transition>,
}

pub trait Normalize {
    fn normalize(self) -> Self;
}

impl<R: Normalize> Normalize for Vec<R> {
    fn normalize(self) -> Self {
        self.map_in_place(Normalize::normalize)
    }
}

impl Dfa {
    pub fn from_derivatives<R: Differentiable + Normalize + Ord + Clone>(initial: Vec<R>) -> (Dfa, BTreeMap<R, u32>) {
        fn index<R: Ord + Clone>(worklist: &mut (BTreeMap<R, u32>, VecDeque<R>), re: R) -> u32 {
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

        let mut result = Dfa { transitions: Vec::new() };
        let mut worklist = (BTreeMap::new(), VecDeque::new());

        for r in initial.into_iter() {
            index(&mut worklist, r.normalize());
        }

        while let Some(re) = worklist.1.pop_front() {
            let d = re.derivative();
            let mut by_char = BTreeMap::new();
            for (chars, dre) in d.d.into_iter() {
                let ix = index(&mut worklist, dre.normalize());
                for ch in chars.into_iter() {
                    by_char.insert(ch, ix);
                }
            }
            let default = index(&mut worklist, d.rest.normalize());
            result.transitions.push(Transition {
                by_char: by_char,
                default: default,
            });
        }

        (result, worklist.0)
    }
}
