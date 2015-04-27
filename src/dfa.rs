use derivatives::Differentiable;
use std::collections::{BTreeMap, BTreeSet, VecDeque, VecMap, BitSet};

/// A state in a DFA.
#[derive(Debug, Clone)]
pub struct State<T, V> {
    /// Labelled transitions.
    pub by_char: BTreeMap<T, u32>,
    /// The default transition (for values not in `by_char`).
    /// Note that `by_char` is assumed not to cover the entire alphabet (`T`).
    pub default: u32,
    /// A value associated with the state.
    pub value: V,
}

/// A deterministic finite automaton (DFA), over the alphabet `T`.
/// Each state is annotated with a value of type `V`.
/// The starting state is the first one.
#[derive(Debug, Clone)]
pub struct Dfa<T, V> {
    pub states: Vec<State<T, V>>,
}

pub trait Normalize {
    fn normalize(self) -> Self;
}

impl<R: Normalize> Normalize for Vec<R> {
    fn normalize(self) -> Self {
        self.map_in_place(Normalize::normalize)
    }
}

impl<T, V> Dfa<T, V> {
    /// Construct a DFA from a list of differentiable objects.
    /// The elements of `initial` form the first states of the DFA.
    /// Returns the DFA, together with a mapping from derivatives to state numbers.
    pub fn from_derivatives(initial: Vec<V>) -> (Dfa<T, V>, BTreeMap<V, u32>)
        where T: Ord,
              V: Differentiable<T> + Normalize + Ord + Clone {
        fn index<V: Ord + Clone>(&mut (ref mut indices, ref mut next): &mut (BTreeMap<V, u32>, VecDeque<V>), re: V) -> u32 {
            let next_index = indices.len() as u32;
            *indices.entry(re.clone()) // FIXME: unnecessary allocation
                .or_insert_with(|| {
                    next.push_back(re);
                    next_index
                })
        }

        let mut result = Dfa { states: Vec::new() };
        let mut worklist = (BTreeMap::new(), VecDeque::new());

        for r in initial {
            index(&mut worklist, r.normalize());
        }

        while let Some(re) = worklist.1.pop_front() {
            let d = re.derivative();
            let mut by_char = BTreeMap::new();
            for (chars, dre) in d.d {
                let ix = index(&mut worklist, dre.normalize());
                for ch in chars {
                    by_char.insert(ch, ix);
                }
            }
            let default = index(&mut worklist, d.rest.normalize());
            result.states.push(State {
                by_char: by_char,
                default: default,
                value: re,
            });
        }

        (result, worklist.0)
    }

    pub fn map<U, F>(self, mut f: F) -> Dfa<T, U>
        where F: FnMut(V) -> U {
        Dfa {
            states: self.states.into_iter().map(|state| State {
                by_char: state.by_char,
                default: state.default,
                value: f(state.value),
            }).collect()
        }
    }

    /// Find the reverse transitions from each state in the DFA.
    pub fn reverse(&self) -> Vec<(BTreeMap<&T, BTreeSet<usize> >, BTreeSet<usize>)>
        where T: Ord {
        let mut result = vec![(BTreeMap::new(), BTreeSet::new()); self.states.len()];
        for (state_ix, state) in self.states.iter().enumerate() {
            let mut rev: BTreeMap<usize, BTreeSet<_>> = BTreeMap::new();
            for (by, &to) in &state.by_char {
                rev.entry(to as usize).or_insert_with(BTreeSet::new).insert(by);
            }
            for (&to, by) in &rev {
                for what in by {
                    let &mut (ref mut trans, ref default) = &mut result[to];
                    trans.entry(*what).or_insert_with(|| default.clone()).insert(state_ix);
                }
            }
            // `state.default` means that, for all characters NOT in
            // `state.by_char`, there is a transition to `state_ix`.
            let &mut (ref mut trans, ref mut default) = &mut result[state.default as usize];
            for c in state.by_char.keys() {
                // make sure that characters in `state.by_char` are _excluded_
                trans.entry(c).or_insert_with(|| default.clone());
            }
            // any other characters, _not_ in `state.by_char`, should be
            // included
            for (key, ref mut val) in trans {
                if !state.by_char.contains_key(key) {
                    val.insert(state_ix);
                }
            }
            // as should the default
            default.insert(state_ix);
        }
        result
    }

    /// Minimize a DFA; i.e. find a DFA with the fewest states that is
    /// equivalent to the given DFA.
    /// Two DFAs are equivalent if, given the same string, they always lead to a
    /// state with the same associated value.
    pub fn minimize(&self) -> Dfa<T, &V>
        where T: Ord + Clone, V: Ord {

        assert!(!self.states.is_empty());

        // `partitions` is a partition of the DFA states, representing the
        // current set of equivalence classes
        let mut partitions: Vec<BTreeSet<usize>> = vec![];
        {
            // Calculate the initial partition. The choice of initial partition
            // determines what states the algorithm considers distinguishable.
            // We only consider states that are reachable from the starting
            // state, so we traverse the DFA to find these states.
            let mut initial_partition = BTreeMap::new();
            let mut worklist = VecDeque::new();
            let mut seen = BitSet::new();
            worklist.push_back(0);
            seen.insert(0);
            while let Some(state_ix) = worklist.pop_front() {
                let state = &self.states[state_ix];
                let part = *initial_partition.entry(&state.value)
                    .or_insert_with(|| {
                        let ix = partitions.len();
                        partitions.push(BTreeSet::new());
                        ix
                    });
                partitions[part].insert(state_ix);
                for &next in state.by_char.values()
                    .chain(Some(&state.default).into_iter()) {
                    if seen.insert(next as usize) {
                        worklist.push_back(next as usize);
                    }
                }
            }
        }
        // The above code should always put state 0 in partition 0.
        debug_assert!(partitions[0].contains(&0));

        let preimages = self.reverse();
        let mut worklist: BTreeSet<usize> = (0..partitions.len()).collect();
        while let Some(&cur_ix) = worklist.iter().next() {
            // XXX: I wish there were a way to just grab the first element...
            worklist.remove(&cur_ix);
            let chars: BTreeSet<&T> = partitions[cur_ix].iter().flat_map(|&state| preimages[state].0.keys().cloned()).collect();
            for c in chars.into_iter().map(Some).chain(Some(None).into_iter()) {
                let mut l = BTreeSet::new();
                if let Some(c) = c {
                    for &state in &partitions[cur_ix] {
                        if let Some(prevs) = preimages[state].0.get(c) {
                            l.extend(prevs.iter().cloned());
                        } else {
                            l.extend(preimages[state].1.iter().cloned());
                        }
                    }
                } else {
                    for &state in &partitions[cur_ix] {
                        l.extend(preimages[state].1.iter().cloned());
                    }
                }
                let l = l;
                for part_ix in 0..partitions.len() {
                    let r1: BTreeSet<_> = partitions[part_ix].intersection(&l).cloned().collect();
                    if r1.is_empty() {
                        continue;
                    }
                    let r2: BTreeSet<_> = partitions[part_ix].difference(&r1).cloned().collect();
                    if r2.is_empty() {
                        continue;
                    }
                    // make sure that the starting state (#0) stays where it is
                    let (r1, r2) = if r2.contains(&0) {
                        (r2, r1)
                    } else {
                        (r1, r2)
                    };
                    // partitions[part_ix] = r1, partitions[new_ix] = r2
                    let new_ix = partitions.len();
                    // first update the worklist
                    if worklist.contains(&part_ix) {
                        // if the refined partition was already there, then keep
                        // both halves
                        worklist.insert(new_ix);
                    } else {
                        // otherwise, we need to add one half to the worklist
                        if r1.len() <= r2.len() {
                            worklist.insert(part_ix);
                        } else {
                            worklist.insert(new_ix);
                        }
                    }
                    // then refine partitions[part_ix]
                    partitions[part_ix] = r1;
                    partitions.push(r2);
                }
            }
        }

        // After refinement, the first partition should still contain the
        // starting state.
        debug_assert!(partitions[0].contains(&0));

        let partition: BTreeMap<usize, u32> = partitions.iter().enumerate()
            .flat_map(|(part_ix, part)| part.iter()
                      .map(move |&state_ix| (state_ix, part_ix as u32)))
            .collect();

        let states = partitions.iter().map(|part| {
            let state_ix = *part.iter().next().unwrap();
            let state = &self.states[state_ix];
            let default = partition[&(state.default as usize)];
            State {
                by_char: state.by_char.iter()
                    .map(|(key, &to)| (key.clone(), partition[&(to as usize)]))
                    .filter(|&(_, to)| to != default)
                    .collect(),
                default: default,
                value: &state.value,
            }
        }).collect();

        Dfa { states: states }
    }
}

/// Compare DFAs by graph isomorphism.
impl<T: Ord, U, V: PartialEq<U>> PartialEq<Dfa<T, U>> for Dfa<T, V> {
    fn eq(&self, other: &Dfa<T, U>) -> bool {
        if self.states.len() != other.states.len() {
            return false;
        }
        let mut mapping = VecMap::with_capacity(self.states.len());
        let mut worklist = VecDeque::new();
        mapping.insert(0, 0);
        worklist.push_back(0);
        while let Some(ix) = worklist.pop_front() {
            let other_ix = mapping[ix as usize];
            let a = &self.states[ix as usize];
            let b = &other.states[other_ix as usize];
            if a.by_char.len() != b.by_char.len() {
                return false;
            }
            for (c, &to) in &a.by_char {
                if let Some(&other_to) = b.by_char.get(c) {
                    if let Some(old_mapping) = mapping.insert(to as usize, other_to) {
                        // make sure the replaced element was the same
                        if old_mapping != other_to {
                            return false;
                        }
                    } else {
                        // new mapping
                        worklist.push_back(to);
                    }
                } else {
                    return false;
                }
            }
            if let Some(old_mapping) = mapping.insert(a.default as usize, b.default) {
                if old_mapping != b.default {
                    return false;
                }
            } else {
                worklist.push_back(a.default);
            }
        }
        return true;
    }
}
impl<T: Ord, V: Eq> Eq for Dfa<T, V> {
}
