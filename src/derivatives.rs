use std::cmp::Ordering;
use std::iter::Peekable;
use crate::Regex;
use crate::Regex::*;

/// The set of some object's derivatives with respect to an alphabet `T`.
#[derive(Debug, Clone)]
pub struct Derivatives<T, R> {
    /// Holds a set of pairs `(chars, derivative)`, meaning that the derivative
    /// with respect to any element of `chars` is `derivative`.
    pub d: Vec<(Vec<T>, R)>,
    /// The derivative with respect to any character not listed in `d`.
    pub rest: R,
}

impl<T, R> Derivatives<T, R> {
    pub fn map<F: FnMut(R) -> R>(self, mut f: F) -> Derivatives<T, R> {
        Derivatives {
            d: self.d.into_iter().map(|(x, r)| (x, f(r))).collect(),
            rest: f(self.rest),
        }
    }
}

/// A trait for types which can be differentiated with respect to an alphabet
/// `T`.
pub trait Differentiable<T>: Sized {
    fn derivative(&self) -> Derivatives<T, Self>;
}

struct Union<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>> {
    a: Peekable<It1>,
    b: Peekable<It2>,
}
fn union<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>>(
    a: It1,
    b: It2,
) -> Union<T, It1, It2> {
    Union {
        a: a.peekable(),
        b: b.peekable(),
    }
}
impl<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>> Iterator for Union<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        match match self.a.peek() {
            Some(av) => match self.b.peek() {
                Some(bv) => av.cmp(bv),
                None => Ordering::Less,
            },
            None => Ordering::Greater,
        } {
            Ordering::Less => self.a.next(),
            Ordering::Greater => self.b.next(),
            Ordering::Equal => {
                self.a.next();
                self.b.next()
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a1, a2) = self.a.size_hint();
        let (b1, b2) = self.b.size_hint();
        (
            a1 + b1,
            if let (Some(a2), Some(b2)) = (a2, b2) {
                Some(a2 + b2)
            } else {
                None
            },
        )
    }
}

struct Inter<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>> {
    a: Peekable<It1>,
    b: Peekable<It2>,
}
fn inter<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>>(
    a: It1,
    b: It2,
) -> Inter<T, It1, It2> {
    Inter {
        a: a.peekable(),
        b: b.peekable(),
    }
}
impl<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>> Iterator for Inter<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        loop {
            match if let (Some(av), Some(bv)) = (self.a.peek(), self.b.peek()) {
                av.cmp(bv)
            } else {
                return None;
            } {
                Ordering::Less => {
                    self.a.next();
                }
                Ordering::Greater => {
                    self.b.next();
                }
                Ordering::Equal => {
                    self.a.next();
                    return self.b.next();
                }
            }
        }
    }
}

struct Subtract<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>> {
    a: Peekable<It1>,
    b: Peekable<It2>,
}
fn subtract<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>>(
    a: It1,
    b: It2,
) -> Subtract<T, It1, It2> {
    Subtract {
        a: a.peekable(),
        b: b.peekable(),
    }
}
impl<T: Ord, It1: Iterator<Item = T>, It2: Iterator<Item = T>> Iterator for Subtract<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        loop {
            match match (self.a.peek(), self.b.peek()) {
                (Some(av), Some(bv)) => av.cmp(bv),
                (_, None) => Ordering::Less,
                (None, _) => return None,
            } {
                Ordering::Less => {
                    return self.a.next();
                }
                Ordering::Greater => {
                    self.b.next();
                }
                Ordering::Equal => {
                    self.a.next();
                    self.b.next();
                }
            }
        }
    }
}

enum Set<T> {
    Just(Vec<T>),
    Not(Vec<T>),
}

impl<T: Ord + Clone> Set<T> {
    fn inter(&self, b: &[T]) -> Set<T> {
        use self::Set::*;
        match *self {
            Just(ref a) => Just(inter(a.iter().cloned(), b.iter().cloned()).collect()),
            Not(ref a) => Just(subtract(b.iter().cloned(), a.iter().cloned()).collect()),
        }
    }
    fn subtract(&self, b: &[T]) -> Set<T> {
        use self::Set::*;
        match *self {
            Just(ref a) => Just(subtract(a.iter().cloned(), b.iter().cloned()).collect()),
            Not(ref a) => Not(union(a.iter().cloned(), b.iter().cloned()).collect()),
        }
    }
}

fn combine<T: Ord + Clone, R, S, F: FnMut(&[&R]) -> S>(
    v: &[Derivatives<T, R>],
    mut f: F,
) -> Derivatives<T, S> {
    fn go<'a, T: Ord + Clone, R, S, F: FnMut(&[&R]) -> S>(
        v: &'a [Derivatives<T, R>],
        f: &mut F,
        what: Set<T>,
        current: &mut Vec<&'a R>,
        out: &mut (Vec<(Vec<T>, S)>, Option<S>),
    ) {
        if let Set::Just(ref v) = what {
            if v.len() == 0 {
                // prune
                return;
            }
        }
        if v.len() == 0 {
            let reg = f(&current);
            match what {
                Set::Just(c) => out.0.push((c, reg)),
                Set::Not(_) => {
                    assert!(out.1.is_none());
                    out.1 = Some(reg);
                }
            }
            return;
        }
        let (first, rest) = v.split_at(1);
        let first = &first[0];
        let mut all_chars = Vec::new();
        for &(ref chars, ref reg) in first.d.iter() {
            all_chars = union(all_chars.into_iter(), chars.iter().cloned()).collect();
            let inter = what.inter(&chars);
            current.push(reg);
            go(rest, f, inter, current, out);
            current.pop();
        }
        let inter = what.subtract(&all_chars);
        current.push(&first.rest);
        go(rest, f, inter, current, out);
        current.pop();
    }
    let mut result = (Vec::new(), None);
    let mut regexes = Vec::new();
    go(v, &mut f, Set::Not(Vec::new()), &mut regexes, &mut result);
    Derivatives {
        d: result.0,
        rest: result.1.unwrap(),
    }
}

impl<T: Ord + Clone> Differentiable<T> for Regex<T> {
    fn derivative(&self) -> Derivatives<T, Regex<T>> {
        match *self {
            Null => Derivatives {
                d: Vec::new(),
                rest: Null,
            },
            Empty => Derivatives {
                d: Vec::new(),
                rest: Null,
            },
            Except(ref cs) => {
                if cs.len() == 0 {
                    Derivatives {
                        d: Vec::new(),
                        rest: Empty,
                    }
                } else {
                    Derivatives {
                        d: vec![(cs.clone(), Null)],
                        rest: Empty,
                    }
                }
            }
            Alt(ref cs, ref xs) => {
                let mut ds = Vec::with_capacity(if cs.len() > 0 { 1 } else { 0 } + xs.len());
                if cs.len() > 0 {
                    ds.push(Derivatives {
                        d: vec![(cs.clone(), Empty)],
                        rest: Null,
                    });
                }
                ds.extend(xs.iter().map(Differentiable::derivative));
                combine(&ds, |regexes| {
                    Alt(Vec::new(), regexes.iter().map(|r| (*r).clone()).collect())
                })
            }
            And(ref xs) => {
                let ds: Vec<_> = xs.iter().map(Differentiable::derivative).collect();
                combine(&ds, |regexes| {
                    And(regexes.iter().map(|r| (*r).clone()).collect())
                })
            }
            Not(ref x) => x.derivative().map(|r| Not(Box::new(r))),
            Cat(ref xs) => {
                let mut ds = Vec::new();
                for i in 0..xs.len() {
                    ds.push(xs[i].derivative().map(|r| {
                        let mut v = vec![r];
                        v.extend(xs[i + 1..].iter().cloned());
                        Cat(v)
                    }));
                    if !xs[i].nullable() {
                        break;
                    }
                }
                combine(&ds, |regexes| {
                    Alt(Vec::new(), regexes.iter().map(|r| (*r).clone()).collect())
                })
            }
            Kleene(ref x) => x.derivative().map(|r| Cat(vec![r, Kleene(x.clone())])),
        }
    }
}

// Derivatives of "regular vectors", as described in "Regular-expression derivatives reexamined" by Owens et al.
impl<T: Ord + Clone, R: Differentiable<T> + Clone> Differentiable<T> for Vec<R> {
    fn derivative(&self) -> Derivatives<T, Vec<R>> {
        let v: Vec<Derivatives<T, R>> = self.iter().map(Differentiable::derivative).collect();
        combine(&*v, |xs: &[&R]| xs.iter().map(|&x| x.clone()).collect())
    }
}
