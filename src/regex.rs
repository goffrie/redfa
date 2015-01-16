use std::{vec, char, mem};
use std::collections::BTreeSet;
use std::iter::{self, Peekable};
use std::cmp::Ordering;
use self::Regex::*;

#[derive(PartialOrd,Ord,PartialEq,Eq,Show,Clone)]
pub enum Regex {
    Null, // the null set (never matches)
    Empty, // the empty string (matches exactly "")
    Except(Vec<char>), // any character except these
    Alt(Vec<char>, Vec<Regex>), // alternation (disjunction)
    And(Vec<Regex>), // conjunction
    Not(Box<Regex>), // negation
    Cat(Vec<Regex>), // concatenation
    Kleene(Box<Regex>), // Kleene closure
}

struct Puller<A, Fun: FnMut(A) -> Result<Vec<A>, A>, Iter: Iterator> {
    s: Iter,
    f: Fun,
    cur: Vec<vec::IntoIter<A>>,
}

trait Pull<A> {
    fn pull<Fun: FnMut(A) -> Result<Vec<A>, A>>(self, f: Fun) -> Puller<A, Fun, Self>;
}

impl<A, It: Iterator<Item=A>> Pull<A> for It {
    fn pull<Fun: FnMut(A) -> Result<Vec<A>, A>>(self, f: Fun) -> Puller<A, Fun, Self> {
        Puller {
            s: self,
            f: f,
            cur: Vec::new()
        }
    }
}

impl<A, Fun: FnMut(A) -> Result<Vec<A>, A>, Iter: Iterator<Item=A>> Iterator for Puller<A, Fun, Iter> {
    type Item = A;
    fn next(&mut self) -> Option<A> {
        let mut el = None;
        while self.cur.len() > 0 {
            let l = self.cur.len();
            if let Some(y) = self.cur[l-1].next() {
                el = Some(y);
                break;
            } else {
                self.cur.pop();
            }
        }
        if let None = el {
            el = self.s.next();
        }
        match el {
            Some(val) => {
                match (self.f)(val) {
                    Ok(v) => {
                        self.cur.push(v.into_iter());
                        self.next()
                    }
                    Err(it) => {
                        Some(it)
                    }
                }
            }
            None => None
        }
    }
}

impl Regex {
    pub fn simplify(self) -> Regex {
        let not_null = Not(box Null); // FIXME: allocation here ;_;
        match self {
            Null => Null,
            Empty => Empty,
            Except(a) => Except(a.into_iter().collect::<BTreeSet<_>>().into_iter().collect()),
            Alt(a, xs) => {
                let mut chars = BTreeSet::new();
                for c in a.into_iter() {
                    chars.insert(c);
                }
                let mut xs = xs.into_iter().map(Regex::simplify).pull(|&mut: x| match x {
                    Alt(cs, v) => {
                        for c in cs.into_iter() {
                            chars.insert(c);
                        }
                        Ok(v)
                    }
                    x => Err(x)
                }).collect::<BTreeSet<Regex>>();

                if xs.contains(&not_null) {
                    return not_null;
                }
                xs.remove(&Null);

                let chars: Vec<_> = chars.into_iter().collect();
                let mut xs: Vec<_> = xs.into_iter().collect();

                match (chars.len(), xs.len()) {
                    (0, 0) => Null,
                    (0, 1) => xs.pop().unwrap(),
                    _      => Alt(chars, xs)
                }
            }
            And(xs) => {
                let mut xs: BTreeSet<Regex> = xs.into_iter().map(Regex::simplify).pull(|&: x| match x {
                    And(v) => Ok(v),
                    x => Err(x)
                }).collect();
                if xs.contains(&Null) {
                    return Null;
                }
                xs.remove(&not_null);
                let mut xs: Vec<_> = xs.into_iter().collect();
                match xs.len() {
                    0 => not_null,
                    1 => xs.pop().unwrap(),
                    _ => And(xs)
                }
            }
            Not(box x) => {
                match x.simplify() {
                    Not(box y) => y,
                    y => Not(box y)
                }
            }
            Cat(xs) => {
                let mut killed = false;
                let mut xs: Vec<_> = xs.into_iter().map(Regex::simplify).pull(|&: x| match x {
                    Cat(v) => Ok(v),
                    x => Err(x)
                }).filter(|&mut: x| match *x {
                    Null => {
                        killed = true;
                        false
                    }
                    Empty => false,
                    _ => true
                }).collect();
                if killed {
                    return Null;
                }
                match xs.len() {
                    0 => Empty,
                    1 => xs.pop().unwrap(),
                    _ => Cat(xs)
                }
            }
            Kleene(box x) => {
                match x.simplify() {
                    Kleene(y) => Kleene(y),
                    Null => Empty,
                    Empty => Empty,
                    Except(ref chs) if chs.len() == 0 => not_null,
                    y => Kleene(box y),
                }
            }
        }
    }
}

/*
Char : NORMAL
     : '\' CHAR
Chars :
      : Char Chars
Atom : Char
     : '(' Alt ')'
     : '[' Chars ']'
Kleene : Atom
       : Kleene '*'
Cat : Kleene
    : Kleene Cat
Not : Cat
    : '~' Not
And : Not
    : Not '&' And
Alt : And
    : And '|' Alt
*/
#[derive(Copy,Show)]
pub enum ParseError {
    UnexpectedEof(&'static str),
    UnexpectedChar(&'static str, char),
    BadRange(&'static str, char, char),
}
type Pk<I> = Peekable<char, I>;
type Res = Result<Regex, ParseError>;
impl Regex {
    fn parse<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
        fn char<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Result<char, ParseError> {
            match it.next() {
                Some('\\') => {
                    match it.next() {
                        Some('r') => Ok('\r'),
                        Some('n') => Ok('\n'),
                        Some('t') => Ok('\t'),
                        Some(c) => Ok(c),
                        None => Err(ParseError::UnexpectedEof("unfollowed '\\'"))
                    }
                }
                Some(c) => {
                    Ok(c)
                }
                None => panic!("char not nullable")
            }
        }
        fn char_first(c: char) -> bool {
            c != '~' && c != '|' && c != '&'
                && c != '[' && c != ']'
                && c != '(' && c != ')'
                && c != '*' && c != '~'
                && c != '.'
        }
        fn char_group(c: char) -> bool {
            c != ']'
        }
        fn chars<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Result<Vec<char>, ParseError> {
            let mut v = Vec::new();
            loop {
                match it.peek() {
                    Some(&c) if char_group(c) => {
                        let c = try!(char(it));
                        if let Some(&'-') = it.peek() {
                            it.next();
                            if let None = it.peek() {
                                return Err(ParseError::UnexpectedEof("unterminated range"));
                            }
                            let d = try!(char(it));
                            for x in iter::range_inclusive(c as u32, d as u32) {
                                if let Some(x) = char::from_u32(x) {
                                    v.push(x);
                                } else {
                                    return Err(ParseError::BadRange("range contains bad codepoints", c, d));
                                }
                            }
                        } else {
                            v.push(c);
                        }
                    }
                    _ => {
                        break;
                    }
                }
            }
            Ok(v)
        }
        fn atom<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
            match it.peek() {
                Some(&'(') => {
                    it.next();
                    let r = try!(alt(it));
                    match it.next() {
                        Some(')') => Ok(r),
                        Some(c) => Err(ParseError::UnexpectedChar("unexpected character", c)),
                        None => Err(ParseError::UnexpectedEof("unmatched '('")),
                    }
                }
                Some(&'[') => {
                    it.next();
                    let except = if let Some(&'^') = it.peek() {
                        it.next();
                        true
                    } else {
                        false
                    };
                    let r = try!(chars(it));
                    match it.next() {
                        Some(']') => Ok(if except {
                            Except(r)
                        } else {
                            Alt(r, Vec::new())
                        }),
                        Some(c) => Err(ParseError::UnexpectedChar("bad char for character class", c)),
                        None => Err(ParseError::UnexpectedEof("unmatched '['")),
                    }
                }
                Some(&'.') => {
                    it.next();
                    Ok(Except(Vec::new()))
                }
                Some(_) => Ok(Alt(vec![try!(char(it))], Vec::new())),
                None => panic!("atom not nullable"),
            }
        }
        fn atom_first(c: char) -> bool {
            c == '(' || c == '[' || c == '.' || char_first(c)
        }
        fn kleene<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
            let mut r = try!(atom(it));
            loop {
                match it.peek() {
                    Some(&'*') => {
                        it.next();
                        r = Kleene(box r)
                    }
                    _ => break,
                }
            }
            Ok(r)
        }
        fn kleene_first(c: char) -> bool {
            atom_first(c)
        }
        fn cat<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
            let mut r = Vec::new();
            loop {
                match it.peek() {
                    Some(&c) if kleene_first(c) => r.push(try!(kleene(it))),
                    _ => break,
                }
            }
            Ok(Cat(r))
        }
        fn cat_first(c: char) -> bool {
            kleene_first(c) || c == '&' || c == '|' || c == ')'
        }
        fn not<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
            match it.peek() {
                Some(&'~') => {
                    it.next();
                    Ok(Not(box try!(not(it))))
                }
                _ => {
                    cat(it)
                }
            }
        }
        fn not_first(c: char) -> bool {
            c == '~' || cat_first(c)
        }
        fn and<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
            let mut r = vec![try!(not(it))];
            loop {
                match it.peek() {
                    Some(&'&') => {
                        it.next();
                        r.push(try!(not(it)));
                    }
                    _ => break,
                }
            }
            Ok(And(r))
        }
        fn alt<I: Iterator<Item=char>>(it: &mut Pk<I>) -> Res {
            let mut r = vec![try!(and(it))];
            loop {
                match it.peek() {
                    Some(&'|') => {
                        it.next();
                        r.push(try!(and(it)));
                    }
                    _ => break,
                }
            }
            Ok(Alt(Vec::new(),r))
        }
        alt(it)
    }
    pub fn new(s: &str) -> Result<Regex, ParseError> {
        let mut iter = s.chars().peekable();
        let r = try!(Regex::parse(&mut iter));
        if let Some(c) = iter.next() {
            Err(ParseError::UnexpectedChar("bad char in regex", c))
        } else {
            Ok(r)
        }
    }
}

#[derive(Show,Clone)]
pub struct Derivatives {
    pub d: Vec<(Vec<char>, Regex)>,
    pub rest: Regex,
}

impl Derivatives {
    pub fn map<F: FnMut(Regex) -> Regex>(mut self, mut f: F) -> Derivatives {
        for &mut (_, ref mut r) in self.d.iter_mut() {
            // have to swap something in in the meantime
            let v = mem::replace(r, Null);
            *r = f(v);
        }
        self.rest = f(self.rest);
        self
    }
}

struct Union<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> {
    a : Peekable<T, It1>,
    b : Peekable<T, It2>,
}
fn union<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Union<T, It1, It2> {
    Union { a: a.peekable(), b: b.peekable() }
}
impl<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> Iterator for Union<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        match match self.a.peek() {
            Some(av) => match self.b.peek() {
                Some(bv) => av.cmp(bv),
                None => Ordering::Less,
            },
            None => Ordering::Greater,
        } {
            Ordering::Less => {
                self.a.next()
            }
            Ordering::Greater => {
                self.b.next()
            }
            Ordering::Equal => {
                self.a.next();
                self.b.next()
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a1, a2) = self.a.size_hint();
        let (b1, b2) = self.b.size_hint();
        (a1 + b1,
         if let (Some(a2), Some(b2)) = (a2, b2) {
             Some(a2 + b2)
         } else {
             None
         })
    }
}

struct Inter<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> {
    a : Peekable<T, It1>,
    b : Peekable<T, It2>,
}
fn inter<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Inter<T, It1, It2> {
    Inter { a: a.peekable(), b: b.peekable() }
}
impl<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> Iterator for Inter<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        loop {
            match if let (Some(av), Some(bv)) = (self.a.peek(), self.b.peek()) {
                av.cmp(bv)
            } else {
                return None
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

struct Subtract<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> {
    a : Peekable<T, It1>,
    b : Peekable<T, It2>,
}
fn subtract<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Subtract<T, It1, It2> {
    Subtract { a: a.peekable(), b: b.peekable() }
}
impl<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> Iterator for Subtract<T, It1, It2> {
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

enum CharSet {
    Just(Vec<char>),
    Not(Vec<char>),
}

impl CharSet {
    fn inter(&self, b: &[char]) -> CharSet {
        use self::CharSet::{Just, Not};
        match *self {
            Just(ref a) => {
                Just(inter(a.iter().map(|&: x| *x), b.iter().map(|&: x| *x)).collect())
            }
            Not(ref a) => {
                Just(subtract(b.iter().map(|&: x| *x), a.iter().map(|&: x| *x)).collect())
            }
        }
    }
    fn subtract(&self, b: &[char]) -> CharSet {
        use self::CharSet::{Just, Not};
        match *self {
            Just(ref a) => {
                Just(subtract(a.iter().map(|&: x| *x), b.iter().map(|&: x| *x)).collect())
            }
            Not(ref a) => {
                Not(union(a.iter().map(|&: x| *x), b.iter().map(|&: x| *x)).collect())
            }
        }
    }
}

fn combine<F: Fn(&[&Regex]) -> Regex>(v: &[Derivatives], f: F) -> Derivatives {
    fn go<'a, 'b, 'd, F: Fn(&[&Regex]) -> Regex>(v: &'a [Derivatives], f: &F, what: CharSet, res: &'b mut Vec<&'a Regex>, out: &'d mut Derivatives) {
        if let CharSet::Just(ref v) = what {
            if v.len() == 0 {
                // prune
                return;
            }
        }
        if v.len() == 0 {
            let reg = f(&res[]);
            match what {
                CharSet::Just(c) => out.d.push((c, reg)),
                CharSet::Not(_) => {
                    assert!(out.rest == Null);
                    out.rest = reg;
                }
            }
            return;
        }
        let (first, rest) = v.split_at(1);
        let first = &first[0];
        let mut all_chars = Vec::new();
        for &(ref chars, ref reg) in first.d.iter() {
            all_chars = union(all_chars.into_iter(), chars.iter().map(|&: x| *x)).collect();
            let inter = what.inter(&chars[]);
            res.push(reg);
            go(rest, f, inter, res, out);
            res.pop();
        }
        let inter = what.subtract(&all_chars[]);
        res.push(&first.rest);
        go(rest, f, inter, res, out);
        res.pop();
    }
    let mut result = Derivatives {
        d: Vec::new(),
        rest: Null // FIXME: Would prefer a slightly better init value than this
    };
    let mut regexes = Vec::new();
    go(v, &f, CharSet::Not(Vec::new()), &mut regexes, &mut result);
    result
}

impl Regex {
    // FIXME: This could be inefficient. Try to cache it in the data structure
    pub fn nullable(&self) -> bool {
        match *self {
            Null => false,
            Empty => true,
            Except(_) => false,
            Alt(_, ref xs) => xs.iter().any(Regex::nullable),
            And(ref xs) => xs.iter().all(Regex::nullable),
            Not(ref x) => !x.nullable(),
            Cat(ref xs) => xs.iter().all(Regex::nullable),
            Kleene(_) => true,
        }
    }
    pub fn derivative(&self) -> Derivatives {
        match *self {
            Null => Derivatives { d: Vec::new(), rest: Null },
            Empty => Derivatives { d: Vec::new(), rest: Null },
            Except(ref cs) => {
                if cs.len() == 0 {
                    Derivatives { d: Vec::new(), rest: Empty }
                } else {
                    Derivatives { d: vec![(cs.clone(), Null)], rest: Empty }
                }
            }
            Alt(ref cs, ref xs) => {
                let mut ds = Vec::with_capacity(if cs.len() > 0 { 1 } else { 0 } + xs.len());
                if cs.len() > 0 {
                    ds.push(Derivatives { d: vec![(cs.clone(), Empty)], rest: Null });
                }
                ds.extend(xs.iter().map(Regex::derivative));
                combine(&ds[], |&: regexes| Alt(Vec::new(), regexes.iter().map(|&: r| (*r).clone()).collect()))
            }
            And(ref xs) => {
                let ds: Vec<_> = xs.iter().map(Regex::derivative).collect();
                combine(&ds[], |&: regexes| And(regexes.iter().map(|&: r| (*r).clone()).collect()))
            }
            Not(box ref x) => x.derivative().map(|&: r| Not(box r)),
            Cat(ref xs) => {
                let mut ds = Vec::new();
                for i in 0..xs.len() {
                    ds.push(xs[i].derivative().map(|&: r| {
                        let mut v = vec![r];
                        v.push_all(xs.slice_from(i+1));
                        Cat(v)
                    }));
                    if !xs[i].nullable() {
                        break;
                    }
                }
                combine(&ds[], |&: regexes| Alt(Vec::new(), regexes.iter().map(|&: r| (*r).clone()).collect()))
            }
            Kleene(box ref x) => x.derivative().map(|&: r| Cat(vec![r, Kleene(box x.clone())])),
        }
    }
}
