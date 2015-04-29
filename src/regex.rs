use std::{vec, char, fmt, str};
use std::collections::BTreeSet;
use std::iter::Peekable;
use self::Regex::*;
use dfa::Normalize;

/// A regular expression over the alphabet `T`.
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone)]
pub enum Regex<T> {
    /// The null set. This never matches anything.
    Null,
    /// The empty string (matches exactly "").
    Empty,
    /// Matches any single character except the listed ones.
    Except(Vec<T>),
    /// Alternation (also known as disjunction). Matches any of the contained
    /// characters, as well as any string matched by a contained regex.
    Alt(Vec<T>, Vec<Regex<T>>),
    /// Conjunction. Matches iff all contained regexes match.
    And(Vec<Regex<T>>),
    /// Negation. Matches iff the contained regex does not match.
    Not(Box<Regex<T>>),
    /// Concatenation. Matches iff the contained regexes match in sequence.
    Cat(Vec<Regex<T>>),
    /// Kleene closure. Matches zero or more repetitions of the contained regex.
    Kleene(Box<Regex<T>>),
}

struct Puller<A, B, Fun: FnMut(A) -> Result<Vec<A>, B>, Iter: Iterator> {
    s: Iter,
    f: Fun,
    cur: Vec<vec::IntoIter<A>>,
}

trait Pull<A> {
    fn pull<B, Fun: FnMut(A) -> Result<Vec<A>, B>>(self, f: Fun) -> Puller<A, B, Fun, Self>;
}

impl<A, It: Iterator<Item=A>> Pull<A> for It {
    fn pull<B, Fun: FnMut(A) -> Result<Vec<A>, B>>(self, f: Fun) -> Puller<A, B, Fun, Self> {
        Puller {
            s: self,
            f: f,
            cur: Vec::new()
        }
    }
}

impl<A, B, Fun: FnMut(A) -> Result<Vec<A>, B>, Iter: Iterator<Item=A>> Iterator for Puller<A, B, Fun, Iter> {
    type Item = B;
    fn next(&mut self) -> Option<B> {
        let mut el = None;
        while let Some(mut it) = self.cur.pop() {
            if let Some(y) = it.next() {
                el = Some(y);
                self.cur.push(it);
                break;
            }
        }
        if el.is_none() {
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

impl<T: Ord> Normalize for Regex<T> {
    fn normalize(self) -> Self {
        let not_null = Not(Box::new(Null)); // FIXME: allocation here ;_;
        match self {
            Null => Null,
            Empty => Empty,
            Except(a) => Except(a.into_iter().collect::<BTreeSet<_>>().into_iter().collect()),
            Alt(a, xs) => {
                let mut chars = BTreeSet::new();
                for c in a.into_iter() {
                    chars.insert(c);
                }
                let mut xs: BTreeSet<_> = xs.into_iter().map(Normalize::normalize).pull(|x| match x {
                    Alt(cs, v) => {
                        for c in cs.into_iter() {
                            chars.insert(c);
                        }
                        Ok(v)
                    }
                    x => Err(x)
                }).collect();

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
                let mut xs: BTreeSet<_> = xs.into_iter().map(Normalize::normalize).pull(|x| match x {
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
            Not(x) => {
                match x.normalize() {
                    Not(y) => *y,
                    y => Not(Box::new(y))
                }
            }
            Cat(xs) => {
                let mut killed = false;
                let mut xs: Vec<_> = xs.into_iter().map(Normalize::normalize).pull(|x| match x {
                    Cat(v) => Ok(v),
                    x => Err(x)
                }).filter(|x| match *x {
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
            Kleene(x) => {
                match x.normalize() {
                    Kleene(y) => Kleene(y),
                    Null => Empty,
                    Empty => Empty,
                    Except(ref chs) if chs.len() == 0 => not_null,
                    y => Kleene(Box::new(y)),
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
       : Kleene '+'
Cat : Kleene
    : Kleene Cat
Not : Cat
    : '~' Not
And : Not
    : Not '&' And
Alt : And
    : And '|' Alt
*/
#[derive(Copy, Clone, Debug)]
pub enum ParseError {
    UnexpectedEof(&'static str),
    UnexpectedChar(&'static str, char),
    BadRange(&'static str, char, char),
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::UnexpectedEof(s) => write!(f, "{}", s),
            ParseError::UnexpectedChar(s, c) => write!(f, "{}: `{}`", s, c),
            ParseError::BadRange(s, c, d) => write!(f, "{}: `{}-{}`", s, c, d),
        }
    }
}
struct Parser<I: Iterator<Item=char>> {
    it: Peekable<I>,
}
type Res<T> = Result<T, ParseError>;
impl<I: Iterator<Item=char>> Parser<I> {
    fn char(&mut self) -> Res<char> {
        match self.it.next() {
            Some('\\') => {
                match self.it.next() {
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
            && c != '*' && c != '+'
            && c != '~' && c != '.'
    }
    fn char_group(c: char) -> bool {
        c != ']'
    }
    fn chars(&mut self) -> Res<Vec<char>> {
        let mut v = Vec::new();
        loop {
            match self.it.peek() {
                Some(&c) if Parser::<I>::char_group(c) => {
                    let c = try!(self.char());
                    if let Some(&'-') = self.it.peek() {
                        self.it.next();
                        if let None = self.it.peek() {
                            return Err(ParseError::UnexpectedEof("unterminated range"));
                        }
                        let d = try!(self.char());
                        // FIXME: This should be an inclusive range.
                        for x in (c as u64)..(d as u64 + 1) {
                            if let Some(x) = char::from_u32(x as u32) {
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
    fn atom(&mut self) -> Res<Regex<char>> {
        match self.it.peek() {
            Some(&'(') => {
                self.it.next();
                let r = try!(self.alt());
                match self.it.next() {
                    Some(')') => Ok(r),
                    Some(c) => Err(ParseError::UnexpectedChar("unexpected character", c)),
                    None => Err(ParseError::UnexpectedEof("unmatched '('")),
                }
            }
            Some(&'[') => {
                self.it.next();
                let except = if let Some(&'^') = self.it.peek() {
                    self.it.next();
                    true
                } else {
                    false
                };
                let r = try!(self.chars());
                match self.it.next() {
                    Some(']') => Ok(if except {
                        Except(r)
                    } else {
                        Alt(r, Vec::new())
                    }),
                    Some(c) => Err(ParseError::UnexpectedChar("bad character for character class", c)),
                    None => Err(ParseError::UnexpectedEof("unmatched '['")),
                }
            }
            Some(&'.') => {
                self.it.next();
                Ok(Except(Vec::new()))
            }
            Some(_) => Ok(Alt(vec![try!(self.char())], Vec::new())),
            None => panic!("atom not nullable"),
        }
    }
    fn atom_first(c: char) -> bool {
        c == '(' || c == '[' || c == '.' || Parser::<I>::char_first(c)
    }
    fn kleene(&mut self) -> Res<Regex<char>> {
        let mut r = try!(self.atom());
        loop {
            match self.it.peek() {
                Some(&'*') => {
                    self.it.next();
                    r = Kleene(Box::new(r))
                }
                Some(&'+') => {
                    self.it.next();
                    r = Cat(vec![r.clone(), Kleene(Box::new(r))])
                }
                _ => break,
            }
        }
        Ok(r)
    }
    fn kleene_first(c: char) -> bool {
        Parser::<I>::atom_first(c)
    }
    fn cat(&mut self) -> Res<Regex<char>> {
        let mut r = Vec::new();
        loop {
            match self.it.peek() {
                Some(&c) if Parser::<I>::kleene_first(c) => r.push(try!(self.kleene())),
                _ => break,
            }
        }
        Ok(Cat(r))
    }
    fn not(&mut self) -> Res<Regex<char>> {
        match self.it.peek() {
            Some(&'~') => {
                self.it.next();
                Ok(Not(Box::new(try!(self.not()))))
            }
            _ => self.cat()
        }
    }
    fn and(&mut self) -> Res<Regex<char>> {
        let mut r = vec![try!(self.not())];
        loop {
            match self.it.peek() {
                Some(&'&') => {
                    self.it.next();
                    r.push(try!(self.not()));
                }
                _ => break,
            }
        }
        Ok(And(r))
    }
    fn alt(&mut self) -> Res<Regex<char>> {
        let mut r = vec![try!(self.and())];
        loop {
            match self.it.peek() {
                Some(&'|') => {
                    self.it.next();
                    r.push(try!(self.and()));
                }
                _ => break,
            }
        }
        Ok(Alt(Vec::new(),r))
    }
    fn parse(it: I) -> Res<Regex<char>> {
        let mut parser = Parser { it: it.peekable() };
        let r = try!(parser.alt());
        if let Some(c) = parser.it.next() {
            Err(ParseError::UnexpectedChar("bad character in regex", c))
        } else {
            Ok(r)
        }
    }
}

impl str::FromStr for Regex<char> {
    type Err = ParseError;
    /// Parse a string as a regular expression.
    fn from_str(s: &str) -> Result<Regex<char>, ParseError> {
        Parser::parse(s.chars())
    }
}

impl<T> Regex<T> {
    /// Tests whether a regular expression is nullable, i.e. whether it matches
    /// the empty string.
    pub fn nullable(&self) -> bool {
        // FIXME: This could be inefficient. Try to cache it in the data structure
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
}
