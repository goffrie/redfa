use regex::*;
use regex::Regex::*;
use dfa::*;
use std::collections::BTreeMap;

#[test]
fn test_regex_parse() {
    assert_eq!("".parse::<Regex<char>>().unwrap().normalize(), Empty);
    assert_eq!("a".parse::<Regex<char>>().unwrap().normalize(), Alt(vec!['a'], vec![]));
    assert_eq!("abc".parse::<Regex<char>>().unwrap().normalize(),
        Cat(vec![Alt(vec!['a'], vec![]),
                 Alt(vec!['b'], vec![]),
                 Alt(vec!['c'], vec![])]));
    assert_eq!("ab*".parse::<Regex<char>>().unwrap().normalize(),
        Cat(vec![Alt(vec!['a'], vec![]),
                 Kleene(Box::new(Alt(vec!['b'], vec![])))]));
    assert_eq!("[a-d)]".parse::<Regex<char>>().unwrap().normalize(),
        Alt(vec![')', 'a', 'b', 'c', 'd'], vec![]));
    assert_eq!("[^a-d]".parse::<Regex<char>>().unwrap().normalize(),
        Except(vec!['a', 'b', 'c', 'd']));
    assert_eq!("[]".parse::<Regex<char>>().unwrap().normalize(), Null);
    assert_eq!("~".parse::<Regex<char>>().unwrap().normalize(), Not(Box::new(Empty)));
    assert_eq!("~[]".parse::<Regex<char>>().unwrap().normalize(), Not(Box::new(Null)));
    assert_eq!("a.|bc*".parse::<Regex<char>>().unwrap().normalize(),
        Alt(vec![], vec![
            Cat(vec![Alt(vec!['a'], vec![]), Except(vec![])]),
            Cat(vec![Alt(vec!['b'], vec![]), Kleene(Box::new(Alt(vec!['c'], vec![])))])
        ]));
    assert_eq!("a|b&c".parse::<Regex<char>>().unwrap().normalize(),
        Alt(vec!['a'], vec![And(vec![Alt(vec!['b'], vec![]), Alt(vec!['c'], vec![])])]));
    assert_eq!("a&b|c".parse::<Regex<char>>().unwrap().normalize(),
        Alt(vec!['c'], vec![And(vec![Alt(vec!['a'], vec![]), Alt(vec!['b'], vec![])])]));
    assert_eq!("~a".parse::<Regex<char>>().unwrap().normalize(),
        Not(Box::new(Alt(vec!['a'], vec![]))));
    assert_eq!("~b*".parse::<Regex<char>>().unwrap().normalize(),
        Not(Box::new(Kleene(Box::new(Alt(vec!['b'], vec![]))))));
    assert_eq!("a&b*".parse::<Regex<char>>().unwrap().normalize(),
        And(vec![Alt(vec!['a'], vec![]), Kleene(Box::new(Alt(vec!['b'], vec![])))]));
    assert_eq!("(a|b)&([cd]|d)*".parse::<Regex<char>>().unwrap().normalize(),
        And(vec![
            Alt(vec!['a', 'b'], vec![]),
            Kleene(Box::new(Alt(vec!['c', 'd'], vec![])))
        ]));
    assert_eq!("\\[".parse::<Regex<char>>().unwrap().normalize(), Alt(vec!['['], vec![]));
    assert_eq!("[\\[]".parse::<Regex<char>>().unwrap().normalize(), Alt(vec!['['], vec![]));
    assert_eq!("[\\]]".parse::<Regex<char>>().unwrap().normalize(), Alt(vec![']'], vec![]));
    assert_eq!("(\\))".parse::<Regex<char>>().unwrap().normalize(), Alt(vec![')'], vec![]));
}

#[test]
fn test_regex_parse_error() {
    assert!("*".parse::<Regex<char>>().is_err());
    assert!("*a".parse::<Regex<char>>().is_err());
    assert!("a~b".parse::<Regex<char>>().is_err());
    assert!("a*~".parse::<Regex<char>>().is_err());
    assert!("[asdf".parse::<Regex<char>>().is_err());
    assert!("[a-z".parse::<Regex<char>>().is_err());
    assert!("&*".parse::<Regex<char>>().is_err());
    assert!("|*".parse::<Regex<char>>().is_err());
    assert!("(".parse::<Regex<char>>().is_err());
    assert!("(()".parse::<Regex<char>>().is_err());
    assert!(")()".parse::<Regex<char>>().is_err());
    assert!(")(".parse::<Regex<char>>().is_err());
    assert!("(]".parse::<Regex<char>>().is_err());
    assert!("(])".parse::<Regex<char>>().is_err());
    assert!("[\u{d7ff}-\u{e000}]".parse::<Regex<char>>().is_err());
}

macro_rules! dfa_add {
    ($dfa: expr, $from: expr, $by: expr, $to: expr) => ({
        $dfa.states[$from].by_char.insert($by, $to);
    });
    ($dfa: expr, $from: expr, $to: expr) => ({
        $dfa.states[$from].default = $to;
    });
    ($dfa: expr, $from: expr) => ({
        $dfa.states[$from].value = true;
    });
}

macro_rules! dfa {
    ($last: expr; $($($xs:expr),*;)*) => ({
        let last: usize = $last;
        let mut dfa = Dfa { states: vec![State { by_char: BTreeMap::new(), default: last as u32, value: false }; last+1] };
        $(dfa_add!(dfa, $($xs),*);)*
        dfa
    });
}

macro_rules! assert_equiv {
    ($a: expr, $b: expr) => (assert_eq!($a.minimize(), $b.minimize()));
}
macro_rules! assert_not_equiv {
    ($a: expr, $b: expr) => (assert!(!$a.equiv(&$b)));
}

#[test]
fn test_dfa_minimize() {
    assert_eq!(dfa! {
        9;
        0, 'a', 2;
        0, 'b', 3;
        0;
        1, 'a', 1;
        1, 'b', 1;
        2, 'a', 4;
        2, 'b', 5;
        2;
        3, 'a', 2;
        3, 'b', 6;
        3;
        4, 'a', 2;
        4, 'b', 3;
        4;
        5, 'a', 1;
        5, 'b', 7;
        6, 'a', 2;
        6, 'b', 6;
        6;
        7, 'a', 8;
        7, 'b', 5;
        7;
        8, 'a', 8;
        8, 'b', 5;
        8;
    }.minimize().map(|x| *x), dfa! {
        4;
        0, 'a', 1;
        0, 'b', 0;
        0;
        1, 'a', 0;
        1, 'b', 2;
        1;
        2, 'b', 3;
        3, 'a', 3;
        3, 'b', 2;
        3;
    });
}

#[test]
fn test_regex_to_dfa() {
    fn to_dfa(s: &str) -> Dfa<char, bool> {
        Dfa::from_derivatives(vec![s.parse::<Regex<char>>().unwrap()]).0.map(|r| r.nullable())
    }
    assert_equiv!(to_dfa(""), dfa! { 1; 0; });
    assert_equiv!(to_dfa("[ab][b-d]"), dfa! {
        3;
        0, 'a', 1;
        0, 'b', 1;
        1, 'b', 2;
        1, 'c', 2;
        1, 'd', 2;
        2;
    });
    assert_equiv!(to_dfa("~ab"), dfa! {
        4;
        0, 'a', 1;
        0, 3;
        0;
        1, 'b', 2;
        1, 3;
        1;
        2, 3;
        3, 3;
        3;
    });
    assert_equiv!(to_dfa("a*b"), dfa! {
        2;
        0, 'a', 0;
        0, 'b', 1;
        1;
    });
    assert_equiv!(to_dfa("(b(a(bb)*a)*(b|a(bb)*ba)|a(bb)*(ba(a(bb)*a)*(b|a(bb)*ba)|a))*(b(a(bb)*a)*|a(bb)*ba(a(bb)*a)*)"), dfa! {
        4;
        0, 'a', 2;
        0, 'b', 1;
        1, 'a', 3;
        1, 'b', 0;
        1;
        2, 'a', 0;
        2, 'b', 3;
        3, 'a', 1;
        3, 'b', 2;
    });
    assert_equiv!(to_dfa("a*b"), to_dfa("(a|a*(a*)*)(b&b*)"));
    assert_not_equiv!(to_dfa("a*b"), to_dfa("(a|a*(a*)*)(b&a*)"));
    assert_equiv!(to_dfa("~a"), to_dfa("|[^a]|...*"));
    assert_not_equiv!(to_dfa("~a"), to_dfa("[^a]"));
    assert_equiv!(to_dfa("(a.*b)&(.[b-d]*.)&(.*c..)"), to_dfa("a[b-d]*c[b-d]b"));
}
