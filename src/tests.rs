use regex::*;
use regex::Regex::*;
use derivatives::*;
use dfa::*;

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
