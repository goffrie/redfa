#![crate_type="bin"]
extern crate regex_dfa;
use regex_dfa::*;
use regex_dfa::derivatives::Differentiable;
use regex_dfa::dfa::Normalize;
fn main() {
    let mut line = String::new();
    while let Ok(_) = std::io::stdin().read_line(&mut line) {
        match line.trim().parse::<Regex<char>>() {
            Err(e) => println!("error: {}", e),
            Ok(x) => {
                println!("ok: {:?}", x);
                let x = x.normalize();
                println!("{:?}", x);
                println!("{:?}", x.derivative().map(Normalize::normalize));
                let (dfa, _mapping) = Dfa::from_derivatives(vec![x, Regex::Null]);
                let dfa = dfa.map(|reg| reg.nullable());
                println!("DFA: {:?}\n", dfa);
                let mdfa = dfa.minimize().map(|x| *x);
                println!("Minimized DFA: {:?}\n", mdfa);
                println!("dfa == mdfa: {:?}\n", dfa == mdfa);
            }
        }
        line = String::new();
    }
}
