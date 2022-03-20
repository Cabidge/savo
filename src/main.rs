#![feature(never_type)]

use std::process;
use std::fs;

mod lexing;
mod parsing;

fn main() {
    let source = fs::read_to_string("src/main.savo").unwrap();

    let tokens = match lexing::lex(&source) {
        Ok(tokens) => tokens,
        Err(_) => process::exit(1),
    };

    for tok in tokens {
        print!("{}", tok.kind);
    }

    println!("");
}
