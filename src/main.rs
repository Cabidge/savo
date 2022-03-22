#![feature(never_type)]

use std::process;
use std::fs;

mod lexing;
mod parsing;
mod resolving;
mod compiling;

fn main() {
    let source = fs::read_to_string("src/main.savo").unwrap();

    let tokens = match lexing::lex(&source) {
        Ok(tokens) => tokens,
        Err(_) => process::exit(1),
    };

    for tok in tokens.iter() {
        print!("{}", tok.kind);
    }
    println!("");

    let ast_root = match parsing::parse_tokens(tokens) {
        Ok(root) => root,
        Err(_) => process::exit(1),
    };

    for expr in ast_root.iter() {
        println!("{}", expr);
    }
}
