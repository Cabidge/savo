use std::process;

mod lexing;

fn main() {
    let tokens = match lexing::lex("hello <- & 12") {
        Ok(tokens) => tokens,
        Err(_) => process::exit(1),
    };

    for tok in tokens {
        println!("{}", tok.kind);
    }
}
