mod lexing;

fn main() {
    for tok in lexing::lex("let x = 12;").unwrap() {
        println!("{}", tok.kind);
    }
}
