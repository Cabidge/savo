mod lexing;

fn main() {
    lexing::lex("let x = 12;").unwrap();
}
