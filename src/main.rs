mod lexing;

fn main() -> Result<(),()> {
    for tok in lexing::lex("hello <- & 12;")? {
        println!("{}", tok.kind);
    }

    Ok(())
}
