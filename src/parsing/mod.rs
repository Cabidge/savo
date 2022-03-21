pub use parser::{ Parser, Root, Expr };
use crate::lexing::{ self, Token };

mod parser;

pub fn parse(src: &str) -> Result<Root, ()> {
    let tokens = lexing::lex(src)?;
    parse_tokens(tokens)
}

pub fn parse_tokens(tokens: Vec<Token>) -> Result<Root, ()> {
    let parser = Parser::new(tokens);

    match parser.parse() {
        Ok(root) => Ok(root),
        Err(errs) => {
            todo!();
        },
    }
}