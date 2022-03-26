pub use parser::{ Parser, Stmt, StmtKind, CondStmt, Expr, ExprKind };

mod parser;

use crate::lexing::{ self, Token };

pub fn parse(src: &str) -> Result<Vec<Stmt>, ()> {
    let tokens = lexing::lex(src)?;
    parse_tokens(tokens)
}

pub fn parse_tokens(tokens: Vec<Token>) -> Result<Vec<Stmt>, ()> {
    let parser = Parser::new(tokens);

    match parser.parse() {
        Ok(root) => Ok(root),
        Err(errs) => {
            for err in errs.iter() {
                println!("{:?}", err);
            }
            Err(())
        },
    }
}