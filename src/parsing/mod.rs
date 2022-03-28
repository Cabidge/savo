pub use parser::{ Parser, Stmt, StmtKind, CondStmt, Expr, ExprKind };
use parser::ErrorKind::*;

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
                let msg = match err.kind {
                    ExpectIdentAfterLet => "Expected identifier after let keyword".to_string(),
                    ExpectParenOrEqAfterLetIdent => "Expected `=` or `(` after a let-declaration".to_string(),
                    DuplicateParam => format!("Duplicate parameter `{}`", err.token.get_ident().unwrap()),
                    ExpectIdentParam => "Expected identifier in parameter list".to_string(),
                    ExpectCommaOrParenAfterParam => "Expected `,` or `(` after a parameter".to_string(),
                    ExpectCommaOrParenAfterArg => "Expected `,` or `(` after an argument".to_string(),
                    ExpectBraceAfterParams => "Expected a `{` after parameter list".to_string(),
                    ExpectClosingBrace => "Unmatched `{`".to_string(),
                    ExpectSemicolonAfterStmt => "Expected `;` after statement".to_string(),
                    StmtAfterTerminator => "Unreachable statement after unconditional terminator".to_string(),
                    UnexpectedToken => format!("Unexpected token {}", err.token.kind),
                    UnmatchedParen => "Unmatched `(`".to_string(),
                };

                // TODO: Make it so it's not needed to check this
                // Perhaps make a Location enum or something like that
                let at = if err.token.line == 0 {
                    "EOF".to_string()
                } else {
                    format!("{}:{}", err.token.line, err.token.col)
                };

                eprintln!("Error: {} at {}", msg, at);
            }
            Err(())
        },
    }
}