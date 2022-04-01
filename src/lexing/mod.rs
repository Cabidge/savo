pub use lexer::{ Token, TokenKind };

mod lexer;
mod char_stream;

use lexer::ErrorKind::*;

pub fn lex(src: &str) -> Result<Vec<Token>, ()> {
    let lexer = lexer::Lexer::new(src);
    let mut has_error = false;
    let mut tokens = Vec::new();

    for token in lexer {
        match token.kind {
            TokenKind::Error(e) => {
                let msg = match e {
                    UnexpectedChar(ch) => format!("Unexpected character `{}`", ch),
                    InvalidValue(v) => format!("Invalid value `{}`", v),
                    UnknownEscapeChar(ch) => format!("Unknown escape character `\\{}`", ch),
                    UnmatchedSingleQuote => "Unmatched single quote".to_string(),
                    UnmatchedDoubleQuote => "Unmatched double quote".to_string(),
                    DunderReservedKeyword => "Double underscore identifiers are reserved".to_string()
                };

                eprintln!("Error: {} at {}:{}", msg, token.line, token.col);

                has_error = true;
            },
            _ => tokens.push(token),
        }
    }

    if has_error {
        Err(())
    } else {
        Ok(tokens)
    }
}