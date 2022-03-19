use lexer::{ TokenKind, ErrorKind::* };
mod lexer;
mod char_stream;

pub fn lex(src: &str) -> Result<Vec<lexer::Token>, ()> {
    let lexer = lexer::Lexer::new(src);
    let mut has_error = false;
    let mut tokens = Vec::new();

    for token in lexer {
        match token.kind {
            TokenKind::Error(e) => {
                let msg = match e {
                    UnexpectedChar(ch) => format!("Unexpected character `{}`", ch),
                    InvalidValue(v) => format!("Invalid value `{}`", v),
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