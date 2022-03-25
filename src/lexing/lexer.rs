use std::fmt;
use super::char_stream::CharStream;

pub struct Lexer {
    stream: CharStream,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Value(f64),
    Let,
    Ident(String),

    If,
    Else,

    EQ, // =
    LT, // <
    GT, // >
    NE, // <>
    GE, // >=
    LE, // <=

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /

    LArrow, // <-
    RArrow, // ->
    RFatArrow, // =>

    Dump, // >>
    Char(char),
    Str(String),

    Comma,     // ,
    Semicolon, // ;

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    Error(ErrorKind),
    EOF,
}

#[derive(PartialEq, Clone, Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    InvalidValue(String),
    UnknownEscapeChar(char),
    UnmatchedSingleQuote,
    UnmatchedDoubleQuote,
    ReservedKeyword(&'static str),
}

impl Token {
    pub fn eof() -> Self {
        Token {
            kind: TokenKind::EOF,
            line: 0,
            col: 0,
        }
    }

    pub fn get_ident(&self) -> Option<String> {
        if let TokenKind::Ident(ident) = &self.kind {
            Some(ident.clone())
        } else {
            None
        }
    }
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        Lexer {
            stream: CharStream::new(src),
        }
    }

    pub fn next_token(&mut self) -> Token {
        while self.stream.current().is_whitespace() {
            self.stream.skip_while(char::is_whitespace);

            if self.stream.eat_current('#') {
                let current_line = self.stream.line();
                while self.stream.line() == current_line && self.stream.current() != '\0' {
                    self.stream.advance();
                }
            }
        }

        let line = self.stream.line();
        let col = self.stream.col();
        let kind = self.lex();

        Token {
            kind,
            line,
            col,
        }
    }

    fn lex(&mut self) -> TokenKind {
        if self.stream.eat_current('\0') {
            return TokenKind::EOF;
        }

        if self.stream.eat_current('=') {
            return if self.stream.eat_current('>') {
                TokenKind::RFatArrow // =>
            } else {
                TokenKind::EQ // =
            }
        }

        if self.stream.eat_current('<') {
            return if self.stream.eat_current('>') {
                TokenKind::NE // <>
            } else if self.stream.eat_current('=') {
                TokenKind::LE // <=
            } else if self.stream.eat_current('-') {
                TokenKind::LArrow // <-
            } else {
                TokenKind::LT // <
            };
        }

        if self.stream.eat_current('>') {
            return if self.stream.eat_current('>') {
                TokenKind::Dump // >>
            } else if self.stream.eat_current('=') {
                TokenKind::GE // >=
            } else {
                TokenKind::GT // >
            };
        }

        if self.stream.eat_current(',') {
            return TokenKind::Comma; // ;
        }

        if self.stream.eat_current(';') {
            return TokenKind::Semicolon; // ;
        }

        if self.stream.eat_current('+') {
            return TokenKind::Add; // +
        }

        if self.stream.eat_current('-') {
            return if self.stream.eat_current('>') {
                TokenKind::RArrow // ->
            } else {
                TokenKind::Sub // -
            };
        }

        if self.stream.eat_current('*') {
            return TokenKind::Mul;
        }

        if self.stream.eat_current('/') {
            return TokenKind::Div;
        }

        if self.stream.eat_current('(') {
            return TokenKind::LParen;
        }

        if self.stream.eat_current(')') {
            return TokenKind::RParen;
        }

        if self.stream.eat_current('{') {
            return TokenKind::LBrace;
        }

        if self.stream.eat_current('}') {
            return TokenKind::RBrace;
        }

        if self.stream.eat_current('\'') {
            return match self.anal_char() {
                Ok(ch) if self.stream.eat_current('\'') => TokenKind::Char(ch),
                Ok(_) => TokenKind::Error(ErrorKind::UnmatchedSingleQuote),
                Err(kind) => TokenKind::Error(kind),
            }
        }

        if self.stream.eat_current('"') {
            return self.lex_string();
        }

        if self.stream.current().is_numeric() {
            return self.lex_number();
        }

        if is_ident(self.stream.current()) {
            return self.lex_ident();
        }

        let current = self.stream.current();
        self.stream.advance();
        TokenKind::Error(ErrorKind::UnexpectedChar(current))
    }

    /// lol
    /// stands for "analyze character"
    /// like advance but handles escape characters
    fn anal_char(&mut self) -> Result<char, ErrorKind> {
        if self.stream.eat_current('\\') {
            // TODO: Add other escape chars
            return if self.stream.eat_current('n') {
                Ok('\n')
            } else if self.stream.eat_current('t') {
                Ok('\t')
            } else if self.stream.eat_current('\\') {
                Ok('\\')
            } else if self.stream.eat_current('\'') {
                Ok('\'')
            } else if self.stream.eat_current('"') {
                Ok('"')
            } else {
                Err(ErrorKind::UnknownEscapeChar(self.stream.take_current()))
            }
        }

        Ok(self.stream.take_current())
    }

    fn lex_string(&mut self) -> TokenKind {
        let mut string = String::new();

        while !self.stream.eat_current('"') {
            if self.stream.current() == '\0' {
                return TokenKind::Error(ErrorKind::UnmatchedDoubleQuote);
            }

            match self.anal_char() {
                Ok(ch) => string.push(ch),
                Err(err) => return TokenKind::Error(err),
            }
        }

        TokenKind::Str(string)
    }

    fn lex_number(&mut self) -> TokenKind {
        let mut num = String::new();
        num.push(self.stream.current());

        loop {
            match self.stream.advance() {
                '0'..='9' | '.' => num.push(self.stream.current()),
                '_' => (),
                _ => break,
            }
        }

        match num.parse::<f64>() {
            Ok(n) => TokenKind::Value(n),
            Err(_) => TokenKind::Error(ErrorKind::InvalidValue(num)),
        }
    }

    fn lex_ident(&mut self) -> TokenKind {
        let mut ident = String::new();

        // lol
        while {
            ident.push(self.stream.current());
            is_ident(self.stream.advance())
        } {}

        match ident.as_str() {
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "printf" => TokenKind::Error(ErrorKind::ReservedKeyword("printf")),
            _ => TokenKind::Ident(ident),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token.kind {
            TokenKind::EOF => None,
            _ => Some(token),
        }
    }
}

fn is_ident(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Value(val)   => write!(f, "[{}]", val),
            TokenKind::Let          => write!(f, "[let]"),
            TokenKind::Ident(ident) => write!(f, "['{}']", ident),

            TokenKind::EQ => write!(f, "[=]"),
            TokenKind::LT => write!(f, "[<]"),
            TokenKind::GT => write!(f, "[>]"),
            TokenKind::NE => write!(f, "[<>]"),
            TokenKind::GE => write!(f, "[>=]"),
            TokenKind::LE => write!(f, "[<=]"),

            TokenKind::Add => write!(f, "[+]"),
            TokenKind::Sub => write!(f, "[-]"),
            TokenKind::Mul => write!(f, "[*]"),
            TokenKind::Div => write!(f, "[/]"),

            TokenKind::LArrow => write!(f, "[<-]"),
            TokenKind::RArrow => write!(f, "[->]"),

            TokenKind::Dump     => write!(f, "[>>]"),
            TokenKind::Char(ch) => write!(f, "c'{}'", ch),
            TokenKind::Str(s)   => write!(f, "s\"{}\"", s),

            TokenKind::Comma     => write!(f, "[,]"),
            TokenKind::Semicolon => write!(f, "[;]"),

            TokenKind::LParen => write!(f, "[(]"),
            TokenKind::RParen => write!(f, "[)]"),
            TokenKind::LBrace => write!(f, "[{{]"),
            TokenKind::RBrace => write!(f, "[}}]"),

            TokenKind::EOF | TokenKind::Error(_) => unimplemented!(),
        }
    }
}