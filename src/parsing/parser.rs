use std::cmp::Ordering;
use std::fmt;
use crate::lexing::{ Token, TokenKind::{ self, * } };

pub struct Parser {
    tokens: Vec<Token>,
    eof: Token,
    index: usize,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub token: Token,
}

#[derive(Debug)]
pub enum ExprKind {
    Value(f64),
    Var(String, Expr),
    Func(String, Vec<String>, Vec<Expr>),
    Set(String, Expr),
    Get(String),
    Return(Expr),
    BinOp(Expr, Expr),
    Negate(Expr),
    Call(Vec<Expr>),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    token: Token,
}

#[derive(Debug)]
pub enum ErrorKind {
    ExpectIdentAfterLet,
    ExpectParenOrEqAfterLetIdent,
    DuplicateParam,
    ExpectIdentParam,
    ExpectCommaOrParenAfterParam,
    ExpectCommaOrParenAfterArg,
    ExpectBraceAfterParams,
    ExpectClosingBrace,
    ExpectSemicolonAfterStmt,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            eof: Token::eof(),
            index: 0,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Expr>, Vec<Error>> {
        let mut exprs = Vec::<Expr>::new();
        let mut errors = Vec::<Error>::new();

        while self.current().kind != TokenKind::EOF {
            while self.eat_current(&TokenKind::Semicolon) {};

            match self.parse_expr() {
                Ok(expr) if errors.len() == 0 => exprs.push(expr),
                Err(err) => {
                    errors.push(err);
                    self.stabilize();
                },
                _ => (),
            }
        }

        if errors.len() > 0 {
            return Err(errors);
        }
        
        Ok(exprs)
    }

    fn stabilize(&mut self) {
        while !(self.eat_current(&TokenKind::Semicolon) ||
                self.current().kind == TokenKind::EOF)
        {
            self.advance();
        }
    }

    fn parse_let(&mut self) -> Result<Expr, Error> {
        let let_token = self.current().clone();
        if let_token.kind != TokenKind::Let {
            panic!("Cannot call parse_let on a non-`let` token...");
        }

        let ident_token = self.advance();
        let ident = if let TokenKind::Ident(ident) = &ident_token.kind {
            ident.clone()
        } else {
            ErrorKind::ExpectIdentAfterLet.raise_from(ident_token)?;
        };

        self.advance();

        if self.eat_current(&TokenKind::EQ) {
            let expr = self.parse_expr()?;

            return Ok(Expr {
                kind: Box::new(ExprKind::Var(ident, expr)),
                token: let_token,
            })
        }

        if self.eat_current(&TokenKind::LParen) {
            let mut params = Vec::<String>::new();
            while !self.eat_current(&TokenKind::RParen) {
                let param = if let TokenKind::Ident(ident) = &self.current().kind {
                    ident.clone()
                } else {
                    ErrorKind::ExpectIdentParam.raise_from(self.current())?;
                };

                if params.contains(&param) {
                    ErrorKind::DuplicateParam.raise_from(self.current())?;
                }

                params.push(param);

                if self.advance().kind != TokenKind::RParen && !self.eat_current(&TokenKind::Comma) {
                    ErrorKind::ExpectCommaOrParenAfterParam.raise_from(self.current())?;
                }
            }

            let block = if self.current().kind == TokenKind::LBrace {
                self.parse_block()?
            } else {
                ErrorKind::ExpectBraceAfterParams.raise_from(self.current())?;
            };

            return Ok(Expr {
                kind: Box::new(ExprKind::Func(ident, params, block)),
                token: let_token,
            });
        }

        ErrorKind::ExpectParenOrEqAfterLetIdent.raise_from(self.current())?;
    }

    fn parse_block(&mut self) -> Result<Vec<Expr>, Error> {
        if !self.eat_current(&TokenKind::LBrace) {
            panic!("Cannot call parse_block on a non-`{{` token...");
        }

        let mut exprs = Vec::<Expr>::new();
        let mut error = None;

        while !self.eat_current(&TokenKind::RBrace) {
            if self.current().kind == TokenKind::EOF {
                ErrorKind::ExpectClosingBrace.raise_from(self.current())?;
            }

            if error.is_none() {
                match self.parse_expr() {
                    Ok(expr) => {
                        exprs.push(expr);
                        if !self.eat_current(&TokenKind::Semicolon) {
                            ErrorKind::ExpectSemicolonAfterStmt.raise_from(self.current())?;
                        }
                    },
                    Err(err) => error = Some(err),
                }
            }
        }

        if let Some(err) = error {
            Err(err)
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_expr_prec(Precedence::new_l(0))
    }

    fn parse_expr_prec(&mut self, left_prec: Precedence) -> Result<Expr, Error> {
        let mut lhs = self.parse_unary()?;
        
        loop {
            let op_token = self.current().clone();
            let right_prec = match prec_of(&op_token.kind) {
                Some(prec) => prec,
                None => break,
            };

            if left_prec > right_prec {
                break;
            }
            
            self.advance();
            let rhs = self.parse_expr_prec(right_prec)?;

            lhs = Expr {
                kind: Box::new(ExprKind::BinOp(lhs, rhs)),
                token: op_token,
            }
        }
        
        return Ok(lhs);
    }

    fn parse_unary(&mut self) -> Result<Expr, Error> {
        match &self.current().kind {
            TokenKind::Value(_) => self.parse_value(),
            TokenKind::Let => self.parse_let(),
            TokenKind::RArrow => self.parse_return(),
            TokenKind::Ident(_) => self.parse_ident(),
            TokenKind::Sub => self.parse_negate(),
            _ => panic!("{:?}", self.current()),
        }
    }

    fn parse_value(&mut self) -> Result<Expr, Error> {
        let value_token = self.current().clone();

        let v = match value_token.kind {
            TokenKind::Value(v) => v,
            _ => panic!("Cannot call parse_value on a non-value token"),
        };

        self.advance();

        Ok(Expr {
            kind: ExprKind::Value(v).into(),
            token: value_token,
        })
    }

    fn parse_ident(&mut self) -> Result<Expr, Error> {
        let ident_token = self.current().clone();
        let ident = if let TokenKind::Ident(id) = &ident_token.kind {
            id.clone()
        } else {
            panic!("Cannot call parse_ident on a non-ident token...");
        };

        self.advance();

        match self.current().kind {
            TokenKind::LArrow => self.parse_set(ident),
            TokenKind::LParen => self.parse_call(ident_token),
            _ => Ok(Expr {
                kind: ExprKind::Get(ident).into(),
                token: ident_token,
            })
        }
    }

    fn parse_set(&mut self, ident: String) -> Result<Expr, Error> {
        let larrow_token = self.current().clone();
        if larrow_token.kind != TokenKind::LArrow {
            panic!("Cannot call parse_set on a non-`<-` token...");
        }

        self.advance();

        let expr = self.parse_expr()?;

        Ok(Expr {
            kind: Box::new(ExprKind::Set(ident, expr)),
            token: larrow_token,
        })
    }

    fn parse_call(&mut self, token: Token) -> Result<Expr, Error> {
        let lparen_token = self.current().clone();
        if lparen_token.kind != TokenKind::LParen {
            panic!("Cannot call parse_call on a non-`(` token...");
        }

        self.advance();
        
        let mut args = Vec::<Expr>::new();

        while !self.eat_current(&TokenKind::RParen) {
            args.push(self.parse_expr()?);

            if self.current().kind != TokenKind::RParen && !self.eat_current(&TokenKind::Comma) {
                ErrorKind::ExpectCommaOrParenAfterArg.raise_from(self.current())?;
            }
        }

        Ok(Expr {
            kind: Box::new(ExprKind::Call(args)),
            token,
        })
    }

    fn parse_return(&mut self) -> Result<Expr, Error> {
        let rarrow_token = self.current().clone();
        if rarrow_token.kind != TokenKind::RArrow {
            panic!("Cannot call parse_set on a non-`->` token...");
        }

        self.advance();

        let expr = self.parse_expr()?;

        Ok(Expr {
            kind: ExprKind::Return(expr).into(),
            token: rarrow_token,
        })
    }

    fn parse_negate(&mut self) -> Result<Expr, Error> {
        let negate_token = self.current().clone();
        if negate_token.kind != TokenKind::Sub {
            panic!("Cannot call parse_negate on a non-`-` token...");
        }

        self.advance();

        let expr = self.parse_expr()?;

        Ok(Expr {
            kind: ExprKind::Negate(expr).into(),
            token: negate_token,
        })
    }

    fn get_token(&self, index: usize) -> &Token {
        self.tokens.get(index)
            .unwrap_or(&self.eof)
    }

    fn current(&self) -> &Token {
        self.get_token(self.index)
    }

    fn eat_current(&mut self, expected: &TokenKind) -> bool {
        if &self.current().kind == expected {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> &Token {
        self.index += 1;
        self.current()
    }

    fn peek(&self) -> &Token {
        self.get_token(self.index + 1)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.kind {
            ExprKind::BinOp(lhs, rhs) => write!(f, "({} {} {})", self.token.kind, lhs, rhs),
            ExprKind::Call(args) => {
                write!(f, "({}", self.token.kind)?;
                for arg in args.iter() {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")?;

                Ok(())
            },
            kind => kind.fmt(f),
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprKind::Value(v) => v.fmt(f),
            ExprKind::Var(name, expr) => write!(f, "(let {} {})", name, expr),
            ExprKind::Func(name, _, _) => write!(f, "(let {}(...) ...)", name),
            ExprKind::Set(name, expr) => write!(f, "({} <- {})", name, expr),
            ExprKind::Get(name) => name.fmt(f),
            ExprKind::Return(expr) => write!(f, "(-> {})", expr),
            ExprKind::Negate(expr) => write!(f, "(neg {})", expr),
            ExprKind::BinOp(_, _) |
            ExprKind::Call(_) => unimplemented!(),
            _ => todo!(),
        }
    }
}

impl ErrorKind {
    fn raise_from(self, token: &Token) -> Result<!, Error> {
        Err(Error {
            kind: self,
            token: token.clone(),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Precedence {
    power: u8,
    assoc: Associativity,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Associativity {
    Left,
    Right,
}

impl Precedence {
    fn new_l(power: u8) -> Self {
        Precedence {
            power,
            assoc: Associativity::Left,
        }
    }

    fn new_r(power: u8) -> Self {
        Precedence {
            power,
            assoc: Associativity::Right,
        }
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match self.power.cmp(&other.power) {
            Ordering::Equal if self.assoc == other.assoc => {
                match self.assoc {
                    Associativity::Left  => Ordering::Greater,
                    Associativity::Right => Ordering::Less,
                }
            },
            ord => ord,
        })
    }
}

fn prec_of(token: &TokenKind) -> Option<Precedence> {
    let prec = match token {
        EQ | NE |
        GT | LT |
        GE | LE => Precedence::new_l(1),
        Add | Sub => Precedence::new_l(2),
        Mul | Div => Precedence::new_l(3),
        _ => return None,
    };

    Some(prec)
}