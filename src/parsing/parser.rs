use std::cmp::Ordering;
use std::fmt;
use crate::lexing::{ Token, TokenKind::{ self, * } };

pub struct Parser {
    tokens: Vec<Token>,
    eof: Token,
    index: usize,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub token: Token,
}

#[derive(Debug)]
pub enum StmtKind {
    Var(String, Expr),
    Func(String, Vec<String>, Vec<Stmt>),
    Cond(CondStmt, Option<Expr>),
}

// Statements that can be optionally conditional
#[derive(Debug)]
pub enum CondStmt {
    Set(String, Expr),
    Break(Expr),  // -> Used for "resolving" blocks
    Return(Expr), // => Used for returning from functions
    Rewind,       // ^^ Return to start of block
    Dump(Expr),
    DumpChar(char),
    DumpStr(String),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub token: Token,
}

#[derive(Debug)]
pub enum ExprKind {
    Value(f64),
    Get(String),
    BinOp(Expr, Expr),
    Negate(Expr),
    Not(Expr),
    Call(Vec<Expr>),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub token: Token,
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
    StmtAfterTerminator,
    UnexpectedToken,
    UnmatchedParen,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            eof: Token::eof(),
            index: 0,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut stmts = Vec::new();
        let mut errors = Vec::<Error>::new();

        while self.current().kind != TokenKind::EOF {
            while self.eat_current(&TokenKind::Semicolon) {};

            match self.parse_stmt() {
                Ok(stmt) if errors.len() == 0 => stmts.push(stmt),
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

        Ok(stmts)
    }

    fn stabilize(&mut self) {
        while !(self.eat_current(&TokenKind::Semicolon) ||
                self.current().kind == TokenKind::EOF)
        {
            self.advance();
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, Error> {
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

            return Ok(Stmt {
                kind: StmtKind::Var(ident, expr),
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

            return Ok(Stmt {
                kind: StmtKind::Func(ident, params, block),
                token: let_token,
            });
        }

        ErrorKind::ExpectParenOrEqAfterLetIdent.raise_from(self.current())?;
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, Error> {
        if !self.eat_current(&TokenKind::LBrace) {
            panic!("Cannot call parse_block on a non-`{{` token...");
        }

        let mut stmts = Vec::new();
        let mut error = None;

        while !self.eat_current(&TokenKind::RBrace) {
            if self.current().kind == TokenKind::EOF {
                ErrorKind::ExpectClosingBrace.raise_from(self.current())?;
            }

            if error.is_none() {
                match self.parse_stmt() {
                    Ok(stmt) => {
                        if !self.eat_current(&TokenKind::Semicolon) &&
                            self.peek_previous().unwrap().kind != TokenKind::RBrace
                        {
                            error = Some(
                                ErrorKind::ExpectSemicolonAfterStmt.raise_from(self.current())
                                    .unwrap_err()
                            );
                        }

                        stmts.push(stmt);
                    },
                    Err(err) => error = Some(err),
                }
            } else {
                self.advance();
            }
        }

        fn is_terminator(stmt: &Stmt) -> bool {
            match &stmt.kind {
                StmtKind::Cond(cond_stmt, None) => match cond_stmt {
                    CondStmt::Return(_) |
                    CondStmt::Break(_) |
                    CondStmt::Rewind => true,
                    _ => false,
                },
                _ => false,
            }
        }

        if let Some(err) = error {
            Err(err)
        } else {
            if let Some(_) = stmts.last() {
                for stmt in &stmts[..stmts.len()-1] {
                    if is_terminator(stmt) {
                        ErrorKind::StmtAfterTerminator.raise_from(&stmt.token)?;
                    }
                }
            }

            let is_resolved = match stmts.last() {
                Some(stmt) => is_terminator(stmt),
                _ => false,
            };

            if !is_resolved {
                stmts.push(Stmt {
                    kind: CondStmt::Break(Expr {
                        kind: ExprKind::Value(f64::NAN).into(),
                        token: self.current().clone(),
                    }).into(),
                    token: self.current().clone(),
                })
            }

            Ok(stmts)
        }
    }

    fn parse_block_expr(&mut self) -> Result<Expr, Error> {
        let token = self.current().clone();

        Ok(Expr {
            kind: ExprKind::Block(self.parse_block()?).into(),
            token,
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        match &self.current().kind {
            TokenKind::Let => self.parse_let(),
            _ => self.parse_cond(None),
        }
    }

    fn parse_cond(&mut self, condition: Option<Expr>) -> Result<Stmt, Error> {
        let token = self.current().clone();
        let cond_stmt = match &token.kind {
            TokenKind::RArrow => self.parse_break(),
            TokenKind::RFatArrow => self.parse_return(),
            TokenKind::DRight => self.parse_dump(),
            TokenKind::DExp => {
                self.advance();
                Ok(CondStmt::Rewind)
            },
            TokenKind::Ident(_) => {
                let stmt = self.parse_ident_stmt()?;

                match stmt {
                    CondStmt::Expr(expr) if self.eat_current(&TokenKind::Cond) => {
                        return self.parse_cond(Some(expr));
                    },
                    _ => Ok(stmt)
                }
            }
            _ => {
                let expr = self.parse_expr()?;

                if self.eat_current(&TokenKind::Cond) {
                    return self.parse_cond(Some(expr));
                }

                Ok(CondStmt::Expr(expr))
            }
        }?;

        Ok(Stmt {
            kind: StmtKind::Cond(cond_stmt, condition),
            token,
        })
    }

    fn parse_dump(&mut self) -> Result<CondStmt, Error> {
        let token = self.advance().clone();
        let stmt = match token.kind {
            TokenKind::Char(ch) => {
                self.advance();
                CondStmt::DumpChar(ch)
            }
            TokenKind::Str(s) => {
                self.advance();
                CondStmt::DumpStr(s.clone())
            }
            _ => {
                let expr = self.parse_expr()?;
                CondStmt::Dump(expr)
            }
        };

        Ok(stmt)
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
            TokenKind::Ident(_) => self.parse_ident_expr(),
            TokenKind::Sub => self.parse_negate(),
            TokenKind::Bang => self.parse_not(),
            TokenKind::LBrace => self.parse_block_expr(),
            TokenKind::LParen => self.parse_group(),
            _ => ErrorKind::UnexpectedToken.raise_from(self.current())?,
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

    fn parse_ident_stmt(&mut self) -> Result<CondStmt, Error> {
        let ident = if let TokenKind::Ident(id) = &self.current().kind {
            id.clone()
        } else {
            panic!("Cannot call parse_ident_stmt on a non-ident token...");
        };

        match self.peek().kind {
            TokenKind::LArrow => {
                self.advance();
                self.parse_set(ident)
            },
            _ => Ok(CondStmt::Expr(self.parse_expr()?))
        }
    }

    fn parse_ident_expr(&mut self) -> Result<Expr, Error> {
        let ident_token = self.current().clone();
        let ident = if let TokenKind::Ident(id) = &ident_token.kind {
            id.clone()
        } else {
            panic!("Cannot call parse_ident_expr on a non-ident token...");
        };

        self.advance();

        match self.current().kind {
            TokenKind::LParen => self.parse_call(ident_token),
            _ => Ok(Expr {
                kind: ExprKind::Get(ident).into(),
                token: ident_token,
            })
        }
    }

    fn parse_set(&mut self, ident: String) -> Result<CondStmt, Error> {
        self.advance();

        let expr = self.parse_expr()?;

        Ok(CondStmt::Set(ident, expr))
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

    fn parse_break(&mut self) -> Result<CondStmt, Error> {
        self.advance();

        let expr = self.parse_expr()?;

        Ok(CondStmt::Break(expr))
    }

    fn parse_return(&mut self) -> Result<CondStmt, Error> {
        self.advance();

        let expr = self.parse_expr()?;

        Ok(CondStmt::Return(expr))
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

    fn parse_not(&mut self) -> Result<Expr, Error> {
        let bang_token = self.current().clone();

        self.advance();

        let expr = self.parse_expr()?;

        Ok(Expr {
            kind: ExprKind::Not(expr).into(),
            token: bang_token,
        })
    }

    fn parse_group(&mut self) -> Result<Expr, Error> {
        self.advance();
        let expr = self.parse_expr()?;

        if !self.eat_current(&TokenKind::RParen) {
            ErrorKind::UnmatchedParen.raise_from(self.current())?;
        }

        Ok(expr)
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

    fn peek_previous(&self) -> Option<&Token> {
        if self.index > 0 {
            Some(self.get_token(self.index - 1))
        } else {
            None
        }
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

impl From<CondStmt> for StmtKind {
    fn from(cond: CondStmt) -> StmtKind {
        StmtKind::Cond(cond, None)
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprKind::Value(v) => v.fmt(f),
            ExprKind::Get(name) => name.fmt(f),
            ExprKind::Negate(expr) => write!(f, "-{}", expr),
            ExprKind::Not(expr) => write!(f, "!{}", expr),
            ExprKind::BinOp(_, _) |
            ExprKind::Call(_) => todo!(),
            ExprKind::Block(_) => todo!(),
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
        And | Or => Precedence::new_l(1),
        EQ | NE |
        GT | LT |
        GE | LE => Precedence::new_l(2),
        Add | Sub => Precedence::new_l(3),
        Mul | Div | Mod => Precedence::new_l(4),
        _ => return None,
    };

    Some(prec)
}