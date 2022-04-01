use std::cmp::Ordering;
use std::fmt;
use crate::lexing::{ Token, TokenKind::{ self, * } };

pub struct Parser {
    tokens: Vec<Token>,
    eof: Token,
    index: usize,
}

#[derive(Debug)]
pub enum Decl {
    Var(Token, Expr),
    Func(Token, Vec<String>, Vec<Decl>),
    Deque(Token, Vec<Expr>),
    Stmt(Stmt)
}

#[derive(Debug)]
pub enum Stmt {
    Cond(Box<Stmt>, Expr),
    Set(Token, Expr),
    Break(Expr),
    Return(Expr),
    Rewind,
    Dump(Expr),
    DumpStr(String),
    DumpVal(Expr),
    Expr(Expr),
    Push(Token, Vec<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Value(f64),
    Get(Token),
    BinOp(Token, Box<Expr>, Box<Expr>),
    IsNan(Box<Expr>),
    Negate(Box<Expr>),
    Not(Box<Expr>),
    Call(Token, Vec<Expr>),
    Block(Vec<Decl>),
    Pull,
    Pop(Token),
    Peek(Token),
    PopHead(Token),
    PeekHead(Token),
    Len(Token), // Get the size of a deque
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub token: Token,
}

#[derive(Debug)]
pub enum ErrorKind {
    ExpectIdentAfterLet,
    ExpectStackIdent,
    ExpectParenOrEqAfterLetIdent,
    DuplicateParam,
    ExpectIdentParam,
    ExpectCommaOrParenAfterParam,
    ExpectCommaOrParenAfterArg,
    ExpectCommaOrBrackInDeque,
    ExpectBrackAfterDequeExpr,
    ExpectBraceAfterParams,
    ExpectClosingBrace,
    ExpectSemicolonAfterStmt,
    StmtAfterTerminator,
    UnexpectedToken,
    UnmatchedParen,
    UnmatchedAngleBrack,
}

// TODO: Maybe move this into its own module
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

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            eof: Token::eof(),
            index: 0,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Decl>, Vec<Error>> {
        let mut decls = Vec::new();
        let mut errors = Vec::<Error>::new();

        while self.current().kind != TokenKind::EOF {
            while self.eat_current(&TokenKind::Semicolon) {};

            match self.parse_decl() {
                Ok(decl) if errors.len() == 0 => decls.push(decl),
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

        Ok(decls)
    }

    fn stabilize(&mut self) {
        while !(self.eat_current(&TokenKind::Semicolon) ||
                self.current().kind == TokenKind::EOF)
        {
            self.advance();
        }
    }

    fn parse_decl(&mut self) -> Result<Decl, Error> {
        if self.eat_current(&TokenKind::Let) {
            return self.parse_let();
        }

        Ok(Decl::Stmt(self.parse_stmt()?))
    }

    fn parse_let(&mut self) -> Result<Decl, Error> {
        if self.eat_current(&TokenKind::LBrack) {
            return self.parse_deque_decl();
        }

        let ident_token = self.current().clone();

        match ident_token.kind {
            TokenKind::Ident(_) => (),
            _ => ErrorKind::ExpectIdentAfterLet.raise_from(&ident_token)?,
        }

        self.advance();

        if self.eat_current(&TokenKind::EQ) {
            let expr = self.parse_expr()?;
            return Ok(Decl::Var(ident_token, expr));
        }

        let is_func = match self.current().kind {
            TokenKind::LParen |
            TokenKind::LBrace => true,
            _ => false,
        };

        if is_func {
            let params = if self.eat_current(&TokenKind::LParen) {
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
                params
            } else {
                Vec::new()
            };

            let block = if self.current().kind == TokenKind::LBrace {
                self.parse_block()?
            } else {
                ErrorKind::ExpectBraceAfterParams.raise_from(self.current())?;
            };

            return Ok(Decl::Func(ident_token, params, block));
        }

        ErrorKind::ExpectParenOrEqAfterLetIdent.raise_from(self.current())?;
    }

    fn parse_deque_decl(&mut self) -> Result<Decl, Error> {
        let ident_token = self.current().clone();

        match ident_token.kind {
            TokenKind::Ident(_) => (),
            _ => ErrorKind::ExpectStackIdent.raise_from(&ident_token)?,
        }

        self.advance();

        let mut exprs = Vec::new();

        while !self.eat_current(&TokenKind::RBrack) {
            if !self.eat_current(&TokenKind::Comma) {
                ErrorKind::ExpectCommaOrBrackInDeque.raise_from(self.current())?;
            }

            let expr = self.parse_expr()?;

            exprs.push(expr);
        }

        Ok(Decl::Deque(ident_token, exprs))
    }

    fn parse_block(&mut self) -> Result<Vec<Decl>, Error> {
        if !self.eat_current(&TokenKind::LBrace) {
            panic!("Cannot call parse_block on a non-`{{` token...");
        }

        fn is_terminator(decl: &Decl) -> bool {
            if let Decl::Stmt(stmt) = decl {
                match stmt {
                    Stmt::Return(_) |
                    Stmt::Break(_) |
                    Stmt::Rewind => return true,
                    _ => (),
                }
            }

            return false;
        }

        let mut decls = Vec::new();
        let mut error = None;
        let mut has_terminated = false;
        while !self.eat_current(&TokenKind::RBrace) {
            if self.current().kind == TokenKind::EOF {
                ErrorKind::ExpectClosingBrace.raise_from(self.current())?;
            }

            if !error.is_none() {
                self.advance();
                continue;
            }

            if has_terminated {
                ErrorKind::StmtAfterTerminator.raise_from(self.current())?;
            }

            match self.parse_decl() {
                Ok(decl) => {
                    if !self.eat_current(&TokenKind::Semicolon) &&
                        self.peek_previous().unwrap().kind != TokenKind::RBrace
                    {
                        error = Some(
                            Error {
                                kind: ErrorKind::ExpectSemicolonAfterStmt,
                                token: self.current().clone(),
                            }
                        );
                    }

                    if is_terminator(&decl) {
                        has_terminated = true;
                    }

                    decls.push(decl);
                },
                Err(err) => error = Some(err),
            }
        }


        if let Some(err) = error {
            Err(err)
        } else {
            let is_resolved = match decls.last() {
                Some(decl) => is_terminator(decl),
                _ => false,
            };

            if !is_resolved {
                decls.push(Decl::Stmt(Stmt::Break(Expr::Value(f64::NAN))));
            }

            Ok(decls)
        }
    }

    fn parse_block_expr(&mut self) -> Result<Expr, Error> {
        Ok(Expr::Block(self.parse_block()?))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        let stmt = match &self.current().kind {
            TokenKind::RArrow => self.parse_break(),
            TokenKind::RFatArrow => self.parse_return(),
            TokenKind::DRight => self.parse_dump(),
            TokenKind::TRight => self.parse_dump_val(),
            TokenKind::DExp => {
                self.advance();
                Ok(Stmt::Rewind)
            },
            TokenKind::Ident(_) => self.parse_ident_stmt(),
            TokenKind::LBrack => self.parse_deque_stmt(),
            _ => Ok(Stmt::Expr(self.parse_expr()?))
        }?;

        Ok(match stmt {
            Stmt::Expr(expr) if self.eat_current(&TokenKind::Cond) =>
                Stmt::Cond(self.parse_stmt()?.into(), expr),
            _ => stmt,
        })
    }

    fn parse_deque_stmt(&mut self) -> Result<Stmt, Error> {
        self.advance();

        if self.peek().kind != TokenKind::Comma {
            return Ok(Stmt::Expr(self.parse_deque_expr()?));
        }

        let ident_token = self.current().clone();

        match ident_token.kind {
            TokenKind::Ident(_) => (),
            _ => ErrorKind::ExpectStackIdent.raise_from(&ident_token)?,
        }

        self.advance();

        let mut exprs = Vec::new();

        while !self.eat_current(&TokenKind::RBrack) {
            if !self.eat_current(&TokenKind::Comma) {
                ErrorKind::ExpectCommaOrBrackInDeque.raise_from(self.current())?;
            }

            let expr = self.parse_expr()?;

            exprs.push(expr);
        }

        Ok(Stmt::Push(ident_token, exprs))
    }

    fn parse_deque_expr(&mut self) -> Result<Expr, Error> {
        let is_pop_head = self.eat_current(&TokenKind::Bang);
        let is_peek_head = !is_pop_head && self.eat_current(&TokenKind::Cond);

        let ident_token = self.current().clone();

        match ident_token.kind {
            TokenKind::Ident(_) => (),
            _ => ErrorKind::ExpectStackIdent.raise_from(&ident_token)?,
        }

        self.advance();

        fn expect_brack(parser: &mut Parser) -> Result<(), Error> {
            if !parser.eat_current(&TokenKind::RBrack) {
                ErrorKind::ExpectBrackAfterDequeExpr.raise_from(parser.current())?;
            }
            Ok(())
        }

        if is_pop_head {
            expect_brack(self)?;
            return Ok(Expr::PopHead(ident_token));
        }

        if is_peek_head {
            expect_brack(self)?;
            return Ok(Expr::PeekHead(ident_token));
        }

        if self.eat_current(&TokenKind::Bang) {
            expect_brack(self)?;
            return Ok(Expr::Pop(ident_token));
        }

        if self.eat_current(&TokenKind::Cond) {
            expect_brack(self)?;
            return Ok(Expr::Peek(ident_token));
        }

        if self.eat_current(&TokenKind::RBrack) {
            return Ok(Expr::Len(ident_token));
        }

        ErrorKind::UnexpectedToken.raise_from(self.current())?;
    }

    fn parse_dump(&mut self) -> Result<Stmt, Error> {
        let tkn = self.advance().clone();
        let stmt = match tkn.kind {
            TokenKind::Str(s) => {
                self.advance();
                Stmt::DumpStr(s.clone())
            },
            _ => Stmt::Dump(self.parse_expr()?)
        };

        Ok(stmt)
    }

    fn parse_dump_val(&mut self) -> Result<Stmt, Error> {
        self.advance();
        Ok(Stmt::DumpVal(self.parse_expr()?))
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

            lhs = Expr::BinOp(op_token, lhs.into(), rhs.into());
        }
        
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expr, Error> {
        if self.eat_current(&TokenKind::Sub) {
            let expr = self.parse_expr_prec(Precedence::new_l(u8::MAX))?;
            return Ok(Expr::Negate(expr.into()));
        }

        if self.eat_current(&TokenKind::Bang) {
            let expr = self.parse_expr_prec(Precedence::new_l(u8::MAX))?;
            return Ok(Expr::Not(expr.into()));
        }

        if self.eat_current(&TokenKind::LParen) {
            let expr = self.parse_expr()?;

            if !self.eat_current(&TokenKind::RParen) {
                ErrorKind::UnmatchedParen.raise_from(self.current())?;
            }

            return Ok(expr);
        }

        if self.eat_current(&TokenKind::LT) { // <
            let expr = self.parse_expr_prec(prec_of(&TokenKind::Add).unwrap())?; // Precedence higher than comparison ops

            if !self.eat_current(&TokenKind::GT) { // >
                ErrorKind::UnmatchedAngleBrack.raise_from(self.current())?;
            }

            return Ok(Expr::IsNan(expr.into()));
        }

        if self.eat_current(&TokenKind::DLeft) {
            return Ok(Expr::Pull);
        }

        if self.eat_current(&TokenKind::LBrack) {
            return self.parse_deque_expr();
        }

        match &self.current().kind {
            TokenKind::Value(_) => self.parse_value(),
            TokenKind::Char(_) => self.parse_char(),
            TokenKind::Ident(_) => self.parse_ident_expr(),
            TokenKind::LBrace => self.parse_block_expr(),
            _ => ErrorKind::UnexpectedToken.raise_from(self.current())?,
        }
    }

    fn parse_value(&mut self) -> Result<Expr, Error> {
        let v = match &self.current().kind {
            TokenKind::Value(v) => *v,
            _ => panic!("Cannot call parse_value on a non-value token"),
        };

        self.advance();

        Ok(Expr::Value(v))
    }

    fn parse_char(&mut self) -> Result<Expr, Error> {
        let ch = match &self.current().kind {
            TokenKind::Char(ch) => *ch,
            _ => panic!("Cannot call parse_char on a non-char token"),
        };

        self.advance();

        Ok(Expr::Value(ch as i8 as f64))
    }

    fn parse_ident_stmt(&mut self) -> Result<Stmt, Error> {
        match self.peek().kind {
            TokenKind::LArrow => self.parse_set(),
            _ => Ok(Stmt::Expr(self.parse_expr()?))
        }
    }

    fn parse_ident_expr(&mut self) -> Result<Expr, Error> {
        let ident_token = self.current().clone();
        match self.advance().kind {
            TokenKind::LParen => self.parse_call(ident_token),
            _ => Ok(Expr::Get(ident_token)),
        }
    }

    fn parse_set(&mut self) -> Result<Stmt, Error> {
        let ident_token = self.current().clone();

        self.advance();
        self.advance();

        let expr = self.parse_expr()?;

        Ok(Stmt::Set(ident_token, expr))
    }

    fn parse_call(&mut self, token: Token) -> Result<Expr, Error> {
        self.advance();
        
        let mut args = Vec::<Expr>::new();

        while !self.eat_current(&TokenKind::RParen) {
            args.push(self.parse_expr()?);

            if self.current().kind != TokenKind::RParen && !self.eat_current(&TokenKind::Comma) {
                ErrorKind::ExpectCommaOrParenAfterArg.raise_from(self.current())?;
            }
        }

        Ok(Expr::Call(token, args))
    }

    fn parse_break(&mut self) -> Result<Stmt, Error> {
        self.advance();

        let expr = self.parse_expr()?;

        Ok(Stmt::Break(expr.into()))
    }

    fn parse_return(&mut self) -> Result<Stmt, Error> {
        self.advance();

        let expr = self.parse_expr()?;

        Ok(Stmt::Return(expr.into()))
    }

    // -- Token stream helper methods

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
        match self {
            Expr::BinOp(tkn, lhs, rhs) => write!(f, "({} {} {})", tkn.kind, lhs, rhs),
            Expr::Call(tkn, args) => {
                write!(f, "({}", tkn.kind)?;
                for arg in args.iter() {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")?;

                Ok(())
            },
            Expr::Value(v) => v.fmt(f),
            Expr::Get(tkn) => tkn.get_ident().unwrap().fmt(f),
            Expr::IsNan(expr) => write!(f, "(isnan {})", expr),
            Expr::Negate(expr) => write!(f, "-{}", expr),
            Expr::Not(expr) => write!(f, "!{}", expr),
            Expr::Pull => write!(f, "<<"),
            Expr::Block(_) => todo!(),
            Expr::Pop(tkn) => write!(f, "[{}!]", tkn.get_ident().unwrap()),
            Expr::Peek(tkn) => write!(f, "[{}?]", tkn.get_ident().unwrap()),
            Expr::PopHead(tkn) => write!(f, "[!{}]", tkn.get_ident().unwrap()),
            Expr::PeekHead(tkn) => write!(f, "[?{}]", tkn.get_ident().unwrap()),
            Expr::Len(tkn) => write!(f, "[{}]", tkn.get_ident().unwrap()),
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
        Exp => Precedence::new_r(5),
        _ => return None,
    };

    Some(prec)
}