use std::collections::HashMap;
use std::fmt;
use crate::lexing::{ Token, TokenKind };

pub struct Program {
    pub globals: HashMap<String, f64>,
    pub funcs: HashMap<String, BlockRoot>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            globals: HashMap::new(),
            funcs: HashMap::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (name, n) in self.globals.iter() {
            write!(f, "{} = {}\n", name, n)?;
        }

        for (name, block) in self.funcs.iter() {
            write!(f, "\n")?;
            write!(f, "{}:\n", name)?;

            for local in block.get_vars().iter() {
                write!(f, "  let {}\n", local)?;
            }

            for stmt in block.stmts.iter() {
                write!(f, "  {:?}\n", stmt)?;
            }
        }

        Ok(())
    }
}

pub struct BlockRoot {
    pub param_count: usize,
    vars: HashMap<String, u8>,
    pub stmts: Vec<Stmt>,
}

pub enum Block<'a> {
    Root(BlockRoot),
    Sub(SubBlock<'a>),
}

pub struct SubBlock<'a> {
    pub parent: &'a mut Block<'a>,
}

impl BlockRoot {
    pub fn new(param_count: usize) -> Self {
        Self {
            param_count,
            vars: HashMap::new(),
            stmts: Vec::new(),
        }
    }

    fn format_var(id: u8, name: &str) -> String {
        format!("{} #{}", name, id)
    }

    pub(super) fn define(&mut self, name: String) {
        *self.vars.entry(name).or_insert(0) += 1;
    }

    pub(super) fn get_var_name(&self, name: &str, globals: &HashMap<String, f64>) -> Option<String> {
        self.vars.get(name)
            .map(|&id| Self::format_var(id, name))
            .or_else(|| {
                if globals.contains_key(name) {
                    Some(name.to_string())
                } else {
                    None
                }
            })
    }

    pub fn get_vars(&self) -> Vec<String> {
        let mut vars = Vec::<String>::new();

        for (name, &count) in self.vars.iter() {
            for id in 1..=count {
                vars.push(Self::format_var(id, name));
            }
        }

        vars
    }
}

impl<'a> Block<'a> {
    pub(super) fn add_stmt(&mut self, stmt: Stmt) {
        match self {
            Block::Root(root) => root.stmts.push(stmt),
            Block::Sub(sub) => sub.parent.add_stmt(stmt),
        }
    }

    pub(super) fn define(&mut self, name: String) {
        match self {
            Block::Root(root) => root.define(name),
            Block::Sub(sub) => sub.define(name),
        }
    }

    pub(super) fn get_var_name(&self, name: &str, globals: &HashMap<String, f64>) -> Option<String> {
        match self {
            Block::Root(root) => root.get_var_name(name, globals),
            Block::Sub(sub) => sub.get_var_name(name, globals),
        }
    }
}

impl<'a> SubBlock<'a> {
    pub fn from(parent: &'a mut Block<'a>) -> SubBlock<'a> {
        Self {
            parent,
        }
    }

    pub(super) fn define(&mut self, name: String) {
        self.parent.define(format!(">{}", name))
    }

    pub(super) fn get_var_name(&self, name: &str, globals: &HashMap<String, f64>) -> Option<String> {
        self.parent
            .get_var_name(&format!(">{}", name), globals)
            .or_else(|| self.parent.get_var_name(name, globals))
    }
}

#[derive(Debug)]
pub enum Stmt {
    Set(String, Expr),
    Break(Expr),
    Return(Expr),
    Expr(Expr),
    Dump(Expr),
    DumpChar(char),
}

#[derive(Debug)]
pub enum Expr {
    Val(f64),
    Param(usize),
    Get(String),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>)
}

#[derive(Debug)]
pub enum Op {
    // Algebraic
    Add,
    Sub,
    Mul,
    Div,
    
    // Comparison
    EQ,
    NE,
    GT,
    LT,
    GE,
    LE,
}

impl TryFrom<&TokenKind> for Op {
    type Error = ();
    fn try_from(tkn_kind: &TokenKind) -> Result<Self, Self::Error> {
        let op = match tkn_kind {
            TokenKind::EQ => Op::EQ,
            TokenKind::LT => Op::LT,
            TokenKind::GT => Op::GT,
            TokenKind::NE => Op::NE,
            TokenKind::GE => Op::GE,
            TokenKind::LE => Op::LE,
            TokenKind::Add => Op::Add,
            TokenKind::Sub => Op::Sub,
            TokenKind::Mul => Op::Mul,
            TokenKind::Div => Op::Div,
            _ => return Err(()),
        };
        Ok(op)
    }
}

impl TryFrom<&Token> for Op {
    type Error = ();
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Self::try_from(&token.kind)
    }
}