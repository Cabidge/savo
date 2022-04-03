use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use crate::lexing::{ Token, TokenKind };

#[derive(Clone)]
pub enum Ty {
    Num,
    Fun(Vec<Ty>),
    Deq,
}

pub enum Global {
    Num(f64),
    Fun(BlockRoot),
    Deq(Vec<f64>),
}

pub struct Program {
    pub globals: HashMap<String, Global>,
}

pub struct BlockRoot {
    pub params: Vec<Ty>,
    pub stmts: Vec<Stmt>,
    vars: HashMap<String, Vec<Ty>>,
}

pub enum Block {
    Root(BlockRoot),
    Sub(SubBlock),
}

pub struct SubBlock {
    pub parent: Rc<RefCell<Block>>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Cond(Box<Stmt>, Expr),
    Set(String, Expr),
    Break(Expr),
    Return(Expr),
    Rewind,
    Expr(Expr),
    Dump(Expr),
    DumpVal(Expr),
    Push(String, Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Val(f64),
    Param(usize),
    Get(String),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Block(Vec<Stmt>),
    DequeExpr(String, &'static str),
    DequeAt(String, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Op {
    // Algebraic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    
    // Comparison
    EQ,
    NE,
    GT,
    LT,
    GE,
    LE,
}

impl Program {
    pub fn new() -> Self {
        Program {
            globals: HashMap::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        todo!();
    }
}

impl BlockRoot {
    pub fn new(params: Vec<Ty>) -> Self {
        Self {
            params,
            stmts: Vec::new(),
            vars: HashMap::new(),
        }
    }

    fn format_var(id: usize, name: &str) -> String {
        format!("{} #{}", name, id)
    }

    pub(super) fn define(&mut self, name: String, ty: Ty) {
        self.vars.entry(name).or_insert(Vec::new()).push(ty);
    }

    pub(super) fn get_var(&self, name: &str, globals: &HashMap<String, Global>) -> Option<(String, Ty)> {
        self.vars.get(name)
            .map(|types| {
                let id = types.len();
                let ty = types.last().expect("Expect at least one type in vars vec");
                let var_name = Self::format_var(id, name);
                (var_name, ty.clone())
            })
            .or_else(||
                globals.get(name)
                    .map(|glob| (name.to_string(), glob.type_of()))
            )
    }

    pub fn get_vars(&self) -> Vec<(String, Ty)> {
        let mut vars = Vec::new();

        for (name, types) in self.vars.iter() {
            for (n, ty) in types.iter().enumerate() {
                let full_name = Self::format_var(n + 1, name);
                vars.push((full_name, ty.clone()));
            }
        }

        vars
    }
}

impl Block {
    pub(super) fn add_stmt(&mut self, stmt: Stmt) {
        match self {
            Block::Root(root) => root.stmts.push(stmt),
            Block::Sub(sub) => sub.stmts.push(stmt),
        }
    }

    pub(super) fn define(&mut self, name: String, ty: Ty) {
        match self {
            Block::Root(root) => root.define(name, ty),
            Block::Sub(sub) => sub.define(name, ty),
        }
    }

    pub(super) fn get_var(&self, name: &str, globals: &HashMap<String, Global>) -> Option<(String, Ty)> {
        match self {
            Block::Root(root) => root.get_var(name, globals),
            Block::Sub(sub) => sub.get_var(name, globals),
        }
    }
}

impl SubBlock {
    pub fn from(parent: Rc<RefCell<Block>>) -> SubBlock {
        Self {
            parent,
            stmts: Vec::new(),
        }
    }

    pub(super) fn define(&mut self, name: String, ty: Ty) {
        self.parent.borrow_mut().define(format!(">{}", name), ty)
    }

    pub(super) fn get_var(&self, name: &str, globals: &HashMap<String, Global>) -> Option<(String, Ty)> {
        self.parent.borrow()
            .get_var(&format!(">{}", name), globals)
            .or_else(|| self.parent.borrow().get_var(name, globals))
    }
}

impl Global {
    fn type_of(&self) -> Ty {
        match self {
            Global::Num(_) => Ty::Num,
            Global::Fun(block) => Ty::Fun(block.params.clone()),
            Global::Deq(_) => Ty::Deq,
        }
    }
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
            TokenKind::Mod => Op::Mod,
            TokenKind::Exp => Op::Exp,
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