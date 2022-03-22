use std::collections::{ HashMap, HashSet };

pub struct Program {
    globals: HashMap<String, f64>,
    funcs: HashMap<String, Block>,
}

struct Block {
    vars: HashSet<String>,
    exprs: Vec<Stmt>,
}

enum Stmt {
    Set(String, Expr),
    Return(Expr),
    Expr(Expr),
}

enum Expr {
    Val(f64),
    Param(u64),
    Get(String),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>)
}

enum Op {
    // Algebraic
    Add,
    Sub,
    Mul,
    Div,
    
    // Comparison
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}