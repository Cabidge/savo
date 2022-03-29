use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::program::{
    Program,
    BlockRoot,
    SubBlock,
    Block,
    Op,
    Expr as IRExpr,
    Stmt as IRStmt,
};
use crate::lexing::TokenKind;
use crate::parsing::*;

pub fn resolve_decls(decls: &[Decl]) -> Program {
    let mut program = Program::new();

    for decl in decls.iter() {
        match decl {
            Decl::Var(tkn, expr) => {
                let name = tkn.get_ident().expect("Var's token should be an ident");
                let value = calculate_literal(&program.globals, &expr);
                program.globals.insert(name, value);
            },
            Decl::Func(tkn, params, body) => {
                let name = tkn.get_ident().expect("Func's token should be an ident");
                let block = resolve_func(&program, params, body);
                program.funcs.insert(name, block);
            },
            _ => unimplemented!(),
        }
    }

    if !program.funcs.contains_key("$main") {
        panic!("No main function found");
    }

    program
}

fn resolve_func(
    program: &Program,
    params: &[String],
    body: &[Decl]
) -> BlockRoot {
    let mut block = Block::Root(BlockRoot::new(params.len()));

    for (i, param) in params.iter().enumerate() {
        block.define(param.clone());
        let param = block.get_var_name(param, &program.globals).unwrap();
        block.add_stmt(IRStmt::Set(param, IRExpr::Param(i)).into());
    }

    let block = Rc::new(RefCell::new(block));
    for decl in body.iter() {
        resolve_decl(block.clone(), program, decl);
    }

    let block = Rc::try_unwrap(block)
        .map_err(|_| "How da dog doin?".to_string())
        .unwrap()
        .into_inner();

    if let Block::Root(root) = block {
        root
    } else {
        unreachable!()
    }
}

fn resolve_decl(block: Rc<RefCell<Block>>, program: &Program, decl: &Decl) {
    let res_expr = |expr| resolve_expr(block.clone(), program, expr);

    let stmt = match decl {
        Decl::Var(tkn, initial) => {
            let initial = res_expr(&initial);
            let name = tkn.get_ident().expect("Var's token should be an ident");
            block.borrow_mut().define(name.clone());
            let name = block.borrow().get_var_name(&name, &program.globals).unwrap();
            IRStmt::Set(name, initial)
        },
        Decl::Func(_, _, _) => todo!(),
        Decl::Stmt(stmt) => resolve_stmt(block.clone(), program, &stmt),
    };

    block.borrow_mut().add_stmt(stmt);
}

fn resolve_stmt(block: Rc<RefCell<Block>>, program: &Program, stmt: &Stmt) -> IRStmt {
    let res_expr = |expr| resolve_expr(block.clone(), program, expr);

    match stmt {
        Stmt::Cond(stmt, cond) => {
            let cond = res_expr(&cond);
            let stmt = resolve_stmt(block, program, &stmt);
            IRStmt::Cond(stmt.into(), cond)
        },
        Stmt::Set(tkn, value) => {
            let value = res_expr(&value);
            let name = tkn.get_ident().expect("Set's token should be an ident");
            let name = block.borrow().get_var_name(&name, &program.globals).unwrap();
            IRStmt::Set(name, value)
        },
        Stmt::Break(value) => {
            let value = res_expr(&value);
            match &*block.borrow() {
                Block::Root(_) => IRStmt::Return(value),
                Block::Sub(_) => IRStmt::Break(value),
            }
        },
        Stmt::Return(value) => {
            let value = res_expr(&value);
            IRStmt::Return(value)
        },
        Stmt::Expr(expr) => IRStmt::Expr(res_expr(&expr)),
        Stmt::Dump(expr) => IRStmt::Dump(res_expr(&expr)),
        Stmt::DumpVal(expr) => IRStmt::DumpVal(res_expr(&expr)),
        Stmt::DumpStr(s) => {
            let mut stmts = s.chars()
                .map(|ch| IRStmt::Dump(IRExpr::Val(ch as i8 as f64)))
                .collect::<Vec<_>>();

            stmts.push(IRStmt::Break(IRExpr::Val(f64::NAN)));

            IRStmt::Expr(IRExpr::Block(stmts))
        },
        Stmt::Rewind => IRStmt::Rewind,
    }
}

fn resolve_scope(parent: Rc<RefCell<Block>>, program: &Program, decls: &[Decl]) -> Vec<IRStmt> {
    let block = Rc::new(RefCell::new(Block::Sub(SubBlock::from(parent))));

    for decl in decls.iter() {
        resolve_decl(block.clone(), program, decl);
    }

    let block = Rc::try_unwrap(block)
        .map_err(|_| "What da dog doin?".to_string())
        .unwrap()
        .into_inner();

    if let Block::Sub(sub) = block {
        sub.stmts
    } else {
        unreachable!()
    }
}

fn resolve_expr(block: Rc<RefCell<Block>>, program: &Program, expr: &Expr) -> IRExpr {
    match expr {
        Expr::Value(n) => IRExpr::Val(*n),
        Expr::Get(tkn) => {
            let name = tkn.get_ident().expect("Get's token should be an ident");
            let name = block.borrow().get_var_name(&name, &program.globals).unwrap();
            IRExpr::Get(name)
        },
        Expr::BinOp(tkn, lhs, rhs) => {
            let lhs = resolve_expr(block.clone(), program, lhs);
            let rhs = resolve_expr(block, program, rhs);

            if let Ok(op) = Op::try_from(tkn) {
                IRExpr::BinOp(op, lhs.into(), rhs.into())
            } else {
                match &tkn.kind {
                    TokenKind::And => IRExpr::Block(vec![
                        IRStmt::Cond(
                            IRStmt::Break(rhs).into(),
                            lhs,
                        ),
                        IRStmt::Break(IRExpr::Val(0.0)),
                    ]),
                    TokenKind::Or => IRExpr::Block(vec![
                        IRStmt::Cond(
                            IRStmt::Break(IRExpr::Val(1.0)).into(),
                            lhs,
                        ),
                        IRStmt::Break(rhs),
                    ]),
                    _ => unreachable!(),
                }
            }
        }
        Expr::Negate(value) => {
            let lhs = IRExpr::Val(0.0);
            let rhs = resolve_expr(block, program, value);
            IRExpr::BinOp(Op::Sub, lhs.into(), rhs.into())
        },
        Expr::Not(value) => {
            let value = resolve_expr(block, program, value);
            IRExpr::Block(vec![
                IRStmt::Cond(
                    IRStmt::Break(IRExpr::Val(0.0)).into(),
                    value,
                ),
                IRStmt::Break(IRExpr::Val(1.0)),
            ])
        },
        Expr::Call(tkn, args) => {
            let args: Vec<_> = args.iter()
                .map(|arg| resolve_expr(block.clone(), program, arg))
                .collect();

            let fn_name = tkn.get_ident().unwrap();
            IRExpr::Call(fn_name, args)
        },
        Expr::Block(stmts) => {
            let stmts = resolve_scope(block, program, &stmts[..]);
            IRExpr::Block(stmts)
        },
        Expr::Pull => IRExpr::Call("getfc".to_string(), Vec::new()),
    }
}

fn calculate_literal(globals: &HashMap<String, f64>, expr: &Expr) -> f64 {
    match expr {
        Expr::Value(val) => *val,
        Expr::Get(tkn) => globals[tkn.get_ident().unwrap().as_str()],
        Expr::Negate(expr) => -calculate_literal(globals, &expr),
        Expr::Not(expr) => if calculate_literal(globals, &expr) == 0.0 { 1.0 } else { 0.0 },
        Expr::BinOp(tkn, lhs, rhs) => {
            match &tkn.kind {
                TokenKind::And => {
                    if calculate_literal(globals, &lhs) == 0.0 {
                        return 0.0;
                    }
                    return calculate_literal(globals, &rhs);
                },
                TokenKind::Or => {
                    if calculate_literal(globals, &lhs) == 1.0 {
                        return 1.0;
                    }
                    return calculate_literal(globals, &rhs);
                },
                _ => (),
            }

            let lhs = calculate_literal(globals, &lhs);
            let rhs = calculate_literal(globals, &rhs);

            match Op::try_from(tkn).unwrap() {
                Op::Add => lhs + rhs,
                Op::Sub => lhs - rhs,
                Op::Mul => lhs * rhs,
                Op::Div => lhs / rhs,
                Op::Mod => lhs.rem_euclid(rhs),
                Op::EQ if lhs == rhs => 1.0,
                Op::NE if lhs != rhs => 1.0,
                Op::GT if lhs >  rhs => 1.0,
                Op::LT if lhs <  rhs => 1.0,
                Op::GE if lhs >= rhs => 1.0,
                Op::LE if lhs <= rhs => 1.0,
                _ => 0.0,
            }
        },
        _ => unimplemented!(),
    }
}