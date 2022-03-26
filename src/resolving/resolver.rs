use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::program::{ Program, BlockRoot, SubBlock, Block, Op, Expr as IRExpr, Stmt as IRStmt };
use crate::parsing::*;

pub fn resolve_stmts(stmts: &[Stmt]) -> Program {
    let mut program = Program::new();

    for stmt in stmts.iter() {
        match &stmt.kind {
            StmtKind::Var(name, expr) => {
                let value = calculate_literal(&program.globals, &expr);
                program.globals.insert(name.clone(), value);
            },
            StmtKind::Func(name, params, body) => {
                let block = resolve_func(&program, params, body);

                let name = {
                    if name == "main" {
                        "$main".to_string()
                    } else {
                        name.to_string()
                    }
                };
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
    body: &[Stmt]
) -> BlockRoot {
    let mut block = Block::Root(BlockRoot::new(params.len()));

    for (i, param) in params.iter().enumerate() {
        block.define(param.clone());
        let param = block.get_var_name(param, &program.globals).unwrap();
        block.add_stmt(IRStmt::Set(param, IRExpr::Param(i)));
    }

    let block = Rc::new(RefCell::new(block));
    for stmt in body.iter() {
        resolve_stmt(block.clone(), program, stmt);
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

fn resolve_stmt(block: Rc<RefCell<Block>>, program: &Program, stmt: &Stmt) {
    let res_expr = |expr| resolve_expr(block.clone(), program, expr);

    let stmt = match &stmt.kind {
        StmtKind::Var(name, initial) => {
            let initial = res_expr(initial);
            block.borrow_mut().define(name.clone());
            let name = block.borrow().get_var_name(name, &program.globals).unwrap();
            IRStmt::Set(name, initial)
        },
        StmtKind::Func(_, _, _) => todo!(),
        StmtKind::Cond(cond_stmt, _) => match cond_stmt {
            CondStmt::Set(name, value) => {
                let value = res_expr(value);
                let name = block.borrow().get_var_name(name, &program.globals).unwrap();
                IRStmt::Set(name, value)
            },
            CondStmt::Break(value) => {
                let value = res_expr(value);
                match &*block.borrow() {
                    Block::Root(_) => IRStmt::Return(value),
                    Block::Sub(_) => IRStmt::Break(value),
                }
            },
            CondStmt::Return(value) => {
                let value = res_expr(value);
                IRStmt::Return(value)
            },
            CondStmt::Expr(expr) => IRStmt::Expr(res_expr(expr)),
            CondStmt::Dump(expr) => IRStmt::Dump(res_expr(expr)),
            CondStmt::DumpChar(ch) => IRStmt::DumpChar(*ch),
            CondStmt::DumpStr(s) => {
                s.chars().for_each(|ch| {
                    block.borrow_mut().add_stmt(IRStmt::DumpChar(ch))
                });
                return;
            }
            _ => todo!(),
        }
    };

    block.borrow_mut().add_stmt(stmt);
}

fn resolve_scope(parent: Rc<RefCell<Block>>, program: &Program, stmts: &[Stmt]) -> Vec<IRStmt> {
    let block = Rc::new(RefCell::new(Block::Sub(SubBlock::from(parent))));

    for stmt in stmts.iter() {
        resolve_stmt(block.clone(), program, stmt);
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
    match &*expr.kind {
        ExprKind::Value(n) => IRExpr::Val(*n),
        ExprKind::Get(name) => {
            let name = block.borrow().get_var_name(name, &program.globals).unwrap();
            IRExpr::Get(name)
        },
        ExprKind::BinOp(lhs, rhs) => {
            let op = Op::try_from(&expr.token).unwrap();
            let lhs = resolve_expr(block.clone(), program, lhs);
            let rhs = resolve_expr(block, program, rhs);
            IRExpr::BinOp(op, lhs.into(), rhs.into())
        }
        ExprKind::Negate(value) => {
            let lhs = IRExpr::Val(0.0);
            let rhs = resolve_expr(block, program, value);
            IRExpr::BinOp(Op::Sub, lhs.into(), rhs.into())
        },
        ExprKind::Call(args) => {
            let args: Vec<_> = args.iter()
                .map(|arg| resolve_expr(block.clone(), program, arg))
                .collect();

            let fn_name = {
                let name = expr.token.get_ident().unwrap();
                if name.as_str() == "main" {
                    "$main".to_string()
                } else {
                    name
                }
            };
            IRExpr::Call(fn_name, args)
        },
        _ => todo!(),
    }
}

fn calculate_literal(globals: &HashMap<String, f64>, expr: &Expr) -> f64 {
    match &*expr.kind {
        ExprKind::Value(val) => *val,
        ExprKind::Get(name) => globals[name.as_str()],
        ExprKind::Negate(expr) => -calculate_literal(globals, &expr),
        ExprKind::BinOp(lhs, rhs) => {
            let lhs = calculate_literal(globals, &lhs);
            let rhs = calculate_literal(globals, &rhs);

            match Op::try_from(&expr.token).unwrap() {
                Op::Add => lhs + rhs,
                Op::Sub => lhs - rhs,
                Op::Mul => lhs * rhs,
                Op::Div => lhs / rhs,
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