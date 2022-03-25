use std::collections::HashMap;

use super::program::{ Program, Block, Op, Expr as IRExpr, Stmt as IRStmt };
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
) -> Block {
    let mut block = Block::new(params.len());

    for (i, param) in params.iter().enumerate() {
        block.define(param.clone());
        let param = block.get_var_name(param, &program.globals).unwrap();
        block.stmts.push(IRStmt::Set(param, IRExpr::Param(i)));
    }

    for stmt in body.iter() {
        resolve_stmt(&mut block, program, stmt);
    }

    // I'll need to figure out a better way to do this some other time...
    if block.stmts.len() == 0 {
        block.stmts.push(IRStmt::Return(IRExpr::Val(0.0)));
    } else if let IRStmt::Return(_) = block.stmts[block.stmts.len() - 1] {

    } else {
        block.stmts.push(IRStmt::Return(IRExpr::Val(0.0)));
    }

    block
}

fn resolve_stmt(block: &mut Block, program: &Program, stmt: &Stmt) {
    let mut res_expr = |expr| resolve_expr(block, program, expr);

    let stmt = match &stmt.kind {
        StmtKind::Var(name, initial) => {
            let initial = res_expr(initial);
            block.define(name.clone());
            let name = block.get_var_name(name, &program.globals).unwrap();
            IRStmt::Set(name, initial)
        },
        StmtKind::Set(name, value) => {
            let value = res_expr(value);
            let name = block.get_var_name(name, &program.globals).unwrap();
            IRStmt::Set(name, value)
        },
        StmtKind::Return(value) => {
            let value = res_expr(value);
            if let Some(IRStmt::Return(_)) = &block.stmts[..].last() {
                panic!("Unreachable return"); // TODO: Better error messages
            }
            IRStmt::Return(value)
        },
        StmtKind::Func(_, _, _) => todo!(),
        StmtKind::Expr(expr) => IRStmt::Expr(res_expr(expr)),
        StmtKind::Dump(expr) => IRStmt::Dump(res_expr(expr)),
        StmtKind::DumpChar(ch) => IRStmt::DumpChar(*ch),
        StmtKind::DumpStr(s) => {
            s.chars().for_each(|ch| {
                block.stmts.push(IRStmt::DumpChar(ch))
            });
            return;
        }
    };

    block.stmts.push(stmt);
}

fn resolve_expr(block: &mut Block, program: &Program, expr: &Expr) -> IRExpr {
    match &*expr.kind {
        ExprKind::Value(n) => IRExpr::Val(*n),
        ExprKind::Get(name) => {
            let name = block.get_var_name(name, &program.globals).unwrap();
            IRExpr::Get(name)
        },
        ExprKind::BinOp(lhs, rhs) => {
            let op = Op::try_from(&expr.token).unwrap();
            let lhs = resolve_expr(block, program, lhs);
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
                .map(|arg| resolve_expr(block, program, arg))
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