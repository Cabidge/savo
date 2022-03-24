use std::collections::HashMap;

use super::program::{ Program, Block, Op, Expr as IRExpr, Stmt };
use crate::parsing::*;

pub fn resolve_exprs(exprs: &[Expr]) -> Program {
    let mut program = Program::new();

    for expr in exprs.iter() {
        match &*expr.kind {
            ExprKind::Var(name, expr) => {
                let value = calculate_literal(&program.globals, &expr);
                program.globals.insert(name.clone(), value);
            },
            ExprKind::Func(name, params, body) => {
                let block = resolve_func(&program, params, body);
                program.funcs.insert(name.clone(), block);
            },
            _ => unimplemented!(),
        }
    }

    program
}

fn resolve_func(
    program: &Program,
    params: &[String],
    body: &[Expr]
) -> Block {
    let mut block = Block::new(params.len());

    for (i, param) in params.iter().enumerate() {
        block.define(param.clone());
        let param = block.get_var_name(param, &program.globals).unwrap();
        block.stmts.push(Stmt::Set(param, IRExpr::Param(i)));
    }

    for expr in body.iter() {
        resolve_stmt(&mut block, program, expr);
    }

    block
}

fn resolve_stmt(block: &mut Block, program: &Program, expr: &Expr) {
    let mut res_expr = |expr| resolve_expr(block, program, expr);

    let stmt = match &*expr.kind {
        ExprKind::Var(name, initial) => {
            let initial = res_expr(initial);
            block.define(name.clone());
            let name = block.get_var_name(name, &program.globals).unwrap();
            Stmt::Set(name, initial)
        },
        ExprKind::Set(name, value) => {
            let value = res_expr(value);
            let name = block.get_var_name(name, &program.globals).unwrap();
            Stmt::Set(name, value)
        },
        ExprKind::Return(value) => Stmt::Return(res_expr(value)),
        ExprKind::Func(_, _, _) => todo!(),
        _ => Stmt::Expr(res_expr(expr)),
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

            let fn_name = expr.token.get_ident().unwrap();
            IRExpr::Call(fn_name, args)
        },
        _ => { 
            resolve_stmt(block, program, expr);
            IRExpr::Val(0.0)
        }
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