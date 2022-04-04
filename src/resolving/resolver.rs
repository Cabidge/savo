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
    Global,
    Ty,
};
use crate::lexing::{Token, TokenKind};
use crate::parsing::*;

enum Error {
    Positional(PositionalError, usize, usize),
    DuplicateGlobal(String),
    UndefinedMain,
}

enum PositionalError {
    UndefinedVariable(String),
    MismatchedTypes { expect: Ty, got: Ty },
}

type ResolveResult<T> = Result<T, Vec<Error>>;

pub fn resolve_decls(decls: &[Decl]) -> Result<Program, ()> {
    let mut program = Program::new();
    let mut errors = Vec::new();

    fn add_glob(globals: &mut HashMap<String, Global>, errors: &mut Vec<Error>, name: String, glob: Global) {
        if globals.insert(name.clone(), glob).is_some() {
            errors.push(Error::DuplicateGlobal(name));
        }
    }

    for decl in decls.iter() {
        match decl {
            Decl::Var(tkn, expr) => {
                let name = tkn.get_ident().expect("Var's token should be an ident");
                let value = calculate_literal(&program.globals, &expr);
                add_glob(&mut program.globals, &mut errors, name, Global::Num(value));
            },
            Decl::Func{ name: tkn, params, body, .. } => {
                let name = tkn.get_ident().expect("Func's token should be an ident");
                match resolve_func(name.clone(), &mut program, params, body) {
                    Ok(block) => add_glob(&mut program.globals, &mut errors, name, Global::Fun(block)),
                    Err(mut errs) => errors.append(&mut errs),
                }
            },
            Decl::Deque(tkn, exprs) => {
                let name = tkn.get_ident().expect("Deque's token should be an ident");
                let exprs = exprs
                    .iter()
                    .map(|expr| calculate_literal(&program.globals, expr))
                    .collect::<Vec<_>>();
                add_glob(&mut program.globals, &mut errors, name, Global::Deq(exprs));
            },
            _ => unimplemented!(),
        }
    }

    match program.globals.get("$main") {
        Some(Global::Fun(_)) => (),
        _ => errors.push(Error::UndefinedMain)
    }

    if errors.len() == 0 {
        Ok(program)
    } else {
        for err in errors {
            let msg = match err {
                Error::UndefinedMain => "Required function `main` is not defined".to_string(),
                Error::DuplicateGlobal(name) =>
                    format!("Duplicate global declaration `{}`", name),
                Error::Positional(err, line, col) => {
                    let msg = match err {
                        PositionalError::UndefinedVariable(name) =>
                            format!("Undefined variable `{}`", name),
                        PositionalError::MismatchedTypes { expect, got } =>
                            format!("Expected {} but got {}", expect, got),
                    };
                    format!("{} at {}:{}", msg, line, col)
                }
            };

            eprintln!("Error: {}", msg);
        }
        Err(())
    }
}

fn resolve_func(
    name: String,
    program: &mut Program,
    params: &[String],
    body: &[Decl]
) -> ResolveResult<BlockRoot> {
    let mut block = Block::Root(BlockRoot::new(name, vec![Ty::Num; params.len()]));
    let mut errors = Vec::new();

    for (i, param) in params.iter().enumerate() {
        block.define(param.clone(), Ty::Num);
        let (param, _) = block.get_var(param, &program.globals).unwrap();
        block.add_stmt(IRStmt::Set(param, IRExpr::Param(i)).into());
    }

    let block = Rc::new(RefCell::new(block));
    for decl in body.iter() {
        if let Err(mut errs) = resolve_decl(block.clone(), program, decl) {
            errors.append(&mut errs);
        }
    }

    let block = Rc::try_unwrap(block)
        .map_err(|_| "How da dog doin?".to_string())
        .unwrap()
        .into_inner();

    match block {
        Block::Root(root) => {
            if errors.len() == 0 {
                Ok(root)
            } else {
                Err(errors)
            }
        },
        _ => unreachable!(),
    }
}

fn resolve_decl(block: Rc<RefCell<Block>>, program: &mut Program, decl: &Decl) -> ResolveResult<()> {
    let mut res_expr = |expr| resolve_expr(block.clone(), program, expr);

    let stmt = match decl {
        Decl::Var(tkn, initial) => {
            let initial = res_expr(&initial)?;
            let name = tkn.get_ident().expect("Var's token should be an ident");
            block.borrow_mut().define(name.clone(), Ty::Num);
            let (name, _) = block.borrow().get_var(&name, &program.globals).unwrap();
            IRStmt::Set(name, initial)
        },
        Decl::Func { name: tkn, params, body, .. } => {
            let name = tkn.get_ident().expect("Func's token should be an ident");
            let fn_block = resolve_func(name.clone(), program, params, body)?;
            let glob = Global::Fun(fn_block);
            block.borrow_mut().define(name.clone(), glob.type_of());
            let (name, _) = block.borrow().get_var(&name, &program.globals).unwrap();
            program.globals.insert(name, glob);
            IRStmt::NoOp
        }
        Decl::Deque(..) => todo!(),
        Decl::Stmt(stmt) => resolve_stmt(block.clone(), program, &stmt)?,
    };

    block.borrow_mut().add_stmt(stmt);

    Ok(())
}

fn resolve_stmt(block: Rc<RefCell<Block>>, program: &mut Program, stmt: &Stmt) -> ResolveResult<IRStmt> {
    let mut res_expr = |expr| resolve_expr(block.clone(), program, expr);

    let stmt = match stmt {
        Stmt::Cond(stmt, cond) => {
            let cond = res_expr(&cond)?;
            let stmt = resolve_stmt(block, program, &stmt)?;
            IRStmt::Cond(stmt.into(), cond)
        },
        Stmt::Set(tkn, value) => {
            let value = res_expr(&value)?;
            let name = resolve_var(block, tkn, &program.globals, Ty::Num)?;
            IRStmt::Set(name, value)
        },
        Stmt::Break(value) => {
            let value = res_expr(&value)?;
            match &*block.borrow() {
                Block::Root(_) => IRStmt::Return(value),
                Block::Sub(_) => IRStmt::Break(value),
            }
        },
        Stmt::Return(value) => {
            let value = res_expr(&value)?;
            IRStmt::Return(value)
        },
        Stmt::Expr(expr) => IRStmt::Expr(res_expr(&expr)?),
        Stmt::Dump(expr) => IRStmt::Dump(res_expr(&expr)?),
        Stmt::DumpVal(expr) => IRStmt::DumpVal(res_expr(&expr)?),
        Stmt::DumpStr(s) => {
            let mut stmts = s.chars()
                .map(|ch| IRStmt::Dump(IRExpr::Val(ch as i8 as f64)))
                .collect::<Vec<_>>();

            stmts.push(IRStmt::Break(IRExpr::Val(f64::NAN)));

            IRStmt::Expr(IRExpr::Block(stmts))
        },
        Stmt::Rewind => IRStmt::Rewind,
        Stmt::Push(tkn, exprs) => {
            let name = resolve_var(block.clone(), tkn, &program.globals, Ty::Deq)?;

            let mut stmts = Vec::new();

            for expr in exprs.iter() {
                let expr = resolve_expr(block.clone(), program, expr)?;
                stmts.push(IRStmt::Push(name.clone(), expr));
            }

            stmts.push(IRStmt::Break(IRExpr::Val(f64::NAN)));

            IRStmt::Expr(IRExpr::Block(stmts))
        },
    };

    Ok(stmt)
}

fn resolve_scope(parent: Rc<RefCell<Block>>, program: &mut Program, decls: &[Decl]) -> ResolveResult<Vec<IRStmt>> {
    let block = Rc::new(RefCell::new(Block::Sub(SubBlock::from(parent))));
    let mut errors = Vec::new();

    for decl in decls.iter() {
        if let Err(mut errs) = resolve_decl(block.clone(), program, decl) {
            errors.append(&mut errs);
        }
    }

    let block = Rc::try_unwrap(block)
        .map_err(|_| "What da dog doin?".to_string())
        .unwrap()
        .into_inner();

    match block {
        Block::Sub(sub) => {
            if errors.len() == 0 {
                Ok(sub.stmts)
            } else {
                Err(errors)
            }
        },
        _ => unreachable!(),
    }
}

fn resolve_expr(block: Rc<RefCell<Block>>, program: &mut Program, expr: &Expr) -> ResolveResult<IRExpr> {
    let expr = match expr {
        Expr::Value(n) => IRExpr::Val(*n),
        Expr::Get(tkn) => {
            let name = resolve_var(block, tkn, &program.globals, Ty::Num)?;
            IRExpr::Get(name)
        },
        Expr::BinOp(tkn, lhs, rhs) => {
            let lhs = resolve_expr(block.clone(), program, lhs)?;
            let rhs = resolve_expr(block.clone(), program, rhs)?;

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
        Expr::IsNan(value) => {
            let lhs = resolve_expr(block ,program, value)?;
            let rhs = lhs.clone();
            IRExpr::BinOp(Op::NE, lhs.into(), rhs.into())
        }
        Expr::Negate(value) => {
            let lhs = IRExpr::Val(0.0);
            let rhs = resolve_expr(block, program, value)?;
            IRExpr::BinOp(Op::Sub, lhs.into(), rhs.into())
        },
        Expr::Not(value) => {
            let value = resolve_expr(block, program, value)?;
            IRExpr::Block(vec![
                IRStmt::Cond(
                    IRStmt::Break(IRExpr::Val(0.0)).into(),
                    value,
                ),
                IRStmt::Break(IRExpr::Val(1.0)),
            ])
        },
        Expr::Call(tkn, args) => {
            let mut new_args = Vec::new();

            for old_arg in args.iter() {
                new_args.push(resolve_expr(block.clone(), program, old_arg)?);
            }

            let fn_name = resolve_var(block, tkn, &program.globals, Ty::Fun(vec![Ty::Num; args.len()]))?;
            IRExpr::Call(fn_name, new_args)
        },
        Expr::Block(stmts) => {
            let stmts = resolve_scope(block, program, &stmts[..])?;
            IRExpr::Block(stmts)
        },
        Expr::Pull => IRExpr::Call("__getfc".to_string(), Vec::new()),
        Expr::Pop(tkn) | Expr::Peek(tkn) | Expr::PopHead(tkn) |
        Expr::PeekHead(tkn) | Expr::Len(tkn) => {
            let name = resolve_var(block, tkn, &program.globals, Ty::Deq)?;
            let func = match expr {
                Expr::Pop(_) => "__popDeque",
                Expr::Peek(_) => "__peekDeque",
                Expr::PopHead(_) => "__popHeadDeque",
                Expr::PeekHead(_) => "__PeekHeadDeque",
                Expr::Len(_) => "__sizeOfDeque",
                _ => unreachable!(),
            };
            IRExpr::DequeExpr(name, func)
        },
        Expr::At(tkn, index) => {
            let name = resolve_var(block.clone(), tkn, &program.globals, Ty::Deq)?;
            let index = resolve_expr(block, program, index)?;
            IRExpr::DequeAt(name, index.into())
        }
    };

    Ok(expr)
}

fn resolve_var(
    block: Rc<RefCell<Block>>,
    tkn: &Token,
    globals: &HashMap<String, Global>,
    expected_ty: Ty
) -> ResolveResult<String> {
    let name = tkn.get_ident().expect("Expected ident token");

    let var_opt = block
        .borrow()
        .get_var(&name, globals);

    match var_opt {
        Some((name, ty)) if ty == expected_ty => Ok(name),
        Some((_, ty)) => Err(
            PositionalError::MismatchedTypes {
                expect: expected_ty, 
                got: ty,
            }
            .at(tkn.line, tkn.col)
        ),
        None => Err(PositionalError::UndefinedVariable(name).at(tkn.line, tkn.col)),
    }.map_err(|err| vec![err])
}

impl PositionalError {
    fn at(self, line: usize, col: usize) -> Error {
        Error::Positional(self, line, col)
    }
}

fn calculate_literal(globals: &HashMap<String, Global>, expr: &Expr) -> f64 {
    match expr {
        Expr::Value(val) => *val,
        Expr::Get(tkn) => match globals[tkn.get_ident().unwrap().as_str()] {
            Global::Num(n) => n,
            _ => unimplemented!(),
        }
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
                Op::Exp => lhs.powf(rhs),
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