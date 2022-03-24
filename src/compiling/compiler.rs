use inkwell::{
    context::Context,
    module::Module,
    builder::Builder,
    values::{FloatValue, PointerValue, FunctionValue},
    AddressSpace,
};

use std::collections::HashMap;

use crate::resolving::{ Program, Block, Stmt, Expr, Op };

pub struct Compiler<'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
}

type LocalPtrs<'ctx> = HashMap<String, PointerValue<'ctx>>;

impl<'ctx> Compiler<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        let module = ctx.create_module("module");
        Self {
            ctx,
            module,
        }
    }

    pub fn compile(&self, program: &Program) {
        let f64_type = self.ctx.f64_type();

        // Define globals
        for (name, &value) in program.globals.iter() {
            let global = self.module.add_global(f64_type, Some(AddressSpace::Global), name);
            global.set_initializer(&f64_type.const_float(value));
        }

        // Declare functions
        for (name, block) in program.funcs.iter() {
            let fn_type = f64_type.fn_type(&vec![f64_type.into(); block.param_count][..], false);
            self.module.add_function(name, fn_type, None);
        }

        // Define functions
        for (name, block) in program.funcs.iter() {
            self.compile_func(name, block);
        }
    }

    pub fn export(&self, out: &str) {
        todo!();
    }

    fn compile_func(&self, name: &str, block: &Block) {
        let f64_type = self.ctx.f64_type();

        // Add entry
        let func = self.module.get_function(name).unwrap();
        let fn_block = self.ctx.append_basic_block(func, "entry");

        // Move builder
        let builder = self.ctx.create_builder();
        builder.position_at_end(fn_block);

        // Allocate locals
        let mut locals: LocalPtrs<'ctx> = HashMap::new();
        for local in block.get_vars().iter() {
            let local_ptr = builder.build_alloca(f64_type, local);
            locals.insert(local.to_string(), local_ptr);
        }

        // Compile statements
        for stmt in block.stmts.iter() {
            self.build_stmt(stmt, &builder, &locals, func);
        }

        // Default return value
        let f64_zero = f64_type.const_zero();
        builder.build_return(Some(&f64_zero));
    }

    fn get_var_ptr(&self, name: &str, locals: Option<&LocalPtrs<'ctx>>) -> Option<PointerValue<'ctx>> {
        locals.and_then(|locals| {
            locals.get(name)
                  .map(|&local| local)
        }).or_else(|| {
            self.module.get_global(name)
                .map(|global| global.as_pointer_value())
        })
    }

    fn build_stmt(&self, stmt: &Stmt, builder: &Builder<'ctx>, locals: &LocalPtrs<'ctx>, func: FunctionValue<'ctx>) {
        match stmt {
            Stmt::Set(name, expr) => {
                let ptr = self.get_var_ptr(name, Some(locals)).unwrap();
                let value = self.build_expr(expr, builder, locals, func);
                builder.build_store(ptr, value);
            },
            Stmt::Return(expr) => {
                let value = self.build_expr(expr, builder, locals, func);
                builder.build_return(Some(&value));
            },
            Stmt::Expr(expr) => { self.build_expr(expr, builder, locals, func); },
        }
    }

    fn build_expr(&self, expr: &Expr, builder: &Builder<'ctx>, locals: &LocalPtrs<'ctx>, func: FunctionValue<'ctx>) ->  FloatValue<'ctx> {
        let f64_type = self.ctx.f64_type();
        match expr {
            Expr::Val(v) => f64_type.const_float(*v),
            Expr::Param(n) => func.get_nth_param(*n as u32).unwrap().into_float_value(),
            Expr::Get(name) => {
                let ptr = self.get_var_ptr(name, Some(locals)).unwrap();
                builder.build_load(ptr, "").into_float_value()
            },
            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.build_expr(lhs, builder, locals, func);
                let rhs = self.build_expr(rhs, builder, locals, func);
                match op {
                    Op::Add => builder.build_float_add(lhs, rhs, "add"),
                    _ => todo!(),
                }
            },
            _ => f64_type.const_zero(),
        }
    }
}