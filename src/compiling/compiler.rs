use inkwell::{
    context::Context,
    module::Module,
    values::{FloatValue, PointerValue},
    AddressSpace,
};

use std::collections::HashMap;

use crate::resolving::{ Program, Block, Stmt, Expr };

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


        // Default return value
        let f64_zero = f64_type.const_zero();
        builder.build_return(Some(&f64_zero));
    }

    fn compile_stmt(&self, stmt: &Stmt, locals: &LocalPtrs<'ctx>) {
        match stmt {
            Stmt::Expr(expr) => { self.compile_expr(expr, locals); }
            _ => todo!(),
        }
    }

    fn compile_expr(&self, expr: &Expr, locals: &LocalPtrs<'ctx>) ->  FloatValue<'ctx> {
        let f64_type = self.ctx.f64_type();
        match expr {
            Expr::Val(v) => f64_type.const_float(*v),
            _ => todo!(),
        }
    }
}