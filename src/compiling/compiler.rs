use inkwell::{
    context::Context,
    module::Module,
    values::FloatValue,
    AddressSpace,
};

use crate::resolving::{ Program, Block, Stmt, Expr };

pub struct Compiler<'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
}

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
            //self.compile_func(block);
        }
    }

    pub fn export(&self, out: &str) {
        todo!();
    }

    fn compile_func(&self, block: &Block) {
        todo!();
    }

    fn compile_stmt(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => { self.compile_expr(expr); }
            _ => todo!(),
        }
    }

    fn compile_expr(&self, expr: &Expr) ->  FloatValue<'ctx> {
        todo!()
    }
}