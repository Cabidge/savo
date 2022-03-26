use inkwell::{
    context::Context,
    module::Module,
    builder::Builder,
    values::{
        BasicMetadataValueEnum,
        FloatValue,
        PointerValue,
        FunctionValue,
        InstructionOpcode,
    },
    AddressSpace,
    basic_block::BasicBlock,
    targets::{
        CodeModel,
        FileType,
        InitializationConfig,
        RelocMode,
        Target,
        TargetMachine,
    },
    OptimizationLevel,
    FloatPredicate,
};

use std::collections::HashMap;

use crate::resolving::{ Program, BlockRoot, CondStmt as Stmt, StmtKind, Expr, Op };

pub struct Compiler<'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
}

type LocalPtrs<'ctx> = HashMap<String, PointerValue<'ctx>>;

struct FuncContext<'ctx> {
    builder: Builder<'ctx>,
    locals: LocalPtrs<'ctx>,
    func: FunctionValue<'ctx>,
}

struct BlockContext<'ctx> {
    entry: BasicBlock<'ctx>,
    exit: Option<(BasicBlock<'ctx>, PointerValue<'ctx>)>,
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

        // Define intrinsics
        self.build_intrinsics();

        // Define functions
        for (name, block) in program.funcs.iter() {
            self.build_func(name, block);
        }

        // Define main
        self.build_main();
    }

    pub fn export(&self, out: &str) {
        Target::initialize_all(&InitializationConfig::default());

        // Target based on host machine
        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target = Target::from_triple(&target_triple)
            .map_err(|e| format!("{:?}", e)).unwrap();
        
        // Create target machine
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| "Unable to create target machine.".to_string()).unwrap();

        // Compile
        target_machine
            .write_to_file(&self.module, FileType::Object, out.as_ref())
            .map_err(|e| format!("{:?}", e)).unwrap();
    }

    fn build_main(&self) {
        let f64_type = self.ctx.f64_type();
        let i32_type = self.ctx.i32_type();
        let main_fn_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);

        // Add entry
        let fn_block = self.ctx.append_basic_block(main_fn, "entry");

        // Move builder
        let builder = self.ctx.create_builder();
        builder.position_at_end(fn_block);

        // User-defined main fn
        let user_main_fn = self.module.get_function("$main").unwrap();

        // Build main
        builder.build_call(user_main_fn, &[], "call main")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_float_value();

        // Return
        builder.build_return(Some(&i32_type.const_zero()));
    }

    fn build_intrinsics(&self) {
        let f64_type = self.ctx.f64_type();
        let str_type = self.ctx.i8_type().ptr_type(AddressSpace::Generic);
        let void_type = self.ctx.void_type();

        // -- printf
        let printf_fn_type = void_type.fn_type(&[str_type.into()], true);
        let printf_fn = self.module.add_function("printf", printf_fn_type, None);

        // Create format string
        let temp_fn_type = void_type.fn_type(&[], false);
        let temp_fn = self.module.add_function("?temp?", temp_fn_type, None);
        let temp_block = self.ctx.append_basic_block(temp_fn, "entry");

        let builder = self.ctx.create_builder();
        builder.position_at_end(temp_block);

        builder.build_return(None);

        builder.build_global_string_ptr("%f\n", ".floatfmt").as_pointer_value();
        builder.build_global_string_ptr("%c", ".charfmt").as_pointer_value();

        unsafe { temp_fn.delete() } // I can't figure out a fucking way to do this better
    }

    fn build_local(&self, var_name: &str, func: FunctionValue<'ctx>) -> PointerValue<'ctx> {
        let fn_entry = func.get_first_basic_block()
            .expect("Cannot call build_local on a function without a first block");

        let br_instr = fn_entry.get_last_instruction()
            .expect("Function entry has no instructions");

        let builder = self.ctx.create_builder();

        builder.position_before(&br_instr);

        builder.build_alloca(self.ctx.f64_type(), var_name)
    }

    fn build_func(&self, name: &str, block: &BlockRoot) {
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
        let block_entry = self.ctx.append_basic_block(func, "block.entry");
        builder.build_unconditional_branch(block_entry);
        builder.position_at_end(block_entry);

        let fn_ctx = FuncContext {
            builder,
            locals,
            func,
        };

        let block_ctx = BlockContext {
            entry: block_entry,
            exit: None,
        };

        for stmt in block.stmts.iter() {
            self.build_stmt(stmt, &fn_ctx, &block_ctx);
        }
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

    fn build_stmt(&self, stmt: &Stmt, fn_ctx: &FuncContext<'ctx>, block_ctx: &BlockContext<'ctx>) {
        if let Some(cond) = &stmt.cond {
            let bool_type = self.ctx.bool_type();
            let cond = self.build_expr(&cond, fn_ctx);
            let cond = fn_ctx.builder.build_float_to_unsigned_int(cond, bool_type, "double2bool");

            let then_block = self.ctx.append_basic_block(fn_ctx.func, "cond.then");
            let end_block = self.ctx.append_basic_block(fn_ctx.func, "cond.end");

            fn_ctx.builder.build_conditional_branch(cond, then_block, end_block);

            fn_ctx.builder.position_at_end(then_block);
            self.build_stmt_uncond(&stmt.kind, fn_ctx, block_ctx);
            
            let last_instr = fn_ctx
                .builder
                .get_insert_block()
                .and_then(|block| block.get_last_instruction())
                .unwrap();

            if last_instr.get_opcode() != InstructionOpcode::Br {
                fn_ctx.builder.build_unconditional_branch(end_block);
            }

            fn_ctx.builder.position_at_end(end_block);
        } else {
            self.build_stmt_uncond(&stmt.kind, fn_ctx, block_ctx);
        }
    }

    fn build_stmt_uncond(&self, stmt_kind: &StmtKind, fn_ctx: &FuncContext<'ctx>, block_ctx: &BlockContext<'ctx>) {
        match stmt_kind {
            StmtKind::Set(name, expr) => {
                let ptr = self.get_var_ptr(name, Some(&fn_ctx.locals)).unwrap();
                let value = self.build_expr(expr, fn_ctx);
                fn_ctx.builder.build_store(ptr, value);
            },
            StmtKind::Break(expr) => {
                let value = self.build_expr(expr, fn_ctx);

                if let Some((exit_block, exit_res_ptr)) = block_ctx.exit {
                    fn_ctx.builder.build_store(exit_res_ptr, value);
                    fn_ctx.builder.build_unconditional_branch(exit_block);
                } else {
                    fn_ctx.builder.build_return(Some(&value));
                }
            },
            StmtKind::Return(expr) => {
                let value = self.build_expr(expr, fn_ctx);
                fn_ctx.builder.build_return(Some(&value));
            },
            StmtKind::Rewind => { fn_ctx.builder.build_unconditional_branch(block_ctx.entry); }
            StmtKind::Expr(expr) => { self.build_expr(expr, fn_ctx); },
            StmtKind::Dump(expr) => {
                let expr = self.build_expr(expr, fn_ctx);

                let printf_fn = self.module.get_function("printf").unwrap();

                let fmt_string = self.module
                    .get_global(".floatfmt")
                    .unwrap()
                    .as_pointer_value();

                fn_ctx.builder.build_call(printf_fn, &[fmt_string.into(), expr.into()], "dump");
            },
            StmtKind::DumpChar(ch) => {
                let i8_type = self.ctx.i8_type();
                let ch = i8_type.const_int(*ch as u64, false);

                let printf_fn = self.module.get_function("printf").unwrap();

                let fmt_string = self.module
                    .get_global(".charfmt")
                    .unwrap()
                    .as_pointer_value();

                fn_ctx.builder.build_call(printf_fn, &[fmt_string.into(), ch.into()], "dump ch");
            },
        }
    }

    fn build_expr(&self, expr: &Expr, fn_ctx: &FuncContext<'ctx>) ->  FloatValue<'ctx> {
        let f64_type = self.ctx.f64_type();
        match expr {
            Expr::Val(v) => f64_type.const_float(*v),
            Expr::Param(n) => fn_ctx.func.get_nth_param(*n as u32).unwrap().into_float_value(),
            Expr::Get(name) => {
                let ptr = self.get_var_ptr(name, Some(&fn_ctx.locals)).unwrap();
                fn_ctx.builder.build_load(ptr, "").into_float_value()
            },
            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.build_expr(lhs, fn_ctx);
                let rhs = self.build_expr(rhs, fn_ctx);
                match op {
                    Op::Add => fn_ctx.builder.build_float_add(lhs, rhs, "add"),
                    Op::Sub => fn_ctx.builder.build_float_sub(lhs, rhs, "sub"),
                    Op::Mul => fn_ctx.builder.build_float_mul(lhs, rhs, "mul"),
                    Op::Div => fn_ctx.builder.build_float_div(lhs, rhs, "div"),
                    Op::Mod => {
                        let rem = fn_ctx.builder.build_float_rem(lhs, rhs, "rem");
                        let offset = fn_ctx.builder.build_float_add(rem, rhs, "offset");
                        fn_ctx.builder.build_float_rem(offset, rhs, "mod")
                    }
                    _ => {
                        // Comparison
                        // Cast bool to double
                        let pred = match op {
                            Op::EQ => FloatPredicate::OEQ,
                            Op::LT => FloatPredicate::OLT,
                            Op::GT => FloatPredicate::OGT,
                            Op::NE => FloatPredicate::ONE,
                            Op::GE => FloatPredicate::OGE,
                            Op::LE => FloatPredicate::OLE,
                            _ => unreachable!(),
                        };

                        let boolean = fn_ctx.builder.build_float_compare(
                            pred,
                            lhs,
                            rhs,
                            "compare",
                        );

                        fn_ctx.builder.build_unsigned_int_to_float(boolean, f64_type, "cast2f64")
                    }
                }
            },
            Expr::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|expr| self.build_expr(expr, fn_ctx).into())
                    .collect::<Vec<BasicMetadataValueEnum>>();
                
                let func = self.module.get_function(name).unwrap();

                fn_ctx.builder.build_call(func, &args[..], &format!("call {}", name))
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_float_value()
            },
            Expr::Block(stmts) => {
                let block_res = self.build_local("block.res", fn_ctx.func);
                let block_entry = self.ctx.append_basic_block(fn_ctx.func, "block.entry");
                let block_exit = self.ctx.append_basic_block(fn_ctx.func, "block.exit");

                let block_ctx = BlockContext {
                    entry: block_entry,
                    exit: Some((block_exit, block_res)),
                };

                fn_ctx.builder.build_unconditional_branch(block_entry);

                fn_ctx.builder.position_at_end(block_entry);
                for stmt in stmts.iter() {
                    self.build_stmt(stmt, fn_ctx, &block_ctx);
                }

                fn_ctx.builder.position_at_end(block_exit);
                fn_ctx.builder
                      .build_load(block_res, "")
                      .into_float_value()
            },
        }
    }
}