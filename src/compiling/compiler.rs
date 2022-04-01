mod intrinsics;

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
use std::process;

use crate::resolving::{ Program, BlockRoot, Stmt, Expr, Op };

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
        let ptr_type = self.ctx.bool_type().ptr_type(AddressSpace::Generic);

        // Define globals
        for (name, &value) in program.globals.iter() {
            let global = self.module.add_global(f64_type, Some(AddressSpace::Global), name);
            global.set_initializer(&f64_type.const_float(value));
        }

        // Define global deques
        for name in program.deques.keys() {
            let deque = self.module.add_global(ptr_type, Some(AddressSpace::Global), name);
            deque.set_initializer(&ptr_type.const_null());
        }

        // Declare functions
        for (name, block) in program.funcs.iter() {
            let fn_type = f64_type.fn_type(&vec![f64_type.into(); block.param_count][..], false);
            self.module.add_function(name, fn_type, None);
        }

        // Define intrinsics
        intrinsics::build(&self.ctx, &self.module);

        // Define functions
        for (name, block) in program.funcs.iter() {
            self.build_func(name, block);
        }

        // Define main
        self.build_main(&program.deques);
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

        let o_file = tempfile::NamedTempFile::new().expect("Error creating temp file");

        // Compile to .o file
        target_machine
            .write_to_file(&self.module, FileType::Object, o_file.path().as_ref())
            .map_err(|e| format!("{:?}", e)).unwrap();

        let intrinsics_file = intrinsics::write_intrinsics();

        process::Command::new("cc")
            .arg(o_file.path().to_str().unwrap())
            .arg(intrinsics_file.path().to_str().unwrap())
            .arg("-no-pie")
            .arg("-lm")
            .arg("-o")
            .arg(out)
            .status()
            .unwrap();
    }

    fn build_main(&self, deques: &HashMap<String, Vec<f64>>) {
        let i32_type = self.ctx.i32_type();
        let main_fn_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);

        // Add entry
        let fn_block = self.ctx.append_basic_block(main_fn, "entry");

        // Move builder
        let builder = self.ctx.create_builder();
        builder.position_at_end(fn_block);

        // Construct deques
        for (name, vals) in deques.iter() {
            let f64_type = self.ctx.f64_type();
            let new_deque_fn = self.module.get_function("newDeque").unwrap();
            let push_fn = self.module.get_function("pushDeque").unwrap();

            let deque_ptr = builder
                .build_call(new_deque_fn, &[], "deque.new")
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_pointer_value();

            for val in vals {
                builder.build_call(
                    push_fn,
                    &[deque_ptr.into(), f64_type.const_float(*val).into()],
                    "deque.push"
                );
            }

            let global_deque_ptr = self
                .module
                .get_global(name)
                .unwrap()
                .as_pointer_value();

            builder.build_store(global_deque_ptr, deque_ptr);
        }

        // User-defined main fn
        let user_main_fn = self.module.get_function("$main").unwrap();

        // Build main
        builder.build_call(user_main_fn, &[], "call main")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_float_value();

        // Destruct deques
        for name in deques.keys() {
            let del_deque_fn = self.module.get_function("delDeque").unwrap();

            let deque_ptr = self
                .module
                .get_global(name)
                .unwrap()
                .as_pointer_value();

            let deque_ptr = builder.build_load(deque_ptr, "");
            builder.build_call(del_deque_fn, &[deque_ptr.into()], "deque.free");
        }

        // Return
        builder.build_return(Some(&i32_type.const_zero()));
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
        match stmt {
            Stmt::Cond(stmt, cond) => {
                let bool_type = self.ctx.bool_type();
                let cond = self.build_expr(&cond, fn_ctx);
                let cond = fn_ctx.builder.build_float_to_unsigned_int(cond, bool_type, "double2bool");

                let then_block = self.ctx.append_basic_block(fn_ctx.func, "cond.then");
                let end_block = self.ctx.append_basic_block(fn_ctx.func, "cond.end");

                fn_ctx.builder.build_conditional_branch(cond, then_block, end_block);

                fn_ctx.builder.position_at_end(then_block);
                self.build_stmt(stmt, fn_ctx, block_ctx);
                
                let last_instr = fn_ctx
                    .builder
                    .get_insert_block()
                    .and_then(|block| block.get_last_instruction())
                    .unwrap();

                if last_instr.get_opcode() != InstructionOpcode::Br {
                    fn_ctx.builder.build_unconditional_branch(end_block);
                }

                fn_ctx.builder.position_at_end(end_block);
            },
            Stmt::Set(name, expr) => {
                let ptr = self.get_var_ptr(name, Some(&fn_ctx.locals)).unwrap();
                let value = self.build_expr(expr, fn_ctx);
                fn_ctx.builder.build_store(ptr, value);
            },
            Stmt::Break(expr) => {
                let value = self.build_expr(expr, fn_ctx);

                if let Some((exit_block, exit_res_ptr)) = block_ctx.exit {
                    fn_ctx.builder.build_store(exit_res_ptr, value);
                    fn_ctx.builder.build_unconditional_branch(exit_block);
                } else {
                    fn_ctx.builder.build_return(Some(&value));
                }
            },
            Stmt::Return(expr) => {
                let value = self.build_expr(expr, fn_ctx);
                fn_ctx.builder.build_return(Some(&value));
            },
            Stmt::Rewind => { fn_ctx.builder.build_unconditional_branch(block_ctx.entry); }
            Stmt::Expr(expr) => { self.build_expr(expr, fn_ctx); },
            Stmt::Dump(expr) => {
                let expr = self.build_expr(expr, fn_ctx);

                let putfc = self.module.get_function("putfc").unwrap();
                fn_ctx.builder.build_call(putfc, &[expr.into()], "dump");
            },
            Stmt::DumpVal(expr) => {
                let expr = self.build_expr(expr, fn_ctx);

                let dumpf = self.module.get_function("dumpf").unwrap();
                fn_ctx.builder.build_call(dumpf, &[expr.into()], "dump");
            },
            Stmt::Push(name, expr) => {
                let expr = self.build_expr(expr, fn_ctx);

                let push_fn = self.module.get_function("pushDeque").unwrap();

                let deque_ptr = self
                    .module
                    .get_global(name)
                    .unwrap()
                    .as_pointer_value();

                let deque_ptr = fn_ctx.builder.build_load(deque_ptr, "");

                fn_ctx.builder.build_call(push_fn, &[deque_ptr.into(), expr.into()], "deque.push");
            }
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
                    Op::Exp => {
                        let pow = self.module.get_function("pow").unwrap();
                        fn_ctx.builder
                              .build_call(pow, &[lhs.into(), rhs.into()], "exp")
                              .try_as_basic_value()
                              .left()
                              .unwrap()
                              .into_float_value()

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
            Expr::Len(name) => {
                let len_fn = self.module.get_function("sizeOfDeque").unwrap();

                let deque_ptr = self
                    .module
                    .get_global(name)
                    .unwrap()
                    .as_pointer_value();

                let deque_ptr = fn_ctx.builder.build_load(deque_ptr, "");

                fn_ctx.builder.build_call(len_fn, &[deque_ptr.into()], "deque.len")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_float_value()
            }
            Expr::Pop(name) => {
                let pop_fn = self.module.get_function("popDeque").unwrap();

                let deque_ptr = self
                    .module
                    .get_global(name)
                    .unwrap()
                    .as_pointer_value();

                let deque_ptr = fn_ctx.builder.build_load(deque_ptr, "");

                fn_ctx.builder.build_call(pop_fn, &[deque_ptr.into()], "deque.pop")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_float_value()
            }
        }
    }
}