mod compiler;

use inkwell::context::Context;

use compiler::Compiler;

use crate::parsing::parse;
use crate::resolving::{ resolve_stmts, Program };

pub fn compile(src: &str, out:& str) {
    let stmts = parse(src).unwrap();
    let program = resolve_stmts(&stmts);

    compile_program(&program, out)
}

pub fn compile_program(program: &Program, out: &str) {
    let ctx = Context::create();

    let compiler = Compiler::new(&ctx);
    compiler.compile(program);

    //println!("{}", compiler.module.print_to_string().to_string());

    compiler.export(out)
}