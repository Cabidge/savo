mod compiler;

use inkwell::context::Context;

use std::process;

use compiler::Compiler;

use crate::parsing::parse;
use crate::resolving::{ resolve_decls, Program };

pub fn compile(src: &str, out:& str) {
    let decls = match parse(src) {
        Ok(decls) => decls,
        Err(_) => {
            eprintln!("Compilation failed because of above errors...");
            process::exit(1);
        }
    };

    let program = resolve_decls(&decls);

    compile_program(&program, out)
}

pub fn compile_program(program: &Program, out: &str) {
    let ctx = Context::create();

    let compiler = Compiler::new(&ctx);
    compiler.compile(program);

    //println!("{}", compiler.module.print_to_string().to_string());

    compiler.export(out)
}