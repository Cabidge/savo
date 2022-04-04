mod compiler;

use inkwell::context::Context;

use std::process;

use compiler::Compiler;

use crate::parsing::parse;
use crate::resolving::{ resolve_decls, Program };

pub fn compile(src: &str, out:& str) {
    let res = parse(src)
        .and_then(|decls| resolve_decls(&decls))
        .map(|program| compile_program(&program, out));
    
    match res {
        Ok(_) => (),
        Err(_) => {
            eprintln!("Compilation failed...");
            process::exit(1);
        },
    }
}

pub fn compile_program(program: &Program, out: &str) {
    let ctx = Context::create();

    let compiler = Compiler::new(&ctx);
    compiler.compile(program);

    println!("{}", compiler.module.print_to_string().to_string());

    compiler.export(out)
}