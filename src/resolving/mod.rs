pub use resolver::Program;
use crate::parsing::{ parse, Expr };

mod resolver;

pub fn compile(src: &str) -> Result<Program, ()> {
    compile_ast(parse(src)?)
}

pub fn compile_ast(ast: Vec<Expr>) -> Result<Program, ()> {
    todo!();
}