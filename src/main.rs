#![feature(never_type)]

use clap::{Command, Arg};

use std::fs;

mod lexing;
mod parsing;
mod resolving;
mod compiling;

fn main() {
    let m = Command::new("savoc")
        .arg(
            Arg::new("INPUT")
                .help("source savo file")
                .required(true)
                .index(1)
        )
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .takes_value(true)
                .value_name("FILE")
                .help("output filename")
        )
        .get_matches();

    let source_file = m.value_of("INPUT").unwrap();
    let source = fs::read_to_string(source_file).expect("Input file not found...");

    let output_file = m.value_of("output")
        .map(str::to_string)
        .unwrap_or(default_out_name(source_file));

    compiling::compile(&source, &output_file);
}

fn default_out_name(input_file: &str) -> String {
    let words: Vec<_> = input_file.split('.').collect();
    return format!("{}.o", words[0]);
}