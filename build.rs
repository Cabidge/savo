fn main() {
    cc::Build::new()
        .file("src/intrinsics.c")
        .out_dir("src")
        .compile("intrinsics");
}