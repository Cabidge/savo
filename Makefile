run:
	@cargo run -- -o out/main out/main.savo
	@out/main
bin:
	@cargo build --release
	@sudo cp target/release/savo /usr/bin/savoc