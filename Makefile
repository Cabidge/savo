run:
	@cargo run -- -o main out/main.savo
	@./main
	@rm ./main
bin:
	@cargo build --release
	@cp target/release/savo /usr/bin/savoc