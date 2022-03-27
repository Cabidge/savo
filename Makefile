run:
	@cargo run -- -o out/main.o out/main.savo
	@clang -lm out/main.o
	@rm out/main.o
	@./a.out
	@rm a.out
