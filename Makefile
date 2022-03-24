run:
	@cargo run
	@clang src/savo.o
	@rm src/savo.o
	@./a.out
	@rm a.out
