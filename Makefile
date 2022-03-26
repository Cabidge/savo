run:
	@cargo run
	@clang -lm src/savo.o
	@rm src/savo.o
	@./a.out
	@rm a.out
