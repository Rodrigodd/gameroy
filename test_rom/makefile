
build: test.gb

.ONESHELL:
run: test.gb
	cd ".." && cargo run -- "test_rom\test.gb" -d

clean:
	rm test.gb
	rm test.o

test.gb: test.o
	rgblink -o test.gb test.o
	rgbfix test.gb

test.o: test.asm
	rgbasm -o test.o test.asm
