all: main.byte


main.byte:
	ocamlbuild -use-ocamlfind -use-menhir main.byte

clean:
	ocamlbuild -clean

# compile a program
%.run: %.o
	gcc -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.dp
	./main.byte $< > $@
