repl:
	ocamlbuild -use-menhir -lflags "graphics.cma" main.byte && ./main.byte

test:
	ocamlbuild -use-menhir -package oUnit -lflags "graphics.cma" test.byte && ./test.byte

clean:
	ocamlbuild -clean
