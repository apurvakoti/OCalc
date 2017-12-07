repl:
	ocamlbuild -use-menhir -lflags "graphics.cma" main.byte && ./main.byte

clean:
	ocamlbuild -clean
