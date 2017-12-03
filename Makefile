repl:
	ocamlbuild -use-menhir main.byte && ./main.byte

clean:
	ocamlbuild -clean