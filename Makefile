repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte

clean:
	ocamlbuild -clean